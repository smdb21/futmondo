-- Run after scripts/schema.sql. Review and back up before applying to production.
-- No credentials, network operations, or destructive reset commands are included.
BEGIN;
-- Unknown observed balances must remain NULL, including restricted rival finances.
ALTER TABLE user_team_history ALTER COLUMN budget DROP NOT NULL;
ALTER TABLE market_transactions ADD COLUMN IF NOT EXISTS source_event_id TEXT;
ALTER TABLE market_transactions ADD COLUMN IF NOT EXISTS provenance TEXT NOT NULL DEFAULT 'legacy';
CREATE TABLE IF NOT EXISTS market_transactions_archive
  (LIKE market_transactions INCLUDING DEFAULTS);
ALTER TABLE market_transactions_archive ADD COLUMN IF NOT EXISTS archived_at TIMESTAMPTZ DEFAULT NOW();
-- Archive exact duplicates, preserving the original IDs and all uncertain events.
WITH duplicates AS (
  SELECT id, ROW_NUMBER() OVER (PARTITION BY player_id, championship_id,
    buyer_team_id, seller_team_id, price, is_clause, transaction_date ORDER BY id) AS n
  FROM market_transactions
), moved AS (
  DELETE FROM market_transactions t USING duplicates d
  WHERE t.id=d.id AND d.n>1 RETURNING t.*
)
INSERT INTO market_transactions_archive SELECT moved.*, NOW() FROM moved;
CREATE UNIQUE INDEX IF NOT EXISTS transactions_source_event
  ON market_transactions(championship_id, source_event_id);

CREATE TABLE IF NOT EXISTS auction_observations (
  auction_id TEXT NOT NULL, championship_id TEXT NOT NULL,
  player_id TEXT NOT NULL, winner_id TEXT, winning_amount NUMERIC,
  settled_at TIMESTAMPTZ, observed_at TIMESTAMPTZ NOT NULL,
  outcome TEXT NOT NULL CHECK (outcome IN ('sold','unsold','open','unknown')),
  visibility_complete BOOLEAN NOT NULL DEFAULT FALSE,
  eligible_manager_ids JSONB, reference_value NUMERIC, reference_at TIMESTAMPTZ,
  listing_id TEXT, expires_at TIMESTAMPTZ,
  PRIMARY KEY (championship_id, auction_id)
);
CREATE TABLE IF NOT EXISTS bid_observations (
  championship_id TEXT NOT NULL, auction_id TEXT NOT NULL, player_id TEXT NOT NULL,
  bidder_id TEXT NOT NULL, bidder_name TEXT, amount NUMERIC NOT NULL CHECK(amount>0),
  is_winner BOOLEAN NOT NULL, source_bid_id TEXT,
  settled_at TIMESTAMPTZ, observed_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY(championship_id, auction_id, bidder_id)
);
CREATE TABLE IF NOT EXISTS market_observations (
  championship_id TEXT NOT NULL, player_id TEXT NOT NULL, listing_id TEXT NOT NULL,
  observed_at TIMESTAMPTZ NOT NULL, expires_at TIMESTAMPTZ, value NUMERIC,
  asking_price NUMERIC, bidder_count INTEGER, payload JSONB,
  PRIMARY KEY(championship_id, listing_id, observed_at)
);
CREATE TABLE IF NOT EXISTS player_match_observations (
  player_id TEXT NOT NULL, championship_id TEXT NOT NULL, season TEXT NOT NULL,
  scoring_version TEXT NOT NULL, round NUMERIC NOT NULL, points NUMERIC,
  score_status TEXT NOT NULL, is_home BOOLEAN, minutes_raw NUMERIC, start_raw BOOLEAN,
  participation BOOLEAN, observed_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY(player_id,championship_id,season,scoring_version,round,observed_at)
);
CREATE TABLE IF NOT EXISTS league_rule_snapshots (
  championship_id TEXT NOT NULL, scoring_version TEXT NOT NULL,
  season TEXT NOT NULL, observed_at TIMESTAMPTZ NOT NULL, configuration JSONB NOT NULL,
  PRIMARY KEY(championship_id,scoring_version,observed_at)
);
CREATE TABLE IF NOT EXISTS forecast_records (
  id TEXT PRIMARY KEY, user_id TEXT NOT NULL, championship_id TEXT NOT NULL,
  player_id TEXT, model_version TEXT NOT NULL, forecast_type TEXT NOT NULL,
  cutoff TIMESTAMPTZ NOT NULL, horizon INTEGER, prediction JSONB NOT NULL,
  season TEXT, scoring_version TEXT,
  outcome JSONB, evaluated_at TIMESTAMPTZ, created_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE TABLE IF NOT EXISTS transfer_scenarios (
  id BIGSERIAL PRIMARY KEY, user_id TEXT NOT NULL, championship_id TEXT NOT NULL,
  user_team_id TEXT NOT NULL, name TEXT NOT NULL, scenario JSONB NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(user_id,championship_id,user_team_id,name)
);
ALTER TABLE user_smart_alerts ADD COLUMN IF NOT EXISTS user_id TEXT;
ALTER TABLE user_smart_alerts ADD COLUMN IF NOT EXISTS event_key TEXT;
ALTER TABLE user_smart_alerts ADD COLUMN IF NOT EXISTS player_id TEXT;
CREATE UNIQUE INDEX IF NOT EXISTS alerts_event ON user_smart_alerts(user_id,championship_id,event_key);

CREATE TABLE IF NOT EXISTS automation_sessions (
  user_id TEXT PRIMARY KEY, encrypted_session JSONB NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL, updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE TABLE IF NOT EXISTS automation_policies (
  id TEXT PRIMARY KEY, user_id TEXT NOT NULL, championship_id TEXT NOT NULL,
  user_team_id TEXT NOT NULL, enabled BOOLEAN NOT NULL DEFAULT FALSE,
  mode TEXT NOT NULL DEFAULT 'shadow' CHECK(mode IN ('shadow','live')),
  policy JSONB NOT NULL, expires_at TIMESTAMPTZ NOT NULL,
  shadow_started_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  model_validated BOOLEAN NOT NULL DEFAULT FALSE,
  next_run_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), updated_at TIMESTAMPTZ DEFAULT NOW()
);
CREATE TABLE IF NOT EXISTS automation_jobs (
  id TEXT PRIMARY KEY, user_id TEXT NOT NULL, policy_id TEXT,
  championship_id TEXT NOT NULL, user_team_id TEXT NOT NULL,
  action_type TEXT NOT NULL, payload JSONB NOT NULL, idempotency_key TEXT NOT NULL UNIQUE,
  status TEXT NOT NULL DEFAULT 'pending'
    CHECK(status IN ('pending','running','shadow','succeeded','failed','uncertain','blocked','cancelled')),
  run_after TIMESTAMPTZ NOT NULL DEFAULT NOW(), claimed_at TIMESTAMPTZ,
  finished_at TIMESTAMPTZ, result JSONB, created_at TIMESTAMPTZ DEFAULT NOW()
);
ALTER TABLE automation_policies ADD COLUMN IF NOT EXISTS pause_reason TEXT;
ALTER TABLE automation_policies ADD COLUMN IF NOT EXISTS last_error TEXT;
ALTER TABLE automation_jobs ADD COLUMN IF NOT EXISTS lease_token TEXT;
ALTER TABLE automation_jobs ADD COLUMN IF NOT EXISTS execution_started_at TIMESTAMPTZ;
CREATE TABLE IF NOT EXISTS automation_account_locks (
  user_id TEXT PRIMARY KEY, job_id TEXT NOT NULL, expires_at TIMESTAMPTZ NOT NULL,
  lease_token TEXT NOT NULL
);
ALTER TABLE automation_account_locks ADD COLUMN IF NOT EXISTS lease_token TEXT;

CREATE OR REPLACE FUNCTION claim_automation_job() RETURNS SETOF automation_jobs
LANGUAGE plpgsql SECURITY DEFINER SET search_path=public AS $$
DECLARE candidate automation_jobs; acquired TEXT; token TEXT;
BEGIN
  -- Preflight leases can expire. Execution fences never expire into permission
  -- for another trade: uncertain outcomes keep the entire account locked.
  UPDATE automation_jobs j SET status='uncertain', finished_at=NOW(),
    result=COALESCE(j.result,'{}'::jsonb)||'{"reason":"worker interrupted; reconciliation required"}'::jsonb
    FROM automation_account_locks l
    WHERE j.id=l.job_id AND j.status='running' AND
      ((j.execution_started_at IS NULL AND l.expires_at<NOW()) OR
       j.execution_started_at<NOW()-INTERVAL '2 minutes');
  UPDATE automation_account_locks l SET expires_at='infinity'::timestamptz
    FROM automation_jobs j WHERE l.job_id=j.id AND j.status='uncertain';
  UPDATE automation_policies p SET enabled=FALSE,
    pause_reason='An account action has an uncertain outcome; reconcile it before resuming',updated_at=NOW()
    WHERE p.enabled AND EXISTS(SELECT 1 FROM automation_jobs j WHERE j.user_id=p.user_id AND j.status='uncertain');
  DELETE FROM automation_account_locks l USING automation_jobs j
    WHERE l.job_id=j.id AND j.status IN ('succeeded','failed','shadow','blocked','cancelled');
  FOR candidate IN SELECT j.* FROM automation_jobs j
      WHERE j.status='pending' AND j.run_after<=NOW()
        AND NOT EXISTS(SELECT 1 FROM automation_jobs u WHERE u.user_id=j.user_id AND u.status='uncertain')
      ORDER BY j.created_at FOR UPDATE SKIP LOCKED LIMIT 20 LOOP
    acquired := NULL;
    token := md5(random()::text||clock_timestamp()::text||candidate.id);
    INSERT INTO automation_account_locks(user_id,job_id,expires_at,lease_token)
      VALUES(candidate.user_id,candidate.id,NOW()+INTERVAL '2 minutes',token)
      ON CONFLICT DO NOTHING RETURNING user_id INTO acquired;
    IF acquired IS NOT NULL THEN
      RETURN QUERY UPDATE automation_jobs SET status='running',claimed_at=NOW(),lease_token=token
        WHERE id=candidate.id RETURNING *;
      RETURN;
    END IF;
  END LOOP;
END $$;

CREATE OR REPLACE FUNCTION begin_automation_execution(p_job_id TEXT,p_lease_token TEXT,p_observation JSONB)
RETURNS BOOLEAN LANGUAGE plpgsql SECURITY DEFINER SET search_path=public AS $$
DECLARE j automation_jobs; l automation_account_locks; p automation_policies;
BEGIN
  SELECT * INTO j FROM automation_jobs WHERE id=p_job_id FOR UPDATE;
  IF NOT FOUND OR j.status<>'running' OR j.lease_token IS DISTINCT FROM p_lease_token OR
      j.execution_started_at IS NOT NULL THEN RETURN FALSE; END IF;
  SELECT * INTO l FROM automation_account_locks WHERE user_id=j.user_id FOR UPDATE;
  IF NOT FOUND OR l.job_id<>j.id OR l.lease_token IS DISTINCT FROM p_lease_token OR l.expires_at<=NOW()
    THEN RETURN FALSE; END IF;
  SELECT * INTO p FROM automation_policies WHERE id=j.policy_id FOR UPDATE;
  IF NOT FOUND OR NOT p.enabled OR p.mode<>'live' OR p.expires_at<=NOW() OR
      p.user_id<>j.user_id OR p.championship_id<>j.championship_id OR p.user_team_id<>j.user_team_id
    THEN RETURN FALSE; END IF;
  -- Pin the lease before sending the external request. No worker can acquire
  -- this account until acknowledgement or positive read-only reconciliation.
  UPDATE automation_account_locks SET expires_at='infinity'::timestamptz WHERE user_id=j.user_id;
  UPDATE automation_jobs SET execution_started_at=NOW(),result=p_observation WHERE id=j.id;
  RETURN TRUE;
END $$;

CREATE OR REPLACE FUNCTION finish_automation_job(p_job_id TEXT,p_lease_token TEXT,p_result JSONB)
RETURNS BOOLEAN LANGUAGE plpgsql SECURITY DEFINER SET search_path=public AS $$
DECLARE j automation_jobs; l automation_account_locks; next_status TEXT;
BEGIN
  SELECT * INTO j FROM automation_jobs WHERE id=p_job_id FOR UPDATE;
  IF NOT FOUND OR j.status NOT IN ('running','uncertain') OR j.lease_token IS DISTINCT FROM p_lease_token
    THEN RETURN FALSE; END IF;
  SELECT * INTO l FROM automation_account_locks WHERE user_id=j.user_id FOR UPDATE;
  IF NOT FOUND OR l.job_id<>j.id OR l.lease_token IS DISTINCT FROM p_lease_token THEN RETURN FALSE; END IF;
  next_status := p_result->>'status';
  IF next_status IS NULL OR next_status NOT IN ('shadow','succeeded','failed','uncertain','blocked','cancelled')
    THEN RETURN FALSE; END IF;
  IF j.execution_started_at IS NOT NULL AND next_status IN ('shadow','blocked','cancelled') THEN RETURN FALSE; END IF;
  UPDATE automation_jobs SET status=next_status,finished_at=NOW(),
    result=COALESCE(j.result,'{}'::jsonb)||p_result WHERE id=j.id;
  IF next_status='uncertain' THEN
    UPDATE automation_account_locks SET expires_at='infinity'::timestamptz WHERE user_id=j.user_id;
    UPDATE automation_policies SET enabled=FALSE,
      pause_reason='An account action has an uncertain outcome; reconcile it before resuming',updated_at=NOW()
      WHERE user_id=j.user_id;
  ELSE
    DELETE FROM automation_account_locks WHERE user_id=j.user_id AND job_id=j.id AND lease_token=p_lease_token;
  END IF;
  RETURN TRUE;
END $$;
REVOKE ALL ON FUNCTION claim_automation_job() FROM PUBLIC;
REVOKE ALL ON FUNCTION begin_automation_execution(TEXT,TEXT,JSONB) FROM PUBLIC;
REVOKE ALL ON FUNCTION finish_automation_job(TEXT,TEXT,JSONB) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION claim_automation_job() TO service_role;
GRANT EXECUTE ON FUNCTION begin_automation_execution(TEXT,TEXT,JSONB) TO service_role;
GRANT EXECUTE ON FUNCTION finish_automation_job(TEXT,TEXT,JSONB) TO service_role;
-- Supabase service key is server-only. Browser/anonymous roles cannot read or
-- write private tables, jobs, encrypted sessions, or collected observations.
DO $$ DECLARE t TEXT; BEGIN
  FOREACH t IN ARRAY ARRAY['auction_observations','bid_observations','market_observations',
    'player_match_observations','league_rule_snapshots','forecast_records',
    'transfer_scenarios','automation_sessions','automation_policies','automation_jobs',
    'automation_account_locks','market_transactions_archive','user_smart_alerts'] LOOP
    EXECUTE format('ALTER TABLE %I ENABLE ROW LEVEL SECURITY',t);
    EXECUTE format('REVOKE ALL ON %I FROM anon, authenticated',t);
    EXECUTE format('GRANT ALL ON %I TO service_role',t);
  END LOOP;
END $$;
CREATE INDEX IF NOT EXISTS bids_champ_date ON bid_observations(championship_id,settled_at);
CREATE INDEX IF NOT EXISTS jobs_pending ON automation_jobs(status,run_after);
CREATE INDEX IF NOT EXISTS matches_champ_cutoff ON player_match_observations(championship_id,observed_at);
COMMIT;

-- Connected-session claim: never scans or claims another account's pending work.
CREATE OR REPLACE FUNCTION claim_connected_automation_job(p_job_id TEXT,p_user_id TEXT,p_championship_id TEXT,p_user_team_id TEXT)
RETURNS SETOF automation_jobs LANGUAGE plpgsql SECURITY DEFINER SET search_path=public AS $$
DECLARE candidate automation_jobs; acquired TEXT; token TEXT;
BEGIN
  UPDATE automation_jobs j SET status='uncertain', finished_at=NOW(),
    result=COALESCE(j.result,'{}'::jsonb)||'{"reason":"connected session interrupted; reconciliation required"}'::jsonb
    FROM automation_account_locks l WHERE j.id=l.job_id AND j.status='running' AND
      ((j.execution_started_at IS NULL AND l.expires_at<NOW()) OR j.execution_started_at<NOW()-INTERVAL '2 minutes');
  UPDATE automation_account_locks l SET expires_at='infinity'::timestamptz FROM automation_jobs j WHERE l.job_id=j.id AND j.status='uncertain';
  SELECT * INTO candidate FROM automation_jobs WHERE id=p_job_id AND user_id=p_user_id AND
    championship_id=p_championship_id AND user_team_id=p_user_team_id AND status='pending' AND run_after<=NOW() FOR UPDATE;
  IF NOT FOUND OR EXISTS(SELECT 1 FROM automation_jobs WHERE user_id=p_user_id AND status='uncertain') THEN RETURN; END IF;
  token := md5(random()::text||clock_timestamp()::text||candidate.id);
  INSERT INTO automation_account_locks(user_id,job_id,expires_at,lease_token) VALUES(candidate.user_id,candidate.id,NOW()+INTERVAL '2 minutes',token)
    ON CONFLICT DO NOTHING RETURNING user_id INTO acquired;
  IF acquired IS NOT NULL THEN RETURN QUERY UPDATE automation_jobs SET status='running',claimed_at=NOW(),lease_token=token WHERE id=candidate.id RETURNING *; END IF;
END $$;
REVOKE ALL ON FUNCTION claim_connected_automation_job(TEXT,TEXT,TEXT,TEXT) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION claim_connected_automation_job(TEXT,TEXT,TEXT,TEXT) TO service_role;
