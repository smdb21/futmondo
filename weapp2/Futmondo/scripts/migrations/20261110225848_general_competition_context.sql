-- General football competition/season context and immutable club observations.
-- Apply before deploying the matching context-aware R readers/writers.
-- No authentication sessions, identity aliases or historical values are synthesized.
BEGIN;
ALTER TABLE public.player_match_observations ADD COLUMN IF NOT EXISTS competition_id text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.player_round_history_checks ADD COLUMN IF NOT EXISTS competition_id text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.league_rule_snapshots ADD COLUMN IF NOT EXISTS competition_id text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.player_history ADD COLUMN IF NOT EXISTS competition_id text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.player_history ADD COLUMN IF NOT EXISTS season text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.user_team_history ADD COLUMN IF NOT EXISTS competition_id text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.user_team_history ADD COLUMN IF NOT EXISTS season text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.player_daily_snapshots ADD COLUMN IF NOT EXISTS competition_id text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.player_daily_snapshots ADD COLUMN IF NOT EXISTS season text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.round_dream_team ADD COLUMN IF NOT EXISTS competition_id text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.round_dream_team ADD COLUMN IF NOT EXISTS season text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.market_transactions ADD COLUMN IF NOT EXISTS competition_id text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.market_transactions ADD COLUMN IF NOT EXISTS season text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.rival_finance_evidence ADD COLUMN IF NOT EXISTS competition_id text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.complete_history_baselines ADD COLUMN IF NOT EXISTS competition_id text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.team_lineup_snapshots ADD COLUMN IF NOT EXISTS competition_id text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.round_dream_team ADD COLUMN IF NOT EXISTS scoring_version text NOT NULL DEFAULT 'unknown';
ALTER TABLE public.player_match_observations ADD COLUMN IF NOT EXISTS source_round_key text NOT NULL DEFAULT '';
ALTER TABLE public.player_match_observations ADD COLUMN IF NOT EXISTS round_id text;
ALTER TABLE public.player_match_observations ADD COLUMN IF NOT EXISTS match_id text;
ALTER TABLE public.player_match_observations ADD COLUMN IF NOT EXISTS source_round_number numeric;

DO $$ DECLARE existing_name text; definition text;
BEGIN
 SELECT conname,pg_get_constraintdef(oid) INTO existing_name,definition
 FROM pg_constraint WHERE conrelid='public.player_match_observations'::regclass AND contype='p';
 IF existing_name IS NULL OR position('competition_id' in definition)=0 OR position('source_round_key' in definition)=0 THEN
  IF existing_name IS NOT NULL THEN EXECUTE format('ALTER TABLE public.player_match_observations DROP CONSTRAINT %I',existing_name); END IF;
  ALTER TABLE public.player_match_observations ADD CONSTRAINT player_match_observations_pkey PRIMARY KEY (player_id,championship_id,competition_id,season,scoring_version,round,source_round_key,observed_at);
 END IF;
END $$;

DO $$ DECLARE existing_name text; definition text;
BEGIN
 SELECT conname,pg_get_constraintdef(oid) INTO existing_name,definition
 FROM pg_constraint WHERE conrelid='public.player_round_history_checks'::regclass AND contype='p';
 IF existing_name IS NULL OR position('competition_id' in definition)=0 THEN
  IF existing_name IS NOT NULL THEN EXECUTE format('ALTER TABLE public.player_round_history_checks DROP CONSTRAINT %I',existing_name); END IF;
  ALTER TABLE public.player_round_history_checks ADD CONSTRAINT player_round_history_checks_pkey PRIMARY KEY (championship_id,competition_id,player_id,season,scoring_version,checked_at);
 END IF;
END $$;

DO $$ DECLARE existing_name text; definition text;
BEGIN
 SELECT conname,pg_get_constraintdef(oid) INTO existing_name,definition
 FROM pg_constraint WHERE conrelid='public.league_rule_snapshots'::regclass AND contype='p';
 IF existing_name IS NULL OR position('competition_id' in definition)=0 THEN
  IF existing_name IS NOT NULL THEN EXECUTE format('ALTER TABLE public.league_rule_snapshots DROP CONSTRAINT %I',existing_name); END IF;
  ALTER TABLE public.league_rule_snapshots ADD CONSTRAINT league_rule_snapshots_pkey PRIMARY KEY (championship_id,competition_id,season,scoring_version,observed_at);
 END IF;
END $$;

DO $$ DECLARE existing_name text; definition text;
BEGIN
 SELECT conname,pg_get_constraintdef(oid) INTO existing_name,definition
 FROM pg_constraint WHERE conrelid='public.rival_finance_evidence'::regclass AND contype='p';
 IF existing_name IS NULL OR position('competition_id' in definition)=0 THEN
  IF existing_name IS NOT NULL THEN EXECUTE format('ALTER TABLE public.rival_finance_evidence DROP CONSTRAINT %I',existing_name); END IF;
  ALTER TABLE public.rival_finance_evidence ADD CONSTRAINT rival_finance_evidence_pkey PRIMARY KEY (account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at);
 END IF;
END $$;

DO $$ DECLARE existing_name text; definition text;
BEGIN
 SELECT conname,pg_get_constraintdef(oid) INTO existing_name,definition
 FROM pg_constraint WHERE conrelid='public.complete_history_baselines'::regclass AND contype='p';
 IF existing_name IS NULL OR position('competition_id' in definition)=0 THEN
  IF existing_name IS NOT NULL THEN EXECUTE format('ALTER TABLE public.complete_history_baselines DROP CONSTRAINT %I',existing_name); END IF;
  ALTER TABLE public.complete_history_baselines ADD CONSTRAINT complete_history_baselines_pkey PRIMARY KEY (account_id,championship_id,competition_id,season,scoring_version,resource);
 END IF;
END $$;

DO $$ DECLARE existing_name text; definition text;
BEGIN
 SELECT conname,pg_get_constraintdef(oid) INTO existing_name,definition
 FROM pg_constraint WHERE conrelid='public.team_lineup_snapshots'::regclass AND contype='p';
 IF existing_name IS NULL OR position('competition_id' in definition)=0 THEN
  IF existing_name IS NOT NULL THEN EXECUTE format('ALTER TABLE public.team_lineup_snapshots DROP CONSTRAINT %I',existing_name); END IF;
  ALTER TABLE public.team_lineup_snapshots ADD CONSTRAINT team_lineup_snapshots_pkey PRIMARY KEY (account_id,championship_id,competition_id,user_team_id,target_round,season,scoring_version);
 END IF;
END $$;

DO $$ DECLARE item record;
BEGIN
 FOR item IN SELECT conname,conrelid::regclass AS tbl FROM pg_constraint
  WHERE (conrelid='public.player_daily_snapshots'::regclass AND contype='u' AND
         pg_get_constraintdef(oid)='UNIQUE (player_id, championship_id, snapshot_date)') OR
        (conrelid='public.round_dream_team'::regclass AND contype='u' AND
         pg_get_constraintdef(oid)='UNIQUE (championship_id, round_number, player_id)') LOOP
  EXECUTE format('ALTER TABLE %s DROP CONSTRAINT %I',item.tbl,item.conname);
 END LOOP;
 IF NOT EXISTS(SELECT 1 FROM pg_constraint WHERE conrelid='public.player_daily_snapshots'::regclass AND conname='player_daily_snapshots_context_date_key') THEN
  ALTER TABLE public.player_daily_snapshots ADD CONSTRAINT player_daily_snapshots_context_date_key
   UNIQUE(player_id,championship_id,competition_id,season,snapshot_date);
 END IF;
 IF NOT EXISTS(SELECT 1 FROM pg_constraint WHERE conrelid='public.round_dream_team'::regclass AND conname='round_dream_team_context_round_key') THEN
  ALTER TABLE public.round_dream_team ADD CONSTRAINT round_dream_team_context_round_key
   UNIQUE(championship_id,competition_id,season,scoring_version,round_id,player_id);
 END IF;
END $$;
-- Source observation time belongs to the full league context. Preserve the
-- original event IDs and source-event uniqueness of market transactions.
DROP INDEX IF EXISTS public.player_history_source_time;
CREATE UNIQUE INDEX player_history_source_time ON public.player_history(player_id,championship_id,competition_id,season,observed_at);
DROP INDEX IF EXISTS public.user_team_history_source_time;
CREATE UNIQUE INDEX user_team_history_source_time ON public.user_team_history(user_team_id,championship_id,competition_id,season,observed_at);

CREATE OR REPLACE VIEW public.player_round_points_latest
WITH (security_invoker = true) AS
SELECT DISTINCT ON (championship_id, competition_id, player_id, season, scoring_version, round, source_round_key)
  championship_id, player_id, season, scoring_version, round, points,
  score_status, observed_at, occurred_at, round_start_at,
  competition_id, source_round_key, round_id, match_id, source_round_number
FROM public.player_match_observations
ORDER BY championship_id, competition_id, player_id, season, scoring_version, round, source_round_key, observed_at DESC;
REVOKE ALL ON public.player_round_points_latest FROM PUBLIC, anon, authenticated;
GRANT SELECT ON public.player_round_points_latest TO service_role;
CREATE OR REPLACE VIEW public.player_round_dataset
WITH (security_invoker = true) AS
SELECT 'point'::text AS row_kind,
 championship_id, player_id, season, scoring_version, round, points,
 score_status, observed_at, occurred_at, round_start_at,
 NULL::timestamptz AS checked_at, NULL::text AS result_status, NULL::integer AS point_count,
 competition_id, source_round_key, round_id, match_id, source_round_number
FROM public.player_round_points_latest
UNION ALL
SELECT 'check'::text AS row_kind,
 championship_id, player_id, season, scoring_version, NULL::integer AS round, NULL::numeric AS points,
 NULL::text AS score_status, NULL::timestamptz AS observed_at,
 NULL::timestamptz AS occurred_at, NULL::timestamptz AS round_start_at,
 checked_at, result_status, point_count,
 competition_id, NULL::text AS source_round_key, NULL::text AS round_id, NULL::text AS match_id, NULL::numeric AS source_round_number
FROM (
 SELECT DISTINCT ON (championship_id, competition_id, player_id, season, scoring_version)
 championship_id, player_id, season, scoring_version, checked_at, result_status, point_count, competition_id
 FROM public.player_round_history_checks
 ORDER BY championship_id, competition_id, player_id, season, scoring_version, checked_at DESC
) checks;
REVOKE ALL ON public.player_round_dataset FROM PUBLIC, anon, authenticated;
GRANT SELECT ON public.player_round_dataset TO service_role;
CREATE OR REPLACE VIEW public.rival_finance_evidence_latest WITH (security_invoker = true) AS
 SELECT DISTINCT ON (account_id, championship_id, competition_id, season, evidence_type, source_id)
 account_id, championship_id, season, evidence_type, source_id, observed_at, payload, competition_id
 FROM public.rival_finance_evidence
 ORDER BY account_id, championship_id, competition_id, season, evidence_type, source_id, observed_at DESC;
REVOKE ALL ON public.rival_finance_evidence_latest FROM PUBLIC, anon, authenticated;
GRANT SELECT ON public.rival_finance_evidence_latest TO service_role;

-- Normalized, account-scoped source context. This is metadata, not permission
-- to access a championship: the R server verifies current account membership.
CREATE TABLE IF NOT EXISTS public.league_context_observations (
 account_id text NOT NULL CHECK (length(account_id) BETWEEN 1 AND 256),
 championship_id text NOT NULL CHECK (length(championship_id) BETWEEN 1 AND 256),
 competition_id text NOT NULL CHECK (length(competition_id) BETWEEN 1 AND 256),
 season text NOT NULL CHECK (length(season) BETWEEN 1 AND 256),
 scoring_version text NOT NULL CHECK (length(scoring_version) BETWEEN 1 AND 256),
 observed_at timestamptz NOT NULL CHECK (isfinite(observed_at)),
 expires_at timestamptz NOT NULL CHECK (isfinite(expires_at) AND expires_at >= observed_at),
 complete boolean NOT NULL,
 payload jsonb NOT NULL CHECK (jsonb_typeof(payload) = 'object' AND octet_length(payload::text) <= 1048576),
 PRIMARY KEY (account_id, championship_id, competition_id, season, scoring_version, observed_at)
);
CREATE INDEX IF NOT EXISTS league_context_latest
 ON public.league_context_observations(account_id, championship_id, observed_at DESC);

-- Source player IDs are opaque records, not permanent people. Multiple real
-- competitions may observe the same source player simultaneously. Source
-- effective dates are optional; first observation is never a transfer date.
CREATE TABLE IF NOT EXISTS public.player_membership_observations (
 player_id text NOT NULL CHECK (length(player_id) BETWEEN 1 AND 256),
 competition_id text NOT NULL CHECK (length(competition_id) BETWEEN 1 AND 256),
 season text NOT NULL CHECK (length(season) BETWEEN 1 AND 256),
 real_club_id text CHECK (real_club_id IS NULL OR length(real_club_id) BETWEEN 1 AND 256),
 observed_at timestamptz NOT NULL CHECK (isfinite(observed_at)),
 effective_at timestamptz CHECK (effective_at IS NULL OR isfinite(effective_at)),
 source text NOT NULL CHECK (length(source) BETWEEN 1 AND 128),
 UNIQUE NULLS NOT DISTINCT (player_id, competition_id, season, observed_at, real_club_id)
);
CREATE INDEX IF NOT EXISTS player_membership_context_time
 ON public.player_membership_observations(competition_id, season, player_id, observed_at DESC);

DO $$ DECLARE tbl text;
BEGIN
 FOREACH tbl IN ARRAY ARRAY['league_context_observations','player_membership_observations'] LOOP
  EXECUTE format('ALTER TABLE public.%I ENABLE ROW LEVEL SECURITY',tbl);
  EXECUTE format('REVOKE ALL ON public.%I FROM PUBLIC, anon, authenticated, service_role',tbl);
  EXECUTE format('GRANT SELECT, INSERT ON public.%I TO service_role',tbl);
 END LOOP;
END $$;
COMMENT ON TABLE public.league_context_observations IS
 'Immutable normalized context/configuration evidence. No authentication material. Cached metadata never authorizes league membership.';
COMMENT ON TABLE public.player_membership_observations IS
 'Append-only source player/competition/season club observations. No inferred person aliases, transfer dates, or retirement from catalog disappearance.';

CREATE INDEX IF NOT EXISTS player_round_league_context_revision
 ON public.player_match_observations(championship_id,competition_id,season,scoring_version,player_id,round,source_round_key,observed_at DESC);
CREATE INDEX IF NOT EXISTS player_round_checks_league_context
 ON public.player_round_history_checks(championship_id,competition_id,season,scoring_version,player_id,checked_at DESC);
CREATE INDEX IF NOT EXISTS player_history_full_context_page
 ON public.player_history(championship_id,competition_id,season,player_id,recorded_at,id);
CREATE INDEX IF NOT EXISTS team_history_full_context_page
 ON public.user_team_history(championship_id,competition_id,season,recorded_at,id);

-- Validate only incoming writes. Existing rows are neither deleted nor
-- relabeled; any legacy inconsistency remains available for reconciliation.
CREATE OR REPLACE FUNCTION public.guard_team_linked_league()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
DECLARE ids text[]; tid text; owner_championship text;
BEGIN
 IF NEW.championship_id IS NULL OR btrim(NEW.championship_id) = '' THEN
  RAISE EXCEPTION 'Team-linked observations require a championship' USING ERRCODE='23514';
 END IF;
 IF TG_TABLE_NAME = 'market_transactions' THEN
  -- The Futmondo System/Market has no team ID. Empty IDs have this same meaning.
  NEW.buyer_team_id := NULLIF(btrim(NEW.buyer_team_id),'');
  NEW.seller_team_id := NULLIF(btrim(NEW.seller_team_id),'');
  ids := ARRAY[NEW.buyer_team_id,NEW.seller_team_id];
 ELSIF TG_TABLE_NAME = 'player_daily_snapshots' THEN
  NEW.owner_team_id := NULLIF(btrim(NEW.owner_team_id),'');
  ids := ARRAY[NEW.owner_team_id];
 ELSE
  RAISE EXCEPTION 'Unsupported league ownership guard' USING ERRCODE='23514';
 END IF;
 FOREACH tid IN ARRAY ids LOOP
  IF tid IS NULL THEN CONTINUE; END IF;
  SELECT championship_id INTO owner_championship FROM public.user_teams WHERE id=tid;
  IF owner_championship IS NULL OR owner_championship IS DISTINCT FROM NEW.championship_id THEN
   RAISE EXCEPTION 'Counterparty does not belong to observation championship' USING ERRCODE='23514';
  END IF;
 END LOOP;
 RETURN NEW;
END;
$$;
REVOKE ALL ON FUNCTION public.guard_team_linked_league() FROM PUBLIC, anon, authenticated;
GRANT EXECUTE ON FUNCTION public.guard_team_linked_league() TO service_role;
DROP TRIGGER IF EXISTS guard_market_transaction_league ON public.market_transactions;
CREATE TRIGGER guard_market_transaction_league BEFORE INSERT OR UPDATE ON public.market_transactions
 FOR EACH ROW EXECUTE FUNCTION public.guard_team_linked_league();
DROP TRIGGER IF EXISTS guard_daily_snapshot_league ON public.player_daily_snapshots;
CREATE TRIGGER guard_daily_snapshot_league BEFORE INSERT OR UPDATE ON public.player_daily_snapshots
 FOR EACH ROW EXECUTE FUNCTION public.guard_team_linked_league();

CREATE OR REPLACE FUNCTION public.keep_user_team_league_identity()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
BEGIN
 IF OLD.championship_id IS NOT NULL AND NEW.championship_id IS DISTINCT FROM OLD.championship_id THEN
  RAISE EXCEPTION 'An existing source team cannot change championship ownership' USING ERRCODE='23514';
 END IF;
 RETURN NEW;
END;
$$;
REVOKE ALL ON FUNCTION public.keep_user_team_league_identity() FROM PUBLIC, anon, authenticated;
GRANT EXECUTE ON FUNCTION public.keep_user_team_league_identity() TO service_role;
DROP TRIGGER IF EXISTS keep_user_team_league_identity ON public.user_teams;
CREATE TRIGGER keep_user_team_league_identity BEFORE UPDATE ON public.user_teams
 FOR EACH ROW EXECUTE FUNCTION public.keep_user_team_league_identity();

NOTIFY pgrst, 'reload schema';
COMMIT;
