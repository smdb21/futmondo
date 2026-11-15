-- Explicitly authorized, one-shot clause purchases. Independent of factual-sync consent.
BEGIN;
CREATE TABLE IF NOT EXISTS public.clause_authorizations (
 account_id text PRIMARY KEY, encrypted_token text, credential_id text,
 generation bigint NOT NULL DEFAULT 1, enabled boolean NOT NULL DEFAULT true,
 auth_status text NOT NULL DEFAULT 'active' CHECK(auth_status IN ('active','reauthorize','revoked')),
 updated_at timestamptz NOT NULL DEFAULT now(), rate_limited_until timestamptz,
 CHECK(octet_length(encrypted_token)<=16384)
);
CREATE TABLE IF NOT EXISTS public.clause_jobs (
 id text PRIMARY KEY CHECK(id ~ '^[a-f0-9]{32}$'), account_id text NOT NULL,
 context jsonb NOT NULL, kind text NOT NULL CHECK(kind IN ('scheduled','manual')),
 mode text NOT NULL CHECK(mode IN ('bid','offer','modify','clause')),
 player_id text NOT NULL, player_slug text NOT NULL, player_name text NOT NULL,
 original_owner text NOT NULL, max_price numeric NOT NULL CHECK(max_price>0),
 reserved_amount numeric NOT NULL CHECK(reserved_amount>=0), reserved_slot integer NOT NULL CHECK(reserved_slot IN (0,1)),
 opens_at timestamptz NOT NULL, due_at timestamptz NOT NULL, expires_at timestamptz NOT NULL,
 state text NOT NULL DEFAULT 'scheduled' CHECK(state IN ('scheduled','preparing','dispatched','uncertain','accepted','succeeded','failed','cancelled','expired')),
 result_code text NOT NULL DEFAULT '', actual_price numeric, consent_version integer NOT NULL DEFAULT 1,
 attempts integer NOT NULL DEFAULT 0,reconciliation_attempts integer NOT NULL DEFAULT 0, owner text, lease_until timestamptz,
 dispatched_at timestamptz, finished_at timestamptz, acknowledged_at timestamptz,
 created_at timestamptz NOT NULL DEFAULT now(), updated_at timestamptz NOT NULL DEFAULT now(),player_observed_at timestamptz NOT NULL DEFAULT now(),
 CHECK(context->>'account_id'=account_id), CHECK(context->>'player_id'=player_id),
 CHECK(octet_length(context::text)<=4096), CHECK(length(player_name)<=256), CHECK(length(player_slug)<=256),
 CHECK(result_code ~ '^[a-zA-Z0-9_.:]{0,160}$')
);
CREATE UNIQUE INDEX IF NOT EXISTS clause_jobs_active_player ON public.clause_jobs
 (account_id,(context->>'championship_id'),(context->>'season'),player_id)
 WHERE kind='scheduled' AND state IN ('scheduled','preparing','dispatched','uncertain','accepted');
CREATE INDEX IF NOT EXISTS clause_jobs_deadline ON public.clause_jobs(due_at)
 WHERE state IN ('scheduled','preparing','dispatched','uncertain','accepted');
CREATE INDEX IF NOT EXISTS clause_jobs_expiry ON public.clause_jobs(expires_at) WHERE kind='scheduled' AND state IN ('scheduled','preparing');
CREATE INDEX IF NOT EXISTS clause_jobs_account ON public.clause_jobs(account_id,created_at DESC);
CREATE INDEX IF NOT EXISTS clause_jobs_account_active ON public.clause_jobs(account_id) WHERE state IN ('scheduled','preparing','dispatched','uncertain','accepted');
CREATE INDEX IF NOT EXISTS clause_jobs_recovery ON public.clause_jobs(lease_until) WHERE state IN ('dispatched','preparing');
CREATE TABLE IF NOT EXISTS public.clause_attempts (
 job_id text NOT NULL REFERENCES public.clause_jobs(id), attempt integer NOT NULL,
 started_at timestamptz NOT NULL DEFAULT now(), finished_at timestamptz, result_code text,
 PRIMARY KEY(job_id,attempt), CHECK(result_code ~ '^[a-zA-Z0-9_.:]{0,160}$')
);
CREATE TABLE IF NOT EXISTS public.clause_evidence (
 account_id text NOT NULL, evidence_key text NOT NULL CHECK(evidence_key ~ '^[a-f0-9]{64}$'),
 context jsonb NOT NULL, payload jsonb NOT NULL,payload_bytes integer NOT NULL CHECK(payload_bytes BETWEEN 1 AND 16777216), observed_at timestamptz NOT NULL,
 valid_until timestamptz NOT NULL, complete boolean NOT NULL,
 PRIMARY KEY(account_id,evidence_key), CHECK(octet_length(payload::text)<=16777216)
);
CREATE OR REPLACE FUNCTION public.clause_team_lock(p_context jsonb) RETURNS void
 LANGUAGE sql SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT pg_advisory_xact_lock(hashtextextended(concat_ws('|',p_context->>'account_id',p_context->>'championship_id',p_context->>'user_team_id',p_context->>'season'),0));
$$;
CREATE OR REPLACE FUNCTION public.clause_same_team(a jsonb,b jsonb) RETURNS boolean
 LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT a->>'account_id'=b->>'account_id' AND a->>'championship_id'=b->>'championship_id'
 AND a->>'user_team_id'=b->>'user_team_id' AND a->>'competition_id'=b->>'competition_id'
 AND a->>'season'=b->>'season' AND a->>'scoring_version'=b->>'scoring_version';
$$;
CREATE OR REPLACE FUNCTION public.clause_check_facts(p_context jsonb,p_facts jsonb) RETURNS void
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE f text;
BEGIN
 FOREACH f IN ARRAY ARRAY['account_id','championship_id','user_team_id','competition_id','season','scoring_version'] LOOP
  IF coalesce(p_context->>f,'') IN ('','unknown') THEN RAISE EXCEPTION 'clause_context_unverified'; END IF;
 END LOOP;
 IF p_facts->>'status' IS DISTINCT FROM 'ok' OR p_facts->'roster'->>'status' IS DISTINCT FROM 'ok'
 OR p_facts->>'valid_until' IS NULL OR p_facts->>'checked_at' IS NULL OR p_facts->>'financial_observed_at' IS NULL
 OR (p_facts->>'valid_until')::timestamptz>now()+interval '16 seconds' OR (p_facts->>'financial_observed_at')::timestamptz>now()+interval '2 seconds'
 OR (p_facts->>'valid_until')::timestamptz<=now() OR (p_facts->>'checked_at')::timestamptz<now()-interval '30 seconds'
 OR (p_facts->>'checked_at')::timestamptz>now()+interval '2 seconds'
 OR NOT clause_same_team(p_context,p_facts->'context')
 THEN RAISE EXCEPTION 'clause_facts_unverified'; END IF;
 IF (p_facts->'funds'->>'spendable_budget')::numeric IS NULL
 OR (p_facts->'funds'->>'api_bid_limit')::numeric IS NULL
 OR (p_facts->'roster'->>'effective_count')::integer IS NULL
 OR (p_facts->'roster'->>'cap')::integer IS NULL
 OR (p_facts->'outstanding'->>'count')::integer IS NULL
 OR (p_facts->'funds'->>'spendable_budget')::numeric<0 OR (p_facts->'funds'->>'spendable_budget')::numeric>1e16
 OR (p_facts->'funds'->>'api_bid_limit')::numeric<0 OR (p_facts->'funds'->>'api_bid_limit')::numeric>1e16
 OR (p_facts->'outstanding'->>'count')::integer<0 OR (p_facts->'roster'->>'effective_count')::integer<0
 OR (p_facts->'roster'->>'cap')::integer<1
 OR jsonb_typeof(p_facts->'offers') IS DISTINCT FROM 'array'
 THEN RAISE EXCEPTION 'clause_facts_incomplete'; END IF;
END $$;
-- Called only with verified current source facts. Accepted purchases retain funds
-- until both financial checks and the corresponding source identity are current.
CREATE OR REPLACE FUNCTION public.clause_reconcile_reservations(p_context jsonb,p_facts jsonb) RETURNS void
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 PERFORM clause_check_facts(p_context,p_facts);PERFORM clause_team_lock(p_context);
 UPDATE clause_jobs j SET state='succeeded',reserved_amount=0,reserved_slot=0,
  finished_at=coalesce(finished_at,now()),updated_at=now(),owner=NULL,lease_until=NULL
 WHERE j.account_id=p_context->>'account_id' AND clause_same_team(j.context,p_context)
 AND j.state IN ('accepted','uncertain','dispatched') AND j.dispatched_at IS NOT NULL
 AND (p_facts->>'financial_observed_at')::timestamptz>j.dispatched_at
 AND (CASE WHEN j.mode IN ('bid','offer','modify') THEN EXISTS(
  SELECT 1 FROM jsonb_array_elements(p_facts->'offers') o WHERE o->>'player_id'=j.player_id AND (o->>'price')::numeric=j.max_price)
 ELSE EXISTS(SELECT 1 FROM jsonb_array_elements(coalesce(p_facts->'transactions','[]')) tx
  WHERE tx->>'player_id'=j.player_id AND tx->>'buyer_team_id'=p_context->>'user_team_id'
  AND (tx->>'created')::timestamptz>=j.dispatched_at-interval '2 seconds'
  AND (tx->>'price')::numeric=coalesce(j.actual_price,j.max_price)) END);
 UPDATE clause_jobs j SET state='cancelled',result_code='another_buyer_confirmed',reserved_amount=0,reserved_slot=0,
  finished_at=now(),updated_at=now(),owner=NULL,lease_until=NULL
 WHERE j.account_id=p_context->>'account_id' AND clause_same_team(j.context,p_context)
 AND j.state IN ('scheduled','preparing','uncertain') AND EXISTS(
  SELECT 1 FROM jsonb_array_elements(coalesce(p_facts->'transactions','[]')) tx WHERE tx->>'player_id'=j.player_id
  AND coalesce(tx->>'buyer_team_id','')<>p_context->>'user_team_id'
  AND (tx->>'created')::timestamptz>j.created_at);
 PERFORM clause_forget_if_idle(p_context->>'account_id');
END $$;
CREATE OR REPLACE FUNCTION public.clause_admit(p_id text,p_context jsonb,p_kind text,p_mode text,p_player jsonb,
 p_price numeric,p_facts jsonb,p_token text DEFAULT NULL,p_credential text DEFAULT NULL) RETURNS jsonb
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j clause_jobs; money numeric; slots integer; delta numeric; existing numeric:=0; opening timestamptz;
BEGIN
 PERFORM clause_check_facts(p_context,p_facts);PERFORM clause_team_lock(p_context);
 PERFORM clause_reconcile_reservations(p_context,p_facts);
 IF p_id !~ '^[a-f0-9]{32}$' OR p_kind NOT IN ('scheduled','manual') OR p_mode NOT IN ('bid','offer','modify','clause')
 OR p_price IS NULL OR p_price<=0 OR p_price>1e16 OR p_context->>'player_id' IS DISTINCT FROM p_player->>'id'
 OR p_context->>'player_id' IS DISTINCT FROM p_facts->'player'->>'id'
 THEN RAISE EXCEPTION 'clause_invalid_request'; END IF;
 SELECT * INTO j FROM clause_jobs WHERE id=p_id;
 IF FOUND THEN
  IF j.context IS DISTINCT FROM p_context OR j.max_price<>p_price OR j.kind<>p_kind THEN RAISE EXCEPTION 'clause_request_conflict'; END IF;
  RETURN to_jsonb(j);
 END IF;
 IF (SELECT count(*) FROM clause_jobs WHERE account_id=p_context->>'account_id' AND state IN ('scheduled','preparing','dispatched','uncertain','accepted'))>=256 THEN RAISE EXCEPTION 'clause_queue_capacity'; END IF;
 IF EXISTS(SELECT 1 FROM clause_jobs WHERE account_id=p_context->>'account_id' AND clause_same_team(context,p_context)
 AND state IN ('preparing','dispatched','uncertain')) THEN RAISE EXCEPTION 'clause_team_busy'; END IF;
 IF p_kind='scheduled' THEN
  IF p_mode<>'clause' OR coalesce(p_facts->'player'->>'slug','')='' OR p_token IS NULL OR p_credential IS NULL OR length(p_token)>16384
  OR coalesce(p_player->>'owner_id','')='' OR p_player->>'owner_id'=p_context->>'user_team_id'
  OR p_player->>'owner_id' IS DISTINCT FROM p_facts->'player'->>'owner_id'
  OR p_facts->'player'->>'owner_verified' IS DISTINCT FROM 'true'
  OR p_facts->'player'->>'transferred' IS DISTINCT FROM 'false'
  OR (p_facts->'player'->>'price')::numeric>p_price
  THEN RAISE EXCEPTION 'clause_player_changed'; END IF;
  opening:=(p_facts->'player'->>'opens_at')::timestamptz;
  IF opening IS NULL OR opening<=now() THEN RAISE EXCEPTION 'clause_not_future'; END IF;
 ELSE opening:=now(); END IF;
 SELECT coalesce(sum(reserved_amount),0),coalesce(sum(reserved_slot),0) INTO money,slots FROM clause_jobs
 WHERE account_id=p_context->>'account_id' AND clause_same_team(context,p_context)
 AND state IN ('scheduled','preparing','dispatched','uncertain','accepted');
 IF p_mode='modify' THEN
  SELECT (o->>'price')::numeric INTO existing FROM jsonb_array_elements(p_facts->'offers') o WHERE o->>'player_id'=p_context->>'player_id' LIMIT 1;
  IF existing IS NULL THEN RAISE EXCEPTION 'clause_existing_offer_unverified'; END IF;
 END IF;
 delta:=greatest(0,p_price-existing);
 IF (p_facts->'roster'->>'effective_count')::integer+(p_facts->'outstanding'->>'count')::integer+slots
   >=(p_facts->'roster'->>'cap')::integer AND p_mode<>'modify' THEN RAISE EXCEPTION 'clause_offer_capacity'; END IF;
 IF delta+money>(p_facts->'funds'->>'spendable_budget')::numeric OR p_price>(p_facts->'funds'->>'api_bid_limit')::numeric
 THEN RAISE EXCEPTION 'clause_offer_funds'; END IF;
 IF p_kind='scheduled' THEN
  INSERT INTO clause_authorizations(account_id,encrypted_token,credential_id) VALUES(p_context->>'account_id',p_token,p_credential)
  ON CONFLICT(account_id) DO UPDATE SET encrypted_token=excluded.encrypted_token,credential_id=excluded.credential_id,
    enabled=true,auth_status='active',generation=clause_authorizations.generation+1,updated_at=now();
 END IF;
 INSERT INTO clause_jobs(id,account_id,context,kind,mode,player_id,player_slug,player_name,original_owner,max_price,reserved_amount,reserved_slot,
  opens_at,due_at,expires_at,state)
 VALUES(p_id,p_context->>'account_id',p_context,p_kind,p_mode,p_player->>'id',coalesce(p_facts->'player'->>'slug',''),left(coalesce(p_player->>'name','Player'),256),
 coalesce(p_player->>'owner_id',''),p_price,delta,CASE WHEN p_mode='modify' THEN 0 ELSE 1 END,
 opening,CASE WHEN p_kind='scheduled' THEN opening-interval '10 seconds' ELSE now() END,opening+interval '5 minutes',
 CASE WHEN p_kind='scheduled' THEN 'scheduled' ELSE 'preparing' END) RETURNING * INTO j;
 IF p_kind='manual' THEN UPDATE clause_jobs SET lease_until=now()+interval '60 seconds' WHERE id=p_id RETURNING * INTO j; END IF;
 RETURN to_jsonb(j);
END $$;
CREATE OR REPLACE FUNCTION public.clause_read(p_account text) RETURNS jsonb
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE clause_jobs SET state='expired',result_code='opening_window_expired',reserved_amount=0,reserved_slot=0,
 finished_at=now(),updated_at=now(),owner=NULL,lease_until=NULL
 WHERE account_id=p_account AND kind='scheduled' AND state IN ('scheduled','preparing') AND expires_at<now();
 PERFORM clause_forget_if_idle(p_account);
 RETURN jsonb_build_object('jobs',coalesce((SELECT jsonb_agg(to_jsonb(j)-'owner'-'lease_until' ORDER BY created_at) FROM (
 SELECT * FROM clause_jobs WHERE account_id=p_account AND state IN ('scheduled','preparing','dispatched','uncertain','accepted')
 UNION ALL
 (SELECT * FROM clause_jobs WHERE account_id=p_account AND kind='scheduled' AND acknowledged_at IS NULL
 AND state NOT IN ('scheduled','preparing','dispatched','uncertain','accepted') ORDER BY finished_at DESC LIMIT 100)
 UNION ALL
 (SELECT * FROM clause_jobs WHERE account_id=p_account AND kind='scheduled' AND acknowledged_at IS NOT NULL
 AND state='succeeded' AND finished_at>now()-interval '7 days' ORDER BY finished_at DESC LIMIT 100)
 ) j),'[]'::jsonb),
 'more_outcomes',(SELECT count(*)>100 FROM clause_jobs WHERE account_id=p_account AND kind='scheduled' AND acknowledged_at IS NULL
 AND state NOT IN ('scheduled','preparing','dispatched','uncertain','accepted')),
 'health',(SELECT to_jsonb(h) FROM worker_health h WHERE status='running' ORDER BY heartbeat DESC LIMIT 1));
END $$;
CREATE OR REPLACE FUNCTION public.clause_cancel(p_account text,p_id text) RETURNS boolean
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE ctx jsonb;changed boolean;
BEGIN
 SELECT context INTO ctx FROM clause_jobs WHERE id=p_id AND account_id=p_account;PERFORM clause_team_lock(ctx);
 UPDATE clause_jobs SET state='cancelled',result_code='user_cancelled',reserved_amount=0,reserved_slot=0,finished_at=now(),updated_at=now()
 WHERE id=p_id AND account_id=p_account AND state IN ('scheduled','preparing');
 changed:=FOUND;PERFORM clause_forget_if_idle(p_account);RETURN changed;
END $$;
CREATE OR REPLACE FUNCTION public.clause_ack(p_account text,p_id text) RETURNS boolean
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE clause_jobs SET acknowledged_at=now() WHERE id=p_id AND account_id=p_account AND kind='scheduled'
 AND state IN ('accepted','succeeded','failed','cancelled','expired','uncertain');RETURN FOUND;
END $$;
CREATE OR REPLACE FUNCTION public.clause_renew(p_account text,p_token text,p_credential text) RETURNS boolean
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE clause_authorizations SET encrypted_token=p_token,credential_id=p_credential,auth_status='active',updated_at=now(),generation=generation+1
 WHERE account_id=p_account AND (enabled OR EXISTS(SELECT 1 FROM clause_jobs WHERE account_id=p_account AND state IN ('dispatched','uncertain','accepted'))) AND EXISTS(SELECT 1 FROM clause_jobs WHERE account_id=p_account
 AND state IN ('scheduled','preparing','dispatched','uncertain','accepted'));RETURN FOUND;
END $$;
CREATE OR REPLACE FUNCTION public.clause_revoke(p_account text) RETURNS void
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE clause_authorizations SET enabled=false,auth_status=CASE WHEN EXISTS(SELECT 1 FROM clause_jobs WHERE account_id=p_account AND state IN ('dispatched','uncertain','accepted')) THEN 'active' ELSE 'revoked' END,
 encrypted_token=CASE WHEN EXISTS(SELECT 1 FROM clause_jobs WHERE account_id=p_account AND state IN ('dispatched','uncertain','accepted')) THEN encrypted_token ELSE NULL END,generation=generation+1 WHERE account_id=p_account;
 UPDATE clause_jobs SET state='cancelled',result_code='authorization_revoked',reserved_amount=0,reserved_slot=0,finished_at=now(),updated_at=now()
 WHERE account_id=p_account AND kind='scheduled' AND state IN ('scheduled','preparing');
END $$;
CREATE OR REPLACE FUNCTION public.clause_guard(p_id text,p_owner text) RETURNS boolean
 LANGUAGE sql SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT EXISTS(SELECT 1 FROM clause_jobs j JOIN clause_authorizations a USING(account_id)
 WHERE j.id=p_id AND j.owner=p_owner AND j.lease_until>now() AND j.state IN ('preparing','dispatched','uncertain','accepted')
 AND (a.enabled OR j.state IN ('dispatched','uncertain','accepted')) AND a.auth_status='active' AND a.encrypted_token IS NOT NULL
 AND EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()));
$$;
CREATE OR REPLACE FUNCTION public.clause_claim(p_owner text) RETURNS jsonb
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j clause_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 UPDATE clause_jobs SET state='failed',result_code='dispatch_not_sent',reserved_amount=0,reserved_slot=0,finished_at=now()
 WHERE kind='manual' AND state='preparing' AND lease_until<now();
 -- A lease loss after dispatch authorizes reconciliation only, never replay.
 UPDATE clause_jobs SET state='uncertain',result_code='dispatch_interrupted',due_at=now(),owner=NULL,lease_until=NULL,updated_at=now()
 WHERE state='dispatched' AND lease_until<now();
 SELECT q.* INTO j FROM clause_jobs q JOIN clause_authorizations a USING(account_id)
 WHERE q.kind='scheduled' AND q.state IN ('scheduled','preparing','uncertain','accepted') AND q.due_at<=now()
 AND (q.lease_until IS NULL OR q.lease_until<now()) AND (a.enabled OR q.state IN ('dispatched','uncertain','accepted')) AND a.auth_status='active' AND a.encrypted_token IS NOT NULL
 ORDER BY q.due_at LIMIT 1 FOR UPDATE OF q SKIP LOCKED;
 IF j.id IS NULL THEN RETURN NULL; END IF;
 PERFORM clause_team_lock(j.context);
 IF EXISTS(SELECT 1 FROM clause_jobs x WHERE x.id<>j.id AND clause_same_team(x.context,j.context)
 AND x.state IN ('preparing','dispatched') AND x.lease_until>now()) THEN RETURN NULL; END IF;
 UPDATE clause_jobs SET state=CASE WHEN state='scheduled' THEN 'preparing' ELSE state END,
 reconciliation_attempts=CASE WHEN state IN ('uncertain','accepted') THEN reconciliation_attempts+1 ELSE reconciliation_attempts END,
 owner=p_owner,lease_until=now()+interval '60 seconds',updated_at=now() WHERE id=j.id RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
CREATE OR REPLACE FUNCTION public.clause_touch(p_id text,p_owner text) RETURNS boolean
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 IF NOT clause_guard(p_id,p_owner) THEN RETURN false; END IF;
 UPDATE clause_jobs SET lease_until=now()+interval '60 seconds' WHERE id=p_id;RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.clause_dispatch(p_id text,p_owner text,p_facts jsonb) RETURNS boolean
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j clause_jobs; money numeric; slots integer; spend numeric;
BEGIN
 SELECT * INTO j FROM clause_jobs WHERE id=p_id;PERFORM clause_team_lock(j.context);
 IF j.kind='scheduled' AND (NOT clause_guard(p_id,p_owner) OR NOT EXISTS(SELECT 1 FROM clause_authorizations WHERE account_id=j.account_id AND enabled)) THEN RETURN false; END IF;
 IF j.state<>'preparing' OR j.expires_at<now() THEN RETURN false; END IF;
 PERFORM clause_check_facts(j.context,p_facts);PERFORM clause_reconcile_reservations(j.context,p_facts);
 SELECT coalesce(sum(reserved_amount),0),coalesce(sum(reserved_slot),0) INTO money,slots FROM clause_jobs
 WHERE id<>p_id AND account_id=j.account_id AND clause_same_team(context,j.context)
 AND state IN ('scheduled','preparing','dispatched','uncertain','accepted');
 spend:=CASE WHEN j.kind='scheduled' THEN (p_facts->'player'->>'price')::numeric ELSE j.reserved_amount END;
 IF spend IS NULL OR spend<=0 AND j.mode<>'modify' THEN RETURN false; END IF;
 IF (j.mode<>'modify' AND (p_facts->'roster'->>'effective_count')::integer+(p_facts->'outstanding'->>'count')::integer+slots+j.reserved_slot
  >(p_facts->'roster'->>'cap')::integer)
 OR (spend>0 AND spend+money>(p_facts->'funds'->>'spendable_budget')::numeric)
 OR (CASE WHEN j.kind='scheduled' THEN spend ELSE j.max_price END)>(p_facts->'funds'->>'api_bid_limit')::numeric THEN RETURN false; END IF;
 IF j.kind='scheduled' AND (j.opens_at>now() OR p_facts->'player'->>'owner_id' IS DISTINCT FROM j.original_owner
 OR p_facts->'player'->>'transferred' IS DISTINCT FROM 'false'
 OR (p_facts->'player'->>'price')::numeric IS NULL OR (p_facts->'player'->>'price')::numeric>j.max_price
 OR (p_facts->'player'->>'opens_at')::timestamptz>now()) THEN RETURN false; END IF;
 UPDATE clause_jobs SET state='dispatched',dispatched_at=now(),attempts=attempts+1,
 actual_price=CASE WHEN kind='scheduled' THEN (p_facts->'player'->>'price')::numeric ELSE max_price END,
 lease_until=now()+interval '60 seconds',updated_at=now() WHERE id=p_id AND state='preparing';
 IF NOT FOUND THEN RETURN false; END IF;
 INSERT INTO clause_attempts(job_id,attempt) SELECT id,attempts FROM clause_jobs WHERE id=p_id;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.clause_settle(p_id text,p_owner text,p_state text,p_code text,p_due timestamptz DEFAULT NULL,p_open timestamptz DEFAULT NULL) RETURNS boolean
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j clause_jobs;
BEGIN
 SELECT * INTO j FROM clause_jobs WHERE id=p_id FOR UPDATE;
 IF j.kind='scheduled' AND j.owner IS DISTINCT FROM p_owner THEN RETURN false; END IF;
 IF j.state NOT IN ('preparing','dispatched','uncertain','accepted') OR p_state NOT IN ('scheduled','uncertain','accepted','succeeded','failed','cancelled','expired')
 OR p_code !~ '^[a-zA-Z0-9_.:]{0,160}$' THEN RETURN false; END IF;
 -- Only a proven rejection may return a dispatched job to the queue.
 IF j.state='dispatched' AND p_state='scheduled' AND p_code NOT LIKE 'closed_verified:%' THEN RETURN false; END IF;
 UPDATE clause_attempts SET result_code=p_code,finished_at=now() WHERE job_id=p_id AND attempt=j.attempts AND finished_at IS NULL;
 UPDATE clause_jobs SET state=p_state,result_code=p_code,owner=NULL,lease_until=NULL,
 due_at=coalesce(p_due,now()+interval '30 seconds'),opens_at=coalesce(p_open,opens_at),
 expires_at=CASE WHEN p_open IS NULL THEN expires_at ELSE p_open+interval '5 minutes' END,
 reserved_amount=CASE WHEN p_state IN ('succeeded','failed','cancelled','expired') THEN 0 ELSE reserved_amount END,
 reserved_slot=CASE WHEN p_state IN ('succeeded','failed','cancelled','expired') THEN 0 ELSE reserved_slot END,
 finished_at=CASE WHEN p_state IN ('accepted','succeeded','failed','cancelled','expired') THEN now() ELSE finished_at END,
 acknowledged_at=CASE WHEN j.state IS DISTINCT FROM p_state THEN NULL ELSE acknowledged_at END,updated_at=now() WHERE id=p_id;
 PERFORM clause_forget_if_idle(j.account_id);
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.clause_write_fact(p_id text,p_owner text,
 p_table text,p_payload jsonb,p_conflict text,p_ignore boolean,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;cols text;conflicts text;updates text;clause text;field text;head text;j clause_jobs;p_account text;
BEGIN
 IF NOT clause_guard(p_id,p_owner) THEN RETURN false; END IF;
 SELECT * INTO j FROM clause_jobs WHERE id=p_id; p_account:=j.account_id;
 IF p_table<>ALL(ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history',
 'market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations',
 'market_observations','player_match_observations','league_rule_snapshots','player_round_history_checks',
 'rival_finance_evidence','complete_history_baselines','league_context_observations','player_membership_observations']) THEN RAISE EXCEPTION 'Invalid fact table'; END IF;
 -- Match PostgREST's default merge identity for mutable source catalogs.
 IF p_conflict IS NULL AND p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
  p_conflict:='id';
 END IF;
 IF p_observed IS NULL OR p_observed>now()+interval '60 seconds' THEN RAISE EXCEPTION 'Invalid source time'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
  IF jsonb_typeof(row_data)<>'object' OR octet_length(row_data::text)>1048576 THEN RAISE EXCEPTION 'Invalid fact row'; END IF;
  IF row_data ? 'account_id' AND row_data->>'account_id' IS DISTINCT FROM p_account THEN RAISE EXCEPTION 'Foreign fact account'; END IF;
  IF row_data ? 'championship_id' AND row_data->>'championship_id' IS DISTINCT FROM j.context->>'championship_id'
     THEN RAISE EXCEPTION 'Foreign fact league'; END IF;
  FOR field IN SELECT jsonb_object_keys(row_data) LOOP
   IF field ~* '(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)' OR NOT EXISTS(
    SELECT 1 FROM pg_attribute WHERE attrelid=('public.'||p_table)::regclass AND attname=field AND attnum>0 AND NOT attisdropped)
    THEN RAISE EXCEPTION 'Invalid fact field'; END IF;
  END LOOP;
  IF p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
   head:=NULL;
   IF coalesce(row_data->>'id','')='' THEN RAISE EXCEPTION 'Missing fact identity'; END IF;
   INSERT INTO worker_fact_heads(table_name,entity_id,observed_at) VALUES(p_table,row_data->>'id',p_observed)
   ON CONFLICT(table_name,entity_id) DO UPDATE SET observed_at=excluded.observed_at
   WHERE worker_fact_heads.observed_at<=excluded.observed_at RETURNING entity_id INTO head;
   IF head IS NULL THEN CONTINUE; END IF;
  END IF;
  SELECT string_agg(format('%I',key),',' ORDER BY key),string_agg(format('%I=excluded.%I',key,key),',' ORDER BY key)
  INTO cols,updates FROM jsonb_object_keys(row_data) AS key;
  clause:='';
  IF p_conflict IS NOT NULL THEN
   IF p_conflict !~ '^[a-z_]+(,[a-z_]+)*$' THEN RAISE EXCEPTION 'Invalid fact conflict'; END IF;
   SELECT string_agg(format('%I',v),',') INTO conflicts FROM unnest(string_to_array(p_conflict,',')) AS v;
   clause:=format(' ON CONFLICT (%s) DO %s',conflicts,CASE WHEN p_ignore THEN 'NOTHING' ELSE 'UPDATE SET '||updates END);
  END IF;
  EXECUTE format('INSERT INTO public.%I (%s) SELECT %s FROM jsonb_populate_record(NULL::public.%I,$1)%s',p_table,cols,cols,p_table,clause) USING row_data;
 END LOOP;
 RETURN true;
END $$;
REVOKE ALL ON FUNCTION public.clause_write_fact(text,text,text,jsonb,text,boolean,timestamptz) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.clause_write_fact(text,text,text,jsonb,text,boolean,timestamptz) TO service_role;


CREATE OR REPLACE FUNCTION public.clause_save_evidence(p_id text,p_owner text,p_key text,p_payload jsonb,p_observed timestamptz,p_until timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j clause_jobs;oldest text;
BEGIN
 IF NOT clause_guard(p_id,p_owner) THEN RETURN false; END IF;
 SELECT * INTO j FROM clause_jobs WHERE id=p_id;
 IF p_key !~ '^[a-f0-9]{64}$' OR p_observed IS NULL OR p_observed>now()+interval '2 seconds'
 OR p_until IS NULL OR p_until<p_observed OR p_until>now()+interval '1 day' OR octet_length(p_payload::text)>16777216
 THEN RAISE EXCEPTION 'clause_invalid_evidence'; END IF;
 PERFORM pg_advisory_xact_lock(hashtextextended('clause_evidence:'||j.account_id,0));
 INSERT INTO clause_evidence(account_id,evidence_key,context,payload,payload_bytes,observed_at,valid_until,complete)
 VALUES(j.account_id,p_key,j.context,p_payload,octet_length(p_payload::text),p_observed,p_until,true)
 ON CONFLICT(account_id,evidence_key) DO UPDATE SET context=excluded.context,payload=excluded.payload,payload_bytes=excluded.payload_bytes,
 observed_at=excluded.observed_at,valid_until=excluded.valid_until,complete=true;
 LOOP
  EXIT WHEN (SELECT count(*)<=256 AND coalesce(sum(payload_bytes),0)<=33554432 FROM clause_evidence WHERE account_id=j.account_id);
  SELECT evidence_key INTO oldest FROM clause_evidence WHERE account_id=j.account_id ORDER BY observed_at,evidence_key LIMIT 1;
  DELETE FROM clause_evidence WHERE account_id=j.account_id AND evidence_key=oldest;
 END LOOP;
 RETURN EXISTS(SELECT 1 FROM clause_evidence WHERE account_id=j.account_id AND evidence_key=p_key);
END $$;
CREATE OR REPLACE FUNCTION public.clause_cleanup() RETURNS void LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE clause_jobs SET state='expired',result_code='opening_window_expired',reserved_amount=0,reserved_slot=0,
 finished_at=now(),updated_at=now(),owner=NULL,lease_until=NULL
 WHERE kind='scheduled' AND state IN ('scheduled','preparing') AND expires_at<now();
 UPDATE clause_authorizations a SET encrypted_token=NULL,credential_id=NULL,enabled=false,updated_at=now()
 WHERE encrypted_token IS NOT NULL AND NOT EXISTS(SELECT 1 FROM clause_jobs j WHERE j.account_id=a.account_id
 AND j.kind='scheduled' AND j.state IN ('scheduled','preparing','dispatched','uncertain','accepted'));
 DELETE FROM clause_evidence WHERE valid_until<now()-interval '1 day';
 DELETE FROM clause_attempts WHERE job_id IN (SELECT id FROM clause_jobs WHERE acknowledged_at<now()-interval '90 days' AND state IN ('succeeded','failed','cancelled','expired'));
 DELETE FROM clause_jobs WHERE acknowledged_at<now()-interval '90 days' AND state IN ('succeeded','failed','cancelled','expired');
 DELETE FROM clause_attempts WHERE job_id IN (SELECT id FROM clause_jobs WHERE kind='manual' AND finished_at<now()-interval '7 days' AND state IN ('succeeded','failed','cancelled','expired'));
 DELETE FROM clause_jobs WHERE kind='manual' AND finished_at<now()-interval '7 days' AND state IN ('succeeded','failed','cancelled','expired');
END $$;


CREATE OR REPLACE FUNCTION public.clause_forget_if_idle(p_account text) RETURNS void
 LANGUAGE sql SECURITY INVOKER SET search_path=public,pg_temp AS $$
 UPDATE clause_authorizations a SET encrypted_token=NULL,credential_id=NULL,enabled=false,updated_at=now()
 WHERE account_id=p_account AND encrypted_token IS NOT NULL AND NOT EXISTS(SELECT 1 FROM clause_jobs j
 WHERE j.account_id=a.account_id AND j.kind='scheduled' AND j.state IN ('scheduled','preparing','dispatched','uncertain','accepted'));
$$;
CREATE OR REPLACE FUNCTION public.clause_publish(p_id text,p_owner text,p_descriptor jsonb,p_manifest jsonb) RETURNS boolean
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j clause_jobs;k text:=p_descriptor->>'resource_key';f text;
BEGIN
 IF NOT clause_guard(p_id,p_owner) THEN RETURN false; END IF;
 SELECT * INTO j FROM clause_jobs WHERE id=p_id;
 IF NOT EXISTS(SELECT 1 FROM worker_accounts WHERE account_id=j.account_id) THEN RETURN true; END IF;
 IF k !~ '^[a-f0-9]{64}$' OR p_descriptor->'context'->>'account_id' IS DISTINCT FROM j.account_id
 OR p_descriptor->>'handler' !~ '^(get_|read_)[a-z0-9_]+$'
 OR p_manifest->'context' IS DISTINCT FROM p_descriptor->'context' THEN RAISE EXCEPTION 'clause_publication_context'; END IF;
 IF coalesce(p_descriptor->'context'->>'championship_id','')<>'' THEN
  FOREACH f IN ARRAY ARRAY['championship_id','competition_id','season','scoring_version'] LOOP
   IF coalesce(p_descriptor->'context'->>f,'unknown') NOT IN ('','unknown') AND
   p_descriptor->'context'->>f IS DISTINCT FROM j.context->>f THEN RAISE EXCEPTION 'clause_publication_context'; END IF;
  END LOOP;
 END IF;
 IF (p_manifest->>'observed_at')::timestamptz>now()+interval '2 seconds'
 OR (p_manifest->>'checked_at')::timestamptz>(p_manifest->>'observed_at')::timestamptz
 OR (SELECT count(*) FROM worker_chunks WHERE account_id=j.account_id AND resource_key=k AND version=(p_manifest->>'version')::uuid)
 IS DISTINCT FROM (p_manifest->>'parts')::integer THEN RAISE EXCEPTION 'clause_publication_incomplete'; END IF;
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts)
 VALUES(j.account_id,k,p_descriptor->'context',p_descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,(p_manifest->>'valid_until')::timestamptz,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts
 WHERE worker_snapshots.observed_at<=excluded.observed_at;
 RETURN true;
END $$;

-- Reuse already-acquired factual summaries; never poll distant scheduled jobs.
CREATE OR REPLACE FUNCTION public.clause_observe(p_account text,p_key text,p_owner text,p_generation bigint,p_context jsonb,p_player jsonb,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE source_job worker_jobs;f text;a text;
BEGIN
 IF NOT worker_guard(p_account,p_key,p_owner,p_generation) THEN RETURN false; END IF;
 SELECT * INTO source_job FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key;
 IF p_context->>'account_id' IS DISTINCT FROM p_account OR p_observed IS NULL OR p_observed>now()+interval '2 seconds'
 OR coalesce(p_player->>'id','')='' THEN RETURN false; END IF;
 FOREACH f IN ARRAY ARRAY['championship_id','competition_id','season','scoring_version'] LOOP
  IF coalesce(p_context->>f,'unknown') IN ('','unknown') OR p_context->>f IS DISTINCT FROM source_job.descriptor->'context'->>f THEN RETURN false; END IF;
 END LOOP;
 -- An incomplete summary is not evidence of an owner change, zero clause or unlock.
 IF p_player->>'owner_verified' IS DISTINCT FROM 'true' OR p_player->>'owner_id' IS NULL
 OR p_player->>'transferred' IS NULL OR p_player->>'transferred' NOT IN ('true','false')
 OR (p_player->>'price')::numeric IS NULL OR (p_player->>'price')::numeric<=0
 OR p_player->>'opens_at' IS NULL THEN RETURN false; END IF;
 FOR a IN
  UPDATE clause_jobs j SET
   state=CASE WHEN p_player->>'owner_id' IS DISTINCT FROM j.original_owner OR p_player->>'transferred'='true'
      OR (p_player->>'price')::numeric>j.max_price THEN 'cancelled' ELSE state END,
   result_code=CASE WHEN p_player->>'owner_id' IS DISTINCT FROM j.original_owner OR p_player->>'transferred'='true' THEN 'ownership_or_transfer_changed'
      WHEN (p_player->>'price')::numeric>j.max_price THEN 'price_exceeds_authorized_cap' ELSE result_code END,
   reserved_amount=CASE WHEN p_player->>'owner_id' IS DISTINCT FROM j.original_owner OR p_player->>'transferred'='true'
      OR (p_player->>'price')::numeric>j.max_price THEN 0 ELSE reserved_amount END,
   reserved_slot=CASE WHEN p_player->>'owner_id' IS DISTINCT FROM j.original_owner OR p_player->>'transferred'='true'
      OR (p_player->>'price')::numeric>j.max_price THEN 0 ELSE reserved_slot END,
   opens_at=(p_player->>'opens_at')::timestamptz,
   due_at=(p_player->>'opens_at')::timestamptz-interval '10 seconds',expires_at=(p_player->>'opens_at')::timestamptz+interval '5 minutes',
   player_observed_at=p_observed,updated_at=now(),
   finished_at=CASE WHEN p_player->>'owner_id' IS DISTINCT FROM j.original_owner OR p_player->>'transferred'='true'
      OR (p_player->>'price')::numeric>j.max_price THEN now() ELSE finished_at END
  WHERE j.account_id=p_account AND j.kind='scheduled' AND j.state IN ('scheduled','preparing')
  AND j.context->>'championship_id'=p_context->>'championship_id' AND j.context->>'competition_id'=p_context->>'competition_id'
  AND j.context->>'season'=p_context->>'season' AND j.context->>'scoring_version'=p_context->>'scoring_version'
  AND j.player_id=p_player->>'id' AND p_observed>j.player_observed_at
  RETURNING j.account_id
 LOOP PERFORM clause_forget_if_idle(a); END LOOP;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.clause_observe_transfer() RETURNS trigger
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a text;
BEGIN
 IF NEW.provenance NOT IN ('futmondo_pressroom','futmondo_locker') OR NEW.season='unknown' OR NEW.competition_id='unknown'
 OR NEW.transaction_date IS NULL THEN RETURN NEW; END IF;
 FOR a IN
  UPDATE clause_jobs j SET state='cancelled',result_code='another_buyer_confirmed',reserved_amount=0,reserved_slot=0,
   finished_at=now(),updated_at=now()
  WHERE j.kind='scheduled' AND j.state IN ('scheduled','preparing') AND j.player_id=NEW.player_id
  AND j.context->>'championship_id'=NEW.championship_id AND j.context->>'competition_id'=NEW.competition_id
  AND j.context->>'season'=NEW.season AND coalesce(NEW.buyer_team_id,'')<>j.context->>'user_team_id'
  AND NEW.transaction_date>j.created_at RETURNING j.account_id
 LOOP PERFORM clause_forget_if_idle(a); END LOOP;
 RETURN NEW;
END $$;
CREATE INDEX IF NOT EXISTS clause_jobs_transfer_guard ON public.clause_jobs
 ((context->>'championship_id'),(context->>'competition_id'),(context->>'season'),player_id)
 WHERE kind='scheduled' AND state IN ('scheduled','preparing');
DROP TRIGGER IF EXISTS clause_observed_transfer ON public.market_transactions;
CREATE TRIGGER clause_observed_transfer AFTER INSERT OR UPDATE ON public.market_transactions
 FOR EACH ROW EXECUTE FUNCTION public.clause_observe_transfer();

CREATE OR REPLACE FUNCTION public.clause_retire_context(p_context jsonb,p_observed timestamptz) RETURNS integer
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE n integer;f text;
BEGIN
 FOREACH f IN ARRAY ARRAY['account_id','championship_id','competition_id','season','scoring_version'] LOOP
  IF coalesce(p_context->>f,'unknown') IN ('','unknown') THEN RETURN 0; END IF;
 END LOOP;
 IF p_observed IS NULL OR p_observed>now()+interval '2 seconds' THEN RETURN 0; END IF;
 UPDATE clause_jobs SET state='cancelled',result_code='context_superseded',reserved_amount=0,reserved_slot=0,
 finished_at=now(),updated_at=now()
 WHERE account_id=p_context->>'account_id' AND context->>'championship_id'=p_context->>'championship_id'
 AND kind='scheduled' AND state IN ('scheduled','preparing') AND player_observed_at<p_observed
 AND (context->>'competition_id' IS DISTINCT FROM p_context->>'competition_id' OR context->>'season' IS DISTINCT FROM p_context->>'season'
 OR context->>'scoring_version' IS DISTINCT FROM p_context->>'scoring_version');
 GET DIAGNOSTICS n=ROW_COUNT;
 PERFORM clause_forget_if_idle(p_context->>'account_id');RETURN n;
END $$;

DO $$ DECLARE t text; f record; BEGIN
 FOREACH t IN ARRAY ARRAY['clause_authorizations','clause_jobs','clause_attempts','clause_evidence'] LOOP
  EXECUTE format('ALTER TABLE public.%I ENABLE ROW LEVEL SECURITY',t);
  EXECUTE format('REVOKE ALL ON public.%I FROM PUBLIC,anon,authenticated',t);
  EXECUTE format('GRANT SELECT,INSERT,UPDATE,DELETE ON public.%I TO service_role',t);
 END LOOP;
 FOR f IN SELECT oid::regprocedure signature FROM pg_proc WHERE pronamespace='public'::regnamespace AND proname LIKE 'clause_%' LOOP
  EXECUTE format('REVOKE ALL ON FUNCTION %s FROM PUBLIC,anon,authenticated',f.signature);
  EXECUTE format('GRANT EXECUTE ON FUNCTION %s TO service_role',f.signature);
 END LOOP;
END $$;
COMMIT;
