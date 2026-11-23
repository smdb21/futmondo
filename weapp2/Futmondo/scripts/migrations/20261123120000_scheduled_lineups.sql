BEGIN;
CREATE TABLE IF NOT EXISTS public.lineup_schedule_plans (
 id text PRIMARY KEY CHECK (id ~ '^[a-f0-9]{32}$'),
 context jsonb NOT NULL,
 round_id text NOT NULL,
 kickoff timestamptz NOT NULL,
 baseline jsonb NOT NULL,
 stages jsonb NOT NULL CHECK (jsonb_typeof(stages) = 'array' AND octet_length(stages::text) <= 131072),
 doubtful boolean NOT NULL,
 encrypted_token text,
 credential_id text,
 stage_index integer NOT NULL DEFAULT 0,
 outcomes jsonb NOT NULL DEFAULT '[]',
 state text NOT NULL DEFAULT 'queued' CHECK (state IN ('queued','preparing','dispatched','uncertain','saved','cancelled','expired','failed')),
 code text NOT NULL DEFAULT '',
 due_at timestamptz NOT NULL DEFAULT now(),
 owner text,
 lease_until timestamptz,
 dispatched_at timestamptz,
 updated_at timestamptz NOT NULL DEFAULT now()
);
CREATE UNIQUE INDEX IF NOT EXISTS lineup_schedule_active_team ON public.lineup_schedule_plans
 ((context->>'account_id'), (context->>'championship_id'), (context->>'user_team_id'))
 WHERE state IN ('queued','preparing','dispatched','uncertain');
CREATE INDEX IF NOT EXISTS lineup_schedule_due ON public.lineup_schedule_plans (due_at)
 WHERE state IN ('queued','preparing','dispatched','uncertain');
CREATE INDEX IF NOT EXISTS lineup_schedule_team_history ON public.lineup_schedule_plans
 ((context->>'account_id'), (context->>'championship_id'), (context->>'user_team_id'), updated_at DESC);
CREATE INDEX IF NOT EXISTS lineup_schedule_retention ON public.lineup_schedule_plans (updated_at)
 WHERE state IN ('saved','cancelled','expired','failed');
CREATE TABLE IF NOT EXISTS public.lineup_schedule_manual (
 scope text PRIMARY KEY, request_id text NOT NULL, expires_at timestamptz NOT NULL
);
CREATE TABLE IF NOT EXISTS public.lineup_schedule_evidence (
 plan_id text NOT NULL REFERENCES public.lineup_schedule_plans(id) ON DELETE CASCADE,
 resource text NOT NULL CHECK(resource IN ('/1/userteam/lineup','/1/userteam/roster','/1/userteam/information',
   '/5/strategy/availables','/2/user/activechampionships','/2/league/list','/2/league/matches')),
 observed_at timestamptz NOT NULL,
 valid_until timestamptz NOT NULL CHECK (valid_until >= observed_at),
 payload jsonb NOT NULL CHECK (octet_length(payload::text) <= 8388608),
 PRIMARY KEY(plan_id,resource)
);
CREATE INDEX IF NOT EXISTS lineup_schedule_evidence_expiry ON public.lineup_schedule_evidence (valid_until);
ALTER TABLE public.lineup_schedule_plans ENABLE ROW LEVEL SECURITY;
ALTER TABLE public.lineup_schedule_manual ENABLE ROW LEVEL SECURITY;
ALTER TABLE public.lineup_schedule_evidence ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.lineup_schedule_plans, public.lineup_schedule_manual, public.lineup_schedule_evidence FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.lineup_schedule_plans, public.lineup_schedule_manual, public.lineup_schedule_evidence TO service_role;

CREATE OR REPLACE FUNCTION public.lineup_schedule_scope(p_context jsonb) RETURNS text
LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path = '' AS $$
 SELECT jsonb_build_array(p_context->>'account_id',p_context->>'championship_id',p_context->>'user_team_id')::text
$$;

CREATE OR REPLACE FUNCTION public.lineup_schedule_read(p_context jsonb) RETURNS jsonb
LANGUAGE sql SECURITY INVOKER SET search_path = '' AS $$
 SELECT to_jsonb(p)-'encrypted_token'-'credential_id'-'owner'-'lease_until'-'baseline'
 FROM public.lineup_schedule_plans p
 WHERE context->>'account_id'=p_context->>'account_id' AND context->>'championship_id'=p_context->>'championship_id'
   AND context->>'user_team_id'=p_context->>'user_team_id'
 ORDER BY (state IN ('queued','preparing','dispatched','uncertain')) DESC,updated_at DESC LIMIT 1
$$;

CREATE OR REPLACE FUNCTION public.lineup_schedule_admit(p_id text,p_context jsonb,p_round text,p_kickoff timestamptz,
 p_baseline jsonb,p_stages jsonb,p_doubtful boolean,p_token text,p_credential text) RETURNS boolean
LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
DECLARE team_scope text := public.lineup_schedule_scope(p_context); stage jsonb; idx integer := 0;
  due timestamptz; previous_due timestamptz; field text; players text[] := '{}'; player text;
BEGIN
 PERFORM pg_advisory_xact_lock(hashtextextended(team_scope, 0));
 IF EXISTS (SELECT 1 FROM public.lineup_schedule_plans WHERE id=p_id AND context=p_context) THEN RETURN true; END IF;
 FOREACH field IN ARRAY ARRAY['account_id','championship_id','user_team_id','competition_id','season','scoring_version'] LOOP
  IF coalesce(p_context->>field,'') IN ('','unknown') OR length(p_context->>field)>256 THEN RETURN false; END IF;
 END LOOP;
 IF p_kickoff<=clock_timestamp() OR coalesce(p_round,'')='' OR p_token IS NULL OR p_credential IS NULL
    OR length(p_token)>8192 OR jsonb_typeof(p_stages) IS DISTINCT FROM 'array'
    OR jsonb_array_length(p_stages) NOT BETWEEN 2 AND 256
    OR EXISTS (SELECT 1 FROM public.lineup_schedule_manual WHERE lineup_schedule_manual.scope=team_scope AND expires_at>clock_timestamp())
    OR EXISTS (SELECT 1 FROM public.lineup_schedule_plans p WHERE p.context->>'account_id'=p_context->>'account_id'
       AND p.context->>'championship_id'=p_context->>'championship_id' AND p.context->>'user_team_id'=p_context->>'user_team_id'
       AND p.state IN ('queued','preparing','dispatched','uncertain')) THEN RETURN false; END IF;
 FOR stage IN SELECT value FROM jsonb_array_elements(p_stages) LOOP
  player := stage->>'player_id';
  due := (stage->>'due_at')::timestamptz;
  IF jsonb_typeof(stage#>'{lineup,starters}') IS DISTINCT FROM 'array'
     OR jsonb_array_length(stage#>'{lineup,starters}')<>11
     OR jsonb_typeof(stage#>'{lineup,bench}') IS DISTINCT FROM 'array'
     OR jsonb_array_length(stage#>'{lineup,bench}')<>4 OR coalesce(stage#>>'{lineup,strategy}','')=''
     OR (idx=0 AND (player IS DISTINCT FROM '' OR due IS NOT NULL))
     OR (idx>0 AND (coalesce(player,'')='' OR player=ANY(players) OR due IS NULL OR due>=p_kickoff OR due<previous_due)) THEN
      RETURN false;
  END IF;
  players := array_append(players,player); previous_due := due; idx := idx+1;
 END LOOP;
 INSERT INTO public.lineup_schedule_plans(id,context,round_id,kickoff,baseline,stages,doubtful,encrypted_token,credential_id)
 VALUES(p_id,p_context,p_round,p_kickoff,p_baseline,p_stages,p_doubtful,p_token,p_credential);
 RETURN true;
END $$;

CREATE OR REPLACE FUNCTION public.lineup_schedule_claim(p_owner text) RETURNS jsonb
LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
DECLARE job public.lineup_schedule_plans;
BEGIN
 IF NOT EXISTS (SELECT 1 FROM public.worker_leader WHERE id=1 AND owner=p_owner AND lease_until>clock_timestamp()) THEN RETURN NULL; END IF;
 SELECT * INTO job FROM public.lineup_schedule_plans p WHERE state IN ('queued','preparing','dispatched','uncertain')
  AND due_at<=statement_timestamp() AND (p.owner IS NULL OR p.lease_until<=statement_timestamp())
  ORDER BY due_at LIMIT 1 FOR UPDATE SKIP LOCKED;
 IF NOT FOUND THEN RETURN NULL; END IF;
 IF job.kickoff<=clock_timestamp() AND job.state IN ('queued','preparing') THEN
  UPDATE public.lineup_schedule_plans SET state='expired',code='kickoff',owner=NULL,lease_until=NULL,
   encrypted_token=NULL,credential_id=NULL,updated_at=clock_timestamp() WHERE id=job.id;
  RETURN NULL;
 END IF;
 UPDATE public.lineup_schedule_plans SET owner=p_owner,lease_until=clock_timestamp()+interval '100 seconds',
  state=CASE WHEN state IN ('dispatched','uncertain') THEN 'uncertain' ELSE 'preparing' END
  WHERE id=job.id RETURNING * INTO job;
 RETURN to_jsonb(job);
END $$;

CREATE OR REPLACE FUNCTION public.lineup_schedule_dispatch(p_id text,p_index integer,p_owner text,p_until timestamptz) RETURNS boolean
LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
BEGIN
 UPDATE public.lineup_schedule_plans p SET state='dispatched',dispatched_at=clock_timestamp(),updated_at=clock_timestamp()
 WHERE id=p_id AND stage_index=p_index AND state='preparing' AND owner=p_owner AND lease_until>clock_timestamp()
  AND clock_timestamp()<least(kickoff,p_until) AND EXISTS
   (SELECT 1 FROM public.worker_leader WHERE id=1 AND owner=p_owner AND lease_until>clock_timestamp());
 RETURN FOUND;
END $$;

CREATE OR REPLACE FUNCTION public.lineup_schedule_settle(p_id text,p_index integer,p_owner text,p_code text,p_due timestamptz DEFAULT NULL) RETURNS boolean
LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
DECLARE job public.lineup_schedule_plans; next_index integer; next_due timestamptz; next_state text;
BEGIN
 SELECT * INTO job FROM public.lineup_schedule_plans WHERE id=p_id AND stage_index=p_index AND owner=p_owner
  AND lease_until>clock_timestamp() FOR UPDATE;
 IF NOT FOUND THEN RETURN false; END IF;
 next_index := job.stage_index;
 IF p_code IN ('saved','unchanged') AND (job.state IN ('dispatched','uncertain') AND p_code='saved' OR job.state='preparing' AND p_code='unchanged') THEN
  next_index := next_index + 1;
  next_state := CASE WHEN next_index=jsonb_array_length(job.stages) THEN 'saved' ELSE 'queued' END;
  next_due := (job.stages->next_index->>'due_at')::timestamptz;
 ELSIF p_code IN ('waiting_for_ownership','source_unavailable') AND job.state='preparing' THEN
  next_state := CASE WHEN clock_timestamp()>=job.kickoff THEN 'expired' ELSE 'queued' END;
  next_due := least(job.kickoff,p_due);
 ELSIF p_code='uncertain' AND job.state IN ('dispatched','uncertain') THEN
  next_state := 'uncertain'; next_due := least(job.kickoff,p_due);
 ELSIF p_code IN ('readback_mismatch','rejected','readback_unverified','auth_unavailable') AND job.state IN ('dispatched','uncertain') THEN
  next_state := 'failed';
 ELSIF p_code IN ('round_changed','lineup_changed','context_changed','lineup_unavailable','auth_unavailable') AND job.state='preparing' THEN
  next_state := CASE WHEN p_code IN ('round_changed','lineup_changed','context_changed') THEN 'cancelled' ELSE 'failed' END;
 ELSE RETURN false;
 END IF;
 UPDATE public.lineup_schedule_plans SET stage_index=next_index,state=next_state,code=p_code,
  outcomes=CASE WHEN next_index>job.stage_index THEN outcomes||jsonb_build_array(jsonb_build_object('code',p_code,'at',clock_timestamp(),'dispatched_at',job.dispatched_at)) ELSE outcomes END,
  due_at=coalesce(next_due,clock_timestamp()),owner=NULL,lease_until=NULL,updated_at=clock_timestamp(),
  dispatched_at=CASE WHEN next_state IN ('uncertain','failed') THEN dispatched_at ELSE NULL END,
  encrypted_token=CASE WHEN next_state IN ('queued','uncertain') THEN encrypted_token ELSE NULL END,
  credential_id=CASE WHEN next_state IN ('queued','uncertain') THEN credential_id ELSE NULL END WHERE id=p_id;
 RETURN true;
END $$;

CREATE OR REPLACE FUNCTION public.lineup_schedule_manual_begin(p_context jsonb,p_request text,p_confirmed text DEFAULT NULL)
RETURNS text LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
DECLARE team_scope text:=public.lineup_schedule_scope(p_context); job public.lineup_schedule_plans;
BEGIN
 PERFORM pg_advisory_xact_lock(hashtextextended(team_scope,0));
 IF EXISTS(SELECT 1 FROM public.lineup_schedule_manual WHERE scope=team_scope AND request_id<>p_request AND expires_at>clock_timestamp()) THEN
  RETURN 'busy';
 END IF;
 SELECT * INTO job FROM public.lineup_schedule_plans p WHERE p.context->>'account_id'=p_context->>'account_id'
  AND p.context->>'championship_id'=p_context->>'championship_id' AND p.context->>'user_team_id'=p_context->>'user_team_id'
  AND p.state IN ('queued','preparing','dispatched','uncertain') FOR UPDATE;
 IF FOUND THEN
  IF job.state IN ('dispatched','uncertain') THEN RETURN 'busy'; END IF;
  IF job.id IS DISTINCT FROM p_confirmed THEN RETURN 'confirm:'||job.id; END IF;
  UPDATE public.lineup_schedule_plans SET state='cancelled',code='manual_save',owner=NULL,lease_until=NULL,
   encrypted_token=NULL,credential_id=NULL,updated_at=clock_timestamp() WHERE id=job.id;
 END IF;
 INSERT INTO public.lineup_schedule_manual VALUES(team_scope,p_request,clock_timestamp()+interval '120 seconds')
 ON CONFLICT(scope) DO UPDATE SET request_id=EXCLUDED.request_id,expires_at=EXCLUDED.expires_at;
 RETURN 'ready';
END $$;

CREATE OR REPLACE FUNCTION public.lineup_schedule_manual_end(p_context jsonb,p_request text) RETURNS void
LANGUAGE sql SECURITY INVOKER SET search_path = '' AS $$
 DELETE FROM public.lineup_schedule_manual WHERE scope=public.lineup_schedule_scope(p_context) AND request_id=p_request
$$;

CREATE OR REPLACE FUNCTION public.lineup_schedule_cancel(p_context jsonb,p_id text) RETURNS boolean
LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
BEGIN
 UPDATE public.lineup_schedule_plans SET state='cancelled',code='user_cancelled',owner=NULL,lease_until=NULL,
  encrypted_token=NULL,credential_id=NULL,updated_at=clock_timestamp()
 WHERE id=p_id AND context->>'account_id'=p_context->>'account_id' AND context->>'championship_id'=p_context->>'championship_id'
  AND context->>'user_team_id'=p_context->>'user_team_id' AND state IN ('queued','preparing');
 RETURN FOUND;
END $$;

CREATE OR REPLACE FUNCTION public.lineup_schedule_cleanup() RETURNS void
LANGUAGE sql SECURITY INVOKER SET search_path = '' AS $$
 DELETE FROM public.lineup_schedule_manual WHERE expires_at<clock_timestamp();
 DELETE FROM public.lineup_schedule_evidence WHERE valid_until<clock_timestamp()-interval '1 day';
 DELETE FROM public.lineup_schedule_plans WHERE state IN ('saved','cancelled','expired','failed') AND updated_at<clock_timestamp()-interval '90 days';
$$;

DO $$ DECLARE signature regprocedure;
BEGIN
 FOR signature IN SELECT p.oid::regprocedure FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace
  WHERE n.nspname='public' AND left(p.proname,16)='lineup_schedule_'
 LOOP
  EXECUTE format('REVOKE ALL ON FUNCTION %s FROM PUBLIC,anon,authenticated',signature);
  EXECUTE format('GRANT EXECUTE ON FUNCTION %s TO service_role',signature);
 END LOOP;
END $$;
NOTIFY pgrst, 'reload schema';
COMMIT;
