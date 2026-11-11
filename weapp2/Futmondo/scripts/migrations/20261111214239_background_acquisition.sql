-- Durable, server-only background acquisition. Additive; no existing facts are rewritten.
BEGIN;
CREATE TABLE IF NOT EXISTS public.worker_accounts (
 account_id text PRIMARY KEY, enabled boolean NOT NULL DEFAULT false,
 generation bigint NOT NULL DEFAULT 1, encrypted_token text,credential_id text,
 timezone text NOT NULL DEFAULT 'UTC', quiet_start integer NOT NULL DEFAULT 0 CHECK(quiet_start BETWEEN 0 AND 23),
 quiet_end integer NOT NULL DEFAULT 7 CHECK(quiet_end BETWEEN 0 AND 23), excluded_leagues jsonb NOT NULL DEFAULT '[]',
 verified_leagues jsonb NOT NULL DEFAULT '{}',last_dispatched timestamptz,rate_limited_until timestamptz,
 auth_status text NOT NULL DEFAULT 'active' CHECK(auth_status IN ('active','reauthorize','configuration')),
 updated_at timestamptz NOT NULL DEFAULT now(), CHECK(NOT enabled OR (encrypted_token IS NOT NULL AND credential_id IS NOT NULL)),
 CHECK(jsonb_typeof(excluded_leagues)='array'), CHECK(octet_length(encrypted_token)<=16384)
);
CREATE TABLE IF NOT EXISTS public.worker_jobs (
 account_id text NOT NULL REFERENCES public.worker_accounts(account_id), resource_key text NOT NULL CHECK(resource_key ~ '^[a-f0-9]{64}$'),
 descriptor jsonb NOT NULL, generation bigint NOT NULL, interval_seconds integer NOT NULL DEFAULT 60 CHECK(interval_seconds BETWEEN 15 AND 86400),
 base_priority integer NOT NULL DEFAULT 0 CHECK(base_priority BETWEEN 0 AND 4),
 requested_at timestamptz NOT NULL DEFAULT now(),started_at timestamptz,
 priority integer NOT NULL DEFAULT 0 CHECK(priority BETWEEN 0 AND 10), due_at timestamptz NOT NULL DEFAULT now(),
 owner text, lease_until timestamptz, run_id uuid NOT NULL DEFAULT gen_random_uuid(), attempts integer NOT NULL DEFAULT 0,
 last_error text, checked_at timestamptz,last_audit_at timestamptz, checkpoint jsonb NOT NULL DEFAULT '{}',
 PRIMARY KEY(account_id,resource_key), CHECK(octet_length(descriptor::text)<=262144),CHECK(octet_length(checkpoint::text)<=1048576)
);
CREATE INDEX IF NOT EXISTS worker_jobs_due ON public.worker_jobs(due_at,priority DESC) WHERE owner IS NULL;
CREATE TABLE IF NOT EXISTS public.worker_snapshots (
 account_id text NOT NULL REFERENCES public.worker_accounts(account_id), resource_key text NOT NULL,
 context jsonb NOT NULL, handler text NOT NULL, version uuid NOT NULL,
 observed_at timestamptz NOT NULL, checked_at timestamptz NOT NULL, valid_until timestamptz NOT NULL,
 completeness text NOT NULL CHECK(completeness IN ('complete','partial','unknown')), status text NOT NULL,
 json_bytes integer NOT NULL CHECK(json_bytes BETWEEN 1 AND 16777216), sha256 text NOT NULL CHECK(sha256 ~ '^[a-f0-9]{64}$'),
 parts integer NOT NULL CHECK(parts BETWEEN 1 AND 128), PRIMARY KEY(account_id,resource_key)
);
CREATE TABLE IF NOT EXISTS public.worker_chunks (
 account_id text NOT NULL,resource_key text NOT NULL,version uuid NOT NULL,part integer NOT NULL CHECK(part BETWEEN 0 AND 127),
 data text NOT NULL CHECK(octet_length(data)<=131072),created_at timestamptz NOT NULL DEFAULT now(),
 PRIMARY KEY(account_id,resource_key,version,part)
);
CREATE INDEX IF NOT EXISTS worker_chunks_created ON public.worker_chunks(created_at);
CREATE TABLE IF NOT EXISTS public.worker_health (
 id text PRIMARY KEY, heartbeat timestamptz NOT NULL DEFAULT now(), status text NOT NULL,
 requests bigint NOT NULL DEFAULT 0, jobs bigint NOT NULL DEFAULT 0
);
CREATE TABLE IF NOT EXISTS public.worker_leader (
 id integer PRIMARY KEY CHECK(id=1),owner text NOT NULL,lease_until timestamptz NOT NULL
);
ALTER TABLE public.worker_leader ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.worker_leader FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.worker_leader TO service_role;
CREATE OR REPLACE FUNCTION public.worker_elect(p_owner text) RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 INSERT INTO worker_leader(id,owner,lease_until) VALUES(1,p_owner,now()+interval '60 seconds')
 ON CONFLICT(id) DO UPDATE SET owner=excluded.owner,lease_until=excluded.lease_until
 WHERE worker_leader.owner=p_owner OR worker_leader.lease_until<now();
 RETURN FOUND;
END $$;
CREATE OR REPLACE FUNCTION public.worker_release(p_owner text) RETURNS void LANGUAGE sql SECURITY INVOKER SET search_path=public,pg_temp AS $$
 DELETE FROM worker_leader WHERE owner=p_owner;
$$;
REVOKE ALL ON FUNCTION public.worker_elect(text),public.worker_release(text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_elect(text),public.worker_release(text) TO service_role;

CREATE OR REPLACE FUNCTION public.worker_authorize(p_account text,p_enabled boolean,p_token text DEFAULT NULL,
 p_timezone text DEFAULT 'UTC',p_start integer DEFAULT 0,p_end integer DEFAULT 7,p_excluded jsonb DEFAULT '[]',p_credential text DEFAULT NULL)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE result worker_accounts;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM pg_timezone_names WHERE name=p_timezone) THEN RAISE EXCEPTION 'Invalid timezone'; END IF;
 INSERT INTO worker_accounts(account_id,enabled,encrypted_token,timezone,quiet_start,quiet_end,excluded_leagues,credential_id)
 VALUES(p_account,p_enabled,CASE WHEN p_enabled THEN p_token ELSE NULL END,p_timezone,p_start,p_end,p_excluded,CASE WHEN p_enabled THEN p_credential ELSE NULL END)
 ON CONFLICT(account_id) DO UPDATE SET enabled=excluded.enabled,encrypted_token=excluded.encrypted_token,credential_id=excluded.credential_id,
 generation=worker_accounts.generation+1,timezone=excluded.timezone,quiet_start=excluded.quiet_start,quiet_end=excluded.quiet_end,
 excluded_leagues=excluded.excluded_leagues,auth_status='active',updated_at=now() RETURNING * INTO result;
 IF p_enabled THEN
 UPDATE worker_jobs SET generation=result.generation,owner=NULL,lease_until=NULL,checkpoint='{}',run_id=gen_random_uuid(),attempts=0,last_error=NULL WHERE account_id=p_account;
 ELSE DELETE FROM worker_jobs WHERE account_id=p_account; END IF;
 RETURN to_jsonb(result)-'encrypted_token'-'last_dispatched'-'verified_leagues';
END $$;
CREATE OR REPLACE FUNCTION public.worker_enqueue(p_account text,p_descriptor jsonb,p_interval integer DEFAULT 60,p_priority integer DEFAULT 10)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;k text:=p_descriptor->>'resource_key';
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' THEN RETURN false; END IF;
 IF p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account OR (p_descriptor->>'handler') !~ '^(get_|read_|fm_worker_finances$)' THEN RAISE EXCEPTION 'Invalid descriptor'; END IF;
 IF a.excluded_leagues ? (p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF NOT EXISTS(SELECT 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=k) AND
 (SELECT count(*) FROM worker_jobs WHERE account_id=p_account)>=10000 THEN RAISE EXCEPTION 'Worker job limit'; END IF;
 INSERT INTO worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds,priority,base_priority)
 VALUES(p_account,k,p_descriptor,a.generation,p_interval,greatest(p_priority,CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END),CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET
 due_at=CASE WHEN worker_jobs.owner IS NULL AND excluded.priority>=5 THEN least(worker_jobs.due_at,now()) ELSE worker_jobs.due_at END,
 requested_at=CASE WHEN excluded.priority>=5 THEN now() ELSE worker_jobs.requested_at END,priority=greatest(worker_jobs.priority,excluded.priority),base_priority=greatest(worker_jobs.base_priority,excluded.base_priority),interval_seconds=least(worker_jobs.interval_seconds,excluded.interval_seconds);
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND (q.owner IS NULL OR q.lease_until<now()) AND (NOT p_foreground OR q.priority>=5)
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND (q.priority>=5 OR q.checked_at IS NULL OR q.checked_at +
 make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE q.interval_seconds END)<=now())
 ORDER BY q.priority DESC,a.last_dispatched NULLS FIRST,q.due_at,q.account_id,q.resource_key FOR UPDATE OF q SKIP LOCKED LIMIT 1;
 IF j.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
CREATE OR REPLACE FUNCTION public.worker_guard(p_account text,p_key text,p_owner text,p_generation bigint,p_checkpoint jsonb DEFAULT NULL)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE worker_jobs j SET lease_until=now()+interval '60 seconds',checkpoint=coalesce(p_checkpoint,j.checkpoint)
 FROM worker_accounts a WHERE j.account_id=p_account AND j.resource_key=p_key AND j.owner=p_owner
 AND j.lease_until>now() AND a.account_id=j.account_id AND a.enabled AND a.auth_status='active'
 AND a.generation=p_generation AND j.generation=p_generation
 AND EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now());
 RETURN FOUND;
END $$;
CREATE OR REPLACE FUNCTION public.worker_publish(p_account text,p_key text,p_owner text,p_generation bigint,p_manifest jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;a worker_accounts;
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key FOR UPDATE;
 IF a.account_id IS NULL OR j.account_id IS NULL OR NOT a.enabled OR a.generation<>p_generation OR a.auth_status<>'active' OR j.owner IS NULL OR j.lease_until IS NULL OR j.owner IS DISTINCT FROM p_owner OR j.lease_until<=now() OR NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN false; END IF;
 IF (SELECT count(*) FROM worker_chunks WHERE account_id=p_account AND resource_key=p_key AND version=(p_manifest->>'version')::uuid)<>(p_manifest->>'parts')::integer THEN RAISE EXCEPTION 'Incomplete snapshot'; END IF;
 IF j.descriptor->>'handler'='get_active_championships' AND jsonb_typeof(p_manifest->'memberships')='object' THEN
 UPDATE worker_accounts SET verified_leagues=p_manifest->'memberships' WHERE account_id=p_account;
 DELETE FROM worker_jobs q WHERE q.account_id=p_account AND q.resource_key<>p_key AND coalesce(q.descriptor->'context'->>'championship_id','')<>'' AND
 (NOT (p_manifest->'memberships' ? (q.descriptor->'context'->>'championship_id')) OR EXISTS(
 SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) AS dim WHERE
 coalesce(q.descriptor->'context'->>dim,'unknown')<>'unknown' AND
 coalesce(p_manifest->'memberships'->(q.descriptor->'context'->>'championship_id')->>dim,'unknown')<>'unknown' AND
 q.descriptor->'context'->>dim IS DISTINCT FROM p_manifest->'memberships'->(q.descriptor->'context'->>'championship_id')->>dim)); END IF;
 IF p_manifest->'context' IS DISTINCT FROM j.descriptor->'context' THEN RAISE EXCEPTION 'Foreign context'; END IF;
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts)
 VALUES(p_account,p_key,p_manifest->'context',j.descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,CASE WHEN j.requested_at>j.started_at THEN least((p_manifest->>'valid_until')::timestamptz,now()-interval '1 second') ELSE (p_manifest->>'valid_until')::timestamptz END,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts
 WHERE worker_snapshots.checked_at<=excluded.checked_at;
 UPDATE worker_jobs SET owner=NULL,lease_until=NULL,checked_at=now(),due_at=CASE WHEN requested_at>started_at THEN now() ELSE now()+make_interval(secs=>interval_seconds) END,
 priority=CASE WHEN requested_at>started_at THEN 10 ELSE base_priority END,attempts=0,last_error=p_manifest->>'error',last_audit_at=CASE WHEN p_manifest->>'full_audit'='true' THEN now() ELSE last_audit_at END,checkpoint='{}',run_id=gen_random_uuid() WHERE account_id=p_account AND resource_key=p_key;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.worker_read(p_account text,p_versions jsonb DEFAULT '{}')
RETURNS jsonb LANGUAGE sql SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT jsonb_build_object(
 'account',(SELECT to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues' FROM worker_accounts a WHERE account_id=p_account),
 'health',(SELECT to_jsonb(h) FROM worker_health h WHERE status='running' ORDER BY heartbeat DESC LIMIT 1),
 'resources',coalesce((SELECT jsonb_agg(jsonb_build_object('key',s.resource_key,'manifest',to_jsonb(s),
 'chunks',(SELECT jsonb_agg(c.data ORDER BY c.part) FROM worker_chunks c WHERE c.account_id=s.account_id AND c.resource_key=s.resource_key AND c.version=s.version),
 'error',j.last_error)) FROM worker_snapshots s LEFT JOIN worker_jobs j USING(account_id,resource_key)
 WHERE s.account_id=p_account AND p_versions ? s.resource_key AND s.version::text IS DISTINCT FROM p_versions->>s.resource_key),'[]'),
 'jobs',coalesce((SELECT jsonb_agg(jsonb_build_object('key',resource_key,'error',last_error,'running',owner IS NOT NULL,'pending',owner IS NOT NULL OR due_at<=now()))
 FROM worker_jobs WHERE account_id=p_account AND p_versions ? resource_key),'[]'));
$$;
-- Server-only objects and invoker functions. No browser token grants access.
DO $$ DECLARE t text;f record; BEGIN
 FOREACH t IN ARRAY ARRAY['worker_accounts','worker_jobs','worker_snapshots','worker_chunks','worker_health'] LOOP
 EXECUTE format('ALTER TABLE public.%I ENABLE ROW LEVEL SECURITY',t);
 EXECUTE format('REVOKE ALL ON public.%I FROM PUBLIC,anon,authenticated',t);
 EXECUTE format('GRANT SELECT,INSERT,UPDATE,DELETE ON public.%I TO service_role',t);
 END LOOP;
 FOR f IN SELECT oid::regprocedure AS signature FROM pg_proc WHERE pronamespace='public'::regnamespace AND proname IN
 ('worker_authorize','worker_enqueue','worker_claim','worker_guard','worker_publish','worker_read') LOOP
 EXECUTE format('REVOKE ALL ON FUNCTION %s FROM PUBLIC,anon,authenticated',f.signature);
 EXECUTE format('GRANT EXECUTE ON FUNCTION %s TO service_role',f.signature);
 END LOOP;
END $$;

CREATE OR REPLACE FUNCTION public.worker_cleanup() RETURNS void LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 DELETE FROM worker_chunks c WHERE created_at<now()-interval '1 day' AND NOT EXISTS(
 SELECT 1 FROM worker_snapshots s WHERE s.account_id=c.account_id AND s.resource_key=c.resource_key AND s.version=c.version);
 DELETE FROM worker_health WHERE heartbeat<now()-interval '1 day';
END $$;
REVOKE ALL ON FUNCTION public.worker_cleanup() FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_cleanup() TO service_role;
ALTER TABLE public.worker_accounts ADD COLUMN IF NOT EXISTS verified_leagues jsonb NOT NULL DEFAULT '{}';
ALTER TABLE public.worker_accounts ADD COLUMN IF NOT EXISTS last_dispatched timestamptz;
CREATE TABLE IF NOT EXISTS public.worker_checkpoints (
 account_id text NOT NULL,resource_key text NOT NULL,run_id uuid NOT NULL,request_key text NOT NULL,
 response jsonb NOT NULL CHECK(octet_length(response::text)<=4194304),bytes integer GENERATED ALWAYS AS (octet_length(response::text)) STORED,created_at timestamptz NOT NULL DEFAULT now(),
 PRIMARY KEY(account_id,resource_key,run_id,request_key),
 FOREIGN KEY(account_id,resource_key) REFERENCES public.worker_jobs(account_id,resource_key) ON DELETE CASCADE
);
ALTER TABLE public.worker_checkpoints ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.worker_checkpoints FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.worker_checkpoints TO service_role;
CREATE OR REPLACE FUNCTION public.worker_checkpoint(p_account text,p_key text,p_owner text,p_generation bigint,p_request text,p_response jsonb DEFAULT NULL)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;r jsonb;
BEGIN
 PERFORM 1 FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT worker_guard(p_account,p_key,p_owner,p_generation) THEN RAISE EXCEPTION 'Worker lease lost'; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key;
 IF p_response IS NULL THEN
 SELECT response INTO r FROM worker_checkpoints WHERE account_id=p_account AND resource_key=p_key AND run_id=j.run_id AND request_key=p_request;
 ELSE
 IF (SELECT coalesce(sum(bytes),0) FROM worker_checkpoints WHERE account_id=p_account)+octet_length(p_response::text)>16777216 THEN RETURN NULL; END IF;
 INSERT INTO worker_checkpoints(account_id,resource_key,run_id,request_key,response) VALUES(p_account,p_key,j.run_id,p_request,p_response)
 ON CONFLICT DO NOTHING;
 r:=p_response;
 END IF;
 RETURN r;
END $$;
REVOKE ALL ON FUNCTION public.worker_checkpoint(text,text,text,bigint,text,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_checkpoint(text,text,text,bigint,text,jsonb) TO service_role;
CREATE OR REPLACE FUNCTION public.worker_cleanup() RETURNS void LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 DELETE FROM worker_chunks c WHERE created_at<now()-interval '1 day' AND NOT EXISTS(
 SELECT 1 FROM worker_snapshots s WHERE s.account_id=c.account_id AND s.resource_key=c.resource_key AND s.version=c.version);
 DELETE FROM worker_checkpoints c WHERE created_at<now()-interval '1 day' OR NOT EXISTS(
 SELECT 1 FROM worker_jobs j WHERE j.account_id=c.account_id AND j.resource_key=c.resource_key AND j.run_id=c.run_id);
 DELETE FROM worker_health WHERE heartbeat<now()-interval '1 day';
END $$;
CREATE TABLE IF NOT EXISTS public.worker_fact_heads (
 table_name text NOT NULL,entity_id text NOT NULL,observed_at timestamptz NOT NULL,PRIMARY KEY(table_name,entity_id)
);
ALTER TABLE public.worker_fact_heads ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.worker_fact_heads FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE ON public.worker_fact_heads TO service_role;
CREATE OR REPLACE FUNCTION public.worker_write_fact(p_account text,p_key text,p_owner text,p_generation bigint,
 p_table text,p_payload jsonb,p_conflict text,p_ignore boolean,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;cols text;conflicts text;updates text;clause text;field text;head text;j worker_jobs;
BEGIN
 PERFORM 1 FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT worker_guard(p_account,p_key,p_owner,p_generation) THEN RETURN false; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key;
 IF p_table<>ALL(ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history',
 'market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations',
 'market_observations','player_match_observations','league_rule_snapshots','player_round_history_checks',
 'rival_finance_evidence','complete_history_baselines','league_context_observations','player_membership_observations']) THEN RAISE EXCEPTION 'Invalid fact table'; END IF;
 IF p_observed IS NULL OR p_observed>now()+interval '60 seconds' THEN RAISE EXCEPTION 'Invalid source time'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
  IF jsonb_typeof(row_data)<>'object' OR octet_length(row_data::text)>1048576 THEN RAISE EXCEPTION 'Invalid fact row'; END IF;
  IF row_data ? 'account_id' AND row_data->>'account_id' IS DISTINCT FROM p_account THEN RAISE EXCEPTION 'Foreign fact account'; END IF;
  IF row_data ? 'championship_id' AND row_data->>'championship_id' IS DISTINCT FROM j.descriptor->'context'->>'championship_id'
    AND j.descriptor->>'handler'<>'get_active_championships' THEN RAISE EXCEPTION 'Foreign fact league'; END IF;
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
REVOKE ALL ON FUNCTION public.worker_write_fact(text,text,text,bigint,text,jsonb,text,boolean,timestamptz) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_write_fact(text,text,text,bigint,text,jsonb,text,boolean,timestamptz) TO service_role;
CREATE OR REPLACE FUNCTION public.worker_publish_child(p_account text,p_parent text,p_owner text,p_generation bigint,p_descriptor jsonb,p_manifest jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;k text:=p_descriptor->>'resource_key';
BEGIN
 PERFORM 1 FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT worker_guard(p_account,p_parent,p_owner,p_generation) THEN RETURN false; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_parent;
 IF k !~ '^[a-f0-9]{64}$' OR p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account
 OR p_manifest->'context' IS DISTINCT FROM p_descriptor->'context'
 OR (coalesce(p_descriptor->'context'->>'championship_id','')<>'' AND
 p_descriptor->'context'->>'championship_id' IS DISTINCT FROM j.descriptor->'context'->>'championship_id')
 THEN RAISE EXCEPTION 'Foreign child resource'; END IF;
 IF (SELECT count(*) FROM worker_chunks WHERE account_id=p_account AND resource_key=k AND version=(p_manifest->>'version')::uuid)<>(p_manifest->>'parts')::integer THEN RAISE EXCEPTION 'Incomplete child snapshot'; END IF;
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts)
 VALUES(p_account,k,p_descriptor->'context',p_descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,CASE WHEN j.requested_at>j.started_at THEN least((p_manifest->>'valid_until')::timestamptz,now()-interval '1 second') ELSE (p_manifest->>'valid_until')::timestamptz END,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts
 WHERE worker_snapshots.checked_at<=excluded.checked_at;
 RETURN true;
END $$;
REVOKE ALL ON FUNCTION public.worker_publish_child(text,text,text,bigint,jsonb,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_publish_child(text,text,text,bigint,jsonb,jsonb) TO service_role;

CREATE OR REPLACE FUNCTION public.worker_renew(p_account text,p_token text,p_credential text DEFAULT NULL)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;
BEGIN
 UPDATE worker_accounts SET encrypted_token=p_token,credential_id=p_credential,generation=generation+1,auth_status='active',updated_at=now()
 WHERE account_id=p_account AND enabled RETURNING * INTO a;
 IF a.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_jobs SET generation=a.generation,owner=NULL,lease_until=NULL,checkpoint='{}',run_id=gen_random_uuid(),attempts=0,last_error=NULL WHERE account_id=p_account;
 RETURN to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues';
END $$;
REVOKE ALL ON FUNCTION public.worker_renew(text,text,text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_renew(text,text,text) TO service_role;

COMMIT;
