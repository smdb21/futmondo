-- Football-only application scope and durable last selected league per account.
BEGIN;
CREATE TABLE IF NOT EXISTS public.blocked_leagues (
 championship_id text PRIMARY KEY,
 sport text NOT NULL CHECK(sport IN ('basket','basketball','baloncesto')),
 blocked_at timestamptz NOT NULL DEFAULT now()
);
ALTER TABLE public.blocked_leagues ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.blocked_leagues FROM PUBLIC,anon,authenticated,service_role;
GRANT SELECT,INSERT ON public.blocked_leagues TO service_role;
INSERT INTO public.blocked_leagues(championship_id,sport)
 SELECT id,lower(btrim(sport)) FROM public.championships
 WHERE lower(btrim(coalesce(sport,''))) IN ('basket','basketball','baloncesto')
 ON CONFLICT DO NOTHING;

CREATE TABLE IF NOT EXISTS public.user_league_preferences (
 user_id text PRIMARY KEY CHECK(length(user_id) BETWEEN 1 AND 256 AND user_id ~ '^[A-Za-z0-9_-]+$'),
 championship_id text NOT NULL REFERENCES public.championships(id) ON DELETE CASCADE,
 selected_at timestamptz NOT NULL CHECK(isfinite(selected_at)),
 recorded_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
ALTER TABLE public.user_league_preferences ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.user_league_preferences FROM PUBLIC,anon,authenticated,service_role;
GRANT SELECT,INSERT,UPDATE ON public.user_league_preferences TO service_role;
CREATE OR REPLACE FUNCTION public.keep_latest_user_league()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path='' AS $$
BEGIN
 IF NEW.selected_at < OLD.selected_at THEN RETURN OLD; END IF;
 NEW.recorded_at:=clock_timestamp();RETURN NEW;
END $$;
REVOKE ALL ON FUNCTION public.keep_latest_user_league() FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.keep_latest_user_league() TO service_role;
DROP TRIGGER IF EXISTS keep_latest_user_league ON public.user_league_preferences;
CREATE TRIGGER keep_latest_user_league BEFORE UPDATE ON public.user_league_preferences
 FOR EACH ROW EXECUTE FUNCTION public.keep_latest_user_league();

-- A minimal ID/sport tombstone prevents old sessions recreating removed data.
CREATE OR REPLACE FUNCTION public.block_basketball_observation()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path='' AS $$
BEGIN
 IF TG_TABLE_NAME='championships' THEN
  IF lower(btrim(coalesce(NEW.sport,''))) IN ('basket','basketball','baloncesto') THEN
   INSERT INTO public.blocked_leagues(championship_id,sport) VALUES(NEW.id,lower(btrim(NEW.sport))) ON CONFLICT DO NOTHING;
   RETURN NULL;
  END IF;
  IF EXISTS(SELECT 1 FROM public.blocked_leagues WHERE championship_id=NEW.id) THEN RETURN NULL; END IF;
 ELSE
  IF EXISTS(SELECT 1 FROM public.blocked_leagues WHERE championship_id=NEW.championship_id) THEN
   IF TG_TABLE_NAME='user_league_preferences' THEN RAISE EXCEPTION 'Unsupported league preference' USING ERRCODE='23514'; END IF;
   RETURN NULL;
  END IF;
 END IF;
 RETURN NEW;
END $$;
REVOKE ALL ON FUNCTION public.block_basketball_observation() FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.block_basketball_observation() TO service_role;
DROP TRIGGER IF EXISTS block_basketball_observation ON public.championships;
CREATE TRIGGER block_basketball_observation BEFORE INSERT OR UPDATE ON public.championships
 FOR EACH ROW EXECUTE FUNCTION public.block_basketball_observation();
DO $$ DECLARE tbl text;
BEGIN
 FOR tbl IN SELECT t.table_name FROM information_schema.tables t JOIN information_schema.columns c
  ON c.table_schema=t.table_schema AND c.table_name=t.table_name AND c.column_name='championship_id'
  WHERE t.table_schema='public' AND t.table_type='BASE TABLE' AND t.table_name<>'blocked_leagues' LOOP
  EXECUTE format('DROP TRIGGER IF EXISTS block_basketball_observation ON public.%I',tbl);
  EXECUTE format('CREATE TRIGGER block_basketball_observation BEFORE INSERT OR UPDATE ON public.%I FOR EACH ROW EXECUTE FUNCTION public.block_basketball_observation()',tbl);
 END LOOP;
END $$;

-- Use existing consent-aware queue admission and lease guards; no new worker.
CREATE OR REPLACE FUNCTION public.worker_enqueue(p_account text,p_descriptor jsonb,p_interval integer DEFAULT 60,p_priority integer DEFAULT 10)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;k text:=p_descriptor->>'resource_key';
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' THEN RETURN false; END IF;
 IF EXISTS(SELECT 1 FROM public.blocked_leagues WHERE championship_id=p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
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
CREATE OR REPLACE FUNCTION public.worker_guard(p_account text,p_key text,p_owner text,p_generation bigint,p_checkpoint jsonb DEFAULT NULL)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE worker_jobs j SET lease_until=now()+interval '60 seconds',checkpoint=coalesce(p_checkpoint,j.checkpoint)
 FROM worker_accounts a WHERE j.account_id=p_account AND j.resource_key=p_key AND j.owner=p_owner
 AND j.lease_until>now() AND a.account_id=j.account_id AND a.enabled AND a.auth_status='active'
 AND a.generation=p_generation AND j.generation=p_generation
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=j.descriptor->'context'->>'championship_id')
 AND EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now());
 RETURN FOUND;
END $$;

REVOKE ALL ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer),public.worker_guard(text,text,text,bigint,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer),public.worker_guard(text,text,text,bigint,jsonb) TO service_role;

CREATE OR REPLACE FUNCTION public.worker_publish(p_account text,p_key text,p_owner text,p_generation bigint,p_manifest jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;a worker_accounts;
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key FOR UPDATE;
 IF a.account_id IS NULL OR j.account_id IS NULL OR NOT a.enabled OR a.generation<>p_generation OR a.auth_status<>'active' OR j.owner IS NULL OR j.lease_until IS NULL OR j.owner IS DISTINCT FROM p_owner OR j.lease_until<=now() OR NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN false; END IF;
 IF (SELECT count(*) FROM worker_chunks WHERE account_id=p_account AND resource_key=p_key AND version=(p_manifest->>'version')::uuid)<>(p_manifest->>'parts')::integer THEN RAISE EXCEPTION 'Incomplete snapshot'; END IF;
 IF j.descriptor->>'handler'='get_active_championships' AND jsonb_typeof(p_manifest->'memberships')='object' THEN
 UPDATE worker_accounts SET verified_leagues=coalesce((SELECT jsonb_object_agg(key,value) FROM jsonb_each(p_manifest->'memberships') m WHERE NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=m.key)),'{}'::jsonb) WHERE account_id=p_account;
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
REVOKE ALL ON FUNCTION public.worker_publish(text,text,text,bigint,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_publish(text,text,text,bigint,jsonb) TO service_role;

CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND (q.owner IS NULL OR q.lease_until<now()) AND (NOT p_foreground OR q.priority>=5)
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
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
REVOKE ALL ON FUNCTION public.worker_claim(text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_claim(text,boolean) TO service_role;

-- Reversible blocking only. Stored league data remains until separately approved cleanup.
DO $$ DECLARE blocked text[];affected text[];tbl text;
BEGIN
 SELECT array_agg(championship_id) INTO blocked FROM public.blocked_leagues;
 IF blocked IS NULL THEN RETURN; END IF;
 SELECT array_agg(account_id) INTO affected FROM public.worker_accounts a
 WHERE EXISTS(SELECT 1 FROM unnest(blocked) id WHERE a.verified_leagues ? id)
 OR EXISTS(SELECT 1 FROM public.worker_jobs j WHERE j.account_id=a.account_id AND j.descriptor->'context'->>'championship_id'=ANY(blocked));
 UPDATE public.worker_accounts a SET verified_leagues=a.verified_leagues-blocked,
  excluded_leagues=(SELECT jsonb_agg(DISTINCT value) FROM jsonb_array_elements(a.excluded_leagues||to_jsonb(blocked)))
 WHERE a.account_id=ANY(affected);
END $$;
NOTIFY pgrst,'reload schema';
COMMIT;
