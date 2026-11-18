-- Worker-owned cadence, durable login refresh, and combined maintenance.
BEGIN;
CREATE TABLE public.worker_schedule_policy(id integer PRIMARY KEY CHECK(id=1),base_interval integer NOT NULL CHECK(base_interval>0));
INSERT INTO public.worker_schedule_policy VALUES(1,1800);
ALTER TABLE public.worker_schedule_policy ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.worker_schedule_policy FROM PUBLIC,anon,authenticated;
GRANT ALL ON public.worker_schedule_policy TO service_role;
ALTER TABLE public.worker_jobs DROP CONSTRAINT worker_jobs_interval_seconds_check;
ALTER TABLE public.worker_jobs ADD CHECK(interval_seconds>=15);
ALTER TABLE public.worker_jobs ADD COLUMN native_interval_seconds integer;
UPDATE public.worker_jobs SET native_interval_seconds=interval_seconds;
ALTER TABLE public.worker_jobs ALTER COLUMN native_interval_seconds SET NOT NULL;
ALTER TABLE public.worker_accounts ADD COLUMN last_login_refresh timestamptz;
CREATE FUNCTION public.worker_interval_floor() RETURNS trigger LANGUAGE plpgsql SET search_path=public,pg_temp AS $$
DECLARE base integer;
BEGIN
 SELECT base_interval INTO base FROM worker_schedule_policy WHERE id=1;
 IF TG_OP='INSERT' THEN NEW.native_interval_seconds:=NEW.interval_seconds;
 ELSIF NEW.native_interval_seconds IS NOT DISTINCT FROM OLD.native_interval_seconds AND NEW.interval_seconds<>OLD.interval_seconds AND NEW.interval_seconds<>greatest(OLD.native_interval_seconds,base) THEN
 NEW.native_interval_seconds:=NEW.interval_seconds;
 END IF;
 NEW.interval_seconds:=greatest(NEW.native_interval_seconds,base);
 RETURN NEW;
END $$;
CREATE TRIGGER worker_interval_floor BEFORE INSERT OR UPDATE OF interval_seconds ON public.worker_jobs FOR EACH ROW EXECUTE FUNCTION public.worker_interval_floor();
CREATE OR REPLACE FUNCTION public.worker_next_refresh_due(p_handler text,p_interval integer,p_valid_until timestamptz,p_started timestamptz,p_now timestamptz DEFAULT now())
RETURNS timestamptz LANGUAGE sql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT p_now+make_interval(secs=>greatest(p_interval,(SELECT base_interval FROM worker_schedule_policy WHERE id=1)));
$$;
CREATE FUNCTION public.worker_login_refresh(p_account text) RETURNS void LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE worker_accounts SET last_login_refresh=now() WHERE account_id=p_account AND enabled AND auth_status='active'
 AND (last_login_refresh IS NULL OR last_login_refresh<=now()-interval '5 minutes');
 IF NOT FOUND THEN RETURN; END IF;
 UPDATE worker_jobs j SET due_at=least(j.due_at,now()),priority=greatest(j.priority,10)
 FROM worker_accounts a WHERE j.account_id=p_account AND a.account_id=j.account_id AND j.generation=a.generation
 AND j.owner IS NULL AND j.last_error IS NULL
 AND NOT(a.excluded_leagues ? coalesce(j.descriptor->'context'->>'championship_id',''))
 AND NOT EXISTS(SELECT 1 FROM blocked_leagues b WHERE b.championship_id=j.descriptor->'context'->>'championship_id')
 AND (j.native_interval_seconds<=300 AND j.descriptor->>'handler' NOT IN ('get_player_full_profile','get_player_matches','get_player_summary'));
END $$;
CREATE OR REPLACE FUNCTION public.worker_connect(p_account text,p_token text,p_credential text,p_generation bigint,p_timezone text DEFAULT 'UTC')
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;
BEGIN
 IF p_account IS NULL OR length(p_account) NOT BETWEEN 1 AND 256 OR p_token IS NULL OR length(p_token) NOT BETWEEN 1 AND 65536
 OR p_credential IS NULL OR length(p_credential) NOT BETWEEN 1 AND 128 OR p_generation IS NULL OR p_generation<0 THEN RAISE EXCEPTION 'Invalid connection'; END IF;
 IF NOT EXISTS(SELECT 1 FROM pg_timezone_names WHERE name=p_timezone) THEN RAISE EXCEPTION 'Invalid timezone'; END IF;
 IF p_generation=0 THEN
  INSERT INTO worker_accounts(account_id,enabled,encrypted_token,credential_id,timezone,quiet_start,quiet_end)
  VALUES(p_account,true,p_token,p_credential,p_timezone,0,0) ON CONFLICT DO NOTHING;
 END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR UPDATE;
 IF a.account_id IS NULL OR (a.generation<>p_generation AND NOT(p_generation=0 AND a.generation=1 AND a.enabled)) THEN RETURN NULL; END IF;
 -- Token rotation is not authorization revocation. Preserve every running job.
 UPDATE worker_accounts SET enabled=true,encrypted_token=p_token,credential_id=p_credential,
  auth_status='active',quiet_start=0,quiet_end=0,updated_at=now() WHERE account_id=p_account RETURNING * INTO a;
 UPDATE worker_jobs SET due_at=least(due_at,now()),last_error=NULL,attempts=0
 WHERE account_id=p_account AND generation=a.generation AND owner IS NULL AND last_error='source_authentication';
 PERFORM public.worker_login_refresh(p_account);
 RETURN to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues';
END $$;

CREATE FUNCTION public.worker_maintenance(p_owner text,p_base_interval integer,p_requests bigint,p_jobs bigint) RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE elected boolean; admitted boolean; old_base integer;
BEGIN
 IF p_base_interval<60 THEN RAISE EXCEPTION 'Invalid worker base interval'; END IF;
 INSERT INTO worker_leader(id,owner,lease_until) VALUES(1,p_owner,now()+interval '180 seconds')
 ON CONFLICT(id) DO UPDATE SET owner=excluded.owner,lease_until=excluded.lease_until
 WHERE worker_leader.owner=p_owner OR worker_leader.lease_until<now();
 elected:=FOUND;
 IF elected THEN
 SELECT base_interval INTO old_base FROM worker_schedule_policy WHERE id=1 FOR UPDATE;
 IF old_base<>p_base_interval THEN
 UPDATE worker_schedule_policy SET base_interval=p_base_interval WHERE id=1;
 UPDATE worker_jobs SET interval_seconds=greatest(native_interval_seconds,p_base_interval),
 due_at=CASE WHEN owner IS NULL AND last_error IS NULL THEN now()+make_interval(secs=>1+mod(abs(hashtext(resource_key)::bigint),greatest(native_interval_seconds,p_base_interval))) ELSE due_at END;
 END IF;
 END IF;
 SELECT coalesce((SELECT NOT admin_only FROM application_access WHERE id='main'),false) INTO admitted;
 INSERT INTO worker_health(id,status,heartbeat,requests,jobs) VALUES(p_owner,CASE WHEN elected THEN 'running' ELSE 'standby' END,now(),p_requests,p_jobs)
 ON CONFLICT(id) DO UPDATE SET status=excluded.status,heartbeat=excluded.heartbeat,requests=excluded.requests,jobs=excluded.jobs;
 RETURN jsonb_build_object('leader',elected,'admitted',admitted);
END $$;
UPDATE worker_jobs SET interval_seconds=greatest(native_interval_seconds,1800),
 due_at=CASE WHEN owner IS NULL AND last_error IS NULL THEN now()+make_interval(secs=>1+mod(abs(hashtext(resource_key)::bigint),greatest(native_interval_seconds,1800))) ELSE due_at END;
REVOKE ALL ON FUNCTION public.worker_interval_floor(),public.worker_login_refresh(text),public.worker_maintenance(text,integer,bigint,bigint) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_interval_floor(),public.worker_login_refresh(text),public.worker_maintenance(text,integer,bigint,bigint) TO service_role;
CREATE OR REPLACE FUNCTION public.worker_enqueue(p_account text,p_descriptor jsonb,p_interval integer DEFAULT 60,p_priority integer DEFAULT 10)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;k text:=p_descriptor->>'resource_key';
BEGIN
 -- Browser processes from the previous image may still send the old default.
 IF p_descriptor->>'handler'='get_player_recent_points' AND p_interval=60 THEN p_interval:=300; END IF;
 IF p_descriptor->>'handler'='get_clause_transfer_summary' THEN p_interval:=300; END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' THEN RETURN false; END IF;
 IF NOT public.worker_finance_context_matches(p_descriptor,a.verified_leagues->(p_descriptor->'context'->>'championship_id')) THEN RETURN false; END IF;
 IF EXISTS(SELECT 1 FROM public.blocked_leagues WHERE championship_id=p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account OR (p_descriptor->>'handler') !~ '^(get_|read_|fm_worker_finances$)' THEN RAISE EXCEPTION 'Invalid descriptor'; END IF;
 IF a.excluded_leagues ? (p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF NOT EXISTS(SELECT 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=k) AND
 (SELECT count(*) FROM worker_jobs WHERE account_id=p_account)>=10000 THEN RAISE EXCEPTION 'Worker job limit'; END IF;
 INSERT INTO worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds,priority,base_priority)
 VALUES(p_account,k,p_descriptor,a.generation,p_interval,greatest(p_priority,CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END),CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET
 due_at=CASE WHEN worker_jobs.owner IS NULL AND excluded.priority>=5 THEN least(worker_jobs.due_at,now())
  -- A newly observed roster/transfer can shorten an existing hourly summary
  -- cadence. Apply that cadence from its last completion instead of keeping
  -- the old hourly due date. Rediscovery at the same cadence is inert, and
  -- failed or running work retains its retry/lease schedule.
  WHEN worker_jobs.owner IS NULL AND worker_jobs.last_error IS NULL
   AND worker_jobs.checked_at IS NOT NULL AND excluded.interval_seconds<worker_jobs.interval_seconds
  THEN least(worker_jobs.due_at,worker_jobs.checked_at+make_interval(secs=>excluded.interval_seconds))
  ELSE worker_jobs.due_at END,
 requested_at=CASE WHEN excluded.priority>=5 THEN now() ELSE worker_jobs.requested_at END,priority=greatest(worker_jobs.priority,excluded.priority),base_priority=greatest(worker_jobs.base_priority,excluded.base_priority),native_interval_seconds=least(worker_jobs.native_interval_seconds,excluded.native_interval_seconds),interval_seconds=greatest(least(worker_jobs.native_interval_seconds,excluded.native_interval_seconds),(SELECT base_interval FROM worker_schedule_policy WHERE id=1));
 RETURN true;
END $$;

CREATE OR REPLACE FUNCTION public.worker_enqueue_batch(p_account text,p_owner text,p_generation bigint,p_descriptors jsonb)
RETURNS integer LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;item jsonb;d jsonb;seconds integer;accepted integer:=0;
BEGIN
 IF jsonb_typeof(p_descriptors) IS DISTINCT FROM 'array' THEN RAISE EXCEPTION 'Invalid discovery batch'; END IF;
 IF jsonb_array_length(p_descriptors)>128 OR octet_length(p_descriptors::text)>1048576 THEN RAISE EXCEPTION 'Discovery batch limit'; END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' OR a.generation<>p_generation
 OR NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 FOR item IN SELECT value FROM jsonb_array_elements(p_descriptors) LOOP
  d:=item-'interval_seconds';seconds:=coalesce((item->>'interval_seconds')::integer,300);
  IF jsonb_typeof(d) IS DISTINCT FROM 'object' OR d->'context'->>'account_id' IS DISTINCT FROM p_account
   OR coalesce(d->>'resource_key','') !~ '^[a-f0-9]{64}$'
   OR coalesce(d->>'handler','') !~ '^(get_|read_|fm_worker_finances$)' OR seconds NOT BETWEEN 15 AND 86400
  THEN RAISE EXCEPTION 'Invalid discovery descriptor'; END IF;
  IF a.excluded_leagues ? (d->'context'->>'championship_id')
   OR EXISTS(SELECT 1 FROM blocked_leagues WHERE championship_id=d->'context'->>'championship_id')
   OR NOT worker_finance_context_matches(d,a.verified_leagues->(d->'context'->>'championship_id')) THEN CONTINUE; END IF;
  -- Routine rediscovery must not rewrite thousands of unchanged durable jobs.
  IF EXISTS(SELECT 1 FROM worker_jobs j WHERE j.account_id=p_account AND j.resource_key=d->>'resource_key'
   AND j.generation=p_generation AND j.descriptor=d AND j.native_interval_seconds<=seconds
   AND j.base_priority>=CASE WHEN seconds<=60 THEN 4 WHEN seconds<=300 THEN 3 ELSE 1 END) THEN
   accepted:=accepted+1;
  ELSIF worker_enqueue(p_account,d,seconds,0) THEN accepted:=accepted+1;
  END IF;
  -- Account-only endpoints used to inherit browser league scope / named empty
  -- arguments. Retire duplicate jobs only after the canonical replacement is
  -- durably registered. Factual snapshots and source times are preserved.
  IF d->>'handler' IN ('get_active_championships','get_notifications','get_notification_unread')
   AND d->'args'='[]'::jsonb AND coalesce(d->'context'->>'championship_id','')=''
   AND EXISTS(SELECT 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=d->>'resource_key') THEN
   DELETE FROM worker_jobs WHERE account_id=p_account AND descriptor->>'handler'=d->>'handler'
    AND resource_key<>d->>'resource_key' AND owner IS NULL;
  END IF;
 END LOOP;
 DELETE FROM worker_jobs old WHERE old.account_id=p_account AND old.owner IS NULL
 AND old.descriptor->>'handler' IN ('get_active_championships','get_notifications','get_notification_unread')
 AND old.resource_key<>public.worker_account_descriptor(p_account,old.descriptor->>'handler')->>'resource_key'
 AND EXISTS(SELECT 1 FROM worker_jobs current WHERE current.account_id=p_account
  AND current.resource_key=public.worker_account_descriptor(p_account,old.descriptor->>'handler')->>'resource_key');
 -- Card history now comes from summary publication and existing DB readers.
 DELETE FROM worker_jobs WHERE account_id=p_account AND descriptor->>'handler'='get_player_card_history' AND owner IS NULL;
 RETURN accepted;
END $$;
NOTIFY pgrst,'reload schema';
COMMIT;
