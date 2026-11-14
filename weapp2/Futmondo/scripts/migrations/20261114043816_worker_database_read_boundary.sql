-- Acquisition belongs to the autonomous worker; browser lookups never create jobs.
BEGIN;
-- Enrollment seeds one source job in the database; login/page rendering does not.
CREATE OR REPLACE FUNCTION public.worker_account_descriptor(p_account text,p_handler text DEFAULT 'get_active_championships')
RETURNS jsonb LANGUAGE plpgsql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
DECLARE wire text;
BEGIN
 IF p_handler NOT IN ('get_active_championships','get_notifications','get_notification_unread') THEN RAISE EXCEPTION 'Invalid account source'; END IF;
 wire:=format('{"args":[],"context":{"account_id":%s,"championship_id":"","competition_id":"unknown","player_id":"","scoring_version":"unknown","season":"unknown","user_team_id":""},"encoded_args":{"context":false,"kind":"list","meta":[],"names":null,"values":[]},"handler":%s,"version":1}',to_json(p_account)::text,to_json(p_handler)::text);
 RETURN wire::jsonb || jsonb_build_object('resource_key',encode(sha256(convert_to(wire,'UTF8')),'hex'));
END $$;
CREATE OR REPLACE FUNCTION public.worker_seed_enrollment()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE d jsonb;
BEGIN
 IF NEW.enabled AND NEW.auth_status='active' THEN
  d:=public.worker_account_descriptor(NEW.account_id);
  INSERT INTO worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds,priority,base_priority)
   VALUES(NEW.account_id,d->>'resource_key',d,NEW.generation,300,3,3) ON CONFLICT DO NOTHING;
 END IF;
 RETURN NEW;
END $$;
REVOKE ALL ON FUNCTION public.worker_account_descriptor(text,text),public.worker_seed_enrollment() FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_account_descriptor(text,text),public.worker_seed_enrollment() TO service_role;
DROP TRIGGER IF EXISTS worker_seed_enrollment ON public.worker_accounts;
CREATE TRIGGER worker_seed_enrollment AFTER INSERT OR UPDATE OF enabled,auth_status,generation ON public.worker_accounts
 FOR EACH ROW EXECUTE FUNCTION public.worker_seed_enrollment();
INSERT INTO public.worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds,priority,base_priority)
 SELECT a.account_id,d->>'resource_key',d,a.generation,300,3,3 FROM public.worker_accounts a
 CROSS JOIN LATERAL (SELECT public.worker_account_descriptor(a.account_id) d) bootstrap
 WHERE a.enabled AND a.auth_status='active' ON CONFLICT DO NOTHING;
ALTER TABLE public.worker_snapshots ADD COLUMN IF NOT EXISTS descriptor jsonb;
UPDATE public.worker_snapshots s SET descriptor=j.descriptor FROM public.worker_jobs j
 WHERE s.descriptor IS NULL AND s.account_id=j.account_id AND s.resource_key=j.resource_key
 AND s.context=j.descriptor->'context' AND s.handler=j.descriptor->>'handler';
CREATE INDEX IF NOT EXISTS worker_snapshots_peer_scope ON public.worker_snapshots(account_id,handler,md5((descriptor->'args')::text)) WHERE descriptor IS NOT NULL;
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
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id,descriptor)
 VALUES(p_account,p_key,p_manifest->'context',j.descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,CASE WHEN j.requested_at>j.started_at THEN least((p_manifest->>'valid_until')::timestamptz,now()-interval '1 second') ELSE (p_manifest->>'valid_until')::timestamptz END,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer,j.run_id,j.descriptor)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts,run_id=excluded.run_id,descriptor=excluded.descriptor
 WHERE worker_snapshots.checked_at<=excluded.checked_at OR worker_snapshots.run_id=j.run_id
 OR (j.descriptor->>'handler'='fm_worker_finances' AND worker_snapshots.resource_key=j.resource_key);
 UPDATE worker_jobs SET owner=NULL,lease_until=NULL,checked_at=now(),due_at=CASE WHEN requested_at>started_at THEN now() WHEN p_manifest->>'status' IN ('ok','empty') THEN public.worker_next_refresh_due(j.descriptor->>'handler',interval_seconds,(p_manifest->>'valid_until')::timestamptz,started_at,now()) ELSE now()+make_interval(secs=>interval_seconds) END,
 priority=CASE WHEN requested_at>started_at THEN 10 ELSE base_priority END,attempts=0,last_error=p_manifest->>'error',last_audit_at=CASE WHEN p_manifest->>'full_audit'='true' THEN now() ELSE last_audit_at END,checkpoint='{}',run_id=gen_random_uuid() WHERE account_id=p_account AND resource_key=p_key;
 RETURN true;
END $$;
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
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id,descriptor)
 VALUES(p_account,k,p_descriptor->'context',p_descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,CASE WHEN j.requested_at>j.started_at THEN least((p_manifest->>'valid_until')::timestamptz,now()-interval '1 second') ELSE (p_manifest->>'valid_until')::timestamptz END,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer,j.run_id,p_descriptor)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts,run_id=excluded.run_id,descriptor=excluded.descriptor
 WHERE worker_snapshots.checked_at<=excluded.checked_at OR worker_snapshots.run_id=j.run_id
 OR (j.descriptor->>'handler'='fm_worker_finances' AND worker_snapshots.resource_key=j.resource_key);
 IF j.descriptor->>'handler'='get_player_summary' AND p_descriptor->>'handler' IN ('get_clause_transfer_summary','get_player_recent_points')
 AND p_manifest->>'status' IN ('ok','empty') THEN
  DELETE FROM worker_jobs WHERE account_id=p_account AND resource_key=k AND owner IS NULL;
 END IF;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.worker_resolved_peer(p_account text,p_descriptor jsonb)
RETURNS jsonb LANGUAGE plpgsql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 IF p_descriptor IS NULL OR p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account
 OR NOT EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
 WHERE coalesce(p_descriptor->'context'->>f,'unknown') IN ('','unknown')) THEN RETURN NULL; END IF;
 RETURN (WITH possible AS (
  SELECT j.descriptor FROM worker_jobs j JOIN worker_accounts a USING(account_id)
  WHERE j.account_id=p_account AND j.generation=a.generation
   AND j.descriptor->>'handler'=p_descriptor->>'handler'
   AND md5((j.descriptor->'args')::text)=md5((p_descriptor->'args')::text)
  UNION
  SELECT s.descriptor FROM worker_snapshots s WHERE s.account_id=p_account AND s.descriptor IS NOT NULL
   AND s.handler=p_descriptor->>'handler' AND md5((s.descriptor->'args')::text)=md5((p_descriptor->'args')::text)
 ), candidates AS (
 SELECT peer.descriptor FROM possible peer JOIN worker_accounts a ON a.account_id=p_account
 WHERE p_descriptor->'context'->>'account_id'=p_account AND a.enabled AND a.auth_status='active'
 AND EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
   WHERE coalesce(p_descriptor->'context'->>f,'unknown') IN ('','unknown'))
 AND peer.descriptor->'version'=p_descriptor->'version'
 AND peer.descriptor->'args'=p_descriptor->'args'
 AND ((peer.descriptor->'context')-ARRAY['competition_id','season','scoring_version']) =
     ((p_descriptor->'context')-ARRAY['competition_id','season','scoring_version'])
 AND NOT EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
   WHERE coalesce(peer.descriptor->'context'->>f,'unknown') IN ('','unknown')
     OR coalesce(a.verified_leagues->(peer.descriptor->'context'->>'championship_id')->>f,'unknown') IN ('','unknown')
     OR peer.descriptor->'context'->>f IS DISTINCT FROM a.verified_leagues->(peer.descriptor->'context'->>'championship_id')->>f
     OR (coalesce(p_descriptor->'context'->>f,'unknown') NOT IN ('','unknown')
       AND peer.descriptor->'context'->>f IS DISTINCT FROM p_descriptor->'context'->>f))
 AND NOT(a.excluded_leagues ? (peer.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM blocked_leagues b WHERE b.championship_id=peer.descriptor->'context'->>'championship_id')
 LIMIT 2)
 SELECT CASE WHEN count(*)=1 THEN (jsonb_agg(descriptor))->0 ELSE NULL END FROM candidates);
END $$;
-- Replace the old signature: legacy two-argument callers still use the default.
DROP FUNCTION IF EXISTS public.worker_read(text,jsonb);
CREATE OR REPLACE FUNCTION public.worker_read(p_account text,p_versions jsonb DEFAULT '{}',p_descriptors jsonb DEFAULT '{}')
RETURNS jsonb LANGUAGE plpgsql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 IF jsonb_typeof(p_versions) IS DISTINCT FROM 'object' OR jsonb_typeof(p_descriptors) IS DISTINCT FROM 'object'
 OR octet_length(p_versions::text)>65536 OR octet_length(p_descriptors::text)>1048576
 OR (SELECT count(*) FROM jsonb_object_keys(p_versions))>128
 OR (SELECT count(*) FROM jsonb_object_keys(p_descriptors))>128 THEN RAISE EXCEPTION 'Invalid read request'; END IF;
 IF EXISTS(SELECT 1 FROM jsonb_each(p_descriptors) d WHERE d.key !~ '^[a-f0-9]{64}$'
  OR d.value->>'resource_key' IS DISTINCT FROM d.key OR d.value->'context'->>'account_id' IS DISTINCT FROM p_account
  OR coalesce(d.value->>'handler','') !~ '^(get_|read_|fm_worker_finances$)') THEN RAISE EXCEPTION 'Foreign read descriptor'; END IF;
 RETURN (
 WITH requested AS MATERIALIZED (SELECT key,value FROM jsonb_each_text(p_versions)),
 routes AS MATERIALIZED (
  SELECT r.key requested_key,coalesce(peer.descriptor->>'resource_key',r.key) key,peer.descriptor
  FROM requested r LEFT JOIN worker_jobs j ON j.account_id=p_account AND j.resource_key=r.key
  LEFT JOIN LATERAL (SELECT public.worker_resolved_peer(p_account,coalesce(p_descriptors->r.key,j.descriptor)) descriptor) peer ON true),
 targets AS MATERIALIZED (SELECT DISTINCT key FROM routes)
 SELECT jsonb_build_object(
 'account',(SELECT to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues' FROM worker_accounts a WHERE account_id=p_account),
 'health',(SELECT to_jsonb(h) FROM worker_health h WHERE status='running' ORDER BY heartbeat DESC LIMIT 1),
 'redirects',coalesce((SELECT jsonb_agg(jsonb_build_object('requested_key',requested_key,'descriptor',descriptor)) FROM routes WHERE descriptor IS NOT NULL),'[]'),
 'resources',coalesce((SELECT jsonb_agg(jsonb_build_object('key',s.resource_key,'manifest',to_jsonb(s)-'descriptor',
 'chunks',(SELECT jsonb_agg(c.data ORDER BY c.part) FROM worker_chunks c WHERE c.account_id=s.account_id AND c.resource_key=s.resource_key AND c.version=s.version),
 'error',j.last_error)) FROM worker_snapshots s JOIN targets t ON t.key=s.resource_key LEFT JOIN worker_jobs j USING(account_id,resource_key)
 WHERE s.account_id=p_account AND s.version::text IS DISTINCT FROM p_versions->>s.resource_key),'[]'),
 'jobs',coalesce((SELECT jsonb_agg(jsonb_build_object('key',j.resource_key,'error',j.last_error,
 'running',j.owner IS NOT NULL AND j.lease_until>now(),'pending',(j.owner IS NOT NULL AND j.lease_until>now()) OR j.due_at<=now(),
 'next_due_at',j.due_at)) FROM worker_jobs j JOIN targets t ON t.key=j.resource_key
 JOIN worker_accounts a ON a.account_id=j.account_id WHERE j.account_id=p_account AND j.generation=a.generation
 AND a.enabled AND a.auth_status='active' AND NOT(a.excluded_leagues ? (j.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM blocked_leagues b WHERE b.championship_id=j.descriptor->'context'->>'championship_id')),'[]')));
END $$;
REVOKE ALL ON FUNCTION public.worker_read(text,jsonb,jsonb),public.worker_resolved_peer(text,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_read(text,jsonb,jsonb),public.worker_resolved_peer(text,jsonb) TO service_role;
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
   AND j.generation=p_generation AND j.descriptor=d AND j.interval_seconds<=seconds
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
