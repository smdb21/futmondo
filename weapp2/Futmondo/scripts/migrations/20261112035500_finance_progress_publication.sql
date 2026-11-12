-- Order progress within its leased run without renewing source freshness.
BEGIN;
ALTER TABLE public.worker_snapshots ADD COLUMN IF NOT EXISTS run_id uuid;
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
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id)
 VALUES(p_account,p_key,p_manifest->'context',j.descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,CASE WHEN j.requested_at>j.started_at THEN least((p_manifest->>'valid_until')::timestamptz,now()-interval '1 second') ELSE (p_manifest->>'valid_until')::timestamptz END,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer,j.run_id)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts,run_id=excluded.run_id
 WHERE worker_snapshots.checked_at<=excluded.checked_at OR worker_snapshots.run_id=j.run_id
 OR (j.descriptor->>'handler'='fm_worker_finances' AND worker_snapshots.resource_key=j.resource_key);
 UPDATE worker_jobs SET owner=NULL,lease_until=NULL,checked_at=now(),due_at=CASE WHEN requested_at>started_at THEN now() ELSE now()+make_interval(secs=>interval_seconds) END,
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
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id)
 VALUES(p_account,k,p_descriptor->'context',p_descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,CASE WHEN j.requested_at>j.started_at THEN least((p_manifest->>'valid_until')::timestamptz,now()-interval '1 second') ELSE (p_manifest->>'valid_until')::timestamptz END,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer,j.run_id)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts,run_id=excluded.run_id
 WHERE worker_snapshots.checked_at<=excluded.checked_at OR worker_snapshots.run_id=j.run_id
 OR (j.descriptor->>'handler'='fm_worker_finances' AND worker_snapshots.resource_key=j.resource_key);
 RETURN true;
END $$;
REVOKE ALL ON FUNCTION public.worker_publish(text,text,text,bigint,jsonb),public.worker_publish_child(text,text,text,bigint,jsonb,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_publish(text,text,text,bigint,jsonb),public.worker_publish_child(text,text,text,bigint,jsonb,jsonb) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
