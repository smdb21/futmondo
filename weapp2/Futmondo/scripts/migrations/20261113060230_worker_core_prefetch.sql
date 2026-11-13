-- Begin core refresh before source validity ends; never renew source time.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_next_refresh_due(p_handler text,p_interval integer,p_valid_until timestamptz,p_started timestamptz,p_now timestamptz DEFAULT now())
RETURNS timestamptz LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
 SELECT CASE WHEN public.worker_request_priority(p_handler)>=9 AND p_interval BETWEEN 15 AND 300 THEN
  CASE WHEN p_valid_until<=p_now THEN p_now+make_interval(secs=>least(15,p_interval/2))
  ELSE greatest(p_now+interval '5 seconds',least(p_now+make_interval(secs=>p_interval),
   p_valid_until-make_interval(secs=>least(p_interval/2.0,greatest(5,coalesce(extract(epoch from p_now-p_started),0)+5))::double precision))) END
 ELSE p_now+make_interval(secs=>p_interval) END;
$$;
REVOKE ALL ON FUNCTION public.worker_next_refresh_due(text,integer,timestamptz,timestamptz,timestamptz) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_next_refresh_due(text,integer,timestamptz,timestamptz,timestamptz) TO service_role;
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
 UPDATE worker_jobs SET owner=NULL,lease_until=NULL,checked_at=now(),due_at=CASE WHEN requested_at>started_at THEN now() WHEN p_manifest->>'status' IN ('ok','empty') THEN public.worker_next_refresh_due(j.descriptor->>'handler',interval_seconds,(p_manifest->>'valid_until')::timestamptz,started_at,now()) ELSE now()+make_interval(secs=>interval_seconds) END,
 priority=CASE WHEN requested_at>started_at THEN 10 ELSE base_priority END,attempts=0,last_error=p_manifest->>'error',last_audit_at=CASE WHEN p_manifest->>'full_audit'='true' THEN now() ELSE last_audit_at END,checkpoint='{}',run_id=gen_random_uuid() WHERE account_id=p_account AND resource_key=p_key;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND public.worker_finance_context_matches(q.descriptor,a.verified_leagues->(q.descriptor->'context'->>'championship_id'))
 AND (q.owner IS NULL OR q.lease_until<now()) AND (NOT p_foreground OR ((q.priority>=5 OR (q.priority>=3 AND public.worker_request_priority(q.descriptor->>'handler')>=9)) AND q.descriptor->>'handler' NOT IN
  ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot')))
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
 AND (q.priority>=5 OR q.checked_at IS NULL OR q.checked_at +
 make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE 0 END)<=now())
 ORDER BY q.priority DESC,a.last_dispatched NULLS FIRST,q.due_at,q.account_id,q.resource_key FOR UPDATE OF q SKIP LOCKED LIMIT 1;
 IF j.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
REVOKE ALL ON FUNCTION public.worker_publish(text,text,text,bigint,jsonb),public.worker_claim(text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_publish(text,text,text,bigint,jsonb),public.worker_claim(text,boolean) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
