-- Active finance preparation must match verified current league context.
-- Retire unresolved work without deleting observations, snapshots or checkpoints.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_finance_context_matches(p_descriptor jsonb,p_membership jsonb)
RETURNS boolean LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
 SELECT CASE WHEN p_descriptor->>'handler'<>'fm_worker_finances' THEN true ELSE coalesce(
  jsonb_typeof(p_membership)='object'
  AND coalesce(p_membership->>'competition_id','unknown') NOT IN ('','unknown')
  AND coalesce(p_membership->>'season','unknown') NOT IN ('','unknown')
  AND p_descriptor->'context'->>'competition_id'=p_membership->>'competition_id'
  AND p_descriptor->'context'->>'season'=p_membership->>'season'
  AND coalesce(p_descriptor->'context'->>'user_team_id','')<>''
  AND (coalesce(p_membership->>'scoring_version','unknown') IN ('','unknown')
   OR p_descriptor->'context'->>'scoring_version'=p_membership->>'scoring_version'),false) END;
$$;
REVOKE ALL ON FUNCTION public.worker_finance_context_matches(jsonb,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_finance_context_matches(jsonb,jsonb) TO service_role;
CREATE OR REPLACE FUNCTION public.worker_enqueue(p_account text,p_descriptor jsonb,p_interval integer DEFAULT 60,p_priority integer DEFAULT 10)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;k text:=p_descriptor->>'resource_key';
BEGIN
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
 AND public.worker_finance_context_matches(q.descriptor,a.verified_leagues->(q.descriptor->'context'->>'championship_id'))
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
CREATE OR REPLACE FUNCTION public.worker_guard(p_account text,p_key text,p_owner text,p_generation bigint,p_checkpoint jsonb DEFAULT NULL)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE worker_jobs j SET lease_until=now()+interval '60 seconds',checkpoint=coalesce(p_checkpoint,j.checkpoint)
 FROM worker_accounts a WHERE j.account_id=p_account AND j.resource_key=p_key AND j.owner=p_owner
 AND j.lease_until>now() AND a.account_id=j.account_id AND a.enabled AND a.auth_status='active'
 AND a.generation=p_generation AND j.generation=p_generation
 AND public.worker_finance_context_matches(j.descriptor,a.verified_leagues->(j.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=j.descriptor->'context'->>'championship_id')
 AND EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now());
 RETURN FOUND;
END $$;
REVOKE ALL ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer),public.worker_claim(text,boolean),public.worker_guard(text,text,text,bigint,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer),public.worker_claim(text,boolean),public.worker_guard(text,text,text,bigint,jsonb) TO service_role;
UPDATE public.worker_jobs j SET owner=NULL,lease_until=NULL,due_at='infinity'::timestamptz,last_error='context_unresolved'
 FROM public.worker_accounts a WHERE a.account_id=j.account_id AND j.descriptor->>'handler'='fm_worker_finances'
 AND NOT public.worker_finance_context_matches(j.descriptor,a.verified_leagues->(j.descriptor->'context'->>'championship_id'));
NOTIFY pgrst,'reload schema';
COMMIT;
