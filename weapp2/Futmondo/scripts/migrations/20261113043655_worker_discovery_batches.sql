-- Bound discovery queue round trips; no source facts or snapshots are rewritten.
BEGIN;
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
 END LOOP;
 RETURN accepted;
END $$;
REVOKE ALL ON FUNCTION public.worker_enqueue_batch(text,text,bigint,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_enqueue_batch(text,text,bigint,jsonb) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
