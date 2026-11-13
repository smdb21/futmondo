-- Resolve obsolete unknown scopes without relabelling persisted observations.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_resolved_peer(p_account text,p_descriptor jsonb)
RETURNS jsonb LANGUAGE sql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
 WITH candidates AS (
 SELECT peer.descriptor FROM worker_jobs peer JOIN worker_accounts a USING(account_id)
 WHERE p_descriptor->>'handler' IN ('get_players_from_team','get_championship_pressroom','get_clause_transfer_summary')
 AND p_descriptor->'context'->>'account_id'=p_account
 AND EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
   WHERE coalesce(p_descriptor->'context'->>f,'unknown') IN ('','unknown'))
 AND peer.account_id=p_account AND peer.generation=a.generation AND a.enabled AND a.auth_status='active'
 AND peer.descriptor->'version'=p_descriptor->'version'
 AND peer.descriptor->>'handler'=p_descriptor->>'handler'
 AND md5((peer.descriptor->'args')::text)=md5((p_descriptor->'args')::text)
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
 SELECT CASE WHEN count(*)=1 THEN (jsonb_agg(descriptor))->0 ELSE NULL END FROM candidates;
$$;
CREATE OR REPLACE FUNCTION public.worker_has_resolved_peer(p_account text,p_descriptor jsonb)
RETURNS boolean LANGUAGE sql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT public.worker_resolved_peer(p_account,p_descriptor) IS NOT NULL;
$$;
CREATE OR REPLACE FUNCTION public.worker_read(p_account text,p_versions jsonb DEFAULT '{}')
RETURNS jsonb LANGUAGE sql SECURITY INVOKER SET search_path=public,pg_temp AS $$
 WITH requested AS MATERIALIZED (SELECT key,value FROM jsonb_each_text(p_versions) LIMIT 128),
 routes AS MATERIALIZED (
  SELECT r.key requested_key,coalesce(peer.descriptor->>'resource_key',r.key) key,peer.descriptor
  FROM requested r LEFT JOIN worker_jobs j ON j.account_id=p_account AND j.resource_key=r.key
  LEFT JOIN LATERAL (SELECT public.worker_resolved_peer(p_account,j.descriptor) descriptor) peer ON true),
 targets AS MATERIALIZED (SELECT DISTINCT key FROM routes)
 SELECT jsonb_build_object(
 'account',(SELECT to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues' FROM worker_accounts a WHERE account_id=p_account),
 'health',(SELECT to_jsonb(h) FROM worker_health h WHERE status='running' ORDER BY heartbeat DESC LIMIT 1),
 'redirects',coalesce((SELECT jsonb_agg(jsonb_build_object('requested_key',requested_key,'descriptor',descriptor)) FROM routes WHERE descriptor IS NOT NULL),'[]'),
 'resources',coalesce((SELECT jsonb_agg(jsonb_build_object('key',s.resource_key,'manifest',to_jsonb(s),
 'chunks',(SELECT jsonb_agg(c.data ORDER BY c.part) FROM worker_chunks c WHERE c.account_id=s.account_id AND c.resource_key=s.resource_key AND c.version=s.version),
 'error',j.last_error)) FROM worker_snapshots s JOIN targets t ON t.key=s.resource_key LEFT JOIN worker_jobs j USING(account_id,resource_key)
 WHERE s.account_id=p_account AND s.version::text IS DISTINCT FROM p_versions->>s.resource_key),'[]'),
 'jobs',coalesce((SELECT jsonb_agg(jsonb_build_object('key',j.resource_key,'error',j.last_error,
 'running',j.owner IS NOT NULL AND j.lease_until>now(),'pending',(j.owner IS NOT NULL AND j.lease_until>now()) OR j.due_at<=now()))
 FROM worker_jobs j JOIN targets t ON t.key=j.resource_key WHERE j.account_id=p_account),'[]'));
$$;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND public.worker_finance_context_matches(q.descriptor,a.verified_leagues->(q.descriptor->'context'->>'championship_id'))
 AND NOT public.worker_has_resolved_peer(q.account_id,q.descriptor)
 AND (q.owner IS NULL OR q.lease_until<now()) AND (NOT p_foreground OR ((q.priority>=5 OR (q.priority>=3 AND (public.worker_request_priority(q.descriptor->>'handler')>=9 OR q.descriptor->>'handler'='get_clause_transfer_summary'))) AND q.descriptor->>'handler' NOT IN
  ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot')))
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
 AND (q.priority>=5 OR q.checked_at IS NULL OR q.checked_at +
 make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE 0 END)<=now())
 ORDER BY CASE
 WHEN p_foreground AND q.priority<5 AND public.worker_request_priority(q.descriptor->>'handler')>=9 THEN 9
 WHEN p_foreground AND q.descriptor->>'handler'='get_clause_transfer_summary' THEN greatest(q.priority,8)
 ELSE q.priority END DESC,a.last_dispatched NULLS FIRST,q.due_at,q.account_id,q.resource_key FOR UPDATE OF q SKIP LOCKED LIMIT 1;
 IF j.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
REVOKE ALL ON FUNCTION public.worker_resolved_peer(text,jsonb),public.worker_has_resolved_peer(text,jsonb),public.worker_read(text,jsonb),public.worker_claim(text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_resolved_peer(text,jsonb),public.worker_has_resolved_peer(text,jsonb),public.worker_read(text,jsonb),public.worker_claim(text,boolean) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
