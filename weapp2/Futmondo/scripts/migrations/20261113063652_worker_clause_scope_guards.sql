-- Protect visible prerequisite reads from long finance and history work.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_request_priority(p_handler text)
RETURNS integer LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
 SELECT CASE WHEN p_handler IN ('get_players_from_team','get_championship_pressroom','get_market_players',
  'get_my_market_players','get_user_team_info','get_roster_bids','get_lineup_from_team',
  'get_notifications','get_notification_unread','get_active_championships','get_clause_transfer_summary') THEN 10
 WHEN p_handler IN ('get_resolved_league_context','get_finished_rounds') THEN 9
 WHEN p_handler IN ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot') THEN 8
 ELSE 6 END;
$$;
REVOKE ALL ON FUNCTION public.worker_request_priority(text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_request_priority(text) TO service_role;
CREATE INDEX IF NOT EXISTS worker_jobs_peer_scope ON public.worker_jobs(account_id,(descriptor->>'handler'),md5((descriptor->'args')::text));
CREATE OR REPLACE FUNCTION public.worker_has_resolved_peer(p_account text,p_descriptor jsonb)
RETURNS boolean LANGUAGE sql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT p_descriptor->>'handler' IN ('get_players_from_team','get_championship_pressroom')
 AND EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
   WHERE coalesce(p_descriptor->'context'->>f,'unknown') IN ('','unknown'))
 AND EXISTS(SELECT 1 FROM worker_jobs peer JOIN worker_accounts a USING(account_id)
   WHERE peer.account_id=p_account AND peer.generation=a.generation
   AND peer.descriptor->>'handler'=p_descriptor->>'handler'
   AND md5((peer.descriptor->'args')::text)=md5((p_descriptor->'args')::text)
   AND peer.descriptor->'args'=p_descriptor->'args'
   AND ((peer.descriptor->'context') - ARRAY['competition_id','season','scoring_version']) =
       ((p_descriptor->'context') - ARRAY['competition_id','season','scoring_version'])
   AND NOT EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
     WHERE coalesce(peer.descriptor->'context'->>f,'unknown') IN ('','unknown')
       OR (coalesce(p_descriptor->'context'->>f,'unknown') NOT IN ('','unknown')
         AND peer.descriptor->'context'->>f IS DISTINCT FROM p_descriptor->'context'->>f))
   AND public.worker_finance_context_matches(peer.descriptor,a.verified_leagues->(peer.descriptor->'context'->>'championship_id')));
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
CREATE OR REPLACE FUNCTION public.worker_enqueue(p_account text,p_descriptor jsonb,p_interval integer DEFAULT 60,p_priority integer DEFAULT 10)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;k text:=p_descriptor->>'resource_key';
BEGIN
 -- Browser processes from the previous image may still send the old default.
 IF p_descriptor->>'handler'='get_player_recent_points' AND p_interval=60 THEN p_interval:=300; END IF;
 IF p_descriptor->>'handler'='get_clause_transfer_summary' THEN p_interval:=60; END IF;
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
UPDATE public.worker_jobs SET interval_seconds=60,base_priority=4,priority=greatest(priority,10),due_at=least(due_at,now())
 WHERE descriptor->>'handler'='get_clause_transfer_summary';
REVOKE ALL ON FUNCTION public.worker_has_resolved_peer(text,jsonb),public.worker_claim(text,boolean),public.worker_enqueue(text,jsonb,integer,integer) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_has_resolved_peer(text,jsonb),public.worker_claim(text,boolean),public.worker_enqueue(text,jsonb,integer,integer) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
