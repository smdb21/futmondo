-- Protect visible prerequisite reads from long finance and history work.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_request_priority(p_handler text)
RETURNS integer LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
 SELECT CASE WHEN p_handler IN ('get_players_from_team','get_championship_pressroom','get_market_players',
  'get_my_market_players','get_user_team_info','get_roster_bids','get_lineup_from_team',
  'get_notifications','get_notification_unread','get_active_championships') THEN 10
 WHEN p_handler IN ('get_resolved_league_context','get_finished_rounds') THEN 9
 WHEN p_handler IN ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot') THEN 8
 ELSE 6 END;
$$;
REVOKE ALL ON FUNCTION public.worker_request_priority(text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_request_priority(text) TO service_role;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND public.worker_finance_context_matches(q.descriptor,a.verified_leagues->(q.descriptor->'context'->>'championship_id'))
 AND (q.owner IS NULL OR q.lease_until<now()) AND (NOT p_foreground OR (q.priority>=5 AND q.descriptor->>'handler' NOT IN
  ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot')))
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
-- Preserve leases, demand timestamps and snapshots; normalize only queued
-- historical/default demand so old priority-10 entries cannot defeat the policy.
UPDATE public.worker_jobs SET priority=public.worker_request_priority(descriptor->>'handler')
 WHERE owner IS NULL AND priority>=5 AND priority>public.worker_request_priority(descriptor->>'handler');
-- Recent points reuse the same five-minute summary source. Keep the two
-- resource cadences aligned rather than refetching the same saved source/minute.
UPDATE public.worker_jobs SET interval_seconds=300,base_priority=3
 WHERE descriptor->>'handler'='get_player_recent_points' AND interval_seconds=60;
REVOKE ALL ON FUNCTION public.worker_claim(text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_claim(text,boolean) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
