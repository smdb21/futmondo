-- Fixed completion-based cadence and fair service between independent query families.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_next_refresh_due(p_handler text,p_interval integer,p_valid_until timestamptz,p_started timestamptz,p_now timestamptz DEFAULT now())
RETURNS timestamptz LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
 SELECT p_now+make_interval(secs=>p_interval);
$$;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 WITH dispatched AS MATERIALIZED (
  SELECT account_id,descriptor->>'handler' handler,max(started_at) served_at FROM worker_jobs GROUP BY 1,2)
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 JOIN dispatched d ON d.account_id=q.account_id AND d.handler=q.descriptor->>'handler'
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND public.worker_finance_context_matches(q.descriptor,a.verified_leagues->(q.descriptor->'context'->>'championship_id'))
 AND NOT public.worker_has_resolved_peer(q.account_id,q.descriptor)
 AND (q.owner IS NULL OR q.lease_until<now())
 AND (NOT p_foreground OR q.descriptor->>'handler' NOT IN
   ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot'))
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
 AND (q.checked_at IS NULL OR q.checked_at + make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE 0 END)<=now())
 -- The short lane prefers core observations, then helps other short reads. The other lane gives each due
 -- query family a turn, so a recurring minute job or527 player jobs cannot
 -- indefinitely exclude catalogs, history or accounting. One-lane installs
 -- retain the same family fairness across core and background work.
 ORDER BY CASE WHEN p_foreground AND (public.worker_request_priority(q.descriptor->>'handler')>=9 OR q.descriptor->>'handler'='get_clause_transfer_summary') THEN 0 ELSE 1 END,
 d.served_at NULLS FIRST,a.last_dispatched NULLS FIRST,q.due_at,q.account_id,q.resource_key
 FOR UPDATE OF q SKIP LOCKED LIMIT 1;
 IF j.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
REVOKE ALL ON FUNCTION public.worker_next_refresh_due(text,integer,timestamptz,timestamptz,timestamptz),public.worker_claim(text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_next_refresh_due(text,integer,timestamptz,timestamptz,timestamptz),public.worker_claim(text,boolean) TO service_role;
UPDATE public.worker_jobs SET interval_seconds=300,base_priority=3,
 due_at=CASE WHEN owner IS NULL AND checked_at IS NOT NULL THEN greatest(due_at,checked_at+interval '300 seconds') ELSE due_at END
 WHERE descriptor->>'handler'='get_team_acquisition_capacity' AND interval_seconds<300;
DELETE FROM public.worker_jobs WHERE descriptor->>'handler'='get_player_card_history' AND owner IS NULL;
NOTIFY pgrst,'reload schema';
COMMIT;
