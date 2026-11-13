-- Background acquisition stays independent of GUI navigation.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_request_priority(p_handler text)
RETURNS integer LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
 SELECT CASE WHEN p_handler IN ('get_players_from_team','get_championship_pressroom','get_market_players',
  'get_my_market_players','get_user_team_info','get_roster_bids','get_lineup_from_team',
  'get_notifications','get_notification_unread','get_active_championships') THEN 10
 WHEN p_handler IN ('get_resolved_league_context','get_finished_rounds') THEN 9
 WHEN p_handler IN ('get_clause_transfer_summary','fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot') THEN 8
 ELSE 6 END;
$$;
REVOKE ALL ON FUNCTION public.worker_request_priority(text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_request_priority(text) TO service_role;
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
 due_at=CASE WHEN worker_jobs.owner IS NULL AND excluded.priority>=5 THEN least(worker_jobs.due_at,now()) ELSE worker_jobs.due_at END,
 requested_at=CASE WHEN excluded.priority>=5 THEN now() ELSE worker_jobs.requested_at END,priority=greatest(worker_jobs.priority,excluded.priority),base_priority=greatest(worker_jobs.base_priority,excluded.base_priority),interval_seconds=least(worker_jobs.interval_seconds,excluded.interval_seconds);
 RETURN true;
END $$;
UPDATE public.worker_jobs SET interval_seconds=300,base_priority=3,
 priority=CASE WHEN owner IS NULL AND priority<10 THEN 3 ELSE priority END,
 due_at=CASE WHEN owner IS NULL AND checked_at IS NOT NULL AND priority<10
   THEN greatest(due_at,checked_at+interval '300 seconds') ELSE due_at END
 WHERE descriptor->>'handler'='get_clause_transfer_summary';
REVOKE ALL ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
