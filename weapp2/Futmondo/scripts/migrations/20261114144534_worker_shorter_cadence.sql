-- Make a shorter autonomous cadence effective for an existing successful job.
-- This changes scheduling only; saved observations and freshness never change.
BEGIN;
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
 due_at=CASE WHEN worker_jobs.owner IS NULL AND excluded.priority>=5 THEN least(worker_jobs.due_at,now())
  -- A newly observed roster/transfer can shorten an existing hourly summary
  -- cadence. Apply that cadence from its last completion instead of keeping
  -- the old hourly due date. Rediscovery at the same cadence is inert, and
  -- failed or running work retains its retry/lease schedule.
  WHEN worker_jobs.owner IS NULL AND worker_jobs.last_error IS NULL
   AND worker_jobs.checked_at IS NOT NULL AND excluded.interval_seconds<worker_jobs.interval_seconds
  THEN least(worker_jobs.due_at,worker_jobs.checked_at+make_interval(secs=>excluded.interval_seconds))
  ELSE worker_jobs.due_at END,
 requested_at=CASE WHEN excluded.priority>=5 THEN now() ELSE worker_jobs.requested_at END,priority=greatest(worker_jobs.priority,excluded.priority),base_priority=greatest(worker_jobs.base_priority,excluded.base_priority),interval_seconds=least(worker_jobs.interval_seconds,excluded.interval_seconds);
 RETURN true;
END $$;
REVOKE ALL ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer) TO service_role;
-- Earlier promotions changed the interval but retained the hourly due date.
-- Repair only the account's current successful market scope; rival/ambiguous
-- identities, retries, active leases and every source snapshot remain untouched.
UPDATE public.worker_jobs j SET due_at=j.checked_at+make_interval(secs=>j.interval_seconds)
FROM public.worker_accounts a
WHERE j.account_id=a.account_id AND a.enabled AND a.auth_status='active' AND j.generation=a.generation
 AND j.descriptor->>'handler'='get_player_summary' AND j.interval_seconds=300
 AND j.owner IS NULL AND j.last_error IS NULL AND j.checked_at IS NOT NULL
 AND j.due_at>j.checked_at+make_interval(secs=>j.interval_seconds)
 AND j.descriptor->'context'->>'account_id'=j.account_id
 AND coalesce(j.descriptor->'context'->>'user_team_id','')<>''
 AND NOT(a.excluded_leagues ? (j.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=j.descriptor->'context'->>'championship_id')
 AND NOT EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
  WHERE coalesce(j.descriptor->'context'->>f,'unknown') IN ('','unknown')
   OR j.descriptor->'context'->>f IS DISTINCT FROM a.verified_leagues->(j.descriptor->'context'->>'championship_id')->>f)
 AND EXISTS(SELECT 1 FROM public.worker_jobs own JOIN public.worker_snapshots s USING(account_id,resource_key)
  WHERE own.account_id=j.account_id AND own.generation=a.generation AND own.descriptor->>'handler'='get_market_players'
   AND s.handler='get_market_players' AND s.context=own.descriptor->'context'
   AND s.status IN ('ok','empty') AND s.completeness IN ('complete','unknown') AND s.observed_at<=now()
   AND ((own.descriptor->'context')-'player_id')=((j.descriptor->'context')-'player_id'));
NOTIFY pgrst,'reload schema';
COMMIT;
