BEGIN;
SET LOCAL lock_timeout = '5s';
CREATE FUNCTION public.clause_reconcile_active_offers(
 p_context jsonb,p_market_key text,p_market_version uuid,p_balance_key text,p_balance_version uuid,p_matches jsonb
) RETURNS integer LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE market_at timestamptz; balance_at timestamptz; affected integer;
BEGIN
 IF jsonb_typeof(p_matches) IS DISTINCT FROM 'array' THEN RETURN 0; END IF;
 IF jsonb_array_length(p_matches) NOT BETWEEN 1 AND 64 THEN RETURN 0; END IF;
 SELECT observed_at INTO market_at FROM worker_snapshots
 WHERE account_id=p_context->>'account_id' AND resource_key=p_market_key AND version=p_market_version
  AND handler='get_market_players' AND status='ok' AND completeness<>'partial'
  AND context=p_context||'{"player_id":""}'::jsonb;
 SELECT observed_at INTO balance_at FROM worker_snapshots
 WHERE account_id=p_context->>'account_id' AND resource_key=p_balance_key AND version=p_balance_version
  AND handler='get_user_team_info' AND status='ok' AND completeness<>'partial'
  AND context=p_context||'{"player_id":""}'::jsonb;
 IF market_at IS NULL OR balance_at IS NULL OR greatest(market_at,balance_at)>now() THEN RETURN 0; END IF;
 PERFORM clause_team_lock(p_context);
 UPDATE clause_jobs j SET state='succeeded',reserved_amount=0,reserved_slot=0,
  result_code='active_offer_confirmed',finished_at=now(),updated_at=now(),owner=NULL,lease_until=NULL
 FROM jsonb_to_recordset(p_matches) AS o(job_id text,player_id text,bid_id text,price numeric)
 WHERE j.id=o.job_id AND j.account_id=p_context->>'account_id' AND clause_same_team(j.context,p_context)
  AND j.kind='manual' AND j.mode IN ('bid','offer','modify') AND j.state IN ('accepted','uncertain','dispatched')
  AND j.player_id=o.player_id AND nullif(o.bid_id,'') IS NOT NULL AND o.price=j.max_price
  AND least(market_at,balance_at)>j.dispatched_at;
 GET DIAGNOSTICS affected=ROW_COUNT;
 RETURN affected;
END $$;
REVOKE ALL ON FUNCTION public.clause_reconcile_active_offers(jsonb,text,uuid,text,uuid,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.clause_reconcile_active_offers(jsonb,text,uuid,text,uuid,jsonb) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
