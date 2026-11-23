BEGIN;
CREATE FUNCTION public.clause_reconcile_completed_bids(
 p_context jsonb,p_history_key text,p_history_version uuid,p_balance_key text,p_balance_version uuid,p_matches jsonb
) RETURNS integer LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE history_at timestamptz; balance_at timestamptz; affected integer;
BEGIN
 IF jsonb_typeof(p_matches) IS DISTINCT FROM 'array' THEN RETURN 0; END IF;
 IF jsonb_array_length(p_matches) NOT BETWEEN 1 AND 64 THEN RETURN 0; END IF;
 SELECT observed_at INTO history_at FROM worker_snapshots
 WHERE account_id=p_context->>'account_id' AND resource_key=p_history_key AND version=p_history_version
  AND handler='get_championship_pressroom' AND status='ok' AND completeness<>'partial'
  AND context=p_context||'{"user_team_id":"","player_id":""}'::jsonb;
 SELECT observed_at INTO balance_at FROM worker_snapshots
 WHERE account_id=p_context->>'account_id' AND resource_key=p_balance_key AND version=p_balance_version
  AND handler='get_user_team_info' AND status='ok' AND completeness<>'partial'
  AND context=p_context||'{"player_id":""}'::jsonb;
 IF history_at IS NULL OR balance_at IS NULL OR greatest(history_at,balance_at)>now() THEN RETURN 0; END IF;
 PERFORM clause_team_lock(p_context);
 UPDATE clause_jobs j SET state='succeeded',reserved_amount=0,reserved_slot=0,
  result_code='market_purchase_confirmed',finished_at=now(),updated_at=now(),owner=NULL,lease_until=NULL
 FROM jsonb_to_recordset(p_matches) AS t(job_id text,player_id text,buyer_team_id text,seller_team_id text,price numeric,created timestamptz)
 WHERE j.id=t.job_id AND j.account_id=p_context->>'account_id' AND clause_same_team(j.context,p_context)
  AND j.kind='manual' AND j.mode='bid' AND j.state IN ('accepted','uncertain','dispatched')
  AND j.player_id=t.player_id AND t.buyer_team_id=p_context->>'user_team_id'
  AND coalesce(t.seller_team_id,'')=coalesce(j.original_owner,'') AND t.price=j.max_price
  AND t.created>=j.dispatched_at-interval '2 seconds' AND t.created<=history_at
  AND balance_at>greatest(t.created,j.dispatched_at);
 GET DIAGNOSTICS affected=ROW_COUNT;
 RETURN affected;
END $$;
REVOKE ALL ON FUNCTION public.clause_reconcile_completed_bids(jsonb,text,uuid,text,uuid,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.clause_reconcile_completed_bids(jsonb,text,uuid,text,uuid,jsonb) TO service_role;
CREATE OR REPLACE FUNCTION public.clause_reconcile_reservations(p_context jsonb,p_facts jsonb) RETURNS void
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 PERFORM clause_check_facts(p_context,p_facts);PERFORM clause_team_lock(p_context);
 UPDATE clause_jobs j SET state='succeeded',reserved_amount=0,reserved_slot=0,
  finished_at=coalesce(finished_at,now()),updated_at=now(),owner=NULL,lease_until=NULL
 WHERE j.account_id=p_context->>'account_id' AND clause_same_team(j.context,p_context)
 AND j.state IN ('accepted','uncertain','dispatched') AND j.dispatched_at IS NOT NULL
 AND (p_facts->>'financial_observed_at')::timestamptz>j.dispatched_at
 AND (CASE WHEN j.mode IN ('bid','offer','modify') THEN EXISTS(
  SELECT 1 FROM jsonb_array_elements(p_facts->'offers') o WHERE o->>'player_id'=j.player_id AND (o->>'price')::numeric=j.max_price)
  OR (j.mode='bid' AND EXISTS(SELECT 1 FROM jsonb_array_elements(coalesce(p_facts->'transactions','[]')) tx
   WHERE tx->>'player_id'=j.player_id AND tx->>'buyer_team_id'=p_context->>'user_team_id'
   AND coalesce(tx->>'seller_team_id','')=j.original_owner
   AND (j.original_owner='' OR tx->>'is_clause'='false')
   AND (tx->>'price')::numeric=j.max_price
   AND (tx->>'created')::timestamptz>=j.dispatched_at-interval '2 seconds'
   AND (tx->>'created')::timestamptz<(p_facts->>'financial_observed_at')::timestamptz))
 ELSE EXISTS(SELECT 1 FROM jsonb_array_elements(coalesce(p_facts->'transactions','[]')) tx
  WHERE tx->>'player_id'=j.player_id AND tx->>'buyer_team_id'=p_context->>'user_team_id'
  AND (tx->>'created')::timestamptz>=j.dispatched_at-interval '2 seconds'
  AND (tx->>'price')::numeric=coalesce(j.actual_price,j.max_price)) END);
 UPDATE clause_jobs j SET state='cancelled',result_code='another_buyer_confirmed',reserved_amount=0,reserved_slot=0,
  finished_at=now(),updated_at=now(),owner=NULL,lease_until=NULL
 WHERE j.account_id=p_context->>'account_id' AND clause_same_team(j.context,p_context)
 AND j.state IN ('scheduled','preparing','uncertain') AND EXISTS(
  SELECT 1 FROM jsonb_array_elements(coalesce(p_facts->'transactions','[]')) tx WHERE tx->>'player_id'=j.player_id
  AND coalesce(tx->>'buyer_team_id','')<>p_context->>'user_team_id'
  AND (tx->>'created')::timestamptz>j.created_at);
 PERFORM clause_forget_if_idle(p_context->>'account_id');
END $$;
NOTIFY pgrst,'reload schema';
COMMIT;
