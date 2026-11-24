BEGIN;
CREATE OR REPLACE FUNCTION public.clause_reconcile_completed_bids(
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
  result_code=CASE WHEN j.mode='clause' THEN 'clause_purchase_confirmed' ELSE 'market_purchase_confirmed' END,finished_at=now(),updated_at=now(),owner=NULL,lease_until=NULL
 FROM jsonb_to_recordset(p_matches) AS t(job_id text,player_id text,buyer_team_id text,seller_team_id text,price numeric,created timestamptz,is_clause boolean)
 WHERE j.id=t.job_id AND j.account_id=p_context->>'account_id' AND clause_same_team(j.context,p_context)
  AND j.kind='manual' AND j.state IN ('accepted','uncertain','dispatched')
  AND (j.mode='bid' OR (j.mode='clause' AND coalesce(j.original_owner,'')<>''
   AND (t.is_clause IS TRUE OR (t.is_clause IS NULL AND j.state='accepted' AND j.result_code='api.general.ok'))))
  AND j.player_id=t.player_id AND t.buyer_team_id=p_context->>'user_team_id'
  AND coalesce(t.seller_team_id,'')=coalesce(j.original_owner,'') AND t.price=CASE WHEN j.mode='clause' THEN coalesce(j.actual_price,j.max_price) ELSE j.max_price END
  AND t.created>=j.dispatched_at-interval '2 seconds' AND t.created<=history_at
  AND balance_at>greatest(t.created,j.dispatched_at);
 GET DIAGNOSTICS affected=ROW_COUNT;
 RETURN affected;
END $$;
NOTIFY pgrst,'reload schema';
COMMIT;
