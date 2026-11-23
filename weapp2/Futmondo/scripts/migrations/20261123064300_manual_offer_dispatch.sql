BEGIN;

-- Claim and dispatch a manually confirmed offer under the same lock as the
-- scheduled purchase lane. Futmondo decides ordinary bid eligibility.
CREATE OR REPLACE FUNCTION public.clause_dispatch_manual_offer(
 p_id text, p_context jsonb, p_mode text, p_player jsonb, p_price numeric,
 p_capacity jsonb DEFAULT NULL
) RETURNS text
LANGUAGE plpgsql SECURITY INVOKER SET search_path = public, pg_temp AS $$
DECLARE
 field text;
 held_amount numeric;
 held_slots integer;
 busy boolean;
 same_player boolean;
BEGIN
 FOREACH field IN ARRAY ARRAY['account_id','championship_id','user_team_id','competition_id','season','scoring_version','player_id'] LOOP
  IF coalesce(p_context->>field,'') IN ('','unknown') THEN RETURN 'clause_context_unverified'; END IF;
 END LOOP;
 IF p_id IS NULL OR p_id !~ '^[a-f0-9]{32}$' OR p_mode IS NULL OR p_mode NOT IN ('bid','offer')
 OR p_price IS NULL OR NOT (p_price>0 AND p_price<=1e16)
 OR p_context->>'player_id' IS DISTINCT FROM p_player->>'id' THEN RETURN 'clause_invalid_request'; END IF;

 PERFORM clause_team_lock(p_context);
 SELECT coalesce(sum(j.reserved_amount),0),coalesce(sum(j.reserved_slot),0),
  coalesce(bool_or(j.state IN ('preparing','dispatched','uncertain')),false),
  coalesce(bool_or(j.player_id=p_context->>'player_id'),false)
 INTO held_amount,held_slots,busy,same_player
 FROM clause_jobs j
 WHERE j.account_id=p_context->>'account_id' AND clause_same_team(j.context,p_context)
 AND j.state IN ('scheduled','preparing','dispatched','uncertain','accepted');
 IF busy OR same_player THEN RETURN 'clause_team_busy'; END IF;
 IF (SELECT count(*) FROM clause_jobs WHERE account_id=p_context->>'account_id'
     AND state IN ('scheduled','preparing','dispatched','uncertain','accepted'))>=256 THEN RETURN 'clause_queue_capacity'; END IF;
 IF held_amount>0 OR held_slots>0 THEN
  -- Only an existing local reservation needs a capacity comparison. The
  -- deadline belongs to the saved source observation and is never renewed.
  IF p_capacity->>'status' IS DISTINCT FROM 'ok' OR p_capacity->'roster'->>'status' IS DISTINCT FROM 'ok'
  OR NOT coalesce((p_capacity->>'valid_until')::numeric > extract(epoch from now()),false)
  OR p_capacity->'funds'->>'spendable_budget' IS NULL OR p_capacity->'funds'->>'api_bid_limit' IS NULL
  OR p_capacity->'roster'->>'effective_count' IS NULL OR p_capacity->'roster'->>'cap' IS NULL
  OR p_capacity->'outstanding'->>'count' IS NULL THEN RETURN 'clause_reservation_capacity_unavailable'; END IF;
  IF (p_capacity->'roster'->>'effective_count')::integer+(p_capacity->'outstanding'->>'count')::integer+held_slots
     >=(p_capacity->'roster'->>'cap')::integer THEN RETURN 'clause_offer_capacity'; END IF;
  IF p_price+held_amount>(p_capacity->'funds'->>'spendable_budget')::numeric
     OR p_price>(p_capacity->'funds'->>'api_bid_limit')::numeric THEN RETURN 'clause_offer_funds'; END IF;
 END IF;

 INSERT INTO clause_jobs(id,account_id,context,kind,mode,player_id,player_slug,player_name,
  original_owner,max_price,reserved_amount,reserved_slot,opens_at,due_at,expires_at,
  state,actual_price,attempts,dispatched_at,lease_until)
 VALUES(p_id,p_context->>'account_id',p_context,'manual',p_mode,p_player->>'id',
  left(coalesce(p_player->>'slug',''),256),left(coalesce(p_player->>'name','Player'),256),
  coalesce(p_player->>'owner_id',''),p_price,p_price,1,now(),now(),now()+interval '5 minutes',
  'dispatched',p_price,1,now(),now()+interval '60 seconds')
 ON CONFLICT (id) DO NOTHING;
 IF NOT FOUND THEN RETURN 'clause_request_conflict'; END IF;
 INSERT INTO clause_attempts(job_id,attempt) VALUES(p_id,1);
 RETURN 'dispatched';
END;
$$;

REVOKE ALL ON FUNCTION public.clause_dispatch_manual_offer(text,jsonb,text,jsonb,numeric,jsonb) FROM PUBLIC, anon, authenticated;
GRANT EXECUTE ON FUNCTION public.clause_dispatch_manual_offer(text,jsonb,text,jsonb,numeric,jsonb) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
