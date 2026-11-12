-- Preserve default catalog upserts in the guarded worker persistence bridge.
-- Existing source-time heads continue to reject older catalog observations.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_write_fact(p_account text,p_key text,p_owner text,p_generation bigint,
 p_table text,p_payload jsonb,p_conflict text,p_ignore boolean,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;cols text;conflicts text;updates text;clause text;field text;head text;j worker_jobs;
BEGIN
 PERFORM 1 FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT worker_guard(p_account,p_key,p_owner,p_generation) THEN RETURN false; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key;
 IF p_table<>ALL(ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history',
 'market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations',
 'market_observations','player_match_observations','league_rule_snapshots','player_round_history_checks',
 'rival_finance_evidence','complete_history_baselines','league_context_observations','player_membership_observations']) THEN RAISE EXCEPTION 'Invalid fact table'; END IF;
 -- Match PostgREST's default merge identity for mutable source catalogs.
 IF p_conflict IS NULL AND p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
  p_conflict:='id';
 END IF;
 IF p_observed IS NULL OR p_observed>now()+interval '60 seconds' THEN RAISE EXCEPTION 'Invalid source time'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
  IF jsonb_typeof(row_data)<>'object' OR octet_length(row_data::text)>1048576 THEN RAISE EXCEPTION 'Invalid fact row'; END IF;
  IF row_data ? 'account_id' AND row_data->>'account_id' IS DISTINCT FROM p_account THEN RAISE EXCEPTION 'Foreign fact account'; END IF;
  IF row_data ? 'championship_id' AND row_data->>'championship_id' IS DISTINCT FROM j.descriptor->'context'->>'championship_id'
    AND j.descriptor->>'handler'<>'get_active_championships' THEN RAISE EXCEPTION 'Foreign fact league'; END IF;
  FOR field IN SELECT jsonb_object_keys(row_data) LOOP
   IF field ~* '(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)' OR NOT EXISTS(
    SELECT 1 FROM pg_attribute WHERE attrelid=('public.'||p_table)::regclass AND attname=field AND attnum>0 AND NOT attisdropped)
    THEN RAISE EXCEPTION 'Invalid fact field'; END IF;
  END LOOP;
  IF p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
   head:=NULL;
   IF coalesce(row_data->>'id','')='' THEN RAISE EXCEPTION 'Missing fact identity'; END IF;
   INSERT INTO worker_fact_heads(table_name,entity_id,observed_at) VALUES(p_table,row_data->>'id',p_observed)
   ON CONFLICT(table_name,entity_id) DO UPDATE SET observed_at=excluded.observed_at
   WHERE worker_fact_heads.observed_at<=excluded.observed_at RETURNING entity_id INTO head;
   IF head IS NULL THEN CONTINUE; END IF;
  END IF;
  SELECT string_agg(format('%I',key),',' ORDER BY key),string_agg(format('%I=excluded.%I',key,key),',' ORDER BY key)
  INTO cols,updates FROM jsonb_object_keys(row_data) AS key;
  clause:='';
  IF p_conflict IS NOT NULL THEN
   IF p_conflict !~ '^[a-z_]+(,[a-z_]+)*$' THEN RAISE EXCEPTION 'Invalid fact conflict'; END IF;
   SELECT string_agg(format('%I',v),',') INTO conflicts FROM unnest(string_to_array(p_conflict,',')) AS v;
   clause:=format(' ON CONFLICT (%s) DO %s',conflicts,CASE WHEN p_ignore THEN 'NOTHING' ELSE 'UPDATE SET '||updates END);
  END IF;
  EXECUTE format('INSERT INTO public.%I (%s) SELECT %s FROM jsonb_populate_record(NULL::public.%I,$1)%s',p_table,cols,cols,p_table,clause) USING row_data;
 END LOOP;
 RETURN true;
END $$;
REVOKE ALL ON FUNCTION public.worker_write_fact(text,text,text,bigint,text,jsonb,text,boolean,timestamptz) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_write_fact(text,text,text,bigint,text,jsonb,text,boolean,timestamptz) TO service_role;
NOTIFY pgrst, 'reload schema';
COMMIT;
