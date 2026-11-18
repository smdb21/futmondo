-- Lossless membership normalization; all observation times retained.
BEGIN;
SET LOCAL lock_timeout='5s';
LOCK TABLE public.player_membership_observations IN ACCESS EXCLUSIVE MODE;
ALTER TABLE public.player_membership_observations RENAME TO player_membership_legacy;
CREATE TABLE public.membership_entities (
 id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
 player_id text NOT NULL CHECK(length(player_id) BETWEEN 1 AND 256),
 competition_id text NOT NULL CHECK(length(competition_id) BETWEEN 1 AND 256),
 season text NOT NULL CHECK(length(season) BETWEEN 1 AND 256),
 real_club_id text CHECK(real_club_id IS NULL OR length(real_club_id) BETWEEN 1 AND 256),
 UNIQUE NULLS NOT DISTINCT(player_id,competition_id,season,real_club_id)
);
CREATE INDEX membership_entity_context ON public.membership_entities(competition_id,season,player_id);
CREATE TABLE public.membership_states (
 id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
 entity_id bigint NOT NULL REFERENCES public.membership_entities(id),
 effective_at timestamptz CHECK(effective_at IS NULL OR isfinite(effective_at)),
 source text NOT NULL CHECK(length(source) BETWEEN 1 AND 128),
 UNIQUE NULLS NOT DISTINCT(entity_id,effective_at,source),UNIQUE(id,entity_id)
);
CREATE TABLE public.membership_times (
 entity_id bigint NOT NULL REFERENCES public.membership_entities(id),
 observed_at timestamptz NOT NULL CHECK(isfinite(observed_at)),
 state_id bigint NOT NULL,
 PRIMARY KEY(entity_id,observed_at),
 FOREIGN KEY(state_id,entity_id) REFERENCES public.membership_states(id,entity_id)
);
CREATE VIEW public.player_membership_observations WITH(security_invoker=true) AS
 SELECT e.player_id,e.competition_id,e.season,e.real_club_id,t.observed_at,s.effective_at,s.source
 FROM public.membership_times t JOIN public.membership_entities e ON e.id=t.entity_id
 JOIN public.membership_states s ON s.id=t.state_id;
INSERT INTO public.membership_entities(player_id,competition_id,season,real_club_id)
 SELECT DISTINCT player_id,competition_id,season,real_club_id FROM public.player_membership_legacy;
INSERT INTO public.membership_states(entity_id,effective_at,source)
 SELECT DISTINCT e.id,o.effective_at,o.source FROM public.player_membership_legacy o
 JOIN public.membership_entities e ON e.player_id=o.player_id AND e.competition_id=o.competition_id
 AND e.season=o.season AND e.real_club_id IS NOT DISTINCT FROM o.real_club_id;
INSERT INTO public.membership_times(entity_id,observed_at,state_id)
 SELECT e.id,o.observed_at,s.id FROM public.player_membership_legacy o
 JOIN public.membership_entities e ON e.player_id=o.player_id AND e.competition_id=o.competition_id
 AND e.season=o.season AND e.real_club_id IS NOT DISTINCT FROM o.real_club_id
 JOIN public.membership_states s ON s.entity_id=e.id AND s.effective_at IS NOT DISTINCT FROM o.effective_at AND s.source=o.source;
DO $$ BEGIN
 IF EXISTS((SELECT * FROM public.player_membership_legacy EXCEPT ALL SELECT * FROM public.player_membership_observations)
 UNION ALL (SELECT * FROM public.player_membership_observations EXCEPT ALL SELECT * FROM public.player_membership_legacy)) THEN
 RAISE EXCEPTION 'Membership reconstruction mismatch; migration rolled back'; END IF;
END $$;
-- The locked transaction proved bidirectional row equality before releasing space.
DROP TABLE public.player_membership_legacy;
CREATE FUNCTION public.membership_write(p_payload jsonb,p_ignore boolean DEFAULT true) RETURNS boolean
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;r public.player_membership_observations;e bigint;s bigint;
BEGIN
 IF p_payload IS NULL OR jsonb_typeof(p_payload) NOT IN ('array','object') THEN RAISE EXCEPTION 'Invalid membership payload'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
 IF jsonb_typeof(row_data)<>'object' OR EXISTS(SELECT 1 FROM jsonb_object_keys(row_data) k WHERE k<>ALL(ARRAY['player_id','competition_id','season','real_club_id','observed_at','effective_at','source'])) THEN RAISE EXCEPTION 'Invalid membership fields'; END IF;
 r:=jsonb_populate_record(NULL::public.player_membership_observations,row_data);
 -- A transaction-scoped identity lock serializes concurrent first inserts and corrections.
 PERFORM pg_advisory_xact_lock(hashtextextended(jsonb_build_array(r.player_id,r.competition_id,r.season,r.real_club_id)::text,0));
 SELECT id INTO e FROM membership_entities WHERE player_id=r.player_id AND competition_id=r.competition_id AND season=r.season AND real_club_id IS NOT DISTINCT FROM r.real_club_id;
 IF e IS NULL THEN INSERT INTO membership_entities(player_id,competition_id,season,real_club_id) VALUES(r.player_id,r.competition_id,r.season,r.real_club_id) RETURNING id INTO e; END IF;
 SELECT id INTO s FROM membership_states WHERE entity_id=e AND effective_at IS NOT DISTINCT FROM r.effective_at AND source=r.source;
 IF s IS NULL THEN INSERT INTO membership_states(entity_id,effective_at,source) VALUES(e,r.effective_at,r.source) RETURNING id INTO s; END IF;
 IF p_ignore THEN INSERT INTO membership_times VALUES(e,r.observed_at,s) ON CONFLICT DO NOTHING;
 ELSE INSERT INTO membership_times VALUES(e,r.observed_at,s) ON CONFLICT(entity_id,observed_at) DO UPDATE SET state_id=excluded.state_id; END IF;
 END LOOP;
 RETURN true;
END $$;
DO $$ DECLARE t text; BEGIN
 FOREACH t IN ARRAY ARRAY['membership_entities','membership_states','membership_times'] LOOP
 EXECUTE format('ALTER TABLE public.%I ENABLE ROW LEVEL SECURITY',t);
 EXECUTE format('REVOKE ALL ON public.%I FROM PUBLIC,anon,authenticated',t);
 EXECUTE format('GRANT SELECT,INSERT,UPDATE ON public.%I TO service_role',t);
 END LOOP;
END $$;
GRANT USAGE,SELECT ON SEQUENCE public.membership_entities_id_seq,public.membership_states_id_seq TO service_role;
REVOKE ALL ON public.player_membership_observations FROM PUBLIC,anon,authenticated;
GRANT SELECT ON public.player_membership_observations TO service_role;
REVOKE ALL ON FUNCTION public.membership_write(jsonb,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.membership_write(jsonb,boolean) TO service_role;
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
  IF p_table='player_membership_observations' THEN
   PERFORM membership_write(row_data,coalesce(p_ignore,false)); CONTINUE;
  END IF;
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
CREATE OR REPLACE FUNCTION public.clause_write_fact(p_id text,p_owner text,
 p_table text,p_payload jsonb,p_conflict text,p_ignore boolean,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;cols text;conflicts text;updates text;clause text;field text;head text;j clause_jobs;p_account text;
BEGIN
 IF NOT clause_guard(p_id,p_owner) THEN RETURN false; END IF;
 SELECT * INTO j FROM clause_jobs WHERE id=p_id; p_account:=j.account_id;
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
  IF row_data ? 'championship_id' AND row_data->>'championship_id' IS DISTINCT FROM j.context->>'championship_id'
     THEN RAISE EXCEPTION 'Foreign fact league'; END IF;
  FOR field IN SELECT jsonb_object_keys(row_data) LOOP
   IF field ~* '(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)' OR NOT EXISTS(
    SELECT 1 FROM pg_attribute WHERE attrelid=('public.'||p_table)::regclass AND attname=field AND attnum>0 AND NOT attisdropped)
    THEN RAISE EXCEPTION 'Invalid fact field'; END IF;
  END LOOP;
  IF p_table='player_membership_observations' THEN
   PERFORM membership_write(row_data,coalesce(p_ignore,false)); CONTINUE;
  END IF;
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
ANALYZE public.membership_entities;
ANALYZE public.membership_states;
ANALYZE public.membership_times;
NOTIFY pgrst,'reload schema';
COMMIT;
