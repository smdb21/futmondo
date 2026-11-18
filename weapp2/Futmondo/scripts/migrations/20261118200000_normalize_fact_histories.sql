-- Normalize repeated historical facts: price; first/last confirmations.
BEGIN;
SET LOCAL lock_timeout='5s';
SET LOCAL statement_timeout='120s';
SET LOCAL transaction_timeout='150s';
LOCK TABLE public.player_history IN ACCESS EXCLUSIVE MODE;
ALTER TABLE public.player_history RENAME TO player_history_legacy;
-- Bulk history scans use sequential reads and typed comparisons, not JSON conversion.
SET LOCAL enable_indexscan=off;
SET LOCAL enable_bitmapscan=off;
SET LOCAL work_mem='64MB';
SET LOCAL max_parallel_workers_per_gather=0;
CREATE TEMP TABLE price_retained ON COMMIT DROP AS
 SELECT (original).* FROM (
 SELECT o AS original,(observed_at IS NULL OR NOT isfinite(observed_at) OR player_id IS NULL OR championship_id IS NULL) AS uncertain,
 ROW(value,change,points,avg_points,avg_last_five,matches) AS fact,
 lag(ROW(value,change,points,avg_points,avg_last_five,matches)) OVER w AS preceding,
 lead(ROW(value,change,points,avg_points,avg_last_five,matches)) OVER w AS following
 FROM public.player_history_legacy o
 WINDOW w AS (PARTITION BY player_id,championship_id,competition_id,season ORDER BY observed_at,recorded_at,id)
 ) periods WHERE uncertain OR fact IS DISTINCT FROM preceding OR fact IS DISTINCT FROM following;
ANALYZE price_retained;
SET LOCAL enable_indexscan=DEFAULT;
SET LOCAL enable_bitmapscan=DEFAULT;
SET LOCAL work_mem=DEFAULT;
SET LOCAL max_parallel_workers_per_gather=DEFAULT;

CREATE TABLE public.price_history_entities(entity_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,player_id text REFERENCES public.players(id) ON DELETE CASCADE,championship_id text REFERENCES public.championships(id) ON DELETE CASCADE,competition_id text NOT NULL DEFAULT 'unknown',season text NOT NULL DEFAULT 'unknown',UNIQUE NULLS NOT DISTINCT(player_id,championship_id,competition_id,season));
CREATE INDEX price_history_scope ON public.price_history_entities(championship_id,competition_id,season,player_id);
CREATE TABLE public.price_history_states(state_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,entity_id bigint NOT NULL REFERENCES public.price_history_entities ON DELETE CASCADE,signature text NOT NULL,value bigint NOT NULL,change bigint,points integer,avg_points numeric,avg_last_five numeric,matches integer,UNIQUE(state_id,entity_id));
CREATE INDEX price_history_state_lookup ON public.price_history_states(entity_id,signature);
ALTER SEQUENCE public.player_history_id_seq OWNED BY NONE;
CREATE TABLE public.price_history_times(id bigint PRIMARY KEY DEFAULT nextval('public.player_history_id_seq'),entity_id bigint NOT NULL REFERENCES public.price_history_entities ON DELETE CASCADE,state_id bigint NOT NULL,observed_at timestamptz,recorded_at timestamptz DEFAULT now(),source_unique boolean NOT NULL,FOREIGN KEY(state_id,entity_id) REFERENCES public.price_history_states(state_id,entity_id) ON DELETE CASCADE);
ALTER SEQUENCE public.player_history_id_seq OWNED BY public.price_history_times.id;
CREATE UNIQUE INDEX price_history_source_time ON public.price_history_times(entity_id,observed_at) WHERE source_unique;
CREATE INDEX price_history_observed ON public.price_history_times(entity_id,observed_at,recorded_at,id);
CREATE INDEX price_history_recorded_page ON public.price_history_times(entity_id,recorded_at,id);
INSERT INTO public.price_history_entities(player_id,championship_id,competition_id,season) SELECT DISTINCT player_id,championship_id,competition_id,season FROM public.player_history_legacy;
ANALYZE public.price_history_entities;
INSERT INTO public.price_history_states(entity_id,signature,value,change,points,avg_points,avg_last_five,matches) SELECT DISTINCT e.entity_id,md5(jsonb_build_array(o.value,o.change,o.points,o.avg_points,o.avg_last_five,o.matches)::text),o.value,o.change,o.points,o.avg_points,o.avg_last_five,o.matches FROM price_retained o JOIN public.price_history_entities e ON e.player_id IS NOT DISTINCT FROM o.player_id AND e.championship_id IS NOT DISTINCT FROM o.championship_id AND e.competition_id IS NOT DISTINCT FROM o.competition_id AND e.season IS NOT DISTINCT FROM o.season AND md5(jsonb_build_array(e.player_id,e.championship_id,e.competition_id,e.season)::text)=md5(jsonb_build_array(o.player_id,o.championship_id,o.competition_id,o.season)::text);
ANALYZE public.price_history_states;
INSERT INTO public.price_history_times(entity_id,state_id,id,observed_at,recorded_at,source_unique) SELECT e.entity_id,s.state_id,o.id,o.observed_at,o.recorded_at,o.player_id IS NOT NULL AND o.championship_id IS NOT NULL FROM price_retained o JOIN public.price_history_entities e ON e.player_id IS NOT DISTINCT FROM o.player_id AND e.championship_id IS NOT DISTINCT FROM o.championship_id AND e.competition_id IS NOT DISTINCT FROM o.competition_id AND e.season IS NOT DISTINCT FROM o.season AND md5(jsonb_build_array(e.player_id,e.championship_id,e.competition_id,e.season)::text)=md5(jsonb_build_array(o.player_id,o.championship_id,o.competition_id,o.season)::text) JOIN public.price_history_states s ON s.entity_id=e.entity_id AND s.signature=md5(jsonb_build_array(o.value,o.change,o.points,o.avg_points,o.avg_last_five,o.matches)::text) AND s.value IS NOT DISTINCT FROM o.value AND s.change IS NOT DISTINCT FROM o.change AND s.points IS NOT DISTINCT FROM o.points AND s.avg_points IS NOT DISTINCT FROM o.avg_points AND s.avg_last_five IS NOT DISTINCT FROM o.avg_last_five AND s.matches IS NOT DISTINCT FROM o.matches;
DO $$ DECLARE columns text; BEGIN
 SELECT string_agg(CASE WHEN attname=ANY(ARRAY['player_id','championship_id','competition_id','season']) THEN 'e.' WHEN attname=ANY(ARRAY['value','change','points','avg_points','avg_last_five','matches']) THEN 's.' ELSE 't.' END||quote_ident(attname),',' ORDER BY attnum) INTO columns
 FROM pg_attribute WHERE attrelid='public.player_history_legacy'::regclass AND attnum>0 AND NOT attisdropped;
 EXECUTE 'CREATE VIEW public.player_history WITH(security_invoker=true) AS SELECT '||columns||' FROM public.price_history_times t JOIN public.price_history_entities e USING(entity_id) JOIN public.price_history_states s ON s.state_id=t.state_id';
 IF EXISTS((SELECT to_jsonb(o) FROM price_retained o EXCEPT ALL SELECT to_jsonb(v) FROM public.player_history v)
 UNION ALL (SELECT to_jsonb(v) FROM public.player_history v EXCEPT ALL SELECT to_jsonb(o) FROM price_retained o)) THEN RAISE EXCEPTION 'player_history reconstruction mismatch'; END IF;
END $$;
CREATE FUNCTION public.price_history_write(p_payload jsonb,p_conflict text DEFAULT NULL,p_ignore boolean DEFAULT false) RETURNS boolean
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE item jsonb;r public.player_history; previous jsonb; e bigint;s bigint; fields text[];
BEGIN
 IF p_payload IS NULL OR jsonb_typeof(p_payload) NOT IN ('array','object') THEN RAISE EXCEPTION 'Invalid history payload'; END IF;
 fields:=CASE WHEN p_conflict IS NULL THEN NULL ELSE string_to_array(p_conflict,',') END;
 IF fields IS NOT NULL AND fields IS DISTINCT FROM ARRAY['player_id','championship_id','competition_id','season','observed_at'] AND fields IS DISTINCT FROM ARRAY['id'] THEN RAISE EXCEPTION 'Unsupported history conflict'; END IF;
 FOR item IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
 IF jsonb_typeof(item)<>'object' OR EXISTS(SELECT 1 FROM jsonb_object_keys(item) k WHERE k<>ALL(ARRAY['player_id','championship_id','competition_id','season','value','change','points','avg_points','avg_last_five','matches','id','observed_at','recorded_at'])) THEN RAISE EXCEPTION 'Invalid history fields'; END IF;
 r:=jsonb_populate_record(NULL::public.player_history,jsonb_build_object('competition_id','unknown','season','unknown','change',0,'points',0,'matches',0,'recorded_at',now())||item);
 IF EXISTS(SELECT 1 FROM blocked_leagues WHERE championship_id=r.championship_id) THEN CONTINUE; END IF;
 IF r.competition_id IS NULL OR r.season IS NULL OR r.value IS NULL THEN RAISE EXCEPTION 'Missing history value' USING ERRCODE='23502'; END IF;
 PERFORM pg_advisory_xact_lock(hashtextextended('price'||jsonb_build_array(r.player_id,r.championship_id,r.competition_id,r.season)::text,0));
 previous:=NULL;
 IF fields IS NOT NULL THEN
 IF fields=ARRAY['id'] THEN SELECT to_jsonb(v) INTO previous FROM public.player_history v WHERE v.id=r.id; ELSE
SELECT to_jsonb(v) INTO previous FROM public.player_history v WHERE v.player_id=r.player_id AND v.championship_id=r.championship_id AND v.competition_id=r.competition_id AND v.season=r.season AND v.observed_at=r.observed_at;
 END IF;
 END IF;
 IF previous IS NOT NULL THEN
 IF p_ignore THEN CONTINUE; END IF;
 r:=jsonb_populate_record(NULL::public.player_history,previous||item);
 END IF;
 IF r.id IS NULL AND NOT(item ? 'id') THEN r.id:=nextval('public.player_history_id_seq'); END IF;
 IF r.competition_id IS NULL OR r.season IS NULL OR r.value IS NULL OR r.id IS NULL THEN RAISE EXCEPTION 'Missing history value' USING ERRCODE='23502'; END IF;
SELECT entity_id INTO e FROM price_history_entities e WHERE e.player_id IS NOT DISTINCT FROM r.player_id AND e.championship_id IS NOT DISTINCT FROM r.championship_id AND e.competition_id IS NOT DISTINCT FROM r.competition_id AND e.season IS NOT DISTINCT FROM r.season;
IF e IS NULL THEN INSERT INTO price_history_entities(player_id,championship_id,competition_id,season) VALUES(r.player_id,r.championship_id,r.competition_id,r.season) RETURNING entity_id INTO e; END IF;
SELECT state_id INTO s FROM price_history_states s WHERE s.entity_id=e AND s.signature=md5(jsonb_build_array(r.value,r.change,r.points,r.avg_points,r.avg_last_five,r.matches)::text) AND s.value IS NOT DISTINCT FROM r.value AND s.change IS NOT DISTINCT FROM r.change AND s.points IS NOT DISTINCT FROM r.points AND s.avg_points IS NOT DISTINCT FROM r.avg_points AND s.avg_last_five IS NOT DISTINCT FROM r.avg_last_five AND s.matches IS NOT DISTINCT FROM r.matches;
IF s IS NULL THEN INSERT INTO price_history_states(entity_id,signature,value,change,points,avg_points,avg_last_five,matches) VALUES(e,md5(jsonb_build_array(r.value,r.change,r.points,r.avg_points,r.avg_last_five,r.matches)::text),r.value,r.change,r.points,r.avg_points,r.avg_last_five,r.matches) RETURNING state_id INTO s; END IF;
IF previous IS NOT NULL THEN DELETE FROM price_history_times WHERE id=(previous->>'id')::bigint; END IF;
INSERT INTO price_history_times(id,entity_id,state_id,observed_at,recorded_at,source_unique) VALUES(r.id,e,s,r.observed_at,r.recorded_at,r.player_id IS NOT NULL AND r.championship_id IS NOT NULL);

-- Keep both endpoints of consecutive identical states, never merge A/B/A.
DELETE FROM price_history_times t USING (
 SELECT id,state_id,lag(state_id) OVER w AS preceding,lead(state_id) OVER w AS following,
 observed_at IS NOT NULL AND isfinite(observed_at) AND source_unique AS eligible FROM price_history_times WHERE entity_id=e
 WINDOW w AS (ORDER BY observed_at,recorded_at,id)
) interior WHERE t.entity_id=e AND t.id=interior.id AND interior.eligible
 AND interior.state_id=interior.preceding AND interior.state_id=interior.following;
END LOOP; RETURN true; END $$;
REVOKE ALL ON FUNCTION public.price_history_write(jsonb,text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.price_history_write(jsonb,text,boolean) TO service_role;
ALTER TABLE public.price_history_entities ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.price_history_entities FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.price_history_entities TO service_role;
ALTER TABLE public.price_history_states ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.price_history_states FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.price_history_states TO service_role;
ALTER TABLE public.price_history_times ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.price_history_times FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.price_history_times TO service_role;
GRANT USAGE,SELECT ON SEQUENCE public.price_history_entities_entity_id_seq TO service_role;
GRANT USAGE,SELECT ON SEQUENCE public.price_history_states_state_id_seq TO service_role;
REVOKE ALL ON public.player_history FROM PUBLIC,anon,authenticated;
GRANT SELECT ON public.player_history TO service_role;
DROP TABLE public.player_history_legacy;
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
  IF p_table='player_history' AND to_regprocedure('public.price_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM price_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_match_observations' AND to_regprocedure('public.match_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM match_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='rival_finance_evidence' AND to_regprocedure('public.finance_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM finance_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
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
  IF p_table='player_history' AND to_regprocedure('public.price_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM price_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_match_observations' AND to_regprocedure('public.match_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM match_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='rival_finance_evidence' AND to_regprocedure('public.finance_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM finance_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
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
ANALYZE public.price_history_entities;
ANALYZE public.price_history_states;
ANALYZE public.price_history_times;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Normalize repeated historical facts: match; first/last confirmations.
BEGIN;
SET LOCAL lock_timeout='5s';
SET LOCAL statement_timeout='120s';
SET LOCAL transaction_timeout='150s';
LOCK TABLE public.player_match_observations IN ACCESS EXCLUSIVE MODE;
ALTER TABLE public.player_match_observations RENAME TO player_match_observations_legacy;
-- Bulk history scans use sequential reads and typed comparisons, not JSON conversion.
SET LOCAL enable_indexscan=off;
SET LOCAL enable_bitmapscan=off;
SET LOCAL work_mem='64MB';
SET LOCAL max_parallel_workers_per_gather=0;
CREATE TEMP TABLE match_retained ON COMMIT DROP AS
 SELECT (original).* FROM (
 SELECT o AS original,(observed_at IS NULL OR NOT isfinite(observed_at)) AS uncertain,
 ROW(points,score_status,is_home,minutes_raw,start_raw,participation,round_id,match_id,source_round_number,occurred_at,round_start_at) AS fact,
 lag(ROW(points,score_status,is_home,minutes_raw,start_raw,participation,round_id,match_id,source_round_number,occurred_at,round_start_at)) OVER w AS preceding,
 lead(ROW(points,score_status,is_home,minutes_raw,start_raw,participation,round_id,match_id,source_round_number,occurred_at,round_start_at)) OVER w AS following
 FROM public.player_match_observations_legacy o
 WINDOW w AS (PARTITION BY player_id,championship_id,competition_id,season,scoring_version,round,source_round_key ORDER BY observed_at)
 ) periods WHERE uncertain OR fact IS DISTINCT FROM preceding OR fact IS DISTINCT FROM following;
ANALYZE match_retained;
SET LOCAL enable_indexscan=DEFAULT;
SET LOCAL enable_bitmapscan=DEFAULT;
SET LOCAL work_mem=DEFAULT;
SET LOCAL max_parallel_workers_per_gather=DEFAULT;

CREATE TABLE public.match_history_entities(entity_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,player_id text NOT NULL,championship_id text NOT NULL,competition_id text NOT NULL DEFAULT 'unknown',season text NOT NULL,scoring_version text NOT NULL,round numeric NOT NULL,source_round_key text NOT NULL DEFAULT '',UNIQUE NULLS NOT DISTINCT(player_id,championship_id,competition_id,season,scoring_version,round,source_round_key));
CREATE INDEX match_history_scope ON public.match_history_entities(championship_id,competition_id,season,player_id);
CREATE TABLE public.match_history_states(state_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,entity_id bigint NOT NULL REFERENCES public.match_history_entities ON DELETE CASCADE,signature text NOT NULL,points numeric,score_status text NOT NULL,is_home boolean,minutes_raw numeric,start_raw boolean,participation boolean,round_id text,match_id text,source_round_number numeric,occurred_at timestamptz,round_start_at timestamptz,UNIQUE(state_id,entity_id));
CREATE INDEX match_history_state_lookup ON public.match_history_states(entity_id,signature);
CREATE TABLE public.match_history_times(entity_id bigint NOT NULL REFERENCES public.match_history_entities ON DELETE CASCADE,state_id bigint NOT NULL,observed_at timestamptz NOT NULL,PRIMARY KEY(entity_id,observed_at),FOREIGN KEY(state_id,entity_id) REFERENCES public.match_history_states(state_id,entity_id) ON DELETE CASCADE);
INSERT INTO public.match_history_entities(player_id,championship_id,competition_id,season,scoring_version,round,source_round_key) SELECT DISTINCT player_id,championship_id,competition_id,season,scoring_version,round,source_round_key FROM public.player_match_observations_legacy;
ANALYZE public.match_history_entities;
INSERT INTO public.match_history_states(entity_id,signature,points,score_status,is_home,minutes_raw,start_raw,participation,round_id,match_id,source_round_number,occurred_at,round_start_at) SELECT DISTINCT e.entity_id,md5(jsonb_build_array(o.points,o.score_status,o.is_home,o.minutes_raw,o.start_raw,o.participation,o.round_id,o.match_id,o.source_round_number,o.occurred_at,o.round_start_at)::text),o.points,o.score_status,o.is_home,o.minutes_raw,o.start_raw,o.participation,o.round_id,o.match_id,o.source_round_number,o.occurred_at,o.round_start_at FROM match_retained o JOIN public.match_history_entities e ON e.player_id=o.player_id AND e.championship_id=o.championship_id AND e.competition_id=o.competition_id AND e.season=o.season AND md5(jsonb_build_array(e.player_id,e.championship_id,e.competition_id,e.season)::text)=md5(jsonb_build_array(o.player_id,o.championship_id,o.competition_id,o.season)::text) AND e.scoring_version=o.scoring_version AND e.round=o.round AND e.source_round_key=o.source_round_key;
ANALYZE public.match_history_states;
INSERT INTO public.match_history_times(entity_id,state_id,observed_at) SELECT e.entity_id,s.state_id,o.observed_at FROM match_retained o JOIN public.match_history_entities e ON e.player_id=o.player_id AND e.championship_id=o.championship_id AND e.competition_id=o.competition_id AND e.season=o.season AND md5(jsonb_build_array(e.player_id,e.championship_id,e.competition_id,e.season)::text)=md5(jsonb_build_array(o.player_id,o.championship_id,o.competition_id,o.season)::text) AND e.scoring_version=o.scoring_version AND e.round=o.round AND e.source_round_key=o.source_round_key JOIN public.match_history_states s ON s.entity_id=e.entity_id AND s.signature=md5(jsonb_build_array(o.points,o.score_status,o.is_home,o.minutes_raw,o.start_raw,o.participation,o.round_id,o.match_id,o.source_round_number,o.occurred_at,o.round_start_at)::text) AND s.points IS NOT DISTINCT FROM o.points AND s.score_status IS NOT DISTINCT FROM o.score_status AND s.is_home IS NOT DISTINCT FROM o.is_home AND s.minutes_raw IS NOT DISTINCT FROM o.minutes_raw AND s.start_raw IS NOT DISTINCT FROM o.start_raw AND s.participation IS NOT DISTINCT FROM o.participation AND s.round_id IS NOT DISTINCT FROM o.round_id AND s.match_id IS NOT DISTINCT FROM o.match_id AND s.source_round_number IS NOT DISTINCT FROM o.source_round_number AND s.occurred_at IS NOT DISTINCT FROM o.occurred_at AND s.round_start_at IS NOT DISTINCT FROM o.round_start_at;
DO $$ DECLARE columns text; BEGIN
 SELECT string_agg(CASE WHEN attname=ANY(ARRAY['player_id','championship_id','competition_id','season','scoring_version','round','source_round_key']) THEN 'e.' WHEN attname=ANY(ARRAY['points','score_status','is_home','minutes_raw','start_raw','participation','round_id','match_id','source_round_number','occurred_at','round_start_at']) THEN 's.' ELSE 't.' END||quote_ident(attname),',' ORDER BY attnum) INTO columns
 FROM pg_attribute WHERE attrelid='public.player_match_observations_legacy'::regclass AND attnum>0 AND NOT attisdropped;
 EXECUTE 'CREATE VIEW public.player_match_observations WITH(security_invoker=true) AS SELECT '||columns||' FROM public.match_history_times t JOIN public.match_history_entities e USING(entity_id) JOIN public.match_history_states s ON s.state_id=t.state_id';
 IF EXISTS((SELECT to_jsonb(o) FROM match_retained o EXCEPT ALL SELECT to_jsonb(v) FROM public.player_match_observations v)
 UNION ALL (SELECT to_jsonb(v) FROM public.player_match_observations v EXCEPT ALL SELECT to_jsonb(o) FROM match_retained o)) THEN RAISE EXCEPTION 'player_match_observations reconstruction mismatch'; END IF;
END $$;
CREATE FUNCTION public.match_history_write(p_payload jsonb,p_conflict text DEFAULT NULL,p_ignore boolean DEFAULT false) RETURNS boolean
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE item jsonb;r public.player_match_observations; previous jsonb; e bigint;s bigint; fields text[];
BEGIN
 IF p_payload IS NULL OR jsonb_typeof(p_payload) NOT IN ('array','object') THEN RAISE EXCEPTION 'Invalid history payload'; END IF;
 fields:=CASE WHEN p_conflict IS NULL THEN NULL ELSE string_to_array(p_conflict,',') END;
 IF fields IS NOT NULL AND fields IS DISTINCT FROM ARRAY['player_id','championship_id','competition_id','season','scoring_version','round','source_round_key','observed_at']  THEN RAISE EXCEPTION 'Unsupported history conflict'; END IF;
 FOR item IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
 IF jsonb_typeof(item)<>'object' OR EXISTS(SELECT 1 FROM jsonb_object_keys(item) k WHERE k<>ALL(ARRAY['player_id','championship_id','competition_id','season','scoring_version','round','source_round_key','points','score_status','is_home','minutes_raw','start_raw','participation','round_id','match_id','source_round_number','occurred_at','round_start_at','observed_at'])) THEN RAISE EXCEPTION 'Invalid history fields'; END IF;
 r:=jsonb_populate_record(NULL::public.player_match_observations,jsonb_build_object('competition_id','unknown','source_round_key','')||item);
 IF EXISTS(SELECT 1 FROM blocked_leagues WHERE championship_id=r.championship_id) THEN CONTINUE; END IF;
 IF r.player_id IS NULL OR r.championship_id IS NULL OR r.competition_id IS NULL OR r.season IS NULL OR r.scoring_version IS NULL OR r.round IS NULL OR r.source_round_key IS NULL OR r.score_status IS NULL OR r.observed_at IS NULL THEN RAISE EXCEPTION 'Missing history value' USING ERRCODE='23502'; END IF;
 PERFORM pg_advisory_xact_lock(hashtextextended('match'||jsonb_build_array(r.player_id,r.championship_id,r.competition_id,r.season,r.scoring_version,r.round,r.source_round_key)::text,0));
 previous:=NULL;
 IF fields IS NOT NULL THEN
SELECT to_jsonb(v) INTO previous FROM public.player_match_observations v WHERE v.player_id=r.player_id AND v.championship_id=r.championship_id AND v.competition_id=r.competition_id AND v.season=r.season AND v.scoring_version=r.scoring_version AND v.round=r.round AND v.source_round_key=r.source_round_key AND v.observed_at=r.observed_at;
 END IF;
 IF previous IS NOT NULL THEN
 IF p_ignore THEN CONTINUE; END IF;
 r:=jsonb_populate_record(NULL::public.player_match_observations,previous||item);
 END IF;
 IF r.player_id IS NULL OR r.championship_id IS NULL OR r.competition_id IS NULL OR r.season IS NULL OR r.scoring_version IS NULL OR r.round IS NULL OR r.source_round_key IS NULL OR r.score_status IS NULL OR r.observed_at IS NULL THEN RAISE EXCEPTION 'Missing history value' USING ERRCODE='23502'; END IF;
SELECT entity_id INTO e FROM match_history_entities e WHERE e.player_id IS NOT DISTINCT FROM r.player_id AND e.championship_id IS NOT DISTINCT FROM r.championship_id AND e.competition_id IS NOT DISTINCT FROM r.competition_id AND e.season IS NOT DISTINCT FROM r.season AND e.scoring_version IS NOT DISTINCT FROM r.scoring_version AND e.round IS NOT DISTINCT FROM r.round AND e.source_round_key IS NOT DISTINCT FROM r.source_round_key;
IF e IS NULL THEN INSERT INTO match_history_entities(player_id,championship_id,competition_id,season,scoring_version,round,source_round_key) VALUES(r.player_id,r.championship_id,r.competition_id,r.season,r.scoring_version,r.round,r.source_round_key) RETURNING entity_id INTO e; END IF;
SELECT state_id INTO s FROM match_history_states s WHERE s.entity_id=e AND s.signature=md5(jsonb_build_array(r.points,r.score_status,r.is_home,r.minutes_raw,r.start_raw,r.participation,r.round_id,r.match_id,r.source_round_number,r.occurred_at,r.round_start_at)::text) AND s.points IS NOT DISTINCT FROM r.points AND s.score_status IS NOT DISTINCT FROM r.score_status AND s.is_home IS NOT DISTINCT FROM r.is_home AND s.minutes_raw IS NOT DISTINCT FROM r.minutes_raw AND s.start_raw IS NOT DISTINCT FROM r.start_raw AND s.participation IS NOT DISTINCT FROM r.participation AND s.round_id IS NOT DISTINCT FROM r.round_id AND s.match_id IS NOT DISTINCT FROM r.match_id AND s.source_round_number IS NOT DISTINCT FROM r.source_round_number AND s.occurred_at IS NOT DISTINCT FROM r.occurred_at AND s.round_start_at IS NOT DISTINCT FROM r.round_start_at;
IF s IS NULL THEN INSERT INTO match_history_states(entity_id,signature,points,score_status,is_home,minutes_raw,start_raw,participation,round_id,match_id,source_round_number,occurred_at,round_start_at) VALUES(e,md5(jsonb_build_array(r.points,r.score_status,r.is_home,r.minutes_raw,r.start_raw,r.participation,r.round_id,r.match_id,r.source_round_number,r.occurred_at,r.round_start_at)::text),r.points,r.score_status,r.is_home,r.minutes_raw,r.start_raw,r.participation,r.round_id,r.match_id,r.source_round_number,r.occurred_at,r.round_start_at) RETURNING state_id INTO s; END IF;
IF previous IS NOT NULL THEN UPDATE match_history_times SET state_id=s WHERE entity_id=e AND observed_at=r.observed_at; ELSE INSERT INTO match_history_times VALUES(e,s,r.observed_at); END IF;

-- Keep both endpoints of consecutive identical states, never merge A/B/A.
DELETE FROM match_history_times t USING (
 SELECT observed_at,state_id,lag(state_id) OVER w AS preceding,lead(state_id) OVER w AS following,
 observed_at IS NOT NULL AND isfinite(observed_at) AS eligible FROM match_history_times WHERE entity_id=e
 WINDOW w AS (ORDER BY observed_at)
) interior WHERE t.entity_id=e AND t.observed_at=interior.observed_at AND interior.eligible
 AND interior.state_id=interior.preceding AND interior.state_id=interior.following;
END LOOP; RETURN true; END $$;
REVOKE ALL ON FUNCTION public.match_history_write(jsonb,text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.match_history_write(jsonb,text,boolean) TO service_role;
ALTER TABLE public.match_history_entities ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.match_history_entities FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.match_history_entities TO service_role;
ALTER TABLE public.match_history_states ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.match_history_states FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.match_history_states TO service_role;
ALTER TABLE public.match_history_times ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.match_history_times FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.match_history_times TO service_role;
GRANT USAGE,SELECT ON SEQUENCE public.match_history_entities_entity_id_seq TO service_role;
GRANT USAGE,SELECT ON SEQUENCE public.match_history_states_state_id_seq TO service_role;
REVOKE ALL ON public.player_match_observations FROM PUBLIC,anon,authenticated;
GRANT SELECT ON public.player_match_observations TO service_role;
CREATE OR REPLACE VIEW public.player_round_points_latest WITH(security_invoker=true) AS
 SELECT e.championship_id,e.player_id,e.season,e.scoring_version,e.round,s.points,
 s.score_status,t.observed_at,s.occurred_at,s.round_start_at,e.competition_id,e.source_round_key,
 s.round_id,s.match_id,s.source_round_number
 FROM public.match_history_entities e
 JOIN LATERAL (SELECT observed_at,state_id FROM public.match_history_times WHERE entity_id=e.entity_id ORDER BY observed_at DESC LIMIT 1) t ON true
 JOIN public.match_history_states s ON s.state_id=t.state_id;
DROP TABLE public.player_match_observations_legacy;
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
  IF p_table='player_history' AND to_regprocedure('public.price_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM price_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_match_observations' AND to_regprocedure('public.match_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM match_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='rival_finance_evidence' AND to_regprocedure('public.finance_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM finance_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
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
  IF p_table='player_history' AND to_regprocedure('public.price_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM price_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_match_observations' AND to_regprocedure('public.match_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM match_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='rival_finance_evidence' AND to_regprocedure('public.finance_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM finance_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
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
ANALYZE public.match_history_entities;
ANALYZE public.match_history_states;
ANALYZE public.match_history_times;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Normalize repeated historical facts: finance; first/last confirmations.
BEGIN;
SET LOCAL lock_timeout='5s';
SET LOCAL statement_timeout='120s';
SET LOCAL transaction_timeout='150s';
LOCK TABLE public.rival_finance_evidence IN ACCESS EXCLUSIVE MODE;
ALTER TABLE public.rival_finance_evidence RENAME TO rival_finance_evidence_legacy;
-- Bulk history scans use sequential reads and typed comparisons, not JSON conversion.
SET LOCAL enable_indexscan=off;
SET LOCAL enable_bitmapscan=off;
SET LOCAL work_mem='64MB';
SET LOCAL max_parallel_workers_per_gather=0;
CREATE TEMP TABLE finance_retained ON COMMIT DROP AS
 SELECT (original).* FROM (
 SELECT o AS original,(observed_at IS NULL OR NOT isfinite(observed_at)) AS uncertain,
 ROW(payload) AS fact,
 lag(ROW(payload)) OVER w AS preceding,
 lead(ROW(payload)) OVER w AS following
 FROM public.rival_finance_evidence_legacy o
 WINDOW w AS (PARTITION BY account_id,championship_id,competition_id,season,evidence_type,source_id ORDER BY observed_at)
 ) periods WHERE uncertain OR fact IS DISTINCT FROM preceding OR fact IS DISTINCT FROM following;
ANALYZE finance_retained;
SET LOCAL enable_indexscan=DEFAULT;
SET LOCAL enable_bitmapscan=DEFAULT;
SET LOCAL work_mem=DEFAULT;
SET LOCAL max_parallel_workers_per_gather=DEFAULT;

CREATE TABLE public.finance_history_entities(entity_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,account_id text NOT NULL CHECK(length(btrim(account_id))>0),championship_id text NOT NULL CHECK(length(btrim(championship_id))>0),competition_id text NOT NULL DEFAULT 'unknown',season text NOT NULL CHECK(length(btrim(season))>0),evidence_type text NOT NULL CHECK(evidence_type IN ('awards','rules','movements','team','coverage','eligibility','notifications')),source_id text NOT NULL CHECK(length(btrim(source_id))>0),UNIQUE NULLS NOT DISTINCT(account_id,championship_id,competition_id,season,evidence_type,source_id));
CREATE INDEX finance_history_scope ON public.finance_history_entities(championship_id,competition_id,season,account_id,evidence_type,source_id);
CREATE TABLE public.finance_history_states(state_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,entity_id bigint NOT NULL REFERENCES public.finance_history_entities ON DELETE CASCADE,signature text NOT NULL,payload jsonb NOT NULL CHECK(jsonb_typeof(payload)='object' AND octet_length(payload::text)<=1048576),UNIQUE(state_id,entity_id));
CREATE INDEX finance_history_state_lookup ON public.finance_history_states(entity_id,signature);
CREATE TABLE public.finance_history_times(entity_id bigint NOT NULL REFERENCES public.finance_history_entities ON DELETE CASCADE,state_id bigint NOT NULL,observed_at timestamptz NOT NULL CHECK(isfinite(observed_at)),PRIMARY KEY(entity_id,observed_at),FOREIGN KEY(state_id,entity_id) REFERENCES public.finance_history_states(state_id,entity_id) ON DELETE CASCADE);
INSERT INTO public.finance_history_entities(account_id,championship_id,competition_id,season,evidence_type,source_id) SELECT DISTINCT account_id,championship_id,competition_id,season,evidence_type,source_id FROM public.rival_finance_evidence_legacy;
ANALYZE public.finance_history_entities;
INSERT INTO public.finance_history_states(entity_id,signature,payload) SELECT DISTINCT e.entity_id,md5(jsonb_build_array(o.payload)::text),o.payload FROM finance_retained o JOIN public.finance_history_entities e ON e.account_id=o.account_id AND e.championship_id=o.championship_id AND e.competition_id=o.competition_id AND e.season=o.season AND e.evidence_type=o.evidence_type AND e.source_id=o.source_id;
ANALYZE public.finance_history_states;
INSERT INTO public.finance_history_times(entity_id,state_id,observed_at) SELECT e.entity_id,s.state_id,o.observed_at FROM finance_retained o JOIN public.finance_history_entities e ON e.account_id=o.account_id AND e.championship_id=o.championship_id AND e.competition_id=o.competition_id AND e.season=o.season AND e.evidence_type=o.evidence_type AND e.source_id=o.source_id JOIN public.finance_history_states s ON s.entity_id=e.entity_id AND s.signature=md5(jsonb_build_array(o.payload)::text) AND s.payload IS NOT DISTINCT FROM o.payload;
DO $$ DECLARE columns text; BEGIN
 SELECT string_agg(CASE WHEN attname=ANY(ARRAY['account_id','championship_id','competition_id','season','evidence_type','source_id']) THEN 'e.' WHEN attname=ANY(ARRAY['payload']) THEN 's.' ELSE 't.' END||quote_ident(attname),',' ORDER BY attnum) INTO columns
 FROM pg_attribute WHERE attrelid='public.rival_finance_evidence_legacy'::regclass AND attnum>0 AND NOT attisdropped;
 EXECUTE 'CREATE VIEW public.rival_finance_evidence WITH(security_invoker=true) AS SELECT '||columns||' FROM public.finance_history_times t JOIN public.finance_history_entities e USING(entity_id) JOIN public.finance_history_states s ON s.state_id=t.state_id';
 IF EXISTS((SELECT to_jsonb(o) FROM finance_retained o EXCEPT ALL SELECT to_jsonb(v) FROM public.rival_finance_evidence v)
 UNION ALL (SELECT to_jsonb(v) FROM public.rival_finance_evidence v EXCEPT ALL SELECT to_jsonb(o) FROM finance_retained o)) THEN RAISE EXCEPTION 'rival_finance_evidence reconstruction mismatch'; END IF;
END $$;
CREATE FUNCTION public.finance_history_write(p_payload jsonb,p_conflict text DEFAULT NULL,p_ignore boolean DEFAULT false) RETURNS boolean
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE item jsonb;r public.rival_finance_evidence; previous jsonb; e bigint;s bigint; fields text[];
BEGIN
 IF p_payload IS NULL OR jsonb_typeof(p_payload) NOT IN ('array','object') THEN RAISE EXCEPTION 'Invalid history payload'; END IF;
 fields:=CASE WHEN p_conflict IS NULL THEN NULL ELSE string_to_array(p_conflict,',') END;
 IF fields IS NOT NULL AND fields IS DISTINCT FROM ARRAY['account_id','championship_id','competition_id','season','evidence_type','source_id','observed_at']  THEN RAISE EXCEPTION 'Unsupported history conflict'; END IF;
 FOR item IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
 IF jsonb_typeof(item)<>'object' OR EXISTS(SELECT 1 FROM jsonb_object_keys(item) k WHERE k<>ALL(ARRAY['account_id','championship_id','competition_id','season','evidence_type','source_id','payload','observed_at'])) THEN RAISE EXCEPTION 'Invalid history fields'; END IF;
 r:=jsonb_populate_record(NULL::public.rival_finance_evidence,jsonb_build_object('competition_id','unknown')||item);
 IF EXISTS(SELECT 1 FROM blocked_leagues WHERE championship_id=r.championship_id) THEN CONTINUE; END IF;
 IF r.account_id IS NULL OR r.championship_id IS NULL OR r.competition_id IS NULL OR r.season IS NULL OR r.evidence_type IS NULL OR r.source_id IS NULL OR r.payload IS NULL OR r.observed_at IS NULL THEN RAISE EXCEPTION 'Missing history value' USING ERRCODE='23502'; END IF;
 PERFORM pg_advisory_xact_lock(hashtextextended('finance'||jsonb_build_array(r.account_id,r.championship_id,r.competition_id,r.season,r.evidence_type,r.source_id)::text,0));
 previous:=NULL;
 IF fields IS NOT NULL THEN
SELECT to_jsonb(v) INTO previous FROM public.rival_finance_evidence v WHERE v.account_id=r.account_id AND v.championship_id=r.championship_id AND v.competition_id=r.competition_id AND v.season=r.season AND v.evidence_type=r.evidence_type AND v.source_id=r.source_id AND v.observed_at=r.observed_at;
 END IF;
 IF previous IS NOT NULL THEN
 IF p_ignore THEN CONTINUE; END IF;
 r:=jsonb_populate_record(NULL::public.rival_finance_evidence,previous||item);
 END IF;
 IF r.account_id IS NULL OR r.championship_id IS NULL OR r.competition_id IS NULL OR r.season IS NULL OR r.evidence_type IS NULL OR r.source_id IS NULL OR r.payload IS NULL OR r.observed_at IS NULL THEN RAISE EXCEPTION 'Missing history value' USING ERRCODE='23502'; END IF;
SELECT entity_id INTO e FROM finance_history_entities e WHERE e.account_id IS NOT DISTINCT FROM r.account_id AND e.championship_id IS NOT DISTINCT FROM r.championship_id AND e.competition_id IS NOT DISTINCT FROM r.competition_id AND e.season IS NOT DISTINCT FROM r.season AND e.evidence_type IS NOT DISTINCT FROM r.evidence_type AND e.source_id IS NOT DISTINCT FROM r.source_id;
IF e IS NULL THEN INSERT INTO finance_history_entities(account_id,championship_id,competition_id,season,evidence_type,source_id) VALUES(r.account_id,r.championship_id,r.competition_id,r.season,r.evidence_type,r.source_id) RETURNING entity_id INTO e; END IF;
SELECT state_id INTO s FROM finance_history_states s WHERE s.entity_id=e AND s.signature=md5(jsonb_build_array(r.payload)::text) AND s.payload IS NOT DISTINCT FROM r.payload;
IF s IS NULL THEN INSERT INTO finance_history_states(entity_id,signature,payload) VALUES(e,md5(jsonb_build_array(r.payload)::text),r.payload) RETURNING state_id INTO s; END IF;
IF previous IS NOT NULL THEN UPDATE finance_history_times SET state_id=s WHERE entity_id=e AND observed_at=r.observed_at; ELSE INSERT INTO finance_history_times VALUES(e,s,r.observed_at); END IF;

-- Keep both endpoints of consecutive identical states, never merge A/B/A.
DELETE FROM finance_history_times t USING (
 SELECT observed_at,state_id,lag(state_id) OVER w AS preceding,lead(state_id) OVER w AS following,
 observed_at IS NOT NULL AND isfinite(observed_at) AS eligible FROM finance_history_times WHERE entity_id=e
 WINDOW w AS (ORDER BY observed_at)
) interior WHERE t.entity_id=e AND t.observed_at=interior.observed_at AND interior.eligible
 AND interior.state_id=interior.preceding AND interior.state_id=interior.following;
END LOOP; RETURN true; END $$;
REVOKE ALL ON FUNCTION public.finance_history_write(jsonb,text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.finance_history_write(jsonb,text,boolean) TO service_role;
ALTER TABLE public.finance_history_entities ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.finance_history_entities FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.finance_history_entities TO service_role;
ALTER TABLE public.finance_history_states ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.finance_history_states FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.finance_history_states TO service_role;
ALTER TABLE public.finance_history_times ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.finance_history_times FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.finance_history_times TO service_role;
GRANT USAGE,SELECT ON SEQUENCE public.finance_history_entities_entity_id_seq TO service_role;
GRANT USAGE,SELECT ON SEQUENCE public.finance_history_states_state_id_seq TO service_role;
REVOKE ALL ON public.rival_finance_evidence FROM PUBLIC,anon,authenticated;
GRANT SELECT ON public.rival_finance_evidence TO service_role;
CREATE OR REPLACE VIEW public.rival_finance_evidence_latest WITH(security_invoker=true) AS
 SELECT e.account_id,e.championship_id,e.season,e.evidence_type,e.source_id,t.observed_at,s.payload,e.competition_id
 FROM public.finance_history_entities e
 JOIN LATERAL (SELECT observed_at,state_id FROM public.finance_history_times WHERE entity_id=e.entity_id ORDER BY observed_at DESC LIMIT 1) t ON true
 JOIN public.finance_history_states s ON s.state_id=t.state_id;
DROP TABLE public.rival_finance_evidence_legacy;
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
  IF p_table='player_history' AND to_regprocedure('public.price_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM price_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_match_observations' AND to_regprocedure('public.match_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM match_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='rival_finance_evidence' AND to_regprocedure('public.finance_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM finance_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
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
  IF p_table='player_history' AND to_regprocedure('public.price_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM price_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_match_observations' AND to_regprocedure('public.match_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM match_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='rival_finance_evidence' AND to_regprocedure('public.finance_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM finance_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
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
ANALYZE public.finance_history_entities;
ANALYZE public.finance_history_states;
ANALYZE public.finance_history_times;
NOTIFY pgrst,'reload schema';
COMMIT;
