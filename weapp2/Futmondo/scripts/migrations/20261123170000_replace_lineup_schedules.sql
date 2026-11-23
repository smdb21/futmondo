BEGIN;
CREATE OR REPLACE FUNCTION public.lineup_schedule_admit_plan(p_id text,p_context jsonb,p_round text,p_kickoff timestamptz,
 p_baseline jsonb,p_stages jsonb,p_doubtful boolean,p_token text,p_credential text,p_confirmed text DEFAULT NULL) RETURNS text
LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
DECLARE team_scope text := public.lineup_schedule_scope(p_context); stage jsonb; idx integer := 0;
  due timestamptz; first_due timestamptz; previous_due timestamptz; field text; players text[] := '{}'; player text;
  job public.lineup_schedule_plans;
BEGIN
 PERFORM pg_advisory_xact_lock(hashtextextended(team_scope, 0));
 IF EXISTS (SELECT 1 FROM public.lineup_schedule_plans WHERE id=p_id AND context=p_context) THEN RETURN 'ready'; END IF;
 FOREACH field IN ARRAY ARRAY['account_id','championship_id','user_team_id','competition_id','season','scoring_version'] LOOP
  IF coalesce(p_context->>field,'') IN ('','unknown') OR length(p_context->>field)>256 THEN RETURN 'invalid'; END IF;
 END LOOP;
 IF p_kickoff<=clock_timestamp() OR coalesce(p_round,'')='' OR p_token IS NULL OR p_credential IS NULL
    OR length(p_token)>8192 OR jsonb_typeof(p_stages) IS DISTINCT FROM 'array'
    OR jsonb_array_length(p_stages) NOT BETWEEN 1 AND 256 THEN RETURN 'invalid'; END IF;
 IF EXISTS (SELECT 1 FROM public.lineup_schedule_manual m WHERE m.scope=team_scope AND m.expires_at>clock_timestamp()) THEN
  RETURN 'busy';
 END IF;
 FOR stage IN SELECT value FROM jsonb_array_elements(p_stages) LOOP
  player := stage->>'player_id';
  due := (stage->>'due_at')::timestamptz;
  IF jsonb_typeof(stage#>'{lineup,starters}') IS DISTINCT FROM 'array'
     OR jsonb_array_length(stage#>'{lineup,starters}')<>11
     OR jsonb_typeof(stage#>'{lineup,bench}') IS DISTINCT FROM 'array'
     OR jsonb_array_length(stage#>'{lineup,bench}')<>4 OR coalesce(stage#>>'{lineup,strategy}','')=''
     OR player IS NULL
     OR (player='' AND (idx<>0 OR due IS NOT NULL OR jsonb_array_length(p_stages)<2))
     OR (player<>'' AND (player=ANY(players) OR due IS NULL OR due>=p_kickoff OR due<previous_due)) THEN
      RETURN 'invalid';
  END IF;
  IF idx=0 THEN first_due:=due; END IF;
  players := array_append(players,player); previous_due := due; idx := idx+1;
 END LOOP;
 SELECT * INTO job FROM public.lineup_schedule_plans p WHERE p.context->>'account_id'=p_context->>'account_id'
  AND p.context->>'championship_id'=p_context->>'championship_id' AND p.context->>'user_team_id'=p_context->>'user_team_id'
  AND p.state IN ('queued','preparing','dispatched','uncertain') FOR UPDATE;
 IF FOUND THEN
  IF job.state IN ('dispatched','uncertain') THEN RETURN 'busy'; END IF;
  IF job.id IS DISTINCT FROM p_confirmed THEN RETURN 'confirm:'||job.id; END IF;
  IF job.stage_index>0 AND p_baseline IS DISTINCT FROM (job.stages->(job.stage_index-1)->'lineup') THEN
    RETURN 'lineup_changed';
  END IF;
  UPDATE public.lineup_schedule_plans SET state='cancelled',code='replaced',owner=NULL,lease_until=NULL,
    encrypted_token=NULL,credential_id=NULL,updated_at=clock_timestamp() WHERE id=job.id;
 END IF;
 INSERT INTO public.lineup_schedule_plans(id,context,round_id,kickoff,baseline,stages,doubtful,encrypted_token,credential_id,due_at)
 VALUES(p_id,p_context,p_round,p_kickoff,p_baseline,p_stages,p_doubtful,p_token,p_credential,coalesce(first_due,clock_timestamp()));
 RETURN 'ready';
END $$;

REVOKE ALL ON FUNCTION public.lineup_schedule_admit_plan(text,jsonb,text,timestamptz,jsonb,jsonb,boolean,text,text,text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.lineup_schedule_admit_plan(text,jsonb,text,timestamptz,jsonb,jsonb,boolean,text,text,text) TO service_role;
NOTIFY pgrst, 'reload schema';
COMMIT;
