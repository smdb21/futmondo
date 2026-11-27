-- The Rivals button shares the existing core refresh and advances the
-- already configured league accounting job on its usual background lane.
CREATE OR REPLACE FUNCTION public.worker_rival_refresh(p_account text,p_league text,p_team text)
RETURNS integer LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE requested timestamptz:=clock_timestamp(); job public.worker_jobs;
BEGIN
  PERFORM public.worker_league_refresh(p_account,p_league,p_team);
  SELECT j.* INTO job FROM worker_jobs j JOIN worker_accounts a USING(account_id)
  WHERE a.account_id=p_account AND j.account_id=a.account_id AND j.generation=a.generation
    AND a.enabled AND a.auth_status='active' AND a.verified_leagues ? p_league
    AND NOT a.excluded_leagues ? p_league AND coalesce(a.rate_limited_until,'-infinity')<=now()
    AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues WHERE championship_id=p_league)
    AND j.descriptor->>'handler'='fm_worker_finances'
    AND j.descriptor->'context'->>'championship_id'=p_league AND j.descriptor->'context'->>'user_team_id'=p_team
    AND public.worker_finance_context_matches(j.descriptor,a.verified_leagues->p_league)
    -- Private rival budgets and movements routinely return source_rejected.
    -- The public accounting result still completes. Preserve actual backoff.
    AND (j.last_error IS NULL OR string_to_array(j.last_error,',') <@ ARRAY[
      'source_rejected_1_userteam_information','source_rejected_1_userteam_moneymovements'])
  FOR UPDATE OF j;
  IF NOT FOUND THEN RETURN 0; END IF;
  IF job.league_refresh_at IS NULL OR (job.owner IS NOT NULL AND job.started_at>=job.league_refresh_at
      AND job.league_refresh_at<requested-interval '5 seconds') THEN
    UPDATE worker_jobs SET requested_at=greatest(requested_at,requested),league_refresh_at=requested,
      due_at=CASE WHEN owner IS NULL THEN least(due_at,now()) ELSE due_at END
    WHERE account_id=p_account AND resource_key=job.resource_key;
  END IF;
  RETURN 1;
END $$;
REVOKE ALL ON FUNCTION public.worker_rival_refresh(text,text,text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_rival_refresh(text,text,text) TO service_role;
NOTIFY pgrst,'reload schema';
