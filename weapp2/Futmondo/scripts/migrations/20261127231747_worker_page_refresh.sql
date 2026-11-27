-- Explicit button requests only advance factual jobs already discovered by the worker.
CREATE FUNCTION public.worker_page_refresh(p_account text,p_league text,p_team text,p_page text,
  p_round text DEFAULT '',p_history boolean DEFAULT false)
RETURNS integer LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE
  a record;
  handlers text[];
  requested timestamptz := clock_timestamp();
  eligible integer;
BEGIN
  SELECT enabled,auth_status,generation,verified_leagues,excluded_leagues,rate_limited_until
    INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
  IF NOT FOUND OR NOT a.enabled OR a.auth_status<>'active' OR coalesce(p_team,'')=''
    OR NOT (a.verified_leagues ? p_league) OR a.excluded_leagues ? p_league
    OR a.rate_limited_until>now() OR EXISTS (SELECT 1 FROM blocked_leagues WHERE championship_id=p_league)
  THEN RETURN 0; END IF;

  handlers := CASE p_page
    WHEN 'classification' THEN ARRAY['get_classification_round_catalog','get_classification_round_ranking','get_round_dreamteam'] ||
      CASE WHEN p_history THEN ARRAY['get_user_team_rounds','get_teams','fm_worker_finances'] ELSE '{}'::text[] END
    WHEN 'today' THEN ARRAY['get_market_players','get_user_team_info','get_championship_pressroom','get_team_acquisition_capacity']
    WHEN 'lineup' THEN ARRAY['get_worker_http_read','get_finished_rounds','get_lineup_match_details']
    WHEN 'yourteam' THEN ARRAY['get_players_from_team','get_my_market_players','get_roster_bids','get_user_team_info','get_championship_pressroom']
    WHEN 'players_in_championship' THEN ARRAY['get_championship_players','get_teams','get_user_team_info']
    WHEN 'plots' THEN ARRAY['get_classification_round_catalog','get_classification_round_ranking','get_user_team_rounds','get_teams','get_championship_players','get_championship_pressroom']
    WHEN 'round_mvps' THEN ARRAY['get_mvp_saved_catalog','get_championship_players']
    WHEN 'notifications' THEN ARRAY['get_notifications','get_notification_unread']
    WHEN 'admin' THEN ARRAY['get_user_team_info','get_active_championships']
    WHEN 'login' THEN ARRAY['get_user_team_info','get_players_from_team','get_finished_rounds','get_team_acquisition_capacity','get_championship_pressroom','get_notification_unread']
  END;
  IF handlers IS NULL THEN RETURN 0; END IF;

  WITH targets AS MATERIALIZED (
    SELECT j.account_id,j.resource_key,j.owner,j.started_at,j.league_refresh_at
    FROM worker_jobs j
    WHERE j.account_id=p_account AND j.generation=a.generation AND j.descriptor->>'handler'=ANY(handlers)
      AND (j.last_error IS NULL OR (j.descriptor->>'handler'='fm_worker_finances'
        AND string_to_array(j.last_error,',') <@ ARRAY['source_rejected_1_userteam_information','source_rejected_1_userteam_moneymovements']))
      AND CASE WHEN j.descriptor->>'handler' IN ('get_notifications','get_notification_unread','get_active_championships')
        THEN j.descriptor->'context'->>'championship_id'=''
        ELSE j.descriptor->'context'->>'championship_id'=p_league AND
          NOT EXISTS (SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) field
            WHERE a.verified_leagues->p_league->>field NOT IN ('','unknown')
            AND coalesce(j.descriptor->'context'->>field,'unknown') NOT IN ('','unknown',a.verified_leagues->p_league->>field))
        END
      AND (j.descriptor->>'handler'='get_user_team_rounds' OR j.descriptor->'context'->>'user_team_id' IN ('',p_team))
      AND CASE j.descriptor->>'handler'
        WHEN 'get_worker_http_read' THEN j.descriptor->'args'->'query'->>'userteamId'=p_team
          AND j.descriptor->'args'->'query'->>'championshipId'=p_league
          AND j.descriptor->'args'->>'route' IN ('/1/userteam/lineup','/1/userteam/roster','/1/userteam/information','/5/strategy/availables')
        WHEN 'get_classification_round_ranking' THEN CASE WHEN p_round<>'' THEN j.descriptor->'args'->>'round_id'=p_round ELSE j.native_interval_seconds<=300 END
        WHEN 'get_round_dreamteam' THEN CASE WHEN p_round<>'' THEN j.descriptor->'args'->>'reference'=p_round ELSE NOT p_history AND j.native_interval_seconds<=300 END
        ELSE true END
      AND worker_finance_context_matches(j.descriptor,a.verified_leagues->p_league)
      AND NOT worker_has_resolved_peer(p_account,j.descriptor)
    ORDER BY j.resource_key FOR UPDATE OF j
  ), queued AS (
    UPDATE worker_jobs j SET league_refresh_at=requested,requested_at=greatest(j.requested_at,requested),
      due_at=CASE WHEN t.owner IS NULL THEN least(j.due_at,now()) ELSE j.due_at END
    FROM targets t WHERE j.account_id=t.account_id AND j.resource_key=t.resource_key
      AND (t.league_refresh_at IS NULL OR (t.owner IS NOT NULL AND t.started_at>=t.league_refresh_at
        AND requested-t.league_refresh_at>=interval '5 seconds'))
  )
  SELECT count(*) INTO eligible FROM targets;
  RETURN eligible;
END $$;
REVOKE ALL ON FUNCTION public.worker_page_refresh(text,text,text,text,text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_page_refresh(text,text,text,text,text,boolean) TO service_role;
NOTIFY pgrst,'reload schema';
