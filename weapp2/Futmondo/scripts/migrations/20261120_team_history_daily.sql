-- Daily standings history: the last observation per team per UTC day.
-- Lets the Plots module read a full season (about teams x days rows) instead of
-- every raw observation, which exceeded the 20,000-row read cap.
-- Read-only, SECURITY INVOKER, callable only by the server role.

CREATE INDEX IF NOT EXISTS user_team_history_daily_scope
  ON public.user_team_history(championship_id, competition_id, season, user_team_id, recorded_at DESC);

CREATE OR REPLACE FUNCTION public.league_team_history_daily(
  p_championship_id TEXT, p_competition_id TEXT, p_season TEXT)
RETURNS TABLE(user_team_id TEXT, teamname TEXT, day DATE, points INTEGER, budget BIGINT,
  "position" INTEGER, team_value BIGINT, recorded_at TIMESTAMPTZ)
LANGUAGE sql STABLE SECURITY INVOKER
SET search_path = public, pg_temp
AS $$
  SELECT DISTINCT ON (h.user_team_id, (h.recorded_at AT TIME ZONE 'UTC')::date)
    h.user_team_id, t.name, (h.recorded_at AT TIME ZONE 'UTC')::date,
    h.points, h.budget, h.position, h.team_value, h.recorded_at
  FROM public.user_team_history h
  JOIN public.user_teams t ON t.id = h.user_team_id AND t.championship_id = h.championship_id
  WHERE h.championship_id = p_championship_id
    AND h.competition_id = p_competition_id AND h.season = p_season
    AND p_competition_id <> 'unknown' AND p_season <> 'unknown'
    AND h.recorded_at IS NOT NULL
  ORDER BY h.user_team_id, (h.recorded_at AT TIME ZONE 'UTC')::date, h.recorded_at DESC, h.id DESC
$$;

REVOKE ALL ON FUNCTION public.league_team_history_daily(TEXT, TEXT, TEXT) FROM PUBLIC, anon, authenticated;
GRANT EXECUTE ON FUNCTION public.league_team_history_daily(TEXT, TEXT, TEXT) TO service_role;
NOTIFY pgrst, 'reload schema';
