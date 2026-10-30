-- Latest observed score per player/round, preserving all source revisions.
-- Uses player_round_context_revision; no duplicate covering index is needed.
CREATE OR REPLACE VIEW public.player_round_points_latest
WITH (security_invoker = true) AS
SELECT DISTINCT ON (championship_id, player_id, season, scoring_version, round)
  championship_id, player_id, season, scoring_version, round, points,
  score_status, observed_at, occurred_at, round_start_at
FROM public.player_match_observations
ORDER BY championship_id, player_id, season, scoring_version, round, observed_at DESC;
REVOKE ALL ON public.player_round_points_latest FROM PUBLIC, anon, authenticated;
GRANT SELECT ON public.player_round_points_latest TO service_role;
COMMENT ON VIEW public.player_round_points_latest IS
  'Server-only current player/round observations. NULL points remain unknown; source revisions are retained.';
NOTIFY pgrst, 'reload schema';
