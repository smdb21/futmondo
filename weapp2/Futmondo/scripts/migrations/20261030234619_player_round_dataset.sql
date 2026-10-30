-- Successful active-session acquisitions are recorded only after score writes.
-- Immutable check timestamps prevent older concurrent responses replacing newer checks.
CREATE TABLE IF NOT EXISTS public.player_round_history_checks (
 championship_id text NOT NULL,
 player_id text NOT NULL,
 season text NOT NULL,
 scoring_version text NOT NULL,
 checked_at timestamptz NOT NULL,
 result_status text NOT NULL CHECK (result_status IN ('scores','empty')),
 point_count integer NOT NULL CHECK (point_count BETWEEN 0 AND 500),
 PRIMARY KEY (championship_id, player_id, season, scoring_version, checked_at),
 CHECK ((result_status = 'empty' AND point_count = 0) OR (result_status = 'scores' AND point_count > 0))
);
ALTER TABLE public.player_round_history_checks ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.player_round_history_checks FROM PUBLIC, anon, authenticated;
GRANT SELECT, INSERT, UPDATE ON public.player_round_history_checks TO service_role;
CREATE OR REPLACE VIEW public.player_round_dataset
WITH (security_invoker = true) AS
SELECT 'point'::text AS row_kind,
 championship_id, player_id, season, scoring_version, round, points,
 score_status, observed_at, occurred_at, round_start_at,
 NULL::timestamptz AS checked_at, NULL::text AS result_status, NULL::integer AS point_count
FROM public.player_round_points_latest
UNION ALL
SELECT 'check'::text AS row_kind,
 championship_id, player_id, season, scoring_version, NULL::integer AS round, NULL::numeric AS points,
 NULL::text AS score_status, NULL::timestamptz AS observed_at,
 NULL::timestamptz AS occurred_at, NULL::timestamptz AS round_start_at,
 checked_at, result_status, point_count
FROM (
 SELECT DISTINCT ON (championship_id, player_id, season, scoring_version)
 championship_id, player_id, season, scoring_version, checked_at, result_status, point_count
 FROM public.player_round_history_checks
 ORDER BY championship_id, player_id, season, scoring_version, checked_at DESC
) checks;
REVOKE ALL ON public.player_round_dataset FROM PUBLIC, anon, authenticated;
GRANT SELECT ON public.player_round_dataset TO service_role;
COMMENT ON TABLE public.player_round_history_checks IS
 'Successful source checks, including empty histories; written after all scores are acknowledged.';
COMMENT ON VIEW public.player_round_dataset IS
 'Server-only league batch containing latest score revisions and successful source check metadata.';
NOTIFY pgrst, 'reload schema';
