-- APPLIED 2026-11-20 (owner-approved) via the REST API in 120 id-batches; 59,682 rows relabelled.
-- Boundary: season opening budget movement at 2026-07-30T12:58:37Z (rival_finance_evidence).
-- Left 'unknown': 1,310 rows with NULL observed_at, 2 rows colliding with existing labelled rows.
-- Undo list (row ids) kept outside the repo; this SQL is the equivalent reviewed statement.
-- Relabels user_team_history rows saved under competition_id/season 'unknown'
-- for league 5b55f9d767214483120b87cf (OHY CAMPEÓN) to the observed context
-- 504e4f584d8bec9a67000079 / 2025-2026 (league_context_observations, 2026-11-20).
--
-- Evidence (read-only, 2026-11-20): 60,994 'unknown' rows, all in this league;
-- 285 rows already labelled 2025-2026. The unique index
-- user_team_history_source_time(user_team_id,championship_id,competition_id,season,observed_at)
-- means a relabelled row can collide with an existing labelled row; those
-- duplicates are skipped (the labelled row is kept), never overwritten.
--
-- Only rows observed on/after the start of the 2025-2026 context are relabelled.
-- Set the boundary below after confirming the season start; rows before it stay 'unknown'.
-- player_history / player_daily_snapshots also contain 'unknown' rows; they are
-- normalized views and are NOT covered by this draft.

BEGIN;

-- 1. Preview (run alone first).
SELECT count(*) AS to_relabel,
       min(observed_at) AS first_observed, max(observed_at) AS last_observed
FROM public.user_team_history h
WHERE h.championship_id = '5b55f9d767214483120b87cf'
  AND h.competition_id = 'unknown' AND h.season = 'unknown'
  AND h.observed_at >= TIMESTAMPTZ '2026-07-30T12:58:37Z'
  AND NOT EXISTS (
    SELECT 1 FROM public.user_team_history k
    WHERE k.user_team_id = h.user_team_id AND k.championship_id = h.championship_id
      AND k.competition_id = '504e4f584d8bec9a67000079' AND k.season = '2025-2026'
      AND k.observed_at IS NOT DISTINCT FROM h.observed_at);

-- 2. Relabel non-colliding rows.
UPDATE public.user_team_history h
SET competition_id = '504e4f584d8bec9a67000079', season = '2025-2026'
WHERE h.championship_id = '5b55f9d767214483120b87cf'
  AND h.competition_id = 'unknown' AND h.season = 'unknown'
  AND h.observed_at >= TIMESTAMPTZ '2026-07-30T12:58:37Z'
  AND NOT EXISTS (
    SELECT 1 FROM public.user_team_history k
    WHERE k.user_team_id = h.user_team_id AND k.championship_id = h.championship_id
      AND k.competition_id = '504e4f584d8bec9a67000079' AND k.season = '2025-2026'
      AND k.observed_at IS NOT DISTINCT FROM h.observed_at);

-- 3. Verify, then COMMIT (or ROLLBACK if counts differ from the preview).
SELECT competition_id, season, count(*) FROM public.user_team_history
WHERE championship_id = '5b55f9d767214483120b87cf' GROUP BY 1, 2;

ROLLBACK;  -- change to COMMIT only after review
