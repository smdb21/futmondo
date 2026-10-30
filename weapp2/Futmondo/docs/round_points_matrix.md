# Player/round score matrix

The canonical observations remain in `player_match_observations`. Every row identifies a player, championship, season, scoring version, round, source score status, and observation time. Corrections are retained as revisions rather than overwriting the historical record.

`player_round_points_latest` projects the latest revision for each complete identity key. A latest NULL score stays unknown; the view does not resurrect an older numeric value. It uses the existing `player_round_context_revision` index. A second index or a physically wide table is unnecessary. The view is `security_invoker=true`, has no PUBLIC/anon/authenticated grants, and grants SELECT only to the server service role. Credentials remain on the R server. This follows the [Supabase view/RLS guidance](https://supabase.com/docs/guides/database/postgres/row-level-security#views).

`read_league_round_points(championship_id, max_rows=50000L)` loads the league's latest observations as one logical batch. REST pagination uses deterministic player/season/scoring/round order, 1,000-row requested pages, and actual returned row counts. Short server pages are followed until an empty page; the maximum-row cap produces explicit partial coverage. N is not a query parameter. Callers must preserve the season and scoring scopes when calculating averages. The result is a long-form matrix that can be indexed or pivoted locally without additional database queries.

## Applied database change

Migration `20261030075801_player_round_points_latest.sql` was applied to the configured Supabase project through the connector. Existing observations were not rewritten. The live source table contained seven observations for one player at inspection time, so population of the remaining matrix requires Futmondo retrieval; the view alone does not create missing scores.

Live known-answer tests inserted synthetic revisions inside a transaction and rolled back: latest revision selection, zero, negative, latest NULL, and season/scoring/league separation passed, with zero retained test rows. Grant checks confirmed server SELECT and denied anon/authenticated SELECT. The actual query used `player_round_context_revision`, returning the seven existing rows in 1.039 ms execution time (not representative of larger hosted loads). Independent SQL validation using PGlite also passed. Existing security-advisor findings concern unrelated pre-existing objects; the new view has no browser-role grants.

The application reader's independent fake-PostgREST test returned all 3,020 rows despite a 700-row server page cap; capped and empty results were reported correctly. Direct R-to-live-REST validation was not available because this workspace lacks database credentials; the configured Supabase connector was used for live SQL verification. The hosted application must use its existing server credentials.

Rollback: restore the preceding application reader, then drop only `public.player_round_points_latest`. Source observations and their existing index remain intact. No backfill or live Futmondo account access has been performed from this workspace.
