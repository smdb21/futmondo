# Players Table Module Documentation

This document describes the `Players_Table_Module.R` Shiny module, which renders a data table of players with filtering and position breakdown capabilities.

---

## 1. Overview

The Players Table Module provides two exported functions:
* `players_table_UI(id, show_position_breakdown = FALSE)` -- Renders the player table UI, including optional position breakdown card.
* `players_table_Server(id, data, ...)` -- Processes player data, applies filters, and renders the reactive table.

---

## 2. Parameters

### `show_position_breakdown`

Boolean parameter in `players_table_UI()`. When set to `TRUE` (enabled on Your Team page), renders a 2-column header layout with a Squad Position Breakdown card next to the filter controls.

When `FALSE` (default), only the filter controls and player table are rendered.

---

## 3. Squad Position Breakdown Box

When `show_position_breakdown = TRUE`, the module calculates and displays position counts for the squad:

| Position | Calculation |
|---|---|
| Goalkeepers | Count of players with `role` or `role2` matching goalkeeper |
| Defenders | Count of players with `role` or `role2` matching defender |
| Midfielders | Count of players with `role` or `role2` matching midfielder |
| Forwards | Count of players with `role` or `role2` matching forward |
| Total Squad | `nrow(df)` representing unique actual players |

The position breakdown card checks both the primary `role` and secondary `role2` fields to ensure accurate counting of players with dual-position classifications.

---

## 8. Configurable Recent-Points Average

Every shared players table includes **Average points: latest rounds**, initially 5 and constrained to 1–20. Today, Your Team, and All Players use the same implementation. Changing N updates `Avg Last N` locally from the league's shared round-score matrix; N is absent from all request keys.

`add_recent_points_average(players_df, history_df, n=5L)` accepts player `id` and history `player_id`, numeric `round`, and `points`. It returns `recent_points_avg`, `recent_points_count`, `recent_points_required`, and `recent_points_status`. The selected window ends at the latest observed round in the current league/season/scoring scope and contains N consecutive round numbers. The optional `latest_round` attribute preserves the window when its latest score is unknown. Every score must exist and be finite before an average is calculated. For example, rounds 10/9/8 with 18/19/7 points give 44/3; eight known scores give their sum divided by eight. Zero and negative scores remain in the denominator. A missing score displays coverage such as `7/8 rounds`, and does not become zero or a shorter-window average.

For N=5 only, Futmondo's `average.averageLastFive` remains a fallback when individual round coverage is incomplete. Its cell explicitly says `(reported)` and identifies Futmondo in the tooltip. It never supplies inferred N=3 or N=8 values. Finality is preserved as source metadata and does not exclude an observed numeric score. No fitness-array ordering is assumed.

`recent_player_points_source(session, login_token, championship_id, user_team_id, players_RV, is_active, refresh_trigger)` owns optional active-session acquisition and exposes reactive `history`, `status`, `states`, `sources`, and `pending` values. It starts after the primary page data settles, without a financial snapshot dependency:

- `read_league_round_points(championship_id)` retrieves one logical league batch from the latest-observation view. Pagination and coverage limits are documented in [round_points_matrix.md](round_points_matrix.md).
- Cached league-player and lineup resources provide the finite player catalog and scoring context. Current table rows receive priority. At most two asynchronous player-summary reads are pending; the catalog is capped at 2,000 distinct players.
- `get_player_recent_points(login, championship_id, user_team_id, player_id, scoring_version)` uses the existing player-summary endpoint, normalizes its explicit round/point records, and preserves the source season. Source responses and matching stored rows are reused for every N. A successful current source establishes the exact season/scoring scope and latest source round. `recent_points_saved_players(stored, context, acquired, now)` reuses recently observed players only when their saved rows include that round. Earlier gaps do not invalidate the whole player history. The freshness window is 24 hours for a known season/scoring context and one hour for an unknown context; neither window supplies a season identity. A partial latest-round DB fill cannot hide other players still missing that round.
- Changed observations are passed through the existing `player_card_history` persistence path, comparing prior matrix rows. Pending saves and failures remain visible; fetched does not mean acknowledged. Completion signals pause acquisition at 20 account jobs, 190 global jobs, or 14 MiB of queued payloads, below existing queue limits. There is no queue polling timer. The matrix is read once more after the finite fill and queued saves settle.
- Leaving the active view cancels outstanding generations and publication timers while retaining completed, empty, and failed acquisitions for that same account/league/team. Returning resumes pending work without resetting the completed-player counter or replaying completed summaries. Obsolete or closed-session responses do not update the UI or record points. Changing identity or an explicit refresh clears the acquisition state; refresh invalidates the current league's outer score resources and raw summary caches. For that finite refresh pass, current table IDs bypass saved-row reuse so corrected or previously omitted scores can be checked again; other catalog players remain eligible for reuse. The flag survives primary-page loading temporarily making the table inactive. N-specific caches do not exist.

Coverage remains factual. A summary can omit older rounds; the app saves every returned score but cannot manufacture omitted values or assume an undocumented older-round endpoint. Unknown season/scoring scopes remain separate from known scopes, and complete cross-session reuse cannot be guaranteed until that identity is established. The latest observed round is not a claim that every player's current or historical score is available. Source finished-flag semantics still await the user's HAR evidence.

Acquisition and table publication are separate. The exposed `history` contains only the current table's player IDs, together with the latest league-round anchor. Ordinary updates publish at most once every five seconds while acquisition is busy; the initial matrix, a context/table identity change, and completion of the visible player group publish immediately. Completing other league players with the same round anchor does not invalidate the table. An off-table source that establishes a newer round updates the window without inventing scores. Only one cancellable publication timer exists per active controller, with no idle polling.

The progress message uses a stable text output below the average input, with mobile wrapping and no Shiny recalculation dimming on that text. It reports retrieval and queued saving separately; successful retrieval is not a save acknowledgment. The explanatory paragraph was removed from the frequently changing status. Coverage and reported-average provenance remain in their table cells.

Performance review: database reads remain batched independently of N, API concurrency stays at two, and persistence backpressure and memory/catalog bounds remain unchanged. The source-season reactive changes only when its value changes, rather than on every player response. Reconciliation is restricted to table players when a snapshot publishes. In an actual Shiny browser fixture with three table players and an 83-player league catalog, an identical 15-second sample changed table update events from 72 additional updates before the fix to two after it; all 83 histories finished in the fixed run. At 390 pixels the sampled progress opacity was one and the text stayed within the viewport; the stable text output has recalculation dimming disabled. These local delayed-source measurements are not hosted response-time guarantees. No dependency, environment variable, database migration, or unattended worker was added.

Verification: `test/test_recent_points_average.R` passed 9 checks; `test/test_recent_points_async.R` passed 33 checks, retaining exact N3/N8, sparse/signed/NULL handling, backpressure, close/refresh behavior, and adding zero invalidations for 28 off-table completions, batched publication for 30 visible responses, timer publication, navigation/empty-result retention, exact-context freshness, partial latest-round DB coverage, off-table round-window advancement, and targeted table refresh without replaying other catalog players. The mandatory offline lifecycle passed 13 checks plus eight retained pages with zero external HTTP/live database mutation attempts. The primary agent ran the browser comparison above. Live Futmondo retrieval, actual save success, production concurrency, and hosted timing were not exercised.

---

## 9. Immediate Player-Card Opening

Selecting a player sends the modal shell to the browser before enabling the
selected-player reactive. The module uses `session$onFlushed(..., once=TRUE)`
to start summary and history requests in the following reactive
cycle. Until then, the card displays a loading message. This keeps API latency
from delaying click feedback while retaining the selected player ID and table
context checks.

## 10. User Team Owner Filter

The **User Team Owner** selector is built from every league team, even when the
player feed omits the team display name. Its labels are team names, but its
submitted values use the stable `team:<team_id>` form.

`players_table_normalize_owner(players_df, teams_df = NULL)` accepts a player
data frame plus the league-team data frame. It reads common normalized owner-ID
fields (`owner_team_id`, `user_team_id`, and Futmondo aliases), coalescing them
row by row when an earlier alias is empty. It resolves an empty player owner
name from the league-team ID/name mapping, and returns
`owner_team_id`, `user_team_id` (the same resolved ID), and `userTeam` columns.
Keeping both ID columns consistent lets selected-player cards and action handlers
recognize the same owner as the filter. Empty owner IDs remain free agents.

`players_table_owner_choices(teams_df)` returns named select-input choices for
`All`, `Free agents`, and every named league team. New selections filter by
immutable ID; saved name-only selections retain a display-name fallback.

```r
teams <- data.frame(teamid = "team-7", teamname = "Aurora")
players <- data.frame(user_team_id = "team-7", userTeam = "")
players_table_normalize_owner(players, teams)$userTeam
# [1] "Aurora"
players_table_owner_choices(teams)["Aurora"]

## Market access through Today

Today is the sole market-listing page. Its shared players table includes both Your Bids Only and Bidded Only filters, as well as the existing player-card/manual actions. The redundant Market navigation/tab/server instance has been removed. Filters operate on already-loaded rows and do not add requests.
