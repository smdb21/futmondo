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

When explicit numbered history is incomplete and N is at most five, `average.fitness` supplies the requested trailing N source scores. The parser preserves the source sequence and unavailable positions; all N selected scores must be finite. These results have `recent_points_status="reported"` and an N-score count, without assigning round numbers or acknowledging a full-history fetch. Captures confirm that the sequence may preserve source order even when older numbered rounds are out of order. Complete explicit numbered history takes precedence. For N=5 only, `average.averageLastFive` remains the final scalar fallback; it cannot supply any other N. For N>5, including eight, the controller requests and reuses explicit round history, and the average requires every score in the N-round window. Zero and negative scores remain in the calculation. Finality does not exclude an observed numeric score.

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

## Optional filters and initial page defaults (2026-11-01)

`players_table_UI()` accepts `default_active_clause=FALSE` and `default_free_agents=FALSE`. Today opts into Free Agents Only initially; the Players page opts into Active Clause Only initially. Other call sites keep their previous unchecked defaults. Clear Filters continues to clear both checkboxes.

`players_table_filter_fields()` supplies typed missing values for source-optional filter fields. These additions are filter-only: the table projection removes newly added placeholders, preserving each endpoint's original displayed columns plus intentional owner/average columns. Missing roles, favorites, offer counts and clause metadata therefore cannot crash the unguarded schema observer, and unknown values are not converted to zero. A genuine zero-row source and a zero-match filter combination retain a renderable empty table and recover when filters change.

`players_table_active_clause(rows, now)` requires a positive observed clause price, an available transfer state (or an explicit normalized clause flag when the transfer field is absent), and no supplied future lock. Missing/empty lock dates mean no supplied lock, matching the Futmondo payload contract; malformed nonempty dates remain ineligible. UTC/offset timestamps use the shared parser. This does not change transaction submission checks or the separate Today clause helper.

Ownership normalization also produces an internal `is_free_agent` mask shared by the owner dropdown's Free agents choice and the checkbox. A populated owner ID alias or owner name takes precedence over `computer`. An explicit computer FALSE with unknown ownership is not treated as free; computer TRUE can establish a free agent when owner fields are absent. The internal mask is hidden from rendered columns.

## Points-to-price ratios

Today passes `points_ratio_basis="value"`; Players passes `"clause_price"`. The numeric column immediately after Avg Last N divides that same displayed average by the selected amount in millions of euros (points per €1M). Market value is `value`, not the listing asking price. Missing/nonpositive denominators and missing averages produce blank cells; zero and negative averages remain numeric. Both ratios can be sorted, and default ordering remains Avg Last N descending. Other tables omit the ratio. Ratios use the existing rows/history without requests or persistence; JavaScript formatting lets history-only updates refresh both numeric columns in place without recreating the table.

## Numeric metric color scales (2026-11-01)

Avg Last N has a continuous red (`#FF6B6B`) → yellow (`#F4C95D`) → green (`#7CFF9B`) background in every shared player table. Points-to-price columns use the same palette where already enabled. Each column has its own finite min/max over all loaded source rows, before any application filter, client search, sorting or pagination. The numeric midpoint is yellow; an equal-valued column is yellow throughout. Zero/negative numbers participate normally; unavailable/nonfinite cells remain uncolored. Dark cell text retains contrast across the palette; values remain numeric and sorting stays unchanged.

`players_table_enriched_RV` computes ownership, the N-round average and an optional ratio once per source/history/N update. Filters consume this prepared frame. `players_table_metric_ranges(rows)` returns named `list(min, max)` domains, with NULL bounds when no finite observations exist. `table_color_meta()` supplies `list(playerMetricRanges=<domains>)` to Reactable. `players_table_metric_style()` is a shared JavaScript column-style callback reading only the cell value and `state.meta.playerMetricRanges[column.id]`; it does constant work per cell and never scans visible client rows.

Initial widgets receive metadata; history updates send numeric data and metadata together through `updateReactable()`. Metadata is compared independently of displayed rows, so a changed filtered-out player recolors visible cells even when their values are unchanged. Such updates preserve the existing widget, page and selection. Changing N rebuilds the existing header/schema as before. No additional requests, timers, persistence, dependencies or browser asset are introduced.

Metric presentation update: color is now passed through a cell CSS variable to a padded 7px-radius badge around the value. The cell keeps its normal background; badge text is dark instead of inheriting the table's forced green text. Both metric headers use explicit line breaks and scoped wrapping to remain readable on narrow screens. Numeric data, global domains and update behavior are unchanged.

The Points column also uses the shared numeric badge renderer and its own finite min/max over the full loaded player dataset. Filtering and pagination do not change this scale; points remain numeric and sortable.

Filter layout: checkbox controls and Clear Filters are inside the same bordered panel as dropdown/numeric filters, in both shared layouts. My Team enables only Position and Active Clause Only in that panel; the average-round count remains a calculation control outside it.

### Positive-only metric scale (2026-11-02)
Points, Avg Last N and value ratios use compact pills from the active `--fm-surface` background at the source minimum to green at the maximum. Equal domains remain neutral. The callback reads the theme surface and chooses black/white text by mixed-background luminance (4.5:1 contrast); source-wide domains and numeric sorting are unchanged. The current app theme remains fixed dark; this does not introduce device-theme switching.

Position pills show GK, DEF, MID and FWD, retaining full names in tooltips and original data for filters and sorting.

Shared table headings now wrap at words (and within long labels when necessary), including Reactable rt-text-content. Offer, My Offer and Offers by Others have 180px minimum widths. Price fields are hidden only when their numeric values and missingness match another price field across the full loaded source; filtered subsets do not change this decision.


## Recent averages and compact identity columns — 2026-11-06

The five-round scalar fallback previously masked the absence of numbered history, while changing to two discarded the already available recent-score sequence. The local source-window calculation now fills that gap without changing acquisition, anchoring, persistence acknowledgements or the 1–20 input range. Wider windows continue through shared labelled history; N=8 never extrapolates the five-score source array. See the independent source-window test and the table ratio regression for known answers and warm reuse.

`get_reactable_columns_for_players(table)` keeps Name as a plain sortable/filterable value and renders the existing club crest beside it, using the source logo and existing club-name fallback. Team and logo remain hidden metadata for the name formatter. Club text appears in the crest alt text and name tooltip, long names wrap, and images retain lazy loading/async decoding and hide on error. `players_table_Server()` orders the visible identity/metric columns as Name, Position, Avg Last N, then the optional average/value or average/clause ratio. The removed Team column adds no image request: the existing crest moves into Name.

Performance: source fallback parses at most five values per affected player only when data/history/N changes; filters reuse enriched values. It introduces no HTTP/database call, timer, cache, extra stored observation or additional reactive publication. Crest markup uses the existing 22-pixel image and name cell. Local measurements and browser checks are recorded in data_review_results.md; they do not establish authenticated hosted behavior.


### Wider recent-round demand — 2026-11-06

A durable `scores` or `empty` check proves that a source request succeeded at its original time. It does not prove that its stored rows cover a later request for eight rounds. For active table clients requesting N>5, `availability_for()` compares the demanded numbered window with the stored/live matrix even when an old check exists. Only missing visible IDs enter the existing two-slot acquisition queue; already covered players and offscreen catalog IDs remain reusable. If the same session previously fetched only a short window, a wider request can recheck that player once. A successful wider source response then prevents further automatic replay for N changes or navigation in that session. A genuinely sparse response stays unavailable rather than inventing older scores.

`recent_points_covered_players()` now requires the league's latest labelled round to be at least N; a seven-round season cannot report complete eight-round coverage. Pending wider demand no longer counts as a completed player. The visible completion signature resets when the targeted request starts so newly retrieved scores publish immediately when all affected visible requests settle. Source observations still persist before the success check; a fresh session reuses all eight recorded rounds without a source replay. Explicit Refresh remains available for later corrections.

The existing check table does not store the requested N or a source revision token. A fresh session may recheck an old sparse/empty marker when it asks for a wider missing window; bounded in-session acquisition and completed-window reuse prevent a request loop. Current captures establish that player summary responses can return seven labelled rounds without an N parameter, but they do not prove any specific user's current source has eight. If the authoritative response has fewer than eight recorded scores, the cell stays blank.


## My Team asking price — 2026-11-07

My Team supplies a numeric `selling_by` projection using the authenticated manager’s listing price, with existing roster price aliases as fallbacks. `get_reactable_columns_for_players()` labels it **Selling by** and hides `market_inMarket`, `price`, `market_price`, `effective_market_price` and `market_asking_price` only when this projection is present. Raw values remain in row metadata for manual player-card actions. **Offer** continues to represent a received bid; the new My Team summary shows the full received set instead of only that highest-per-player value. Other table pages retain their existing listing-price display. Bid source/immutable bidder IDs are internal metadata. This projection adds no HTTP/database request or history work and preserves numeric sorting.

## Shared display columns — 2026-11-09

`table_display_data()` omits `clause_suggestedClause`, `fav` and `owner_team_id` for every player table. Filtering and player actions retain these fields in the full row source; hiding them never rewrites the source data. Renewal settings no longer govern whether Suggested Clause is displayed. The shared Reactable definition labels `average.fitness` as **Latest 5**, keeping the field key and values unchanged.

Performance: fewer displayed/payload columns and no new requests, database work, persistence, observers or calculations. Removing the old conditional visibility branch also removes a settings dependency from display-schema rebuilding.
