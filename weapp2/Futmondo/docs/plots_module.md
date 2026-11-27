# Trends

## Points and ranks (2026-11-26)

**Trends → Points** opens with cumulative season points. Use the **Points / Ranks** switch to see each team's league position for the same rounds. First place appears at the top. A missing score leaves a gap in the affected team's points, and a round with incomplete comparable totals has no verified ranks.

**From round** and **Through round** select an inclusive interval in kickoff order, including decimal subrounds and fixtures played early. Choose the same round at both ends to inspect one round. The first and latest published rounds are selected by default. Changing the view keeps both endpoints. With **First published**, Points shows season totals. Selecting a specific starting round adds a zero marker before that round and sums only the scores in the interval. For example, round scores of +5 and −3 appear as 0, 5, 2. Ranks always use season totals.

`plots_UI(id)` and `plots_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger = NULL)` provide five chart tabs on **Trends**. Points, Squad values, Cash & transfers, Money movements and Owned by real team each have their own controls. The Points tab sends the selected metric and visible interval to one Plotly output. The switch reuses the existing team histories and published rankings. `plots_interval_points(rows)` uses the reconciled per-round scores in kickoff order. A missing score within the interval interrupts that team’s total. Confirmed nonparticipation leaves a gap and allows the total to resume at the next score.

`plots_owned_by_real_team(players)` is a pure helper returning `real_team_id`, `real_team`, and `owned_players`. It counts each immutable player ID once only when both a current fantasy owner ID and a real-team ID/name are present. Free agents and rows with unverifiable club data are excluded. Rows are ordered by descending count then club name.

```r
plots_owned_by_real_team(data.frame(id = 'p1', user_team_id = 'manager-1', teamId = 'club-1', team = 'Club'))
# real_team_id real_team owned_players
# club-1       Club      1
```

For crowded multi-team history series, persistent legends are intentionally hidden so they do not shrink the phone plot; every trace retains its team label in its hover tooltip. Focused verification: `Rscript test/test_plots_module.R`.


## Round-based standings and chart presentation — 2026-11-23

Standings previously sent `user_team_history.points` observations to
`plots_history()`, using `recorded_at` as x. These were cumulative snapshots,
including unchanged observations and later revisions, rather than round results.
A downward segment establishes that consecutive saved totals differ; the
screenshot alone does not identify the native reason for that revision. No live
account diagnosis or historical-data repair is claimed.

Standings and rank now share `plots_round_totals(rows)` and
`plots_round_chart(totals, metric = "points")`. `rank_rows()` reads the existing
worker-backed `get_user_team_rounds` and `get_classification_round_catalog`
observations. `classification_round_rows()` maps opaque source round IDs through
the catalog, retains numeric subrounds such as 6.2 and resolves duplicate
team/round observations to the last supplied revision. The pure projection uses
one cumulative sum per team, then ranks each round (ties use minimum rank).
Category axes use the same first-kickoff order as the cumulative calculation. A numeric subround without its own date follows its parent round when both appear.

The `played_rounds` attribute carries the numbers of observed, active, partially played and finished rounds from the shared standings projection. For 2026–27 LaLiga, J23 can appear between J14 and J15 while J16–J22 are still upcoming. The chart applies the latest observed total for each round once. Later corrections revise that point on the curve. Exact December subtotals require a dated observation from December. Missing team records and explicit unknown scores in a published round keep later cumulative results unknown; they never become zero or a sum of fewer rounds. Genuine zero
and negative points remain intact. `connectgaps = FALSE` and a status explain
incomplete saved evidence. Cumulative totals are computed before applying the
rank display range. The former single-round inspector is removed. The range
allows decimal round boundaries.

No new acquisition or persistence route is introduced. Bound factual reads reuse
the existing context-scoped durable database snapshots and their worker freshness
and completeness state; navigation does not call the source API. The shared
round projection is reactive and reused by both charts. Failed reads propagate
through the existing resource adapter instead of being converted to empty
successful histories. Database schema and worker cadence are unchanged.

Squad valuations retain their observation instants and account timezone on a real
date axis, with `%b-%d` ticks and date-only hover labels (e.g. Nov-12). Cash uses
a 4/8 desktop column split for dates/metrics, stacked on phones, with vertical
radio options. Cash and ownership bars explicitly use muted blue `#608BC1`, which
survives theme changes. Each chart includes a description; cash explicitly
identifies transfer-only estimates and exclusions.

`plots_owned_chart(players)` reuses `plots_owned_by_real_team()` for counts and
`players_table_clubs()` for exact-ID source crest mapping. It draws count labels
outside horizontal bars, reserves right-side headroom and a 60 px top margin,
and places source crests beside club names. Its bounded resize listener maintains
a 22 px crest width and 8 px axis gap (row height can constrain the final image),
only relayouts when sizing changes, and replaces its previous listener on rerender.
Missing logos never generate guessed club filenames. Browsers may request one
existing static crest per represented club; no factual API request is added.

Checks: `Rscript test/test_plots_module.R`; `Rscript test/test_plots_browser_app.R`
with `node test/test_plots_browser.cjs` in a separate process. The browser fixture
uses synthetic saved observations, blocks external requests except fixture crest
images, and supports the standard `FUTMONDO_TEST_NODE_MODULES` and
`FUTMONDO_TEST_CHROMIUM` overrides. No dependency or deployment setup changed.


## Independent saved-round loading and redraw reuse — 2026-11-23

The first round-based implementation called the catalog, then each team history
without capturing pending Shiny read conditions. A pending catalog or team
aborted the entire reactive, so later teams were not registered until earlier
reads settled. One missing/failed read could therefore suppress all curves.

`catalog_state()` and `round_state()` now capture each dependency with the
existing `classification_read_state()` contract. Every known team registers its
own saved-data interest in the same reactive pass; the existing worker read batch
coalesces them. Numeric native round numbers can be used while the ID catalog is
pending. Opaque IDs still require the authoritative catalog. Verified teams render
independently; a missing team keeps unknown scores. League ranks remain unknown
for any round without complete comparable scores for every team.

`round_status()` reports pending reads visibly outside the collapsed information
panel, and reports failed reads through the existing safe failure references.
One `chart_status` output selects messages for the active chart. Standings and
Rank progression share the round status, so each displays a single warning when
scores are missing. The page information panel receives only the selected
chart's notices because it collects disclosures outside the individual chart
tabs. Money movements displays its own transfer status inline. Plot outputs
retain normal hidden-tab suspension.
`round_totals` and `round_domain` use `resource_display_source()` so a changed
source timestamp/version with identical visible facts does not redraw the chart
or reset the user's rank range. No new timer, worker job, source call, persistence
schema or cache layer is introduced; existing scoped durable observations remain
the source of browsing data.

Regression: `test/test_plots_loading.R` uses the real shared adapters and held or
rejected reads. `test/test_plots_loading_browser_app.R` with
`test/test_plots_loading_browser.cjs` uses the real worker descriptor, compressed
snapshot decoder, page information scripts and actual plots. It checks pending
and recovered histories, unchanged publications, input heartbeats and bounded idle
browser work at phone and desktop widths. All fixtures use synthetic data and
forbid source HTTP. Exact hosted diagnosis is separate from these local checks.


Approved private-response follow-up (2026-11-23): one actual saved native
team-round response passed the real codec, adapter and module replay with all
13 source scores retained. No body/score/identifier fixture was committed.
See the dated review in `data_review_results.md`. This verifies that response's
compatibility with the loading fix, without claiming a signed-in hosted freeze
reproduction or complete league coverage.


## Readable points axis — 2026-11-23

The points branch of `plots_round_chart()` previously included
`dtick = NULL` in its y-axis list. The Plotly R layout serialized this entry as
`dtick: []`. In the browser this produced a unit tick step: one label and
gridline for every integer score. Even a 105-point synthetic fixture drew 119
overlapping labels; league totals above 1,000 created much heavier SVG work.

The points axis now omits `dtick` entirely and sets `tickmode = "auto"` with
`nticks = 6`. Plotly chooses a readable interval; the 1,190-point fixture shows
three labels (0, 500 and 1,000). Rank keeps `dtick = 1` and reversed ordering.
Source scores, cumulative sums, completeness handling, subround identities,
snapshot reuse and loading behavior are unchanged.

The loading browser fixture now uses distinct team totals above 1,000 and checks
actual rendered tick/grid counts, label separation and opacity on phone/desktop
in both themes. The R regression checks that points layouts have no `dtick`
key while rank layouts retain integer steps. This fixes chart rendering and
reduces browser work without adding API/DB operations or timers.


## Money movements · 2026-11-23

The Money movements tab sums league transfer prices in EUR. It counts each
saved transfer once, including trades between managers, market transactions
with a blank counterparty, and clause payments merged with their eventual
delivery by the existing pressroom parser. The chart covers transfer prices.
Opening funds, rewards and other finance adjustments are outside that metric.

Cash & transfers and Money movements share a reactive
`get_championship_pressroom(login, championship_id)` read. In deployed worker
mode the existing adapter reuses the scoped session result or reads the
persisted worker snapshot. The saved observation retains its account and
championship scope, source timestamps, freshness and completeness. The new
chart adds no acquisition handler, storage schema, timer or API call.

`plots_transfer_days(history, timezone)` projects `created` to the account's
calendar date and validates the recorded `price`. `plots_transfer_periods(rows,
period)` groups the projected facts by day, Monday–Sunday week (default), or
calendar month. It returns inclusive start and exclusive end dates, the sum of
amounts, and the number of transfers. `plots_transfer_chart(bars)` uses one bar
per observed bucket on a date axis, retaining the elapsed spacing between
buckets. Tooltips give the calendar interval, total EUR and transfer count.
Pending, unavailable, incomplete, stale and confirmed-empty snapshots have
visible states. A missing date or amount leaves the affected row out and marks
the displayed totals incomplete.

The source adapter's verified route is
`POST https://api.futmondo.com/1/locker/pressroom`. The authenticated envelope
carries `header.token` and `header.userid` with
`query = {championshipId, from}`. The route takes the fantasy championship ID.
The parser projects `answer.news[]` from `_id`, `created` and `price` and
reconciles clause events through the established Locker merge. The current
[official client bundle](https://app.futmondo.com/main.dart.js) was inspected
on 2026-11-23 (SHA-256
`08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`) and confirms
the route and pagination arguments. Browser checks use synthetic saved rows
and the production snapshot codec. See [official comparison](feature_comparison.md#league-transfer-activity-over-time-2026-11-23).

### Date and amount snapshot · 2026-11-23

Money movements reuses a decoded, current Pressroom snapshot when one is
already loaded. On a cold read it requests `get_championship_transfer_amounts`
through the normal scoped worker database read. The date-and-amount frame
has only `created` and `price`. The worker publishes it as a derived child
of an existing complete Pressroom observation, retaining its scope, observed
time and validity interval. Projection introduces no new acquisition job,
Futmondo request, schema or polling timer.

A successful database response with no projection lets the histogram use
the existing saved Pressroom reader and exact-scope audited baseline.
The baseline is marked stale while the regular snapshot is unavailable.
A pending or failed lookup remains distinct from a verified empty history.
Cash & transfers continues to use the full record because its calculations
need counterparties. Period controls reuse the loaded date and amount data.

## Historical participation and faster first curves — 2026-11-25

Standings requests the signed-in team's saved round history in the first
database batch, alongside the team directory and round catalog. As soon as its
name and scores arrive it can draw the first curve. The other teams still load
together as soon as the directory is available. Squad valuations load only
when selected or needed to initialize the Cash & transfers date controls.
Returning to Cash & transfers preserves the selected dates when the league and
history bounds are the same.

After the team histories settle, `plots_ranking_targets()` selects published rounds with a missing score and no evidence of pending games. An explicitly unfinished or partially played round cannot establish final nonparticipation, even if an interim ranking uses a closure code. It reuses the existing scoped database snapshots for
`get_classification_round_ranking`. That handler's verified first-party request
is `POST /1/ranking/round` with the fantasy `championshipId`, viewer `userteamId`
and the opaque round ID in `roundNumber`. The persisted parser output carries
the exact round ID/number and ranking rows with team identity and points.
See [Classification's source contract](classification_module.md) and the
[official public client](https://app.futmondo.com/main.dart.js).

`plots_saved_round_ranking()` validates identities, finite scores, unique
participants and agreement with known scores. Native metadata `P` or `V` is
accepted for historical participation only with separate evidence of closure
from the catalog or matching closed team records. A corroborated ranking can
supply an unread score or verify a team's absence for that round. An absent team
has no point or rank marker for that round and can start its curve later.
Unknown scores still interrupt affected curves and keep incomparable ranks
unavailable. If a recovered ranking lists teams outside the current directory,
the ranks in that round require their cumulative history. The warning reports
the first genuinely uncovered round once reads settle.

The chart reuses persisted ranking payloads, source timestamps and scope
through the existing database-first read and bounded replay. Its points widget
is keyed by the visible point series, so newly resolved ranks alone do not
republish Standings. No browser navigation acquires Futmondo data.

## Published scores during a postponed round (2026-11-26)

Trends keeps the saved navigation ID `plots`. Standings and Rank progression use the published scoring stage while a fixture is postponed. The calendar fields `partially_played = TRUE` and `status = "closed"` together identify a candidate. The factual worker already stores the ranking, catalog and team histories for the same league and scoring context.

For a stage, `plots_saved_round_ranking()` requires an exact round match and an independently closed team-history row agreeing with every ranked participant and score. An unranked team's absence counts for that stage only when its successfully retrieved history also has no row for the round. A failed or partial team-history read leaves that team's result unknown. When the worker publishes revised scores, both charts recompute from the new observations. Final round awards continue to follow their own settlement rules.
