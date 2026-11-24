# Plots Module

`plots_UI(id)` and `plots_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger = NULL)` provide the top-level **Plots** page. It presents one full-width, phone-friendly chart at a time: league standings, squad values, cash and transfers, money movements, rank progression, and real-team ownership. Chart controls belong to their chart tab and do not alter Classification-table filters.

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
Category axes retain numeric ordering with equal spacing for rounds/subrounds.

The `round_numbers` attribute carries catalog coverage through the latest
observed round, excluding later scheduled rounds. Missing team records, absent
catalog rounds and explicit unknown scores keep cumulative results unknown from
that point onward; they never become zero or a sum of fewer rounds. Genuine zero
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
The two round status outputs recover even when hidden; they project data without
building inactive charts. Plot outputs retain normal hidden-tab suspension.
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
