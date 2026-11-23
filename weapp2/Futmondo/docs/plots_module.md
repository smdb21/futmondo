# Plots Module

`plots_UI(id)` and `plots_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger = NULL)` provide the top-level **Plots** page. It presents one full-width, phone-friendly chart at a time: league standings, squad values, cash and transfers, rank progression, and real-team ownership. Chart controls belong to their chart tab and do not alter Classification-table filters.

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
