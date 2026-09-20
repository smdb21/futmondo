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

## 6. FIS Score Rating Column

The player table includes a dedicated FIS Score column (`fis_score`) that displays each player's Fantasy Insight Score as a color-coded badge.

### Badge Color Tiers

| Score Range | Tier | Badge Color |
|---|---|---|
| 80 - 100 | Elite | Green |
| 60 - 79 | Strong | Blue |
| 40 - 59 | Average | Amber |
| 0 - 39 | Weak | Red |

### Tooltips

Hovering over the FIS Score badge reveals a tooltip with:
- Full FIS Score value (to one decimal place)
- Brief interpretive text (e.g., "Elite buy target", "Strong prospect", "Average value", "Weak outlook")
- Prediction Confidence percentage

---

## 7. FIS Tier Filter (`fis_tier_filter`)

A dropdown filter control (`fis_tier_filter`) is available in the filter bar above the player table. It allows users to narrow the displayed players by their FIS Score tier:

| Filter Value | Description |
|---|---|
| All | Show all players regardless of FIS Score |
| Elite (80+) | Only players with an FIS Score of 80 or above |
| Strong (60-79) | Only players with an FIS Score between 60 and 79 |
| Average (40-59) | Only players with an FIS Score between 40 and 59 |
| Weak (0-39) | Only players with an FIS Score below 40 |

The filter integrates with the existing reactive pipeline so that selecting a tier immediately updates the displayed table without requiring a full page reload.
---

## 8. Configurable Recent-Points Average

Every shared players table includes the numeric control **Average points: latest games**, initially set to 5 and constrained to 1–20. This applies to Your Team, Market, and All Players in Championship because all three use `players_table_UI()` and `players_table_Server()`. Changing the value recalculates the column heading and values immediately as `Avg Last N`.

`add_recent_points_average(players_df, history_df, n=5L)` returns `players_df` with numeric `recent_points_avg`. `players_df` requires `id`; finalized `history_df` uses `player_id`, `points`, and preferably `round`. For each player, the helper sorts rounds newest first, selects at most N finite observed scores, and calculates their arithmetic mean. Players with fewer than N recorded games use every available game. Missing observations remain `NA`. When N is exactly 5, the normalized API field `average.averageLastFive` is used only for players without stored finalized-round history.

```r
players <- add_recent_points_average(players, finalized_match_history, n=3L)
# players$recent_points_avg contains each player's observed last-three mean
```

The server obtains the league-wide finalized observations with one cached `read_player_match_history()` call rather than issuing per-player requests. The control uses a fluid Bootstrap row and moves to the left on narrow screens. It does not add a package or environment variable.

Focused verification: `Rscript test/test_recent_points_average.R`. Cross-page rendering is covered by `Rscript test/test_application_offline.R` and the full Shiny simulation.

---

## 9. User Team Owner Filter

The **User Team Owner** selector is built from every league team, even when the
player feed omits the team display name. Its labels are team names, but its
submitted values use the stable `team:<team_id>` form.

`players_table_normalize_owner(players_df, teams_df = NULL)` accepts a player
data frame plus the league-team data frame. It reads common normalized owner-ID
fields (`owner_team_id`, `user_team_id`, and Futmondo aliases), resolves an
empty player owner name from the league-team ID/name mapping, and returns
`owner_team_id` and `userTeam` columns. Empty owner IDs remain free agents.

`players_table_owner_choices(teams_df)` returns named select-input choices for
`All`, `Free agents`, and every named league team. New selections filter by
immutable ID; saved name-only selections retain a display-name fallback.

```r
teams <- data.frame(teamid = "team-7", teamname = "Aurora")
players <- data.frame(user_team_id = "team-7", userTeam = "")
players_table_normalize_owner(players, teams)$userTeam
# [1] "Aurora"
players_table_owner_choices(teams)["Aurora"]
