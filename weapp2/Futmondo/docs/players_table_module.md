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