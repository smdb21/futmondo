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