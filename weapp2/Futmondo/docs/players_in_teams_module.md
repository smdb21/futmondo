# Players In Teams Module Documentation

This document describes the player roster visualization components rendered within the Shiny application, including the Liquid Cash Plot.

---

## 1. Overview

The Players In Teams module provides visual summaries of team composition and financial status across all user teams in a given league. Key components include:

* `league_finances_plot` -- Horizontal bar chart rendering liquid cash balances across all user teams in the league.

---

## 2. Components

### A. `league_finances_plot`

Horizontal bar chart rendering liquid cash balances across all user teams in the league. Calls `calculate_league_finances()` to compute total player acquisition costs (`Initial Budget - sum(buyPrice)`). Color-coded emerald green for positive cash balances and crimson red for negative balances.

#### Data Pipeline

1. **Input**: Receives the authenticated `login` token, `championship_id`, and a data frame of all user teams (`user_teams_df`) from the active league.
2. **Computation**: Invokes `calculate_league_finances(login, championship_id, user_teams_df)` which iterates over each user team, fetches current rosters, and calculates:
   - `total_spent`: Sum of all `buyPrice` values for players on the roster.
   - `budget` (Money Left): `Initial Budget - total_spent`.
   - `team_value`: Aggregate market value of the squad.
   - `net_profit_loss`: `team_value - total_spent`.
3. **Rendering**: Produces a horizontal bar chart where each bar represents a team's liquid cash balance. Bars with positive values are colored emerald green, while bars with negative values are colored crimson red.

#### Parameters

- `login`: Character vector/list containing `token` and `userid`.
- `championship_id`: String championship ID.
- `user_teams_df`: Data frame of all user teams in the championship.

#### Return Value

A `ggplot2` object suitable for rendering via `renderPlot()` in a Shiny UI.

### B. KPI Summary Boxes (2x2 Layout)

A 2x2 grid of summary boxes rendered at the top of the Players In Teams tab. Provides an at-a-glance financial overview for the selected team. The layout uses two `fluidRow` containers (two boxes per row) to form a compact 2x2 grid.

| Position | Label | Description |
|----------|-------|-------------|
| Top-Left | Classification Rank | The team's current position in the league classification. |
| Top-Right | Points | The team's accumulated league points. |
| Bottom-Left | Total Volume Earned | The cumulative inflow volume: `300,000,000` (initial budget) + total sales revenue + all bonuses (round bonuses, point bonuses, etc.). |
| Bottom-Right | Total Volume Spent | The cumulative outflow volume: the sum of all player purchase costs (`sum(buyPrice)`) across the full transaction history. |

#### Data Pipeline

1. **Input**: Receives the authenticated `login` token, `championship_id`, and the selected team identifier.
2. **Computation**: Derives each metric from the team's live data via the existing API pipeline shared with `league_finances_plot`. Total Volume Earned aggregates the initial budget, all sales proceeds, and all bonus inflows. Total Volume Spent aggregates all purchase outflows.
3. **Rendering**: Outputs four summary cards arranged in a 2x2 Bootstrap grid (two `fluidRow` containers, each with two columns), rendered inside the module UI.

#### Parameters

- `login`: Character vector/list containing `token` and `userid`.
- `championship_id`: String championship ID.
- `team_id`: String identifier of the selected user team.

#### Return Value

A Shiny `tagList` containing four summary boxes, suitable for rendering via `renderUI()` in a Shiny UI.

---

## 3. Bulk Squad Market Listing

### "Put All Players on Market" (`btn_put_all_on_market`)
Prompts a warning modal to confirm the user's intent. On confirmation, executes a bulk listing of all squad players via `put_all_on_market()`, which calls `POST https://api.futmondo.com/5/market/putallonmarket`. On success, invalidates the API cache and displays a confirmation notification.

---

## 4. Player Table Enhancements

The player roster table on the Your Team tab has been enhanced with additional columns and display refinements.

### A. "In Market" Status Badge Column

A column inserted immediately after the `team` column in the player roster table. The header name is `"In market"`. Indicates whether a player is currently listed for sale on the marketplace.

#### Rendering

| Condition | Cell Content |
|-----------|-------------|
| Player listed on market | `20.852.517 EUR` -- renders the formatted asking price alone inside an amber status badge. |
| Player not listed | `""` -- renders as an empty cell. |

#### Data Pipeline

1. **Input**: The player record from the roster API response.
2. **Computation**: Checks the `market_listing` flag (or equivalent) on each player record. If set, formats the `asking_price` value with thousand separators and the Euro symbol.
3. **Rendering**: Outputs the formatted asking price wrapped in an amber status badge, or an empty string, rendered within the table cell.

### B. Hidden "Your Bid" Column

The `bid_price` column can be hidden on the Your Team tab via a configuration parameter.

#### Parameter

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `hide_bid_column` | Logical | `FALSE` | When set to `TRUE`, the `bid_price` column is excluded from the player table rendered on the Your Team tab. |

#### Usage Example

```R
render_player_table(login, championship_id, team_id, hide_bid_column = TRUE)
```

### C. "Received Offer" Column

A single column, `"Received Offer"` (`bid_price`), displays incoming offers. The offer amount is rendered in an emerald badge with a hover tooltip (`title="Offer from [Bidder]"`). For system-generated offers where the bidder name is empty, the tooltip defaults to `"Offer from Futmondo"`.

The standalone `"Bidder"` (`bid_user`) column is hidden from the table via `colDef(show = FALSE)`.

#### Rendering

| Condition | Cell Content |
|-----------|-------------|
| Incoming offer present | Offer amount in an emerald badge with hover tooltip showing the bidder name. |
| No incoming offer | `""` -- renders as an empty cell. |

#### Data Pipeline

1. **Input**: The player record from the roster API response, including `bid_price` and `bid_user` fields.
2. **Computation**: Extracts the `bid_price` and `bid_user` values directly from the player record. If `bid_user` is empty (system offer), the tooltip text defaults to `"Futmondo"`.
3. **Rendering**: Displays the offer amount in an emerald badge with a hover tooltip. The `bid_user` column is hidden via `colDef(show = FALSE)`.

### D. Sanitized V1 Column

Unnamed columns introduced by JSON parsing artifacts (commonly labeled `V1`) are sanitized during data processing and hidden from the user-facing display.

#### Behavior

- During JSON deserialization, any column with the name `V1` (or other unnamed-generic identifiers) is detected and removed from the display data frame.
- This prevents spurious empty or malformed columns from appearing in the player table.
- The sanitization occurs before any rendering step, ensuring clean table output regardless of API payload variations.

---

## 5. Resizable Columns

All Reactable tables in the Players In Teams module are initialized with `resizable = TRUE`, allowing users to click and drag column borders to adjust column widths to their preference.

#### Behavior

- Users can resize any column by hovering over the column border until a resize cursor appears, then clicking and dragging horizontally.
- The adjusted width persists for the current session and applies to all rows within the table.
- This is particularly useful for columns with long text values (e.g., player names, market listings) that may be truncated at default widths.

#### Configuration

The `resizable` option is set directly on the `reactable()` call:

```R
reactable(
  data,
  resizable = TRUE,
  # ... other options
)
```

---

## 6. Shortened Cache Timeouts

Roster and bid cache timeouts have been reduced to `timeout_sec = 15-30s` across the Players In Teams module. This ensures that live offers (such as a bid of 20.097.019 EUR on Borja Iglesias) render immediately without unnecessary delay.

#### Rationale

The default cache timeout was too long for time-sensitive market data. Incoming offers, bid updates, and market listings can change rapidly during active trading periods. Shorter timeouts ensure the user sees the latest data without stale cache masking real-time updates.

#### Affected Caches

| Cache | Timeout | Purpose |
|-------|---------|---------|
| Roster cache | `15s` | Player roster data fetched from the roster API. |
| Bid cache | `30s` | Incoming offer and bid data fetched from the bid API. |

#### Implementation

The timeout is passed to the `get_cached_data()` wrapper function:

```R
# Roster data with 15-second cache timeout
get_cached_data(
  key = paste0("roster_", team_id),
  fetch_fn = fetch_roster,
  timeout_sec = 15
)

# Bid data with 30-second cache timeout
get_cached_data(
  key = paste0("bids_", team_id),
  fetch_fn = fetch_bids,
  timeout_sec = 30
)
```

#### Impact

- Live offers appear within 15-30 seconds of being placed by other users.
- Tab switches remain sub-millisecond for cached data within the timeout window.
- API rate limits are respected since caching is still active; only the cache duration is shortened.

---

## 7. Standings Evolution Plot (Bottom Placement)

The league standings evolution plot has been repositioned to the bottom of the Players In Teams tab, below the player roster table and all other interactive components. This placement ensures that the primary data table and KPI boxes are immediately visible without requiring the user to scroll past a large chart.

#### Behavior

- The plot renders as a time-series line chart tracking the standings position (rank) of each team across matchdays.
- Each team is represented by a distinct colored line, with the user's own team highlighted for quick identification.
- The plot is rendered via `plotlyOutput` for interactivity (hover tooltips, zoom, pan).
- Positioned at the bottom of the tab, after the player table and any filter controls.
