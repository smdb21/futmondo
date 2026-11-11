> Current contract (2026-10-31): [Rival cash accounting](rival_cash_accounting.md) is authoritative. Summary, league table and transaction history share one reconstructed ledger. Actual dated movements show running cash; all included rewards appear in the main financial history, with source-backed reconstructed dates labelled and unresolved dates kept explicit. Observed official cash is separate. Older implementation descriptions below are historical and superseded by that contract.


## Current directory display — 2026-11-02

The signed-in team's row has a subtle background tint in both the rival finance table (including its fallback directory) and all standings views. Matching uses the active team ID, independently of scouting selection, names and sorting. The shared CSS adapts to light/dark themes and leaves the selected-row indicator intact. Each table assigns one class per existing row; no API/database reads, caches, timers or additional calculations are introduced.

Focused verification: `test_rival_summary_layout.R` covers normal/fallback rendering, duplicate names, missing IDs and selection independence. The rival summary and classification lineup browser checks cover phone/desktop widths, theme changes, sorting and round/partial views. Live hosted testing and the broader suite are deliberately skipped for this presentation change.

Own-team cells must be transparent so the table theme's opaque cell backgrounds do not cover the row tint. Browser regressions check the cell backgrounds in both themes as well as the row class/color; checking the row color alone missed this layering defect. This CSS correction adds no data work or reactive updates.

The **Player transactions** column counts included purchases and sales, including clause transfers, once per reconciled ledger entry. Bonuses, fees, opening cash and excluded entries are omitted. The numeric count is sortable and covers retrieved history; unavailable history stays unavailable, while a known history containing only bonuses gives zero. Counts are cached with the existing per-team ledger calculation and projected into the table without new requests, persistence or trade matching.

The selectable financial standings table now retains public **Squad Value**, **Last Access (UTC)** and **Points** even when financial details are unavailable. Raw monetary/score values and numeric timestamps remain sortable. `rivals_last_access_cell()` formats UTC dates with a precise timestamp tooltip. `rivals_points_pills()` precomputes one league-wide background-to-red scale; negative/zero scores remain numeric, missing scores remain unavailable, and equal scores use the background color. Text switches between black/white for readable contrast. Sorting and pagination preserve the color domain.

`rivals_summary_values()` returns `squad_value` using directory `teamValue`, ledger `team_value`, then information `teamValue`. The selected card no longer recomputes a roster sum or invents zero while loading. These display changes make no extra data requests. The former fixed €300M default described in historical sections below is removed; see the current accounting contract.


# Rivals Module - Rivals Scouting Page

This document describes the `Rivals_Module.R` Shiny module that implements the "Rivals" scouting page. It allows the logged-in user to select any team in the championship, view their financial overview, and inspect their full player roster with clause-to-value ratio indicators.

---

## 1. Module Overview

The Rivals Module provides two exported functions:

* `rivals_UI(id)` -- Shiny UI module that renders the scouting interface.
* `rivals_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV)` -- Shiny server module that drives reactivity and data fetching.

---

## 2. `rivals_UI(id)`

### Parameters

| Parameter | Type   | Description                              |
|-----------|--------|------------------------------------------|
| `id`      | `char` | Unique namespace ID for the Shiny module. |

### Return Value

Returns a `tagList` containing:
1. A Net Transfer Profit/Loss KPI box (standalone summary box in the top summary row).
2. A League Financial Standings & Budget Left table (`reactableOutput`) displaying per-team finances.
3. A `uiOutput` placeholder (`scouted_rival_details_ui`) for the selected rival details, which renders:
    - Financial summary cards (Standings Position, Money Left, Trading profit, Squad Investment, Squad valuation and Overall wealth gained).
- A `tabsetPanel` (type = "pills") with tabs including:
      - **Tab 1: "Player Roster & Clauses"** -- Contains the `players_table_UI` call for the scouted player roster and purchase breakdown.
      - **Tab 2: "Transaction & Financial History"** -- Displays all transactions directly (no filter bar). Includes a "Pivot by Player" toggle that switches the view between the full chronological transaction log and a per-player pivot summary.
      - **Tab 3: "League Cash & Observed Transfers"** -- Contains the league-wide cash/transfer chart, date-range slider, and metric selector for transfer-only balance, squad purchases, or transaction volume.
      - **Player Buy/Sell Pivot Ledger** -- A paired buy/sell comparison view for each player the rival team has ever acquired, with two-line hover tooltips, re-bought player handling, and Net P/L calculation.
4. A League Squad Value Evolution chart (`plotlyOutput`) showing historical valuations of all teams, positioned at the bottom of the page below all interactive components.

### Usage Example

```R
rivals_UI("rivals_explorer")
```

---

## 3. `rivals_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV)`

### Parameters

| Parameter          | Type       | Description                                                      |
|--------------------|------------|------------------------------------------------------------------|
| `id`               | `char`     | Unique namespace ID matching the UI module.                      |
| `is_module_active` | `reactive` | Boolean reactive indicating whether the tab/module is currently active. |
| `login_token`      | `reactive` | The user's authenticated session token (string).                 |
| `championship_id`  | `reactive` | The active championship ID (string).                             |
| `user_team_id`     | `reactive` | The logged-in user's own team ID. Forwarded to `players_table_Server` so clause-buying executes on the user's behalf. |
| `user_teams_RV`    | `reactive` | Data frame of all teams in the championship (columns: `teamid`, `teamname`). |

### Return Value

Returns `selected_player_RV`, a reactive supplied by the nested `players_table_Server` call. Contains the currently selected player data from the scouted roster table.

### Internal Reactives

| Reactive Name                    | Purpose                                                              |
|----------------------------------|----------------------------------------------------------------------|
| `selected_rival_team_id`         | Holds the team ID chosen by clicking a row in the rivals table.       |
| `league_finances_RV`             | Reuses `shared_rival_finance_source()` with account/league-scoped directory, evidence, reconstructed finances, per-team cash ledgers and the shared pressroom. |
| `rival_financial_summary_box_RV` | Fetches financial stats via `get_user_team_info()` for the selected rival. |
| `rival_players_table_RV`         | Fetches the full squad via `get_players_from_team()`, computes `clause_ratio`, and pipes through `translate_player_positions()`, `calculate_player_changes()`, and `unify_columns()`. |
| `rival_moneymovements_raw_RV` | Selects the rival's precomputed `cash_ledgers` entry from the shared dataset; no HTTP/DB fetch or separate calculation. |
| `rivals_cash_history_rows()` | Projects the complete ledger by default without recalculating cash; explicit dated/undated subsets remain supported. |
| `rivals_cash_history_widget()` | Renders all per-transaction balances, preserves numeric sorting, and labels undated amounts as calculation subtotals. |

### `output$league_finances_table`

Renders a `reactable` table bound to `league_finances_RV()$team_finances`. Displays the League Financial Standings & Budget Left for every team in the championship. Key columns include:

| Column            | Description                                                  |
|-------------------|--------------------------------------------------------------|
| `teamname`        | Name of the user team.                                       |
| `initial_budget`  | Baseline starting budget (default 300,000,000 EUR).          |
| `total_spent`     | Sum of `buyPrice` across all players in the current roster.  |
| `budget`          | Money Left = initial_budget - total_spent + total_sales. No artificial bonuses. Overridden by API `actual_info$budget` when available. |
| `team_value`      | Total squad valuation (sum of player `value`).               |
| `net_profit_loss` | Signed included cash-ledger activity excluding opening cash; independent of squad value. |
| `squad_size`      | Number of players in the roster.                             |
| `points`          | Championship points accumulated.                             |
| `point_bonus`     | Points multiplied by 70,000 EUR.                             |

### Financial Summary Cards

The module renders four summary boxes in a fluid row:

| Card              | Data Field      | Source                     |
|-------------------|-----------------|----------------------------|
| Standings Position| `info$position` | `get_user_team_info()`     |
| Money Left (Budget)| `budget` from `league_finances_RV()$team_finances` | Pressroom-driven `budget` column from `calculate_league_finances()`, which overrides the default `300000000 - total_spent` calculation. This ensures the summary box matches the main standings table and plot. |
| Squad Investment  | `total_spent` (roster) / `money_out` (transactions) | Computed from roster `buyPrice` and transaction movements |
| Trading profit | `summary$trading_profit` | Completed shared-ledger sale/purchase cycles, less related fees; unresolved sold costs remain unavailable. |
| Squad valuation | `summary$squad_value` | Observed directory valuation, with existing shared-finance/information fallback. |
| Overall wealth gained | `summary$wealth_gained` | Reconstructed cash plus observed squad value minus starting cash; estimated where source/reward assumptions remain. |

#### Squad Investment Card -- Money Out / Money In Calculation

The "Squad Investment" card displays the total money spent on squad acquisitions as the main metric, with a styled subtitle breaking down Money Out (purchases) and Money In (sales).

**Calculation logic:**

1. Fetch transaction movements via `rival_moneymovements_raw_RV()` wrapped in `tryCatch()` (returns `NULL` on error).
2. If `tx_raw` is available and has rows:
   - **Money Out**: Sum of `abs(money)` where `money < 0` or `type == "buy"`.
   - **Money In**: Sum of `money` where `type == "sell"` or (`money > 0` and `type != "budget"`).
3. If `tx_raw` is `NULL` or empty (fallback mode):
   - **Money Out**: Falls back to `total_spent` (sum of `buyPrice` from the roster).
   - **Money In**: `0`.

The subtitle renders as a styled HTML snippet:

```html
<span style="color: #ef4444; font-weight: 600;">Out: EUR X</span>
<span style="color: #94a3b8;">&bull;</span>
<span style="color: #10b981; font-weight: 600;">In: EUR Y</span>
```

### League Buying Power Chart -- Mode Selector and Date Slider

The League Buying Power horizontal bar chart supports three display modes, selectable via a dropdown control positioned at the top of the chart area. A date range slider sits directly above the chart, allowing the user to filter the data to a specific time window.

**Mode Selector**

| Mode | Label | Description |
|------|-------|-------------|
| Liquid Cash | "Liquid Cash" | Displays each team's current liquid cash balance (budget left). Bars extend right for positive balances and left for negative balances. |
| Squad Purchases | "Squad Purchases" | Displays the total amount each team has spent on player acquisitions (`sum(buyPrice)`). Bars extend right with length proportional to total spending. |
| Transaction Volume | "Transaction Volume" | Displays the combined transaction volume (purchases + sales) for each team. Represents total market activity. |

**Date Slider**

- Positioned at the top of the chart area, above the mode selector.
- Type: `sliderInput` with `timeFormat = "%Y-%m-%d"`.
- Default range: full season-to-date (from the earliest transaction to `Sys.Date()`).
- `min` is set to the earliest transaction date in the dataset; `max` is capped at `Sys.Date()`.
- When the user adjusts the slider, the chart re-renders showing only transactions within the selected date range.
- The slider updates reactively; changing the rival selection or the mode also triggers a chart refresh.

#### Data Pipeline

Per-team values are computed by the pure helper `rivals_buying_power_values(pressroom_df, teams, metric, start_date, end_date, initial_budget = 300000000)`, which returns a data frame with `team_id`, `team`, `value`, and `range_label`.

1. **Input**: Receives the authenticated `login` token, `championship_id`, the selected mode, and the slider's `[start, end]` window.
2. **Computation** (per mode):
   - **Liquid Cash**: `300000000 - (all purchases through end) + (all sales through end)`. The balance is computed from **all transfers through the END date** and is **not reset at the slider start** -- moving the start handle does not change the cash balance, only the end handle does. `range_label = "all transfers through end date"`.
   - **Squad Purchases (investment)**: total purchases **within the `[start, end]` range** only. `range_label = "within selected range"`.
   - **Transaction Volume**: purchases + sales **within the `[start, end]` range** only. `range_label = "within selected range"`.
3. **Rendering**: Produces a horizontal bar chart via `plotly` with interactive hover tooltips. The tooltip shows the metric value **and** the `range_label`, so the user can see whether the figure is an all-time-through-end balance (cash) or a within-range total (investment/volume).

### Clause Ratio Calculation

For each player in the scouted roster, the module computes:

```
clause_ratio = clause_price / value
```

This ratio drives the visual scouting indicators:
* **STEAL** -- clause_ratio is significantly below 1 (cheap buyout relative to player value).
* **GOOD VALUE** -- clause_ratio is close to 1 (fair buyout).
* **OVERPRiced** -- clause_ratio is well above 1 (expensive buyout).

The calculation is performed defensively: if `clause_price` or `value` columns are missing, `clause_ratio` defaults to `NA_real_`.

### Empty Roster Handling

If `get_players_from_team()` returns `NULL` or a zero-row data frame, the module returns a gracefully shaped empty data frame with the expected column schema:

```R
data.frame(
  id = character(0), name = character(0), role = character(0), role2 = character(0),
  value = numeric(0), change = numeric(0), points = numeric(0), buyPrice = numeric(0),
  clause_price = numeric(0), isClause = logical(0), clause_ratio = numeric(0),
  stringsAsFactors = FALSE
)
```

---

## 4. Tab 2: Transaction History and Finances

When a rival team is selected, the "Historial de Transacciones y Finanzas" tab provides a detailed view of the team's financial movements.

### 4.0 Money Out / Money In Volume Metrics and Budget Sync

The same transaction data fetched by `rival_moneymovements_raw_RV()` feeds into the "Squad Investment" summary card (outside the tab, in the financial overview). The calculation mirrors the logic used in the Period Summary Cards but operates on the **unfiltered** raw dataset:

- **Money Out (Spent)**: Sum of `abs(money)` for rows where `money < 0` or `type == "buy"`.
- **Money In (Sold)**: Sum of `money` for rows where `type == "sell"` or (`money > 0` and `type != "budget"`).

When the API returns no data, Money Out falls back to `total_spent` (sum of `buyPrice` from the roster) and Money In defaults to `0`.

**Budget sync**: The "Money Left (Budget)" card now reads the `budget` column directly from `league_finances_RV()$team_finances` for the selected rival (via `rival_row$budget`). This value is computed by `calculate_league_finances()` using pressroom-driven purchase/sale data, ensuring the summary box matches the main standings table and the buying power plot. If the `budget` column is absent, non-numeric, or NA, the card falls back to the default `300000000 - total_spent` calculation.

### 4.1 Data Fetching

The module calls `get_user_team_moneymovements(login, championship_id, user_team_id)` to retrieve the rival's transaction log. The API endpoint is `POST https://api.futmondo.com/1/userteam/moneymovements`.

**Response shape handling**: The Futmondo API may return the movements array in two different shapes:
- `ans$answer` (direct array of movement objects)
- `ans$answer$answer` (nested, where the outer `answer` wraps an inner `answer` containing the array)

The `get_user_team_moneymovements()` function in `futmondo_functions.R` detects which shape is returned by checking whether `ans$answer` is a list that itself contains an `answer` key. If so, it extracts `ans$answer$answer`; otherwise it uses `ans$answer` directly. This ensures all movements (e.g., the full set of 19) are parsed regardless of API response nesting.

The response is wrapped in `tryCatch()` to handle API errors defensively. On success, the data frame has columns:

| Column     | Type     | Description                                      |
|------------|----------|--------------------------------------------------|
| `id`       | `char`   | Unique transaction identifier.                   |
| `concept`  | `char`   | Human-readable description (e.g., player name).  |
| `type`     | `char`   | Transaction type: `buy`, `sell`, `bonus`, `budget`. |
| `category` | `char`   | Transaction category: `market`, `round`, `bonus`. |
| `money`    | `numeric`| Amount in EUR (positive = inflow, negative = outflow). |
| `date`     | `char`   | ISO timestamp string (`YYYY-MM-DDTHH:MM:SS`).    |

### 4.2 Running Balance Calculation

The `running_balance` column is computed as follows:

1. Parse the `date` column to `POSIXct` timestamps.
2. Sort the **full** dataset **ascending** by timestamp (chronological order).
3. Compute `running_balance = cumsum(money)` on the sorted dataset.

This ensures the "Money Left After Transaction" column reflects the cumulative cash position after each transaction in chronological order, regardless of the display sort order.

### 4.3 Defensive Fallback Mode with Reconstructed Transaction History

If the API returns an empty data frame or throws an error (which can happen due to API restrictions on rival private transaction feeds), the module enters fallback mode:

1. Sets `is_fallback` to `TRUE`.
2. **First fallback -- Pressroom feed**: Calls `get_championship_pressroom(login, championship_id)` to retrieve the public pressroom feed for the championship. Filters rows where `buyer_team_id` or `seller_team_id` matches the selected rival. For each matching row, constructs a transaction record:
   - `id`: `"pressroom_"` + the pressroom entry ID.
   - `concept`: Player name + `" (Purchased)"` if the rival is the buyer, or `" (Sold)"` if the rival is the seller.
   - `type`: `"buy"` if `buyer_team_id` matches the rival, else `"sell"`.
   - `category`: `"market"`.
   - `money`: Negative of `price` for buys, positive `price` for sells.
   - `date`: The `created` timestamp from the pressroom entry.
3. **Second fallback -- Roster-based**: If the pressroom feed returns no matching entries for the rival, falls back to the current squad roster from `rival_players_table_RV()`. For each player with `buyPrice > 0`, constructs a synthetic purchase transaction:
   - `id`: `"fallback_buy_"` + sequential index.
   - `concept`: Player name.
   - `type`: `"buy"`.
   - `category`: `"market"`.
   - `money`: Negative of `buyPrice` (e.g., `-buyPrice`).
   - `date`: Current system time (`Sys.time()`).
4. Computes `running_balance` on the fallback dataset.

**Note**: The restricted information callout banner that previously informed users about API limitations has been removed. Fallback data is now presented without a warning banner, as the reconstructed transaction history (pressroom feed + roster fallback) provides sufficient accuracy to warrant silent presentation.

### 4.3a Reconstructed Transaction History (Pressroom Fallback)

When the pressroom feed is used as a fallback, the module builds a complete reconstructed transaction log by combining four distinct row types into a single data frame. This ensures the rival's financial picture is as accurate as possible despite API restrictions.

**Row type (a): Initial Budget**

A single synthetic row representing the starting budget allocation:

| Field      | Value                    |
|------------|--------------------------|
| `id`       | `"recon_initial_budget"` |
| `concept`  | `"Initial Budget"`       |
| `type`     | `"budget"`               |
| `category` | `"bonus"`                |
| `money`    | `300000000`              |
| `date`     | Season start date (derived from earliest pressroom transaction or budget-reset detection) |

**Row type (b): Market Transfers**

For each pressroom entry involving the rival team:
- **Buys**: `money = -price` (negative outflow). Concept: `"Player Name (Purchased)"`.
- **Sells**: `money = +price` (positive inflow). Concept: `"Player Name (Sold)"`.

**Row type (c): Finished Round Bonuses**

For each completed matchday, the module computes a per-round bonus based on the rival's average points:

1. Retrieves the rival's total championship points from `user_teams_RV()`.
2. Counts the number of finished rounds via `get_finished_rounds()`.
3. Computes `avg_pts_per_round = rival_points / num_finished_rounds`.
4. For each finished round, creates a bonus row with:
   - `id`: `"recon_round_bonus_"` + round number.
   - `concept`: `"Jornada N Bonus"` (where N is the round number).
   - `type`: `"bonus"`.
   - `category`: `"round"`.
   - `money`: `avg_pts_per_round * 70000` (the standard Futmondo point bonus rate).
   - `date`: The `begin_process` timestamp from the finished round data.

This distributes the rival's total points across finished matchdays and applies the 70,000 EUR per-point bonus rate, matching the actual game mechanics.

**Row type (d): Roster Fallback (when pressroom yields no data)**

If the pressroom feed has no entries for the rival, the module falls back to constructing buy transactions from the current roster. Each player with `buyPrice > 0` generates a synthetic purchase row. No bonus rows are added in this mode since the pressroom feed is unavailable to determine finished round counts.

**Running Balance Calculation**

After assembling all rows:

1. Parse `date` to `POSIXct` timestamps.
2. Sort **ascending** by timestamp (chronological order).
3. Compute `running_balance = cumsum(money)` on the sorted dataset.
4. Re-sort **descending** by date for display (newest first).

The running balance starts from `300,000,000 EUR` (the initial budget row) and accumulates all subsequent transactions. This fixes the previous roster-fallback bug where the running balance started from `0 EUR` because no initial budget row was included.

### 4.3b Synchronization Between KPI Summary Cards and Transaction Log

The four financial summary cards rendered above the transaction tab are synchronized with the reconstructed transaction log to ensure consistency across the page.

**How it works:**

1. The module fetches `rival_moneymovements_raw_RV()` which contains the full reconstructed transaction log (including the initial budget row, market transfers, round bonuses, and roster fallback buys).
2. If transaction data is available:
   - **Money Out**: Sum of `abs(money)` for all negative-money rows (purchases).
   - **Money In**: Sum of positive `money` for all non-budget rows (sales and bonuses).
   - **Budget (Money Left)**: The `running_balance` of the most recent transaction (first row in the descending-sorted dataset). This reflects the cumulative cash position after all known transactions.
3. If transaction data is unavailable (fallback mode, no pressroom data):
   - **Money Out**: Falls back to `total_spent` (sum of `buyPrice` from the roster).
   - **Money In**: `0`.
   - **Budget**: Falls back to `300000000 - total_spent`.
4. The "Money Left (Budget)" card can be further overridden by the API-provided `info$budget` from `get_user_team_info()` if the value is available, numeric, and positive. This ensures the summary box matches the official API value when accessible.

**Summary Cards (within Tab 2):**

The three KPI cards inside the Transaction History tab (Total Inflow, Total Outflow, Net Cash Flow) are computed from the full dataset (`rival_moneymovements_filtered_RV()`), which now contains all transactions (no user filters). This provides a complete cash-flow overview for the rival.

| Card         | Calculation                                     |
|--------------|-------------------------------------------------|
| Total Inflow | Sum of positive `money` values in the dataset. |
| Total Outflow   | Sum of negative `money` values in the dataset. |
| Net Cash Flow   | Net sum of all `money` values in the dataset.  |

The summary cards outside the tab operate on the raw dataset, providing a full-picture view of the rival's finances.

### 4.4 Display Behavior -- No Filter Bar, Pivot by Player Toggle

The filter bar (date range, transaction type, and category controls) has been removed from the Transaction & Financial History tab. All transactions are displayed directly in the reactable table, sorted descending by date (newest first). No filtering UI is present.

**Pivot by Player Toggle**

A toggle switch labeled "Pivot by Player" is available in the tab. When activated, it switches the view from the full chronological transaction log to a per-player pivot summary. In pivot mode, the table groups transactions by player, showing paired buy/sell rows with net P/L calculation for each player.

### 4.5 Transaction Table

The `reactable` table displays all transactions. The **Date column is first**, and the default ordering is **timestamp-based** (`defaultSorted = list(timestamp = "desc")`) so rows are sorted chronologically (newest first) by the parsed `POSIXct` timestamp -- not by the raw `date` string, which would sort incorrectly across years.

| Column                     | Formatting                                                        |
|----------------------------|-------------------------------------------------------------------|
| `date` (displayed as "Date") | Lowercase `date` column name matching the data frame column. Formatted as `dd/mm/YYYY HH:MM`. Shown first. |
| Type / Category            | Status badge: Buy = Red (`#ef4444`), Sell = Green (`#10b981`), Bonus = Blue (`#3b82f6`), Budget = Slate (`#64748b`). |
| Concept / Description      | Player name or concept string, medium font weight.               |
| Amount                     | Formatted EUR via `format_table_currency()`. Green with `+` prefix if positive, red if negative. |
| Money Left After Transaction | Formatted EUR via `format_table_currency()`, bold styled.      |

**Hidden helper columns**: `id`, `category`, `timestamp`, `batch_key`, `is_batch_header`, and `batch_final_balance` are all hidden via `colDef(show = FALSE)`. In particular, the internal `id` column is never shown to the user.

**Note**: The column definition uses lowercase `date = colDef(...)` to match the actual column name in the data frame produced by `get_user_team_moneymovements()`. Using uppercase `Date` would cause a `reactable` error because the column name does not exist in the data.

### Usage Example

```R
rivals_Server(
  id = "rivals_explorer",
  is_module_active = reactive(TRUE),
  login_token = reactive("USER_TOKEN"),
  championship_id = reactive("CHAMP_ID"),
  user_team_id = reactive("MY_TEAM_ID"),
  user_teams_RV = reactive(all_teams_df)
)
```

---

## 5. Player Buy/Sell Pivot Ledger

The Pivot Ledger is a dedicated view within the Rivals Module that presents a paired buy/sell comparison for each player the rival team has ever acquired. It allows the user to see, at a glance, whether the rival turned a profit or loss on each player they purchased.

### 5.1 Paired Buy/Sell Rows

The ledger is built by the pure helper `rivals_build_pivot_ledger(pressroom_df, rival_id)`. For each player the rival team has bought, the ledger displays **one row** with the buy and (if any) the matching sell side by side:

| Column | Content |
|--------|---------|
| `Player` | Player display name. |
| `Bought Price` | Purchase price (red), with a two-line tooltip (date + type). |
| `Sold Price` | Sale price (green) with a two-line tooltip, or `-` when the player has not yet been sold. |
| `Net P/L` | `sell_price - buy_price` (green/red), or `-` when unsold. |

**Pairing key**: buys and sells are paired by **`player_id`**, falling back to `player_name` when no `player_id` is exposed. This avoids mis-pairing different players who share a name.

**Raw dates**: matching and sorting use the **raw ISO 8601 timestamps** (which sort chronologically as strings), so a sell is matched to the first *unused* sell strictly after the buy. The raw date strings are kept in the data frame (hidden helper columns) and formatted for display in the cell renderers.

**No `Sold` column**: there is no separate boolean `Sold` column; an unsold buy simply has `NA` sell fields, and the cell renderers display `-`.

**Hidden helper columns**: `PlayerID`, `Buy_Date`, `Buy_Type`, `Sell_Date`, and `Sell_Type` are present for pairing/tooltips/sorting but are hidden from the table via `colDef(show = FALSE)`.

### 5.2 Two-Line Hover Tooltips

Hovering over a buy or sell cell reveals a two-line tooltip:

- **Line 1**: Transaction type and date (e.g., `"Buy on 15/03/2025"`).
- **Line 2**: Counterparty name (e.g., `"From: Team X"` for buys, `"To: Team Y"` for sells). When the counterparty ID is missing, `NULL`, or empty, the tooltip displays `"Futmondo (System)"` per the domain rule.

### 5.3 Handling of Re-Bought Players

Because pairing is by `player_id` (fallback name), re-buys are handled correctly:

- Each buy generates its own ledger row, in chronological order (newest first).
- A sell is consumed by the earliest still-unmatched buy of the same player, so a sell-and-rebuy sequence produces distinct rows (the first buy matched to the first sell, the re-buy left unsold until a later sell).
- A sell that has no earlier matching buy is ignored (it cannot be paired to a purchase the rival never made).

### 5.4 Net P/L Calculation

For each paired buy/sell entry, the ledger computes a Net Profit/Loss value:

```
Net P/L = sell_price - buy_price
```

- If the player has been sold: `Net P/L` is displayed as a green value (profit) or red value (loss).
- If the player has not yet been sold: `Net P/L` is displayed as `"--"` (pending).

The ledger also provides a **total Net P/L** summary at the bottom, aggregating all completed buy/sell pairs:

```
Total Net P/L = sum(sell_price) - sum(buy_price) for all players that have been sold
```

---

## 6. Net Transfer Profit/Loss KPI Box

A standalone KPI box rendered in the Rivals Module that displays the rival team's Net Transfer Profit/Loss. This metric represents the aggregate profit or loss across all completed player transfers (buys and subsequent sells).

### Calculation

```
Net Transfer P/L = sum(sell_price for all sold players) - sum(buy_price for all sold players)
```

Only players that have both a buy and a sell transaction are included. Players still on the roster (bought but not yet sold) are excluded from this calculation.

### Rendering

- Displayed as a single summary box with a prominent numeric value.
- Positive values (profit) are rendered in green (`#10b981`).
- Negative values (loss) are rendered in red (`#ef4444`).
- Zero or no completed transfers: displayed as `"0 EUR"` in neutral gray.

### Data Pipeline

1. **Input**: The rival's transaction history from `rival_moneymovements_raw_RV()`.
2. **Computation**: Filters for `type == "buy"` and `type == "sell"` transactions, pairs them by player, and computes the aggregate difference.
3. **Rendering**: Outputs a single KPI box suitable for rendering via `renderUI()`.

---

## 7. Squad Value Evolution Plot (Bottom Placement)

The League Squad Value Evolution chart has been repositioned to the bottom of the Rivals Module page, below all interactive components (financial summary cards, transaction tabs, pivot ledger, and buying power chart). This placement ensures that the primary scouting controls and data tables are immediately visible without requiring the user to scroll past a large chart.

#### Behavior

- The plot renders as a time-series line chart tracking the squad valuation of each team across matchdays.
- Each team is represented by a distinct colored line, with the user's own team and the currently scouted rival highlighted.
- The plot is rendered via `plotlyOutput` for interactivity (hover tooltips, zoom, pan).
- Positioned at the bottom of the module, after all tabs and KPI boxes.
The main league table now displays numeric `estimated_cash` with descending initial sort, `confidence`, source update time and debt status. Opening cash is retained in the expandable audit breakdown, not a table column. `shared_rival_finance_source()` loads saved account/league/season evidence before bounded live refreshes; see [rival_cash_accounting.md](rival_cash_accounting.md). Existing roster and manual trade controls are unchanged.


### Selectable directory and financial rendering failure recovery (2026-10-31)

Rivals uses only the Reactable row selection for scouting. The selected team ID remains scoped to the account/championship/own-team context and is cleared when that context changes. Removing the duplicate dropdown also removes its synchronization observer; no API or database reads are added.

The status reports the number of loaded teams and the actual number of pending financial tasks. It does not infer that the table is visible from directory readiness. `rivals_finance_table_widget()` formats unknown amounts and dates as blank; a financial display-schema/detail error falls back to selectable Team/Points rows and records a redacted schema reference. A finance-build error keeps the same directory with unknown cash and an explicit failure message. Errors inside a resolved saved/seed/team callback are caught before continuing the bounded queue; they cannot leave the displayed loading state waiting for work that is no longer running. Errors never establish a balance or change accounting calculations.

The hosted blank-widget cause remains unconfirmed. Locally, the captured ten-team response shape renders correctly, including its nested metadata flattened to character columns by `get_teams()`. Synthetic malformed saved payloads and completion failures reproduce distinct previously unhandled paths and are covered by focused regression checks.


Selected-rival summary correction (2026-10-31): budget cards reuse the shared league ledger estimate and confidence, with expandable evidence gaps. Position uses the observed API position or total-points rank (ties share rank), explicitly labeled. No additional API/DB requests, timers, caches, or accounting changes; bounded ranking over the league directory and one ledger-row lookup. Historical reward settlement/eligibility integration remains incomplete; this change does not certify estimates or validate hosted data. Focused regression and offline lifecycle checks recorded with delivery.


### Reconstructed rival balances (2026-10-31)

The league column is **Reconstructed balance** and the selected-rival card is **Reconstructed cash balance (estimate)**, using the same numeric ledger result. An accessible fresh own budget remains labeled observed cash. Reconstruction includes actual round-history-derived points and ranking bonuses, saved XI/MVP selections associated with historical starter/bench ownership, public transfers, and known signed adjustments. It does not require a private rival budget. Expand the evidence notes for specific missing round/player ownership, unpublished rounds, hidden adjustments and current-rule assumptions. Historical collection uses the shared two-read queue; no card-selection request loop or timer was added. See [rival_cash_accounting.md](rival_cash_accounting.md) for contracts, reuse and correction behavior.


## 2026-10-31 shared running-cash history

The transaction tab now consumes the same ledger as the reconstructed budget card and league table. Opening cash is applied once, signed source transfers/fees and applicable reward components are retained, and the final cumulative amount is the displayed estimate. Known movement/transfer revisions replace older rows; recorded payments suppress overlapping derived rewards. Sorting or filtering does not reset the starting cash. Same-minute transactions each show a numeric balance, with stable ID ordering explicitly distinguished from a known real ordering.

The main **Financial history** shows every ledger component, including all applicable bonus types. Reconstructed notification-based dates are labelled. Any still-undated rows remain visible for reconciliation and do not claim a historical payment sequence; the final subtotal matches the reconstructed amount. Opening cash without a supplied date is labeled as an opening balance. No current-time or round-date substitute is generated. Missing amounts remain unavailable and the known-subtotal limitation is visible; missing opening cash leaves balances unavailable. Accessible observed cash/freshness and any discrepancy are separate details.

The pivot consumes the shared pressroom, and transfer summary cards consume shared purchase/sale totals rather than all signed ledger amounts. This avoids counting opening cash/bonuses as sales or fees as purchases. The pivot choice survives background ledger publications. No new HTTP/database read, timer, worker, dependency or persistence path was introduced. Both history tables paginate fifteen rows and share one bounded ledger calculation per coalesced source publication. See `data_review_results.md` for measurements and verification limits.


## Reconciled totals and dated bonuses (2026-11-01)

Starting cash is an explicit inflow component. `rivals_inflow_totals()` and `rivals_outflow_totals()` consume every included positive/negative ledger amount, so their difference equals final reconstructed cash. Ordinary/clause sale and purchase subtotals use observed source evidence; numeric parent totals include unresolved methods, whose coverage notice is separate. Position, point, MVP and Best XI bonuses have separate inflow lines. The cash table labels each reward type and expands grouped player award calculations.

Unambiguous recorded league position-payment times date reconstructed position/MVP/XI bonuses under the confirmed same-time-payment rule. Any missing notification remains an explicit source-data issue; repeated notifications do not multiply cumulative award amounts. Recorded payments replace applicable estimates. Raw clause/ordinary types are normalized consistently across the summary and history, and historical eligibility now loads through the corrected asynchronous read allowlist. Existing ownership, hidden adjustment and missing-amount limitations remain visible. See `rival_cash_accounting.md` for exact matching and correction contracts.

## Concise reconstruction status (2026-11-02)

Selecting a league row now selects its team without expanding a second raw calculation table. The selected budget card shows `estimate_summary`, which groups repeated missing-standings notes into a round count and summarizes award-eligibility/coverage gaps. The original `estimate_notes`, observed balance and discrepancy remain available under a collapsed native **Calculation details** control with mobile-safe wrapping. The transaction history tab retains the complete dated/undated cash ledger, running balances and reconciliation totals. No selected-team or transaction data is discarded.


### Unified financial history and notification dates — 2026-11-02

The financial history shows the full shared ledger, including position, Best XI, MVP and configured per-point bonuses. Actual movement dates are retained. Reconstructed position/MVP/Best XI bonuses keep unambiguous recorded league payment dates where applicable; otherwise reconstructed bonuses use the matching round-points publication date. These assigned dates are labelled **reconstructed** in the Date column. Per-point estimates do not borrow position-payment timestamps. Older notification coverage is not presumed complete. Undated entries remain visible when source evidence is missing; no arbitrary date or balancing cash adjustment is invented. All amounts and running subtotals come from the same ledger as the balance cards.


The rivals points pill scale excludes zero scores from its minimum and maximum. Zero remains visible with the neutral theme surface; nonzero scores use the full existing colour scale. Sorting and pagination do not change the scale.
Performance: this changes only the existing linear colour calculation, with no additional requests, database work or reactive updates. Focused rival-directory checks (7) and deployment checks (3) passed; full lifecycle and hosted visual checks were skipped for this presentation-only change. No delegation.


### Trading profit and overall wealth gained — 2026-11-06

The summary now displays **Trading profit** and **Overall wealth gained**, with observed **Squad valuation** separate. Trading profit replaces the former transfer-cash-flow card and totals completed sale/purchase cycles less related fees. The wealth card uses reconstructed cash plus current squad value minus starting cash, with an estimate label and expandable evidence notes. Starting cash is the actual ledger allocation, not a hardcoded €300M; no historical initial squad allocation is invented. Unknown source amounts/valuation/baseline remain unavailable with specific missing-input details.

`rivals_summary_values()` consumes the shared `trading_profit`, `trading_profit_complete`, `trading_profit_notes` and `cash_activity_complete` fields and returns those trading fields plus `wealth_gained`, `wealth_gain_estimated` and `wealth_gain_notes`. The internal cash-activity compatibility fields remain available but no longer drive a displayed Net Gain figure.

The **Pivot by Player** table now calls `rivals_realized_trade_rows()` on the selected shared cash ledger. It projects the same cached completed pairs used by the trading card and retains unsold purchases and unmatched sales with unknown opposite costs/profit. A **Related fees** column makes deductions visible. There is no second public-feed matching calculation. Buy/sale date tooltips use actual row indices and source dates. The former `rivals_build_pivot_ledger()` helper remains for legacy isolated callers; the active view no longer calls it. Card details use native expandable controls, and the valuation/wealth columns stack through the existing Bootstrap grid. Selecting a rival, sorting/paging the pivot or opening details adds no HTTP/database request or historical retrieval. Exact calculation and uncertainty behavior is documented in [rival_cash_accounting.md](rival_cash_accounting.md).


### Six-card financial row and league metrics — 2026-11-06

The six selected-team cards share `.rivals-summary-grid`: six columns at desktop viewports of at least 1200px, three from 768px, two from 480px and one below 480px. Scoped styles keep the existing Bootstrap box structure and fit long euro values with the sidebar visible. Smaller screens retain the table's own horizontal scroll without widening the page.

The top financial standings table includes numeric **Overall wealth gained** and **Trading profit** columns. `rivals_financial_metrics(rows)` projects `estimated_cash + team_value - initial_budget` only with known included cash activity and finite inputs; it projects cached `trading_profit` only with `trading_profit_complete=TRUE`. Cards call the same projection after applying observed squad-value precedence. Negative and zero values remain sortable numbers, unknown inputs remain unavailable and sort last, and the fallback directory retains both unavailable columns and selection. Estimate tooltips preserve the existing cash/reward coverage qualification.

`rivals_summary_values()` additionally returns `opening_cash` and `wealth_missing`. `rivals_calculation_details(summary, ledger, metric)` always supplies an expandable numeric definition list: cash, current squad value, starting cash subtraction and wealth gained; or completed sales, their matched purchase costs, related fees and trading profit. Trade components reuse the final ledger's cached `realized_trades$pairs`, excluding held-player costs without rematching. Missing matching inputs do not become zero components.

`rivals_calculation_coverage(note)` groups historical ownership/eligibility, transfer coverage, source availability, reward rules, point/position reconciliation, payment dates and reset-allocation limitations into readable messages. Explicit missing cash/valuation/opening inputs remain alongside these groups. Detailed round/player source diagnostics are retained in the shared dataset's `estimate_notes`; the card does not dump IDs. Trade-specific missing cost/date/fee notes remain readable, with internal keys replaced and repeated sentences deduplicated. This presentation does not certify missing historical ownership or complete source coverage.

No new endpoint, source/database read, reactive subscription, timer, dependency or schema is added by the display. Sorting, selecting and expanding details reuse the published financial dataset.


### Progressive balance loading (2026-11-10)

`get_rival_finance_seed()` publishes `{info, rules}` through the async adapter's `on_progress` hook immediately after configuration, before pressroom/notification history. The shared source releases team observations at this point within the existing two-task queue, with generation and season checks. `seed_ready` permits acquisition; `seed_complete` records completion or failure of the whole seed. Both reset with context.

A validated database snapshot remains a labelled preview until seed completion and initial team acquisition drain. Either snapshot/history response order is accepted. Reconstruction is deferred during this preview and consumes accumulated evidence at batch completion. Preview rows retain source timestamps and pass through `rival_refresh_finance_row()` to expire observed cash truthfully. Snapshots never become accounting evidence. Existing bounded database storage, write acknowledgement/retry and scope isolation remain unchanged; memory is only an accelerator. With no valid persisted snapshot, authoritative acquisition remains necessary and unknown values stay unknown.
