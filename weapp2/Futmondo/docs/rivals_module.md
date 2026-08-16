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
1. A League Squad Value Evolution chart (`plotlyOutput`) showing historical valuations of all teams.
2. A League Financial Standings & Budget Left table (`reactableOutput`) displaying per-team finances.
3. A League Buying Power chart (`plotlyOutput`) showing liquid cash standings.
4. A `uiOutput` placeholder (`scouted_rival_details_ui`) for the selected rival details, which renders:
   - Financial summary cards (Standings Position, Money Left, Squad Investment, Squad Valuation & Net Gain).
- A `tabsetPanel` (type = "pills") with two tabs:
      - **Tab 1: "Player Roster & Clauses"** -- Contains the `players_table_UI` call for the scouted player roster and purchase breakdown.
      - **Tab 2: "Transaction & Financial History"** -- Contains date range and transaction type/category filters, a period cash flow KPI summary bar, and an interactive `reactable` displaying the rival's transaction log with running cash balance.

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
| `selected_rival_team_id`         | Holds the team ID chosen by the user in the dropdown selector.       |
| `league_finances_RV`             | Calls `calculate_league_finances()` to compute per-team finances (total spent, budget left, squad valuation, net profit/loss) and logs snapshots to Supabase. Returns a list with `team_finances` and `all_purchases` data frames. |
| `rival_financial_summary_box_RV` | Fetches financial stats via `get_user_team_info()` for the selected rival. |
| `rival_players_table_RV`         | Fetches the full squad via `get_players_from_team()`, computes `clause_ratio`, and pipes through `translate_player_positions()`, `calculate_player_changes()`, and `unify_columns()`. |
| `is_fallback`                    | `reactiveVal` flag (boolean). `TRUE` when the moneymovements API returns empty or errors for a rival team; `FALSE` when real data is available. |
| `rival_moneymovements_raw_RV`    | Fetches raw money movements via `get_user_team_moneymovements()` inside `tryCatch()`. Parses ISO dates, sorts ascending by timestamp, and computes `running_balance = cumsum(money)` on the full chronological dataset. If the API returns empty or errors, first tries `get_championship_pressroom()` to reconstruct transactions from the public pressroom feed for the selected rival. If the pressroom yields no matching entries, falls back to `rival_players_table_RV()` (each player with `buyPrice > 0` becomes a purchase transaction). Sets `is_fallback = TRUE` in all fallback paths. |
| `rival_moneymovements_filtered_RV` | Takes `rival_moneymovements_raw_RV()` and applies user-selected filters (date range, transaction type, category). Sorts descending by date (newest first) for display in the reactable. |

### `output$league_finances_table`

Renders a `reactable` table bound to `league_finances_RV()$team_finances`. Displays the League Financial Standings & Budget Left for every team in the championship. Key columns include:

| Column            | Description                                                  |
|-------------------|--------------------------------------------------------------|
| `teamname`        | Name of the user team.                                       |
| `initial_budget`  | Baseline starting budget (default 300,000,000 EUR).          |
| `total_spent`     | Sum of `buyPrice` across all players in the current roster.  |
| `budget`          | Money Left = initial_budget - total_spent + total_sales. No artificial bonuses. Overridden by API `actual_info$budget` when available. |
| `team_value`      | Total squad valuation (sum of player `value`).               |
| `net_profit_loss` | team_value - total_spent.                                    |
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
| Squad Valuation & Gain | `info$teamValue` | `get_user_team_info()`     |

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
5. Renders a styled callout alert banner informing the user of the data limitation.

The callout text (in English):

> Information restricted by Futmondo: The Futmondo API restricts direct access to private financial transactions for rival teams. The transactions shown below have been calculated from current squad purchases.

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

**Period Summary Cards (within Tab 2):**

The three KPI cards inside the Transaction History tab (Total Inflow, Total Outflow, Net Cash Flow) are computed from the **filtered** dataset (`rival_moneymovements_filtered_RV()`), which respects the user's date range, type, and category filters. This allows the user to inspect cash flow for a specific period or transaction category.

| Card         | Calculation                                     |
|--------------|-------------------------------------------------|
| Total Inflow | Sum of positive `money` values in filtered set. |
| Total Outflow   | Sum of negative `money` values in filtered set. |
| Net Cash Flow   | Net sum of all `money` values in filtered set.  |

The summary cards outside the tab operate on the **unfiltered** raw dataset, providing a full-picture view of the rival's finances regardless of the active filters.

### 4.4 Filter Bar

The tab provides three filter controls in a `fluidRow`:

| Control            | Input Type        | Description                                              |
|--------------------|-------------------|----------------------------------------------------------|
| Date Range         | `dateRangeInput`  | Start and end date filter (format: dd/mm/yyyy, language: en). Defaults to season-to-date (see `get_season_start_date` below). `max` is capped at `Sys.Date()`. |
| Transaction Type   | `selectInput`     | Filter by type: All, Purchases, Sales, Bonuses / Rewards, Initial Budget. |
| Category           | `selectInput`     | Filter by category: All, Market, Rounds, Bonuses.        |
| Reset Filters      | `actionButton`    | Resets all filters to default values (date range back to season-to-date, type and category to "All"). |

#### `get_season_start_date` Helper

The date range filter defaults to a season-to-date range, computed by the `get_season_start_date(raw_movements = NULL)` helper. The function programmatically identifies the season/split reset date using a three-tier priority:

1. **Priority -- Budget-reset detection**: If `raw_movements` contains rows where `type == "budget"`, or where `category == "bonus"` AND `money >= 100,000,000` (initial budget allocation), the function finds the timestamp of the **most recent** such row. That timestamp is the exact moment the championship was reset by the admin. Returns `as.Date(max(reset_dates))`.
2. **Secondary -- Earliest transaction**: If no explicit budget-reset row exists (e.g., when working with the pressroom feed fallback that lacks budget entries), the function falls back to `min(parse_safe_datetime(raw_movements$date))` -- the earliest transaction recorded in the active split.
3. **Final fallback**: If `raw_movements` is `NULL`, empty, or all dates parse to `NA`, the function returns `Sys.Date() - 180` (180 days before today).

The date range filter is auto-updated by an `observeEvent` watching `rival_moneymovements_raw_RV()`. Whenever the rival is switched or new transaction data loads, the date range resets to `[season_start, Sys.Date()]`.

The filter logic is implemented in `rival_moneymovements_filtered_RV`, which:
- Parses dates to `POSIXct`.
- Applies date range filter (inclusive of both start and end dates).
- Applies type filter (exact match, skipped when "All").
- Applies category filter (exact match, skipped when "All").
- Sorts the result **descending** by date (newest first) for display.

### 4.5 Period Summary Cards

Three KPI cards are computed from the **filtered** dataset:

| Card         | Calculation                                     |
|--------------|-------------------------------------------------|
| Total Inflow | Sum of positive `money` values in filtered set. |
| Total Outflow   | Sum of negative `money` values in filtered set. |
| Net Cash Flow   | Net sum of all `money` values in filtered set.  |

### 4.6 Transaction Table

The `reactable` table displays the filtered transaction log with the following columns:

| Column                     | Formatting                                                        |
|----------------------------|-------------------------------------------------------------------|
| `date` (displayed as "Date") | Lowercase `date` column name matching the data frame column. Formatted as `dd/mm/YYYY HH:MM`. |
| Type / Category            | Status badge: Buy = Red (`#ef4444`), Sell = Green (`#10b981`), Bonus = Blue (`#3b82f6`), Budget = Slate (`#64748b`). |
| Concept / Description      | Player name or concept string, medium font weight.               |
| Amount                     | Formatted EUR via `format_table_currency()`. Green with `+` prefix if positive, red if negative. |
| Money Left After Transaction | Formatted EUR via `format_table_currency()`, bold styled.      |

**Note**: The column definition uses lowercase `date = colDef(...)` to match the actual column name in the data frame produced by `get_user_team_moneymovements()`. Using uppercase `Date` would cause a `reactable` error because the column name does not exist in the data.

### 4.7 Reset Filters Observer

An `observeEvent` on `input$tx_reset_filters` resets:
- `tx_date_range` to `start = season_start, end = Sys.Date(), max = Sys.Date()` (season-to-date, computed via `get_season_start_date`).
- `tx_type_filter` to `"All"`.
- `tx_category_filter` to `"All"`.

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