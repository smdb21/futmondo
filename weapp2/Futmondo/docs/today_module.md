# Today Module Documentation -- Manager Command Center

This document describes the `Today_Module.R` Shiny module, which provides a daily actionable dashboard for Futmondo managers with KPIs, FIS-driven recommendations, market intelligence, and recent transfer tracking.

---

## 1. Overview

The Today Module provides two exported functions:
* `today_UI(id)` -- Renders the Manager Command Center layout with a hero banner, 4 KPI value boxes, a 2-column main area (recommendations feed + market radar / recent deals). The recommendations box also carries the unchecked **"Include rival-owned market listings"** toggle (`include_rival_listings`).
* `today_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger = NULL)` -- Drives all reactive data fetching, FIS scoring, recommendation generation, and UI rendering. It also hosts the Today-local selected-player flow: a nested `selected_player_Server` under the `selected_player` namespace that opens the player modal (and, for "Place Bid" / "Exercise Clause" recommendations, the existing market-offer / clause-buyout modals) without routing through the Market tab (see section 4).

### Recommendation Policy (Buy / Place Bid and Clause)

* **Default Buy/Place Bid recommendations include only explicit Futmondo/system market listings** (`computer` is an **explicit scalar logical `TRUE`**, strict `isTRUE` -- no coercion: numeric `1`, character values (including `"true"`), `NA`, `FALSE`, and malformed/multiple values are NOT system). Confirmed rival-owned and unknown-owner market listings are **hidden by default**.
* **Current-team owner rows are always excluded**: any row whose resolved immutable owner ID equals the current team ID is `unknown` **before** the computer/system classification applies, so `computer = TRUE` plus a current-team owner ID can never enter either the default or the opt-in acquisition candidates.
* The unchecked **"Include rival-owned market listings"** toggle (`include_rival_listings`) opts in to also showing **rival-owned** market listings (a resolvable immutable owner ID that is not the current team). **Unknown-owner listings are always hidden**, in both modes.
* **Clause recommendations are built exclusively from strict open rival clauses** (see `today_is_clause_open` / `today_filter_clause_candidates`): finite positive `clause_price`, explicitly `FALSE` `clause_transferred`, and a parseable `clause_date <= now`. Rival rosters are fetched via **cached** `get_players_from_team()` calls (one per rival team, current team excluded); malformed teams data yields empty candidates.
* **Dual route**: when a player is BOTH a market listing and an open rival clause, the feed shows a **single clause recommendation** (action `clause_buyout`). `max(market price, clause price)` is included in the description as **comparison metadata only**; the **executed price is always the clause price** (the clause endpoint never receives the comparison price).
* Action validation is candidate-based: a `market_bid` event resolves **only from the current filtered market candidates** and a `clause_buyout` event resolves **only from the current open clause candidates**; stale/unknown input is rejected (warning, no modal).

---

## 2. UI Layout

### Hero Banner
- Dark gradient banner with `icon("bolt")` icon
- Title: "Manager Command Center"
- Dynamic date subtitle rendered via `renderText` (e.g., "Monday, January 01, 2025 | Your daily intelligence briefing")

### KPI Value Boxes (4 columns)
1. **Available Liquid Cash** -- Green gradient box showing the user's current budget (from `get_user_team_info` or fallback calculation: 300M - total spent).
2. **Squad Market Valuation** -- Blue gradient box showing total market value of all squad players.
3. **Active Market Opportunities** -- Orange gradient box showing the count of high-FIS (Strong Buy / Buy tier) players currently on the market.
4. **Clause Threat Radar** -- Color-coded box (green/orange/red) showing the count of starter players with vulnerable release clauses (clause price below 80% of market value).

### Main Content (2-column layout)
- **Left Column (width=8)**: "What Should I Do Today?" box
  - Renders actionable recommendation cards via `generate_command_center_feed()`
  - Each card shows: type icon, title, detailed description, confidence pill badge (e.g., "Confidence: 85%"), type badge, and action button
- **Right Column (width=4)**:
  - "Today's Market Intelligence Radar" box -- reactable table of top 10 FIS bargains on the market with player name, role, price, FIS score badge, and tier. Rows are prepared by the pure helper `today_prepare_radar_df()`, which coerces FIS to numeric, **drops non-finite (NA/Inf/-Inf) rows**, sorts by FIS descending, and keeps the top 10 -- so non-finite FIS values can never reach the rendered table (fixes the FIS "error effect"). Clicking a row selects that player (see section 4).
  - "Recent League Transfers" box -- list of the 6 most recent pressroom transactions with player name, buyer/seller, price, and date

---

## 3. Server Logic

### Reactive Data Sources
All data fetching is wrapped in `tryCatch()` blocks for defensive error handling:
- `market_players_RV` -- Fetches market players via `get_market_players()`, enriches with FIS scores via `calculate_fis_score()`
- `squad_players_RV` -- Fetches squad players via `get_players_from_team()`, enriches with FIS scores
- `pressroom_RV` -- Fetches championship pressroom feed via `get_championship_pressroom()`
- `user_finances_RV` -- Fetches user team info via `get_user_team_info()`
- `all_players_RV` -- Combines market + squad players, deduplicates by ID, recalculates FIS
- `include_rival_RV` -- Reads the `include_rival_listings` checkbox (unchecked by default; `NULL` input fails closed to `FALSE`)
- `market_candidates_RV` -- **Single source of truth for `market_bid` resolution.** Applies `today_filter_market_candidates()` to `market_players_RV()`: default keeps only `system` rows (`computer` is an explicit scalar logical `TRUE`); the opt-in toggle also admits `rival` rows; `unknown` rows are always excluded -- including rows whose owner ID equals the current team (excluded before the system classification applies). Owner IDs are resolved via `today_resolve_market_owner_ids()` (immutable `user_team_id` kept as-is; otherwise the `userTeam` name is resolved to the immutable team ID through the `user_teams_RV()` teams table **only when the name maps to exactly one distinct team ID** -- ambiguous names fail closed to `NA`; names are used ONLY for this ID resolution, never for classification, and a displayed name alone is not ownership evidence).
- `rival_roster_RV` -- Combined rival rosters: one **cached** `get_players_from_team()` call per rival team (all `user_teams_RV()` teams except the current team), deduplicated by player `id`. Malformed teams data (NULL / not a data frame / no rows / no immutable `teamid`/`id` column) yields an empty data frame -- never an error.
- `clause_candidates_RV` -- **Single source of truth for `clause_buyout` resolution.** Applies `today_filter_clause_candidates()` to `rival_roster_RV()`: keeps only rival-owned rows (immutable `user_team_id` non-empty and != current team) with a strict open release clause (`today_is_clause_open`).
- `recommendations_RV` -- Generates actionable recommendations via `generate_command_center_feed()`, **always supplying** `market_candidates = market_candidates_RV()` and `clause_candidates = clause_candidates_RV()` (possibly 0-row), so the feed's Buy/Clause sections are built exclusively from the candidate reactives (see `docs/intelligence_engine.md` section 4).

### Caching
All API calls leverage the existing `get_cached_data()` mechanism via the underlying functions. The `refresh_trigger` reactiveVal is used as a dependency to invalidate caches on manual refresh.

### Recommendation Types
The feed supports 5 recommendation types:
- **Buy** -- High-FIS players on the market (Strong Buy / Buy tier)
- **Sell** -- Weak-FIS players owned by the user (Sell tier)
- **Bid** -- Active bids on owned players that should be accepted or evaluated
- **Clause** -- Players with active release clauses offering good discounts
- **Hold** -- Stable assets with no immediate action needed

---

## 4. Action Buttons & Selection (browser JS -> namespaced inputs)

Player selection is driven entirely by **browser-side `Shiny.setInputValue`** events that write to **namespaced** inputs, which the server observes. This avoids the previous broken patterns (an R-closure `onClick` that reactable rejects, and input-existence observers that fired on render rather than on click).

### Market Radar row selection
* The reactable `onClick` is a `htmlwidgets::JS()` function built by `today_radar_onclick_js(ns)`. On row click it reads `rowInfo.values.PlayerID` and calls:
  ```js
  Shiny.setInputValue('<ns>-radar_selected_player', pid, {priority: 'event'});
  ```
* The server handles it with a single `observeEvent(input$radar_selected_player, ...)` that stores a selection event `list(player_id = pid, action = "view")` in `selected_from_today_RV`.

### Recommendation action buttons
* Each action button (Buy/Bid/Clause) carries an `onclick` attribute built by `today_rec_action_onclick_js(ns, player_id, action_label)` that sends `{player_id, action}` to the namespaced input `rec_action_clicked` (single quotes in values are escaped for safe JS). The JS sends the raw feed label; the **server** maps it to a stable action code (see below).
* A single `observeEvent(input$rec_action_clicked, ...)` handles all dynamically-created buttons reliably (no per-button observers, no false positives). It stores `list(player_id = <id>, action = today_normalize_action(<label>))` in `selected_from_today_RV`, so the mapped intent is retained end-to-end.

### Action label -> stable action code mapping
* `today_normalize_action(action_label)` is the **single** place where label matching happens (no brittle label matching elsewhere, no intent inferred from `actionButton` presence):
  * `"Place Bid"` (case-insensitive, trimmed) -> `"market_bid"`
  * `"Exercise Clause"` (case-insensitive, trimmed) -> `"clause_buyout"`
  * already-stable codes (`"market_bid"`, `"clause_buyout"`, `"view"`) pass through unchanged
  * anything else -> `"view"`
* The feed emits a stable `action_code` column (`"market_bid"` / `"clause_buyout"` / `"view"`); the action button's `onclick` sends that **code directly** (the observer normalizes it; stable codes pass through). The label is used only as a fallback when the column is absent.

### Today-local selected player flow (Place Bid / Exercise Clause)
When a selection event becomes valid, Today resolves and opens the player **locally** (it does not route through or programmatically select the Market reactable):

1. **Resolution (action-aware, candidate-based)** -- `selected_today_player_RV` resolves `event$player_id` via the pure helper `today_resolve_player_for_action(player_id, action, market_df, all_df, clause_df = ...)`:
   * **`"market_bid"` (Place Bid)** resolves the player **from the CURRENT FILTERED MARKET CANDIDATES (`market_candidates_RV()`) ONLY** -- so a stale / non-listed / hidden-owner (rival by default, unknown always) player can never open a market offer from the feed. Eligibility is decided purely by the immutable `id` being present in the candidate rows (never by visual/heuristic market fields).
   * **`"clause_buyout"` (Exercise Clause)** resolves the player **from the CURRENT OPEN CLAUSE CANDIDATES (`clause_candidates_RV()`) ONLY** -- so a stale / locked / transferred clause can never open a buyout from the feed.
   * **any other action (e.g. `"view"`)** resolves from `all_players_RV()`.
   * Yields the one-row player data frame or `NULL` when the id is unknown/stale (or absent from the candidate set for the action).
2. **Modal** -- an observer on `selected_from_today_RV` consumes the SAME action-aware resolved reactive (`selected_today_player_RV`; it does **not** re-resolve against `all_players_RV()`) and shows `selected_player_UI(id = ns("selected_player"))` in a Today-local `modalDialog` when the player resolves. When the id is absent from the appropriate candidate set for the action, a clear warning is shown and **no modal** (player, market-offer, or clause-buyout) is opened:
   * `market_bid` -- "This player is not currently listed on the market (the market data may be stale or still loading)..."
   * `clause_buyout` -- "This player's release clause is no longer available (the clause data may be stale or still loading)..."
   * otherwise -- "Selected player could not be found in the current data..."
3. **Nested module** -- `today_Server` instantiates a nested `selected_player_Server(id = "selected_player", selected_player = selected_today_player_RV, login_token = ..., championship_id = ..., user_team_id = ..., on_bid_updated = ..., open_action = ...)` under the Today namespace (inputs/UI live under `today-selected_player-...`).
   * `open_action` is the optional reactive carrying the stable action code (`today_open_action_RV`). Inside `selected_player_Server`, an observer routes **only** the two stable codes when a selected player is currently valid (guarded against stale startup reactive values):
     * **`"market_bid"`** -> the SAME `open_market_offer_modal()` helper as the "Make Market Offer" button (identical `run_acquisition_preflight` behavior, fail closed).
     * **`"clause_buyout"`** -> the SAME `open_clause_buyout_modal()` helper as the "Buy Release Clause" button. It **rechecks the strict open-clause state before showing** (finite positive price, explicitly `FALSE` transferred, parseable `clause_date <= now`), runs the clause preflight, and shows the clause confirmation modal. It **never opens the market bid modal**, and **only the recomputed `clause_price` is shown/executed** (no comparison price is ever sent to the clause endpoint). See `docs/selected_player_module.md`.
   * `on_bid_updated` only invalidates/recomputes Today data (it bumps the `refresh_trigger` reactiveVal so cached API data is refetched); **Today itself never performs writes** -- all bid/clause submission logic stays in `selected_player_Server`.

### Pure helpers (testable, top-level)
* `today_prepare_radar_df(mkt, top_n = 10)` -- coerce/filter non-finite FIS, sort desc, top-N, build display columns.
* `today_radar_onclick_js(ns)` -- returns the `htmlwidgets::JS()` row-click handler.
* `today_escape_js_string(x)` -- escapes a value for embedding in a single-quoted JS string literal: **backslashes first** (`\` -> `\\`), then single quotes (`'` -> `\'`).
* `today_rec_action_onclick_js(ns, player_id, action_label)` -- returns the action-button `onclick` attribute string; values are escaped via `today_escape_js_string` (backslash + single quote).
* `today_normalize_action(action_label)` -- maps a feed action label to a stable action code (`"Place Bid"` -> `"market_bid"`, `"Exercise Clause"` -> `"clause_buyout"`, stable codes pass through, everything else -> `"view"`).
* `today_resolve_player(player_id, players_df)` -- resolves a player id against a players data frame by immutable `id`; returns the 1-row data frame or `NULL`.
* `today_resolve_player_for_action(player_id, action, market_df, all_df, clause_df = NULL)` -- action-aware resolution: `"market_bid"` resolves **from `market_df` (the filtered market candidates) only**, `"clause_buyout"` resolves **from `clause_df` (the open clause candidates) only** (NULL/absent fails closed), any other action resolves from `all_df`. Returns the 1-row data frame or `NULL`.
* `today_classify_owner(player_row, current_team_id)` -- classifies a player row's owner (immutable IDs only): the resolved immutable owner ID (resolved `owner_team_id`, else `user_team_id`) is checked **first** -- a row whose owner ID equals `current_team_id` is `"unknown"` (excluded) **before** any computer/system classification; then `"system"` when `computer` is an **explicit scalar logical `TRUE`** (strict `isTRUE`, no coercion -- numeric `1`, character values (including `"true"`), `NA`, `FALSE`, and malformed/multiple values are NOT system); then `"rival"` when the owner ID is non-empty and != `current_team_id`; `"unknown"` otherwise. **Team names are never used for classification.**
* `today_resolve_market_owner_ids(mkt_df, teams_df = NULL)` -- resolves immutable owner team IDs into an `owner_team_id` column: a non-empty `user_team_id` is kept as-is; otherwise the `userTeam` name is resolved to the immutable team ID via the teams table (name -> `teamid` join) **only when the name maps to exactly one distinct team ID** -- ambiguous names (multiple distinct IDs) fail closed and stay `NA` (repeated rows with the same ID are not ambiguous); unresolvable rows get `NA`. The name is used ONLY for this ID resolution (a normalization step for the optional rival listings); **a displayed name alone is never ownership evidence.**
* `today_filter_market_candidates(mkt_df, current_team_id, include_rival = FALSE, teams_df = NULL)` -- default/opt-in market candidate filtering: default keeps only `"system"` rows; `include_rival = TRUE` also keeps `"rival"` rows; `"unknown"` rows are always excluded. Malformed input yields an empty (0-row) data frame.
* `today_is_clause_open(player_row, now = Sys.time())` -- strict open release-clause check: `clause_price` finite and > 0, `clause_transferred` **explicitly FALSE** (NA/missing/TRUE -> not open), `clause_date` present, parseable, and <= `now`. Fails closed on any missing/ambiguous field.
* `today_filter_clause_candidates(rival_roster_df, current_team_id, now = Sys.time())` -- strict rival clause candidate filtering: keeps only rows with a non-empty immutable `user_team_id` != current team (current team always excluded) and a strict open clause (`today_is_clause_open`). Malformed input (NULL / not a data frame / zero rows / no `id` column) yields an empty (0-row) data frame.

---

## 5. Dependencies

This module depends on:
- `calculate_fis_score()` from `intelligence_engine.R`
- `generate_command_center_feed()` from `intelligence_engine.R`
- `get_market_players()` from `futmondo_functions.R`
- `get_players_from_team()` from `futmondo_functions.R`
- `get_championship_pressroom()` from `futmondo_functions.R`
- `get_user_team_info()` from `futmondo_functions.R`
- `translate_player_positions()`, `calculate_player_changes()`, `unify_columns()` from `futmondo_functions.R`
- `format_table_currency()` from `futmondo_functions.R`
- `selected_player_Server()` / `selected_player_UI()` from `Modules/Selected_Player_Module.R` (Today-local selected player modal + market-offer flow)
- `reactable` package for the market radar table

---

## 6. Wires

### global.R
```R
source("Modules/Today_Module.R")
```

### ui.R
```R
shinydashboard::tabItem(
  tabName = "today",
  today_UI(id = "today")
)
```

### server.R
```R
today_Server(id = "today",
             is_module_active = reactive({ input$tabs == "today" }),
             login_token = login_token_RV,
             championship_id = championship_id_RV,
             user_team_id = user_team_id_RV,
             user_teams_RV = user_teams_RV,
             refresh_trigger = refresh_trigger)
```

### Menu
```R
shinydashboard::menuItem("Today", tabName = "today", icon = icon("bolt"))
```

### Default Landing Tab
After login, the default tab is set to `"today"`:
```R
updateTabsetPanel(inputId = "tabs", selected = "today")
```