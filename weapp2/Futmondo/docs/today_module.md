# Today Module Documentation -- Manager Command Center

This document describes the `Today_Module.R` Shiny module, which provides a daily actionable dashboard for Futmondo managers with KPIs, FIS-driven recommendations, market intelligence, and recent transfer tracking.

---

## 1. Overview

The Today Module provides two exported functions:
* `today_UI(id)` -- Renders the Manager Command Center layout with a hero banner, 4 KPI value boxes, a 2-column main area (recommendations feed + market radar / recent deals).
* `today_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger = NULL)` -- Drives all reactive data fetching, FIS scoring, recommendation generation, and UI rendering.

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
  - "Today's Market Intelligence Radar" box -- reactable table of top 10 FIS bargains on the market with player name, role, price, FIS score badge, and tier
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
- `recommendations_RV` -- Generates actionable recommendations via `generate_command_center_feed()`

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

## 4. Action Buttons

Recommendation action buttons (for Buy, Bid, Clause types) trigger a `selected_from_today_RV` reactiveVal that returns the player ID. The parent server can observe this value to open the player details modal via `selected_player_Server`.

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