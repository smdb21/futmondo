# Squad Optimizer & Transfer Simulator Module (Phase 3.4)

The Squad Optimizer & Transfer Simulator is a Pro-tier module embedded within the "Your Team" section of Futmondo Insights. It provides constraint-based squad optimization, tactical lineup selection, and a what-if transfer sandbox to help managers make data-driven roster decisions.

## File Location

- **UI:** `Modules/Your_Team_Module.R`
- **Engine:** `intelligence_engine.R` (functions: `optimize_starting_xi`, `recommend_transfers`, `simulate_transfer_scenario`)

---

## Module Overview

The "Your Team" module contains three sub-tabs:

1. **Squad Roster** -- Current roster table, squad valuation KPIs, bulk market listing, financial breakdown.
2. **Tactical Lineup Optimizer** -- Interactive formation selector with five optimization modes and a responsive soccer pitch visualization.
3. **Transfer Sandbox** -- What-if transfer simulation with live budget and FIS deltas, top recommendations feed, and 1-click apply.

---

## 1. Squad Roster

### Description

Displays the manager's current squad in a sortable, filterable table. Shows per-player FIS scores, market values, form trends, and aggregate squad KPIs.

### Features

| Feature | Description |
|---------|-------------|
| Roster Table | Sortable columns: Name, Position, FIS Score, Market Value, Form, Status, Minutes Played |
| Squad Valuation KPIs | Total squad value, average FIS, highest/lowest valued player, position balance chart |
| Bulk Market Listing | Select multiple players and list them for sale on the market in a single action |
| Financial Breakdown | Per-position spending, total invested vs current valuation, unrealized gain/loss |

### UI Components

- `roster_table()`: Rendered via `DT::renderDataTable()` with server-side pagination.
- `squad_kpi_cards()`: Bootstrap card row showing total value, avg FIS, squad depth score.
- `bulk_list_button()`: Action button that triggers a modal with multi-select checkboxes and a "List Selected" confirm.
- `financial_breakdown_panel()`: Bar chart of spending by position, line chart of squad valuation over time.

### Usage Example

```R
# Server-side: load roster and compute KPIs
roster <- get_user_squad(login, user_team_id)
roster <- calculate_fis_score(roster)

# Render roster table
output$roster_table <- DT::renderDataTable({
  roster[, c("name", "position", "fis_score", "fis_tier", "value", "form", "status")]
})

# Render KPI cards
output$squad_kpis <- renderUI({
  total_value <- sum(roster$value, na.rm = TRUE)
  avg_fis <- mean(roster$fis_score, na.rm = TRUE)
  tags$div(class = "row",
    tags$div(class = "col-md-4", kpi_card("Total Squad Value", format_eur(total_value))),
    tags$div(class = "col-md-4", kpi_card("Average FIS", sprintf("%.1f", avg_fis))),
    tags$div(class = "col-md-4", kpi_card("Squad Depth", length(roster$id)))
  )
})
```

---

## 2. Tactical Lineup Optimizer

### Description

Constraint-based optimizer that selects the best Starting 11 from the current squad given a tactical formation and an optimization mode. Renders an interactive soccer pitch visualization with draggable player positions.

### Tactical Formations

| Formation | GK | DEF | MID | FWD |
|-----------|----|-----|-----|-----|
| 4-3-3     | 1  | 4   | 3   | 3   |
| 4-4-2     | 1  | 4   | 4   | 2   |
| 3-5-2     | 1  | 3   | 5   | 2   |
| 3-4-3     | 1  | 3   | 4   | 3   |
| 4-5-1     | 1  | 4   | 5   | 1   |
| 5-3-2     | 1  | 5   | 3   | 2   |
| 5-4-1     | 1  | 5   | 4   | 1   |

### Optimization Modes

| Mode | Strategy |
|------|----------|
| Max FIS | Select players with the highest composite FIS scores, subject to formation constraints |
| Safe XI | Prioritize players with high fixture_risk sub-scores (low injury/suspension risk, high minutes consistency) |
| Upside XI | Prioritize players with high momentum and efficiency sub-scores (high ceiling, volatile performers) |
| Form XI | Prioritize players with the highest form sub-scores (best recent match ratings over last 5 games) |
| Fixture XI | Prioritize players whose positions align with upcoming fixture ease (weak opponent defenses) |

### Interactive Soccer Pitch Visualization

- SVG-based pitch rendered via `htmlwidgets` or custom HTML.
- Player circles positioned at formation coordinates.
- Click a player on the pitch to view their FIS breakdown panel.
- Drag-and-drop to manually swap players between Starting 11 and Bench.

### Starting 11 and Bench Reactables

- `starting_xi_table()`: DT reactable showing the 11 selected players with position, name, FIS, and key metrics.
- `bench_table()`: DT reactable showing remaining squad members available for substitution.
- Both tables are reactive: changing the formation or mode triggers a full re-optimization.

### Usage Example

```R
# Server-side: optimize starting XI
output$starting_xi <- renderReactable({
  formation <- input$formation_selector  # e.g., "4-3-3"
  mode      <- input$optimization_mode    # e.g., "Max FIS"

  result <- optimize_starting_xi(
    squad_df = roster,
    formation = formation,
    mode = mode
  )

  reactable(result$starting_xi,
    columns = list(
      position = colDef(name = "Pos"),
      name = colDef(name = "Player"),
      fis_score = colDef(name = "FIS"),
      form = colDef(name = "Form")
    ),
    defaultSorted = list(fis_score = "desc")
  )
})

# Render pitch visualization
output$pitch_board <- renderUI({
  result <- optimize_starting_xi(roster, input$formation_selector, input$optimization_mode)
  render_pitch_svg(result$starting_xi, input$formation_selector)
})
```

---

## 3. Transfer Sandbox

### Description

A what-if simulation environment where managers can test hypothetical transfers without executing real transactions. Shows live budget impact, FIS deltas, and projected squad composition.

### Features

| Feature | Description |
|---------|-------------|
| Interactive Sell/Buy Selectors | Dropdown menus to pick players to sell from current squad and players to buy from the market |
| Live Financial Deltas | Real-time display of budget change: sell proceeds minus buy costs |
| Live FIS Deltas | Real-time display of projected squad FIS change: incoming FIS minus outgoing FIS |
| Top Transfer Recommendations Feed | Auto-generated list of recommended buy/sell pairs ranked by expected squad improvement |
| 1-Click Apply | Click a recommendation to auto-populate the sell/buy selectors |
| Projected Squad Preview | Side-by-side table: current squad vs projected squad after simulated transfers |

### Workflow

1. Manager selects one or more players to sell from the current squad dropdown.
2. Manager selects one or more players to buy from the market dropdown.
3. The engine runs `simulate_transfer_scenario()` to compute:
   - Budget delta (sell proceeds - buy costs)
   - FIS delta (incoming FIS - outgoing FIS)
   - Position balance changes
   - Projected Starting XI under current formation
4. Results display in real-time in the sandbox panel.
5. Manager can iterate by adjusting selections or applying a top recommendation.

### Usage Example

```R
# Server-side: simulate a transfer scenario
output$simulation_results <- renderUI({
  sell_ids <- input$sell_players   # character vector of player IDs
  buy_ids  <- input$buy_players    # character vector of player IDs

  if (is.null(sell_ids) || is.null(buy_ids)) return(tags$p("Select players to simulate."))

  result <- simulate_transfer_scenario(
    squad_df = roster,
    current_budget = user_cash,
    sell_player_ids = sell_ids,
    buy_player_ids = buy_ids,
    market_df = market
  )

  tags$div(class = "simulation-panel",
    tags$h4("Transfer Simulation Results"),
    tags$p(strong("Budget Delta: "), format_eur(result$budget_delta)),
    tags$p(strong("FIS Delta: "), sprintf("%.2f", result$fis_delta)),
    tags$p(strong("New Budget: "), format_eur(result$projected_budget)),
    tags$p(strong("New Avg FIS: "), sprintf("%.2f", result$projected_avg_fis)),
    tags$hr(),
    DT::reactable(result$projected_squad,
      columns = list(
        name = colDef(name = "Player"),
        position = colDef(name = "Pos"),
        fis_score = colDef(name = "FIS"),
        value = colDef(name = "Value")
      )
    )
  )
})

# Server-side: generate top recommendations
output$recommendations <- renderReactable({
  recs <- recommend_transfers(
    squad_df = roster,
    market_df = market,
    current_budget = user_cash,
    max_transfers = 5
  )

  reactable(recs,
    columns = list(
      action = colDef(name = "Action"),
      player_name = colDef(name = "Player"),
      fis_impact = colDef(name = "FIS Impact"),
      budget_impact = colDef(name = "Budget Impact"),
      confidence = colDef(name = "Confidence %")
    ),
    clickable = "row",
    onCellClicked = function(cellInfo) {
      # 1-click apply: populate selectors
      if (cellInfo$row$action == "Buy") {
        updateSelectizeInput(session, "buy_players", selected = cellInfo$row$player_id)
      } else {
        updateSelectizeInput(session, "sell_players", selected = cellInfo$row$player_id)
      }
    }
  )
})
```

---

## Data Flow

```
User Team Module
  |
  +-- Squad Roster Tab
  |     |-- get_user_squad() -> roster_df
  |     |-- calculate_fis_score(roster_df) -> roster with FIS columns
  |     |-- render roster table, KPI cards, financial breakdown
  |
  +-- Tactical Lineup Optimizer Tab
  |     |-- optimize_starting_xi(roster, formation, mode) -> starting_xi + bench
  |     |-- render pitch SVG, Starting XI reactable, Bench reactable
  |
  +-- Transfer Sandbox Tab
        |-- recommend_transfers(roster, market, budget, max_transfers) -> recs_df
        |-- simulate_transfer_scenario(roster, budget, sell_ids, buy_ids, market) -> sim_result
        |-- render recommendations feed, simulation panel, projected squad preview
```

---

## Defensive Programming

All three functions (`optimize_starting_xi`, `recommend_transfers`, `simulate_transfer_scenario`) are wrapped in `tryCatch()` blocks. Missing columns, empty data frames, NULL inputs, and budget violations are handled gracefully with sensible defaults and console warnings. No user thread blocks on network I/O.

---

## Caching

All data inputs (`roster`, `market`) are fetched via `get_cached_data()` to ensure sub-millisecond tab switches and protect against API rate limiting. Simulation results are NOT cached (they are user-specific and ephemeral).

---

## Mobile-First CSS

- Pitch visualization scales fluidly via `max-width: 100%; height: auto;`.
- Reactable tables use horizontal scroll on narrow viewports (`overflow-x: auto`).
- KPI cards stack vertically on mobile (`col-12 col-md-4`).
- Sandbox selectors use full-width dropdowns on mobile.