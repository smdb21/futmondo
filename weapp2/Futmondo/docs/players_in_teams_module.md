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

---

## 3. Bulk Squad Market Listing

### "Put All Players on Market" (`btn_put_all_on_market`)
Prompts a warning modal to confirm the user's intent. On confirmation, executes a bulk listing of all squad players via `put_all_on_market()`, which calls `POST https://api.futmondo.com/5/market/putallonmarket`. On success, invalidates the API cache and displays a confirmation notification.