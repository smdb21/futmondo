# League Finances & Purchase Tracking Module Documentation

This document describes the financial computation handlers and historical logging functions implemented in `futmondo_functions.R` and `supabase_connector.R`.

---

## 1. Overview

Each user in a Futmondo championship starts with a baseline budget (default `300.000.000` EUR in API integer format). As players are purchased from the market or clauses, their `buyPrice` is tracked per user team roster.

The remaining liquid budget ("Money Left") is computed as:
$$\text{Money Left} = \text{Initial Budget} - \sum \text{buyPrice} + (\text{Points} \times 70.000 \text{ EUR})$$

---

## 2. Functions

### A. `get_user_team_moneymovements(login, championship_id, user_team_id)`

Fetches detailed official money movements (purchases, sales, bonuses) from `POST https://api.futmondo.com/1/userteam/moneymovements`.

#### Parameters
- `login`: Character vector/list containing `token` and `userid`.
- `championship_id`: String championship ID.
- `user_team_id`: String user team ID.

#### Return Value
Data frame with columns: `id`, `concept`, `type`, `category`, `money`, `date`.

---

### B. `calculate_league_finances(login, championship_id, user_teams_df, initial_budget = 300000000)`

Iterates through all user teams in a league, fetches their current rosters, calculates total money spent on acquisitions (`buyPrice`), calculated money left, squad valuation, and net gain/loss, and logs snapshots to Supabase.

#### Parameters
- `login`: Character vector/list containing `token` and `userid`.
- `championship_id`: String championship ID.
- `user_teams_df`: Data frame of all user teams in the championship.
- `initial_budget`: Numeric baseline starting budget (default `300000000`).

#### Return Value
List with two data frames:
1. `team_finances`: Data frame with columns `teamid`, `teamname`, `initial_budget`, `total_spent`, `budget` (Money Left), `team_value`, `net_profit_loss`, `squad_size`, `points`, `point_bonus`.
2. `all_purchases`: Data frame containing every player purchase across all teams with `buyPrice`, `value`, `net_gain_loss`, `clause_price`, `owner_teamid`, `owner_teamname`.

---

### C. `get_league_finances_history(championship_id)`

Queries historical financial snapshots (`budget`, `team_value`, `points`, `position`, `recorded_at`, `teamname`) from Supabase table `user_team_history`.

#### Parameters
- `championship_id`: String championship ID.

#### Return Value
Data frame containing historical user team financial records.