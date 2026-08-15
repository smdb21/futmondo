# League Finances & Purchase Tracking Module Documentation

This document describes the financial computation handlers and historical logging functions implemented in `futmondo_functions.R` and `supabase_connector.R`.

---

## 1. Overview

Each user in a Futmondo championship starts with a baseline budget (default `300.000.000` EUR in API integer format). As players are purchased from the market or clauses, their `buyPrice` is tracked per user team roster.

The Pressroom API (`POST /1/locker/pressroom`) provides a complete transaction feed of all player transfers within a championship. Each transaction records the buyer team, seller team, player, and sale price. This feed is used to compute accurate per-team purchase and sale volumes.

### Money Out (Total Spent)
Money Out represents the total amount a team has spent on player acquisitions. It is derived from the pressroom feed:
$$\text{Money Out} = \sum \{\text{price} \mid \text{buyer\_team\_id} = \text{team\_id}\}$$
If the pressroom feed returns no purchases for a team, the value falls back to the roster-based `buyPrice` sum.

### Money In (Total Sales)
Money In represents the total revenue a team has earned from selling players to other teams:
$$\text{Money In} = \sum \{\text{price} \mid \text{seller\_team\_id} = \text{team\_id}\}$$

### Money Left (Budget)
The remaining liquid budget ("Money Left") is computed as:
$$\text{Money Left} = \text{Initial Budget} - \text{Money Out} + \text{Money In}$$

No artificial point bonuses or ranking prizes are added. The budget reflects the team's actual liquid cash based purely on real transaction history. When the API provides an explicit budget via `get_user_team_info()`, that value overrides the calculated figure.

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

### A1. `get_championship_pressroom(login, championship_id)`

Fetches the complete pressroom transfer feed for a championship via `POST https://api.futmondo.com/1/locker/pressroom`. Results are cached under the key `pressroom_{championship_id}`.

#### Parameters
- `login`: Character vector/list containing `token` and `userid`.
- `championship_id`: String championship ID.

#### Return Value
Data frame with columns: `id`, `created`, `player_id`, `player_name`, `buyer_team_id`, `buyer_team_name`, `seller_team_id`, `seller_team_name`, `price`. Returns an empty data frame on error or empty response.

---

### B. `calculate_league_finances(login, championship_id, user_teams_df, initial_budget = 300000000)`

Iterates through all user teams in a league, fetches their current rosters, pulls the pressroom transfer feed, syncs pressroom transactions to Supabase, calculates total money spent on acquisitions (`total_spent`), total sales revenue (`total_sales`), calculated money left, squad valuation, and net gain/loss, and logs snapshots to Supabase.

#### Parameters
- `login`: Character vector/list containing `token` and `userid`.
- `championship_id`: String championship ID.
- `user_teams_df`: Data frame of all user teams in the championship.
- `initial_budget`: Numeric baseline starting budget (default `300000000`).

#### Return Value
List with two data frames:
1. `team_finances`: Data frame with columns `teamid`, `teamname`, `initial_budget`, `total_spent`, `total_sales`, `budget` (Money Left), `team_value`, `net_profit_loss`, `squad_size`, `points`, `point_bonus`, `ranking_prize`.
2. `all_purchases`: Data frame containing every player purchase across all teams with `buyPrice`, `value`, `net_gain_loss`, `clause_price`, `owner_teamid`, `owner_teamname`.

#### Pressroom Integration
Before iterating teams, the function calls `get_championship_pressroom()` to fetch the full transfer feed using cursor-based pagination (up to 25 pages). It then calls `sync_pressroom_transactions_to_supabase()` to persist the data in batches. For each team:
- `pressroom_purchases`: Sum of `price` where `buyer_team_id` matches the team.
- `pressroom_sales`: Sum of `price` where `seller_team_id` matches the team.
- `total_spent`: Uses `pressroom_purchases` if greater than 0, otherwise falls back to the roster `buyPrice` sum.
- `total_sales`: Equals `pressroom_sales`.
- `point_bonus`: Always 0 (removed artificial bonus).
- `ranking_prize`: Always 0 (removed artificial ranking prize).
- `Money Left`: `initial_budget - total_spent + total_sales`. When `get_user_team_info()` returns a valid `budget` value, that overrides the calculated figure.

---

### C. `get_league_finances_history(championship_id)`

Queries historical financial snapshots (`budget`, `team_value`, `points`, `position`, `recorded_at`, `teamname`) from Supabase table `user_team_history`.

#### Parameters
- `championship_id`: String championship ID.

#### Return Value
Data frame containing historical user team financial records.

---

### D. `calculate_futmondo_ranking_prizes(money, members)`

Computes the Futmondo official ranking prize distribution for a league using a triangular-number ratio formula.

#### Parameters
- `money`: Numeric total prize pool to distribute across all ranking positions.
- `members`: Data frame of user teams in the championship (as returned by `get_user_teams`).

#### Algorithm

The function determines the active league size $N$ from the number of rows in `members` (i.e. $N = \text{nrow(user\_teams\_df)}$). Teams flagged with `is_active = FALSE` are considered inactive/former teams and are excluded from the active count.

The total percentage denominator is the sum of integers from 1 to $N$:

$$\text{totalPct} = \sum_{i=1}^{N} i = \frac{N(N+1)}{2}$$

Each ranking position $k$ (where $k = 1$ is the top rank) receives a ratio:

$$\text{ratio}_k = \frac{N - k + 1}{\text{totalPct}}$$

The prize awarded to rank $k$ is:

$$\text{Ranking Prize}_k = \text{round}\left(\text{moneyPerRanking} \times \text{ratio}_k\right)$$

#### Return Value
Numeric vector of length $N$ containing the rounded prize amount for each ranking position (index 1 corresponds to rank 1).