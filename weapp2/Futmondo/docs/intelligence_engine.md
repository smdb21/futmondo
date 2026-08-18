# Intelligence Engine

The Intelligence Engine provides advanced analytics for the Futmondo Insights application. It computes composite player scores, smart bidding recommendations, manager behavioral profiles, and actionable command-center feeds.

## File Location

`/home/rstudio/workspace/futmondo/weapp2/Futmondo/intelligence_engine.R`

---

## 1. calculate_fis_score

Computes a composite Futmondo Intelligence Score (FIS) for each player in a data frame.

### Parameters

| Parameter | Type | Description |
|---|---|---|
| `players_df` | data.frame | Player data with columns: `id`, `name`, `points`, `value`, `change`, `average.average`, `average.averageLastFive`, `average.matches`, `role`, `status` |
| `weights` | list | Optional named list with numeric weights: `perf` (default 0.30), `form` (default 0.20), `efficiency` (default 0.20), `momentum` (default 0.15), `fixture_risk` (default 0.15) |

### Sub-scores

| Sub-score | Range | Description |
|---|---|---|
| `perf` | 0-100 | Performance: based on points, average points, and matches played |
| `form` | 0-100 | Form momentum: ratio of `averageLastFive` to overall average |
| `efficiency` | 0-100 | Points per million EUR valuation relative to position peers |
| `momentum` | 0-100 | 24h market value change normalized |
| `fixture_risk` | 0-100 | Minutes played trend and injury/suspension status |

### Return Columns

| Column | Type | Description |
|---|---|---|
| `perf` | numeric | Performance sub-score (0-100) |
| `form` | numeric | Form sub-score (0-100) |
| `efficiency` | numeric | Efficiency sub-score (0-100) |
| `momentum` | numeric | Momentum sub-score (0-100) |
| `fixture_risk` | numeric | Fixture risk sub-score (0-100) |
| `fis_score` | numeric | Composite weighted score (0-100, clamped) |
| `fis_tier` | character | Tier: "Strong Buy" (>=80), "Buy" (65-79), "Hold" (45-64), "Sell" (<=44) |
| `fis_summary` | character | One-sentence explanatory string |

### Usage Example

```R
players <- get_championship_players(login, championship_id)
players <- calculate_fis_score(players)
head(players[, c("name", "fis_score", "fis_tier", "fis_summary")])
```

---

## 2. calculate_smart_bid

Computes a structured smart-bid recommendation for a single player.

### Parameters

| Parameter | Type | Description |
|---|---|---|
| `player_row` | data.frame / list | Single-row player data with: `id`, `name`, `value`, `change`, `points`, `role`, `average.average`, `average.averageLastFive`, `average.matches`, `status`, `clause_price` |
| `championship_id` | character | Championship identifier |
| `pressroom_df` | data.frame | Optional pressroom transactions for market context |
| `user_teams_df` | data.frame | Optional user teams data |
| `user_cash` | numeric | Available budget in EUR (default 300,000,000) |

### Return Fields

| Field | Type | Description |
|---|---|---|
| `fair_value` | numeric | Base value adjusted by form and status factors |
| `league_premium_pct` | numeric | Percentage above fair value the market pays (from pressroom history) |
| `min_winning_bid` | numeric | Fair value + 2% premium |
| `recommended_bid` | numeric | Balanced bid considering league premium |
| `max_rational_bid` | numeric | Cap at 150% of fair value or user budget |
| `expected_roi_pct` | numeric | Expected return on investment percentage |
| `competition_level` | character | "High", "Medium", "Low", or "Unknown" |
| `likely_competitors` | list | Team IDs that have previously bought this player |
| `confidence_pct` | numeric | Confidence score (0-100) based on data availability |

### Usage Example

```R
player_row <- players[players$name == "Lamine Yamal", ]
bid_info <- calculate_smart_bid(player_row, championship_id, pressroom_df = pressroom)
cat("Recommended bid:", bid_info$recommended_bid, "\n")
cat("Expected ROI:", bid_info$expected_roi_pct, "%\n")
```

---

## 3. calculate_manager_dna

Computes a behavioral profile for a manager based on pressroom transaction history.

### Parameters

| Parameter | Type | Description |
|---|---|---|
| `team_id` | character | User team identifier |
| `pressroom_df` | data.frame | Pressroom transactions (columns: `id`, `created`, `player_id`, `buyer_team_id`, `seller_team_id`, `price`) |
| `user_teams_df` | data.frame | Optional user teams data |

### Return Fields

| Field | Type | Description |
|---|---|---|
| `team_id` | character | The team identifier |
| `aggressiveness` | numeric | 0-100 score; higher = more aggressive trading |
| `avg_overpayment_pct` | numeric | Average percentage overpaid relative to market baseline |
| `fav_position` | character | Most frequently targeted position (requires player join) |
| `trading_frequency` | numeric | Average trades per estimated round |
| `avg_holding_days` | numeric | Average days a player is held before being sold |
| `total_trades` | integer | Cumulative count of completed trades |
| `insights` | character | Human-readable behavioral summary |

### Usage Example

```R
pressroom <- get_championship_pressroom(login, championship_id)
dna <- calculate_manager_dna("team-123", pressroom)
cat("Aggressiveness:", dna$aggressiveness, "\n")
cat("Insights:", dna$insights, "\n")
```

---

## 4. generate_command_center_feed

Generates top daily actionable manager recommendations.

### Parameters

| Parameter | Type | Description |
|---|---|---|
| `login` | character vector | Login token vector (token, userid, user_name) |
| `championship_id` | character | Championship identifier |
| `user_team_id` | character | Current user's team ID |
| `user_teams_df` | data.frame | All user teams data |
| `players_df` | data.frame | Player data (FIS scores computed if missing) |
| `pressroom_df` | data.frame | Optional pressroom transactions |

### Return Columns

| Column | Type | Description |
|---|---|---|
| `type` | character | Recommendation type: "Buy", "Sell", "Bid", "Clause", "Hold" |
| `title` | character | Short scannable title (e.g., "BUY: Player Name") |
| `description` | character | Detailed explanation |
| `confidence_pct` | numeric | Confidence score (0-100) |
| `action_label` | character | Suggested action (e.g., "Place Bid", "List on Market", "Exercise Clause", "No Action") |
| `player_id` | character | Player identifier |

### Usage Example

```R
players <- get_championship_players(login, championship_id)
players <- calculate_fis_score(players)
feed <- generate_command_center_feed(
  login = login,
  championship_id = championship_id,
  user_team_id = user_team_id,
  user_teams_df = teams_df,
  players_df = players
)
print(feed)
```

---

## Supabase Connector Extensions

The following new functions were added to `supabase_connector.R` to support the intelligence engine:

### log_player_daily_snapshots

Batches and upserts player daily snapshots to the `player_daily_snapshots` table.

```R
log_player_daily_snapshots(players_df, championship_id)
```

### sync_manager_dna_profiles

Batches and upserts manager DNA profiles to the `manager_dna_profiles` table.

```R
sync_manager_dna_profiles(dna_df, championship_id)
```

### log_decision

Inserts a single recommendation record into the `decision_log` table.

```R
log_decision(championship_id, user_team_id, player_id, recommendation_type,
             recommended_value, actual_action, confidence, roi)
```

### fetch_user_smart_alerts

Retrieves personalized alerts from the `user_smart_alerts` table.

```R
fetch_user_smart_alerts(user_team_id, championship_id)
```

---

## 5. optimize_starting_xi

Runs a constraint satisfaction solver to select the optimal Starting 11 from the current squad, given a tactical formation and an optimization mode.

### Parameters

| Parameter | Type | Description |
|---|---|---|
| `squad_df` | data.frame | Current squad data with columns: `id`, `name`, `position`, `fis_score`, `perf`, `form`, `efficiency`, `momentum`, `fixture_risk`, `value`, `status`, `minutes_played` |
| `formation` | character | One of: `"4-3-3"`, `"4-4-2"`, `"3-5-2"`, `"3-4-3"`, `"4-5-1"`, `"5-3-2"`, `"5-4-1"` |
| `mode` | character | One of: `"Max FIS"`, `"Safe XI"`, `"Upside XI"`, `"Form XI"`, `"Fixture XI"` |

### Formation Constraints

Each formation dictates the minimum number of players per position:

| Formation | GK | DEF | MID | FWD |
|-----------|----|-----|-----|-----|
| 4-3-3     | 1  | 4   | 3   | 3   |
| 4-4-2     | 1  | 4   | 4   | 2   |
| 3-5-2     | 1  | 3   | 5   | 2   |
| 3-4-3     | 1  | 3   | 4   | 3   |
| 4-5-1     | 1  | 4   | 5   | 1   |
| 5-3-2     | 1  | 5   | 3   | 2   |
| 5-4-1     | 1  | 5   | 4   | 1   |

### Mode Scoring Formulas

Each mode re-weights the FIS sub-scores to produce a mode-specific selection score:

| Mode | Score Formula |
|------|---------------|
| Max FIS | `fis_score` (raw composite, no reweighting) |
| Safe XI | `0.10 * perf + 0.10 * form + 0.10 * efficiency + 0.10 * momentum + 0.60 * fixture_risk` |
| Upside XI | `0.10 * perf + 0.10 * form + 0.40 * efficiency + 0.40 * momentum + 0.00 * fixture_risk` |
| Form XI | `0.10 * perf + 0.60 * form + 0.10 * efficiency + 0.10 * momentum + 0.10 * fixture_risk` |
| Fixture XI | `0.10 * perf + 0.10 * form + 0.10 * efficiency + 0.10 * momentum + 0.60 * fixture_risk` |

### Solver Approach

1. **Pre-filter**: remove players with `status` = "Injured" or "Suspended".
2. **Position assignment**: map each player to GK/DEF/MID/FWD based on `position` column.
3. **Greedy initialization**: for each position bucket, pick the top-N players by mode score where N matches the formation constraint.
4. **Local search**: iterate pairwise swaps across position boundaries to improve the total squad score while maintaining feasibility.
5. **Tie-breaking**: if two lineups have equal total score, prefer the one with higher budget remaining (cheaper players).

### Return Structure

Returns a list with two elements:

| Element | Type | Description |
|---|---|---|
| `starting_xi` | data.frame | 11 rows; columns: `id`, `name`, `position`, `fis_score`, `mode_score`, `perf`, `form`, `efficiency`, `momentum`, `fixture_risk` |
| `bench` | data.frame | Remaining squad members; same columns as `starting_xi` |

### Usage Example

```R
roster <- get_user_squad(login, user_team_id)
roster <- calculate_fis_score(roster)

result <- optimize_starting_xi(
  squad_df = roster,
  formation = "4-3-3",
  mode = "Max FIS"
)

print(result$starting_xi[, c("name", "position", "fis_score", "mode_score")])
cat("Bench size:", nrow(result$bench), "\n")
```

---

## 6. recommend_transfers

Generates a ranked list of recommended buy/sell transfer pairs that would improve the squad's overall FIS score while respecting budget constraints.

### Parameters

| Parameter | Type | Description |
|---|---|---|
| `squad_df` | data.frame | Current squad data with columns: `id`, `name`, `position`, `fis_score`, `value`, `status` |
| `market_df` | data.frame | Available market data with columns: `id`, `name`, `position`, `fis_score`, `value`, `status` |
| `current_budget` | numeric | Available cash in EUR |
| `max_transfers` | integer | Maximum number of transfer recommendations to return (default 5) |

### Algorithm

1. **Sell candidates**: rank squad players by inverse FIS (lowest FIS first); exclude players with FIS >= squad median.
2. **Buy candidates**: rank market players by FIS (highest first); filter to players whose FIS > squad median and whose `value` <= `current_budget`.
3. **Pairing**: for each sell candidate, find the buy candidate at the same position (or adjacent position) with the highest FIS that fits within the budget after the sale.
4. **Scoring**: compute `fis_impact = buy_fis - sell_fis` and `budget_impact = sell_value - buy_value`.
5. **Ranking**: sort pairs by `fis_impact` descending, then by `budget_impact` descending (prefer pairs that improve FIS and leave budget intact).
6. **Confidence**: compute `confidence_pct` based on data freshness, sample size of player stats, and market volatility (see Prediction Confidence formula in `v3_roadmap.md`).

### Return Structure

Returns a data.frame with one row per recommendation:

| Column | Type | Description |
|---|---|---|
| `action` | character | `"Sell"` or `"Buy"` |
| `player_id` | character | Player identifier |
| `player_name` | character | Player name |
| `position` | character | Player position |
| `fis_impact` | numeric | Expected FIS change for the squad (+ for buys, - for sells) |
| `budget_impact` | numeric | Expected budget change (sell_value - buy_value) |
| `confidence` | numeric | Confidence score (0-100) |
| `paired_with` | character | ID of the counter-transfer player (sell paired with buy and vice versa) |

### Usage Example

```R
roster <- get_user_squad(login, user_team_id)
roster <- calculate_fis_score(roster)
market <- get_championship_players(login, championship_id)
market <- calculate_fis_score(market)

recs <- recommend_transfers(
  squad_df = roster,
  market_df = market,
  current_budget = 50000000,
  max_transfers = 5
)

print(recs[, c("action", "player_name", "fis_impact", "budget_impact", "confidence")])
```

---

## 7. simulate_transfer_scenario

Runs a what-if simulation of a specific set of sell and buy transfers, returning the projected squad composition, budget, and FIS metrics.

### Parameters

| Parameter | Type | Description |
|---|---|---|
| `squad_df` | data.frame | Current squad data with columns: `id`, `name`, `position`, `fis_score`, `value`, `status` |
| `current_budget` | numeric | Available cash in EUR |
| `sell_player_ids` | character vector | IDs of players to sell from the current squad |
| `buy_player_ids` | character vector | IDs of players to buy from the market |
| `market_df` | data.frame | Market data with columns: `id`, `name`, `position`, `fis_score`, `value`, `status` |

### Computation Steps

1. **Validate**: check that all `sell_player_ids` exist in `squad_df` and all `buy_player_ids` exist in `market_df`. Return error list if not.
2. **Remove sells**: subset `squad_df` to exclude `sell_player_ids`. Sum their `value` to get `sell_proceeds`.
3. **Add buys**: subset `market_df` to include only `buy_player_ids`. Sum their `value` to get `buy_costs`.
4. **Budget delta**: `budget_delta = sell_proceeds - buy_costs`.
5. **Projected budget**: `projected_budget = current_budget + budget_delta`. If `projected_budget < 0`, flag as invalid.
6. **FIS delta**: `fis_delta = mean(buy_fis_scores) - mean(sell_fis_scores)`.
7. **Projected squad**: union of remaining squad and bought players.
8. **Projected avg FIS**: `mean(projected_squad$fis_score)`.

### Return Structure

Returns a list with the following elements:

| Element | Type | Description |
|---|---|---|
| `budget_delta` | numeric | Net budget change (sell_proceeds - buy_costs) |
| `projected_budget` | numeric | Budget after transfers |
| `fis_delta` | numeric | Net FIS change (mean buy FIS - mean sell FIS) |
| `projected_avg_fis` | numeric | Average FIS of the projected squad |
| `sell_proceeds` | numeric | Total value from sold players |
| `buy_costs` | numeric | Total cost of bought players |
| `projected_squad` | data.frame | Full projected squad with columns: `id`, `name`, `position`, `fis_score`, `value`, `status`, `transfer_action` ("Sell", "Buy", or "Retained") |
| `is_valid` | boolean | TRUE if `projected_budget >= 0` and all player IDs resolved |
| `errors` | character vector | Empty if valid; contains error messages if validation failed |

### Usage Example

```R
roster <- get_user_squad(login, user_team_id)
roster <- calculate_fis_score(roster)
market <- get_championship_players(login, championship_id)
market <- calculate_fis_score(market)

result <- simulate_transfer_scenario(
  squad_df = roster,
  current_budget = 50000000,
  sell_player_ids = c("player-101", "player-202"),
  buy_player_ids = c("player-303", "player-404"),
  market_df = market
)

if (result$is_valid) {
  cat("Budget delta:", result$budget_delta, "\n")
  cat("FIS delta:", result$fis_delta, "\n")
  cat("Projected avg FIS:", result$projected_avg_fis, "\n")
  print(result$projected_squad[, c("name", "position", "fis_score", "transfer_action")])
} else {
  cat("Simulation errors:", paste(result$errors, collapse = ", "), "\n")
}
```

---

## Defensive Programming

All functions are wrapped in `tryCatch()` blocks to prevent user thread blocking or parent server crashes. Network failures, missing columns, and edge cases (empty data frames, NULL inputs) are handled gracefully with sensible defaults and error messages printed to the console.