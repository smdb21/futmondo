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

## Defensive Programming

All functions are wrapped in `tryCatch()` blocks to prevent user thread blocking or parent server crashes. Network failures, missing columns, and edge cases (empty data frames, NULL inputs) are handled gracefully with sensible defaults and error messages printed to the console.