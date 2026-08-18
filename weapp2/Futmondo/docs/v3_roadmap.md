# Futmondo Insights 3.0 -- Engineering and Product Roadmap

**Version:** 3.0.0-draft  
**Date:** 2026-08-16  
**Status:** Planning  

---

## Table of Contents

1. [Architectural Philosophy](#1-architectural-philosophy)
2. [The 13 Key Features & Concept Breakdown](#2-the-13-key-features--concept-breakdown)
3. [Phased Milestone Roadmap (Phases 3.0 - 3.9)](#3-phased-milestone-roadmap-phases-30--39)
4. [Exact Mathematical Formulas](#4-exact-mathematical-formulas)
5. [Supabase Schema Extensions](#5-supabase-schema-extensions)
6. [Free vs Pro Feature Matrix](#6-free-vs-pro-feature-matrix)
7. [Navigation Structure](#7-navigation-structure)

---

## 1. Architectural Philosophy

### The Decision Loop: Discover -> Decide -> Execute -> Learn

Futmondo Insights 3.0 is built around a continuous decision loop that mirrors how elite fantasy managers operate in the real world. Every module, metric, and feature maps to one of four stages:

```
  +-----------+       +-----------+       +-----------+       +-----------+
  | DISCOVER  | ----> |  DECIDE   | ----> |  EXECUTE  | ----> |   LEARN   |
  |           |       |           |       |           |       |           |
  | Scan the  |       | Analyze   |       | Place the |       | Measure   |
  | landscape |       | & Choose  |       | action    |       | outcomes  |
  +-----------+       +-----------+       +-----------+       +-----------+
        ^                                                         |
        |                                                         |
        +---------------------------------------------------------+
```

| Stage    | User Goal                          | Supporting Features                              |
|----------|------------------------------------|--------------------------------------------------|
| Discover | Find signal in market noise        | Command Center, Alerts, Data Moat, Power Rankings |
| Decide   | Pick the right move                | FIS Score, Smart Bid, Moneyball Metrics, Deal Grades, Prediction Confidence |
| Execute  | Act with confidence                | Bid Competition, Transfer Simulator, Squad Optimizer |
| Learn    | Improve future decisions           | Manager DNA, Decision Log, Prediction Confidence recalibration |

### Core Principles

1. **Defensive by default** -- every external API call wrapped in `tryCatch()`; no user thread blocks on network I/O.
2. **Cache-first** -- all data fetching routes through `get_cached_data()` to protect against rate limits and deliver sub-millisecond tab switches.
3. **Mobile-first CSS** -- no fixed-width elements; all layouts use Bootstrap fluid structures or responsive media queries.
4. **Single source of truth** -- Supabase is the sole persistent store; no local state duplicates production data.
5. **Progressive disclosure** -- Free users see surface-level metrics; Pro users unlock the full analytical engine.

---

## 2. The 13 Key Features & Concept Breakdown

### 2.1 Command Center

**Stage:** Discover  
**Tier:** Free (summary) / Pro (full)

A single-pane dashboard that replaces the need to jump between tabs. Shows:

- Market heat map (price velocity across positions)
- Top 5 "buy" and top 5 "sell" alerts ranked by FIS Score
- Manager DNA snapshot (your current playstyle profile)
- Recent transfer activity from your league
- Daily league financial summary

**Data sources:** `player_daily_snapshots`, `manager_dna_profiles`, league transaction log.

### 2.2 FIS Score (Fantasy Insight Score)

**Stage:** Decide  
**Tier:** Pro

A composite 0-100 metric that quantifies how attractive a player is as a buy target at the current moment. Combines:

- Recent form (weighted rolling average of match ratings)
- Price momentum (direction and acceleration)
- Ownership change delta (are other managers buying or selling?)
- Positional scarcity index (how rare is this player's quality at their position?)
- Fixture difficulty ahead (next 5 games)

See Section 4 for the exact formula.

### 2.3 Smart Bid

**Stage:** Decide  
**Tier:** Pro

An AI-assisted bid recommendation engine. Given a target player and the current market, it outputs:

- `recommended_bid`: the optimal bid amount
- `win_probability`: estimated chance this bid wins
- `value_rating`: is the bid a good deal? (Overpay / Fair / Steal)
- `bid_history_context`: how this bid compares to last 10 successful bids

The Smart Bid respects the user's Manager DNA (aggressive vs conservative) and their remaining budget.

See Section 4 for the exact formula.

### 2.4 Manager DNA

**Stage:** Learn  
**Tier:** Pro

A persistent profile that tracks each manager's behavioral fingerprint across seasons. Dimensions:

- **Aggression** (0-100): average bid premium over market price
- **Patience** (0-100): average holding period before selling
- **Positional Bias** (categorical): which positions the manager over-indexes on
- **Risk Tolerance** (0-100): willingness to buy players with high variance
- **Market Timing** (0-100): tendency to buy low vs buy high

Profile updates continuously from `decision_log` entries. Used by Smart Bid and Squad Optimizer to personalize recommendations.

### 2.5 Bid Competition

**Stage:** Execute  
**Tier:** Free (view) / Pro (analytics)

Real-time monitoring of active auctions. Shows:

- Current highest bid and number of bidders
- Time remaining on auction
- Historical close price for this player
- Your Smart Bid recommendation overlaid on the live bid

Pro users see probability curves: "At bid X, your win chance is Y%."

### 2.6 Squad Optimizer

**Stage:** Decide / Execute  
**Tier:** Pro

Given a manager's current squad, budget, and constraints (formation, minimum players per position, budget cap), the optimizer:

1. Scores every available player via FIS Score
2. Runs a constraint satisfaction solver to find the highest-scoring valid squad
3. Outputs a ranked list of recommended transfers (buy + sell pairs)
4. Shows the projected squad score improvement

See Section 4 for the constraint optimization formula.

### 2.7 Transfer Simulator

**Stage:** Decide / Execute  
**Tier:** Pro

A "what-if" sandbox. Users can:

- Drag-and-drop hypothetical transfers
- See projected squad strength changes
- See budget impact and remaining flexibility
- Compare "before" and "after" squad compositions side by side
- Save simulation scenarios for later reference

Does NOT execute real transfers; purely analytical.

### 2.8 Moneyball Metrics

**Stage:** Discover / Decide  
**Tier:** Pro

Advanced analytics that go beyond surface-level stats. Includes:

- **xG (Expected Goals)**: shot quality adjusted goal expectation
- **xA (Expected Assists)**: pass quality adjusted assist expectation
- **xGOT (Expected Goals Over/Underperforming Goals)**: xG minus actual goals; identifies regression candidates
- **xAOT**: xA minus actual assists; identifies assist regression candidates
- **Underperformance Index**: composite of xGOT + xAOT; high values = undervalued
- **Overperformance Index**: negative of Underperformance Index; high values = overvalued
- **Shot Conversion Efficiency**: goals per shot, benchmarked against position average
- **Pass Completion Under Pressure**: completion rate when defender is within 2m

These metrics feed into FIS Score and Deal Grades.

### 2.9 Deal Grades

**Stage:** Learn  
**Tier:** Pro

Every completed transfer receives a letter grade (A+ through F-) based on:

- Price paid vs player's intrinsic value (derived from Moneyball Metrics)
- Subsequent performance over the next 5 matches
- Comparison to league average transfer value

Grade calculation:

```
deal_score = 0.4 * value_ratio + 0.35 * post_transfer_form + 0.25 * league_comparison
```

Where:
- `value_ratio = intrinsic_value / price_paid` (capped at 3.0)
- `post_transfer_form = rolling_5_match_rating / position_average_rating` (capped at 2.0)
- `league_comparison = player_rating / league_avg_rating` (capped at 2.0)

Map `deal_score` to letter grade:

| Score Range | Grade |
|-------------|-------|
| 2.00 - 3.00 | A+    |
| 1.80 - 1.99 | A     |
| 1.60 - 1.79 | A-    |
| 1.40 - 1.59 | B+    |
| 1.20 - 1.39 | B     |
| 1.00 - 1.19 | B-    |
| 0.80 - 0.99 | C     |
| 0.60 - 0.79 | D     |
| 0.00 - 0.59 | F     |

### 2.10 Power Rankings

**Stage:** Discover  
**Tier:** Free (top 10) / Pro (full)

League-wide squad strength rankings. Combines:

- Current squad's aggregate player ratings
- Squad depth (quality of bench players)
- Budget remaining (financial flexibility index)
- Recent transfer activity quality (average Deal Grade)
- Projected form over next 5 fixtures

Updated daily via cron job. Displayed as a sortable leaderboard.

### 2.11 Alerts

**Stage:** Discover  
**Tier:** Free (basic) / Pro (advanced)

User-configurable notification system. Alert types:

| Alert Type               | Trigger Condition                              | Free | Pro |
|--------------------------|------------------------------------------------|------|-----|
| Price Drop               | Player price drops by X% in Y days             | Yes  | Yes |
| Price Spike              | Player price rises by X% in Y days             | Yes  | Yes |
| FIS Threshold            | Player's FIS Score crosses user-defined threshold | No   | Yes |
| Manager DNA Shift        | Your own playstyle metric changes by >10 points | No   | Yes |
| Bid Outbid               | Another manager outbids you on active auction  | No   | Yes |
| Squad Optimizer Suggestion | Optimizer finds improvement > threshold       | No   | Yes |
| League Activity          | Any transfer in your league                    | Yes  | Yes |

Alerts stored in `user_smart_alerts` table. Delivered via in-app notification panel.

### 2.12 Prediction Confidence

**Stage:** Learn  
**Tier:** Pro

Every analytical prediction (FIS Score, Smart Bid, Deal Grade) carries a confidence score (0-100%). Confidence is derived from:

- **Data freshness**: how recent is the underlying data?
- **Sample size**: how many data points support this prediction?
- **Historical accuracy**: how well did similar predictions perform in the past?
- **Market volatility**: is the current market stable or chaotic?

Formula:

```
confidence = min(100, w1 * freshness_score + w2 * sample_score + w3 * accuracy_score + w4 * stability_score)
```

Default weights: w1=0.25, w2=0.25, w3=0.30, w4=0.20.

Confidence is displayed alongside every prediction. Low-confidence predictions are flagged with a warning icon.

### 2.13 Data Moat

**Stage:** Discover  
**Tier:** Pro

The proprietary data layer that makes Futmondo Insights irreplaceable. Includes:

- **Daily player snapshots**: full stat dumps captured at a fixed time each day, enabling historical analysis and trend detection
- **Manager behavioral database**: anonymized aggregation of all manager decision patterns across the platform
- **Market microstructure data**: bid history, auction duration distributions, price velocity curves
- **Cross-league benchmarks**: how does your league's average spending compare to other leagues?

The Data Moat grows more valuable with every user and every season. New users immediately benefit from the accumulated intelligence of the platform.

---

## 3. Phased Milestone Roadmap (Phases 3.0 - 3.9)

### Phase 3.0 -- Foundation (Weeks 1-4) [COMPLETED] [IMPLEMENTED]

**Status:** [COMPLETED]  
**Completion Date:** 2026-08-16  
**Delivered Components:** `intelligence_engine.R`, schema extensions Tables 9-12, FIS v1, Smart Bid v1, Manager DNA v1, Command Center feed.

**Goal:** Infrastructure and data layer ready for all downstream features.

| Task | Deliverable |
|------|-------------|
| 3.0.1 | Create `player_daily_snapshots` table; deploy daily cron job |
| 3.0.2 | Create `manager_dna_profiles` table; implement profile calculation |
| 3.0.3 | Create `decision_log` table; hook into all transfer endpoints |
| 3.0.4 | Create `user_smart_alerts` table; implement alert engine skeleton |
| 3.0.5 | Build `get_cached_data()` wrapper; refactor existing fetchers |
| 3.0.6 | Wrap all Supabase writes in `tryCatch()` |
| 3.0.7 | Create docs/v3_roadmap.md (this document) |

**Definition of Done:** All four new tables deployed. Cron job running. Existing fetchers wrapped. No regressions on current modules.

### Phase 3.1 -- Command Center (Weeks 5-6) [COMPLETED] [IMPLEMENTED]

**Status:** [COMPLETED]
**Completion Date:** 2026-08-16
**Delivered Components:** Modules/Today_Module.R, Command Center UI Shell, Actionable Feed, Market Intelligence Radar, Recent League Deals, Default Landing View in ui.R and server.R.

**Goal:** Single-pane dashboard replaces tab-hopping.

| Task | Status | Deliverable |
|------|--------|-------------|
| 3.1.1 | [x] | Build Command Center UI shell (responsive, mobile-first) |
| 3.1.2 | [x] | Implement market heat map widget |
| 3.1.3 | [x] | Implement top buy/sell alert widget |
| 3.1.4 | [x] | Implement Manager DNA snapshot widget |
| 3.1.5 | [x] | Implement recent league activity widget |
| 3.1.6 | [x] | Implement daily financial summary widget |
| 3.1.7 | [x] | Set Command Center as default landing page |

**Definition of Done:** Dashboard loads in <2s on mobile. All widgets render with cached data. Free users see summary; Pro users see full detail.

### Phase 3.2 -- FIS Score & Moneyball Metrics (Weeks 7-9) [COMPLETED] [IMPLEMENTED]

**Status:** [COMPLETED]
**Completion Date:** 2026-08-16
**Delivered Components:** FIS rating column in player tables, fis_tier_filter dropdown, FIS 5-Pillar breakdown panel in player modal with 1-sentence verdict and confidence pill.

**Goal:** Core analytical engine operational.

| Task | Status | Deliverable |
|------|--------|-------------|
| 3.2.1 | [x] | Implement Moneyball Metrics calculation pipeline (xG, xA, xGOT, xAOT) |
| 3.2.2 | [x] | Implement FIS Score formula |
| 3.2.3 | [x] | Build FIS Score ranking table with filters |
| 3.2.4 | [x] | Build Moneyball Metrics detail panel per player |
| 3.2.5 | [x] | Integrate Moneyball Metrics into existing player profiles |
| 3.2.6 | [x] | Add Prediction Confidence to all FIS Score outputs |

**Definition of Done:** FIS Score computed for all active players. Moneyball Metrics visible on player detail pages. Prediction Confidence displayed.

### Phase 3.3 -- Smart Bid & Bid Competition (Weeks 10-12) [COMPLETED] [IMPLEMENTED]

**Status:** [COMPLETED]
**Completion Date:** 2026-08-16
**Delivered Components:** Smart Bid widget in Selected_Player_Module.R, live fair value, recommended bid, expected winning range, max rational bid, expected ROI %, competition level, likely competitor managers prediction, 'Use Smart Bid' 1-click pre-fill.

**Goal:** AI-assisted bidding and live auction analytics.

| Task | Status | Deliverable |
|------|--------|-------------|
| 3.3.1 | [x] | Implement Smart Bid formula |
| 3.3.2 | [x] | Build Smart Bid UI component |
| 3.3.3 | [x] | Integrate Manager DNA into Smart Bid personalization |
| 3.3.4 | [x] | Build Bid Competition live monitoring panel |
| 3.3.5 | [x] | Implement win probability curve visualization |
| 3.3.6 | [x] | Wire Smart Bid into existing auction flow |

**Definition of Done:** Smart Bid recommendation appears on every auction page. Bid Competition panel shows live data. Pro users see probability curves.

### Phase 3.4 -- Squad Optimizer & Transfer Simulator (Weeks 13-15) [COMPLETED] [IMPLEMENTED]

**Status:** [COMPLETED]
**Completion Date:** 2026-08-18
**Delivered Components:** Constraint satisfaction solver (optimize_starting_xi), 7 formations, 5 strategy modes, responsive soccer pitch board UI, Starting XI and Bench reactables, recommend_transfers engine, Transfer Simulator Sandbox with live budget and FIS deltas, 1-click recommendation apply buttons.

**Goal:** Constraint-based optimization and what-if sandbox.

| Task | Status | Deliverable |
|------|--------|-------------|
| 3.4.1 | [x] | Implement constraint satisfaction solver |
| 3.4.2 | [x] | Build Squad Optimizer UI |
| 3.4.3 | [x] | Implement "recommended transfers" output |
| 3.4.4 | [x] | Build Transfer Simulator drag-and-drop / selector interface |
| 3.4.5 | [x] | Implement scenario save/restore |
| 3.4.6 | [x] | Add budget impact visualization |

**Definition of Done:** Optimizer returns valid squad recommendations in <5s. Simulator allows unlimited hypothetical scenarios.

### Phase 3.5 -- Deal Grades & Power Rankings (Weeks 16-17)

**Goal:** Post-hoc analysis and league-wide competition.

| Task | Deliverable |
|------|-------------|
| 3.5.1 | Implement Deal Grade calculation on transfer completion |
| 3.5.2 | Build Deal Grade history panel per manager |
| 3.5.3 | Implement Power Rankings algorithm |
| 3.5.4 | Build Power Rankings leaderboard UI |
| 3.5.5 | Schedule daily Power Rankings update via cron |

**Definition of Done:** Every completed transfer receives a grade. Leaderboard updates daily.

### Phase 3.6 -- Alerts System (Weeks 18-19)

**Goal:** User-configurable notification engine.

| Task | Deliverable |
|------|-------------|
| 3.6.1 | Build alert configuration UI |
| 3.6.2 | Implement all seven alert types |
| 3.6.3 | Build in-app notification panel |
| 3.6.4 | Implement alert history and archive |
| 3.6.5 | Add alert digest email (optional, Pro only) |

**Definition of Done:** Users can configure, view, and archive alerts. Pro users receive all alert types.

### Phase 3.7 -- Manager DNA & Prediction Confidence (Weeks 20-21)

**Goal:** Behavioral profiling and prediction quality tracking.

| Task | Deliverable |
|------|-------------|
| 3.7.1 | Implement Manager DNA continuous update from decision_log |
| 3.7.2 | Build Manager DNA profile visualization |
| 3.7.3 | Implement Prediction Confidence scoring |
| 3.7.4 | Add confidence badges to all predictions |
| 3.7.5 | Build historical accuracy tracker for predictions |

**Definition of Done:** Manager DNA profiles auto-update. All predictions display confidence scores. Historical accuracy available.

### Phase 3.8 -- Data Moat & Cross-League Benchmarks (Weeks 22-24)

**Goal:** Proprietary data layer and competitive intelligence.

| Task | Deliverable |
|------|-------------|
| 3.8.1 | Build cross-league benchmark calculations |
| 3.8.2 | Implement market microstructure analytics |
| 3.8.3 | Build anonymized manager behavioral dashboard |
| 3.8.4 | Add benchmark comparisons to player profiles |
| 3.8.5 | Build Data Moat marketing page (internal) |

**Definition of Done:** Benchmarks available across all leagues. Data Moat documented and ready for marketing.

### Phase 3.9 -- Polish, QA, and Launch (Weeks 25-26)

**Goal:** Production readiness and user onboarding.

| Task | Deliverable |
|------|-------------|
| 3.9.1 | Full regression test suite across all modules |
| 3.9.2 | Performance audit (mobile load times, API response times) |
| 3.9.3 | Accessibility audit (WCAG 2.1 AA) |
| 3.9.4 | Free/Pro tier enforcement audit |
| 3.9.5 | User onboarding flow for 3.0 features |
| 3.9.6 | WriteManifest() for deployment |
| 3.9.7 | Release notes and changelog |

**Definition of Done:** All tests pass. Performance targets met. Tier enforcement verified. Ready for production deploy.

---

## 4. Exact Mathematical Formulas

### 4.1 FIS Score (Fantasy Insight Score)

```
FIS = 100 * ( w1 * form_component
             + w2 * momentum_component
             + w3 * ownership_component
             + w4 * scarcity_component
             + w5 * fixture_component )

Default weights:
  w1 = 0.30  (form)
  w2 = 0.20  (momentum)
  w3 = 0.15  (ownership)
  w4 = 0.15  (scarcity)
  w5 = 0.20  (fixture)
```

**Form Component** (normalized 0-1):

```
form_component = rolling_avg_rating(n=5) / max_possible_rating

Where:
  rolling_avg_rating(n) = mean(match_ratings[-n:])
  max_possible_rating = 10.0
```

**Momentum Component** (normalized 0-1):

```
momentum_component = clamp( (price_direction * 0.6) + (price_acceleration * 0.4), 0, 1 )

Where:
  price_direction = sign(delta_price_7d) * |delta_price_7d| / max_price
  price_acceleration = sign(delta_delta_price) * |delta_delta_price| / max_price
  delta_price_7d = current_price - price_7_days_ago
  delta_delta_price = delta_price_7d - delta_price_7d_prior_week
  clamp(x, 0, 1) = max(0, min(1, x))
```

**Ownership Component** (normalized 0-1):

```
ownership_component = clamp( 0.5 + (ownership_delta_7d / max_ownership_delta), 0, 1 )

Where:
  ownership_delta_7d = current_ownership_pct - ownership_pct_7_days_ago
  max_ownership_delta = 50  (percent points)
```

**Scarcity Component** (normalized 0-1):

```
scarcity_component = 1 - (rank_at_position / total_players_at_position)

Where:
  rank_at_position = percentile rank of player's rolling_avg_rating among players at same position
  total_players_at_position = count of active players at that position
```

**Fixture Component** (normalized 0-1):

```
fixture_component = mean(fixture_ease_score(next_5_games))

Where:
  fixture_ease_score(game) = 1 - (opponent_defensive_strength / max_defensive_strength)
  opponent_defensive_strength = rolling_avg_conceded_rating(opponent, n=5)
```

### 4.2 Smart Bid

```
recommended_bid = base_price * (1 + premium_factor) * dna_adjustment

Where:
  base_price = median(last_10_successful_bids)
  
  premium_factor = f(win_probability_target, market_volatility)
                  = (win_probability_target / 50) * (1 + market_volatility_index)
  
  win_probability_target = 70  (default; user-configurable)
  
  market_volatility_index = std_dev(last_10_bid_prices) / mean(last_10_bid_prices)
  
  dna_adjustment = 1 + (aggression_score - 50) / 200
  # Aggression score from Manager DNA (0-100, 50 = neutral)
  # Range: 0.75 (very conservative) to 1.25 (very aggressive)
```

**Win Probability Estimation**:

```
win_probability(bid) = sigmoid( (bid - current_highest_bid) / (sigma * time_factor) )

Where:
  sigmoid(x) = 1 / (1 + exp(-x))
  sigma = std_dev(last_10_successful_bids)
  time_factor = max(0.1, remaining_auction_time / average_auction_duration)
```

**Value Rating**:

```
value_ratio = intrinsic_player_value / recommended_bid

If value_ratio >= 1.20:  value_rating = "Steal"
If 0.90 <= value_ratio < 1.20:  value_rating = "Fair"
If value_ratio < 0.90:  value_rating = "Overpay"
```

### 4.3 Manager DNA

Each dimension is a rolling calculation over the manager's last N decisions (default N=50):

**Aggression** (0-100):

```
aggression = clamp( 100 * mean( (bid_amount - market_price_at_bid_time) / market_price_at_bid_time ), 0, 100 )
```

**Patience** (0-100):

```
patience = clamp( 100 * (mean(holding_period_days) / max_holding_period_days), 0, 100 )
```

**Positional Bias**:

```
positional_bias = mode( position_of_purchased_players[-N:] )
```

**Risk Tolerance** (0-100):

```
risk_tolerance = clamp( 100 * mean( std_dev(player_match_ratings[-5:]) / mean(player_match_ratings[-5:]) ), 0, 100 )
```

**Market Timing** (0-100):

```
market_timing = clamp( 100 * mean( (price_at_buy - price_30d_low) / (price_30d_high - price_30d_low) ), 0, 100 )
# 0 = always buys at 30-day low; 100 = always buys at 30-day high
```

Profile update frequency: after each completed transfer, batched to Supabase every 60 seconds.

### 4.4 Constraint Squad Optimizer

**Objective**:

```
maximize: sum( FIS(player_i) ) for all player_i in selected_squad
```

**Subject to constraints**:

```
C1: sum(player_i.cost) <= available_budget
C2: count(player_i) == 11  (exactly 11 players)
C3: count(player_i WHERE position = GK) >= 1
C4: count(player_i WHERE position = DEF) >= 3
C5: count(player_i WHERE position = MID) >= 3
C6: count(player_i WHERE position = FWD) >= 1
C7: for each player_i: player_i is not already in current_squad OR player_i is a sell candidate
C8: sum(sell_proceeds) + available_budget >= sum(buy_costs)
```

**Solver approach**:

1. Pre-filter: remove players who violate hard constraints individually
2. Greedy initialization: pick top FIS players per position constraint
3. Local search: swap pairs to improve objective while maintaining feasibility
4. Return top-K solutions ranked by objective value

Implementation note: Use a branch-and-bound solver for exact results on small leagues; fall back to greedy + local search for large player pools.

---

## 5. Supabase Schema Extensions

### 5.1 player_daily_snapshots

Captures a full snapshot of every player's stats at a fixed time each day.

```sql
CREATE TABLE player_daily_snapshots (
    id            BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    player_id     UUID NOT NULL REFERENCES players(id) ON DELETE CASCADE,
    snapshot_date DATE NOT NULL DEFAULT CURRENT_DATE,
    rating        NUMERIC(4,2),
    goals         INTEGER DEFAULT 0,
    assists       INTEGER DEFAULT 0,
    clean_sheets  INTEGER DEFAULT 0,
    xg            NUMERIC(4,3),
    xa            NUMERIC(4,3),
    price         NUMERIC(10,2),
    ownership_pct NUMERIC(5,2),
    total_points  INTEGER DEFAULT 0,
    position      VARCHAR(10),
    team_id       UUID,
    created_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    UNIQUE(player_id, snapshot_date)
);

CREATE INDEX idx_snapshots_player_date ON player_daily_snapshots(player_id, snapshot_date DESC);
CREATE INDEX idx_snapshots_date ON player_daily_snapshots(snapshot_date DESC);
```

**Cron job:** Runs daily at 06:00 UTC. Inserts one row per active player.

### 5.2 manager_dna_profiles

Stores each manager's behavioral fingerprint.

```sql
CREATE TABLE manager_dna_profiles (
    id                  BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    manager_id          UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    aggression_score    NUMERIC(5,2) DEFAULT 50.00,
    patience_score      NUMERIC(5,2) DEFAULT 50.00,
    positional_bias     VARCHAR(10),
    risk_tolerance      NUMERIC(5,2) DEFAULT 50.00,
    market_timing_score NUMERIC(5,2) DEFAULT 50.00,
    decisions_analyzed  INTEGER DEFAULT 0,
    last_updated        TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    UNIQUE(manager_id)
);

CREATE INDEX idx_dna_manager ON manager_dna_profiles(manager_id);
```

**Update policy:** Recalculated after each transfer, batched writes every 60 seconds.

### 5.3 decision_log

Immutable record of every managerial decision for post-hoc analysis.

```sql
CREATE TABLE decision_log (
    id              BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    manager_id      UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    player_id       UUID NOT NULL REFERENCES players(id) ON DELETE CASCADE,
    action          VARCHAR(20) NOT NULL CHECK (action IN ('buy', 'sell', 'bid', 'counter_bid', 'withdraw_bid')),
    bid_amount      NUMERIC(10,2),
    market_price_at_decision NUMERIC(10,2),
    outcome         VARCHAR(20) DEFAULT 'pending' CHECK (outcome IN ('pending', 'won', 'lost', 'cancelled')),
    deal_grade      VARCHAR(3),
    decision_context JSONB,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    resolved_at     TIMESTAMPTZ
);

CREATE INDEX idx_decisions_manager ON decision_log(manager_id, created_at DESC);
CREATE INDEX idx_decisions_player ON decision_log(player_id);
CREATE INDEX idx_decisions_outcome ON decision_log(outcome);
```

**Hook:** Every transfer endpoint writes a row before executing the transaction.

### 5.4 user_smart_alerts

User-configurable alerts and their delivery history.

```sql
CREATE TABLE user_smart_alerts (
    id              BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    user_id         UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    alert_type      VARCHAR(30) NOT NULL,
    player_id       UUID REFERENCES players(id) ON DELETE SET NULL,
    threshold_value NUMERIC(10,2),
    is_active       BOOLEAN NOT NULL DEFAULT TRUE,
    last_triggered  TIMESTAMPTZ,
    trigger_count   INTEGER DEFAULT 0,
    notification_sent BOOLEAN DEFAULT FALSE,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_alerts_user ON user_smart_alerts(user_id, is_active);
CREATE INDEX idx_alerts_type ON user_smart_alerts(alert_type);
```

**Alert engine:** Polls every 5 minutes for threshold breaches. Fires in-app notification; emails for Pro users only.

---

## 6. Free vs Pro Feature Matrix

| Feature                  | Free Tier                          | Pro Tier                              |
|--------------------------|------------------------------------|---------------------------------------|
| Command Center           | Summary view (3 widgets)           | Full view (all 6 widgets)             |
| FIS Score                | Top 5 only                         | Full rankings with filters            |
| Smart Bid                | Not available                      | Full recommendation + probability     |
| Manager DNA              | Not available                      | Full profile + visualization          |
| Bid Competition          | View only                          | Analytics + probability curves        |
| Squad Optimizer          | Not available                      | Full optimizer with recommendations   |
| Transfer Simulator       | Not available                      | Unlimited scenarios                   |
| Moneyball Metrics        | Not available                      | Full suite (xG, xA, xGOT, xAOT, etc.) |
| Deal Grades              | Not available                      | Full grading + history                |
| Power Rankings           | Top 10 only                        | Full leaderboard with filters         |
| Alerts                   | 3 types, 5 max                     | 7 types, unlimited                    |
| Prediction Confidence    | Not available                      | Confidence scores on all predictions  |
| Data Moat                | Not available                      | Benchmarks + cross-league analytics   |

**Pricing note:** Pro tier is a monthly subscription. Pricing TBD by product team.

---

## 7. Navigation Structure

### Primary Navigation (Top Bar)

```
[Logo]  |  Dashboard  |  Players  |  Transfers  |  Rankings  |  Alerts  |  [User Menu]
```

### Dashboard (Command Center)

```
/
  |-- /dashboard              [Default landing page]
  |-- /dashboard/market       [Market heat map detail]
  |-- /dashboard/finance      [League financials detail]
```

### Players

```
/players
  |-- /players/list           [Player search + filters]
  |-- /players/:id            [Player detail page]
  |-- /players/:id/fis        [FIS Score breakdown]
  |-- /players/:id/moneyball  [Moneyball Metrics panel]
  |-- /players/:id/history    [Historical snapshot chart]
```

### Transfers

```
/transfers
  |-- /transfers/active       [Active auctions + Bid Competition]
  |-- /transfers/:id          [Auction detail + Smart Bid]
  |-- /transfers/history      [Transfer history + Deal Grades]
  |-- /transfers/simulator    [Transfer Simulator]
  |-- /transfers/optimizer    [Squad Optimizer]
```

### Rankings

```
/rankings
  |-- /rankings/power         [Power Rankings leaderboard]
  |-- /rankings/managers      [Manager rankings by DNA traits]
  |-- /rankings/deals         [Best/worst deals leaderboard]
```

### Alerts

```
/alerts
  |-- /alerts/active          [Active alerts]
  |-- /alerts/history         [Alert history]
  |-- /alerts/configure       [Alert configuration]
```

### User Menu (Dropdown)

```
[User Avatar]
  |-- Profile
  |-- Manager DNA
  |-- Settings
  |-- Billing (Pro management)
  |-- Logout
```

### Mobile Navigation (Bottom Bar)

```
[Dashboard]  [Players]  [Transfers]  [Rankings]  [More]
                                                         |
                                                         +-- Alerts
                                                         +-- Profile
                                                         +-- Settings
```

---

## Appendix A: Glossary

| Term | Definition |
|------|-----------|
| FIS | Fantasy Insight Score; composite 0-100 attractiveness metric |
| xG | Expected Goals; shot-quality adjusted goal expectation |
| xA | Expected Assists; pass-quality adjusted assist expectation |
| xGOT | Expected Goals Over/Underperforming; xG minus actual goals |
| xAOT | Expected Assists Over/Underperforming; xA minus actual assists |
| DNA | Manager behavioral fingerprint across five dimensions |
| Data Moat | Proprietary accumulated data layer unique to Futmondo |
| Clamp | Mathematical function: clamp(x, min, max) = max(min, min(max, x)) |

## Appendix B: Version History

| Version | Date       | Change |
|---------|------------|--------|
| 3.0.0-draft | 2026-08-16 | Initial roadmap document |

---

*End of document.*