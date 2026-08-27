# Futmondo Insights - PostgreSQL Database Schema

This document details the relational database schema used by the Futmondo Insights application to persist historical trends, track bids, and record user standings. These tables are hosted remotely inside your **Supabase (PostgreSQL)** backend instance.

---

## 1. Table Definitions (DDL SQL)

Execute the following DDL query inside your **Supabase SQL Editor** to create the tables, define foreign key relations, and configure primary keys.

```sql
-- 1. Championships Table (Metadata for leagues)
CREATE TABLE IF NOT EXISTS championships (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    mode TEXT,
    sport TEXT,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- 2. Real Clubs Table (Spanish clubs in the league)
CREATE TABLE IF NOT EXISTS real_clubs (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    logo TEXT,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- 3. Players Catalog Table (Player bio & static details)
CREATE TABLE IF NOT EXISTS players (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    slug TEXT NOT NULL,
    role TEXT,
    role2 TEXT,
    photo TEXT,
    real_club_id TEXT REFERENCES real_clubs(id) ON DELETE SET NULL,
    status TEXT,
    rating INTEGER,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- 4. User Teams Table (Championship members)
CREATE TABLE IF NOT EXISTS user_teams (
    id TEXT PRIMARY KEY,
    championship_id TEXT REFERENCES championships(id) ON DELETE CASCADE,
    name TEXT NOT NULL,
    budget BIGINT DEFAULT 0,
    points INTEGER DEFAULT 0,
    position INTEGER,
    team_value BIGINT DEFAULT 0,
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- 5. User Team History (League Standings Progress tracking)
CREATE TABLE IF NOT EXISTS user_team_history (
    id BIGSERIAL PRIMARY KEY,
    user_team_id TEXT REFERENCES user_teams(id) ON DELETE CASCADE,
    points INTEGER NOT NULL,
    budget BIGINT NOT NULL,
    position INTEGER,
    team_value BIGINT DEFAULT 0,
    round_number INTEGER,
    active_teams_count INTEGER,
    recorded_at TIMESTAMPTZ DEFAULT NOW()
);

-- 6. Player History (Roster & Market snapshots for trends)
CREATE TABLE IF NOT EXISTS player_history (
    id BIGSERIAL PRIMARY KEY,
    player_id TEXT REFERENCES players(id) ON DELETE CASCADE,
    championship_id TEXT REFERENCES championships(id) ON DELETE CASCADE,
    value BIGINT NOT NULL,
    change BIGINT DEFAULT 0,
    points INTEGER DEFAULT 0,
    avg_points NUMERIC,
    avg_last_five NUMERIC,
    matches INTEGER DEFAULT 0,
    recorded_at TIMESTAMPTZ DEFAULT NOW()
);

-- 7. Market Transactions (Offers, Clause log, and Pressroom transfer feed)
CREATE TABLE IF NOT EXISTS market_transactions (
    id BIGSERIAL PRIMARY KEY,
    player_id TEXT REFERENCES players(id) ON DELETE CASCADE,
    championship_id TEXT REFERENCES championships(id) ON DELETE CASCADE,
    buyer_team_id TEXT REFERENCES user_teams(id) ON DELETE SET NULL,
    seller_team_id TEXT REFERENCES user_teams(id) ON DELETE SET NULL,
    price BIGINT NOT NULL,
    is_clause BOOLEAN DEFAULT FALSE,
    transaction_date TIMESTAMPTZ DEFAULT NOW()
);

-- 8. Round Dream Team (Best 11 & MVP Accolades per Round)
CREATE TABLE IF NOT EXISTS round_dream_team (
    id BIGSERIAL PRIMARY KEY,
    championship_id TEXT REFERENCES championships(id) ON DELETE CASCADE,
    round_id TEXT NOT NULL,
    round_number NUMERIC NOT NULL,
    player_id TEXT REFERENCES players(id) ON DELETE CASCADE,
    player_name TEXT,
    player_role TEXT,
    points INTEGER NOT NULL DEFAULT 0,
    is_mvp BOOLEAN DEFAULT FALSE,
    is_finished BOOLEAN DEFAULT TRUE,
    updated_at TIMESTAMPTZ DEFAULT NOW(),
    UNIQUE(championship_id, round_number, player_id)
);

-- 9. Player Daily Snapshots (Valuation, stats, and ownership snapshots)
CREATE TABLE IF NOT EXISTS player_daily_snapshots (
    id BIGSERIAL PRIMARY KEY,
    player_id TEXT REFERENCES players(id) ON DELETE CASCADE,
    championship_id TEXT REFERENCES championships(id) ON DELETE CASCADE,
    value BIGINT NOT NULL,
    daily_change BIGINT DEFAULT 0,
    points INTEGER DEFAULT 0,
    fis_score NUMERIC(5,2),
    status TEXT DEFAULT 'ok',
    owner_team_id TEXT REFERENCES user_teams(id) ON DELETE SET NULL,
    is_on_market BOOLEAN DEFAULT FALSE,
    clause_price BIGINT,
    snapshot_date DATE DEFAULT CURRENT_DATE,
    recorded_at TIMESTAMPTZ DEFAULT NOW(),
    UNIQUE(player_id, championship_id, snapshot_date)
);

-- 10. Manager DNA Profiles (Computed trading-behavior profiles)
CREATE TABLE IF NOT EXISTS manager_dna_profiles (
    team_id TEXT PRIMARY KEY REFERENCES user_teams(id) ON DELETE CASCADE,
    championship_id TEXT REFERENCES championships(id) ON DELETE CASCADE,
    aggressiveness NUMERIC(5,2) DEFAULT 50.0,
    avg_overpayment_pct NUMERIC(5,2) DEFAULT 0.0,
    fav_position TEXT,
    trading_frequency NUMERIC(5,2) DEFAULT 0.0,
    avg_holding_days NUMERIC(5,2) DEFAULT 0.0,
    total_trades INTEGER DEFAULT 0,
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- 11. Decision Log (AI recommendation audit trail)
CREATE TABLE IF NOT EXISTS decision_log (
    id BIGSERIAL PRIMARY KEY,
    championship_id TEXT REFERENCES championships(id) ON DELETE CASCADE,
    user_team_id TEXT REFERENCES user_teams(id) ON DELETE CASCADE,
    player_id TEXT REFERENCES players(id) ON DELETE CASCADE,
    recommendation_type TEXT NOT NULL,
    recommended_value NUMERIC,
    actual_action_taken TEXT,
    confidence_pct NUMERIC(5,2),
    outcome_roi NUMERIC(5,2),
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- 12. User Smart Alerts (Personalized push notifications)
CREATE TABLE IF NOT EXISTS user_smart_alerts (
    id BIGSERIAL PRIMARY KEY,
    user_team_id TEXT REFERENCES user_teams(id) ON DELETE CASCADE,
    championship_id TEXT REFERENCES championships(id) ON DELETE CASCADE,
    alert_type TEXT NOT NULL,
    title TEXT NOT NULL,
    message TEXT NOT NULL,
    severity TEXT DEFAULT 'info',
    is_read BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMPTZ DEFAULT NOW()
);
```

## Existing-install upgrade

`scripts/schema.sql` is the canonical DDL for fresh installations. PostgreSQL `CREATE TABLE IF NOT EXISTS` does not add columns to tables that already exist. For an existing deployment created before these fields were added, run this idempotent upgrade in the Supabase SQL Editor:

```sql
ALTER TABLE user_teams ADD COLUMN IF NOT EXISTS is_active BOOLEAN DEFAULT TRUE;
ALTER TABLE user_team_history ADD COLUMN IF NOT EXISTS round_number INTEGER;
ALTER TABLE user_team_history ADD COLUMN IF NOT EXISTS active_teams_count INTEGER;
```

---

## 2. High-Performance Indices

To maintain lightning-fast response times as transaction volumes and snapshot histories grow over the season, run these high-performance indices:

```sql
-- Fast historical analysis queries
CREATE INDEX IF NOT EXISTS idx_player_history_player ON player_history(player_id, recorded_at DESC);
CREATE INDEX IF NOT EXISTS idx_player_history_championship ON player_history(championship_id, recorded_at DESC);

-- Fast team standings tracking queries
CREATE INDEX IF NOT EXISTS idx_team_history_team ON user_team_history(user_team_id, recorded_at DESC);

-- Fast transaction log queries
CREATE INDEX IF NOT EXISTS idx_transactions_player ON market_transactions(player_id, transaction_date DESC);
CREATE INDEX IF NOT EXISTS idx_transactions_championship ON market_transactions(championship_id, transaction_date DESC);

-- Fast dream team lookups per round
CREATE INDEX IF NOT EXISTS idx_dream_team_round ON round_dream_team(championship_id, round_number);
CREATE INDEX IF NOT EXISTS idx_dream_team_player ON round_dream_team(player_id);

-- Intelligence table indices

-- Fast snapshot lookups by player, championship, owner, and market status
CREATE INDEX IF NOT EXISTS idx_snapshots_player ON player_daily_snapshots(player_id, snapshot_date DESC);
CREATE INDEX IF NOT EXISTS idx_snapshots_championship ON player_daily_snapshots(championship_id, snapshot_date DESC);
CREATE INDEX IF NOT EXISTS idx_snapshots_owner ON player_daily_snapshots(owner_team_id, snapshot_date DESC);
CREATE INDEX IF NOT EXISTS idx_snapshots_market ON player_daily_snapshots(is_on_market, snapshot_date DESC);

-- Fast manager profile queries by championship and aggressiveness
CREATE INDEX IF NOT EXISTS idx_manager_dna_championship ON manager_dna_profiles(championship_id);
CREATE INDEX IF NOT EXISTS idx_manager_dna_aggressiveness ON manager_dna_profiles(aggressiveness);

-- Fast decision log queries by team, championship, player, and recommendation type
CREATE INDEX IF NOT EXISTS idx_decision_log_team ON decision_log(user_team_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_decision_log_championship ON decision_log(championship_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_decision_log_player ON decision_log(player_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_decision_log_type ON decision_log(recommendation_type);

-- Fast alert queries by team, unread status, championship, and alert type
CREATE INDEX IF NOT EXISTS idx_alerts_team ON user_smart_alerts(user_team_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_alerts_unread ON user_smart_alerts(user_team_id, is_read, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_alerts_championship ON user_smart_alerts(championship_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_alerts_type ON user_smart_alerts(alert_type);
```

---

## 3. Relational Schema Diagram

* **championships** (1) -> (N) **user_teams**: One championship has many participant teams.
* **user_teams** (1) -> (N) **user_team_history**: One user team logs daily standings/budget progress over the season.
* **real_clubs** (1) -> (N) **players**: One real-world club has many registered players.
* **players** (1) -> (N) **player_history**: One player logs daily valuation changes for historical curve trend plotting.
* **players** (1) -> (N) **market_transactions**: One player receives multiple market offers/buyout clauses.
* **championships** (1) -> (N) **round_dream_team**: One championship has dream team selections for each round.
* **players** (1) -> (N) **round_dream_team**: One player can appear in multiple round dream teams across the season.
* **players** (1) -> (N) **player_daily_snapshots**: One player has a daily snapshot per championship per date.
* **user_teams** (1) -> (N) **player_daily_snapshots**: One team can own many players across snapshots.
* **user_teams** (1) -> (1) **manager_dna_profiles**: One team has exactly one DNA profile per championship.
* **user_teams** (1) -> (N) **decision_log**: One team has many AI recommendation records over time.
* **players** (1) -> (N) **decision_log**: One player is the subject of many recommendations.
* **user_teams** (1) -> (N) **user_smart_alerts**: One team receives many personalized alerts.

---

## 5. Round Dream Team Table

The `round_dream_team` table stores the official "Best 11" selections and MVP accolades for each round of a championship.

### Column Descriptions

| Column | Type | Description |
|---|---|---|
| `id` | BIGSERIAL | Primary key, auto-incrementing unique identifier. |
| `championship_id` | TEXT | Foreign key to `championships(id)`. Identifies which championship/league the selection belongs to. |
| `round_id` | TEXT | The external API round identifier string. |
| `round_number` | NUMERIC | The numeric round (jornada) number, e.g. 1, 2, 3, ... |
| `player_id` | TEXT | Foreign key to `players(id)`. The selected player. |
| `player_name` | TEXT | Denormalized player name for quick display without a join. |
| `player_role` | TEXT | The player's role/position in the dream team formation (e.g. "Portero", "Defensa", "Mediocampista", "Delantero"). |
| `points` | INTEGER | The points the player scored in that round. |
| `is_mvp` | BOOLEAN | Whether this player was named MVP of the round. Exactly one player per round should have `is_mvp = TRUE`. |
| `is_finished` | BOOLEAN | Whether the round's matches are fully completed. Set to FALSE for in-progress rounds where the dream team may still change. |
| `updated_at` | TIMESTAMPTZ | Timestamp of the last update to this record. |

### Constraints

* **UNIQUE(championship_id, round_number, player_id)**: A player can appear at most once per round in a given championship. This prevents duplicate entries when the data is synced from the API.

### Delayed Match Reconciliation

In some championships, not all matches in a round finish at the same time. When a round is still in progress (`is_finished = FALSE`), the dream team selection may be incomplete or provisional. The synchronization process handles this by:

1. **Initial sync**: When a round first has a dream team published (even if incomplete), records are inserted with `is_finished = FALSE`.
2. **Update sync**: Once all matches in the round conclude, the API returns the final dream team. Existing records for that `(championship_id, round_number)` are updated (or upserted via the unique constraint) with the final player list, corrected points, and `is_finished = TRUE`.
3. **Idempotency**: Because of the `UNIQUE(championship_id, round_number, player_id)` constraint, repeated syncs for the same round will not create duplicate rows. A player that drops out of the dream team between the provisional and final selection will have their row deleted during reconciliation.

## 4. Pressroom Market Log Usage

The `market_transactions` table serves dual purpose:

1. **Manual transaction logging**: Individual `log_market_transaction()` calls record specific bid/clause events.
2. **Pressroom feed sync**: `sync_pressroom_transactions_to_supabase()` bulk-inserts the full pressroom transfer feed, capturing every completed player sale across the championship. Each pressroom entry maps to:
    - `player_id`: The transferred player.
    - `championship_id`: The league the transfer occurred in.
    - `buyer_team_id`: The user team that purchased the player. Empty or missing values are sanitized to `NULL` (Futmondo System / Market).
    - `seller_team_id`: The user team that sold the player. Empty or missing values are sanitized to `NULL` (Futmondo System / Market).
    - `price`: The sale price in EUR (integer scale).
    - `created_at`: The timestamp from the pressroom feed.
    - `transaction_date`: Duplicate of `created_at` for compatibility with consumers that expect this field name.

The pressroom data is queried back via `get_pressroom_transactions_from_supabase(championship_id)` for historical analysis and is consumed by `calculate_league_finances()` to compute per-team purchase and sale volumes.

---

## 6. Player Daily Snapshots Table

The `player_daily_snapshots` table records a daily point-in-time snapshot of each player's valuation, statistics, ownership, and market status within a given championship. It powers trend charting, historical regression, and "value at date" lookups.

### Column Descriptions

| Column | Type | Description |
|---|---|---|
| `id` | BIGSERIAL | Primary key, auto-incrementing unique identifier. |
| `player_id` | TEXT | Foreign key to `players(id)`. The player being snapshotted. |
| `championship_id` | TEXT | Foreign key to `championships(id)`. The championship context. |
| `value` | BIGINT | The player's market value in EUR (integer scale) at snapshot time. |
| `daily_change` | BIGINT | The day-over-day change in value. Positive means appreciation, negative means depreciation. |
| `points` | INTEGER | The player's accumulated fantasy points at snapshot time. |
| `fis_score` | NUMERIC(5,2) | The Futmondo Intelligence Score (FIS), a composite metric derived from performance, scarcity, and market momentum. |
| `status` | TEXT | Player status flag (default "ok"). May indicate injuries, suspensions, or other conditions. |
| `owner_team_id` | TEXT | Foreign key to `user_teams(id)`. The team currently owning the player. NULL if unowned. |
| `is_on_market` | BOOLEAN | Whether the player is currently listed on the market for sale. |
| `clause_price` | BIGINT | The player's buyout clause price in EUR, if applicable. |
| `snapshot_date` | DATE | The calendar date of the snapshot. Defaults to CURRENT_DATE. |
| `recorded_at` | TIMESTAMPTZ | The timestamp when the snapshot was written to the database. |

### Constraints

* **UNIQUE(player_id, championship_id, snapshot_date)**: Ensures exactly one snapshot per player per championship per calendar day. Upserts on this constraint enable idempotent daily sync jobs.

### Usage Role

Consumed by the Shiny app to render valuation trend charts, compute daily ROI, and feed the FIS scoring engine. The `idx_snapshots_player` and `idx_snapshots_championship` indices accelerate range queries over date spans.

---

## 7. Manager DNA Profiles Table

The `manager_dna_profiles` table stores a computed behavioral profile for each user team (manager) within a championship. It is updated periodically by a background job that analyzes the team's transaction history.

### Column Descriptions

| Column | Type | Description |
|---|---|---|
| `team_id` | TEXT | Primary key. Foreign key to `user_teams(id)`. The managed team. |
| `championship_id` | TEXT | Foreign key to `championships(id)`. The championship context. |
| `aggressiveness` | NUMERIC(5,2) | Score from 0 to 100 representing how aggressively the manager trades. Higher values indicate more frequent, higher-risk trades. |
| `avg_overpayment_pct` | NUMERIC(5,2) | Average percentage overpaid relative to market value across all purchases. |
| `fav_position` | TEXT | The player position the manager most frequently targets (e.g. "Delantero", "Mediocampista"). |
| `trading_frequency` | NUMERIC(5,2) | Average number of trades per round. |
| `avg_holding_days` | NUMERIC(5,2) | Average number of days a player is held before being sold. |
| `total_trades` | INTEGER | Cumulative count of completed trades. |
| `updated_at` | TIMESTAMPTZ | Timestamp of the last profile recomputation. |

### Constraints

* **PRIMARY KEY (team_id)**: One profile per team per championship. The team_id alone is the PK because the profile is scoped to the team; championship_id provides context.

### Usage Role

Used by the AI recommendation engine to personalize advice based on the manager's historical behavior. A manager with high aggressiveness receives different recommendations than a conservative one. The profile is also surfaced in the analytics dashboard for self-awareness.

---

## 8. Decision Log Table

The `decision_log` table serves as an immutable audit trail for every AI recommendation issued to a user team. It records what was recommended, what action the user actually took, and the eventual financial outcome.

### Column Descriptions

| Column | Type | Description |
|---|---|---|
| `id` | BIGSERIAL | Primary key, auto-incrementing unique identifier. |
| `championship_id` | TEXT | Foreign key to `championships(id)`. The championship context. |
| `user_team_id` | TEXT | Foreign key to `user_teams(id)`. The team that received the recommendation. |
| `player_id` | TEXT | Foreign key to `players(id)`. The player the recommendation concerns. |
| `recommendation_type` | TEXT | Category of recommendation (e.g. "buy", "sell", "hold", "clause"). NOT NULL. |
| `recommended_value` | NUMERIC | The value or price the AI recommended. |
| `actual_action_taken` | TEXT | What the user actually did (e.g. "bought", "sold", "ignored", "partial"). |
| `confidence_pct` | NUMERIC(5,2) | The AI's confidence in the recommendation, expressed as a percentage. |
| `outcome_roi` | NUMERIC(5,2) | The realized return on investment, populated after the outcome is known. |
| `created_at` | TIMESTAMPTZ | Timestamp of when the recommendation was issued. |

### Constraints

* **recommendation_type NOT NULL**: Every log entry must specify the type of recommendation.

### Usage Role

Feeds the model training pipeline for continuous improvement. The `outcome_roi` column enables supervised learning by comparing AI recommendations against realized results. Also used for user-facing "AI Performance" reports that show how often the system's advice was profitable.

---

## 9. User Smart Alerts Table

The `user_smart_alerts` table stores personalized push notifications generated by the intelligence engine. Each alert is scoped to a specific user team and can be marked as read.

### Column Descriptions

| Column | Type | Description |
|---|---|---|
| `id` | BIGSERIAL | Primary key, auto-incrementing unique identifier. |
| `user_team_id` | TEXT | Foreign key to `user_teams(id)`. The team that receives the alert. |
| `championship_id` | TEXT | Foreign key to `championships(id)`. The championship context. |
| `alert_type` | TEXT | Category of alert (e.g. "value_spike", "undervalued", "clause_opportunity", "dna_match"). NOT NULL. |
| `title` | TEXT | Short, scannable title displayed in the notification banner. NOT NULL. |
| `message` | TEXT | Detailed body of the alert. NOT NULL. |
| `severity` | TEXT | Priority level: "info", "warning", or "critical". Defaults to "info". |
| `is_read` | BOOLEAN | Whether the user has acknowledged the alert. Defaults to FALSE. |
| `created_at` | TIMESTAMPTZ | Timestamp of when the alert was generated. |

### Constraints

* **alert_type NOT NULL**, **title NOT NULL**, **message NOT NULL**: Every alert must have a type, title, and body.

### Usage Role

Queried by the Shiny UI to display the notification center. The `idx_alerts_unread` composite index enables efficient retrieval of unread alerts for a given team, ordered by recency. Alerts are generated by background rules that monitor valuation changes, market conditions, and DNA profile matches.