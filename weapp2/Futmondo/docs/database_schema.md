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