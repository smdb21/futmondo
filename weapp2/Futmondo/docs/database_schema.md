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

-- 7. Market Transactions (Offers & Clause log)
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
```

---

## 3. Relational Schema Diagram

* **championships** (1) -> (N) **user_teams**: One championship has many participant teams.
* **user_teams** (1) -> (N) **user_team_history**: One user team logs daily standings/budget progress over the season.
* **real_clubs** (1) -> (N) **players**: One real-world club has many registered players.
* **players** (1) -> (N) **player_history**: One player logs daily valuation changes for historical curve trend plotting.
* **players** (1) -> (N) **market_transactions**: One player receives multiple market offers/buyout clauses.