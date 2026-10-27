-- Futmondo Insights - Complete Database Schema
-- Execute this in the Supabase SQL Editor to create all tables, foreign keys, and indices.

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

-- 8. ROUND DREAM TEAM (Best 11 & MVP Accolades per Round)
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

-- ============================================================
-- High-Performance Indices
-- ============================================================

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

-- ============================================================
-- PHASE 1 INTELLIGENCE TABLES
-- ============================================================

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

-- ============================================================
-- Intelligence Table Indices
-- ============================================================

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