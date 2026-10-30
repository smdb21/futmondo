-- Futmondo factual application schema for NEW databases only
-- Existing installations use the forward migrations; do not rerun retired migrations.
BEGIN;

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
    championship_id TEXT NOT NULL REFERENCES championships(id),
    points INTEGER NOT NULL,
    budget BIGINT,
    position INTEGER,
    team_value BIGINT DEFAULT 0,
    round_number INTEGER,
    active_teams_count INTEGER,
    observed_at TIMESTAMPTZ,
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
    observed_at TIMESTAMPTZ,
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
    source_event_id TEXT,
    provenance TEXT NOT NULL DEFAULT 'futmondo_pressroom',
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
-- League-specific observed snapshots
-- ============================================================

-- 9. Player Daily Snapshots (Valuation, stats, and ownership snapshots)
CREATE TABLE IF NOT EXISTS player_daily_snapshots (
    id BIGSERIAL PRIMARY KEY,
    player_id TEXT REFERENCES players(id) ON DELETE CASCADE,
    championship_id TEXT REFERENCES championships(id) ON DELETE CASCADE,
    value BIGINT NOT NULL,
    daily_change BIGINT DEFAULT 0,
    points INTEGER DEFAULT 0,
    role TEXT, role2 TEXT, primary_role TEXT, rating NUMERIC,
    status TEXT,
    owner_team_id TEXT REFERENCES user_teams(id) ON DELETE SET NULL,
    is_on_market BOOLEAN DEFAULT FALSE,
    clause_price BIGINT,
    snapshot_date DATE DEFAULT CURRENT_DATE,
    observed_at TIMESTAMPTZ,
    recorded_at TIMESTAMPTZ DEFAULT NOW(),
    UNIQUE(player_id, championship_id, snapshot_date)
);


CREATE TABLE IF NOT EXISTS auction_observations (
  auction_id TEXT NOT NULL, championship_id TEXT NOT NULL,
  player_id TEXT NOT NULL, winner_id TEXT, winning_amount NUMERIC,
  settled_at TIMESTAMPTZ, observed_at TIMESTAMPTZ NOT NULL,
  outcome TEXT NOT NULL CHECK (outcome IN ('sold','unsold','open','unknown')),
  visibility_complete BOOLEAN NOT NULL DEFAULT FALSE,
  eligible_manager_ids JSONB, reference_value NUMERIC, reference_at TIMESTAMPTZ,
  listing_id TEXT, expires_at TIMESTAMPTZ,
  PRIMARY KEY (championship_id, auction_id)
);
CREATE TABLE IF NOT EXISTS bid_observations (
  championship_id TEXT NOT NULL, auction_id TEXT NOT NULL, player_id TEXT NOT NULL,
  bidder_id TEXT NOT NULL, bidder_name TEXT, amount NUMERIC NOT NULL CHECK(amount>0),
  is_winner BOOLEAN NOT NULL, source_bid_id TEXT,
  settled_at TIMESTAMPTZ, observed_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY(championship_id, auction_id, bidder_id)
);
CREATE TABLE IF NOT EXISTS market_observations (
  championship_id TEXT NOT NULL, player_id TEXT NOT NULL, listing_id TEXT NOT NULL,
  observed_at TIMESTAMPTZ NOT NULL, expires_at TIMESTAMPTZ, value NUMERIC,
  asking_price NUMERIC, bidder_count INTEGER, payload JSONB,
  PRIMARY KEY(championship_id, listing_id, observed_at)
);
CREATE TABLE IF NOT EXISTS player_match_observations (
  player_id TEXT NOT NULL, championship_id TEXT NOT NULL, season TEXT NOT NULL,
  scoring_version TEXT NOT NULL, round NUMERIC NOT NULL, points NUMERIC,
  score_status TEXT NOT NULL, is_home BOOLEAN, minutes_raw NUMERIC, start_raw BOOLEAN,
  participation BOOLEAN, observed_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY(player_id,championship_id,season,scoring_version,round,observed_at)
);
CREATE TABLE IF NOT EXISTS league_rule_snapshots (
  championship_id TEXT NOT NULL, scoring_version TEXT NOT NULL,
  season TEXT NOT NULL, observed_at TIMESTAMPTZ NOT NULL, configuration JSONB NOT NULL,
  PRIMARY KEY(championship_id,scoring_version,observed_at)
);

ALTER TABLE player_match_observations ADD COLUMN IF NOT EXISTS occurred_at TIMESTAMPTZ;
ALTER TABLE player_match_observations ADD COLUMN IF NOT EXISTS round_start_at TIMESTAMPTZ;

CREATE OR REPLACE FUNCTION public.scope_user_team_history()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY INVOKER
SET search_path = public, pg_temp
AS $$
DECLARE owner_championship TEXT;
BEGIN
  SELECT championship_id INTO owner_championship
    FROM public.user_teams WHERE id=NEW.user_team_id;
  IF owner_championship IS NULL THEN
    RAISE EXCEPTION 'History requires a team with a known championship' USING ERRCODE='23514';
  END IF;
  IF NEW.championship_id IS NULL THEN
    NEW.championship_id := owner_championship;
  ELSIF NEW.championship_id IS DISTINCT FROM owner_championship THEN
    RAISE EXCEPTION 'History championship does not match team ownership' USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END;
$$;
REVOKE ALL ON FUNCTION public.scope_user_team_history() FROM PUBLIC, anon, authenticated;
GRANT EXECUTE ON FUNCTION public.scope_user_team_history() TO service_role;
DROP TRIGGER IF EXISTS scope_user_team_history_before_write ON public.user_team_history;
CREATE TRIGGER scope_user_team_history_before_write
BEFORE INSERT OR UPDATE ON public.user_team_history
FOR EACH ROW EXECUTE FUNCTION public.scope_user_team_history();


CREATE OR REPLACE FUNCTION public.reconcile_legacy_market_transaction()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY INVOKER
SET search_path = public, pg_temp
AS $$
DECLARE
  candidate_ids BIGINT[];
  existing_provenance TEXT;
BEGIN
  -- Unidentified observations cannot reconcile historical events.
  IF NEW.source_event_id IS NULL OR NEW.source_event_id = '' OR NEW.championship_id IS NULL THEN
    RETURN NEW;
  END IF;

  -- Normal ON CONFLICT ingestion must keep its existing event identity. Keep
  -- provenance annotations from prior adoption/ambiguity on subsequent syncs.
  SELECT provenance INTO existing_provenance
    FROM public.market_transactions
    WHERE championship_id = NEW.championship_id AND source_event_id = NEW.source_event_id;
  IF FOUND THEN
    IF existing_provenance LIKE '%:legacy_%' THEN
      NEW.provenance := existing_provenance;
    END IF;
    RETURN NEW;
  END IF;

  SELECT array_agg(candidate.id) INTO candidate_ids
    FROM (
      SELECT id FROM public.market_transactions
      WHERE source_event_id IS NULL AND provenance = 'legacy'
        AND championship_id IS NOT DISTINCT FROM NEW.championship_id
        AND player_id IS NOT DISTINCT FROM NEW.player_id
        AND buyer_team_id IS NOT DISTINCT FROM NEW.buyer_team_id
        AND seller_team_id IS NOT DISTINCT FROM NEW.seller_team_id
        AND price IS NOT DISTINCT FROM NEW.price
        AND is_clause IS NOT DISTINCT FROM NEW.is_clause
        AND transaction_date IS NOT DISTINCT FROM NEW.transaction_date
      FOR UPDATE
    ) AS candidate;

  IF cardinality(candidate_ids) = 1 THEN
    -- Preserve the historical row ID and all financial values. A concurrent
    -- collector may already have adopted the row; the NULL predicate prevents
    -- reassignment to a different source event after waiting for its row lock.
    UPDATE public.market_transactions
      SET source_event_id = NEW.source_event_id,
          provenance = COALESCE(NEW.provenance, 'futmondo_pressroom') || ':legacy_adopted'
      WHERE id = candidate_ids[1] AND source_event_id IS NULL AND provenance = 'legacy';
    IF FOUND THEN
      RETURN NULL; -- Already persisted by UPDATE: suppress the duplicate INSERT.
    END IF;
    NEW.provenance := COALESCE(NEW.provenance, 'futmondo_pressroom') || ':legacy_changed';
  ELSIF cardinality(candidate_ids) > 1 THEN
    -- Multiple exact candidates remain untouched for review. The new event is
    -- retained independently with explicit provenance, not guessed or deleted.
    NEW.provenance := COALESCE(NEW.provenance, 'futmondo_pressroom') || ':legacy_ambiguous';
  END IF;
  RETURN NEW;
END;
$$;

REVOKE ALL ON FUNCTION public.reconcile_legacy_market_transaction() FROM PUBLIC, anon, authenticated;
GRANT EXECUTE ON FUNCTION public.reconcile_legacy_market_transaction() TO service_role;
DROP TRIGGER IF EXISTS reconcile_legacy_market_transaction_before_insert ON public.market_transactions;
CREATE TRIGGER reconcile_legacy_market_transaction_before_insert
BEFORE INSERT ON public.market_transactions
FOR EACH ROW EXECUTE FUNCTION public.reconcile_legacy_market_transaction();


CREATE TABLE IF NOT EXISTS observation_revisions (
  kind TEXT NOT NULL, championship_id TEXT NOT NULL, event_key TEXT NOT NULL,
  observed_at TIMESTAMPTZ NOT NULL, payload JSONB NOT NULL,
  PRIMARY KEY(kind,championship_id,event_key,observed_at)
);
ALTER TABLE auction_observations ADD COLUMN IF NOT EXISTS first_observed_at TIMESTAMPTZ;
ALTER TABLE auction_observations ADD COLUMN IF NOT EXISTS last_seen_at TIMESTAMPTZ;
ALTER TABLE bid_observations ADD COLUMN IF NOT EXISTS first_observed_at TIMESTAMPTZ;
ALTER TABLE bid_observations ADD COLUMN IF NOT EXISTS last_seen_at TIMESTAMPTZ;
-- Existing collection timestamps cannot reconstruct earlier knowledge.
ALTER TABLE auction_observations ADD COLUMN IF NOT EXISTS legacy_timing BOOLEAN NOT NULL DEFAULT TRUE;
ALTER TABLE bid_observations ADD COLUMN IF NOT EXISTS legacy_timing BOOLEAN NOT NULL DEFAULT TRUE;
ALTER TABLE auction_observations ALTER COLUMN legacy_timing SET DEFAULT FALSE;
ALTER TABLE bid_observations ALTER COLUMN legacy_timing SET DEFAULT FALSE;

CREATE OR REPLACE FUNCTION preserve_observation_revision() RETURNS TRIGGER
LANGUAGE plpgsql SET search_path=public AS $$
DECLARE event TEXT; previous JSONB; incoming JSONB;
BEGIN
  NEW.first_observed_at := COALESCE(NEW.first_observed_at,NEW.observed_at);
  NEW.last_seen_at := NEW.observed_at;
  IF TG_OP='UPDATE' THEN
    IF OLD.legacy_timing AND NEW.observed_at>OLD.observed_at THEN
      event := OLD.auction_id;
      IF TG_TABLE_NAME='bid_observations' THEN event := event || ':' || (to_jsonb(OLD)->>'bidder_id'); END IF;
      INSERT INTO observation_revisions VALUES(TG_TABLE_NAME,OLD.championship_id,event,OLD.observed_at,to_jsonb(OLD))
        ON CONFLICT DO NOTHING;
      -- A fresh API observation proves knowledge now, not at a guessed legacy date.
      NEW.first_observed_at := NEW.observed_at;
      NEW.legacy_timing := FALSE;
      RETURN NEW;
    END IF;
    NEW.first_observed_at := COALESCE(OLD.first_observed_at,OLD.observed_at);
    NEW.last_seen_at := GREATEST(COALESCE(OLD.last_seen_at,OLD.observed_at),NEW.observed_at);
    previous := to_jsonb(OLD) - ARRAY['observed_at','first_observed_at','last_seen_at','legacy_timing'];
    incoming := to_jsonb(NEW) - ARRAY['observed_at','first_observed_at','last_seen_at','legacy_timing'];
    IF incoming=previous THEN
      NEW.observed_at := OLD.observed_at;
      NEW.legacy_timing := OLD.legacy_timing;
    ELSE
      IF NEW.observed_at <= OLD.observed_at THEN RETURN OLD; END IF;
      event := OLD.auction_id;
      IF TG_TABLE_NAME='bid_observations' THEN event := event || ':' || (to_jsonb(OLD)->>'bidder_id'); END IF;
      INSERT INTO observation_revisions VALUES(TG_TABLE_NAME,OLD.championship_id,event,OLD.observed_at,to_jsonb(OLD))
        ON CONFLICT DO NOTHING;
    END IF;
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS preserve_auction_revision ON auction_observations;
CREATE TRIGGER preserve_auction_revision BEFORE INSERT OR UPDATE ON auction_observations
  FOR EACH ROW EXECUTE FUNCTION preserve_observation_revision();
DROP TRIGGER IF EXISTS preserve_bid_revision ON bid_observations;
CREATE TRIGGER preserve_bid_revision BEFORE INSERT OR UPDATE ON bid_observations
  FOR EACH ROW EXECUTE FUNCTION preserve_observation_revision();

CREATE OR REPLACE VIEW auction_observation_history WITH (security_invoker=true) AS
  SELECT * FROM auction_observations UNION ALL
  SELECT (jsonb_populate_record(NULL::auction_observations,payload)).*
    FROM observation_revisions WHERE kind='auction_observations';
CREATE OR REPLACE VIEW bid_observation_history WITH (security_invoker=true) AS
  SELECT * FROM bid_observations UNION ALL
  SELECT (jsonb_populate_record(NULL::bid_observations,payload)).*
    FROM observation_revisions WHERE kind='bid_observations';
REVOKE ALL ON auction_observation_history,bid_observation_history FROM anon,authenticated;
GRANT SELECT ON auction_observation_history,bid_observation_history TO service_role;


CREATE UNIQUE INDEX IF NOT EXISTS transactions_source_event ON market_transactions(championship_id,source_event_id);
CREATE UNIQUE INDEX IF NOT EXISTS player_history_source_time ON player_history(player_id,championship_id,observed_at);
CREATE UNIQUE INDEX IF NOT EXISTS user_team_history_source_time ON user_team_history(user_team_id,championship_id,observed_at);

CREATE INDEX IF NOT EXISTS player_history_context_page ON public.player_history(championship_id,player_id,recorded_at,id);
CREATE INDEX IF NOT EXISTS team_history_context_page ON public.user_team_history(championship_id,recorded_at,id);
CREATE INDEX IF NOT EXISTS player_round_context_revision ON public.player_match_observations(championship_id,player_id,season,scoring_version,round,observed_at DESC);
CREATE INDEX IF NOT EXISTS market_transactions_buyer ON public.market_transactions(buyer_team_id);
CREATE INDEX IF NOT EXISTS market_transactions_seller ON public.market_transactions(seller_team_id);
CREATE INDEX IF NOT EXISTS players_club ON public.players(real_club_id);
CREATE INDEX IF NOT EXISTS user_teams_championship ON public.user_teams(championship_id);

DO $$
DECLARE tbl TEXT; seq TEXT;
BEGIN
  FOREACH tbl IN ARRAY ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history','market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations','market_observations','player_match_observations','league_rule_snapshots','observation_revisions'] LOOP
    IF to_regclass(format('public.%I',tbl)) IS NOT NULL THEN
      EXECUTE format('ALTER TABLE public.%I ENABLE ROW LEVEL SECURITY',tbl);
      EXECUTE format('REVOKE ALL ON TABLE public.%I FROM PUBLIC, anon, authenticated',tbl);
      EXECUTE format('GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE public.%I TO service_role',tbl);
      IF EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name=tbl AND column_name='id') THEN
        seq := pg_get_serial_sequence(format('public.%I',tbl),'id');
        IF seq IS NOT NULL THEN
          EXECUTE format('REVOKE ALL ON SEQUENCE %s FROM PUBLIC, anon, authenticated',seq);
          EXECUTE format('GRANT USAGE, SELECT ON SEQUENCE %s TO service_role',seq);
        END IF;
      END IF;
    END IF;
  END LOOP;
END $$;

COMMIT;

-- Latest observed score per player/round, preserving all source revisions.
-- Uses player_round_context_revision; no duplicate covering index is needed.
CREATE OR REPLACE VIEW public.player_round_points_latest
WITH (security_invoker = true) AS
SELECT DISTINCT ON (championship_id, player_id, season, scoring_version, round)
  championship_id, player_id, season, scoring_version, round, points,
  score_status, observed_at, occurred_at, round_start_at
FROM public.player_match_observations
ORDER BY championship_id, player_id, season, scoring_version, round, observed_at DESC;
REVOKE ALL ON public.player_round_points_latest FROM PUBLIC, anon, authenticated;
GRANT SELECT ON public.player_round_points_latest TO service_role;
COMMENT ON VIEW public.player_round_points_latest IS
  'Server-only current player/round observations. NULL points remain unknown; source revisions are retained.';
NOTIFY pgrst, 'reload schema';
