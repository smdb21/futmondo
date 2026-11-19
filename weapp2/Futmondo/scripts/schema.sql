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

-- 2. Real Clubs Table (source club identities across football competitions)
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
    competition_id TEXT NOT NULL DEFAULT 'unknown',
    season TEXT NOT NULL DEFAULT 'unknown',
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
    competition_id TEXT NOT NULL DEFAULT 'unknown',
    season TEXT NOT NULL DEFAULT 'unknown',
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
    competition_id TEXT NOT NULL DEFAULT 'unknown',
    season TEXT NOT NULL DEFAULT 'unknown',
    buyer_team_id TEXT REFERENCES user_teams(id) ON DELETE SET NULL,
    seller_team_id TEXT REFERENCES user_teams(id) ON DELETE SET NULL,
    price BIGINT NOT NULL,
    is_clause BOOLEAN DEFAULT NULL,
    source_event_id TEXT,
    provenance TEXT NOT NULL DEFAULT 'futmondo_pressroom',
    transaction_date TIMESTAMPTZ DEFAULT NOW()
);

-- 8. ROUND DREAM TEAM (Best 11 & MVP Accolades per Round)
CREATE TABLE IF NOT EXISTS round_dream_team (
    id BIGSERIAL PRIMARY KEY,
    championship_id TEXT REFERENCES championships(id) ON DELETE CASCADE,
    competition_id TEXT NOT NULL DEFAULT 'unknown',
    season TEXT NOT NULL DEFAULT 'unknown',
    scoring_version TEXT NOT NULL DEFAULT 'unknown',
    round_id TEXT NOT NULL,
    round_number NUMERIC NOT NULL,
    player_id TEXT REFERENCES players(id) ON DELETE CASCADE,
    player_name TEXT,
    player_role TEXT,
    points INTEGER NOT NULL DEFAULT 0,
    is_mvp BOOLEAN DEFAULT FALSE,
    is_finished BOOLEAN DEFAULT TRUE,
    updated_at TIMESTAMPTZ DEFAULT NOW(),
    CONSTRAINT round_dream_team_context_round_key UNIQUE(championship_id, competition_id, season, scoring_version, round_id, player_id)
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
    competition_id TEXT NOT NULL DEFAULT 'unknown',
    season TEXT NOT NULL DEFAULT 'unknown',
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
    CONSTRAINT player_daily_snapshots_context_date_key UNIQUE(player_id, championship_id, competition_id, season, snapshot_date)
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
  competition_id TEXT NOT NULL DEFAULT 'unknown',
  source_round_key TEXT NOT NULL DEFAULT '', round_id TEXT, match_id TEXT,
  PRIMARY KEY(player_id,championship_id,competition_id,season,scoring_version,round,source_round_key,observed_at)
);
CREATE TABLE IF NOT EXISTS league_rule_snapshots (
  championship_id TEXT NOT NULL, scoring_version TEXT NOT NULL,
  season TEXT NOT NULL, observed_at TIMESTAMPTZ NOT NULL, configuration JSONB NOT NULL,
  competition_id TEXT NOT NULL DEFAULT 'unknown',
  PRIMARY KEY(championship_id,competition_id,season,scoring_version,observed_at)
);

ALTER TABLE player_match_observations ADD COLUMN IF NOT EXISTS source_round_number NUMERIC;
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
  existing_clause BOOLEAN;
BEGIN
  -- Unidentified observations cannot reconcile historical events.
  IF NEW.source_event_id IS NULL OR NEW.source_event_id = '' OR NEW.championship_id IS NULL THEN
    RETURN NEW;
  END IF;

  -- Normal ON CONFLICT ingestion must keep its existing event identity. Keep
  -- provenance annotations from prior adoption/ambiguity on subsequent syncs.
  SELECT provenance, is_clause INTO existing_provenance, existing_clause
    FROM public.market_transactions
    WHERE championship_id = NEW.championship_id AND source_event_id = NEW.source_event_id;
  IF FOUND THEN
    -- A pressroom replay carries no new method evidence. Retain a known method.
    NEW.is_clause := COALESCE(NEW.is_clause, existing_clause);
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
        AND (NEW.is_clause IS NULL OR is_clause IS NULL OR is_clause = NEW.is_clause)
        AND transaction_date IS NOT DISTINCT FROM NEW.transaction_date
      FOR UPDATE
    ) AS candidate;

  IF cardinality(candidate_ids) = 1 THEN
    -- Preserve the historical row ID and all financial values. A concurrent
    -- collector may already have adopted the row; the NULL predicate prevents
    -- reassignment to a different source event after waiting for its row lock.
    UPDATE public.market_transactions
      SET source_event_id = NEW.source_event_id,
          is_clause = COALESCE(NEW.is_clause, is_clause),
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
CREATE UNIQUE INDEX IF NOT EXISTS player_history_source_time ON player_history(player_id,championship_id,competition_id,season,observed_at);
CREATE UNIQUE INDEX IF NOT EXISTS user_team_history_source_time ON user_team_history(user_team_id,championship_id,competition_id,season,observed_at);

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
SELECT DISTINCT ON (championship_id, competition_id, player_id, season, scoring_version, round, source_round_key)
  championship_id, player_id, season, scoring_version, round, points,
  score_status, observed_at, occurred_at, round_start_at,
  competition_id, source_round_key, round_id, match_id, source_round_number
FROM public.player_match_observations
ORDER BY championship_id, competition_id, player_id, season, scoring_version, round, source_round_key, observed_at DESC;
REVOKE ALL ON public.player_round_points_latest FROM PUBLIC, anon, authenticated;
GRANT SELECT ON public.player_round_points_latest TO service_role;
COMMENT ON VIEW public.player_round_points_latest IS
  'Server-only current player/round observations. NULL points remain unknown; source revisions are retained.';
NOTIFY pgrst, 'reload schema';

-- Successful active-session acquisitions are recorded only after score writes.
-- Immutable check timestamps prevent older concurrent responses replacing newer checks.
CREATE TABLE IF NOT EXISTS public.player_round_history_checks (
 championship_id text NOT NULL,
 player_id text NOT NULL,
 season text NOT NULL,
 scoring_version text NOT NULL,
 checked_at timestamptz NOT NULL,
 result_status text NOT NULL CHECK (result_status IN ('scores','empty')),
 point_count integer NOT NULL CHECK (point_count BETWEEN 0 AND 500),
 competition_id text NOT NULL DEFAULT 'unknown',
 PRIMARY KEY (championship_id, competition_id, player_id, season, scoring_version, checked_at),
 CHECK ((result_status = 'empty' AND point_count = 0) OR (result_status = 'scores' AND point_count > 0))
);
ALTER TABLE public.player_round_history_checks ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.player_round_history_checks FROM PUBLIC, anon, authenticated;
GRANT SELECT, INSERT, UPDATE ON public.player_round_history_checks TO service_role;
CREATE OR REPLACE VIEW public.player_round_dataset
WITH (security_invoker = true) AS
SELECT 'point'::text AS row_kind,
 championship_id, player_id, season, scoring_version, round, points,
 score_status, observed_at, occurred_at, round_start_at,
 NULL::timestamptz AS checked_at, NULL::text AS result_status, NULL::integer AS point_count,
 competition_id, source_round_key, round_id, match_id, source_round_number
FROM public.player_round_points_latest
UNION ALL
SELECT 'check'::text AS row_kind,
 championship_id, player_id, season, scoring_version, NULL::integer AS round, NULL::numeric AS points,
 NULL::text AS score_status, NULL::timestamptz AS observed_at,
 NULL::timestamptz AS occurred_at, NULL::timestamptz AS round_start_at,
 checked_at, result_status, point_count,
 competition_id, NULL::text AS source_round_key, NULL::text AS round_id, NULL::text AS match_id, NULL::numeric AS source_round_number
FROM (
 SELECT DISTINCT ON (championship_id, competition_id, player_id, season, scoring_version)
 championship_id, player_id, season, scoring_version, checked_at, result_status, point_count, competition_id
 FROM public.player_round_history_checks
 ORDER BY championship_id, competition_id, player_id, season, scoring_version, checked_at DESC
) checks;
REVOKE ALL ON public.player_round_dataset FROM PUBLIC, anon, authenticated;
GRANT SELECT ON public.player_round_dataset TO service_role;
COMMENT ON TABLE public.player_round_history_checks IS
 'Successful source checks, including empty histories; written after all scores are acknowledged.';
COMMENT ON VIEW public.player_round_dataset IS
 'Server-only league batch containing latest score revisions and successful source check metadata.';
NOTIFY pgrst, 'reload schema';

-- Immutable, account-scoped evidence; corrections preserve earlier observations.
CREATE TABLE IF NOT EXISTS public.league_cash_defaults (
 championship_id text PRIMARY KEY CHECK (length(btrim(championship_id)) > 0),
 amount numeric NOT NULL CHECK (amount >= 0 AND amount < 'Infinity'::numeric),
 source text NOT NULL CHECK (length(btrim(source)) > 0),
 observed_at timestamptz NOT NULL DEFAULT now() CHECK (isfinite(observed_at))
);
CREATE TABLE IF NOT EXISTS public.rival_finance_evidence (
 account_id text NOT NULL CHECK (length(btrim(account_id)) > 0),
 championship_id text NOT NULL CHECK (length(btrim(championship_id)) > 0),
 competition_id text NOT NULL DEFAULT 'unknown',
 season text NOT NULL CHECK (length(btrim(season)) > 0),
 evidence_type text NOT NULL CHECK (evidence_type IN ('awards','rules','movements','team','coverage','eligibility','notifications')),
 source_id text NOT NULL CHECK (length(btrim(source_id)) > 0),
 observed_at timestamptz NOT NULL CHECK (isfinite(observed_at)),
 payload jsonb NOT NULL CHECK (jsonb_typeof(payload) = 'object' AND octet_length(payload::text) <= 1048576),
 PRIMARY KEY (account_id, championship_id, competition_id, season, evidence_type, source_id, observed_at)
);
CREATE OR REPLACE VIEW public.rival_finance_evidence_latest WITH (security_invoker = true) AS
 SELECT DISTINCT ON (account_id, championship_id, competition_id, season, evidence_type, source_id)
 account_id, championship_id, season, evidence_type, source_id, observed_at, payload, competition_id
 FROM public.rival_finance_evidence
 ORDER BY account_id, championship_id, competition_id, season, evidence_type, source_id, observed_at DESC;
ALTER TABLE public.league_cash_defaults ENABLE ROW LEVEL SECURITY;
ALTER TABLE public.rival_finance_evidence ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.league_cash_defaults, public.rival_finance_evidence, public.rival_finance_evidence_latest FROM PUBLIC, anon, authenticated, service_role;
GRANT SELECT, INSERT, UPDATE ON public.league_cash_defaults, public.rival_finance_evidence TO service_role;
GRANT SELECT ON public.rival_finance_evidence_latest TO service_role;
INSERT INTO public.league_cash_defaults(championship_id, amount, source)
 VALUES ('5b55f9d767214483120b87cf', 300000000, 'user_confirmed') ON CONFLICT (championship_id) DO NOTHING;
COMMENT ON TABLE public.rival_finance_evidence IS 'Server-only account/league/season-scoped observed accounting revisions. Award payloads replace the whole source selection; notifications are not payments.';
NOTIFY pgrst, 'reload schema';
-- Browser credentials are opaque. Only encrypted Futmondo tokens are retained.
CREATE TABLE IF NOT EXISTS public.remembered_logins (
 browser_hash text PRIMARY KEY CHECK (browser_hash ~ '^[a-f0-9]{64}$'),
 encrypted_payload text NOT NULL CHECK (length(encrypted_payload) BETWEEN 56 AND 16384),
 created_at timestamptz NOT NULL DEFAULT now() CHECK (isfinite(created_at)),
 expires_at timestamptz NOT NULL CHECK (isfinite(expires_at)),
 revoked_at timestamptz CHECK (revoked_at IS NULL OR isfinite(revoked_at)),
 CHECK (expires_at > created_at AND expires_at <= created_at + interval '7 days')
);
CREATE INDEX IF NOT EXISTS remembered_logins_expiry_idx ON public.remembered_logins (expires_at);
ALTER TABLE public.remembered_logins ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.remembered_logins FROM PUBLIC, anon, authenticated, service_role;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.remembered_logins TO service_role;
COMMENT ON TABLE public.remembered_logins IS 'Server-only seven-day remembered login credentials: SHA-256 browser secret hash and authenticated encrypted login envelope. No passwords or plaintext tokens. No public RLS policies.';
NOTIFY pgrst, 'reload schema';

-- Atomic, server-only baselines for complete source history reads.
-- A missing row is a cache miss; this table never replaces factual ledgers.
BEGIN;
CREATE TABLE IF NOT EXISTS public.complete_history_baselines (
 account_id text NOT NULL CHECK (length(account_id) BETWEEN 1 AND 256),
 championship_id text NOT NULL CHECK (length(championship_id) BETWEEN 1 AND 256),
 competition_id text NOT NULL DEFAULT 'unknown',
 season text NOT NULL CHECK (length(season) BETWEEN 1 AND 256),
 scoring_version text NOT NULL CHECK (length(scoring_version) BETWEEN 1 AND 256),
 resource text NOT NULL,
 encoding text NOT NULL CHECK (encoding = 'json-gzip-base64-v1'),
 payload text NOT NULL CHECK (octet_length(payload) BETWEEN 1 AND 921600),
 payload_sha256 text NOT NULL CHECK (payload_sha256 ~ '^[0-9a-f]{64}$'),
 json_bytes integer NOT NULL CHECK (json_bytes BETWEEN 1 AND 16777216),
 snapshot_hash text NOT NULL CHECK (snapshot_hash ~ '^[0-9a-f]{64}$'),
 observed_at timestamptz NOT NULL,
 recorded_at timestamptz NOT NULL DEFAULT now(),
 PRIMARY KEY (account_id,championship_id,competition_id,season,scoring_version,resource),
 CHECK (resource = 'pressroom_' || championship_id OR
        resource = 'pressroom_locker_payments_' || championship_id)
);
ALTER TABLE public.complete_history_baselines ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.complete_history_baselines FROM PUBLIC, anon, authenticated;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.complete_history_baselines TO service_role;

-- Concurrent sessions must not replace newer complete history with older data.
CREATE OR REPLACE FUNCTION public.keep_newest_complete_history_baseline()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
BEGIN
 IF NEW.observed_at < OLD.observed_at THEN RETURN OLD; END IF;
 NEW.recorded_at := clock_timestamp();
 RETURN NEW;
END;
$$;
REVOKE ALL ON FUNCTION public.keep_newest_complete_history_baseline() FROM PUBLIC, anon, authenticated;
GRANT EXECUTE ON FUNCTION public.keep_newest_complete_history_baseline() TO service_role;
DROP TRIGGER IF EXISTS keep_newest_complete_history_baseline ON public.complete_history_baselines;
CREATE TRIGGER keep_newest_complete_history_baseline
BEFORE UPDATE ON public.complete_history_baselines
FOR EACH ROW EXECUTE FUNCTION public.keep_newest_complete_history_baseline();
COMMENT ON TABLE public.complete_history_baselines IS
 'Bounded normalized source snapshots and completion metadata, atomically replaced after active-session reads. No authentication material.';
NOTIFY pgrst, 'reload schema';
COMMIT;
BEGIN;
CREATE TABLE IF NOT EXISTS public.user_page_preferences (
 user_id text PRIMARY KEY CHECK (length(user_id) BETWEEN 1 AND 256 AND user_id ~ '^[A-Za-z0-9_-]+$'),
 page text NOT NULL CHECK (page IN ('notifications','today','yourteam','lineup','players_in_championship','rivals','plots','classification','round_mvps','admin')),
 visited_at timestamptz NOT NULL,
 recorded_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
ALTER TABLE public.user_page_preferences ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.user_page_preferences FROM PUBLIC, anon, authenticated, service_role;
GRANT SELECT, INSERT, UPDATE ON public.user_page_preferences TO service_role;
CREATE OR REPLACE FUNCTION public.keep_latest_user_page()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
BEGIN
 IF NEW.visited_at < OLD.visited_at THEN RETURN OLD; END IF;
 NEW.recorded_at := clock_timestamp();
 RETURN NEW;
END;
$$;
REVOKE ALL ON FUNCTION public.keep_latest_user_page() FROM PUBLIC, anon, authenticated;
GRANT EXECUTE ON FUNCTION public.keep_latest_user_page() TO service_role;
DROP TRIGGER IF EXISTS keep_latest_user_page ON public.user_page_preferences;
CREATE TRIGGER keep_latest_user_page BEFORE UPDATE ON public.user_page_preferences
FOR EACH ROW EXECUTE FUNCTION public.keep_latest_user_page();
COMMENT ON TABLE public.user_page_preferences IS 'Server-only last visited application page per Futmondo account. No credentials. Login is excluded; authorization is checked before restoring Admin.';
NOTIFY pgrst, 'reload schema';
COMMIT;

-- Server-only admission policy. Application admins are verified on the R server.
CREATE TABLE IF NOT EXISTS public.application_access (
  id TEXT PRIMARY KEY DEFAULT 'main' CHECK (id = 'main'),
  admin_only BOOLEAN NOT NULL DEFAULT TRUE,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
ALTER TABLE public.application_access ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.application_access FROM PUBLIC, anon, authenticated, service_role;
GRANT SELECT, INSERT, UPDATE ON public.application_access TO service_role;
INSERT INTO public.application_access (id, admin_only) VALUES ('main', TRUE) ON CONFLICT (id) DO NOTHING;
COMMENT ON TABLE public.application_access IS 'Server-only application access switch. Default blocks non-admins. No browser policies; app server verifies administrator identity before writes. Excluded from factual-data reset.';

BEGIN;
CREATE TABLE IF NOT EXISTS public.team_lineup_snapshots (
 account_id text NOT NULL CHECK (length(account_id) BETWEEN 1 AND 256),
 championship_id text NOT NULL CHECK (length(championship_id) BETWEEN 1 AND 256),
 competition_id text NOT NULL DEFAULT 'unknown',
 user_team_id text NOT NULL CHECK (length(user_team_id) BETWEEN 1 AND 256),
 target_round text NOT NULL CHECK (length(target_round) <= 256),
 season text NOT NULL,
 scoring_version text NOT NULL,
 payload jsonb NOT NULL CHECK (jsonb_typeof(payload) = 'object' AND octet_length(payload::text) <= 524288),
 observed_at timestamptz NOT NULL,
 expires_at timestamptz NOT NULL CHECK (expires_at >= observed_at),
 complete boolean NOT NULL CHECK (complete),
 PRIMARY KEY (account_id, championship_id, competition_id, user_team_id, target_round, season, scoring_version)
);
ALTER TABLE public.team_lineup_snapshots ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.team_lineup_snapshots FROM PUBLIC, anon, authenticated, service_role;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.team_lineup_snapshots TO service_role;
CREATE OR REPLACE FUNCTION public.keep_latest_team_lineup_snapshot()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
BEGIN
 IF NEW.observed_at < OLD.observed_at THEN RETURN OLD; END IF;
 RETURN NEW;
END;
$$;
REVOKE ALL ON FUNCTION public.keep_latest_team_lineup_snapshot() FROM PUBLIC, anon, authenticated;
GRANT EXECUTE ON FUNCTION public.keep_latest_team_lineup_snapshot() TO service_role;
DROP TRIGGER IF EXISTS keep_latest_team_lineup_snapshot ON public.team_lineup_snapshots;
CREATE TRIGGER keep_latest_team_lineup_snapshot BEFORE UPDATE ON public.team_lineup_snapshots
 FOR EACH ROW EXECUTE FUNCTION public.keep_latest_team_lineup_snapshot();
COMMENT ON TABLE public.team_lineup_snapshots IS 'Server-only normalized editable lineup, roster eligibility and formation entitlement snapshots. Exact account/team/round scope; no credentials. Incomplete acquisitions are never saved.';

NOTIFY pgrst, 'reload schema';
COMMIT;

-- Shared competition contracts for a fresh installation.
BEGIN;

-- Normalized, account-scoped source context. This is metadata, not permission
-- to access a championship: the R server verifies current account membership.
CREATE TABLE IF NOT EXISTS public.league_context_observations (
 account_id text NOT NULL CHECK (length(account_id) BETWEEN 1 AND 256),
 championship_id text NOT NULL CHECK (length(championship_id) BETWEEN 1 AND 256),
 competition_id text NOT NULL CHECK (length(competition_id) BETWEEN 1 AND 256),
 season text NOT NULL CHECK (length(season) BETWEEN 1 AND 256),
 scoring_version text NOT NULL CHECK (length(scoring_version) BETWEEN 1 AND 256),
 observed_at timestamptz NOT NULL CHECK (isfinite(observed_at)),
 expires_at timestamptz NOT NULL CHECK (isfinite(expires_at) AND expires_at >= observed_at),
 complete boolean NOT NULL,
 payload jsonb NOT NULL CHECK (jsonb_typeof(payload) = 'object' AND octet_length(payload::text) <= 1048576),
 PRIMARY KEY (account_id, championship_id, competition_id, season, scoring_version, observed_at)
);
CREATE INDEX IF NOT EXISTS league_context_latest
 ON public.league_context_observations(account_id, championship_id, observed_at DESC);

-- Source player IDs are opaque records, not permanent people. Multiple real
-- competitions may observe the same source player simultaneously. Source
-- effective dates are optional; first observation is never a transfer date.
CREATE TABLE IF NOT EXISTS public.player_membership_observations (
 player_id text NOT NULL CHECK (length(player_id) BETWEEN 1 AND 256),
 competition_id text NOT NULL CHECK (length(competition_id) BETWEEN 1 AND 256),
 season text NOT NULL CHECK (length(season) BETWEEN 1 AND 256),
 real_club_id text CHECK (real_club_id IS NULL OR length(real_club_id) BETWEEN 1 AND 256),
 observed_at timestamptz NOT NULL CHECK (isfinite(observed_at)),
 effective_at timestamptz CHECK (effective_at IS NULL OR isfinite(effective_at)),
 source text NOT NULL CHECK (length(source) BETWEEN 1 AND 128),
 UNIQUE NULLS NOT DISTINCT (player_id, competition_id, season, observed_at, real_club_id)
);
CREATE INDEX IF NOT EXISTS player_membership_context_time
 ON public.player_membership_observations(competition_id, season, player_id, observed_at DESC);

DO $$ DECLARE tbl text;
BEGIN
 FOREACH tbl IN ARRAY ARRAY['league_context_observations','player_membership_observations'] LOOP
  EXECUTE format('ALTER TABLE public.%I ENABLE ROW LEVEL SECURITY',tbl);
  EXECUTE format('REVOKE ALL ON public.%I FROM PUBLIC, anon, authenticated, service_role',tbl);
  EXECUTE format('GRANT SELECT, INSERT ON public.%I TO service_role',tbl);
 END LOOP;
END $$;
COMMENT ON TABLE public.league_context_observations IS
 'Immutable normalized context/configuration evidence. No authentication material. Cached metadata never authorizes league membership.';
COMMENT ON TABLE public.player_membership_observations IS
 'Append-only source player/competition/season club observations. No inferred person aliases, transfer dates, or retirement from catalog disappearance.';

CREATE INDEX IF NOT EXISTS player_round_league_context_revision
 ON public.player_match_observations(championship_id,competition_id,season,scoring_version,player_id,round,source_round_key,observed_at DESC);
CREATE INDEX IF NOT EXISTS player_round_checks_league_context
 ON public.player_round_history_checks(championship_id,competition_id,season,scoring_version,player_id,checked_at DESC);
CREATE INDEX IF NOT EXISTS player_history_full_context_page
 ON public.player_history(championship_id,competition_id,season,player_id,recorded_at,id);
CREATE INDEX IF NOT EXISTS team_history_full_context_page
 ON public.user_team_history(championship_id,competition_id,season,recorded_at,id);

-- Validate only incoming writes. Existing rows are neither deleted nor
-- relabeled; any legacy inconsistency remains available for reconciliation.
CREATE OR REPLACE FUNCTION public.guard_team_linked_league()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
DECLARE ids text[]; tid text; owner_championship text;
BEGIN
 IF NEW.championship_id IS NULL OR btrim(NEW.championship_id) = '' THEN
  RAISE EXCEPTION 'Team-linked observations require a championship' USING ERRCODE='23514';
 END IF;
 IF TG_TABLE_NAME = 'market_transactions' THEN
  -- The Futmondo System/Market has no team ID. Empty IDs have this same meaning.
  NEW.buyer_team_id := NULLIF(btrim(NEW.buyer_team_id),'');
  NEW.seller_team_id := NULLIF(btrim(NEW.seller_team_id),'');
  ids := ARRAY[NEW.buyer_team_id,NEW.seller_team_id];
 ELSIF TG_TABLE_NAME = 'player_daily_snapshots' THEN
  NEW.owner_team_id := NULLIF(btrim(NEW.owner_team_id),'');
  ids := ARRAY[NEW.owner_team_id];
 ELSE
  RAISE EXCEPTION 'Unsupported league ownership guard' USING ERRCODE='23514';
 END IF;
 FOREACH tid IN ARRAY ids LOOP
  IF tid IS NULL THEN CONTINUE; END IF;
  SELECT championship_id INTO owner_championship FROM public.user_teams WHERE id=tid;
  IF owner_championship IS NULL OR owner_championship IS DISTINCT FROM NEW.championship_id THEN
   RAISE EXCEPTION 'Counterparty does not belong to observation championship' USING ERRCODE='23514';
  END IF;
 END LOOP;
 RETURN NEW;
END;
$$;
REVOKE ALL ON FUNCTION public.guard_team_linked_league() FROM PUBLIC, anon, authenticated;
GRANT EXECUTE ON FUNCTION public.guard_team_linked_league() TO service_role;
DROP TRIGGER IF EXISTS guard_market_transaction_league ON public.market_transactions;
CREATE TRIGGER guard_market_transaction_league BEFORE INSERT OR UPDATE ON public.market_transactions
 FOR EACH ROW EXECUTE FUNCTION public.guard_team_linked_league();
DROP TRIGGER IF EXISTS guard_daily_snapshot_league ON public.player_daily_snapshots;
CREATE TRIGGER guard_daily_snapshot_league BEFORE INSERT OR UPDATE ON public.player_daily_snapshots
 FOR EACH ROW EXECUTE FUNCTION public.guard_team_linked_league();

CREATE OR REPLACE FUNCTION public.keep_user_team_league_identity()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
BEGIN
 IF OLD.championship_id IS NOT NULL AND NEW.championship_id IS DISTINCT FROM OLD.championship_id THEN
  RAISE EXCEPTION 'An existing source team cannot change championship ownership' USING ERRCODE='23514';
 END IF;
 RETURN NEW;
END;
$$;
REVOKE ALL ON FUNCTION public.keep_user_team_league_identity() FROM PUBLIC, anon, authenticated;
GRANT EXECUTE ON FUNCTION public.keep_user_team_league_identity() TO service_role;
DROP TRIGGER IF EXISTS keep_user_team_league_identity ON public.user_teams;
CREATE TRIGGER keep_user_team_league_identity BEFORE UPDATE ON public.user_teams
 FOR EACH ROW EXECUTE FUNCTION public.keep_user_team_league_identity();

NOTIFY pgrst, 'reload schema';
COMMIT;


-- Durable, server-only background acquisition. Additive; no existing facts are rewritten.
BEGIN;
CREATE TABLE IF NOT EXISTS public.worker_accounts (
 account_id text PRIMARY KEY, enabled boolean NOT NULL DEFAULT false,
 generation bigint NOT NULL DEFAULT 1, encrypted_token text,credential_id text,
 timezone text NOT NULL DEFAULT 'UTC', quiet_start integer NOT NULL DEFAULT 0 CHECK(quiet_start BETWEEN 0 AND 23),
 quiet_end integer NOT NULL DEFAULT 7 CHECK(quiet_end BETWEEN 0 AND 23), excluded_leagues jsonb NOT NULL DEFAULT '[]',
 verified_leagues jsonb NOT NULL DEFAULT '{}',last_dispatched timestamptz,rate_limited_until timestamptz,
 auth_status text NOT NULL DEFAULT 'active' CHECK(auth_status IN ('active','reauthorize','configuration')),
 updated_at timestamptz NOT NULL DEFAULT now(), CHECK(NOT enabled OR (encrypted_token IS NOT NULL AND credential_id IS NOT NULL)),
 CHECK(jsonb_typeof(excluded_leagues)='array'), CHECK(octet_length(encrypted_token)<=16384)
);
CREATE TABLE IF NOT EXISTS public.worker_jobs (
 account_id text NOT NULL REFERENCES public.worker_accounts(account_id), resource_key text NOT NULL CHECK(resource_key ~ '^[a-f0-9]{64}$'),
 descriptor jsonb NOT NULL, generation bigint NOT NULL, interval_seconds integer NOT NULL DEFAULT 60 CHECK(interval_seconds BETWEEN 15 AND 86400),
 base_priority integer NOT NULL DEFAULT 0 CHECK(base_priority BETWEEN 0 AND 4),
 requested_at timestamptz NOT NULL DEFAULT now(),started_at timestamptz,
 priority integer NOT NULL DEFAULT 0 CHECK(priority BETWEEN 0 AND 10), due_at timestamptz NOT NULL DEFAULT now(),
 owner text, lease_until timestamptz, run_id uuid NOT NULL DEFAULT gen_random_uuid(), attempts integer NOT NULL DEFAULT 0,
 last_error text, checked_at timestamptz,last_audit_at timestamptz, checkpoint jsonb NOT NULL DEFAULT '{}',
 PRIMARY KEY(account_id,resource_key), CHECK(octet_length(descriptor::text)<=262144),CHECK(octet_length(checkpoint::text)<=1048576)
);
CREATE INDEX IF NOT EXISTS worker_jobs_due ON public.worker_jobs(due_at,priority DESC) WHERE owner IS NULL;
CREATE TABLE IF NOT EXISTS public.worker_snapshots (
 account_id text NOT NULL REFERENCES public.worker_accounts(account_id), resource_key text NOT NULL,
 context jsonb NOT NULL, handler text NOT NULL, version uuid NOT NULL,
 observed_at timestamptz NOT NULL, checked_at timestamptz NOT NULL, valid_until timestamptz NOT NULL,
 completeness text NOT NULL CHECK(completeness IN ('complete','partial','unknown')), status text NOT NULL,
 json_bytes integer NOT NULL CHECK(json_bytes BETWEEN 1 AND 16777216), sha256 text NOT NULL CHECK(sha256 ~ '^[a-f0-9]{64}$'),
 parts integer NOT NULL CHECK(parts BETWEEN 1 AND 128), PRIMARY KEY(account_id,resource_key)
);
CREATE TABLE IF NOT EXISTS public.worker_chunks (
 account_id text NOT NULL,resource_key text NOT NULL,version uuid NOT NULL,part integer NOT NULL CHECK(part BETWEEN 0 AND 127),
 data text NOT NULL CHECK(octet_length(data)<=131072),created_at timestamptz NOT NULL DEFAULT now(),
 PRIMARY KEY(account_id,resource_key,version,part)
);
CREATE INDEX IF NOT EXISTS worker_chunks_created ON public.worker_chunks(created_at);
CREATE TABLE IF NOT EXISTS public.worker_health (
 id text PRIMARY KEY, heartbeat timestamptz NOT NULL DEFAULT now(), status text NOT NULL,
 requests bigint NOT NULL DEFAULT 0, jobs bigint NOT NULL DEFAULT 0
);
CREATE TABLE IF NOT EXISTS public.worker_leader (
 id integer PRIMARY KEY CHECK(id=1),owner text NOT NULL,lease_until timestamptz NOT NULL
);
ALTER TABLE public.worker_leader ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.worker_leader FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.worker_leader TO service_role;
CREATE OR REPLACE FUNCTION public.worker_elect(p_owner text) RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 INSERT INTO worker_leader(id,owner,lease_until) VALUES(1,p_owner,now()+interval '60 seconds')
 ON CONFLICT(id) DO UPDATE SET owner=excluded.owner,lease_until=excluded.lease_until
 WHERE worker_leader.owner=p_owner OR worker_leader.lease_until<now();
 RETURN FOUND;
END $$;
CREATE OR REPLACE FUNCTION public.worker_release(p_owner text) RETURNS void LANGUAGE sql SECURITY INVOKER SET search_path=public,pg_temp AS $$
 DELETE FROM worker_leader WHERE owner=p_owner;
$$;
REVOKE ALL ON FUNCTION public.worker_elect(text),public.worker_release(text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_elect(text),public.worker_release(text) TO service_role;

CREATE OR REPLACE FUNCTION public.worker_authorize(p_account text,p_enabled boolean,p_token text DEFAULT NULL,
 p_timezone text DEFAULT 'UTC',p_start integer DEFAULT 0,p_end integer DEFAULT 7,p_excluded jsonb DEFAULT '[]',p_credential text DEFAULT NULL)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE result worker_accounts;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM pg_timezone_names WHERE name=p_timezone) THEN RAISE EXCEPTION 'Invalid timezone'; END IF;
 INSERT INTO worker_accounts(account_id,enabled,encrypted_token,timezone,quiet_start,quiet_end,excluded_leagues,credential_id)
 VALUES(p_account,p_enabled,CASE WHEN p_enabled THEN p_token ELSE NULL END,p_timezone,p_start,p_end,p_excluded,CASE WHEN p_enabled THEN p_credential ELSE NULL END)
 ON CONFLICT(account_id) DO UPDATE SET enabled=excluded.enabled,encrypted_token=excluded.encrypted_token,credential_id=excluded.credential_id,
 generation=worker_accounts.generation+1,timezone=excluded.timezone,quiet_start=excluded.quiet_start,quiet_end=excluded.quiet_end,
 excluded_leagues=excluded.excluded_leagues,auth_status='active',updated_at=now() RETURNING * INTO result;
 IF p_enabled THEN
 UPDATE worker_jobs SET generation=result.generation,owner=NULL,lease_until=NULL,checkpoint='{}',run_id=gen_random_uuid(),attempts=0,last_error=NULL WHERE account_id=p_account;
 ELSE DELETE FROM worker_jobs WHERE account_id=p_account; END IF;
 RETURN to_jsonb(result)-'encrypted_token'-'last_dispatched'-'verified_leagues';
END $$;
CREATE OR REPLACE FUNCTION public.worker_enqueue(p_account text,p_descriptor jsonb,p_interval integer DEFAULT 60,p_priority integer DEFAULT 10)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;k text:=p_descriptor->>'resource_key';
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' THEN RETURN false; END IF;
 IF p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account OR (p_descriptor->>'handler') !~ '^(get_|read_|fm_worker_finances$)' THEN RAISE EXCEPTION 'Invalid descriptor'; END IF;
 IF a.excluded_leagues ? (p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF NOT EXISTS(SELECT 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=k) AND
 (SELECT count(*) FROM worker_jobs WHERE account_id=p_account)>=10000 THEN RAISE EXCEPTION 'Worker job limit'; END IF;
 INSERT INTO worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds,priority,base_priority)
 VALUES(p_account,k,p_descriptor,a.generation,p_interval,greatest(p_priority,CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END),CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET
 due_at=CASE WHEN worker_jobs.owner IS NULL AND excluded.priority>=5 THEN least(worker_jobs.due_at,now()) ELSE worker_jobs.due_at END,
 requested_at=CASE WHEN excluded.priority>=5 THEN now() ELSE worker_jobs.requested_at END,priority=greatest(worker_jobs.priority,excluded.priority),base_priority=greatest(worker_jobs.base_priority,excluded.base_priority),interval_seconds=least(worker_jobs.interval_seconds,excluded.interval_seconds);
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND (q.owner IS NULL OR q.lease_until<now()) AND (NOT p_foreground OR q.priority>=5)
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND (q.priority>=5 OR q.checked_at IS NULL OR q.checked_at +
 make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE q.interval_seconds END)<=now())
 ORDER BY q.priority DESC,a.last_dispatched NULLS FIRST,q.due_at,q.account_id,q.resource_key FOR UPDATE OF q SKIP LOCKED LIMIT 1;
 IF j.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
CREATE OR REPLACE FUNCTION public.worker_guard(p_account text,p_key text,p_owner text,p_generation bigint,p_checkpoint jsonb DEFAULT NULL)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE worker_jobs j SET lease_until=now()+interval '60 seconds',checkpoint=coalesce(p_checkpoint,j.checkpoint)
 FROM worker_accounts a WHERE j.account_id=p_account AND j.resource_key=p_key AND j.owner=p_owner
 AND j.lease_until>now() AND a.account_id=j.account_id AND a.enabled AND a.auth_status='active'
 AND a.generation=p_generation AND j.generation=p_generation
 AND EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now());
 RETURN FOUND;
END $$;
CREATE OR REPLACE FUNCTION public.worker_publish(p_account text,p_key text,p_owner text,p_generation bigint,p_manifest jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;a worker_accounts;
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key FOR UPDATE;
 IF a.account_id IS NULL OR j.account_id IS NULL OR NOT a.enabled OR a.generation<>p_generation OR a.auth_status<>'active' OR j.owner IS NULL OR j.lease_until IS NULL OR j.owner IS DISTINCT FROM p_owner OR j.lease_until<=now() OR NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN false; END IF;
 IF (SELECT count(*) FROM worker_chunks WHERE account_id=p_account AND resource_key=p_key AND version=(p_manifest->>'version')::uuid)<>(p_manifest->>'parts')::integer THEN RAISE EXCEPTION 'Incomplete snapshot'; END IF;
 IF j.descriptor->>'handler'='get_active_championships' AND jsonb_typeof(p_manifest->'memberships')='object' THEN
 UPDATE worker_accounts SET verified_leagues=p_manifest->'memberships' WHERE account_id=p_account;
 DELETE FROM worker_jobs q WHERE q.account_id=p_account AND q.resource_key<>p_key AND coalesce(q.descriptor->'context'->>'championship_id','')<>'' AND
 (NOT (p_manifest->'memberships' ? (q.descriptor->'context'->>'championship_id')) OR EXISTS(
 SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) AS dim WHERE
 coalesce(q.descriptor->'context'->>dim,'unknown')<>'unknown' AND
 coalesce(p_manifest->'memberships'->(q.descriptor->'context'->>'championship_id')->>dim,'unknown')<>'unknown' AND
 q.descriptor->'context'->>dim IS DISTINCT FROM p_manifest->'memberships'->(q.descriptor->'context'->>'championship_id')->>dim)); END IF;
 IF p_manifest->'context' IS DISTINCT FROM j.descriptor->'context' THEN RAISE EXCEPTION 'Foreign context'; END IF;
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts)
 VALUES(p_account,p_key,p_manifest->'context',j.descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,CASE WHEN j.requested_at>j.started_at THEN least((p_manifest->>'valid_until')::timestamptz,now()-interval '1 second') ELSE (p_manifest->>'valid_until')::timestamptz END,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts
 WHERE worker_snapshots.checked_at<=excluded.checked_at;
 UPDATE worker_jobs SET owner=NULL,lease_until=NULL,checked_at=now(),due_at=CASE WHEN requested_at>started_at THEN now() ELSE now()+make_interval(secs=>interval_seconds) END,
 priority=CASE WHEN requested_at>started_at THEN 10 ELSE base_priority END,attempts=0,last_error=p_manifest->>'error',last_audit_at=CASE WHEN p_manifest->>'full_audit'='true' THEN now() ELSE last_audit_at END,checkpoint='{}',run_id=gen_random_uuid() WHERE account_id=p_account AND resource_key=p_key;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.worker_read(p_account text,p_versions jsonb DEFAULT '{}')
RETURNS jsonb LANGUAGE sql SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT jsonb_build_object(
 'account',(SELECT to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues' FROM worker_accounts a WHERE account_id=p_account),
 'health',(SELECT to_jsonb(h) FROM worker_health h WHERE status='running' ORDER BY heartbeat DESC LIMIT 1),
 'resources',coalesce((SELECT jsonb_agg(jsonb_build_object('key',s.resource_key,'manifest',to_jsonb(s),
 'chunks',(SELECT jsonb_agg(c.data ORDER BY c.part) FROM worker_chunks c WHERE c.account_id=s.account_id AND c.resource_key=s.resource_key AND c.version=s.version),
 'error',j.last_error)) FROM worker_snapshots s LEFT JOIN worker_jobs j USING(account_id,resource_key)
 WHERE s.account_id=p_account AND p_versions ? s.resource_key AND s.version::text IS DISTINCT FROM p_versions->>s.resource_key),'[]'),
 'jobs',coalesce((SELECT jsonb_agg(jsonb_build_object('key',resource_key,'error',last_error,'running',owner IS NOT NULL,'pending',owner IS NOT NULL OR due_at<=now()))
 FROM worker_jobs WHERE account_id=p_account AND p_versions ? resource_key),'[]'));
$$;
-- Server-only objects and invoker functions. No browser token grants access.
DO $$ DECLARE t text;f record; BEGIN
 FOREACH t IN ARRAY ARRAY['worker_accounts','worker_jobs','worker_snapshots','worker_chunks','worker_health'] LOOP
 EXECUTE format('ALTER TABLE public.%I ENABLE ROW LEVEL SECURITY',t);
 EXECUTE format('REVOKE ALL ON public.%I FROM PUBLIC,anon,authenticated',t);
 EXECUTE format('GRANT SELECT,INSERT,UPDATE,DELETE ON public.%I TO service_role',t);
 END LOOP;
 FOR f IN SELECT oid::regprocedure AS signature FROM pg_proc WHERE pronamespace='public'::regnamespace AND proname IN
 ('worker_authorize','worker_enqueue','worker_claim','worker_guard','worker_publish','worker_read') LOOP
 EXECUTE format('REVOKE ALL ON FUNCTION %s FROM PUBLIC,anon,authenticated',f.signature);
 EXECUTE format('GRANT EXECUTE ON FUNCTION %s TO service_role',f.signature);
 END LOOP;
END $$;

CREATE OR REPLACE FUNCTION public.worker_cleanup() RETURNS void LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 DELETE FROM worker_chunks c WHERE created_at<now()-interval '1 day' AND NOT EXISTS(
 SELECT 1 FROM worker_snapshots s WHERE s.account_id=c.account_id AND s.resource_key=c.resource_key AND s.version=c.version);
 DELETE FROM worker_health WHERE heartbeat<now()-interval '1 day';
END $$;
REVOKE ALL ON FUNCTION public.worker_cleanup() FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_cleanup() TO service_role;
ALTER TABLE public.worker_accounts ADD COLUMN IF NOT EXISTS verified_leagues jsonb NOT NULL DEFAULT '{}';
ALTER TABLE public.worker_accounts ADD COLUMN IF NOT EXISTS last_dispatched timestamptz;
CREATE TABLE IF NOT EXISTS public.worker_checkpoints (
 account_id text NOT NULL,resource_key text NOT NULL,run_id uuid NOT NULL,request_key text NOT NULL,
 response jsonb NOT NULL CHECK(octet_length(response::text)<=4194304),bytes integer GENERATED ALWAYS AS (octet_length(response::text)) STORED,created_at timestamptz NOT NULL DEFAULT now(),
 PRIMARY KEY(account_id,resource_key,run_id,request_key),
 FOREIGN KEY(account_id,resource_key) REFERENCES public.worker_jobs(account_id,resource_key) ON DELETE CASCADE
);
ALTER TABLE public.worker_checkpoints ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.worker_checkpoints FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.worker_checkpoints TO service_role;
CREATE OR REPLACE FUNCTION public.worker_checkpoint(p_account text,p_key text,p_owner text,p_generation bigint,p_request text,p_response jsonb DEFAULT NULL)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;r jsonb;
BEGIN
 PERFORM 1 FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT worker_guard(p_account,p_key,p_owner,p_generation) THEN RAISE EXCEPTION 'Worker lease lost'; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key;
 IF p_response IS NULL THEN
 SELECT response INTO r FROM worker_checkpoints WHERE account_id=p_account AND resource_key=p_key AND run_id=j.run_id AND request_key=p_request;
 ELSE
 IF (SELECT coalesce(sum(bytes),0) FROM worker_checkpoints WHERE account_id=p_account)+octet_length(p_response::text)>16777216 THEN RETURN NULL; END IF;
 INSERT INTO worker_checkpoints(account_id,resource_key,run_id,request_key,response) VALUES(p_account,p_key,j.run_id,p_request,p_response)
 ON CONFLICT DO NOTHING;
 r:=p_response;
 END IF;
 RETURN r;
END $$;
REVOKE ALL ON FUNCTION public.worker_checkpoint(text,text,text,bigint,text,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_checkpoint(text,text,text,bigint,text,jsonb) TO service_role;
CREATE OR REPLACE FUNCTION public.worker_cleanup() RETURNS void LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 DELETE FROM worker_chunks c WHERE created_at<now()-interval '1 day' AND NOT EXISTS(
 SELECT 1 FROM worker_snapshots s WHERE s.account_id=c.account_id AND s.resource_key=c.resource_key AND s.version=c.version);
 DELETE FROM worker_checkpoints c WHERE created_at<now()-interval '1 day' OR NOT EXISTS(
 SELECT 1 FROM worker_jobs j WHERE j.account_id=c.account_id AND j.resource_key=c.resource_key AND j.run_id=c.run_id);
 DELETE FROM worker_health WHERE heartbeat<now()-interval '1 day';
END $$;
CREATE TABLE IF NOT EXISTS public.worker_fact_heads (
 table_name text NOT NULL,entity_id text NOT NULL,observed_at timestamptz NOT NULL,PRIMARY KEY(table_name,entity_id)
);
ALTER TABLE public.worker_fact_heads ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.worker_fact_heads FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE ON public.worker_fact_heads TO service_role;
CREATE OR REPLACE FUNCTION public.worker_write_fact(p_account text,p_key text,p_owner text,p_generation bigint,
 p_table text,p_payload jsonb,p_conflict text,p_ignore boolean,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;cols text;conflicts text;updates text;clause text;field text;head text;j worker_jobs;
BEGIN
 PERFORM 1 FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT worker_guard(p_account,p_key,p_owner,p_generation) THEN RETURN false; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key;
 IF p_table<>ALL(ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history',
 'market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations',
 'market_observations','player_match_observations','league_rule_snapshots','player_round_history_checks',
 'rival_finance_evidence','complete_history_baselines','league_context_observations','player_membership_observations']) THEN RAISE EXCEPTION 'Invalid fact table'; END IF;
 -- Match PostgREST's default merge identity for mutable source catalogs.
 IF p_conflict IS NULL AND p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
  p_conflict:='id';
 END IF;
 IF p_observed IS NULL OR p_observed>now()+interval '60 seconds' THEN RAISE EXCEPTION 'Invalid source time'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
  IF jsonb_typeof(row_data)<>'object' OR octet_length(row_data::text)>1048576 THEN RAISE EXCEPTION 'Invalid fact row'; END IF;
  IF row_data ? 'account_id' AND row_data->>'account_id' IS DISTINCT FROM p_account THEN RAISE EXCEPTION 'Foreign fact account'; END IF;
  IF row_data ? 'championship_id' AND row_data->>'championship_id' IS DISTINCT FROM j.descriptor->'context'->>'championship_id'
    AND j.descriptor->>'handler'<>'get_active_championships' THEN RAISE EXCEPTION 'Foreign fact league'; END IF;
  FOR field IN SELECT jsonb_object_keys(row_data) LOOP
   IF field ~* '(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)' OR NOT EXISTS(
    SELECT 1 FROM pg_attribute WHERE attrelid=('public.'||p_table)::regclass AND attname=field AND attnum>0 AND NOT attisdropped)
    THEN RAISE EXCEPTION 'Invalid fact field'; END IF;
  END LOOP;
  IF p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
   head:=NULL;
   IF coalesce(row_data->>'id','')='' THEN RAISE EXCEPTION 'Missing fact identity'; END IF;
   INSERT INTO worker_fact_heads(table_name,entity_id,observed_at) VALUES(p_table,row_data->>'id',p_observed)
   ON CONFLICT(table_name,entity_id) DO UPDATE SET observed_at=excluded.observed_at
   WHERE worker_fact_heads.observed_at<=excluded.observed_at RETURNING entity_id INTO head;
   IF head IS NULL THEN CONTINUE; END IF;
  END IF;
  SELECT string_agg(format('%I',key),',' ORDER BY key),string_agg(format('%I=excluded.%I',key,key),',' ORDER BY key)
  INTO cols,updates FROM jsonb_object_keys(row_data) AS key;
  clause:='';
  IF p_conflict IS NOT NULL THEN
   IF p_conflict !~ '^[a-z_]+(,[a-z_]+)*$' THEN RAISE EXCEPTION 'Invalid fact conflict'; END IF;
   SELECT string_agg(format('%I',v),',') INTO conflicts FROM unnest(string_to_array(p_conflict,',')) AS v;
   clause:=format(' ON CONFLICT (%s) DO %s',conflicts,CASE WHEN p_ignore THEN 'NOTHING' ELSE 'UPDATE SET '||updates END);
  END IF;
  EXECUTE format('INSERT INTO public.%I (%s) SELECT %s FROM jsonb_populate_record(NULL::public.%I,$1)%s',p_table,cols,cols,p_table,clause) USING row_data;
 END LOOP;
 RETURN true;
END $$;
REVOKE ALL ON FUNCTION public.worker_write_fact(text,text,text,bigint,text,jsonb,text,boolean,timestamptz) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_write_fact(text,text,text,bigint,text,jsonb,text,boolean,timestamptz) TO service_role;
CREATE OR REPLACE FUNCTION public.worker_publish_child(p_account text,p_parent text,p_owner text,p_generation bigint,p_descriptor jsonb,p_manifest jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;k text:=p_descriptor->>'resource_key';
BEGIN
 PERFORM 1 FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT worker_guard(p_account,p_parent,p_owner,p_generation) THEN RETURN false; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_parent;
 IF k !~ '^[a-f0-9]{64}$' OR p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account
 OR p_manifest->'context' IS DISTINCT FROM p_descriptor->'context'
 OR (coalesce(p_descriptor->'context'->>'championship_id','')<>'' AND
 p_descriptor->'context'->>'championship_id' IS DISTINCT FROM j.descriptor->'context'->>'championship_id')
 THEN RAISE EXCEPTION 'Foreign child resource'; END IF;
 IF (SELECT count(*) FROM worker_chunks WHERE account_id=p_account AND resource_key=k AND version=(p_manifest->>'version')::uuid)<>(p_manifest->>'parts')::integer THEN RAISE EXCEPTION 'Incomplete child snapshot'; END IF;
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts)
 VALUES(p_account,k,p_descriptor->'context',p_descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,CASE WHEN j.requested_at>j.started_at THEN least((p_manifest->>'valid_until')::timestamptz,now()-interval '1 second') ELSE (p_manifest->>'valid_until')::timestamptz END,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts
 WHERE worker_snapshots.checked_at<=excluded.checked_at;
 RETURN true;
END $$;
REVOKE ALL ON FUNCTION public.worker_publish_child(text,text,text,bigint,jsonb,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_publish_child(text,text,text,bigint,jsonb,jsonb) TO service_role;

CREATE OR REPLACE FUNCTION public.worker_renew(p_account text,p_token text,p_credential text DEFAULT NULL)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;
BEGIN
 UPDATE worker_accounts SET encrypted_token=p_token,credential_id=p_credential,generation=generation+1,auth_status='active',updated_at=now()
 WHERE account_id=p_account AND enabled RETURNING * INTO a;
 IF a.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_jobs SET generation=a.generation,owner=NULL,lease_until=NULL,checkpoint='{}',run_id=gen_random_uuid(),attempts=0,last_error=NULL WHERE account_id=p_account;
 RETURN to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues';
END $$;
REVOKE ALL ON FUNCTION public.worker_renew(text,text,text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_renew(text,text,text) TO service_role;

COMMIT;

-- Football-only application scope and durable last selected league per account.
BEGIN;
CREATE TABLE IF NOT EXISTS public.blocked_leagues (
 championship_id text PRIMARY KEY,
 sport text NOT NULL CHECK(sport IN ('basket','basketball','baloncesto')),
 blocked_at timestamptz NOT NULL DEFAULT now()
);
ALTER TABLE public.blocked_leagues ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.blocked_leagues FROM PUBLIC,anon,authenticated,service_role;
GRANT SELECT,INSERT ON public.blocked_leagues TO service_role;
INSERT INTO public.blocked_leagues(championship_id,sport)
 SELECT id,lower(btrim(sport)) FROM public.championships
 WHERE lower(btrim(coalesce(sport,''))) IN ('basket','basketball','baloncesto')
 ON CONFLICT DO NOTHING;

CREATE TABLE IF NOT EXISTS public.user_league_preferences (
 user_id text PRIMARY KEY CHECK(length(user_id) BETWEEN 1 AND 256 AND user_id ~ '^[A-Za-z0-9_-]+$'),
 championship_id text NOT NULL REFERENCES public.championships(id) ON DELETE CASCADE,
 selected_at timestamptz NOT NULL CHECK(isfinite(selected_at)),
 recorded_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
ALTER TABLE public.user_league_preferences ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.user_league_preferences FROM PUBLIC,anon,authenticated,service_role;
GRANT SELECT,INSERT,UPDATE ON public.user_league_preferences TO service_role;
CREATE OR REPLACE FUNCTION public.keep_latest_user_league()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path='' AS $$
BEGIN
 IF NEW.selected_at < OLD.selected_at THEN RETURN OLD; END IF;
 NEW.recorded_at:=clock_timestamp();RETURN NEW;
END $$;
REVOKE ALL ON FUNCTION public.keep_latest_user_league() FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.keep_latest_user_league() TO service_role;
DROP TRIGGER IF EXISTS keep_latest_user_league ON public.user_league_preferences;
CREATE TRIGGER keep_latest_user_league BEFORE UPDATE ON public.user_league_preferences
 FOR EACH ROW EXECUTE FUNCTION public.keep_latest_user_league();

-- A minimal ID/sport tombstone prevents old sessions recreating removed data.
CREATE OR REPLACE FUNCTION public.block_basketball_observation()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path='' AS $$
BEGIN
 IF TG_TABLE_NAME='championships' THEN
  IF lower(btrim(coalesce(NEW.sport,''))) IN ('basket','basketball','baloncesto') THEN
   INSERT INTO public.blocked_leagues(championship_id,sport) VALUES(NEW.id,lower(btrim(NEW.sport))) ON CONFLICT DO NOTHING;
   RETURN NULL;
  END IF;
  IF EXISTS(SELECT 1 FROM public.blocked_leagues WHERE championship_id=NEW.id) THEN RETURN NULL; END IF;
 ELSE
  IF EXISTS(SELECT 1 FROM public.blocked_leagues WHERE championship_id=NEW.championship_id) THEN
   IF TG_TABLE_NAME='user_league_preferences' THEN RAISE EXCEPTION 'Unsupported league preference' USING ERRCODE='23514'; END IF;
   RETURN NULL;
  END IF;
 END IF;
 RETURN NEW;
END $$;
REVOKE ALL ON FUNCTION public.block_basketball_observation() FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.block_basketball_observation() TO service_role;
DROP TRIGGER IF EXISTS block_basketball_observation ON public.championships;
CREATE TRIGGER block_basketball_observation BEFORE INSERT OR UPDATE ON public.championships
 FOR EACH ROW EXECUTE FUNCTION public.block_basketball_observation();
DO $$ DECLARE tbl text;
BEGIN
 FOR tbl IN SELECT t.table_name FROM information_schema.tables t JOIN information_schema.columns c
  ON c.table_schema=t.table_schema AND c.table_name=t.table_name AND c.column_name='championship_id'
  WHERE t.table_schema='public' AND t.table_type='BASE TABLE' AND t.table_name<>'blocked_leagues' LOOP
  EXECUTE format('DROP TRIGGER IF EXISTS block_basketball_observation ON public.%I',tbl);
  EXECUTE format('CREATE TRIGGER block_basketball_observation BEFORE INSERT OR UPDATE ON public.%I FOR EACH ROW EXECUTE FUNCTION public.block_basketball_observation()',tbl);
 END LOOP;
END $$;

-- Use existing consent-aware queue admission and lease guards; no new worker.
CREATE OR REPLACE FUNCTION public.worker_enqueue(p_account text,p_descriptor jsonb,p_interval integer DEFAULT 60,p_priority integer DEFAULT 10)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;k text:=p_descriptor->>'resource_key';
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' THEN RETURN false; END IF;
 IF EXISTS(SELECT 1 FROM public.blocked_leagues WHERE championship_id=p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account OR (p_descriptor->>'handler') !~ '^(get_|read_|fm_worker_finances$)' THEN RAISE EXCEPTION 'Invalid descriptor'; END IF;
 IF a.excluded_leagues ? (p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF NOT EXISTS(SELECT 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=k) AND
 (SELECT count(*) FROM worker_jobs WHERE account_id=p_account)>=10000 THEN RAISE EXCEPTION 'Worker job limit'; END IF;
 INSERT INTO worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds,priority,base_priority)
 VALUES(p_account,k,p_descriptor,a.generation,p_interval,greatest(p_priority,CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END),CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET
 due_at=CASE WHEN worker_jobs.owner IS NULL AND excluded.priority>=5 THEN least(worker_jobs.due_at,now()) ELSE worker_jobs.due_at END,
 requested_at=CASE WHEN excluded.priority>=5 THEN now() ELSE worker_jobs.requested_at END,priority=greatest(worker_jobs.priority,excluded.priority),base_priority=greatest(worker_jobs.base_priority,excluded.base_priority),interval_seconds=least(worker_jobs.interval_seconds,excluded.interval_seconds);
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.worker_guard(p_account text,p_key text,p_owner text,p_generation bigint,p_checkpoint jsonb DEFAULT NULL)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE worker_jobs j SET lease_until=now()+interval '60 seconds',checkpoint=coalesce(p_checkpoint,j.checkpoint)
 FROM worker_accounts a WHERE j.account_id=p_account AND j.resource_key=p_key AND j.owner=p_owner
 AND j.lease_until>now() AND a.account_id=j.account_id AND a.enabled AND a.auth_status='active'
 AND a.generation=p_generation AND j.generation=p_generation
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=j.descriptor->'context'->>'championship_id')
 AND EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now());
 RETURN FOUND;
END $$;

REVOKE ALL ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer),public.worker_guard(text,text,text,bigint,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer),public.worker_guard(text,text,text,bigint,jsonb) TO service_role;

CREATE OR REPLACE FUNCTION public.worker_publish(p_account text,p_key text,p_owner text,p_generation bigint,p_manifest jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;a worker_accounts;
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key FOR UPDATE;
 IF a.account_id IS NULL OR j.account_id IS NULL OR NOT a.enabled OR a.generation<>p_generation OR a.auth_status<>'active' OR j.owner IS NULL OR j.lease_until IS NULL OR j.owner IS DISTINCT FROM p_owner OR j.lease_until<=now() OR NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN false; END IF;
 IF (SELECT count(*) FROM worker_chunks WHERE account_id=p_account AND resource_key=p_key AND version=(p_manifest->>'version')::uuid)<>(p_manifest->>'parts')::integer THEN RAISE EXCEPTION 'Incomplete snapshot'; END IF;
 IF j.descriptor->>'handler'='get_active_championships' AND jsonb_typeof(p_manifest->'memberships')='object' THEN
 UPDATE worker_accounts SET verified_leagues=coalesce((SELECT jsonb_object_agg(key,value) FROM jsonb_each(p_manifest->'memberships') m WHERE NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=m.key)),'{}'::jsonb) WHERE account_id=p_account;
 DELETE FROM worker_jobs q WHERE q.account_id=p_account AND q.resource_key<>p_key AND coalesce(q.descriptor->'context'->>'championship_id','')<>'' AND
 (NOT (p_manifest->'memberships' ? (q.descriptor->'context'->>'championship_id')) OR EXISTS(
 SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) AS dim WHERE
 coalesce(q.descriptor->'context'->>dim,'unknown')<>'unknown' AND
 coalesce(p_manifest->'memberships'->(q.descriptor->'context'->>'championship_id')->>dim,'unknown')<>'unknown' AND
 q.descriptor->'context'->>dim IS DISTINCT FROM p_manifest->'memberships'->(q.descriptor->'context'->>'championship_id')->>dim)); END IF;
 IF p_manifest->'context' IS DISTINCT FROM j.descriptor->'context' THEN RAISE EXCEPTION 'Foreign context'; END IF;
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts)
 VALUES(p_account,p_key,p_manifest->'context',j.descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,CASE WHEN j.requested_at>j.started_at THEN least((p_manifest->>'valid_until')::timestamptz,now()-interval '1 second') ELSE (p_manifest->>'valid_until')::timestamptz END,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts
 WHERE worker_snapshots.checked_at<=excluded.checked_at;
 UPDATE worker_jobs SET owner=NULL,lease_until=NULL,checked_at=now(),due_at=CASE WHEN requested_at>started_at THEN now() ELSE now()+make_interval(secs=>interval_seconds) END,
 priority=CASE WHEN requested_at>started_at THEN 10 ELSE base_priority END,attempts=0,last_error=p_manifest->>'error',last_audit_at=CASE WHEN p_manifest->>'full_audit'='true' THEN now() ELSE last_audit_at END,checkpoint='{}',run_id=gen_random_uuid() WHERE account_id=p_account AND resource_key=p_key;
 RETURN true;
END $$;
REVOKE ALL ON FUNCTION public.worker_publish(text,text,text,bigint,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_publish(text,text,text,bigint,jsonb) TO service_role;

CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND (q.owner IS NULL OR q.lease_until<now()) AND (NOT p_foreground OR q.priority>=5)
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
 AND (q.priority>=5 OR q.checked_at IS NULL OR q.checked_at +
 make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE q.interval_seconds END)<=now())
 ORDER BY q.priority DESC,a.last_dispatched NULLS FIRST,q.due_at,q.account_id,q.resource_key FOR UPDATE OF q SKIP LOCKED LIMIT 1;
 IF j.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
REVOKE ALL ON FUNCTION public.worker_claim(text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_claim(text,boolean) TO service_role;

-- Reversible blocking only. Stored league data remains until separately approved cleanup.
DO $$ DECLARE blocked text[];affected text[];tbl text;
BEGIN
 SELECT array_agg(championship_id) INTO blocked FROM public.blocked_leagues;
 IF blocked IS NULL THEN RETURN; END IF;
 SELECT array_agg(account_id) INTO affected FROM public.worker_accounts a
 WHERE EXISTS(SELECT 1 FROM unnest(blocked) id WHERE a.verified_leagues ? id)
 OR EXISTS(SELECT 1 FROM public.worker_jobs j WHERE j.account_id=a.account_id AND j.descriptor->'context'->>'championship_id'=ANY(blocked));
 UPDATE public.worker_accounts a SET verified_leagues=a.verified_leagues-blocked,
  excluded_leagues=(SELECT jsonb_agg(DISTINCT value) FROM jsonb_array_elements(a.excluded_leagues||to_jsonb(blocked)))
 WHERE a.account_id=ANY(affected);
END $$;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Active finance preparation must match verified current league context.
-- Retire unresolved work without deleting observations, snapshots or checkpoints.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_finance_context_matches(p_descriptor jsonb,p_membership jsonb)
RETURNS boolean LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
 SELECT CASE WHEN p_descriptor->>'handler'<>'fm_worker_finances' THEN true ELSE coalesce(
  jsonb_typeof(p_membership)='object'
  AND coalesce(p_membership->>'competition_id','unknown') NOT IN ('','unknown')
  AND coalesce(p_membership->>'season','unknown') NOT IN ('','unknown')
  AND p_descriptor->'context'->>'competition_id'=p_membership->>'competition_id'
  AND p_descriptor->'context'->>'season'=p_membership->>'season'
  AND coalesce(p_descriptor->'context'->>'user_team_id','')<>''
  AND (coalesce(p_membership->>'scoring_version','unknown') IN ('','unknown')
   OR p_descriptor->'context'->>'scoring_version'=p_membership->>'scoring_version'),false) END;
$$;
REVOKE ALL ON FUNCTION public.worker_finance_context_matches(jsonb,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_finance_context_matches(jsonb,jsonb) TO service_role;
CREATE OR REPLACE FUNCTION public.worker_enqueue(p_account text,p_descriptor jsonb,p_interval integer DEFAULT 60,p_priority integer DEFAULT 10)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;k text:=p_descriptor->>'resource_key';
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' THEN RETURN false; END IF;
 IF NOT public.worker_finance_context_matches(p_descriptor,a.verified_leagues->(p_descriptor->'context'->>'championship_id')) THEN RETURN false; END IF;
 IF EXISTS(SELECT 1 FROM public.blocked_leagues WHERE championship_id=p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account OR (p_descriptor->>'handler') !~ '^(get_|read_|fm_worker_finances$)' THEN RAISE EXCEPTION 'Invalid descriptor'; END IF;
 IF a.excluded_leagues ? (p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF NOT EXISTS(SELECT 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=k) AND
 (SELECT count(*) FROM worker_jobs WHERE account_id=p_account)>=10000 THEN RAISE EXCEPTION 'Worker job limit'; END IF;
 INSERT INTO worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds,priority,base_priority)
 VALUES(p_account,k,p_descriptor,a.generation,p_interval,greatest(p_priority,CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END),CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET
 due_at=CASE WHEN worker_jobs.owner IS NULL AND excluded.priority>=5 THEN least(worker_jobs.due_at,now()) ELSE worker_jobs.due_at END,
 requested_at=CASE WHEN excluded.priority>=5 THEN now() ELSE worker_jobs.requested_at END,priority=greatest(worker_jobs.priority,excluded.priority),base_priority=greatest(worker_jobs.base_priority,excluded.base_priority),interval_seconds=least(worker_jobs.interval_seconds,excluded.interval_seconds);
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND public.worker_finance_context_matches(q.descriptor,a.verified_leagues->(q.descriptor->'context'->>'championship_id'))
 AND (q.owner IS NULL OR q.lease_until<now()) AND (NOT p_foreground OR q.priority>=5)
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
 AND (q.priority>=5 OR q.checked_at IS NULL OR q.checked_at +
 make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE q.interval_seconds END)<=now())
 ORDER BY q.priority DESC,a.last_dispatched NULLS FIRST,q.due_at,q.account_id,q.resource_key FOR UPDATE OF q SKIP LOCKED LIMIT 1;
 IF j.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
CREATE OR REPLACE FUNCTION public.worker_guard(p_account text,p_key text,p_owner text,p_generation bigint,p_checkpoint jsonb DEFAULT NULL)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE worker_jobs j SET lease_until=now()+interval '60 seconds',checkpoint=coalesce(p_checkpoint,j.checkpoint)
 FROM worker_accounts a WHERE j.account_id=p_account AND j.resource_key=p_key AND j.owner=p_owner
 AND j.lease_until>now() AND a.account_id=j.account_id AND a.enabled AND a.auth_status='active'
 AND a.generation=p_generation AND j.generation=p_generation
 AND public.worker_finance_context_matches(j.descriptor,a.verified_leagues->(j.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=j.descriptor->'context'->>'championship_id')
 AND EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now());
 RETURN FOUND;
END $$;
REVOKE ALL ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer),public.worker_claim(text,boolean),public.worker_guard(text,text,text,bigint,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer),public.worker_claim(text,boolean),public.worker_guard(text,text,text,bigint,jsonb) TO service_role;
UPDATE public.worker_jobs j SET owner=NULL,lease_until=NULL,due_at='infinity'::timestamptz,last_error='context_unresolved'
 FROM public.worker_accounts a WHERE a.account_id=j.account_id AND j.descriptor->>'handler'='fm_worker_finances'
 AND NOT public.worker_finance_context_matches(j.descriptor,a.verified_leagues->(j.descriptor->'context'->>'championship_id'));
NOTIFY pgrst,'reload schema';
COMMIT;

-- Order progress within its leased run without renewing source freshness.
BEGIN;
ALTER TABLE public.worker_snapshots ADD COLUMN IF NOT EXISTS run_id uuid;
CREATE OR REPLACE FUNCTION public.worker_publish(p_account text,p_key text,p_owner text,p_generation bigint,p_manifest jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;a worker_accounts;
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key FOR UPDATE;
 IF a.account_id IS NULL OR j.account_id IS NULL OR NOT a.enabled OR a.generation<>p_generation OR a.auth_status<>'active' OR j.owner IS NULL OR j.lease_until IS NULL OR j.owner IS DISTINCT FROM p_owner OR j.lease_until<=now() OR NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN false; END IF;
 IF (SELECT count(*) FROM worker_chunks WHERE account_id=p_account AND resource_key=p_key AND version=(p_manifest->>'version')::uuid)<>(p_manifest->>'parts')::integer THEN RAISE EXCEPTION 'Incomplete snapshot'; END IF;
 IF j.descriptor->>'handler'='get_active_championships' AND jsonb_typeof(p_manifest->'memberships')='object' THEN
 UPDATE worker_accounts SET verified_leagues=coalesce((SELECT jsonb_object_agg(key,value) FROM jsonb_each(p_manifest->'memberships') m WHERE NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=m.key)),'{}'::jsonb) WHERE account_id=p_account;
 DELETE FROM worker_jobs q WHERE q.account_id=p_account AND q.resource_key<>p_key AND coalesce(q.descriptor->'context'->>'championship_id','')<>'' AND
 (NOT (p_manifest->'memberships' ? (q.descriptor->'context'->>'championship_id')) OR EXISTS(
 SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) AS dim WHERE
 coalesce(q.descriptor->'context'->>dim,'unknown')<>'unknown' AND
 coalesce(p_manifest->'memberships'->(q.descriptor->'context'->>'championship_id')->>dim,'unknown')<>'unknown' AND
 q.descriptor->'context'->>dim IS DISTINCT FROM p_manifest->'memberships'->(q.descriptor->'context'->>'championship_id')->>dim)); END IF;
 IF p_manifest->'context' IS DISTINCT FROM j.descriptor->'context' THEN RAISE EXCEPTION 'Foreign context'; END IF;
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id)
 VALUES(p_account,p_key,p_manifest->'context',j.descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,CASE WHEN j.requested_at>j.started_at THEN least((p_manifest->>'valid_until')::timestamptz,now()-interval '1 second') ELSE (p_manifest->>'valid_until')::timestamptz END,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer,j.run_id)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts,run_id=excluded.run_id
 WHERE worker_snapshots.checked_at<=excluded.checked_at OR worker_snapshots.run_id=j.run_id
 OR (j.descriptor->>'handler'='fm_worker_finances' AND worker_snapshots.resource_key=j.resource_key);
 UPDATE worker_jobs SET owner=NULL,lease_until=NULL,checked_at=now(),due_at=CASE WHEN requested_at>started_at THEN now() ELSE now()+make_interval(secs=>interval_seconds) END,
 priority=CASE WHEN requested_at>started_at THEN 10 ELSE base_priority END,attempts=0,last_error=p_manifest->>'error',last_audit_at=CASE WHEN p_manifest->>'full_audit'='true' THEN now() ELSE last_audit_at END,checkpoint='{}',run_id=gen_random_uuid() WHERE account_id=p_account AND resource_key=p_key;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.worker_publish_child(p_account text,p_parent text,p_owner text,p_generation bigint,p_descriptor jsonb,p_manifest jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;k text:=p_descriptor->>'resource_key';
BEGIN
 PERFORM 1 FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT worker_guard(p_account,p_parent,p_owner,p_generation) THEN RETURN false; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_parent;
 IF k !~ '^[a-f0-9]{64}$' OR p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account
 OR p_manifest->'context' IS DISTINCT FROM p_descriptor->'context'
 OR (coalesce(p_descriptor->'context'->>'championship_id','')<>'' AND
 p_descriptor->'context'->>'championship_id' IS DISTINCT FROM j.descriptor->'context'->>'championship_id')
 THEN RAISE EXCEPTION 'Foreign child resource'; END IF;
 IF (SELECT count(*) FROM worker_chunks WHERE account_id=p_account AND resource_key=k AND version=(p_manifest->>'version')::uuid)<>(p_manifest->>'parts')::integer THEN RAISE EXCEPTION 'Incomplete child snapshot'; END IF;
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id)
 VALUES(p_account,k,p_descriptor->'context',p_descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,CASE WHEN j.requested_at>j.started_at THEN least((p_manifest->>'valid_until')::timestamptz,now()-interval '1 second') ELSE (p_manifest->>'valid_until')::timestamptz END,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer,j.run_id)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts,run_id=excluded.run_id
 WHERE worker_snapshots.checked_at<=excluded.checked_at OR worker_snapshots.run_id=j.run_id
 OR (j.descriptor->>'handler'='fm_worker_finances' AND worker_snapshots.resource_key=j.resource_key);
 RETURN true;
END $$;
REVOKE ALL ON FUNCTION public.worker_publish(text,text,text,bigint,jsonb),public.worker_publish_child(text,text,text,bigint,jsonb,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_publish(text,text,text,bigint,jsonb),public.worker_publish_child(text,text,text,bigint,jsonb,jsonb) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Bound discovery queue round trips; no source facts or snapshots are rewritten.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_enqueue_batch(p_account text,p_owner text,p_generation bigint,p_descriptors jsonb)
RETURNS integer LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;item jsonb;d jsonb;seconds integer;accepted integer:=0;
BEGIN
 IF jsonb_typeof(p_descriptors) IS DISTINCT FROM 'array' THEN RAISE EXCEPTION 'Invalid discovery batch'; END IF;
 IF jsonb_array_length(p_descriptors)>128 OR octet_length(p_descriptors::text)>1048576 THEN RAISE EXCEPTION 'Discovery batch limit'; END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' OR a.generation<>p_generation
 OR NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 FOR item IN SELECT value FROM jsonb_array_elements(p_descriptors) LOOP
  d:=item-'interval_seconds';seconds:=coalesce((item->>'interval_seconds')::integer,300);
  IF jsonb_typeof(d) IS DISTINCT FROM 'object' OR d->'context'->>'account_id' IS DISTINCT FROM p_account
   OR coalesce(d->>'resource_key','') !~ '^[a-f0-9]{64}$'
   OR coalesce(d->>'handler','') !~ '^(get_|read_|fm_worker_finances$)' OR seconds NOT BETWEEN 15 AND 86400
  THEN RAISE EXCEPTION 'Invalid discovery descriptor'; END IF;
  IF a.excluded_leagues ? (d->'context'->>'championship_id')
   OR EXISTS(SELECT 1 FROM blocked_leagues WHERE championship_id=d->'context'->>'championship_id')
   OR NOT worker_finance_context_matches(d,a.verified_leagues->(d->'context'->>'championship_id')) THEN CONTINUE; END IF;
  -- Routine rediscovery must not rewrite thousands of unchanged durable jobs.
  IF EXISTS(SELECT 1 FROM worker_jobs j WHERE j.account_id=p_account AND j.resource_key=d->>'resource_key'
   AND j.generation=p_generation AND j.descriptor=d AND j.interval_seconds<=seconds
   AND j.base_priority>=CASE WHEN seconds<=60 THEN 4 WHEN seconds<=300 THEN 3 ELSE 1 END) THEN
   accepted:=accepted+1;
  ELSIF worker_enqueue(p_account,d,seconds,0) THEN accepted:=accepted+1;
  END IF;
 END LOOP;
 RETURN accepted;
END $$;
REVOKE ALL ON FUNCTION public.worker_enqueue_batch(text,text,bigint,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_enqueue_batch(text,text,bigint,jsonb) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Protect visible prerequisite reads from long finance and history work.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_request_priority(p_handler text)
RETURNS integer LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
 SELECT CASE WHEN p_handler IN ('get_players_from_team','get_championship_pressroom','get_market_players',
  'get_my_market_players','get_user_team_info','get_roster_bids','get_lineup_from_team',
  'get_notifications','get_notification_unread','get_active_championships') THEN 10
 WHEN p_handler IN ('get_resolved_league_context','get_finished_rounds') THEN 9
 WHEN p_handler IN ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot') THEN 8
 ELSE 6 END;
$$;
REVOKE ALL ON FUNCTION public.worker_request_priority(text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_request_priority(text) TO service_role;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND public.worker_finance_context_matches(q.descriptor,a.verified_leagues->(q.descriptor->'context'->>'championship_id'))
 AND (q.owner IS NULL OR q.lease_until<now()) AND (NOT p_foreground OR (q.priority>=5 AND q.descriptor->>'handler' NOT IN
  ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot')))
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
 AND (q.priority>=5 OR q.checked_at IS NULL OR q.checked_at +
 make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE q.interval_seconds END)<=now())
 ORDER BY q.priority DESC,a.last_dispatched NULLS FIRST,q.due_at,q.account_id,q.resource_key FOR UPDATE OF q SKIP LOCKED LIMIT 1;
 IF j.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
-- Preserve leases, demand timestamps and snapshots; normalize only queued
-- historical/default demand so old priority-10 entries cannot defeat the policy.
UPDATE public.worker_jobs SET priority=public.worker_request_priority(descriptor->>'handler')
 WHERE owner IS NULL AND priority>=5 AND priority>public.worker_request_priority(descriptor->>'handler');
-- Recent points reuse the same five-minute summary source. Keep the two
-- resource cadences aligned rather than refetching the same saved source/minute.
UPDATE public.worker_jobs SET interval_seconds=300,base_priority=3
 WHERE descriptor->>'handler'='get_player_recent_points' AND interval_seconds=60;
REVOKE ALL ON FUNCTION public.worker_claim(text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_claim(text,boolean) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Keep mixed-version browser requests aligned with summary source validity.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_enqueue(p_account text,p_descriptor jsonb,p_interval integer DEFAULT 60,p_priority integer DEFAULT 10)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;k text:=p_descriptor->>'resource_key';
BEGIN
 -- Browser processes from the previous image may still send the old default.
 IF p_descriptor->>'handler'='get_player_recent_points' AND p_interval=60 THEN p_interval:=300; END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' THEN RETURN false; END IF;
 IF NOT public.worker_finance_context_matches(p_descriptor,a.verified_leagues->(p_descriptor->'context'->>'championship_id')) THEN RETURN false; END IF;
 IF EXISTS(SELECT 1 FROM public.blocked_leagues WHERE championship_id=p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account OR (p_descriptor->>'handler') !~ '^(get_|read_|fm_worker_finances$)' THEN RAISE EXCEPTION 'Invalid descriptor'; END IF;
 IF a.excluded_leagues ? (p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF NOT EXISTS(SELECT 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=k) AND
 (SELECT count(*) FROM worker_jobs WHERE account_id=p_account)>=10000 THEN RAISE EXCEPTION 'Worker job limit'; END IF;
 INSERT INTO worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds,priority,base_priority)
 VALUES(p_account,k,p_descriptor,a.generation,p_interval,greatest(p_priority,CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END),CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET
 due_at=CASE WHEN worker_jobs.owner IS NULL AND excluded.priority>=5 THEN least(worker_jobs.due_at,now()) ELSE worker_jobs.due_at END,
 requested_at=CASE WHEN excluded.priority>=5 THEN now() ELSE worker_jobs.requested_at END,priority=greatest(worker_jobs.priority,excluded.priority),base_priority=greatest(worker_jobs.base_priority,excluded.base_priority),interval_seconds=least(worker_jobs.interval_seconds,excluded.interval_seconds);
 RETURN true;
END $$;
UPDATE public.worker_jobs SET interval_seconds=300,base_priority=3 WHERE descriptor->>'handler'='get_player_recent_points' AND interval_seconds=60;
REVOKE ALL ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Reuse the reserved lane for short core factual reads while no browser is open.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND public.worker_finance_context_matches(q.descriptor,a.verified_leagues->(q.descriptor->'context'->>'championship_id'))
 AND (q.owner IS NULL OR q.lease_until<now()) AND (NOT p_foreground OR ((q.priority>=5 OR (q.priority>=3 AND public.worker_request_priority(q.descriptor->>'handler')>=9)) AND q.descriptor->>'handler' NOT IN
  ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot')))
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
 AND (q.priority>=5 OR q.checked_at IS NULL OR q.checked_at +
 make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE q.interval_seconds END)<=now())
 ORDER BY q.priority DESC,a.last_dispatched NULLS FIRST,q.due_at,q.account_id,q.resource_key FOR UPDATE OF q SKIP LOCKED LIMIT 1;
 IF j.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
UPDATE public.worker_jobs SET priority=base_priority WHERE owner IS NULL AND descriptor->>'handler'='get_player_recent_points' AND priority=4 AND base_priority=3;
REVOKE ALL ON FUNCTION public.worker_claim(text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_claim(text,boolean) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Begin core refresh before source validity ends; never renew source time.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_next_refresh_due(p_handler text,p_interval integer,p_valid_until timestamptz,p_started timestamptz,p_now timestamptz DEFAULT now())
RETURNS timestamptz LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
 SELECT CASE WHEN public.worker_request_priority(p_handler)>=9 AND p_interval BETWEEN 15 AND 300 THEN
  CASE WHEN p_valid_until<=p_now THEN p_now+make_interval(secs=>least(15,p_interval/2))
  ELSE greatest(p_now+interval '5 seconds',least(p_now+make_interval(secs=>p_interval),
   p_valid_until-make_interval(secs=>least(p_interval/2.0,greatest(5,coalesce(extract(epoch from p_now-p_started),0)+5))::double precision))) END
 ELSE p_now+make_interval(secs=>p_interval) END;
$$;
REVOKE ALL ON FUNCTION public.worker_next_refresh_due(text,integer,timestamptz,timestamptz,timestamptz) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_next_refresh_due(text,integer,timestamptz,timestamptz,timestamptz) TO service_role;
CREATE OR REPLACE FUNCTION public.worker_publish(p_account text,p_key text,p_owner text,p_generation bigint,p_manifest jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;a worker_accounts;
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key FOR UPDATE;
 IF a.account_id IS NULL OR j.account_id IS NULL OR NOT a.enabled OR a.generation<>p_generation OR a.auth_status<>'active' OR j.owner IS NULL OR j.lease_until IS NULL OR j.owner IS DISTINCT FROM p_owner OR j.lease_until<=now() OR NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN false; END IF;
 IF (SELECT count(*) FROM worker_chunks WHERE account_id=p_account AND resource_key=p_key AND version=(p_manifest->>'version')::uuid)<>(p_manifest->>'parts')::integer THEN RAISE EXCEPTION 'Incomplete snapshot'; END IF;
 IF j.descriptor->>'handler'='get_active_championships' AND jsonb_typeof(p_manifest->'memberships')='object' THEN
 UPDATE worker_accounts SET verified_leagues=coalesce((SELECT jsonb_object_agg(key,value) FROM jsonb_each(p_manifest->'memberships') m WHERE NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=m.key)),'{}'::jsonb) WHERE account_id=p_account;
 DELETE FROM worker_jobs q WHERE q.account_id=p_account AND q.resource_key<>p_key AND coalesce(q.descriptor->'context'->>'championship_id','')<>'' AND
 (NOT (p_manifest->'memberships' ? (q.descriptor->'context'->>'championship_id')) OR EXISTS(
 SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) AS dim WHERE
 coalesce(q.descriptor->'context'->>dim,'unknown')<>'unknown' AND
 coalesce(p_manifest->'memberships'->(q.descriptor->'context'->>'championship_id')->>dim,'unknown')<>'unknown' AND
 q.descriptor->'context'->>dim IS DISTINCT FROM p_manifest->'memberships'->(q.descriptor->'context'->>'championship_id')->>dim)); END IF;
 IF p_manifest->'context' IS DISTINCT FROM j.descriptor->'context' THEN RAISE EXCEPTION 'Foreign context'; END IF;
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id)
 VALUES(p_account,p_key,p_manifest->'context',j.descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,CASE WHEN j.requested_at>j.started_at THEN least((p_manifest->>'valid_until')::timestamptz,now()-interval '1 second') ELSE (p_manifest->>'valid_until')::timestamptz END,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer,j.run_id)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts,run_id=excluded.run_id
 WHERE worker_snapshots.checked_at<=excluded.checked_at OR worker_snapshots.run_id=j.run_id
 OR (j.descriptor->>'handler'='fm_worker_finances' AND worker_snapshots.resource_key=j.resource_key);
 UPDATE worker_jobs SET owner=NULL,lease_until=NULL,checked_at=now(),due_at=CASE WHEN requested_at>started_at THEN now() WHEN p_manifest->>'status' IN ('ok','empty') THEN public.worker_next_refresh_due(j.descriptor->>'handler',interval_seconds,(p_manifest->>'valid_until')::timestamptz,started_at,now()) ELSE now()+make_interval(secs=>interval_seconds) END,
 priority=CASE WHEN requested_at>started_at THEN 10 ELSE base_priority END,attempts=0,last_error=p_manifest->>'error',last_audit_at=CASE WHEN p_manifest->>'full_audit'='true' THEN now() ELSE last_audit_at END,checkpoint='{}',run_id=gen_random_uuid() WHERE account_id=p_account AND resource_key=p_key;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND public.worker_finance_context_matches(q.descriptor,a.verified_leagues->(q.descriptor->'context'->>'championship_id'))
 AND (q.owner IS NULL OR q.lease_until<now()) AND (NOT p_foreground OR ((q.priority>=5 OR (q.priority>=3 AND public.worker_request_priority(q.descriptor->>'handler')>=9)) AND q.descriptor->>'handler' NOT IN
  ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot')))
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
 AND (q.priority>=5 OR q.checked_at IS NULL OR q.checked_at +
 make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE 0 END)<=now())
 ORDER BY q.priority DESC,a.last_dispatched NULLS FIRST,q.due_at,q.account_id,q.resource_key FOR UPDATE OF q SKIP LOCKED LIMIT 1;
 IF j.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
REVOKE ALL ON FUNCTION public.worker_publish(text,text,text,bigint,jsonb),public.worker_claim(text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_publish(text,text,text,bigint,jsonb),public.worker_claim(text,boolean) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Protect visible prerequisite reads from long finance and history work.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_request_priority(p_handler text)
RETURNS integer LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
 SELECT CASE WHEN p_handler IN ('get_players_from_team','get_championship_pressroom','get_market_players',
  'get_my_market_players','get_user_team_info','get_roster_bids','get_lineup_from_team',
  'get_notifications','get_notification_unread','get_active_championships','get_clause_transfer_summary') THEN 10
 WHEN p_handler IN ('get_resolved_league_context','get_finished_rounds') THEN 9
 WHEN p_handler IN ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot') THEN 8
 ELSE 6 END;
$$;
REVOKE ALL ON FUNCTION public.worker_request_priority(text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_request_priority(text) TO service_role;
CREATE INDEX IF NOT EXISTS worker_jobs_peer_scope ON public.worker_jobs(account_id,(descriptor->>'handler'),md5((descriptor->'args')::text));
CREATE OR REPLACE FUNCTION public.worker_has_resolved_peer(p_account text,p_descriptor jsonb)
RETURNS boolean LANGUAGE sql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT p_descriptor->>'handler' IN ('get_players_from_team','get_championship_pressroom')
 AND EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
   WHERE coalesce(p_descriptor->'context'->>f,'unknown') IN ('','unknown'))
 AND EXISTS(SELECT 1 FROM worker_jobs peer JOIN worker_accounts a USING(account_id)
   WHERE peer.account_id=p_account AND peer.generation=a.generation
   AND peer.descriptor->>'handler'=p_descriptor->>'handler'
   AND md5((peer.descriptor->'args')::text)=md5((p_descriptor->'args')::text)
   AND peer.descriptor->'args'=p_descriptor->'args'
   AND ((peer.descriptor->'context') - ARRAY['competition_id','season','scoring_version']) =
       ((p_descriptor->'context') - ARRAY['competition_id','season','scoring_version'])
   AND NOT EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
     WHERE coalesce(peer.descriptor->'context'->>f,'unknown') IN ('','unknown')
       OR (coalesce(p_descriptor->'context'->>f,'unknown') NOT IN ('','unknown')
         AND peer.descriptor->'context'->>f IS DISTINCT FROM p_descriptor->'context'->>f))
   AND public.worker_finance_context_matches(peer.descriptor,a.verified_leagues->(peer.descriptor->'context'->>'championship_id')));
$$;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND public.worker_finance_context_matches(q.descriptor,a.verified_leagues->(q.descriptor->'context'->>'championship_id'))
 AND NOT public.worker_has_resolved_peer(q.account_id,q.descriptor)
 AND (q.owner IS NULL OR q.lease_until<now()) AND (NOT p_foreground OR ((q.priority>=5 OR (q.priority>=3 AND public.worker_request_priority(q.descriptor->>'handler')>=9)) AND q.descriptor->>'handler' NOT IN
  ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot')))
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
 AND (q.priority>=5 OR q.checked_at IS NULL OR q.checked_at +
 make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE 0 END)<=now())
 ORDER BY q.priority DESC,a.last_dispatched NULLS FIRST,q.due_at,q.account_id,q.resource_key FOR UPDATE OF q SKIP LOCKED LIMIT 1;
 IF j.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
CREATE OR REPLACE FUNCTION public.worker_enqueue(p_account text,p_descriptor jsonb,p_interval integer DEFAULT 60,p_priority integer DEFAULT 10)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;k text:=p_descriptor->>'resource_key';
BEGIN
 -- Browser processes from the previous image may still send the old default.
 IF p_descriptor->>'handler'='get_player_recent_points' AND p_interval=60 THEN p_interval:=300; END IF;
 IF p_descriptor->>'handler'='get_clause_transfer_summary' THEN p_interval:=60; END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' THEN RETURN false; END IF;
 IF NOT public.worker_finance_context_matches(p_descriptor,a.verified_leagues->(p_descriptor->'context'->>'championship_id')) THEN RETURN false; END IF;
 IF EXISTS(SELECT 1 FROM public.blocked_leagues WHERE championship_id=p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account OR (p_descriptor->>'handler') !~ '^(get_|read_|fm_worker_finances$)' THEN RAISE EXCEPTION 'Invalid descriptor'; END IF;
 IF a.excluded_leagues ? (p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF NOT EXISTS(SELECT 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=k) AND
 (SELECT count(*) FROM worker_jobs WHERE account_id=p_account)>=10000 THEN RAISE EXCEPTION 'Worker job limit'; END IF;
 INSERT INTO worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds,priority,base_priority)
 VALUES(p_account,k,p_descriptor,a.generation,p_interval,greatest(p_priority,CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END),CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET
 due_at=CASE WHEN worker_jobs.owner IS NULL AND excluded.priority>=5 THEN least(worker_jobs.due_at,now()) ELSE worker_jobs.due_at END,
 requested_at=CASE WHEN excluded.priority>=5 THEN now() ELSE worker_jobs.requested_at END,priority=greatest(worker_jobs.priority,excluded.priority),base_priority=greatest(worker_jobs.base_priority,excluded.base_priority),interval_seconds=least(worker_jobs.interval_seconds,excluded.interval_seconds);
 RETURN true;
END $$;
UPDATE public.worker_jobs SET interval_seconds=60,base_priority=4,priority=greatest(priority,10),due_at=least(due_at,now())
 WHERE descriptor->>'handler'='get_clause_transfer_summary';
REVOKE ALL ON FUNCTION public.worker_has_resolved_peer(text,jsonb),public.worker_claim(text,boolean),public.worker_enqueue(text,jsonb,integer,integer) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_has_resolved_peer(text,jsonb),public.worker_claim(text,boolean),public.worker_enqueue(text,jsonb,integer,integer) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Background acquisition stays independent of GUI navigation.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_request_priority(p_handler text)
RETURNS integer LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
 SELECT CASE WHEN p_handler IN ('get_players_from_team','get_championship_pressroom','get_market_players',
  'get_my_market_players','get_user_team_info','get_roster_bids','get_lineup_from_team',
  'get_notifications','get_notification_unread','get_active_championships') THEN 10
 WHEN p_handler IN ('get_resolved_league_context','get_finished_rounds') THEN 9
 WHEN p_handler IN ('get_clause_transfer_summary','fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot') THEN 8
 ELSE 6 END;
$$;
REVOKE ALL ON FUNCTION public.worker_request_priority(text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_request_priority(text) TO service_role;
CREATE OR REPLACE FUNCTION public.worker_enqueue(p_account text,p_descriptor jsonb,p_interval integer DEFAULT 60,p_priority integer DEFAULT 10)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;k text:=p_descriptor->>'resource_key';
BEGIN
 -- Browser processes from the previous image may still send the old default.
 IF p_descriptor->>'handler'='get_player_recent_points' AND p_interval=60 THEN p_interval:=300; END IF;
 IF p_descriptor->>'handler'='get_clause_transfer_summary' THEN p_interval:=300; END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' THEN RETURN false; END IF;
 IF NOT public.worker_finance_context_matches(p_descriptor,a.verified_leagues->(p_descriptor->'context'->>'championship_id')) THEN RETURN false; END IF;
 IF EXISTS(SELECT 1 FROM public.blocked_leagues WHERE championship_id=p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account OR (p_descriptor->>'handler') !~ '^(get_|read_|fm_worker_finances$)' THEN RAISE EXCEPTION 'Invalid descriptor'; END IF;
 IF a.excluded_leagues ? (p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF NOT EXISTS(SELECT 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=k) AND
 (SELECT count(*) FROM worker_jobs WHERE account_id=p_account)>=10000 THEN RAISE EXCEPTION 'Worker job limit'; END IF;
 INSERT INTO worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds,priority,base_priority)
 VALUES(p_account,k,p_descriptor,a.generation,p_interval,greatest(p_priority,CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END),CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET
 due_at=CASE WHEN worker_jobs.owner IS NULL AND excluded.priority>=5 THEN least(worker_jobs.due_at,now()) ELSE worker_jobs.due_at END,
 requested_at=CASE WHEN excluded.priority>=5 THEN now() ELSE worker_jobs.requested_at END,priority=greatest(worker_jobs.priority,excluded.priority),base_priority=greatest(worker_jobs.base_priority,excluded.base_priority),interval_seconds=least(worker_jobs.interval_seconds,excluded.interval_seconds);
 RETURN true;
END $$;
UPDATE public.worker_jobs SET interval_seconds=300,base_priority=3,
 priority=CASE WHEN owner IS NULL AND priority<10 THEN 3 ELSE priority END,
 due_at=CASE WHEN owner IS NULL AND checked_at IS NOT NULL AND priority<10
   THEN greatest(due_at,checked_at+interval '300 seconds') ELSE due_at END
 WHERE descriptor->>'handler'='get_clause_transfer_summary';
REVOKE ALL ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Resolve obsolete unknown scopes without relabelling persisted observations.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_resolved_peer(p_account text,p_descriptor jsonb)
RETURNS jsonb LANGUAGE sql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
 WITH candidates AS (
 SELECT peer.descriptor FROM worker_jobs peer JOIN worker_accounts a USING(account_id)
 WHERE p_descriptor->>'handler' IN ('get_players_from_team','get_championship_pressroom','get_clause_transfer_summary')
 AND p_descriptor->'context'->>'account_id'=p_account
 AND EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
   WHERE coalesce(p_descriptor->'context'->>f,'unknown') IN ('','unknown'))
 AND peer.account_id=p_account AND peer.generation=a.generation AND a.enabled AND a.auth_status='active'
 AND peer.descriptor->'version'=p_descriptor->'version'
 AND peer.descriptor->>'handler'=p_descriptor->>'handler'
 AND md5((peer.descriptor->'args')::text)=md5((p_descriptor->'args')::text)
 AND peer.descriptor->'args'=p_descriptor->'args'
 AND ((peer.descriptor->'context')-ARRAY['competition_id','season','scoring_version']) =
     ((p_descriptor->'context')-ARRAY['competition_id','season','scoring_version'])
 AND NOT EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
   WHERE coalesce(peer.descriptor->'context'->>f,'unknown') IN ('','unknown')
     OR coalesce(a.verified_leagues->(peer.descriptor->'context'->>'championship_id')->>f,'unknown') IN ('','unknown')
     OR peer.descriptor->'context'->>f IS DISTINCT FROM a.verified_leagues->(peer.descriptor->'context'->>'championship_id')->>f
     OR (coalesce(p_descriptor->'context'->>f,'unknown') NOT IN ('','unknown')
       AND peer.descriptor->'context'->>f IS DISTINCT FROM p_descriptor->'context'->>f))
 AND NOT(a.excluded_leagues ? (peer.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM blocked_leagues b WHERE b.championship_id=peer.descriptor->'context'->>'championship_id')
 LIMIT 2)
 SELECT CASE WHEN count(*)=1 THEN (jsonb_agg(descriptor))->0 ELSE NULL END FROM candidates;
$$;
CREATE OR REPLACE FUNCTION public.worker_has_resolved_peer(p_account text,p_descriptor jsonb)
RETURNS boolean LANGUAGE sql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT public.worker_resolved_peer(p_account,p_descriptor) IS NOT NULL;
$$;
CREATE OR REPLACE FUNCTION public.worker_read(p_account text,p_versions jsonb DEFAULT '{}')
RETURNS jsonb LANGUAGE sql SECURITY INVOKER SET search_path=public,pg_temp AS $$
 WITH requested AS MATERIALIZED (SELECT key,value FROM jsonb_each_text(p_versions) LIMIT 128),
 routes AS MATERIALIZED (
  SELECT r.key requested_key,coalesce(peer.descriptor->>'resource_key',r.key) key,peer.descriptor
  FROM requested r LEFT JOIN worker_jobs j ON j.account_id=p_account AND j.resource_key=r.key
  LEFT JOIN LATERAL (SELECT public.worker_resolved_peer(p_account,j.descriptor) descriptor) peer ON true),
 targets AS MATERIALIZED (SELECT DISTINCT key FROM routes)
 SELECT jsonb_build_object(
 'account',(SELECT to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues' FROM worker_accounts a WHERE account_id=p_account),
 'health',(SELECT to_jsonb(h) FROM worker_health h WHERE status='running' ORDER BY heartbeat DESC LIMIT 1),
 'redirects',coalesce((SELECT jsonb_agg(jsonb_build_object('requested_key',requested_key,'descriptor',descriptor)) FROM routes WHERE descriptor IS NOT NULL),'[]'),
 'resources',coalesce((SELECT jsonb_agg(jsonb_build_object('key',s.resource_key,'manifest',to_jsonb(s),
 'chunks',(SELECT jsonb_agg(c.data ORDER BY c.part) FROM worker_chunks c WHERE c.account_id=s.account_id AND c.resource_key=s.resource_key AND c.version=s.version),
 'error',j.last_error)) FROM worker_snapshots s JOIN targets t ON t.key=s.resource_key LEFT JOIN worker_jobs j USING(account_id,resource_key)
 WHERE s.account_id=p_account AND s.version::text IS DISTINCT FROM p_versions->>s.resource_key),'[]'),
 'jobs',coalesce((SELECT jsonb_agg(jsonb_build_object('key',j.resource_key,'error',j.last_error,
 'running',j.owner IS NOT NULL AND j.lease_until>now(),'pending',(j.owner IS NOT NULL AND j.lease_until>now()) OR j.due_at<=now()))
 FROM worker_jobs j JOIN targets t ON t.key=j.resource_key WHERE j.account_id=p_account),'[]'));
$$;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND public.worker_finance_context_matches(q.descriptor,a.verified_leagues->(q.descriptor->'context'->>'championship_id'))
 AND NOT public.worker_has_resolved_peer(q.account_id,q.descriptor)
 AND (q.owner IS NULL OR q.lease_until<now()) AND (NOT p_foreground OR ((q.priority>=5 OR (q.priority>=3 AND (public.worker_request_priority(q.descriptor->>'handler')>=9 OR q.descriptor->>'handler'='get_clause_transfer_summary'))) AND q.descriptor->>'handler' NOT IN
  ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot')))
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
 AND (q.priority>=5 OR q.checked_at IS NULL OR q.checked_at +
 make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE 0 END)<=now())
 ORDER BY CASE
 WHEN p_foreground AND q.priority<5 AND public.worker_request_priority(q.descriptor->>'handler')>=9 THEN 9
 WHEN p_foreground AND q.descriptor->>'handler'='get_clause_transfer_summary' THEN greatest(q.priority,8)
 ELSE q.priority END DESC,a.last_dispatched NULLS FIRST,q.due_at,q.account_id,q.resource_key FOR UPDATE OF q SKIP LOCKED LIMIT 1;
 IF j.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
REVOKE ALL ON FUNCTION public.worker_resolved_peer(text,jsonb),public.worker_has_resolved_peer(text,jsonb),public.worker_read(text,jsonb),public.worker_claim(text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_resolved_peer(text,jsonb),public.worker_has_resolved_peer(text,jsonb),public.worker_read(text,jsonb),public.worker_claim(text,boolean) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Aging prevents continuously due core observations starving clause checks.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND public.worker_finance_context_matches(q.descriptor,a.verified_leagues->(q.descriptor->'context'->>'championship_id'))
 AND NOT public.worker_has_resolved_peer(q.account_id,q.descriptor)
 AND (q.owner IS NULL OR q.lease_until<now()) AND (NOT p_foreground OR ((q.priority>=5 OR (q.priority>=3 AND (public.worker_request_priority(q.descriptor->>'handler')>=9 OR q.descriptor->>'handler'='get_clause_transfer_summary'))) AND q.descriptor->>'handler' NOT IN
  ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot')))
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
 AND (q.priority>=5 OR q.checked_at IS NULL OR q.checked_at +
 make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE 0 END)<=now())
 ORDER BY CASE
 WHEN p_foreground AND q.priority<5 AND public.worker_request_priority(q.descriptor->>'handler')>=9 THEN 9
 WHEN p_foreground AND q.descriptor->>'handler'='get_clause_transfer_summary' THEN greatest(q.priority,CASE WHEN q.due_at<=now()-interval '60 seconds' THEN 9 ELSE 8 END)
 ELSE q.priority END DESC,a.last_dispatched NULLS FIRST,q.due_at,q.account_id,q.resource_key FOR UPDATE OF q SKIP LOCKED LIMIT 1;
 IF j.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
REVOKE ALL ON FUNCTION public.worker_claim(text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_claim(text,boolean) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Acquisition belongs to the autonomous worker; browser lookups never create jobs.
BEGIN;
-- Enrollment seeds one source job in the database; login/page rendering does not.
CREATE OR REPLACE FUNCTION public.worker_account_descriptor(p_account text,p_handler text DEFAULT 'get_active_championships')
RETURNS jsonb LANGUAGE plpgsql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
DECLARE wire text;
BEGIN
 IF p_handler NOT IN ('get_active_championships','get_notifications','get_notification_unread') THEN RAISE EXCEPTION 'Invalid account source'; END IF;
 wire:=format('{"args":[],"context":{"account_id":%s,"championship_id":"","competition_id":"unknown","player_id":"","scoring_version":"unknown","season":"unknown","user_team_id":""},"encoded_args":{"context":false,"kind":"list","meta":[],"names":null,"values":[]},"handler":%s,"version":1}',to_json(p_account)::text,to_json(p_handler)::text);
 RETURN wire::jsonb || jsonb_build_object('resource_key',encode(sha256(convert_to(wire,'UTF8')),'hex'));
END $$;
CREATE OR REPLACE FUNCTION public.worker_seed_enrollment()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE d jsonb;
BEGIN
 IF NEW.enabled AND NEW.auth_status='active' THEN
  d:=public.worker_account_descriptor(NEW.account_id);
  INSERT INTO worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds,priority,base_priority)
   VALUES(NEW.account_id,d->>'resource_key',d,NEW.generation,300,3,3) ON CONFLICT DO NOTHING;
 END IF;
 RETURN NEW;
END $$;
REVOKE ALL ON FUNCTION public.worker_account_descriptor(text,text),public.worker_seed_enrollment() FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_account_descriptor(text,text),public.worker_seed_enrollment() TO service_role;
DROP TRIGGER IF EXISTS worker_seed_enrollment ON public.worker_accounts;
CREATE TRIGGER worker_seed_enrollment AFTER INSERT OR UPDATE OF enabled,auth_status,generation ON public.worker_accounts
 FOR EACH ROW EXECUTE FUNCTION public.worker_seed_enrollment();
INSERT INTO public.worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds,priority,base_priority)
 SELECT a.account_id,d->>'resource_key',d,a.generation,300,3,3 FROM public.worker_accounts a
 CROSS JOIN LATERAL (SELECT public.worker_account_descriptor(a.account_id) d) bootstrap
 WHERE a.enabled AND a.auth_status='active' ON CONFLICT DO NOTHING;
ALTER TABLE public.worker_snapshots ADD COLUMN IF NOT EXISTS descriptor jsonb;
UPDATE public.worker_snapshots s SET descriptor=j.descriptor FROM public.worker_jobs j
 WHERE s.descriptor IS NULL AND s.account_id=j.account_id AND s.resource_key=j.resource_key
 AND s.context=j.descriptor->'context' AND s.handler=j.descriptor->>'handler';
CREATE INDEX IF NOT EXISTS worker_snapshots_peer_scope ON public.worker_snapshots(account_id,handler,md5((descriptor->'args')::text)) WHERE descriptor IS NOT NULL;
CREATE OR REPLACE FUNCTION public.worker_publish(p_account text,p_key text,p_owner text,p_generation bigint,p_manifest jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;a worker_accounts;
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key FOR UPDATE;
 IF a.account_id IS NULL OR j.account_id IS NULL OR NOT a.enabled OR a.generation<>p_generation OR a.auth_status<>'active' OR j.owner IS NULL OR j.lease_until IS NULL OR j.owner IS DISTINCT FROM p_owner OR j.lease_until<=now() OR NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN false; END IF;
 IF (SELECT count(*) FROM worker_chunks WHERE account_id=p_account AND resource_key=p_key AND version=(p_manifest->>'version')::uuid)<>(p_manifest->>'parts')::integer THEN RAISE EXCEPTION 'Incomplete snapshot'; END IF;
 IF j.descriptor->>'handler'='get_active_championships' AND jsonb_typeof(p_manifest->'memberships')='object' THEN
 UPDATE worker_accounts SET verified_leagues=coalesce((SELECT jsonb_object_agg(key,value) FROM jsonb_each(p_manifest->'memberships') m WHERE NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=m.key)),'{}'::jsonb) WHERE account_id=p_account;
 DELETE FROM worker_jobs q WHERE q.account_id=p_account AND q.resource_key<>p_key AND coalesce(q.descriptor->'context'->>'championship_id','')<>'' AND
 (NOT (p_manifest->'memberships' ? (q.descriptor->'context'->>'championship_id')) OR EXISTS(
 SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) AS dim WHERE
 coalesce(q.descriptor->'context'->>dim,'unknown')<>'unknown' AND
 coalesce(p_manifest->'memberships'->(q.descriptor->'context'->>'championship_id')->>dim,'unknown')<>'unknown' AND
 q.descriptor->'context'->>dim IS DISTINCT FROM p_manifest->'memberships'->(q.descriptor->'context'->>'championship_id')->>dim)); END IF;
 IF p_manifest->'context' IS DISTINCT FROM j.descriptor->'context' THEN RAISE EXCEPTION 'Foreign context'; END IF;
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id,descriptor)
 VALUES(p_account,p_key,p_manifest->'context',j.descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,CASE WHEN j.requested_at>j.started_at THEN least((p_manifest->>'valid_until')::timestamptz,now()-interval '1 second') ELSE (p_manifest->>'valid_until')::timestamptz END,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer,j.run_id,j.descriptor)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts,run_id=excluded.run_id,descriptor=excluded.descriptor
 WHERE worker_snapshots.checked_at<=excluded.checked_at OR worker_snapshots.run_id=j.run_id
 OR (j.descriptor->>'handler'='fm_worker_finances' AND worker_snapshots.resource_key=j.resource_key);
 UPDATE worker_jobs SET owner=NULL,lease_until=NULL,checked_at=now(),due_at=CASE WHEN requested_at>started_at THEN now() WHEN p_manifest->>'status' IN ('ok','empty') THEN public.worker_next_refresh_due(j.descriptor->>'handler',interval_seconds,(p_manifest->>'valid_until')::timestamptz,started_at,now()) ELSE now()+make_interval(secs=>interval_seconds) END,
 priority=CASE WHEN requested_at>started_at THEN 10 ELSE base_priority END,attempts=0,last_error=p_manifest->>'error',last_audit_at=CASE WHEN p_manifest->>'full_audit'='true' THEN now() ELSE last_audit_at END,checkpoint='{}',run_id=gen_random_uuid() WHERE account_id=p_account AND resource_key=p_key;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.worker_publish_child(p_account text,p_parent text,p_owner text,p_generation bigint,p_descriptor jsonb,p_manifest jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;k text:=p_descriptor->>'resource_key';
BEGIN
 PERFORM 1 FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT worker_guard(p_account,p_parent,p_owner,p_generation) THEN RETURN false; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_parent;
 IF k !~ '^[a-f0-9]{64}$' OR p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account
 OR p_manifest->'context' IS DISTINCT FROM p_descriptor->'context'
 OR (coalesce(p_descriptor->'context'->>'championship_id','')<>'' AND
 p_descriptor->'context'->>'championship_id' IS DISTINCT FROM j.descriptor->'context'->>'championship_id')
 THEN RAISE EXCEPTION 'Foreign child resource'; END IF;
 IF (SELECT count(*) FROM worker_chunks WHERE account_id=p_account AND resource_key=k AND version=(p_manifest->>'version')::uuid)<>(p_manifest->>'parts')::integer THEN RAISE EXCEPTION 'Incomplete child snapshot'; END IF;
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id,descriptor)
 VALUES(p_account,k,p_descriptor->'context',p_descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,CASE WHEN j.requested_at>j.started_at THEN least((p_manifest->>'valid_until')::timestamptz,now()-interval '1 second') ELSE (p_manifest->>'valid_until')::timestamptz END,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer,j.run_id,p_descriptor)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts,run_id=excluded.run_id,descriptor=excluded.descriptor
 WHERE worker_snapshots.checked_at<=excluded.checked_at OR worker_snapshots.run_id=j.run_id
 OR (j.descriptor->>'handler'='fm_worker_finances' AND worker_snapshots.resource_key=j.resource_key);
 IF j.descriptor->>'handler'='get_player_summary' AND p_descriptor->>'handler' IN ('get_clause_transfer_summary','get_player_recent_points')
 AND p_manifest->>'status' IN ('ok','empty') THEN
  DELETE FROM worker_jobs WHERE account_id=p_account AND resource_key=k AND owner IS NULL;
 END IF;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.worker_resolved_peer(p_account text,p_descriptor jsonb)
RETURNS jsonb LANGUAGE plpgsql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 IF p_descriptor IS NULL OR p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account
 OR NOT EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
 WHERE coalesce(p_descriptor->'context'->>f,'unknown') IN ('','unknown')) THEN RETURN NULL; END IF;
 RETURN (WITH possible AS (
  SELECT j.descriptor FROM worker_jobs j JOIN worker_accounts a USING(account_id)
  WHERE j.account_id=p_account AND j.generation=a.generation
   AND j.descriptor->>'handler'=p_descriptor->>'handler'
   AND md5((j.descriptor->'args')::text)=md5((p_descriptor->'args')::text)
  UNION
  SELECT s.descriptor FROM worker_snapshots s WHERE s.account_id=p_account AND s.descriptor IS NOT NULL
   AND s.handler=p_descriptor->>'handler' AND md5((s.descriptor->'args')::text)=md5((p_descriptor->'args')::text)
 ), candidates AS (
 SELECT peer.descriptor FROM possible peer JOIN worker_accounts a ON a.account_id=p_account
 WHERE p_descriptor->'context'->>'account_id'=p_account AND a.enabled AND a.auth_status='active'
 AND EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
   WHERE coalesce(p_descriptor->'context'->>f,'unknown') IN ('','unknown'))
 AND peer.descriptor->'version'=p_descriptor->'version'
 AND peer.descriptor->'args'=p_descriptor->'args'
 AND ((peer.descriptor->'context')-ARRAY['competition_id','season','scoring_version']) =
     ((p_descriptor->'context')-ARRAY['competition_id','season','scoring_version'])
 AND NOT EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
   WHERE coalesce(peer.descriptor->'context'->>f,'unknown') IN ('','unknown')
     OR coalesce(a.verified_leagues->(peer.descriptor->'context'->>'championship_id')->>f,'unknown') IN ('','unknown')
     OR peer.descriptor->'context'->>f IS DISTINCT FROM a.verified_leagues->(peer.descriptor->'context'->>'championship_id')->>f
     OR (coalesce(p_descriptor->'context'->>f,'unknown') NOT IN ('','unknown')
       AND peer.descriptor->'context'->>f IS DISTINCT FROM p_descriptor->'context'->>f))
 AND NOT(a.excluded_leagues ? (peer.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM blocked_leagues b WHERE b.championship_id=peer.descriptor->'context'->>'championship_id')
 LIMIT 2)
 SELECT CASE WHEN count(*)=1 THEN (jsonb_agg(descriptor))->0 ELSE NULL END FROM candidates);
END $$;
-- Replace the old signature: legacy two-argument callers still use the default.
DROP FUNCTION IF EXISTS public.worker_read(text,jsonb);
CREATE OR REPLACE FUNCTION public.worker_read(p_account text,p_versions jsonb DEFAULT '{}',p_descriptors jsonb DEFAULT '{}')
RETURNS jsonb LANGUAGE plpgsql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 IF jsonb_typeof(p_versions) IS DISTINCT FROM 'object' OR jsonb_typeof(p_descriptors) IS DISTINCT FROM 'object'
 OR octet_length(p_versions::text)>65536 OR octet_length(p_descriptors::text)>1048576
 OR (SELECT count(*) FROM jsonb_object_keys(p_versions))>128
 OR (SELECT count(*) FROM jsonb_object_keys(p_descriptors))>128 THEN RAISE EXCEPTION 'Invalid read request'; END IF;
 IF EXISTS(SELECT 1 FROM jsonb_each(p_descriptors) d WHERE d.key !~ '^[a-f0-9]{64}$'
  OR d.value->>'resource_key' IS DISTINCT FROM d.key OR d.value->'context'->>'account_id' IS DISTINCT FROM p_account
  OR coalesce(d.value->>'handler','') !~ '^(get_|read_|fm_worker_finances$)') THEN RAISE EXCEPTION 'Foreign read descriptor'; END IF;
 RETURN (
 WITH requested AS MATERIALIZED (SELECT key,value FROM jsonb_each_text(p_versions)),
 routes AS MATERIALIZED (
  SELECT r.key requested_key,coalesce(peer.descriptor->>'resource_key',r.key) key,peer.descriptor
  FROM requested r LEFT JOIN worker_jobs j ON j.account_id=p_account AND j.resource_key=r.key
  LEFT JOIN LATERAL (SELECT public.worker_resolved_peer(p_account,coalesce(p_descriptors->r.key,j.descriptor)) descriptor) peer ON true),
 targets AS MATERIALIZED (SELECT DISTINCT key FROM routes)
 SELECT jsonb_build_object(
 'account',(SELECT to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues' FROM worker_accounts a WHERE account_id=p_account),
 'health',(SELECT to_jsonb(h) FROM worker_health h WHERE status='running' ORDER BY heartbeat DESC LIMIT 1),
 'redirects',coalesce((SELECT jsonb_agg(jsonb_build_object('requested_key',requested_key,'descriptor',descriptor)) FROM routes WHERE descriptor IS NOT NULL),'[]'),
 'resources',coalesce((SELECT jsonb_agg(jsonb_build_object('key',s.resource_key,'manifest',to_jsonb(s)-'descriptor',
 'chunks',(SELECT jsonb_agg(c.data ORDER BY c.part) FROM worker_chunks c WHERE c.account_id=s.account_id AND c.resource_key=s.resource_key AND c.version=s.version),
 'error',j.last_error)) FROM worker_snapshots s JOIN targets t ON t.key=s.resource_key LEFT JOIN worker_jobs j USING(account_id,resource_key)
 WHERE s.account_id=p_account AND s.version::text IS DISTINCT FROM p_versions->>s.resource_key),'[]'),
 'jobs',coalesce((SELECT jsonb_agg(jsonb_build_object('key',j.resource_key,'error',j.last_error,
 'running',j.owner IS NOT NULL AND j.lease_until>now(),'pending',(j.owner IS NOT NULL AND j.lease_until>now()) OR j.due_at<=now(),
 'next_due_at',j.due_at)) FROM worker_jobs j JOIN targets t ON t.key=j.resource_key
 JOIN worker_accounts a ON a.account_id=j.account_id WHERE j.account_id=p_account AND j.generation=a.generation
 AND a.enabled AND a.auth_status='active' AND NOT(a.excluded_leagues ? (j.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM blocked_leagues b WHERE b.championship_id=j.descriptor->'context'->>'championship_id')),'[]')));
END $$;
REVOKE ALL ON FUNCTION public.worker_read(text,jsonb,jsonb),public.worker_resolved_peer(text,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_read(text,jsonb,jsonb),public.worker_resolved_peer(text,jsonb) TO service_role;
CREATE OR REPLACE FUNCTION public.worker_enqueue_batch(p_account text,p_owner text,p_generation bigint,p_descriptors jsonb)
RETURNS integer LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;item jsonb;d jsonb;seconds integer;accepted integer:=0;
BEGIN
 IF jsonb_typeof(p_descriptors) IS DISTINCT FROM 'array' THEN RAISE EXCEPTION 'Invalid discovery batch'; END IF;
 IF jsonb_array_length(p_descriptors)>128 OR octet_length(p_descriptors::text)>1048576 THEN RAISE EXCEPTION 'Discovery batch limit'; END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' OR a.generation<>p_generation
 OR NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 FOR item IN SELECT value FROM jsonb_array_elements(p_descriptors) LOOP
  d:=item-'interval_seconds';seconds:=coalesce((item->>'interval_seconds')::integer,300);
  IF jsonb_typeof(d) IS DISTINCT FROM 'object' OR d->'context'->>'account_id' IS DISTINCT FROM p_account
   OR coalesce(d->>'resource_key','') !~ '^[a-f0-9]{64}$'
   OR coalesce(d->>'handler','') !~ '^(get_|read_|fm_worker_finances$)' OR seconds NOT BETWEEN 15 AND 86400
  THEN RAISE EXCEPTION 'Invalid discovery descriptor'; END IF;
  IF a.excluded_leagues ? (d->'context'->>'championship_id')
   OR EXISTS(SELECT 1 FROM blocked_leagues WHERE championship_id=d->'context'->>'championship_id')
   OR NOT worker_finance_context_matches(d,a.verified_leagues->(d->'context'->>'championship_id')) THEN CONTINUE; END IF;
  -- Routine rediscovery must not rewrite thousands of unchanged durable jobs.
  IF EXISTS(SELECT 1 FROM worker_jobs j WHERE j.account_id=p_account AND j.resource_key=d->>'resource_key'
   AND j.generation=p_generation AND j.descriptor=d AND j.interval_seconds<=seconds
   AND j.base_priority>=CASE WHEN seconds<=60 THEN 4 WHEN seconds<=300 THEN 3 ELSE 1 END) THEN
   accepted:=accepted+1;
  ELSIF worker_enqueue(p_account,d,seconds,0) THEN accepted:=accepted+1;
  END IF;
  -- Account-only endpoints used to inherit browser league scope / named empty
  -- arguments. Retire duplicate jobs only after the canonical replacement is
  -- durably registered. Factual snapshots and source times are preserved.
  IF d->>'handler' IN ('get_active_championships','get_notifications','get_notification_unread')
   AND d->'args'='[]'::jsonb AND coalesce(d->'context'->>'championship_id','')=''
   AND EXISTS(SELECT 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=d->>'resource_key') THEN
   DELETE FROM worker_jobs WHERE account_id=p_account AND descriptor->>'handler'=d->>'handler'
    AND resource_key<>d->>'resource_key' AND owner IS NULL;
  END IF;
 END LOOP;
 DELETE FROM worker_jobs old WHERE old.account_id=p_account AND old.owner IS NULL
 AND old.descriptor->>'handler' IN ('get_active_championships','get_notifications','get_notification_unread')
 AND old.resource_key<>public.worker_account_descriptor(p_account,old.descriptor->>'handler')->>'resource_key'
 AND EXISTS(SELECT 1 FROM worker_jobs current WHERE current.account_id=p_account
  AND current.resource_key=public.worker_account_descriptor(p_account,old.descriptor->>'handler')->>'resource_key');
 -- Card history now comes from summary publication and existing DB readers.
 DELETE FROM worker_jobs WHERE account_id=p_account AND descriptor->>'handler'='get_player_card_history' AND owner IS NULL;
 RETURN accepted;
END $$;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Fixed completion-based cadence and fair service between independent query families.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_next_refresh_due(p_handler text,p_interval integer,p_valid_until timestamptz,p_started timestamptz,p_now timestamptz DEFAULT now())
RETURNS timestamptz LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
 SELECT p_now+make_interval(secs=>p_interval);
$$;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 WITH dispatched AS MATERIALIZED (
  SELECT account_id,descriptor->>'handler' handler,max(started_at) served_at FROM worker_jobs GROUP BY 1,2)
 SELECT q.* INTO j FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 JOIN dispatched d ON d.account_id=q.account_id AND d.handler=q.descriptor->>'handler'
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND public.worker_finance_context_matches(q.descriptor,a.verified_leagues->(q.descriptor->'context'->>'championship_id'))
 AND NOT public.worker_has_resolved_peer(q.account_id,q.descriptor)
 AND (q.owner IS NULL OR q.lease_until<now())
 AND (NOT p_foreground OR q.descriptor->>'handler' NOT IN
   ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot'))
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
 AND (q.checked_at IS NULL OR q.checked_at + make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE 0 END)<=now())
 -- The short lane prefers core observations, then helps other short reads. The other lane gives each due
 -- query family a turn, so a recurring minute job or527 player jobs cannot
 -- indefinitely exclude catalogs, history or accounting. One-lane installs
 -- retain the same family fairness across core and background work.
 ORDER BY CASE WHEN p_foreground AND (public.worker_request_priority(q.descriptor->>'handler')>=9 OR q.descriptor->>'handler'='get_clause_transfer_summary') THEN 0 ELSE 1 END,
 d.served_at NULLS FIRST,a.last_dispatched NULLS FIRST,q.due_at,q.account_id,q.resource_key
 FOR UPDATE OF q SKIP LOCKED LIMIT 1;
 IF j.account_id IS NULL THEN RETURN NULL; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
REVOKE ALL ON FUNCTION public.worker_next_refresh_due(text,integer,timestamptz,timestamptz,timestamptz),public.worker_claim(text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_next_refresh_due(text,integer,timestamptz,timestamptz,timestamptz),public.worker_claim(text,boolean) TO service_role;
UPDATE public.worker_jobs SET interval_seconds=300,base_priority=3,
 due_at=CASE WHEN owner IS NULL AND checked_at IS NOT NULL THEN greatest(due_at,checked_at+interval '300 seconds') ELSE due_at END
 WHERE descriptor->>'handler'='get_team_acquisition_capacity' AND interval_seconds<300;
DELETE FROM public.worker_jobs WHERE descriptor->>'handler'='get_player_card_history' AND owner IS NULL;
NOTIFY pgrst,'reload schema';
COMMIT;

-- November 14: retire superseded player-summary queue descriptors only.
-- Retire only idle legacy summary jobs with an acknowledged current-context
-- replacement. Retain every factual snapshot/chunk and all ambiguous team scopes.
-- The database-only GUI and canonical worker discovery no longer recreate these
-- historical browser-generated descriptors, so no recurring queue scan is needed.
BEGIN;
WITH active AS MATERIALIZED (
 SELECT j.*,a.verified_leagues->(j.descriptor->'context'->>'championship_id') membership
 FROM public.worker_jobs j JOIN public.worker_accounts a USING(account_id)
 WHERE j.descriptor->>'handler'='get_player_summary'
 AND a.enabled AND a.auth_status='active' AND j.generation=a.generation
 AND j.descriptor->'context'->>'account_id'=j.account_id
 AND NOT(a.excluded_leagues ? (j.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b
  WHERE b.championship_id=j.descriptor->'context'->>'championship_id')
), canonical AS MATERIALIZED (
 SELECT j.* FROM active j JOIN public.worker_snapshots s USING(account_id,resource_key)
 WHERE s.handler='get_player_summary' AND s.context=j.descriptor->'context'
 AND s.status IN ('ok','empty') AND s.completeness IN ('complete','unknown') AND s.observed_at<=now()
 AND NOT EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
  WHERE coalesce(j.descriptor->'context'->>f,'unknown') IN ('','unknown')
  OR j.descriptor->'context'->>f IS DISTINCT FROM j.membership->>f)
), unknown_replaced AS (
 SELECT old.account_id,old.resource_key FROM active old JOIN canonical peer
 ON peer.account_id=old.account_id AND peer.resource_key<>old.resource_key
 AND peer.descriptor->'args'=old.descriptor->'args'
 AND ((peer.descriptor->'context')-ARRAY['competition_id','season','scoring_version'])=
     ((old.descriptor->'context')-ARRAY['competition_id','season','scoring_version'])
 WHERE old.owner IS NULL
 AND EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
  WHERE coalesce(old.descriptor->'context'->>f,'unknown') IN ('','unknown'))
 AND NOT EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
  WHERE coalesce(old.descriptor->'context'->>f,'unknown') NOT IN ('','unknown')
  AND old.descriptor->'context'->>f IS DISTINCT FROM peer.descriptor->'context'->>f)
 GROUP BY old.account_id,old.resource_key HAVING count(*)=1
), blank_team_replaced AS (
 SELECT old.account_id,old.resource_key FROM canonical old JOIN canonical peer
 ON peer.account_id=old.account_id AND peer.resource_key<>old.resource_key
 AND ((peer.descriptor->'args')-'user_team_id')=((old.descriptor->'args')-'user_team_id')
 AND ((peer.descriptor->'context')-'user_team_id')=((old.descriptor->'context')-'user_team_id')
 WHERE old.owner IS NULL AND coalesce(old.descriptor->'context'->>'user_team_id','')=''
 AND coalesce(peer.descriptor->'context'->>'user_team_id','')<>''
 GROUP BY old.account_id,old.resource_key HAVING count(*)=1
), retired AS (
 SELECT * FROM unknown_replaced UNION SELECT * FROM blank_team_replaced
)
DELETE FROM public.worker_jobs old USING retired
WHERE old.account_id=retired.account_id AND old.resource_key=retired.resource_key AND old.owner IS NULL;
COMMIT;

-- Make a shorter autonomous cadence effective for an existing successful job.
-- This changes scheduling only; saved observations and freshness never change.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_enqueue(p_account text,p_descriptor jsonb,p_interval integer DEFAULT 60,p_priority integer DEFAULT 10)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;k text:=p_descriptor->>'resource_key';
BEGIN
 -- Browser processes from the previous image may still send the old default.
 IF p_descriptor->>'handler'='get_player_recent_points' AND p_interval=60 THEN p_interval:=300; END IF;
 IF p_descriptor->>'handler'='get_clause_transfer_summary' THEN p_interval:=300; END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' THEN RETURN false; END IF;
 IF NOT public.worker_finance_context_matches(p_descriptor,a.verified_leagues->(p_descriptor->'context'->>'championship_id')) THEN RETURN false; END IF;
 IF EXISTS(SELECT 1 FROM public.blocked_leagues WHERE championship_id=p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account OR (p_descriptor->>'handler') !~ '^(get_|read_|fm_worker_finances$)' THEN RAISE EXCEPTION 'Invalid descriptor'; END IF;
 IF a.excluded_leagues ? (p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF NOT EXISTS(SELECT 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=k) AND
 (SELECT count(*) FROM worker_jobs WHERE account_id=p_account)>=10000 THEN RAISE EXCEPTION 'Worker job limit'; END IF;
 INSERT INTO worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds,priority,base_priority)
 VALUES(p_account,k,p_descriptor,a.generation,p_interval,greatest(p_priority,CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END),CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET
 due_at=CASE WHEN worker_jobs.owner IS NULL AND excluded.priority>=5 THEN least(worker_jobs.due_at,now())
  -- A newly observed roster/transfer can shorten an existing hourly summary
  -- cadence. Apply that cadence from its last completion instead of keeping
  -- the old hourly due date. Rediscovery at the same cadence is inert, and
  -- failed or running work retains its retry/lease schedule.
  WHEN worker_jobs.owner IS NULL AND worker_jobs.last_error IS NULL
   AND worker_jobs.checked_at IS NOT NULL AND excluded.interval_seconds<worker_jobs.interval_seconds
  THEN least(worker_jobs.due_at,worker_jobs.checked_at+make_interval(secs=>excluded.interval_seconds))
  ELSE worker_jobs.due_at END,
 requested_at=CASE WHEN excluded.priority>=5 THEN now() ELSE worker_jobs.requested_at END,priority=greatest(worker_jobs.priority,excluded.priority),base_priority=greatest(worker_jobs.base_priority,excluded.base_priority),interval_seconds=least(worker_jobs.interval_seconds,excluded.interval_seconds);
 RETURN true;
END $$;
REVOKE ALL ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_enqueue(text,jsonb,integer,integer) TO service_role;
-- Earlier promotions changed the interval but retained the hourly due date.
-- Repair only the account's current successful market scope; rival/ambiguous
-- identities, retries, active leases and every source snapshot remain untouched.
UPDATE public.worker_jobs j SET due_at=j.checked_at+make_interval(secs=>j.interval_seconds)
FROM public.worker_accounts a
WHERE j.account_id=a.account_id AND a.enabled AND a.auth_status='active' AND j.generation=a.generation
 AND j.descriptor->>'handler'='get_player_summary' AND j.interval_seconds=300
 AND j.owner IS NULL AND j.last_error IS NULL AND j.checked_at IS NOT NULL
 AND j.due_at>j.checked_at+make_interval(secs=>j.interval_seconds)
 AND j.descriptor->'context'->>'account_id'=j.account_id
 AND coalesce(j.descriptor->'context'->>'user_team_id','')<>''
 AND NOT(a.excluded_leagues ? (j.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=j.descriptor->'context'->>'championship_id')
 AND NOT EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
  WHERE coalesce(j.descriptor->'context'->>f,'unknown') IN ('','unknown')
   OR j.descriptor->'context'->>f IS DISTINCT FROM a.verified_leagues->(j.descriptor->'context'->>'championship_id')->>f)
 AND EXISTS(SELECT 1 FROM public.worker_jobs own JOIN public.worker_snapshots s USING(account_id,resource_key)
  WHERE own.account_id=j.account_id AND own.generation=a.generation AND own.descriptor->>'handler'='get_market_players'
   AND s.handler='get_market_players' AND s.context=own.descriptor->'context'
   AND s.status IN ('ok','empty') AND s.completeness IN ('complete','unknown') AND s.observed_at<=now()
   AND ((own.descriptor->'context')-'player_id')=((j.descriptor->'context')-'player_id'));
NOTIFY pgrst,'reload schema';
COMMIT;
BEGIN;
SET LOCAL lock_timeout='5s';
SET LOCAL statement_timeout='30s';
CREATE TABLE IF NOT EXISTS public.user_timezone_preferences (
 user_id text PRIMARY KEY CHECK (length(user_id) BETWEEN 1 AND 256 AND user_id ~ '^[A-Za-z0-9_-]+$'),
 display_name text NOT NULL DEFAULT '' CHECK (length(display_name)<=256),
 timezone text NOT NULL DEFAULT 'Europe/Madrid',
 source text NOT NULL CHECK (source IN ('browser','user','admin')),
 updated_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
ALTER TABLE public.user_timezone_preferences ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.user_timezone_preferences FROM PUBLIC, anon, authenticated, service_role;
GRANT SELECT, INSERT, UPDATE ON public.user_timezone_preferences TO service_role;
-- Server-only validated RPCs; no credentials or browser locations.
CREATE OR REPLACE FUNCTION public.initialize_user_timezone(p_user text,p_timezone text,p_label text DEFAULT '')
RETURNS SETOF public.user_timezone_preferences LANGUAGE plpgsql SECURITY INVOKER SET search_path='' AS $$
BEGIN
 IF NOT EXISTS(SELECT 1 FROM pg_timezone_names WHERE name=p_timezone) THEN RAISE EXCEPTION 'Invalid timezone'; END IF;
 RETURN QUERY INSERT INTO public.user_timezone_preferences(user_id,display_name,timezone,source)
 VALUES(p_user,p_label,p_timezone,'browser') ON CONFLICT(user_id) DO UPDATE SET display_name=excluded.display_name
 RETURNING *;
END;
$$;
CREATE OR REPLACE FUNCTION public.set_user_timezone(p_user text,p_timezone text,p_source text)
RETURNS SETOF public.user_timezone_preferences LANGUAGE plpgsql SECURITY INVOKER SET search_path='' AS $$
BEGIN
 IF NOT EXISTS(SELECT 1 FROM pg_timezone_names WHERE name=p_timezone) THEN RAISE EXCEPTION 'Invalid timezone'; END IF;
 IF p_source NOT IN ('user','admin') OR p_source IS NULL THEN RAISE EXCEPTION 'Invalid source'; END IF;
 RETURN QUERY INSERT INTO public.user_timezone_preferences(user_id,timezone,source)
 VALUES(p_user,p_timezone,p_source) ON CONFLICT(user_id) DO UPDATE
 SET timezone=excluded.timezone,source=excluded.source,updated_at=clock_timestamp() RETURNING *;
END;
$$;
CREATE OR REPLACE FUNCTION public.list_user_timezones(p_after text DEFAULT '',p_search text DEFAULT '')
RETURNS TABLE(user_id text,display_name text,timezone text,source text,updated_at timestamptz)
LANGUAGE sql STABLE SECURITY INVOKER SET search_path='' AS $$
 WITH accounts AS (
  SELECT user_id FROM public.user_timezone_preferences
  UNION SELECT user_id FROM public.user_page_preferences
  UNION SELECT account_id FROM public.worker_accounts
 )
 SELECT a.user_id,coalesce(t.display_name,''),t.timezone,t.source,t.updated_at
 FROM accounts a LEFT JOIN public.user_timezone_preferences t ON t.user_id=a.user_id
 WHERE a.user_id>p_after AND (p_search='' OR strpos(lower(a.user_id),lower(p_search))>0
  OR strpos(lower(coalesce(t.display_name,'')),lower(p_search))>0)
 ORDER BY a.user_id LIMIT 201;
$$;
REVOKE ALL ON FUNCTION public.initialize_user_timezone(text,text,text),public.set_user_timezone(text,text,text),public.list_user_timezones(text,text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.initialize_user_timezone(text,text,text),public.set_user_timezone(text,text,text),public.list_user_timezones(text,text) TO service_role;
COMMENT ON TABLE public.user_timezone_preferences IS 'Server-only display timezone per Futmondo account; browser initialized once, user/admin override persists. No coordinates or authentication secrets.';
NOTIFY pgrst,'reload schema';
COMMIT;

-- Explicitly authorized, one-shot clause purchases. Independent of factual-sync consent.
BEGIN;
CREATE TABLE IF NOT EXISTS public.clause_authorizations (
 account_id text PRIMARY KEY, encrypted_token text, credential_id text,
 generation bigint NOT NULL DEFAULT 1, enabled boolean NOT NULL DEFAULT true,
 auth_status text NOT NULL DEFAULT 'active' CHECK(auth_status IN ('active','reauthorize','revoked')),
 updated_at timestamptz NOT NULL DEFAULT now(), rate_limited_until timestamptz,
 CHECK(octet_length(encrypted_token)<=16384)
);
CREATE TABLE IF NOT EXISTS public.clause_jobs (
 id text PRIMARY KEY CHECK(id ~ '^[a-f0-9]{32}$'), account_id text NOT NULL,
 context jsonb NOT NULL, kind text NOT NULL CHECK(kind IN ('scheduled','manual')),
 mode text NOT NULL CHECK(mode IN ('bid','offer','modify','clause')),
 player_id text NOT NULL, player_slug text NOT NULL, player_name text NOT NULL,
 original_owner text NOT NULL, max_price numeric NOT NULL CHECK(max_price>0),
 reserved_amount numeric NOT NULL CHECK(reserved_amount>=0), reserved_slot integer NOT NULL CHECK(reserved_slot IN (0,1)),
 opens_at timestamptz NOT NULL, due_at timestamptz NOT NULL, expires_at timestamptz NOT NULL,
 state text NOT NULL DEFAULT 'scheduled' CHECK(state IN ('scheduled','preparing','dispatched','uncertain','accepted','succeeded','failed','cancelled','expired')),
 result_code text NOT NULL DEFAULT '', actual_price numeric, consent_version integer NOT NULL DEFAULT 1,
 attempts integer NOT NULL DEFAULT 0,reconciliation_attempts integer NOT NULL DEFAULT 0, owner text, lease_until timestamptz,
 dispatched_at timestamptz, finished_at timestamptz, acknowledged_at timestamptz,
 created_at timestamptz NOT NULL DEFAULT now(), updated_at timestamptz NOT NULL DEFAULT now(),player_observed_at timestamptz NOT NULL DEFAULT now(),
 CHECK(context->>'account_id'=account_id), CHECK(context->>'player_id'=player_id),
 CHECK(octet_length(context::text)<=4096), CHECK(length(player_name)<=256), CHECK(length(player_slug)<=256),
 CHECK(result_code ~ '^[a-zA-Z0-9_.:]{0,160}$')
);
CREATE UNIQUE INDEX IF NOT EXISTS clause_jobs_active_player ON public.clause_jobs
 (account_id,(context->>'championship_id'),(context->>'season'),player_id)
 WHERE kind='scheduled' AND state IN ('scheduled','preparing','dispatched','uncertain','accepted');
CREATE INDEX IF NOT EXISTS clause_jobs_deadline ON public.clause_jobs(due_at)
 WHERE state IN ('scheduled','preparing','dispatched','uncertain','accepted');
CREATE INDEX IF NOT EXISTS clause_jobs_expiry ON public.clause_jobs(expires_at) WHERE kind='scheduled' AND state IN ('scheduled','preparing');
CREATE INDEX IF NOT EXISTS clause_jobs_account ON public.clause_jobs(account_id,created_at DESC);
CREATE INDEX IF NOT EXISTS clause_jobs_account_active ON public.clause_jobs(account_id) WHERE state IN ('scheduled','preparing','dispatched','uncertain','accepted');
CREATE INDEX IF NOT EXISTS clause_jobs_recovery ON public.clause_jobs(lease_until) WHERE state IN ('dispatched','preparing');
CREATE TABLE IF NOT EXISTS public.clause_attempts (
 job_id text NOT NULL REFERENCES public.clause_jobs(id), attempt integer NOT NULL,
 started_at timestamptz NOT NULL DEFAULT now(), finished_at timestamptz, result_code text,
 PRIMARY KEY(job_id,attempt), CHECK(result_code ~ '^[a-zA-Z0-9_.:]{0,160}$')
);
CREATE TABLE IF NOT EXISTS public.clause_evidence (
 account_id text NOT NULL, evidence_key text NOT NULL CHECK(evidence_key ~ '^[a-f0-9]{64}$'),
 context jsonb NOT NULL, payload jsonb NOT NULL,payload_bytes integer NOT NULL CHECK(payload_bytes BETWEEN 1 AND 16777216), observed_at timestamptz NOT NULL,
 valid_until timestamptz NOT NULL, complete boolean NOT NULL,
 PRIMARY KEY(account_id,evidence_key), CHECK(octet_length(payload::text)<=16777216)
);
CREATE OR REPLACE FUNCTION public.clause_team_lock(p_context jsonb) RETURNS void
 LANGUAGE sql SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT pg_advisory_xact_lock(hashtextextended(concat_ws('|',p_context->>'account_id',p_context->>'championship_id',p_context->>'user_team_id',p_context->>'season'),0));
$$;
CREATE OR REPLACE FUNCTION public.clause_same_team(a jsonb,b jsonb) RETURNS boolean
 LANGUAGE sql IMMUTABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT a->>'account_id'=b->>'account_id' AND a->>'championship_id'=b->>'championship_id'
 AND a->>'user_team_id'=b->>'user_team_id' AND a->>'competition_id'=b->>'competition_id'
 AND a->>'season'=b->>'season' AND a->>'scoring_version'=b->>'scoring_version';
$$;
CREATE OR REPLACE FUNCTION public.clause_check_facts(p_context jsonb,p_facts jsonb) RETURNS void
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE f text;
BEGIN
 FOREACH f IN ARRAY ARRAY['account_id','championship_id','user_team_id','competition_id','season','scoring_version'] LOOP
  IF coalesce(p_context->>f,'') IN ('','unknown') THEN RAISE EXCEPTION 'clause_context_unverified'; END IF;
 END LOOP;
 IF p_facts->>'status' IS DISTINCT FROM 'ok' OR p_facts->'roster'->>'status' IS DISTINCT FROM 'ok'
 OR p_facts->>'valid_until' IS NULL OR p_facts->>'checked_at' IS NULL OR p_facts->>'financial_observed_at' IS NULL
 OR (p_facts->>'valid_until')::timestamptz>now()+interval '16 seconds' OR (p_facts->>'financial_observed_at')::timestamptz>now()+interval '2 seconds'
 OR (p_facts->>'valid_until')::timestamptz<=now() OR (p_facts->>'checked_at')::timestamptz<now()-interval '30 seconds'
 OR (p_facts->>'checked_at')::timestamptz>now()+interval '2 seconds'
 OR NOT clause_same_team(p_context,p_facts->'context')
 THEN RAISE EXCEPTION 'clause_facts_unverified'; END IF;
 IF (p_facts->'funds'->>'spendable_budget')::numeric IS NULL
 OR (p_facts->'funds'->>'api_bid_limit')::numeric IS NULL
 OR (p_facts->'roster'->>'effective_count')::integer IS NULL
 OR (p_facts->'roster'->>'cap')::integer IS NULL
 OR (p_facts->'outstanding'->>'count')::integer IS NULL
 OR (p_facts->'funds'->>'spendable_budget')::numeric<0 OR (p_facts->'funds'->>'spendable_budget')::numeric>1e16
 OR (p_facts->'funds'->>'api_bid_limit')::numeric<0 OR (p_facts->'funds'->>'api_bid_limit')::numeric>1e16
 OR (p_facts->'outstanding'->>'count')::integer<0 OR (p_facts->'roster'->>'effective_count')::integer<0
 OR (p_facts->'roster'->>'cap')::integer<1
 OR jsonb_typeof(p_facts->'offers') IS DISTINCT FROM 'array'
 THEN RAISE EXCEPTION 'clause_facts_incomplete'; END IF;
END $$;
-- Called only with verified current source facts. Accepted purchases retain funds
-- until both financial checks and the corresponding source identity are current.
CREATE OR REPLACE FUNCTION public.clause_reconcile_reservations(p_context jsonb,p_facts jsonb) RETURNS void
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 PERFORM clause_check_facts(p_context,p_facts);PERFORM clause_team_lock(p_context);
 UPDATE clause_jobs j SET state='succeeded',reserved_amount=0,reserved_slot=0,
  finished_at=coalesce(finished_at,now()),updated_at=now(),owner=NULL,lease_until=NULL
 WHERE j.account_id=p_context->>'account_id' AND clause_same_team(j.context,p_context)
 AND j.state IN ('accepted','uncertain','dispatched') AND j.dispatched_at IS NOT NULL
 AND (p_facts->>'financial_observed_at')::timestamptz>j.dispatched_at
 AND (CASE WHEN j.mode IN ('bid','offer','modify') THEN EXISTS(
  SELECT 1 FROM jsonb_array_elements(p_facts->'offers') o WHERE o->>'player_id'=j.player_id AND (o->>'price')::numeric=j.max_price)
 ELSE EXISTS(SELECT 1 FROM jsonb_array_elements(coalesce(p_facts->'transactions','[]')) tx
  WHERE tx->>'player_id'=j.player_id AND tx->>'buyer_team_id'=p_context->>'user_team_id'
  AND (tx->>'created')::timestamptz>=j.dispatched_at-interval '2 seconds'
  AND (tx->>'price')::numeric=coalesce(j.actual_price,j.max_price)) END);
 UPDATE clause_jobs j SET state='cancelled',result_code='another_buyer_confirmed',reserved_amount=0,reserved_slot=0,
  finished_at=now(),updated_at=now(),owner=NULL,lease_until=NULL
 WHERE j.account_id=p_context->>'account_id' AND clause_same_team(j.context,p_context)
 AND j.state IN ('scheduled','preparing','uncertain') AND EXISTS(
  SELECT 1 FROM jsonb_array_elements(coalesce(p_facts->'transactions','[]')) tx WHERE tx->>'player_id'=j.player_id
  AND coalesce(tx->>'buyer_team_id','')<>p_context->>'user_team_id'
  AND (tx->>'created')::timestamptz>j.created_at);
 PERFORM clause_forget_if_idle(p_context->>'account_id');
END $$;
CREATE OR REPLACE FUNCTION public.clause_admit(p_id text,p_context jsonb,p_kind text,p_mode text,p_player jsonb,
 p_price numeric,p_facts jsonb,p_token text DEFAULT NULL,p_credential text DEFAULT NULL) RETURNS jsonb
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j clause_jobs; money numeric; slots integer; delta numeric; existing numeric:=0; opening timestamptz;
BEGIN
 PERFORM clause_check_facts(p_context,p_facts);PERFORM clause_team_lock(p_context);
 PERFORM clause_reconcile_reservations(p_context,p_facts);
 IF p_id !~ '^[a-f0-9]{32}$' OR p_kind NOT IN ('scheduled','manual') OR p_mode NOT IN ('bid','offer','modify','clause')
 OR p_price IS NULL OR p_price<=0 OR p_price>1e16 OR p_context->>'player_id' IS DISTINCT FROM p_player->>'id'
 OR p_context->>'player_id' IS DISTINCT FROM p_facts->'player'->>'id'
 THEN RAISE EXCEPTION 'clause_invalid_request'; END IF;
 SELECT * INTO j FROM clause_jobs WHERE id=p_id;
 IF FOUND THEN
  IF j.context IS DISTINCT FROM p_context OR j.max_price<>p_price OR j.kind<>p_kind THEN RAISE EXCEPTION 'clause_request_conflict'; END IF;
  RETURN to_jsonb(j);
 END IF;
 IF (SELECT count(*) FROM clause_jobs WHERE account_id=p_context->>'account_id' AND state IN ('scheduled','preparing','dispatched','uncertain','accepted'))>=256 THEN RAISE EXCEPTION 'clause_queue_capacity'; END IF;
 IF EXISTS(SELECT 1 FROM clause_jobs WHERE account_id=p_context->>'account_id' AND clause_same_team(context,p_context)
 AND state IN ('preparing','dispatched','uncertain')) THEN RAISE EXCEPTION 'clause_team_busy'; END IF;
 IF p_kind='scheduled' THEN
  IF p_mode<>'clause' OR coalesce(p_facts->'player'->>'slug','')='' OR p_token IS NULL OR p_credential IS NULL OR length(p_token)>16384
  OR coalesce(p_player->>'owner_id','')='' OR p_player->>'owner_id'=p_context->>'user_team_id'
  OR p_player->>'owner_id' IS DISTINCT FROM p_facts->'player'->>'owner_id'
  OR p_facts->'player'->>'owner_verified' IS DISTINCT FROM 'true'
  OR p_facts->'player'->>'transferred' IS DISTINCT FROM 'false'
  OR (p_facts->'player'->>'price')::numeric>p_price
  THEN RAISE EXCEPTION 'clause_player_changed'; END IF;
  opening:=(p_facts->'player'->>'opens_at')::timestamptz;
  IF opening IS NULL OR opening<=now() THEN RAISE EXCEPTION 'clause_not_future'; END IF;
 ELSE opening:=now(); END IF;
 SELECT coalesce(sum(reserved_amount),0),coalesce(sum(reserved_slot),0) INTO money,slots FROM clause_jobs
 WHERE account_id=p_context->>'account_id' AND clause_same_team(context,p_context)
 AND state IN ('scheduled','preparing','dispatched','uncertain','accepted');
 IF p_mode='modify' THEN
  SELECT (o->>'price')::numeric INTO existing FROM jsonb_array_elements(p_facts->'offers') o WHERE o->>'player_id'=p_context->>'player_id' LIMIT 1;
  IF existing IS NULL THEN RAISE EXCEPTION 'clause_existing_offer_unverified'; END IF;
 END IF;
 delta:=greatest(0,p_price-existing);
 IF (p_facts->'roster'->>'effective_count')::integer+(p_facts->'outstanding'->>'count')::integer+slots
   >=(p_facts->'roster'->>'cap')::integer AND p_mode<>'modify' THEN RAISE EXCEPTION 'clause_offer_capacity'; END IF;
 IF delta+money>(p_facts->'funds'->>'spendable_budget')::numeric OR p_price>(p_facts->'funds'->>'api_bid_limit')::numeric
 THEN RAISE EXCEPTION 'clause_offer_funds'; END IF;
 IF p_kind='scheduled' THEN
  INSERT INTO clause_authorizations(account_id,encrypted_token,credential_id) VALUES(p_context->>'account_id',p_token,p_credential)
  ON CONFLICT(account_id) DO UPDATE SET encrypted_token=excluded.encrypted_token,credential_id=excluded.credential_id,
    enabled=true,auth_status='active',generation=clause_authorizations.generation+1,updated_at=now();
 END IF;
 INSERT INTO clause_jobs(id,account_id,context,kind,mode,player_id,player_slug,player_name,original_owner,max_price,reserved_amount,reserved_slot,
  opens_at,due_at,expires_at,state)
 VALUES(p_id,p_context->>'account_id',p_context,p_kind,p_mode,p_player->>'id',coalesce(p_facts->'player'->>'slug',''),left(coalesce(p_player->>'name','Player'),256),
 coalesce(p_player->>'owner_id',''),p_price,delta,CASE WHEN p_mode='modify' THEN 0 ELSE 1 END,
 opening,CASE WHEN p_kind='scheduled' THEN opening-interval '10 seconds' ELSE now() END,opening+interval '5 minutes',
 CASE WHEN p_kind='scheduled' THEN 'scheduled' ELSE 'preparing' END) RETURNING * INTO j;
 IF p_kind='manual' THEN UPDATE clause_jobs SET lease_until=now()+interval '60 seconds' WHERE id=p_id RETURNING * INTO j; END IF;
 RETURN to_jsonb(j);
END $$;
CREATE OR REPLACE FUNCTION public.clause_read(p_account text) RETURNS jsonb
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE clause_jobs SET state='expired',result_code='opening_window_expired',reserved_amount=0,reserved_slot=0,
 finished_at=now(),updated_at=now(),owner=NULL,lease_until=NULL
 WHERE account_id=p_account AND kind='scheduled' AND state IN ('scheduled','preparing') AND expires_at<now();
 PERFORM clause_forget_if_idle(p_account);
 RETURN jsonb_build_object('jobs',coalesce((SELECT jsonb_agg(to_jsonb(j)-'owner'-'lease_until' ORDER BY created_at) FROM (
 SELECT * FROM clause_jobs WHERE account_id=p_account AND state IN ('scheduled','preparing','dispatched','uncertain','accepted')
 UNION ALL
 (SELECT * FROM clause_jobs WHERE account_id=p_account AND kind='scheduled' AND acknowledged_at IS NULL
 AND state NOT IN ('scheduled','preparing','dispatched','uncertain','accepted') ORDER BY finished_at DESC LIMIT 100)
 UNION ALL
 (SELECT * FROM clause_jobs WHERE account_id=p_account AND kind='scheduled' AND acknowledged_at IS NOT NULL
 AND state='succeeded' AND finished_at>now()-interval '7 days' ORDER BY finished_at DESC LIMIT 100)
 ) j),'[]'::jsonb),
 'more_outcomes',(SELECT count(*)>100 FROM clause_jobs WHERE account_id=p_account AND kind='scheduled' AND acknowledged_at IS NULL
 AND state NOT IN ('scheduled','preparing','dispatched','uncertain','accepted')),
 'health',(SELECT to_jsonb(h) FROM worker_health h WHERE status='running' ORDER BY heartbeat DESC LIMIT 1));
END $$;
CREATE OR REPLACE FUNCTION public.clause_cancel(p_account text,p_id text) RETURNS boolean
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE ctx jsonb;changed boolean;
BEGIN
 SELECT context INTO ctx FROM clause_jobs WHERE id=p_id AND account_id=p_account;PERFORM clause_team_lock(ctx);
 UPDATE clause_jobs SET state='cancelled',result_code='user_cancelled',reserved_amount=0,reserved_slot=0,finished_at=now(),updated_at=now()
 WHERE id=p_id AND account_id=p_account AND state IN ('scheduled','preparing');
 changed:=FOUND;PERFORM clause_forget_if_idle(p_account);RETURN changed;
END $$;
CREATE OR REPLACE FUNCTION public.clause_ack(p_account text,p_id text) RETURNS boolean
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE clause_jobs SET acknowledged_at=now() WHERE id=p_id AND account_id=p_account AND kind='scheduled'
 AND state IN ('accepted','succeeded','failed','cancelled','expired','uncertain');RETURN FOUND;
END $$;
CREATE OR REPLACE FUNCTION public.clause_renew(p_account text,p_token text,p_credential text) RETURNS boolean
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE clause_authorizations SET encrypted_token=p_token,credential_id=p_credential,auth_status='active',updated_at=now(),generation=generation+1
 WHERE account_id=p_account AND (enabled OR EXISTS(SELECT 1 FROM clause_jobs WHERE account_id=p_account AND state IN ('dispatched','uncertain','accepted'))) AND EXISTS(SELECT 1 FROM clause_jobs WHERE account_id=p_account
 AND state IN ('scheduled','preparing','dispatched','uncertain','accepted'));RETURN FOUND;
END $$;
CREATE OR REPLACE FUNCTION public.clause_revoke(p_account text) RETURNS void
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE clause_authorizations SET enabled=false,auth_status=CASE WHEN EXISTS(SELECT 1 FROM clause_jobs WHERE account_id=p_account AND state IN ('dispatched','uncertain','accepted')) THEN 'active' ELSE 'revoked' END,
 encrypted_token=CASE WHEN EXISTS(SELECT 1 FROM clause_jobs WHERE account_id=p_account AND state IN ('dispatched','uncertain','accepted')) THEN encrypted_token ELSE NULL END,generation=generation+1 WHERE account_id=p_account;
 UPDATE clause_jobs SET state='cancelled',result_code='authorization_revoked',reserved_amount=0,reserved_slot=0,finished_at=now(),updated_at=now()
 WHERE account_id=p_account AND kind='scheduled' AND state IN ('scheduled','preparing');
END $$;
CREATE OR REPLACE FUNCTION public.clause_guard(p_id text,p_owner text) RETURNS boolean
 LANGUAGE sql SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT EXISTS(SELECT 1 FROM clause_jobs j JOIN clause_authorizations a USING(account_id)
 WHERE j.id=p_id AND j.owner=p_owner AND j.lease_until>now() AND j.state IN ('preparing','dispatched','uncertain','accepted')
 AND (a.enabled OR j.state IN ('dispatched','uncertain','accepted')) AND a.auth_status='active' AND a.encrypted_token IS NOT NULL
 AND EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()));
$$;
CREATE OR REPLACE FUNCTION public.clause_claim(p_owner text) RETURNS jsonb
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j clause_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 UPDATE clause_jobs SET state='failed',result_code='dispatch_not_sent',reserved_amount=0,reserved_slot=0,finished_at=now()
 WHERE kind='manual' AND state='preparing' AND lease_until<now();
 -- A lease loss after dispatch authorizes reconciliation only, never replay.
 UPDATE clause_jobs SET state='uncertain',result_code='dispatch_interrupted',due_at=now(),owner=NULL,lease_until=NULL,updated_at=now()
 WHERE state='dispatched' AND lease_until<now();
 SELECT q.* INTO j FROM clause_jobs q JOIN clause_authorizations a USING(account_id)
 WHERE q.kind='scheduled' AND q.state IN ('scheduled','preparing','uncertain','accepted') AND q.due_at<=now()
 AND (q.lease_until IS NULL OR q.lease_until<now()) AND (a.enabled OR q.state IN ('dispatched','uncertain','accepted')) AND a.auth_status='active' AND a.encrypted_token IS NOT NULL
 ORDER BY q.due_at LIMIT 1 FOR UPDATE OF q SKIP LOCKED;
 IF j.id IS NULL THEN RETURN NULL; END IF;
 PERFORM clause_team_lock(j.context);
 IF EXISTS(SELECT 1 FROM clause_jobs x WHERE x.id<>j.id AND clause_same_team(x.context,j.context)
 AND x.state IN ('preparing','dispatched') AND x.lease_until>now()) THEN RETURN NULL; END IF;
 UPDATE clause_jobs SET state=CASE WHEN state='scheduled' THEN 'preparing' ELSE state END,
 reconciliation_attempts=CASE WHEN state IN ('uncertain','accepted') THEN reconciliation_attempts+1 ELSE reconciliation_attempts END,
 owner=p_owner,lease_until=now()+interval '60 seconds',updated_at=now() WHERE id=j.id RETURNING * INTO j;
 RETURN to_jsonb(j);
END $$;
CREATE OR REPLACE FUNCTION public.clause_touch(p_id text,p_owner text) RETURNS boolean
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 IF NOT clause_guard(p_id,p_owner) THEN RETURN false; END IF;
 UPDATE clause_jobs SET lease_until=now()+interval '60 seconds' WHERE id=p_id;RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.clause_dispatch(p_id text,p_owner text,p_facts jsonb) RETURNS boolean
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j clause_jobs; money numeric; slots integer; spend numeric;
BEGIN
 SELECT * INTO j FROM clause_jobs WHERE id=p_id;PERFORM clause_team_lock(j.context);
 IF j.kind='scheduled' AND (NOT clause_guard(p_id,p_owner) OR NOT EXISTS(SELECT 1 FROM clause_authorizations WHERE account_id=j.account_id AND enabled)) THEN RETURN false; END IF;
 IF j.state<>'preparing' OR j.expires_at<now() THEN RETURN false; END IF;
 PERFORM clause_check_facts(j.context,p_facts);PERFORM clause_reconcile_reservations(j.context,p_facts);
 SELECT coalesce(sum(reserved_amount),0),coalesce(sum(reserved_slot),0) INTO money,slots FROM clause_jobs
 WHERE id<>p_id AND account_id=j.account_id AND clause_same_team(context,j.context)
 AND state IN ('scheduled','preparing','dispatched','uncertain','accepted');
 spend:=CASE WHEN j.kind='scheduled' THEN (p_facts->'player'->>'price')::numeric ELSE j.reserved_amount END;
 IF spend IS NULL OR spend<=0 AND j.mode<>'modify' THEN RETURN false; END IF;
 IF (j.mode<>'modify' AND (p_facts->'roster'->>'effective_count')::integer+(p_facts->'outstanding'->>'count')::integer+slots+j.reserved_slot
  >(p_facts->'roster'->>'cap')::integer)
 OR (spend>0 AND spend+money>(p_facts->'funds'->>'spendable_budget')::numeric)
 OR (CASE WHEN j.kind='scheduled' THEN spend ELSE j.max_price END)>(p_facts->'funds'->>'api_bid_limit')::numeric THEN RETURN false; END IF;
 IF j.kind='scheduled' AND (j.opens_at>now() OR p_facts->'player'->>'owner_id' IS DISTINCT FROM j.original_owner
 OR p_facts->'player'->>'transferred' IS DISTINCT FROM 'false'
 OR (p_facts->'player'->>'price')::numeric IS NULL OR (p_facts->'player'->>'price')::numeric>j.max_price
 OR (p_facts->'player'->>'opens_at')::timestamptz>now()) THEN RETURN false; END IF;
 UPDATE clause_jobs SET state='dispatched',dispatched_at=now(),attempts=attempts+1,
 actual_price=CASE WHEN kind='scheduled' THEN (p_facts->'player'->>'price')::numeric ELSE max_price END,
 lease_until=now()+interval '60 seconds',updated_at=now() WHERE id=p_id AND state='preparing';
 IF NOT FOUND THEN RETURN false; END IF;
 INSERT INTO clause_attempts(job_id,attempt) SELECT id,attempts FROM clause_jobs WHERE id=p_id;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.clause_settle(p_id text,p_owner text,p_state text,p_code text,p_due timestamptz DEFAULT NULL,p_open timestamptz DEFAULT NULL) RETURNS boolean
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j clause_jobs;
BEGIN
 SELECT * INTO j FROM clause_jobs WHERE id=p_id FOR UPDATE;
 IF j.kind='scheduled' AND j.owner IS DISTINCT FROM p_owner THEN RETURN false; END IF;
 IF j.state NOT IN ('preparing','dispatched','uncertain','accepted') OR p_state NOT IN ('scheduled','uncertain','accepted','succeeded','failed','cancelled','expired')
 OR p_code !~ '^[a-zA-Z0-9_.:]{0,160}$' THEN RETURN false; END IF;
 -- Only a proven rejection may return a dispatched job to the queue.
 IF j.state='dispatched' AND p_state='scheduled' AND p_code NOT LIKE 'closed_verified:%' THEN RETURN false; END IF;
 UPDATE clause_attempts SET result_code=p_code,finished_at=now() WHERE job_id=p_id AND attempt=j.attempts AND finished_at IS NULL;
 UPDATE clause_jobs SET state=p_state,result_code=p_code,owner=NULL,lease_until=NULL,
 due_at=coalesce(p_due,now()+interval '30 seconds'),opens_at=coalesce(p_open,opens_at),
 expires_at=CASE WHEN p_open IS NULL THEN expires_at ELSE p_open+interval '5 minutes' END,
 reserved_amount=CASE WHEN p_state IN ('succeeded','failed','cancelled','expired') THEN 0 ELSE reserved_amount END,
 reserved_slot=CASE WHEN p_state IN ('succeeded','failed','cancelled','expired') THEN 0 ELSE reserved_slot END,
 finished_at=CASE WHEN p_state IN ('accepted','succeeded','failed','cancelled','expired') THEN now() ELSE finished_at END,
 acknowledged_at=CASE WHEN j.state IS DISTINCT FROM p_state THEN NULL ELSE acknowledged_at END,updated_at=now() WHERE id=p_id;
 PERFORM clause_forget_if_idle(j.account_id);
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.clause_write_fact(p_id text,p_owner text,
 p_table text,p_payload jsonb,p_conflict text,p_ignore boolean,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;cols text;conflicts text;updates text;clause text;field text;head text;j clause_jobs;p_account text;
BEGIN
 IF NOT clause_guard(p_id,p_owner) THEN RETURN false; END IF;
 SELECT * INTO j FROM clause_jobs WHERE id=p_id; p_account:=j.account_id;
 IF p_table<>ALL(ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history',
 'market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations',
 'market_observations','player_match_observations','league_rule_snapshots','player_round_history_checks',
 'rival_finance_evidence','complete_history_baselines','league_context_observations','player_membership_observations']) THEN RAISE EXCEPTION 'Invalid fact table'; END IF;
 -- Match PostgREST's default merge identity for mutable source catalogs.
 IF p_conflict IS NULL AND p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
  p_conflict:='id';
 END IF;
 IF p_observed IS NULL OR p_observed>now()+interval '60 seconds' THEN RAISE EXCEPTION 'Invalid source time'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
  IF jsonb_typeof(row_data)<>'object' OR octet_length(row_data::text)>1048576 THEN RAISE EXCEPTION 'Invalid fact row'; END IF;
  IF row_data ? 'account_id' AND row_data->>'account_id' IS DISTINCT FROM p_account THEN RAISE EXCEPTION 'Foreign fact account'; END IF;
  IF row_data ? 'championship_id' AND row_data->>'championship_id' IS DISTINCT FROM j.context->>'championship_id'
     THEN RAISE EXCEPTION 'Foreign fact league'; END IF;
  FOR field IN SELECT jsonb_object_keys(row_data) LOOP
   IF field ~* '(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)' OR NOT EXISTS(
    SELECT 1 FROM pg_attribute WHERE attrelid=('public.'||p_table)::regclass AND attname=field AND attnum>0 AND NOT attisdropped)
    THEN RAISE EXCEPTION 'Invalid fact field'; END IF;
  END LOOP;
  IF p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
   head:=NULL;
   IF coalesce(row_data->>'id','')='' THEN RAISE EXCEPTION 'Missing fact identity'; END IF;
   INSERT INTO worker_fact_heads(table_name,entity_id,observed_at) VALUES(p_table,row_data->>'id',p_observed)
   ON CONFLICT(table_name,entity_id) DO UPDATE SET observed_at=excluded.observed_at
   WHERE worker_fact_heads.observed_at<=excluded.observed_at RETURNING entity_id INTO head;
   IF head IS NULL THEN CONTINUE; END IF;
  END IF;
  SELECT string_agg(format('%I',key),',' ORDER BY key),string_agg(format('%I=excluded.%I',key,key),',' ORDER BY key)
  INTO cols,updates FROM jsonb_object_keys(row_data) AS key;
  clause:='';
  IF p_conflict IS NOT NULL THEN
   IF p_conflict !~ '^[a-z_]+(,[a-z_]+)*$' THEN RAISE EXCEPTION 'Invalid fact conflict'; END IF;
   SELECT string_agg(format('%I',v),',') INTO conflicts FROM unnest(string_to_array(p_conflict,',')) AS v;
   clause:=format(' ON CONFLICT (%s) DO %s',conflicts,CASE WHEN p_ignore THEN 'NOTHING' ELSE 'UPDATE SET '||updates END);
  END IF;
  EXECUTE format('INSERT INTO public.%I (%s) SELECT %s FROM jsonb_populate_record(NULL::public.%I,$1)%s',p_table,cols,cols,p_table,clause) USING row_data;
 END LOOP;
 RETURN true;
END $$;
REVOKE ALL ON FUNCTION public.clause_write_fact(text,text,text,jsonb,text,boolean,timestamptz) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.clause_write_fact(text,text,text,jsonb,text,boolean,timestamptz) TO service_role;


CREATE OR REPLACE FUNCTION public.clause_save_evidence(p_id text,p_owner text,p_key text,p_payload jsonb,p_observed timestamptz,p_until timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j clause_jobs;oldest text;
BEGIN
 IF NOT clause_guard(p_id,p_owner) THEN RETURN false; END IF;
 SELECT * INTO j FROM clause_jobs WHERE id=p_id;
 IF p_key !~ '^[a-f0-9]{64}$' OR p_observed IS NULL OR p_observed>now()+interval '2 seconds'
 OR p_until IS NULL OR p_until<p_observed OR p_until>now()+interval '1 day' OR octet_length(p_payload::text)>16777216
 THEN RAISE EXCEPTION 'clause_invalid_evidence'; END IF;
 PERFORM pg_advisory_xact_lock(hashtextextended('clause_evidence:'||j.account_id,0));
 INSERT INTO clause_evidence(account_id,evidence_key,context,payload,payload_bytes,observed_at,valid_until,complete)
 VALUES(j.account_id,p_key,j.context,p_payload,octet_length(p_payload::text),p_observed,p_until,true)
 ON CONFLICT(account_id,evidence_key) DO UPDATE SET context=excluded.context,payload=excluded.payload,payload_bytes=excluded.payload_bytes,
 observed_at=excluded.observed_at,valid_until=excluded.valid_until,complete=true;
 LOOP
  EXIT WHEN (SELECT count(*)<=256 AND coalesce(sum(payload_bytes),0)<=33554432 FROM clause_evidence WHERE account_id=j.account_id);
  SELECT evidence_key INTO oldest FROM clause_evidence WHERE account_id=j.account_id ORDER BY observed_at,evidence_key LIMIT 1;
  DELETE FROM clause_evidence WHERE account_id=j.account_id AND evidence_key=oldest;
 END LOOP;
 RETURN EXISTS(SELECT 1 FROM clause_evidence WHERE account_id=j.account_id AND evidence_key=p_key);
END $$;
CREATE OR REPLACE FUNCTION public.clause_cleanup() RETURNS void LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE clause_jobs SET state='expired',result_code='opening_window_expired',reserved_amount=0,reserved_slot=0,
 finished_at=now(),updated_at=now(),owner=NULL,lease_until=NULL
 WHERE kind='scheduled' AND state IN ('scheduled','preparing') AND expires_at<now();
 UPDATE clause_authorizations a SET encrypted_token=NULL,credential_id=NULL,enabled=false,updated_at=now()
 WHERE encrypted_token IS NOT NULL AND NOT EXISTS(SELECT 1 FROM clause_jobs j WHERE j.account_id=a.account_id
 AND j.kind='scheduled' AND j.state IN ('scheduled','preparing','dispatched','uncertain','accepted'));
 DELETE FROM clause_evidence WHERE valid_until<now()-interval '1 day';
 DELETE FROM clause_attempts WHERE job_id IN (SELECT id FROM clause_jobs WHERE acknowledged_at<now()-interval '90 days' AND state IN ('succeeded','failed','cancelled','expired'));
 DELETE FROM clause_jobs WHERE acknowledged_at<now()-interval '90 days' AND state IN ('succeeded','failed','cancelled','expired');
 DELETE FROM clause_attempts WHERE job_id IN (SELECT id FROM clause_jobs WHERE kind='manual' AND finished_at<now()-interval '7 days' AND state IN ('succeeded','failed','cancelled','expired'));
 DELETE FROM clause_jobs WHERE kind='manual' AND finished_at<now()-interval '7 days' AND state IN ('succeeded','failed','cancelled','expired');
END $$;


CREATE OR REPLACE FUNCTION public.clause_forget_if_idle(p_account text) RETURNS void
 LANGUAGE sql SECURITY INVOKER SET search_path=public,pg_temp AS $$
 UPDATE clause_authorizations a SET encrypted_token=NULL,credential_id=NULL,enabled=false,updated_at=now()
 WHERE account_id=p_account AND encrypted_token IS NOT NULL AND NOT EXISTS(SELECT 1 FROM clause_jobs j
 WHERE j.account_id=a.account_id AND j.kind='scheduled' AND j.state IN ('scheduled','preparing','dispatched','uncertain','accepted'));
$$;
CREATE OR REPLACE FUNCTION public.clause_publish(p_id text,p_owner text,p_descriptor jsonb,p_manifest jsonb) RETURNS boolean
 LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j clause_jobs;k text:=p_descriptor->>'resource_key';f text;
BEGIN
 IF NOT clause_guard(p_id,p_owner) THEN RETURN false; END IF;
 SELECT * INTO j FROM clause_jobs WHERE id=p_id;
 IF NOT EXISTS(SELECT 1 FROM worker_accounts WHERE account_id=j.account_id) THEN RETURN true; END IF;
 IF k !~ '^[a-f0-9]{64}$' OR p_descriptor->'context'->>'account_id' IS DISTINCT FROM j.account_id
 OR p_descriptor->>'handler' !~ '^(get_|read_)[a-z0-9_]+$'
 OR p_manifest->'context' IS DISTINCT FROM p_descriptor->'context' THEN RAISE EXCEPTION 'clause_publication_context'; END IF;
 IF coalesce(p_descriptor->'context'->>'championship_id','')<>'' THEN
  FOREACH f IN ARRAY ARRAY['championship_id','competition_id','season','scoring_version'] LOOP
   IF coalesce(p_descriptor->'context'->>f,'unknown') NOT IN ('','unknown') AND
   p_descriptor->'context'->>f IS DISTINCT FROM j.context->>f THEN RAISE EXCEPTION 'clause_publication_context'; END IF;
  END LOOP;
 END IF;
 IF (p_manifest->>'observed_at')::timestamptz>now()+interval '2 seconds'
 OR (p_manifest->>'checked_at')::timestamptz>(p_manifest->>'observed_at')::timestamptz
 OR (SELECT count(*) FROM worker_chunks WHERE account_id=j.account_id AND resource_key=k AND version=(p_manifest->>'version')::uuid)
 IS DISTINCT FROM (p_manifest->>'parts')::integer THEN RAISE EXCEPTION 'clause_publication_incomplete'; END IF;
 INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts)
 VALUES(j.account_id,k,p_descriptor->'context',p_descriptor->>'handler',(p_manifest->>'version')::uuid,
 (p_manifest->>'observed_at')::timestamptz,(p_manifest->>'checked_at')::timestamptz,(p_manifest->>'valid_until')::timestamptz,
 p_manifest->>'completeness',p_manifest->>'status',(p_manifest->>'json_bytes')::integer,p_manifest->>'sha256',(p_manifest->>'parts')::integer)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET context=excluded.context,handler=excluded.handler,version=excluded.version,
 observed_at=excluded.observed_at,checked_at=excluded.checked_at,valid_until=excluded.valid_until,completeness=excluded.completeness,
 status=excluded.status,json_bytes=excluded.json_bytes,sha256=excluded.sha256,parts=excluded.parts
 WHERE worker_snapshots.observed_at<=excluded.observed_at;
 RETURN true;
END $$;

-- Reuse already-acquired factual summaries; never poll distant scheduled jobs.
CREATE OR REPLACE FUNCTION public.clause_observe(p_account text,p_key text,p_owner text,p_generation bigint,p_context jsonb,p_player jsonb,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE source_job worker_jobs;f text;a text;
BEGIN
 IF NOT worker_guard(p_account,p_key,p_owner,p_generation) THEN RETURN false; END IF;
 SELECT * INTO source_job FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key;
 IF p_context->>'account_id' IS DISTINCT FROM p_account OR p_observed IS NULL OR p_observed>now()+interval '2 seconds'
 OR coalesce(p_player->>'id','')='' THEN RETURN false; END IF;
 FOREACH f IN ARRAY ARRAY['championship_id','competition_id','season','scoring_version'] LOOP
  IF coalesce(p_context->>f,'unknown') IN ('','unknown') OR p_context->>f IS DISTINCT FROM source_job.descriptor->'context'->>f THEN RETURN false; END IF;
 END LOOP;
 -- An incomplete summary is not evidence of an owner change, zero clause or unlock.
 IF p_player->>'owner_verified' IS DISTINCT FROM 'true' OR p_player->>'owner_id' IS NULL
 OR p_player->>'transferred' IS NULL OR p_player->>'transferred' NOT IN ('true','false')
 OR (p_player->>'price')::numeric IS NULL OR (p_player->>'price')::numeric<=0
 OR p_player->>'opens_at' IS NULL THEN RETURN false; END IF;
 FOR a IN
  UPDATE clause_jobs j SET
   state=CASE WHEN p_player->>'owner_id' IS DISTINCT FROM j.original_owner OR p_player->>'transferred'='true'
      OR (p_player->>'price')::numeric>j.max_price THEN 'cancelled' ELSE state END,
   result_code=CASE WHEN p_player->>'owner_id' IS DISTINCT FROM j.original_owner OR p_player->>'transferred'='true' THEN 'ownership_or_transfer_changed'
      WHEN (p_player->>'price')::numeric>j.max_price THEN 'price_exceeds_authorized_cap' ELSE result_code END,
   reserved_amount=CASE WHEN p_player->>'owner_id' IS DISTINCT FROM j.original_owner OR p_player->>'transferred'='true'
      OR (p_player->>'price')::numeric>j.max_price THEN 0 ELSE reserved_amount END,
   reserved_slot=CASE WHEN p_player->>'owner_id' IS DISTINCT FROM j.original_owner OR p_player->>'transferred'='true'
      OR (p_player->>'price')::numeric>j.max_price THEN 0 ELSE reserved_slot END,
   opens_at=(p_player->>'opens_at')::timestamptz,
   due_at=(p_player->>'opens_at')::timestamptz-interval '10 seconds',expires_at=(p_player->>'opens_at')::timestamptz+interval '5 minutes',
   player_observed_at=p_observed,updated_at=now(),
   finished_at=CASE WHEN p_player->>'owner_id' IS DISTINCT FROM j.original_owner OR p_player->>'transferred'='true'
      OR (p_player->>'price')::numeric>j.max_price THEN now() ELSE finished_at END
  WHERE j.account_id=p_account AND j.kind='scheduled' AND j.state IN ('scheduled','preparing')
  AND j.context->>'championship_id'=p_context->>'championship_id' AND j.context->>'competition_id'=p_context->>'competition_id'
  AND j.context->>'season'=p_context->>'season' AND j.context->>'scoring_version'=p_context->>'scoring_version'
  AND j.player_id=p_player->>'id' AND p_observed>j.player_observed_at
  RETURNING j.account_id
 LOOP PERFORM clause_forget_if_idle(a); END LOOP;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.clause_observe_transfer() RETURNS trigger
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a text;
BEGIN
 IF NEW.provenance NOT IN ('futmondo_pressroom','futmondo_locker') OR NEW.season='unknown' OR NEW.competition_id='unknown'
 OR NEW.transaction_date IS NULL THEN RETURN NEW; END IF;
 FOR a IN
  UPDATE clause_jobs j SET state='cancelled',result_code='another_buyer_confirmed',reserved_amount=0,reserved_slot=0,
   finished_at=now(),updated_at=now()
  WHERE j.kind='scheduled' AND j.state IN ('scheduled','preparing') AND j.player_id=NEW.player_id
  AND j.context->>'championship_id'=NEW.championship_id AND j.context->>'competition_id'=NEW.competition_id
  AND j.context->>'season'=NEW.season AND coalesce(NEW.buyer_team_id,'')<>j.context->>'user_team_id'
  AND NEW.transaction_date>j.created_at RETURNING j.account_id
 LOOP PERFORM clause_forget_if_idle(a); END LOOP;
 RETURN NEW;
END $$;
CREATE INDEX IF NOT EXISTS clause_jobs_transfer_guard ON public.clause_jobs
 ((context->>'championship_id'),(context->>'competition_id'),(context->>'season'),player_id)
 WHERE kind='scheduled' AND state IN ('scheduled','preparing');
DROP TRIGGER IF EXISTS clause_observed_transfer ON public.market_transactions;
CREATE TRIGGER clause_observed_transfer AFTER INSERT OR UPDATE ON public.market_transactions
 FOR EACH ROW EXECUTE FUNCTION public.clause_observe_transfer();

CREATE OR REPLACE FUNCTION public.clause_retire_context(p_context jsonb,p_observed timestamptz) RETURNS integer
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE n integer;f text;
BEGIN
 FOREACH f IN ARRAY ARRAY['account_id','championship_id','competition_id','season','scoring_version'] LOOP
  IF coalesce(p_context->>f,'unknown') IN ('','unknown') THEN RETURN 0; END IF;
 END LOOP;
 IF p_observed IS NULL OR p_observed>now()+interval '2 seconds' THEN RETURN 0; END IF;
 UPDATE clause_jobs SET state='cancelled',result_code='context_superseded',reserved_amount=0,reserved_slot=0,
 finished_at=now(),updated_at=now()
 WHERE account_id=p_context->>'account_id' AND context->>'championship_id'=p_context->>'championship_id'
 AND kind='scheduled' AND state IN ('scheduled','preparing') AND player_observed_at<p_observed
 AND (context->>'competition_id' IS DISTINCT FROM p_context->>'competition_id' OR context->>'season' IS DISTINCT FROM p_context->>'season'
 OR context->>'scoring_version' IS DISTINCT FROM p_context->>'scoring_version');
 GET DIAGNOSTICS n=ROW_COUNT;
 PERFORM clause_forget_if_idle(p_context->>'account_id');RETURN n;
END $$;

DO $$ DECLARE t text; f record; BEGIN
 FOREACH t IN ARRAY ARRAY['clause_authorizations','clause_jobs','clause_attempts','clause_evidence'] LOOP
  EXECUTE format('ALTER TABLE public.%I ENABLE ROW LEVEL SECURITY',t);
  EXECUTE format('REVOKE ALL ON public.%I FROM PUBLIC,anon,authenticated',t);
  EXECUTE format('GRANT SELECT,INSERT,UPDATE,DELETE ON public.%I TO service_role',t);
 END LOOP;
 FOR f IN SELECT oid::regprocedure signature FROM pg_proc WHERE pronamespace='public'::regnamespace AND proname LIKE 'clause_%' LOOP
  EXECUTE format('REVOKE ALL ON FUNCTION %s FROM PUBLIC,anon,authenticated',f.signature);
  EXECUTE format('GRANT EXECUTE ON FUNCTION %s TO service_role',f.signature);
 END LOOP;
END $$;
COMMIT;

-- Automatic synchronization; display reads never own acquisition.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_bootstrap(p_account text)
RETURNS jsonb LANGUAGE sql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT public.worker_read(p_account,jsonb_build_object(d->>'resource_key',''),jsonb_build_object(d->>'resource_key',d))
 || jsonb_build_object('preferred_league',(SELECT jsonb_build_object('championship_id',championship_id,'selected_at',selected_at)
 FROM public.user_league_preferences WHERE user_id=p_account))
 FROM (SELECT public.worker_account_descriptor(p_account) d) descriptor;
$$;
CREATE OR REPLACE FUNCTION public.worker_connect(p_account text,p_token text,p_credential text,p_generation bigint,p_timezone text DEFAULT 'UTC')
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;
BEGIN
 IF p_account IS NULL OR length(p_account) NOT BETWEEN 1 AND 256 OR p_token IS NULL OR length(p_token) NOT BETWEEN 1 AND 65536
 OR p_credential IS NULL OR length(p_credential) NOT BETWEEN 1 AND 128 OR p_generation IS NULL OR p_generation<0 THEN RAISE EXCEPTION 'Invalid connection'; END IF;
 IF NOT EXISTS(SELECT 1 FROM pg_timezone_names WHERE name=p_timezone) THEN RAISE EXCEPTION 'Invalid timezone'; END IF;
 IF p_generation=0 THEN
  INSERT INTO worker_accounts(account_id,enabled,encrypted_token,credential_id,timezone,quiet_start,quiet_end)
  VALUES(p_account,true,p_token,p_credential,p_timezone,0,0) ON CONFLICT DO NOTHING;
 END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR UPDATE;
 IF a.account_id IS NULL OR (a.generation<>p_generation AND NOT(p_generation=0 AND a.generation=1 AND a.enabled)) THEN RETURN NULL; END IF;
 -- Token rotation is not authorization revocation. Preserve every running job.
 UPDATE worker_accounts SET enabled=true,encrypted_token=p_token,credential_id=p_credential,
  auth_status='active',quiet_start=0,quiet_end=0,updated_at=now() WHERE account_id=p_account RETURNING * INTO a;
 UPDATE worker_jobs SET due_at=least(due_at,now()),last_error=NULL,attempts=0
 WHERE account_id=p_account AND generation=a.generation AND owner IS NULL AND last_error='source_authentication';
 RETURN to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues';
END $$;
CREATE OR REPLACE FUNCTION public.worker_preferences(p_account text,p_generation bigint,p_excluded jsonb)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;
BEGIN
 IF jsonb_typeof(p_excluded) IS DISTINCT FROM 'array' OR octet_length(p_excluded::text)>65536 THEN RAISE EXCEPTION 'Invalid leagues'; END IF;
 IF EXISTS(SELECT 1 FROM jsonb_array_elements(p_excluded) v WHERE jsonb_typeof(v)<>'string') THEN RAISE EXCEPTION 'Invalid league'; END IF;
 UPDATE worker_accounts SET excluded_leagues=p_excluded,quiet_start=0,quiet_end=0,updated_at=now()
 WHERE account_id=p_account AND generation=p_generation RETURNING * INTO a;
 IF a.account_id IS NULL THEN RETURN NULL; END IF;
 DELETE FROM worker_jobs WHERE account_id=p_account AND p_excluded ? (descriptor->'context'->>'championship_id');
 RETURN to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues';
END $$;
CREATE OR REPLACE FUNCTION public.worker_disconnect(p_account text,p_generation bigint)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;
BEGIN
 UPDATE worker_accounts SET enabled=false,encrypted_token=NULL,credential_id=NULL,generation=generation+1,updated_at=now()
 WHERE account_id=p_account AND generation=p_generation RETURNING * INTO a;
 IF a.account_id IS NULL THEN RETURN NULL; END IF;
 DELETE FROM worker_jobs WHERE account_id=p_account;
 RETURN to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues';
END $$;
-- Old images can renew safely during rollout. A delayed renewal cannot re-enable revocation.
CREATE OR REPLACE FUNCTION public.worker_renew(p_account text,p_token text,p_credential text DEFAULT NULL)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account AND enabled FOR UPDATE;
 IF a.account_id IS NULL THEN RETURN NULL; END IF;
 RETURN public.worker_connect(p_account,p_token,p_credential,a.generation,a.timezone);
END $$;
CREATE OR REPLACE FUNCTION public.worker_authorize(p_account text,p_enabled boolean,p_token text DEFAULT NULL,
 p_timezone text DEFAULT 'UTC',p_start integer DEFAULT 0,p_end integer DEFAULT 7,p_excluded jsonb DEFAULT '[]',p_credential text DEFAULT NULL)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;result jsonb;
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR UPDATE;
 IF NOT p_enabled THEN RETURN public.worker_disconnect(p_account,a.generation); END IF;
 result:=public.worker_connect(p_account,p_token,p_credential,coalesce(a.generation,0),p_timezone);
 IF result IS NULL THEN RETURN NULL; END IF;
 RETURN public.worker_preferences(p_account,(result->>'generation')::bigint,p_excluded);
END $$;
CREATE OR REPLACE FUNCTION public.worker_prepare_source(p_account text,p_key text,p_owner text,p_generation bigint,p_request text)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;j worker_jobs;
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT public.worker_guard(p_account,p_key,p_owner,p_generation) THEN RETURN NULL; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key;
 IF a.excluded_leagues ? (j.descriptor->'context'->>'championship_id') THEN RETURN NULL; END IF;
 RETURN jsonb_build_object('account',to_jsonb(a),'checkpoint',(SELECT response FROM worker_checkpoints
 WHERE account_id=p_account AND resource_key=p_key AND run_id=j.run_id AND request_key=p_request));
END $$;
-- Already-guarded writes do not need a separate HTTP guard before this operation.
CREATE OR REPLACE FUNCTION public.worker_guard(p_account text,p_key text,p_owner text,p_generation bigint,p_checkpoint jsonb DEFAULT NULL)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE worker_jobs j SET lease_until=now()+interval '60 seconds',checkpoint=coalesce(p_checkpoint,j.checkpoint)
 FROM worker_accounts a WHERE j.account_id=p_account AND j.resource_key=p_key AND j.owner=p_owner
 AND j.lease_until>now() AND a.account_id=j.account_id AND a.enabled AND a.auth_status='active'
 AND a.generation=p_generation AND j.generation=p_generation
 AND NOT(a.excluded_leagues ? (j.descriptor->'context'->>'championship_id'))
 AND public.worker_finance_context_matches(j.descriptor,a.verified_leagues->(j.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=j.descriptor->'context'->>'championship_id')
 AND EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now());
 RETURN FOUND;
END $$;
CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;candidate worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 FOR candidate IN
 WITH dispatched AS MATERIALIZED (
  SELECT account_id,descriptor->>'handler' handler,max(started_at) served_at FROM worker_jobs GROUP BY 1,2)
 SELECT q.* FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 JOIN dispatched d ON d.account_id=q.account_id AND d.handler=q.descriptor->>'handler'
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND (q.owner IS NULL OR q.lease_until<now())
 AND (NOT p_foreground OR q.descriptor->>'handler' NOT IN
   ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot'))
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
 AND (q.checked_at IS NULL OR q.checked_at + make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE 0 END)<=now())
 -- The short lane prefers core observations, then helps other short reads. The other lane gives each due
 -- query family a turn, so a recurring minute job or 527 player jobs cannot
 -- indefinitely exclude catalogs, history or accounting. One-lane installs
 -- retain the same family fairness across core and background work.
 ORDER BY CASE WHEN p_foreground AND (public.worker_request_priority(q.descriptor->>'handler')>=9 OR q.descriptor->>'handler'='get_clause_transfer_summary') THEN 0 ELSE 1 END,
 d.served_at NULLS FIRST,a.last_dispatched NULLS FIRST,
 CASE WHEN p_foreground AND q.interval_seconds<=60 THEN 0 ELSE 1 END,q.due_at,q.account_id,q.resource_key
 LOOP
 IF NOT public.worker_finance_context_matches(candidate.descriptor,(SELECT verified_leagues->(candidate.descriptor->'context'->>'championship_id') FROM worker_accounts WHERE account_id=candidate.account_id))
 OR public.worker_has_resolved_peer(candidate.account_id,candidate.descriptor) THEN CONTINUE; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=candidate.account_id AND resource_key=candidate.resource_key
 AND generation=candidate.generation AND descriptor=candidate.descriptor AND due_at<=now() AND (owner IS NULL OR lease_until<now()) FOR UPDATE SKIP LOCKED;
 IF j.account_id IS NULL THEN CONTINUE; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
 END LOOP;
 RETURN NULL;
END $$;
ALTER TABLE public.worker_accounts ALTER COLUMN quiet_end SET DEFAULT 0;
UPDATE public.worker_accounts SET quiet_start=0,quiet_end=0 WHERE enabled AND (quiet_start<>0 OR quiet_end<>0);
REVOKE ALL ON FUNCTION public.worker_bootstrap(text),public.worker_connect(text,text,text,bigint,text),
 public.worker_preferences(text,bigint,jsonb),public.worker_disconnect(text,bigint),
 public.worker_prepare_source(text,text,text,bigint,text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_bootstrap(text),public.worker_connect(text,text,text,bigint,text),
 public.worker_preferences(text,bigint,jsonb),public.worker_disconnect(text,bigint),
 public.worker_prepare_source(text,text,text,bigint,text) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Worker-owned cadence, durable login refresh, and combined maintenance.
BEGIN;
CREATE TABLE public.worker_schedule_policy(id integer PRIMARY KEY CHECK(id=1),base_interval integer NOT NULL CHECK(base_interval>0));
INSERT INTO public.worker_schedule_policy VALUES(1,1800);
ALTER TABLE public.worker_schedule_policy ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.worker_schedule_policy FROM PUBLIC,anon,authenticated;
GRANT ALL ON public.worker_schedule_policy TO service_role;
ALTER TABLE public.worker_jobs DROP CONSTRAINT worker_jobs_interval_seconds_check;
ALTER TABLE public.worker_jobs ADD CHECK(interval_seconds>=15);
ALTER TABLE public.worker_jobs ADD COLUMN native_interval_seconds integer;
UPDATE public.worker_jobs SET native_interval_seconds=interval_seconds;
ALTER TABLE public.worker_jobs ALTER COLUMN native_interval_seconds SET NOT NULL;
ALTER TABLE public.worker_accounts ADD COLUMN last_login_refresh timestamptz;
CREATE FUNCTION public.worker_interval_floor() RETURNS trigger LANGUAGE plpgsql SET search_path=public,pg_temp AS $$
DECLARE base integer;
BEGIN
 SELECT base_interval INTO base FROM worker_schedule_policy WHERE id=1;
 IF TG_OP='INSERT' THEN NEW.native_interval_seconds:=NEW.interval_seconds;
 ELSIF NEW.native_interval_seconds IS NOT DISTINCT FROM OLD.native_interval_seconds AND NEW.interval_seconds<>OLD.interval_seconds AND NEW.interval_seconds<>greatest(OLD.native_interval_seconds,base) THEN
 NEW.native_interval_seconds:=NEW.interval_seconds;
 END IF;
 NEW.interval_seconds:=greatest(NEW.native_interval_seconds,base);
 RETURN NEW;
END $$;
CREATE TRIGGER worker_interval_floor BEFORE INSERT OR UPDATE OF interval_seconds ON public.worker_jobs FOR EACH ROW EXECUTE FUNCTION public.worker_interval_floor();
CREATE OR REPLACE FUNCTION public.worker_next_refresh_due(p_handler text,p_interval integer,p_valid_until timestamptz,p_started timestamptz,p_now timestamptz DEFAULT now())
RETURNS timestamptz LANGUAGE sql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT p_now+make_interval(secs=>greatest(p_interval,(SELECT base_interval FROM worker_schedule_policy WHERE id=1)));
$$;
CREATE FUNCTION public.worker_login_refresh(p_account text) RETURNS void LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE worker_accounts SET last_login_refresh=now() WHERE account_id=p_account AND enabled AND auth_status='active'
 AND (last_login_refresh IS NULL OR last_login_refresh<=now()-interval '5 minutes');
 IF NOT FOUND THEN RETURN; END IF;
 UPDATE worker_jobs j SET due_at=least(j.due_at,now()),priority=greatest(j.priority,10)
 FROM worker_accounts a WHERE j.account_id=p_account AND a.account_id=j.account_id AND j.generation=a.generation
 AND j.owner IS NULL AND j.last_error IS NULL
 AND NOT(a.excluded_leagues ? coalesce(j.descriptor->'context'->>'championship_id',''))
 AND NOT EXISTS(SELECT 1 FROM blocked_leagues b WHERE b.championship_id=j.descriptor->'context'->>'championship_id')
 AND (j.native_interval_seconds<=300 AND j.descriptor->>'handler' NOT IN ('get_player_full_profile','get_player_matches','get_player_summary'));
END $$;
CREATE OR REPLACE FUNCTION public.worker_connect(p_account text,p_token text,p_credential text,p_generation bigint,p_timezone text DEFAULT 'UTC')
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;
BEGIN
 IF p_account IS NULL OR length(p_account) NOT BETWEEN 1 AND 256 OR p_token IS NULL OR length(p_token) NOT BETWEEN 1 AND 65536
 OR p_credential IS NULL OR length(p_credential) NOT BETWEEN 1 AND 128 OR p_generation IS NULL OR p_generation<0 THEN RAISE EXCEPTION 'Invalid connection'; END IF;
 IF NOT EXISTS(SELECT 1 FROM pg_timezone_names WHERE name=p_timezone) THEN RAISE EXCEPTION 'Invalid timezone'; END IF;
 IF p_generation=0 THEN
  INSERT INTO worker_accounts(account_id,enabled,encrypted_token,credential_id,timezone,quiet_start,quiet_end)
  VALUES(p_account,true,p_token,p_credential,p_timezone,0,0) ON CONFLICT DO NOTHING;
 END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR UPDATE;
 IF a.account_id IS NULL OR (a.generation<>p_generation AND NOT(p_generation=0 AND a.generation=1 AND a.enabled)) THEN RETURN NULL; END IF;
 -- Token rotation is not authorization revocation. Preserve every running job.
 UPDATE worker_accounts SET enabled=true,encrypted_token=p_token,credential_id=p_credential,
  auth_status='active',quiet_start=0,quiet_end=0,updated_at=now() WHERE account_id=p_account RETURNING * INTO a;
 UPDATE worker_jobs SET due_at=least(due_at,now()),last_error=NULL,attempts=0
 WHERE account_id=p_account AND generation=a.generation AND owner IS NULL AND last_error='source_authentication';
 PERFORM public.worker_login_refresh(p_account);
 RETURN to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues';
END $$;

CREATE OR REPLACE FUNCTION public.worker_maintenance(p_owner text,p_base_interval integer,p_requests bigint,p_jobs bigint) RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE elected boolean; admitted boolean; old_base integer;
BEGIN
 IF p_base_interval<60 THEN RAISE EXCEPTION 'Invalid worker base interval'; END IF;
 INSERT INTO worker_leader(id,owner,lease_until) VALUES(1,p_owner,now()+interval '180 seconds')
 ON CONFLICT(id) DO UPDATE SET owner=excluded.owner,lease_until=excluded.lease_until
 WHERE worker_leader.owner=p_owner OR worker_leader.lease_until<now();
 elected:=FOUND;
 IF elected THEN
 SELECT base_interval INTO old_base FROM worker_schedule_policy WHERE id=1 FOR UPDATE;
 IF old_base<>p_base_interval THEN
 UPDATE worker_schedule_policy SET base_interval=p_base_interval WHERE id=1;
 UPDATE worker_jobs SET interval_seconds=greatest(native_interval_seconds,p_base_interval),
 due_at=CASE WHEN owner IS NULL AND last_error IS NULL THEN now()+make_interval(secs=>1+mod(abs(hashtext(resource_key)::bigint),greatest(native_interval_seconds,p_base_interval))) ELSE due_at END
 WHERE interval_seconds IS DISTINCT FROM greatest(native_interval_seconds,p_base_interval);
 END IF;
 END IF;
 SELECT coalesce((SELECT NOT admin_only FROM application_access WHERE id='main'),false) INTO admitted;
 INSERT INTO worker_health(id,status,heartbeat,requests,jobs) VALUES(p_owner,CASE WHEN elected THEN 'running' ELSE 'standby' END,now(),p_requests,p_jobs)
 ON CONFLICT(id) DO UPDATE SET status=excluded.status,heartbeat=excluded.heartbeat,requests=excluded.requests,jobs=excluded.jobs;
 RETURN jsonb_build_object('leader',elected,'admitted',admitted);
END $$;
UPDATE worker_jobs SET interval_seconds=greatest(native_interval_seconds,1800),
 due_at=CASE WHEN owner IS NULL AND last_error IS NULL THEN now()+make_interval(secs=>1+mod(abs(hashtext(resource_key)::bigint),greatest(native_interval_seconds,1800))) ELSE due_at END;
REVOKE ALL ON FUNCTION public.worker_interval_floor(),public.worker_login_refresh(text),public.worker_maintenance(text,integer,bigint,bigint) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_interval_floor(),public.worker_login_refresh(text),public.worker_maintenance(text,integer,bigint,bigint) TO service_role;
CREATE OR REPLACE FUNCTION public.worker_enqueue(p_account text,p_descriptor jsonb,p_interval integer DEFAULT 60,p_priority integer DEFAULT 10)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;k text:=p_descriptor->>'resource_key';
BEGIN
 -- Browser processes from the previous image may still send the old default.
 IF p_descriptor->>'handler'='get_player_recent_points' AND p_interval=60 THEN p_interval:=300; END IF;
 IF p_descriptor->>'handler'='get_clause_transfer_summary' THEN p_interval:=300; END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' THEN RETURN false; END IF;
 IF NOT public.worker_finance_context_matches(p_descriptor,a.verified_leagues->(p_descriptor->'context'->>'championship_id')) THEN RETURN false; END IF;
 IF EXISTS(SELECT 1 FROM public.blocked_leagues WHERE championship_id=p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF p_descriptor->'context'->>'account_id' IS DISTINCT FROM p_account OR (p_descriptor->>'handler') !~ '^(get_|read_|fm_worker_finances$)' THEN RAISE EXCEPTION 'Invalid descriptor'; END IF;
 IF a.excluded_leagues ? (p_descriptor->'context'->>'championship_id') THEN RETURN false; END IF;
 IF NOT EXISTS(SELECT 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=k) AND
 (SELECT count(*) FROM worker_jobs WHERE account_id=p_account)>=10000 THEN RAISE EXCEPTION 'Worker job limit'; END IF;
 INSERT INTO worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds,priority,base_priority)
 VALUES(p_account,k,p_descriptor,a.generation,p_interval,greatest(p_priority,CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END),CASE WHEN p_interval<=60 THEN 4 WHEN p_interval<=300 THEN 3 ELSE 1 END)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET
 due_at=CASE WHEN worker_jobs.owner IS NULL AND excluded.priority>=5 THEN least(worker_jobs.due_at,now())
  -- A newly observed roster/transfer can shorten an existing hourly summary
  -- cadence. Apply that cadence from its last completion instead of keeping
  -- the old hourly due date. Rediscovery at the same cadence is inert, and
  -- failed or running work retains its retry/lease schedule.
  WHEN worker_jobs.owner IS NULL AND worker_jobs.last_error IS NULL
   AND worker_jobs.checked_at IS NOT NULL AND excluded.interval_seconds<worker_jobs.interval_seconds
  THEN least(worker_jobs.due_at,worker_jobs.checked_at+make_interval(secs=>excluded.interval_seconds))
  ELSE worker_jobs.due_at END,
 requested_at=CASE WHEN excluded.priority>=5 THEN now() ELSE worker_jobs.requested_at END,priority=greatest(worker_jobs.priority,excluded.priority),base_priority=greatest(worker_jobs.base_priority,excluded.base_priority),native_interval_seconds=least(worker_jobs.native_interval_seconds,excluded.native_interval_seconds),interval_seconds=greatest(least(worker_jobs.native_interval_seconds,excluded.native_interval_seconds),(SELECT base_interval FROM worker_schedule_policy WHERE id=1));
 RETURN true;
END $$;

CREATE OR REPLACE FUNCTION public.worker_enqueue_batch(p_account text,p_owner text,p_generation bigint,p_descriptors jsonb)
RETURNS integer LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;item jsonb;d jsonb;seconds integer;accepted integer:=0;
BEGIN
 IF jsonb_typeof(p_descriptors) IS DISTINCT FROM 'array' THEN RAISE EXCEPTION 'Invalid discovery batch'; END IF;
 IF jsonb_array_length(p_descriptors)>128 OR octet_length(p_descriptors::text)>1048576 THEN RAISE EXCEPTION 'Discovery batch limit'; END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' OR a.generation<>p_generation
 OR NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 FOR item IN SELECT value FROM jsonb_array_elements(p_descriptors) LOOP
  d:=item-'interval_seconds';seconds:=coalesce((item->>'interval_seconds')::integer,300);
  IF jsonb_typeof(d) IS DISTINCT FROM 'object' OR d->'context'->>'account_id' IS DISTINCT FROM p_account
   OR coalesce(d->>'resource_key','') !~ '^[a-f0-9]{64}$'
   OR coalesce(d->>'handler','') !~ '^(get_|read_|fm_worker_finances$)' OR seconds NOT BETWEEN 15 AND 86400
  THEN RAISE EXCEPTION 'Invalid discovery descriptor'; END IF;
  IF a.excluded_leagues ? (d->'context'->>'championship_id')
   OR EXISTS(SELECT 1 FROM blocked_leagues WHERE championship_id=d->'context'->>'championship_id')
   OR NOT worker_finance_context_matches(d,a.verified_leagues->(d->'context'->>'championship_id')) THEN CONTINUE; END IF;
  -- Routine rediscovery must not rewrite thousands of unchanged durable jobs.
  IF EXISTS(SELECT 1 FROM worker_jobs j WHERE j.account_id=p_account AND j.resource_key=d->>'resource_key'
   AND j.generation=p_generation AND j.descriptor=d AND j.native_interval_seconds<=seconds
   AND j.base_priority>=CASE WHEN seconds<=60 THEN 4 WHEN seconds<=300 THEN 3 ELSE 1 END) THEN
   accepted:=accepted+1;
  ELSIF worker_enqueue(p_account,d,seconds,0) THEN accepted:=accepted+1;
  END IF;
  -- Account-only endpoints used to inherit browser league scope / named empty
  -- arguments. Retire duplicate jobs only after the canonical replacement is
  -- durably registered. Factual snapshots and source times are preserved.
  IF d->>'handler' IN ('get_active_championships','get_notifications','get_notification_unread')
   AND d->'args'='[]'::jsonb AND coalesce(d->'context'->>'championship_id','')=''
   AND EXISTS(SELECT 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=d->>'resource_key') THEN
   DELETE FROM worker_jobs WHERE account_id=p_account AND descriptor->>'handler'=d->>'handler'
    AND resource_key<>d->>'resource_key' AND owner IS NULL;
  END IF;
 END LOOP;
 DELETE FROM worker_jobs old WHERE old.account_id=p_account AND old.owner IS NULL
 AND old.descriptor->>'handler' IN ('get_active_championships','get_notifications','get_notification_unread')
 AND old.resource_key<>public.worker_account_descriptor(p_account,old.descriptor->>'handler')->>'resource_key'
 AND EXISTS(SELECT 1 FROM worker_jobs current WHERE current.account_id=p_account
  AND current.resource_key=public.worker_account_descriptor(p_account,old.descriptor->>'handler')->>'resource_key');
 -- Card history now comes from summary publication and existing DB readers.
 DELETE FROM worker_jobs WHERE account_id=p_account AND descriptor->>'handler'='get_player_card_history' AND owner IS NULL;
 RETURN accepted;
END $$;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Reference-safe bounded cleanup, independent of source acquisition and heartbeat.
BEGIN;
CREATE TABLE public.worker_cleanup_status (
 id integer PRIMARY KEY CHECK(id=1), last_success_at timestamptz NOT NULL,
 chunks_deleted bigint NOT NULL DEFAULT 0, checkpoints_deleted bigint NOT NULL DEFAULT 0,
 last_chunks integer NOT NULL DEFAULT 0, last_checkpoints integer NOT NULL DEFAULT 0
);
ALTER TABLE public.worker_cleanup_status ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.worker_cleanup_status FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE ON public.worker_cleanup_status TO service_role;
CREATE FUNCTION public.worker_cleanup_batch(p_limit integer DEFAULT 500) RETURNS jsonb
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE chunks integer; checkpoints integer;
BEGIN
 IF p_limit<1 OR p_limit>1000 THEN RAISE EXCEPTION 'Invalid cleanup batch size'; END IF;
 -- Current snapshots always win. The grace window protects uploads not yet published.
 WITH candidates AS (
 SELECT c.ctid FROM worker_chunks c WHERE c.created_at<now()-interval '1 hour'
 AND NOT EXISTS(SELECT 1 FROM worker_snapshots s WHERE s.account_id=c.account_id
 AND s.resource_key=c.resource_key AND s.version=c.version)
 ORDER BY c.created_at LIMIT p_limit FOR UPDATE OF c SKIP LOCKED
 ) DELETE FROM worker_chunks c USING candidates d WHERE c.ctid=d.ctid;
 GET DIAGNOSTICS chunks=ROW_COUNT;
 -- Keep every checkpoint belonging to the current run, even for long retries.
 WITH candidates AS (
 SELECT c.ctid FROM worker_checkpoints c WHERE c.created_at<now()-interval '1 hour'
 AND NOT EXISTS(SELECT 1 FROM worker_jobs j WHERE j.account_id=c.account_id
 AND j.resource_key=c.resource_key AND j.run_id=c.run_id)
 LIMIT p_limit FOR UPDATE OF c SKIP LOCKED
 ) DELETE FROM worker_checkpoints c USING candidates d WHERE c.ctid=d.ctid;
 GET DIAGNOSTICS checkpoints=ROW_COUNT;
 DELETE FROM worker_health WHERE heartbeat<now()-interval '1 day';
 INSERT INTO worker_cleanup_status(id,last_success_at,chunks_deleted,checkpoints_deleted,last_chunks,last_checkpoints)
 VALUES(1,now(),chunks,checkpoints,chunks,checkpoints)
 ON CONFLICT(id) DO UPDATE SET last_success_at=excluded.last_success_at,
 chunks_deleted=worker_cleanup_status.chunks_deleted+excluded.chunks_deleted,
 checkpoints_deleted=worker_cleanup_status.checkpoints_deleted+excluded.checkpoints_deleted,
 last_chunks=excluded.last_chunks,last_checkpoints=excluded.last_checkpoints;
 RETURN jsonb_build_object('chunks',chunks,'checkpoints',checkpoints);
END $$;
CREATE OR REPLACE FUNCTION public.worker_cleanup() RETURNS void
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 PERFORM worker_cleanup_batch(500);
END $$;
REVOKE ALL ON FUNCTION public.worker_cleanup_batch(integer) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_cleanup_batch(integer) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Lossless membership normalization; all observation times retained.
BEGIN;
SET LOCAL lock_timeout='5s';
LOCK TABLE public.player_membership_observations IN ACCESS EXCLUSIVE MODE;
ALTER TABLE public.player_membership_observations RENAME TO player_membership_legacy;
CREATE TABLE public.membership_entities (
 id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
 player_id text NOT NULL CHECK(length(player_id) BETWEEN 1 AND 256),
 competition_id text NOT NULL CHECK(length(competition_id) BETWEEN 1 AND 256),
 season text NOT NULL CHECK(length(season) BETWEEN 1 AND 256),
 real_club_id text CHECK(real_club_id IS NULL OR length(real_club_id) BETWEEN 1 AND 256),
 UNIQUE NULLS NOT DISTINCT(player_id,competition_id,season,real_club_id)
);
CREATE INDEX membership_entity_context ON public.membership_entities(competition_id,season,player_id);
CREATE TABLE public.membership_states (
 id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
 entity_id bigint NOT NULL REFERENCES public.membership_entities(id),
 effective_at timestamptz CHECK(effective_at IS NULL OR isfinite(effective_at)),
 source text NOT NULL CHECK(length(source) BETWEEN 1 AND 128),
 UNIQUE NULLS NOT DISTINCT(entity_id,effective_at,source),UNIQUE(id,entity_id)
);
CREATE TABLE public.membership_times (
 entity_id bigint NOT NULL REFERENCES public.membership_entities(id),
 observed_at timestamptz NOT NULL CHECK(isfinite(observed_at)),
 state_id bigint NOT NULL,
 PRIMARY KEY(entity_id,observed_at),
 FOREIGN KEY(state_id,entity_id) REFERENCES public.membership_states(id,entity_id)
);
CREATE VIEW public.player_membership_observations WITH(security_invoker=true) AS
 SELECT e.player_id,e.competition_id,e.season,e.real_club_id,t.observed_at,s.effective_at,s.source
 FROM public.membership_times t JOIN public.membership_entities e ON e.id=t.entity_id
 JOIN public.membership_states s ON s.id=t.state_id;
INSERT INTO public.membership_entities(player_id,competition_id,season,real_club_id)
 SELECT DISTINCT player_id,competition_id,season,real_club_id FROM public.player_membership_legacy;
INSERT INTO public.membership_states(entity_id,effective_at,source)
 SELECT DISTINCT e.id,o.effective_at,o.source FROM public.player_membership_legacy o
 JOIN public.membership_entities e ON e.player_id=o.player_id AND e.competition_id=o.competition_id
 AND e.season=o.season AND e.real_club_id IS NOT DISTINCT FROM o.real_club_id;
INSERT INTO public.membership_times(entity_id,observed_at,state_id)
 SELECT e.id,o.observed_at,s.id FROM public.player_membership_legacy o
 JOIN public.membership_entities e ON e.player_id=o.player_id AND e.competition_id=o.competition_id
 AND e.season=o.season AND e.real_club_id IS NOT DISTINCT FROM o.real_club_id
 JOIN public.membership_states s ON s.entity_id=e.id AND s.effective_at IS NOT DISTINCT FROM o.effective_at AND s.source=o.source;
DO $$ BEGIN
 IF EXISTS((SELECT * FROM public.player_membership_legacy EXCEPT ALL SELECT * FROM public.player_membership_observations)
 UNION ALL (SELECT * FROM public.player_membership_observations EXCEPT ALL SELECT * FROM public.player_membership_legacy)) THEN
 RAISE EXCEPTION 'Membership reconstruction mismatch; migration rolled back'; END IF;
END $$;
-- The locked transaction proved bidirectional row equality before releasing space.
DROP TABLE public.player_membership_legacy;
CREATE FUNCTION public.membership_write(p_payload jsonb,p_ignore boolean DEFAULT true) RETURNS boolean
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;r public.player_membership_observations;e bigint;s bigint;
BEGIN
 IF p_payload IS NULL OR jsonb_typeof(p_payload) NOT IN ('array','object') THEN RAISE EXCEPTION 'Invalid membership payload'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
 IF jsonb_typeof(row_data)<>'object' OR EXISTS(SELECT 1 FROM jsonb_object_keys(row_data) k WHERE k<>ALL(ARRAY['player_id','competition_id','season','real_club_id','observed_at','effective_at','source'])) THEN RAISE EXCEPTION 'Invalid membership fields'; END IF;
 r:=jsonb_populate_record(NULL::public.player_membership_observations,row_data);
 -- A transaction-scoped identity lock serializes concurrent first inserts and corrections.
 PERFORM pg_advisory_xact_lock(hashtextextended(jsonb_build_array(r.player_id,r.competition_id,r.season,r.real_club_id)::text,0));
 SELECT id INTO e FROM membership_entities WHERE player_id=r.player_id AND competition_id=r.competition_id AND season=r.season AND real_club_id IS NOT DISTINCT FROM r.real_club_id;
 IF e IS NULL THEN INSERT INTO membership_entities(player_id,competition_id,season,real_club_id) VALUES(r.player_id,r.competition_id,r.season,r.real_club_id) RETURNING id INTO e; END IF;
 SELECT id INTO s FROM membership_states WHERE entity_id=e AND effective_at IS NOT DISTINCT FROM r.effective_at AND source=r.source;
 IF s IS NULL THEN INSERT INTO membership_states(entity_id,effective_at,source) VALUES(e,r.effective_at,r.source) RETURNING id INTO s; END IF;
 IF p_ignore THEN INSERT INTO membership_times VALUES(e,r.observed_at,s) ON CONFLICT DO NOTHING;
 ELSE INSERT INTO membership_times VALUES(e,r.observed_at,s) ON CONFLICT(entity_id,observed_at) DO UPDATE SET state_id=excluded.state_id; END IF;
 END LOOP;
 RETURN true;
END $$;
DO $$ DECLARE t text; BEGIN
 FOREACH t IN ARRAY ARRAY['membership_entities','membership_states','membership_times'] LOOP
 EXECUTE format('ALTER TABLE public.%I ENABLE ROW LEVEL SECURITY',t);
 EXECUTE format('REVOKE ALL ON public.%I FROM PUBLIC,anon,authenticated',t);
 EXECUTE format('GRANT SELECT,INSERT,UPDATE ON public.%I TO service_role',t);
 END LOOP;
END $$;
GRANT USAGE,SELECT ON SEQUENCE public.membership_entities_id_seq,public.membership_states_id_seq TO service_role;
REVOKE ALL ON public.player_membership_observations FROM PUBLIC,anon,authenticated;
GRANT SELECT ON public.player_membership_observations TO service_role;
REVOKE ALL ON FUNCTION public.membership_write(jsonb,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.membership_write(jsonb,boolean) TO service_role;
CREATE OR REPLACE FUNCTION public.worker_write_fact(p_account text,p_key text,p_owner text,p_generation bigint,
 p_table text,p_payload jsonb,p_conflict text,p_ignore boolean,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;cols text;conflicts text;updates text;clause text;field text;head text;j worker_jobs;
BEGIN
 PERFORM 1 FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT worker_guard(p_account,p_key,p_owner,p_generation) THEN RETURN false; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key;
 IF p_table<>ALL(ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history',
 'market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations',
 'market_observations','player_match_observations','league_rule_snapshots','player_round_history_checks',
 'rival_finance_evidence','complete_history_baselines','league_context_observations','player_membership_observations']) THEN RAISE EXCEPTION 'Invalid fact table'; END IF;
 -- Match PostgREST's default merge identity for mutable source catalogs.
 IF p_conflict IS NULL AND p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
  p_conflict:='id';
 END IF;
 IF p_observed IS NULL OR p_observed>now()+interval '60 seconds' THEN RAISE EXCEPTION 'Invalid source time'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
  IF jsonb_typeof(row_data)<>'object' OR octet_length(row_data::text)>1048576 THEN RAISE EXCEPTION 'Invalid fact row'; END IF;
  IF row_data ? 'account_id' AND row_data->>'account_id' IS DISTINCT FROM p_account THEN RAISE EXCEPTION 'Foreign fact account'; END IF;
  IF row_data ? 'championship_id' AND row_data->>'championship_id' IS DISTINCT FROM j.descriptor->'context'->>'championship_id'
    AND j.descriptor->>'handler'<>'get_active_championships' THEN RAISE EXCEPTION 'Foreign fact league'; END IF;
  FOR field IN SELECT jsonb_object_keys(row_data) LOOP
   IF field ~* '(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)' OR NOT EXISTS(
    SELECT 1 FROM pg_attribute WHERE attrelid=('public.'||p_table)::regclass AND attname=field AND attnum>0 AND NOT attisdropped)
    THEN RAISE EXCEPTION 'Invalid fact field'; END IF;
  END LOOP;
  IF p_table='player_membership_observations' THEN
   PERFORM membership_write(row_data,coalesce(p_ignore,false)); CONTINUE;
  END IF;
  IF p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
   head:=NULL;
   IF coalesce(row_data->>'id','')='' THEN RAISE EXCEPTION 'Missing fact identity'; END IF;
   INSERT INTO worker_fact_heads(table_name,entity_id,observed_at) VALUES(p_table,row_data->>'id',p_observed)
   ON CONFLICT(table_name,entity_id) DO UPDATE SET observed_at=excluded.observed_at
   WHERE worker_fact_heads.observed_at<=excluded.observed_at RETURNING entity_id INTO head;
   IF head IS NULL THEN CONTINUE; END IF;
  END IF;
  SELECT string_agg(format('%I',key),',' ORDER BY key),string_agg(format('%I=excluded.%I',key,key),',' ORDER BY key)
  INTO cols,updates FROM jsonb_object_keys(row_data) AS key;
  clause:='';
  IF p_conflict IS NOT NULL THEN
   IF p_conflict !~ '^[a-z_]+(,[a-z_]+)*$' THEN RAISE EXCEPTION 'Invalid fact conflict'; END IF;
   SELECT string_agg(format('%I',v),',') INTO conflicts FROM unnest(string_to_array(p_conflict,',')) AS v;
   clause:=format(' ON CONFLICT (%s) DO %s',conflicts,CASE WHEN p_ignore THEN 'NOTHING' ELSE 'UPDATE SET '||updates END);
  END IF;
  EXECUTE format('INSERT INTO public.%I (%s) SELECT %s FROM jsonb_populate_record(NULL::public.%I,$1)%s',p_table,cols,cols,p_table,clause) USING row_data;
 END LOOP;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.clause_write_fact(p_id text,p_owner text,
 p_table text,p_payload jsonb,p_conflict text,p_ignore boolean,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;cols text;conflicts text;updates text;clause text;field text;head text;j clause_jobs;p_account text;
BEGIN
 IF NOT clause_guard(p_id,p_owner) THEN RETURN false; END IF;
 SELECT * INTO j FROM clause_jobs WHERE id=p_id; p_account:=j.account_id;
 IF p_table<>ALL(ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history',
 'market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations',
 'market_observations','player_match_observations','league_rule_snapshots','player_round_history_checks',
 'rival_finance_evidence','complete_history_baselines','league_context_observations','player_membership_observations']) THEN RAISE EXCEPTION 'Invalid fact table'; END IF;
 -- Match PostgREST's default merge identity for mutable source catalogs.
 IF p_conflict IS NULL AND p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
  p_conflict:='id';
 END IF;
 IF p_observed IS NULL OR p_observed>now()+interval '60 seconds' THEN RAISE EXCEPTION 'Invalid source time'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
  IF jsonb_typeof(row_data)<>'object' OR octet_length(row_data::text)>1048576 THEN RAISE EXCEPTION 'Invalid fact row'; END IF;
  IF row_data ? 'account_id' AND row_data->>'account_id' IS DISTINCT FROM p_account THEN RAISE EXCEPTION 'Foreign fact account'; END IF;
  IF row_data ? 'championship_id' AND row_data->>'championship_id' IS DISTINCT FROM j.context->>'championship_id'
     THEN RAISE EXCEPTION 'Foreign fact league'; END IF;
  FOR field IN SELECT jsonb_object_keys(row_data) LOOP
   IF field ~* '(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)' OR NOT EXISTS(
    SELECT 1 FROM pg_attribute WHERE attrelid=('public.'||p_table)::regclass AND attname=field AND attnum>0 AND NOT attisdropped)
    THEN RAISE EXCEPTION 'Invalid fact field'; END IF;
  END LOOP;
  IF p_table='player_membership_observations' THEN
   PERFORM membership_write(row_data,coalesce(p_ignore,false)); CONTINUE;
  END IF;
  IF p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
   head:=NULL;
   IF coalesce(row_data->>'id','')='' THEN RAISE EXCEPTION 'Missing fact identity'; END IF;
   INSERT INTO worker_fact_heads(table_name,entity_id,observed_at) VALUES(p_table,row_data->>'id',p_observed)
   ON CONFLICT(table_name,entity_id) DO UPDATE SET observed_at=excluded.observed_at
   WHERE worker_fact_heads.observed_at<=excluded.observed_at RETURNING entity_id INTO head;
   IF head IS NULL THEN CONTINUE; END IF;
  END IF;
  SELECT string_agg(format('%I',key),',' ORDER BY key),string_agg(format('%I=excluded.%I',key,key),',' ORDER BY key)
  INTO cols,updates FROM jsonb_object_keys(row_data) AS key;
  clause:='';
  IF p_conflict IS NOT NULL THEN
   IF p_conflict !~ '^[a-z_]+(,[a-z_]+)*$' THEN RAISE EXCEPTION 'Invalid fact conflict'; END IF;
   SELECT string_agg(format('%I',v),',') INTO conflicts FROM unnest(string_to_array(p_conflict,',')) AS v;
   clause:=format(' ON CONFLICT (%s) DO %s',conflicts,CASE WHEN p_ignore THEN 'NOTHING' ELSE 'UPDATE SET '||updates END);
  END IF;
  EXECUTE format('INSERT INTO public.%I (%s) SELECT %s FROM jsonb_populate_record(NULL::public.%I,$1)%s',p_table,cols,cols,p_table,clause) USING row_data;
 END LOOP;
 RETURN true;
END $$;
ANALYZE public.membership_entities;
ANALYZE public.membership_states;
ANALYZE public.membership_times;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Normalize repeated historical facts: price; first/last confirmations.
BEGIN;
SET LOCAL lock_timeout='5s';
SET LOCAL statement_timeout='120s';
SET LOCAL transaction_timeout='150s';
LOCK TABLE public.player_history IN ACCESS EXCLUSIVE MODE;
ALTER TABLE public.player_history RENAME TO player_history_legacy;
-- Bulk history scans use sequential reads and typed comparisons, not JSON conversion.
SET LOCAL enable_indexscan=off;
SET LOCAL enable_bitmapscan=off;
SET LOCAL work_mem='64MB';
SET LOCAL max_parallel_workers_per_gather=0;
CREATE TEMP TABLE price_retained ON COMMIT DROP AS
 SELECT (original).* FROM (
 SELECT o AS original,(observed_at IS NULL OR NOT isfinite(observed_at) OR player_id IS NULL OR championship_id IS NULL) AS uncertain,
 ROW(value,change,points,avg_points,avg_last_five,matches) AS fact,
 lag(ROW(value,change,points,avg_points,avg_last_five,matches)) OVER w AS preceding,
 lead(ROW(value,change,points,avg_points,avg_last_five,matches)) OVER w AS following
 FROM public.player_history_legacy o
 WINDOW w AS (PARTITION BY player_id,championship_id,competition_id,season ORDER BY observed_at,recorded_at,id)
 ) periods WHERE uncertain OR fact IS DISTINCT FROM preceding OR fact IS DISTINCT FROM following;
ANALYZE price_retained;
SET LOCAL enable_indexscan=DEFAULT;
SET LOCAL enable_bitmapscan=DEFAULT;
SET LOCAL work_mem=DEFAULT;
SET LOCAL max_parallel_workers_per_gather=DEFAULT;

CREATE TABLE public.price_history_entities(entity_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,player_id text REFERENCES public.players(id) ON DELETE CASCADE,championship_id text REFERENCES public.championships(id) ON DELETE CASCADE,competition_id text NOT NULL DEFAULT 'unknown',season text NOT NULL DEFAULT 'unknown',UNIQUE NULLS NOT DISTINCT(player_id,championship_id,competition_id,season));
CREATE INDEX price_history_scope ON public.price_history_entities(championship_id,competition_id,season,player_id);
CREATE TABLE public.price_history_states(state_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,entity_id bigint NOT NULL REFERENCES public.price_history_entities ON DELETE CASCADE,signature text NOT NULL,value bigint NOT NULL,change bigint,points integer,avg_points numeric,avg_last_five numeric,matches integer,UNIQUE(state_id,entity_id));
CREATE INDEX price_history_state_lookup ON public.price_history_states(entity_id,signature);
ALTER SEQUENCE public.player_history_id_seq OWNED BY NONE;
CREATE TABLE public.price_history_times(id bigint PRIMARY KEY DEFAULT nextval('public.player_history_id_seq'),entity_id bigint NOT NULL REFERENCES public.price_history_entities ON DELETE CASCADE,state_id bigint NOT NULL,observed_at timestamptz,recorded_at timestamptz DEFAULT now(),source_unique boolean NOT NULL,FOREIGN KEY(state_id,entity_id) REFERENCES public.price_history_states(state_id,entity_id) ON DELETE CASCADE);
ALTER SEQUENCE public.player_history_id_seq OWNED BY public.price_history_times.id;
CREATE UNIQUE INDEX price_history_source_time ON public.price_history_times(entity_id,observed_at) WHERE source_unique;
CREATE INDEX price_history_observed ON public.price_history_times(entity_id,observed_at,recorded_at,id);
CREATE INDEX price_history_recorded_page ON public.price_history_times(entity_id,recorded_at,id);
INSERT INTO public.price_history_entities(player_id,championship_id,competition_id,season) SELECT DISTINCT player_id,championship_id,competition_id,season FROM public.player_history_legacy;
ANALYZE public.price_history_entities;
INSERT INTO public.price_history_states(entity_id,signature,value,change,points,avg_points,avg_last_five,matches) SELECT DISTINCT e.entity_id,md5(jsonb_build_array(o.value,o.change,o.points,o.avg_points,o.avg_last_five,o.matches)::text),o.value,o.change,o.points,o.avg_points,o.avg_last_five,o.matches FROM price_retained o JOIN public.price_history_entities e ON e.player_id IS NOT DISTINCT FROM o.player_id AND e.championship_id IS NOT DISTINCT FROM o.championship_id AND e.competition_id IS NOT DISTINCT FROM o.competition_id AND e.season IS NOT DISTINCT FROM o.season AND md5(jsonb_build_array(e.player_id,e.championship_id,e.competition_id,e.season)::text)=md5(jsonb_build_array(o.player_id,o.championship_id,o.competition_id,o.season)::text);
ANALYZE public.price_history_states;
INSERT INTO public.price_history_times(entity_id,state_id,id,observed_at,recorded_at,source_unique) SELECT e.entity_id,s.state_id,o.id,o.observed_at,o.recorded_at,o.player_id IS NOT NULL AND o.championship_id IS NOT NULL FROM price_retained o JOIN public.price_history_entities e ON e.player_id IS NOT DISTINCT FROM o.player_id AND e.championship_id IS NOT DISTINCT FROM o.championship_id AND e.competition_id IS NOT DISTINCT FROM o.competition_id AND e.season IS NOT DISTINCT FROM o.season AND md5(jsonb_build_array(e.player_id,e.championship_id,e.competition_id,e.season)::text)=md5(jsonb_build_array(o.player_id,o.championship_id,o.competition_id,o.season)::text) JOIN public.price_history_states s ON s.entity_id=e.entity_id AND s.signature=md5(jsonb_build_array(o.value,o.change,o.points,o.avg_points,o.avg_last_five,o.matches)::text) AND s.value IS NOT DISTINCT FROM o.value AND s.change IS NOT DISTINCT FROM o.change AND s.points IS NOT DISTINCT FROM o.points AND s.avg_points IS NOT DISTINCT FROM o.avg_points AND s.avg_last_five IS NOT DISTINCT FROM o.avg_last_five AND s.matches IS NOT DISTINCT FROM o.matches;
DO $$ DECLARE columns text; BEGIN
 SELECT string_agg(CASE WHEN attname=ANY(ARRAY['player_id','championship_id','competition_id','season']) THEN 'e.' WHEN attname=ANY(ARRAY['value','change','points','avg_points','avg_last_five','matches']) THEN 's.' ELSE 't.' END||quote_ident(attname),',' ORDER BY attnum) INTO columns
 FROM pg_attribute WHERE attrelid='public.player_history_legacy'::regclass AND attnum>0 AND NOT attisdropped;
 EXECUTE 'CREATE VIEW public.player_history WITH(security_invoker=true) AS SELECT '||columns||' FROM public.price_history_times t JOIN public.price_history_entities e USING(entity_id) JOIN public.price_history_states s ON s.state_id=t.state_id';
 IF EXISTS((SELECT to_jsonb(o) FROM price_retained o EXCEPT ALL SELECT to_jsonb(v) FROM public.player_history v)
 UNION ALL (SELECT to_jsonb(v) FROM public.player_history v EXCEPT ALL SELECT to_jsonb(o) FROM price_retained o)) THEN RAISE EXCEPTION 'player_history reconstruction mismatch'; END IF;
END $$;
CREATE FUNCTION public.price_history_write(p_payload jsonb,p_conflict text DEFAULT NULL,p_ignore boolean DEFAULT false) RETURNS boolean
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE item jsonb;r public.player_history; previous jsonb; e bigint;s bigint; fields text[];
BEGIN
 IF p_payload IS NULL OR jsonb_typeof(p_payload) NOT IN ('array','object') THEN RAISE EXCEPTION 'Invalid history payload'; END IF;
 fields:=CASE WHEN p_conflict IS NULL THEN NULL ELSE string_to_array(p_conflict,',') END;
 IF fields IS NOT NULL AND fields IS DISTINCT FROM ARRAY['player_id','championship_id','competition_id','season','observed_at'] AND fields IS DISTINCT FROM ARRAY['id'] THEN RAISE EXCEPTION 'Unsupported history conflict'; END IF;
 FOR item IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
 IF jsonb_typeof(item)<>'object' OR EXISTS(SELECT 1 FROM jsonb_object_keys(item) k WHERE k<>ALL(ARRAY['player_id','championship_id','competition_id','season','value','change','points','avg_points','avg_last_five','matches','id','observed_at','recorded_at'])) THEN RAISE EXCEPTION 'Invalid history fields'; END IF;
 r:=jsonb_populate_record(NULL::public.player_history,jsonb_build_object('competition_id','unknown','season','unknown','change',0,'points',0,'matches',0,'recorded_at',now())||item);
 IF EXISTS(SELECT 1 FROM blocked_leagues WHERE championship_id=r.championship_id) THEN CONTINUE; END IF;
 IF r.competition_id IS NULL OR r.season IS NULL OR r.value IS NULL THEN RAISE EXCEPTION 'Missing history value' USING ERRCODE='23502'; END IF;
 PERFORM pg_advisory_xact_lock(hashtextextended('price'||jsonb_build_array(r.player_id,r.championship_id,r.competition_id,r.season)::text,0));
 previous:=NULL;
 IF fields IS NOT NULL THEN
 IF fields=ARRAY['id'] THEN SELECT to_jsonb(v) INTO previous FROM public.player_history v WHERE v.id=r.id; ELSE
SELECT to_jsonb(v) INTO previous FROM public.player_history v WHERE v.player_id=r.player_id AND v.championship_id=r.championship_id AND v.competition_id=r.competition_id AND v.season=r.season AND v.observed_at=r.observed_at;
 END IF;
 END IF;
 IF previous IS NOT NULL THEN
 IF p_ignore THEN CONTINUE; END IF;
 r:=jsonb_populate_record(NULL::public.player_history,previous||item);
 END IF;
 IF r.id IS NULL AND NOT(item ? 'id') THEN r.id:=nextval('public.player_history_id_seq'); END IF;
 IF r.competition_id IS NULL OR r.season IS NULL OR r.value IS NULL OR r.id IS NULL THEN RAISE EXCEPTION 'Missing history value' USING ERRCODE='23502'; END IF;
SELECT entity_id INTO e FROM price_history_entities e WHERE e.player_id IS NOT DISTINCT FROM r.player_id AND e.championship_id IS NOT DISTINCT FROM r.championship_id AND e.competition_id IS NOT DISTINCT FROM r.competition_id AND e.season IS NOT DISTINCT FROM r.season;
IF e IS NULL THEN INSERT INTO price_history_entities(player_id,championship_id,competition_id,season) VALUES(r.player_id,r.championship_id,r.competition_id,r.season) RETURNING entity_id INTO e; END IF;
SELECT state_id INTO s FROM price_history_states s WHERE s.entity_id=e AND s.signature=md5(jsonb_build_array(r.value,r.change,r.points,r.avg_points,r.avg_last_five,r.matches)::text) AND s.value IS NOT DISTINCT FROM r.value AND s.change IS NOT DISTINCT FROM r.change AND s.points IS NOT DISTINCT FROM r.points AND s.avg_points IS NOT DISTINCT FROM r.avg_points AND s.avg_last_five IS NOT DISTINCT FROM r.avg_last_five AND s.matches IS NOT DISTINCT FROM r.matches;
IF s IS NULL THEN INSERT INTO price_history_states(entity_id,signature,value,change,points,avg_points,avg_last_five,matches) VALUES(e,md5(jsonb_build_array(r.value,r.change,r.points,r.avg_points,r.avg_last_five,r.matches)::text),r.value,r.change,r.points,r.avg_points,r.avg_last_five,r.matches) RETURNING state_id INTO s; END IF;
IF previous IS NOT NULL THEN DELETE FROM price_history_times WHERE id=(previous->>'id')::bigint; END IF;
INSERT INTO price_history_times(id,entity_id,state_id,observed_at,recorded_at,source_unique) VALUES(r.id,e,s,r.observed_at,r.recorded_at,r.player_id IS NOT NULL AND r.championship_id IS NOT NULL);

-- Keep both endpoints of consecutive identical states, never merge A/B/A.
DELETE FROM price_history_times t USING (
 SELECT id,state_id,lag(state_id) OVER w AS preceding,lead(state_id) OVER w AS following,
 observed_at IS NOT NULL AND isfinite(observed_at) AND source_unique AS eligible FROM price_history_times WHERE entity_id=e
 WINDOW w AS (ORDER BY observed_at,recorded_at,id)
) interior WHERE t.entity_id=e AND t.id=interior.id AND interior.eligible
 AND interior.state_id=interior.preceding AND interior.state_id=interior.following;
END LOOP; RETURN true; END $$;
REVOKE ALL ON FUNCTION public.price_history_write(jsonb,text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.price_history_write(jsonb,text,boolean) TO service_role;
ALTER TABLE public.price_history_entities ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.price_history_entities FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.price_history_entities TO service_role;
ALTER TABLE public.price_history_states ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.price_history_states FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.price_history_states TO service_role;
ALTER TABLE public.price_history_times ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.price_history_times FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.price_history_times TO service_role;
GRANT USAGE,SELECT ON SEQUENCE public.price_history_entities_entity_id_seq TO service_role;
GRANT USAGE,SELECT ON SEQUENCE public.price_history_states_state_id_seq TO service_role;
REVOKE ALL ON public.player_history FROM PUBLIC,anon,authenticated;
GRANT SELECT ON public.player_history TO service_role;
DROP TABLE public.player_history_legacy;
CREATE OR REPLACE FUNCTION public.worker_write_fact(p_account text,p_key text,p_owner text,p_generation bigint,
 p_table text,p_payload jsonb,p_conflict text,p_ignore boolean,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;cols text;conflicts text;updates text;clause text;field text;head text;j worker_jobs;
BEGIN
 PERFORM 1 FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT worker_guard(p_account,p_key,p_owner,p_generation) THEN RETURN false; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key;
 IF p_table<>ALL(ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history',
 'market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations',
 'market_observations','player_match_observations','league_rule_snapshots','player_round_history_checks',
 'rival_finance_evidence','complete_history_baselines','league_context_observations','player_membership_observations']) THEN RAISE EXCEPTION 'Invalid fact table'; END IF;
 -- Match PostgREST's default merge identity for mutable source catalogs.
 IF p_conflict IS NULL AND p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
  p_conflict:='id';
 END IF;
 IF p_observed IS NULL OR p_observed>now()+interval '60 seconds' THEN RAISE EXCEPTION 'Invalid source time'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
  IF jsonb_typeof(row_data)<>'object' OR octet_length(row_data::text)>1048576 THEN RAISE EXCEPTION 'Invalid fact row'; END IF;
  IF row_data ? 'account_id' AND row_data->>'account_id' IS DISTINCT FROM p_account THEN RAISE EXCEPTION 'Foreign fact account'; END IF;
  IF row_data ? 'championship_id' AND row_data->>'championship_id' IS DISTINCT FROM j.descriptor->'context'->>'championship_id'
    AND j.descriptor->>'handler'<>'get_active_championships' THEN RAISE EXCEPTION 'Foreign fact league'; END IF;
  FOR field IN SELECT jsonb_object_keys(row_data) LOOP
   IF field ~* '(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)' OR NOT EXISTS(
    SELECT 1 FROM pg_attribute WHERE attrelid=('public.'||p_table)::regclass AND attname=field AND attnum>0 AND NOT attisdropped)
    THEN RAISE EXCEPTION 'Invalid fact field'; END IF;
  END LOOP;
  IF p_table='player_history' AND to_regprocedure('public.price_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM price_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_match_observations' AND to_regprocedure('public.match_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM match_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='rival_finance_evidence' AND to_regprocedure('public.finance_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM finance_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_membership_observations' THEN
   PERFORM membership_write(row_data,coalesce(p_ignore,false)); CONTINUE;
  END IF;
  IF p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
   head:=NULL;
   IF coalesce(row_data->>'id','')='' THEN RAISE EXCEPTION 'Missing fact identity'; END IF;
   INSERT INTO worker_fact_heads(table_name,entity_id,observed_at) VALUES(p_table,row_data->>'id',p_observed)
   ON CONFLICT(table_name,entity_id) DO UPDATE SET observed_at=excluded.observed_at
   WHERE worker_fact_heads.observed_at<=excluded.observed_at RETURNING entity_id INTO head;
   IF head IS NULL THEN CONTINUE; END IF;
  END IF;
  SELECT string_agg(format('%I',key),',' ORDER BY key),string_agg(format('%I=excluded.%I',key,key),',' ORDER BY key)
  INTO cols,updates FROM jsonb_object_keys(row_data) AS key;
  clause:='';
  IF p_conflict IS NOT NULL THEN
   IF p_conflict !~ '^[a-z_]+(,[a-z_]+)*$' THEN RAISE EXCEPTION 'Invalid fact conflict'; END IF;
   SELECT string_agg(format('%I',v),',') INTO conflicts FROM unnest(string_to_array(p_conflict,',')) AS v;
   clause:=format(' ON CONFLICT (%s) DO %s',conflicts,CASE WHEN p_ignore THEN 'NOTHING' ELSE 'UPDATE SET '||updates END);
  END IF;
  EXECUTE format('INSERT INTO public.%I (%s) SELECT %s FROM jsonb_populate_record(NULL::public.%I,$1)%s',p_table,cols,cols,p_table,clause) USING row_data;
 END LOOP;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.clause_write_fact(p_id text,p_owner text,
 p_table text,p_payload jsonb,p_conflict text,p_ignore boolean,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;cols text;conflicts text;updates text;clause text;field text;head text;j clause_jobs;p_account text;
BEGIN
 IF NOT clause_guard(p_id,p_owner) THEN RETURN false; END IF;
 SELECT * INTO j FROM clause_jobs WHERE id=p_id; p_account:=j.account_id;
 IF p_table<>ALL(ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history',
 'market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations',
 'market_observations','player_match_observations','league_rule_snapshots','player_round_history_checks',
 'rival_finance_evidence','complete_history_baselines','league_context_observations','player_membership_observations']) THEN RAISE EXCEPTION 'Invalid fact table'; END IF;
 -- Match PostgREST's default merge identity for mutable source catalogs.
 IF p_conflict IS NULL AND p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
  p_conflict:='id';
 END IF;
 IF p_observed IS NULL OR p_observed>now()+interval '60 seconds' THEN RAISE EXCEPTION 'Invalid source time'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
  IF jsonb_typeof(row_data)<>'object' OR octet_length(row_data::text)>1048576 THEN RAISE EXCEPTION 'Invalid fact row'; END IF;
  IF row_data ? 'account_id' AND row_data->>'account_id' IS DISTINCT FROM p_account THEN RAISE EXCEPTION 'Foreign fact account'; END IF;
  IF row_data ? 'championship_id' AND row_data->>'championship_id' IS DISTINCT FROM j.context->>'championship_id'
     THEN RAISE EXCEPTION 'Foreign fact league'; END IF;
  FOR field IN SELECT jsonb_object_keys(row_data) LOOP
   IF field ~* '(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)' OR NOT EXISTS(
    SELECT 1 FROM pg_attribute WHERE attrelid=('public.'||p_table)::regclass AND attname=field AND attnum>0 AND NOT attisdropped)
    THEN RAISE EXCEPTION 'Invalid fact field'; END IF;
  END LOOP;
  IF p_table='player_history' AND to_regprocedure('public.price_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM price_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_match_observations' AND to_regprocedure('public.match_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM match_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='rival_finance_evidence' AND to_regprocedure('public.finance_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM finance_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_membership_observations' THEN
   PERFORM membership_write(row_data,coalesce(p_ignore,false)); CONTINUE;
  END IF;
  IF p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
   head:=NULL;
   IF coalesce(row_data->>'id','')='' THEN RAISE EXCEPTION 'Missing fact identity'; END IF;
   INSERT INTO worker_fact_heads(table_name,entity_id,observed_at) VALUES(p_table,row_data->>'id',p_observed)
   ON CONFLICT(table_name,entity_id) DO UPDATE SET observed_at=excluded.observed_at
   WHERE worker_fact_heads.observed_at<=excluded.observed_at RETURNING entity_id INTO head;
   IF head IS NULL THEN CONTINUE; END IF;
  END IF;
  SELECT string_agg(format('%I',key),',' ORDER BY key),string_agg(format('%I=excluded.%I',key,key),',' ORDER BY key)
  INTO cols,updates FROM jsonb_object_keys(row_data) AS key;
  clause:='';
  IF p_conflict IS NOT NULL THEN
   IF p_conflict !~ '^[a-z_]+(,[a-z_]+)*$' THEN RAISE EXCEPTION 'Invalid fact conflict'; END IF;
   SELECT string_agg(format('%I',v),',') INTO conflicts FROM unnest(string_to_array(p_conflict,',')) AS v;
   clause:=format(' ON CONFLICT (%s) DO %s',conflicts,CASE WHEN p_ignore THEN 'NOTHING' ELSE 'UPDATE SET '||updates END);
  END IF;
  EXECUTE format('INSERT INTO public.%I (%s) SELECT %s FROM jsonb_populate_record(NULL::public.%I,$1)%s',p_table,cols,cols,p_table,clause) USING row_data;
 END LOOP;
 RETURN true;
END $$;
ANALYZE public.price_history_entities;
ANALYZE public.price_history_states;
ANALYZE public.price_history_times;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Normalize repeated historical facts: match; first/last confirmations.
BEGIN;
SET LOCAL lock_timeout='5s';
SET LOCAL statement_timeout='120s';
SET LOCAL transaction_timeout='150s';
LOCK TABLE public.player_match_observations IN ACCESS EXCLUSIVE MODE;
ALTER TABLE public.player_match_observations RENAME TO player_match_observations_legacy;
-- Bulk history scans use sequential reads and typed comparisons, not JSON conversion.
SET LOCAL enable_indexscan=off;
SET LOCAL enable_bitmapscan=off;
SET LOCAL work_mem='64MB';
SET LOCAL max_parallel_workers_per_gather=0;
CREATE TEMP TABLE match_retained ON COMMIT DROP AS
 SELECT (original).* FROM (
 SELECT o AS original,(observed_at IS NULL OR NOT isfinite(observed_at)) AS uncertain,
 ROW(points,score_status,is_home,minutes_raw,start_raw,participation,round_id,match_id,source_round_number,occurred_at,round_start_at) AS fact,
 lag(ROW(points,score_status,is_home,minutes_raw,start_raw,participation,round_id,match_id,source_round_number,occurred_at,round_start_at)) OVER w AS preceding,
 lead(ROW(points,score_status,is_home,minutes_raw,start_raw,participation,round_id,match_id,source_round_number,occurred_at,round_start_at)) OVER w AS following
 FROM public.player_match_observations_legacy o
 WINDOW w AS (PARTITION BY player_id,championship_id,competition_id,season,scoring_version,round,source_round_key ORDER BY observed_at)
 ) periods WHERE uncertain OR fact IS DISTINCT FROM preceding OR fact IS DISTINCT FROM following;
ANALYZE match_retained;
SET LOCAL enable_indexscan=DEFAULT;
SET LOCAL enable_bitmapscan=DEFAULT;
SET LOCAL work_mem=DEFAULT;
SET LOCAL max_parallel_workers_per_gather=DEFAULT;

CREATE TABLE public.match_history_entities(entity_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,player_id text NOT NULL,championship_id text NOT NULL,competition_id text NOT NULL DEFAULT 'unknown',season text NOT NULL,scoring_version text NOT NULL,round numeric NOT NULL,source_round_key text NOT NULL DEFAULT '',UNIQUE NULLS NOT DISTINCT(player_id,championship_id,competition_id,season,scoring_version,round,source_round_key));
CREATE INDEX match_history_scope ON public.match_history_entities(championship_id,competition_id,season,player_id);
CREATE TABLE public.match_history_states(state_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,entity_id bigint NOT NULL REFERENCES public.match_history_entities ON DELETE CASCADE,signature text NOT NULL,points numeric,score_status text NOT NULL,is_home boolean,minutes_raw numeric,start_raw boolean,participation boolean,round_id text,match_id text,source_round_number numeric,occurred_at timestamptz,round_start_at timestamptz,UNIQUE(state_id,entity_id));
CREATE INDEX match_history_state_lookup ON public.match_history_states(entity_id,signature);
CREATE TABLE public.match_history_times(entity_id bigint NOT NULL REFERENCES public.match_history_entities ON DELETE CASCADE,state_id bigint NOT NULL,observed_at timestamptz NOT NULL,PRIMARY KEY(entity_id,observed_at),FOREIGN KEY(state_id,entity_id) REFERENCES public.match_history_states(state_id,entity_id) ON DELETE CASCADE);
INSERT INTO public.match_history_entities(player_id,championship_id,competition_id,season,scoring_version,round,source_round_key) SELECT DISTINCT player_id,championship_id,competition_id,season,scoring_version,round,source_round_key FROM public.player_match_observations_legacy;
ANALYZE public.match_history_entities;
INSERT INTO public.match_history_states(entity_id,signature,points,score_status,is_home,minutes_raw,start_raw,participation,round_id,match_id,source_round_number,occurred_at,round_start_at) SELECT DISTINCT e.entity_id,md5(jsonb_build_array(o.points,o.score_status,o.is_home,o.minutes_raw,o.start_raw,o.participation,o.round_id,o.match_id,o.source_round_number,o.occurred_at,o.round_start_at)::text),o.points,o.score_status,o.is_home,o.minutes_raw,o.start_raw,o.participation,o.round_id,o.match_id,o.source_round_number,o.occurred_at,o.round_start_at FROM match_retained o JOIN public.match_history_entities e ON e.player_id=o.player_id AND e.championship_id=o.championship_id AND e.competition_id=o.competition_id AND e.season=o.season AND md5(jsonb_build_array(e.player_id,e.championship_id,e.competition_id,e.season)::text)=md5(jsonb_build_array(o.player_id,o.championship_id,o.competition_id,o.season)::text) AND e.scoring_version=o.scoring_version AND e.round=o.round AND e.source_round_key=o.source_round_key;
ANALYZE public.match_history_states;
INSERT INTO public.match_history_times(entity_id,state_id,observed_at) SELECT e.entity_id,s.state_id,o.observed_at FROM match_retained o JOIN public.match_history_entities e ON e.player_id=o.player_id AND e.championship_id=o.championship_id AND e.competition_id=o.competition_id AND e.season=o.season AND md5(jsonb_build_array(e.player_id,e.championship_id,e.competition_id,e.season)::text)=md5(jsonb_build_array(o.player_id,o.championship_id,o.competition_id,o.season)::text) AND e.scoring_version=o.scoring_version AND e.round=o.round AND e.source_round_key=o.source_round_key JOIN public.match_history_states s ON s.entity_id=e.entity_id AND s.signature=md5(jsonb_build_array(o.points,o.score_status,o.is_home,o.minutes_raw,o.start_raw,o.participation,o.round_id,o.match_id,o.source_round_number,o.occurred_at,o.round_start_at)::text) AND s.points IS NOT DISTINCT FROM o.points AND s.score_status IS NOT DISTINCT FROM o.score_status AND s.is_home IS NOT DISTINCT FROM o.is_home AND s.minutes_raw IS NOT DISTINCT FROM o.minutes_raw AND s.start_raw IS NOT DISTINCT FROM o.start_raw AND s.participation IS NOT DISTINCT FROM o.participation AND s.round_id IS NOT DISTINCT FROM o.round_id AND s.match_id IS NOT DISTINCT FROM o.match_id AND s.source_round_number IS NOT DISTINCT FROM o.source_round_number AND s.occurred_at IS NOT DISTINCT FROM o.occurred_at AND s.round_start_at IS NOT DISTINCT FROM o.round_start_at;
DO $$ DECLARE columns text; BEGIN
 SELECT string_agg(CASE WHEN attname=ANY(ARRAY['player_id','championship_id','competition_id','season','scoring_version','round','source_round_key']) THEN 'e.' WHEN attname=ANY(ARRAY['points','score_status','is_home','minutes_raw','start_raw','participation','round_id','match_id','source_round_number','occurred_at','round_start_at']) THEN 's.' ELSE 't.' END||quote_ident(attname),',' ORDER BY attnum) INTO columns
 FROM pg_attribute WHERE attrelid='public.player_match_observations_legacy'::regclass AND attnum>0 AND NOT attisdropped;
 EXECUTE 'CREATE VIEW public.player_match_observations WITH(security_invoker=true) AS SELECT '||columns||' FROM public.match_history_times t JOIN public.match_history_entities e USING(entity_id) JOIN public.match_history_states s ON s.state_id=t.state_id';
 IF EXISTS((SELECT to_jsonb(o) FROM match_retained o EXCEPT ALL SELECT to_jsonb(v) FROM public.player_match_observations v)
 UNION ALL (SELECT to_jsonb(v) FROM public.player_match_observations v EXCEPT ALL SELECT to_jsonb(o) FROM match_retained o)) THEN RAISE EXCEPTION 'player_match_observations reconstruction mismatch'; END IF;
END $$;
CREATE FUNCTION public.match_history_write(p_payload jsonb,p_conflict text DEFAULT NULL,p_ignore boolean DEFAULT false) RETURNS boolean
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE item jsonb;r public.player_match_observations; previous jsonb; e bigint;s bigint; fields text[];
BEGIN
 IF p_payload IS NULL OR jsonb_typeof(p_payload) NOT IN ('array','object') THEN RAISE EXCEPTION 'Invalid history payload'; END IF;
 fields:=CASE WHEN p_conflict IS NULL THEN NULL ELSE string_to_array(p_conflict,',') END;
 IF fields IS NOT NULL AND fields IS DISTINCT FROM ARRAY['player_id','championship_id','competition_id','season','scoring_version','round','source_round_key','observed_at']  THEN RAISE EXCEPTION 'Unsupported history conflict'; END IF;
 FOR item IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
 IF jsonb_typeof(item)<>'object' OR EXISTS(SELECT 1 FROM jsonb_object_keys(item) k WHERE k<>ALL(ARRAY['player_id','championship_id','competition_id','season','scoring_version','round','source_round_key','points','score_status','is_home','minutes_raw','start_raw','participation','round_id','match_id','source_round_number','occurred_at','round_start_at','observed_at'])) THEN RAISE EXCEPTION 'Invalid history fields'; END IF;
 r:=jsonb_populate_record(NULL::public.player_match_observations,jsonb_build_object('competition_id','unknown','source_round_key','')||item);
 IF EXISTS(SELECT 1 FROM blocked_leagues WHERE championship_id=r.championship_id) THEN CONTINUE; END IF;
 IF r.player_id IS NULL OR r.championship_id IS NULL OR r.competition_id IS NULL OR r.season IS NULL OR r.scoring_version IS NULL OR r.round IS NULL OR r.source_round_key IS NULL OR r.score_status IS NULL OR r.observed_at IS NULL THEN RAISE EXCEPTION 'Missing history value' USING ERRCODE='23502'; END IF;
 PERFORM pg_advisory_xact_lock(hashtextextended('match'||jsonb_build_array(r.player_id,r.championship_id,r.competition_id,r.season,r.scoring_version,r.round,r.source_round_key)::text,0));
 previous:=NULL;
 IF fields IS NOT NULL THEN
SELECT to_jsonb(v) INTO previous FROM public.player_match_observations v WHERE v.player_id=r.player_id AND v.championship_id=r.championship_id AND v.competition_id=r.competition_id AND v.season=r.season AND v.scoring_version=r.scoring_version AND v.round=r.round AND v.source_round_key=r.source_round_key AND v.observed_at=r.observed_at;
 END IF;
 IF previous IS NOT NULL THEN
 IF p_ignore THEN CONTINUE; END IF;
 r:=jsonb_populate_record(NULL::public.player_match_observations,previous||item);
 END IF;
 IF r.player_id IS NULL OR r.championship_id IS NULL OR r.competition_id IS NULL OR r.season IS NULL OR r.scoring_version IS NULL OR r.round IS NULL OR r.source_round_key IS NULL OR r.score_status IS NULL OR r.observed_at IS NULL THEN RAISE EXCEPTION 'Missing history value' USING ERRCODE='23502'; END IF;
SELECT entity_id INTO e FROM match_history_entities e WHERE e.player_id IS NOT DISTINCT FROM r.player_id AND e.championship_id IS NOT DISTINCT FROM r.championship_id AND e.competition_id IS NOT DISTINCT FROM r.competition_id AND e.season IS NOT DISTINCT FROM r.season AND e.scoring_version IS NOT DISTINCT FROM r.scoring_version AND e.round IS NOT DISTINCT FROM r.round AND e.source_round_key IS NOT DISTINCT FROM r.source_round_key;
IF e IS NULL THEN INSERT INTO match_history_entities(player_id,championship_id,competition_id,season,scoring_version,round,source_round_key) VALUES(r.player_id,r.championship_id,r.competition_id,r.season,r.scoring_version,r.round,r.source_round_key) RETURNING entity_id INTO e; END IF;
SELECT state_id INTO s FROM match_history_states s WHERE s.entity_id=e AND s.signature=md5(jsonb_build_array(r.points,r.score_status,r.is_home,r.minutes_raw,r.start_raw,r.participation,r.round_id,r.match_id,r.source_round_number,r.occurred_at,r.round_start_at)::text) AND s.points IS NOT DISTINCT FROM r.points AND s.score_status IS NOT DISTINCT FROM r.score_status AND s.is_home IS NOT DISTINCT FROM r.is_home AND s.minutes_raw IS NOT DISTINCT FROM r.minutes_raw AND s.start_raw IS NOT DISTINCT FROM r.start_raw AND s.participation IS NOT DISTINCT FROM r.participation AND s.round_id IS NOT DISTINCT FROM r.round_id AND s.match_id IS NOT DISTINCT FROM r.match_id AND s.source_round_number IS NOT DISTINCT FROM r.source_round_number AND s.occurred_at IS NOT DISTINCT FROM r.occurred_at AND s.round_start_at IS NOT DISTINCT FROM r.round_start_at;
IF s IS NULL THEN INSERT INTO match_history_states(entity_id,signature,points,score_status,is_home,minutes_raw,start_raw,participation,round_id,match_id,source_round_number,occurred_at,round_start_at) VALUES(e,md5(jsonb_build_array(r.points,r.score_status,r.is_home,r.minutes_raw,r.start_raw,r.participation,r.round_id,r.match_id,r.source_round_number,r.occurred_at,r.round_start_at)::text),r.points,r.score_status,r.is_home,r.minutes_raw,r.start_raw,r.participation,r.round_id,r.match_id,r.source_round_number,r.occurred_at,r.round_start_at) RETURNING state_id INTO s; END IF;
IF previous IS NOT NULL THEN UPDATE match_history_times SET state_id=s WHERE entity_id=e AND observed_at=r.observed_at; ELSE INSERT INTO match_history_times VALUES(e,s,r.observed_at); END IF;

-- Keep both endpoints of consecutive identical states, never merge A/B/A.
DELETE FROM match_history_times t USING (
 SELECT observed_at,state_id,lag(state_id) OVER w AS preceding,lead(state_id) OVER w AS following,
 observed_at IS NOT NULL AND isfinite(observed_at) AS eligible FROM match_history_times WHERE entity_id=e
 WINDOW w AS (ORDER BY observed_at)
) interior WHERE t.entity_id=e AND t.observed_at=interior.observed_at AND interior.eligible
 AND interior.state_id=interior.preceding AND interior.state_id=interior.following;
END LOOP; RETURN true; END $$;
REVOKE ALL ON FUNCTION public.match_history_write(jsonb,text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.match_history_write(jsonb,text,boolean) TO service_role;
ALTER TABLE public.match_history_entities ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.match_history_entities FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.match_history_entities TO service_role;
ALTER TABLE public.match_history_states ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.match_history_states FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.match_history_states TO service_role;
ALTER TABLE public.match_history_times ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.match_history_times FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.match_history_times TO service_role;
GRANT USAGE,SELECT ON SEQUENCE public.match_history_entities_entity_id_seq TO service_role;
GRANT USAGE,SELECT ON SEQUENCE public.match_history_states_state_id_seq TO service_role;
REVOKE ALL ON public.player_match_observations FROM PUBLIC,anon,authenticated;
GRANT SELECT ON public.player_match_observations TO service_role;
CREATE OR REPLACE VIEW public.player_round_points_latest WITH(security_invoker=true) AS
 SELECT e.championship_id,e.player_id,e.season,e.scoring_version,e.round,s.points,
 s.score_status,t.observed_at,s.occurred_at,s.round_start_at,e.competition_id,e.source_round_key,
 s.round_id,s.match_id,s.source_round_number
 FROM public.match_history_entities e
 JOIN LATERAL (SELECT observed_at,state_id FROM public.match_history_times WHERE entity_id=e.entity_id ORDER BY observed_at DESC LIMIT 1) t ON true
 JOIN public.match_history_states s ON s.state_id=t.state_id;
DROP TABLE public.player_match_observations_legacy;
CREATE OR REPLACE FUNCTION public.worker_write_fact(p_account text,p_key text,p_owner text,p_generation bigint,
 p_table text,p_payload jsonb,p_conflict text,p_ignore boolean,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;cols text;conflicts text;updates text;clause text;field text;head text;j worker_jobs;
BEGIN
 PERFORM 1 FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT worker_guard(p_account,p_key,p_owner,p_generation) THEN RETURN false; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key;
 IF p_table<>ALL(ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history',
 'market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations',
 'market_observations','player_match_observations','league_rule_snapshots','player_round_history_checks',
 'rival_finance_evidence','complete_history_baselines','league_context_observations','player_membership_observations']) THEN RAISE EXCEPTION 'Invalid fact table'; END IF;
 -- Match PostgREST's default merge identity for mutable source catalogs.
 IF p_conflict IS NULL AND p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
  p_conflict:='id';
 END IF;
 IF p_observed IS NULL OR p_observed>now()+interval '60 seconds' THEN RAISE EXCEPTION 'Invalid source time'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
  IF jsonb_typeof(row_data)<>'object' OR octet_length(row_data::text)>1048576 THEN RAISE EXCEPTION 'Invalid fact row'; END IF;
  IF row_data ? 'account_id' AND row_data->>'account_id' IS DISTINCT FROM p_account THEN RAISE EXCEPTION 'Foreign fact account'; END IF;
  IF row_data ? 'championship_id' AND row_data->>'championship_id' IS DISTINCT FROM j.descriptor->'context'->>'championship_id'
    AND j.descriptor->>'handler'<>'get_active_championships' THEN RAISE EXCEPTION 'Foreign fact league'; END IF;
  FOR field IN SELECT jsonb_object_keys(row_data) LOOP
   IF field ~* '(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)' OR NOT EXISTS(
    SELECT 1 FROM pg_attribute WHERE attrelid=('public.'||p_table)::regclass AND attname=field AND attnum>0 AND NOT attisdropped)
    THEN RAISE EXCEPTION 'Invalid fact field'; END IF;
  END LOOP;
  IF p_table='player_history' AND to_regprocedure('public.price_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM price_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_match_observations' AND to_regprocedure('public.match_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM match_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='rival_finance_evidence' AND to_regprocedure('public.finance_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM finance_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_membership_observations' THEN
   PERFORM membership_write(row_data,coalesce(p_ignore,false)); CONTINUE;
  END IF;
  IF p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
   head:=NULL;
   IF coalesce(row_data->>'id','')='' THEN RAISE EXCEPTION 'Missing fact identity'; END IF;
   INSERT INTO worker_fact_heads(table_name,entity_id,observed_at) VALUES(p_table,row_data->>'id',p_observed)
   ON CONFLICT(table_name,entity_id) DO UPDATE SET observed_at=excluded.observed_at
   WHERE worker_fact_heads.observed_at<=excluded.observed_at RETURNING entity_id INTO head;
   IF head IS NULL THEN CONTINUE; END IF;
  END IF;
  SELECT string_agg(format('%I',key),',' ORDER BY key),string_agg(format('%I=excluded.%I',key,key),',' ORDER BY key)
  INTO cols,updates FROM jsonb_object_keys(row_data) AS key;
  clause:='';
  IF p_conflict IS NOT NULL THEN
   IF p_conflict !~ '^[a-z_]+(,[a-z_]+)*$' THEN RAISE EXCEPTION 'Invalid fact conflict'; END IF;
   SELECT string_agg(format('%I',v),',') INTO conflicts FROM unnest(string_to_array(p_conflict,',')) AS v;
   clause:=format(' ON CONFLICT (%s) DO %s',conflicts,CASE WHEN p_ignore THEN 'NOTHING' ELSE 'UPDATE SET '||updates END);
  END IF;
  EXECUTE format('INSERT INTO public.%I (%s) SELECT %s FROM jsonb_populate_record(NULL::public.%I,$1)%s',p_table,cols,cols,p_table,clause) USING row_data;
 END LOOP;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.clause_write_fact(p_id text,p_owner text,
 p_table text,p_payload jsonb,p_conflict text,p_ignore boolean,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;cols text;conflicts text;updates text;clause text;field text;head text;j clause_jobs;p_account text;
BEGIN
 IF NOT clause_guard(p_id,p_owner) THEN RETURN false; END IF;
 SELECT * INTO j FROM clause_jobs WHERE id=p_id; p_account:=j.account_id;
 IF p_table<>ALL(ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history',
 'market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations',
 'market_observations','player_match_observations','league_rule_snapshots','player_round_history_checks',
 'rival_finance_evidence','complete_history_baselines','league_context_observations','player_membership_observations']) THEN RAISE EXCEPTION 'Invalid fact table'; END IF;
 -- Match PostgREST's default merge identity for mutable source catalogs.
 IF p_conflict IS NULL AND p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
  p_conflict:='id';
 END IF;
 IF p_observed IS NULL OR p_observed>now()+interval '60 seconds' THEN RAISE EXCEPTION 'Invalid source time'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
  IF jsonb_typeof(row_data)<>'object' OR octet_length(row_data::text)>1048576 THEN RAISE EXCEPTION 'Invalid fact row'; END IF;
  IF row_data ? 'account_id' AND row_data->>'account_id' IS DISTINCT FROM p_account THEN RAISE EXCEPTION 'Foreign fact account'; END IF;
  IF row_data ? 'championship_id' AND row_data->>'championship_id' IS DISTINCT FROM j.context->>'championship_id'
     THEN RAISE EXCEPTION 'Foreign fact league'; END IF;
  FOR field IN SELECT jsonb_object_keys(row_data) LOOP
   IF field ~* '(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)' OR NOT EXISTS(
    SELECT 1 FROM pg_attribute WHERE attrelid=('public.'||p_table)::regclass AND attname=field AND attnum>0 AND NOT attisdropped)
    THEN RAISE EXCEPTION 'Invalid fact field'; END IF;
  END LOOP;
  IF p_table='player_history' AND to_regprocedure('public.price_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM price_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_match_observations' AND to_regprocedure('public.match_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM match_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='rival_finance_evidence' AND to_regprocedure('public.finance_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM finance_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_membership_observations' THEN
   PERFORM membership_write(row_data,coalesce(p_ignore,false)); CONTINUE;
  END IF;
  IF p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
   head:=NULL;
   IF coalesce(row_data->>'id','')='' THEN RAISE EXCEPTION 'Missing fact identity'; END IF;
   INSERT INTO worker_fact_heads(table_name,entity_id,observed_at) VALUES(p_table,row_data->>'id',p_observed)
   ON CONFLICT(table_name,entity_id) DO UPDATE SET observed_at=excluded.observed_at
   WHERE worker_fact_heads.observed_at<=excluded.observed_at RETURNING entity_id INTO head;
   IF head IS NULL THEN CONTINUE; END IF;
  END IF;
  SELECT string_agg(format('%I',key),',' ORDER BY key),string_agg(format('%I=excluded.%I',key,key),',' ORDER BY key)
  INTO cols,updates FROM jsonb_object_keys(row_data) AS key;
  clause:='';
  IF p_conflict IS NOT NULL THEN
   IF p_conflict !~ '^[a-z_]+(,[a-z_]+)*$' THEN RAISE EXCEPTION 'Invalid fact conflict'; END IF;
   SELECT string_agg(format('%I',v),',') INTO conflicts FROM unnest(string_to_array(p_conflict,',')) AS v;
   clause:=format(' ON CONFLICT (%s) DO %s',conflicts,CASE WHEN p_ignore THEN 'NOTHING' ELSE 'UPDATE SET '||updates END);
  END IF;
  EXECUTE format('INSERT INTO public.%I (%s) SELECT %s FROM jsonb_populate_record(NULL::public.%I,$1)%s',p_table,cols,cols,p_table,clause) USING row_data;
 END LOOP;
 RETURN true;
END $$;
ANALYZE public.match_history_entities;
ANALYZE public.match_history_states;
ANALYZE public.match_history_times;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Normalize repeated historical facts: finance; first/last confirmations.
BEGIN;
SET LOCAL lock_timeout='5s';
SET LOCAL statement_timeout='120s';
SET LOCAL transaction_timeout='150s';
LOCK TABLE public.rival_finance_evidence IN ACCESS EXCLUSIVE MODE;
ALTER TABLE public.rival_finance_evidence RENAME TO rival_finance_evidence_legacy;
-- Bulk history scans use sequential reads and typed comparisons, not JSON conversion.
SET LOCAL enable_indexscan=off;
SET LOCAL enable_bitmapscan=off;
SET LOCAL work_mem='64MB';
SET LOCAL max_parallel_workers_per_gather=0;
CREATE TEMP TABLE finance_retained ON COMMIT DROP AS
 SELECT (original).* FROM (
 SELECT o AS original,(observed_at IS NULL OR NOT isfinite(observed_at)) AS uncertain,
 ROW(payload) AS fact,
 lag(ROW(payload)) OVER w AS preceding,
 lead(ROW(payload)) OVER w AS following
 FROM public.rival_finance_evidence_legacy o
 WINDOW w AS (PARTITION BY account_id,championship_id,competition_id,season,evidence_type,source_id ORDER BY observed_at)
 ) periods WHERE uncertain OR fact IS DISTINCT FROM preceding OR fact IS DISTINCT FROM following;
ANALYZE finance_retained;
SET LOCAL enable_indexscan=DEFAULT;
SET LOCAL enable_bitmapscan=DEFAULT;
SET LOCAL work_mem=DEFAULT;
SET LOCAL max_parallel_workers_per_gather=DEFAULT;

CREATE TABLE public.finance_history_entities(entity_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,account_id text NOT NULL CHECK(length(btrim(account_id))>0),championship_id text NOT NULL CHECK(length(btrim(championship_id))>0),competition_id text NOT NULL DEFAULT 'unknown',season text NOT NULL CHECK(length(btrim(season))>0),evidence_type text NOT NULL CHECK(evidence_type IN ('awards','rules','movements','team','coverage','eligibility','notifications')),source_id text NOT NULL CHECK(length(btrim(source_id))>0),UNIQUE NULLS NOT DISTINCT(account_id,championship_id,competition_id,season,evidence_type,source_id));
CREATE INDEX finance_history_scope ON public.finance_history_entities(championship_id,competition_id,season,account_id,evidence_type,source_id);
CREATE TABLE public.finance_history_states(state_id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,entity_id bigint NOT NULL REFERENCES public.finance_history_entities ON DELETE CASCADE,signature text NOT NULL,payload jsonb NOT NULL CHECK(jsonb_typeof(payload)='object' AND octet_length(payload::text)<=1048576),UNIQUE(state_id,entity_id));
CREATE INDEX finance_history_state_lookup ON public.finance_history_states(entity_id,signature);
CREATE TABLE public.finance_history_times(entity_id bigint NOT NULL REFERENCES public.finance_history_entities ON DELETE CASCADE,state_id bigint NOT NULL,observed_at timestamptz NOT NULL CHECK(isfinite(observed_at)),PRIMARY KEY(entity_id,observed_at),FOREIGN KEY(state_id,entity_id) REFERENCES public.finance_history_states(state_id,entity_id) ON DELETE CASCADE);
INSERT INTO public.finance_history_entities(account_id,championship_id,competition_id,season,evidence_type,source_id) SELECT DISTINCT account_id,championship_id,competition_id,season,evidence_type,source_id FROM public.rival_finance_evidence_legacy;
ANALYZE public.finance_history_entities;
INSERT INTO public.finance_history_states(entity_id,signature,payload) SELECT DISTINCT e.entity_id,md5(jsonb_build_array(o.payload)::text),o.payload FROM finance_retained o JOIN public.finance_history_entities e ON e.account_id=o.account_id AND e.championship_id=o.championship_id AND e.competition_id=o.competition_id AND e.season=o.season AND e.evidence_type=o.evidence_type AND e.source_id=o.source_id;
ANALYZE public.finance_history_states;
INSERT INTO public.finance_history_times(entity_id,state_id,observed_at) SELECT e.entity_id,s.state_id,o.observed_at FROM finance_retained o JOIN public.finance_history_entities e ON e.account_id=o.account_id AND e.championship_id=o.championship_id AND e.competition_id=o.competition_id AND e.season=o.season AND e.evidence_type=o.evidence_type AND e.source_id=o.source_id JOIN public.finance_history_states s ON s.entity_id=e.entity_id AND s.signature=md5(jsonb_build_array(o.payload)::text) AND s.payload IS NOT DISTINCT FROM o.payload;
DO $$ DECLARE columns text; BEGIN
 SELECT string_agg(CASE WHEN attname=ANY(ARRAY['account_id','championship_id','competition_id','season','evidence_type','source_id']) THEN 'e.' WHEN attname=ANY(ARRAY['payload']) THEN 's.' ELSE 't.' END||quote_ident(attname),',' ORDER BY attnum) INTO columns
 FROM pg_attribute WHERE attrelid='public.rival_finance_evidence_legacy'::regclass AND attnum>0 AND NOT attisdropped;
 EXECUTE 'CREATE VIEW public.rival_finance_evidence WITH(security_invoker=true) AS SELECT '||columns||' FROM public.finance_history_times t JOIN public.finance_history_entities e USING(entity_id) JOIN public.finance_history_states s ON s.state_id=t.state_id';
 IF EXISTS((SELECT to_jsonb(o) FROM finance_retained o EXCEPT ALL SELECT to_jsonb(v) FROM public.rival_finance_evidence v)
 UNION ALL (SELECT to_jsonb(v) FROM public.rival_finance_evidence v EXCEPT ALL SELECT to_jsonb(o) FROM finance_retained o)) THEN RAISE EXCEPTION 'rival_finance_evidence reconstruction mismatch'; END IF;
END $$;
CREATE FUNCTION public.finance_history_write(p_payload jsonb,p_conflict text DEFAULT NULL,p_ignore boolean DEFAULT false) RETURNS boolean
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE item jsonb;r public.rival_finance_evidence; previous jsonb; e bigint;s bigint; fields text[];
BEGIN
 IF p_payload IS NULL OR jsonb_typeof(p_payload) NOT IN ('array','object') THEN RAISE EXCEPTION 'Invalid history payload'; END IF;
 fields:=CASE WHEN p_conflict IS NULL THEN NULL ELSE string_to_array(p_conflict,',') END;
 IF fields IS NOT NULL AND fields IS DISTINCT FROM ARRAY['account_id','championship_id','competition_id','season','evidence_type','source_id','observed_at']  THEN RAISE EXCEPTION 'Unsupported history conflict'; END IF;
 FOR item IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
 IF jsonb_typeof(item)<>'object' OR EXISTS(SELECT 1 FROM jsonb_object_keys(item) k WHERE k<>ALL(ARRAY['account_id','championship_id','competition_id','season','evidence_type','source_id','payload','observed_at'])) THEN RAISE EXCEPTION 'Invalid history fields'; END IF;
 r:=jsonb_populate_record(NULL::public.rival_finance_evidence,jsonb_build_object('competition_id','unknown')||item);
 IF EXISTS(SELECT 1 FROM blocked_leagues WHERE championship_id=r.championship_id) THEN CONTINUE; END IF;
 IF r.account_id IS NULL OR r.championship_id IS NULL OR r.competition_id IS NULL OR r.season IS NULL OR r.evidence_type IS NULL OR r.source_id IS NULL OR r.payload IS NULL OR r.observed_at IS NULL THEN RAISE EXCEPTION 'Missing history value' USING ERRCODE='23502'; END IF;
 PERFORM pg_advisory_xact_lock(hashtextextended('finance'||jsonb_build_array(r.account_id,r.championship_id,r.competition_id,r.season,r.evidence_type,r.source_id)::text,0));
 previous:=NULL;
 IF fields IS NOT NULL THEN
SELECT to_jsonb(v) INTO previous FROM public.rival_finance_evidence v WHERE v.account_id=r.account_id AND v.championship_id=r.championship_id AND v.competition_id=r.competition_id AND v.season=r.season AND v.evidence_type=r.evidence_type AND v.source_id=r.source_id AND v.observed_at=r.observed_at;
 END IF;
 IF previous IS NOT NULL THEN
 IF p_ignore THEN CONTINUE; END IF;
 r:=jsonb_populate_record(NULL::public.rival_finance_evidence,previous||item);
 END IF;
 IF r.account_id IS NULL OR r.championship_id IS NULL OR r.competition_id IS NULL OR r.season IS NULL OR r.evidence_type IS NULL OR r.source_id IS NULL OR r.payload IS NULL OR r.observed_at IS NULL THEN RAISE EXCEPTION 'Missing history value' USING ERRCODE='23502'; END IF;
SELECT entity_id INTO e FROM finance_history_entities e WHERE e.account_id IS NOT DISTINCT FROM r.account_id AND e.championship_id IS NOT DISTINCT FROM r.championship_id AND e.competition_id IS NOT DISTINCT FROM r.competition_id AND e.season IS NOT DISTINCT FROM r.season AND e.evidence_type IS NOT DISTINCT FROM r.evidence_type AND e.source_id IS NOT DISTINCT FROM r.source_id;
IF e IS NULL THEN INSERT INTO finance_history_entities(account_id,championship_id,competition_id,season,evidence_type,source_id) VALUES(r.account_id,r.championship_id,r.competition_id,r.season,r.evidence_type,r.source_id) RETURNING entity_id INTO e; END IF;
SELECT state_id INTO s FROM finance_history_states s WHERE s.entity_id=e AND s.signature=md5(jsonb_build_array(r.payload)::text) AND s.payload IS NOT DISTINCT FROM r.payload;
IF s IS NULL THEN INSERT INTO finance_history_states(entity_id,signature,payload) VALUES(e,md5(jsonb_build_array(r.payload)::text),r.payload) RETURNING state_id INTO s; END IF;
IF previous IS NOT NULL THEN UPDATE finance_history_times SET state_id=s WHERE entity_id=e AND observed_at=r.observed_at; ELSE INSERT INTO finance_history_times VALUES(e,s,r.observed_at); END IF;

-- Keep both endpoints of consecutive identical states, never merge A/B/A.
DELETE FROM finance_history_times t USING (
 SELECT observed_at,state_id,lag(state_id) OVER w AS preceding,lead(state_id) OVER w AS following,
 observed_at IS NOT NULL AND isfinite(observed_at) AS eligible FROM finance_history_times WHERE entity_id=e
 WINDOW w AS (ORDER BY observed_at)
) interior WHERE t.entity_id=e AND t.observed_at=interior.observed_at AND interior.eligible
 AND interior.state_id=interior.preceding AND interior.state_id=interior.following;
END LOOP; RETURN true; END $$;
REVOKE ALL ON FUNCTION public.finance_history_write(jsonb,text,boolean) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.finance_history_write(jsonb,text,boolean) TO service_role;
ALTER TABLE public.finance_history_entities ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.finance_history_entities FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.finance_history_entities TO service_role;
ALTER TABLE public.finance_history_states ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.finance_history_states FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.finance_history_states TO service_role;
ALTER TABLE public.finance_history_times ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.finance_history_times FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.finance_history_times TO service_role;
GRANT USAGE,SELECT ON SEQUENCE public.finance_history_entities_entity_id_seq TO service_role;
GRANT USAGE,SELECT ON SEQUENCE public.finance_history_states_state_id_seq TO service_role;
REVOKE ALL ON public.rival_finance_evidence FROM PUBLIC,anon,authenticated;
GRANT SELECT ON public.rival_finance_evidence TO service_role;
CREATE OR REPLACE VIEW public.rival_finance_evidence_latest WITH(security_invoker=true) AS
 SELECT e.account_id,e.championship_id,e.season,e.evidence_type,e.source_id,t.observed_at,s.payload,e.competition_id
 FROM public.finance_history_entities e
 JOIN LATERAL (SELECT observed_at,state_id FROM public.finance_history_times WHERE entity_id=e.entity_id ORDER BY observed_at DESC LIMIT 1) t ON true
 JOIN public.finance_history_states s ON s.state_id=t.state_id;
DROP TABLE public.rival_finance_evidence_legacy;
CREATE OR REPLACE FUNCTION public.worker_write_fact(p_account text,p_key text,p_owner text,p_generation bigint,
 p_table text,p_payload jsonb,p_conflict text,p_ignore boolean,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;cols text;conflicts text;updates text;clause text;field text;head text;j worker_jobs;
BEGIN
 PERFORM 1 FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT worker_guard(p_account,p_key,p_owner,p_generation) THEN RETURN false; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key;
 IF p_table<>ALL(ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history',
 'market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations',
 'market_observations','player_match_observations','league_rule_snapshots','player_round_history_checks',
 'rival_finance_evidence','complete_history_baselines','league_context_observations','player_membership_observations']) THEN RAISE EXCEPTION 'Invalid fact table'; END IF;
 -- Match PostgREST's default merge identity for mutable source catalogs.
 IF p_conflict IS NULL AND p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
  p_conflict:='id';
 END IF;
 IF p_observed IS NULL OR p_observed>now()+interval '60 seconds' THEN RAISE EXCEPTION 'Invalid source time'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
  IF jsonb_typeof(row_data)<>'object' OR octet_length(row_data::text)>1048576 THEN RAISE EXCEPTION 'Invalid fact row'; END IF;
  IF row_data ? 'account_id' AND row_data->>'account_id' IS DISTINCT FROM p_account THEN RAISE EXCEPTION 'Foreign fact account'; END IF;
  IF row_data ? 'championship_id' AND row_data->>'championship_id' IS DISTINCT FROM j.descriptor->'context'->>'championship_id'
    AND j.descriptor->>'handler'<>'get_active_championships' THEN RAISE EXCEPTION 'Foreign fact league'; END IF;
  FOR field IN SELECT jsonb_object_keys(row_data) LOOP
   IF field ~* '(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)' OR NOT EXISTS(
    SELECT 1 FROM pg_attribute WHERE attrelid=('public.'||p_table)::regclass AND attname=field AND attnum>0 AND NOT attisdropped)
    THEN RAISE EXCEPTION 'Invalid fact field'; END IF;
  END LOOP;
  IF p_table='player_history' AND to_regprocedure('public.price_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM price_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_match_observations' AND to_regprocedure('public.match_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM match_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='rival_finance_evidence' AND to_regprocedure('public.finance_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM finance_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_membership_observations' THEN
   PERFORM membership_write(row_data,coalesce(p_ignore,false)); CONTINUE;
  END IF;
  IF p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
   head:=NULL;
   IF coalesce(row_data->>'id','')='' THEN RAISE EXCEPTION 'Missing fact identity'; END IF;
   INSERT INTO worker_fact_heads(table_name,entity_id,observed_at) VALUES(p_table,row_data->>'id',p_observed)
   ON CONFLICT(table_name,entity_id) DO UPDATE SET observed_at=excluded.observed_at
   WHERE worker_fact_heads.observed_at<=excluded.observed_at RETURNING entity_id INTO head;
   IF head IS NULL THEN CONTINUE; END IF;
  END IF;
  SELECT string_agg(format('%I',key),',' ORDER BY key),string_agg(format('%I=excluded.%I',key,key),',' ORDER BY key)
  INTO cols,updates FROM jsonb_object_keys(row_data) AS key;
  clause:='';
  IF p_conflict IS NOT NULL THEN
   IF p_conflict !~ '^[a-z_]+(,[a-z_]+)*$' THEN RAISE EXCEPTION 'Invalid fact conflict'; END IF;
   SELECT string_agg(format('%I',v),',') INTO conflicts FROM unnest(string_to_array(p_conflict,',')) AS v;
   clause:=format(' ON CONFLICT (%s) DO %s',conflicts,CASE WHEN p_ignore THEN 'NOTHING' ELSE 'UPDATE SET '||updates END);
  END IF;
  EXECUTE format('INSERT INTO public.%I (%s) SELECT %s FROM jsonb_populate_record(NULL::public.%I,$1)%s',p_table,cols,cols,p_table,clause) USING row_data;
 END LOOP;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION public.clause_write_fact(p_id text,p_owner text,
 p_table text,p_payload jsonb,p_conflict text,p_ignore boolean,p_observed timestamptz)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE row_data jsonb;cols text;conflicts text;updates text;clause text;field text;head text;j clause_jobs;p_account text;
BEGIN
 IF NOT clause_guard(p_id,p_owner) THEN RETURN false; END IF;
 SELECT * INTO j FROM clause_jobs WHERE id=p_id; p_account:=j.account_id;
 IF p_table<>ALL(ARRAY['championships','real_clubs','players','user_teams','user_team_history','player_history',
 'market_transactions','round_dream_team','player_daily_snapshots','auction_observations','bid_observations',
 'market_observations','player_match_observations','league_rule_snapshots','player_round_history_checks',
 'rival_finance_evidence','complete_history_baselines','league_context_observations','player_membership_observations']) THEN RAISE EXCEPTION 'Invalid fact table'; END IF;
 -- Match PostgREST's default merge identity for mutable source catalogs.
 IF p_conflict IS NULL AND p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
  p_conflict:='id';
 END IF;
 IF p_observed IS NULL OR p_observed>now()+interval '60 seconds' THEN RAISE EXCEPTION 'Invalid source time'; END IF;
 FOR row_data IN SELECT value FROM jsonb_array_elements(CASE WHEN jsonb_typeof(p_payload)='array' THEN p_payload ELSE jsonb_build_array(p_payload) END) LOOP
  IF jsonb_typeof(row_data)<>'object' OR octet_length(row_data::text)>1048576 THEN RAISE EXCEPTION 'Invalid fact row'; END IF;
  IF row_data ? 'account_id' AND row_data->>'account_id' IS DISTINCT FROM p_account THEN RAISE EXCEPTION 'Foreign fact account'; END IF;
  IF row_data ? 'championship_id' AND row_data->>'championship_id' IS DISTINCT FROM j.context->>'championship_id'
     THEN RAISE EXCEPTION 'Foreign fact league'; END IF;
  FOR field IN SELECT jsonb_object_keys(row_data) LOOP
   IF field ~* '(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)' OR NOT EXISTS(
    SELECT 1 FROM pg_attribute WHERE attrelid=('public.'||p_table)::regclass AND attname=field AND attnum>0 AND NOT attisdropped)
    THEN RAISE EXCEPTION 'Invalid fact field'; END IF;
  END LOOP;
  IF p_table='player_history' AND to_regprocedure('public.price_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM price_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_match_observations' AND to_regprocedure('public.match_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM match_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='rival_finance_evidence' AND to_regprocedure('public.finance_history_write(jsonb,text,boolean)') IS NOT NULL THEN PERFORM finance_history_write(row_data,p_conflict,p_ignore); CONTINUE; END IF;
  IF p_table='player_membership_observations' THEN
   PERFORM membership_write(row_data,coalesce(p_ignore,false)); CONTINUE;
  END IF;
  IF p_table=ANY(ARRAY['championships','real_clubs','players','user_teams']) THEN
   head:=NULL;
   IF coalesce(row_data->>'id','')='' THEN RAISE EXCEPTION 'Missing fact identity'; END IF;
   INSERT INTO worker_fact_heads(table_name,entity_id,observed_at) VALUES(p_table,row_data->>'id',p_observed)
   ON CONFLICT(table_name,entity_id) DO UPDATE SET observed_at=excluded.observed_at
   WHERE worker_fact_heads.observed_at<=excluded.observed_at RETURNING entity_id INTO head;
   IF head IS NULL THEN CONTINUE; END IF;
  END IF;
  SELECT string_agg(format('%I',key),',' ORDER BY key),string_agg(format('%I=excluded.%I',key,key),',' ORDER BY key)
  INTO cols,updates FROM jsonb_object_keys(row_data) AS key;
  clause:='';
  IF p_conflict IS NOT NULL THEN
   IF p_conflict !~ '^[a-z_]+(,[a-z_]+)*$' THEN RAISE EXCEPTION 'Invalid fact conflict'; END IF;
   SELECT string_agg(format('%I',v),',') INTO conflicts FROM unnest(string_to_array(p_conflict,',')) AS v;
   clause:=format(' ON CONFLICT (%s) DO %s',conflicts,CASE WHEN p_ignore THEN 'NOTHING' ELSE 'UPDATE SET '||updates END);
  END IF;
  EXECUTE format('INSERT INTO public.%I (%s) SELECT %s FROM jsonb_populate_record(NULL::public.%I,$1)%s',p_table,cols,cols,p_table,clause) USING row_data;
 END LOOP;
 RETURN true;
END $$;
ANALYZE public.finance_history_entities;
ANALYZE public.finance_history_states;
ANALYZE public.finance_history_times;
NOTIFY pgrst,'reload schema';
COMMIT;

-- Internal worker reads receive current metadata for unchanged versions, without chunks.
CREATE OR REPLACE FUNCTION public.worker_read_cached(p_account text,p_versions jsonb DEFAULT '{}')
RETURNS jsonb LANGUAGE plpgsql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE result jsonb;
BEGIN
 result:=public.worker_read(p_account,p_versions);
 RETURN result || jsonb_build_object('unchanged',coalesce((
 SELECT jsonb_agg(jsonb_build_object('key',s.resource_key,'manifest',to_jsonb(s)-'descriptor'))
 FROM public.worker_snapshots s JOIN jsonb_each_text(p_versions) v ON v.key=s.resource_key AND v.value=s.version::text
 WHERE s.account_id=p_account),'[]'::jsonb));
END $$;
REVOKE ALL ON FUNCTION public.worker_read_cached(text,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_read_cached(text,jsonb) TO service_role;
-- Keep legacy checkpoint read/write API compatible; new writers receive a tiny acknowledgement.
CREATE OR REPLACE FUNCTION public.worker_checkpoint_ack(p_account text,p_key text,p_owner text,p_generation bigint,p_request text,p_response jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 IF p_response IS NULL THEN RAISE EXCEPTION 'Missing checkpoint payload'; END IF;
 RETURN public.worker_checkpoint(p_account,p_key,p_owner,p_generation,p_request,p_response) IS NOT NULL;
END $$;
REVOKE ALL ON FUNCTION public.worker_checkpoint_ack(text,text,text,bigint,text,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_checkpoint_ack(text,text,text,bigint,text,jsonb) TO service_role;
NOTIFY pgrst,'reload schema';
