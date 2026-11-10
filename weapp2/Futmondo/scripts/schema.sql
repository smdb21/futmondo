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
