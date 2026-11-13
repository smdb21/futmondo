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
