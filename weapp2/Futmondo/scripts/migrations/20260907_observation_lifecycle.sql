-- Apply after 20260907_private_history.sql in staging before production.
BEGIN;
ALTER TABLE player_history ADD COLUMN IF NOT EXISTS observed_at TIMESTAMPTZ;
ALTER TABLE user_team_history ADD COLUMN IF NOT EXISTS observed_at TIMESTAMPTZ;
ALTER TABLE player_daily_snapshots ADD COLUMN IF NOT EXISTS observed_at TIMESTAMPTZ;
CREATE UNIQUE INDEX IF NOT EXISTS user_team_history_source_time
  ON user_team_history(user_team_id,championship_id,observed_at);
CREATE UNIQUE INDEX IF NOT EXISTS player_history_source_time
  ON player_history(player_id,championship_id,observed_at);
ALTER TABLE player_match_observations ADD COLUMN IF NOT EXISTS occurred_at TIMESTAMPTZ;
ALTER TABLE player_match_observations ADD COLUMN IF NOT EXISTS round_start_at TIMESTAMPTZ;

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

CREATE TABLE IF NOT EXISTS league_preferences (
  user_id TEXT NOT NULL, championship_id TEXT NOT NULL, user_team_id TEXT NOT NULL,
  auto_coach BOOLEAN NOT NULL DEFAULT TRUE, updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY(user_id,championship_id,user_team_id)
);
CREATE TABLE IF NOT EXISTS fixture_observations (
  championship_id TEXT NOT NULL, season TEXT NOT NULL, scoring_version TEXT NOT NULL,
  player_id TEXT NOT NULL, round NUMERIC NOT NULL, fixture_id TEXT NOT NULL,
  occurred_at TIMESTAMPTZ NOT NULL, observed_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY(championship_id,season,scoring_version,player_id,round,observed_at)
);
CREATE TABLE IF NOT EXISTS auction_opportunities (
  championship_id TEXT NOT NULL, listing_id TEXT NOT NULL, player_id TEXT NOT NULL,
  first_observed_at TIMESTAMPTZ NOT NULL, observed_at TIMESTAMPTZ NOT NULL,
  expires_at TIMESTAMPTZ, outcome TEXT NOT NULL DEFAULT 'open'
    CHECK(outcome IN ('open','sold','unsold','unknown')),
  auction_id TEXT, eligible_manager_ids JSONB, visibility_complete BOOLEAN NOT NULL DEFAULT FALSE,
  evidence JSONB NOT NULL DEFAULT '{}',
  PRIMARY KEY(championship_id,listing_id,observed_at)
);
CREATE TABLE IF NOT EXISTS observation_subscriptions (
  user_id TEXT NOT NULL, championship_id TEXT NOT NULL, user_team_id TEXT NOT NULL,
  enabled BOOLEAN NOT NULL DEFAULT FALSE, next_run_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  last_player_id TEXT, last_error TEXT, last_success_at TIMESTAMPTZ,
  PRIMARY KEY(user_id,championship_id,user_team_id)
);
CREATE TABLE IF NOT EXISTS sale_observations (
  user_id TEXT NOT NULL, championship_id TEXT NOT NULL, user_team_id TEXT NOT NULL,
  offer_id TEXT NOT NULL, player_id TEXT NOT NULL, amount NUMERIC NOT NULL,
  market_value NUMERIC, observed_at TIMESTAMPTZ NOT NULL, expires_at TIMESTAMPTZ,
  counterparty TEXT NOT NULL DEFAULT 'unknown', outcome TEXT NOT NULL DEFAULT 'offered',
  PRIMARY KEY(user_id,championship_id,offer_id,observed_at)
);
CREATE TABLE IF NOT EXISTS model_evaluations (
  id TEXT PRIMARY KEY, user_id TEXT NOT NULL, championship_id TEXT NOT NULL,
  model_version TEXT NOT NULL, cutoff TIMESTAMPTZ NOT NULL, status TEXT NOT NULL,
  metrics JSONB NOT NULL, evidence JSONB NOT NULL, approved BOOLEAN NOT NULL DEFAULT FALSE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
DO $$ DECLARE name TEXT; BEGIN
  FOREACH name IN ARRAY ARRAY['observation_revisions','league_preferences','observation_subscriptions','sale_observations','model_evaluations','fixture_observations','auction_opportunities'] LOOP
    EXECUTE format('ALTER TABLE %I ENABLE ROW LEVEL SECURITY',name);
    EXECUTE format('REVOKE ALL ON TABLE %I FROM anon, authenticated',name);
    EXECUTE format('GRANT ALL ON TABLE %I TO service_role',name);
  END LOOP;
END $$;
COMMIT;
