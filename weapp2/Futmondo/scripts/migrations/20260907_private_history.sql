-- Follow-up to 20260905_reliable_insights.sql; review before deployment.
-- Keep legacy league/account history server-only and adopt unambiguous legacy
-- transfers during source-identified ingestion. No rows are deleted.
BEGIN;

-- League ownership belongs on each historical record, not only on the current
-- team row used by a query join. Backfill from the immutable Futmondo team ID.
ALTER TABLE public.user_team_history ADD COLUMN IF NOT EXISTS championship_id TEXT
  REFERENCES public.championships(id) ON DELETE CASCADE;
UPDATE public.user_team_history AS history
  SET championship_id = team.championship_id
  FROM public.user_teams AS team
  WHERE history.user_team_id = team.id AND history.championship_id IS NULL;
CREATE INDEX IF NOT EXISTS team_history_championship_recorded
  ON public.user_team_history(championship_id, recorded_at DESC);

-- Old orphaned records are preserved for audit. NOT VALID preserves only those
-- existing unknown rows; every new/changed row must have an explicit league.
DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint
      WHERE conrelid='public.user_team_history'::regclass
        AND conname='user_team_history_championship_required') THEN
    ALTER TABLE public.user_team_history
      ADD CONSTRAINT user_team_history_championship_required
      CHECK (championship_id IS NOT NULL) NOT VALID;
  END IF;
END $$;

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

-- Player identity and club identity remain global. League-specific ratings and
-- eligible roles live beside league-specific points/value/ownership snapshots.
ALTER TABLE public.player_daily_snapshots ADD COLUMN IF NOT EXISTS role TEXT;
ALTER TABLE public.player_daily_snapshots ADD COLUMN IF NOT EXISTS role2 TEXT;
ALTER TABLE public.player_daily_snapshots ADD COLUMN IF NOT EXISTS primary_role TEXT;
ALTER TABLE public.player_daily_snapshots ADD COLUMN IF NOT EXISTS rating NUMERIC;

DO $$
DECLARE table_name TEXT;
BEGIN
  FOREACH table_name IN ARRAY ARRAY[
    'championships', 'user_teams', 'user_team_history', 'player_history',
    'market_transactions', 'round_dream_team', 'player_daily_snapshots',
    'manager_dna_profiles', 'decision_log'
  ] LOOP
    EXECUTE format('ALTER TABLE public.%I ENABLE ROW LEVEL SECURITY', table_name);
    EXECUTE format('REVOKE ALL ON TABLE public.%I FROM anon, authenticated', table_name);
    EXECUTE format('GRANT ALL ON TABLE public.%I TO service_role', table_name);
  END LOOP;
END $$;

-- Table privileges do not automatically grant access to BIGSERIAL sequences.
-- Grant only sequences owned by the protected tables, rather than all sequences
-- in the public schema. Anonymous/browser roles have no sequence privileges.
DO $$
DECLARE sequence_name TEXT;
BEGIN
  FOR sequence_name IN
    SELECT pg_get_serial_sequence(format('public.%I', table_name), 'id')
      FROM unnest(ARRAY[
        'user_team_history', 'player_history', 'market_transactions',
        'round_dream_team', 'player_daily_snapshots', 'decision_log',
        'user_smart_alerts', 'transfer_scenarios'
      ]) AS tables(table_name)
  LOOP
    IF sequence_name IS NOT NULL THEN
      EXECUTE format('REVOKE ALL ON SEQUENCE %s FROM anon, authenticated', sequence_name);
      EXECUTE format('GRANT USAGE, SELECT ON SEQUENCE %s TO service_role', sequence_name);
    END IF;
  END LOOP;
END $$;

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

COMMIT;
