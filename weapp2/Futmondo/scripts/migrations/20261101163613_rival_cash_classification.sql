-- Unknown transfer method is nullable. Never change identities or cash amounts.
BEGIN;
ALTER TABLE public.market_transactions ALTER COLUMN is_clause DROP NOT NULL;
ALTER TABLE public.market_transactions ALTER COLUMN is_clause DROP DEFAULT;
-- The historical importer hardcoded FALSE even for rival-to-rival transfers.
-- Explicit clause TRUE and ordinary transfers involving the System are retained.
UPDATE public.market_transactions
SET is_clause = NULL
WHERE is_clause IS FALSE
  AND provenance IN ('futmondo_pressroom','futmondo_pressroom:legacy_adopted')
  AND NULLIF(buyer_team_id, '') IS NOT NULL
  AND NULLIF(seller_team_id, '') IS NOT NULL;
COMMENT ON COLUMN public.market_transactions.is_clause IS
  'TRUE=observed clause, FALSE=observed ordinary/System transfer, NULL=method not established; never infer from current clause price.';

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
COMMIT;
