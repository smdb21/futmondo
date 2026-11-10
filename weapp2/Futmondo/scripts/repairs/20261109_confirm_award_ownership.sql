-- User-supplied official-app Historial screenshots, corroborated by saved transfers.
-- These are two round-scoped facts, not a claim of complete owner-history coverage.
-- Screenshot dates retain their displayed day precision and unspecified timezone.
-- Replaying this repair does not append another revision for the same source ID.
WITH confirmations (round_id, player_id, team_id, eligible, ownership_status,
                    owner_label, from_date, until_date, attachment) AS (
  VALUES
    ('6a4af7efae633549bd0f03a0', '575c2bacaf91c7b04d8c2513',
     '5f3475b38aff9b7b3242c07d', true, 'owned', 'Hala Modric',
     '2026-09-03', '2026-10-22', '1000086217.jpg'),
    ('6a4af7f0ae633549bd0f03cc', '504f65184d8bec9a670005f7',
     '', false, 'unowned', 'futmondo',
     '2026-10-09', '2026-10-15', '1000086216.jpg')
), evidence AS (
  SELECT '5b55fb19be298c4b5913fc44'::text AS account_id,
         '5b55f9d767214483120b87cf'::text AS championship_id,
         'unknown'::text AS season, 'eligibility'::text AS evidence_type,
         'user-history-ownership:' || round_id || ':' || player_id AS source_id,
         jsonb_build_object(
           'round_id', round_id, 'player_id', player_id, 'team_id', team_id,
           'eligible', eligible, 'verified', true,
           'ownership_status', ownership_status,
           'source', 'user_provided_transfer_history',
           'owner_label', owner_label, 'from_date', from_date,
           'until_date', until_date, 'date_precision', 'day',
           'date_timezone', 'unspecified', 'source_attachment', attachment
         ) AS payload
  FROM confirmations
)
INSERT INTO public.rival_finance_evidence
  (account_id, championship_id, season, evidence_type, source_id, observed_at, payload)
SELECT e.account_id, e.championship_id, e.season, e.evidence_type, e.source_id,
       statement_timestamp(), e.payload
FROM evidence e
WHERE NOT EXISTS (
  SELECT 1 FROM public.rival_finance_evidence existing
  WHERE existing.account_id = e.account_id
    AND existing.championship_id = e.championship_id
    AND existing.season = e.season
    AND existing.evidence_type = e.evidence_type
    AND existing.source_id = e.source_id
)
ON CONFLICT DO NOTHING
RETURNING source_id, payload->>'ownership_status' AS ownership_status;
