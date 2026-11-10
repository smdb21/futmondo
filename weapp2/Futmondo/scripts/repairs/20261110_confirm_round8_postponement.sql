-- The league owner confirmed no round-8 position payment because a game is
-- postponed to 9 December. This records pending status, never a zero payment.
INSERT INTO public.rival_finance_evidence
  (account_id, championship_id, season, evidence_type, source_id, observed_at, payload)
SELECT '5b55fb19be298c4b5913fc44', '5b55f9d767214483120b87cf', 'unknown',
       'eligibility', 'round-settlement:6a4af7f0ae633549bd0f03cc', statement_timestamp(),
       jsonb_build_object('kind','round_settlement_status','version',1,
         'round_id','6a4af7f0ae633549bd0f03cc','round_number',8,
         'status','pending','verified',true,
         'postponed_match_date','2026-12-09','date_precision','day',
         'source','user_confirmed_postponement')
WHERE NOT EXISTS (
  SELECT 1 FROM public.rival_finance_evidence
  WHERE account_id='5b55fb19be298c4b5913fc44'
    AND championship_id='5b55f9d767214483120b87cf' AND season='unknown'
    AND evidence_type='eligibility'
    AND source_id='round-settlement:6a4af7f0ae633549bd0f03cc'
)
ON CONFLICT DO NOTHING
RETURNING source_id, payload->>'status' AS status;
