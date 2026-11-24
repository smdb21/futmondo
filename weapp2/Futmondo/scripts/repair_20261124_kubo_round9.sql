-- Scoped Kubo round 9 verification from the official owner history and transfers.
WITH scope AS (
  SELECT DISTINCT account_id, championship_id, competition_id, season
  FROM public.rival_finance_evidence_latest
  WHERE season = '2025-2026' AND competition_id <> 'unknown'
    AND payload->>'source' = 'user_confirmed_postponement'
), player AS (
  SELECT '5d5ea4ec12a917530bc3e4b0'::text AS player_id,
         '2026-09-14T15:59:51.571Z'::timestamptz AS market_since,
         '2026-10-22T04:23:04.295Z'::timestamptz AS next_owner
), evidence AS MATERIALIZED (
  SELECT e.* FROM public.rival_finance_evidence_latest e
  JOIN scope USING (account_id,championship_id,competition_id,season)
  WHERE (SELECT count(*) FROM scope) = 1
), round_identity AS (
  SELECT DISTINCT a.payload->>'round_id' AS round_id
  FROM evidence a CROSS JOIN player p
  WHERE a.evidence_type='awards' AND a.payload->>'round_number'='9'
    AND a.payload->>'finality'='final'
    AND a.payload->'players' @> jsonb_build_array(jsonb_build_object('player_id',p.player_id))
), boundaries AS (
  SELECT r.round_id, min((f->>'begin_process')::timestamptz) AS kickoff,
    (SELECT max((m.payload->>'date')::timestamptz) FROM evidence m
     WHERE m.evidence_type='movements' AND m.payload->>'type'='ranking'
       AND m.payload->>'concept'='9' AND (m.payload->>'money')::numeric>0) AS settled_at
  FROM round_identity r CROSS JOIN evidence e
  CROSS JOIN LATERAL jsonb_array_elements(e.payload->'fixture_rounds') f
  WHERE e.payload->>'kind'='league_round_calendar'
    AND f->>'round_id'=r.round_id AND f->'all_fixtures_dated'='true'::jsonb
  GROUP BY r.round_id
), facts AS (
  SELECT s.account_id,s.championship_id,s.competition_id,s.season,
         'eligibility'::text AS evidence_type,
         'verified-market-ownership:' || b.round_id || ':' || p.player_id AS source_id,
         statement_timestamp() AS observed_at,
         jsonb_build_object('round_id',b.round_id,'player_id',p.player_id,'team_id','',
           'eligible',false,'verified',true,'ownership_status','unowned',
           'source','official_owner_history_and_market_transfer',
           'market_since',p.market_since,'next_owner_transition',p.next_owner,
           'award_window_from',b.kickoff,'award_window_through',b.settled_at) AS payload
  FROM scope s CROSS JOIN player p CROSS JOIN boundaries b
  WHERE (SELECT count(*) FROM round_identity)=1
    AND p.market_since < b.kickoff AND b.kickoff < b.settled_at AND b.settled_at < p.next_owner
    AND EXISTS (
      SELECT 1 FROM public.market_transactions t WHERE t.championship_id::text=s.championship_id
        AND t.player_id::text=p.player_id AND t.source_event_id IS NOT NULL
        AND coalesce(t.buyer_team_id::text,'')=''
        AND coalesce(t.seller_team_id::text,'')<>''
        AND abs(extract(epoch FROM t.transaction_date-p.market_since))<=1
    )
    AND NOT EXISTS (
      SELECT 1 FROM public.market_transactions t WHERE t.championship_id::text=s.championship_id
        AND t.player_id::text=p.player_id AND t.source_event_id IS NOT NULL
        AND t.transaction_date>p.market_since+interval '1 second'
        AND t.transaction_date<=b.settled_at
    )
    AND NOT EXISTS (
      SELECT 1 FROM evidence e WHERE e.evidence_type='eligibility'
        AND e.payload->>'round_id'=b.round_id
        AND (e.payload->>'kind'='round_lineup'
          AND (e.payload->'players' ? p.player_id OR e.payload->'bench' ? p.player_id)
          OR e.payload->>'player_id'=p.player_id AND e.payload->'verified'='true'::jsonb
             AND coalesce(e.payload->>'team_id','')<>'')
    )
    AND NOT EXISTS (
      SELECT 1 FROM public.user_teams u WHERE u.championship_id::text=s.championship_id
        AND lower(u.name)='futmondo'
    )
), reconciled AS (
 SELECT f.*,existing.observed_at IS NOT NULL AS already_scoped,
        existing.observed_at IS NOT NULL AND existing.payload IS DISTINCT FROM f.payload AS conflict
 FROM facts f LEFT JOIN public.rival_finance_evidence_latest existing
 USING(account_id,championship_id,competition_id,season,evidence_type,source_id)
)
SELECT count(*) AS verified,count(*) FILTER (WHERE already_scoped) AS already_scoped,
       count(*) FILTER (WHERE conflict) AS conflicts,
       CASE WHEN count(*)=1 AND NOT bool_or(conflict) AND NOT bool_or(already_scoped) THEN
         public.finance_history_write(jsonb_agg(to_jsonb(reconciled)-'already_scoped'-'conflict'),
           'account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at',true)
       END AS acknowledged
FROM reconciled;
