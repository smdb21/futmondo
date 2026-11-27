-- Reconstruct the round 12 payment basis from the first closed team histories.
-- Resolve the legacy archive through the exact account, league, round, teams
-- and matching first observations in the resolved competition and season.
WITH scope AS (
  SELECT account_id,championship_id,competition_id,season,payload->>'team_id' AS reference,
    payload->>'round_id' AS explicit_round,(payload->>'date')::timestamptz AS paid_at,
    (payload->>'money')::numeric AS point_credit
  FROM public.rival_finance_evidence_latest
  WHERE championship_id='5b55f9d767214483120b87cf' AND competition_id='504e4f584d8bec9a67000079'
    AND season='2025-2026' AND evidence_type='movements'
    AND payload->>'type'='points' AND payload->>'concept'='12'
), round_key AS (
  SELECT s.*,c.payload->>'round_id' AS rid,c.payload->'answer'->'ranking' AS ranking,
    (m.payload->>'money')::numeric AS rank_credit,(m.payload->>'date')::timestamptz AS rank_at
  FROM scope s JOIN public.rival_finance_evidence_latest c USING(account_id,championship_id,competition_id,season)
  JOIN public.rival_finance_evidence_latest m USING(account_id,championship_id,competition_id,season)
  WHERE c.payload->>'kind'='classification_round_ranking' AND c.payload->>'round_number'='12'
    AND m.evidence_type='movements' AND m.payload->>'type'='ranking' AND m.payload->>'concept'='12'
    AND m.payload->>'team_id'=s.reference AND s.point_credit=6230000 AND (m.payload->>'money')::numeric=2333333
    AND (s.explicit_round IS NULL OR s.explicit_round IN ('',c.payload->>'round_id'))
    AND EXISTS (SELECT 1 FROM public.rival_finance_evidence_latest rule
      WHERE (rule.account_id,rule.championship_id,rule.competition_id,rule.season)=
        (s.account_id,s.championship_id,s.competition_id,s.season) AND rule.evidence_type='rules'
        AND rule.payload->'configuration' @> '{"moneyPerPoint":70000,"moneyPerRanking":30000000,"rankingMode":"flop","usersToRank":-1}')
), first_closed AS (
  SELECT DISTINCT ON (e.source_id,e.competition_id) e.source_id,e.competition_id,
    e.payload->>'team_id' AS team,(r->>'points')::int AS pts,e.observed_at
  FROM round_key k JOIN public.rival_finance_evidence e USING(account_id,championship_id)
  CROSS JOIN LATERAL jsonb_array_elements(e.payload->'rounds') r
  WHERE (SELECT count(*) FROM round_key)=1 AND e.evidence_type='eligibility'
    AND e.source_id='rounds:'||(e.payload->>'team_id') AND e.payload->>'kind'='team_rounds'
    AND r->>'id'=k.rid AND r->>'number'='12' AND r->>'status'='closed'
    AND r->'negative'='false'::jsonb AND jsonb_typeof(r->'points')='number'
    AND e.observed_at>=greatest(k.paid_at,k.rank_at) AND
      ((e.competition_id='unknown' AND e.season='unknown' AND e.observed_at<least(k.paid_at,k.rank_at)+interval '1 day')
       OR (e.competition_id=k.competition_id AND e.season=k.season AND e.observed_at<k.paid_at+interval '4 days'))
  ORDER BY e.source_id,e.competition_id,e.observed_at
), scores AS (
  SELECT a.* FROM first_closed a JOIN first_closed b USING(team,pts)
  WHERE a.competition_id='unknown' AND b.competition_id<>'unknown'
), slots AS (
  SELECT *,floor(30000000::numeric*(10-row_number() OVER(ORDER BY pts,team))/45+0.5) AS prize
  FROM scores
), amounts AS (
  SELECT *,floor(avg(prize) OVER(PARTITION BY pts)) AS position_credit FROM slots
), fact AS (
  SELECT k.account_id,k.championship_id,k.competition_id,k.season,'eligibility' AS evidence_type,
    'paid-scores:'||k.rid AS source_id,(SELECT max(observed_at) FROM scores) AS observed_at,
    jsonb_build_object('kind','settled_round_scores','round_id',k.rid,'round_number',12,
      'status','closed','reference_team_id',k.reference,'source','first_closed_team_round_histories',
      'ranking',(SELECT jsonb_agg(jsonb_build_object('id',team,'points',pts,'negative',false) ORDER BY team) FROM scores)) AS payload
  FROM round_key k
  WHERE (SELECT count(*) FROM scores)=9 AND jsonb_array_length(k.ranking)=9
    AND (SELECT max(observed_at)-min(observed_at) FROM scores)<interval '1 minute'
    AND NOT EXISTS (SELECT 1 FROM jsonb_array_elements(k.ranking) x WHERE NOT EXISTS
      (SELECT 1 FROM scores WHERE team=x->>'id'))
    AND EXISTS (SELECT 1 FROM amounts WHERE team=k.reference AND pts*70000=k.point_credit AND position_credit=k.rank_credit)
), checked AS (
  SELECT f.*,old.observed_at IS NOT NULL AS existing,old.observed_at IS NOT NULL AND
    (old.payload<>f.payload OR old.observed_at<>f.observed_at) AS conflict
  FROM fact f LEFT JOIN public.rival_finance_evidence_latest old
    USING(account_id,championship_id,competition_id,season,evidence_type,source_id)
)
SELECT count(*) AS verified,count(*) FILTER (WHERE existing) AS existing,count(*) FILTER (WHERE conflict) AS conflicts,
  CASE WHEN count(*)=1 AND NOT bool_or(existing) THEN
    public.finance_history_write(jsonb_agg(to_jsonb(checked)-'existing'-'conflict'),
      'account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at',true)
  END AS acknowledged
FROM checked;
