-- Restore the seven verified facts created by the November 7, 9 and 10 repairs.
-- Require one resolved scoring context and matching final source selections.
-- Preserve original observations and append only absent, nonconflicting keys.
WITH scope AS (
  SELECT DISTINCT account_id, championship_id, competition_id, season, scoring_version
  FROM public.league_context_observations
  WHERE account_id = '5b55fb19be298c4b5913fc44'
    AND championship_id = '5b55f9d767214483120b87cf'
    AND complete AND competition_id <> 'unknown' AND season <> 'unknown'
), candidates AS (
  SELECT s.account_id, s.championship_id, s.competition_id, s.season,
         old.evidence_type, old.source_id, old.observed_at, old.payload
  FROM scope s
  JOIN public.rival_finance_evidence_latest old USING (account_id, championship_id)
  WHERE (SELECT count(*) FROM scope) = 1
    AND old.competition_id = 'unknown' AND old.season = 'unknown'
    AND old.evidence_type = 'eligibility' AND old.payload->'verified' = 'true'::jsonb
    AND EXISTS (
      SELECT 1 FROM public.rival_finance_evidence_latest a
      WHERE (a.account_id,a.championship_id,a.competition_id,a.season) =
            (s.account_id,s.championship_id,s.competition_id,s.season)
        AND a.evidence_type = 'awards' AND a.payload->>'finality' = 'final'
        AND a.payload->>'round_id' = old.payload->>'round_id'
        AND a.payload->>'scoring_version' = s.scoring_version
        AND (
          (old.payload->>'kind' = 'round_settlement_status'
            AND old.payload->>'status' = 'pending'
            AND old.payload->>'source' = 'user_confirmed_postponement'
            AND old.source_id = 'round-settlement:' || (old.payload->>'round_id')
            AND a.payload->>'round_number' = old.payload->>'round_number')
          OR (
            ((old.payload->>'source' = 'user_confirmed_unowned'
              AND old.source_id = 'user-confirmed-unowned:' || (old.payload->>'round_id') || ':' || (old.payload->>'player_id'))
             OR (old.payload->>'source' = 'user_provided_transfer_history'
              AND old.source_id = 'user-history-ownership:' || (old.payload->>'round_id') || ':' || (old.payload->>'player_id')))
            AND a.payload->'players' @> jsonb_build_array(jsonb_build_object('player_id',old.payload->>'player_id'))
            AND (
              (old.payload->>'ownership_status' = 'unowned' AND old.payload->>'team_id' = ''
               AND old.payload->'eligible' = 'false'::jsonb)
              OR (old.payload->>'ownership_status' = 'owned' AND old.payload->>'team_id' <> ''
                AND old.payload->'eligible' = 'true'::jsonb
                AND EXISTS (
                  SELECT 1 FROM public.rival_finance_evidence_latest h
                  WHERE (h.account_id,h.championship_id,h.competition_id,h.season) =
                        (s.account_id,s.championship_id,s.competition_id,s.season)
                    AND h.evidence_type = 'eligibility'
                    AND h.source_id = 'rounds:' || (old.payload->>'team_id')
                    AND h.payload->>'kind' = 'team_rounds'
                    AND h.payload->'rounds' @> jsonb_build_array(jsonb_build_object(
                      'id',old.payload->>'round_id','status','closed','negative',false))
                ))
            )
          )
        )
    )
), reconciled AS (
  SELECT c.*, e.observed_at IS NOT NULL AS already_scoped,
         e.observed_at IS NOT NULL AND e.payload IS DISTINCT FROM c.payload AS conflict
  FROM candidates c LEFT JOIN public.rival_finance_evidence_latest e
  USING (account_id,championship_id,competition_id,season,evidence_type,source_id)
)
SELECT count(*) AS verified,
       count(*) FILTER (WHERE already_scoped) AS already_scoped,
       count(*) FILTER (WHERE conflict) AS conflicting,
       CASE WHEN count(*) = 7 AND NOT bool_or(conflict) AND NOT bool_and(already_scoped)
            THEN public.finance_history_write(
              jsonb_agg(to_jsonb(reconciled) - 'already_scoped' - 'conflict') FILTER (WHERE NOT already_scoped),
              'account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at', true)
       END AS acknowledged
FROM reconciled;
