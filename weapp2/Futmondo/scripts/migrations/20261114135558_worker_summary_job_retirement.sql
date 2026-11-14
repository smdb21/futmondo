-- Retire only idle legacy summary jobs with an acknowledged current-context
-- replacement. Retain every factual snapshot/chunk and all ambiguous team scopes.
-- The database-only GUI and canonical worker discovery no longer recreate these
-- historical browser-generated descriptors, so no recurring queue scan is needed.
BEGIN;
WITH active AS MATERIALIZED (
 SELECT j.*,a.verified_leagues->(j.descriptor->'context'->>'championship_id') membership
 FROM public.worker_jobs j JOIN public.worker_accounts a USING(account_id)
 WHERE j.descriptor->>'handler'='get_player_summary'
 AND a.enabled AND a.auth_status='active' AND j.generation=a.generation
 AND j.descriptor->'context'->>'account_id'=j.account_id
 AND NOT(a.excluded_leagues ? (j.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b
  WHERE b.championship_id=j.descriptor->'context'->>'championship_id')
), canonical AS MATERIALIZED (
 SELECT j.* FROM active j JOIN public.worker_snapshots s USING(account_id,resource_key)
 WHERE s.handler='get_player_summary' AND s.context=j.descriptor->'context'
 AND s.status IN ('ok','empty') AND s.completeness IN ('complete','unknown') AND s.observed_at<=now()
 AND NOT EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
  WHERE coalesce(j.descriptor->'context'->>f,'unknown') IN ('','unknown')
  OR j.descriptor->'context'->>f IS DISTINCT FROM j.membership->>f)
), unknown_replaced AS (
 SELECT old.account_id,old.resource_key FROM active old JOIN canonical peer
 ON peer.account_id=old.account_id AND peer.resource_key<>old.resource_key
 AND peer.descriptor->'args'=old.descriptor->'args'
 AND ((peer.descriptor->'context')-ARRAY['competition_id','season','scoring_version'])=
     ((old.descriptor->'context')-ARRAY['competition_id','season','scoring_version'])
 WHERE old.owner IS NULL
 AND EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
  WHERE coalesce(old.descriptor->'context'->>f,'unknown') IN ('','unknown'))
 AND NOT EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) f
  WHERE coalesce(old.descriptor->'context'->>f,'unknown') NOT IN ('','unknown')
  AND old.descriptor->'context'->>f IS DISTINCT FROM peer.descriptor->'context'->>f)
 GROUP BY old.account_id,old.resource_key HAVING count(*)=1
), blank_team_replaced AS (
 SELECT old.account_id,old.resource_key FROM canonical old JOIN canonical peer
 ON peer.account_id=old.account_id AND peer.resource_key<>old.resource_key
 AND ((peer.descriptor->'args')-'user_team_id')=((old.descriptor->'args')-'user_team_id')
 AND ((peer.descriptor->'context')-'user_team_id')=((old.descriptor->'context')-'user_team_id')
 WHERE old.owner IS NULL AND coalesce(old.descriptor->'context'->>'user_team_id','')=''
 AND coalesce(peer.descriptor->'context'->>'user_team_id','')<>''
 GROUP BY old.account_id,old.resource_key HAVING count(*)=1
), retired AS (
 SELECT * FROM unknown_replaced UNION SELECT * FROM blank_team_replaced
)
DELETE FROM public.worker_jobs old USING retired
WHERE old.account_id=retired.account_id AND old.resource_key=retired.resource_key AND old.owner IS NULL;
COMMIT;
