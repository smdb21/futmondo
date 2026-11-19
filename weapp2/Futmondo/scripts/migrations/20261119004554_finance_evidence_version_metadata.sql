CREATE OR REPLACE VIEW public.rival_finance_evidence_versions WITH(security_invoker=true) AS
 SELECT e.account_id,e.championship_id,e.season,e.evidence_type,e.source_id,t.observed_at,e.competition_id,t.state_id
 FROM public.finance_history_entities e
 JOIN LATERAL (SELECT observed_at,state_id FROM public.finance_history_times WHERE entity_id=e.entity_id ORDER BY observed_at DESC LIMIT 1) t ON true;
REVOKE ALL ON public.rival_finance_evidence_versions FROM PUBLIC,anon,authenticated;
GRANT SELECT ON public.rival_finance_evidence_versions TO service_role;
NOTIFY pgrst,'reload schema';
