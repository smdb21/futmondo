-- Apply only after the factual application is deployed and verified.
-- Retired rows are preserved outside the public API schema; no CASCADE deletes.
BEGIN;
DROP FUNCTION IF EXISTS public.claim_connected_automation_job(TEXT,TEXT,TEXT,TEXT);
DROP FUNCTION IF EXISTS public.begin_automation_execution(TEXT,TEXT,JSONB);
DROP FUNCTION IF EXISTS public.finish_automation_job(TEXT,TEXT,JSONB);
DROP FUNCTION IF EXISTS public.claim_automation_job();
CREATE SCHEMA IF NOT EXISTS retired_features;
REVOKE ALL ON SCHEMA retired_features FROM PUBLIC, anon, authenticated, service_role;
DO $$
DECLARE tbl TEXT;
BEGIN
  FOREACH tbl IN ARRAY ARRAY['automation_jobs','automation_policies','automation_account_locks','automation_sessions','forecast_records','transfer_scenarios','model_evaluations','manager_dna_profiles','decision_log','user_smart_alerts','league_preferences','observation_subscriptions'] LOOP
    IF to_regclass(format('public.%I',tbl)) IS NOT NULL THEN
      IF to_regclass(format('retired_features.%I',tbl)) IS NOT NULL THEN
        RAISE EXCEPTION 'Both public and retired tables exist for %; resolve before retirement',tbl;
      END IF;
      EXECUTE format('REVOKE ALL ON TABLE public.%I FROM PUBLIC, anon, authenticated, service_role',tbl);
      EXECUTE format('ALTER TABLE public.%I SET SCHEMA retired_features',tbl);
    END IF;
  END LOOP;
END $$;
REVOKE ALL ON ALL TABLES IN SCHEMA retired_features FROM PUBLIC, anon, authenticated, service_role;
REVOKE ALL ON ALL SEQUENCES IN SCHEMA retired_features FROM PUBLIC, anon, authenticated, service_role;
COMMIT;
