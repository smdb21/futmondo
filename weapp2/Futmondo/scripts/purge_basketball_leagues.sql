-- REQUIRES EXPLICIT APPROVAL: permanently removes app-stored basketball league data.
-- Never executed by startup or the block-only migration. No Futmondo writes.
BEGIN;
-- Purge only leagues identified by authoritative source sport metadata.
DO $$ DECLARE blocked text[];affected text[];tbl text;
BEGIN
 SELECT array_agg(championship_id) INTO blocked FROM public.blocked_leagues;
 IF blocked IS NULL THEN RETURN; END IF;
 SELECT array_agg(account_id) INTO affected FROM public.worker_accounts a
 WHERE EXISTS(SELECT 1 FROM unnest(blocked) id WHERE a.verified_leagues ? id)
 OR EXISTS(SELECT 1 FROM public.worker_jobs j WHERE j.account_id=a.account_id AND j.descriptor->'context'->>'championship_id'=ANY(blocked));
 UPDATE public.worker_accounts a SET verified_leagues=a.verified_leagues-blocked,
  excluded_leagues=(SELECT jsonb_agg(DISTINCT value) FROM jsonb_array_elements(a.excluded_leagues||to_jsonb(blocked)))
 WHERE a.account_id=ANY(affected);
 DELETE FROM public.worker_chunks c WHERE EXISTS(SELECT 1 FROM public.worker_snapshots s
  WHERE s.account_id=c.account_id AND s.resource_key=c.resource_key AND
  (s.context->>'championship_id'=ANY(blocked) OR s.handler='get_active_championships' AND s.account_id=ANY(affected)))
 OR EXISTS(SELECT 1 FROM public.worker_jobs j WHERE j.account_id=c.account_id AND j.resource_key=c.resource_key
  AND j.descriptor->'context'->>'championship_id'=ANY(blocked));
 DELETE FROM public.worker_snapshots WHERE context->>'championship_id'=ANY(blocked)
  OR handler='get_active_championships' AND account_id=ANY(affected);
 DELETE FROM public.worker_jobs WHERE descriptor->'context'->>'championship_id'=ANY(blocked);
 DELETE FROM public.worker_checkpoints c USING public.worker_jobs j
  WHERE c.account_id=j.account_id AND c.resource_key=j.resource_key AND j.account_id=ANY(affected)
  AND j.descriptor->>'handler'='get_active_championships';
 UPDATE public.worker_jobs SET due_at=now(),requested_at=now(),priority=10
  WHERE account_id=ANY(affected) AND descriptor->>'handler'='get_active_championships';
 FOREACH tbl IN ARRAY ARRAY['auction_observations','auction_opportunities','bid_observations','complete_history_baselines','fixture_observations','league_cash_defaults','league_context_observations','league_rule_snapshots','market_observations','market_transactions','market_transactions_archive','observation_revisions','player_daily_snapshots','player_history','player_match_observations','player_round_history_checks','rival_finance_evidence','round_dream_team','sale_observations','team_lineup_snapshots','user_team_history','user_league_preferences','user_teams'] LOOP
  IF to_regclass('public.'||tbl) IS NOT NULL THEN
   EXECUTE format('DELETE FROM public.%I WHERE championship_id=ANY($1)',tbl) USING blocked;
  END IF;
 END LOOP;
 DELETE FROM public.championships WHERE id=ANY(blocked);
END $$;
NOTIFY pgrst,'reload schema';
COMMIT;
