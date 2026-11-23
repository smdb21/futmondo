BEGIN;

ALTER TABLE public.worker_jobs ADD COLUMN league_refresh_at timestamptz;

CREATE FUNCTION public.worker_league_refresh(
 p_account text, p_league text, p_team text, p_players text[] DEFAULT '{}',
 p_core boolean DEFAULT true, p_observed_request timestamptz DEFAULT NULL
) RETURNS integer LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts; requested timestamptz:=coalesce(p_observed_request,clock_timestamp()); affected integer;
BEGIN
 IF coalesce(p_league,'')='' OR coalesce(p_team,'')='' OR cardinality(p_players)>64
  OR requested>clock_timestamp() OR requested<now()-interval '1 hour' THEN RETURN 0; END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active'
  OR NOT(a.verified_leagues ? p_league) OR a.excluded_leagues ? p_league
  OR EXISTS(SELECT 1 FROM blocked_leagues WHERE championship_id=p_league) THEN RETURN 0; END IF;
 UPDATE worker_jobs j SET
  league_refresh_at=requested, requested_at=greatest(j.requested_at,requested),
  due_at=CASE WHEN j.owner IS NULL AND j.last_error IS NULL THEN least(j.due_at,now()) ELSE j.due_at END
 WHERE j.account_id=p_account AND j.generation=a.generation AND j.descriptor->'context'->>'championship_id'=p_league
  AND ((p_core AND (
   (j.descriptor->>'handler' IN ('get_championship_players','get_championship_pressroom')
    AND j.descriptor->'context'->>'user_team_id'='')
   OR (j.descriptor->>'handler' IN ('get_user_team_info','get_market_players','get_players_from_team','get_financial_snapshot')
    AND j.descriptor->'context'->>'user_team_id'=p_team)))
   OR (j.descriptor->>'handler'='get_player_summary' AND j.descriptor->'context'->>'user_team_id'=p_team
    AND j.descriptor->'context'->>'player_id'=ANY(p_players)))
  AND NOT EXISTS(SELECT 1 FROM unnest(ARRAY['competition_id','season','scoring_version']) AS scope(field)
   WHERE a.verified_leagues->p_league->>field NOT IN ('','unknown')
    AND coalesce(j.descriptor->'context'->>field,'unknown') NOT IN ('','unknown',a.verified_leagues->p_league->>field))
  AND NOT public.worker_has_resolved_peer(p_account,j.descriptor)
  AND coalesce(j.league_refresh_at,'-infinity')<requested
  AND (p_observed_request IS NULL OR NOT EXISTS(
   SELECT 1 FROM worker_snapshots snap WHERE snap.account_id=j.account_id AND snap.resource_key=j.resource_key
    AND snap.observed_at>=requested AND snap.status IN ('ok','empty')))
  AND (NOT p_core OR cardinality(p_players)>0 OR p_observed_request IS NOT NULL
       OR coalesce(j.league_refresh_at,'-infinity')<=requested-interval '5 seconds');
 GET DIAGNOSTICS affected=ROW_COUNT;
 IF affected=0 AND p_core AND cardinality(p_players)=0 AND EXISTS(
  SELECT 1 FROM worker_jobs j WHERE j.account_id=p_account AND j.generation=a.generation
   AND j.descriptor->'context'->>'championship_id'=p_league AND j.descriptor->'context'->>'user_team_id'=p_team
   AND j.descriptor->>'handler'='get_user_team_info' AND j.league_refresh_at>requested-interval '5 seconds'
 ) THEN RETURN 1; END IF;
 RETURN affected;
END $$;

-- A run that started before the request publishes using its own source clock
-- and remains due for one follow-up. A completed post-request run consumes it.
CREATE FUNCTION public.worker_consume_league_refresh() RETURNS trigger
LANGUAGE plpgsql SECURITY INVOKER SET search_path='' AS $$
BEGIN
 IF OLD.started_at>=OLD.league_refresh_at THEN NEW.league_refresh_at:=NULL; END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER worker_consume_league_refresh BEFORE UPDATE OF run_id ON public.worker_jobs
 FOR EACH ROW WHEN (OLD.league_refresh_at IS NOT NULL AND OLD.run_id IS DISTINCT FROM NEW.run_id)
 EXECUTE FUNCTION public.worker_consume_league_refresh();
REVOKE ALL ON FUNCTION public.worker_consume_league_refresh() FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_consume_league_refresh() TO service_role;

CREATE OR REPLACE FUNCTION public.worker_claim(p_owner text,p_foreground boolean DEFAULT false)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE j worker_jobs;candidate worker_jobs;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 FOR candidate IN
 WITH dispatched AS MATERIALIZED (
  SELECT account_id,descriptor->>'handler' handler,max(started_at) served_at FROM worker_jobs GROUP BY 1,2)
 SELECT q.* FROM worker_jobs q JOIN worker_accounts a USING(account_id)
 JOIN dispatched d ON d.account_id=q.account_id AND d.handler=q.descriptor->>'handler'
 WHERE a.enabled AND a.auth_status='active' AND q.generation=a.generation AND q.due_at<=now()
 AND (q.owner IS NULL OR q.lease_until<now())
 AND (NOT p_foreground OR q.descriptor->>'handler' NOT IN
   ('fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot'))
 AND NOT(a.excluded_leagues ? (q.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=q.descriptor->'context'->>'championship_id')
 AND (q.checked_at IS NULL OR q.checked_at + make_interval(secs=>CASE WHEN
 (a.quiet_start<a.quiet_end AND extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start AND extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end)
 OR (a.quiet_start>a.quiet_end AND (extract(hour FROM now() AT TIME ZONE a.timezone)>=a.quiet_start OR extract(hour FROM now() AT TIME ZONE a.timezone)<a.quiet_end))
 THEN greatest(q.interval_seconds,3600) ELSE 0 END)<=now())
 -- The short lane prefers core observations, then helps other short reads. The other lane gives each due
 -- query family a turn, so a recurring minute job or 527 player jobs cannot
 -- indefinitely exclude catalogs, history or accounting. One-lane installs
 -- retain the same family fairness across core and background work.
 ORDER BY CASE WHEN q.league_refresh_at IS NOT NULL AND q.descriptor->>'handler'<>'get_financial_snapshot' THEN 0
               WHEN q.league_refresh_at IS NOT NULL THEN 1 ELSE 2 END,
 CASE WHEN p_foreground AND (public.worker_request_priority(q.descriptor->>'handler')>=9 OR q.descriptor->>'handler'='get_clause_transfer_summary') THEN 0 ELSE 1 END,
 d.served_at NULLS FIRST,a.last_dispatched NULLS FIRST,
 CASE WHEN p_foreground AND q.interval_seconds<=60 THEN 0 ELSE 1 END,q.due_at,q.account_id,q.resource_key
 LOOP
 IF NOT public.worker_finance_context_matches(candidate.descriptor,(SELECT verified_leagues->(candidate.descriptor->'context'->>'championship_id') FROM worker_accounts WHERE account_id=candidate.account_id))
 OR public.worker_has_resolved_peer(candidate.account_id,candidate.descriptor) THEN CONTINUE; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=candidate.account_id AND resource_key=candidate.resource_key
 AND generation=candidate.generation AND descriptor=candidate.descriptor AND due_at<=now() AND (owner IS NULL OR lease_until<now()) FOR UPDATE SKIP LOCKED;
 IF j.account_id IS NULL THEN CONTINUE; END IF;
 UPDATE worker_accounts SET last_dispatched=now() WHERE account_id=j.account_id;
 UPDATE worker_jobs SET owner=p_owner,started_at=now(),lease_until=now()+interval '60 seconds',attempts=attempts+1
 WHERE account_id=j.account_id AND resource_key=j.resource_key RETURNING * INTO j;
 RETURN to_jsonb(j);
 END LOOP;
 RETURN NULL;
END $$;

REVOKE ALL ON FUNCTION public.worker_league_refresh(text,text,text,text[],boolean,timestamptz) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_league_refresh(text,text,text,text[],boolean,timestamptz) TO service_role;
NOTIFY pgrst,'reload schema';

COMMIT;
