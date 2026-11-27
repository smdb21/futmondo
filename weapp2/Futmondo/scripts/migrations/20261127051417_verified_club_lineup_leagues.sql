-- Existing fixture permits predate multi-league support and all refer to La Liga.
BEGIN;
ALTER TABLE public.worker_club_lineup_requests ADD COLUMN competition_id text NOT NULL DEFAULT '504e4f584d8bec9a67000079';
ALTER TABLE public.worker_club_lineup_limits ADD COLUMN competition_id text NOT NULL DEFAULT '504e4f584d8bec9a67000079';
ALTER TABLE public.worker_club_lineup_limits DROP CONSTRAINT worker_club_lineup_limits_pkey,
 ADD PRIMARY KEY(account_id,kickoff,match_id,competition_id);
ALTER TABLE public.worker_club_lineup_requests ALTER COLUMN competition_id DROP DEFAULT;
ALTER TABLE public.worker_club_lineup_limits ALTER COLUMN competition_id DROP DEFAULT;
CREATE OR REPLACE FUNCTION public.worker_request_club_lineup(p_account text,p_descriptor jsonb,p_match text,p_round text,p_kickoff text)
RETURNS text LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE ctx jsonb:=p_descriptor->'context'; k text:=p_descriptor->>'resource_key'; a worker_accounts; budget worker_club_lineup_limits;
BEGIN
 IF p_descriptor->>'handler' IS DISTINCT FROM 'get_club_lineups' OR ctx->>'account_id' IS DISTINCT FROM p_account
 OR coalesce(ctx->>'competition_id','') NOT IN ('504e4f584d8bec9a67000079','51c96d15e67621cf7000000d','51ad1f8f52c92e260f00000a')
 OR p_descriptor->'args' IS DISTINCT FROM jsonb_build_object('championship_id',ctx->>'championship_id','manual_only',true)
 OR coalesce(k,'') !~ '^[a-f0-9]{64}$' OR coalesce(p_match,'') !~ '^[[:alnum:]_-]{1,128}$'
 OR coalesce(p_round,'') !~ '^[[:alnum:]_-]{1,128}$' THEN RETURN 'unavailable'; END IF;
 IF p_kickoff IS NULL OR p_kickoff::timestamptz<=now() OR p_kickoff::timestamptz>now()+interval '90 minutes'
 THEN RETURN 'outside_window'; END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT FOUND OR NOT a.enabled OR a.auth_status<>'active' OR NOT (a.verified_leagues ? (ctx->>'championship_id'))
 OR a.excluded_leagues ? (ctx->>'championship_id') OR EXISTS(SELECT 1 FROM blocked_leagues WHERE championship_id=ctx->>'championship_id')
 OR a.verified_leagues->(ctx->>'championship_id')->>'competition_id' IS DISTINCT FROM ctx->>'competition_id'
 OR NOT worker_finance_context_matches(p_descriptor,a.verified_leagues->(ctx->>'championship_id')) THEN RETURN 'unavailable'; END IF;
 -- Use the same job-then-request lock order as claim, publish and release.
 PERFORM 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=k FOR UPDATE;
 DELETE FROM worker_club_lineup_limits WHERE account_id=p_account AND kickoff<now()-interval '7 days';
 INSERT INTO worker_club_lineup_limits(account_id,kickoff,match_id,competition_id)
 VALUES(p_account,p_kickoff::timestamptz,p_match,ctx->>'competition_id') ON CONFLICT DO NOTHING;
 SELECT * INTO STRICT budget FROM worker_club_lineup_limits
 WHERE account_id=p_account AND kickoff=p_kickoff::timestamptz AND competition_id=ctx->>'competition_id' AND match_id=p_match FOR UPDATE;
 IF budget.checks>=2 THEN RETURN 'limit_reached'; END IF;
 IF budget.reserved_until>now() THEN RETURN 'busy'; END IF;
 IF budget.checks=1 AND now()<greatest(budget.last_check+interval '15 minutes',budget.kickoff-interval '30 minutes')
 THEN RETURN 'retry_at:'||ceil(extract(epoch from greatest(budget.last_check+interval '15 minutes',budget.kickoff-interval '30 minutes')))::bigint; END IF;
 INSERT INTO worker_club_lineup_requests AS r(account_id,resource_key,generation,match_id,round_id,kickoff,competition_id)
 VALUES(p_account,k,a.generation,p_match,p_round,p_kickoff,ctx->>'competition_id')
 ON CONFLICT(account_id,resource_key) DO UPDATE SET generation=excluded.generation,match_id=excluded.match_id,
 competition_id=excluded.competition_id,round_id=excluded.round_id,kickoff=excluded.kickoff,requested_at=now(),consumed_at=NULL
 WHERE r.generation<>excluded.generation OR r.requested_at<now()-interval '3 minutes'
 OR (r.consumed_at IS NOT NULL AND r.match_id<>excluded.match_id);
 IF NOT FOUND THEN RETURN 'busy'; END IF;
 UPDATE worker_club_lineup_limits SET reserved_until=now()+interval '3 minutes'
 WHERE account_id=p_account AND kickoff=p_kickoff::timestamptz AND competition_id=ctx->>'competition_id' AND match_id=p_match;
 IF NOT worker_enqueue(p_account,p_descriptor,86400,10) THEN RAISE EXCEPTION 'Could not queue club check'; END IF;
 RETURN 'requested';
END $$;

CREATE OR REPLACE FUNCTION public.worker_take_club_lineup(p_account text,p_key text,p_owner text,p_generation bigint)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE req jsonb; attempts integer; retry_at timestamptz;
BEGIN
 IF NOT worker_guard(p_account,p_key,p_owner,p_generation) THEN RETURN NULL; END IF;
 UPDATE worker_club_lineup_requests SET consumed_at=now() WHERE account_id=p_account AND resource_key=p_key
 AND generation=p_generation AND consumed_at IS NULL AND requested_at>=now()-interval '3 minutes' AND kickoff::timestamptz>now()
 RETURNING jsonb_build_object('competition_id',competition_id,'match_id',match_id,'round_id',round_id,'kickoff',kickoff) INTO req;
 IF req IS NULL THEN RETURN NULL; END IF;
 -- Count a single consumed permit, even if the worker later times out. Expired queued taps cost no attempt.
 UPDATE worker_club_lineup_limits SET checks=checks+1,last_check=now(),reserved_until=NULL
 WHERE account_id=p_account AND kickoff=(req->>'kickoff')::timestamptz AND match_id=req->>'match_id' AND competition_id=req->>'competition_id'
 AND checks<2 AND (checks=0 OR now()>=greatest(last_check+interval '15 minutes',kickoff-interval '30 minutes'))
 RETURNING checks,greatest(last_check+interval '15 minutes',kickoff-interval '30 minutes') INTO attempts,retry_at;
 IF NOT FOUND THEN RETURN NULL; END IF;
 RETURN req || jsonb_build_object('checks',attempts,'retry_at',extract(epoch from retry_at));
END $$;

NOTIFY pgrst,'reload schema';
COMMIT;
