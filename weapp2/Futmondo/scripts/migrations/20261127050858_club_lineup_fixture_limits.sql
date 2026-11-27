-- Budget one initial check and one late retry for a user's real fixture across league views.
BEGIN;
CREATE TABLE public.worker_club_lineup_limits (
 account_id text NOT NULL REFERENCES public.worker_accounts(account_id) ON DELETE CASCADE,
 kickoff timestamptz NOT NULL,
 match_id text NOT NULL,
 checks smallint NOT NULL DEFAULT 0 CHECK(checks BETWEEN 0 AND 2),
 last_check timestamptz,
 reserved_until timestamptz,
 PRIMARY KEY(account_id,kickoff,match_id)
);
ALTER TABLE public.worker_club_lineup_limits ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.worker_club_lineup_limits FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.worker_club_lineup_limits TO service_role;
-- Carry forward known checks and unexpired requests during rollout.
LOCK TABLE public.worker_club_lineup_requests IN SHARE ROW EXCLUSIVE MODE;
INSERT INTO public.worker_club_lineup_limits(account_id,kickoff,match_id,checks,last_check,reserved_until)
 SELECT account_id,kickoff::timestamptz,match_id,least(2,count(consumed_at)),max(consumed_at),
 max(requested_at+interval '3 minutes') FILTER(WHERE consumed_at IS NULL)
 FROM public.worker_club_lineup_requests WHERE kickoff::timestamptz>now()
 GROUP BY account_id,kickoff::timestamptz,match_id;
CREATE OR REPLACE FUNCTION public.worker_request_club_lineup(p_account text,p_descriptor jsonb,p_match text,p_round text,p_kickoff text)
RETURNS text LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE ctx jsonb:=p_descriptor->'context'; k text:=p_descriptor->>'resource_key'; a worker_accounts; budget worker_club_lineup_limits;
BEGIN
 IF p_descriptor->>'handler' IS DISTINCT FROM 'get_club_lineups' OR ctx->>'account_id' IS DISTINCT FROM p_account
 OR ctx->>'competition_id' IS DISTINCT FROM '504e4f584d8bec9a67000079'
 OR p_descriptor->'args' IS DISTINCT FROM jsonb_build_object('championship_id',ctx->>'championship_id','manual_only',true)
 OR coalesce(k,'') !~ '^[a-f0-9]{64}$' OR coalesce(p_match,'') !~ '^[[:alnum:]_-]{1,128}$'
 OR coalesce(p_round,'') !~ '^[[:alnum:]_-]{1,128}$' THEN RETURN 'unavailable'; END IF;
 IF p_kickoff IS NULL OR p_kickoff::timestamptz<=now() OR p_kickoff::timestamptz>now()+interval '90 minutes'
 THEN RETURN 'outside_window'; END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF NOT FOUND OR NOT a.enabled OR a.auth_status<>'active' OR NOT (a.verified_leagues ? (ctx->>'championship_id'))
 OR a.excluded_leagues ? (ctx->>'championship_id') OR EXISTS(SELECT 1 FROM blocked_leagues WHERE championship_id=ctx->>'championship_id')
 OR NOT worker_finance_context_matches(p_descriptor,a.verified_leagues->(ctx->>'championship_id')) THEN RETURN 'unavailable'; END IF;
 -- Use the same job-then-request lock order as claim, publish and release.
 PERFORM 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=k FOR UPDATE;
 DELETE FROM worker_club_lineup_limits WHERE account_id=p_account AND kickoff<now()-interval '7 days';
 INSERT INTO worker_club_lineup_limits(account_id,kickoff,match_id)
 VALUES(p_account,p_kickoff::timestamptz,p_match) ON CONFLICT DO NOTHING;
 SELECT * INTO STRICT budget FROM worker_club_lineup_limits
 WHERE account_id=p_account AND kickoff=p_kickoff::timestamptz AND match_id=p_match FOR UPDATE;
 IF budget.checks>=2 THEN RETURN 'limit_reached'; END IF;
 IF budget.reserved_until>now() THEN RETURN 'busy'; END IF;
 IF budget.checks=1 AND now()<greatest(budget.last_check+interval '15 minutes',budget.kickoff-interval '30 minutes')
 THEN RETURN 'retry_at:'||ceil(extract(epoch from greatest(budget.last_check+interval '15 minutes',budget.kickoff-interval '30 minutes')))::bigint; END IF;
 INSERT INTO worker_club_lineup_requests AS r(account_id,resource_key,generation,match_id,round_id,kickoff)
 VALUES(p_account,k,a.generation,p_match,p_round,p_kickoff)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET generation=excluded.generation,match_id=excluded.match_id,
 round_id=excluded.round_id,kickoff=excluded.kickoff,requested_at=now(),consumed_at=NULL
 WHERE r.generation<>excluded.generation OR r.requested_at<now()-interval '3 minutes'
 OR (r.consumed_at IS NOT NULL AND r.match_id<>excluded.match_id);
 IF NOT FOUND THEN RETURN 'busy'; END IF;
 UPDATE worker_club_lineup_limits SET reserved_until=now()+interval '3 minutes'
 WHERE account_id=p_account AND kickoff=p_kickoff::timestamptz AND match_id=p_match;
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
 RETURNING jsonb_build_object('match_id',match_id,'round_id',round_id,'kickoff',kickoff) INTO req;
 IF req IS NULL THEN RETURN NULL; END IF;
 -- Count a single consumed permit, even if the worker later times out. Expired queued taps cost no attempt.
 UPDATE worker_club_lineup_limits SET checks=checks+1,last_check=now(),reserved_until=NULL
 WHERE account_id=p_account AND kickoff=(req->>'kickoff')::timestamptz AND match_id=req->>'match_id'
 AND checks<2 AND (checks=0 OR now()>=greatest(last_check+interval '15 minutes',kickoff-interval '30 minutes'))
 RETURNING checks,greatest(last_check+interval '15 minutes',kickoff-interval '30 minutes') INTO attempts,retry_at;
 IF NOT FOUND THEN RETURN NULL; END IF;
 RETURN req || jsonb_build_object('checks',attempts,'retry_at',extract(epoch from retry_at));
END $$;

-- Shared by every worker lane and fantasy account using this provider subscription.
CREATE TABLE public.striker_credit_reservations (
 id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
 reserved_at timestamptz NOT NULL DEFAULT now(),
 endpoint text NOT NULL,
 credits integer NOT NULL CHECK(credits>=0)
);
ALTER TABLE public.striker_credit_reservations ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.striker_credit_reservations FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.striker_credit_reservations TO service_role;

CREATE FUNCTION public.worker_reserve_striker_call(p_endpoint text)
RETURNS uuid LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE cost integer; reservation uuid;
BEGIN
 cost:=CASE p_endpoint WHEN 'get_lineups' THEN 5 WHEN 'get_squad' THEN 2 WHEN 'list_games' THEN 2 END;
 IF cost IS NULL THEN RAISE EXCEPTION 'Unknown lineup provider read'; END IF;
 -- A short write lock makes the total and insert atomic across all accounts, even on an empty ledger.
 LOCK TABLE striker_credit_reservations IN SHARE ROW EXCLUSIVE MODE;
 DELETE FROM striker_credit_reservations WHERE reserved_at<=now()-interval '744 hours';
 IF (SELECT coalesce(sum(credits),0) FROM striker_credit_reservations)+cost>500 THEN RETURN NULL; END IF;
 INSERT INTO striker_credit_reservations(endpoint,credits) VALUES(p_endpoint,cost) RETURNING id INTO reservation;
 RETURN reservation;
END $$;

CREATE FUNCTION public.worker_settle_striker_call(p_id uuid,p_used integer)
RETURNS void LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 IF p_used IS NULL OR p_used<0 THEN RAISE EXCEPTION 'Invalid provider credit count'; END IF;
 IF p_used=0 THEN DELETE FROM striker_credit_reservations WHERE id=p_id;
 ELSE UPDATE striker_credit_reservations SET credits=p_used WHERE id=p_id; END IF;
END $$;
REVOKE ALL ON FUNCTION public.worker_reserve_striker_call(text),public.worker_settle_striker_call(uuid,integer) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_reserve_striker_call(text),public.worker_settle_striker_call(uuid,integer) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
