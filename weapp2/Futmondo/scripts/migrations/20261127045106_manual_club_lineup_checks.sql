-- An explicit, expiring request admits one external team-sheet lookup.
BEGIN;
CREATE TABLE public.worker_club_lineup_requests (
 account_id text NOT NULL REFERENCES public.worker_accounts(account_id) ON DELETE CASCADE,
 resource_key text NOT NULL,
 generation bigint NOT NULL,
 match_id text NOT NULL,
 round_id text NOT NULL,
 kickoff text NOT NULL,
 requested_at timestamptz NOT NULL DEFAULT now(),
 consumed_at timestamptz,
 PRIMARY KEY(account_id,resource_key)
);
ALTER TABLE public.worker_club_lineup_requests ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.worker_club_lineup_requests FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE ON public.worker_club_lineup_requests TO service_role;

CREATE FUNCTION public.worker_request_club_lineup(p_account text,p_descriptor jsonb,p_match text,p_round text,p_kickoff text)
RETURNS text LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE ctx jsonb:=p_descriptor->'context'; k text:=p_descriptor->>'resource_key'; a worker_accounts;
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
 INSERT INTO worker_club_lineup_requests AS r(account_id,resource_key,generation,match_id,round_id,kickoff)
 VALUES(p_account,k,a.generation,p_match,p_round,p_kickoff)
 ON CONFLICT(account_id,resource_key) DO UPDATE SET generation=excluded.generation,match_id=excluded.match_id,
 round_id=excluded.round_id,kickoff=excluded.kickoff,requested_at=now(),consumed_at=NULL
 WHERE r.generation<>excluded.generation OR r.requested_at<now()-interval '3 minutes'
 OR (r.consumed_at IS NOT NULL AND r.match_id<>excluded.match_id);
 IF NOT FOUND THEN RETURN 'busy'; END IF;
 IF NOT worker_enqueue(p_account,p_descriptor,86400,10) THEN RAISE EXCEPTION 'Could not queue club check'; END IF;
 RETURN 'requested';
END $$;

CREATE FUNCTION public.worker_take_club_lineup(p_account text,p_key text,p_owner text,p_generation bigint)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE req jsonb;
BEGIN
 IF NOT worker_guard(p_account,p_key,p_owner,p_generation) THEN RETURN NULL; END IF;
 UPDATE worker_club_lineup_requests SET consumed_at=now() WHERE account_id=p_account AND resource_key=p_key
 AND generation=p_generation AND consumed_at IS NULL AND requested_at>=now()-interval '3 minutes' AND kickoff::timestamptz>now()
 RETURNING jsonb_build_object('match_id',match_id,'round_id',round_id,'kickoff',kickoff) INTO req;
 RETURN req;
END $$;

CREATE FUNCTION public.worker_park_club_lineup() RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 IF NOT EXISTS(SELECT 1 FROM worker_club_lineup_requests r WHERE r.account_id=NEW.account_id AND r.resource_key=NEW.resource_key
    AND r.generation=NEW.generation AND r.consumed_at IS NULL AND r.requested_at>=now()-interval '3 minutes'
    AND r.kickoff::timestamptz>now()) THEN NEW.due_at:='infinity'; END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER park_club_lineup BEFORE INSERT OR UPDATE OF due_at ON public.worker_jobs FOR EACH ROW
 WHEN (NEW.descriptor->>'handler'='get_club_lineups') EXECUTE FUNCTION public.worker_park_club_lineup();
UPDATE public.worker_jobs SET due_at='infinity' WHERE descriptor->>'handler'='get_club_lineups' AND due_at<>'infinity';
REVOKE ALL ON FUNCTION public.worker_request_club_lineup(text,jsonb,text,text,text),
 public.worker_take_club_lineup(text,text,text,bigint),public.worker_park_club_lineup() FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_request_club_lineup(text,jsonb,text,text,text),
 public.worker_take_club_lineup(text,text,text,bigint),public.worker_park_club_lineup() TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
