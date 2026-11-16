-- Automatic synchronization; display reads never own acquisition.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_bootstrap(p_account text)
RETURNS jsonb LANGUAGE sql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT public.worker_read(p_account,jsonb_build_object(d->>'resource_key',''),jsonb_build_object(d->>'resource_key',d))
 || jsonb_build_object('preferred_league',(SELECT jsonb_build_object('championship_id',championship_id,'selected_at',selected_at)
 FROM public.user_league_preferences WHERE user_id=p_account))
 FROM (SELECT public.worker_account_descriptor(p_account) d) descriptor;
$$;
CREATE OR REPLACE FUNCTION public.worker_connect(p_account text,p_token text,p_credential text,p_generation bigint,p_timezone text DEFAULT 'UTC')
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;
BEGIN
 IF p_account IS NULL OR length(p_account) NOT BETWEEN 1 AND 256 OR p_token IS NULL OR length(p_token) NOT BETWEEN 1 AND 65536
 OR p_credential IS NULL OR length(p_credential) NOT BETWEEN 1 AND 128 OR p_generation IS NULL OR p_generation<0 THEN RAISE EXCEPTION 'Invalid connection'; END IF;
 IF NOT EXISTS(SELECT 1 FROM pg_timezone_names WHERE name=p_timezone) THEN RAISE EXCEPTION 'Invalid timezone'; END IF;
 IF p_generation=0 THEN
  INSERT INTO worker_accounts(account_id,enabled,encrypted_token,credential_id,timezone,quiet_start,quiet_end)
  VALUES(p_account,true,p_token,p_credential,p_timezone,0,0) ON CONFLICT DO NOTHING;
 END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR UPDATE;
 IF a.account_id IS NULL OR (a.generation<>p_generation AND NOT(p_generation=0 AND a.generation=1 AND a.enabled)) THEN RETURN NULL; END IF;
 -- Token rotation is not authorization revocation. Preserve every running job.
 UPDATE worker_accounts SET enabled=true,encrypted_token=p_token,credential_id=p_credential,
  auth_status='active',quiet_start=0,quiet_end=0,updated_at=now() WHERE account_id=p_account RETURNING * INTO a;
 UPDATE worker_jobs SET due_at=least(due_at,now()),last_error=NULL,attempts=0
 WHERE account_id=p_account AND generation=a.generation AND owner IS NULL AND last_error='source_authentication';
 RETURN to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues';
END $$;
CREATE OR REPLACE FUNCTION public.worker_preferences(p_account text,p_generation bigint,p_excluded jsonb)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;
BEGIN
 IF jsonb_typeof(p_excluded) IS DISTINCT FROM 'array' OR octet_length(p_excluded::text)>65536 THEN RAISE EXCEPTION 'Invalid leagues'; END IF;
 IF EXISTS(SELECT 1 FROM jsonb_array_elements(p_excluded) v WHERE jsonb_typeof(v)<>'string') THEN RAISE EXCEPTION 'Invalid league'; END IF;
 UPDATE worker_accounts SET excluded_leagues=p_excluded,quiet_start=0,quiet_end=0,updated_at=now()
 WHERE account_id=p_account AND generation=p_generation RETURNING * INTO a;
 IF a.account_id IS NULL THEN RETURN NULL; END IF;
 DELETE FROM worker_jobs WHERE account_id=p_account AND p_excluded ? (descriptor->'context'->>'championship_id');
 RETURN to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues';
END $$;
CREATE OR REPLACE FUNCTION public.worker_disconnect(p_account text,p_generation bigint)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;
BEGIN
 UPDATE worker_accounts SET enabled=false,encrypted_token=NULL,credential_id=NULL,generation=generation+1,updated_at=now()
 WHERE account_id=p_account AND generation=p_generation RETURNING * INTO a;
 IF a.account_id IS NULL THEN RETURN NULL; END IF;
 DELETE FROM worker_jobs WHERE account_id=p_account;
 RETURN to_jsonb(a)-'encrypted_token'-'last_dispatched'-'verified_leagues';
END $$;
-- Old images can renew safely during rollout. A delayed renewal cannot re-enable revocation.
CREATE OR REPLACE FUNCTION public.worker_renew(p_account text,p_token text,p_credential text DEFAULT NULL)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account AND enabled FOR UPDATE;
 IF a.account_id IS NULL THEN RETURN NULL; END IF;
 RETURN public.worker_connect(p_account,p_token,p_credential,a.generation,a.timezone);
END $$;
CREATE OR REPLACE FUNCTION public.worker_authorize(p_account text,p_enabled boolean,p_token text DEFAULT NULL,
 p_timezone text DEFAULT 'UTC',p_start integer DEFAULT 0,p_end integer DEFAULT 7,p_excluded jsonb DEFAULT '[]',p_credential text DEFAULT NULL)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;result jsonb;
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR UPDATE;
 IF NOT p_enabled THEN RETURN public.worker_disconnect(p_account,a.generation); END IF;
 result:=public.worker_connect(p_account,p_token,p_credential,coalesce(a.generation,0),p_timezone);
 IF result IS NULL THEN RETURN NULL; END IF;
 RETURN public.worker_preferences(p_account,(result->>'generation')::bigint,p_excluded);
END $$;
CREATE OR REPLACE FUNCTION public.worker_prepare_source(p_account text,p_key text,p_owner text,p_generation bigint,p_request text)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;j worker_jobs;
BEGIN
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT public.worker_guard(p_account,p_key,p_owner,p_generation) THEN RETURN NULL; END IF;
 SELECT * INTO j FROM worker_jobs WHERE account_id=p_account AND resource_key=p_key;
 IF a.excluded_leagues ? (j.descriptor->'context'->>'championship_id') THEN RETURN NULL; END IF;
 RETURN jsonb_build_object('account',to_jsonb(a),'checkpoint',(SELECT response FROM worker_checkpoints
 WHERE account_id=p_account AND resource_key=p_key AND run_id=j.run_id AND request_key=p_request));
END $$;
-- Already-guarded writes do not need a separate HTTP guard before this operation.
CREATE OR REPLACE FUNCTION public.worker_guard(p_account text,p_key text,p_owner text,p_generation bigint,p_checkpoint jsonb DEFAULT NULL)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 UPDATE worker_jobs j SET lease_until=now()+interval '60 seconds',checkpoint=coalesce(p_checkpoint,j.checkpoint)
 FROM worker_accounts a WHERE j.account_id=p_account AND j.resource_key=p_key AND j.owner=p_owner
 AND j.lease_until>now() AND a.account_id=j.account_id AND a.enabled AND a.auth_status='active'
 AND a.generation=p_generation AND j.generation=p_generation
 AND NOT(a.excluded_leagues ? (j.descriptor->'context'->>'championship_id'))
 AND public.worker_finance_context_matches(j.descriptor,a.verified_leagues->(j.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=j.descriptor->'context'->>'championship_id')
 AND EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now());
 RETURN FOUND;
END $$;
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
 ORDER BY CASE WHEN p_foreground AND (public.worker_request_priority(q.descriptor->>'handler')>=9 OR q.descriptor->>'handler'='get_clause_transfer_summary') THEN 0 ELSE 1 END,
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
ALTER TABLE public.worker_accounts ALTER COLUMN quiet_end SET DEFAULT 0;
UPDATE public.worker_accounts SET quiet_start=0,quiet_end=0 WHERE enabled AND (quiet_start<>0 OR quiet_end<>0);
REVOKE ALL ON FUNCTION public.worker_bootstrap(text),public.worker_connect(text,text,text,bigint,text),
 public.worker_preferences(text,bigint,jsonb),public.worker_disconnect(text,bigint),
 public.worker_prepare_source(text,text,text,bigint,text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_bootstrap(text),public.worker_connect(text,text,text,bigint,text),
 public.worker_preferences(text,bigint,jsonb),public.worker_disconnect(text,bigint),
 public.worker_prepare_source(text,text,text,bigint,text) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
