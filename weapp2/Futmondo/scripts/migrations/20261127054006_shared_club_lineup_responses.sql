-- Public provider observations shared across accounts. No credentials or fantasy roster fields.
BEGIN;
CREATE TABLE public.striker_public_responses (
 league text NOT NULL, endpoint text NOT NULL, lookup text NOT NULL,
 reservation uuid UNIQUE, lease_until timestamptz, observed_at timestamptz,
 expires_at timestamptz NOT NULL DEFAULT now(), completeness text CHECK(completeness IN ('complete','pending')),
 payload jsonb CHECK(jsonb_typeof(payload)='object' AND octet_length(payload::text)<=1048576),
 PRIMARY KEY(league,endpoint,lookup)
);
CREATE INDEX striker_public_responses_expiry ON public.striker_public_responses(expires_at);
ALTER TABLE public.striker_public_responses ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.striker_public_responses FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE,DELETE ON public.striker_public_responses TO service_role;

CREATE FUNCTION public.worker_claim_striker_response(p_league text,p_endpoint text,p_lookup text,p_fetch boolean)
RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE saved striker_public_responses; permit uuid;
BEGIN
 IF coalesce(p_league,'') NOT IN ('soccer','laliga','seriea') OR NOT coalesce(CASE p_endpoint
   WHEN 'list_games' THEN p_lookup ~ '^[0-9]{4}-[0-9]{2}-[0-9]{2}$'
   WHEN 'get_lineups' THEN p_lookup ~ '^[1-9][0-9]{0,17}$'
   WHEN 'get_squad' THEN p_lookup ~ '^[1-9][0-9]{0,17}:[0-9]{4}$' ELSE false END,false)
 THEN RAISE EXCEPTION 'Invalid public lineup lookup'; END IF;
 SELECT * INTO saved FROM striker_public_responses WHERE league=p_league AND endpoint=p_endpoint AND lookup=p_lookup;
 IF saved.expires_at>now() AND saved.payload IS NOT NULL THEN
   RETURN jsonb_build_object('status','hit','data',saved.payload,'observed',extract(epoch from saved.observed_at),
     'expires',extract(epoch from saved.expires_at)); END IF;
 IF NOT coalesce(p_fetch,false) THEN RETURN '{"status":"unavailable"}'::jsonb; END IF;
 IF saved.lease_until>now() THEN RETURN '{"status":"busy"}'::jsonb; END IF;
 DELETE FROM striker_public_responses WHERE expires_at<now()-interval '7 days' AND (lease_until IS NULL OR lease_until<=now());
 INSERT INTO striker_public_responses(league,endpoint,lookup) VALUES(p_league,p_endpoint,p_lookup) ON CONFLICT DO NOTHING;
 SELECT * INTO STRICT saved FROM striker_public_responses WHERE league=p_league AND endpoint=p_endpoint AND lookup=p_lookup FOR UPDATE;
 IF saved.expires_at>now() AND saved.payload IS NOT NULL THEN
   RETURN jsonb_build_object('status','hit','data',saved.payload,'observed',extract(epoch from saved.observed_at),
     'expires',extract(epoch from saved.expires_at)); END IF;
 IF saved.lease_until>now() THEN RETURN '{"status":"busy"}'::jsonb; END IF;
 permit:=worker_reserve_striker_call(p_endpoint);
 IF permit IS NULL THEN RETURN '{"status":"monthly_limit"}'::jsonb; END IF;
 UPDATE striker_public_responses SET reservation=permit,lease_until=now()+interval '25 seconds',expires_at=now()
 WHERE league=p_league AND endpoint=p_endpoint AND lookup=p_lookup;
 RETURN jsonb_build_object('status','fetch','reservation',permit);
END $$;

CREATE FUNCTION public.worker_save_striker_response(p_reservation uuid,p_observed double precision,p_partial boolean,p_data jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE saved striker_public_responses; lifetime interval;
BEGIN
 SELECT * INTO saved FROM striker_public_responses WHERE reservation=p_reservation FOR UPDATE;
 IF NOT FOUND THEN RETURN false; END IF;
 IF saved.lease_until IS NULL THEN RETURN true; END IF;
 IF p_data IS NULL OR p_observed IS NULL OR p_partial IS NULL THEN RAISE EXCEPTION 'Missing public result'; END IF;
 lifetime:=CASE WHEN p_partial THEN interval '2 minutes' WHEN saved.endpoint='list_games' THEN interval '24 hours'
   WHEN saved.endpoint='get_squad' THEN interval '7 days' ELSE interval '3 hours' END;
 UPDATE striker_public_responses SET payload=p_data,observed_at=to_timestamp(p_observed),
   expires_at=CASE WHEN saved.endpoint='list_games' AND NOT p_partial THEN
     greatest(to_timestamp(p_observed)+lifetime,((saved.lookup::date+1)::timestamp AT TIME ZONE 'UTC'))
     ELSE to_timestamp(p_observed)+lifetime END,completeness=CASE WHEN p_partial THEN 'pending' ELSE 'complete' END,lease_until=NULL
 WHERE reservation=p_reservation;
 RETURN true;
END $$;
REVOKE ALL ON FUNCTION public.worker_claim_striker_response(text,text,text,boolean),public.worker_save_striker_response(uuid,double precision,boolean,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_claim_striker_response(text,text,text,boolean),public.worker_save_striker_response(uuid,double precision,boolean,jsonb) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
