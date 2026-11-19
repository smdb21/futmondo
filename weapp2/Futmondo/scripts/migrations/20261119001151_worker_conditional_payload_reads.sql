-- Internal worker reads receive current metadata for unchanged versions, without chunks.
CREATE OR REPLACE FUNCTION public.worker_read_cached(p_account text,p_versions jsonb DEFAULT '{}')
RETURNS jsonb LANGUAGE plpgsql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE result jsonb;
BEGIN
 result:=public.worker_read(p_account,p_versions);
 RETURN result || jsonb_build_object('unchanged',coalesce((
 SELECT jsonb_agg(jsonb_build_object('key',s.resource_key,'manifest',to_jsonb(s)-'descriptor'))
 FROM public.worker_snapshots s JOIN jsonb_each_text(p_versions) v ON v.key=s.resource_key AND v.value=s.version::text
 WHERE s.account_id=p_account),'[]'::jsonb));
END $$;
REVOKE ALL ON FUNCTION public.worker_read_cached(text,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_read_cached(text,jsonb) TO service_role;
-- Keep legacy checkpoint read/write API compatible; new writers receive a tiny acknowledgement.
CREATE OR REPLACE FUNCTION public.worker_checkpoint_ack(p_account text,p_key text,p_owner text,p_generation bigint,p_request text,p_response jsonb)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 IF p_response IS NULL THEN RAISE EXCEPTION 'Missing checkpoint payload'; END IF;
 RETURN public.worker_checkpoint(p_account,p_key,p_owner,p_generation,p_request,p_response) IS NOT NULL;
END $$;
REVOKE ALL ON FUNCTION public.worker_checkpoint_ack(text,text,text,bigint,text,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_checkpoint_ack(text,text,text,bigint,text,jsonb) TO service_role;
NOTIFY pgrst,'reload schema';
