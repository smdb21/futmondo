-- Reference-safe bounded cleanup, independent of source acquisition and heartbeat.
BEGIN;
CREATE TABLE public.worker_cleanup_status (
 id integer PRIMARY KEY CHECK(id=1), last_success_at timestamptz NOT NULL,
 chunks_deleted bigint NOT NULL DEFAULT 0, checkpoints_deleted bigint NOT NULL DEFAULT 0,
 last_chunks integer NOT NULL DEFAULT 0, last_checkpoints integer NOT NULL DEFAULT 0
);
ALTER TABLE public.worker_cleanup_status ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.worker_cleanup_status FROM PUBLIC,anon,authenticated;
GRANT SELECT,INSERT,UPDATE ON public.worker_cleanup_status TO service_role;
CREATE FUNCTION public.worker_cleanup_batch(p_limit integer DEFAULT 500) RETURNS jsonb
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE chunks integer; checkpoints integer;
BEGIN
 IF p_limit<1 OR p_limit>1000 THEN RAISE EXCEPTION 'Invalid cleanup batch size'; END IF;
 -- Current snapshots always win. The grace window protects uploads not yet published.
 WITH candidates AS (
 SELECT c.ctid FROM worker_chunks c WHERE c.created_at<now()-interval '1 hour'
 AND NOT EXISTS(SELECT 1 FROM worker_snapshots s WHERE s.account_id=c.account_id
 AND s.resource_key=c.resource_key AND s.version=c.version)
 ORDER BY c.created_at LIMIT p_limit FOR UPDATE OF c SKIP LOCKED
 ) DELETE FROM worker_chunks c USING candidates d WHERE c.ctid=d.ctid;
 GET DIAGNOSTICS chunks=ROW_COUNT;
 -- Keep every checkpoint belonging to the current run, even for long retries.
 WITH candidates AS (
 SELECT c.ctid FROM worker_checkpoints c WHERE c.created_at<now()-interval '1 hour'
 AND NOT EXISTS(SELECT 1 FROM worker_jobs j WHERE j.account_id=c.account_id
 AND j.resource_key=c.resource_key AND j.run_id=c.run_id)
 LIMIT p_limit FOR UPDATE OF c SKIP LOCKED
 ) DELETE FROM worker_checkpoints c USING candidates d WHERE c.ctid=d.ctid;
 GET DIAGNOSTICS checkpoints=ROW_COUNT;
 DELETE FROM worker_health WHERE heartbeat<now()-interval '1 day';
 INSERT INTO worker_cleanup_status(id,last_success_at,chunks_deleted,checkpoints_deleted,last_chunks,last_checkpoints)
 VALUES(1,now(),chunks,checkpoints,chunks,checkpoints)
 ON CONFLICT(id) DO UPDATE SET last_success_at=excluded.last_success_at,
 chunks_deleted=worker_cleanup_status.chunks_deleted+excluded.chunks_deleted,
 checkpoints_deleted=worker_cleanup_status.checkpoints_deleted+excluded.checkpoints_deleted,
 last_chunks=excluded.last_chunks,last_checkpoints=excluded.last_checkpoints;
 RETURN jsonb_build_object('chunks',chunks,'checkpoints',checkpoints);
END $$;
CREATE OR REPLACE FUNCTION public.worker_cleanup() RETURNS void
LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 PERFORM worker_cleanup_batch(500);
END $$;
REVOKE ALL ON FUNCTION public.worker_cleanup_batch(integer) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_cleanup_batch(integer) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
