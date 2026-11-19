CREATE OR REPLACE FUNCTION public.worker_maintenance(p_owner text,p_base_interval integer,p_requests bigint,p_jobs bigint) RETURNS jsonb LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE elected boolean; admitted boolean; old_base integer;
BEGIN
 IF p_base_interval<60 THEN RAISE EXCEPTION 'Invalid worker base interval'; END IF;
 INSERT INTO worker_leader(id,owner,lease_until) VALUES(1,p_owner,now()+interval '180 seconds')
 ON CONFLICT(id) DO UPDATE SET owner=excluded.owner,lease_until=excluded.lease_until
 WHERE worker_leader.owner=p_owner OR worker_leader.lease_until<now();
 elected:=FOUND;
 IF elected THEN
 SELECT base_interval INTO old_base FROM worker_schedule_policy WHERE id=1 FOR UPDATE;
 IF old_base<>p_base_interval THEN
 UPDATE worker_schedule_policy SET base_interval=p_base_interval WHERE id=1;
 UPDATE worker_jobs SET interval_seconds=greatest(native_interval_seconds,p_base_interval),
 due_at=CASE WHEN owner IS NULL AND last_error IS NULL THEN now()+make_interval(secs=>1+mod(abs(hashtext(resource_key)::bigint),greatest(native_interval_seconds,p_base_interval))) ELSE due_at END
 WHERE interval_seconds IS DISTINCT FROM greatest(native_interval_seconds,p_base_interval);
 END IF;
 END IF;
 SELECT coalesce((SELECT NOT admin_only FROM application_access WHERE id='main'),false) INTO admitted;
 INSERT INTO worker_health(id,status,heartbeat,requests,jobs) VALUES(p_owner,CASE WHEN elected THEN 'running' ELSE 'standby' END,now(),p_requests,p_jobs)
 ON CONFLICT(id) DO UPDATE SET status=excluded.status,heartbeat=excluded.heartbeat,requests=excluded.requests,jobs=excluded.jobs;
 RETURN jsonb_build_object('leader',elected,'admitted',admitted);
END $$;