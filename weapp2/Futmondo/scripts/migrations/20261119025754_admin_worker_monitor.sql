-- Sampled operational metadata only; no request payloads or credentials.
BEGIN;
ALTER TABLE public.worker_jobs ADD COLUMN activity jsonb;
ALTER TABLE public.worker_jobs ADD COLUMN activity_at timestamptz;
ALTER TABLE public.worker_jobs ADD CONSTRAINT worker_jobs_activity_size CHECK
 (activity IS NULL OR (jsonb_typeof(activity)='object' AND octet_length(activity::text)<=2048));
CREATE INDEX worker_jobs_active_monitor ON public.worker_jobs(lease_until,owner) WHERE owner IS NOT NULL;

-- Keep the route list aligned with Python's explicit diagnostic allowlists.
CREATE FUNCTION public.worker_activity_valid(p_activity jsonb) RETURNS boolean
LANGUAGE plpgsql IMMUTABLE SECURITY INVOKER SET search_path='' AS $$
BEGIN
 IF NOT coalesce(jsonb_typeof(p_activity)='object' AND octet_length(p_activity::text)<=1536
 AND p_activity ?& ARRAY['phase','route','method','started_at','sequence','run_id']
 AND (p_activity-ARRAY['phase','route','method','started_at','sequence','run_id'])='{}'::jsonb
 AND jsonb_typeof(p_activity->'phase')='string'
 AND p_activity->>'phase' IN ('starting','processing','source_wait','source_request','database','publishing')
 AND ((p_activity->'route'='null'::jsonb AND p_activity->'method'='null'::jsonb)
  OR (jsonb_typeof(p_activity->'route')='string' AND p_activity->>'route'=ANY(ARRAY[
  '/1/championship/information',
  '/1/league/championshipteams',
  '/1/league/getdtconfig',
  '/1/league/information',
  '/1/league/matches',
  '/1/league/players',
  '/1/league/rounds',
  '/1/locker/pressroom',
  '/1/market/myplayers',
  '/1/market/players',
  '/1/market/rosterbids',
  '/1/match/list',
  '/1/notification/list',
  '/1/notification/unread',
  '/1/player/fullprofile',
  '/1/player/summary',
  '/1/ranking/round',
  '/1/user/information',
  '/1/userteam/availables',
  '/1/userteam/dreamteam',
  '/1/userteam/information',
  '/1/userteam/lineup',
  '/1/userteam/moneymovements',
  '/1/userteam/nightmareteam',
  '/1/userteam/roster',
  '/1/userteam/roundlineup',
  '/1/userteam/rounds',
  '/2/championship/teams',
  '/2/league/list',
  '/2/league/matches',
  '/2/locker/news',
  '/2/player/matches',
  '/2/user/activechampionships',
  '/5/league/championshipplayers',
  '/5/strategy/availables',
  'auction_observations',
  'bid_observations',
  'championships',
  'complete_history_baselines',
  'finance_history_states',
  'league_context_observations',
  'league_rule_snapshots',
  'market_observations',
  'market_transactions',
  'player_daily_snapshots',
  'player_history',
  'player_match_observations',
  'player_membership_observations',
  'player_round_dataset',
  'player_round_history_checks',
  'players',
  'real_clubs',
  'rival_finance_evidence',
  'rival_finance_evidence_latest',
  'rival_finance_evidence_versions',
  'round_dream_team',
  'rpc/clause_claim',
  'rpc/clause_cleanup',
  'rpc/clause_observe',
  'rpc/clause_retire_context',
  'rpc/worker_admin_monitor',
  'rpc/worker_checkpoint',
  'rpc/worker_checkpoint_ack',
  'rpc/worker_claim',
  'rpc/worker_cleanup_batch',
  'rpc/worker_enqueue_batch',
  'rpc/worker_guard',
  'rpc/worker_guard_activity',
  'rpc/worker_maintenance',
  'rpc/worker_prepare_source',
  'rpc/worker_publish',
  'rpc/worker_publish_child',
  'rpc/worker_read',
  'rpc/worker_read_cached',
  'rpc/worker_release',
  'rpc/worker_write_fact',
  'user_team_history',
  'user_teams',
  'worker_accounts',
  'worker_checkpoints',
  'worker_chunks',
  'worker_health',
  'worker_jobs',
  'worker_schedule_policy',
  'worker_snapshots'
 ])
   AND jsonb_typeof(p_activity->'method')='string' AND p_activity->>'method' IN ('GET','POST','PATCH','DELETE')))
 AND jsonb_typeof(p_activity->'started_at')='string'
 AND p_activity->>'started_at' ~ '^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]{1,6})?(Z|[+]00:00)$'
 AND jsonb_typeof(p_activity->'sequence')='number' AND p_activity->>'sequence' ~ '^[0-9]{1,15}$'
 AND jsonb_typeof(p_activity->'run_id')='string'
 AND p_activity->>'run_id' ~ '^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$',false) THEN RETURN false; END IF;
 PERFORM (p_activity->>'started_at')::timestamptz;
 RETURN true;
EXCEPTION WHEN invalid_datetime_format OR datetime_field_overflow THEN RETURN false;
END $$;

-- Existing clients keep worker_guard(). New clients piggyback activity on the
-- same lease-renewal requests; no second write or acquisition timer is added.
CREATE FUNCTION public.worker_guard_activity(p_account text,p_key text,p_owner text,p_generation bigint,
 p_checkpoint jsonb DEFAULT NULL,p_activity jsonb DEFAULT NULL)
RETURNS boolean LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
BEGIN
 IF p_activity IS NOT NULL AND NOT public.worker_activity_valid(p_activity) THEN
  RAISE EXCEPTION 'Invalid worker activity';
 END IF;
 UPDATE worker_jobs j SET lease_until=now()+interval '60 seconds',checkpoint=coalesce(p_checkpoint,j.checkpoint),
 activity=CASE WHEN p_activity IS NOT NULL AND
  (j.activity IS NULL OR j.activity->>'owner' IS DISTINCT FROM p_owner
   OR j.activity->>'run_id' IS DISTINCT FROM j.run_id::text
   OR (p_activity->>'sequence')::bigint>=coalesce((j.activity->>'sequence')::bigint,-1))
  THEN p_activity||jsonb_build_object('owner',p_owner) ELSE j.activity END,
 activity_at=CASE WHEN p_activity IS NOT NULL AND
  (j.activity IS NULL OR j.activity->>'owner' IS DISTINCT FROM p_owner
   OR j.activity->>'run_id' IS DISTINCT FROM j.run_id::text
   OR (p_activity->>'sequence')::bigint>=coalesce((j.activity->>'sequence')::bigint,-1))
  THEN now() ELSE j.activity_at END
 FROM worker_accounts a WHERE j.account_id=p_account AND j.resource_key=p_key AND j.owner=p_owner
 AND j.lease_until>now() AND a.account_id=j.account_id AND a.enabled AND a.auth_status='active'
 AND a.generation=p_generation AND j.generation=p_generation
 AND (p_activity IS NULL OR p_activity->>'run_id'=j.run_id::text)
 AND NOT(a.excluded_leagues ? (j.descriptor->'context'->>'championship_id'))
 AND public.worker_finance_context_matches(j.descriptor,a.verified_leagues->(j.descriptor->'context'->>'championship_id'))
 AND NOT EXISTS(SELECT 1 FROM public.blocked_leagues b WHERE b.championship_id=j.descriptor->'context'->>'championship_id')
 AND EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now());
 RETURN FOUND;
END $$;

-- Cross-account operational view. Only the trusted R server may execute this;
-- it independently verifies the signed-in administrator before every read.
CREATE FUNCTION public.worker_admin_monitor(p_account text) RETURNS jsonb
LANGUAGE sql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
 WITH recent AS MATERIALIZED (
  SELECT h.*,l.lease_until AS leader_lease
  FROM worker_health h LEFT JOIN worker_leader l ON l.owner=h.id AND l.lease_until>now()
  WHERE h.heartbeat>now()-interval '180 seconds'
 ), running AS MATERIALIZED (
  SELECT j.account_id,j.resource_key,j.owner,j.run_id,j.started_at,j.lease_until,
   j.activity,j.activity_at,j.descriptor->>'handler' AS handler,split_part(j.owner,':',1) AS worker_id,
   CASE WHEN public.worker_activity_valid(j.activity-'owner')
    AND j.activity->>'owner'=j.owner AND j.activity->>'run_id'=j.run_id::text
    AND j.activity_at IS NOT NULL THEN true ELSE false END AS activity_valid
  FROM worker_jobs j JOIN recent h ON h.id=split_part(j.owner,':',1)
  WHERE j.owner IS NOT NULL AND j.lease_until>now() AND h.leader_lease>now()
 ), jobs AS MATERIALIZED (
  SELECT account_id,worker_id,started_at,resource_key,
   jsonb_build_object('job_id',left(resource_key,12),
    'worker_id',CASE WHEN worker_id ~ '^[0-9a-fA-F-]{8,64}$' THEN worker_id ELSE left(md5(worker_id),16) END,
    'lane',CASE WHEN split_part(owner,':',2) ~ '^[0-9]{1,2}$' THEN split_part(owner,':',2) ELSE '?' END,
    'account_ref',left(md5(account_id),12),
    'handler',CASE WHEN handler ~ '^(get_|read_|fm_worker_)[a-z_]{1,80}$'
      THEN handler ELSE 'unknown' END,
    'started_at',started_at,'lease_until',lease_until,
    'activity',CASE WHEN activity_valid THEN jsonb_build_object(
      'phase',activity->>'phase','route',activity->'route','method',activity->'method',
      'started_at',activity->>'started_at','reported_at',activity_at,
      'fresh',activity_at>now()-interval '30 seconds') ELSE NULL END) AS item
  FROM running ORDER BY (account_id=p_account) DESC,started_at,resource_key LIMIT 128
 ), workers AS MATERIALIZED (
  SELECT h.id,h.heartbeat,jsonb_build_object(
   'worker_id',CASE WHEN h.id ~ '^[0-9a-fA-F-]{8,64}$' THEN h.id ELSE left(md5(h.id),16) END,
   'status',CASE WHEN h.leader_lease>now() THEN 'leader' WHEN h.status='standby' THEN 'standby' ELSE 'recent' END,
   'heartbeat',h.heartbeat,'lease_until',h.leader_lease,'requests',h.requests,'jobs',h.jobs,
   'active_jobs',(SELECT count(*) FROM running j WHERE j.worker_id=h.id)) AS item
  FROM recent h ORDER BY (h.leader_lease>now()) DESC NULLS LAST,h.heartbeat DESC,h.id LIMIT 64
 )
 SELECT jsonb_build_object(
  'checked_at',now(),'heartbeat_timeout_seconds',180,'activity_sample_seconds',15,
  'summary',jsonb_build_object('live_workers',(SELECT count(*) FROM recent),
   'working_workers',(SELECT count(DISTINCT worker_id) FROM running),
   'active_jobs',(SELECT count(*) FROM running),
   'retained_workers',(SELECT count(*) FROM worker_health WHERE heartbeat<=now()-interval '180 seconds'),
   'expired_jobs',(SELECT count(*) FROM worker_jobs WHERE owner IS NOT NULL AND lease_until<=now())),
  'workers',coalesce((SELECT jsonb_agg(item ORDER BY heartbeat DESC,id) FROM workers),'[]'::jsonb),
  'current',jsonb_build_object('active_jobs',(SELECT count(*) FROM running WHERE account_id=p_account),
   'jobs',coalesce((SELECT jsonb_agg(item ORDER BY started_at,resource_key) FROM jobs WHERE account_id=p_account),'[]'::jsonb)),
  'others',jsonb_build_object('active_jobs',(SELECT count(*) FROM running WHERE account_id<>p_account),
   'jobs',coalesce((SELECT jsonb_agg(item ORDER BY started_at,resource_key) FROM jobs WHERE account_id<>p_account),'[]'::jsonb)),
  'limits',jsonb_build_object('workers_truncated',(SELECT count(*)>64 FROM recent),
   'jobs_truncated',(SELECT count(*)>128 FROM running)))
 WHERE p_account IS NOT NULL AND length(trim(p_account)) BETWEEN 1 AND 128;
$$;
REVOKE ALL ON FUNCTION public.worker_activity_valid(jsonb),
 public.worker_guard_activity(text,text,text,bigint,jsonb,jsonb),public.worker_admin_monitor(text)
 FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_activity_valid(jsonb),
 public.worker_guard_activity(text,text,text,bigint,jsonb,jsonb),public.worker_admin_monitor(text)
 TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
