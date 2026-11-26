-- Use the latest scoped fixture calendar to slow a round between distant phases.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_enqueue_batch(p_account text,p_owner text,p_generation bigint,p_descriptors jsonb)
RETURNS integer LANGUAGE plpgsql SECURITY INVOKER SET search_path=public,pg_temp AS $$
DECLARE a worker_accounts;item jsonb;d jsonb;seconds integer;accepted integer:=0;
 old_seconds integer;old_priority integer;round_id text;gap jsonb;ctx jsonb;
 at_epoch numeric:=extract(epoch FROM now());
BEGIN
 IF jsonb_typeof(p_descriptors) IS DISTINCT FROM 'array' THEN RAISE EXCEPTION 'Invalid discovery batch'; END IF;
 IF jsonb_array_length(p_descriptors)>128 OR octet_length(p_descriptors::text)>1048576 THEN RAISE EXCEPTION 'Discovery batch limit'; END IF;
 SELECT * INTO a FROM worker_accounts WHERE account_id=p_account FOR SHARE;
 IF a.account_id IS NULL OR NOT a.enabled OR a.auth_status<>'active' OR a.generation<>p_generation
 OR NOT EXISTS(SELECT 1 FROM worker_leader WHERE owner=split_part(p_owner,':',1) AND lease_until>now()) THEN RETURN NULL; END IF;
 FOR item IN SELECT value FROM jsonb_array_elements(p_descriptors) LOOP
  d:=item-'interval_seconds';seconds:=coalesce((item->>'interval_seconds')::integer,300);
  IF jsonb_typeof(d) IS DISTINCT FROM 'object' OR d->'context'->>'account_id' IS DISTINCT FROM p_account
   OR coalesce(d->>'resource_key','') !~ '^[a-f0-9]{64}$'
   OR coalesce(d->>'handler','') !~ '^(get_|read_|fm_worker_finances$)' OR seconds NOT BETWEEN 15 AND 86400
  THEN RAISE EXCEPTION 'Invalid discovery descriptor'; END IF;
  IF a.excluded_leagues ? (d->'context'->>'championship_id')
   OR EXISTS(SELECT 1 FROM blocked_leagues WHERE championship_id=d->'context'->>'championship_id')
   OR NOT worker_finance_context_matches(d,a.verified_leagues->(d->'context'->>'championship_id')) THEN CONTINUE; END IF;
  -- Reuse the existing primary-key lookup for unchanged discoveries.
  SELECT j.native_interval_seconds,j.base_priority INTO old_seconds,old_priority
  FROM worker_jobs j WHERE j.account_id=p_account AND j.resource_key=d->>'resource_key'
   AND j.generation=p_generation AND j.descriptor=d;
  IF old_seconds IS NOT NULL AND old_seconds<>seconds AND seconds IN (300,86400)
   AND d->>'handler' IN ('get_classification_round_ranking','get_round_dreamteam','get_round_lineup_snapshot') THEN
   ctx:=jsonb_build_object('account_id',p_account,'championship_id',d->'context'->>'championship_id',
    'competition_id',d->'context'->>'competition_id','season',d->'context'->>'season');
   round_id:=CASE WHEN d->>'handler'='get_round_dreamteam' THEN d->'args'->>'reference'
                  ELSE d->'args'->>'round_id' END;
   IF coalesce(ctx->>'competition_id','') NOT IN ('','unknown')
    AND coalesce(ctx->>'season','') NOT IN ('','unknown') AND coalesce(round_id,'')<>'' THEN
    -- Serialize rare transitions for this exact job before consulting the
    -- latest durable calendar. Caller order cannot select an older calendar.
    SELECT j.native_interval_seconds,j.base_priority INTO old_seconds,old_priority FROM worker_jobs j
     WHERE j.account_id=p_account AND j.resource_key=d->>'resource_key'
      AND j.generation=p_generation AND j.descriptor=d FOR UPDATE;
    SELECT e.payload->'split_gaps'->round_id INTO gap FROM rival_finance_evidence_latest e
      WHERE e.account_id=p_account AND e.championship_id=ctx->>'championship_id'
       AND e.competition_id=ctx->>'competition_id' AND e.season=ctx->>'season'
       AND e.evidence_type='eligibility'
       AND e.source_id='calendar:v1:'||encode(sha256(convert_to(e.competition_id,'UTF8')),'hex')
       AND e.payload->>'kind'='league_round_calendar' AND e.payload->>'completeness'='complete'
       AND e.payload->'scope' @> ctx AND e.payload->'scope'->>'league_id'=e.competition_id
       AND e.observed_at<=now();
    IF jsonb_typeof(gap->'slow_from')='number' AND jsonb_typeof(gap->'resume_at')='number' THEN
     IF at_epoch>=(gap->>'slow_from')::numeric AND at_epoch<(gap->>'resume_at')::numeric THEN
      seconds:=86400;
      UPDATE worker_jobs j SET native_interval_seconds=86400,interval_seconds=86400,
       due_at=CASE WHEN j.owner IS NULL AND j.last_error IS NULL AND j.checked_at IS NOT NULL
        AND j.priority<=j.base_priority THEN greatest(j.due_at,least(j.checked_at+interval '1 day',
         to_timestamp((gap->>'resume_at')::double precision))) ELSE j.due_at END
       WHERE j.account_id=p_account AND j.resource_key=d->>'resource_key' AND j.generation=p_generation
        AND j.descriptor=d AND j.native_interval_seconds<86400;
     END IF;
    END IF;
   END IF;
  END IF;
  IF old_seconds<=seconds AND old_priority>=(CASE WHEN seconds<=60 THEN 4 WHEN seconds<=300 THEN 3 ELSE 1 END) THEN
   accepted:=accepted+1;
  ELSIF worker_enqueue(p_account,d,seconds,0) THEN accepted:=accepted+1;
  END IF;
  -- Account-only endpoints used to inherit browser league scope / named empty
  -- arguments. Retire duplicate jobs only after the canonical replacement is
  -- durably registered. Factual snapshots and source times are preserved.
  IF d->>'handler' IN ('get_active_championships','get_notifications','get_notification_unread')
   AND d->'args'='[]'::jsonb AND coalesce(d->'context'->>'championship_id','')=''
   AND EXISTS(SELECT 1 FROM worker_jobs WHERE account_id=p_account AND resource_key=d->>'resource_key') THEN
   DELETE FROM worker_jobs WHERE account_id=p_account AND descriptor->>'handler'=d->>'handler'
    AND resource_key<>d->>'resource_key' AND owner IS NULL;
  END IF;
 END LOOP;
 DELETE FROM worker_jobs old WHERE old.account_id=p_account AND old.owner IS NULL
 AND old.descriptor->>'handler' IN ('get_active_championships','get_notifications','get_notification_unread')
 AND old.resource_key<>public.worker_account_descriptor(p_account,old.descriptor->>'handler')->>'resource_key'
 AND EXISTS(SELECT 1 FROM worker_jobs current WHERE current.account_id=p_account
  AND current.resource_key=public.worker_account_descriptor(p_account,old.descriptor->>'handler')->>'resource_key');
 -- Card history now comes from summary publication and existing DB readers.
 DELETE FROM worker_jobs WHERE account_id=p_account AND descriptor->>'handler'='get_player_card_history' AND owner IS NULL;
 RETURN accepted;
END $$;
REVOKE ALL ON FUNCTION public.worker_enqueue_batch(text,text,bigint,jsonb) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_enqueue_batch(text,text,bigint,jsonb) TO service_role;
NOTIFY pgrst,'reload schema';
COMMIT;
