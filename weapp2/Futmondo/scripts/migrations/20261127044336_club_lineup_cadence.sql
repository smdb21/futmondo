-- An external team-sheet job can wake for a fixture before ordinary factual sync.
BEGIN;
CREATE OR REPLACE FUNCTION public.worker_next_refresh_due(p_handler text,p_interval integer,p_valid_until timestamptz,p_started timestamptz,p_now timestamptz DEFAULT now())
RETURNS timestamptz LANGUAGE sql STABLE SECURITY INVOKER SET search_path=public,pg_temp AS $$
 SELECT CASE WHEN p_handler='get_club_lineups' AND p_valid_until IS NOT NULL
   THEN greatest(p_now+interval '1 minute',least(ordinary_due,p_valid_until))
   ELSE ordinary_due END
 FROM (SELECT p_now+make_interval(secs=>greatest(p_interval,(SELECT base_interval FROM public.worker_schedule_policy WHERE id=1))) AS ordinary_due) schedule;
$$;
REVOKE ALL ON FUNCTION public.worker_next_refresh_due(text,integer,timestamptz,timestamptz,timestamptz) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.worker_next_refresh_due(text,integer,timestamptz,timestamptz,timestamptz) TO service_role;
COMMIT;
