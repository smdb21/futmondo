BEGIN;
SET LOCAL lock_timeout='5s';
SET LOCAL statement_timeout='30s';
CREATE TABLE IF NOT EXISTS public.user_timezone_preferences (
 user_id text PRIMARY KEY CHECK (length(user_id) BETWEEN 1 AND 256 AND user_id ~ '^[A-Za-z0-9_-]+$'),
 display_name text NOT NULL DEFAULT '' CHECK (length(display_name)<=256),
 timezone text NOT NULL DEFAULT 'Europe/Madrid',
 source text NOT NULL CHECK (source IN ('browser','user','admin')),
 updated_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
ALTER TABLE public.user_timezone_preferences ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.user_timezone_preferences FROM PUBLIC, anon, authenticated, service_role;
GRANT SELECT, INSERT, UPDATE ON public.user_timezone_preferences TO service_role;
-- Server-only validated RPCs; no credentials or browser locations.
CREATE OR REPLACE FUNCTION public.initialize_user_timezone(p_user text,p_timezone text,p_label text DEFAULT '')
RETURNS SETOF public.user_timezone_preferences LANGUAGE plpgsql SECURITY INVOKER SET search_path='' AS $$
BEGIN
 IF NOT EXISTS(SELECT 1 FROM pg_timezone_names WHERE name=p_timezone) THEN RAISE EXCEPTION 'Invalid timezone'; END IF;
 RETURN QUERY INSERT INTO public.user_timezone_preferences(user_id,display_name,timezone,source)
 VALUES(p_user,p_label,p_timezone,'browser') ON CONFLICT(user_id) DO UPDATE SET display_name=excluded.display_name
 RETURNING *;
END;
$$;
CREATE OR REPLACE FUNCTION public.set_user_timezone(p_user text,p_timezone text,p_source text)
RETURNS SETOF public.user_timezone_preferences LANGUAGE plpgsql SECURITY INVOKER SET search_path='' AS $$
BEGIN
 IF NOT EXISTS(SELECT 1 FROM pg_timezone_names WHERE name=p_timezone) THEN RAISE EXCEPTION 'Invalid timezone'; END IF;
 IF p_source NOT IN ('user','admin') OR p_source IS NULL THEN RAISE EXCEPTION 'Invalid source'; END IF;
 RETURN QUERY INSERT INTO public.user_timezone_preferences(user_id,timezone,source)
 VALUES(p_user,p_timezone,p_source) ON CONFLICT(user_id) DO UPDATE
 SET timezone=excluded.timezone,source=excluded.source,updated_at=clock_timestamp() RETURNING *;
END;
$$;
CREATE OR REPLACE FUNCTION public.list_user_timezones(p_after text DEFAULT '',p_search text DEFAULT '')
RETURNS TABLE(user_id text,display_name text,timezone text,source text,updated_at timestamptz)
LANGUAGE sql STABLE SECURITY INVOKER SET search_path='' AS $$
 WITH accounts AS (
  SELECT user_id FROM public.user_timezone_preferences
  UNION SELECT user_id FROM public.user_page_preferences
  UNION SELECT account_id FROM public.worker_accounts
 )
 SELECT a.user_id,coalesce(t.display_name,''),t.timezone,t.source,t.updated_at
 FROM accounts a LEFT JOIN public.user_timezone_preferences t ON t.user_id=a.user_id
 WHERE a.user_id>p_after AND (p_search='' OR strpos(lower(a.user_id),lower(p_search))>0
  OR strpos(lower(coalesce(t.display_name,'')),lower(p_search))>0)
 ORDER BY a.user_id LIMIT 201;
$$;
REVOKE ALL ON FUNCTION public.initialize_user_timezone(text,text,text),public.set_user_timezone(text,text,text),public.list_user_timezones(text,text) FROM PUBLIC,anon,authenticated;
GRANT EXECUTE ON FUNCTION public.initialize_user_timezone(text,text,text),public.set_user_timezone(text,text,text),public.list_user_timezones(text,text) TO service_role;
COMMENT ON TABLE public.user_timezone_preferences IS 'Server-only display timezone per Futmondo account; browser initialized once, user/admin override persists. No coordinates or authentication secrets.';
NOTIFY pgrst,'reload schema';
COMMIT;
