BEGIN;
CREATE TABLE public.user_page_preferences (
 user_id text PRIMARY KEY CHECK (length(user_id) BETWEEN 1 AND 256 AND user_id ~ '^[A-Za-z0-9_-]+$'),
 page text NOT NULL CHECK (page IN ('notifications','today','yourteam','players_in_championship','rivals','plots','classification','round_mvps','admin')),
 visited_at timestamptz NOT NULL,
 recorded_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
ALTER TABLE public.user_page_preferences ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.user_page_preferences FROM PUBLIC, anon, authenticated, service_role;
GRANT SELECT, INSERT, UPDATE ON public.user_page_preferences TO service_role;
CREATE FUNCTION public.keep_latest_user_page()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
BEGIN
 IF NEW.visited_at < OLD.visited_at THEN RETURN OLD; END IF;
 NEW.recorded_at := clock_timestamp();
 RETURN NEW;
END;
$$;
REVOKE ALL ON FUNCTION public.keep_latest_user_page() FROM PUBLIC, anon, authenticated;
GRANT EXECUTE ON FUNCTION public.keep_latest_user_page() TO service_role;
CREATE TRIGGER keep_latest_user_page BEFORE UPDATE ON public.user_page_preferences
FOR EACH ROW EXECUTE FUNCTION public.keep_latest_user_page();
COMMENT ON TABLE public.user_page_preferences IS 'Server-only last visited application page per Futmondo account. No credentials. Login is excluded; authorization is checked before restoring Admin.';
NOTIFY pgrst, 'reload schema';
COMMIT;
