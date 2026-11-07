BEGIN;
CREATE TABLE IF NOT EXISTS public.team_lineup_snapshots (
 account_id text NOT NULL CHECK (length(account_id) BETWEEN 1 AND 256),
 championship_id text NOT NULL CHECK (length(championship_id) BETWEEN 1 AND 256),
 user_team_id text NOT NULL CHECK (length(user_team_id) BETWEEN 1 AND 256),
 target_round text NOT NULL CHECK (length(target_round) <= 256),
 season text NOT NULL,
 scoring_version text NOT NULL,
 payload jsonb NOT NULL CHECK (jsonb_typeof(payload) = 'object' AND octet_length(payload::text) <= 524288),
 observed_at timestamptz NOT NULL,
 expires_at timestamptz NOT NULL CHECK (expires_at >= observed_at),
 complete boolean NOT NULL CHECK (complete),
 PRIMARY KEY (account_id, championship_id, user_team_id, target_round, season, scoring_version)
);
ALTER TABLE public.team_lineup_snapshots ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.team_lineup_snapshots FROM PUBLIC, anon, authenticated, service_role;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.team_lineup_snapshots TO service_role;
CREATE OR REPLACE FUNCTION public.keep_latest_team_lineup_snapshot()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
BEGIN
 IF NEW.observed_at < OLD.observed_at THEN RETURN OLD; END IF;
 RETURN NEW;
END;
$$;
REVOKE ALL ON FUNCTION public.keep_latest_team_lineup_snapshot() FROM PUBLIC, anon, authenticated;
GRANT EXECUTE ON FUNCTION public.keep_latest_team_lineup_snapshot() TO service_role;
DROP TRIGGER IF EXISTS keep_latest_team_lineup_snapshot ON public.team_lineup_snapshots;
CREATE TRIGGER keep_latest_team_lineup_snapshot BEFORE UPDATE ON public.team_lineup_snapshots
 FOR EACH ROW EXECUTE FUNCTION public.keep_latest_team_lineup_snapshot();
COMMENT ON TABLE public.team_lineup_snapshots IS 'Server-only normalized editable lineup, roster eligibility and formation entitlement snapshots. Exact account/team/round scope; no credentials. Incomplete acquisitions are never saved.';
ALTER TABLE public.user_page_preferences DROP CONSTRAINT IF EXISTS user_page_preferences_page_check;
ALTER TABLE public.user_page_preferences ADD CONSTRAINT user_page_preferences_page_check
 CHECK (page IN ('notifications','today','yourteam','lineup','players_in_championship','rivals','plots','classification','round_mvps','admin'));
NOTIFY pgrst, 'reload schema';
COMMIT;
