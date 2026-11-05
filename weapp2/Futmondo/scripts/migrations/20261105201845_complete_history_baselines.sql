-- Atomic, server-only baselines for complete source history reads.
-- A missing row is a cache miss; this table never replaces factual ledgers.
BEGIN;
CREATE TABLE IF NOT EXISTS public.complete_history_baselines (
 account_id text NOT NULL CHECK (length(account_id) BETWEEN 1 AND 256),
 championship_id text NOT NULL CHECK (length(championship_id) BETWEEN 1 AND 256),
 season text NOT NULL CHECK (length(season) BETWEEN 1 AND 256),
 scoring_version text NOT NULL CHECK (length(scoring_version) BETWEEN 1 AND 256),
 resource text NOT NULL,
 encoding text NOT NULL CHECK (encoding = 'json-gzip-base64-v1'),
 payload text NOT NULL CHECK (octet_length(payload) BETWEEN 1 AND 921600),
 payload_sha256 text NOT NULL CHECK (payload_sha256 ~ '^[0-9a-f]{64}$'),
 json_bytes integer NOT NULL CHECK (json_bytes BETWEEN 1 AND 16777216),
 snapshot_hash text NOT NULL CHECK (snapshot_hash ~ '^[0-9a-f]{64}$'),
 observed_at timestamptz NOT NULL,
 recorded_at timestamptz NOT NULL DEFAULT now(),
 PRIMARY KEY (account_id,championship_id,season,scoring_version,resource),
 CHECK (resource = 'pressroom_' || championship_id OR
        resource = 'pressroom_locker_payments_' || championship_id)
);
ALTER TABLE public.complete_history_baselines ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.complete_history_baselines FROM PUBLIC, anon, authenticated;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.complete_history_baselines TO service_role;

-- Concurrent sessions must not replace newer complete history with older data.
CREATE OR REPLACE FUNCTION public.keep_newest_complete_history_baseline()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER SET search_path = '' AS $$
BEGIN
 IF NEW.observed_at < OLD.observed_at THEN RETURN OLD; END IF;
 NEW.recorded_at := clock_timestamp();
 RETURN NEW;
END;
$$;
REVOKE ALL ON FUNCTION public.keep_newest_complete_history_baseline() FROM PUBLIC, anon, authenticated;
GRANT EXECUTE ON FUNCTION public.keep_newest_complete_history_baseline() TO service_role;
DROP TRIGGER IF EXISTS keep_newest_complete_history_baseline ON public.complete_history_baselines;
CREATE TRIGGER keep_newest_complete_history_baseline
BEFORE UPDATE ON public.complete_history_baselines
FOR EACH ROW EXECUTE FUNCTION public.keep_newest_complete_history_baseline();
COMMENT ON TABLE public.complete_history_baselines IS
 'Bounded normalized source snapshots and completion metadata, atomically replaced after active-session reads. No authentication material.';
NOTIFY pgrst, 'reload schema';
COMMIT;
