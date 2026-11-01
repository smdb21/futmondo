-- Browser credentials are opaque. Only encrypted Futmondo tokens are retained.
CREATE TABLE public.remembered_logins (
 browser_hash text PRIMARY KEY CHECK (browser_hash ~ '^[a-f0-9]{64}$'),
 encrypted_payload text NOT NULL CHECK (length(encrypted_payload) BETWEEN 56 AND 16384),
 created_at timestamptz NOT NULL DEFAULT now() CHECK (isfinite(created_at)),
 expires_at timestamptz NOT NULL CHECK (isfinite(expires_at)),
 revoked_at timestamptz CHECK (revoked_at IS NULL OR isfinite(revoked_at)),
 CHECK (expires_at > created_at AND expires_at <= created_at + interval '7 days')
);
CREATE INDEX remembered_logins_expiry_idx ON public.remembered_logins (expires_at);
ALTER TABLE public.remembered_logins ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.remembered_logins FROM PUBLIC, anon, authenticated, service_role;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.remembered_logins TO service_role;
COMMENT ON TABLE public.remembered_logins IS 'Server-only seven-day remembered login credentials: SHA-256 browser secret hash and authenticated encrypted login envelope. No passwords or plaintext tokens. No public RLS policies.';
NOTIFY pgrst, 'reload schema';
