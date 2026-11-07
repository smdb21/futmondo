-- Server-only admission policy. Application admins are verified on the R server.
CREATE TABLE public.application_access (
  id TEXT PRIMARY KEY DEFAULT 'main' CHECK (id = 'main'),
  admin_only BOOLEAN NOT NULL DEFAULT TRUE,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
ALTER TABLE public.application_access ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.application_access FROM PUBLIC, anon, authenticated, service_role;
GRANT SELECT, INSERT, UPDATE ON public.application_access TO service_role;
INSERT INTO public.application_access (id, admin_only) VALUES ('main', TRUE);
COMMENT ON TABLE public.application_access IS 'Server-only application access switch. Default blocks non-admins. No browser policies; app server verifies administrator identity before writes. Excluded from factual-data reset.';
