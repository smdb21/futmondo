-- Immutable, account-scoped evidence; corrections preserve earlier observations.
CREATE TABLE public.league_cash_defaults (
 championship_id text PRIMARY KEY CHECK (length(btrim(championship_id)) > 0),
 amount numeric NOT NULL CHECK (amount >= 0 AND amount < 'Infinity'::numeric),
 source text NOT NULL CHECK (length(btrim(source)) > 0),
 observed_at timestamptz NOT NULL DEFAULT now() CHECK (isfinite(observed_at))
);
CREATE TABLE public.rival_finance_evidence (
 account_id text NOT NULL CHECK (length(btrim(account_id)) > 0),
 championship_id text NOT NULL CHECK (length(btrim(championship_id)) > 0),
 season text NOT NULL CHECK (length(btrim(season)) > 0),
 evidence_type text NOT NULL CHECK (evidence_type IN ('awards','rules','movements','team','coverage','eligibility','notifications')),
 source_id text NOT NULL CHECK (length(btrim(source_id)) > 0),
 observed_at timestamptz NOT NULL CHECK (isfinite(observed_at)),
 payload jsonb NOT NULL CHECK (jsonb_typeof(payload) = 'object' AND octet_length(payload::text) <= 1048576),
 PRIMARY KEY (account_id, championship_id, season, evidence_type, source_id, observed_at)
);
CREATE VIEW public.rival_finance_evidence_latest WITH (security_invoker = true) AS
 SELECT DISTINCT ON (account_id, championship_id, season, evidence_type, source_id)
 account_id, championship_id, season, evidence_type, source_id, observed_at, payload
 FROM public.rival_finance_evidence
 ORDER BY account_id, championship_id, season, evidence_type, source_id, observed_at DESC;
ALTER TABLE public.league_cash_defaults ENABLE ROW LEVEL SECURITY;
ALTER TABLE public.rival_finance_evidence ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.league_cash_defaults, public.rival_finance_evidence, public.rival_finance_evidence_latest FROM PUBLIC, anon, authenticated, service_role;
GRANT SELECT, INSERT, UPDATE ON public.league_cash_defaults, public.rival_finance_evidence TO service_role;
GRANT SELECT ON public.rival_finance_evidence_latest TO service_role;
INSERT INTO public.league_cash_defaults(championship_id, amount, source)
 VALUES ('5b55f9d767214483120b87cf', 300000000, 'user_confirmed');
COMMENT ON TABLE public.rival_finance_evidence IS 'Server-only account/league/season-scoped observed accounting revisions. Award payloads replace the whole source selection; notifications are not payments.';
NOTIFY pgrst, 'reload schema';
