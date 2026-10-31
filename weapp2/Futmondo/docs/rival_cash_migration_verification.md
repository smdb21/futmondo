# Rival cash evidence migration verification

Applied migration `20261031192116_rival_cash_evidence` to Supabase project `ozhnextrcxxksswpnglt` on 2026-10-31.

- Latest-view transactional assertions passed: revised movement replaces its previous value (including zero), a full revised award snapshot removes former recipients, and account/season scopes stay separate.
- Synthetic verification was rolled back; zero fixture rows remain.
- Confirmed opening cash is 300,000,000 EUR only for league `5b55f9d767214483120b87cf` (OHY CAMPEÓN).
- Both tables have RLS enabled. Anonymous and authenticated roles cannot read; service_role can write. The latest view uses security_invoker=true.
- Security advisor reports expected policy-free RLS for server-only tables. Existing unrelated warnings remain for futmondo_players GraphQL visibility and rls_auto_enable execution privileges; no permissions on those existing objects were changed.
- Primary-key prefix supports account/league/season bulk reads. No additional speculative indexes or dependencies were introduced.

This verifies storage behavior and permissions, not authenticated Futmondo API contracts or hosted deployment. Complete historical payments and award eligibility remain dependent on source evidence.
