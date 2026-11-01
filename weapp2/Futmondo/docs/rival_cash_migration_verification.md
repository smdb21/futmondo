# Rival cash evidence migration verification

Applied migration `20261031192116_rival_cash_evidence` to Supabase project `ozhnextrcxxksswpnglt` on 2026-10-31.

- Latest-view transactional assertions passed: revised movement replaces its previous value (including zero), a full revised award snapshot removes former recipients, and account/season scopes stay separate.
- Synthetic verification was rolled back; zero fixture rows remain.
- Confirmed opening cash is 300,000,000 EUR only for league `5b55f9d767214483120b87cf` (OHY CAMPEÓN).
- Both tables have RLS enabled. Anonymous and authenticated roles cannot read; service_role can write. The latest view uses security_invoker=true.
- Security advisor reports expected policy-free RLS for server-only tables. Existing unrelated warnings remain for futmondo_players GraphQL visibility and rls_auto_enable execution privileges; no permissions on those existing objects were changed.
- Primary-key prefix supports account/league/season bulk reads. No additional speculative indexes or dependencies were introduced.

This verifies storage behavior and permissions, not authenticated Futmondo API contracts or hosted deployment. Complete historical payments and award eligibility remain dependent on source evidence.

## Nullable transfer-method correction (2026-11-01)

Primary agent applied Supabase migration version **20261101164116**, name `rival_cash_classification`; repository SQL uses CLI-generated `20261101163613_rival_cash_classification.sql`.

- Before/after: **1,101 rows**, same financial/identity digest **172596241afefd2a193078f116827327**, ordered by row ID over ID/player/league/buyer/seller/price/date/source-event/provenance. No cash amount, identity, source annotation or transaction date changed.
- Explicit clause TRUE: **15 retained**. FALSE: **1,086 → 976**; unknown NULL: **0 → 110**, restricted to assumed pressroom/adopted rival-to-rival FALSE records. System counterparties remain ordinary.
- `is_clause` is nullable with NULL default. Updated reconciliation function accepts a unique financially identical legacy candidate despite a missing method, preserves its row identity and retains known classification on unknown replay. No permission/RLS expansion or new public function was introduced.
- Hosted application deployment is separate. The old deployed importer can still write its assumed FALSE values until the new app is deployed; production behavior has not been verified on Posit or Android.
