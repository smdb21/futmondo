# Background acquisition production migration — 2026-11-11

Applied the exact `scripts/migrations/20261111214239_background_acquisition.sql` to the documented production Supabase project `ozhnextrcxxksswpnglt` using the Supabase migration tool. The hosted tool recorded it as version `20261111231834`, name `background_acquisition`; this differs from the source filename timestamp. Source SHA-256: `b80c333c35f4a40179415cfb6e31c1c5d0ee5d4d6458c3a907796472d5dea563`.

Verification after successful application:

- Migration history contains the successful entry.
- All eight worker tables exist with RLS enabled. Anonymous and authenticated browser roles have no SELECT/INSERT/UPDATE/DELETE privileges; service_role has the required privileges.
- All thirteen worker functions are SECURITY INVOKER, deny execution to anonymous/authenticated roles and permit service_role execution.
- In a rolled-back transaction, service_role successfully called worker_read for a nonexistent verification account and received account/health null, jobs/resources empty. No account or factual test data was inserted.
- EXPLAIN of the due-job lookup uses worker_jobs_due. The migration adds empty worker tables/indexes and functions; existing application facts are not rewritten. This confirms index availability, not production throughput or latency under acquisition load.
- Security advisors report the expected informational no-policy notices for server-only worker tables. Separate warnings concern existing futmondo_players GraphQL discovery and rls_auto_enable SECURITY DEFINER execution; these objects were not modified by this migration.

Docker build/start, deployment, FUTMONDO_WORKER_KEY configuration, authenticated source acceptance and VM sizing remain unverified. Applying this migration does not enroll accounts or start acquisition. This delivery changes documentation only in the repository, with no new runtime/dependency change; no further local runtime tests are required for this record.
