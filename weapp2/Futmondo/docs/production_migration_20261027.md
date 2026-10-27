# Production database migration — 2026-10-27 UTC

Applied directly through the Supabase connector to project `ozhnextrcxxksswpnglt`, following the user's explicit request. No delegation was used for the rollout. The application was not deployed during this operation.

| Supabase migration version | Migration | Reviewed source |
| --- | --- | --- |
| `20261027071111` | `factual_history_indexes` | `scripts/migrations/20261027034734_factual_history_indexes.sql` |
| `20261027071142` | `retire_prediction_automation` | `scripts/migrations/20261027034735_retire_prediction_automation.sql` |

Supabase assigned the execution versions above; the source filenames retain their preparation timestamps. Both migrations committed successfully. Execution added transaction-local lock and statement timeouts of 5 and 60 seconds. Existing schema objects were present, but the migration-history table was empty before this rollout; older SQL files had not been registered there. Do not replay the historical feature migrations.

## Verification passed

- Seven new indexes exist and are valid/ready.
- All 15 retained factual tables have RLS enabled, no anonymous/authenticated SELECT privilege, and server-role CRUD access.
- Twelve retired feature tables moved to `retired_features`; none remain in `public`. The archive contains 44 existing rows, with identical content checksums. API roles, including `service_role`, have no archive schema usage or table SELECT access.
- All three existing obsolete automation functions were removed. The fourth targeted function was already absent. No targeted automation function remains in `public`.
- All 32 original tables reconcile: 151,932 rows total, with identical per-table counts and content digests before/after migration. No historical timestamps or records were rewritten.
- Read-only queries under the actual `service_role` successfully read price history (113,367 rows), team history (2,570), transactions (1,054), and both auction/bid history views (540/933).

Before rollout there were no active client queries, no `cron.job` table, and no foreign keys referencing the retired tables. The database was healthy on PostgreSQL 17.6. Pre-migration schema metadata, object permissions, automation function definitions, and row checksums are retained in [before.json](migration_runs/20261027/before.json); [after.json](migration_runs/20261027/after.json) records versions and verification. These are audit/recovery metadata, not a full database backup or an export of private records. Rows remain in their factual tables or the private archive.

## Remaining deployment state

The Shiny application deployment and hosted end-to-end checks remain pending. The user explicitly authorized applying the DB migrations before that separate release step. A still-deployed older application cannot use retired prediction/automation interfaces. The new factual app is locally implemented and tested.

`player_match_observations` still has zero rows: this schema migration does not invent or backfill scores. The new app reads actual API round history and records observations as users inspect players. Existing historical prices and transaction histories remain intact.

The post-migration security advisor reports no warnings on the changed factual tables or retired automation functions. Existing warnings remain for the unrelated legacy `public.futmondo_players` object's [GraphQL schema visibility](https://supabase.com/docs/guides/database/database-linter?lint=0026_pg_graphql_anon_table_exposed) and the existing `public.rls_auto_enable()` function's [public execution grant](https://supabase.com/docs/guides/database/database-linter?lint=0028_anon_security_definer_function_executable). These were outside the reviewed migration and unchanged. RLS-without-policy informational notices are expected for server-only/archived tables.

No live trades, test inserts, or application deployment were performed. Existing isolated migration/retry/rollback checks were already passing and were not redundantly rerun; production counts, digests, privileges, indexes, migration history and server-role reads were checked after commit. Follow [migration and rollback](database_migration.md) for future recovery; restoring excluded automation as an application rollback is not intended.
