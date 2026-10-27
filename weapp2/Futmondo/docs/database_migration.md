# Database migration and rollback

The app's factual redesign does not require deleting historical data. New installations use `scripts/schema.sql`. The two forward migrations below target an existing database with the earlier history migrations already applied.

## Existing installation

1. Back up the database and record the deployed schema/application version.
2. Rehearse `scripts/migrations/20261027034734_factual_history_indexes.sql` in an isolated or staging database. It adds query indexes and explicit server-only grants without rewriting records or unknown timestamps.
3. Deploy and verify the factual application. Check representative player cards, histories, league switching, and manual transaction preflights.
4. Apply `scripts/migrations/20261027034735_retire_prediction_automation.sql` only after no retained application code needs those objects.

Retirement drops the obsolete automation RPCs and moves feature tables into `retired_features`. It revokes schema/table/sequence access from API roles, including the service role. Existing feature records remain available to the database owner for audit. Factual histories, transaction rows, catalog identities, and source timestamps remain in place.

Retirement refuses to overwrite a pre-existing archived table of the same name. Both migrations are repeatable. Already-applied historical migration files remain immutable records; do not rerun them after retirement.

## Rollback

Each migration is transactional. An error rolls back its operations. Before applying, rehearse the statements with `ROLLBACK` replacing the final `COMMIT`; the isolated test exercises this path.

After a successful commit, restore a compatible factual application revision if a UI regression occurs. Additive indexes can remain. Retired tables are recoverable by the database owner from the archive or backup, but restoring their public schema access or executable RPCs would reactivate excluded features and is not the application rollback path.

If factual data reconciliation fails, stop the rollout and restore the reviewed database backup using the hosting backup mechanism. Do not reset all tables or manufacture missing season/time context to force validation through.

## Verification and production state

`node test/test_factual_schema.cjs` exercises both new and existing schemas, repeatability, source-time preservation, access restrictions, and pre-commit rollback in isolated PostgreSQL. This does not substitute for a production backup or establish production concurrency characteristics.

Both production migrations were applied and verified on 2026-10-27 UTC at the user’s request. Supabase recorded versions `20261027071111` and `20261027071142`; application deployment remains pending. See [production rollout evidence](production_migration_20261027.md) for unchanged-row checksums, privileges, migration history, and remaining hosted checks.
