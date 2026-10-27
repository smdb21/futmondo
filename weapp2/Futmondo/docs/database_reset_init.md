# Database lifecycle helpers

`supabase_known_tables()` is the shared registry of 15 retained factual tables. `get_table_row_counts()` uses exact count headers and minimal identity columns; missing or rejected counts are `NA`, not zero. `init_supabase_db(verbose=FALSE)` checks table access and returns a logical result without applying DDL.

`supabase_primary_key(table_name)` supplies a nonnullable identity component for count/delete filters. Composite observation keys use `championship_id`, except player-match observations use `player_id`; ordinary entities use `id`. This helper does not claim those single columns uniquely identify composite records.

`supabase_delete_all(table_name)` rejects tables outside the factual registry. `supabase_reset_database(force=FALSE)` performs no deletion unless `force=TRUE`; then it deletes observation revisions and dependent histories before teams, players, clubs, and championships. Its result reports each table separately. The administrator UI adds authenticated confirmation and context checks.

`scripts/init_db.R` verifies access and `scripts/reset_db.R --force` is an explicit destructive maintenance command. Neither runs at startup or on ordinary Refresh. Fresh installation and forward migration SQL are documented in [database migration](database_migration.md). The broad population script and its callable entry point have been removed.

`test/test_supabase_schema_helpers.R` exercises the registry, unknown-table rejection, reset gate/order, minimal query construction, missing counts, and failed probes using local fixtures only.
