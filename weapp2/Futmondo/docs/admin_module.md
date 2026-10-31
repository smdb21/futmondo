# Administration

`admin_UI(id)` and `admin_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV)` expose explicit database diagnostics to the configured administrator. Every operation rechecks the authenticated account and active module. Reset confirmation captures the current account/league; changing context invalidates it.

`get_table_definitions()` derives the 15 retained factual tables from `supabase_known_tables()`. Counts load only after **Refresh table counts**; activation does not start a synchronous scan. Missing count headers and failed requests remain unknown. **Verify database connection** checks minimal identity columns; it does not create schemas. These explicit administrative probes are synchronous and can take up to the configured HTTP timeout per table.

**Save official round MVP results** preserves fetched/queued/acknowledged/failed counts. Queued work is never reported as saved. The ordinary MVP page uses the shared async loader. There is no full-database population or rotating player collection action.

**Reset retained database tables** requires the explicit confirmation modal and removes factual records in child-to-parent order. Take a restorable backup first. The redesign does not execute a reset. Archived retired feature tables are outside this operation.

See [database lifecycle helpers](database_reset_init.md) and [migration and rollback](database_migration.md). Offline coverage includes schema helpers and the full application lifecycle.


## Round awards — 2026-10-31

Admin now reads the shared full-XI snapshot coverage and exposes explicit missing/provisional/selected retrieval and failed-save retry. Numeric round ordering, selection/finality/MVP, last fetched and acknowledged save states are independent. Acquisition never starts on navigation. See [coach_and_round_awards.md](coach_and_round_awards.md) for the API, persistence, concurrency and finality contracts.
