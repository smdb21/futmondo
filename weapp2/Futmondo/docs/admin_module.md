# Administration

`admin_UI(id)` and `admin_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV)` expose explicit database diagnostics to the configured administrator. Every operation rechecks the authenticated account and active module. Reset confirmation captures the current account/league; changing context invalidates it.

`get_table_definitions()` derives the 15 retained factual tables from `supabase_known_tables()`. Counts load only after **Refresh table counts**; activation does not start a synchronous scan. Missing count headers and failed requests remain unknown. **Verify database connection** checks minimal identity columns; it does not create schemas. These explicit administrative probes are synchronous and can take up to the configured HTTP timeout per table.

**Save official round MVP results** preserves fetched/queued/acknowledged/failed counts. Queued work is never reported as saved. The ordinary MVP page uses the shared async loader. There is no full-database population or rotating player collection action.

**Reset retained database tables** requires the explicit confirmation modal and removes factual records in child-to-parent order. Take a restorable backup first. The redesign does not execute a reset. Archived retired feature tables are outside this operation.

See [database lifecycle helpers](database_reset_init.md) and [migration and rollback](database_migration.md). Offline coverage includes schema helpers and the full application lifecycle.


## Round awards — 2026-10-31

Admin now reads the shared full-XI snapshot coverage and exposes explicit missing/provisional/selected retrieval and failed-save retry. Numeric round ordering, selection/finality/MVP, last fetched and acknowledged save states are independent. Acquisition never starts on navigation. See [coach_and_round_awards.md](coach_and_round_awards.md) for the API, persistence, concurrency and finality contracts.


## League settings — 2026-10-31

Admin starts with a read-only **League settings** section for the currently selected league. Starting conditions, round rewards, market settings and squad/league limits use stacked label/value/explanation rows that wrap on phones. Database and award controls remain below.

`admin_league_settings(league, info, championship_id)` is a pure presentation adapter returning group, setting, value and explanation columns. It combines the league configuration with the primary team configuration, with the latter taking precedence. It exposes only known fields, preserves zero/false, marks missing values, and labels the user-confirmed opening cash fallback separately from API values. Unknown ranking modes are not interpreted. Ranking direction/distribution uses the established contracts documented in `rival_cash_accounting.md`; this display does not calculate payments.

`admin_Server(..., league_settings_source = NULL)` accepts a reactive snapshot with `league`, `info` and `stale` fields. The application supplies its existing selected league and primary team resource state. Rendering is gated by active Admin navigation and the existing administrator check. No new requests, writes, caches, timers, or dependencies are added. The module can still initialize without this optional source.

Configuration changes must be made in Futmondo. The existing app refresh updates shared resource observations. The display describes current rules, not proof of historical rules or bonus payments. Focused coverage: `test/test_admin_league_settings.R`; shared wiring is covered by the offline lifecycle harness.
