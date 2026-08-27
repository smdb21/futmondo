# Database Reset and Initialization Functions

This document covers the database reset and initialization functions added to `supabase_connector.R` and the standalone scripts in `scripts/`.

## Functions in `supabase_connector.R`

### `supabase_delete(table_name, filter)`

Sends a DELETE request to the Supabase REST API for a given table.

**Parameters:**
- `table_name` (character): The name of the Supabase table to delete from.
- `filter` (character, default `"id=neq.00000000-0000-0000-0000-0000-000000000000"`): A filter string in the format `"column=operator.value"`. The reset flow uses `<primary key>=not.is.null` (e.g. `"id=not.is.null"`, or `"team_id=not.is.null"` for `manager_dna_profiles`).

**Return:** A named list with fields:
- `status` (character): One of `"deleted"`, `"error"`, or `"skipped"`.
- `http_code` (numeric, optional): The HTTP response code if the request was made.
- `reason` (character, optional): Error message or skip reason.

**Usage:**
```R
supabase_delete("championships", filter = "id=not.is.null")
supabase_delete("manager_dna_profiles", filter = "team_id=not.is.null")
```

---

### `supabase_delete_all(table_name)`

Deletes all rows from a known table using the filter `<primary key>=not.is.null`. The primary key is resolved by `supabase_primary_key()`: `manager_dna_profiles` uses `team_id`, and all other tables use `id`. Unknown tables are rejected with an error status.

**Parameters:**
- `table_name` (character): The name of the Supabase table. Must be one of the 12 known tables handled by the connector.

**Return:** A named list (same shape as `supabase_delete`).

**Primary Key Mapping:**
| Table | Primary key |
|---|---|
| `manager_dna_profiles` | `team_id` |
| all other tables | `id` |

**Usage:**
```R
supabase_delete_all("market_transactions")
```

---

### `supabase_reset_database(force = FALSE)`

Resets all tables to an empty state in child-to-parent order to respect foreign key constraints.

**Parameters:**
- `force` (logical, default `FALSE`): Must be `TRUE` to proceed.

**Reset Order (child to parent):**
1. `user_smart_alerts`
2. `decision_log`
3. `manager_dna_profiles`
4. `player_daily_snapshots`
5. `round_dream_team`
6. `market_transactions`
7. `player_history`
8. `user_team_history`
9. `user_teams`
10. `players`
11. `real_clubs`
12. `championships`

The system handles all 12 existing `required_tables` from the connector.

**Return:** A named list mapping table names to status strings.

**Usage:**
```R
result <- supabase_reset_database(force = TRUE)
```

---

### `init_supabase_db(verbose = FALSE)`

Verifies that all required Supabase tables are accessible on startup. Does not halt the application on failure.

**Parameters:**
- `verbose` (logical, default `FALSE`): If `TRUE`, prints per-table status to console.

**Return:** `TRUE` if all tables respond with a 2xx HTTP status, otherwise `FALSE`.

**Tables Verified (all 12 `required_tables` from the connector):**
- `championships`, `real_clubs`, `players`, `user_teams`, `user_team_history`, `player_history`, `market_transactions`, `round_dream_team`, `player_daily_snapshots`, `manager_dna_profiles`, `decision_log`, `user_smart_alerts`

Each probe selects the table's primary key via `supabase_primary_key()` (`team_id` for `manager_dna_profiles`, `id` for all other tables).

**Usage:**
```R
ok <- init_supabase_db(verbose = TRUE)
```

---

### `populate_entire_database(login, championship_id, verbose = TRUE)`

Executes a full database population in strict foreign-key order, syncing data from the Futmondo API into Supabase. Each step is wrapped defensively in `tryCatch()` so that a failure in one step does not prevent subsequent steps from running.

**Parameters:**
- `login` (list): A login object with at least `token` and `userid` fields, as returned by `login()` in `futmondo_functions.R`.
- `championship_id` (character): The championship ID to populate. If `NULL` or `""`, all championships are synced.
- `verbose` (logical, default `TRUE`): If `TRUE`, prints per-step progress and row counts to console.

**Population Order (parent to child):**
1. **Championships** -- `get_championships()` -> `sync_championship_to_supabase` (via `supabase_post`)
2. **Real Clubs** -- `get_real_clubs()` -> `sync_real_clubs_to_supabase`
3. **Players Catalog** -- `get_championship_players()` -> `sync_players_to_supabase`
4. **User Teams** -- `get_teams()` -> `sync_user_teams_to_supabase`
5. **Standings Snapshot** -- `get_teams()` -> `log_user_team_history`
6. **Player History** -- `get_championship_players()` -> `log_player_history`
7. **Pressroom Transactions** -- `get_championship_pressroom()` -> `sync_pressroom_transactions_to_supabase`

**Return:** A named list with one entry per step. Each entry is a list with:
- `status` (character): `"ok"` or `"error"`.
- `count` (integer, present on success): Number of rows processed.
- `message` (character, present on error): Error message from the failed step.

On a fatal outer error, an additional `fatal_error` entry is added.

**Usage:**
```R
result <- populate_entire_database(login = login_result, championship_id = "abc123", verbose = TRUE)
print(result)
```

**Error Handling:**
- Each individual step is independently wrapped in `tryCatch()`. A failure in one step does not abort the remaining steps.
- The entire function body is also wrapped in an outer `tryCatch()` to catch catastrophic failures (e.g., missing API credentials).

---

## Standalone Scripts

### `scripts/reset_db.R`

Standalone script to reset all Supabase tables.

**Usage:**
```bash
Rscript scripts/reset_db.R --force
```

Without `--force`, the script prompts for confirmation in interactive mode and exits with an error in non-interactive mode.

**Behavior:**
1. Loads `.Renviron` if present.
2. Sources `supabase_connector.R`.
3. Calls `supabase_reset_database(force = TRUE)`.
4. Prints a summary table of reset status per table.

---

### `scripts/init_db.R`

Standalone script to verify the Supabase database schema on startup.

**Usage:**
```bash
Rscript scripts/init_db.R
```

**Behavior:**
1. Loads `.Renviron` if present.
2. Sources `supabase_connector.R`.
3. Calls `init_supabase_db(verbose = TRUE)`.
4. Displays table status list and instructions if any tables are missing.

---

### `scripts/populate_db.R`

Standalone script to populate all Supabase tables from the Futmondo API.

**Usage:**
```bash
Rscript scripts/populate_db.R
```

**Behavior:**
1. Loads `.Renviron` if present (requires `user_name` and `password`).
2. Sources `futmondo_functions.R` and `supabase_connector.R`.
3. Logs in via `login()` using `.Renviron` credentials.
4. Retrieves the active championship ID via `get_championships()`.
5. Calls `populate_entire_database(login, championship_id, verbose = TRUE)`.
6. Fetches and displays row counts for all 12 tables via `get_table_row_counts()`.

---

## `scripts/schema.sql`

Canonical DDL for all 12 tables plus high-performance indices. It creates a fresh schema only: PostgreSQL `CREATE TABLE IF NOT EXISTS` does not add columns to tables that already exist. For an existing installation, see the "Existing-install upgrade" section in `docs/database_schema.md`.

## Integration with `global.R`

`init_supabase_db(verbose = FALSE)` is called in `global.R` after sourcing `supabase_connector.R` and loading `.Renviron`. This ensures the database schema is verified at application startup without blocking the user experience.