# Database Reset and Initialization Functions

This document covers the database reset and initialization functions added to `supabase_connector.R` and the standalone scripts in `scripts/`.

## Functions in `supabase_connector.R`

### `supabase_delete(table_name, filter)`

Sends a DELETE request to the Supabase REST API for a given table.

**Parameters:**
- `table_name` (character): The name of the Supabase table to delete from.
- `filter` (character, default `"id=neq.00000000-0000-0000-0000-000000000000"`): A filter string in the format `"column=operator.value"`. For text PK tables, use a UUID-neq filter. For bigint PK tables, use `"id=gte.0"`.

**Return:** A named list with fields:
- `status` (character): One of `"deleted"`, `"error"`, or `"skipped"`.
- `http_code` (numeric, optional): The HTTP response code if the request was made.
- `reason` (character, optional): Error message or skip reason.

**Usage:**
```R
supabase_delete("championships", filter = "id=neq.00000000-0000-0000-0000-000000000000")
supabase_delete("user_team_history", filter = "id=gte.0")
```

---

### `supabase_delete_all(table_name)`

Deletes all rows from a table, selecting the appropriate filter based on the primary key type.

**Parameters:**
- `table_name` (character): The name of the Supabase table.

**Return:** A named list (same shape as `supabase_delete`).

**PK Type Mapping:**
| BigInt PK | Text PK |
|---|---|
| `user_team_history` | `championships` |
| `player_history` | `real_clubs` |
| `market_transactions` | `players` |
| | `user_teams` |

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
1. `market_transactions`
2. `player_history`
3. `user_team_history`
4. `user_teams`
5. `players`
6. `real_clubs`
7. `championships`

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

**Return:** `TRUE` if all tables respond with HTTP 200, otherwise `FALSE`.

**Tables Verified:**
- `championships`, `real_clubs`, `players`, `user_teams`, `user_team_history`, `player_history`, `market_transactions`

**Usage:**
```R
ok <- init_supabase_db(verbose = TRUE)
```

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

## `scripts/schema.sql`

Complete DDL for all seven tables plus five high-performance indices. Includes `is_active` in `user_teams` and `round_number` / `active_teams_count` in `user_team_history`. Execute via the Supabase SQL Editor to create or update the schema.

## Integration with `global.R`

`init_supabase_db(verbose = FALSE)` is called in `global.R` after sourcing `supabase_connector.R` and loading `.Renviron`. This ensures the database schema is verified at application startup without blocking the user experience.