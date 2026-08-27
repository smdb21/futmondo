# Admin Module Documentation

This document describes the `Admin_Module.R` Shiny module, which provides database telemetry, schema verification, and a controlled database reset workflow for authorized administrators.

---

## 1. Overview

The Admin Module provides two exported functions:

* `admin_UI(id)` -- Renders the admin dashboard with telemetry KPIs, table statistics, and database operations.
* `admin_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV)` -- Drives reactive row-count loading, schema verification, and the database reset confirmation modal.

Additionally, two helper functions are defined in the same file:

* `get_table_definitions()` -- Returns a static data frame describing all 8 database tables.
* `get_table_row_counts()` -- (Defined in `supabase_connector.R`) Queries live row counts for every table via the Supabase REST API.

---

## 2. Functional Parameters

### `admin_UI(id)`

| Parameter | Type   | Description                                          |
|-----------|--------|------------------------------------------------------|
| `id`      | string | The Shiny module namespace ID (e.g., `"admin"`).    |

**Return type:** `tagList` -- A Shiny UI tag list containing fluid rows, boxes, KPI outputs, a reactable table, and action buttons.

**UI layout:**

1. **Telemetry KPI row** (4 columns, width 3 each):
   - Connection Status (static indicator for Supabase API endpoint)
   - Admin Email (static indicator showing the authorized operator from `.Renviron`)
   - Total Tables (static count of 8 schema-verified tables)
   - Total DB Records (dynamic grand total, refreshable)

2. **Per-table KPI row** (collapsible box, 8 columns in 2 fluid rows):
    - Championships, Real Clubs, Players, User Teams (first row)
    - Team History, Player History, Market Transactions, Round Dream Team (second row)
    - Each cell displays a reactive `textOutput` bound to `kpi_<table_name>`.

3. **Main content row** (8 + 4 column split):
    - Left (width 8): `reactableOutput` named `tables_stats_table` showing merged table definitions with live row counts, primary keys, descriptions, and live status. A `btn_refresh_stats` action button sits below the table.
    - Right (width 4): `btn_populate_db` action button for full database population from the Futmondo API, `btn_verify_db` for schema verification, `btn_sync_dreamteams` for syncing round dream teams and MVP accolades, and a "Danger Zone" section containing `btn_reset_db` for the database reset.

### `admin_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV)`

| Parameter         | Type     | Description                                                        |
|-------------------|----------|--------------------------------------------------------------------|
| `id`              | string   | The Shiny module namespace ID (e.g., `"admin"`).                  |
| `is_module_active`| reactive | Returns `TRUE` when the admin tab is selected, `FALSE` otherwise. |
| `login_token`     | reactive | The current login token (named vector with `user_name`, etc.).    |
| `championship_id` | reactive | The current championship ID from the API.                          |
| `user_team_id`    | reactive | The logged-in user's team ID from the API.                         |
| `user_teams_RV`   | reactive | The full user teams data frame from the API.                       |

**Return type:** `NULL` -- Standard Shiny module server; side effects are reactive outputs and event observers.

**Reactive behavior:**

- `row_counts_df` (reactiveVal): Holds the current row-count data frame. Populated on module activation and on manual refresh.
- `load_row_counts()`: Internal helper that calls `get_table_row_counts()` wrapped in `tryCatch`. Updates `row_counts_df`.
- `get_count(tbl_name)`: Internal helper that looks up a single table's count from `row_counts_df()`, returning a comma-formatted string or `"N/A"`.

**Rendered outputs:**

- `kpi_championships`, `kpi_real_clubs`, `kpi_players`, `kpi_user_teams`, `kpi_user_team_history`, `kpi_player_history`, `kpi_market_transactions`, `kpi_round_dream_team`: Each is a `renderText` bound to `get_count()` for the respective table.
- `kpi_total_records`: A `renderText` summing all row counts across tables.
- `tables_stats_table`: A `renderReactable` merging `get_table_definitions()` with live row counts, adding a `live_status` column ("Active" or "Unknown").

**Event observers:**

- `input$btn_refresh_stats`: Calls `load_row_counts()` to refresh all counts.
- `input$btn_verify_db`: Calls `init_supabase_db(verbose = TRUE)` and shows a notification (message, warning, or error) based on the result.
- `input$btn_populate_db`: Checks that `login_token()` and `championship_id()` are available. Shows a progress notification, calls `populate_entire_database(login, championship_id, verbose = TRUE)`, shows a success notification, and calls `load_row_counts()` to refresh the dashboard counts immediately.
- `input$btn_sync_dreamteams`: Checks that `login_token()` and `championship_id()` are available. Shows a progress notification, calls `sync_all_championship_dreamteams(login, championship_id, verbose = TRUE)`, shows a summary notification with per-round sync counts, and calls `load_row_counts()` to refresh the dashboard counts.
- `input$btn_reset_db`: Opens a confirmation modal (see Section 5).
- `input$btn_confirm_reset`: Executes `supabase_reset_database(force = TRUE)`, shows a summary notification, removes the modal, and calls `load_row_counts()` to refresh counts to zero.

---

## 3. Helper Functions

### `get_table_definitions()`

| Parameter | Type | Description |
|-----------|------|-------------|
| (none)    |      | No parameters. |

**Return type:** `data.frame` with 8 rows and 3 columns.

| Column       | Type   | Description                                      |
|--------------|--------|--------------------------------------------------|
| `table_name` | char   | The Supabase table name.                         |
| `description`| char   | Human-readable description of the table's purpose.|
| `primary_key`| char   | The primary key column name and type (e.g., `"id (text)"`). |

**Payload shape (return value):**

```
  table_name          description                            primary_key
  championships      Active championships and league metadata  id (text)
  real_clubs         Real-world football clubs with logos      id (text)
  players            Full player catalog from the API          id (text)
  user_teams         User-managed teams within a championship  id (text)
  user_team_history  Historical snapshot of user team standings id (bigint)
  player_history     Historical snapshot of player valuations  id (bigint)
  market_transactions Market transfer and clause transactions  id (bigint)
  round_dream_team   Best 11 (Dream Team) and MVP per round   id (BIGSERIAL)
  player_daily_snapshots Daily valuation, stats, and ownership snapshots  id (BIGSERIAL)
  manager_dna_profiles   Computed manager trading-behavior profiles       team_id (text)
  decision_log           AI recommendation audit trail                    id (BIGSERIAL)
  user_smart_alerts      Personalized smart alerts                        id (BIGSERIAL)
```

### `get_table_row_counts()` (in `supabase_connector.R`)

| Parameter | Type | Description |
|-----------|------|-------------|
| (none)    |      | No parameters. Reads credentials from `.Renviron` via `get_sb_url()` and `get_sb_key()`. |

**Return type:** `data.frame` with 12 rows and 2 columns.

| Column       | Type    | Description                           |
|--------------|---------|---------------------------------------|
| `table_name` | char    | The Supabase table name.              |
| `row_count`  | integer | The exact row count from the API.     |

**Mechanism:** For each of the 12 tables, issues a `GET` request to `{sb_url}/rest/v1/{tbl}` with `Prefer: count=exact` and `Range: 0-0` headers. Extracts the total from the `Content-Range` response header (e.g., `"0-0/999"` yields `999`). If credentials are missing, returns an empty data frame with the correct column types and emits a warning.

**Error handling:** Each table query is wrapped in `tryCatch`. On failure, `row_count` is set to `NA` for that table.

### `populate_entire_database(login, championship_id, verbose)`

Defined in `supabase_connector.R`. Orchestrates a full sync of all 8 Supabase tables from the Futmondo API.

| Parameter         | Type   | Description                                                        |
|-------------------|--------|--------------------------------------------------------------------|
| `login`           | named vector | The login token returned by `login()` (contains `token`, `userid`, `user_name`, etc.). |
| `championship_id` | char   | The active championship ID to scope the sync against.              |
| `verbose`         | logical | If `TRUE` (default), prints per-step progress messages to stdout. |

**Return type:** `list` -- A named list with one entry per table. Each entry is itself a list with `status` (`"ok"` or `"error"`) and either `count` (number of records synced) or `message` (error string on failure).

**Return shape:**

```
$championships        list(status = "ok", count = 1L)
$real_clubs           list(status = "ok", count = 120L)
$players              list(status = "ok", count = 5432L)
$user_teams           list(status = "ok", count = 999L)
$user_team_history    list(status = "ok", count = 999L)
$player_history       list(status = "ok", count = 5432L)
$market_transactions  list(status = "ok", count = 8421L)
$round_dream_team     list(status = "ok", total_rounds = 10L, total_players = 110L)
```

On a fatal top-level error, an additional element `$fatal_error` is appended with `list(status = "error", message = "...")`.

**Sync sequence (8 steps):**

1. **Championships** -- Calls `get_championships()`, parses the flattened vector, upserts via `supabase_post("championships", ...)`. If `championship_id` is provided, only that championship is synced.
2. **Real Clubs** -- Calls `get_real_clubs()`, passes the result to `sync_real_clubs_to_supabase()`.
3. **Players** -- Calls `get_championship_players()`, passes the result to `sync_players_to_supabase()`.
4. **User Teams** -- Calls `get_teams()`, passes the result to `sync_user_teams_to_supabase(...)`.
5. **User Team History** -- Calls `get_teams()` again, passes the result to `log_user_team_history()` to snapshot current standings.
6. **Player History** -- Calls `get_championship_players()` again, passes the result to `log_player_history(...)` to snapshot current valuations.
7. **Market Transactions** -- Calls `get_championship_pressroom()`, passes the result to `sync_pressroom_transactions_to_supabase(...)`.
8. **Round Dream Teams** -- Calls `sync_all_championship_dreamteams(login, championship_id, verbose)` which iterates over all finished matchdays and syncs each round's Best 11 and MVP player.

Each step is independently wrapped in `tryCatch`. A failure in one step does not abort the remaining steps.

**Error handling:** Per-step errors are caught and recorded in the return list. A top-level `tryCatch` wraps the entire function to catch fatal errors that prevent any step from running.

**Usage example:**

```R
login_result <- login(user_name = "user@example.com", password = "secret")
champ_id <- "12345"
result <- populate_entire_database(login_result, champ_id, verbose = TRUE)
print(result$players$status)
# [1] "ok"
print(result$players$count)
# [1] 5432
```

---

## 4. Admin Authentication Logic

The admin panel is gated by an environment variable, not by database credentials or API tokens.

**How it works (in `server.R`):**

1. `Sys.getenv("admin")` reads the `admin` variable from `.Renviron`.
2. The value is trimmed and compared case-insensitively against the currently logged-in user's email (`login_token_RV()[["user_name"]]`).
3. If they match, `is_admin` is set to `TRUE`, and the "Admin" menu item is appended to the sidebar menu via `renderMenu`.
4. If they do not match (or the `admin` env var is empty), `is_admin` remains `FALSE` and the Admin tab is never rendered in the menu.

**Configuration:**

Add the following line to your `.Renviron` file in the project root:

```
admin=your_email@example.com
```

Replace `your_email@example.com` with the exact email address used to log in to the Futmondo API. The comparison is case-insensitive, so `Admin=USER@Example.COM` works identically to `admin=user@example.com`.

---

## 5. Database Reset Confirmation Modal

The database reset workflow uses a two-step confirmation to prevent accidental data loss.

### Step 1: Trigger the modal

Clicking `btn_reset_db` fires `input$btn_reset_db`, which calls `showModal()` with a `modalDialog` containing:

- **Title:** Warning icon + "Confirm Database Reset"
- **Body:**
  - Plain-text explanation that ALL records across every table will be permanently deleted.
  - Bold red warning: "This action cannot be undone."
  - A bordered box listing the 7 affected tables by name.
- **Footer:**
  - `modalButton("Cancel")` -- dismisses the modal.
  - `actionButton("btn_confirm_reset", "Yes, Reset Everything")` -- the only path to execution.

### Step 2: Execute the reset

Clicking `btn_confirm_reset` fires `input$btn_confirm_reset`, which:

1. Calls `supabase_reset_database(force = TRUE)` wrapped in `tryCatch`.
2. On success:
   - Builds a summary string listing each table and its reset result.
   - Shows a `showNotification` with `type = "message"` and `duration = 12`.
   - Calls `removeModal()` to dismiss the dialog.
   - Calls `load_row_counts()` to refresh the KPI displays (all counts should read 0).
3. On error:
   - Shows a `showNotification` with `type = "error"` and `duration = 10`.
   - Calls `removeModal()` to dismiss the dialog.

---

## 6. Database Population Button (`btn_populate_db`)

The `btn_populate_db` action button, rendered in the right-hand column of the Admin Dashboard UI, triggers a full database population from the Futmondo API.

### UI location

Inside `admin_UI(id)`, the button is placed in the right-side column (width 4) of the main content row, above the schema verification button and the Danger Zone section:

```R
actionButton(ns("btn_populate_db"), "Populate Entire Database", class = "btn-warning")
```

### Server behavior

The observer on `input$btn_populate_db` (inside `admin_Server`) performs the following steps:

1. **Validation** -- Checks that `login_token()` and `championship_id()` are available and non-empty. If either is missing, it shows an error notification and aborts.
2. **Progress notification** -- Calls `showNotification("Populating database...", type = "message", duration = NULL)` to display a persistent progress indicator.
3. **Execution** -- Calls `populate_entire_database(login, championship_id, verbose = TRUE)` wrapped in `tryCatch`.
4. **Success path:**
   - Shows a `showNotification` with `type = "message"` and `duration = 10` confirming completion.
   - Calls `load_row_counts()` to refresh the telemetry KPIs and the stats table immediately, so the user sees updated counts without needing to click "Refresh".
5. **Error path:**
   - Shows a `showNotification` with `type = "error"` and `duration = 10` containing the error message.

### Code usage example

The button is fully wired inside the module; no external code is needed to invoke it. To trigger the equivalent logic programmatically from outside the module:

```R
login <- login_token_RV()
champ_id <- championship_id_RV()
req(login, champ_id)
result <- populate_entire_database(
  login = login,
  championship_id = champ_id,
  verbose = TRUE
)
```

---

## 7. Sync Round Dream Teams Button (`btn_sync_dreamteams`)

The `btn_sync_dreamteams` action button, rendered in the right-hand column of the Admin Dashboard UI, triggers a full synchronization of Best 11 (Dream Team) and MVP player accolades for all finished matchdays.

### UI location

Inside `admin_UI(id)`, the button is placed in the right-side column (width 4) of the main content row, between the schema verification button and the Danger Zone section:

```R
actionButton(
  inputId = ns("btn_sync_dreamteams"),
  label = "Sync Round Dream Teams",
  icon = icon("trophy"),
  class = "btn-info"
)
```

A helper text below reads: "Verifies and syncs the Best 11 (Dream Team) and MVP accolades for all finished matchdays, reconciling delayed matches."

### Server behavior

The observer on `input$btn_sync_dreamteams` (inside `admin_Server`) performs the following steps:

1. **Validation** -- Checks that `login_token()` and `championship_id()` are available and non-empty. If either is missing, it shows an error notification and aborts.
2. **Progress notification** -- Calls `showNotification("Syncing round dream teams and MVP accolades...", type = "message", duration = 10)` to display a persistent progress indicator.
3. **Execution** -- Calls `sync_all_championship_dreamteams(login, championship_id, verbose = TRUE)` wrapped in `tryCatch`.
4. **Success path:**
   - Builds a summary string listing per-round sync counts.
   - Shows a `showNotification` with `type = "message"` and `duration = 8` confirming completion.
   - Calls `load_row_counts()` to refresh the telemetry KPIs and the stats table immediately, so the user sees updated counts without needing to click "Refresh".
5. **Error path:**
   - Shows a `showNotification` with `type = "error"` and `duration = 10` containing the error message.

### Code usage example

The button is fully wired inside the module; no external code is needed to invoke it. To trigger the equivalent logic programmatically from outside the module:

```R
login <- login_token_RV()
champ_id <- championship_id_RV()
req(login, champ_id)
result <- sync_all_championship_dreamteams(
  login = login,
  championship_id = champ_id,
  verbose = TRUE
)
```

---

## 8. Dream Team Sync Functions

### `sync_round_dreamteam_to_supabase(login, championship_id, round_id, round_number)`

Defined in `supabase_connector.R`. Fetches the Best 11 (Dream Team) and MVP player for a single finished matchday and upserts the records into the `round_dream_team` Supabase table.

**Parameters:**

| Parameter         | Type   | Description                                                        |
|-------------------|--------|--------------------------------------------------------------------|
| `login`           | named vector | The login token returned by `login()` (contains `token`, `userid`, etc.). |
| `championship_id` | char   | The active championship ID to scope the sync against.              |
| `round_id`        | char   | The unique round identifier from the API.                          |
| `round_number`    | numeric | The sequential matchday number (e.g., 1, 2, 3, ...).              |

**Return type:** `integer` -- The number of players synced for the round (typically 11). Returns `0L` on error or if no valid dream team data is available.

**Mechanism:**

1. Calls `get_round_dreamteam(login, championship_id, round_id)` to fetch the API response.
2. Validates that the response contains both `players` and `mvp` keys.
3. Iterates over `ans$players`, constructing a data frame with columns: `championship_id`, `round_id`, `round_number`, `player_id`, `player_name`, `player_role`, `points`, `is_mvp` (boolean, `TRUE` if `player_id == mvp_id`), `is_finished` (always `TRUE`).
4. Upserts the data frame into `round_dream_team` via `supabase_post("round_dream_team", dreamteam_df)`.
5. Returns the row count.

**Delayed match handling:** The function only syncs rounds that have already been identified as finished by `get_finished_rounds()` (see below). It does not attempt to fetch dream teams for in-progress rounds, so delayed matches that have not yet crossed their `beginProcess` timestamp are naturally skipped.

**Error handling:** The entire function body is wrapped in `tryCatch`. On error, prints an error message and returns `0L`.

**Usage example:**

```R
count <- sync_round_dreamteam_to_supabase(login_result, "12345", "round_abc", 3)
# [DreamTeam] Synced 11 players for round 3.
# [1] 11
```

### `sync_all_championship_dreamteams(login, championship_id, verbose)`

Defined in `supabase_connector.R`. Orchestrates the dream team sync for all finished matchdays in a championship.

**Parameters:**

| Parameter         | Type   | Description                                                        |
|-------------------|--------|--------------------------------------------------------------------|
| `login`           | named vector | The login token returned by `login()`.                             |
| `championship_id` | char   | The active championship ID to scope the sync against.              |
| `verbose`         | logical | If `TRUE` (default), prints per-step progress messages to stdout. |

**Return type:** `list` -- A named list with sync results.

**Return shape (success):**

```
$status           "ok"
$total_rounds     10
$total_players    110
$per_round        list("1" = 11, "2" = 11, ..., "10" = 11)
```

**Return shape (skipped -- no login or championship):**

```
$status           "skipped"
$total_rounds     0
$total_players    0
```

**Return shape (error):**

```
$status           "error"
$message          "Error description"
$total_rounds     0
$total_players    0
```

**Mechanism:**

1. Calls `get_finished_rounds(login, championship_id)` to retrieve all rounds and their completion status.
2. Filters to only `is_finished == TRUE` rounds. A round is considered finished if:
   - Its `beginProcess` timestamp is in the past (`begin_time < Sys.time()`), OR
   - Its `status` field equals `"done"` (case-insensitive).
3. Iterates over each finished round, calling `sync_round_dreamteam_to_supabase()` for each.
4. Accumulates per-round counts into `per_round` and totals into `total_players`.
5. Returns the aggregated result list.

**Delayed match handling:** The `get_finished_rounds()` function determines which rounds are eligible for dream team extraction. A round whose `beginProcess` timestamp has not yet passed is marked `is_finished = FALSE` and will be skipped. This means that if a matchday has delayed matches (e.g., some fixtures were postponed), the dream team for that round will only be synced once the `beginProcess` deadline has passed and the API considers the round finished. Running `sync_all_championship_dreamteams` again after the delay will pick up the previously skipped round.

**Error handling:** Per-round errors are caught inside `sync_round_dreamteam_to_supabase` and yield `0L` for that round. A top-level `tryCatch` wraps the entire function to catch fatal errors (e.g., network failure) and returns an error-shaped list.

**Usage example:**

```R
login_result <- login(user_name = "user@example.com", password = "secret")
champ_id <- "12345"
result <- sync_all_championship_dreamteams(login_result, champ_id, verbose = TRUE)
# [DreamTeam] Syncing dream teams for 10 finished round(s).
# [DreamTeam] Round 1... [DreamTeam] Synced 11 players for round 1.
# ...
# [DreamTeam] Complete. Total players synced: 110
print(result$status)
# [1] "ok"
print(result$total_players)
# [1] 110
```

---

## 9. Code Usage Examples

### Using the module in `server.R`

```R
admin_Server(
  id = "admin",
  is_module_active = reactive({
    req(input$tabs); input$tabs == "admin"
  }),
  login_token = login_token_RV,
  championship_id = championship_id_RV,
  user_team_id = user_team_id_RV,
  user_teams_RV = user_teams_RV
)
```

### Using the module in `ui.R`

```R
tabPanel(
  title = "Admin",
  tabName = "admin",
  admin_UI("admin")
)
```

### Standalone row-count query (outside the module)

```R
counts <- get_table_row_counts()
print(counts)
#   table_name          row_count
# 1 championships           1
# 2 real_clubs             120
# 3 players               5432
# ...
```

### Standalone table definitions

```R
defs <- get_table_definitions()
print(defs$table_name)
# [1] "championships"       "real_clubs"          "players"
# [4] "user_teams"          "user_team_history"   "player_history"
# [7] "market_transactions" "round_dream_team"
```

---

## 10. Dynamic Menu Rendering (in `server.R`)

The sidebar menu is generated reactively via `output$menu <- renderMenu({...})`. The logic is:

```R
is_admin <- FALSE
admin_env <- trimws(Sys.getenv("admin"))
if (admin_env != "" && !is.null(login_token_RV()) && length(login_token_RV()) >= 3) {
  current_user <- trimws(as.character(login_token_RV()[["user_name"]]))
  is_admin <- tolower(current_user) == tolower(admin_env)
}
```

The standard menu items (Login, Your team, Market, Players, Rivals, Classification) are always rendered. The Admin menu item (`menuItem("Admin", tabName = "admin", icon = icon("gears"))`) is appended only when `is_admin` is `TRUE`.

The tabset panel uses `id = "tabs"`, and `updateTabsetPanel` is called on successful login to switch the default tab to `"yourteam"`.