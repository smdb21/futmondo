# Admin Module Documentation

This document describes the `Admin_Module.R` Shiny module, which provides database telemetry, schema verification, and a controlled database reset workflow for authorized administrators.

---

## 1. Overview

The Admin Module provides two exported functions:

* `admin_UI(id)` -- Renders the admin dashboard with telemetry KPIs, table statistics, and database operations.
* `admin_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV)` -- Drives reactive row-count loading, schema verification, and the database reset confirmation modal.

Additionally, two helper functions are defined in the same file:

* `get_table_definitions()` -- Returns a static data frame describing all 7 database tables.
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
   - Total Tables (static count of 7 schema-verified tables)
   - Total DB Records (dynamic grand total, refreshable)

2. **Per-table KPI row** (collapsible box, 8 columns in 2 fluid rows):
   - Championships, Real Clubs, Players, User Teams (first row)
   - Team History, Player History, Market Transactions, Grand Total (second row)
   - Each cell displays a reactive `textOutput` bound to `kpi_<table_name>`.

3. **Main content row** (8 + 4 column split):
   - Left (width 8): `reactableOutput` named `tables_stats_table` showing merged table definitions with live row counts, primary keys, descriptions, and live status. A `btn_refresh_stats` action button sits below the table.
   - Right (width 4): `btn_verify_db` action button for schema verification, and a "Danger Zone" section containing `btn_reset_db` for the database reset.

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

- `kpi_championships`, `kpi_real_clubs`, `kpi_players`, `kpi_user_teams`, `kpi_user_team_history`, `kpi_player_history`, `kpi_market_transactions`: Each is a `renderText` bound to `get_count()` for the respective table.
- `kpi_total_records`: A `renderText` summing all row counts across tables.
- `tables_stats_table`: A `renderReactable` merging `get_table_definitions()` with live row counts, adding a `live_status` column ("Active" or "Unknown").

**Event observers:**

- `input$btn_refresh_stats`: Calls `load_row_counts()` to refresh all counts.
- `input$btn_verify_db`: Calls `init_supabase_db(verbose = TRUE)` and shows a notification (message, warning, or error) based on the result.
- `input$btn_reset_db`: Opens a confirmation modal (see Section 5).
- `input$btn_confirm_reset`: Executes `supabase_reset_database(force = TRUE)`, shows a summary notification, removes the modal, and calls `load_row_counts()` to refresh counts to zero.

---

## 3. Helper Functions

### `get_table_definitions()`

| Parameter | Type | Description |
|-----------|------|-------------|
| (none)    |      | No parameters. |

**Return type:** `data.frame` with 7 rows and 3 columns.

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
```

### `get_table_row_counts()` (in `supabase_connector.R`)

| Parameter | Type | Description |
|-----------|------|-------------|
| (none)    |      | No parameters. Reads credentials from `.Renviron` via `get_sb_url()` and `get_sb_key()`. |

**Return type:** `data.frame` with 7 rows and 2 columns.

| Column       | Type    | Description                           |
|--------------|---------|---------------------------------------|
| `table_name` | char    | The Supabase table name.              |
| `row_count`  | integer | The exact row count from the API.     |

**Mechanism:** For each of the 7 tables, issues a `GET` request to `{sb_url}/rest/v1/{tbl}` with `Prefer: count=exact` and `Range: 0-0` headers. Extracts the total from the `Content-Range` response header (e.g., `"0-0/999"` yields `999`). If credentials are missing, returns an empty data frame with the correct column types and emits a warning.

**Error handling:** Each table query is wrapped in `tryCatch`. On failure, `row_count` is set to `NA` for that table.

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

## 6. Code Usage Examples

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
# [7] "market_transactions"
```

---

## 7. Dynamic Menu Rendering (in `server.R`)

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