# Futmondo Insights - R Shiny Application

An interactive R Shiny Dashboard built to provide powerful real-time insights, valuations, and market trends for players participating in the online fantasy football game **Futmondo**. 

This application connects securely to Futmondo's private mobile/web API, flattens the complex nested JSON payloads, and visualizes market changes, player points, and squad standings.

## Features

- **Secure Login**: Session-based login using email and password to retrieve stateless access tokens.
- **Your Team**: Displays active squad standings, total squad value, 24-hour squad value changes, and individual player performance logs.
- **Transfer Market**: Lists all currently available transfer market players, listing expiration times, real-world team context, and bid histories.
- **Championship Players**: Complete roster search for all registered players in the fantasy championship league, complete with deep filters.
- **Advanced Filtering**: Filters players by position, real-world club, valuation, market changes, active release clauses, or favorites.
- **Admin Panel & Telemetry**: Gated behind an `admin` environment variable, the admin panel provides real-time database telemetry (per-table row counts, grand total), schema verification, and a two-step confirmation modal for full database resets.

## Getting Started

### Prerequisites

You must have **R** installed, along with the following library dependencies:
```R
install.packages(c("shiny", "shinydashboard", "shinydashboardPlus", "reactable", "httr", "jsonlite", "dplyr", "data.table", "scales", "waiter"))
```

### Configuration

Create a `.Renviron` file in the root directory to store your credentials:
```env
user_name=your_email@example.com
password=your_password
supabase_project_id=your_project_id
supabase_project_url=your_project_url
supabase_secret_key=your_secret_key
admin=your_admin_email@example.com
```

The `admin` variable is optional. When set, the logged-in user whose email matches this value (case-insensitive) will see an additional **Admin** tab in the sidebar menu, granting access to database telemetry and maintenance operations.

### Running the Application

To run the application, execute:
```R
shiny::runApp()
```

## Database

### Database Tables

The Supabase database contains 8 tables:

| Table                  | Description                                              | Primary Key       |
|------------------------|----------------------------------------------------------|-------------------|
| `championships`        | Active championships and league metadata                | `id (text)`       |
| `real_clubs`           | Real-world football clubs with logos                    | `id (text)`       |
| `players`              | Full player catalog from the API                        | `id (text)`       |
| `user_teams`           | User-managed teams within a championship                | `id (text)`       |
| `user_team_history`    | Historical snapshot of user team standings              | `id (bigint)`     |
| `player_history`       | Historical snapshot of player valuations                | `id (bigint)`     |
| `market_transactions`  | Market transfer and clause transactions                 | `id (bigint)`     |
| `round_dream_team`     | Best 11 (Dream Team) and MVP player accolades per round | `id (BIGSERIAL)`  |

### Initialization and Startup Verification

On first launch, `global.R` calls `init_supabase_db()` to ensure the required tables and columns exist. The full initialization script lives in `scripts/init_db.R` and can be run standalone:

```bash
Rscript scripts/init_db.R
```

### Database Population & Full Sync

To populate all 8 Supabase tables from the Futmondo API, use either of the following methods:

**Command line:**

```bash
Rscript scripts/populate_db.R
```

This reads credentials from `.Renviron`, logs in to the Futmondo API, retrieves the active championship ID, and sequentially syncs all tables (championships, real clubs, players, user teams, user team history, player history, market transactions, round dream teams). It prints per-table row counts and a grand total upon completion.

**Admin Dashboard (in-app):**

While logged in as the configured admin user, navigate to the **Admin** tab and click the **"Populate Entire Database"** button (`btn_populate_db`). The server calls `populate_entire_database()` with the current session token and championship ID, displays a progress notification, and automatically refreshes the telemetry counts once the operation completes.

**Sync Round Dream Teams (in-app):**

While logged in as the configured admin user, navigate to the **Admin** tab and click the **"Sync Round Dream Teams"** button (`btn_sync_dreamteams`). The server calls `sync_all_championship_dreamteams()` which iterates over all finished matchdays in the championship, fetches each round's Best 11 (Dream Team) and MVP player from the Futmondo API, and upserts the records into the `round_dream_team` Supabase table. Rounds with delayed matches (whose `beginProcess` timestamp has not yet passed) are automatically skipped and will be picked up on the next sync. The button displays a progress notification and refreshes the telemetry counts upon completion.

### Database Reset

To wipe all data and recreate the schema from scratch:

```bash
Rscript scripts/reset_db.R --force
```

This drops existing tables and re-applies the schema defined in `scripts/schema.sql`.

## Architecture

- `global.R`: Sources all modules, initializes environments, loads hidden column filters, and calls `init_supabase_db()` on startup.
- `server.R` / `ui.R`: Handles application navigation and reactive state bindings.
- `futmondo_functions.R`: Houses core wrapper functions for communicating with the Futmondo API.
- `supabase_connector.R`: Provides a thin wrapper around the Supabase HTTP API for all database reads and writes.
- `Modules/`:
  - `Login_Module.R`: Authentication form and reactive session storage.
  - `Players_Table_Module.R`: Reusable reactable components with sidebar filtering.
  - `Players_in_Teams_Module.R`: Logged-in user's squad performance and championship positions.
  - `Market_Module.R`: Active transfer list and bidding statuses.
  - `Admin_Module.R`: Database telemetry dashboard, schema verification, and controlled database reset workflow (admin-gated).
- `scripts/`:
  - `init_db.R`: Standalone script to initialize the Supabase database schema.
  - `reset_db.R`: Drops all tables and re-creates the schema from `schema.sql`.
  - `populate_db.R`: Populates all 8 tables from the Futmondo API (login, championship lookup, full sync, row-count summary).
  - `schema.sql`: Authoritative SQL definition of all database tables and columns.