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

### Initialization and Startup Verification

On first launch, `global.R` calls `init_supabase_db()` to ensure the required tables and columns exist. The full initialization script lives in `scripts/init_db.R` and can be run standalone:

```bash
Rscript scripts/init_db.R
```

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
  - `schema.sql`: Authoritative SQL definition of all database tables and columns.