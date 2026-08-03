# Futmondo Insights - R Shiny Application

An interactive R Shiny Dashboard built to provide powerful real-time insights, valuations, and market trends for players participating in the online fantasy football game **Futmondo**. 

This application connects securely to Futmondo's private mobile/web API, flattens the complex nested JSON payloads, and visualizes market changes, player points, and squad standings.

## Features

- **Secure Login**: Session-based login using email and password to retrieve stateless access tokens.
- **Your Team**: Displays active squad standings, total squad value, 24-hour squad value changes, and individual player performance logs.
- **Transfer Market**: Lists all currently available transfer market players, listing expiration times, real-world team context, and bid histories.
- **Championship Players**: Complete roster search for all registered players in the fantasy championship league, complete with deep filters.
- **Advanced Filtering**: Filters players by position, real-world club, valuation, market changes, active release clauses, or favorites.

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
```

### Running the Application

To run the application, execute:
```R
shiny::runApp()
```

## Architecture

- `global.R`: Sources all modules, initializes environments, and loads hidden column filters.
- `server.R` / `ui.R`: Handles application navigation and reactive state bindings.
- `futmondo_functions.R`: Houses core wrapper functions for communicating with the Futmondo API.
- `Modules/`:
  - `Login_Module.R`: Authentication form and reactive session storage.
  - `Players_Table_Module.R`: Reusable reactable components with sidebar filtering.
  - `Players_in_Teams_Module.R`: Logged-in user's squad performance and championship positions.
  - `Market_Module.R`: Active transfer list and bidding statuses.