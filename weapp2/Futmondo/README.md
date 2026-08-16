# Futmondo Insights - R Shiny Application

## Overview

Futmondo Insights is an R Shiny application that provides a comprehensive dashboard for managing and analyzing player data in the Futmondo fantasy football platform. The application integrates with the Futmondo API to fetch real-time data about players, teams, and market transactions.

## Features

### Players In Teams Module

- **2x2 KPI Boxes**: Compact grid showing Classification Rank, Points, Total Volume Earned (initial 300M budget plus sales and bonuses), and Total Volume Spent (aggregate purchase costs).
- **"In Market" Status Badge**: Amber badge column in the player roster table displaying the formatted asking price for players currently listed on the marketplace.
- **Standings Evolution Plot**: Time-series line chart repositioned to the bottom of the tab, tracking team standings across matchdays with interactive tooltips.

### Rivals Module

- **League Buying Power Chart**: Horizontal bar chart with a mode selector (Liquid Cash, Squad Purchases, Transaction Volume) and a top date range slider for filtering by time window.
- **Player Buy/Sell Pivot Ledger**: Paired buy/sell rows for each player the rival acquired, with two-line hover tooltips (transaction date and counterparty), handling of re-bought players via numeric suffixes, and per-player Net P/L calculation.
- **Net Transfer Profit/Loss KPI Box**: Standalone summary box showing the rival's aggregate transfer profit or loss across all completed buy/sell pairs.
- **Squad Value Evolution Plot**: Time-series line chart repositioned to the bottom of the page, tracking squad valuations across matchdays with interactive tooltips.

### General

- **Resized Columns**: All Reactable tables support click-and-drag column resizing.
- **Shortened Cache Timeouts**: Roster and bid caches reduced to 15-30 seconds for near-real-time offer visibility.
- **Defensive Fallback Mode**: Rival transaction history falls back to pressroom feed reconstruction and roster-based synthesis when the private API is restricted.

## Architecture

The application follows a modular architecture with the following key components:

- `app.R`: Main application entry point
- `Modules/`: Contains modular Shiny components for different features
- `Utils/`: Utility functions for data processing, API integration, and formatting
- `www/`: Static assets including custom CSS styles

## Key Modules

### Rivals Module (`Modules/Rivals_Module.R`)
Displays information about rival teams and their players. Provides comparative analysis between your team and competitors.

### Selected Player Module (`Modules/Selected_Player_Module.R`)
Detailed view of a selected player including:
- Player statistics and performance metrics
- Historical data visualization
- Market information and valuation trends
- Action buttons for market interactions (bidding, offers, clause buyouts)

### Additional Modules
- `Market_Module.R`: Transfer market functionality
- `My_Squad_Module.R`: User's squad management
- `Free_Agents_Module.R`: Available free agents
- `Press_Room_Module.R`: News and press updates
- `Player_Details_Module.R`: Comprehensive player information
- `Player_Ratings_Module.R`: Player ratings and performance analysis
- `Player_Ratings_History_Module.R`: Historical ratings data
- `Player_Ratings_Comparison_Module.R`: Comparative player ratings
- `Player_Ratings_Dashboard_Module.R`: Dashboard for player ratings
- `Player_Ratings_Export_Module.R`: Export functionality for ratings data
- `Player_Ratings_Filter_Module.R`: Filtering options for player ratings
- `Player_Ratings_Search_Module.R`: Search functionality for player ratings
- `Player_Ratings_Settings_Module.R`: Settings for player ratings display
- `Player_Ratings_Share_Module.R`: Sharing options for player ratings
- `Player_Ratings_Team_Module.R`: Team-based player ratings
- `Player_Ratings_Week_Module.R`: Weekly player ratings
- `Player_Ratings_Year_Module.R`: Yearly player ratings trends

## Dependencies

### R Packages
- `shiny`: Web application framework
- `shinydashboard`: Dashboard layout components
- `shinyjs`: JavaScript integration for Shiny
- `plotly`: Interactive plotting
- `reactable`: Interactive tables
- `dplyr`: Data manipulation
- `tidyr`: Data tidying
- `readr`: Data import
- `httr`: HTTP tools for API communication
- `jsonlite`: JSON parsing
- `lubridate`: Date/time manipulation
- `stringr`: String manipulation
- `purrr`: Functional programming
- `magrittr`: Pipe operator
- `DescTools`: Statistical tools
- `shinyWidgets`: Enhanced UI widgets
- `shinyalert`: Alert dialogs
- `shinycssloaders`: Loading animations
- `shinybusy`: Busy indicators
- `shinydashboardPlus`: Enhanced dashboard components
- `shinydashboardPlus`: Enhanced dashboard components
- `shinydashboardPlus`: Enhanced dashboard components

### API Integration
The application connects to the Futmondo API using the following base URLs:
- Production: `https://api.futmondo.com`
- Sandbox: `https://api-sandbox.futmondo.com`

## Configuration

### Environment Variables
- `FUTMONDO_API_KEY`: API key for authentication
- `FUTMONDO_ENV`: Environment (`production` or `sandbox`)
- `FUTMONDO_BASE_URL`: Base URL for API requests
- `FUTMONDO_PHOTO_URL`: Base URL for player photos

### Local Development
1. Install required R packages
2. Set environment variables in `.Renviron` file
3. Run the application using `shiny::runApp()`

## Deployment

### ShinyApps.io
1. Install the `rsconnect` package
2. Run `rsconnect::writeManifest()` to generate deployment manifest
3. Deploy using `rsconnect::deployApp()`

### RStudio Connect / Posit Connect
1. Install the `rsconnect` package
2. Run `rsconnect::writeManifest()` to generate deployment manifest
3. Deploy using `rsconnect::deployApp()`

## Customization

### CSS Styling
Custom styles are located in `www/custom_style.css`. The application uses Bootstrap for responsive design.

### Color Scheme
- Primary: `#f59e0b` (amber)
- Secondary: `#10b981` (green)
- Accent: `#3b82f6` (blue)
- Background: `#f8fafc` (light gray)

## Testing

### Unit Tests
Located in `tests/testthat/` directory. Run tests using:
```R
devtools::test()
```

### Integration Tests
Located in `tests/testthat/` directory with `_integration` suffix. Run tests using:
```R
devtools::test(filter = "integration")
```

## Contributing

1. Follow the coding standards outlined in `AGENTS.md`
2. Ensure all new features have corresponding documentation
3. Write tests for new functionality
4. Update the deployment manifest when adding dependencies

## License

This project is proprietary and confidential. All rights reserved.

## Contact

For questions or support, contact the development team at [support@futmondo.com](mailto:support@futmondo.com).