header <- shinydashboardPlus::dashboardHeader(
  title = "Futmondo"
)
body <- shinydashboard::dashboardBody(
  # Add custom CSS here
  tags$head(
    tags$style(HTML("
        .ReactTable .rt-thead {
          z-index: 1;
        }
      "))
  ),
  shinydashboard::tabItems(
    shinydashboard::tabItem(
      tabName = "login",
      fluidRow(
        column(
          width = 12,
          login_UI(id = "login")
        )
      )
    ),
    shinydashboard::tabItem(
      tabName = "yourteam",
      fluidRow(
        column(
          12,
          div(
            style = "overflow-x: auto;", # Allow horizontal scrolling
            players_in_teams_UI(id = "players_in_teams")
          )
        )
      )
    ),
    shinydashboard::tabItem(
      tabName = "market",
      market_UI(id = "market")
    ),
    shinydashboard::tabItem(
      tabName = "players_in_championship",
      players_in_championship_UI(id = "players_in_championship")
    )
  )
)

shinydashboardPlus::dashboardPage(
  preloader = list(html = tagList(waiter::spin_1(), "Loading ..."), color = "#3c8dbc"),
  header,
  shinydashboardPlus::dashboardSidebar(
    width = 135, # pixels
    shinydashboard::sidebarMenuOutput(outputId = "menu")
  ),
  body
)
