header <- shinydashboardPlus::dashboardHeader(
  title = "Futmondo",
  tags$li(class = "dropdown", uiOutput("notification_bell"))
)
body <- shinydashboard::dashboardBody(
  # Add custom CSS here
  shiny::tags$head(
    shiny::tags$style(shiny::HTML(fm_theme_css())),
    # Embed the current stylesheet: cached legacy CSS must not restore white surfaces.
    shiny::includeCSS("www/custom_style.css"),
    shiny::tags$style(shiny::HTML("
        .ReactTable .rt-thead {
          z-index: 1;
        }
      "))
  ),
  uiOutput("round_countdown"),
  selectInput("selected_league", "League", choices = character()),
  uiOutput("background_sync"),
  shinydashboard::tabItems(
    shinydashboard::tabItem(tabName="intelligence", intelligence_UI("intelligence")),
    shinydashboard::tabItem(tabName="notifications", notifications_UI("notifications")),
    shinydashboard::tabItem(tabName="automation", automation_UI("automation")),
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
      tabName = "today",
      today_UI(id = "today")
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
    ),
    shinydashboard::tabItem(
      tabName = "rivals",
      rivals_UI(id = "rivals")
    ),
    shinydashboard::tabItem(
      tabName = "classification",
      classification_UI(id = "classification")
    ),
    shinydashboard::tabItem(
      tabName = "admin",
      admin_UI(id = "admin")
    )
  ),
  # Floating Action Button for global cache refresh
  actionButton(
    inputId = "refresh_all",
    label = icon("rotate"),
    class = "btn-refresh-floating",
    title = "Clear Cache & Refresh Data"
  )
)

shinydashboardPlus::dashboardPage(
  preloader = list(html = tagList(waiter::spin_1(), "Loading ..."), color = fm_theme_tokens()$bg),
  header,
  shinydashboardPlus::dashboardSidebar(
    width = 135, # pixels
    shinydashboard::sidebarMenuOutput(outputId = "menu")
  ),
  body
)
