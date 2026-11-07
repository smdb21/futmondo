header <- shinydashboardPlus::dashboardHeader(
  title = "Futmondo",
  tags$li(class = "dropdown fm-theme-control",
    tags$button(id = "fm-theme-toggle", type = "button", class = "btn fm-theme-button",
      `aria-label` = "Color theme: System", title = "Color theme: System",
      `aria-expanded` = "false", `aria-controls` = "fm-theme-options", icon("desktop")),
    tags$div(id = "fm-theme-options", class = "fm-theme-options", hidden = "hidden",
      role = "group", `aria-label` = "Theme options",
      tags$button(type = "button", class = "btn fm-theme-button", `data-theme` = "system",
        `aria-label` = "Follow system", title = "Follow system", `aria-pressed` = "true", icon("desktop")),
      tags$button(type = "button", class = "btn fm-theme-button", `data-theme` = "light",
        `aria-label` = "Light", title = "Light", `aria-pressed` = "false", icon("sun")),
      tags$button(type = "button", class = "btn fm-theme-button", `data-theme` = "dark",
        `aria-label` = "Dark", title = "Dark", `aria-pressed` = "false", icon("moon")))),
  tags$li(class = "dropdown", uiOutput("notification_bell"))
)
body <- shinydashboard::dashboardBody(
  # Add custom CSS here
  shiny::tags$head(
    shiny::tags$style(shiny::HTML(fm_theme_css())),
    # Embed the current stylesheet: cached legacy CSS must not restore white surfaces.
    shiny::includeCSS("www/custom_style.css"),
    shiny::includeScript("www/theme-switch.js"),
    shiny::tags$style(shiny::HTML("
        .ReactTable .rt-thead {
          z-index: 1;
        }
      "))
  ),
  uiOutput("startup_status"),
  uiOutput("page_preference_status"),
  # Saved-access feedback must remain visible after navigation away from Welcome.
  uiOutput(shiny::NS("login", "remember_feedback")),
  conditionalPanel(
    condition = "['today', 'players_in_championship', 'yourteam'].includes(input.tabs)",
    uiOutput("round_countdown")
  ),
  uiOutput("background_sync"),
  shinydashboard::tabItems(
    shinydashboard::tabItem(tabName="notifications", notifications_UI("notifications")),
    shinydashboard::tabItem(
      tabName = "login",
      selectInput("selected_league", "League", choices = character()),
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
    shinydashboard::tabItem(tabName = "lineup", lineup_UI("lineup")),
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
      tabName = "players_in_championship",
      players_in_championship_UI(id = "players_in_championship")
    ),
    shinydashboard::tabItem(
      tabName = "rivals",
      rivals_UI(id = "rivals")
    ),
    shinydashboard::tabItem(
      tabName = "plots",
      plots_UI(id = "plots")
    ),
    shinydashboard::tabItem(
      tabName = "classification",
      classification_UI(id = "classification")
    ),
    shinydashboard::tabItem(
      tabName = "round_mvps",
      round_mvps_UI(id = "round_mvps")
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
    width = 180, # pixels; keeps navigation labels readable without changing mobile navigation
    shinydashboard::sidebarMenuOutput(outputId = "menu")
  ),
  body
)
