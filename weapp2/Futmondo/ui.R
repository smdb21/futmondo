# Every page owns one information panel, including pages without a former title.
fm_information_tab <- function(tabName, ...) {
  titles<-c(notifications="Notifications",login="Welcome",today="Today's market",
    lineup="Lineup",yourteam="My Team",players_in_championship="Players",
    rivals="Rivals",plots="Plots",classification="Classification",
    round_mvps="Round awards",admin="Administration")
  shinydashboard::tabItem(tabName=tabName,fm_info_page_UI(unname(titles[[tabName]]),...))
}

fm_theme_control <- tags$div(class = "fm-theme-control",
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
        `aria-label` = "Dark", title = "Dark", `aria-pressed` = "false", icon("moon"))))

header <- shinydashboardPlus::dashboardHeader(
  title = tags$span(class = "fm-header-brand",
    actionLink("open_welcome", "LigaRadar", class = "fm-header-title",
      `aria-label` = "LigaRadar — Welcome"),
    uiOutput("notification_bell",inline=TRUE))
)
body <- shinydashboard::dashboardBody(
  # Add custom CSS here
  shiny::tags$head(
    shiny::tags$link(rel = "icon", type = "image/x-icon", sizes = "16x16 32x32 48x48",
      href = "favicon.ico?v=20261123"),
    shiny::tags$link(rel = "icon", type = "image/svg+xml", sizes = "any",
      href = "favicon.svg?v=20261123"),
    shiny::tags$style(shiny::HTML(fm_theme_css())),
    # Embed the current stylesheet: cached legacy CSS must not restore white surfaces.
    shiny::includeCSS("www/custom_style.css"),
    shiny::includeScript("www/theme-switch.js"),
    shiny::includeCSS("www/loading-visual.css"),
    shiny::includeScript("www/loading-visual.js"),
    shiny::includeCSS("www/disconnect.css"),
    shiny::includeScript("www/disconnect.js"),
    shiny::includeScript("www/worker-state.js"),
    shiny::includeScript("www/info-disclosures.js"),
    shiny::tags$style(shiny::HTML("
        .ReactTable .rt-thead {
          z-index: 1;
        }
      "))
  ),
  tags$dialog(id = "fm-disconnect", `aria-labelledby` = "fm-disconnect-title",
    `aria-describedby` = "fm-disconnect-description",
    tags$div(class = "fm-disconnect-visual", `aria-hidden` = "true", fm_pitch_radar(sweep = TRUE)),
    tags$h2(id = "fm-disconnect-title", "Connection lost"),
    tags$p(id = "fm-disconnect-description", "Refresh the page to reconnect to LigaRadar."),
    tags$button(type = "button", id = "fm-disconnect-refresh", autofocus = "autofocus",
      "Refresh page")
  ),
  # The server owns page visibility; keep account finances above page notices.
  uiOutput("account_finance_summary",class="fm-summary-output"),
  uiOutput("startup_status"),
  uiOutput("page_preference_status"),
  uiOutput("timezone_preference_status"),
  # Saved-access feedback must remain visible after navigation away from Welcome.
  uiOutput(shiny::NS("login", "remember_feedback")),
  uiOutput("background_sync"),
  uiOutput("worker_status"),
  uiOutput("clause_results"),
  shinydashboard::tabItems(
    fm_information_tab(tabName="notifications", notifications_UI("notifications")),
    fm_information_tab(
      tabName = "login",
      selectInput("selected_league", "League", choices = character()),
      uiOutput("league_preference_status"),
      uiOutput("timezone_settings"),
      uiOutput("worker_settings"),
      fluidRow(
        column(
          width = 12,
          login_UI(id = "login")
        )
      )
    ),
    fm_information_tab(
      tabName = "today",
      today_UI(id = "today")
    ),
    fm_information_tab(tabName = "lineup", lineup_UI("lineup")),
    fm_information_tab(
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
    fm_information_tab(
      tabName = "players_in_championship",
      players_in_championship_UI(id = "players_in_championship")
    ),
    fm_information_tab(
      tabName = "rivals",
      rivals_UI(id = "rivals")
    ),
    fm_information_tab(
      tabName = "plots",
      plots_UI(id = "plots")
    ),
    fm_information_tab(
      tabName = "classification",
      classification_UI(id = "classification")
    ),
    fm_information_tab(
      tabName = "round_mvps",
      round_mvps_UI(id = "round_mvps")
    ),
    fm_information_tab(
      tabName = "admin",
      admin_UI(id = "admin")
    )
  ),
  # Floating action button for the selected league
  actionButton(
    inputId = "refresh_all",
    label = icon("rotate"),
    class = "btn-refresh-floating",
    title = "Refresh league balances and players",
    `aria-label` = "Refresh league balances and players"
  )
)

shinydashboardPlus::dashboardPage(
  title = "LigaRadar",
  preloader = list(html = tags$div(class = "fm-loading-launch",
    fm_loading_visual("fm-launch-visual"),
    tags$div(class = "fm-loading-brand", "LigaRadar"),
    tags$p(class = "fm-loading-caption", role = "status", `aria-live` = "polite", "Loading…")),
    color = fm_theme_tokens()$bg),
  header,
  shinydashboardPlus::dashboardSidebar(
    width = 180, # pixels; keeps navigation labels readable without changing mobile navigation
    shinydashboard::sidebarMenuOutput(outputId = "menu"),
    fm_theme_control
  ),
  body
)
