# Every page owns one information panel, including pages without a former title.
fm_information_tab <- function(tabName, ...) {
  pages <- fm_navigation_pages(TRUE)
  titles <- c(login = "Home", setNames(pages$label, pages$tab))
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
    shiny::includeCSS("www/home.css"),
    shiny::includeScript("www/home.js"),
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
    fm_information_tab(
      tabName = "login",
      login_UI("login"),
      tags$section(class = "fm-home", `aria-label` = "League home",
        tags$div(class = "fm-home-top",
          tags$div(class = "fm-home-heading", tags$h1(tabindex = "-1", "Home"),
            textOutput("home_team", inline = TRUE)),
          selectInput("selected_league", "League", choices = character())),
        uiOutput("league_preference_status"),
        tags$section(class = "fm-home-overview", `aria-label` = "League overview",
          tags$div(tags$span(class = "fm-home-caption", icon("calendar-days"), "Round schedule"),
            tags$div(id = "home_round", class = "shiny-html-output fm-summary-output", "Loading…")),
          tags$div(tags$span(class = "fm-home-caption", icon("wallet"), "Balance"),
            tags$div(id = "home_balance", class = "shiny-html-output fm-summary-output", "Loading…")),
          tags$div(tags$span(class = "fm-home-caption", icon("users"), "Squad size"),
            tags$div(id = "home_squad", class = "shiny-html-output fm-summary-output", "Loading…"))),
        uiOutput("home_continue"),
        uiOutput("home_destinations"),
        tags$details(class = "fm-home-settings", tags$summary(icon("gear"), "Account settings"),
          uiOutput("timezone_settings"), uiOutput("worker_settings"), login_account_UI("login"))
      )
    ),
    fm_information_tab(tabName="notifications", notifications_UI("notifications")),
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
  tags$div(class = "fm-page-actions", role = "group", `aria-label` = "Page actions",
    tags$div(id = "league-refresh-status", role = "status", `aria-atomic` = "true"),
    tags$button(type = "button", class = "btn fm-home-return", `data-fm-page` = "login",
      icon("house"), "Home"),
    actionButton("refresh_all", "Refresh", icon = icon("rotate"),
      title = "Refresh league balances and players",
      `aria-label` = "Refresh league balances and players")
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
