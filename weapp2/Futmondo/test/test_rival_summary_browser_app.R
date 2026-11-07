#!/usr/bin/env Rscript
# Synthetic actual-module browser fixture; never logs in or calls a remote API.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source("global.R"))
for (method in c("GET", "POST", "PUT", "PATCH", "DELETE")) {
  suppressMessages(trace(method, where = asNamespace("httr"), print = FALSE,
    tracer = quote(stop("Unexpected HTTP in offline rival summary browser fixture"))))
}
M <- 1000000
fixture_now <- as.POSIXct("2026-11-06T12:00:00", tz = "UTC")
fixture_league <- "summary-browser-fixture"
fixture_teams <- data.frame(teamid = c("a", "b", "c", "d"),
  teamname = c("Alpha", "Beta", "Gamma", "Delta"), points = c(5, -2, 0, NA),
  teamValue = c(25 * M, 0, 0, NA_real_), stringsAsFactors = FALSE)
fixture_rules <- list(initial_budget = 300 * M, money_per_point = 0,
  ranking_pool = 0, ranking_mode = "top", users_to_rank = -1,
  dreamteam_reward = 0, mvp_reward = 0)
fixture_movement <- function(team, id, type, player, amount, day) {
  rival_finance_evidence("fixture-account", fixture_league, "unknown", "movements",
    paste(team, id, sep = ":"), list(id = id, team_id = team, type = type,
      concept = player, player_id = player, money = amount,
      date = sprintf("2026-10-%02dT10:00:00Z", day)), fixture_now)
}
fixture_dataset <- build_rival_cash_dataset(fixture_teams, fixture_league,
  evidence = list(
    fixture_movement("a", "a-buy-p", "buy", "p", -30 * M, 1),
    fixture_movement("a", "a-sell-p", "sell", "p", 40 * M, 2),
    fixture_movement("a", "a-buy-q", "buy", "q", -20 * M, 3),
    fixture_movement("a", "a-reward", "bonus", "", 10 * M, 4),
    fixture_movement("b", "b-buy", "buy", "r", -30 * M, 1),
    fixture_movement("b", "b-sell", "sell", "r", 20 * M, 2),
    fixture_movement("c", "c-buy", "buy", "s", -10 * M, 1),
    fixture_movement("c", "c-sell", "sell", "s", 10 * M, 2),
    fixture_movement("d", "d-buy", "buy", "t", NA_real_, 1),
    fixture_movement("d", "d-sell", "sell", "t", 10 * M, 2)),
  rules = fixture_rules, account_id = "fixture-account", coverage_complete = TRUE,
  now = fixture_now)
# Reproduce the screenshot's diagnostic shape using synthetic IDs only.
a <- match("a", fixture_dataset$team_finances$teamid)
fixture_dataset$team_finances$estimate_notes[a] <- paste(
  "A changed round is waiting for its historical lineup to refresh.",
  "Movement history completeness is unverified; secondary transfer feed was not added twice.",
  "Round 64a4f7efae633549bd0f038a player 65a14cffd9c62803f70f8509 : historical ownership (including roster-only recipients), eligibility or reward rule unavailable.")
fixture_dataset$team_finances$estimate_summary[a] <-
  "Historical award eligibility is incomplete."
fixture_dataset$team_finances$net_gain_estimated[a] <- TRUE

bind_factual_reads <- function(...) NULL
shared_mvp_source <- function(...) shiny::reactive(list(rows = data.frame(), complete = FALSE))
shared_rival_finance_source <- function(...) shiny::reactive(fixture_dataset)
get_user_team_info <- function(login, championship_id, user_team_id, ...) {
  i <- match(user_team_id, fixture_teams$teamid)
  list(teamValue = fixture_teams$teamValue[i], position = i)
}
get_players_from_team <- function(...) data.frame()
get_financial_snapshot <- function(...) list(configuration = list(budget = 300 * M))
get_championship_pressroom <- function(...) data.frame()
get_league_standings_history <- function(...) data.frame()
calculate_league_finances <- function(...) fixture_dataset
players_table_Server <- function(...) shiny::reactive(NULL)
players_table_UI <- function(...) shiny::tags$div()

fixture_ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Offline Rivals"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Rivals", tabName = "rivals", selected = TRUE))),
  shinydashboard::dashboardBody(
    shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),
      shiny::includeCSS("www/custom_style.css")),
    rivals_UI("rivals")))
fixture_server <- function(input, output, session) {
  rivals_Server("rivals", is_module_active = function() TRUE,
    login_token = function() c(userid = "fixture-account", token = "synthetic"),
    championship_id = function() fixture_league, user_team_id = function() "a",
    user_teams_RV = function() fixture_teams)
}
shiny::runApp(shiny::shinyApp(fixture_ui, fixture_server), host = "127.0.0.1",
  port = as.integer(Sys.getenv("FUTMONDO_TEST_PORT", "8497")), launch.browser = FALSE)
