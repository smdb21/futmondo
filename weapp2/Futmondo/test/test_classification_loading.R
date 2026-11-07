#!/usr/bin/env Rscript
# Offline delayed reads exercise the real module and shared-resource scheduler.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source("global.R"))
auth <- c(token = "synthetic", userid = "classification-fixture")
calls <- character()
pending <- new.env(parent = emptyenv())
async_adapter_fetch <- function(fn, args = list(), context, ...) {
  calls <<- c(calls, fn)
  promises::promise(function(resolve, reject) pending[[fn]] <- list(resolve = resolve, reject = reject))
}
shared_mvp_source <- function(...) reactive(list())
finance <- reactiveVal(list(loading = TRUE))
shared_rival_finance_source <- function(...) finance
settle <- function(session) for (i in 1:20) {
  later::run_now(0)
  session$flushReact()
}
teams <- data.frame(teamid = "a", teamname = "Alpha", points = 5, position = 1)
clear_api_cache()
shiny::testServer(classification_Server, args = list(
  is_module_active = reactive(TRUE), login_token = reactive(auth),
  championship_id = reactive("league"), user_team_id = reactive("a"),
  user_teams_RV = reactive(teams)), {
  session$setInputs(single_round_select = "all", round_range_slider = c(1, 2))
  settle(session)
  stopifnot(sum(calls == "get_user_team_info") == 1L,
    !any(calls %in% c("get_financial_snapshot", "get_acquisition_capacity", "get_lineup_from_team")),
    grepl("Select one round", output$dreamteam_box_ui$html, fixed = TRUE))
  # Settings become usable while standings and the finance ledger are pending.
  pending$get_user_team_info$resolve(list(configuration = list(
    moneyPerPoint = 70000, moneyPerRanking = 30000000, rankingMode = "flop",
    mvpPlayer = 1000000, dreamTeamPlayer = 500000)))
  settle(session)
  rewards <- output$configured_rewards_ui$html
  stopifnot(grepl("70.000", rewards, fixed = TRUE), grepl("500.000", rewards, fixed = TRUE),
    !grepl("Unavailable", rewards, fixed = TRUE), isTRUE(finance()$loading))
  pending$get_classification_round_catalog$resolve(list(rounds=data.frame(round_id = "r1", round_number = 1)))
  pending$get_user_team_rounds$resolve(list(list(round = "r1", points = 5)))
  settle(session)
  stopifnot(grepl("Bonus totals are loading", output$classification_status$html, fixed = TRUE))
  ledger <- data.frame()
  attr(ledger, "round_bonus_totals") <- data.frame(round_number = 1,
    bonus_type = c("points", "mvp", "dreamteam", "ranking"), amount = c(350000, 0, 0, 0))
  finance(list(loading = FALSE, cash_ledgers = list(a = ledger)))
  settle(session)
  status <- output$classification_status$html
  stopifnot(!grepl("unavailable|missing|loading", status, ignore.case = TRUE))
  # Selecting a round starts only its dream-team read; settings stay visible.
  before <- calls
  session$setInputs(single_round_select = "1")
  settle(session)
  stopifnot(identical(tail(calls, length(calls) - length(before)), "get_round_dreamteam"),
    identical(output$configured_rewards_ui$html, rewards))
  pending$get_round_dreamteam$resolve(list(mvp = "p", players = list(list(id = "p", name = "Player", points = 9))))
  settle(session)
  stopifnot(grepl("Player", output$dreamteam_box_ui$html, fixed = TRUE),
    grepl("MVP", output$dreamteam_box_ui$html, fixed = TRUE),
    identical(output$configured_rewards_ui$html, rewards))
  # A real coverage gap stays visible and never becomes a zero.
  attr(ledger, "round_bonus_totals")$amount[2] <- NA_real_
  finance(list(loading = FALSE, cash_ledgers = list(a = ledger)))
  settle(session)
  stopifnot(grepl("Some bonus totals lack complete", output$classification_status$html, fixed = TRUE),
    is.na(standings_RV()$mvp_bonus))
  session$setInputs(single_round_select = "all")
  settle(session)
  stopifnot(sum(calls == "get_user_team_info") == 1L)
})
cat("PASS classification: independent settings, one settings read, no capacity/lineup reads, honest coverage status\n")

# The application passes the same source used by the League settings page.
# Configuration is immediately usable without a module-specific information read.
clear_api_cache()
calls <- character()
settings <- reactiveVal(list(league = list(configuration = list(moneyPerPoint = 70000,
  moneyPerRanking = 30000000, rankingMode = "flop", mvpPlayer = 1000000,
  dreamTeamPlayer = 500000)), info = list(configuration = list(mvpPlayer = 0)), status = "loading"))
shiny::testServer(classification_Server, args = list(
  is_module_active = reactive(TRUE), login_token = reactive(auth),
  championship_id = reactive("league"), user_team_id = reactive("a"),
  user_teams_RV = reactive(teams), league_settings_source = settings), {
  session$setInputs(single_round_select = "all", round_range_slider = c(1, 2))
  settle(session)
  html <- gsub("[[:space:]]+", " ", output$configured_rewards_ui$html)
  stopifnot(grepl("70.000", html, fixed = TRUE), grepl("MVP: 0", html, fixed = TRUE),
    !any(calls %in% c("get_user_team_info", "get_financial_snapshot")))
  before <- calls
  updated <- settings()
  updated$info$configuration$moneyPerPoint <- 0
  updated$stale <- TRUE
  settings(updated)
  settle(session)
  stopifnot(grepl("points: 0", gsub("[[:space:]]+", " ", output$configured_rewards_ui$html), fixed = TRUE),
    grepl("Showing saved league settings", output$configured_rewards_ui$html, fixed = TRUE),
    identical(calls, before))
})
cat("PASS shared league configuration: zero additional settings reads, live updates, explicit zero and stale values\n")
