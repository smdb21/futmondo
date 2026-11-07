#!/usr/bin/env Rscript
# Synthetic accounting and rendered-card checks; no account or network access.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source("global.R"))

checks <- 0L
check <- function(name, expression) {
  force(expression)
  checks <<- checks + 1L
  cat("PASS", name, "\n")
}
ledger_rows <- function(type, amount, included = rep(TRUE, length(type))) {
  data.frame(type = type, amount = amount, included = included,
    estimated = FALSE, stringsAsFactors = FALSE)
}

check("net gain includes signed sales, purchases, fees and rewards without starting cash", {
  # Independently known answer: -30 + 20 - 5 + 10 + 5 + 10 = +10.
  rows <- ledger_rows(c("opening", "buy", "sell", "fee", "bonus", "bonus", "bonus"),
    c(100, -30, 20, -5, 10, 5, 10))
  stopifnot(rival_cash_net_gain(rows) == 10)
  rows$amount[1] <- 300000000
  stopifnot(rival_cash_net_gain(rows) == 10,
    rival_cash_net_gain(rows[nrow(rows):1, ]) == 10)
})

check("billions of historical purchases do not become a loss against the current squad value", {
  squad_value <- 711915656
  purchases <- 5576315158
  # These reproduce the reported old display exactly; the other amounts are
  # synthetic fixtures, not a claim about the user's actual ledger.
  stopifnot(squad_value - purchases == -4864399502)
  rows <- ledger_rows(c("opening", "buy", "sell", "bonus", "fee"),
    c(300000000, -purchases, 5400000000, 100000000, -10000000))
  stopifnot(rival_cash_net_gain(rows) == -86315158)
})

check("zero and negative gains survive while excluded resets and unpaid estimates contribute nothing", {
  stopifnot(rival_cash_net_gain(ledger_rows("opening", 300000000)) == 0,
    rival_cash_net_gain(ledger_rows(c("opening", "buy", "sell"), c(100, -30, 30))) == 0,
    rival_cash_net_gain(ledger_rows(c("opening", "buy", "bonus"), c(100, -30, -5))) == -35)
  rows <- ledger_rows(c("opening", "fee", "budget", "bonus"),
    c(300000000, -5, 300000000, 90), c(TRUE, TRUE, FALSE, FALSE))
  rows$estimated[4] <- TRUE
  stopifnot(rival_cash_net_gain(rows) == -5)
  rows$amount[3:4] <- NA_real_
  stopifnot(rival_cash_net_gain(rows) == -5)
})

check("unknown included amounts, identity and inclusion cannot become a zero net gain", {
  rows <- ledger_rows(c("opening", "fee"), c(100, NA_real_))
  stopifnot(is.na(rival_cash_net_gain(rows)))
  rows$amount[2] <- 5
  rows$type[2] <- NA_character_
  stopifnot(is.na(rival_cash_net_gain(rows)))
  rows$type[2] <- ""
  stopifnot(is.na(rival_cash_net_gain(rows)))
  rows$type[2] <- "fee"
  rows$included[2] <- NA
  stopifnot(is.na(rival_cash_net_gain(rows)),
    is.na(rival_cash_net_gain(NULL)), is.na(rival_cash_net_gain(data.frame())),
    is.na(rival_cash_net_gain(rows[, c("type", "amount")])),
    is.na(rival_cash_net_gain(ledger_rows("fee", 5))))
  rows$included <- c(FALSE, TRUE)
  stopifnot(is.na(rival_cash_net_gain(rows)))
})

check("an unknown opening amount stays outside otherwise known net gain", {
  rows <- ledger_rows(c("opening", "sell", "fee"), c(NA_real_, 20, -5))
  stopifnot(rival_cash_net_gain(rows) == 15,
    rival_cash_net_gain(rows[1, , drop = FALSE]) == 0)
})

now <- as.POSIXct("2026-11-06T12:00:00", tz = "UTC")
teams <- data.frame(teamid = "a", teamname = "Synthetic team", points = 5,
  teamValue = 711915656, stringsAsFactors = FALSE)
championship <- "net-gain-fixture"
rules <- list(initial_budget = 100, money_per_point = 2, ranking_pool = 0,
  ranking_mode = "top", users_to_rank = -1, dreamteam_reward = 5, mvp_reward = 10)
evidence <- function(type, id, payload, observed = now) {
  rival_finance_evidence("fixture-account", championship, "unknown", type, id, payload, observed)
}
movement <- function(id, amount, type, bonus = "", observed = now) {
  evidence("movements", paste0("a:", id), list(id = id, team_id = "a", money = amount,
    type = type, concept = id, date = "2026-10-01T10:00:00Z", bonus_type = bonus,
    round_id = if (nzchar(bonus)) "r1" else ""), observed)
}
history <- evidence("team_rounds", "a", list(team_id = "a", rounds = list(
  list(id = "r1", number = 1, points = 5, status = "done", negative = FALSE))))
movements <- list(movement("buy", -30, "buy"), movement("sell", 20, "sell"),
  movement("fee", -5, "fee"), movement("points", 10, "bonus", "points"))
awards <- data.frame(round_id = "r1", player_id = "p", is_mvp = TRUE, is_finished = TRUE)
eligibility <- data.frame(round_id = "r1", player_id = "p", team_id = "a",
  eligible = TRUE, verified = TRUE)
arguments <- list(teams = teams, championship_id = championship,
  evidence = c(list(history), movements), official = awards, pressroom = data.frame(),
  rules = rules, eligibility = eligibility, coverage_complete = TRUE, now = now,
  account_id = "fixture-account")
build <- function(args = arguments, cache = NULL) {
  do.call(build_rival_cash_dataset, c(args, list(cache = cache)))
}
dataset <- build()

check("player transaction count excludes fees and rewards and reuses reconciled events", {
  stopifnot(dataset$team_finances$player_transactions == 2L)
  args <- arguments
  args$evidence <- c(args$evidence, list(movement("clause-buy", -10, "clause"),
    movement("clause-sell", 15, "clause_sell"), movement("transfer", 0, "transfer"),
    movement("points", 12, "bonus", "points", observed = now + 1)))
  stopifnot(build(args)$team_finances$player_transactions == 5L)
  args$evidence <- c(args$evidence, args$evidence)
  stopifnot(build(args)$team_finances$player_transactions == 5L)
  args$evidence <- list(movement("points-only", 10, "bonus", "points"))
  stopifnot(build(args)$team_finances$player_transactions == 0L)
  args$evidence <- list()
  stopifnot(is.na(build(args)$team_finances$player_transactions))
})

check("shared dataset and summary publish the same known-answer ledger net gain", {
  stopifnot(dataset$team_finances$net_profit_loss == 10,
    dataset$team_finances$estimated_cash == 110,
    rival_cash_net_gain(dataset$cash_ledgers$a) == 10)
  summary <- rivals_summary_values(list(), teams, dataset$team_finances, "a")
  stopifnot(summary$net_gain == 10, summary$cash == 110, isTRUE(summary$net_gain_estimated))
})

check("shared publication preserves unknown activity and distinguishes an unknown opening amount", {
  args <- arguments
  args$evidence <- c(args$evidence, list(movement("unknown-fee", NA_real_, "fee")))
  unknown_activity <- build(args)
  fee <- unknown_activity$cash_ledgers$a[
    unknown_activity$cash_ledgers$a$id == "movement:unknown-fee", , drop = FALSE]
  summary <- rivals_summary_values(list(), teams, unknown_activity$team_finances, "a")
  stopifnot(nrow(fee) == 1L, isTRUE(fee$included), is.na(fee$amount),
    is.na(unknown_activity$team_finances$net_profit_loss),
    is.na(summary$net_gain), isTRUE(summary$net_gain_estimated))

  args <- arguments
  args$rules$initial_budget <- NA_real_
  unknown_opening <- build(args)
  summary <- rivals_summary_values(list(), teams, unknown_opening$team_finances, "a")
  stopifnot(is.na(unknown_opening$team_finances$initial_budget),
    is.na(unknown_opening$team_finances$reconstructed_cash),
    unknown_opening$team_finances$net_profit_loss == 10,
    is.na(summary$cash), summary$net_gain == 10, isTRUE(summary$net_gain_estimated))
})

check("a corrected source movement changes net gain once and replay does not duplicate it", {
  args <- arguments
  args$evidence <- c(args$evidence, list(movement("buy", -35, "buy", observed = now + 1)))
  revised <- build(args)
  stopifnot(revised$team_finances$net_profit_loss == 5,
    sum(revised$cash_ledgers$a$id == "movement:buy") == 1L)
  args$evidence <- c(args$evidence, args$evidence)
  stopifnot(build(args)$team_finances$net_profit_loss == 5)
})

check("recorded starting allocations replace configured cash without changing net gain", {
  args <- arguments
  args$evidence <- c(args$evidence, list(movement("allocation", 50, "budget")))
  recorded <- build(args)
  stopifnot(recorded$team_finances$initial_budget == 50,
    recorded$team_finances$estimated_cash == 60,
    recorded$team_finances$net_profit_loss == 10)
})

check("directory loading preserves unavailable net gain and valuation changes reuse the ledger", {
  directory <- rival_finance_directory(teams)
  stopifnot(is.na(rivals_summary_values(list(), teams, directory, "a")$net_gain))
  cache <- rival_calculation_cache()
  first <- build(cache = cache)
  repeated <- build(cache = cache)
  stopifnot(identical(first, repeated), cache$team_builds == 1L, cache$hits == 1L)
  args <- arguments
  args$teams$teamValue <- 1
  changed <- build(args, cache)
  stopifnot(changed$team_finances$net_profit_loss == 10,
    changed$team_finances$team_value == 1, cache$team_builds == 1L, cache$hits == 2L,
    rivals_summary_values(list(teamValue = 999), args$teams,
      changed$team_finances, "a")$net_gain == 10)
})

check("selected rival card distinguishes trading profit from total wealth gain", {
  local({
    stub_names <- c("bind_factual_reads", "shared_mvp_source", "shared_rival_finance_source",
      "get_user_team_info", "get_players_from_team", "get_financial_snapshot",
      "get_championship_pressroom", "get_league_standings_history",
      "calculate_league_finances", "players_table_Server", "players_table_UI")
    original <- mget(stub_names, envir = .GlobalEnv)
    on.exit(list2env(original, envir = .GlobalEnv))
    stubs <- list(
      bind_factual_reads = function(...) NULL,
      shared_mvp_source = function(...) shiny::reactive(list(rows = data.frame(), complete = FALSE)),
      shared_rival_finance_source = function(...) shiny::reactive(dataset),
      get_user_team_info = function(...) list(teamValue = 711915656, position = 1),
      get_players_from_team = function(...) data.frame(),
      get_financial_snapshot = function(...) list(configuration = list(budget = 100)),
      get_championship_pressroom = function(...) data.frame(),
      get_league_standings_history = function(...) data.frame(),
      calculate_league_finances = function(...) dataset,
      players_table_Server = function(...) shiny::reactive(NULL),
      players_table_UI = function(...) shiny::tags$div())
    list2env(stubs, envir = .GlobalEnv)
    shiny::testServer(rivals_Server, args = list(is_module_active = function() TRUE,
      login_token = function() c(userid = "fixture-account", token = "synthetic"),
      championship_id = function() championship, user_team_id = function() "a",
      user_teams_RV = function() teams), {
      session$flushReact()
      session$setInputs(rival_team_selection = list(id = "a", context = list(
        user = "fixture-account", championship = championship, team = "a")))
      session$flushReact()
      html <- output$rival_financial_summary_box$html
      stopifnot(grepl("Trading profit", html, fixed = TRUE),
        grepl("Overall wealth gained", html, fixed = TRUE),
        grepl("Cash plus squad value minus starting cash", html, fixed = TRUE),
        grepl(ui_financial_amount(711915666), html, fixed = TRUE),
        !grepl("Net Gain (estimate)", html, fixed = TRUE))
    })
  })
})

check("large ledger net gain uses bounded vectorized work", {
  rows <- ledger_rows(c("opening", rep(c("buy", "sell"), 10000L)),
    c(300000000, rep(c(-30, 35), 10000L)))
  elapsed <- system.time(for (iteration in seq_len(100L)) {
    stopifnot(rival_cash_net_gain(rows) == 50000)
  })[["elapsed"]]
  cat(sprintf("MEASURE net gain: %d rows x 100 evaluations, %.3fs total, %.1f KiB ledger\n",
    nrow(rows), elapsed, as.numeric(object.size(rows)) / 1024))
})

cat(sprintf("Rival net gain: %d passed / 0 failed\n", checks))
