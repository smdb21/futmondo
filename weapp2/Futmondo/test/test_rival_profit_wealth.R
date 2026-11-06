#!/usr/bin/env Rscript
# Independently known synthetic profit and wealth examples; no real account.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source("global.R"))

checks <- 0L
check <- function(name, expression) {
  force(expression)
  checks <<- checks + 1L
  cat("PASS", name, "\n")
}
M <- 1000000
entry <- function(id, type, player, amount, date, included = TRUE) {
  rival_cash_entry(id, paste(player, type), amount, "Synthetic recorded transfer",
    type = type, player_id = player, date = date, included = included)
}
opening <- rival_cash_entry("opening", "Opening cash", 300 * M, "Synthetic rules", type = "opening")
ledger <- function(...) rival_cash_ledger(dplyr::bind_rows(opening, ...))
at <- function(day) sprintf("2026-10-%02dT10:00:00Z", day)
pbuy <- entry("p-buy", "buy", "p", -30 * M, at(1))
psell <- entry("p-sell", "sell", "p", 40 * M, at(2))
qbuy <- entry("q-buy", "buy", "q", -20 * M, at(3))
qsell <- entry("q-sell", "sell", "q", 15 * M, at(4))

check("completed sale margins add to five million independently of opening cash", {
  rows <- ledger(pbuy, psell, qbuy, qsell)
  result <- rival_realized_trades(rows)
  stopifnot(result$profit == 5 * M, isTRUE(result$complete), nrow(result$pairs) == 2L,
    sum(result$pairs$profit) == 5 * M)
  rows$amount[rows$type == "opening"] <- 17 * M
  stopifnot(rival_realized_trades(rows)$profit == 5 * M)
})

check("held assets, rewards and general fees do not become completed transfer profit", {
  rows <- ledger(pbuy, psell, qbuy,
    entry("points", "bonus", "", 10 * M, at(5)),
    entry("coach", "fee", "", -3 * M, at(6)))
  result <- rival_realized_trades(rows)
  stopifnot(result$profit == 10 * M, nrow(result$pairs) == 1L,
    !any(result$pairs$buy_id == "q-buy"))
  held_unknown <- qbuy
  held_unknown$amount <- NA_real_
  stopifnot(rival_realized_trades(ledger(pbuy, psell, held_unknown))$profit == 10 * M)
})

check("rebought players close each acquisition once and retain negative or zero margins", {
  rows <- ledger(entry("b1", "buy", "p", -10 * M, at(1)),
    entry("s1", "sell", "p", 15 * M, at(2)),
    entry("b2", "clause", "p", -20 * M, at(3)),
    entry("s2", "clause_sell", "p", 18 * M, at(4)),
    entry("held", "buy", "p", -100 * M, at(5)))
  result <- rival_realized_trades(rows)
  stopifnot(result$profit == 3 * M, nrow(result$pairs) == 2L,
    !anyDuplicated(result$pairs$buy_id), !anyDuplicated(result$pairs$sell_id))
  stopifnot(rival_realized_trades(ledger(qbuy, qsell))$profit == -5 * M)
  even <- psell
  even$amount <- 30 * M
  stopifnot(rival_realized_trades(ledger(pbuy, even))$profit == 0)
})

check("only explicitly associated trade fees reduce completed profit", {
  fee <- entry("fee", "transfer_fee", "p", -2 * M, at(2))
  fee$source_event_id <- "p-sell"
  result <- rival_realized_trades(ledger(pbuy, psell, qbuy, fee))
  stopifnot(result$profit == 8 * M, sum(result$pairs$fees) == 2 * M)
  excluded <- psell
  excluded$id <- "excluded-sale"
  excluded$included <- FALSE
  stopifnot(rival_realized_trades(ledger(pbuy, psell, excluded))$profit == 10 * M)
})

check("missing sold cost, price, identity or ordering stays unknown", {
  stopifnot(is.na(rival_realized_trades(ledger(psell))$profit))
  unknown <- pbuy
  unknown$amount <- NA_real_
  stopifnot(is.na(rival_realized_trades(ledger(unknown, psell))$profit))
  unknown <- psell
  unknown$amount <- NA_real_
  stopifnot(is.na(rival_realized_trades(ledger(pbuy, unknown))$profit))
  unknown <- psell
  unknown$player_id <- ""
  unknown$component <- "Unidentified sale"
  stopifnot(is.na(rival_realized_trades(ledger(pbuy, unknown))$profit))
  unknown <- psell
  unknown$date <- ""
  stopifnot(is.na(rival_realized_trades(ledger(pbuy, unknown))$profit))
})

check("same-time ownership order is only used where the completed cycle is unambiguous", {
  sale <- psell
  sale$date <- pbuy$date
  result <- rival_realized_trades(ledger(pbuy, sale))
  stopifnot(result$profit == 10 * M)
  rebuy <- entry("rebuy", "buy", "p", -20 * M, psell$date)
  forced <- rival_realized_trades(ledger(pbuy, psell, rebuy))
  stopifnot(forced$profit == 10 * M, nrow(forced$pairs) == 1L,
    forced$pairs$buy_id == "p-buy")
  tied_buy <- pbuy
  tied_buy$date <- psell$date
  ambiguous <- rival_realized_trades(ledger(tied_buy, psell, rebuy))
  stopifnot(is.na(ambiguous$profit), !isTRUE(ambiguous$complete), nzchar(ambiguous$notes))
  ordered <- ledger(pbuy, psell, qbuy, qsell)
  stopifnot(rival_realized_trades(ordered[nrow(ordered):1, ])$profit == 5 * M)
})

check("no completed trades is true zero while absent ledgers remain unavailable", {
  stopifnot(rival_realized_trades(ledger())$profit == 0,
    rival_realized_trades(ledger(pbuy, qbuy))$profit == 0,
    is.na(rival_realized_trades(NULL)$profit),
    is.na(rival_realized_trades(data.frame())$profit))
})

now <- as.POSIXct("2026-11-06T12:00:00", tz = "UTC")
championship <- "profit-wealth-fixture"
teams <- data.frame(teamid = "a", teamname = "Synthetic team", points = 0,
  teamValue = 25 * M, stringsAsFactors = FALSE)
rules <- list(initial_budget = 300 * M, money_per_point = 0, ranking_pool = 0,
  ranking_mode = "top", users_to_rank = -1, dreamteam_reward = 0, mvp_reward = 0)
evidence <- function(id, payload, observed = now, account = "fixture-account",
                     league = championship, season = "unknown") {
  rival_finance_evidence(account, league, season, "movements", paste0("a:", id), payload, observed)
}
movement <- function(id, type, player, amount, day, observed = now) {
  evidence(id, list(id = id, team_id = "a", type = type, concept = player,
    player_id = player, money = amount, date = at(day)), observed)
}
source_rows <- list(movement("p-buy", "buy", "p", -30 * M, 1),
  movement("p-sell", "sell", "p", 40 * M, 2),
  movement("q-buy", "buy", "q", -20 * M, 3),
  movement("reward", "bonus", "", 10 * M, 4))
arguments <- list(teams = teams, championship_id = championship, evidence = source_rows,
  rules = rules, account_id = "fixture-account", coverage_complete = TRUE, now = now)
build <- function(args = arguments, cache = NULL) {
  do.call(build_rival_cash_dataset, c(args, list(cache = cache)))
}
dataset <- build()
summary_values <- function(data = dataset, public = teams, info = list()) {
  rivals_summary_values(info, public, data$team_finances, "a")
}

check("shared ledger publishes ten million realised profit and twenty-five million wealth gain", {
  summary <- summary_values()
  stopifnot(dataset$team_finances$estimated_cash == 300 * M,
    dataset$team_finances$trading_profit == 10 * M,
    summary$trading_profit == 10 * M, summary$wealth_gained == 25 * M,
    summary$squad_value == 25 * M, is.logical(summary$wealth_gain_estimated))
})

check("source revisions replace profit once and replay does not duplicate it", {
  args <- arguments
  args$evidence <- c(args$evidence, list(movement("p-buy", "buy", "p", -35 * M, 1, now + 1)))
  revised <- build(args)
  stopifnot(revised$team_finances$trading_profit == 5 * M,
    summary_values(revised)$wealth_gained == 20 * M)
  args$evidence <- c(args$evidence, args$evidence)
  replay <- build(args)
  stopifnot(replay$team_finances$trading_profit == 5 * M,
    sum(replay$cash_ledgers$a$id == "movement:p-buy") == 1L)
})

check("opening replacement uses the source allocation and keeps wealth activity consistent", {
  args <- arguments
  args$evidence <- c(args$evidence,
    list(movement("allocation", "budget", "", 50 * M, 1)))
  result <- build(args)
  summary <- summary_values(result)
  stopifnot(result$team_finances$initial_budget == 50 * M,
    result$team_finances$estimated_cash == 50 * M,
    summary$trading_profit == 10 * M, summary$wealth_gained == 25 * M)
})

check("unknown included cash activity, opening allocation and squad valuation cannot become known wealth", {
  args <- arguments
  args$evidence <- c(args$evidence,
    list(movement("unknown-fee", "fee", "", NA_real_, 5)))
  unknown <- build(args)
  stopifnot(is.na(summary_values(unknown)$wealth_gained),
    summary_values(unknown)$trading_profit == 10 * M)
  args <- arguments
  args$rules$initial_budget <- NA_real_
  stopifnot(is.na(summary_values(build(args))$wealth_gained))
  public <- teams
  public$teamValue <- NA_real_
  unknown <- dataset
  unknown$team_finances$team_value <- NA_real_
  stopifnot(is.na(summary_values(unknown, public)$wealth_gained))
})

check("zero and negative wealth and public directory value precedence are retained", {
  zero <- teams
  zero$teamValue <- 0
  stopifnot(summary_values(public = zero)$wealth_gained == 0)
  negative <- dataset
  negative$team_finances$estimated_cash <- 260 * M
  negative$team_finances$reconstructed_cash <- 260 * M
  stopifnot(summary_values(negative)$wealth_gained == -15 * M)
  stopifnot(summary_values(info = list(teamValue = 999 * M))$wealth_gained == 25 * M)
})

check("cached trade calculations are reused while live valuation changes affect only wealth", {
  cache <- rival_calculation_cache()
  first <- build(cache = cache)
  repeated <- build(cache = cache)
  stopifnot(identical(first, repeated), cache$team_builds == 1L, cache$hits == 1L)
  args <- arguments
  args$teams$teamValue <- 28 * M
  updated <- build(args, cache)
  stopifnot(updated$team_finances$trading_profit == 10 * M,
    summary_values(updated, args$teams)$wealth_gained == 28 * M,
    cache$team_builds == 1L, cache$hits == 2L)
})

check("foreign-account and foreign-league movement evidence cannot alter either metric", {
  args <- arguments
  foreign <- list(id = "bad", team_id = "a", type = "sell", player_id = "p",
    money = 900 * M, date = at(6))
  args$evidence <- c(args$evidence,
    list(evidence("foreign-user", foreign, account = "another-account"),
      evidence("foreign-league", foreign, league = "another-league"),
      evidence("foreign-season", foreign, season = "another-season")))
  scoped <- build(args)
  stopifnot(scoped$team_finances$trading_profit == 10 * M,
    summary_values(scoped)$wealth_gained == 25 * M)
})

check("Market counterparties and recorded movements use the same completed trade margin", {
  transfer <- function(id, player, buyer, seller, price, day) data.frame(
    id = id, player_id = player, player_name = toupper(player),
    buyer_team_id = buyer, seller_team_id = seller, price = price,
    created = at(day), is_clause = FALSE, stringsAsFactors = FALSE)
  args <- arguments
  args$evidence <- list()
  args$pressroom <- rbind(transfer("pb", "p", "a", "", 30 * M, 1),
    transfer("ps", "p", "", "a", 40 * M, 2),
    transfer("qb", "q", "a", NA_character_, 20 * M, 3))
  result <- build(args)
  stopifnot(result$team_finances$trading_profit == 10 * M,
    result$team_finances$estimated_cash == 290 * M,
    summary_values(result)$wealth_gained == 15 * M)
})

check("recorded transfers recover player identity from unique saved source-event matches", {
  args <- arguments
  ids <- c("public-p-buy", "public-p-sell", "public-q-buy")
  for (i in seq_len(3L)) {
    args$evidence[[i]]$payload$player_id <- ""
    args$evidence[[i]]$payload$source_event_id <- ids[i]
  }
  args$pressroom <- data.frame(id = ids, player_id = c("p", "p", "q"),
    player_name = c("p", "p", "q"), buyer_team_id = c("a", "", "a"),
    seller_team_id = c("", "a", ""), price = c(30, 40, 20) * M,
    created = vapply(1:3, at, character(1)), is_clause = FALSE)
  recovered <- build(args)
  stopifnot(recovered$team_finances$trading_profit == 10 * M,
    summary_values(recovered)$wealth_gained == 25 * M,
    sum(recovered$cash_ledgers$a$type %in% c("buy", "sell")) == 3L,
    identical(recovered$cash_ledgers$a$player_id[
      match(c("movement:p-buy", "movement:p-sell", "movement:q-buy"), recovered$cash_ledgers$a$id)],
      c("p", "p", "q")))
})

check("directory-only loading never invents zero profit or wealth", {
  pending <- list(team_finances = rival_finance_directory(teams))
  summary <- summary_values(pending)
  stopifnot(is.na(summary$trading_profit), is.na(summary$wealth_gained),
    nzchar(summary$wealth_gain_notes), isTRUE(summary$wealth_gain_estimated))
})

check("player pivot projects the same paired profit and preserves unsold holdings", {
  rows <- rivals_realized_trade_rows(dataset$cash_ledgers$a)
  completed <- rows[!is.na(rows$Sell_Date) & !is.na(rows$Buy_Date), , drop = FALSE]
  held <- rows[rows$PlayerID == "q", , drop = FALSE]
  stopifnot(nrow(completed) == 1L, completed$Net_PL == 10 * M,
    sum(completed$Net_PL) == dataset$team_finances$trading_profit,
    nrow(held) == 1L, is.na(held$Net_PL), held$Bought_Price == 20 * M,
    is.na(held$Sold_Price))
  args <- arguments
  args$evidence <- list(source_rows[[2]])
  unmatched <- build(args)
  rows <- rivals_realized_trade_rows(unmatched$cash_ledgers$a)
  stopifnot(nrow(rows) == 1L, rows$Sold_Price == 40 * M,
    is.na(rows$Bought_Price), is.na(rows$Net_PL))
})

check("player pivot shares the canonical related fee deduction", {
  fee <- entry("fee", "transfer_fee", "p", -2 * M, at(2))
  fee$source_event_id <- "p-sell"
  rows <- ledger(pbuy, psell, qbuy, fee)
  attr(rows, "realized_trades") <- rival_realized_trades(rows)
  projected <- rivals_realized_trade_rows(rows)
  completed <- projected[!is.na(projected$Net_PL), , drop = FALSE]
  stopifnot(nrow(completed) == 1L, completed$Fees == 2 * M,
    completed$Net_PL == 8 * M)
})

check("fully closed same-time batches project one exact aggregate without duplicate members", {
  rows <- ledger(entry("b1", "buy", "p", -10 * M, at(1)),
    entry("b2", "buy", "p", -20 * M, at(1)),
    entry("s1", "sell", "p", 15 * M, at(1)),
    entry("s2", "sell", "p", 25 * M, at(1)))
  result <- rival_realized_trades(rows)
  stopifnot(result$profit == 10 * M, isTRUE(result$complete),
    nrow(result$pairs) == 1L, result$pairs$buy_price == 30 * M,
    result$pairs$sell_price == 40 * M)
  attr(rows, "realized_trades") <- result
  projected <- rivals_realized_trade_rows(rows)
  stopifnot(nrow(projected) == 1L, projected$Bought_Price == 30 * M,
    projected$Sold_Price == 40 * M, projected$Fees == 0,
    projected$Net_PL == 10 * M)
})

check("opening-only publication without a transfer source keeps both summary metrics unavailable", {
  args <- arguments
  args$evidence <- list()
  args$pressroom <- data.frame()
  pending <- build(args)
  summary <- summary_values(pending)
  stopifnot(is.na(summary$trading_profit), is.na(summary$wealth_gained),
    !isTRUE(summary$trading_profit_complete), nzchar(summary$wealth_gain_notes),
    rival_realized_trades(ledger())$profit == 0)
})

check("player identities require authoritative evidence and reject contradictory source-event mappings", {
  buy <- pbuy
  sale <- psell
  buy$player_id <- sale$player_id <- ""
  buy$transfer_player_name <- sale$transfer_player_name <- "Same observed name"
  stopifnot(is.na(rival_realized_trades(ledger(buy, sale))$profit))
  buy$player_id <- "p"
  stopifnot(rival_realized_trades(ledger(buy, sale))$profit == 10 * M)

  args <- arguments
  args$evidence[[1]]$payload$source_event_id <- "conflicting-public-event"
  args$pressroom <- data.frame(id = "conflicting-public-event", player_id = "another-player",
    player_name = "Other player", buyer_team_id = "a", seller_team_id = "",
    price = 30 * M, created = at(1), is_clause = FALSE)
  conflict <- build(args)
  stopifnot(is.na(conflict$team_finances$trading_profit),
    is.na(summary_values(conflict)$trading_profit),
    any(conflict$cash_ledgers$a$transfer_identity_conflict %in% TRUE))
})

check("explicit transfer fees stay unknown until assigned and held fees do not reduce realised profit", {
  fee <- entry("unlinked-fee", "transfer_fee", "p", -2 * M, "")
  unresolved <- rival_realized_trades(ledger(pbuy, psell, fee))
  stopifnot(is.na(unresolved$profit), !isTRUE(unresolved$complete),
    is.na(rival_realized_trades(ledger(fee))$profit))
  fee$source_event_id <- "q-buy"
  held <- rival_realized_trades(ledger(qbuy, fee))
  stopifnot(held$profit == 0, isTRUE(held$complete), nrow(held$pairs) == 0L)
  fee$source_event_id <- "p-sell"
  rows <- ledger(pbuy, psell, fee)
  closed <- rival_realized_trades(rows)
  stopifnot(closed$profit == 8 * M, sum(closed$pairs$fees) == 2 * M,
    identical(closed, rival_realized_trades(rows)))
})

check("vectorised single-cycle results agree with the general cycle and fee resolver", {
  count <- 12L
  costs <- (20 + seq_len(count)) * M
  margins <- (seq_len(count) - 7) * M
  rows <- ledger(entry(paste0("compare-buy-", seq_len(count)), "buy",
      paste0("compare-player-", seq_len(count)), -costs, rep(at(1), count)),
    entry(paste0("compare-sell-", seq_len(count)), "sell",
      paste0("compare-player-", seq_len(count)), costs + margins, rep(at(2), count)))
  common <- rival_realized_trades(rows)
  zero_fee <- entry("zero-related-fee", "transfer_fee", "", 0, "")
  zero_fee$source_event_id <- "compare-sell-1"
  general <- rival_realized_trades(rival_cash_ledger(dplyr::bind_rows(rows, zero_fee)))
  ordered_pairs <- function(value) {
    rows <- as.data.frame(value$pairs)[order(value$pairs$player_id), , drop = FALSE]
    rownames(rows) <- NULL
    rows
  }
  stopifnot(common$profit == sum(margins), general$profit == sum(margins),
    isTRUE(common$complete), isTRUE(general$complete),
    identical(ordered_pairs(common), ordered_pairs(general)))
})

check("the selected rival renders both distinct metrics with clear formulas", {
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
      get_user_team_info = function(...) list(teamValue = 25 * M, position = 1),
      get_players_from_team = function(...) data.frame(),
      get_financial_snapshot = function(...) list(configuration = list(budget = 300 * M)),
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
        grepl(ui_financial_amount(10 * M), html, fixed = TRUE),
        grepl(ui_financial_amount(25 * M), html, fixed = TRUE),
        !grepl("Net Gain", html, fixed = TRUE),
        !grepl("Ledger total excluding starting cash", html, fixed = TRUE))
    })
  })
})

check("many completed player cycles remain bounded and reuse source rows", {
  count <- 1000L
  rows <- dplyr::bind_rows(opening,
    entry(paste0("buy-", seq_len(count)), "buy", paste0("p-", seq_len(count)),
      rep(-30 * M, count), rep(at(1), count)),
    entry(paste0("sell-", seq_len(count)), "sell", paste0("p-", seq_len(count)),
      rep(35 * M, count), rep(at(2), count)))
  rows <- rival_cash_ledger(rows)
  elapsed <- system.time(result <- rival_realized_trades(rows))[["elapsed"]]
  stopifnot(result$profit == 5000 * M, nrow(result$pairs) == count)
  cat(sprintf("MEASURE realised profit: %d rows, %d completed cycles, %.3fs, %.1f KiB ledger\n",
    nrow(rows), count, elapsed, as.numeric(object.size(rows)) / 1024))
})

cat(sprintf("Rival profit and wealth: %d passed / 0 failed\n", checks))
