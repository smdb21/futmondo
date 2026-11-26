#!/usr/bin/env Rscript
# Real card confirmation and summary rendering with independently timed saved reads.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source("global.R"))

now <- base::Sys.time()
Sys.time <- function() now
auth <- c(userid = "finance-fixture", token = "synthetic")
team_facts <- function(cash, held = 100, limit = 900)
  list(budget = cash, withheld = held, maxBid = limit, teamValue = 1000, configuration = list(mnmp = 0))
observed <- function(data, at = now, ...) {
  state <- resource_state(observed_data(data, observed_at = at), ...)
  attr(state$data, "worker_acquisition_status") <- "ok"
  state
}
market_player <- data.frame(id = "target", name = "Fixture Player", slug = "target", role = "defensa",
  team = "Club", value = 200, price = 150, change = 0, points = 10, user_team_id = "", market_inMarket = TRUE)
offers <- transform(market_player[rep(1L, 524L), ], id = c("other", "target", paste0("player-", seq_len(522))),
  bid_id = c("existing", rep(NA_character_, 523)), bid_price = c(100, rep(NA_real_, 523)))
clause_player <- transform(market_player, id = "clause", slug = "clause", price = NA_real_,
  user_team_id = "rival", userTeam = "Fixture Rival", market_inMarket = FALSE,
  clause_price = 300, clause_transferred = FALSE, clause_date = "2020-01-01T00:00:00Z")
direct_player <- transform(clause_player, id = "direct", slug = "direct", clause_price = NA_real_)
responses <- list()
summary_reads <- list()
calls <- character()
refreshes <- 0L
fm_async_http <- function(url, method, body, ...) {
  query <- jsonlite::fromJSON(body)$query
  stopifnot(method == "POST", url %in% c(BID_URL, ROSTER_CLAUSE_URL), query$championshipId == "league")
  promises::promise(function(resolve, reject) {
    responses[[length(responses) + 1L]] <<- list(resolve = resolve, reject = reject, query = query)
  })
}
ack <- function(index, code = API_CODE_OK) responses[[index]]$resolve(list(status_code = 200L,
  content = charToRaw(jsonlite::toJSON(list(answer = list(code = code)), auto_unbox = TRUE))))
async_adapter_fetch <- function(fn, args = list(), ...) {
  calls <<- c(calls, fn)
  promises::promise(function(resolve, reject) {
    if (fn == "get_player_summary" && identical(args$player_id, "direct"))
      summary_reads[[length(summary_reads) + 1L]] <<- resolve
  })
}
invalidate_manual_resources <- function(...) invisible(TRUE)
fm_worker_refresh_league <- function(...) { refreshes <<- refreshes + 1L; TRUE }
log_market_transaction <- function(...) invisible(TRUE)
settle <- function(session) for (i in seq_len(15L)) { later::run_now(0); session$flushReact() }
header <- c("account_finance_visible", "output$account_finance_summary", "round_finance_RV", "round_finance_card_RV",
  "output$round_finance_amounts", "round_finance_status_display", "output$round_finance_status",
  "round_finance_checked", "output$round_finance_checked")
expressions <- as.list(parse("server.R")[[1]][[3]])[-1L]

shiny::testServer(function(input, output, session) {
  login_token_RV <- reactiveVal(auth)
  championship_id_RV <- reactiveVal("league")
  user_team_id_RV <- reactiveVal("mine")
  primary_team_state <- reactiveVal(observed(team_facts(1000), now - 5))
  primary_market_state <- reactiveVal(observed(offers, now - 5))
  history <- reactiveVal(observed(data.frame(), now - 5))
  selected <- reactiveVal(market_player)
  fm_manual_finance_session(session, login_token_RV, championship_id_RV, user_team_id_RV,
    primary_team_state, history)
  selected_player_Server("card", selected, login_token_RV, championship_id_RV, user_team_id_RV)
  for (expr in expressions) if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
    paste(deparse(expr[[2]]), collapse = "") %in% header) eval(expr, envir = environment())
  today_Server("today", reactive(input$tabs == "today"), login_token_RV, championship_id_RV, user_team_id_RV,
    user_teams_RV = function() data.frame(), primary_sources = list(team_state = primary_team_state,
      market_state = primary_market_state, display_cash = function() round_finance_RV()$cash))
}, {
  session$setInputs(tabs = "today")
  settle(session)
  shown <- function(cash, total, count, after, spend) {
    x <- round_finance_card_RV()
    stopifnot(x$cash == cash, x$commitments == total, x$active_offer_count == count,
      x$after_offers_balance == after, x$spendable_budget == spend)
    output$round_finance_amounts$html
  }
  offer <- function(button, amount, nonce) {
    do.call(session$setInputs, setNames(list(nonce), button))
    # The button and dialog share the same Shiny generation, beginning at one.
    session$setInputs(`card-confirm_player_offer` = list(generation = nonce, amount = amount, nonce = nonce))
    settle(session)
  }
  before <- shown(1000, 100, 1, 900, 900)
  offer("card-btn_bid_market", 200, 1L)
  stopifnot(length(responses) == 1L, responses[[1L]]$query$price == 200,
    identical(output$round_finance_amounts$html, before))
  started <- proc.time()[["elapsed"]]
  reads <- calls
  ack(1)
  settle(session)
  after_offer <- shown(1000, 300, 2, 700, 700)
  cat(sprintf("PASS market acknowledgement through summary in %.1f ms locally\n",
    (proc.time()[["elapsed"]] - started) * 1000))
  stopifnot(identical(calls, reads), grepl("Active offers: 300 EUR (2)", after_offer, fixed = TRUE),
    round_finance_RV()$confirmed_actions)
  with_bid <- offers
  with_bid$bid_id[2] <- "official-target"
  with_bid$bid_price[2] <- 200
  now <<- now + 5
  primary_market_state(observed(with_bid))
  settle(session)
  stopifnot(identical(shown(1000, 300, 2, 700, 700), after_offer))
  now <<- now + 5
  primary_team_state(observed(team_facts(1000, 300, 700)))
  settle(session)
  stopifnot(identical(shown(1000, 300, 2, 700, 700), after_offer), !round_finance_RV()$confirmed_actions)
  cat("PASS the offer counts once before and after the market and balance snapshots\n")

  selected(clause_player)
  settle(session)
  session$setInputs(`card-btn_pay_clause` = 1L)
  session$setInputs(`card-submit_clause` = 1L)
  settle(session)
  stopifnot(length(responses) == 2L, responses[[2L]]$query$price == 300,
    identical(output$round_finance_amounts$html, after_offer))
  now <<- now + 1
  paid_at <- now
  ack(2)
  settle(session)
  payment <- shown(700, 300, 2, 400, 400)
  reads_before_home <- length(calls)
  home_cash <- session$userData$manual_finance_projection(list(team=primary_team_state()), cash_only=TRUE)
  stopifnot(home_cash$team$data$budget == 700, home_cash$confirmed_actions,
    length(calls) == reads_before_home)
  stopifnot(refreshes == 1L, round_finance_RV()$confirmed_actions,
    grepl("700 €", output[["today-kpi_cash_box"]]$html, fixed = TRUE))
  transfer <- data.frame(id = "tx", player_id = "clause", buyer_team_id = "mine",
    seller_team_id = "rival", price = 300, is_clause = TRUE, created = paid_at)
  now <<- now + 5
  primary_team_state(observed(team_facts(700, 300, 400)))
  history(observed(transform(transfer, seller_team_id = "foreign")))
  settle(session)
  stopifnot(identical(shown(700, 300, 2, 400, 400), payment), round_finance_RV()$confirmed_actions)
  history(observed(transfer, status = "partial"))
  settle(session)
  stopifnot(round_finance_RV()$confirmed_actions)
  history(observed(transfer))
  settle(session)
  stopifnot(identical(shown(700, 300, 2, 400, 400), payment), !round_finance_RV()$confirmed_actions)
  now <<- now + 5
  primary_team_state(observed(team_facts(780, 300, 480)))
  settle(session)
  invisible(shown(780, 300, 2, 480, 480))
  cat("PASS the confirmed clause debit uses its pre-dispatch balance until exact transfer evidence arrives\n")

  selected(direct_player)
  settle(session)
  offer("card-btn_offer_owner", 150, 2L)
  stopifnot(length(responses) == 3L)
  ack(3, "api.market.max_number_players_in_roster")
  settle(session)
  invisible(shown(780, 300, 2, 480, 480))
  offer("card-btn_offer_owner", 150, 3L)
  stopifnot(length(responses) == 4L)
  responses[[4]]$reject(simpleError("synthetic lost response"))
  settle(session)
  invisible(shown(780, 300, 2, 480, 480))
  offer("card-btn_offer_owner", 150, 4L)
  stopifnot(length(responses) == 5L)
  ack(5)
  settle(session)
  direct_amounts <- shown(780, 450, 3, 330, 330)
  cat("PASS only acknowledged direct offers change the summary\n")
  now <<- now + 5
  primary_team_state(observed(team_facts(780, 450, 330)))
  settle(session)
  stopifnot(identical(shown(780, 450, 3, 330, 330), direct_amounts), round_finance_RV()$confirmed_actions)
  direct_summary <- observed(list(data = list(`_id` = "direct"), bids = list(list(id = "own-direct")),
    my_bid_id = "own-direct", my_bid_price = 150))$data
  stopifnot(length(summary_reads) > 0L)
  for (resolve in summary_reads) resolve(direct_summary)
  settle(session)
  stopifnot(identical(shown(780, 450, 3, 330, 330), direct_amounts), !round_finance_RV()$confirmed_actions)
  cat("PASS the newer balance and saved unlisted direct offer each leave the total unchanged\n")

  championship_id_RV("different")
  settle(session)
  invisible(shown(780, 300, 2, 480, 330))
  championship_id_RV("league")
  settle(session)
  invisible(shown(780, 450, 3, 330, 330))
  login_token_RV(NULL)
  settle(session)
  login_token_RV(auth)
  settle(session)
  invisible(shown(780, 300, 2, 480, 330))
  cat("PASS league switches isolate confirmed actions and logout clears them\n")

  selected(transform(clause_player, id = "clause-2", slug = "clause-2", clause_price = 125))
  settle(session)
  session$setInputs(`card-btn_pay_clause` = 2L)
  session$setInputs(`card-submit_clause` = 2L)
  settle(session)
  stopifnot(length(responses) == 6L)
  now <<- now + 5
  # A worker read can reflect the debit while the native POST is still resolving.
  primary_team_state(observed(team_facts(655, 450, 205)))
  settle(session)
  in_flight <- shown(655, 300, 2, 355, 205)
  ack(6)
  settle(session)
  stopifnot(identical(shown(655, 300, 2, 355, 205), in_flight), round_finance_RV()$confirmed_actions)
  cat("PASS a balance published during the purchase request does not double the debit\n")
})
