#!/usr/bin/env Rscript
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source('global.R'))
auth <- c(userid = 'offer-fixture', token = 'synthetic')
row <- data.frame(id = 'player', name = 'Fixture Player', slug = 'fixture', role = 'defensa', team = 'Club',
  value = 200, price = 150, change = 0, points = 10, user_team_id = '', market_inMarket = TRUE)
reads <- character()
jobs <- list()
rpc <- character()
refreshes <- list()
settled <- character()
logged <- list()
modal <- NULL
get_sb_url <- function() ''
get_sb_key <- function() ''
fm_clause_enabled <- function() TRUE
fm_clause_refresh <- function(...) invisible(NULL)
fm_clause_jobs <- function(...) list()
record_acquired_resource <- function(...) invisible(TRUE)
invalidate_manual_resources <- function(...) invisible(NULL)
fm_worker_refresh_league <- function(login, league, team, player) refreshes[[length(refreshes) + 1L]] <<- c(league, team, player)
log_market_transaction <- function(...) logged[[length(logged) + 1L]] <<- list(...)
showModal <- function(ui, ...) modal <<- as.character(ui)
async_adapter_fetch <- function(fn, ...) {
  reads <<- c(reads, fn)
  stopifnot(!fn %in% c('get_scheduled_clause_check', 'get_team_acquisition_capacity', 'get_acquisition_capacity'))
  promises::promise(function(resolve, reject) NULL)
}
fm_clause_rpc <- function(name, payload, ...) {
  rpc <<- c(rpc, name)
  if (name == 'settle') {
    settled <<- c(settled, payload$p_state)
    return(promises::promise(function(resolve, reject) NULL))
  }
  stopifnot(name == 'dispatch_manual_offer', payload$p_mode %in% c('bid', 'offer'))
  promises::promise_resolve("dispatched")
}
fm_async_http <- function(url, method, body, ...) {
  payload <- jsonlite::fromJSON(body)$query
  stopifnot(identical(url, BID_URL), identical(method, 'POST'), identical(payload$isClause, FALSE))
  promises::promise(function(resolve, reject) {
    jobs[[length(jobs) + 1L]] <<- list(resolve = resolve, reject = reject, payload = payload)
  })
}
response <- function(code, message = code) list(status_code = 200L,
  content = charToRaw(jsonlite::toJSON(list(answer = list(code = code, msg = message)), auto_unbox = TRUE)))
settle <- function(session) for (i in seq_len(20L)) { later::run_now(0); session$flushReact() }
selected <- reactiveVal(row)
testServer(selected_player_Server, args = list(selected_player = selected, login_token = reactive(auth),
  championship_id = reactive('league'), user_team_id = reactive('mine'),
  capacity_fetcher = function(...) stop('Unexpected synchronous verification')), {
  settle(session)
  before <- reads
  stopifnot(grepl('Make Market Offer', output$action_buttons$html, fixed = TRUE))
  started <- proc.time()[['elapsed']]
  session$setInputs(btn_bid_market = 1L)
  elapsed <- proc.time()[['elapsed']] - started
  stopifnot(identical(before, reads), length(jobs) == 0L, length(rpc) == 0L, grepl('value="150"', modal, fixed = TRUE))
  for (amount in c(151,152,153,156,158,165)) stopifnot(grepl(paste0('data-fm-offer-premium="', amount, '"'), modal, fixed = TRUE))
  cat(sprintf('PASS cold editor opens with amount and premiums in %.0f ms while all card reads are held\n', elapsed * 1000))
  send <- function(generation, amount, nonce = 1L) {
    session$setInputs(confirm_player_offer = list(generation = generation, amount = amount, nonce = nonce))
    settle(session)
  }
  generation <- market_offer_dialog_RV()$generation
  send(generation - 1L, 170)
  send(generation, -1)
  stopifnot(length(jobs) == 0L, length(rpc) == 0L)
  send(generation, 171)
  send(generation, 172)
  stopifnot(identical(before, reads), length(jobs) == 1L, jobs[[1]]$payload$price == 171,
    offer_modal_opened_RV(), length(refreshes) == 0L)
  jobs[[1]]$resolve(response(API_CODE_OK)); settle(session)
  stopifnot(!offer_modal_opened_RV(),
    identical(settled, 'accepted'), length(logged) == 1L, !length(refreshes))
  send(generation, 171, 2L)
  stopifnot(length(jobs) == 1L)
  cat('PASS offered amount travels atomically; one write despite repeated confirmation; the result does not wait on database settlement\n')

  rival <- row; rival$user_team_id <- 'rival'; rival$userTeam <- 'Fixture Rival'; rival$market_inMarket <- FALSE; rival$price <- NA_real_
  selected(rival); settle(session)
  session$setInputs(btn_offer_owner = 1L)
  stopifnot(grepl('Offer Money to Fixture Rival', modal, fixed = TRUE), grepl('value="200"', modal, fixed = TRUE))
  send(market_offer_dialog_RV()$generation, 201)
  jobs[[2]]$resolve(response('api.market.max_number_players_in_roster')); settle(session)
  stopifnot(length(jobs) == 2L, length(refreshes) == 1L, length(logged) == 1L, tail(settled, 1L) == 'failed')
  session$setInputs(btn_offer_owner = 2L)
  send(market_offer_dialog_RV()$generation, 202)
  jobs[[3]]$reject(simpleError('synthetic lost acknowledgement')); settle(session)
  stopifnot(length(jobs) == 3L, length(refreshes) == 2L, length(logged) == 1L,
    tail(settled, 1L) == 'uncertain', all(vapply(refreshes, identical, logical(1), c('league', 'mine', 'player'))))
  cat('PASS rejection and lost acknowledgement each refresh the same context and never retry an offer\n')

  session$setInputs(btn_offer_owner = 3L)
  generation <- market_offer_dialog_RV()$generation
  session$setInputs(market_offer_closed = as.character(generation))
  send(generation, 203)
  stopifnot(length(jobs) == 3L, is.null(market_offer_dialog_RV()))
  cat('PASS closing invalidates that confirmation generation\n')
})
