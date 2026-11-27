# Exercise the live market parser and single-attempt withdrawal through the card.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source("global.R"))
auth <- c(userid = "withdraw-fixture", token = "synthetic")
player <- data.frame(id = "target", name = "Fixture", role = "defensa", team = "Club",
  value = 1000, change = 0, points = 3, price = 1000, user_team_id = "", market_inMarket = TRUE)
rows <- transform(player, bid_id = "own-id", bid_price = 1200)
source_state <- resource_state(observed_data(rows, status = "stale", observed_at = Sys.time() - 90))
source_state$context <- resource_context(auth, "league", "mine")
attr(source_state$data, "worker_acquisition_status") <- "ok"
reads <- list()
writes <- list()
native_adapter <- async_adapter_fetch
async_adapter_fetch <- function(fn, args, ..., intent = "display") {
  if (intent == "manual_action") return(native_adapter(fn, args, ..., intent = intent))
  promises::promise(function(resolve, reject) NULL)
}
fm_worker_configured <- function() TRUE
fm_worker_rpc <- function(...) promises::promise_resolve(list(account = list(), resources = list(), jobs = list()))
record_acquired_resource <- function(...) invisible(TRUE)
get_sb_url <- get_sb_key <- function() ""
fm_async_read_http <- function(url, method, body, ...) {
  stopifnot(url == MARKET_URL, method == "POST",
    identical(jsonlite::fromJSON(body)$query, list(championshipId = "league", userteamId = "mine", type = "market")))
  promises::promise(function(resolve, reject) reads[[length(reads) + 1L]] <<- resolve)
}
fm_async_http <- function(url, method, body, ..., before_dispatch) {
  stopifnot(url == CANCEL_BID_URL, method == "POST", before_dispatch(),
    identical(jsonlite::fromJSON(body)$query, list(championshipId = "league", userteamId = "mine", bid = "own-id")))
  promises::promise(function(resolve, reject) writes[[length(writes) + 1L]] <<- list(resolve = resolve, reject = reject))
}
fm_worker_refresh_league <- function(...) TRUE
encode <- function(answer) list(status_code = 200L, times = c(total = 0.1),
  content = charToRaw(jsonlite::toJSON(list(answer = answer), auto_unbox = TRUE)))
publish <- function(i, id = "own-id") reads[[i]](encode(list(list(id = "target", change = 0,
  bid = list(id = id, price = 1200)))))
settle <- function(session) for (i in seq_len(12)) {later::run_now(0); session$flushReact()}
shiny::testServer(function(input, output, session) {
  market <- reactiveVal(source_state)
  session$userData$market_offer_state <- market
  selected <- reactiveVal(player)
  selected_player_Server("card", selected, reactive(auth), reactive("league"), reactive("mine"))
}, {
  settle(session)
  stopifnot(grepl("Cancel Bid", output[["card-action_buttons"]]$html), !length(reads), !length(writes))
  foreign <- source_state
  foreign$context$user_team_id <- "other-team"
  market(foreign); settle(session)
  stopifnot(!grepl("Cancel Bid", output[["card-action_buttons"]]$html))
  absent <- source_state
  absent$data$bid_id <- NA_character_
  market(absent); settle(session)
  stopifnot(!grepl("Cancel Bid", output[["card-action_buttons"]]$html))
  market(source_state); settle(session)
  session$setInputs(`card-btn_cancel_bid` = 1)
  done <- FALSE
  later::later(function() done <<- TRUE, 0)
  settle(session)
  stopifnot(done, length(reads) == 1L, !length(writes))
  publish(1)
  settle(session)
  session$setInputs(`card-submit_cancel_bid` = 1)
  session$setInputs(`card-submit_cancel_bid` = 2)
  settle(session)
  stopifnot(length(reads) == 2L, !length(writes))
  publish(2)
  settle(session)
  stopifnot(length(writes) == 1L)
  writes[[1]]$reject(simpleError("lost acknowledgement"))
  settle(session)
  session$setInputs(`card-submit_cancel_bid` = 3)
  settle(session)
  stopifnot(length(writes) == 1L, length(reads) == 2L)
  session$setInputs(`card-btn_cancel_bid` = 2)
  settle(session)
  publish(3, "replacement")
  settle(session)
  session$setInputs(`card-submit_cancel_bid` = 4)
  settle(session)
  stopifnot(length(writes) == 1L, length(reads) == 3L)
})
cat("PASS saved bid, delayed live reads, exact cancellation payload, replacement and lost acknowledgement\n")
