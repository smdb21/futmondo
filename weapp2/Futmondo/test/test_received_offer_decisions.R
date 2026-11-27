#!/usr/bin/env Rscript
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source("global.R"))

auth <- c(userid = "fixture", token = "synthetic")
bid <- function(id, price, team = NULL) list(id = id, price = price, userTeam = team)
rival <- list(id = "other", name = "Rival")
listing <- bid("higher", 900)
direct <- bid("001", 450, rival)
summary <- list(data = list(id = "player"), championship = list(owner = list(`_id` = "mine")),
  market = list(bids = list(listing), direct = list(direct)), bids = list(bid("unrelated", 700)))
wire <- function(x) list(status_code = 200L, content = charToRaw(jsonlite::toJSON(x, auto_unbox = TRUE)))
expected <- list(price = 450, team = "other", label = offer_bidder_label("Rival", "rival", "other"))
lookup <- function(x) received_offer_collection(x, "mine", "player", "001", expected)
stopifnot(lookup(summary) == "direct")
for (changed in c("price", "bidder", "owner", "player", "duplicate", "gone", "replacement")) {
  x <- summary
  switch(changed,
    price = { x$market$direct[[1]]$price <- 451 },
    bidder = { x$market$direct[[1]]$userTeam$id <- "replacement" },
    owner = { x$championship$owner <- rival },
    player = { x$data$id <- "replacement" },
    duplicate = { x$market$bids <- list(direct) },
    gone = { x$bids <- list(direct); x$market$direct <- list() },
    replacement = { x$market$direct[[1]]$id <- "new" })
  stopifnot(is.null(lookup(x)))
}

requests <- character()
futmondo_post <- function(url, body, ...) {
  payload <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  q <- payload$query
  stopifnot(q$championshipId == "league", q$userteamId == "mine", identical(payload$header$userid, "fixture"))
  route <- basename(url)
  requests <<- c(requests, route)
  if (route == "summary") {
    stopifnot(q$playerId == "player", is.null(q$player_id))
    return(wire(list(answer = summary)))
  }
  stopifnot(identical(q$bid, if (grepl("roster", route)) "001" else "higher"), q$player_id == "player")
  wire(list(answer = list(code = API_CODE_OK)))
}
for (action in c("accept", "reject")) {
  for (id in c("higher", "001")) {
    result <- get(paste0(action, "_bid"))(auth, "league", "mine", "player", id)
    stopifnot(result$success, identical(tail(requests, 2L),
      c("summary", paste0(action, if (id == "001") "rosterbid" else "bid"))))
  }
}
cat("PASS listing and direct decisions use the official routes and opaque bid IDs\n")

# Hold real card confirmation I/O while Shiny processes more inputs.
row <- data.frame(id = "player", name = "Player", slug = "player", role = "defensa", team = "Club",
  value = 300, change = 0, points = 10, user_team_id = "mine", market_inMarket = TRUE,
  bid_id = "001", bid_price = 450, bid_user = "Rival", bid_source = "rival", bidder_team_id = "other")
selected <- shiny::reactiveVal(row)
reads <- writes <- refreshes <- 0L
read_done <- write_done <- write_failed <- NULL
async_adapter_fetch <- function(...) promises::promise(function(resolve, reject) NULL)
fm_async_read_http <- function(url, ...) {
  stopifnot(identical(url, PLAYER_SUMMARY_URL))
  reads <<- reads + 1L
  promises::promise(function(resolve, reject) read_done <<- resolve)
}
fm_async_http <- function(url, method, body, before_dispatch, ...) {
  if (!isTRUE(before_dispatch())) return(promises::promise_reject(simpleError("expired")))
  writes <<- writes + 1L
  stopifnot(basename(url) == "acceptrosterbid", jsonlite::fromJSON(body)$query$bid == "001")
  promises::promise(function(resolve, reject) {write_done <<- resolve; write_failed <<- reject})
}
invalidate_manual_resources <- function(...) invisible(TRUE)
fm_worker_refresh_league <- function(...) { refreshes <<- refreshes + 1L; TRUE }
settle <- function(session) for (i in seq_len(12L)) {later::run_now(0); session$flushReact()}
shiny::testServer(selected_player_Server, args = list(selected_player = selected,
  login_token = shiny::reactive(auth), championship_id = shiny::reactive("league"), user_team_id = shiny::reactive("mine")), {
  settle(session)
  session$setInputs(btn_accept_offer = 1L)
  stopifnot(reads == 0L, writes == 0L)
  session$setInputs(submit_accept_offer = 1L)
  session$setInputs(submit_accept_offer = 2L)
  heartbeat <- FALSE
  later::later(function() heartbeat <<- TRUE, 0)
  settle(session)
  stopifnot(heartbeat, reads == 1L, writes == 0L)
  read_done(wire(list(answer = summary))); settle(session)
  stopifnot(writes == 1L)
  write_failed(simpleError("timeout after dispatch")); settle(session)
  stopifnot(writes == 1L, refreshes == 1L, !received_decision_pending)
  session$setInputs(btn_accept_offer = 2L, submit_accept_offer = 3L)
  settle(session)
  stale <- summary
  stale$market$direct[[1]]$id <- "replacement"
  read_done(wire(list(answer = stale))); settle(session)
  stopifnot(reads == 2L, writes == 1L, refreshes == 2L)
  session$setInputs(btn_accept_offer = 3L, submit_accept_offer = 4L)
  settle(session)
  selected(transform(row, id = "different-player")); settle(session)
  read_done(wire(list(answer = summary))); settle(session)
  stopifnot(reads == 3L, writes == 1L, refreshes == 2L)
  selected(row); settle(session)
  for (code in c("api.error.not_found", API_CODE_OK)) {
    n <- reads + 1L
    session$setInputs(btn_accept_offer = n + 1L, submit_accept_offer = n + 4L)
    settle(session)
    read_done(wire(list(answer = summary))); settle(session)
    stopifnot(writes == n - 2L)
    write_done(wire(list(answer = list(code = code)))); settle(session)
    stopifnot(refreshes == n - 1L, !received_decision_pending)
  }

})
cat("PASS delayed confirmation, duplicate click, stale bid, context change and uncertain-write reconciliation\n")
