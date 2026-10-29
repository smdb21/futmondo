#!/usr/bin/env Rscript
# Offline JSON responses exercise the actual roster/market parsers and cache.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source("futmondo_functions.R"))
checks <- 0L
check <- function(name, expr) {
  force(expr); checks <<- checks + 1L; cat("[PASS]", name, "\n")
}
auth <- c(token = "offline", userid = "availability-fixture")
league <- "fixture-league"; team <- "fixture-team"
clubs_url <- "https://api.futmondo.com/1/league/championshipteams"
routes <- c(ROSTER_URL, MARKET_URL, clubs_url, ROSTER_BIDS_URL, MY_PLAYERS_URL, CHAMPIONSHIP_PLAYERS)
owned <- list(id = "owned", name = "Owned player", role = "defensa", value = 10000,
  change = -50, points = -3, teamId = "club", team = "Native club", logo = "native.svg")
listed <- list(id = "target", name = "Market player", role = "delantero", value = "1000",
  change = "0", points = 0, numberOfBids = 2, bid = list(id = "my-bid", price = "300"))
requests <- setNames(integer(length(routes)), routes)
parses <- 0L; normalizations <- 0L
original_parse <- parse_player_json; original_normalize <- normalize_player_fields
parse_player_json <- function(player) { parses <<- parses + 1L; original_parse(player) }
normalize_player_fields <- function(player) {
  normalizations <<- normalizations + 1L; original_normalize(player)
}
reset <- function() {
  clear_api_cache(); requests[] <<- 0L; parses <<- 0L; normalizations <<- 0L
  fixtures <<- setNames(list(list(answer = list(owned)), list(answer = list(listed)),
    list(answer = list(list(id = "club", name = "Enriched club", logo = "enriched.svg"))),
    list(answer = list()), list(answer = list()), list(answer = list(players = list(listed)))), routes)
}
futmondo_post <- function(url, ...) {
  stopifnot(url %in% routes)
  requests[[url]] <<- requests[[url]] + 1L
  fixture <- fixtures[[url]]
  if (inherits(fixture, "error")) stop("Synthetic read failure")
  payload <- if (is.character(fixture)) fixture else jsonlite::toJSON(fixture, auto_unbox = TRUE, null = "null")
  structure(list(status_code = 200L, headers = list("Content-Type" = "application/json"),
    content = charToRaw(payload), url = url), class = "response")
}
# Keep finance prerequisites deterministic; player and offer loaders stay real.
get_user_team_info <- function(...) list(budget = -100, withheld = 200, teamValue = 10000,
  maxBid = 10000, configuration = list(maxPlayersInRoster = 20))
get_championship_pressroom <- function(...) data.frame(player_id = character(),
  buyer_team_id = character(), seller_team_id = character(), created = character())
load_roster <- function() get_players_from_team(auth, league, team)
load_market <- function() get_market_players(auth, league, team)
load_capacity <- function() get_acquisition_capacity(auth, league, team, "target")

reset()
roster <- load_roster(); market <- load_market(); capacity <- load_capacity()
check("baseline JSON loaders retain native metadata and valid acquisition totals", {
  stopifnot(nrow(roster) == 1L, roster$id == "owned", roster$change == -50, roster$points == -3,
    roster$team == "Native club", roster$logo == "native.svg", nrow(market) == 1L,
    market$bid_id == "my-bid", fm_number(market$bid_price) == 300,
    capacity$status == "ok", capacity$roster$raw_count == 1L,
    capacity$outstanding$count == 1L, capacity$outstanding$total_amount == 300,
    capacity$funds$reported_budget == -100, capacity$funds$spendable_budget == 4600,
    capacity$target$my_bid_id == "my-bid", capacity$target$my_bid_amount == 300)
})
check("cold loads make five reads and warm consumers do not request or parse again", {
  stopifnot(sum(requests) == 5L, all(requests[c(ROSTER_URL, MARKET_URL, clubs_url,
    ROSTER_BIDS_URL, MY_PLAYERS_URL)] == 1L), parses == 1L, normalizations == 2L)
  before <- list(requests = requests, parses = parses, normalizations = normalizations,
    entries = resource_metrics()$entries, bytes = resource_metrics()$retained_bytes)
  for (i in seq_len(3L)) {
    stopifnot(identical(load_roster(), roster), identical(load_market(), market),
      identical(load_capacity(), capacity))
  }
  stopifnot(identical(requests, before$requests), parses == before$parses,
    normalizations == before$normalizations, resource_metrics()$entries == before$entries,
    resource_metrics()$retained_bytes == before$bytes)
  cat("[METRIC] cold reads=5; warm reads=0; warm parses=0; warm retained bytes unchanged\n")
})

reset()
optional <- list(
  c(owned, list(role2 = NULL, average = list(average = -1.5, matches = 2, fitness = list(-3, 0)),
    clause = list(price = "20000", transferDate = NULL), market = list())),
  list(id = "empty-bids", name = "Empty bids", role = "portero", role2 = list(), value = 0,
    change = 0, points = 0, teamId = "club", team = NULL, logo = NULL,
    market = list(bids = list()), average = list(), clause = list()),
  list(id = "null-fields", name = "Unknown amounts", role = "defensa", role2 = NULL,
    value = NULL, change = NULL, points = NULL, market = NULL,
    average = list(fitness = list(10, NULL)), clause = list(price = NULL)))
fixtures[[ROSTER_URL]] <- list(answer = optional)
fixtures[[MARKET_URL]] <- list(answer = list(c(listed, list(role2 = NULL, average = list(),
  clause = list(), market = list(bids = list()))), optional[[3L]]))
fixtures[[CHAMPIONSHIP_PLAYERS]] <- list(answer = list(players = optional))
fixtures[[clubs_url]] <- simpleError("Synthetic optional club failure")
roster <- load_roster(); market <- load_market()
catalog <- suppressWarnings(get_championship_players(auth, league))
check("null and empty optional metadata retain every roster, market, and catalog player", {
  stopifnot(nrow(roster) == 3L, nrow(market) == 2L, nrow(catalog) == 3L,
    setequal(roster$id, vapply(optional, `[[`, character(1), "id")),
    roster$team[roster$id == "owned"] == "Native club",
    roster$logo[roster$id == "owned"] == "native.svg",
    all(c("value", "change", "points") %in% names(roster)),
    all(c("value", "change", "points") %in% names(market)))
})
check("zero, negative, and observed string values survive while missing numbers stay NA", {
  zero <- roster[roster$id == "empty-bids", ]; unknown <- roster[roster$id == "null-fields", ]
  p <- roster[roster$id == "owned", ]; m <- market[market$id == "target", ]
  stopifnot(zero$value == 0, zero$change == 0, zero$points == 0, p$points == -3,
    p$average.total == -3, p$average.averageLastFive == -1.5,
    is.na(unknown$value), is.na(unknown$change), is.na(unknown$points),
    is.na(unknown$average.total), is.na(unknown$average.averageLastFive),
    m$value == "1000", m$change == "0", m$bid_price == "300")
})
check("failed optional enrichment is cached with the valid roster", {
  before <- requests; parsed_before <- normalizations
  stopifnot(identical(load_roster(), roster), identical(requests, before),
    normalizations == parsed_before)
})

reset()
fixtures[[ROSTER_URL]] <- list(answer = optional[1:2])
roster <- load_roster()
check("successful enrichment fills gaps without replacing native names or logos", {
  stopifnot(nrow(roster) == 2L, roster$team[roster$id == "owned"] == "Native club",
    roster$logo[roster$id == "owned"] == "native.svg",
    roster$team[roster$id == "empty-bids"] == "Enriched club",
    roster$logo[roster$id == "empty-bids"] == "enriched.svg")
})

reset()
fixtures[[ROSTER_URL]] <- list(answer = optional[3])
fixtures[[MARKET_URL]] <- list(answer = optional[3])
roster <- load_roster(); market <- load_market()
check("all-null core numeric columns remain typed missing and market sorting succeeds", {
  for (data in list(roster, market)) for (field in c("value", "change", "points"))
    stopifnot(is.numeric(data[[field]]), length(data[[field]]) == 1L, is.na(data[[field]]))
})

reset()
fixtures[[MARKET_URL]] <- list(answer = list(within(listed, bid <- list(id = "unknown-bid", price = NULL))))
capacity <- load_capacity()
check("an outgoing offer with unknown price keeps acquisition verification incomplete", {
  stopifnot(capacity$status == "partial", capacity$outstanding$count == 1L,
    is.na(capacity$outstanding$total_amount), is.na(capacity$funds$spendable_budget),
    !evaluate_acquisition_preflight(capacity, "bid", 1)$ok)
})

reset()
fixtures[[ROSTER_BIDS_URL]] <- list(answer = list(
  list(player_id = "owned", bids = list(list(id = "known", price = 250),
    list(id = "unknown", price = NULL, userTeam = list(name = NULL)))),
  list(player_id = "owned", price = 500, id = "duplicate-known"),
  list(player_id = "zero", price = 0, id = "zero-bid"),
  list(player_id = "negative", price = -5, id = "negative-bid"),
  list(player_id = "direct-null", price = NULL, id = "direct-unknown")))
incoming <- get_roster_bids(auth, league, team)
check("incoming offers preserve unknown, zero, and negative amounts", {
  stopifnot(nrow(incoming) == 4L, is.na(incoming$bid_price[incoming$id == "owned"]),
    incoming$bid_id[incoming$id == "owned"] == "unknown",
    incoming$bid_price[incoming$id == "zero"] == 0,
    incoming$bid_price[incoming$id == "negative"] == -5,
    is.na(incoming$bid_price[incoming$id == "direct-null"]))
})

loaders <- list(roster = load_roster, market = load_market,
  offers = function() get_roster_bids(auth, league, team),
  listings = function() get_my_market_players(auth, league, team))
loader_routes <- c(roster = ROSTER_URL, market = MARKET_URL, offers = ROSTER_BIDS_URL, listings = MY_PLAYERS_URL)
check("null, malformed, API errors, and failed reads remain unavailable, not empty", {
  for (name in names(loaders)) for (failure in list(list(answer = NULL), list(),
      list(answer = list(error = TRUE)), simpleError("Synthetic read failure"))) {
    reset(); fixtures[[loader_routes[[name]]]] <- failure
    result <- tryCatch(loaders[[name]](), error = identity)
    stopifnot(inherits(result, "futmondo_unavailable"))
  }
})
check("an explicit empty player or offer list remains a genuine empty result", {
  for (name in names(loaders)) {
    reset(); fixtures[[loader_routes[[name]]]] <- list(answer = list())
    result <- loaders[[name]]()
    stopifnot(is.data.frame(result), nrow(result) == 0L, attr(result, "fetch_status") == "empty")
  }
})
cat("PLAYER DATA AVAILABILITY:", checks, "passed / 0 failed\n")
