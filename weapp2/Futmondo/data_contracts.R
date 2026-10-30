# Shared, explicit contracts for observed data. See docs/data_contracts.md.
source("theme.R")
preserve_observation_time <- function(data, source = data) {
  if (!is.data.frame(data)) return(data)
  stamp <- attr(source, "observed_at")
  if (!"observed_at" %in% names(data)) data$observed_at <- rep(
    if (is.null(stamp)) NA_character_ else format(fm_time(stamp), "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), nrow(data))
  attr(data, "fetch_status") <- attr(source, "fetch_status")
  data
}
fm_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(default)
  as.character(x)
}

fm_number <- function(x, default = NA_real_) {
  n <- suppressWarnings(as.numeric(x))
  if (length(n) != 1L || !is.finite(n)) default else n
}

fm_time <- function(x) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = "UTC"))
  if (is.numeric(x)) return(as.POSIXct(ifelse(abs(x)>1e11,x/1000,x),origin="1970-01-01",tz="UTC"))
  values <- vapply(as.character(x), function(value) {
    if (is.na(value) || !nzchar(value)) return(NA_real_)
    value <- sub("Z$", "+0000", value)
    value <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", value)
    for (fmt in c("%Y-%m-%dT%H:%M:%OS%z", "%Y-%m-%d %H:%M:%OS%z",
                  "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d")) {
      parsed <- suppressWarnings(as.POSIXct(value, format=fmt, tz="UTC"))
      if (!is.na(parsed)) return(as.numeric(parsed))
    }
    NA_real_
  }, numeric(1))
  as.POSIXct(values, origin="1970-01-01", tz="UTC")
}

# Futmondo permits a temporary negative balance down to half the team value.
# `withheld` is the API's reservation for active outgoing offers. When the
# market endpoint reports the same offers, both fields describe the same held
# money and must not be subtracted twice. This is acquisition headroom only:
# cash must be positive when a round begins.
acquisition_headroom <- function(cash, team_value, withheld = 0,
                                 commitments = 0, debt_fraction = 0.5) {
  values <- vapply(list(cash, team_value, withheld, commitments, debt_fraction),
    fm_number, numeric(1))
  names(values) <- c("cash", "team_value", "withheld", "commitments", "debt_fraction")
  if (any(!is.finite(values)) || values["team_value"] < 0 ||
      values["withheld"] < 0 || values["commitments"] < 0 ||
      values["debt_fraction"] < 0) {
    return(list(spendable_budget = NA_real_, debt_limit = NA_real_,
      minimum_balance = NA_real_, projected_committed_balance = NA_real_, reserved_amount = NA_real_))
  }
  debt_limit <- values["team_value"] * values["debt_fraction"]
  reserved_amount <- max(values["withheld"], values["commitments"])
  committed_balance <- values["cash"] - reserved_amount
  list(spendable_budget = max(0, committed_balance + debt_limit),
    debt_limit = debt_limit, minimum_balance = -debt_limit,
    projected_committed_balance = committed_balance, reserved_amount = reserved_amount)
}

# Display-only finances use the same independently loaded team/market states as
# Today. Manual acquisitions still require the full capacity/preflight contract.
round_finance_display <- function(team_state, market_state) {
  fresh <- function(state) is.list(state) && isTRUE(state$status %in% c("ok", "empty")) &&
    !identical(state$freshness, "stale") && !identical(attr(state$data, "fetch_status"), "stale")
  loading <- function(state) is.list(state) && identical(state$status, "loading")
  team <- team_state$data
  cash <- fm_number(team$budget)
  withheld <- fm_number(team$withheld)
  team_value <- fm_number(team$teamValue %||% team$team_value)
  market <- market_state$data
  commitments <- NA_real_; offer_count <- NA_integer_
  # A successful empty market proves zero outgoing offers. Missing/failed/stale
  # responses do not. bid_id identifies own outgoing bids in the market contract.
  if (fresh(market_state) && is.data.frame(market) && (!nrow(market) || "bid_id" %in% names(market))) {
    own <- if ("bid_id" %in% names(market)) which(!is.na(market$bid_id) & nzchar(as.character(market$bid_id))) else integer()
    bids <- market[own, , drop=FALSE]
    if (nrow(bids)) bids <- bids[!duplicated(bids$bid_id), , drop=FALSE]
    prices <- if (nrow(bids) && "bid_price" %in% names(bids)) suppressWarnings(as.numeric(bids$bid_price)) else numeric()
    if (!nrow(bids) || (length(prices) == nrow(bids) && all(is.finite(prices) & prices > 0))) {
      commitments <- sum(prices); offer_count <- nrow(bids)
    }
  }
  projected_committed_balance <- spendable <- NA_real_
  if (fresh(team_state) && is.finite(cash) && is.finite(withheld) && withheld >= 0 && is.finite(commitments)) {
    projected_committed_balance <- cash - max(withheld, commitments)
    spendable <- acquisition_headroom(cash, team_value, withheld, commitments)$spendable_budget
  }
  confirmed_problem <- fresh(team_state) &&
    ((is.finite(cash) && cash <= 0) || (is.finite(projected_committed_balance) && projected_committed_balance <= 0))
  class <- if (confirmed_problem) "round-countdown-warning" else
    if (is.finite(projected_committed_balance) && projected_committed_balance > 0) "round-countdown-solvent" else "round-countdown-unavailable"
  team_stale <- identical(team_state$freshness, "stale") || identical(team_state$status, "stale") ||
    identical(attr(team_state$data, "fetch_status"), "stale")
  stale <- team_stale || identical(market_state$freshness, "stale") || identical(market_state$status, "stale") ||
    identical(attr(market_state$data, "fetch_status"), "stale")
  message <- if (confirmed_problem) {
    balance <- if (is.finite(projected_committed_balance)) projected_committed_balance else cash
    paste0("Restore at least ", format(round(abs(balance) + 1), big.mark=".", decimal.mark=",", scientific=FALSE),
      " EUR before kickoff to score points", if (!is.finite(projected_committed_balance)) "; active offers still need verification" else "")
  } else if (loading(team_state)) "Loading balance…" else if (loading(market_state)) "Loading active offers…" else
    if (stale) "Cached financial data — refresh to verify before kickoff" else
    if (!is.null(team_state$error) || !is.null(market_state$error)) "Financial data could not load — refresh to retry" else
    if (!is.finite(cash)) "Balance unavailable — cannot verify kickoff balance" else
    if (!is.finite(commitments)) "Active offers unavailable — cannot verify kickoff balance" else
    if (!is.finite(withheld) || withheld < 0) "Held funds unavailable — cannot verify kickoff balance" else
    if (!is.finite(spendable)) "Spending allowance unavailable" else NULL
  list(cash=cash, cash_loading=loading(team_state), cash_stale=team_stale && is.finite(cash),
    cash_unverified=!fresh(team_state) && !team_stale && is.finite(cash),
    commitments=commitments, active_offer_count=offer_count, offers_loading=loading(market_state),
    projected_committed_balance=projected_committed_balance, spendable_budget=spendable,
    class=class, message=message)
}

next_round_context <- function(rounds, now = Sys.time()) {
  unavailable <- list(available = FALSE, round_number = NA_real_, starts_at = as.POSIXct(NA))
  if (!is.data.frame(rounds) || !nrow(rounds) ||
      !all(c("round_number", "begin_process") %in% names(rounds))) return(unavailable)
  starts <- fm_time(rounds$begin_process)
  current <- fm_time(now)
  candidates <- which(!is.na(starts) & starts > current)
  if (!length(candidates)) return(unavailable)
  index <- candidates[which.min(starts[candidates])]
  list(available = TRUE, round_number = fm_number(rounds$round_number[index]),
    starts_at = starts[index])
}

# Return the latest started round that the source still marks unfinished.
current_round_context <- function(rounds, now = Sys.time()) {
  unavailable <- list(available = FALSE, round_number = NA_real_, starts_at = as.POSIXct(NA))
  if (!is.data.frame(rounds) || !nrow(rounds) ||
      !all(c("round_number", "begin_process", "is_finished") %in% names(rounds))) return(unavailable)
  starts <- fm_time(rounds$begin_process)
  current <- fm_time(now)
  unfinished <- !is.na(rounds$is_finished) & !as.logical(rounds$is_finished)
  candidates <- which(!is.na(starts) & starts <= current & unfinished)
  if (!length(candidates)) return(unavailable)
  index <- candidates[which.max(starts[candidates])]
  list(available = TRUE, round_number = fm_number(rounds$round_number[index]),
    starts_at = starts[index])
}

format_round_countdown <- function(starts_at, now = Sys.time()) {
  seconds <- floor(as.numeric(difftime(fm_time(starts_at), fm_time(now), units = "secs")))
  if (length(seconds) != 1L || !is.finite(seconds)) return("Start time unavailable")
  seconds <- max(0, seconds)
  days <- seconds %/% 86400; seconds <- seconds %% 86400
  hours <- seconds %/% 3600; seconds <- seconds %% 3600
  minutes <- seconds %/% 60; seconds <- seconds %% 60
  sprintf("%dd %02dh %02dm %02ds", days, hours, minutes, seconds)
}
fetch_result <- function(data = NULL, status = "ok", source = "futmondo",
                         observed_at = Sys.time(), error = NULL) {
  stopifnot(status %in% c("ok", "empty", "stale", "partial", "unavailable"))
  list(data = data, status = status, source = source,
       observed_at = observed_at, error = error)
}

observed_data <- function(data, source = "futmondo", status = NULL,
                          observed_at = Sys.time()) {
  if (is.null(data)) return(NULL)
  if (is.null(status)) status <- if (length(data) == 0L ||
    (is.data.frame(data) && nrow(data) == 0L)) "empty" else "ok"
  attr(data, "fetch_status") <- status
  attr(data, "observed_at") <- observed_at
  attr(data, "source") <- source
  data
}

as_fetch_result <- function(data, source = "futmondo") {
  if (is.null(data)) return(fetch_result(status = "unavailable", source = source))
  fetch_result(data, attr(data, "fetch_status") %||% "ok",
               attr(data, "source") %||% source,
               attr(data, "observed_at") %||% Sys.time())
}

`%||%` <- function(x, y) if (is.null(x)) y else x

valid_login <- function(token) {
  candidate <- is.atomic(token) || is.list(token)
  isTRUE(candidate) && !is.null(names(token)) &&
    all(c("token", "userid") %in% names(token)) &&
    isTRUE(nzchar(fm_scalar(token[["token"]], ""))) &&
    isTRUE(nzchar(fm_scalar(token[["userid"]], "")))
}

is_authorized_admin <- function(token, admin = Sys.getenv("admin")) {
  valid_login(token) && "user_name" %in% names(token) && nzchar(trimws(admin)) &&
    identical(tolower(trimws(fm_scalar(token[["user_name"]], ""))),
              tolower(trimws(admin)))
}

normalize_bid_observations <- function(pressroom_df, championship_id,
                                       observed_at = Sys.time()) {
  empty <- data.frame(auction_id = character(), championship_id = character(),
    player_id = character(), bidder_id = character(), bidder_name = character(),
    amount = numeric(), is_winner = logical(), source_bid_id = character(),
    settled_at = character(), observed_at = character(), stringsAsFactors = FALSE)
  if (is.null(pressroom_df) || !nrow(pressroom_df)) return(empty)
  rows <- list()
  for (i in seq_len(nrow(pressroom_df))) {
    r <- pressroom_df[i, , drop = FALSE]
    # Missing counterparties represent the system. Only purchases FROM the
    # system are comparable auctions; private sales/clauses stay in the ledger.
    seller <- fm_scalar(r$seller_team_id, "")
    winner <- fm_scalar(r$buyer_team_id, "")
    if (nzchar(seller) || !nzchar(winner) || !nzchar(fm_scalar(r$id, ""))) next
    append_bid <- function(id, name, amount, won, bid_id = "") {
      if (!nzchar(id) || !is.finite(amount) || amount <= 0) return(NULL)
      data.frame(auction_id = fm_scalar(r$id), championship_id = as.character(championship_id),
        player_id = fm_scalar(r$player_id), bidder_id = id, bidder_name = name,
        amount = amount, is_winner = won, source_bid_id = bid_id,
        settled_at = fm_scalar(r$created),
        observed_at = format(observed_at, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
        stringsAsFactors = FALSE)
    }
    rows[[length(rows) + 1L]] <- append_bid(winner, fm_scalar(r$buyer_team_name, ""),
                                            fm_number(r$price), TRUE)
    bids <- if ("bids" %in% names(r)) r$bids[[1]] else list()
    if (is.data.frame(bids)) bids <- split(bids, seq_len(nrow(bids)))
    for (b in bids) {
      id <- fm_scalar(b$u$`_id` %||% b$u$id, "")
      if (identical(id, winner)) next # settlement price wins over duplicate history
      rows[[length(rows) + 1L]] <- append_bid(id, fm_scalar(b$u$name, ""),
        fm_number(b$bid), FALSE, fm_scalar(b$`_id`, ""))
    }
  }
  out <- dplyr::bind_rows(c(list(empty), rows))
  if (!nrow(out)) return(empty)
  out <- out[order(-out$amount), , drop = FALSE]
  out[!duplicated(out[c("championship_id", "auction_id", "bidder_id")]), , drop = FALSE]
}

normalize_auction_observations <- function(pressroom_df, championship_id,
                                           observed_at = Sys.time()) {
  if (is.null(pressroom_df) || !nrow(pressroom_df)) return(data.frame())
  d <- pressroom_df
  keep <- (is.na(d$seller_team_id) | d$seller_team_id == "") &
    !is.na(d$buyer_team_id) & d$buyer_team_id != ""
  d <- d[keep, , drop = FALSE]
  if (!nrow(d)) return(data.frame())
  data.frame(auction_id = as.character(d$id), championship_id = as.character(championship_id),
    player_id = as.character(d$player_id), winner_id = as.character(d$buyer_team_id),
    winning_amount = as.numeric(d$price), settled_at = as.character(d$created),
    observed_at = format(observed_at, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    outcome = "sold", visibility_complete = FALSE, stringsAsFactors = FALSE)
}

# Only explicit per-player logical flags establish finality. Missing, malformed
# or contradictory evidence is unknown; round/match status cannot fill this gap.
player_points_finality <- function(point) {
  fields<-intersect(c("isFinished","finished"),names(point))
  if(!length(fields))return("unknown")
  values<-lapply(fields,function(field)point[[field]])
  recognized<-vapply(values,function(value)is.logical(value)&&length(value)==1L&&!is.na(value),logical(1))
  if(!all(recognized))return("unknown")
  values<-unique(unlist(values,use.names=FALSE))
  if(length(values)!=1L)return("unknown")
  if(values)"final"else "provisional"
}

normalize_player_match_observations <- function(summary, player_id, championship_id,
                                                 scoring_version = "unknown",
                                                 season = "unknown", observed_at = Sys.time(), rounds = NULL) {
  points <- summary$points
  if (is.null(points) || !length(points)) return(data.frame())
  if (is.data.frame(points)) points <- split(points, seq_len(nrow(points)))
  # Historical round numbers do not establish per-player score finality.
  dplyr::bind_rows(lapply(points, function(p) {
    round <- fm_number(p$round)
    if (!is.finite(round)) return(NULL)
    pts <- fm_number(p$points)
    source_match <- if(is.list(p$match))p$match else list()
    timing <- if(is.data.frame(rounds) && all(c('round_number','round_start_at') %in% names(rounds)))
      rounds[rounds$round_number==round,,drop=FALSE] else data.frame()
    occurred <- if(nrow(timing)==1L) fm_scalar(timing$round_start_at) else NA_character_
    # minutesPlayed/initialLineUp semantics are not yet verified: retain raw
    # observations but do not manufacture participation or calibrated minutes.
    data.frame(player_id = as.character(player_id), championship_id = as.character(championship_id),
      season = season, scoring_version = scoring_version, round = round, points = pts,
      match_id=fm_scalar(p$matchId %||% source_match$id %||% source_match$`_id`,""),
      round_id=fm_scalar(p$roundId,""),
      competition_id=fm_scalar(p$competitionId %||% summary$championship$competitionId,""),
      score_status = if (!is.finite(pts)) "unavailable" else player_points_finality(p),
      is_home = if (is.logical(p$isHomeTeam)) p$isHomeTeam else NA,
      minutes_raw = fm_number(p$minutesPlayed),
      start_raw = if (is.logical(p$initialLineUp)) p$initialLineUp else NA,
      participation = NA, round_start_at=occurred, occurred_at=occurred,
      observed_at = format(observed_at, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      stringsAsFactors = FALSE)
  }))
}

league_season_context <- function(championship_id, observed_season=NULL,
                                  configured=Sys.getenv("FUTMONDO_SEASON_CONTEXTS","{}")) {
  observed <- fm_scalar(observed_season,"")
  if(nzchar(observed)) return(observed)
  mapping <- tryCatch(jsonlite::fromJSON(configured,simplifyVector=FALSE),error=function(e)list())
  id <- fm_scalar(championship_id,"")
  if(!nzchar(id) || !is.list(mapping)) return("unknown")
  fm_scalar(mapping[[id]],"unknown")
}

normalize_league_rules <- function(info, lineup = NULL, championship_id=NULL) {
  cfg <- info$configuration %||% list()
  scoring <- list(point_system=lineup$pointSystem,custom=lineup$custom)
  version <- as.character(openssl::sha256(charToRaw(jsonlite::toJSON(scoring,auto_unbox=TRUE))))
  list(championship_id=fm_scalar(championship_id %||% info$championshipId),
    season=league_season_context(championship_id %||% info$championshipId,info$season %||% info$seasonId),
    scoring_version=if(is.null(lineup)) "unknown" else version,
    configuration = cfg, initial_budget = fm_number(cfg$budget),
    money_per_point = fm_number(cfg$moneyPerPoint),
    ranking_pool = fm_number(cfg$moneyPerRanking), ranking_mode = fm_scalar(cfg$rankingMode),
    mvp_reward = fm_number(cfg$mvpPlayer), dreamteam_reward = fm_number(cfg$dreamTeamPlayer),
    roster_cap = fm_number(cfg$maxPlayersInRoster),
    captain_enabled = isTRUE(cfg$cpt), captain_multiplier = 2,
    multiposition = isTRUE(lineup$multiposition),
    bench_enabled = isTRUE(lineup$bench$config$enabled),
    bench_mode = fm_scalar(lineup$bench$config$mode, "unknown"),
    bench_size = if(isTRUE(lineup$bench$config$enabled)) 4L else NA_integer_,
    club_limit = NA_integer_, club_limit_status = "unknown", formations = NULL,
    verified = FALSE, # Full formation/club-limit contract still requires verification.
    capabilities_verified = !is.null(lineup) && !identical(attr(lineup,"fetch_status"),"stale"),
    market_mode = fm_scalar(cfg$mtype, "unknown"),
    scoring = lineup$custom %||% list(), point_system = lineup$pointSystem %||% NULL,
    # Unknown deadline/solvency boundary must block unattended mutations.
    deadline = NA_character_, solvency_verified = FALSE)
}
