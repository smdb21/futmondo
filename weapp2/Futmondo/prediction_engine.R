# Offline, source-order-independent prediction helpers. See docs/prediction_engine.md.
# Intervals are model-based scenarios until validated on chronological holdouts.

.pe_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
.pe_col <- function(x, names, default = NA) {
  for (name in names) if (name %in% names(x)) return(x[[name]])
  rep(default, nrow(x))
}
.pe_time <- function(x) {
  if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.numeric(as.POSIXct(x, tz = "UTC")))
  if (is.numeric(x)) return(ifelse(abs(x) > 1e11, x / 1000, x))
  vapply(as.character(x), function(value) {
    if (is.na(value) || !nzchar(value)) return(NA_real_)
    value <- sub("Z$", "", gsub("T", " ", value, fixed = TRUE))
    suppressWarnings(tryCatch(as.numeric(as.POSIXct(value, tz = "UTC")), error = function(e) NA_real_))
  }, numeric(1))
}
.pe_ids <- function(x) {
  x <- trimws(as.character(unlist(x, use.names = FALSE)))
  unique(x[!is.na(x) & nzchar(x)])
}
.pe_key <- function(championship, event) paste(championship, event, sep = "\034")
.pe_empty_bids <- function() data.frame(championship_id = character(), auction_id = character(),
  player_id = character(), manager_id = character(), amount = numeric(), is_winner = logical(),
  settled_at = numeric(), observed_at = numeric(), stringsAsFactors = FALSE)
.pe_past <- function(x, as_of, time_columns = c("observed_at")) {
  if (!is.data.frame(x) || !nrow(x)) return(data.frame())
  keep <- rep(TRUE, nrow(x))
  for (name in time_columns) {
    if (!name %in% names(x)) return(x[FALSE, , drop = FALSE])
    times <- .pe_time(x[[name]])
    keep <- keep & is.finite(times) & times <= .pe_time(as_of)[1]
  }
  x[keep, , drop = FALSE]
}

# Context is an explicit league/season/scoring partition, never a pooling feature.
.pe_context_data <- function(frames, context=NULL, columns=c("championship_id","season","scoring_version")) {
  if (is.null(context)) context <- list()
  if (!is.list(context)) stop("context must be a named list")
  resolved <- list()
  for (column in columns) {
    explicit <- .pe_ids(context[[column]])
    if (length(explicit)>1L) stop(paste("context must select one",column))
    observed <- lapply(frames,function(df) if(is.data.frame(df)&&column%in%names(df)) .pe_ids(df[[column]]) else character())
    if (length(explicit)) selected <- explicit else {
      selected <- if(length(observed)) observed[[1]] else character()
      if(!length(selected)) selected <- unique(unlist(observed,use.names=FALSE))
      if(length(selected)>1L) stop(paste("Mixed",column,"requires an explicit context"))
    }
    if(!length(selected)) next
    resolved[[column]] <- selected
    frames <- lapply(frames,function(df) {
      if(is.data.frame(df)&&column%in%names(df)) df <- df[!is.na(df[[column]]) & as.character(df[[column]])==selected,,drop=FALSE]
      df
    })
  }
  list(data=frames,context=resolved)
}
.pe_add_context <- function(df,context) {
  if(is.data.frame(df)) for(column in names(context)) df[[column]] <- rep(context[[column]],nrow(df))
  df
}

normalize_prediction_auctions <- function(auctions, captured_at = Sys.time()) {
  if (!is.data.frame(auctions)) stop("auctions must be a data.frame")
  n <- nrow(auctions)
  a <- data.frame(championship_id = as.character(.pe_col(auctions, "championship_id", "")),
    auction_id = as.character(.pe_col(auctions, c("auction_id", "event_id", "id"), "")),
    player_id = as.character(.pe_col(auctions, "player_id", "")),
    settled_at = .pe_time(.pe_col(auctions, c("settled_at", "created"))),
    observed_at = .pe_time(.pe_col(auctions, c("observed_at", "captured_at"), .pe_time(captured_at)[1])),
    winner_id = as.character(.pe_col(auctions, c("winner_id", "buyer_team_id"), "")),
    winning_bid = .pe_num(.pe_col(auctions, c("winning_bid", "winning_amount", "price"))),
    reference_value = .pe_num(.pe_col(auctions, "reference_value")),
    reference_observed_at = .pe_time(.pe_col(auctions, "reference_observed_at")),
    visibility = as.character(.pe_col(auctions, c("visibility", "bid_visibility"), "unknown")),
    stringsAsFactors = FALSE)
  if ("bids_visible" %in% names(auctions)) {
    a$visibility <- ifelse(!is.na(auctions$bids_visible) & auctions$bids_visible, "complete", "unknown")
  }
  a$visibility[is.na(a$visibility) | !a$visibility %in% c("complete", "partial", "unknown")] <- "unknown"
  a$winner_id[is.na(a$winner_id)] <- ""
  a$eligible_manager_ids <- I(if ("eligible_manager_ids" %in% names(auctions)) {
    lapply(auctions$eligible_manager_ids, .pe_ids)
  } else rep(list(character()), n))
  issues <- character()
  result <- list()
  for (i in seq_len(n)) {
    if (is.na(a$auction_id[i]) || !nzchar(a$auction_id[i])) {
      issues <- c(issues, "missing_auction_id")
      next
    }
    nested <- if ("bids" %in% names(auctions)) auctions$bids[[i]] else NULL
    if (is.null(nested)) a$visibility[i] <- "unknown"
    if (is.data.frame(nested)) nested <- lapply(seq_len(nrow(nested)), function(j) as.list(nested[j, , drop = FALSE]))
    rows <- list()
    if (length(nested)) for (bid in nested) {
      if (!is.list(bid)) next
      bidder <- bid$manager_id
      if (is.null(bidder)) bidder <- bid$bidder_id
      if (is.null(bidder)) bidder <- bid$team_id
      bidder <- .pe_ids(bidder)
      amount <- .pe_num(if (is.null(bid$amount)) bid$price else bid$amount)
      if (length(bidder) != 1 || length(amount) != 1 || !is.finite(amount) || amount <= 0) {
        issues <- c(issues, "invalid_nested_bid")
        a$visibility[i] <- "partial"
        next
      }
      rows[[length(rows) + 1L]] <- data.frame(manager_id = bidder, amount = amount, is_winner = FALSE)
    }
    if (nzchar(a$winner_id[i]) && is.finite(a$winning_bid[i]) && a$winning_bid[i] > 0) {
      rows[[length(rows) + 1L]] <- data.frame(manager_id = a$winner_id[i], amount = a$winning_bid[i], is_winner = TRUE)
    }
    if (!length(rows)) next
    rows <- do.call(rbind, rows)
    # Preserve the settlement price when a winner is repeated in nested bids.
    rows <- rows[order(rows$is_winner, rows$amount), , drop = FALSE]
    rows <- rows[!duplicated(rows$manager_id, fromLast = TRUE), , drop = FALSE]
    rows$championship_id <- a$championship_id[i]
    rows$auction_id <- a$auction_id[i]
    rows$player_id <- a$player_id[i]
    rows$settled_at <- a$settled_at[i]
    rows$observed_at <- a$observed_at[i]
    result[[length(result) + 1L]] <- rows[names(.pe_empty_bids())]
  }
  bids <- if (length(result)) do.call(rbind, result) else .pe_empty_bids()
  if (nrow(bids)) {
    bids <- bids[order(bids$is_winner, bids$observed_at, na.last = FALSE), , drop = FALSE]
    key <- paste(.pe_key(bids$championship_id, bids$auction_id), bids$manager_id, sep = "\034")
    bids <- bids[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  }
  a <- a[!is.na(a$auction_id) & nzchar(a$auction_id), , drop = FALSE]
  a <- a[order(a$observed_at, na.last = FALSE), , drop = FALSE]
  a <- a[!duplicated(.pe_key(a$championship_id, a$auction_id), fromLast = TRUE), , drop = FALSE]
  rownames(a) <- rownames(bids) <- NULL
  list(auctions = a, bids = bids, issues = unique(issues))
}

fit_rival_bid_model <- function(auctions, bids, manager_ids = NULL, as_of = Sys.time(), prior_strength = 20, context = NULL) {
  if (!is.finite(prior_strength) || prior_strength <= 0) stop("prior_strength must be positive")
  if (!is.data.frame(auctions) || !is.data.frame(bids)) stop("auctions and bids must be data.frames")
  scoped <- .pe_context_data(list(auctions=auctions,bids=bids),context)
  auctions <- scoped$data$auctions; bids <- scoped$data$bids
  a <- auctions
  a$auction_id <- as.character(.pe_col(a, c("auction_id", "event_id"), ""))
  a$championship_id <- as.character(.pe_col(a, "championship_id", ""))
  a$visibility <- as.character(.pe_col(a, c("visibility", "bid_visibility"), "unknown"))
  if ("bids_visible" %in% names(a)) a$visibility <- ifelse(a$bids_visible %in% TRUE, "complete", "unknown")
  a <- .pe_past(a, as_of, c("settled_at", "observed_at"))
  if (nrow(a)) {
    a <- a[order(.pe_time(a$observed_at)), , drop = FALSE]
    a <- a[!duplicated(.pe_key(a$championship_id, a$auction_id), fromLast = TRUE), , drop = FALSE]
  }
  b <- bids
  b$auction_id <- as.character(.pe_col(b, c("auction_id", "event_id"), ""))
  b$championship_id <- as.character(.pe_col(b, "championship_id", ""))
  b$manager_id <- as.character(.pe_col(b, c("manager_id", "bidder_id"), ""))
  b$amount <- .pe_num(.pe_col(b, c("amount", "price")))
  b$is_winner <- .pe_col(b, "is_winner", FALSE) %in% TRUE
  b <- .pe_past(b, as_of, c("settled_at", "observed_at"))
  b <- b[is.finite(b$amount) & b$amount > 0 & !is.na(b$manager_id) & nzchar(b$manager_id), , drop = FALSE]
  if (nrow(b)) {
    b <- b[order(b$is_winner, .pe_time(b$observed_at)), , drop = FALSE]
    key <- paste(.pe_key(b$championship_id, b$auction_id), b$manager_id, sep = "\034")
    b <- b[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  }
  akey <- .pe_key(a$championship_id, a$auction_id)
  bkey <- .pe_key(b$championship_id, b$auction_id)
  match_a <- match(bkey, akey)
  eligible <- if ("eligible_manager_ids" %in% names(a)) lapply(a$eligible_manager_ids, .pe_ids) else rep(list(character()), nrow(a))
  ids <- .pe_ids(c(manager_ids, b$manager_id, unlist(eligible)))
  opportunities <- successes <- setNames(rep(0, length(ids)), ids)
  for (i in seq_len(nrow(a))) if (!is.na(a$visibility[i]) && a$visibility[i] == "complete" && length(eligible[[i]])) {
    participants <- b$manager_id[bkey == akey[i]]
    if (any(!participants %in% eligible[[i]])) next
    opportunities[eligible[[i]]] <- opportunities[eligible[[i]]] + 1
    successes[intersect(eligible[[i]], participants)] <- successes[intersect(eligible[[i]], participants)] + 1
  }
  pooled_p <- if (sum(opportunities) > 0) (sum(successes) + 0.5) / (sum(opportunities) + 1) else NA_real_
  ref <- .pe_num(.pe_col(a, "reference_value"))[match_a]
  ref_time <- .pe_time(.pe_col(a, "reference_observed_at"))[match_a]
  settlement <- .pe_time(.pe_col(a, "settled_at"))[match_a]
  valid_premium <- is.finite(ref) & ref > 0 & is.finite(ref_time) & ref_time < settlement
  premiums <- rep(NA_real_, nrow(b))
  premiums[valid_premium] <- log(b$amount[valid_premium] / ref[valid_premium])
  pool <- premiums[is.finite(premiums)]
  pool_mu <- if (length(pool)) mean(pool) else NA_real_
  # A small numerical floor prevents a degenerate CDF, not a confidence claim.
  pool_sd <- if (length(pool) >= 2) max(stats::sd(pool), 0.01) else NA_real_
  rows <- lapply(ids, function(id) {
    den <- opportunities[id]
    num <- successes[id]
    alpha <- num + prior_strength * pooled_p
    beta <- den - num + prior_strength * (1 - pooled_p)
    x <- premiums[b$manager_id == id & is.finite(premiums)]
    mu <- if (length(pool)) (sum(x) + prior_strength * pool_mu) / (length(x) + prior_strength) else NA_real_
    se <- pool_sd * sqrt(1 / (length(x) + prior_strength) + 1 / max(1, length(pool)))
    data.frame(manager_id = id, participation_probability = alpha / (alpha + beta),
      participation_low = if (is.finite(alpha)) stats::qbeta(0.1, alpha, beta) else NA_real_,
      participation_high = if (is.finite(alpha)) stats::qbeta(0.9, alpha, beta) else NA_real_,
      eligible_opportunities = unname(den), observed_participations = unname(num),
      observed_bids = sum(b$manager_id == id), premium_samples = length(x),
      last_observed_at = if (any(b$manager_id == id)) max(.pe_time(b$observed_at[b$manager_id == id])) else NA_real_,
      log_ratio_mean = mu, log_ratio_sd = pool_sd, log_ratio_mean_se = se,
      conditional_bid_ratio_median = exp(mu),
      conditional_bid_ratio_low = exp(mu - stats::qnorm(0.9) * pool_sd),
      conditional_bid_ratio_high = exp(mu + stats::qnorm(0.9) * pool_sd),
      method = if (!is.finite(pooled_p)) "insufficient_opportunity_visibility" else if (!is.finite(pool_sd)) "insufficient_historical_values" else if (den == 0 || !length(x)) "pooled_prior" else "shrunk_history",
      stringsAsFactors = FALSE)
  })
  managers <- if (length(rows)) do.call(rbind, rows) else data.frame(manager_id = character())
  if (!is.null(manager_ids)) managers <- managers[managers$manager_id %in% .pe_ids(manager_ids), , drop = FALSE]
  rownames(managers) <- NULL
  list(managers = managers, metadata = list(as_of = .pe_time(as_of)[1],
    prior_strength = prior_strength, context = scoped$context, auctions = nrow(a), bids = nrow(b), premium_samples = length(pool),
    pooled_participation = pooled_p, interval = "80% marginal model intervals; not calibrated",
    assumptions = "Eligible complete auctions only for non-bids; manager effects are shrunk toward the league."))
}

predict_auction_win_curve <- function(model, bid_grid, reference_value, user_id = NULL,
                                      active_bids = NULL, ties = "lose") {
  if (!ties %in% c("lose", "win")) stop("ties must be lose or win")
  grid <- sort(unique(.pe_num(bid_grid)))
  grid <- grid[is.finite(grid) & grid >= 0]
  out <- data.frame(bid = grid, p_win = NA_real_, p_win_low = NA_real_, p_win_high = NA_real_,
                    method = rep("insufficient_data", length(grid)))
  if (length(reference_value) != 1 || !is.finite(reference_value) || reference_value <= 0 || !length(grid)) return(out)
  rivals <- model$managers
  if (!is.data.frame(rivals) || !nrow(rivals)) return(out)
  rivals <- rivals[!rivals$manager_id %in% .pe_ids(user_id), , drop = FALSE]
  if (!nrow(rivals)) return(out)
  if (is.data.frame(active_bids) && nrow(active_bids)) {
    active_ids <- .pe_ids(.pe_col(active_bids, c("manager_id", "bidder_id"), ""))
    if (any(!active_ids %in% c(rivals$manager_id, .pe_ids(user_id)))) return(out)
  }
  probabilities <- lower <- upper <- rep(1, length(grid))
  for (i in seq_len(nrow(rivals))) {
    r <- rivals[i, , drop = FALSE]
    p <- r$participation_probability
    pl <- r$participation_low
    ph <- r$participation_high
    bound <- NA_real_
    if (is.data.frame(active_bids) && nrow(active_bids)) {
      active_ids <- as.character(.pe_col(active_bids, c("manager_id", "bidder_id"), ""))
      if (r$manager_id %in% active_ids) {
        p <- pl <- ph <- 1
        amounts <- .pe_num(.pe_col(active_bids, c("amount", "price")))[active_ids == r$manager_id]
        amounts <- amounts[is.finite(amounts) & amounts > 0]
        if (length(amounts)) bound <- max(amounts)
      }
    }
    if (any(!is.finite(c(p, pl, ph, r$log_ratio_mean, r$log_ratio_sd, r$log_ratio_mean_se)))) return(out)
    cdf <- function(mu) {
      f <- stats::plnorm(grid / reference_value, mu, r$log_ratio_sd)
      if (is.finite(bound)) {
        # Conditional on the final bid being at least the visible current bid.
        tail_bound <- stats::plnorm(bound / reference_value, mu, r$log_ratio_sd, lower.tail = FALSE)
        tail_grid <- stats::plnorm(grid / reference_value, mu, r$log_ratio_sd, lower.tail = FALSE)
        f <- if (tail_bound > 0) pmax(0, pmin(1, 1 - tail_grid / tail_bound)) else rep(0, length(grid))
        f[if (ties == "lose") grid <= bound else grid < bound] <- 0
      }
      f
    }
    z <- stats::qnorm(0.9)
    probabilities <- probabilities * (1 - p + p * cdf(r$log_ratio_mean))
    lower <- lower * (1 - ph + ph * cdf(r$log_ratio_mean + z * r$log_ratio_mean_se))
    upper <- upper * (1 - pl + pl * cdf(r$log_ratio_mean - z * r$log_ratio_mean_se))
  }
  out$p_win <- probabilities
  out$p_win_low <- pmin(lower, probabilities)
  out$p_win_high <- pmax(upper, probabilities)
  out[grid == 0, c("p_win", "p_win_low", "p_win_high")] <- 0
  out$method <- "conditional_independence_scenario"
  out
}

forecast_fantasy_points <- function(players, history = NULL, as_of = Sys.time(), horizons = c(1, 3), prior_strength = 5, context = NULL) {
  if (!is.data.frame(players)) stop("players must be a data.frame")
  if (!is.finite(prior_strength) || prior_strength <= 0) stop("prior_strength must be positive")
  horizons <- unique(.pe_num(horizons))
  if (any(!is.finite(horizons) | horizons < 1 | horizons != floor(horizons))) stop("horizons must be positive whole rounds")
  scoped <- .pe_context_data(list(players=players,history=history),context)
  players <- scoped$data$players; history <- scoped$data$history
  ids <- as.character(.pe_col(players, c("player_id", "id"), ""))
  roles <- as.character(.pe_col(players, "role", "Unknown"))
  h <- .pe_past(history, as_of, "observed_at")
  if (nrow(h)) {
    if ("captured_at" %in% names(h)) h <- .pe_past(h, as_of, "captured_at")
    if ("is_final" %in% names(h)) h <- h[h$is_final %in% TRUE, , drop = FALSE]
    if ("score_status" %in% names(h)) h <- h[h$score_status %in% "final", , drop = FALSE]
    if (!"round_id" %in% names(h) && "round" %in% names(h)) h$round_id <- as.character(h$round)
    h$player_id <- as.character(.pe_col(h, c("player_id", "id"), ""))
    h$points <- .pe_num(.pe_col(h, "points"))
    if ("played" %in% names(h)) h$points[!is.na(h$played) & !h$played] <- 0
    h <- h[is.finite(h$points), , drop = FALSE]
    h <- h[order(.pe_time(h$observed_at)), , drop = FALSE]
    round <- if ("round_id" %in% names(h)) as.character(h$round_id) else as.character(h$observed_at)
    key <- paste(h$player_id, round, sep = "\034")
    h <- h[!duplicated(key, fromLast = TRUE), , drop = FALSE]
    h$role <- as.character(.pe_col(h, "role", NA_character_))
    missing_role <- is.na(h$role) | !nzchar(h$role)
    h$role[missing_role] <- roles[match(h$player_id[missing_role], ids)]
  }
  average <- .pe_num(.pe_col(players, c("average.average", "average_points")))
  appearances <- .pe_num(.pe_col(players, c("average.matches", "matches")))
  snapshot_time <- .pe_time(.pe_col(players, "observed_at"))
  usable_snapshot <- is.finite(snapshot_time) & snapshot_time <= .pe_time(as_of)[1]
  average[!usable_snapshot] <- NA_real_
  statuses <- tolower(as.character(.pe_col(players, "status", "unknown")))
  statuses[is.na(statuses) | !nzchar(statuses) | !usable_snapshot] <- "unknown"
  unavailable <- statuses %in% c("injured", "injured2", "redcard", "suspended", "unavailable")
  if ("available" %in% names(players)) unavailable <- unavailable | (usable_snapshot & !is.na(players$available) & !players$available)
  results <- list()
  for (i in seq_len(nrow(players))) {
    x <- if (nrow(h)) h[h$player_id == ids[i], , drop = FALSE] else data.frame()
    peer <- if (nrow(h)) h$points[!is.na(h$role) & !is.na(roles[i]) & h$role == roles[i]] else numeric()
    if (!length(peer) && nrow(h)) peer <- h$points
    prior_mean <- if (length(peer)) mean(peer) else NA_real_
    prior_var <- if (length(peer) >= 2) stats::var(peer) else NA_real_
    method <- "insufficient_data"
    mu <- variance <- NA_real_
    n_eff <- 0
    if (nrow(x)) {
      occurred <- .pe_time(.pe_col(x, c('occurred_at','round_start_at')))
      round_number <- .pe_num(.pe_col(x,'round'))
      # Observation time controls what was knowable, never form recency.
      # Verified match dates take precedence; otherwise use ordinal rounds.
      age <- if(all(is.finite(occurred))) (.pe_time(as_of)[1]-occurred)/86400 else
        if(all(is.finite(round_number))) (max(round_number)-round_number)*7 else
          rep(0,nrow(x))
      w <- exp(-log(2) * age / 35)
      w <- w / max(w)
      n_eff <- sum(w)^2 / sum(w^2)
      xm <- sum(w * x$points) / sum(w)
      if (!is.finite(prior_mean)) prior_mean <- xm
      mu <- (n_eff * xm + prior_strength * prior_mean) / (n_eff + prior_strength)
      local_var <- sum(w * (x$points - xm)^2) / sum(w)
      if (!is.finite(prior_var)) prior_var <- local_var
      variance <- (n_eff * local_var + prior_strength * prior_var) / (n_eff + prior_strength)
      if (nrow(x) < 2 && length(peer) < 2) variance <- NA_real_
      method <- if(all(is.finite(occurred))) "shrunk_round_history" else "shrunk_round_order"
    } else if (is.finite(average[i]) && is.finite(appearances[i]) && appearances[i] > 0) {
      mu <- if (is.finite(prior_mean)) (appearances[i] * average[i] + prior_strength * prior_mean) / (appearances[i] + prior_strength) else average[i]
      method <- "aggregate_average_baseline"
    } else if (is.finite(prior_mean)) {
      mu <- prior_mean
      variance <- prior_var
      method <- "role_prior"
    }
    availability <- if (unavailable[i]) "unavailable_now" else if (statuses[i] == "ok") "reported_available" else "unknown"
    for (horizon in horizons) {
      horizon_mu <- mu * horizon
      deviation <- stats::qnorm(0.9) * sqrt(variance * (horizon + horizon^2 / (n_eff + prior_strength)))
      # Current injury/suspension only establishes the next-round exclusion scenario.
      if (unavailable[i] && horizon == 1) { horizon_mu <- 0; deviation <- 0 }
      if (unavailable[i] && horizon > 1) { horizon_mu <- NA_real_; deviation <- NA_real_ }
      results[[length(results) + 1L]] <- data.frame(player_id = ids[i], horizon = horizon,
        expected_points = horizon_mu, lower = horizon_mu - deviation, upper = horizon_mu + deviation,
        baseline=if(nrow(x))mean(tail(x$points,5))*horizon else average[i]*horizon,
        n_observations = nrow(x), effective_observations = n_eff, availability = availability,
        method = if (unavailable[i]) "current_unavailability_scenario" else method,
        observed_at = if (nrow(x)) max(.pe_time(x$observed_at)) else NA_real_, as_of = .pe_time(as_of)[1],
        stringsAsFactors = FALSE)
    }
  }
  if (length(results)) .pe_add_context(do.call(rbind, results),scoped$context) else data.frame(player_id = character(), horizon = numeric(), expected_points = numeric(), lower = numeric(), upper = numeric())
}

forecast_resale_values <- function(players, history = NULL, as_of = Sys.time(), horizons = c(1, 3, 7),
                                    execution_ratio = NULL, fees = 0, prior_strength = 5, context = NULL) {
  if (!is.data.frame(players)) stop("players must be a data.frame")
  if (!is.finite(prior_strength) || prior_strength <= 0) stop("prior_strength must be positive")
  if (length(fees) != 1 || !is.finite(fees) || fees < 0) stop("fees must be nonnegative")
  horizons <- unique(.pe_num(horizons))
  if (any(!is.finite(horizons) | horizons <= 0)) stop("horizons must be positive days")
  scoped <- .pe_context_data(list(players=players,history=history),context)
  players <- scoped$data$players; history <- scoped$data$history
  ids <- as.character(.pe_col(players, c("player_id", "id"), ""))
  h <- .pe_past(history, as_of, "observed_at")
  if (nrow(h)) {
    if ("captured_at" %in% names(h)) h <- .pe_past(h, as_of, "captured_at")
    h$player_id <- as.character(.pe_col(h, c("player_id", "id"), ""))
    h$value <- .pe_num(.pe_col(h, "value"))
    h <- h[is.finite(h$value) & h$value > 0, , drop = FALSE]
    h$time <- .pe_time(h$observed_at)
    h <- h[order(h$time), , drop = FALSE]
    h <- h[!duplicated(paste(h$player_id, h$time), fromLast = TRUE), , drop = FALSE]
  }
  returns <- list()
  if (nrow(h)) for (id in unique(h$player_id)) {
    x <- h[h$player_id == id, , drop = FALSE]
    if (nrow(x) < 2) next
    # Same-day captures do not become independent daily training observations.
    keep <- 1L
    for (j in seq.int(2L, nrow(x))) if ((x$time[j] - x$time[tail(keep, 1)]) >= 86400) keep <- c(keep, j)
    x <- x[keep, , drop = FALSE]
    if (nrow(x) < 2) next
    days <- diff(x$time) / 86400
    returns[[length(returns) + 1L]] <- data.frame(player_id = id, rate = diff(log(x$value)) / days,
      days = days, end_time = tail(x$time, -1))
  }
  r <- if (length(returns)) do.call(rbind, returns) else data.frame(player_id = character(), rate = numeric(), days = numeric(), end_time = numeric())
  pool_mu <- if (nrow(r)) stats::weighted.mean(r$rate, r$days) else 0
  # Scale residual increments by sqrt(days) so irregular spacing is respected.
  pool_sd <- if (nrow(r) >= 3) max(sqrt(mean((r$rate - pool_mu)^2 * r$days)), 0.005) else NA_real_
  ratios <- if (is.null(execution_ratio)) rep(NA_real_, length(ids)) else if (!is.null(names(execution_ratio))) .pe_num(execution_ratio[ids]) else rep_len(.pe_num(execution_ratio), length(ids))
  ratios[!is.finite(ratios) | ratios <= 0 | ratios > 1] <- NA_real_
  values <- .pe_num(.pe_col(players, "value"))
  player_times <- .pe_time(.pe_col(players, "observed_at"))
  out <- list()
  for (i in seq_along(ids)) {
    x <- if (nrow(h)) h[h$player_id == ids[i], , drop = FALSE] else data.frame()
    value <- if (nrow(x)) tail(x$value, 1) else NA_real_
    timestamp <- if (nrow(x)) tail(x$time, 1) else NA_real_
    if (is.finite(player_times[i]) && player_times[i] <= .pe_time(as_of)[1] &&
        is.finite(values[i]) && values[i] > 0 && (!is.finite(timestamp) || player_times[i] >= timestamp)) {
      value <- values[i]; timestamp <- player_times[i]
    }
    local <- r[r$player_id == ids[i], , drop = FALSE]
    mu <- (sum(local$rate * local$days) + prior_strength * pool_mu) / (sum(local$days) + prior_strength)
    for (horizon in horizons) {
      # Extrapolate from the last known observation through the requested future date.
      elapsed <- if (is.finite(timestamp)) max(0, (.pe_time(as_of)[1] - timestamp) / 86400) + horizon else horizon
      variance <- pool_sd^2 * (elapsed + elapsed^2 / (sum(local$days) + prior_strength))
      mid <- value * exp(mu * elapsed + variance / 2)
      low <- value * exp(mu * elapsed - stats::qnorm(0.9) * sqrt(variance))
      high <- value * exp(mu * elapsed + stats::qnorm(0.9) * sqrt(variance))
      method <- if (!is.finite(value)) "insufficient_data" else if (!is.finite(pool_sd)) "no_change_baseline" else if (!nrow(local)) "pooled_price_prior" else "shrunk_log_return"
      if (!is.finite(pool_sd)) { mid <- value; low <- high <- NA_real_ }
      out[[length(out) + 1L]] <- data.frame(player_id = ids[i], horizon = horizon,
        expected_value = mid, lower_value = low, upper_value = high, baseline=value,
        conservative_proceeds = if (is.finite(low) && is.finite(ratios[i])) max(0, low * ratios[i] - fees) else NA_real_,
        execution_ratio = ratios[i], fees = fees, n_observations = nrow(local), pooled_observations = nrow(r),
        observed_at = timestamp, as_of = .pe_time(as_of)[1], method = method, stringsAsFactors = FALSE)
    }
  }
  if (length(out)) .pe_add_context(do.call(rbind, out),scoped$context) else data.frame(player_id = character(), horizon = numeric(), expected_value = numeric(), lower_value = numeric(), upper_value = numeric(), conservative_proceeds = numeric())
}

select_profit_bid <- function(win_curve, conservative_proceeds, spendable_cash, max_bid = Inf, fees = 0, expected_proceeds=conservative_proceeds) {
  no_bid <- function(reason, candidates = data.frame()) list(recommended_bid = 0, expected_profit = 0,
    win_probability = 0, action = "no_bid", reason = reason, candidates = candidates)
  if (length(expected_proceeds) != 1 || !is.finite(expected_proceeds) || expected_proceeds < 0) return(no_bid("unverified_resale_proceeds"))
  if (length(spendable_cash) != 1 || !is.finite(spendable_cash) || spendable_cash < 0) return(no_bid("unverified_or_negative_cash"))
  if (length(max_bid) != 1 || is.na(max_bid) || max_bid < 0 || length(fees) != 1 || !is.finite(fees) || fees < 0) return(no_bid("invalid_limits"))
  if (!is.data.frame(win_curve) || !all(c("bid", "p_win") %in% names(win_curve))) return(no_bid("missing_win_curve"))
  candidates <- win_curve
  candidates$bid <- .pe_num(candidates$bid)
  candidates$p_win <- .pe_num(candidates$p_win)
  cap <- min(spendable_cash, max_bid)
  valid <- is.finite(candidates$bid) & candidates$bid > 0 & candidates$bid <= cap &
    is.finite(candidates$p_win) & candidates$p_win >= 0 & candidates$p_win <= 1
  candidates <- candidates[valid, , drop = FALSE]
  candidates$expected_profit <- candidates$p_win * (expected_proceeds - candidates$bid - fees)
  candidates$downside_profit <- conservative_proceeds-candidates$bid-fees
  if (!nrow(candidates)) return(no_bid("no_supported_affordable_bid", candidates))
  candidates <- candidates[order(-candidates$expected_profit, candidates$bid), , drop = FALSE]
  if (candidates$expected_profit[1] <= 0) return(no_bid("no_positive_expected_profit", candidates))
  list(recommended_bid = candidates$bid[1], expected_profit = candidates$expected_profit[1],
    win_probability = candidates$p_win[1], downside_profit=candidates$downside_profit[1],
    action = "bid", reason = "maximum_expected_profit", candidates = candidates)
}

evaluate_trade_basket <- function(roster, purchases = data.frame(), sales = data.frame(), verified_cash,
                                  committed_cash = 0, max_roster = Inf, formations = NULL, legal_check = NULL) {
  fail <- function(reasons, cash = NA_real_, squad = roster, lineup = NULL) list(valid = FALSE,
    reasons = unique(reasons), cash_remaining = cash, roster = squad, lineup = lineup)
  if (!is.data.frame(roster) || !is.data.frame(purchases) || !is.data.frame(sales)) return(fail("invalid_dataframes"))
  if (length(verified_cash) != 1 || !is.finite(verified_cash) || length(committed_cash) != 1 ||
      !is.finite(committed_cash) || committed_cash < 0 || length(max_roster) != 1 || is.na(max_roster) || max_roster < 11) return(fail("unverified_financial_limits"))
  for(column in c("championship_id","season","scoring_version")) {
    values <- unique(unlist(lapply(list(roster,purchases,sales),function(df) if(column%in%names(df)) .pe_ids(df[[column]]) else character())))
    if(length(values)>1L) return(fail("cross_context_transactions"))
  }
  ids <- function(x) as.character(.pe_col(x, c("player_id", "id"), ""))
  rid <- ids(roster); pid <- ids(purchases); sid <- ids(sales)
  all_ids <- c(rid, pid, sid)
  if (any(is.na(all_ids) | !nzchar(all_ids)) || anyDuplicated(rid) || anyDuplicated(pid) || anyDuplicated(sid)) return(fail("invalid_or_duplicate_player_ids"))
  if (any(!sid %in% rid)) return(fail("sale_player_not_owned"))
  if (any(pid %in% rid)) return(fail("purchase_player_already_owned"))
  cost <- .pe_num(.pe_col(purchases, c("price", "bid", "amount")))
  proceeds <- .pe_num(.pe_col(sales, c("proceeds", "price")))
  if (any(!is.finite(cost) | cost < 0) || any(!is.finite(proceeds) | proceeds < 0)) return(fail("unverified_transaction_prices"))
  if (nrow(sales) && (!"completed" %in% names(sales) || !all(sales$completed %in% TRUE))) return(fail("sale_proceeds_not_yet_settled"))
  cash <- verified_cash - committed_cash - sum(cost) + sum(proceeds)
  remainder <- roster[!rid %in% sid, , drop = FALSE]
  purchases$id <- pid
  remainder$id <- ids(remainder)
  # Align heterogeneous data frames without losing list columns such as eligible roles.
  columns <- union(names(remainder), names(purchases))
  for (name in setdiff(columns, names(remainder))) remainder[[name]] <- rep(NA, nrow(remainder))
  for (name in setdiff(columns, names(purchases))) purchases[[name]] <- rep(NA, nrow(purchases))
  squad <- tryCatch(rbind(remainder[columns], purchases[columns]), error = function(e) NULL)
  if (is.null(squad)) return(fail("incompatible_roster_schema", cash))
  reasons <- character()
  if (cash < 0) reasons <- c(reasons, "insufficient_cash_for_all_wins")
  if (nrow(squad) > max_roster) reasons <- c(reasons, "roster_capacity_exceeded")
  if (nrow(squad) < 11) reasons <- c(reasons, "fewer_than_eleven_players")
  if (is.null(legal_check) && exists("optimize_starting_xi", mode = "function")) {
    optimizer <- get("optimize_starting_xi", mode = "function")
    legal_check <- function(players) {
      if (is.null(formations)) return(optimizer(players, formation = "auto", rules = list(exclude_unavailable = FALSE)))
      candidates <- lapply(formations, function(formation) optimizer(players, formation = formation, rules = list(exclude_unavailable = FALSE)))
      feasible <- vapply(candidates, function(candidate) is.list(candidate) && isTRUE(candidate$feasible), logical(1))
      if (any(feasible)) candidates[[which(feasible)[1]]] else list(feasible = FALSE)
    }
  }
  legality <- if (is.function(legal_check)) tryCatch(legal_check(squad), error = function(e) list(feasible = FALSE, error = conditionMessage(e))) else NULL
  if (is.null(legality)) reasons <- c(reasons, "legal_xi_unverified") else {
    legal <- if (is.logical(legality) && length(legality) == 1) isTRUE(legality) else is.list(legality) && isTRUE(legality$feasible)
    if (!legal) reasons <- c(reasons, "no_legal_starting_xi")
  }
  list(valid = !length(reasons), reasons = unique(reasons), cash_remaining = cash, roster = squad, lineup = legality)
}


rolling_bid_backtest <- function(auctions, bids, min_train_dates = 3, as_of = Sys.time(),
                                  bid_ratios = c(1, 1.05, 1.1, 1.25), user_id = NULL,
                                  prior_strength = 20, context = NULL) {
  if (!is.data.frame(auctions) || !is.data.frame(bids)) stop("auctions and bids must be data.frames")
  if (length(min_train_dates) != 1 || !is.finite(min_train_dates) || min_train_dates < 1 ||
      min_train_dates != floor(min_train_dates)) stop("min_train_dates must be a positive integer")
  bid_ratios <- sort(unique(.pe_num(bid_ratios)))
  if (!length(bid_ratios) || any(!is.finite(bid_ratios) | bid_ratios <= 0)) stop("bid_ratios must be positive fixed candidates")
  scoped <- .pe_context_data(list(auctions=auctions,bids=bids),context)
  auctions <- scoped$data$auctions; bids <- scoped$data$bids
  a <- auctions
  a$auction_id <- as.character(.pe_col(a, c("auction_id", "event_id"), ""))
  a$championship_id <- as.character(.pe_col(a, "championship_id", ""))
  a$visibility <- as.character(.pe_col(a, c("visibility", "bid_visibility"), "unknown"))
  if ("bids_visible" %in% names(a)) a$visibility <- ifelse(a$bids_visible %in% TRUE, "complete", "unknown")
  a <- .pe_past(a, as_of, c("settled_at", "observed_at"))
  b <- bids
  b$auction_id <- as.character(.pe_col(b, c("auction_id", "event_id"), ""))
  b$championship_id <- as.character(.pe_col(b, "championship_id", ""))
  b$manager_id <- as.character(.pe_col(b, c("manager_id", "bidder_id"), ""))
  b$amount <- .pe_num(.pe_col(b, c("amount", "price")))
  b$is_winner <- .pe_col(b, "is_winner", FALSE) %in% TRUE
  b <- .pe_past(b, as_of, c("settled_at", "observed_at"))
  b <- b[is.finite(b$amount) & b$amount > 0 & !is.na(b$manager_id) & nzchar(b$manager_id), , drop = FALSE]
  empty <- function(status) list(status = status, context = scoped$context, folds = data.frame(), participation = data.frame(),
    premiums = data.frame(), wins = data.frame(), metrics = data.frame(metric = character(), value = numeric(), observations = integer()),
    assumptions = "Expanding UTC settlement-day splits; captures must exist before training cutoff; model-based estimates remain uncalibrated.")
  if (!nrow(a)) return(empty("insufficient_history"))
  a$day <- floor(.pe_time(a$settled_at) / 86400) * 86400
  days <- sort(unique(a$day))
  if (length(days) <= min_train_dates) return(empty("insufficient_history"))
  participation <- premiums <- wins <- folds <- list()
  for (fold_day in days) {
    cutoff <- as.POSIXct(fold_day, origin = "1970-01-01", tz = "UTC")
    train <- a[a$day < fold_day & .pe_time(a$observed_at) < fold_day, , drop = FALSE]
    if (length(unique(train$day)) < min_train_dates) next
    train <- train[order(.pe_time(train$observed_at)), , drop = FALSE]
    train <- train[!duplicated(.pe_key(train$championship_id, train$auction_id), fromLast = TRUE), , drop = FALSE]
    test <- a[a$day == fold_day, , drop = FALSE]
    test <- test[order(.pe_time(test$observed_at)), , drop = FALSE]
    test <- test[!duplicated(.pe_key(test$championship_id, test$auction_id), fromLast = TRUE), , drop = FALSE]
    train_keys <- .pe_key(train$championship_id, train$auction_id)
    train_bids <- b[.pe_key(b$championship_id, b$auction_id) %in% train_keys & .pe_time(b$observed_at) < fold_day, , drop = FALSE]
    known_ids <- if ("eligible_manager_ids" %in% names(test)) .pe_ids(unlist(test$eligible_manager_ids)) else character()
    fitted <- fit_rival_bid_model(train, train_bids, manager_ids = if (length(known_ids)) known_ids else NULL,
      as_of = cutoff, prior_strength = prior_strength)
    folds[[length(folds) + 1L]] <- data.frame(cutoff = fold_day, train_dates = length(unique(train$day)),
      train_auctions = nrow(train), train_bids = fitted$metadata$bids, test_auctions = nrow(test),
      train_max_settled_at = max(.pe_time(train$settled_at)), stringsAsFactors = FALSE)
    for (i in seq_len(nrow(test))) {
      event <- test[i, , drop = FALSE]
      key <- .pe_key(event$championship_id, event$auction_id)
      observed <- b[.pe_key(b$championship_id, b$auction_id) == key, , drop = FALSE]
      if (nrow(observed)) {
        observed <- observed[order(observed$is_winner, .pe_time(observed$observed_at)), , drop = FALSE]
        observed <- observed[!duplicated(observed$manager_id, fromLast = TRUE), , drop = FALSE]
      }
      eligible <- if ("eligible_manager_ids" %in% names(event)) .pe_ids(event$eligible_manager_ids[[1]]) else character()
      complete <- identical(event$visibility[1], "complete") && length(eligible) > 0 && all(observed$manager_id %in% eligible)
      if (complete) for (id in eligible) {
        m <- fitted$managers[fitted$managers$manager_id == id, , drop = FALSE]
        if (!nrow(m) || !is.finite(m$participation_probability)) next
        participation[[length(participation) + 1L]] <- data.frame(cutoff = fold_day,
          championship_id = event$championship_id, auction_id = event$auction_id, manager_id = id,
          prediction = m$participation_probability, observed = as.integer(id %in% observed$manager_id),
          baseline = fitted$metadata$pooled_participation, stringsAsFactors = FALSE)
      }
      ref <- .pe_num(.pe_col(event, "reference_value"))[1]
      ref_time <- .pe_time(.pe_col(event, "reference_observed_at"))[1]
      valid_reference <- is.finite(ref) && ref > 0 && is.finite(ref_time) && ref_time < .pe_time(event$settled_at)[1]
      if (!valid_reference) next
      for (j in seq_len(nrow(observed))) {
        m <- fitted$managers[fitted$managers$manager_id == observed$manager_id[j], , drop = FALSE]
        if (!nrow(m) || !is.finite(m$log_ratio_mean) || !is.finite(m$log_ratio_sd)) next
        premiums[[length(premiums) + 1L]] <- data.frame(cutoff = fold_day,
          championship_id = event$championship_id, auction_id = event$auction_id, manager_id = observed$manager_id[j],
          prediction = m$log_ratio_mean, observed = log(observed$amount[j] / ref),
          lower = m$log_ratio_mean - stats::qnorm(0.9) * m$log_ratio_sd,
          upper = m$log_ratio_mean + stats::qnorm(0.9) * m$log_ratio_sd, stringsAsFactors = FALSE)
      }
      if (!complete) next
      # The grid is chosen before seeing auction outcomes and respects known reserve.
      reserve <- .pe_num(.pe_col(event, c("minimum_bid", "reserve_price")))[1]
      grid <- ref * bid_ratios
      if (is.finite(reserve)) grid <- grid[grid >= reserve]
      event_model <- fitted
      event_model$managers <- fitted$managers[fitted$managers$manager_id %in% eligible, , drop = FALSE]
      curve <- predict_auction_win_curve(event_model, grid, ref, user_id = user_id, ties = "lose")
      curve <- curve[is.finite(curve$p_win), , drop = FALSE]
      if (!nrow(curve)) next
      rival_amounts <- observed$amount[!observed$manager_id %in% .pe_ids(user_id)]
      highest <- if (length(rival_amounts)) max(rival_amounts) else 0
      wins[[length(wins) + 1L]] <- data.frame(cutoff = fold_day,
        championship_id = event$championship_id, auction_id = event$auction_id,
        bid = curve$bid, prediction = curve$p_win, observed = as.integer(curve$bid > highest), stringsAsFactors = FALSE)
    }
  }
  bind <- function(x) if (length(x)) do.call(rbind, x) else data.frame()
  out <- empty("insufficient_test_coverage")
  out$folds <- bind(folds); out$participation <- bind(participation); out$premiums <- bind(premiums); out$wins <- bind(wins)
  metrics <- list()
  metric <- function(name, values) {
    values <- values[is.finite(values)]
    metrics[[length(metrics) + 1L]] <<- data.frame(metric = name,
      value = if (length(values)) mean(values) else NA_real_, observations = length(values))
  }
  p <- out$participation
  if (nrow(p)) {
    clipped <- pmin(1 - 1e-12, pmax(1e-12, p$prediction))
    metric("participation_brier", (p$prediction - p$observed)^2)
    metric("participation_log_loss", -(p$observed * log(clipped) + (1 - p$observed) * log1p(-clipped)))
    metric("pooled_participation_brier", (p$baseline - p$observed)^2)
  }
  q <- out$premiums
  if (nrow(q)) {
    metric("premium_log_mae", abs(q$prediction - q$observed))
    metric("premium_80pct_coverage", as.numeric(q$observed >= q$lower & q$observed <= q$upper))
    metric("premium_lower_pinball", pmax(0.1 * (q$observed - q$lower), -0.9 * (q$observed - q$lower)))
    metric("premium_upper_pinball", pmax(0.9 * (q$observed - q$upper), -0.1 * (q$observed - q$upper)))
  }
  w <- out$wins
  if (nrow(w)) metric("fixed_grid_win_brier", (w$prediction - w$observed)^2)
  out$metrics <- bind(metrics)
  if (nrow(out$metrics)) out$status <- "evaluated"
  if (!nrow(out$folds)) out$status <- "insufficient_history"
  out
}

# Evaluators require historical timestamps; neither fabricates earlier captures.
.pe_validation_metrics <- function(predictions) {
  empty <- data.frame(metric=character(),value=numeric(),observations=integer())
  if (!is.data.frame(predictions) || !nrow(predictions)) return(empty)
  results <- list()
  add <- function(name, values) {
    values <- values[is.finite(values)]
    results[[length(results)+1L]] <<- data.frame(metric=name,
      value=if(length(values)) mean(values) else NA_real_, observations=length(values))
  }
  add('mae',abs(predictions$prediction-predictions$observed))
  add('mse',(predictions$prediction-predictions$observed)^2)
  add('baseline_mae',abs(predictions$baseline-predictions$observed))
  valid <- is.finite(predictions$lower) & is.finite(predictions$upper)
  add('interval_80pct_coverage',as.numeric(predictions$observed[valid]>=predictions$lower[valid] & predictions$observed[valid]<=predictions$upper[valid]))
  add('lower_pinball',pmax(.1*(predictions$observed-predictions$lower),-.9*(predictions$observed-predictions$lower)))
  add('upper_pinball',pmax(.9*(predictions$observed-predictions$upper),-.1*(predictions$observed-predictions$upper)))
  do.call(rbind,results)
}

rolling_point_backtest <- function(players, history, min_train_rounds=3, as_of=Sys.time(), prior_strength=5, context=NULL) {
  empty <- function(status) list(status=status,context=scoped$context,folds=data.frame(),predictions=data.frame(),
    metrics=.pe_validation_metrics(data.frame()),
    assumptions='Expanding whole-round splits at verified first kickoff; captures and observations must precede cutoff. Input must be partitioned by league, season and scoring configuration.')
  if (!is.data.frame(players) || !is.data.frame(history)) stop('players and history must be data.frames')
  scoped <- .pe_context_data(list(players=players,history=history),context)
  players <- scoped$data$players; history <- scoped$data$history
  if (length(min_train_rounds)!=1L || !is.finite(min_train_rounds) || min_train_rounds<1 || min_train_rounds!=floor(min_train_rounds)) stop('min_train_rounds must be a positive integer')
  if (!all(c('round_id','round_start_at','observed_at','points')%in%names(history))) return(empty('insufficient_timing'))
  h <- .pe_past(history,as_of,'observed_at')
  if ('captured_at'%in%names(h)) h <- .pe_past(h,as_of,'captured_at')
  if ('score_status'%in%names(h)) h <- h[h$score_status%in%'final',,drop=FALSE]
  h$player_id <- as.character(.pe_col(h,c('player_id','id'),''))
  h$round_id <- as.character(h$round_id)
  h$points <- .pe_num(h$points)
  if ('played'%in%names(h)) h$points[!is.na(h$played)&!h$played] <- 0
  h$round_start_at <- .pe_time(h$round_start_at)
  h <- h[is.finite(h$points)&is.finite(h$round_start_at)&!is.na(h$round_id)&nzchar(h$round_id)&!is.na(h$player_id)&nzchar(h$player_id),,drop=FALSE]
  if (!nrow(h)) return(empty('insufficient_history'))
  rounds <- unique(h$round_id)
  starts <- vapply(rounds,function(round)min(h$round_start_at[h$round_id==round]),numeric(1))
  rounds <- rounds[order(starts)]; starts <- sort(starts)
  predictions <- folds <- list()
  player_ids <- as.character(.pe_col(players,c('player_id','id'),''))
  for (i in seq_along(rounds)) {
    cutoff <- unname(starts[i])
    train <- h[h$round_start_at<cutoff & h$round_id!=rounds[i] & .pe_time(h$observed_at)<cutoff,,drop=FALSE]
    if ('captured_at'%in%names(train)) train <- train[.pe_time(train$captured_at)<cutoff,,drop=FALSE]
    if (length(unique(train$round_id))<min_train_rounds) next
    train <- train[order(.pe_time(train$observed_at)),,drop=FALSE]
    train <- train[!duplicated(.pe_key(train$player_id,train$round_id),fromLast=TRUE),,drop=FALSE]
    test <- h[h$round_id==rounds[i] & h$player_id%in%player_ids,,drop=FALSE]
    test <- test[order(.pe_time(test$observed_at)),,drop=FALSE]
    test <- test[!duplicated(test$player_id,fromLast=TRUE),,drop=FALSE]
    if (!nrow(test)) next
    cohort <- players[match(test$player_id,player_ids),,drop=FALSE]
    f <- forecast_fantasy_points(cohort,train,as_of=as.POSIXct(cutoff,origin='1970-01-01',tz='UTC'),horizons=1,prior_strength=prior_strength)
    baseline <- vapply(test$player_id,function(id) {
      own <- train$points[train$player_id==id]
      if(length(own)) mean(tail(own,5)) else mean(train$points)
    },numeric(1))
    predictions[[length(predictions)+1L]] <- data.frame(cutoff=cutoff,round_id=rounds[i],player_id=test$player_id,
      prediction=f$expected_points,observed=test$points,lower=f$lower,upper=f$upper,baseline=baseline,
      method=f$method,stringsAsFactors=FALSE)
    folds[[length(folds)+1L]] <- data.frame(cutoff=cutoff,round_id=rounds[i],train_rounds=length(unique(train$round_id)),
      train_observations=nrow(train),test_observations=nrow(test),train_max_observed_at=max(.pe_time(train$observed_at)))
  }
  out <- empty('insufficient_history')
  if (length(folds)) {
    out$folds <- do.call(rbind,folds);out$predictions <- do.call(rbind,predictions)
    out$metrics <- .pe_validation_metrics(out$predictions)
    out$status <- if(any(is.finite(out$predictions$prediction))) 'evaluated' else 'insufficient_test_coverage'
  }
  out
}

rolling_resale_backtest <- function(players, history, min_train_dates=4, as_of=Sys.time(),
                                     horizons=c(1,3,7), tolerance_hours=12, prior_strength=5, context=NULL) {
  empty <- function(status) list(status=status,context=scoped$context,folds=data.frame(),predictions=data.frame(),
    metrics=.pe_validation_metrics(data.frame()),
    assumptions='Expanding UTC-day splits; timestamped prices and capture availability precede cutoff. Targets use first observation at/after horizon within tolerance. Valuation error is not realized trading profit.')
  if (!is.data.frame(players) || !is.data.frame(history)) stop('players and history must be data.frames')
  scoped <- .pe_context_data(list(players=players,history=history),context)
  players <- scoped$data$players; history <- scoped$data$history
  if (length(min_train_dates)!=1L || !is.finite(min_train_dates) || min_train_dates<1 || min_train_dates!=floor(min_train_dates)) stop('min_train_dates must be a positive integer')
  horizons <- sort(unique(.pe_num(horizons)))
  if (!length(horizons)||any(!is.finite(horizons)|horizons<=0)) stop('horizons must be positive days')
  if (length(tolerance_hours)!=1L || !is.finite(tolerance_hours) || tolerance_hours<0 || tolerance_hours>24) stop('tolerance_hours must be between 0 and 24')
  if (!all(c('observed_at','value')%in%names(history))) return(empty('insufficient_timing'))
  h <- .pe_past(history,as_of,'observed_at')
  if ('captured_at'%in%names(h)) h <- .pe_past(h,as_of,'captured_at')
  h$player_id <- as.character(.pe_col(h,c('player_id','id'),''))
  h$value <- .pe_num(h$value);h$time <- .pe_time(h$observed_at)
  h <- h[is.finite(h$value)&h$value>0&!is.na(h$player_id)&nzchar(h$player_id),,drop=FALSE]
  if (!nrow(h)) return(empty('insufficient_history'))
  h <- h[order(h$time),,drop=FALSE]
  h <- h[!duplicated(.pe_key(h$player_id,h$time),fromLast=TRUE),,drop=FALSE]
  days <- sort(unique(floor(h$time/86400)*86400))
  ids <- .pe_ids(.pe_col(players,c('player_id','id'),''))
  predictions <- folds <- list()
  for (cutoff in days) {
    train <- h[h$time<cutoff,,drop=FALSE]
    if ('captured_at'%in%names(train)) train <- train[.pe_time(train$captured_at)<cutoff,,drop=FALSE]
    train_dates <- length(unique(floor(train$time/86400)))
    if (train_dates<min_train_dates) next
    snapshot <- train[train$player_id%in%ids,,drop=FALSE]
    snapshot <- snapshot[!duplicated(snapshot$player_id,fromLast=TRUE),,drop=FALSE]
    if (!nrow(snapshot)) next
    f <- forecast_resale_values(snapshot,train,as_of=as.POSIXct(cutoff,origin='1970-01-01',tz='UTC'),horizons=horizons,prior_strength=prior_strength)
    fold_rows <- 0L
    for (j in seq_len(nrow(f))) {
      target <- cutoff+86400*f$horizon[j]
      test <- h[h$player_id==f$player_id[j]&h$time>=target&h$time<=target+tolerance_hours*3600,,drop=FALSE]
      if (!nrow(test)) next
      test <- test[1,,drop=FALSE]
      predictions[[length(predictions)+1L]] <- data.frame(cutoff=cutoff,player_id=f$player_id[j],horizon=f$horizon[j],
        target_at=target,outcome_at=test$time,prediction=f$expected_value[j],observed=test$value,
        lower=f$lower_value[j],upper=f$upper_value[j],baseline=snapshot$value[match(f$player_id[j],snapshot$player_id)],
        method=f$method[j],stringsAsFactors=FALSE)
      fold_rows <- fold_rows+1L
    }
    if (fold_rows) folds[[length(folds)+1L]] <- data.frame(cutoff=cutoff,train_dates=train_dates,
      train_observations=nrow(train),test_observations=fold_rows,train_max_observed_at=max(train$time))
  }
  out <- empty('insufficient_history')
  if(length(folds)) {
    out$folds <- do.call(rbind,folds);out$predictions <- do.call(rbind,predictions)
    out$metrics <- do.call(rbind,lapply(sort(unique(out$predictions$horizon)),function(horizon) {
      metrics <- .pe_validation_metrics(out$predictions[out$predictions$horizon==horizon,,drop=FALSE])
      metrics$horizon <- horizon;metrics
    }))
    out$status <- 'evaluated'
  }
  out
}
