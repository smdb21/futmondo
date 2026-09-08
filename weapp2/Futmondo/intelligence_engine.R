# ============================================================
# Futmondo Intelligence Engine
# ============================================================
# Provides FIS scoring, smart bidding, manager DNA profiling,
# and command-center recommendation feeds.
# ============================================================

library(dplyr)

# ---- Helper: safe numeric coercion ----
safe_numeric <- function(x, default = NA_real_) {
  if (is.null(x)) return(rep(default, 1))
  vals <- suppressWarnings(as.numeric(as.character(x)))
  vals[is.na(vals) | is.nan(vals)] <- default
  vals
}

# ---- Helper: clamp a value between lo and hi ----
safe_clamp <- function(x, lo = 0, hi = 100) {
  pmin(pmax(x, lo), hi)
}

# ---- Helper: NULL / scalar-NA safe default (local, no %||% dependency) ----
# Returns `default` when x is NULL or a length-1 NA; otherwise returns x.
# Defined locally so calculate_fis_score never relies on an undefined %||%.
default_if_null_na <- function(x, default) {
  if (is.null(x)) return(default)
  if (length(x) == 1 && is.na(x)) return(default)
  x
}


# ============================================================
# 1. calculate_fis_score
# ============================================================
# Computes a composite Futmondo Intelligence Score (FIS) for each
# player in a data frame.  Returns the enriched data frame with
# new columns: perf, form, efficiency, momentum, fixture_risk,
# fis_score, fis_tier, fis_summary.
#
# Parameters:
#   players_df  -- data frame with at least: id, points, value, change,
#                  average.average, average.averageLastFive,
#                  average.matches, role, status
#   weights     -- optional named list with numeric weights for
#                  perf, form, efficiency, momentum, fixture_risk.
#                  Defaults to c(perf=0.30, form=0.20, efficiency=0.20,
#                                momentum=0.15, fixture_risk=0.15).
#
# Returns:
#   Enriched players_df with FIS columns appended.
# ============================================================

# Descriptive, cohort-independent indicators. These are not predictions.
analytics_numeric_column <- function(df, column, default = NA_real_) {
  if (!column %in% names(df)) return(rep(default, nrow(df)))
  x <- suppressWarnings(as.numeric(as.character(df[[column]])))
  x[!is.finite(x)] <- default
  x
}

calculate_fis_score <- function(players_df, weights = NULL) {
  if (is.null(players_df) || nrow(players_df) == 0) return(players_df)
  defaults <- c(perf = .30, form = .20, efficiency = .20, momentum = .15, fixture_risk = .15)
  w <- defaults
  if (!is.null(weights)) for (key in names(defaults)) {
    candidate <- weights[[key]]
    if (is.numeric(candidate) && length(candidate) == 1L && is.finite(candidate) && candidate >= 0) w[key] <- candidate
  }
  if (sum(w) <= 0) w <- defaults
  w <- w / sum(w)
  avg <- analytics_numeric_column(players_df, 'average.average')
  matches <- analytics_numeric_column(players_df, 'average.matches')
  points <- analytics_numeric_column(players_df, 'points')
  missing_avg <- !is.finite(avg) & is.finite(matches) & matches > 0 & is.finite(points)
  avg[missing_avg] <- points[missing_avg] / matches[missing_avg]
  avg[is.finite(matches) & matches == 0] <- NA_real_
  last5 <- analytics_numeric_column(players_df, 'average.averageLastFive')
  value <- analytics_numeric_column(players_df, 'value')
  change <- analytics_numeric_column(players_df, 'change')
  status <- if ('status' %in% names(players_df)) tolower(trimws(as.character(players_df$status))) else rep(NA_character_, nrow(players_df))
  perf <- 100 * (1 - exp(-pmax(avg, 0) / 6))
  form <- safe_clamp(50 + 50 * (last5 - avg) / pmax(abs(avg), 1))
  efficiency <- ifelse(value > 0, 100 * pmax(avg, 0) / (pmax(avg, 0) + value / 1e6), NA_real_)
  momentum <- ifelse(value > 0, 50 + 50 * tanh(20 * change / value), NA_real_)
  availability <- rep(NA_real_, nrow(players_df))
  availability[status %in% c('ok', 'available', 'healthy')] <- 100
  availability[status %in% c('doubt', 'doubtful')] <- 60
  availability[status %in% c('injured', 'injured2', 'redcard', 'suspended', 'unavailable')] <- 0
  pillars <- cbind(perf, form, efficiency, momentum, fixture_risk = availability)
  observed <- is.finite(pillars)
  weighted <- sweep(pillars, 2, w, '*'); weighted[!observed] <- 0
  denominator <- as.vector(observed %*% w)
  score <- ifelse(denominator > 0, rowSums(weighted) / denominator, NA_real_)
  score[!is.finite(avg)] <- NA_real_
  coverage <- rowMeans(observed)
  tier <- ifelse(!is.finite(score), 'Unavailable', ifelse(score >= 80, 'Strong Buy', ifelse(score >= 65, 'Buy', ifelse(score >= 45, 'Hold', 'Sell'))))
  players_df$perf <- perf; players_df$form <- form; players_df$efficiency <- efficiency
  players_df$momentum <- momentum; players_df$fixture_risk <- availability
  players_df$fis_score <- round(score, 2); players_df$fis_tier <- tier
  players_df$data_coverage <- coverage
  players_df$fis_status <- ifelse(!is.finite(score), 'unavailable', ifelse(coverage < 1, 'partial', 'ok'))
  players_df$fis_summary <- ifelse(is.finite(score),
    paste0('Descriptive rating ', round(score, 1), '/100; ', round(coverage * 100), '% of indicators observed. Use forecasts, acquisition cost and your roster before acting.'),
    'Insufficient observed match data for a rating.')
  players_df
}


# ============================================================
# 2. calculate_smart_bid
# ============================================================
# Computes a structured smart-bid recommendation for a single
# player.  Uses market data, pressroom history, and league
# context to derive fair value, recommended bid, and ROI.
#
# Parameters:
#   player_row       -- single-row data frame or list with:
#                       id, name, value, change, points, role,
#                       average.average, average.averageLastFive,
#                       average.matches, status, clause_price
#   championship_id  -- character string
#   pressroom_df     -- optional data frame of pressroom transactions
#   user_teams_df    -- optional data frame of user teams
#   user_cash        -- deprecated advisory input (default NA); never verifies funds.
#                       An authoritative capacity snapshot is required.
#   market_high_bid  -- optional numeric, the current highest competing
#                       bid on this player (from live market/summary data).
#   capacity         -- optional list returned by get_acquisition_capacity().
#                       When status == "ok", its verified spendable funds
#                       bound the recommendation.
#
# Returns:
#   List with: fair_value, league_premium_pct, min_winning_bid,
#   recommended_bid, max_rational_bid, expected_roi_pct,
#   competition_level, likely_competitors, confidence_pct,
#   spendable_funds, funds_verified, market_high_bid
# ============================================================

calculate_smart_bid <- function(player_row, championship_id,
                                 pressroom_df = NULL,
                                 user_teams_df = NULL,
                                 user_cash = NA_real_,
                                 market_high_bid = NULL,
                                 capacity = NULL) {
  if (is.null(player_row)) {
    return(list(error = "player_row is NULL"))
  }

  tryCatch({
    # Coerce to list if data frame
    if (is.data.frame(player_row)) {
      if (nrow(player_row) > 1) player_row <- player_row[1, , drop = FALSE]
      p <- as.list(player_row)
    } else {
      p <- player_row
    }

    player_value <- safe_numeric(p$value, 0)
    player_change <- safe_numeric(p$change, 0)
    player_points <- safe_numeric(p$points, 0)
    player_avg <- safe_numeric(p$average.average, 0)
    player_avg5 <- safe_numeric(p$average.averageLastFive, 0)
    player_matches <- safe_numeric(p$average.matches, 0)
    player_status <- if (!is.null(p$status)) as.character(p$status) else "ok"

    # A numeric user_cash argument cannot establish withheld funds or commitments.
    spendable <- NA_real_
    funds_verified <- FALSE
    if (is.list(capacity) && identical(capacity$status,"ok") && is.list(capacity$funds)) {
      available <- capacity$funds$spendable_budget
      if (is.numeric(available) && length(available)==1L && is.finite(available) && available>=0) {
        spendable <- available
        funds_verified <- TRUE
      }
    }
    if (!funds_verified) return(list(error="Verified available funds are required before recommending a bid.",
      recommended_bid=0,max_rational_bid=NA_real_,spendable_funds=NA_real_,funds_verified=FALSE,
      can_compete=FALSE,action="no_bid",confidence_pct=NA_real_,expected_roi_pct=NA_real_,
      data_coverage_pct=NA_real_,method="unverified_funds",calibrated=FALSE))

    api_bid_limit <- Inf
    if ("api_bid_limit" %in% names(capacity$funds)) {
      limit <- capacity$funds$api_bid_limit
      if (!is.numeric(limit) || length(limit)!=1L || !is.finite(limit) || limit<0)
        return(list(error="The API bidding limit is unavailable.",recommended_bid=0,max_rational_bid=NA_real_,
          spendable_funds=spendable,funds_verified=TRUE,api_bid_limit=NA_real_,can_compete=FALSE,action="no_bid",
          confidence_pct=NA_real_,expected_roi_pct=NA_real_,data_coverage_pct=NA_real_,calibrated=FALSE))
      api_bid_limit <- limit
    }

    # ---- Market high bid (live competing bid) ----
    mhb <- NULL
    if (!is.null(market_high_bid) && length(market_high_bid) == 1 &&
        is.numeric(market_high_bid) && is.finite(market_high_bid) && market_high_bid > 0) {
      mhb <- market_high_bid
    }

    # ---- fair_value: base value adjusted by form and momentum ----
    form_factor <- 1.0
    if (player_avg > 0 && !is.na(player_avg5)) {
      form_ratio <- player_avg5 / player_avg
      form_factor <- 0.8 + 0.4 * (form_ratio - 0.5)  # 0.5->0.8, 1.0->1.0, 1.5->1.2
    }
    form_factor <- safe_clamp(form_factor, 0.5, 1.5)

    # Status penalty
    status_factor <- ifelse(tolower(player_status) == "ok", 1.0,
                        ifelse(tolower(player_status) == "doubt", 0.85,
                        ifelse(tolower(player_status) %in% c("injured", "injured2"), 0.6,
                        ifelse(tolower(player_status) == "redcard", 0.7, 1.0))))

    fair_value <- round(player_value * form_factor * status_factor)

    # ---- league_premium_pct: how much above fair value the market pays ----
    if (!is.null(pressroom_df) && nrow(pressroom_df) > 0 && !is.na(player_value) && player_value > 0) {
      player_tx <- pressroom_df[pressroom_df$player_id == p$id, ]
      if (nrow(player_tx) > 0) {
        avg_tx_price <- mean(suppressWarnings(as.numeric(player_tx$price)), na.rm = TRUE)
        league_premium_pct <- round((avg_tx_price / fair_value - 1) * 100, 2)
      } else {
        league_premium_pct <- 0
      }
    } else {
      league_premium_pct <- 0
    }

    # ---- min_winning_bid: fair_value + small premium, or just above the
    #      current market high bid (to actually win the auction) ----
    base_min <- round(fair_value * 1.02)
    if (!is.null(mhb)) {
      min_winning_bid <- max(base_min, round(mhb * 1.01))
    } else {
      min_winning_bid <- base_min
    }

    # ---- max_rational_bid: rational value guardrail (150% of fair value)
    #      bounded by verified spendable funds ----
    max_rational_bid <- min(round(fair_value * 1.5), spendable, api_bid_limit)

    # ---- recommended_bid: balance between winning and value, bounded by
    #      the rational guardrail and verified spendable funds ----
    recommended_raw <- round(fair_value * (1 + league_premium_pct / 200))
    recommended_bid <- min(recommended_raw, max_rational_bid)
    # Never recommend below the minimum winning bid (when affordable)
    if (min_winning_bid <= max_rational_bid) {
      recommended_bid <- max(recommended_bid, min_winning_bid)
    }
    recommended_bid <- min(recommended_bid, spendable, max_rational_bid)
    can_compete <- min_winning_bid <= max_rational_bid

    valuation_discount_pct <- if(recommended_bid>0) round((fair_value/recommended_bid-1)*100,2) else NA_real_

    # ---- competition_level ----
    if (!is.null(pressroom_df) && nrow(pressroom_df) > 0) {
      player_tx <- pressroom_df[pressroom_df$player_id == p$id, ]
      buyer_count <- length(unique(player_tx$buyer_team_id[
        !is.na(player_tx$buyer_team_id) & player_tx$buyer_team_id != ""
      ]))
      if (buyer_count >= 3) {
        competition_level <- "High"
      } else if (buyer_count >= 1) {
        competition_level <- "Medium"
      } else {
        competition_level <- "Low"
      }
    } else {
      competition_level <- "Unknown"
    }

    # ---- likely_competitors ----
    if (!is.null(pressroom_df) && nrow(pressroom_df) > 0) {
      player_tx <- pressroom_df[pressroom_df$player_id == p$id, ]
      competitors <- unique(player_tx$buyer_team_id[
        !is.na(player_tx$buyer_team_id) & player_tx$buyer_team_id != ""
      ])
      # Replace empty/NA with "Futmondo / Mercado"
      competitors[is.na(competitors) | competitors == ""] <- "Futmondo / Mercado"
      likely_competitors <- as.list(competitors)
    } else {
      likely_competitors <- list()
    }

    list(
      fair_value = fair_value,
      league_premium_pct = league_premium_pct,
      min_winning_bid = min_winning_bid,
      recommended_bid = recommended_bid,
      max_rational_bid = max_rational_bid,
      expected_roi_pct = NA_real_,
      valuation_discount_pct = valuation_discount_pct,
      competition_level = competition_level,
      likely_competitors = likely_competitors,
      confidence_pct = NA_real_,
      data_coverage_pct = NA_real_,
      spendable_funds = spendable,
      funds_verified = funds_verified,
      api_bid_limit = if(is.finite(api_bid_limit)) api_bid_limit else NA_real_,
      market_high_bid = mhb,
      can_compete = can_compete,
      method = "descriptive_heuristic",
      calibrated = FALSE
    )
  }, error = function(e) {
    print(paste0("[SmartBid] Error computing smart bid: ", e$message))
    list(error = e$message)
  })
}


# ============================================================
# 3. calculate_manager_dna
# ============================================================
# Computes a behavioral profile for a manager based on their
# pressroom transaction history.
#
# Parameters:
#   team_id        -- character, the user team ID
#   pressroom_df   -- data frame of pressroom transactions
#   user_teams_df  -- optional data frame of user teams
#
# Returns:
#   List with: team_id, aggressiveness, avg_overpayment_pct,
#   fav_position, trading_frequency, avg_holding_days,
#   total_trades, insights
# ============================================================

analytics_time <- function(x) {
  x <- as.character(x)
  result <- suppressWarnings(as.POSIXct(x, format = '%Y-%m-%dT%H:%M:%OS', tz = 'UTC'))
  missing <- is.na(result)
  if (any(missing)) result[missing] <- suppressWarnings(as.POSIXct(x[missing], format = '%Y-%m-%d %H:%M:%OS', tz = 'UTC'))
  result
}

calculate_manager_dna <- function(team_id, pressroom_df, user_teams_df = NULL) {
  result <- list(team_id = team_id, aggressiveness = NA_real_, avg_overpayment_pct = NA_real_,
    fav_position = 'Unknown', trading_frequency = 0, avg_holding_days = NA_real_, total_trades = 0,
    insights = 'Insufficient observed transactions.', premium_observations = 0L, method = 'observed_transactions')
  if (length(team_id) != 1L || is.na(team_id) || !nzchar(team_id)) return(result)
  if (!is.data.frame(pressroom_df) || !nrow(pressroom_df) ||
      !all(c('buyer_team_id', 'seller_team_id', 'player_id', 'price', 'created') %in% names(pressroom_df))) return(result)
  tx <- pressroom_df
  if ('id' %in% names(tx)) tx <- tx[!duplicated(tx$id), , drop = FALSE]
  tx <- tx[tx$buyer_team_id %in% team_id | tx$seller_team_id %in% team_id, , drop = FALSE]
  if (!nrow(tx)) return(result)
  buy <- tx$buyer_team_id %in% team_id
  result$total_trades <- nrow(tx)
  prices <- analytics_numeric_column(tx, 'price')
  reference <- rep(NA_real_, nrow(tx))
  for (column in c('market_value_at_time', 'reference_value', 'value_at_transaction')) {
    if (column %in% names(tx)) {
      candidate <- analytics_numeric_column(tx, column)
      use <- !is.finite(reference) & is.finite(candidate) & candidate > 0
      reference[use] <- candidate[use]
    }
  }
  usable <- buy & is.finite(prices) & is.finite(reference) & reference > 0
  if (any(usable)) {
    premium <- prices[usable] / reference[usable] - 1
    result$avg_overpayment_pct <- round(mean(premium) * 100, 2)
    result$premium_observations <- sum(usable)
    result$aggressiveness <- round(safe_clamp(50 + 100 * stats::median(premium)), 1)
  }
  if ('role' %in% names(tx) && any(buy)) {
    roles <- as.character(tx$role[buy]); roles <- roles[!is.na(roles) & nzchar(roles)]
    if (length(roles)) result$fav_position <- names(sort(table(roles), decreasing = TRUE))[1]
  }
  dates <- analytics_time(tx$created)
  valid <- !is.na(dates)
  if (any(valid)) {
    span <- max(1, as.numeric(difftime(max(dates[valid]), min(dates[valid]), units = 'days')) + 1)
    result$trading_frequency <- round(sum(valid) / span * 7, 2)
  }
  holdings <- numeric()
  for (pid in unique(tx$player_id)) {
    idx <- which(tx$player_id %in% pid & valid)
    idx <- idx[order(dates[idx], seq_along(idx))]
    acquired <- as.POSIXct(character(), tz = 'UTC')
    for (i in idx) {
      if (buy[i]) acquired <- c(acquired, dates[i])
      else if (length(acquired)) {
        holdings <- c(holdings, as.numeric(difftime(dates[i], acquired[1], units = 'days')))
        acquired <- acquired[-1]
      }
    }
  }
  if (length(holdings)) result$avg_holding_days <- round(mean(holdings[holdings >= 0]), 2)
  result$insights <- paste0(nrow(tx), ' observed transfers; ', result$trading_frequency,
    ' per observed week. ', if (result$premium_observations > 0)
      paste0('Average premium ', result$avg_overpayment_pct, '% from ', result$premium_observations, ' historical reference prices.')
      else 'Historical reference prices unavailable; overpayment cannot be estimated.')
  result
}


# ============================================================
# 4. generate_command_center_feed
# ============================================================
# Generates top daily actionable manager recommendations.
#
# Parameters:
#   login             -- login token vector
#   championship_id   -- character string
#   user_team_id      -- character, the current user's team
#   user_teams_df     -- data frame of all user teams
#   players_df        -- data frame of players (should have FIS scores)
#   pressroom_df      -- optional pressroom transactions
#   market_candidates -- optional data frame of pre-filtered market buy
#                        candidates (supplied by the Today module: system
#                        listings by default, rival listings when opted in).
#                        When supplied (even 0-row), the BUY section is built
#                        EXCLUSIVELY from it (action_code "market_bid"); when
#                        NULL, the legacy players_df-based behavior is kept.
#   clause_candidates -- optional data frame of pre-filtered strict open
#                        rival clause candidates (supplied by the Today
#                        module). When supplied (even 0-row), the CLAUSE
#                        section is built EXCLUSIVELY from it (action_code
#                        "clause_buyout"); when NULL, the legacy
#                        players_df-based behavior is kept.
#
# Dual-route rule: when a player appears in BOTH candidate sets, a SINGLE
# clause recommendation is emitted (no separate Buy card). The value
# max(market price, clause price) is included in the description as comparison
# metadata ONLY; the executed price is always the clause price.
#
# Returns:
#   Data frame with: type, title, description, confidence_pct,
#   action_label, action_code, player_id
#   (action_code is a stable code: "market_bid" / "clause_buyout" / "view")
# ============================================================

generate_command_center_feed <- function(login, championship_id,
                                          user_team_id, user_teams_df,
                                          players_df, pressroom_df = NULL,
                                          market_candidates = NULL,
                                          clause_candidates = NULL) {
  empty_feed <- data.frame(
    type = character(0), title = character(0), description = character(0),
    confidence_pct = numeric(0), action_label = character(0),
    action_code = character(0), player_id = character(0), stringsAsFactors = FALSE
  )

  has_players <- !is.null(players_df) && is.data.frame(players_df) && nrow(players_df) > 0
  has_market_cand <- is.data.frame(market_candidates)
  has_clause_cand <- is.data.frame(clause_candidates)

  if (!has_players && !has_market_cand && !has_clause_cand) {
    return(empty_feed)
  }

  # Ensures FIS columns exist on a candidate data frame (defensive: Today
  # pre-computes them, but the feed must not assume it).
  ensure_candidate_fis <- function(df) {
    if (nrow(df) == 0) return(df)
    if (!"fis_score" %in% colnames(df) || !"fis_tier" %in% colnames(df)) {
      df <- calculate_fis_score(df)
    }
    df$fis_score <- safe_numeric(df$fis_score, 50)
    df$fis_score[!is.finite(df$fis_score)] <- 50
    df$fis_tier <- ifelse(is.na(df$fis_tier), "Hold", as.character(df$fis_tier))
    df
  }

  # Formats a monetary amount as a plain (non-scientific) whole number for
  # human-readable recommendation descriptions.
  fmt_money <- function(x) format(round(x, 0), scientific = FALSE)

  tryCatch({
    recommendations <- list()

    # ---- Ensure FIS scores exist ----
    if (has_players && !"fis_score" %in% colnames(players_df)) {
      players_df <- calculate_fis_score(players_df)
    }

    # ---- BUY recommendations ----
    if (has_market_cand) {
      # Policy path: Buy/Place Bid recommendations are built EXCLUSIVELY from
      # the supplied market candidates (Today pre-filters them to explicit
      # system listings by default, rival listings when opted in).
      mc <- ensure_candidate_fis(market_candidates)
      if (nrow(mc) > 0) {
        # Dual-route dedupe: a player also covered by a clause candidate gets
        # a SINGLE clause recommendation (clause price is executed).
        clause_ids <- if (has_clause_cand && nrow(clause_candidates) > 0) as.character(clause_candidates$id) else character(0)
        buy_pool <- mc[!as.character(mc$id) %in% clause_ids, ]
        buy_pool <- buy_pool[buy_pool$fis_tier %in% c("Strong Buy", "Buy"), ]
        if (nrow(buy_pool) > 0) {
          buy_pool <- buy_pool[order(-buy_pool$fis_score), ]
          top_buys <- head(buy_pool, 3)

          for (i in seq_len(nrow(top_buys))) {
            p <- top_buys[i, ]
            recommendations[[length(recommendations) + 1]] <- data.frame(
              type = "Buy",
              title = paste0("BUY: ", p$name),
              description = if (!is.na(p$fis_summary)) p$fis_summary else "",
              confidence_pct = p$fis_score,
              action_label = "Place Bid",
              action_code = "market_bid",
              player_id = as.character(p$id),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    } else if (has_players) {
      # Legacy path (NULL candidates): Strong Buy / Buy tier players in
      # players_df.
      buy_candidates <- players_df[
        players_df$fis_tier %in% c("Strong Buy", "Buy"),
      ]
      if (nrow(buy_candidates) > 0) {
        # Sort by FIS score descending, take top 3
        buy_candidates <- buy_candidates[order(-buy_candidates$fis_score), ]
        top_buys <- head(buy_candidates, 3)

        for (i in seq_len(nrow(top_buys))) {
          p <- top_buys[i, ]
          recommendations[[length(recommendations) + 1]] <- data.frame(
            type = "Buy",
            title = paste0("BUY: ", p$name),
            description = p$fis_summary,
            confidence_pct = p$fis_score,
            action_label = "Place Bid",
            action_code = "market_bid",
            player_id = as.character(p$id),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    # ---- SELL recommendations: Sell tier players owned by user ----
    # Check if players_df has user_team_id column (from roster)
    if (has_players && "user_team_id" %in% colnames(players_df)) {
      owned <- players_df[players_df$user_team_id == user_team_id, ]
      sell_candidates <- owned[owned$fis_tier == "Sell", ]
      if (nrow(sell_candidates) > 0) {
        sell_candidates <- sell_candidates[order(-sell_candidates$fis_score), ]
        top_sells <- head(sell_candidates, 2)

        for (i in seq_len(nrow(top_sells))) {
          p <- top_sells[i, ]
          recommendations[[length(recommendations) + 1]] <- data.frame(
            type = "Sell",
            title = paste0("SELL: ", p$name),
            description = paste0("Weak metrics suggest listing on market. Current FIS: ", round(p$fis_score, 1)),
            confidence_pct = safe_clamp(100 - p$fis_score),
            action_label = "List on Market",
            action_code = "view",
            player_id = as.character(p$id),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    # ---- BID recommendations: players with active bids ----
    if (has_players && "bid_price" %in% colnames(players_df) && "user_team_id" %in% colnames(players_df)) {
      bid_players <- players_df[
        players_df$user_team_id == user_team_id & !is.na(players_df$bid_price) & players_df$bid_price > 0,
      ]
      if (nrow(bid_players) > 0) {
        for (i in seq_len(nrow(bid_players))) {
          p <- bid_players[i, ]
          bid_val <- suppressWarnings(as.numeric(p$bid_price))
          player_val <- suppressWarnings(as.numeric(p$value))
          accept <- ifelse(!is.na(bid_val) && !is.na(player_val) && bid_val >= player_val * 0.9, "Accept", "Evaluate")
          recommendations[[length(recommendations) + 1]] <- data.frame(
            type = "Bid",
            title = paste0("BID OFFER: ", p$name),
            description = paste0("Active bid of ", bid_val, " EUR on player valued at ", player_val, " EUR. ", accept, " recommended."),
            confidence_pct = ifelse(accept == "Accept", 85, 60),
            action_label = accept,
            action_code = "view",
            player_id = as.character(p$id),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    # ---- CLAUSE recommendations ----
    if (has_clause_cand) {
      # Policy path: clause recommendations are built EXCLUSIVELY from the
      # supplied strict open rival clause candidates.
      cc <- ensure_candidate_fis(clause_candidates)
      if (nrow(cc) > 0) {
        # Filter to those with good FIS scores
        good_clause <- cc[cc$fis_tier %in% c("Strong Buy", "Buy"), ]
        if (nrow(good_clause) > 0) {
          good_clause <- good_clause[order(-good_clause$fis_score), ]
          top_clauses <- head(good_clause, 2)
          mkt_ids <- if (has_market_cand && nrow(market_candidates) > 0) as.character(market_candidates$id) else character(0)

          for (i in seq_len(nrow(top_clauses))) {
            p <- top_clauses[i, ]
            pid <- as.character(p$id)
            clause_price <- suppressWarnings(as.numeric(p$clause_price))

            if (pid %in% mkt_ids) {
              # Dual route: the player is BOTH a market listing and an open
              # rival clause. Emit a SINGLE clause recommendation; the value
              # max(market price, clause price) is comparison metadata only,
              # and the executed price is always the clause price.
              mkt_row <- market_candidates[which(as.character(market_candidates$id) == pid)[1], ]
              mkt_price <- NA_real_
              for (col in c("effective_market_price", "market_price", "price")) {
                if (col %in% names(mkt_row)) {
                  v <- suppressWarnings(as.numeric(mkt_row[[col]]))
                  if (is.finite(v) && v > 0) {
                    mkt_price <- v
                    break
                  }
                }
              }
              if (!is.finite(mkt_price)) mkt_price <- clause_price
              cmp <- max(mkt_price, clause_price)
              description <- paste0(
                "Buyout clause at ", fmt_money(clause_price), " EUR (dual route: also listed on market at ",
                fmt_money(mkt_price), " EUR; comparison max: ", fmt_money(cmp),
                " EUR). Executing clause price only."
              )
            } else {
              player_val <- suppressWarnings(as.numeric(p$value))
              discount <- ifelse(!is.na(clause_price) && !is.na(player_val) && player_val > 0,
                          round((1 - clause_price / player_val) * 100, 1), 0)
              description <- paste0("Buyout clause at ", clause_price, " EUR (", discount, "% discount to market value). Strong Buy candidate.")
            }

            recommendations[[length(recommendations) + 1]] <- data.frame(
              type = "Clause",
              title = paste0("CLAUSE: ", p$name),
              description = description,
              confidence_pct = safe_clamp(p$fis_score + 5),
              action_label = "Exercise Clause",
              action_code = "clause_buyout",
              player_id = pid,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    } else if (has_players && "clause_price" %in% colnames(players_df)) {
      # Legacy path (NULL candidates): players in players_df with a clause.
      clause_pool <- players_df[
        !is.na(players_df$clause_price) & players_df$clause_price > 0,
      ]
      if (nrow(clause_pool) > 0) {
        # Filter to those with good FIS scores
        good_clause <- clause_pool[clause_pool$fis_tier %in% c("Strong Buy", "Buy"), ]
        if (nrow(good_clause) > 0) {
          for (i in seq_len(min(nrow(good_clause), 2))) {
            p <- good_clause[i, ]
            clause_price <- suppressWarnings(as.numeric(p$clause_price))
            player_val <- suppressWarnings(as.numeric(p$value))
            discount <- ifelse(!is.na(clause_price) && !is.na(player_val) && player_val > 0,
                        round((1 - clause_price / player_val) * 100, 1), 0)
            recommendations[[length(recommendations) + 1]] <- data.frame(
              type = "Clause",
              title = paste0("CLAUSE: ", p$name),
              description = paste0("Buyout clause at ", clause_price, " EUR (", discount, "% discount to market value). Strong Buy candidate."),
              confidence_pct = safe_clamp(p$fis_score + 5),
              action_label = "Exercise Clause",
              action_code = "clause_buyout",
              player_id = as.character(p$id),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }

    # ---- HOLD recommendations: top Hold-tier owned players ----
    if (has_players && "user_team_id" %in% colnames(players_df)) {
      owned <- players_df[players_df$user_team_id == user_team_id, ]
      hold_candidates <- owned[owned$fis_tier == "Hold", ]
      if (nrow(hold_candidates) > 0) {
        hold_candidates <- hold_candidates[order(-hold_candidates$fis_score), ]
        top_holds <- head(hold_candidates, 2)

        for (i in seq_len(nrow(top_holds))) {
          p <- top_holds[i, ]
          recommendations[[length(recommendations) + 1]] <- data.frame(
            type = "Hold",
            title = paste0("HOLD: ", p$name),
            description = paste0("Stable asset; no immediate action needed. FIS: ", round(p$fis_score, 1)),
            confidence_pct = safe_clamp(p$fis_score),
            action_label = "No Action",
            action_code = "view",
            player_id = as.character(p$id),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    # ---- Combine and return ----
    if (length(recommendations) == 0) {
      return(empty_feed)
    }

    result_df <- do.call(rbind, recommendations)
    # Sort by confidence descending
    result_df$priority_score <- result_df$confidence_pct
    result_df$confidence_pct <- NA_real_
    result_df <- result_df[order(-result_df$priority_score), ]
    rownames(result_df) <- NULL

    return(result_df)
  }, error = function(e) {
    print(paste0("[CommandCenter] Error generating feed: ", e$message))
    empty_feed
  })
}


# ============================================================
# 5. optimize_starting_xi
# ============================================================
# Picks an optimal starting XI from a squad data frame given a
# formation and scoring mode.  Returns the starting XI, bench,
# and aggregate statistics.
#
# Parameters:
#   squad_df   -- data frame with at least: id, name, role (and
#                 optionally role2).  Must contain (or be enrichable
#                 into) FIS columns via calculate_fis_score().
#   formation  -- one of "4-3-3", "4-4-2", "3-5-2", "3-4-3",
#                 "4-5-1", "5-3-2", "5-4-1".  Default "4-3-3".
#   mode       -- scoring mode: "max_fis"/"fis", "safe", "upside",
#                 "form", "fixture".  Default "max_fis".
#
# Returns:
#   List with: starting_xi (data.frame), bench (data.frame),
#   formation, mode, total_score, avg_fis, feasible (logical),
#   formation_counts (named numeric vector).
# ============================================================

analytics_player_positions <- function(squad_df, multiposition = FALSE) {
  aliases <- c(gk='GK', goalkeeper='GK', portero='GK', df='DEF', def='DEF', defender='DEF', defensa='DEF',
    mf='MID', md='MID', mid='MID', midfielder='MID', centrocampista='MID', fw='FWD', fwd='FWD', forward='FWD', delantero='FWD')
  lapply(seq_len(nrow(squad_df)), function(i) {
    values <- character()
    columns <- if (isTRUE(multiposition)) c('primary_role', 'role', 'role2', 'position', 'eligible_positions') else c('primary_role', 'role', 'position')
    for (column in columns) {
      if (column %in% names(squad_df)) {
        candidate <- as.character(unlist(squad_df[[column]][i]))
        candidate <- candidate[!is.na(candidate) & nzchar(trimws(candidate))]
        values <- c(values, candidate)
        if (!isTRUE(multiposition) && length(candidate)) break
      }
    }
    pieces <- trimws(tolower(unlist(strsplit(values[!is.na(values)], '[,;/|]+'))))
    resolved <- unique(unname(aliases[pieces[pieces %in% names(aliases)]]))
    if (isTRUE(multiposition)) resolved else head(resolved, 1L)
  })
}

analytics_known_unavailable <- function(squad_df) {
  unavailable <- if ('status' %in% names(squad_df))
    tolower(trimws(as.character(squad_df$status))) %in% c('injured', 'injured2', 'redcard', 'suspended', 'unavailable') else rep(FALSE,nrow(squad_df))
  if ('available' %in% names(squad_df) && is.logical(squad_df$available)) unavailable <- unavailable | squad_df$available %in% FALSE
  unavailable
}

optimize_starting_xi <- function(squad_df, formation = '4-3-3', mode = 'max_fis', rules = NULL,
                                 forecast_df = NULL, locked_ids = character(), excluded_ids = character()) {
  if (is.null(rules)) rules <- list()
  empty <- function(reason) list(starting_xi = data.frame(), bench = if (is.data.frame(squad_df)) squad_df else data.frame(),
    formation = formation, mode = mode, total_score = NA_real_, avg_fis = NA_real_, feasible = FALSE,
    formation_counts = c(GK=1, DEF=0, MID=0, FWD=0), expected_points = NA_real_, lower_points = NA_real_,
    upper_points = NA_real_, captain_id = NA_character_, bench_order = character(), diagnostics = reason)
  if (!is.data.frame(squad_df) || !nrow(squad_df)) return(empty('Roster is empty.'))
  if (!'id' %in% names(squad_df) || anyNA(squad_df$id) || any(!nzchar(as.character(squad_df$id))) || anyDuplicated(squad_df$id)) return(empty('Unique player IDs are required.'))
  for (context_column in c('championship_id','season','scoring_version')) {
    context_values <- if(context_column%in%names(squad_df)) unique(as.character(squad_df[[context_column]][!is.na(squad_df[[context_column]])])) else character()
    context_values <- context_values[nzchar(context_values)]
    if(length(context_values)>1L) return(empty('Roster contains multiple league/scoring contexts.'))
    if(is.data.frame(forecast_df)&&context_column%in%names(forecast_df)) {
      forecast_values <- unique(as.character(forecast_df[[context_column]][!is.na(forecast_df[[context_column]])]))
      forecast_values <- forecast_values[nzchar(forecast_values)]
      if(!length(context_values)&&length(forecast_values)>1L) return(empty('Forecast context is ambiguous.'))
      if(length(context_values)) forecast_df <- forecast_df[!is.na(forecast_df[[context_column]]) & as.character(forecast_df[[context_column]])==context_values,,drop=FALSE]
    }
  }
  if (length(formation) != 1L || is.na(formation)) formation <- 'auto'
  allowed <- rules$formations
  if (is.null(allowed) || !length(allowed)) allowed <- c('4-3-3','4-4-2','3-5-2','3-4-3','4-5-1','5-3-2','5-4-1')
  if (is.list(allowed)) allowed <- names(allowed)
  allowed <- as.character(allowed)
  valid_formation <- function(f) {
    parts <- suppressWarnings(as.integer(strsplit(f, '-', fixed=TRUE)[[1]]))
    length(parts) == 3L && all(is.finite(parts)) && all(parts >= 0) && sum(parts) == 10
  }
  allowed <- allowed[vapply(allowed, valid_formation, logical(1))]
  choices <- if (identical(formation, 'auto')) allowed else intersect(formation, allowed)
  if (!length(choices)) return(empty('Formation is not allowed by this league.'))
  squad_df$id <- as.character(squad_df$id)
  positions <- analytics_player_positions(squad_df, multiposition = isTRUE(rules$multiposition))
  excluded <- squad_df$id %in% excluded_ids
  if('acquisition_effective_at'%in%names(squad_df)) {
    effective <- analytics_time(squad_df$acquisition_effective_at)
    deadline <- analytics_time(rules$deadline %||% NA_character_)
    if(length(deadline)!=1L||is.na(deadline)) deadline <- Sys.time()
    excluded <- excluded | (!is.na(effective)&effective>deadline)
  }
  if (!identical(rules$exclude_unavailable, FALSE)) excluded <- excluded | analytics_known_unavailable(squad_df)
  if (any(!locked_ids %in% squad_df$id) || any(locked_ids %in% squad_df$id[excluded])) return(empty('A locked player is unavailable or absent.'))
  if (sum(!excluded & lengths(positions) > 0) < 11) return(empty('Fewer than eleven eligible available players.'))
  if (!'fis_score' %in% names(squad_df)) squad_df <- calculate_fis_score(squad_df)
  avg <- analytics_numeric_column(squad_df, 'average.average')
  pts <- analytics_numeric_column(squad_df, 'points'); played <- analytics_numeric_column(squad_df, 'average.matches')
  use <- !is.finite(avg) & is.finite(pts) & is.finite(played) & played > 0
  avg[use] <- pts[use] / played[use]
  lower <- upper <- rep(NA_real_,nrow(squad_df))
  if (is.null(forecast_df) && exists('forecast_fantasy_points', mode='function')) {
    forecast_df <- tryCatch(forecast_fantasy_points(squad_df, horizons=1), error=function(e) NULL)
  }
  if (is.data.frame(forecast_df) && nrow(forecast_df) && all(c('player_id','expected_points') %in% names(forecast_df))) {
    if ('horizon' %in% names(forecast_df)) forecast_df <- forecast_df[forecast_df$horizon == 1, , drop=FALSE]
    j <- match(squad_df$id, as.character(forecast_df$player_id))
    if (nrow(forecast_df)) {
      estimates <- suppressWarnings(as.numeric(forecast_df$expected_points[j])); replace <- is.finite(estimates)
      avg[replace] <- estimates[replace]
      if ('lower' %in% names(forecast_df)) lower[replace] <- forecast_df$lower[j[replace]]
      if ('upper' %in% names(forecast_df)) upper[replace] <- forecast_df$upper[j[replace]]
    }
  }
  unavailable <- analytics_known_unavailable(squad_df)
  avg[unavailable] <- 0; lower[unavailable] <- 0; upper[unavailable] <- 0
  fallback <- analytics_numeric_column(squad_df, 'fis_score')
  score <- avg
  mode <- if (length(mode) == 1L && !is.na(mode)) tolower(mode) else 'expected'
  if (mode == 'safe') score <- lower
  if (mode == 'upside') score <- upper
  if (mode == 'form') score <- analytics_numeric_column(squad_df, 'average.averageLastFive')
  if (mode == 'fis') score <- fallback
  missing <- !is.finite(score); score[missing] <- avg[missing]
  missing <- !is.finite(score)
  # Unknown point estimates rank after observed estimates; descriptive ratings only
  # order a wholly unobserved squad, and never become displayed point intervals.
  if(any(!missing)) score[missing] <- min(score[!missing])-1 else score[missing] <- fallback[missing]
  score[!is.finite(score)] <- 0
  club <- rep('', nrow(squad_df))
  for (column in c('club_id','real_club_id','teamId','team')) if (column %in% names(squad_df)) {
    candidate <- as.character(squad_df[[column]])
    use <- !nzchar(club) & !is.na(candidate) & nzchar(candidate); club[use] <- candidate[use]
  }
  cap <- if (is.null(rules$club_limit)) Inf else suppressWarnings(as.numeric(rules$club_limit)[1])
  assumptions <- character()
  if (identical(rules$club_limit_status, 'unknown')) {
    cap <- Inf
    assumptions <- c(assumptions, 'Club limit unknown; provisional XI assumes no club limit.')
  }
  if (identical(rules$club_limit_status, 'unrestricted')) cap <- Inf
  if (!isTRUE(rules$verified)) assumptions <- c(assumptions, 'Full league rules and deadline eligibility are not verified.')
  if (is.na(cap) || cap < 1) return(empty('Invalid club limit.'))
  if (is.finite(cap) && any(!nzchar(club[!excluded]))) return(empty('Club identifiers are required to verify the club limit.'))
  captain_enabled <- isTRUE(rules$captain_enabled)
  multiplier <- if (captain_enabled) {
    if (is.null(rules$captain_multiplier)) 2 else suppressWarnings(as.numeric(rules$captain_multiplier)[1])
  } else 1
  if (!is.finite(multiplier) || multiplier < 1) return(empty('Invalid captain multiplier.'))
  best <- NULL; best_score <- -Inf
  for (fmt in choices) {
    counts <- c(GK=1L, setNames(as.integer(strsplit(fmt,'-',fixed=TRUE)[[1]]), c('DEF','MID','FWD')))
    candidates <- lapply(names(counts), function(pos) {
      idx <- which(!excluded & vapply(positions, function(x) pos %in% x, logical(1)))
      idx[order(-score[idx], squad_df$id[idx])]
    }); names(candidates) <- names(counts)
    if (any(lengths(candidates) < counts)) next
    group_order <- names(sort(lengths(candidates) / pmax(counts, 1)))
    group_order <- group_order[counts[group_order] > 0]
    slots <- rep(group_order, counts[group_order])
    # Exact depth-first assignment with admissible score bounds; increasing candidate
    # offsets within each position eliminate permutations of the same selection.
    visit <- function(depth, selected, assigned, total, last_rank) {
      if (depth > length(slots)) {
        if (!all(locked_ids %in% squad_df$id[selected])) return(invisible(NULL))
        objective <- total + (multiplier - 1) * max(score[selected])
        if (objective > best_score + 1e-10) {
          best_score <<- objective
          best <<- list(idx=selected, assigned=assigned, formation=fmt, counts=counts)
        }
        return(invisible(NULL))
      }
      left <- length(slots) - depth + 1L
      if (sum(!locked_ids %in% squad_df$id[selected]) > left) return(invisible(NULL))
      available <- which(!excluded & !seq_len(nrow(squad_df)) %in% selected & lengths(positions) > 0)
      if (length(available) < left) return(invisible(NULL))
      # Position-specific relaxation is exact for single-position independent groups
      # and remains an upper bound when flexible players appear in several groups.
      remaining_counts <- table(slots[seq.int(depth,length(slots))])
      bound <- total
      for (remaining_pos in names(remaining_counts)) {
        pool <- candidates[[remaining_pos]]
        if (depth > 1L && remaining_pos == slots[depth] && slots[depth-1L] == remaining_pos) pool <- pool[seq_along(pool) > last_rank]
        pool <- pool[!pool %in% selected]
        needed <- as.integer(remaining_counts[remaining_pos])
        if (length(pool) < needed) return(invisible(NULL))
        bound <- bound + sum(head(score[pool],needed))
      }
      upper_bound <- bound + (multiplier - 1) * max(score[c(selected, available)])
      if (upper_bound <= best_score + 1e-10) return(invisible(NULL))
      pos <- slots[depth]; cand <- candidates[[pos]]
      start <- if (depth > 1L && slots[depth-1L] == pos) last_rank + 1L else 1L
      if (start > length(cand)) return(invisible(NULL))
      for (rank in seq.int(start, length(cand))) {
        idx <- cand[rank]
        if (idx %in% selected) next
        if (is.finite(cap) && sum(club[selected] == club[idx]) >= cap) next
        visit(depth+1L, c(selected,idx), c(assigned,pos), total+score[idx], rank)
      }
      invisible(NULL)
    }
    visit(1L, integer(), character(), 0, 0L)
  }
  if (is.null(best)) return(empty('No legal XI satisfies positions, club limits and locked players.'))
  idx <- best$idx; squad_df$opt_score <- score; squad_df$expected_points <- avg
  squad_df$forecast_lower <- lower; squad_df$forecast_upper <- upper
  squad_df$pos_group <- vapply(positions, function(x) if(length(x)) x[1] else 'Unknown', character(1))
  starting <- squad_df[idx, , drop=FALSE]; starting$pos_group <- best$assigned
  starting <- starting[order(match(starting$pos_group,c('GK','DEF','MID','FWD')),-starting$opt_score,starting$id),,drop=FALSE]
  bench <- squad_df[!seq_len(nrow(squad_df)) %in% idx,,drop=FALSE]
  bench <- bench[order(-bench$opt_score,bench$id),,drop=FALSE]
  captain <- if (captain_enabled) idx[which.max(score[idx])] else integer()
  aggregate <- function(v) {
    if (any(!is.finite(v[idx]))) return(NA_real_)
    sum(v[idx]) + if(length(captain)) (multiplier-1)*v[captain] else 0
  }
  bench_size <- if (is.null(rules$bench_size) || identical(rules$bench_enabled, FALSE)) 0L else suppressWarnings(as.integer(rules$bench_size)[1])
  if (!is.finite(bench_size)) bench_size <- 0L
  if (bench_size < 0) return(empty('Invalid bench size.'))
  bench_eligible <- !bench$id %in% squad_df$id[excluded] & !analytics_known_unavailable(bench) & lengths(positions[match(bench$id, squad_df$id)]) > 0
  # The complete reserve table remains visible, but only eligible reserves enter an enabled bench.
  ordered_bench_ids <- head(bench$id[bench_eligible], bench_size)
  list(starting_xi=starting, bench=bench, formation=best$formation, mode=mode,
    total_score=round(best_score,3), avg_fis=if(any(is.finite(starting$fis_score))) mean(starting$fis_score,na.rm=TRUE) else NA_real_,
    feasible=TRUE, legality_verified=isTRUE(rules$verified) && !length(assumptions),
    legality_status=if(length(assumptions)) 'provisional' else 'verified', assumptions=assumptions,
    formation_counts=best$counts, expected_points=aggregate(avg), lower_points=aggregate(lower),
    upper_points=aggregate(upper), captain_id=if(length(captain)) squad_df$id[captain] else NA_character_,
    bench_order=ordered_bench_ids, diagnostics=c(assumptions, if(any(!is.finite(avg[idx]))) 'Some players have no point forecast; descriptive ordering used.' else character()))
}

simulate_transfer_scenario <- function(squad_df, current_budget = 0, sell_player_ids = character(),
                                        buy_player_ids = character(), market_df = NULL, rules = NULL,
                                        buy_prices = NULL, sale_proceeds = NULL, forecast_df = NULL) {
  fail <- function(message, status='invalid') list(projected_squad=data.frame(),total_sell_proceeds=NA_real_,
    total_buy_cost=NA_real_,projected_budget=NA_real_,initial_total_val=NA_real_,projected_total_val=NA_real_,
    initial_avg_fis=NA_real_,projected_avg_fis=NA_real_,delta_avg_fis=NA_real_,is_budget_valid=FALSE,
    is_lineup_valid=FALSE,status=status,diagnostics=message,projected_lineup=NULL)
  tryCatch({
    if (!is.data.frame(squad_df) || !nrow(squad_df) || !'id' %in% names(squad_df)) return(fail('Roster data unavailable.'))
    if (length(current_budget)!=1L || !is.finite(current_budget)) return(fail('Verified cash is required.'))
    if (anyNA(squad_df$id) || anyDuplicated(squad_df$id)) return(fail('Roster IDs must be unique.'))
    sells <- unique(as.character(sell_player_ids)); buys <- unique(as.character(buy_player_ids))
    if (anyNA(c(sells,buys)) || any(!nzchar(c(sells,buys)))) return(fail('Invalid transfer player ID.'))
    if (any(!sells %in% squad_df$id)) return(fail('A sale player is not owned.'))
    if (length(intersect(sells,buys))) return(fail('A player cannot be bought and sold in the same scenario.'))
    remaining <- squad_df[!squad_df$id %in% sells,,drop=FALSE]
    if (any(buys %in% remaining$id)) return(fail('A purchase player is already owned.'))
    if (length(buys) && (!is.data.frame(market_df) || !'id' %in% names(market_df) || any(!buys %in% market_df$id))) return(fail('Purchase data is missing.'))
    if (length(buys) && anyDuplicated(market_df$id[market_df$id %in% buys])) return(fail('Purchase IDs are duplicated.'))
    purchased <- if (length(buys)) market_df[match(buys,market_df$id),,drop=FALSE] else squad_df[FALSE,,drop=FALSE]
    sold <- squad_df[match(sells,squad_df$id),,drop=FALSE]
    for(column in c('championship_id','season','scoring_version')) {
      values <- unique(unlist(lapply(list(squad_df,purchased),function(df) if(column%in%names(df)) as.character(df[[column]]) else character())))
      values <- values[!is.na(values)&nzchar(values)]
      if(length(values)>1L) return(fail('Transfers must remain within one league/scoring context.'))
    }

    amount <- function(df, explicit, columns) {
      if (!nrow(df)) return(numeric())
      if (!is.null(explicit)) {
        if (is.null(names(explicit))) return(rep(NA_real_,nrow(df)))
        return(suppressWarnings(as.numeric(explicit[as.character(df$id)])))
      }
      result <- rep(NA_real_,nrow(df))
      for (column in columns) if(column %in% names(df)) {
        candidate <- analytics_numeric_column(df,column)
        take <- !is.finite(result) & is.finite(candidate) & candidate >= 0; result[take] <- candidate[take]
      }
      result
    }
    sell_amounts <- amount(sold,sale_proceeds,c('executable_sale_price','sale_offer','value'))
    buy_amounts <- amount(purchased,buy_prices,c('effective_market_price','price','value'))
    if (any(!is.finite(c(sell_amounts,buy_amounts))) || any(c(sell_amounts,buy_amounts)<0)) return(fail('Transfer amounts are unavailable.'))
    projected <- dplyr::bind_rows(remaining,purchased)
    if (!'fis_score' %in% names(squad_df)) squad_df <- calculate_fis_score(squad_df)
    projected <- calculate_fis_score(projected)
    cash <- current_budget+sum(sell_amounts)-sum(buy_amounts)
    cap <- if(is.null(rules$roster_cap)) Inf else suppressWarnings(as.numeric(rules$roster_cap)[1])
    legal_rules <- if(is.null(rules)) list() else rules
    legal_rules$exclude_unavailable <- FALSE
    lineup <- optimize_starting_xi(projected,formation='auto',rules=legal_rules,forecast_df=forecast_df)
    diagnostics <- character()
    if (cash<0) diagnostics <- c(diagnostics,'Insufficient cash after transfers.')
    if (is.na(cap)) diagnostics <- c(diagnostics,'Roster cap unavailable.')
    else if (nrow(projected)>cap) diagnostics <- c(diagnostics,'Roster cap exceeded.')
    if (!lineup$feasible) diagnostics <- c(diagnostics,lineup$diagnostics)
    average <- function(x) if(length(x)&&any(is.finite(x))) mean(x,na.rm=TRUE) else NA_real_
    initial_avg <- average(squad_df$fis_score); projected_avg <- average(projected$fis_score)
    list(projected_squad=projected,total_sell_proceeds=sum(sell_amounts),total_buy_cost=sum(buy_amounts),
      projected_budget=cash,initial_total_val=sum(analytics_numeric_column(squad_df,'value'),na.rm=TRUE),
      projected_total_val=sum(analytics_numeric_column(projected,'value'),na.rm=TRUE),initial_avg_fis=initial_avg,
      projected_avg_fis=projected_avg,delta_avg_fis=projected_avg-initial_avg,is_budget_valid=cash>=0,
      is_lineup_valid=isTRUE(lineup$feasible)&&!is.na(cap)&&nrow(projected)<=cap,status=if(length(diagnostics)) 'invalid' else 'ok',
      diagnostics=diagnostics,projected_lineup=lineup,prices_verified=all(c('executable_sale_price') %in% names(sold))&&all(c('effective_market_price') %in% names(purchased)))
  },error=function(e) fail(conditionMessage(e),'error'))
}

recommend_transfers <- function(squad_df, market_df, current_budget = 0, max_transfers = 5,
                               rules = NULL, forecast_df = NULL) {
  empty <- data.frame(sell_id=character(),sell_name=character(),sell_role=character(),sell_val=numeric(),sell_fis=numeric(),
    buy_id=character(),buy_name=character(),buy_role=character(),buy_val=numeric(),buy_fis=numeric(),net_cost=numeric(),
    delta_fis=numeric(),roi_pct=numeric(),delta_expected_points=numeric(),points_per_million=numeric(),expected_profit=numeric())
  if (!is.data.frame(squad_df)||!nrow(squad_df)||!is.data.frame(market_df)||!nrow(market_df)||length(current_budget)!=1L||!is.finite(current_budget)) return(empty)
  squad_df <- calculate_fis_score(squad_df); market_df <- calculate_fis_score(market_df)
  legal_rules <- if(is.null(rules)) list() else rules
  legal_rules$exclude_unavailable <- FALSE
  before <- optimize_starting_xi(squad_df,formation='auto',rules=legal_rules,forecast_df=forecast_df)
  if (!before$feasible) return(empty)
  out <- list()
  # Every legal pair is evaluated against the best resulting XI, not player rating differences.
  for (si in seq_len(nrow(squad_df))) for (bi in seq_len(nrow(market_df))) {
    s <- squad_df[si,,drop=FALSE]; b <- market_df[bi,,drop=FALSE]
    if (as.character(b$id) %in% as.character(squad_df$id)) next
    future_buy <- analytics_numeric_column(b,'expected_sale_proceeds')
    future_hold <- analytics_numeric_column(s,'expected_sale_proceeds')
    executable <- analytics_numeric_column(s,'executable_sale_price')
    if(any(!is.finite(c(future_buy,future_hold,executable)))) next
    scenario <- simulate_transfer_scenario(squad_df,current_budget,as.character(s$id),as.character(b$id),market_df,rules=rules,forecast_df=forecast_df)
    if (!identical(scenario$status,'ok')) next
    gain <- scenario$projected_lineup$expected_points-before$expected_points
    cost <- scenario$total_buy_cost-scenario$total_sell_proceeds
    profit <- future_buy-future_hold-cost
    if(!is.finite(profit)||profit<=0) next
    value <- function(df,key,default='') if(key %in% names(df)) as.character(df[[key]][1]) else default
    out[[length(out)+1L]] <- data.frame(sell_id=as.character(s$id),sell_name=value(s,'name'),sell_role=value(s,'role'),
      sell_val=scenario$total_sell_proceeds,sell_fis=s$fis_score,buy_id=as.character(b$id),buy_name=value(b,'name'),buy_role=value(b,'role'),
      buy_val=scenario$total_buy_cost,buy_fis=b$fis_score,net_cost=cost,delta_fis=b$fis_score-s$fis_score,
      roi_pct=NA_real_,delta_expected_points=gain,points_per_million=if(cost>0) gain/(cost/1e6) else NA_real_,expected_profit=profit)
  }
  if(!length(out)) return(empty)
  result <- dplyr::bind_rows(out)
  result <- result[order(-result$expected_profit,result$net_cost,result$buy_id,result$sell_id),,drop=FALSE]
  head(result,max(0,as.integer(max_transfers)))
}
