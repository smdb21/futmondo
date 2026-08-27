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

calculate_fis_score <- function(players_df, weights = NULL) {
  if (is.null(players_df) || nrow(players_df) == 0) {
    return(players_df)
  }

  tryCatch({
    n <- nrow(players_df)

    # ---- Default weights ----
    if (is.null(weights)) {
      weights <- list(
        perf = 0.30,
        form = 0.20,
        efficiency = 0.20,
        momentum = 0.15,
        fixture_risk = 0.15
      )
    }

    # Use the local NULL/NA-safe default helper (no reliance on %||%).
    w_perf <- default_if_null_na(weights$perf, 0.30)
    w_form <- default_if_null_na(weights$form, 0.20)
    w_eff  <- default_if_null_na(weights$efficiency, 0.20)
    w_mom  <- default_if_null_na(weights$momentum, 0.15)
    w_fix  <- default_if_null_na(weights$fixture_risk, 0.15)

    # Coerce weights to finite numerics; fall back to defaults for bad entries.
    w_perf <- if (is.numeric(w_perf) && is.finite(w_perf)) w_perf else 0.30
    w_form <- if (is.numeric(w_form) && is.finite(w_form)) w_form else 0.20
    w_eff  <- if (is.numeric(w_eff) && is.finite(w_eff)) w_eff else 0.20
    w_mom  <- if (is.numeric(w_mom) && is.finite(w_mom)) w_mom else 0.15
    w_fix  <- if (is.numeric(w_fix) && is.finite(w_fix)) w_fix else 0.15

    # ---- Extract raw columns with safe defaults ----
    points      <- safe_numeric(
      if ("points" %in% colnames(players_df)) players_df$points else NULL, 0
    )
    value       <- safe_numeric(
      if ("value" %in% colnames(players_df)) players_df$value else NULL, 0
    )
    change      <- safe_numeric(
      if ("change" %in% colnames(players_df)) players_df$change else NULL, 0
    )
    avg_pts     <- safe_numeric(
      if ("average.average" %in% colnames(players_df)) players_df$average.average else NULL, 0
    )
    avg_last5   <- safe_numeric(
      if ("average.averageLastFive" %in% colnames(players_df)) players_df$average.averageLastFive else NULL, 0
    )
    matches     <- safe_numeric(
      if ("average.matches" %in% colnames(players_df)) players_df$average.matches else NULL, 0
    )
    status_vec  <- if ("status" %in% colnames(players_df)) as.character(players_df$status) else rep("ok", n)
    role_vec    <- if ("role" %in% colnames(players_df)) as.character(players_df$role) else rep("Unknown", n)

    # Sanitize NA/empty role and status so split() and tolower() never drop rows.
    role_vec[is.na(role_vec) | trimws(role_vec) == ""] <- "Unknown"
    status_vec[is.na(status_vec) | trimws(status_vec) == ""] <- "ok"

    # ---- perf (0-100): based on points, average, matches ----
    # Normalize: more points -> higher, capped at 100
    max_points <- max(points, na.rm = TRUE)
    max_points <- ifelse(max_points == 0, 1, max_points)
    perf_raw <- (points / max_points) * 60  # up to 60 pts from raw points

    # Add average contribution (up to 25 pts)
    avg_contribution <- pmin(avg_pts / 10, 1) * 25

    # Add matches played contribution (up to 15 pts) -- rewards consistency
    max_matches <- max(matches, na.rm = TRUE)
    max_matches <- ifelse(max_matches == 0, 1, max_matches)
    match_contribution <- (matches / max_matches) * 15

    perf <- safe_clamp(perf_raw + avg_contribution + match_contribution)

    # ---- form (0-100): form momentum from avg_last5 vs avg ----
    # Where avg > 0, form_ratio = avg_last5 / avg
    form_ratio <- ifelse(avg_pts > 0, avg_last5 / avg_pts, 1)
    # Map ratio to 0-100: ratio=0.5 -> 0, ratio=1.0 -> 50, ratio=1.5 -> 100
    form <- safe_clamp((form_ratio - 0.5) * 100)

    # ---- efficiency (0-100): points per million EUR relative to position peers ----
    # points per million = points / (value / 1e6)
    value_safe <- ifelse(value > 0, value, 1)
    pts_per_million <- points / (value_safe / 1e6)

    # Group by position to compute peer-relative efficiency
    position_groups <- split(seq_len(n), role_vec)
    efficiency <- rep(50, n)  # default neutral

    for (grp_name in names(position_groups)) {
      idx <- position_groups[[grp_name]]
      if (length(idx) < 2) next
      grp_pts_m <- pts_per_million[idx]
      grp_min <- min(grp_pts_m, na.rm = TRUE)
      grp_max <- max(grp_pts_m, na.rm = TRUE)
      grp_range <- grp_max - grp_min
      grp_range <- ifelse(grp_range == 0, 1, grp_range)
      efficiency[idx] <- safe_clamp((grp_pts_m - grp_min) / grp_range * 100)
    }

    # ---- momentum (0-100): 24h market value change normalized ----
    abs_change <- abs(change)
    max_abs_change <- max(abs_change, na.rm = TRUE)
    max_abs_change <- ifelse(max_abs_change == 0, 1, max_abs_change)
    # Normalize: sign matters, positive -> higher score
    momentum_raw <- (change / max_abs_change) * 50 + 50  # maps [-1,1] -> [0,100]
    momentum <- safe_clamp(momentum_raw)

    # ---- fixture_risk (0-100): minutes played trend & status ----
    # status penalty: 'ok' -> 0, 'doubt' -> -20, 'injured'/'injured2' -> -50, 'redcard' -> -40
    status_penalty <- ifelse(tolower(status_vec) == "ok", 0,
                       ifelse(tolower(status_vec) == "doubt", -20,
                       ifelse(tolower(status_vec) %in% c("injured", "injured2"), -50,
                       ifelse(tolower(status_vec) == "redcard", -40, 0))))

    # Minutes played trend proxy via matches contribution
    match_trend <- (matches / max_matches) * 30
    fixture_risk <- safe_clamp(70 + status_penalty + match_trend)

    # ---- Composite FIS score ----
    fis_score <- w_perf * perf + w_form * form + w_eff * efficiency +
                 w_mom * momentum + w_fix * fixture_risk
    fis_score <- safe_clamp(fis_score)
    fis_score[is.na(fis_score) | is.nan(fis_score)] <- 50.0

    # ---- FIS tier ----
fis_tier <- ifelse(fis_score >= 80, "Strong Buy",
                   ifelse(fis_score >= 65, "Buy",
                   ifelse(fis_score >= 45, "Hold", "Sell")))
    fis_tier[is.na(fis_tier)] <- "Hold"

    # ---- FIS summary ----
    player_names <- if ("name" %in% colnames(players_df)) as.character(players_df$name) else rep("Player", n)

    fis_summary <- vapply(seq_len(n), function(i) {
      tier_label <- if (!is.na(fis_tier[i])) fis_tier[i] else "Hold"
      name_label <- if (!is.na(player_names[i])) player_names[i] else "Player"
      score_val  <- if (!is.na(fis_score[i])) round(fis_score[i], 1) else 50.0
      if (identical(tier_label, "Strong Buy")) {
        paste0(name_label, ": Exceptional value with strong performance and positive market momentum (FIS=", score_val, ").")
      } else if (identical(tier_label, "Buy")) {
        paste0(name_label, ": Solid buy candidate with good form and efficiency metrics (FIS=", score_val, ").")
      } else if (identical(tier_label, "Hold")) {
        paste0(name_label, ": Neutral outlook; monitor for form changes before acting (FIS=", score_val, ").")
      } else {
        paste0(name_label, ": Weak metrics or negative trend; consider selling to free budget (FIS=", score_val, ").")
      }
    }, character(1))

    # ---- Assign back to data frame ----
    players_df$perf            <- safe_clamp(perf)
    players_df$form            <- safe_clamp(form)
    players_df$efficiency      <- safe_clamp(efficiency)
    players_df$momentum        <- safe_clamp(momentum)
    players_df$fixture_risk    <- safe_clamp(fixture_risk)
    players_df$fis_score       <- round(fis_score, 2)
    players_df$fis_tier        <- fis_tier
    players_df$fis_summary     <- fis_summary

    return(players_df)
  }, error = function(e) {
    print(paste0("[FIS] Error computing FIS scores: ", e$message))
    # Return original with NA columns
    players_df$perf            <- NA_real_
    players_df$form            <- NA_real_
    players_df$efficiency      <- NA_real_
    players_df$momentum        <- NA_real_
    players_df$fixture_risk    <- NA_real_
    players_df$fis_score       <- NA_real_
    players_df$fis_tier        <- NA_character_
    players_df$fis_summary     <- NA_character_
    return(players_df)
  })
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
#   user_cash        -- numeric, available budget (default 300M).
#                       Pass NA/NULL to mark funds as unverified; the
#                       function then falls back to its own default.
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
                                 user_cash = 300000000,
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

    # ---- Verified spendable funds ----
    # user_cash alone is treated as unverified. A capacity object with
    # status "ok" supplies verified spendable funds (max(0, budget - withheld)).
    spendable <- 300000000
    funds_verified <- FALSE
    if (!is.null(user_cash) && length(user_cash) == 1 && is.numeric(user_cash) &&
        is.finite(user_cash)) {
      spendable <- max(0, user_cash)
    } else {
      # NA/NULL/non-finite user_cash -> unverified engine default
      spendable <- 300000000
    }
    if (!is.null(capacity) && is.list(capacity) &&
        identical(capacity$status, "ok") &&
        is.list(capacity$funds) &&
        is.numeric(capacity$funds$spendable_budget) &&
        is.finite(capacity$funds$spendable_budget)) {
      spendable <- max(0, capacity$funds$spendable_budget)
      funds_verified <- TRUE
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
    max_rational_bid <- min(round(fair_value * 1.5), spendable)

    # ---- recommended_bid: balance between winning and value, bounded by
    #      the rational guardrail and verified spendable funds ----
    recommended_raw <- round(fair_value * (1 + league_premium_pct / 200))
    recommended_bid <- min(recommended_raw, max_rational_bid)
    # Never recommend below the minimum winning bid (when affordable)
    if (min_winning_bid <= spendable) {
      recommended_bid <- max(recommended_bid, min_winning_bid)
    }
    recommended_bid <- min(recommended_bid, spendable)

    # ---- expected_roi_pct ----
    if (recommended_bid > 0) {
      expected_roi_pct <- round((fair_value / recommended_bid - 1) * 100, 2)
    } else {
      expected_roi_pct <- 0
    }

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

    # ---- confidence_pct ----
    base_confidence <- 70
    # Adjust based on data availability
    if (player_matches > 0) base_confidence <- base_confidence + 5
    if (!is.na(player_avg5) && player_avg > 0) base_confidence <- base_confidence + 5
    if (tolower(player_status) == "ok") base_confidence <- base_confidence + 5
    if (length(likely_competitors) > 0) base_confidence <- base_confidence + 5
    # Verified spendable funds and live market high bid increase confidence
    if (funds_verified) base_confidence <- base_confidence + 5
    if (!is.null(mhb)) base_confidence <- base_confidence + 5
    confidence_pct <- safe_clamp(base_confidence)

    list(
      fair_value = fair_value,
      league_premium_pct = league_premium_pct,
      min_winning_bid = min_winning_bid,
      recommended_bid = recommended_bid,
      max_rational_bid = max_rational_bid,
      expected_roi_pct = expected_roi_pct,
      competition_level = competition_level,
      likely_competitors = likely_competitors,
      confidence_pct = confidence_pct,
      spendable_funds = spendable,
      funds_verified = funds_verified,
      market_high_bid = mhb
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

calculate_manager_dna <- function(team_id, pressroom_df, user_teams_df = NULL) {
  if (is.null(team_id) || team_id == "") {
    return(list(error = "team_id is empty"))
  }

  tryCatch({
    # Default empty result
    result <- list(
      team_id = team_id,
      aggressiveness = 50.0,
      avg_overpayment_pct = 0.0,
      fav_position = "Unknown",
      trading_frequency = 0.0,
      avg_holding_days = 0.0,
      total_trades = 0,
      insights = "Insufficient data to compute manager DNA profile."
    )

    if (is.null(pressroom_df) || nrow(pressroom_df) == 0) {
      return(result)
    }

    # Filter transactions where this team is buyer or seller
    team_tx <- pressroom_df[
      pressroom_df$buyer_team_id == team_id | pressroom_df$seller_team_id == team_id,
    ]

    if (nrow(team_tx) == 0) {
      return(result)
    }

    buys  <- team_tx[team_tx$buyer_team_id == team_id, ]
    sells <- team_tx[team_tx$seller_team_id == team_id, ]

    total_trades <- nrow(team_tx)
    result$total_trades <- total_trades

    # ---- aggressiveness (0-100) ----
    # Based on trade volume and buy/sell ratio
    buy_count <- nrow(buys)
    sell_count <- nrow(sells)
    turnover_ratio <- ifelse(total_trades > 0, (buy_count + sell_count) / total_trades, 0)
    # Scale: more trades = more aggressive
    aggressiveness <- safe_clamp(total_trades * 3 + turnover_ratio * 20)
    result$aggressiveness <- round(aggressiveness, 2)

    # ---- avg_overpayment_pct ----
    if (nrow(buys) > 0) {
      # We approximate overpayment by comparing buy price to a baseline
      # (In a real system this would compare to fair_value from smart_bid)
      # Use a heuristic: if price > 25M, assume 10% overpayment baseline
      buy_prices <- suppressWarnings(as.numeric(buys$price))
      avg_buy_price <- mean(buy_prices, na.rm = TRUE)
      # Heuristic overpayment relative to typical market value
      baseline_value <- 25000000  # typical baseline
      avg_overpayment_pct <- round((avg_buy_price / baseline_value - 1) * 100, 2)
      result$avg_overpayment_pct <- avg_overpayment_pct
    }

    # ---- fav_position ----
    # Would require joining with players_df to get roles; use "Unknown" if unavailable
    result$fav_position <- "Unknown"

    # ---- trading_frequency ----
    # Trades per approximate round (estimate from date range)
    if (nrow(team_tx) > 1) {
      dates <- team_tx$created
      valid_dates <- dates[nzchar(dates)]
      if (length(valid_dates) >= 2) {
        parsed_dates <- suppressWarnings(as.POSIXct(valid_dates, tz = "UTC"))
        valid_parsed <- parsed_dates[!is.na(parsed_dates)]
        if (length(valid_parsed) >= 2) {
          date_range_days <- as.numeric(difftime(max(valid_parsed), min(valid_parsed), units = "days"))
          # Assume ~10 days per round
          estimated_rounds <- ifelse(date_range_days > 0, date_range_days / 10, 1)
          trading_frequency <- round(total_trades / estimated_rounds, 2)
          result$trading_frequency <- trading_frequency
        }
      }
    }

    # ---- avg_holding_days ----
    # For each player, compute time between buy and sell
    holding_days_list <- c()
    player_ids_bought <- unique(buys$player_id)
    for (pid in player_ids_bought) {
      buy_dates <- buys$created[buys$player_id == pid]
      sell_dates <- sells$created[sells$player_id == pid]
      valid_buy <- suppressWarnings(as.POSIXct(buy_dates[nzchar(buy_dates)], tz = "UTC"))
      valid_sell <- suppressWarnings(as.POSIXct(sell_dates[nzchar(sell_dates)], tz = "UTC"))
      valid_buy <- valid_buy[!is.na(valid_buy)]
      valid_sell <- valid_sell[!is.na(valid_sell)]
      if (length(valid_buy) > 0 && length(valid_sell) > 0) {
        holding <- as.numeric(difftime(valid_sell, min(valid_buy), units = "days"))
        holding_days_list <- c(holding_days_list, holding[!is.na(holding)])
      }
    }
    if (length(holding_days_list) > 0) {
      result$avg_holding_days <- round(mean(holding_days_list), 2)
    }

    # ---- insights ----
    insights_parts <- c()
    if (aggressiveness >= 70) {
      insights_parts <- c(insights_parts, "Highly aggressive trader with rapid turnover.")
    } else if (aggressiveness >= 40) {
      insights_parts <- c(insights_parts, "Moderate trading activity with balanced approach.")
    } else {
      insights_parts <- c(insights_parts, "Conservative manager; prefers holding assets longer.")
    }
    if (avg_overpayment_pct > 20) {
      insights_parts <- c(insights_parts, "Tends to overpay relative to market baseline.")
    } else if (avg_overpayment_pct < -10) {
      insights_parts <- c(insights_parts, "Strong value hunter; consistently buys below market.")
    }
    result$insights <- paste(insights_parts, collapse = " ")

    return(result)
  }, error = function(e) {
    print(paste0("[ManagerDNA] Error computing DNA profile for team ", team_id, ": ", e$message))
    list(
      team_id = team_id,
      aggressiveness = NA_real_,
      avg_overpayment_pct = NA_real_,
      fav_position = "Unknown",
      trading_frequency = NA_real_,
      avg_holding_days = NA_real_,
      total_trades = 0,
      insights = paste("Error computing profile:", e$message)
    )
  })
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
    result_df <- result_df[order(-result_df$confidence_pct), ]
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

optimize_starting_xi <- function(squad_df, formation = "4-3-3", mode = "max_fis") {
  # ---- Default empty return on failure ----
  empty_result <- list(
    starting_xi = data.frame(),
    bench = data.frame(),
    formation = formation,
    mode = mode,
    total_score = 0,
    avg_fis = 0,
    feasible = FALSE,
    formation_counts = c(GK = 1, DEF = 0, MID = 0, FWD = 0)
  )

  tryCatch({
    # ---- Validate input ----
    if (is.null(squad_df) || nrow(squad_df) == 0) {
      return(empty_result)
    }

    # ---- Parse formation ----
    formation_map <- list(
      "4-3-3" = c(DEF = 4, MID = 3, FWD = 3),
      "4-4-2" = c(DEF = 4, MID = 4, FWD = 2),
      "3-5-2" = c(DEF = 3, MID = 5, FWD = 2),
      "3-4-3" = c(DEF = 3, MID = 4, FWD = 3),
      "4-5-1" = c(DEF = 4, MID = 5, FWD = 1),
      "5-3-2" = c(DEF = 5, MID = 3, FWD = 2),
      "5-4-1" = c(DEF = 5, MID = 4, FWD = 1)
    )

    if (!formation %in% names(formation_map)) {
      print(paste0("[StartingXI] Unknown formation '", formation, "'. Defaulting to 4-3-3."))
      formation <- "4-3-3"
    }

    target <- formation_map[[formation]]
    def_count <- as.integer(target["DEF"])
    mid_count <- as.integer(target["MID"])
    fwd_count <- as.integer(target["FWD"])

    # ---- Normalize mode ----
    mode_norm <- tolower(mode)
    if (mode_norm %in% c("max_fis", "fis")) mode_norm <- "max_fis"

    if (!mode_norm %in% c("max_fis", "safe", "upside", "form", "fixture")) {
      print(paste0("[StartingXI] Unknown mode '", mode, "'. Defaulting to max_fis."))
      mode_norm <- "max_fis"
    }

    # ---- Ensure FIS columns exist ----
    if (!"fis_score" %in% colnames(squad_df)) {
      squad_df <- calculate_fis_score(squad_df)
    }

    # ---- Ensure required numeric columns exist with safe defaults ----
    squad_df$perf         <- safe_numeric(if ("perf" %in% colnames(squad_df)) squad_df$perf else NULL, 50)
    squad_df$form         <- safe_numeric(if ("form" %in% colnames(squad_df)) squad_df$form else NULL, 50)
    squad_df$momentum     <- safe_numeric(if ("momentum" %in% colnames(squad_df)) squad_df$momentum else NULL, 50)
    squad_df$fixture_risk <- safe_numeric(if ("fixture_risk" %in% colnames(squad_df)) squad_df$fixture_risk else NULL, 50)
    squad_df$fis_score    <- safe_numeric(if ("fis_score" %in% colnames(squad_df)) squad_df$fis_score else NULL, 50)

    # ---- Compute avg_pts_scaled from perf (already 0-100) ----
    avg_pts_scaled <- squad_df$perf

    # ---- Compute opt_score per mode ----
    if (mode_norm == "max_fis") {
      squad_df$opt_score <- squad_df$fis_score
    } else if (mode_norm == "safe") {
      squad_df$opt_score <- 0.5 * avg_pts_scaled +
                            0.3 * squad_df$form +
                            0.2 * (100 - squad_df$fixture_risk)
    } else if (mode_norm == "upside") {
      squad_df$opt_score <- 0.4 * squad_df$perf +
                            0.3 * squad_df$momentum +
                            0.3 * squad_df$form
    } else if (mode_norm == "form") {
      squad_df$opt_score <- squad_df$form
    } else if (mode_norm == "fixture") {
      squad_df$opt_score <- 100 - squad_df$fixture_risk
    }

    squad_df$opt_score <- safe_numeric(squad_df$opt_score, 50)

    # ---- Position mapping ----
    # Map role / role2 to GK, DEF, MID, FWD
    map_position <- function(role_val) {
      if (is.na(role_val) || role_val == "" || role_val == "Unknown") return("Unknown")
      r <- tolower(trimws(as.character(role_val)))
      if (r %in% c("goalkeeper", "portero", "gk")) return("GK")
      if (r %in% c("defender", "defensa", "df")) return("DEF")
      if (r %in% c("midfielder", "centrocampista", "md")) return("MID")
      if (r %in% c("forward", "delantero", "fw")) return("FWD")
      return("Unknown")
    }

    # Try role first, fall back to role2
    role_vec <- if ("role" %in% colnames(squad_df)) as.character(squad_df$role) else rep("Unknown", nrow(squad_df))
    role2_vec <- if ("role2" %in% colnames(squad_df)) as.character(squad_df$role2) else rep("", nrow(squad_df))

    squad_df$pos_group <- vapply(seq_len(nrow(squad_df)), function(i) {
      primary <- map_position(role_vec[i])
      if (primary != "Unknown") return(primary)
      secondary <- map_position(role2_vec[i])
      if (secondary != "Unknown") return(secondary)
      "Unknown"
    }, character(1))

    # ---- Greedy selection by position ----
    selected_idx <- integer(0)
    feasible <- TRUE

    # Helper: pick top N from a position group
    pick_from_group <- function(group, count, current_selected) {
      candidates <- squad_df$squad_pos_idx[
        squad_df$pos_group == group & !(squad_df$squad_pos_idx %in% current_selected)
      ]
      if (length(candidates) == 0) return(integer(0))
      # Sort by opt_score descending
      scores <- squad_df$opt_score[candidates]
      ord <- order(scores, decreasing = TRUE)
      picked <- candidates[ord[seq_len(min(count, length(candidates)))]]
      if (length(picked) < count) feasible <<- FALSE
      return(picked)
    }

    # Assign a stable index for tracking
    squad_df$squad_pos_idx <- seq_len(nrow(squad_df))

    # Pick GK
    selected_idx <- c(selected_idx, pick_from_group("GK", 1, selected_idx))

    # Pick DEF
    selected_idx <- c(selected_idx, pick_from_group("DEF", def_count, selected_idx))

    # Pick MID
    selected_idx <- c(selected_idx, pick_from_group("MID", mid_count, selected_idx))

    # Pick FWD
    selected_idx <- c(selected_idx, pick_from_group("FWD", fwd_count, selected_idx))

    # ---- Backfill from remaining if needed ----
    remaining <- squad_df$squad_pos_idx[!(squad_df$squad_pos_idx %in% selected_idx)]
    target_total <- 1 + def_count + mid_count + fwd_count  # 11
    if (length(selected_idx) < target_total && length(remaining) > 0) {
      feasible <- FALSE
      needed <- target_total - length(selected_idx)
      # Sort remaining by opt_score descending
      rem_scores <- squad_df$opt_score[remaining]
      rem_ord <- order(rem_scores, decreasing = TRUE)
      backfill <- remaining[rem_ord[seq_len(min(needed, length(remaining)))]]

      selected_idx <- c(selected_idx, backfill)
    }

    # ---- Build starting_xi and bench ----
    selected_idx <- unique(selected_idx)  # safety
    bench_idx <- squad_df$squad_pos_idx[!(squad_df$squad_pos_idx %in% selected_idx)]

    starting_xi <- squad_df[selected_idx, , drop = FALSE]
    bench <- squad_df[bench_idx, , drop = FALSE]

    # Clean internal tracking column
    starting_xi$squad_pos_idx <- NULL
    bench$squad_pos_idx <- NULL

    # ---- Compute return values ----
    total_score <- round(sum(starting_xi$opt_score, na.rm = TRUE), 1)
    avg_fis <- round(mean(starting_xi$fis_score, na.rm = TRUE), 1)

    list(
      starting_xi = starting_xi,
      bench = bench,
      formation = formation,
      mode = mode,
      total_score = total_score,
      avg_fis = avg_fis,
      feasible = feasible,
      formation_counts = c(GK = 1, DEF = def_count, MID = mid_count, FWD = fwd_count)
    )
  }, error = function(e) {
    print(paste0("[StartingXI] Error optimizing starting XI: ", e$message))
    empty_result
  })
}


# ============================================================
# 6. recommend_transfers
# ============================================================
# Suggests optimal sell-then-buy transfer pairs to improve squad
# quality within budget constraints.
#
# Parameters:
#   squad_df       -- data frame of owned players (must be enrichable
#                     with FIS scores).
#   market_df      -- data frame of available market players (must be
#                     enrichable with FIS scores).
#   current_budget -- numeric, available budget in same units as value.
#   max_transfers  -- integer, maximum number of recommendations.
#
# Returns:
#   Data frame with columns: sell_id, sell_name, sell_role, sell_val,
#   sell_fis, buy_id, buy_name, buy_role, buy_val, buy_fis,
#   net_cost, delta_fis, roi_pct.  Sorted by delta_fis descending.
# ============================================================

recommend_transfers <- function(squad_df, market_df, current_budget = 0, max_transfers = 5) {
  # ---- Default empty return ----
  empty_df <- data.frame(
    sell_id = character(0), sell_name = character(0), sell_role = character(0),
    sell_val = numeric(0), sell_fis = numeric(0),
    buy_id = character(0), buy_name = character(0), buy_role = character(0),
    buy_val = numeric(0), buy_fis = numeric(0),
    net_cost = numeric(0), delta_fis = numeric(0), roi_pct = numeric(0),
    stringsAsFactors = FALSE
  )

  tryCatch({
    # ---- Validate input ----
    if (is.null(squad_df) || nrow(squad_df) == 0 ||
        is.null(market_df) || nrow(market_df) == 0) {
      return(empty_df)
    }

    # ---- Ensure FIS scores ----
    if (!"fis_score" %in% colnames(squad_df)) {
      squad_df <- calculate_fis_score(squad_df)
    }
    if (!"fis_score" %in% colnames(market_df)) {
      market_df <- calculate_fis_score(market_df)
    }

    # ---- Safe numeric for value and fis_score ----
    squad_df$value     <- safe_numeric(if ("value" %in% colnames(squad_df)) squad_df$value else NULL, 0)
    squad_df$fis_score <- safe_numeric(squad_df$fis_score, 50)
    market_df$value     <- safe_numeric(if ("value" %in% colnames(market_df)) market_df$value else NULL, 0)
    market_df$fis_score <- safe_numeric(market_df$fis_score, 50)

    # ---- Candidate sells: Sell tier, Hold tier, or lowest FIS ----
    squad_df$fis_tier <- if ("fis_tier" %in% colnames(squad_df)) as.character(squad_df$fis_tier) else rep("Hold", nrow(squad_df))
    squad_df$fis_tier[is.na(squad_df$fis_tier)] <- "Hold"

    # Prioritize "Sell" tier, then "Hold" tier, then lowest FIS
    sell_candidates <- squad_df[squad_df$fis_tier %in% c("Sell", "Hold"), ]
    # If fewer than 5 candidates, include lowest FIS players
    if (nrow(sell_candidates) < 5) {
      squad_sorted <- squad_df[order(squad_df$fis_score), ]
      extra <- squad_sorted[!squad_sorted$id %in% sell_candidates$id, ]
      sell_candidates <- rbind(sell_candidates, head(extra, 5 - nrow(sell_candidates)))
    }
    # Sort by FIS ascending (worst first)
    sell_candidates <- sell_candidates[order(sell_candidates$fis_score), ]
    sell_candidates <- head(sell_candidates, 20)  # cap candidate pool

    # ---- Candidate buys: Strong Buy / Buy tier or highest FIS ----
    market_df$fis_tier <- if ("fis_tier" %in% colnames(market_df)) as.character(market_df$fis_tier) else rep("Hold", nrow(market_df))
    market_df$fis_tier[is.na(market_df$fis_tier)] <- "Hold"

    buy_candidates <- market_df[market_df$fis_tier %in% c("Strong Buy", "Buy"), ]
    # If fewer than 5 candidates, include highest FIS players
    if (nrow(buy_candidates) < 5) {
      market_sorted <- market_df[order(-market_df$fis_score), ]
      extra <- market_sorted[!market_sorted$id %in% buy_candidates$id, ]
      buy_candidates <- rbind(buy_candidates, head(extra, 5 - nrow(buy_candidates)))
    }
    # Sort by FIS descending (best first)
    buy_candidates <- buy_candidates[order(-buy_candidates$fis_score), ]
    buy_candidates <- head(buy_candidates, 20)  # cap candidate pool

    if (nrow(sell_candidates) == 0 || nrow(buy_candidates) == 0) {
      return(empty_df)
    }

    # ---- Generate all candidate pairs ----
    pairs_list <- list()
    pair_count <- 0

    for (si in seq_len(nrow(sell_candidates))) {
      s <- sell_candidates[si, ]
      for (bi in seq_len(nrow(buy_candidates))) {
        b <- buy_candidates[bi, ]

        # Skip if same player
        if (!is.na(s$id) && !is.na(b$id) && as.character(s$id) == as.character(b$id)) {
          next
        }

        sell_val <- safe_numeric(s$value, 0)
        buy_val  <- safe_numeric(b$value, 0)
        sell_fis <- safe_numeric(s$fis_score, 50)
        buy_fis  <- safe_numeric(b$fis_score, 50)

        net_cost <- buy_val - sell_val
        delta_fis <- buy_fis - sell_fis

        # Only positive improvement
        if (delta_fis <= 0) next

        # Budget feasibility: current_budget + sell_value >= buy_value
        if (current_budget + sell_val < buy_val) next

        # ROI: improvement relative to net cost
        roi_pct <- if (net_cost > 0) round(delta_fis / net_cost * 100, 2) else if (delta_fis > 0) 100.0 else 0.0

        pair_count <- pair_count + 1
        pairs_list[[pair_count]] <- data.frame(
          sell_id = as.character(s$id),
          sell_name = as.character(s$name),
          sell_role = as.character(s$role),
          sell_val = sell_val,
          sell_fis = sell_fis,
          buy_id = as.character(b$id),
          buy_name = as.character(b$name),
          buy_role = as.character(b$role),
          buy_val = buy_val,
          buy_fis = buy_fis,
          net_cost = net_cost,
          delta_fis = delta_fis,
          roi_pct = roi_pct,
          stringsAsFactors = FALSE
        )
      }
    }

    if (length(pairs_list) == 0) {
      return(empty_df)
    }

    result_df <- do.call(rbind, pairs_list)

    # Sort by delta_fis descending
    result_df <- result_df[order(-result_df$delta_fis), ]
    rownames(result_df) <- NULL

    # Return top max_transfers
    result_df <- head(result_df, max_transfers)

    return(result_df)
  }, error = function(e) {
    print(paste0("[TransferRec] Error recommending transfers: ", e$message))
    empty_df
  })
}


# ============================================================
# 7. simulate_transfer_scenario
# ============================================================
# Simulates a hypothetical transfer scenario: selling some players
# and buying others, then computing projected squad metrics.
#
# Parameters:
#   squad_df         -- data frame of current squad (must be enrichable
#                       with FIS scores).
#   current_budget   -- numeric, available budget.
#   sell_player_ids  -- character vector of player IDs to sell.
#   buy_player_ids   -- character vector of player IDs to buy.
#   market_df        -- data frame of market players (required if
#                       buy_player_ids is non-empty).
#
# Returns:
#   List with: projected_squad, total_sell_proceeds, total_buy_cost,
#   projected_budget, initial_total_val, projected_total_val,
#   initial_avg_fis, projected_avg_fis, delta_avg_fis,
#   is_budget_valid.
# ============================================================

simulate_transfer_scenario <- function(squad_df, current_budget = 0,
                                         sell_player_ids = character(0),
                                         buy_player_ids = character(0),
                                         market_df = NULL) {
  # ---- Default empty return ----
  empty_result <- list(
    projected_squad = data.frame(),
    total_sell_proceeds = 0,
    total_buy_cost = 0,
    projected_budget = current_budget,
    initial_total_val = 0,
    projected_total_val = 0,
    initial_avg_fis = 0,
    projected_avg_fis = 0,
    delta_avg_fis = 0,
    is_budget_valid = TRUE
  )

  tryCatch({
    # ---- Validate input ----
    if (is.null(squad_df) || nrow(squad_df) == 0) {
      return(empty_result)
    }

    # ---- Ensure FIS scores ----
    if (!"fis_score" %in% colnames(squad_df)) {
      squad_df <- calculate_fis_score(squad_df)
    }

    # ---- Safe numeric for value ----
    squad_df$value     <- safe_numeric(if ("value" %in% colnames(squad_df)) squad_df$value else NULL, 0)
    squad_df$fis_score <- safe_numeric(squad_df$fis_score, 50)

    # ---- Compute initial metrics ----
    initial_total_val <- sum(squad_df$value, na.rm = TRUE)
    initial_avg_fis   <- mean(squad_df$fis_score, na.rm = TRUE)

    # ---- Remove sold players ----
    sell_player_ids <- sell_player_ids[nzchar(as.character(sell_player_ids))]
    projected_squad <- squad_df[!as.character(squad_df$id) %in% as.character(sell_player_ids), , drop = FALSE]

    # ---- Compute sell proceeds ----
    sold_players <- squad_df[as.character(squad_df$id) %in% as.character(sell_player_ids), ]
    total_sell_proceeds <- sum(safe_numeric(sold_players$value, 0), na.rm = TRUE)

    # ---- Add bought players ----
    buy_player_ids <- buy_player_ids[nzchar(as.character(buy_player_ids))]
    total_buy_cost <- 0

    if (length(buy_player_ids) > 0) {
      if (is.null(market_df) || nrow(market_df) == 0) {
        print("[SimTransfer] market_df is NULL or empty; cannot add buy candidates.")
      } else {
        # Ensure market FIS scores
        if (!"fis_score" %in% colnames(market_df)) {
          market_df <- calculate_fis_score(market_df)
        }
        market_df$value     <- safe_numeric(if ("value" %in% colnames(market_df)) market_df$value else NULL, 0)
        market_df$fis_score <- safe_numeric(market_df$fis_score, 50)

        bought_players <- market_df[as.character(market_df$id) %in% as.character(buy_player_ids), ]
        total_buy_cost <- sum(safe_numeric(bought_players$value, 0), na.rm = TRUE)

        if (nrow(bought_players) > 0) {
          projected_squad <- rbind(projected_squad, bought_players)
        }
      }
    }

    # ---- Compute projected metrics ----
    projected_total_val <- sum(safe_numeric(projected_squad$value, 0), na.rm = TRUE)
    projected_avg_fis   <- if (nrow(projected_squad) > 0) mean(safe_numeric(projected_squad$fis_score, 50), na.rm = TRUE) else 0
    delta_avg_fis       <- round(projected_avg_fis - initial_avg_fis, 2)
    projected_budget    <- current_budget + total_sell_proceeds - total_buy_cost
    is_budget_valid     <- projected_budget >= 0

    # Reset row names
    rownames(projected_squad) <- NULL

    list(
      projected_squad = projected_squad,
      total_sell_proceeds = total_sell_proceeds,
      total_buy_cost = total_buy_cost,
      projected_budget = projected_budget,
      initial_total_val = initial_total_val,
      projected_total_val = projected_total_val,
      initial_avg_fis = round(initial_avg_fis, 2),
      projected_avg_fis = round(projected_avg_fis, 2),
      delta_avg_fis = delta_avg_fis,
      is_budget_valid = is_budget_valid
    )
  }, error = function(e) {
    print(paste0("[SimTransfer] Error simulating transfer scenario: ", e$message))
    empty_result
  })
}