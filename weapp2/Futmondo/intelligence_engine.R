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

    w_perf <- weights$perf %||% 0.30
    w_form <- weights$form %||% 0.20
    w_eff  <- weights$efficiency %||% 0.20
    w_mom  <- weights$momentum %||% 0.15
    w_fix  <- weights$fixture_risk %||% 0.15

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

    # ---- FIS tier ----
    fis_tier <- ifelse(fis_score >= 80, "Strong Buy",
                  ifelse(fis_score >= 65, "Buy",
                  ifelse(fis_score >= 45, "Hold", "Sell")))

    # ---- FIS summary ----
    player_names <- if ("name" %in% colnames(players_df)) as.character(players_df$name) else rep("Player", n)

    fis_summary <- vapply(seq_len(n), function(i) {
      tier_label <- fis_tier[i]
      name_label <- player_names[i]
      if (tier_label == "Strong Buy") {
        paste0(name_label, ": Exceptional value with strong performance and positive market momentum (FIS=", round(fis_score[i], 1), ").")
      } else if (tier_label == "Buy") {
        paste0(name_label, ": Solid buy candidate with good form and efficiency metrics (FIS=", round(fis_score[i], 1), ").")
      } else if (tier_label == "Hold") {
        paste0(name_label, ": Neutral outlook; monitor for form changes before acting (FIS=", round(fis_score[i], 1), ").")
      } else {
        paste0(name_label, ": Weak metrics or negative trend; consider selling to free budget (FIS=", round(fis_score[i], 1), ").")
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
#   user_cash        -- numeric, available budget (default 300M)
#
# Returns:
#   List with: fair_value, league_premium_pct, min_winning_bid,
#   recommended_bid, max_rational_bid, expected_roi_pct,
#   competition_level, likely_competitors, confidence_pct
# ============================================================

calculate_smart_bid <- function(player_row, championship_id,
                                 pressroom_df = NULL,
                                 user_teams_df = NULL,
                                 user_cash = 300000000) {
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

    # ---- min_winning_bid: fair_value + small premium ----
    min_winning_bid <- round(fair_value * 1.02)

    # ---- recommended_bid: balance between winning and value ----
    recommended_bid <- round(fair_value * (1 + league_premium_pct / 200))

    # ---- max_rational_bid: cap at 150% of fair value or user budget ----
    max_rational_bid <- min(round(fair_value * 1.5), user_cash)

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
      confidence_pct = confidence_pct
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
#   login          -- login token vector
#   championship_id -- character string
#   user_team_id   -- character, the current user's team
#   user_teams_df  -- data frame of all user teams
#   players_df     -- data frame of players (should have FIS scores)
#   pressroom_df   -- optional pressroom transactions
#
# Returns:
#   Data frame with: type, title, description, confidence_pct,
#   action_label, player_id
# ============================================================

generate_command_center_feed <- function(login, championship_id,
                                          user_team_id, user_teams_df,
                                          players_df, pressroom_df = NULL) {
  if (is.null(players_df) || nrow(players_df) == 0) {
    return(data.frame(
      type = character(0), title = character(0), description = character(0),
      confidence_pct = numeric(0), action_label = character(0),
      player_id = character(0), stringsAsFactors = FALSE
    ))
  }

  tryCatch({
    recommendations <- list()

    # ---- Ensure FIS scores exist ----
    if (!"fis_score" %in% colnames(players_df)) {
      players_df <- calculate_fis_score(players_df)
    }

    # ---- BUY recommendations: Strong Buy / Buy tier players on market ----
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
          player_id = as.character(p$id),
          stringsAsFactors = FALSE
        )
      }
    }

    # ---- SELL recommendations: Sell tier players owned by user ----
    # Check if players_df has user_team_id column (from roster)
    if ("user_team_id" %in% colnames(players_df)) {
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
            player_id = as.character(p$id),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    # ---- BID recommendations: players with active bids ----
    if ("bid_price" %in% colnames(players_df) && "user_team_id" %in% colnames(players_df)) {
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
            player_id = as.character(p$id),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    # ---- CLAUSE recommendations: players with active clauses ----
    if ("clause_price" %in% colnames(players_df)) {
      clause_candidates <- players_df[
        !is.na(players_df$clause_price) & players_df$clause_price > 0,
      ]
      if (nrow(clause_candidates) > 0) {
        # Filter to those with good FIS scores
        good_clause <- clause_candidates[clause_candidates$fis_tier %in% c("Strong Buy", "Buy"), ]
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
              player_id = as.character(p$id),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }

    # ---- HOLD recommendations: top Hold-tier owned players ----
    if ("user_team_id" %in% colnames(players_df)) {
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
            player_id = as.character(p$id),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    # ---- Combine and return ----
    if (length(recommendations) == 0) {
      return(data.frame(
        type = character(0), title = character(0), description = character(0),
        confidence_pct = numeric(0), action_label = character(0),
        player_id = character(0), stringsAsFactors = FALSE
      ))
    }

    result_df <- do.call(rbind, recommendations)
    # Sort by confidence descending
    result_df <- result_df[order(-result_df$confidence_pct), ]
    rownames(result_df) <- NULL

    return(result_df)
  }, error = function(e) {
    print(paste0("[CommandCenter] Error generating feed: ", e$message))
    data.frame(
      type = character(0), title = character(0), description = character(0),
      confidence_pct = numeric(0), action_label = character(0),
      player_id = character(0), stringsAsFactors = FALSE
    )
  })
}