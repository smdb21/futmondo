#!/usr/bin/env Rscript
# =============================================================================
# test_deterministic_fixes.R
# Standalone, OFFLINE deterministic tests for the approved fixes:
#   - FIS score (NA/empty role+status, non-finite weights)
#   - Smart bid (verified-funds bounds, market high bid, no 300M hardcode)
#   - Acquisition capacity + preflight (fail-closed, capacity, funds, modify)
#   - Roster clause payload (exact shape, no isClause)
#   - Today helpers (radar df, radar onClick JS, rec action onclick,
#     action label -> stable code mapping, player id resolution)
#   - Rivals helpers (buying power values, pivot ledger)
#   - Player points trace (one marker per round, graceful no-points)
#
# No login / network required. Run with:  Rscript test/test_deterministic_fixes.R
# When sourced by test_shiny_simulation.R (option deterministic_fixes_no_quit),
# it sets deterministic_fixes_all_passed instead of quitting.
# =============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(data.table)
  library(shiny)
  library(reactable)
  library(htmlwidgets)
})

# ---- Result accumulator ----
det_results <- list()
det_record <- function(label, expr) {
  res <- tryCatch(
    { expr; list(status = "pass", trace = character(0)) },
    error = function(e) list(status = "error", trace = conditionMessage(e))
  )
  det_results[[label]] <<- res
  if (res$status == "pass") {
    cat(sprintf("  [PASS] %s\n", label))
  } else {
    cat(sprintf("  [FAIL] %s -- %s\n", label, res$trace))
  }
  invisible(res)
}

cat("\n======================================================================\n")
cat("  DETERMINISTIC FIX TESTS (offline)\n")
cat("======================================================================\n")

# ---- Source the code under test (order matters for dependencies) ----
det_record("source_intelligence_engine", { source("intelligence_engine.R") })
det_record("source_futmondo_functions", { source("futmondo_functions.R") })
det_record("source_today_module", { source("Modules/Today_Module.R") })
det_record("source_rivals_module", { source("Modules/Rivals_Module.R") })
det_record("source_selected_player_module", { source("Modules/Selected_Player_Module.R") })

all_sourced <- !any(sapply(det_results, function(r) r$status == "error"))

if (!all_sourced) {
  cat("\n  FATAL: could not source code under test. Aborting.\n")
  deterministic_fixes_all_passed <- FALSE
  if (!getOption("deterministic_fixes_no_quit", FALSE)) quit(status = 1)
}

if (all_sourced) {

# =============================================================================
# 1. FIS score
# =============================================================================
cat("\n--- FIS score ---\n")

det_record("fis_na_empty_role_status_no_rows_dropped", {
  df <- data.frame(
    id = c("a", "b", "c"),
    name = c("A", "B", "C"),
    points = c(10, 20, 30),
    value = c(1000000, 2000000, 3000000),
    change = c(0, 100000, -50000),
    `average.average` = c(5, 8, 10),
    `average.averageLastFive` = c(6, 9, 11),
    `average.matches` = c(10, 12, 14),
    role = c(NA, "", "MF"),
    status = c(NA, "", "ok"),
    stringsAsFactors = FALSE
  )
  out <- calculate_fis_score(df)
  stopifnot(nrow(out) == 3)
  stopifnot(all(is.finite(out$fis_score)))
  stopifnot(all(out$fis_tier %in% c("Strong Buy", "Buy", "Hold", "Sell")))
  stopifnot(all(!is.na(out$fis_score)))
})

det_record("fis_non_finite_weights_fallback", {
  df <- data.frame(
    id = c("a", "b"),
    name = c("A", "B"),
    points = c(10, 20),
    value = c(1000000, 2000000),
    change = c(0, 100000),
    `average.average` = c(5, 8),
    `average.averageLastFive` = c(6, 9),
    `average.matches` = c(10, 12),
    role = c("MF", "MF"),
    status = c("ok", "ok"),
    stringsAsFactors = FALSE
  )
  # Non-finite / NA weights must fall back to defaults without error.
  out <- calculate_fis_score(df, weights = list(perf = NA, form = Inf, efficiency = -Inf, momentum = "x", fixture_risk = NULL))
  stopifnot(nrow(out) == 2)
  stopifnot(all(is.finite(out$fis_score)))
})

det_record("fis_default_weights_sum_to_one", {
  # With default weights the score is a weighted average in [0,100].
  df <- data.frame(
    id = c("a", "b", "c"),
    name = c("A", "B", "C"),
    points = c(10, 20, 30),
    value = c(1000000, 2000000, 3000000),
    change = c(0, 100000, -50000),
    `average.average` = c(5, 8, 10),
    `average.averageLastFive` = c(6, 9, 11),
    `average.matches` = c(10, 12, 14),
    role = c("GK", "DF", "MF"),
    status = c("ok", "ok", "ok"),
    stringsAsFactors = FALSE
  )
  out <- calculate_fis_score(df)
  stopifnot(all(out$fis_score >= 0 & out$fis_score <= 100))
})

# =============================================================================
# 2. Smart bid
# =============================================================================
cat("\n--- Smart bid ---\n")

player_row <- data.frame(
  id = "p1", name = "Player One",
  value = 20000000, change = 1000000, points = 25,
  `average.average` = 8, `average.averageLastFive` = 9, `average.matches` = 12,
  status = "ok", role = "MF", clause_price = NA,
  stringsAsFactors = FALSE
)

det_record("smart_bid_capacity_bounds_by_verified_funds", {
  # Verified spendable funds of 10M must bound max_rational and recommended.
  capacity <- list(
    status = "ok",
    roster = list(count = 5L, cap = 25L, remaining_slots = 20L),
    funds = list(reported_budget = 10000000, withheld = 0, spendable_budget = 10000000),
    outstanding = list(offers = list(), count = 0L, total_amount = 0, completeness = "complete"),
    target = list(my_bid_id = NULL, my_bid_amount = NA_real_, highest_bid = 0, bid_count = 0L),
    diagnostics = character(0)
  )
  res <- calculate_smart_bid(
    player_row = player_row, championship_id = "champ",
    pressroom_df = NULL, user_teams_df = NULL,
    user_cash = NA, market_high_bid = NULL, capacity = capacity
  )
  stopifnot(isTRUE(res$funds_verified))
  stopifnot(res$spendable_funds == 10000000)
  stopifnot(res$max_rational_bid <= 10000000)
  stopifnot(res$recommended_bid <= 10000000)
})

det_record("smart_bid_no_300m_hardcode_unverified", {
  # user_cash = NA and no capacity -> unverified engine default (300M) but
  # funds_verified must be FALSE (i.e. NOT treated as verified funds).
  res <- calculate_smart_bid(
    player_row = player_row, championship_id = "champ",
    pressroom_df = NULL, user_teams_df = NULL,
    user_cash = NA, market_high_bid = NULL, capacity = NULL
  )
  stopifnot(isFALSE(res$funds_verified))
  stopifnot(res$spendable_funds == 300000000)
})

det_record("smart_bid_market_high_bid_raises_min_winning", {
  base <- calculate_smart_bid(player_row = player_row, championship_id = "champ",
                              pressroom_df = NULL, user_teams_df = NULL,
                              user_cash = NA, market_high_bid = NULL, capacity = NULL)
  # A competing bid ABOVE the base minimum (fair_value*1.02) must raise it.
  with_mhb <- calculate_smart_bid(player_row = player_row, championship_id = "champ",
                                  pressroom_df = NULL, user_teams_df = NULL,
                                  user_cash = NA, market_high_bid = 25000000, capacity = NULL)
  stopifnot(with_mhb$min_winning_bid > base$min_winning_bid)
  stopifnot(with_mhb$market_high_bid == 25000000)
  stopifnot(with_mhb$min_winning_bid >= round(25000000 * 1.01))
})

det_record("smart_bid_recommended_never_exceeds_spendable", {
  capacity <- list(
    status = "ok",
    funds = list(reported_budget = 5000000, withheld = 0, spendable_budget = 5000000),
    roster = list(count = 5L, cap = 25L, remaining_slots = 20L),
    outstanding = list(offers = list(), count = 0L, total_amount = 0, completeness = "complete"),
    target = list(my_bid_id = NULL, my_bid_amount = NA_real_, highest_bid = 0, bid_count = 0L),
    diagnostics = character(0)
  )
  res <- calculate_smart_bid(player_row = player_row, championship_id = "champ",
                             pressroom_df = NULL, user_teams_df = NULL,
                             user_cash = NA, market_high_bid = NULL, capacity = capacity)
  stopifnot(res$recommended_bid <= 5000000)
  stopifnot(res$max_rational_bid <= 5000000)
})

# =============================================================================
# 3. Acquisition capacity + preflight
# =============================================================================
cat("\n--- Acquisition capacity + preflight ---\n")

# Seed the cache to simulate a verified snapshot (offline).
login <- c(token = "fake_token", userid = "fake_userid", user_name = "test")
champ <- "champX"; team <- "teamY"; pid <- "playerZ"

det_record("capacity_ok_snapshot", {
  clear_api_cache()
  api_cache_env[[paste0("team_info_", champ, "_", team)]] <- list(
    data = list(budget = 50000000, withheld = 10000000,
                configuration = list(maxPlayersInRoster = 25)),
    time = Sys.time())
  api_cache_env[[paste0("roster_", champ, "_", team)]] <- list(
    data = data.frame(id = paste0("r", 1:24), name = paste0("P", 1:24), stringsAsFactors = FALSE),
    time = Sys.time())
  api_cache_env[[paste0("roster_bids_", champ, "_", team)]] <- list(
    data = data.frame(id = c("b1", "b2"), bid_price = c(5000000, 7000000),
                      bid_user = c("Futmondo", "Futmondo"), bid_id = c("bid1", "bid2"),
                      bidder_team_id = c(team, team), stringsAsFactors = FALSE),
    time = Sys.time())
  api_cache_env[[paste0("player_summary_", "fake_userid", "_", champ, "_", team, "_", pid)]] <- list(
    data = list(data = NULL, prices = list(),
                bids = list(
                  list(id = "bid1", price = 5000000, userTeam = list("_id" = team, name = "Me")),
                  list(id = "bid2", price = 7000000, userTeam = list("_id" = "other", name = "Rival"))
                ),
                my_bid_id = "bid1", my_bid_price = 5000000),
    time = Sys.time())

  cap <- get_acquisition_capacity(login, champ, team, target_player_id = pid)
  stopifnot(cap$status == "ok")
  stopifnot(cap$roster$count == 24L, cap$roster$cap == 25L, cap$roster$remaining_slots == 1L)
  stopifnot(cap$funds$spendable_budget == 40000000)
  stopifnot(cap$outstanding$count == 2L, cap$outstanding$completeness == "complete")
  stopifnot(cap$target$my_bid_id == "bid1", cap$target$my_bid_amount == 5000000)
  stopifnot(cap$target$highest_bid == 7000000, cap$target$bid_count == 2L)
})

det_record("preflight_at_cap_bid_rejected", {
  cap <- list(status = "ok",
              roster = list(count = 25L, cap = 25L, remaining_slots = 0L),
              funds = list(reported_budget = 50000000, withheld = 0, spendable_budget = 50000000),
              outstanding = list(offers = list(), count = 0L, total_amount = 0, completeness = "complete"),
              target = list(my_bid_id = NULL, my_bid_amount = NA_real_, highest_bid = 0, bid_count = 0L),
              diagnostics = character(0))
  r <- evaluate_acquisition_preflight(cap, "bid", amount = 1000000)
  stopifnot(!r$ok, r$reason == "capacity")
})

det_record("preflight_one_slot_free_bid_ok", {
  cap <- list(status = "ok",
              roster = list(count = 24L, cap = 25L, remaining_slots = 1L),
              funds = list(reported_budget = 50000000, withheld = 0, spendable_budget = 50000000),
              outstanding = list(offers = list(), count = 0L, total_amount = 0, completeness = "complete"),
              target = list(my_bid_id = NULL, my_bid_amount = NA_real_, highest_bid = 0, bid_count = 0L),
              diagnostics = character(0))
  r <- evaluate_acquisition_preflight(cap, "bid", amount = 1000000)
  stopifnot(r$ok, r$reason == "ok")
})

det_record("preflight_multi_outstanding_bid_rejected_clause_ok", {
  cap <- list(status = "ok",
              roster = list(count = 24L, cap = 25L, remaining_slots = 1L),
              funds = list(reported_budget = 50000000, withheld = 0, spendable_budget = 50000000),
              outstanding = list(offers = list(), count = 2L, total_amount = 12000000, completeness = "complete"),
              target = list(my_bid_id = NULL, my_bid_amount = NA_real_, highest_bid = 0, bid_count = 0L),
              diagnostics = character(0))
  r_bid <- evaluate_acquisition_preflight(cap, "bid", amount = 1000000)
  stopifnot(!r_bid$ok, r_bid$reason == "capacity")
  r_clause <- evaluate_acquisition_preflight(cap, "clause", amount = 1000000)
  stopifnot(r_clause$ok, r_clause$reason == "ok")
})

det_record("preflight_modify_ok_at_cap_missing_existing_unavailable", {
  cap <- list(status = "ok",
              roster = list(count = 25L, cap = 25L, remaining_slots = 0L),
              funds = list(reported_budget = 50000000, withheld = 0, spendable_budget = 50000000),
              outstanding = list(offers = list(), count = 0L, total_amount = 0, completeness = "complete"),
              target = list(my_bid_id = "bid1", my_bid_amount = 5000000, highest_bid = 5000000, bid_count = 1L),
              diagnostics = character(0))
  r_ok <- evaluate_acquisition_preflight(cap, "modify", amount = 8000000, existing_bid_amount = 5000000)
  stopifnot(r_ok$ok, r_ok$reason == "ok")
  r_no <- evaluate_acquisition_preflight(cap, "modify", amount = 8000000, existing_bid_amount = NULL)
  stopifnot(!r_no$ok, r_no$reason == "unavailable")
})

det_record("preflight_fail_closed_on_partial_unavailable_null", {
  cap_ok <- list(status = "ok",
                 roster = list(count = 5L, cap = 25L, remaining_slots = 20L),
                 funds = list(reported_budget = 50000000, withheld = 0, spendable_budget = 50000000),
                 outstanding = list(offers = list(), count = 0L, total_amount = 0, completeness = "complete"),
                 target = list(my_bid_id = NULL, my_bid_amount = NA_real_, highest_bid = 0, bid_count = 0L),
                 diagnostics = character(0))
  cap_part <- cap_ok; cap_part$status <- "partial"
  cap_unav <- cap_ok; cap_unav$status <- "unavailable"
  stopifnot(!evaluate_acquisition_preflight(cap_part, "bid", amount = 1000000)$ok)
  stopifnot(evaluate_acquisition_preflight(cap_part, "bid", amount = 1000000)$reason == "unavailable")
  stopifnot(evaluate_acquisition_preflight(cap_unav, "clause")$reason == "unavailable")
  stopifnot(evaluate_acquisition_preflight(NULL, "bid")$reason == "unavailable")
})

det_record("preflight_funds_over_spendable_rejected_modify_delta", {
  cap <- list(status = "ok",
              roster = list(count = 5L, cap = 25L, remaining_slots = 20L),
              funds = list(reported_budget = 40000000, withheld = 0, spendable_budget = 40000000),
              outstanding = list(offers = list(), count = 0L, total_amount = 0, completeness = "complete"),
              target = list(my_bid_id = "bid1", my_bid_amount = 5000000, highest_bid = 5000000, bid_count = 1L),
              diagnostics = character(0))
  r_over <- evaluate_acquisition_preflight(cap, "bid", amount = 50000000)
  stopifnot(!r_over$ok, r_over$reason == "funds")
  r_ok <- evaluate_acquisition_preflight(cap, "bid", amount = 30000000)
  stopifnot(r_ok$ok)
  # modify: delta = 45M - 5M = 40M <= 40M -> ok; 46M - 5M = 41M > 40M -> funds
  r_mod_ok <- evaluate_acquisition_preflight(cap, "modify", amount = 45000000, existing_bid_amount = 5000000)
  stopifnot(r_mod_ok$ok)
  r_mod_over <- evaluate_acquisition_preflight(cap, "modify", amount = 46000000, existing_bid_amount = 5000000)
  stopifnot(!r_mod_over$ok, r_mod_over$reason == "funds")
  # lowering a bid never needs new funds
  r_lower <- evaluate_acquisition_preflight(cap, "modify", amount = 2000000, existing_bid_amount = 5000000)
  stopifnot(r_lower$ok)
})

det_record("capacity_missing_withheld_partial_preflight_blocks", {
  clear_api_cache()
  # Team info WITHOUT a withheld field -> funds verification is incomplete.
  api_cache_env[[paste0("team_info_", champ, "_", team)]] <- list(
    data = list(budget = 50000000,
                configuration = list(maxPlayersInRoster = 25)),
    time = Sys.time())
  api_cache_env[[paste0("roster_", champ, "_", team)]] <- list(
    data = data.frame(id = paste0("r", 1:24), name = paste0("P", 1:24), stringsAsFactors = FALSE),
    time = Sys.time())
  api_cache_env[[paste0("roster_bids_", champ, "_", team)]] <- list(
    data = data.frame(id = c("b1"), bid_price = c(5000000),
                      bid_user = c("Futmondo"), bid_id = c("bid1"),
                      bidder_team_id = c(team), stringsAsFactors = FALSE),
    time = Sys.time())

  cap <- get_acquisition_capacity(login, champ, team, target_player_id = NULL)
  # Missing withheld -> NOT ok (funds verification incomplete); spendable is NA.
  stopifnot(cap$status == "partial")
  stopifnot(is.na(cap$funds$spendable_budget))
  # Preflight must block bid and clause (unavailable).
  r_bid <- evaluate_acquisition_preflight(cap, "bid", amount = 1000000)
  stopifnot(!r_bid$ok, r_bid$reason == "unavailable")
  r_clause <- evaluate_acquisition_preflight(cap, "clause", amount = 1000000)
  stopifnot(!r_clause$ok, r_clause$reason == "unavailable")
})

det_record("capacity_negative_withheld_partial", {
  clear_api_cache()
  # A negative withheld is invalid -> funds verification incomplete (fail closed).
  api_cache_env[[paste0("team_info_", champ, "_", team)]] <- list(
    data = list(budget = 50000000, withheld = -5000000,
                configuration = list(maxPlayersInRoster = 25)),
    time = Sys.time())
  api_cache_env[[paste0("roster_", champ, "_", team)]] <- list(
    data = data.frame(id = paste0("r", 1:24), name = paste0("P", 1:24), stringsAsFactors = FALSE),
    time = Sys.time())
  api_cache_env[[paste0("roster_bids_", champ, "_", team)]] <- list(
    data = data.frame(id = character(0), bid_price = numeric(0),
                      bid_user = character(0), bid_id = character(0),
                      bidder_team_id = character(0), stringsAsFactors = FALSE),
    time = Sys.time())
  cap <- get_acquisition_capacity(login, champ, team, target_player_id = NULL)
  stopifnot(cap$status == "partial")
  stopifnot(is.na(cap$funds$spendable_budget))
  stopifnot(!evaluate_acquisition_preflight(cap, "bid", amount = 1000000)$ok)
})

det_record("capacity_valid_zero_withheld_ok", {
  clear_api_cache()
  # withheld = 0 is valid -> status ok, spendable = budget.
  api_cache_env[[paste0("team_info_", champ, "_", team)]] <- list(
    data = list(budget = 50000000, withheld = 0,
                configuration = list(maxPlayersInRoster = 25)),
    time = Sys.time())
  api_cache_env[[paste0("roster_", champ, "_", team)]] <- list(
    data = data.frame(id = paste0("r", 1:24), name = paste0("P", 1:24), stringsAsFactors = FALSE),
    time = Sys.time())
  api_cache_env[[paste0("roster_bids_", champ, "_", team)]] <- list(
    data = data.frame(id = character(0), bid_price = numeric(0),
                      bid_user = character(0), bid_id = character(0),
                      bidder_team_id = character(0), stringsAsFactors = FALSE),
    time = Sys.time())
  cap <- get_acquisition_capacity(login, champ, team, target_player_id = NULL)
  stopifnot(cap$status == "ok")
  stopifnot(cap$funds$spendable_budget == 50000000)
  stopifnot(evaluate_acquisition_preflight(cap, "bid", amount = 1000000)$ok)
})

det_record("capacity_mixed_bid_rival_not_mine", {
  clear_api_cache()
  api_cache_env[[paste0("team_info_", champ, "_", team)]] <- list(
    data = list(budget = 50000000, withheld = 0,
                configuration = list(maxPlayersInRoster = 25)),
    time = Sys.time())
  api_cache_env[[paste0("roster_", champ, "_", team)]] <- list(
    data = data.frame(id = paste0("r", 1:24), name = paste0("P", 1:24), stringsAsFactors = FALSE),
    time = Sys.time())
  api_cache_env[[paste0("roster_bids_", champ, "_", team)]] <- list(
    data = data.frame(id = character(0), bid_price = numeric(0),
                      bid_user = character(0), bid_id = character(0),
                      bidder_team_id = character(0), stringsAsFactors = FALSE),
    time = Sys.time())
  # The target player has only a RIVAL's bid (last). The summary-level
  # my_bid_id/my_bid_price points at that rival bid. We must NOT expose it as
  # ours (no immutable team-ID match, no unsafe fallback).
  api_cache_env[[paste0("player_summary_", "fake_userid", "_", champ, "_", team, "_", pid)]] <- list(
    data = list(data = NULL, prices = list(),
                bids = list(
                  list(id = "rival_bid", price = 9000000, userTeam = list("_id" = "other", name = "Rival"))
                ),
                my_bid_id = "rival_bid", my_bid_price = 9000000),
    time = Sys.time())

  cap <- get_acquisition_capacity(login, champ, team, target_player_id = pid)
  stopifnot(is.null(cap$target$my_bid_id))
  stopifnot(is.na(cap$target$my_bid_amount))
  # The rival's bid is still counted in the market (highest_bid / bid_count).
  stopifnot(cap$target$highest_bid == 9000000, cap$target$bid_count == 1L)
})

det_record("capacity_mixed_bid_own_preferred_over_rival", {
  clear_api_cache()
  api_cache_env[[paste0("team_info_", champ, "_", team)]] <- list(
    data = list(budget = 50000000, withheld = 0,
                configuration = list(maxPlayersInRoster = 25)),
    time = Sys.time())
  api_cache_env[[paste0("roster_", champ, "_", team)]] <- list(
    data = data.frame(id = paste0("r", 1:24), name = paste0("P", 1:24), stringsAsFactors = FALSE),
    time = Sys.time())
  api_cache_env[[paste0("roster_bids_", champ, "_", team)]] <- list(
    data = data.frame(id = character(0), bid_price = numeric(0),
                      bid_user = character(0), bid_id = character(0),
                      bidder_team_id = character(0), stringsAsFactors = FALSE),
    time = Sys.time())
  # Rival's bid is LAST (higher price); ours is first. We must expose OURS,
  # never the rival's "last bid".
  api_cache_env[[paste0("player_summary_", "fake_userid", "_", champ, "_", team, "_", pid)]] <- list(
    data = list(data = NULL, prices = list(),
                bids = list(
                  list(id = "my_bid", price = 5000000, userTeam = list("_id" = team, name = "Me")),
                  list(id = "rival_bid", price = 9000000, userTeam = list("_id" = "other", name = "Rival"))
                ),
                my_bid_id = "rival_bid", my_bid_price = 9000000),
    time = Sys.time())

  cap <- get_acquisition_capacity(login, champ, team, target_player_id = pid)
  stopifnot(cap$target$my_bid_id == "my_bid")
  stopifnot(cap$target$my_bid_amount == 5000000)
  stopifnot(cap$target$highest_bid == 9000000)
})

det_record("player_summary_cache_key_no_collision", {
  clear_api_cache()
  # my_bid_id/my_bid_price are viewer/team-specific: two different user/team
  # cache keys must NOT collide (no shared player_summary_<champ>_<pid> key).
  login_a <- c(token = "tokenA", userid = "userA", user_name = "A")
  login_b <- c(token = "tokenB", userid = "userB", user_name = "B")
  key_a <- paste0("player_summary_", "userA", "_", champ, "_", team, "_", pid)
  key_b <- paste0("player_summary_", "userB", "_", champ, "_", team, "_", pid)
  stopifnot(key_a != key_b)
  api_cache_env[[key_a]] <- list(
    data = list(data = NULL, prices = list(),
                bids = list(list(id = "bidA", price = 1000000, userTeam = list("_id" = team, name = "A"))),
                my_bid_id = "bidA", my_bid_price = 1000000),
    time = Sys.time())
  api_cache_env[[key_b]] <- list(
    data = list(data = NULL, prices = list(),
                bids = list(list(id = "bidB", price = 2000000, userTeam = list("_id" = team, name = "B"))),
                my_bid_id = "bidB", my_bid_price = 2000000),
    time = Sys.time())
  # Each viewer gets its own cached data (no cross-contamination).
  res_a <- get_player_summary(login_a, champ, team, pid)
  res_b <- get_player_summary(login_b, champ, team, pid)
  stopifnot(res_a$my_bid_id == "bidA", res_a$my_bid_price == 1000000)
  stopifnot(res_b$my_bid_id == "bidB", res_b$my_bid_price == 2000000)

  # A different team for the SAME user is also a distinct key.
  team2 <- "teamZ"
  key_c <- paste0("player_summary_", "userA", "_", champ, "_", team2, "_", pid)
  stopifnot(key_a != key_c)
  api_cache_env[[key_c]] <- list(
    data = list(data = NULL, prices = list(),
                bids = list(list(id = "bidC", price = 3000000, userTeam = list("_id" = team2, name = "C"))),
                my_bid_id = "bidC", my_bid_price = 3000000),
    time = Sys.time())
  res_c <- get_player_summary(login_a, champ, team2, pid)
  stopifnot(res_c$my_bid_id == "bidC", res_c$my_bid_price == 3000000)
  # userA/team still returns its own data (not overwritten by team2).
  res_a2 <- get_player_summary(login_a, champ, team, pid)
  stopifnot(res_a2$my_bid_id == "bidA", res_a2$my_bid_price == 1000000)
})

# =============================================================================
# 4. Roster clause payload
# =============================================================================
cat("\n--- Roster clause payload ---\n")

det_record("roster_clause_payload_exact_shape_no_isclause", {
  p <- build_roster_clause_payload(login, champ, team, pid, "slugZ", 12345678)
  stopifnot(identical(names(p), c("header", "query", "answer")))
  stopifnot(identical(names(p$header), c("token", "userid")))
  stopifnot(identical(names(p$query), c("championshipId", "userteamId", "player_slug", "player_id", "price")))
  stopifnot(!("isClause" %in% names(p$query)))
  stopifnot(p$header$token == "fake_token", p$header$userid == "fake_userid")
  stopifnot(p$query$championshipId == champ, p$query$userteamId == team)
  stopifnot(p$query$player_id == pid, p$query$player_slug == "slugZ")
  stopifnot(p$query$price == 12345678)
})

# =============================================================================
# 5. Today helpers
# =============================================================================
cat("\n--- Today helpers ---\n")

det_record("today_prepare_radar_df_filters_non_finite_fis", {
  df <- data.frame(
    id = c("a", "b", "c", "d", "e"),
    name = c("A", "B", "C", "D", "E"),
    role = c("GK", "DF", "MF", "FW", "MF"),
    value = c(100, 200, 300, 400, 500),
    fis_score = c(NA, Inf, -Inf, 85, 70),
    fis_tier = c("Hold", "Strong Buy", "Hold", "Buy", "Hold"),
    stringsAsFactors = FALSE
  )
  out <- today_prepare_radar_df(df, top_n = 10)
  stopifnot(nrow(out) == 2)
  stopifnot(all(out$PlayerID %in% c("d", "e")))
  stopifnot(out$PlayerID[1] == "d", out$PlayerID[2] == "e")  # sorted desc
  stopifnot(all(is.finite(out$FIS)))
})

det_record("today_prepare_radar_df_top_n_cap", {
  df <- data.frame(
    id = paste0("p", 1:15), name = paste0("P", 1:15),
    fis_score = seq(95, 81, length.out = 15),
    stringsAsFactors = FALSE
  )
  out <- today_prepare_radar_df(df, top_n = 10)
  stopifnot(nrow(out) == 10)
  stopifnot(out$FIS[1] == 95)
})

det_record("today_radar_onclick_js_namespaced", {
  ns <- NS("today_test")
  js <- today_radar_onclick_js(ns)
  stopifnot(reactable:::is.JS(js))
  s <- as.character(js)
  stopifnot(grepl("function(rowInfo, column, state)", s, fixed = TRUE))
  stopifnot(grepl("rowInfo.values.PlayerID", s, fixed = TRUE))
  stopifnot(grepl("Shiny.setInputValue('today_test-radar_selected_player'", s, fixed = TRUE))
  stopifnot(grepl("{priority: 'event'}", s, fixed = TRUE))
  # reactable accepts it
  rt <- reactable(data.frame(Player = "A", PlayerID = "p1", stringsAsFactors = FALSE),
                  onClick = js, columns = list(PlayerID = colDef(show = FALSE)))
  stopifnot(inherits(rt, "reactable"))
})

det_record("today_rec_action_onclick_js_escaping", {
  ns <- NS("today_test")
  oc <- today_rec_action_onclick_js(ns, "player123", "Buy Now")
  stopifnot(grepl("Shiny.setInputValue('today_test-rec_action_clicked'", oc, fixed = TRUE))
  stopifnot(grepl("player_id: 'player123'", oc, fixed = TRUE))
  stopifnot(grepl("action: 'Buy Now'", oc, fixed = TRUE))
  oc2 <- today_rec_action_onclick_js(ns, "o'brien", "Buy 'X'")
  stopifnot(grepl("player_id: 'o\\'brien'", oc2, fixed = TRUE))
  stopifnot(grepl("action: 'Buy \\'X\\''", oc2, fixed = TRUE))
})

det_record("today_normalize_action_place_bid_maps_to_market_bid", {
  stopifnot(today_normalize_action("Place Bid") == "market_bid")
  stopifnot(today_normalize_action("place bid") == "market_bid")
  stopifnot(today_normalize_action("PLACE BID") == "market_bid")
  stopifnot(today_normalize_action("  Place Bid  ") == "market_bid")
})

det_record("today_normalize_action_stable_codes_passthrough", {
  stopifnot(today_normalize_action("market_bid") == "market_bid")
  stopifnot(today_normalize_action("view") == "view")
  stopifnot(today_normalize_action("VIEW") == "view")
})

det_record("today_normalize_action_unsupported_maps_to_view", {
  stopifnot(today_normalize_action("Buy Now") == "view")
  stopifnot(today_normalize_action("Sell Now") == "view")
  stopifnot(today_normalize_action("View") == "view")
  stopifnot(today_normalize_action("") == "view")
  stopifnot(today_normalize_action(NULL) == "view")
  stopifnot(today_normalize_action(NA) == "view")
})

det_record("today_resolve_player_known_id_returns_single_row", {
  df <- data.frame(id = c("p1", "p2", "p3"), name = c("A", "B", "C"),
                   value = c(100, 200, 300), stringsAsFactors = FALSE)
  row <- today_resolve_player("p2", df)
  stopifnot(is.data.frame(row), nrow(row) == 1)
  stopifnot(as.character(row$id) == "p2", row$name == "B")
  # Numeric ids resolve against character ids (immutable id comparison).
  df2 <- data.frame(id = c(101, 102), name = c("X", "Y"), stringsAsFactors = FALSE)
  row2 <- today_resolve_player("102", df2)
  stopifnot(is.data.frame(row2), nrow(row2) == 1, row2$name == "Y")
})

det_record("today_resolve_player_unknown_or_missing_returns_null", {
  df <- data.frame(id = c("p1", "p2"), name = c("A", "B"), stringsAsFactors = FALSE)
  stopifnot(is.null(today_resolve_player("nope", df)))
  stopifnot(is.null(today_resolve_player("", df)))
  stopifnot(is.null(today_resolve_player(NULL, df)))
  stopifnot(is.null(today_resolve_player("p1", data.frame())))
  stopifnot(is.null(today_resolve_player("p1", NULL)))
  stopifnot(is.null(today_resolve_player("p1", data.frame(name = "A", stringsAsFactors = FALSE))))
})

# ---- Action-aware resolver: market_bid resolves from market data ONLY ----
# An owned / non-listed player (present in all-players but absent from market
# data) must NOT resolve for a market_bid action; it resolves for view.
det_record("today_resolve_for_action_market_bid_resolves_from_market_only", {
  market_df <- data.frame(id = c("m1", "m2"), name = c("MktA", "MktB"),
                          stringsAsFactors = FALSE)
  all_df <- data.frame(id = c("m1", "m2", "owned1"), name = c("MktA", "MktB", "Owned"),
                       stringsAsFactors = FALSE)
  # market_bid: resolves a market-listed player from market data.
  r1 <- today_resolve_player_for_action("m1", "market_bid", market_df, all_df)
  stopifnot(is.data.frame(r1), nrow(r1) == 1, as.character(r1$id) == "m1")
  # market_bid: an owned / non-listed player (only in all_df) must NOT resolve.
  r2 <- today_resolve_player_for_action("owned1", "market_bid", market_df, all_df)
  stopifnot(is.null(r2))
  # market_bid: a market player is resolved even though it also appears in all_df.
  r3 <- today_resolve_player_for_action("m2", "Place Bid", market_df, all_df)
  stopifnot(is.data.frame(r3), nrow(r3) == 1, as.character(r3$id) == "m2")
})

det_record("today_resolve_for_action_view_resolves_from_all_players", {
  market_df <- data.frame(id = c("m1"), name = c("MktA"), stringsAsFactors = FALSE)
  all_df <- data.frame(id = c("m1", "owned1"), name = c("MktA", "Owned"),
                       stringsAsFactors = FALSE)
  # view: an owned / non-market player resolves from all data.
  r1 <- today_resolve_player_for_action("owned1", "view", market_df, all_df)
  stopifnot(is.data.frame(r1), nrow(r1) == 1, r1$name == "Owned")
  # view: a market player also resolves from all data.
  r2 <- today_resolve_player_for_action("m1", "view", market_df, all_df)
  stopifnot(is.data.frame(r2), nrow(r2) == 1, as.character(r2$id) == "m1")
  # unknown id for view does not resolve.
  stopifnot(is.null(today_resolve_player_for_action("nope", "view", market_df, all_df)))
})

det_record("today_resolve_for_action_market_bid_unknown_or_empty_market_null", {
  market_df <- data.frame(id = character(0), name = character(0), stringsAsFactors = FALSE)
  all_df <- data.frame(id = c("m1"), name = c("MktA"), stringsAsFactors = FALSE)
  # Empty market data: a market_bid cannot resolve (fail closed), even if the
  # player is present in all data.
  stopifnot(is.null(today_resolve_player_for_action("m1", "market_bid", market_df, all_df)))
  # NULL market data also fails closed for market_bid.
  stopifnot(is.null(today_resolve_player_for_action("m1", "market_bid", NULL, all_df)))
})

# ---- Pure market-offer open decision ----
# The shared decision point: preflight fail -> no open; preflight ok -> open;
# no valid player -> no open (regardless of preflight).
det_record("market_offer_decision_preflight_fail_blocks_open", {
  sp <- data.frame(id = "p1", name = "A", stringsAsFactors = FALSE)
  # Capacity rejection.
  d1 <- market_offer_decision(sp, list(ok = FALSE, reason = "capacity", message = "full"))
  stopifnot(isFALSE(d1$open), d1$reason == "capacity")
  # Funds rejection.
  d2 <- market_offer_decision(sp, list(ok = FALSE, reason = "funds", message = "no funds"))
  stopifnot(isFALSE(d2$open), d2$reason == "funds")
  # Unavailable verification.
  d3 <- market_offer_decision(sp, list(ok = FALSE, reason = "unavailable", message = "x"))
  stopifnot(isFALSE(d3$open), d3$reason == "unavailable")
  # NULL preflight (verification failed to run) fails closed.
  d4 <- market_offer_decision(sp, NULL)
  stopifnot(isFALSE(d4$open), d4$reason == "unavailable")
})

det_record("market_offer_decision_preflight_ok_opens", {
  sp <- data.frame(id = "p1", name = "A", stringsAsFactors = FALSE)
  d <- market_offer_decision(sp, list(ok = TRUE, reason = "ok", message = NULL))
  stopifnot(isTRUE(d$open), d$reason == "ok")
})

det_record("market_offer_decision_no_player_blocks_open", {
  # No valid player: blocked even if preflight would pass.
  d1 <- market_offer_decision(NULL, list(ok = TRUE, reason = "ok", message = NULL))
  stopifnot(isFALSE(d1$open), d1$reason == "no_player")
  d2 <- market_offer_decision(data.frame(id = "", name = "A", stringsAsFactors = FALSE),
                              list(ok = TRUE, reason = "ok", message = NULL))
  stopifnot(isFALSE(d2$open), d2$reason == "no_player")
})

# =============================================================================
# 6. Rivals helpers
# =============================================================================
cat("\n--- Rivals helpers ---\n")

teams <- data.frame(teamid = c("T1", "T2"), teamname = c("Alpha", "Beta"), stringsAsFactors = FALSE)
pr <- data.frame(
  id = c("1", "2", "3"),
  created = c("2026-08-01T10:00:00Z", "2026-08-20T10:00:00Z", "2026-08-10T10:00:00Z"),
  player_id = c("p1", "p1", "p2"),
  player_name = c("A", "A", "B"),
  buyer_team_id = c("T1", "T2", "T2"),
  seller_team_id = c(NA, "T1", NA),
  price = c(10000000, 5000000, 8000000),
  stringsAsFactors = FALSE
)
start_dt <- as.POSIXct("2026-08-05", tz = "UTC")
end_dt <- as.POSIXct("2026-08-25", tz = "UTC")

det_record("rivals_cash_uses_all_transfers_through_end", {
  cash <- rivals_buying_power_values(pr, teams, metric = "cash", start_date = start_dt, end_date = end_dt)
  stopifnot(cash$value[cash$team_id == "T1"] == 295000000)  # 300 - 10 + 5
  stopifnot(cash$value[cash$team_id == "T2"] == 287000000)  # 300 - (5+8)
  stopifnot(all(cash$range_label == "all transfers through end date"))
})

det_record("rivals_investment_volume_use_range", {
  inv <- rivals_buying_power_values(pr, teams, metric = "investment", start_date = start_dt, end_date = end_dt)
  stopifnot(inv$value[inv$team_id == "T1"] == 0)      # 10M buy before start
  stopifnot(inv$value[inv$team_id == "T2"] == 13000000)
  stopifnot(all(inv$range_label == "within selected range"))
  vol <- rivals_buying_power_values(pr, teams, metric = "volume", start_date = start_dt, end_date = end_dt)
  stopifnot(vol$value[vol$team_id == "T1"] == 5000000)
  stopifnot(vol$value[vol$team_id == "T2"] == 13000000)
})

det_record("rivals_pivot_ledger_pair_by_id_no_sold_raw_dates", {
  pr2 <- data.frame(
    id = c("1", "2", "3"),
    created = c("2026-08-01T10:00:00Z", "2026-08-20T10:00:00Z", "2026-08-05T10:00:00Z"),
    player_id = c("p1", "p1", "p2"),
    player_name = c("A", "A", "B"),
    buyer_team_id = c("T1", "T2", "T1"),
    seller_team_id = c(NA, "T1", "T1"),
    price = c(10000000, 15000000, 3000000),
    stringsAsFactors = FALSE
  )
  ledger <- rivals_build_pivot_ledger(pr2, "T1")
  stopifnot(nrow(ledger) == 2)
  stopifnot(!("Sold" %in% colnames(ledger)))
  stopifnot("PlayerID" %in% colnames(ledger))
  p1 <- ledger[ledger$PlayerID == "p1", ]
  stopifnot(p1$Bought_Price == 10000000, p1$Sold_Price == 15000000, p1$Net_PL == 5000000)
  p2 <- ledger[ledger$PlayerID == "p2", ]
  stopifnot(p2$Bought_Price == 3000000, is.na(p2$Sold_Price), is.na(p2$Net_PL))
  stopifnot(ledger$Buy_Date[ledger$PlayerID == "p1"] == "2026-08-01T10:00:00Z")
  stopifnot(ledger$PlayerID[1] == "p2", ledger$PlayerID[2] == "p1")  # sorted desc
})

det_record("rivals_pivot_ledger_fallback_name", {
  pr3 <- data.frame(
    id = c("1", "2"),
    created = c("2026-08-01T10:00:00Z", "2026-08-20T10:00:00Z"),
    player_name = c("X", "X"),
    buyer_team_id = c("T1", "T2"),
    seller_team_id = c(NA, "T1"),
    price = c(1000000, 2000000),
    stringsAsFactors = FALSE
  )
  ledger3 <- rivals_build_pivot_ledger(pr3, "T1")
  stopifnot(nrow(ledger3) == 1)
  stopifnot(ledger3$PlayerID == "X")
  stopifnot(ledger3$Sold_Price == 2000000)
})

det_record("rivals_parse_datetime_invalid_na", {
  # Invalid timestamps remain NA (never substituted with Sys.time()).
  parsed <- rivals_parse_datetime(c("2026-08-01T10:00:00Z", "not-a-date", "", NA))
  stopifnot(!is.na(parsed[1]))
  stopifnot(is.na(parsed[2]), is.na(parsed[3]), is.na(parsed[4]))
  stopifnot(!identical(parsed[2], Sys.time()))
})

det_record("rivals_pivot_ledger_skips_invalid_timestamps", {
  # A buy with an invalid timestamp is skipped (not ordered/matched on now).
  pr4 <- data.frame(
    id = c("1", "2", "3"),
    created = c("not-a-date", "2026-08-01T10:00:00Z", "2026-08-20T10:00:00Z"),
    player_id = c("p1", "p1", "p1"),
    player_name = c("A", "A", "A"),
    buyer_team_id = c("T1", "T1", "T2"),
    seller_team_id = c(NA, NA, "T1"),
    price = c(9000000, 10000000, 15000000),
    stringsAsFactors = FALSE
  )
  ledger4 <- rivals_build_pivot_ledger(pr4, "T1")
  # Only the valid buy (10M) appears; the invalid-timestamp buy is skipped.
  stopifnot(nrow(ledger4) == 1)
  stopifnot(ledger4$Bought_Price == 10000000)
  stopifnot(ledger4$Sold_Price == 15000000)
})

det_record("rivals_tx_display_df_date_first_order", {
  # Columns arrive in a scrambled order; the display df must put the visible
  # columns first (date, type, concept, money, running_balance) then the hidden
  # helpers, preserving every field and value.
  df <- data.frame(
    id = c("a", "b"),
    category = c("market", "round"),
    timestamp = as.POSIXct(c("2026-08-01 10:00:00", "2026-08-02 10:00:00"), tz = "UTC"),
    batch_key = c("k1", "k2"),
    is_batch_header = c(TRUE, FALSE),
    batch_final_balance = c(100, 200),
    concept = c("Buy X", "Sell Y"),
    money = c(-50, 30),
    running_balance = c(50, 80),
    type = c("buy", "sell"),
    date = c("2026-08-01T10:00:00Z", "2026-08-02T10:00:00Z"),
    stringsAsFactors = FALSE
  )
  out <- rivals_tx_display_df(df)
  expected_order <- c("date", "type", "concept", "money", "running_balance",
                      "id", "category", "timestamp", "batch_key", "is_batch_header", "batch_final_balance")
  stopifnot(identical(colnames(out), expected_order))
  stopifnot(nrow(out) == 2)
  stopifnot(out$date[1] == "2026-08-01T10:00:00Z", out$money[1] == -50, out$running_balance[2] == 80)
})

# =============================================================================
# 7. Player points trace (bucketed by finished round)
# =============================================================================
cat("\n--- Player points trace ---\n")

# Shared finished-rounds fixture: boundaries at 08-01, 08-08, 08-15.
fr3 <- data.frame(
  round_number = c(1, 2, 3),
  begin_process = c("2026-08-01T10:00:00Z", "2026-08-08T10:00:00Z", "2026-08-15T10:00:00Z"),
  stringsAsFactors = FALSE
)

det_record("points_trace_one_marker_per_round", {
  # Multiple snapshots in one round -> the latest eligible is kept.
  # Round 1 window (-Inf, 08-01]: 07-30 (5) and 08-01 (6) -> keep 6.
  # Round 2 window (08-01, 08-08]: 08-05 (7) and 08-07 (8) -> keep 8.
  # Round 3 window (08-08, 08-15]: 08-12 (9) -> keep 9.
  df <- data.frame(
    recorded_at = c("2026-07-30T10:00:00Z", "2026-08-01T10:00:00Z",
                    "2026-08-05T10:00:00Z", "2026-08-07T10:00:00Z",
                    "2026-08-12T10:00:00Z"),
    points = c(5, 6, 7, 8, 9),
    stringsAsFactors = FALSE
  )
  r <- build_player_points_trace(df, fr3)
  stopifnot(r$has_points, nrow(r$points_df) == 3)
  stopifnot(all(r$points_df$round_number == c(1, 2, 3)))
  stopifnot(all(r$points_df$points == c(6, 8, 9)))
})

det_record("points_trace_missing_round_skipped", {
  # Only a snapshot in Round 1's window; Rounds 2 and 3 have no eligible
  # snapshot and must be skipped (no fabricated markers).
  df <- data.frame(
    recorded_at = "2026-07-30T10:00:00Z",
    points = 5,
    stringsAsFactors = FALSE
  )
  r <- build_player_points_trace(df, fr3)
  stopifnot(r$has_points, nrow(r$points_df) == 1)
  stopifnot(r$points_df$round_number == 1, r$points_df$points == 5)
})

det_record("points_trace_boundary_behavior", {
  # A snapshot exactly at a round boundary belongs to that round (at/before);
  # a snapshot one second after the prior boundary belongs to the next round.
  fr2 <- data.frame(
    round_number = c(1, 2),
    begin_process = c("2026-08-01T10:00:00Z", "2026-08-08T10:00:00Z"),
    stringsAsFactors = FALSE
  )
  df <- data.frame(
    recorded_at = c("2026-08-01T10:00:00Z", "2026-08-01T10:00:01Z"),
    points = c(5, 6),
    stringsAsFactors = FALSE
  )
  r <- build_player_points_trace(df, fr2)
  stopifnot(r$has_points, nrow(r$points_df) == 2)
  r1 <- r$points_df[r$points_df$round_number == 1, ]
  r2 <- r$points_df[r$points_df$round_number == 2, ]
  stopifnot(nrow(r1) == 1, nrow(r2) == 1, r1$points == 5, r2$points == 6)
})

det_record("points_trace_no_round_data", {
  # No finished-round data -> never render points from daily snapshots.
  df <- data.frame(
    recorded_at = c("2026-08-01T10:00:00Z", "2026-08-08T10:00:00Z"),
    points = c(5, 8),
    stringsAsFactors = FALSE
  )
  stopifnot(!build_player_points_trace(df, NULL)$has_points)
  stopifnot(!build_player_points_trace(df, data.frame())$has_points)
  # Finished rounds with no parseable begin_process -> no points.
  fr_bad <- data.frame(round_number = c(1, 2), begin_process = c("", ""), stringsAsFactors = FALSE)
  stopifnot(!build_player_points_trace(df, fr_bad)$has_points)
})

det_record("points_trace_graceful_no_points", {
  stopifnot(!build_player_points_trace(NULL, fr3)$has_points)
  stopifnot(!build_player_points_trace(data.frame(), fr3)$has_points)
  df_na <- data.frame(recorded_at = c("2026-08-01T10:00:00Z", "2026-08-08T10:00:00Z"),
                      points = c(NA, NA), stringsAsFactors = FALSE)
  stopifnot(!build_player_points_trace(df_na, fr3)$has_points)
  df_neg <- data.frame(recorded_at = c("2026-07-30T10:00:00Z", "2026-08-05T10:00:00Z"),
                       points = c(-1, 8), stringsAsFactors = FALSE)
  r_neg <- build_player_points_trace(df_neg, fr3)
  stopifnot(r_neg$has_points, nrow(r_neg$points_df) == 1, r_neg$points_df$points == 8)
})

det_record("points_trace_ignores_unfinished_round", {
  # An otherwise eligible snapshot under an UNFINISHED round must not produce a
  # marker; only the finished round does.
  fr <- data.frame(
    round_number = c(1, 2),
    begin_process = c("2026-08-01T10:00:00Z", "2026-08-08T10:00:00Z"),
    is_finished = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  # Round 1 (finished) has a snapshot; Round 2 (unfinished) also has one.
  df <- data.frame(
    recorded_at = c("2026-07-30T10:00:00Z", "2026-08-05T10:00:00Z"),
    points = c(5, 7),
    stringsAsFactors = FALSE
  )
  r <- build_player_points_trace(df, fr)
  stopifnot(r$has_points, nrow(r$points_df) == 1)
  stopifnot(r$points_df$round_number == 1, r$points_df$points == 5)
  # If every round is unfinished, no points are rendered at all.
  fr_none <- data.frame(
    round_number = c(1, 2),
    begin_process = c("2026-08-01T10:00:00Z", "2026-08-08T10:00:00Z"),
    is_finished = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  stopifnot(!build_player_points_trace(df, fr_none)$has_points)
})

}  # end if (all_sourced)

# =============================================================================
# SUMMARY
# =============================================================================
total <- length(det_results)
passed <- sum(sapply(det_results, function(r) r$status == "pass"))
failed <- total - passed
deterministic_fixes_all_passed <- (failed == 0)

cat("\n======================================================================\n")
cat(sprintf("  DETERMINISTIC FIX TESTS: %d passed / %d failed / %d total\n", passed, failed, total))
cat("======================================================================\n")

if (!deterministic_fixes_all_passed) {
  cat("  FAILED TESTS:\n")
  for (nm in names(det_results)) {
    if (det_results[[nm]]$status == "error") {
      cat(sprintf("    - %s: %s\n", nm, det_results[[nm]]$trace))
    }
  }
}

# Quit only when run directly (not when sourced by the simulation harness).
if (!getOption("deterministic_fixes_no_quit", FALSE)) {
  quit(status = if (deterministic_fixes_all_passed) 0 else 1)
}
