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
det_record("source_factual_helpers", { source("factual_helpers.R") })
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











# =============================================================================
# 3. Acquisition capacity + preflight
# =============================================================================
cat("\n--- Acquisition capacity + preflight ---\n")

# Seed the cache to simulate a verified snapshot (offline).
login <- c(token = "fake_token", userid = "fake_userid", user_name = "test")
champ <- "champX"; team <- "teamY"; pid <- "playerZ"
# Cache fixtures use the production account partition and only outgoing market bids.
det_seed_capacity <- function(info, market, incoming = data.frame()) {
  if (is.null(info$teamValue)) info$teamValue <- 0
  clear_api_cache()
  fixtures <- list(info, data.frame(id=paste0("r",1:24),name=paste0("P",1:24)),market,incoming)
  keys <- c(paste0("team_info_",champ,"_",team),paste0("roster_",champ,"_",team),
    paste0("market_",champ,"_",team),paste0("roster_bids_",champ,"_",team))
  for(i in seq_along(keys)) api_cache_env[[api_cache_key(keys[i],context=resource_context(login,champ,team))]] <- list(data=fixtures[[i]],time=Sys.time())
  empty_pressroom <- data.frame(player_id=character(),buyer_team_id=character(),
    seller_team_id=character(),created=character(),stringsAsFactors=FALSE)
  api_cache_env[[api_cache_key(paste0("pressroom_",champ),context=resource_context(login,champ))]] <-
    list(data=empty_pressroom,time=Sys.time())
}
det_empty_market <- function() data.frame(id=pid,bid_id=NA_character_,bid_price=NA_real_,numberOfBids=2)
det_original_post <- futmondo_post
futmondo_post <- function(...) stop("Unexpected external API request in offline deterministic test")


det_record("capacity_ok_snapshot", {
  det_seed_capacity(list(budget=50000000,withheld=10000000,configuration=list(maxPlayersInRoster=25)),
    data.frame(id=c(pid,"other_player"),bid_id=c("bid1","bid2"),bid_price=c(5000000,7000000),numberOfBids=c(2,1)))
  cap <- get_acquisition_capacity(login, champ, team, target_player_id=pid)
  stopifnot(cap$status=="ok",cap$roster$count==24L,cap$roster$cap==25L,cap$roster$remaining_slots==1L,
    cap$funds$spendable_budget==38000000,cap$outstanding$count==2L,cap$outstanding$completeness=="complete",
    cap$target$my_bid_id=="bid1",cap$target$my_bid_amount==5000000,
    is.na(cap$target$highest_bid),cap$target$bid_count==2L)
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
  det_seed_capacity(list(budget=50000000,configuration=list(maxPlayersInRoster=25)),det_empty_market())
  cap <- get_acquisition_capacity(login,champ,team)
  stopifnot(cap$status=="partial",is.na(cap$funds$spendable_budget))
  for(action in c("bid","clause")) {
    r<-evaluate_acquisition_preflight(cap,action,amount=1000000)
    stopifnot(!r$ok,r$reason=="unavailable")
  }
})

det_record("capacity_negative_withheld_partial", {
  det_seed_capacity(list(budget=50000000,withheld=-5000000,configuration=list(maxPlayersInRoster=25)),det_empty_market())
  cap<-get_acquisition_capacity(login,champ,team)
  stopifnot(cap$status=="partial",is.na(cap$funds$spendable_budget),!evaluate_acquisition_preflight(cap,"bid",1000000)$ok)
})

det_record("capacity_valid_zero_withheld_ok", {
  det_seed_capacity(list(budget=0,withheld=0,configuration=list(maxPlayersInRoster=25)),det_empty_market())
  cap<-get_acquisition_capacity(login,champ,team)
  stopifnot(cap$status=="ok",cap$funds$reported_budget==0,cap$funds$spendable_budget==0,
    !evaluate_acquisition_preflight(cap,"bid",1000000)$ok)
})

det_record("capacity_incoming_offers_are_not_outgoing_bids", {
  incoming<-data.frame(id=pid,bid_id="incoming",bid_price=9000000,bidder_team_id="rival")
  det_seed_capacity(list(budget=50000000,withheld=10000000,configuration=list(maxPlayersInRoster=25)),det_empty_market(),incoming)
  cap<-get_acquisition_capacity(login,champ,team,target_player_id=pid)
  stopifnot(cap$status=="ok",cap$outstanding$count==0,cap$outstanding$total_amount==0,
    cap$funds$spendable_budget==40000000,is.null(cap$target$my_bid_id),is.na(cap$target$my_bid_amount),
    is.na(cap$target$highest_bid),cap$target$bid_count==2)
})

det_record("capacity_own_market_bid_is_counted_once_and_sealed_amounts_remain_unknown", {
  market<-data.frame(id=c(pid,pid),bid_id="own",bid_price=5000000,numberOfBids=2)
  incoming<-data.frame(id=pid,bid_id="incoming",bid_price=9000000,bidder_team_id="rival")
  det_seed_capacity(list(budget=50000000,withheld=10000000,configuration=list(maxPlayersInRoster=25)),market,incoming)
  cap<-get_acquisition_capacity(login,champ,team,target_player_id=pid)
  stopifnot(cap$status=="ok",cap$outstanding$count==1,cap$outstanding$total_amount==5000000,
    cap$funds$spendable_budget==40000000,cap$target$my_bid_id=="own",cap$target$my_bid_amount==5000000,
    is.na(cap$target$highest_bid))
})

det_record("player_summary_cache_key_no_collision", {
  clear_api_cache()
  # my_bid_id/my_bid_price are viewer/team-specific: two different user/team
  # cache keys must NOT collide (no shared player_summary_<champ>_<pid> key).
  login_a <- c(token = "tokenA", userid = "userA", user_name = "A")
  login_b <- c(token = "tokenB", userid = "userB", user_name = "B")
  key_a <- api_cache_key(paste0("player_summary_", "userA", "_", champ, "_", team, "_", pid), context=resource_context(login_a,champ,team,pid))
  key_b <- api_cache_key(paste0("player_summary_", "userB", "_", champ, "_", team, "_", pid), context=resource_context(login_b,champ,team,pid))
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
  stopifnot(!is.null(res_a), identical(res_a$my_bid_id, "bidA"), res_a$my_bid_id == "bidA", res_a$my_bid_price == 1000000)
  stopifnot(!is.null(res_b), identical(res_b$my_bid_id, "bidB"), res_b$my_bid_id == "bidB", res_b$my_bid_price == 2000000)

  # A different team for the SAME user is also a distinct key.
  team2 <- "teamZ"
  key_c <- api_cache_key(paste0("player_summary_", "userA", "_", champ, "_", team2, "_", pid), context=resource_context(login_a,champ,team2,pid))
  stopifnot(key_a != key_c)
  api_cache_env[[key_c]] <- list(
    data = list(data = NULL, prices = list(),
                bids = list(list(id = "bidC", price = 3000000, userTeam = list("_id" = team2, name = "C"))),
                my_bid_id = "bidC", my_bid_price = 3000000),
    time = Sys.time())
  res_c <- get_player_summary(login_a, champ, team2, pid)
  stopifnot(!is.null(res_c), identical(res_c$my_bid_id, "bidC"), res_c$my_bid_id == "bidC", res_c$my_bid_price == 3000000)
  # userA/team still returns its own data (not overwritten by team2).
  res_a2 <- get_player_summary(login_a, champ, team, pid)
  stopifnot(res_a2$my_bid_id == "bidA", res_a2$my_bid_price == 1000000)
})

# =============================================================================
det_record("pending_clause_transfers_are_deduplicated_and_effective", {
  now<-as.POSIXct("2026-09-17 12:00:00",tz="UTC")
  roster<-data.frame(id=c("out","stay"))
  press<-data.frame(player_id=c("out","in","in"),buyer_team_id=c("rival",team,team),
    seller_team_id=c(team,"rival","rival"),created=c("2026-09-17 09:00:00","2026-09-17 08:00:00","2026-09-17 10:00:00"))
  summaries<-list(
    out=list(championship=list(owner=list(`_id`=team),clause=list(transferred=TRUE,transferDate="2026-09-18 12:00:00"))),
    `in`=list(championship=list(owner=list(`_id`="rival"),clause=list(transferred=TRUE,transferDate="2026-09-18 12:00:00"))))
  state<-reconcile_pending_clause_transfers(roster,press,summaries,team,now)
  stopifnot(state$status=="ok",identical(state$pending_outbound_ids,"out"),
    identical(state$pending_inbound_ids,"in"),state$pending_outbound_count==1L,state$pending_inbound_count==1L)
  effective<-nrow(roster)-state$pending_outbound_count+state$pending_inbound_count
  cap<-list(status="ok",
    roster=list(status="ok",count=2L,effective_count=effective,cap=effective,pending_outbound_count=1L,pending_inbound_count=1L),
    funds=list(spendable_budget=1e9),outstanding=list(count=0L))
  stopifnot(!evaluate_acquisition_preflight(cap,"clause")$ok,
    !evaluate_acquisition_preflight(cap,"bid")$ok,
    evaluate_acquisition_preflight(cap,"modify",existing_bid_amount=1)$ok)
})

# 4. Roster clause payload
# =============================================================================
cat("\n--- Roster clause payload ---\n")

futmondo_post <- det_original_post

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



















# ---- Action-aware resolver: market_bid resolves from market data ONLY ----
# An owned / non-listed player (present in all-players but absent from market
# data) must NOT resolve for a market_bid action; it resolves for view.







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

det_record("rivals_cash_uses_all_transfers_through_end_and_observed_initial_budget", {
  cash <- rivals_buying_power_values(pr, teams, metric="cash", start_date=start_dt,end_date=end_dt,initial_budget=100000000)
  stopifnot(cash$value[cash$team_id=="T1"]==95000000,cash$value[cash$team_id=="T2"]==87000000,
    all(cash$range_label=="all transfers through end date"))
  unknown <- rivals_buying_power_values(pr,teams,metric="cash",start_date=start_dt,end_date=end_dt,initial_budget=NA_real_)
  stopifnot(all(is.na(unknown$value)))
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
