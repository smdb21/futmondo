#!/usr/bin/env Rscript
# =============================================================================
# test_today_recommendation_policy.R
# Fully OFFLINE regression tests for the Today recommendation policy:
#   - owner classification (system / rival / unknown; immutable IDs only,
#     never team names; computer is system only when an explicit scalar
#     logical TRUE; current-team owner rows are unknown BEFORE the system
#     check)
#   - market owner-id resolution (unique name-to-ID match required;
#     ambiguous names fail closed to NA) + default/opt-in market candidate
#     filtering
#   - strict open release clause (finite positive price, explicitly FALSE
#     transferred, parseable clause_date <= now)
#   - strict rival clause candidate filtering (malformed input -> empty)
#   - feed generator: candidate-exclusive Buy/Clause sections, dual-route
#     dedupe (single clause recommendation), comparison metadata
#     max(market price, clause price) vs executed clause price, stable
#     action codes, null-arg legacy behavior
#   - action validation (market_bid / clause_buyout / view resolution,
#     stale/unknown input rejected)
#   - JS escaping (backslash + single quote)
#   - selected_player "clause_buyout" open_action routing (stubbed preflight,
#     no network; clause confirmation path, never the market bid modal)
#
# No login / network required. Run with:  Rscript test/test_today_recommendation_policy.R
# =============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(data.table)
  library(shiny)
  library(shinydashboardPlus)
  library(reactable)
  library(htmlwidgets)
})

# ---- Expectation helper (stopifnot with a string message would coerce the
# string to NA and always fail, so messages go through stop()) ----
pol_expect <- function(cond, msg) {
  if (!isTRUE(cond)) stop(msg)
}

# ---- Result accumulator ----
pol_results <- list()
pol_record <- function(label, expr) {
  res <- tryCatch(
    { expr; list(status = "pass", trace = character(0)) },
    error = function(e) list(status = "error", trace = conditionMessage(e))
  )
  pol_results[[label]] <<- res
  if (res$status == "pass") {
    cat(sprintf("  [PASS] %s\n", label))
  } else {
    cat(sprintf("  [FAIL] %s -- %s\n", label, res$trace))
  }
  invisible(res)
}

cat("\n======================================================================\n")
cat("  TODAY RECOMMENDATION POLICY TESTS (offline)\n")
cat("======================================================================\n")

# ---- Source the code under test (order mirrors global.R) ----
pol_record("source_intelligence_engine", { source("intelligence_engine.R") })
pol_record("source_futmondo_functions", { source("futmondo_functions.R") })
pol_record("source_today_module", { source("Modules/Today_Module.R") })
pol_record("source_selected_player_module", { source("Modules/Selected_Player_Module.R") })
pol_record("source_utils", { source("utils.R") })

all_sourced <- !any(sapply(pol_results, function(r) r$status == "error"))

if (!all_sourced) {
  cat("\n  FATAL: could not source code under test. Aborting.\n")
  quit(status = 1)
}

# Deterministic "now" for clause-date assertions (standard parseable format).
NOW <- as.POSIXct("2026-08-01 12:00:00", tz = "UTC")
PAST_DATE <- "2026-01-01T00:00:00Z"
FUTURE_DATE <- "2027-01-01T00:00:00Z"

# =============================================================================
# 1. Owner classification (immutable IDs only, never team names)
# =============================================================================
cat("\n--- Owner classification ---\n")

pol_record("owner_classify_system_strict_true", {
  # Explicit scalar logical TRUE -> system.
  stopifnot(today_classify_owner(
    data.frame(computer = TRUE, stringsAsFactors = FALSE), "me") == "system")
  # System wins over a rival owner id.
  stopifnot(today_classify_owner(
    data.frame(computer = TRUE, user_team_id = "r1", stringsAsFactors = FALSE), "me") == "system")
  # computer = TRUE + CURRENT-team owner id is "unknown": the current-team
  # owner check runs BEFORE the computer/system classification.
  stopifnot(today_classify_owner(
    data.frame(computer = TRUE, user_team_id = "me", stringsAsFactors = FALSE), "me") == "unknown")
})

pol_record("owner_classify_computer_not_strict_true", {
  # Numeric 1 (JSON boolean) is NOT system: unknown without an owner id,
  # rival only via an independently resolved immutable owner id.
  stopifnot(today_classify_owner(
    data.frame(computer = 1, stringsAsFactors = FALSE), "me") == "unknown")
  stopifnot(today_classify_owner(
    data.frame(computer = 1, user_team_id = "r1", stringsAsFactors = FALSE), "me") == "rival")
  # Character values (including "true") are NOT system.
  stopifnot(today_classify_owner(
    data.frame(computer = "true", stringsAsFactors = FALSE), "me") == "unknown")
  stopifnot(today_classify_owner(
    data.frame(computer = "true", user_team_id = "r1", stringsAsFactors = FALSE), "me") == "rival")
  # NA / FALSE are NOT system.
  stopifnot(today_classify_owner(
    data.frame(computer = NA, stringsAsFactors = FALSE), "me") == "unknown")
  stopifnot(today_classify_owner(
    data.frame(computer = FALSE, stringsAsFactors = FALSE), "me") == "unknown")
  # Malformed / multiple values are NOT system.
  stopifnot(today_classify_owner(
    data.frame(computer = I(list(c(TRUE, TRUE))), stringsAsFactors = FALSE), "me") == "unknown")
  stopifnot(today_classify_owner(
    data.frame(computer = I(list(1)), stringsAsFactors = FALSE), "me") == "unknown")
})

pol_record("owner_classify_rival_immutable_id", {
  # Resolved owner id != current team -> rival.
  stopifnot(today_classify_owner(
    data.frame(computer = FALSE, owner_team_id = "r1", stringsAsFactors = FALSE), "me") == "rival")
  # Fallback to user_team_id.
  stopifnot(today_classify_owner(
    data.frame(computer = FALSE, user_team_id = "r1", stringsAsFactors = FALSE), "me") == "rival")
  # computer NA (not strictly TRUE) falls through to the owner-id check.
  stopifnot(today_classify_owner(
    data.frame(computer = NA, user_team_id = "r1", stringsAsFactors = FALSE), "me") == "rival")
})

pol_record("owner_classify_unknown_cases", {
  # No owner id at all -> unknown.
  stopifnot(today_classify_owner(
    data.frame(computer = FALSE, stringsAsFactors = FALSE), "me") == "unknown")
  # Own-team row (id == current) -> unknown (not a buy candidate).
  stopifnot(today_classify_owner(
    data.frame(computer = FALSE, user_team_id = "me", stringsAsFactors = FALSE), "me") == "unknown")
  # NA / empty owner id -> unknown.
  stopifnot(today_classify_owner(
    data.frame(computer = FALSE, user_team_id = NA, stringsAsFactors = FALSE), "me") == "unknown")
  stopifnot(today_classify_owner(
    data.frame(computer = FALSE, user_team_id = "  ", stringsAsFactors = FALSE), "me") == "unknown")
  # Non-TRUE computer (FALSE / NA) with no owner id -> unknown.
  stopifnot(today_classify_owner(
    data.frame(computer = FALSE, stringsAsFactors = FALSE), "me") == "unknown")
  stopifnot(today_classify_owner(
    data.frame(computer = NA, stringsAsFactors = FALSE), "me") == "unknown")
  # NULL row -> unknown.
  stopifnot(today_classify_owner(NULL, "me") == "unknown")
})

pol_record("owner_classify_never_uses_team_names", {
  # A row carrying only a team NAME (no immutable id) must be "unknown",
  # never "rival" -- names are not evidence of ownership.
  stopifnot(today_classify_owner(
    data.frame(computer = FALSE, userTeam = "Rival FC", stringsAsFactors = FALSE), "me") == "unknown")
  stopifnot(today_classify_owner(
    data.frame(computer = NA, teamname = "Rival FC", user = "Rival FC",
               stringsAsFactors = FALSE), "me") == "unknown")
})

# =============================================================================
# 2. Market owner-id resolution + default/opt-in candidate filtering
# =============================================================================
cat("\n--- Market candidate filtering ---\n")

teams_df <- data.frame(
  teamid = c("r1", "r2"),
  teamname = c("Rival A", "Rival FC"),
  stringsAsFactors = FALSE
)

mkt_df <- data.frame(
  id = c("s1", "r1", "u1", "o1"),
  name = c("Sys One", "Rival One", "Ghost One", "Own One"),
  computer = c(TRUE, FALSE, FALSE, FALSE),
  userTeam = c(NA, "Rival FC", "Ghost", NA),
  user_team_id = c(NA, NA, NA, "me"),
  stringsAsFactors = FALSE
)

pol_record("resolve_market_owner_ids", {
  out <- today_resolve_market_owner_ids(mkt_df, teams_df)
  stopifnot("owner_team_id" %in% colnames(out))
  # Direct immutable id kept as-is.
  stopifnot(out$owner_team_id[4] == "me")
  # Name resolved to immutable id via the teams table.
  stopifnot(out$owner_team_id[2] == "r2")
  # Unresolvable name -> NA.
  stopifnot(is.na(out$owner_team_id[3]))
  # No teams table -> name-based rows stay NA.
  out2 <- today_resolve_market_owner_ids(mkt_df, NULL)
  stopifnot(is.na(out2$owner_team_id[2]))
  stopifnot(out2$owner_team_id[4] == "me")
})

pol_record("resolve_market_owner_ids_ambiguous_name_fails_closed", {
  # A displayed name alone is not ownership evidence, and a name mapping to
  # multiple DISTINCT team IDs is ambiguous: owner_team_id stays NA and the
  # row classifies unknown (excluded from candidates, even when opting in).
  teams_dup <- data.frame(
    teamid = c("r1", "r2", "r3"),
    teamname = c("Twin FC", "Twin FC", "Solo FC"),
    stringsAsFactors = FALSE
  )
  mkt_dup <- data.frame(
    id = c("a1", "a2", "a3"),
    name = c("A", "B", "C"),
    computer = c(FALSE, FALSE, FALSE),
    userTeam = c("Twin FC", "Twin FC", "Solo FC"),
    user_team_id = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )
  out <- today_resolve_market_owner_ids(mkt_dup, teams_dup)
  stopifnot(is.na(out$owner_team_id[1]), is.na(out$owner_team_id[2]))
  # A unique name-to-ID match still resolves.
  stopifnot(out$owner_team_id[3] == "r3")
  stopifnot(today_classify_owner(out[1, , drop = FALSE], "me") == "unknown")
  # Ambiguous rows are excluded from candidates in BOTH modes; the unique
  # rival row is kept only when opting in.
  stopifnot(nrow(today_filter_market_candidates(
    mkt_dup, "me", include_rival = FALSE, teams_df = teams_dup)) == 0)
  optin <- today_filter_market_candidates(mkt_dup, "me", include_rival = TRUE, teams_df = teams_dup)
  stopifnot(setequal(optin$id, "a3"))
  # Repeated rows with the SAME team id are NOT ambiguous.
  teams_same <- data.frame(
    teamid = c("r1", "r1"),
    teamname = c("Twin FC", "Twin FC"),
    stringsAsFactors = FALSE
  )
  out2 <- today_resolve_market_owner_ids(mkt_dup, teams_same)
  stopifnot(out2$owner_team_id[1] == "r1", out2$owner_team_id[2] == "r1")
})

pol_record("filter_market_candidates_default_system_only", {
  out <- today_filter_market_candidates(mkt_df, "me", include_rival = FALSE, teams_df = teams_df)
  stopifnot(nrow(out) == 1)
  stopifnot(out$id[1] == "s1")
  # Rival (r1) and unknown (u1) and own-team (o1) rows are hidden by default.
  stopifnot(!c("r1", "u1", "o1") %in% out$id)
})

pol_record("filter_market_candidates_opt_in_includes_rival", {
  out <- today_filter_market_candidates(mkt_df, "me", include_rival = TRUE, teams_df = teams_df)
  stopifnot(setequal(out$id, c("s1", "r1")))
  # Unknown-owner rows are ALWAYS excluded, even when opting in.
  stopifnot(!"u1" %in% out$id)
  # Own-team rows are never candidates.
  stopifnot(!"o1" %in% out$id)
})

pol_record("filter_market_candidates_current_team_owner_never_candidate", {
  # computer = TRUE + current-team owner id: the row classifies "unknown"
  # (owner check runs BEFORE the system check) and can never enter either the
  # default or the opt-in acquisition candidates.
  mkt_own <- data.frame(
    id = c("sys1", "own_sys"),
    name = c("Sys One", "Own Sys"),
    computer = c(TRUE, TRUE),
    user_team_id = c(NA, "me"),
    stringsAsFactors = FALSE
  )
  stopifnot(today_classify_owner(mkt_own[2, , drop = FALSE], "me") == "unknown")
  out_default <- today_filter_market_candidates(mkt_own, "me", include_rival = FALSE)
  stopifnot(nrow(out_default) == 1, out_default$id[1] == "sys1")
  out_optin <- today_filter_market_candidates(mkt_own, "me", include_rival = TRUE)
  stopifnot(nrow(out_optin) == 1, out_optin$id[1] == "sys1")
})

pol_record("filter_market_candidates_malformed_empty", {
  stopifnot(nrow(today_filter_market_candidates(NULL, "me")) == 0)
  stopifnot(nrow(today_filter_market_candidates(data.frame(), "me")) == 0)
  stopifnot(nrow(today_filter_market_candidates(list(), "me")) == 0)
})

# =============================================================================
# 3. Strict open release clause
# =============================================================================
cat("\n--- Strict open clause ---\n")

clause_row <- function(price = 8e6, transferred = FALSE, date = PAST_DATE,
                       drop_transferred = FALSE, drop_date = FALSE) {
  d <- data.frame(
    id = "c1", name = "C",
    clause_price = price,
    clause_transferred = transferred,
    clause_date = date,
    stringsAsFactors = FALSE
  )
  if (drop_transferred) d$clause_transferred <- NULL
  if (drop_date) d$clause_date <- NULL
  d
}

pol_record("clause_open_valid", {
  stopifnot(isTRUE(today_is_clause_open(clause_row(), now = NOW)))
  # Boundary: clause_date exactly == now is open (<= now).
  stopifnot(isTRUE(today_is_clause_open(
    clause_row(date = "2026-08-01T12:00:00Z"), now = NOW)))
})

pol_record("clause_open_price_rules", {
  stopifnot(isFALSE(today_is_clause_open(clause_row(price = 0), now = NOW)))
  stopifnot(isFALSE(today_is_clause_open(clause_row(price = -5), now = NOW)))
  stopifnot(isFALSE(today_is_clause_open(clause_row(price = NA_real_), now = NOW)))
  stopifnot(isFALSE(today_is_clause_open(clause_row(price = Inf), now = NOW)))
})

pol_record("clause_open_transferred_must_be_explicit_false", {
  stopifnot(isFALSE(today_is_clause_open(clause_row(transferred = TRUE), now = NOW)))
  stopifnot(isFALSE(today_is_clause_open(clause_row(transferred = NA), now = NOW)))
  stopifnot(isFALSE(today_is_clause_open(clause_row(drop_transferred = TRUE), now = NOW)))
})

pol_record("clause_open_date_rules", {
  stopifnot(isFALSE(today_is_clause_open(clause_row(date = FUTURE_DATE), now = NOW)))
  stopifnot(isFALSE(today_is_clause_open(clause_row(date = "not-a-date"), now = NOW)))
  stopifnot(isFALSE(today_is_clause_open(clause_row(date = ""), now = NOW)))
  stopifnot(isFALSE(today_is_clause_open(clause_row(drop_date = TRUE), now = NOW)))
})

# =============================================================================
# 4. Strict rival clause candidate filtering
# =============================================================================
cat("\n--- Clause candidate filtering ---\n")

rival_roster_df <- data.frame(
  id = c("c1", "c2", "c3", "c4", "c5"),
  name = c("Open", "Locked", "Mine", "NoClause", "NoOwner"),
  user_team_id = c("rival1", "rival1", "me", "rival2", NA),
  clause_price = c(8e6, 8e6, 8e6, 0, 8e6),
  clause_transferred = c(FALSE, FALSE, FALSE, FALSE, FALSE),
  clause_date = c(PAST_DATE, FUTURE_DATE, PAST_DATE, PAST_DATE, PAST_DATE),
  stringsAsFactors = FALSE
)

pol_record("clause_candidates_keep_only_rival_open", {
  out <- today_filter_clause_candidates(rival_roster_df, "me", now = NOW)
  stopifnot(nrow(out) == 1)
  stopifnot(out$id[1] == "c1")
  # Locked (future date), own-team, no-clause, and no-owner rows are excluded.
  stopifnot(!c("c2", "c3", "c4", "c5") %in% out$id)
})

pol_record("clause_candidates_malformed_empty", {
  stopifnot(nrow(today_filter_clause_candidates(NULL, "me")) == 0)
  stopifnot(nrow(today_filter_clause_candidates(data.frame(), "me")) == 0)
  stopifnot(nrow(today_filter_clause_candidates(
    data.frame(name = "x", clause_price = 1, stringsAsFactors = FALSE), "me")) == 0)  # no id column
})

# =============================================================================
# 5. Feed generator: candidate-exclusive Buy/Clause, dual-route dedupe,
#    comparison metadata vs executed price, action codes, legacy behavior
# =============================================================================
cat("\n--- Feed generator policy ---\n")

players_df <- data.frame(
  id = c("own1", "own2", "own3"),
  name = c("Own One", "Own Two", "Own Three"),
  value = c(5000000, 4000000, 12000000),
  user_team_id = c("me", "me", "me"),
  clause_price = c(NA, NA, 7000000),
  fis_score = c(90, 30, 85),
  fis_tier = c("Strong Buy", "Sell", "Strong Buy"),
  fis_summary = c("o1", "o2", "o3"),
  stringsAsFactors = FALSE
)

mkt_cand <- data.frame(
  id = c("sys1", "sys2", "dual1"),
  name = c("Sys One", "Sys Two", "Dual One"),
  value = c(10000000, 9000000, 10000000),
  price = c(10000000, 9000000, 10000000),
  fis_score = c(90, 85, 88),
  fis_tier = c("Strong Buy", "Buy", "Strong Buy"),
  fis_summary = c("s1", "s2", "s3"),
  stringsAsFactors = FALSE
)

clause_cand <- data.frame(
  id = c("dual1", "rcl1"),
  name = c("Dual One", "Rival Clause"),
  value = c(10000000, 12000000),
  clause_price = c(8000000, 8000000),
  clause_transferred = c(FALSE, FALSE),
  clause_date = c(PAST_DATE, PAST_DATE),
  user_team_id = c("rival2", "rival1"),
  fis_score = c(88, 87),
  fis_tier = c("Strong Buy", "Buy"),
  fis_summary = c("d1", "c1"),
  stringsAsFactors = FALSE
)

pol_record("feed_buy_exclusive_from_market_candidates", {
  feed <- generate_command_center_feed(
    login = NULL, championship_id = "champ", user_team_id = "me",
    user_teams_df = teams_df, players_df = players_df,
    market_candidates = mkt_cand, clause_candidates = clause_cand
  )
  buys <- feed[feed$type == "Buy", ]
  # Buy recs come EXCLUSIVELY from the supplied market candidates:
  # sys1 + sys2; the players_df Strong Buy player (own1) must NOT appear.
  stopifnot(setequal(buys$player_id, c("sys1", "sys2")))
  stopifnot(!"own1" %in% buys$player_id)
  stopifnot(all(buys$action_code == "market_bid"))
  stopifnot(all(buys$action_label == "Place Bid"))
})

pol_record("feed_clause_exclusive_from_clause_candidates", {
  feed <- generate_command_center_feed(
    login = NULL, championship_id = "champ", user_team_id = "me",
    user_teams_df = teams_df, players_df = players_df,
    market_candidates = mkt_cand, clause_candidates = clause_cand
  )
  clauses <- feed[feed$type == "Clause", ]
  # Clause recs come EXCLUSIVELY from the supplied clause candidates:
  # dual1 + rcl1; the players_df clause player (own3) must NOT appear.
  stopifnot(setequal(clauses$player_id, c("dual1", "rcl1")))
  stopifnot(!"own3" %in% clauses$player_id)
  stopifnot(all(clauses$action_code == "clause_buyout"))
  stopifnot(all(clauses$action_label == "Exercise Clause"))
})

pol_record("feed_dual_route_single_clause_recommendation", {
  feed <- generate_command_center_feed(
    login = NULL, championship_id = "champ", user_team_id = "me",
    user_teams_df = teams_df, players_df = players_df,
    market_candidates = mkt_cand, clause_candidates = clause_cand
  )
  # dual1 is in BOTH candidate sets: exactly ONE recommendation, and it is
  # the clause one (no separate Buy card).
  dual_rows <- feed[feed$player_id == "dual1", ]
  stopifnot(nrow(dual_rows) == 1)
  stopifnot(dual_rows$type[1] == "Clause")
  stopifnot(dual_rows$action_code[1] == "clause_buyout")
  stopifnot(!"dual1" %in% feed$player_id[feed$type == "Buy"])
})

pol_record("feed_dual_route_comparison_max_metadata", {
  feed <- generate_command_center_feed(
    login = NULL, championship_id = "champ", user_team_id = "me",
    user_teams_df = teams_df, players_df = players_df,
    market_candidates = mkt_cand, clause_candidates = clause_cand
  )
  dual_desc <- feed$description[feed$player_id == "dual1"]
  # max(market price 10M, clause price 8M) = 10M appears as comparison metadata.
  stopifnot(grepl("comparison max: 10000000", dual_desc, fixed = TRUE))
  # The clause price is stated as the executed price.
  stopifnot(grepl("Buyout clause at 8000000", dual_desc, fixed = TRUE))
  stopifnot(grepl("Executing clause price only", dual_desc, fixed = TRUE))
})

pol_record("feed_dual_route_executes_clause_price_not_max", {
  # Price semantics: the clause_buyout action resolves to the CLAUSE candidate
  # row, whose clause_price (8M) -- not the comparison max (10M) -- is the
  # executed price.
  row <- today_resolve_player_for_action("dual1", "clause_buyout", mkt_cand, players_df,
                                          clause_df = clause_cand)
  stopifnot(is.data.frame(row), nrow(row) == 1)
  stopifnot(suppressWarnings(as.numeric(row$clause_price)) == 8000000)
  # The market price is NOT the clause price.
  mkt_row <- today_resolve_player("dual1", mkt_cand)
  stopifnot(suppressWarnings(as.numeric(mkt_row$price)) == 10000000)
})

pol_record("feed_dual_route_max_when_clause_higher", {
  # When the clause price exceeds the market price, the comparison max is the
  # clause price itself (max of both).
  mkt2 <- data.frame(
    id = "dual2", name = "Dual Two", value = 5000000, price = 5000000,
    fis_score = 86, fis_tier = "Buy", fis_summary = "s",
    stringsAsFactors = FALSE
  )
  cl2 <- data.frame(
    id = "dual2", name = "Dual Two", value = 5000000, clause_price = 8000000,
    clause_transferred = FALSE, clause_date = PAST_DATE, user_team_id = "rival1",
    fis_score = 86, fis_tier = "Buy", fis_summary = "c",
    stringsAsFactors = FALSE
  )
  feed <- generate_command_center_feed(
    login = NULL, championship_id = "champ", user_team_id = "me",
    user_teams_df = teams_df, players_df = players_df,
    market_candidates = mkt2, clause_candidates = cl2
  )
  stopifnot(nrow(feed[feed$player_id == "dual2", ]) == 1)
  stopifnot(feed$type[feed$player_id == "dual2"] == "Clause")
  stopifnot(grepl("comparison max: 8000000",
                  feed$description[feed$player_id == "dual2"], fixed = TRUE))
})

pol_record("feed_action_codes_for_all_types", {
  feed <- generate_command_center_feed(
    login = NULL, championship_id = "champ", user_team_id = "me",
    user_teams_df = teams_df, players_df = players_df,
    market_candidates = mkt_cand, clause_candidates = clause_cand
  )
  stopifnot("action_code" %in% colnames(feed))
  stopifnot(all(feed$action_code %in% c("market_bid", "clause_buyout", "view")))
  # Sell recs (own2) carry the "view" code.
  sells <- feed[feed$type == "Sell", ]
  stopifnot(nrow(sells) >= 1)
  stopifnot(all(sells$action_code == "view"))
})

pol_record("feed_null_args_preserve_legacy_behavior", {
  feed <- generate_command_center_feed(
    login = NULL, championship_id = "champ", user_team_id = "me",
    user_teams_df = teams_df, players_df = players_df
  )
  buys <- feed[feed$type == "Buy", ]
  clauses <- feed[feed$type == "Clause", ]
  # Legacy: Buy from players_df Strong Buy/Buy tier (own1 + own3),
  # Clause from players_df clause players (own3).
  stopifnot(setequal(buys$player_id, c("own1", "own3")))
  stopifnot(setequal(clauses$player_id, "own3"))
  stopifnot(all(buys$action_code == "market_bid"))
  stopifnot(all(clauses$action_code == "clause_buyout"))
})

pol_record("feed_empty_candidates_supplied_yields_no_buy_clause", {
  # 0-row candidate data frames ARE supplied (Today always passes them): the
  # legacy players_df Buy/Clause behavior must NOT be used.
  feed <- generate_command_center_feed(
    login = NULL, championship_id = "champ", user_team_id = "me",
    user_teams_df = teams_df, players_df = players_df,
    market_candidates = data.frame(), clause_candidates = data.frame()
  )
  stopifnot(nrow(feed[feed$type == "Buy", ]) == 0)
  stopifnot(nrow(feed[feed$type == "Clause", ]) == 0)
})

# =============================================================================
# 6. Action validation (market_bid / clause_buyout / view)
# =============================================================================
cat("\n--- Action validation ---\n")

all_df <- data.table::rbindlist(list(mkt_cand, clause_cand, players_df), fill = TRUE) %>% as.data.frame()

pol_record("resolve_market_bid_from_market_candidates_only", {
  # A market candidate resolves.
  r1 <- today_resolve_player_for_action("sys1", "market_bid", mkt_cand, all_df)
  stopifnot(is.data.frame(r1), nrow(r1) == 1, as.character(r1$id) == "sys1")
  # A clause-only player (absent from market candidates) must NOT resolve.
  stopifnot(is.null(today_resolve_player_for_action("rcl1", "market_bid", mkt_cand, all_df)))
  # Stale / unknown ids are rejected.
  stopifnot(is.null(today_resolve_player_for_action("gone", "market_bid", mkt_cand, all_df)))
  # Empty market candidates fail closed even if the id exists elsewhere.
  stopifnot(is.null(today_resolve_player_for_action("sys1", "market_bid", data.frame(), all_df)))
})

pol_record("resolve_clause_buyout_from_clause_candidates_only", {
  # An open clause candidate resolves.
  r1 <- today_resolve_player_for_action("rcl1", "clause_buyout", mkt_cand, all_df,
                                        clause_df = clause_cand)
  stopifnot(is.data.frame(r1), nrow(r1) == 1, as.character(r1$id) == "rcl1")
  # A market-only player (absent from clause candidates) must NOT resolve.
  stopifnot(is.null(today_resolve_player_for_action("sys1", "clause_buyout", mkt_cand, all_df,
                                                    clause_df = clause_cand)))
  # Stale / unknown ids are rejected.
  stopifnot(is.null(today_resolve_player_for_action("gone", "clause_buyout", mkt_cand, all_df,
                                                    clause_df = clause_cand)))
  # NULL clause candidates fail closed.
  stopifnot(is.null(today_resolve_player_for_action("rcl1", "clause_buyout", mkt_cand, all_df)))
  # Raw label "Exercise Clause" maps to the clause_buyout route.
  r2 <- today_resolve_player_for_action("rcl1", "Exercise Clause", mkt_cand, all_df,
                                        clause_df = clause_cand)
  stopifnot(is.data.frame(r2), as.character(r2$id) == "rcl1")
})

pol_record("resolve_view_from_all_players", {
  r1 <- today_resolve_player_for_action("own1", "view", mkt_cand, all_df)
  stopifnot(is.data.frame(r1), as.character(r1$id) == "own1")
  stopifnot(is.null(today_resolve_player_for_action("nope", "view", mkt_cand, all_df)))
})

pol_record("normalize_action_clause_and_market_codes", {
  stopifnot(today_normalize_action("Exercise Clause") == "clause_buyout")
  stopifnot(today_normalize_action("exercise clause") == "clause_buyout")
  stopifnot(today_normalize_action("clause_buyout") == "clause_buyout")
  stopifnot(today_normalize_action("Place Bid") == "market_bid")
  stopifnot(today_normalize_action("market_bid") == "market_bid")
  stopifnot(today_normalize_action("view") == "view")
  stopifnot(today_normalize_action("List on Market") == "view")
  stopifnot(today_normalize_action(NULL) == "view")
})

# =============================================================================
# 7. JS escaping (backslash + single quote)
# =============================================================================
cat("\n--- JS escaping ---\n")

pol_record("rec_action_onclick_js_escapes_backslash_and_quote", {
  ns <- NS("today_test")
  # Backslash only.
  oc1 <- today_rec_action_onclick_js(ns, "a\\b", "Buy Now")
  stopifnot(grepl("player_id: 'a\\\\b'", oc1, fixed = TRUE))
  # Single quote only (regression: existing behavior preserved).
  oc2 <- today_rec_action_onclick_js(ns, "o'brien", "Buy 'X'")
  stopifnot(grepl("player_id: 'o\\'brien'", oc2, fixed = TRUE))
  stopifnot(grepl("action: 'Buy \\'X\\''", oc2, fixed = TRUE))
  # Backslash + quote combined: \ -> \\ first, then ' -> \'.
  oc3 <- today_rec_action_onclick_js(ns, "x\\y'z", "a\\b'c")
  stopifnot(grepl("player_id: 'x\\\\y\\'z'", oc3, fixed = TRUE))
  stopifnot(grepl("action: 'a\\\\b\\'c'", oc3, fixed = TRUE))
})

# =============================================================================
# 8. selected_player "clause_buyout" open_action routing (offline, stubbed
#    preflight; no network write)
# =============================================================================
cat("\n--- selected_player clause_buyout routing ---\n")

# Stub every network-touching function the module may call at runtime so the
# test is fully OFFLINE (main observer, trend plot, smart-bid widget).
get_player_summary <- function(login, championship_id, user_team_id, player_id) NULL
get_player_historical_data <- function(player_id, championship_id) NULL
get_finished_rounds <- function(login, championship_id) data.frame(
  round_id = character(0), round_number = numeric(0),
  begin_process = character(0), is_finished = logical(0), stringsAsFactors = FALSE
)
get_championship_pressroom <- function(login, championship_id) data.frame()
get_acquisition_capacity <- function(login, championship_id, user_team_id, target_player_id = NULL) {
  list(
    status = "ok",
    roster = list(count = 20L, cap = 25L, remaining_slots = 5L),
    outstanding = list(offers = 0L, count = 0L, total_amount = 0, completeness = "ok"),
    funds = list(reported_budget = 10000000, withheld = 0, spendable_budget = 10000000),
    target = list(my_bid_id = NULL, my_bid_amount = NA_real_,
                  highest_bid = NA_real_, bid_count = NA_integer_),
    diagnostics = character(0)
  )
}

pol_record("clause_buyout_opens_clause_modal_not_market_modal", {
  stub_env <- new.env()
  stub_env$count <- 0
  stub_env$capacity <- NULL
  stub_capacity <- function(login, championship_id, user_team_id, target_player_id) {
    stub_env$count <- stub_env$count + 1
    stub_env$capacity
  }

  # Open-clause rival player row (strict open: positive price, explicitly
  # FALSE transferred, past clause_date).
  sp_open <- data.frame(
    id = "cl1", name = "Clause Guy", role = "FW",
    value = 10000000,
    clause_price = 8000000,
    clause_transferred = FALSE,
    clause_date = PAST_DATE,
    user_team_id = "rival1",
    effective_market_price = 10000000,
    stringsAsFactors = FALSE
  )
  # Locked-clause row (future clause_date) -> recheck must block the modal.
  sp_locked <- data.frame(
    id = "cl2", name = "Locked Guy", role = "FW",
    value = 10000000,
    clause_price = 8000000,
    clause_transferred = FALSE,
    clause_date = FUTURE_DATE,
    user_team_id = "rival1",
    effective_market_price = 10000000,
    stringsAsFactors = FALSE
  )
  # Plain market row (no clause) for the market_bid regression check.
  sp_market <- data.frame(
    id = "mk1", name = "Market Guy", role = "FW",
    value = 500000,
    effective_market_price = 500000,
    stringsAsFactors = FALSE
  )

  selected_player_RV <- reactiveVal(sp_open)
  login_token_RV <- reactiveVal(list(token = "t", userid = "u"))
  championship_id_RV <- reactiveVal("champ")
  user_team_id_RV <- reactiveVal("team")
  open_action_RV <- reactiveVal(NULL)

  shiny::testServer(
    selected_player_Server,
    {
      session$flushReact()

      # ---- (a) clause_buyout + open clause + preflight ok -> clause modal ----
      stub_env$capacity <- list(
        status = "ok",
        roster = list(count = 20, cap = 25),
        outstanding = list(count = 0),
        funds = list(spendable_budget = 10000000)
      )
      open_action_RV("clause_buyout")
      session$flushReact()

      pol_expect(stub_env$count >= 1, "clause helper did not attempt preflight")
      pol_expect(isTRUE(clause_modal_opened_RV()), "clause confirmation modal did not open")
      pol_expect(isFALSE(offer_modal_opened_RV()), "market bid modal must never open for clause_buyout")
      cat("    PASS: clause_buyout + open clause -> clause confirmation modal (not market modal)\n")

      # ---- (b) clause_buyout + LOCKED clause -> recheck blocks the modal ----
      selected_player_RV(sp_locked)
      session$flushReact()
      open_action_RV(NULL)
      session$flushReact()
      open_action_RV("clause_buyout")
      session$flushReact()

      pol_expect(isFALSE(clause_modal_opened_RV()), "locked clause must not open the buyout modal")
      pol_expect(isFALSE(offer_modal_opened_RV()), "locked clause must not open the market modal")
      cat("    PASS: clause_buyout + locked clause -> open-state recheck blocks the modal\n")

      # ---- (c) clause_buyout + open clause + preflight FAIL -> no modal ----
      selected_player_RV(sp_open)
      session$flushReact()
      stub_env$capacity <- list(
        status = "ok",
        roster = list(count = 25, cap = 25),  # roster full
        outstanding = list(count = 0),
        funds = list(spendable_budget = 10000000)
      )
      open_action_RV(NULL)
      session$flushReact()
      open_action_RV("clause_buyout")
      session$flushReact()

      pol_expect(isFALSE(clause_modal_opened_RV()), "preflight failure must block the clause modal")
      cat("    PASS: clause_buyout + preflight fail -> no clause modal\n")

      # ---- (d) regression: market_bid still opens the market-offer modal ----
      selected_player_RV(sp_market)
      session$flushReact()
      stub_env$capacity <- list(
        status = "ok",
        roster = list(count = 20, cap = 25),
        outstanding = list(count = 0),
        funds = list(spendable_budget = 10000000)
      )
      open_action_RV(NULL)
      session$flushReact()
      open_action_RV("market_bid")
      session$flushReact()

      pol_expect(isTRUE(offer_modal_opened_RV()), "market_bid must still open the market-offer modal")
      pol_expect(isFALSE(clause_modal_opened_RV()), "market_bid must not open the clause modal")
      cat("    PASS: market_bid regression -> market-offer modal opens (clause modal untouched)\n")

      # ---- (e) unknown action codes are ignored ----
      # Reset the modal flags (they retain the state from step d) so we can
      # assert that an unknown action code opens NO modal.
      offer_modal_opened_RV(FALSE)
      clause_modal_opened_RV(FALSE)
      open_action_RV(NULL)
      session$flushReact()
      open_action_RV("view")
      session$flushReact()
      pol_expect(isFALSE(clause_modal_opened_RV()), "unknown action code must not open the clause modal")
      pol_expect(isFALSE(offer_modal_opened_RV()), "unknown action code must not open the market modal")
      cat("    PASS: unknown action code ignored (no modal)\n")
    },
    args = list(
      id = "sp_policy_test",
      selected_player = selected_player_RV,
      login_token = login_token_RV,
      championship_id = championship_id_RV,
      user_team_id = user_team_id_RV,
      open_action = open_action_RV,
      capacity_fetcher = stub_capacity
    )
  )
})

# =============================================================================
# FINAL REPORT
# =============================================================================
cat("\n======================================================================\n")
cat("  FINAL REPORT\n")
cat("======================================================================\n")

n_pass <- sum(sapply(pol_results, function(r) r$status == "pass"))
n_fail <- sum(sapply(pol_results, function(r) r$status == "error"))
cat(sprintf("  %d passed, %d failed\n", n_pass, n_fail))

if (n_fail > 0) {
  cat("\n  FAILURES:\n")
  for (lbl in names(pol_results)) {
    if (pol_results[[lbl]]$status == "error") {
      cat(sprintf("    - %s: %s\n", lbl, pol_results[[lbl]]$trace))
    }
  }
  quit(status = 1)
} else {
  cat("\n  RESULT: ALL TODAY RECOMMENDATION POLICY TESTS PASSED\n")
  cat("======================================================================\n")
  quit(status = 0)
}
