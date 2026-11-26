#!/usr/bin/env Rscript
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source("global.R"))

fm_worker_configured <- function() TRUE
auth <- c(userid = "award-display-fixture", token = "synthetic")
login <- shiny::reactiveVal(NULL)
league <- shiny::reactiveVal("fixture-league")
ctx <- resource_context(auth, "fixture-league", "fixture-team", season = "fixture-season",
  scoring_version = "fixture-scoring", competition_id = "fixture-competition")
now <- Sys.time()
stamp <- now - 1800
stale_context <- function(scope) list(status = "stale", freshness = "stale", data = scope)
resolved <- shiny::reactiveVal(stale_context(ctx))
account <- fm_worker_account_state(ctx$account_id)
account$bootstrap_complete <- TRUE
fm_worker_set_policy(account, list(enabled = TRUE, auth_status = "active", generation = 1L))

args <- list(championship_id = ctx$championship_id)
descriptor <- fm_worker_descriptor("get_mvp_saved_catalog", args, ctx)
players <- lapply(seq_len(11), function(i) list(id = sprintf("p%02d", i),
  name = sprintf("Player %02d", i), points = i - 2L, role = "defensa"))
selection <- normalize_official_dreamteam(list(players = players, mvp = "p01"), ctx$championship_id,
  list(round_id = "r1", round_number = 1L, is_finished = TRUE))$rows
selection$checked_at <- rival_finance_stamp(stamp)
selection$persistence <- "Saved"
selection$season <- ctx$season
selection$scoring_version <- ctx$scoring_version
catalog <- list(rounds = data.frame(round_id = "r1", round_number = 1L, is_finished = TRUE,
    status = "done"), complete = TRUE, identity_complete = TRUE, reason = "", user_team_id = ctx$user_team_id)
saved <- list(saved = selection, catalog = catalog, season = ctx$season, expected_scoring = ctx$scoring_version,
  saved_complete = TRUE, context_unknown = FALSE, settlement_rechecks = "r1")

# Supply the persisted wire format at the database boundary. The application
# still registers its interests, polls, validates, decompresses and renders.
snapshot <- function(d, value, version, observed_at) {
  wire <- charToRaw(fm_worker_json(fm_worker_encode(value)))
  compressed <- history_baseline_compress(wire)
  manifest <- list(account_id = d$context$account_id, resource_key = d$resource_key,
    context = d$context, handler = d$handler, version = version, json_bytes = length(wire), parts = 1L,
    sha256 = paste0(openssl::sha256(compressed)), observed_at = rival_finance_stamp(observed_at),
    checked_at = rival_finance_stamp(observed_at), valid_until = rival_finance_stamp(observed_at + 60),
    status = "ok", completeness = "complete")
  list(key = d$resource_key, manifest = manifest, chunks = list(jsonlite::base64_enc(compressed)))
}
stored <- snapshot(descriptor, saved, "1", stamp)
served <- character()
requested <- character()
reads <- character()
writes <- 0L
original_fetch <- async_adapter_fetch
async_adapter_fetch <- function(fn, ...) {
  reads <<- c(reads, fn)
  stopifnot(fn %in% c("get_mvp_saved_catalog", "get_championship_players"))
  original_fetch(fn, ...)
}
persist_official_award_snapshot <- function(...) {
  writes <<- writes + 1L
  stop("The display cannot write an award selection")
}
catalog_reads <- function() sum(reads == "get_mvp_saved_catalog")
fm_worker_rpc <- function(name, payload, owner = "") {
  stopifnot(identical(name, "read"))
  results <- list()
  for (d in payload$p_descriptors) {
    if (identical(d$handler, "get_mvp_saved_catalog")) {
      requested <<- c(requested, d$resource_key)
      item <- if (identical(d$resource_key, stored$key)) stored
    } else {
      stopifnot(identical(d$handler, "get_championship_players"))
      item <- snapshot(d, data.frame(id = character(), team = character()), "players", stamp)
    }
    if (!is.null(item) && !identical(payload$p_versions[[d$resource_key]], item$manifest$version)) {
      results[[length(results) + 1L]] <- item
      if (identical(d$handler, "get_mvp_saved_catalog")) served <<- c(served, item$manifest$version)
    }
  }
  promises::promise_resolve(list(account = account$account, jobs = list(), resources = results))
}
settle <- function(session) {
  for (i in seq_len(12)) {
    session$flushReact()
    later::run_now(.002)
  }
  session$flushReact()
}

shiny::testServer(round_mvps_Server, args = list(is_module_active = function() TRUE,
    login_token = login, championship_id = league), {
  root <- session$rootScope()
  root$userData$league_context_enabled <- TRUE
  root$userData$league_context_source <- resolved
  settle(session)
  stopifnot(!length(requested))
  login(auth)
  settle(session)
  html <- output$mvp_cards$html
  stopifnot(length(html) == 1L, !grepl("Saved coverage could not be fully read", html, fixed = TRUE),
    length(xml2::xml_find_all(xml2::read_html(html), "//table/tbody/tr")) == 11L,
    grepl("Player 01", html, fixed = TRUE), grepl(">-1<", html, fixed = TRUE),
    identical(served, "1"), all(requested == descriptor$resource_key),
    all(mvp_snapshot()$rows$checked_at == rival_finance_stamp(stamp)))
  initial_reads <- catalog_reads()
  stopifnot(initial_reads == 1L)
  cat("PASS signed-out to signed-in loads all 11 saved players using the exact expired league context\n")

  updated <- saved
  updated$saved$player_name[1] <- "Corrected winner"
  stored <<- snapshot(descriptor, updated, "2", now)
  fm_worker_poll(ctx$account_id, session$token, TRUE)
  settle(session)
  html <- output$mvp_cards$html
  stopifnot(grepl("Corrected winner", html, fixed = TRUE), !grepl("Player 01", html, fixed = TRUE),
    identical(served, c("1", "2")), catalog_reads() == initial_reads + 1L)
  before <- mvp_snapshot()
  fm_worker_poll(ctx$account_id, session$token, TRUE)
  settle(session)
  stopifnot(identical(before, mvp_snapshot()), identical(served, c("1", "2")), catalog_reads() == initial_reads + 1L)
  cat(sprintf("PASS award adapter reads: %d on cold load, one on replacement, zero on unchanged poll\n", initial_reads))

  partial <- updated
  partial$saved_complete <- FALSE
  stored <<- snapshot(descriptor, partial, "3", now)
  fm_worker_poll(ctx$account_id, session$token, TRUE)
  settle(session)
  html <- output$mvp_cards$html
  stopifnot(identical(mvp_snapshot()$status, "partial"),
    grepl("Saved coverage could not be fully read", html, fixed = TRUE),
    length(xml2::xml_find_all(xml2::read_html(html), "//table/tbody/tr")) == 11L,
    grepl("Corrected winner", html, fixed = TRUE), catalog_reads() == initial_reads + 2L,
    !any(reads == "get_official_dreamteam_result"), writes == 0L)
  cat("PASS an incomplete saved read retains the known XI and reports it; settlement flags cause zero GUI writes or acquisitions\n")

  league("second-league")
  foreign <- ctx
  foreign$account_id <- "another-account"
  foreign$championship_id <- league()
  resolved(stale_context(foreign))
  settle(session)
  stopifnot(NROW(mvp_snapshot()$rows) == 0L, !grepl("Corrected winner", output$mvp_cards$html %||% "", fixed = TRUE))
  unresolved <- foreign
  unresolved$account_id <- ctx$account_id
  unresolved$season <- "unknown"
  resolved(stale_context(unresolved))
  settle(session)
  stopifnot(NROW(mvp_snapshot()$rows) == 0L, !grepl("Corrected winner", output$mvp_cards$html %||% "", fixed = TRUE),
    all(requested == descriptor$resource_key))
  cat("PASS foreign and unresolved identities cannot read or display awards from another league\n")

  resolved(NULL)
  remember_league_context(c(as.list(ctx[c("account_id", "championship_id", "competition_id", "season", "scoring_version")]),
    list(observed_at = rival_finance_stamp(stamp), expires_at = rival_finance_stamp(stamp + 300),
      complete = TRUE, payload = list())))
  stopifnot(is.null(league_context_metadata(ctx$championship_id, ctx$account_id)))
  league(ctx$championship_id)
  settle(session)
  stopifnot(NROW(mvp_snapshot()$rows) == 11L, grepl("Corrected winner", output$mvp_cards$html, fixed = TRUE),
    identical(served, c("1", "2", "3")), writes == 0L)
  cat("PASS expired verified membership restores the same saved XI without a resolved-context response or another database transfer\n")

  scope <- ctx
  scope$scoring_version <- "unknown"
  exact <- fm_worker_descriptor("get_mvp_saved_catalog", args, scope)
  clear_league_contexts(ctx$account_id)
  resolved(stale_context(scope))
  settle(session)
  stopifnot(exact$resource_key %in% requested, NROW(mvp_snapshot()$rows) == 0L,
    !is.null(fm_worker_get(ctx$account_id, descriptor$resource_key)))
  exact_saved <- saved
  exact_saved$expected_scoring <- exact_saved$saved$scoring_version <- "unknown"
  exact_saved$saved$player_name[1] <- "Archived winner"
  stored <<- snapshot(exact, exact_saved, "unknown-scoring", stamp)
  fm_worker_poll(ctx$account_id, session$token, TRUE)
  settle(session)
  stopifnot(grepl("Archived winner", output$mvp_cards$html, fixed = TRUE),
    NROW(mvp_snapshot()$rows) == 11L, identical(tail(served, 1L), "unknown-scoring"))
  cat("PASS missing scoring configuration reads its exact archive and excludes the cached sibling scoring version\n")
})
