#!/usr/bin/env Rscript
# Offline known-answer accounting, durable ledger reuse, and real module paths.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source("global.R"))
source("test/test_classification_bonuses.R")

# The real accounting builder established r1 = (10, 10, 5, 0).
partial_history <- data.frame(
  team_id = "a", team_name = "A", round = 1:2,
  points = c(5, 72), score_status = c("final", "provisional")
)
partial_standings <- classification_window_table(partial_history)
partial <- classification_bonus_table(partial_standings, partial_history, out, c(1, 2))
stopifnot(
  partial$points == 77, partial$points_bonus == 10, partial$mvp_bonus == 10,
  partial$best_xi_bonus == 5, partial$position_bonus == 0,
  identical(attr(partial, "bonus_pending_rounds"), 2L),
  identical(attr(partial, "bonus_completed_rounds"), 1L)
)
wire <- jsonlite::toJSON(rival_snapshot_encode(out$cash_ledgers$a),
  auto_unbox = TRUE, na = "null", null = "null", digits = NA
)
restored <- list(cash_ledgers = list(a = rival_snapshot_decode(jsonlite::fromJSON(wire, simplifyVector = FALSE))))
stopifnot(identical(partial, classification_bonus_table(partial_standings, partial_history, restored, c(1, 2))))
# A missing display score cannot invalidate independent verified bonus evidence.
score_gap <- partial_history
score_gap$score_status[1] <- "unavailable"
score_gap$points[1] <- NA_real_
stopifnot(classification_bonus_table(partial_standings, score_gap, out, c(1, 2))$points_bonus == 10)
# Unknown completed components and absent completed rounds remain unknown.
gap <- out
attr(gap$cash_ledgers$a, "round_bonus_totals")$amount[2] <- NA_real_
unknown <- classification_bonus_table(partial_standings, partial_history, gap, c(1, 2))
stopifnot(is.na(unknown$mvp_bonus), unknown$points_bonus == 10)
missing_round <- classification_bonus_table(partial_standings, partial_history, out, c(1, 3), expected_rounds = 1:3)
stopifnot(all(is.na(missing_round[, c("points_bonus", "mvp_bonus", "best_xi_bonus", "position_bonus")])))
# Calendar status can supersede a stale compact-catalog completion flag.
# It must also work without ranking rows after a failed live read.
conflicting_catalog <- data.frame(round_number = 2, status = "running", is_finished = TRUE)
calendar_only <- classification_bonus_table(partial_standings, partial_history[1, ], out,
  c(1, 2),
  expected_rounds = 1:2, rounds = conflicting_catalog
)
stopifnot(
  calendar_only$points_bonus == 10,
  identical(as.numeric(attr(calendar_only, "bonus_pending_rounds")), 2)
)
cat("PASS partial known ledger answers, persisted reuse, independent score gaps and unknown completed evidence\n")

#' Disable read binding for a synthetic offline module regression
#' @param ... Unused binding arguments.
#' @return NULL invisibly; source fixtures are already reactive.
#' @keywords internal
bind_factual_reads <- function(...) {
  invisible(NULL)
}
#' Provide the offline module's shared award source
#' @param ... Unused source arguments.
#' @return Reactive empty awards; finance evidence is prebuilt separately.
#' @keywords internal
shared_mvp_source <- function(...) {
  reactive(list())
}
partial_finances <- reactiveVal(out)
#' Reuse the prebuilt offline finance source
#' @param ... Unused source arguments.
#' @return Reactive finance dataset with real accounting-builder results.
#' @keywords internal
shared_rival_finance_source <- function(...) {
  partial_finances
}
reads <- character()
#' Provide the synthetic two-round calendar
#' @param ... Unused source arguments.
#' @return One closed round and one playing round.
#' @keywords internal
get_classification_round_catalog <- function(...) {
  reads <<- c(reads, "catalog")
  list(current_round_number = 2, rounds = data.frame(
    round_id = c("r1", "r2"),
    round_number = 1:2, is_finished = c(TRUE, FALSE), status = c("closed", "playing")
  ))
}
ranking_failed <- reactiveVal(FALSE)
#' Provide a synthetic live ranking or a failure
#' @param ... Unused source arguments.
#' @return Provisional ranking; throws when ranking_failed is TRUE.
#' @keywords internal
get_classification_round_ranking <- function(...) {
  reads <<- c(reads, "ranking")
  if (ranking_failed()) {
    stop("Synthetic ranking source failure")
  }
  list(round_id = "r2", round_number = 2, rows = data.frame(
    team_id = "a",
    team_name = "A", round = 2, points = 72, score_status = "provisional", players_left = 3
  ))
}
#' Provide synthetic historical points
#' @param ... Unused source arguments.
#' @return One completed score and one live score.
#' @keywords internal
get_user_team_rounds <- function(...) {
  reads <<- c(reads, "history")
  list(list(round = 1, points = 5), list(round = 2, points = 72))
}
shiny::testServer(classification_Server, args = list(
  is_module_active = reactive(TRUE), login_token = reactive(c(userid = "u", token = "synthetic")),
  championship_id = reactive("league"), user_team_id = reactive("a"),
  user_teams_RV = reactive(data.frame(teamid = "a", teamname = "A", points = 77)),
  league_settings_source = reactive(list(league = list(configuration = list()), info = list()))
), {
  session$setInputs(classification_view = "partial", round_range_slider = c(1, 2))
  stopifnot(
    standings_RV()$points == 77, standings_RV()$points_bonus == 10,
    standings_RV()$mvp_bonus == 10, standings_RV()$best_xi_bonus == 5,
    standings_RV()$position_bonus == 0,
    grepl("completed rounds only", output$classification_status$html, fixed = TRUE),
    all(is.finite(unlist(standings_RV()[, c("points_bonus", "mvp_bonus", "best_xi_bonus", "position_bonus")])))
  )
  # Missing totals are loading while acquisition is pending, not unavailable.
  partial_finances(list(loading = TRUE))
  session$flushReact()
  pending_columns <- jsonlite::fromJSON(output$classification_table, simplifyVector = FALSE)$x$tag$attribs$columns
  stopifnot(all(vapply(Filter(function(column) {
    column$id %in% c("points_bonus", "mvp_bonus", "best_xi_bonus", "position_bonus")
  }, pending_columns), function(column) {
    identical(column$na, "Loading…")
  }, logical(1))))
  partial_finances(out)
  session$flushReact()
  stopifnot(standings_RV()$points_bonus == 10)
  before <- reads
  session$setInputs(round_range_slider = c(2, 2))
  stopifnot(
    standings_RV()$points == 72, is.na(standings_RV()$points_bonus),
    grepl("Pending", output$classification_table, fixed = TRUE),
    grepl("pending", output$classification_status$html, fixed = TRUE)
  )
  session$setInputs(round_range_slider = c(1, 1))
  stopifnot(
    standings_RV()$points == 5, standings_RV()$points_bonus == 10,
    !grepl("completed rounds only|pending", output$classification_status$html), identical(reads, before)
  )
  session$setInputs(round_range_slider = c(1, 2))
  ranking_failed(TRUE)
  session$flushReact()
  stopifnot(
    standings_RV()$points_bonus == 10,
    grepl("unavailable", output$classification_status$html, fixed = TRUE),
    grepl("completed rounds only", output$classification_status$html, fixed = TRUE)
  )
})
cat("PASS real partial module: numeric bonuses, pending-only cells, completed range, failed live read, no extra range reads\n")
