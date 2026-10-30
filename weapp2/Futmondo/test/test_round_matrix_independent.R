#!/usr/bin/env Rscript
# Independent checks for the league-wide latest-round matrix reader.
# Uses the production reader and pager with a deterministic PostgREST fixture.
app <- normalizePath(".")
stopifnot(file.exists(file.path(app, "supabase_connector.R")))

load_assignment <- function(path, name, env) {
  expressions <- as.list(parse(path))
  matches <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], as.name(name)), expressions)
  stopifnot(length(matches) > 0L)
  eval(matches[[length(matches)]], envir = env)
}

reader_env <- new.env(parent = globalenv())
load_assignment(file.path(app, "data_contracts.R"), "fm_scalar", reader_env)
load_assignment(file.path(app, "data_contracts.R"), "fm_time", reader_env)
load_assignment(file.path(app, "data_contracts.R"), "%||%", reader_env)
load_assignment(file.path(app, "history_repository.R"), "history_read_pages", reader_env)
load_assignment(file.path(app, "history_repository.R"), "history_context_filter", reader_env)
load_assignment(file.path(app, "history_repository.R"), "reconcile_player_points", reader_env)
load_assignment(file.path(app, "background_runtime.R"), "record_acquired_resource", reader_env)
load_assignment(file.path(app, "supabase_connector.R"), "read_league_round_points", reader_env)
load_assignment(file.path(app, "Modules/Players_Table_Module.R"), "add_recent_points_average", reader_env)

players <- sprintf("p%03d", 1:151)
rounds <- expand.grid(player_id = players, season = c("2025", "2026"),
  scoring_version = c("A", "B"), round = 1:5,
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
rounds$championship_id <- "league-A"
rounds$points <- as.numeric((seq_len(nrow(rounds)) %% 13L) - 6L)
rounds$score_status <- "unknown"
rounds$observed_at <- "2026-10-30T12:00:00Z"
rounds$occurred_at <- NA_character_
rounds$round_start_at <- NA_character_
foreign <- rounds[1:7, , drop = FALSE]
foreign$championship_id <- "league-B"
fixture <- rbind(rounds, foreign)
fixture <- fixture[order(fixture$player_id, fixture$season,
  fixture$scoring_version, fixture$round), , drop = FALSE]
rownames(fixture) <- NULL

calls <- list()
server_cap <- 700L
reader_env$supabase_get <- function(table_name, query_params = list()) {
  calls[[length(calls) + 1L]] <<- list(table = table_name, query = query_params)
  stopifnot(identical(table_name, "player_round_points_latest"),
    identical(query_params$championship_id, "eq.league-A"),
    identical(query_params$order, "player_id.asc,season.asc,scoring_version.asc,round.asc"),
    identical(query_params$select,
      "championship_id,player_id,season,scoring_version,round,points,score_status,observed_at,occurred_at,round_start_at"),
    is.null(query_params$season), is.null(query_params$scoring_version),
    is.null(query_params$player_id))
  selected <- fixture[fixture$championship_id == "league-A", , drop = FALSE]
  offset <- as.integer(query_params$offset %||% 0L)
  limit <- min(as.integer(query_params$limit), server_cap)
  start <- offset + 1L
  if (start > nrow(selected)) return(selected[0, , drop = FALSE])
  selected[seq.int(start, min(nrow(selected), offset + limit)), , drop = FALSE]
}

`%||%` <- function(x, y) if (is.null(x)) y else x

all_rows <- reader_env$read_league_round_points("league-A", max_rows = 50000L)
expected <- fixture[fixture$championship_id == "league-A", , drop = FALSE]
stopifnot(is.data.frame(all_rows), nrow(all_rows) == nrow(expected),
  isTRUE(attr(all_rows, "complete")), identical(attr(all_rows, "fetch_status"), "ok"),
  identical(all_rows$player_id, expected$player_id),
  identical(all_rows$season, expected$season),
  identical(all_rows$scoring_version, expected$scoring_version),
  identical(as.numeric(all_rows$round), as.numeric(expected$round)),
  !anyDuplicated(all_rows[c("championship_id", "player_id", "season", "scoring_version", "round")]),
  length(calls) == 6L,
  identical(vapply(calls, function(call) as.integer(call$query$offset), integer(1)),
    c(0L, 700L, 1400L, 2100L, 2800L, 3020L)))
cat("PASS league-only read continues short server pages without gaps or duplicates across seasons/scoring versions.\n")

calls <- list()
capped <- reader_env$read_league_round_points("league-A", max_rows = 1500L)
stopifnot(nrow(capped) == 1500L, identical(attr(capped, "complete"), FALSE),
  identical(attr(capped, "fetch_status"), "partial"), length(calls) == 3L)
cat("PASS row cap is reported as partial coverage instead of complete.\n")

calls <- list()
fixture <- fixture[FALSE, , drop = FALSE]
empty <- reader_env$read_league_round_points("league-A", max_rows = 50000L)
stopifnot(is.data.frame(empty), nrow(empty) == 0L,
  isTRUE(attr(empty, "complete")), identical(attr(empty, "fetch_status"), "empty"))
cat("PASS empty league history is a complete empty result.\n")

players_for_average <- data.frame(id = c("three", "eight", "missing", "reported"),
  `average.averageLastFive` = c(NA_real_, NA_real_, NA_real_, 12), check.names = FALSE)
average_history <- data.frame(
  player_id = c("three", "three", "three", "eight",
    rep("eight", 8), "missing", "missing"),
  round = c(8, 9, 10, 3, 3:10, 8, 10),
  points = c(0, -2, 8, 3, 3:10, 2, 6),
  stringsAsFactors = FALSE
)
average_history <- average_history[!duplicated(average_history[c("player_id", "round")],
  fromLast = TRUE), , drop = FALSE]
three <- reader_env$add_recent_points_average(players_for_average, average_history, 3L)
stopifnot(three$recent_points_avg[1] == 2,
  three$recent_points_avg[2] == 9,
  is.na(three$recent_points_avg[3]), three$recent_points_count[3] == 2L,
  three$recent_points_required[3] == 3L,
  three$recent_points_status[3] == "partial")
eight <- reader_env$add_recent_points_average(players_for_average, average_history, 8L)
stopifnot(eight$recent_points_avg[2] == 6.5,
  is.na(eight$recent_points_avg[1]), eight$recent_points_count[1] == 3L,
  is.na(eight$recent_points_avg[3]),
  is.na(eight$recent_points_avg[4]))
five <- reader_env$add_recent_points_average(players_for_average, average_history, 5L)
stopifnot(five$recent_points_avg[4] == 12,
  five$recent_points_status[4] == "reported")
cat("PASS known-answer N=3 and N=8 means; zero/negative values count, gaps stay missing, and only N=5 uses the source-reported fallback.\n")

context <- list(championship_id = "league-A", player_id = "p1",
  season = "2026", scoring_version = "scoring-A")
stored_revision <- data.frame(championship_id = "league-A", player_id = "p1",
  season = "2026", scoring_version = "scoring-A", round = 11,
  points = NA_real_, score_status = "unavailable", observed_at = "2026-10-30T12:00:00Z")
older_live_revision <- transform(stored_revision, points = 8, score_status = "unknown",
  observed_at = "2026-10-29T12:00:00Z")
latest_unknown <- reader_env$reconcile_player_points(stored_revision,
  older_live_revision, context)
stopifnot(nrow(latest_unknown) == 1L, is.na(latest_unknown$points),
  latest_unknown$score_status == "unavailable", latest_unknown$source == "supabase")
window_history <- data.frame(player_id = "p1", round = c(9, 10, 11),
  points = c(2, 4, NA_real_))
anchored <- reader_env$add_recent_points_average(data.frame(id = "p1"), window_history, 3L)
stopifnot(is.na(anchored$recent_points_avg), anchored$recent_points_count == 2L,
  anchored$recent_points_required == 3L, anchored$recent_points_status == "partial")
cat("PASS latest NULL suppresses an older numeric revision and remains the anchor of the N=3 window.\n")

persisted <- list()
reader_env$supabase_post <- function(table_name, payload, ...) {
  persisted[[length(persisted) + 1L]] <<- list(table = table_name, payload = payload)
  invisible(TRUE)
}
persist_context <- c(context, list(user_team_id = "team-A"))
point_payload <- function(value, status, stamp) list(context = persist_context,
  points = data.frame(round_number = 11, points = value, date = "2026-10-25",
    score_status = status, observed_at = stamp, source = "futmondo"))
old_null <- data.frame(round_number = 11, points = NA_real_, date = "2026-10-25",
  score_status = "unavailable", observed_at = "2026-10-29T12:00:00Z")
old_numeric <- transform(old_null, points = 8, score_status = "unknown")
reader_env$record_acquired_resource("player_card_history",
  c(point_payload(NA_real_, "unavailable", "2026-10-30T12:00:00Z"),
    list(previous_points = old_numeric)), persist_context)
stopifnot(length(persisted) == 1L, is.na(persisted[[1]]$payload$points[1]))
persisted <- list()
reader_env$record_acquired_resource("player_card_history",
  c(point_payload(NA_real_, "unavailable", "2026-10-30T12:00:00Z"),
    list(previous_points = old_null)), persist_context)
stopifnot(length(persisted) == 0L)
reader_env$record_acquired_resource("player_card_history",
  c(point_payload(0, "unknown", "2026-10-30T12:00:00Z"),
    list(previous_points = old_null)), persist_context)
stopifnot(length(persisted) == 1L, persisted[[1]]$payload$points[1] == 0)
cat("PASS persistence writes numeric-to-NULL and NULL-to-zero changes, while skipping an unchanged NULL.\n")
