# Explicit lineup scheduling uses the same player and wire contracts as manual Save.
lineup_schedule_enabled <- function() {
  identical(tolower(Sys.getenv("FUTMONDO_LINEUP_SCHEDULING_ENABLED", "false")), "true") &&
    fm_worker_can_connect()
}

lineup_schedule_rpc <- function(name, payload, owner = "") {
  headers <- c(apikey = get_sb_key(), Authorization = paste("Bearer", get_sb_key()),
    `Content-Type` = "application/json")
  promises::then(fm_async_http(paste0(sub("/$", "", get_sb_url()), "/rest/v1/rpc/lineup_schedule_", name),
    "POST", fm_worker_json(payload), headers, owner = owner), function(response) {
      if (length(response$content)) jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE)
    })
}

lineup_schedule_scope <- function(context) fm_clause_scope(context)[setdiff(fm_worker_fields, "player_id")]

lineup_arrival_candidates <- function(transfers, roster, target) {
  rows <- transfers$records
  unknown <- list(complete = FALSE, rows = NULL)
  if (is.null(target) || !isTRUE(transfers$status %in% c("ok", "observed", "empty")) ||
      !is.data.frame(rows) || !all(c("player_id", "joins_at", "timing_error", "role") %in% names(rows))) return(unknown)
  rows <- rows[!rows$player_id %in% roster$id, , drop = FALSE]
  dates <- fm_time(rows$joins_at)
  if (anyNA(dates) || anyNA(rows$player_id) || any(!nzchar(rows$player_id)) ||
      anyDuplicated(rows$player_id) || anyNA(rows$timing_error) || any(nzchar(rows$timing_error))) return(unknown)
  rows <- rows[dates < target$starts_at, , drop = FALSE]
  rows <- rows[order(rows$joins_at, rows$player_id), , drop = FALSE]
  rownames(rows) <- NULL
  list(complete = TRUE, rows = rows)
}

lineup_arrival_identity <- function(rows) {
  rows[intersect(c("player_id", "player_name", "joins_at", "role", "role2", "status",
    "available", "photo", "team", "logo", "purchased_at", "seller_team_id"), names(rows))]
}

lineup_arrival_roster <- function(roster, incoming) {
  if (!nrow(incoming)) return(roster)
  optional <- function(field) incoming[[field]] %||% rep("", nrow(incoming))
  dplyr::bind_rows(roster, data.frame(id = incoming$player_id, name = incoming$player_name,
    role = incoming$role, role2 = optional("role2"), status = optional("status"),
    photo = optional("photo"), club = optional("team"), logo = optional("logo"),
    available = incoming$available %||% rep(NA, nrow(incoming))))
}

lineup_schedule_preview <- function(roster, arrivals, scores, rules, current, excluded, doubtful, coverage) {
  stages <- vector("list", nrow(arrivals) + 1L)
  for (i in seq_along(stages)) {
    rows <- lineup_arrival_roster(roster, arrivals[seq_len(i - 1L), , drop = FALSE])
    result <- lineup_optimize(rows, scores, unlist(rules$allowed), current, doubtful,
      rules$bench_enabled, rules$multiposition, excluded_ids = excluded, require_bench_coverage = coverage)
    if (is.null(result)) return(NULL)
    if (!isTRUE(rules$captain_enabled)) result$draft$captain <- ""
    stages[[i]] <- list(player_id = if (i == 1L) "" else arrivals$player_id[i - 1L],
      due_at = if (i == 1L) NULL else fm_clause_stamp(arrivals$joins_at[i - 1L]), lineup = result$draft)
    current <- result$draft
  }
  stages
}

lineup_schedule_draft <- function(value) {
  list(strategy = fm_scalar(value$strategy, ""), starters = unname(unlist(value$starters)),
    bench = unname(unlist(value$bench)), captain = fm_scalar(value$captain, ""))
}

# Runs only in the scheduled-lineup R bridge. Python owns authentication and HTTP.
lineup_schedule_check <- function(context, args, read, rounds = NULL) {
  desired <- lineup_schedule_draft(args$stage$lineup)
  official_answer <- read("/1/userteam/lineup")
  parsed <- lineup_parse_official(official_answer)
  official <- parsed$draft
  if (isTRUE(args$readback)) return(list(code = if (identical(lineup_identity(official),
    lineup_identity(desired))) "saved" else "readback_mismatch",
    official = official, wire = parsed$wire, locked = parsed$locked))
  target <- lineup_target(rounds())
  if (is.null(target) || !identical(target$id, args$round_id) ||
      abs(as.numeric(target$starts_at) - as.numeric(fm_time(args$kickoff))) > 0.001) {
    return(list(code = "round_changed"))
  }
  if (!identical(lineup_identity(official), lineup_identity(lineup_schedule_draft(args$baseline)))) {
    return(list(code = "lineup_changed"))
  }
  roster_answer <- read("/1/userteam/roster")
  owned <- lineup_roster_frame(list(roster = lineup_roster_records(roster_answer)))
  required <- c(args$stage$player_id, desired$starters, desired$bench)
  if (any(!required[nzchar(required)] %in% owned$id)) return(list(code = "waiting_for_ownership"))
  snapshot <- lineup_normalize_bundle(list(lineup = official_answer, roster = roster_answer,
    info = read("/1/userteam/information", list(type = "market")),
    strategies = read("/5/strategy/availables")), context, target)
  if (!identical(snapshot$competition_id, context$competition_id) ||
      !identical(snapshot$season, context$season) ||
      !identical(snapshot$scoring_version, context$scoring_version)) return(list(code = "context_changed"))
  if (length(lineup_validate(desired, owned, snapshot$rules)) ||
      length(lineup_locked_changes(snapshot, desired)) ||
      any(!lineup_status_eligible(owned[owned$id %in% c(desired$starters, desired$bench), , drop = FALSE],
        args$doubtful))) return(list(code = "lineup_unavailable", snapshot = snapshot))
  # Fresh squad carries the same source clause fields used by transfer reconciliation.
  destinations <- Filter(function(x) fm_scalar(x$id %||% x$`_id`, "") %in%
    c(desired$starters, desired$bench), roster_answer)
  for (player in destinations) {
    clause <- player$clause
    if (!isTRUE(clause$transferred)) next
    leaves <- fm_time(clause$transferDate)
    if (length(leaves) != 1L || is.na(leaves) || leaves <= target$starts_at) {
      return(list(code = "lineup_unavailable", snapshot = snapshot))
    }
  }
  if (identical(lineup_identity(official), lineup_identity(desired))) return(list(code = "unchanged", snapshot = snapshot))
  wire <- lineup_modified_wire(desired, owned, snapshot$rules)
  list(code = "ready", snapshot = snapshot, current = snapshot$source$wire, modified = wire,
    changes = lineup_changes(snapshot$source$wire, wire))
}
