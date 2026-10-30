options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source("global.R"))
source("Modules/Players_Table_Module.R")

checks <- 0L
check <- function(name, expr) {
  force(expr)
  checks <<- checks + 1L
  cat("PASS", name, "\n")
}

# The endpoint must distinguish an explicitly empty points array from an
# absent/malformed points field before the caller records successful coverage.
check("explicit empty points array and zero-row table are successful", {
  stopifnot(recent_points_summary_valid(list(points=list(),points_received=TRUE)),
    recent_points_summary_valid(list(points=data.frame(round=integer(),points=numeric()),points_received=TRUE)),
    recent_points_summary_valid(list(points=NULL,points_received=TRUE)))
})
check("missing, malformed, and stale point responses are rejected", {
  mixed <- list(points=list(list(round=1,points=0),list(points=5)),points_received=TRUE)
  bad_round <- list(points=list(list(round=1,points=0),list(round="bad",points=5)),points_received=TRUE)
  bad_score <- list(points=list(list(round=1,points=0),list(round=2,points="not-a-score")),points_received=TRUE)
  vector_score <- list(points=list(list(round=1,points=c(2,3))),points_received=TRUE)
  unavailable_score <- list(points=list(list(round=1,points=NA_real_),list(round=2,points=NULL)),points_received=TRUE)
  missing <- list(championship=list(season="S1"))
  stale <- list(points=list(),points_received=TRUE)
  attr(stale,"fetch_status") <- "stale"
  stopifnot(!recent_points_summary_valid(mixed),!recent_points_summary_valid(bad_round),
    !recent_points_summary_valid(bad_score),!recent_points_summary_valid(vector_score),
    !recent_points_summary_valid(missing),
    !recent_points_summary_valid(list(points=list(),points_received=FALSE)),
    !recent_points_summary_valid(stale),recent_points_summary_valid(unavailable_score))
})

auth <- c(token="synthetic",userid="account-A")
checked_at <- as.POSIXct("2026-10-30 12:34:56",tz="UTC")
summary_fixture <- list(points=list(list(round=1,points=0),list(round=2,points=-3)),
  points_received=TRUE,championship=list(season="season-A"))
attr(summary_fixture,"observed_at") <- checked_at
original_summary <- get_player_summary
get_player_summary <- function(...) summary_fixture
source_result <- get_player_recent_points(auth,"league-A","team-A","player-A","scoring-A")
get_player_summary <- original_summary
check("successful marker time comes from the fetched source observation", {
  stopifnot(identical(as.numeric(source_result$checked_at),as.numeric(checked_at)),
    all(source_result$observations$observed_at==format(checked_at,"%Y-%m-%dT%H:%M:%OSZ",tz="UTC")),
    identical(source_result$observations$points,c(0,-3)))
})

# The database stores shared factual league rows; the caller still filters every
# candidate marker/observation to the exact league, season, and scoring version.
now <- as.POSIXct("2026-10-30 12:34:56",tz="UTC")
context <- resource_context(auth,"league-A","team-A",season="season-A",scoring_version="scoring-A")
context$player_id <- ""
check_rows <- data.frame(
  championship_id=c("league-A","league-B","league-A","league-A","league-A"),
  player_id=c("empty-A","wrong-league","wrong-season","wrong-scoring","old-check"),
  season=c("season-A","season-A","season-B","season-A","season-A"),
  scoring_version=c("scoring-A","scoring-A","scoring-A","scoring-B","scoring-A"),
  checked_at=c(now,now,now,now,now-25*3600),
  result_status=c("empty","scores","scores","scores","empty"),
  point_count=c(0L,1L,1L,1L,0L),stringsAsFactors=FALSE)
legacy <- data.frame(championship_id=c("league-A","league-A"),
  player_id=c("legacy-A","wrong-legacy-season"),season=c("season-A","season-B"),
  scoring_version=c("scoring-A","scoring-A"),round=c(4L,4L),points=c(0,8),
  observed_at=c(now,now),stringsAsFactors=FALSE)
reusable <- recent_points_saved_players(legacy,context,check_rows,now)
check("only fresh exact-context checks and legacy rows are reused", {
  stopifnot(setequal(reusable,c("empty-A","legacy-A")))
})
check("an empty result cannot be reused until its successful check is stored", {
  stopifnot(!length(recent_points_saved_players(data.frame(),context,data.frame(),now)),
    identical(recent_points_saved_players(data.frame(),context,
      check_rows[check_rows$player_id=="empty-A",,drop=FALSE],now),"empty-A"))
})
context_other_account <- resource_context(c(token="synthetic",userid="account-B"),
  "league-A","team-A",season="season-A",scoring_version="scoring-A")
check("private resource cache keys remain account isolated", {
  stopifnot(api_cache_key("player_summary_points",context=context)!=
    api_cache_key("player_summary_points",context=context_other_account))
})

# Exercise the actual asynchronous persistence queue with a controlled HTTP
# adapter. A check marker must be the last operation, and callback success must
# wait for its acknowledgement after every score chunk.
original_http <- fm_async_http
original_url <- get_sb_url
original_key <- get_sb_key
original_cancel <- fm_transport_cancel
http_pending <- list()
cancelled_transports <- character()
fm_async_http <- function(url,method,body,headers,owner) {
  table <- sub("^.*rest/v1/", "", sub("\\?.*$", "", url))
  row_count <- length(jsonlite::fromJSON(body,simplifyVector=FALSE))
  promises::promise(function(resolve,reject) {
    http_pending[[length(http_pending)+1L]] <<- list(table=table,row_count=row_count,
      resolve=resolve,reject=reject)
  })
}
get_sb_url <- function() "https://unit.test"
get_sb_key <- function() "unit-test-key"
fm_transport_cancel <- function(owner) {
  cancelled_transports <<- c(cancelled_transports,owner)
  invisible(TRUE)
}

empty_persistence_state <- function() {
  .persistence_state$queue <- list()
  .persistence_state$active <- NULL
  .persistence_state$scheduled <- FALSE
  .persistence_state$errors <- list()
  .persistence_state$acknowledged <- list()
  .persistence_state$revision(0L)
}
settle <- function(session) {
  for (i in seq_len(40L)) {
    later::run_now(timeoutSecs=0)
    session$flushReact()
  }
}
point_rows <- function(n,stamp) data.frame(
  player_id=rep("player-A",n),championship_id=rep("league-A",n),
  season=rep("season-A",n),scoring_version=rep("scoring-A",n),round=seq_len(n),
  points=rep(c(-2,0,7),length.out=n),score_status=rep("unknown",n),
  observed_at=rep(format(stamp,"%Y-%m-%dT%H:%M:%OSZ",tz="UTC"),n),
  occurred_at=rep(NA_character_,n),round_start_at=rep(NA_character_,n),stringsAsFactors=FALSE)

persistence_server <- function(input,output,session) {}
shiny::testServer(persistence_server, {
  options(futmondo.offline=FALSE)
  empty_persistence_state()
  http_pending <<- list()
  callback_results <- logical()
  ctx <- resource_context(auth,"league-A","team-A","player-A","season-A","scoring-A",session_id=session$token)
  payload <- list(context=ctx,checked_at=checked_at,observations=point_rows(205L,checked_at))
  append_result <- function(ok) callback_results <<- c(callback_results,isTRUE(ok))
  stopifnot(isTRUE(persist_recent_points_snapshot(payload,ctx,append_result)))
  settle(session)
  stopifnot(length(http_pending)==1L,identical(http_pending[[1L]]$table,"player_match_observations"),!length(callback_results))
  stopifnot(isTRUE(persist_recent_points_snapshot(payload,ctx,append_result)))
  expected_tables <- c(rep("player_match_observations",3L),"player_round_history_checks")
  expected_sizes <- c(100L,100L,5L,1L)
  for (i in seq_along(expected_tables)) {
    stopifnot(identical(http_pending[[i]]$table,expected_tables[[i]]),!length(callback_results))
    http_pending[[i]]$resolve(list(status=201L))
    settle(session)
    if (i<length(expected_tables)) stopifnot(!length(callback_results))
  }
  check("success marker is acknowledged only after all score chunks and marker write", {
    stopifnot(identical(callback_results,c(TRUE,TRUE)),length(http_pending)==4L,
      identical(vapply(http_pending,`[[`,character(1),"table"),expected_tables),
      identical(vapply(http_pending,`[[`,integer(1),"row_count"),expected_sizes))
  })
})

# A failed score chunk must end the job without attempting its trailing marker.
shiny::testServer(persistence_server, {
  options(futmondo.offline=FALSE)
  empty_persistence_state()
  http_pending <<- list()
  cancelled_transports <<- character()
  callback_result <- NULL
  ctx <- resource_context(auth,"league-A","team-A","player-A","season-A","scoring-A",session_id=session$token)
  payload <- list(context=ctx,checked_at=checked_at+1,observations=point_rows(2L,checked_at+1))
  stopifnot(isTRUE(persist_recent_points_snapshot(payload,ctx,function(ok)callback_result<<-isTRUE(ok))))
  settle(session)
  stopifnot(length(http_pending)==1L,identical(http_pending[[1L]]$table,"player_match_observations"))
  http_pending[[1L]]$reject(simpleError("HTTP 500"))
  settle(session)
  check("score write failure is reported and prevents marker write", {
    stopifnot(identical(callback_result,FALSE),length(http_pending)==1L,
      identical(http_pending[[1L]]$table,"player_match_observations"),
      grepl("history could not be saved",background_sync_status("account-A")$error,ignore.case=TRUE))
  })
})

# Coalesced listeners must be settled as failures when their shared write is
# canceled; a dropped write must never leave an empty-history marker reusable.
shiny::testServer(persistence_server, {
  options(futmondo.offline=FALSE)
  empty_persistence_state()
  http_pending <<- list()
  cancelled_transports <<- character()
  ctx_a <- resource_context(auth,"league-A","team-A","player-A","season-A","scoring-A",session_id="session-A")
  ctx_b <- ctx_a;ctx_b$session_id <- "session-B"
  payload <- list(context=ctx_a,checked_at=checked_at+2,observations=point_rows(2L,checked_at+2))
  queued_results <- logical()
  stopifnot(isTRUE(persist_recent_points_snapshot(payload,ctx_a,function(ok)queued_results<<-c(queued_results,isTRUE(ok)))),
    isTRUE(persist_recent_points_snapshot(payload,ctx_b,function(ok)queued_results<<-c(queued_results,isTRUE(ok)))))
  stopifnot(length(.persistence_state$queue)==1L)
  cancel_persistence_jobs("account-A","session-A")
  settle(session)
  check("canceling a queued coalesced write settles every callback false", {
    stopifnot(identical(queued_results,c(FALSE,FALSE)),!length(http_pending))
  })

  empty_persistence_state()
  http_pending <<- list()
  cancelled_transports <<- character()
  active_results <- logical()
  payload$checked_at <- checked_at+3
  payload$observations <- point_rows(2L,checked_at+3)
  stopifnot(isTRUE(persist_recent_points_snapshot(payload,ctx_a,function(ok)active_results<<-c(active_results,isTRUE(ok)))))
  settle(session)
  stopifnot(length(http_pending)==1L,identical(http_pending[[1L]]$table,"player_match_observations"))
  stopifnot(isTRUE(persist_recent_points_snapshot(payload,ctx_b,function(ok)active_results<<-c(active_results,isTRUE(ok)))))
  cancel_persistence_jobs("account-A","session-A")
  settle(session)
  check("canceling an active coalesced score write settles all listeners without a marker", {
    stopifnot(identical(active_results,c(FALSE,FALSE)),length(http_pending)==1L,
      identical(http_pending[[1L]]$table,"player_match_observations"),
      "persistence:session-A"%in%cancelled_transports)
  })
})

fm_async_http <- original_http
get_sb_url <- original_url
get_sb_key <- original_key
fm_transport_cancel <- original_cancel
options(futmondo.offline=TRUE)
cat(sprintf("SHARED ROUND DATASET INDEPENDENT: %d passed / 0 failed\n",checks))
