# In-session cooperative persistence; see docs/background_worker.md.
.persistence_state <- new.env(parent=emptyenv())
.persistence_state$queue <- list()
.persistence_state$active <- NULL
.persistence_state$progress <- list()
.persistence_state$scheduled <- FALSE
.persistence_state$errors <- list()
.persistence_max_jobs_per_owner <- 25L

# Retry identity follows the operation/context, independently of timestamps or tokens.
persistence_job_scope <- function(name, args) {
  context <- list(fn = name)
  if (identical(name, "collect_account_observations") && length(args) >= 3L) {
    context$league <- args[[2]]
    context$team <- args[[3]]
  } else if (identical(name, "log_player_history") && length(args) >= 2L) {
    context$league <- args[[2]]
  } else if (identical(name, "supabase_post_direct") && length(args) >= 2L) {
    context$table <- args[[1]]
    payload <- args[[2]]
    conflict <- if (length(args) >= 3L && !is.null(args[[3]])) args[[3]] else supabase_conflict_key(context$table)
    fields <- if (is.character(conflict) && length(conflict) == 1L && !is.na(conflict))
      setdiff(trimws(strsplit(conflict, ",", fixed = TRUE)[[1]]), "observed_at") else character()
    if (!length(fields)) fields <- switch(context$table,
      championships = "id", real_clubs = "id", players = "id", user_teams = "id",
      manager_dna_profiles = "team_id", character())
    rows <- if (is.data.frame(payload)) {
      lapply(seq_len(nrow(payload)), function(i) as.list(payload[i, , drop = FALSE]))
    } else if (is.list(payload) && !is.null(names(payload))) {
      list(payload)
    } else if (is.list(payload)) payload else list()
    reliable <- length(fields) > 0L && length(rows) > 0L &&
      all(vapply(rows, function(row) is.list(row) && all(fields %in% names(row)) &&
        all(vapply(row[fields], function(value) is.atomic(value) && length(value) == 1L &&
          !is.na(value) && nzchar(as.character(value)), logical(1))), logical(1)))
    if (reliable) {
      # Hash each complete row key before sorting; separate per-column sets can
      # collide when compound identities have different row pairings.
      context$rows <- sort(vapply(rows, function(row) {
        row_fields <- sort(unique(c(fields, intersect(c("championship_id", "user_team_id", "user_id"), names(row)))))
        identity <- lapply(row[row_fields], as.character)
        as.character(openssl::sha256(serialize(identity, NULL)))
      }, character(1)))
    } else {
      # Unknown/generated IDs cannot prove that a different write retried this one.
      context$payload <- payload
    }
  }
  as.character(openssl::sha256(serialize(context, NULL)))
}

persistence_failure_message <- function(issues = list()) {
  categories <- vapply(issues, function(issue) fm_scalar(issue$category, "write"), character(1))
  if ("configuration" %in% categories) return("History saving is unavailable because the database is not configured. Live Futmondo data is still available.")
  if ("authorization" %in% categories) return("History could not be saved because database access was denied. Contact the app administrator.")
  if ("schema" %in% categories) return("History could not be saved because the database needs an update. Contact the app administrator.")
  if ("connection" %in% categories) return("History could not be saved because the database connection failed. Use Refresh to retry.")
  if ("runtime" %in% categories) return("Observation saving stopped unexpectedly. Use Refresh to start collection again.")
  "Some history could not be saved because a write was rejected or its result could not be confirmed. Use Refresh to retry; if this continues, contact the app administrator."
}

persistence_record_result <- function(job, ok, issues = list()) {
  owner <- job$owner; key <- job$error_key %||% job$key
  failures <- .persistence_state$errors[[owner]]
  if (!is.list(failures)) failures <- list()
  details <- vapply(issues,function(issue) paste0(issue$table %||% "unknown",
    if(!is.null(issue$stage) && nzchar(issue$stage)) paste0("/",issue$stage) else "",
    if(is.finite(issue$http_status %||% NA_real_)) paste0(" HTTP ",issue$http_status) else "",
    if(nzchar(issue$code %||% "")) paste0(" ",issue$code) else ""),character(1))
  failures[[key]] <- if (isTRUE(ok)) NULL else paste(persistence_failure_message(issues),
    if(length(details)) paste0("Details: ",paste(unique(details),collapse="; ")) else "")
  .persistence_state$errors[[owner]] <- failures
  if (!isTRUE(ok)) message("[Persistence] Incomplete job: ", job$fn %||% "unknown")
}

defer_persistence <- function(fn, args=list()) {
  if (isTRUE(getOption("futmondo.offline",FALSE))) return(invisible(TRUE))
  name <- if (is.character(fn)) fn else deparse(substitute(fn))
  if (length(name)!=1L || !exists(name,mode="function")) return(FALSE)
  domain <- shiny::getDefaultReactiveDomain()
  owner <- if (!is.null(domain)) domain$userData$futmondo_user_id else NULL
  if (is.null(owner) && length(args) && valid_login(args[[1]])) owner <- args[[1]][["userid"]]
  owner <- fm_scalar(owner,"system")
  session_id <- if (!is.null(domain)) fm_scalar(domain$token, owner) else owner
  key <- as.character(openssl::sha256(serialize(list(owner,name,args),NULL)))
  error_key <- persistence_job_scope(name,args)
  active <- .persistence_state$active
  existing <- c(.persistence_state$queue, if (!is.null(active)) list(active) else list())
  if (any(vapply(existing,function(x)identical(x$key,key),logical(1)))) return(invisible(TRUE))
  if (sum(vapply(existing,function(x)identical(x$owner,owner),logical(1))) >= .persistence_max_jobs_per_owner) {
    failures <- .persistence_state$errors[[owner]]; if (!is.list(failures)) failures <- list()
    failures[[error_key]] <- "Background queue limit reached for this account. Use Refresh to retry."
    .persistence_state$errors[[owner]] <- failures
    return(FALSE)
  }
  if (length(.persistence_state$queue)>=200L) {
    failures <- .persistence_state$errors[[owner]]; if (!is.list(failures)) failures <- list()
    failures[[error_key]] <- "Background queue full. Use Refresh to retry."; .persistence_state$errors[[owner]] <- failures
    return(FALSE)
  }
  .persistence_state$queue[[length(.persistence_state$queue)+1L]] <- list(fn=name,args=args,owner=owner,session_id=session_id,key=key,error_key=error_key)
  if (!isTRUE(.persistence_state$scheduled)) { .persistence_state$scheduled <- TRUE; later::later(persistence_tick,0) }
  invisible(TRUE)
}

# Logout prevents queued work from continuing with that browser session's token.
cancel_persistence_jobs <- function(owner, session_id = NULL) {
  owner <- fm_scalar(owner, "")
  if (!nzchar(owner)) return(invisible(0L))
  matches <- function(job) identical(job$owner, owner) &&
    (is.null(session_id) || identical(job$session_id, session_id))
  queued <- .persistence_state$queue
  dropped <- sum(vapply(queued, matches, logical(1)))
  .persistence_state$queue <- Filter(function(job) !matches(job), queued)
  if (!is.null(.persistence_state$active) && matches(.persistence_state$active)) {
    .persistence_state$active <- NULL; dropped <- dropped + 1L
  }
  .persistence_state$progress[[owner]] <- NULL
  invisible(dropped)
}

background_sync_status <- function(user_id) {
  owner <- fm_scalar(user_id,"")
  active <- .persistence_state$active
  jobs <- c(.persistence_state$queue, if (!is.null(active)) list(active) else list())
  pending <- sum(vapply(jobs,function(x)identical(x$owner,owner),logical(1)))
  errors <- unlist(.persistence_state$errors[[owner]],use.names=FALSE)
  progress <- .persistence_state$progress[[owner]]
  list(pending=pending,error=if(length(errors)) paste(unique(errors),collapse=" ") else NULL,
       progress=if (is.character(progress) && length(progress)==1L) progress else NULL)
}

persistence_tick <- function() {
  if (is.null(.persistence_state$active) && length(.persistence_state$queue)) {
    .persistence_state$active <- .persistence_state$queue[[1]]
    .persistence_state$queue <- .persistence_state$queue[-1]
  }
  job <- .persistence_state$active
  if (is.null(job)) { .persistence_state$scheduled <- FALSE; return(invisible(TRUE)) }
  options(futmondo.persistence_failures=job$failure_count %||% 0L,
    futmondo.persistence_issues=job$issues %||% list())
  if (identical(job$fn,"collect_account_observations")) {
    if (is.null(job$collection_state)) job$collection_state <- new_account_observation_collection(job$args[[1]],job$args[[2]],job$args[[3]])
    old_in_session <- getOption("futmondo.in_session_persistence", FALSE)
    old_stage <- getOption("futmondo.persistence_stage", "")
    options(futmondo.persistence_stage=job$collection_state$stage)
    on.exit(options(futmondo.persistence_stage=old_stage),add=TRUE)
    options(futmondo.in_session_persistence=TRUE)
    on.exit(options(futmondo.in_session_persistence=old_in_session), add=TRUE)
    state <- collect_account_observation_step(job$collection_state)
    options(futmondo.in_session_persistence=old_in_session)
    job$collection_state <- state
    job$failure_count <- getOption("futmondo.persistence_failures",0L)
    job$issues <- getOption("futmondo.persistence_issues",list())
    .persistence_state$progress[[job$owner]] <- state$message
    if (isTRUE(state$complete)) {
      issues <- getOption("futmondo.persistence_issues",list())
      if (!isTRUE(state$ok)) issues <- c(issues,list(list(table="collector",category="runtime",http_status=NA_real_,code="")))
      persistence_record_result(job,isTRUE(state$ok) && getOption("futmondo.persistence_failures",0L)==0L,issues)
      .persistence_state$progress[[job$owner]] <- NULL; .persistence_state$active <- NULL
    } else .persistence_state$active <- job
  } else {
    value <- tryCatch(do.call(job$fn,job$args),error=function(e) FALSE)
    http_failed <- identical(job$fn,"supabase_post_direct") &&
      !(is.numeric(value) && length(value)==1L && is.finite(value) && value>=200 && value<300)
    persistence_record_result(job,!http_failed && !identical(value,FALSE) && getOption("futmondo.persistence_failures",0L)==0L,getOption("futmondo.persistence_issues",list()))
    .persistence_state$active <- NULL
  }
  if (!is.null(.persistence_state$active) || length(.persistence_state$queue)) later::later(persistence_tick,0.05) else .persistence_state$scheduled <- FALSE
  invisible(TRUE)
}
