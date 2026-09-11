# Process-isolated persistence; see docs/background_worker.md.
.persistence_state <- new.env(parent=emptyenv())
.persistence_state$queue <- list()
.persistence_state$batch <- list()
.persistence_state$process <- NULL
.persistence_state$scheduled <- FALSE
.persistence_state$errors <- list()

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
  if ("configuration" %in% categories)
    return("History saving is unavailable because the database is not configured. Live Futmondo data is still available.")
  if ("authorization" %in% categories)
    return("History could not be saved because database access was denied. Contact the app administrator.")
  if ("schema" %in% categories)
    return("History could not be saved because the database needs an update. Contact the app administrator.")
  if ("connection" %in% categories)
    return("History could not be saved because the database connection failed. Use Refresh to retry.")
  "Some history could not be saved. Use Refresh to retry; if this continues, contact the app administrator."
}

defer_persistence <- function(fn, args=list()) {
  if (isTRUE(getOption("futmondo.offline",FALSE))) return(invisible(TRUE))
  name <- if (is.character(fn)) fn else deparse(substitute(fn))
  if (length(name)!=1L || !exists(name,mode="function")) return(FALSE)
  domain <- shiny::getDefaultReactiveDomain()
  owner <- if (!is.null(domain)) domain$userData$futmondo_user_id else NULL
  if (is.null(owner) && length(args) && valid_login(args[[1]])) owner <- args[[1]][["userid"]]
  owner <- fm_scalar(owner,"system")
  key <- as.character(openssl::sha256(serialize(list(owner,name,args),NULL)))
  error_key <- persistence_job_scope(name,args)
  existing <- c(.persistence_state$queue,.persistence_state$batch)
  if (any(vapply(existing,function(x)identical(x$key,key),logical(1)))) return(invisible(TRUE))
  if (length(.persistence_state$queue)>=200L) {
    failures <- .persistence_state$errors[[owner]]
    if (!is.list(failures)) failures <- list()
    failures[[error_key]] <- "Background queue full. Use Refresh to retry."
    .persistence_state$errors[[owner]] <- failures
    return(FALSE)
  }
  .persistence_state$queue[[length(.persistence_state$queue)+1L]] <- list(fn=name,args=args,owner=owner,key=key,
    error_key=error_key)
  if (!isTRUE(.persistence_state$scheduled)) {
    .persistence_state$scheduled <- TRUE
    later::later(persistence_tick,0)
  }
  invisible(TRUE)
}

background_sync_status <- function(user_id) {
  owner <- fm_scalar(user_id,"")
  jobs <- c(.persistence_state$queue,.persistence_state$batch)
  pending <- sum(vapply(jobs,function(x)identical(x$owner,owner),logical(1)))
  errors <- unlist(.persistence_state$errors[[owner]],use.names=FALSE)
  list(pending=pending,error=if(length(errors)) paste(unique(errors),collapse=' ') else NULL)
}

persistence_tick <- function() {
  p <- .persistence_state$process
  if (!is.null(p) && !p$is_alive()) {
    result <- tryCatch(p$get_result(),error=function(e)NULL)
    for (i in seq_along(.persistence_state$batch)) {
      owner <- .persistence_state$batch[[i]]$owner
      item <- if (length(result) >= i) result[[i]] else NULL
      good <- isTRUE(item) || (is.list(item) && isTRUE(item$ok))
      failures <- .persistence_state$errors[[owner]]
      if (!is.list(failures)) failures <- list()
      key <- .persistence_state$batch[[i]]$error_key %||% .persistence_state$batch[[i]]$key
      issues <- if (is.list(item)) item$issues else list()
      failures[[key]] <- if (good) NULL else persistence_failure_message(issues)
      if (!good) {
        message("[Persistence] Incomplete job: ", .persistence_state$batch[[i]]$fn %||% "unknown")
        for (issue in issues) message("[Persistence] table=", issue$table, " category=", issue$category,
          if (length(issue$http_status) == 1L && is.finite(issue$http_status)) paste0(" HTTP=", issue$http_status),
          if (length(issue$code) == 1L && !is.na(issue$code) && nzchar(issue$code)) paste0(" code=", issue$code))
      }
      .persistence_state$errors[[owner]] <- failures
    }
    .persistence_state$batch <- list(); .persistence_state$process <- NULL
  }
  if (is.null(.persistence_state$process) && length(.persistence_state$queue)) {
    batch <- head(.persistence_state$queue,10L)
    .persistence_state$queue <- tail(.persistence_state$queue,-length(batch))
    .persistence_state$batch <- batch
    .persistence_state$process <- tryCatch(callr::r_bg(function(root,batch) {
      setwd(root)
      if (file.exists(".Renviron")) readRenviron(".Renviron")
      source("futmondo_functions.R"); source("supabase_connector.R")
      source("Modules/Notifications_Module.R"); source("intelligence_engine.R"); source("prediction_engine.R")
      source("insights_runtime.R")
      source("portfolio_engine.R")
      lapply(batch,function(job) {
        options(futmondo.persistence_failures=0L, futmondo.persistence_issues=list())
        value <- tryCatch(do.call(job$fn,job$args),error=function(e) {
          # Never put raw exception messages, tokens or payloads in process results.
          message("[Persistence] Job failed: ",job$fn)
          FALSE
        })
        list(ok=!identical(value,FALSE) && getOption("futmondo.persistence_failures",0L)==0L,
          issues=getOption("futmondo.persistence_issues",list()))
      })
    },args=list(root=normalizePath("."),batch=batch),supervise=TRUE),error=function(e)NULL)
    if (is.null(.persistence_state$process)) {
      for (job in batch) {
        failures <- .persistence_state$errors[[job$owner]]
        if (!is.list(failures)) failures <- list()
        failures[[job$error_key %||% job$key]] <- "Background process could not start. Use Refresh to retry."
        .persistence_state$errors[[job$owner]] <- failures
      }
      .persistence_state$batch <- list()
    }
  }
  if (!is.null(.persistence_state$process) || length(.persistence_state$queue)) later::later(persistence_tick,0.5)
  else .persistence_state$scheduled <- FALSE
  invisible(TRUE)
}
