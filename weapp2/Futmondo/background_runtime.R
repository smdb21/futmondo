# Process-isolated persistence; see docs/background_worker.md.
.persistence_state <- new.env(parent=emptyenv())
.persistence_state$queue <- list()
.persistence_state$batch <- list()
.persistence_state$process <- NULL
.persistence_state$scheduled <- FALSE
.persistence_state$errors <- list()

defer_persistence <- function(fn, args=list()) {
  if (isTRUE(getOption("futmondo.offline",FALSE))) return(invisible(TRUE))
  name <- if (is.character(fn)) fn else deparse(substitute(fn))
  if (length(name)!=1L || !exists(name,mode="function")) return(FALSE)
  domain <- shiny::getDefaultReactiveDomain()
  owner <- if (!is.null(domain)) domain$userData$futmondo_user_id else NULL
  if (is.null(owner) && length(args) && valid_login(args[[1]])) owner <- args[[1]][["userid"]]
  owner <- fm_scalar(owner,"system")
  if (length(.persistence_state$queue)>=200L) {
    .persistence_state$errors[[owner]] <- "Background queue full. Refresh to retry."
    return(FALSE)
  }
  key <- as.character(openssl::sha256(serialize(list(owner,name,args),NULL)))
  existing <- c(.persistence_state$queue,.persistence_state$batch)
  if (any(vapply(existing,function(x)identical(x$key,key),logical(1)))) return(invisible(TRUE))
  .persistence_state$queue[[length(.persistence_state$queue)+1L]] <- list(fn=name,args=args,owner=owner,key=key)
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
      good <- length(result)>=i && isTRUE(result[[i]])
      failures <- .persistence_state$errors[[owner]]
      if (!is.list(failures)) failures <- list()
      key <- .persistence_state$batch[[i]]$key
      failures[[key]] <- if (good) NULL else "Background sync incomplete. Check database setup, then refresh."
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
        options(futmondo.persistence_failures=0L)
        value <- tryCatch(do.call(job$fn,job$args),error=function(e)FALSE)
        !identical(value,FALSE) && getOption("futmondo.persistence_failures",0L)==0L
      })
    },args=list(root=normalizePath("."),batch=batch),supervise=TRUE),error=function(e)NULL)
    if (is.null(.persistence_state$process)) {
      for (job in batch) .persistence_state$errors[[job$owner]] <- "Background process could not start. Refresh to retry."
      .persistence_state$batch <- list()
    }
  }
  if (!is.null(.persistence_state$process) || length(.persistence_state$queue)) later::later(persistence_tick,0.5)
  else .persistence_state$scheduled <- FALSE
  invisible(TRUE)
}
