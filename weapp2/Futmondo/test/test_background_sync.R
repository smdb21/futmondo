#!/usr/bin/env Rscript
# In-session queue tests: no HTTP requests or database writes.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R')})
n <- 0L
check <- function(name, code) { force(code); n <<- n + 1L; cat('PASS', name, '\n') }
reset <- function() {
  .persistence_state$queue <- list(); .persistence_state$active <- NULL
  .persistence_state$progress <- list(); .persistence_state$errors <- list(); .persistence_state$scheduled <- FALSE
  options(futmondo.persistence_failures=0L, futmondo.persistence_issues=list())
}

check('retry identity ignores refreshed token and isolates league/team', {
  a <- list(c(userid='u',token='old'),'league-a','team-a')
  b <- a; b[[1]][['token']] <- 'fresh'
  c <- b; c[[2]] <- 'league-b'
  stopifnot(identical(persistence_job_scope('collect_account_observations',a),persistence_job_scope('collect_account_observations',b)),
    !identical(persistence_job_scope('collect_account_observations',a),persistence_job_scope('collect_account_observations',c)))
})
check('direct-write scopes retain stable entity identities', {
  a <- list('auction_observations',list(championship_id='league',auction_id='a',observed_at='old'))
  b <- list('auction_observations',list(championship_id='league',auction_id='b',observed_at='new'))
  stopifnot(!identical(persistence_job_scope('supabase_post_direct',a),persistence_job_scope('supabase_post_direct',b)))
})
check('status is isolated by account and reports active collection progress', {
  reset()
  .persistence_state$active <- list(owner='u',key='k',fn='collect_account_observations')
  .persistence_state$progress$u <- 'Saving player observations (1/30)'
  .persistence_state$errors$v <- list(x='other account failure')
  status <- background_sync_status('u')
  stopifnot(status$pending==1L, grepl('1/30',status$progress,fixed=TRUE), is.null(status$error),
    is.null(background_sync_status('v')$progress))
})
check('runtime failure gives a resumable, specific user message', {
  message <- persistence_failure_message(list(list(category='runtime')))
  stopifnot(grepl('resume from the last saved player',message,fixed=TRUE))
})
check('queue deduplicates an active collection request', {
  reset(); options(futmondo.offline=FALSE)
  on.exit(options(futmondo.offline=TRUE),add=TRUE)
  args <- list(c(userid='u',token='t'),'league','team')
  key <- as.character(openssl::sha256(serialize(list('u','collect_account_observations',args),NULL)))
  .persistence_state$active <- list(owner='u',key=key,fn='collect_account_observations',args=args)
  stopifnot(isTRUE(defer_persistence('collect_account_observations',args)),length(.persistence_state$queue)==0L)
})
cat(sprintf('BACKGROUND SYNC: %d passed / 0 failed\n',n))
