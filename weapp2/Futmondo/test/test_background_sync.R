#!/usr/bin/env Rscript
# Synthetic jobs and HTTP responses only; no database or Futmondo requests.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R')})
n <- 0L
check <- function(name, code) { force(code); n <<- n+1L; cat('PASS', name, '\n') }
reset <- function() {
  .persistence_state$queue <- list(); .persistence_state$batch <- list()
  .persistence_state$process <- NULL; .persistence_state$errors <- list()
  .persistence_state$scheduled <- FALSE
  options(futmondo.persistence_failures=0L, futmondo.persistence_issues=list())
}
finish <- function(jobs, results) {
  .persistence_state$batch <- jobs
  .persistence_state$process <- list(is_alive=function()FALSE,get_result=function()results)
  persistence_tick()
}
job <- function(fn, args, owner='u', key='payload')
  list(fn=fn,args=args,owner=owner,key=key,error_key=persistence_job_scope(fn,args))
check('retry identity ignores changed observations and token but isolates leagues', {
  a <- list(c(userid='u',token='old'),'league-a','team-a')
  b <- a; b[[1]][['token']] <- 'fresh'
  c <- b; c[[2]] <- 'league-b'
  stopifnot(identical(persistence_job_scope('collect_account_observations',a),persistence_job_scope('collect_account_observations',b)),
    !identical(persistence_job_scope('collect_account_observations',a),persistence_job_scope('collect_account_observations',c)))
  old <- list('player_history',data.frame(player_id='p',championship_id='a',observed_at='old',value=100))
  fresh <- list('player_history',data.frame(player_id='p',championship_id='a',observed_at='new',value=200))
  stopifnot(identical(persistence_job_scope('supabase_post_direct',old),persistence_job_scope('supabase_post_direct',fresh)))
})
check('successful refreshed job clears its old failure only', {
  reset()
  a <- job('log_player_history',list(data.frame(value=100),'league-a'),key='old')
  b <- job('log_player_history',list(data.frame(value=100),'league-b'),key='other')
  finish(list(a,b),list(FALSE,FALSE))
  stopifnot(length(.persistence_state$errors$u)==2L,is.null(background_sync_status('other-user')$error))
  fresh <- job('log_player_history',list(data.frame(value=200),'league-a'),key='new')
  finish(list(fresh),list(list(ok=TRUE,issues=list())))
  stopifnot(length(.persistence_state$errors$u)==1L,nzchar(background_sync_status('u')$error))
  finish(list(b),list(TRUE))
  stopifnot(is.null(background_sync_status('u')$error))
})
check('unrelated direct-write entities cannot clear a failed auction', {
  reset()
  a <- job('supabase_post_direct',list('auction_observations',list(championship_id='league-a',auction_id='a',observed_at='old')))
  b <- job('supabase_post_direct',list('auction_observations',list(championship_id='league-a',auction_id='b',observed_at='new')))
  stopifnot(!identical(a$error_key,b$error_key))
  finish(list(a),list(FALSE))
  finish(list(b),list(TRUE))
  stopifnot(length(.persistence_state$errors$u)==1L,nzchar(background_sync_status('u')$error))
  retry <- job('supabase_post_direct',list('auction_observations',list(championship_id='league-a',auction_id='a',observed_at='new')))
  finish(list(retry),list(TRUE))
  stopifnot(is.null(background_sync_status('u')$error))
})
check('direct-write row lists preserve account league and entity identities', {
  scope <- function(rows) persistence_job_scope('supabase_post_direct',list('sale_observations',rows))
  a <- list(user_id='u',championship_id='league-a',user_team_id='team-a',offer_id='offer-a',observed_at='old')
  b <- a; b$championship_id <- 'league-b'
  c <- a; c$offer_id <- 'offer-b'
  d <- a; d$user_id <- 'other'
  e <- a; e$user_team_id <- 'team-b'
  stopifnot(!identical(scope(list(a)),scope(list(b))),!identical(scope(list(a)),scope(list(c))),
    !identical(scope(list(a)),scope(list(d))),!identical(scope(list(a)),scope(list(e))),
    identical(scope(a),scope(list(a))),identical(scope(a),scope(as.data.frame(a))),
    identical(scope(list(a,b)),scope(list(b,a))))
})
check('compound row keys do not collapse different entity pairings', {
  scope <- function(rows) persistence_job_scope('supabase_post_direct',list('auction_observations',rows))
  a <- data.frame(championship_id=c('a','b'),auction_id=c('one','two'))
  b <- a; b$auction_id <- rev(b$auction_id)
  stopifnot(!identical(scope(a),scope(b)),identical(scope(a),scope(a[2:1,,drop=FALSE])))
})
check('standard IDs explicit conflict keys and conservative fallback stay scoped', {
  scope <- function(table,row,conflict=NULL) persistence_job_scope('supabase_post_direct',list(table,row,conflict))
  stopifnot(!identical(scope('players',list(id='p1')),scope('players',list(id='p2'))),
    identical(scope('players',list(id='p',name='old')),scope('players',list(id='p',name='new'))),
    !identical(scope('manager_dna_profiles',list(team_id='a')),scope('manager_dna_profiles',list(team_id='b'))),
    identical(scope('custom_records',list(entity='a',observed_at='old',value=100),'entity,observed_at'),
      scope('custom_records',list(entity='a',observed_at='new',value=200),'entity,observed_at')),
    !identical(scope('decision_log',list(player_id='a',recommended_value=100)),
      scope('decision_log',list(player_id='a',recommended_value=200))),
    !identical(scope('player_history',list(championship_id='a',value=100)),
      scope('player_history',list(championship_id='a',value=200))))
})
check('retry identity retains non-observation timestamp keys', {
  scope <- function(day) persistence_job_scope('supabase_post_direct',
    list('player_daily_snapshots',list(player_id='p',championship_id='a',snapshot_date=day)))
  stopifnot(!identical(scope('2026-09-07'),scope('2026-09-08')))
})
check('rejected enqueue remains visible until the matching retry succeeds', {
  reset()
  tryCatch({
    options(futmondo.offline=FALSE)
    .persistence_state$scheduled <- TRUE
    .persistence_state$queue <- rep(list(job('supabase_post_direct',list('players',list(id='occupied')),owner='system',key='occupied')),200L)
    rejected <- list('auction_observations',list(championship_id='league',auction_id='rejected',observed_at='old'))
    stopifnot(identical(defer_persistence('supabase_post_direct',rejected),FALSE),
      grepl('queue full',background_sync_status('system')$error,fixed=TRUE))
    .persistence_state$queue <- list()
    unrelated <- list('auction_observations',list(championship_id='league',auction_id='other',observed_at='new'))
    stopifnot(isTRUE(defer_persistence('supabase_post_direct',unrelated)),
      grepl('queue full',background_sync_status('system')$error,fixed=TRUE))
    queued <- .persistence_state$queue; .persistence_state$queue <- list()
    finish(queued,list(TRUE))
    stopifnot(grepl('queue full',background_sync_status('system')$error,fixed=TRUE))
    retry <- rejected; retry[[2]]$observed_at <- 'new'
    .persistence_state$scheduled <- TRUE
    stopifnot(isTRUE(defer_persistence('supabase_post_direct',retry)),
      grepl('queue full',background_sync_status('system')$error,fixed=TRUE))
    queued <- .persistence_state$queue; .persistence_state$queue <- list()
    finish(queued,list(TRUE))
    stopifnot(is.null(background_sync_status('system')$error))
  },finally=options(futmondo.offline=TRUE))
})
check('unacknowledged process results remain failures', {
  reset(); a <- job('sync_players_to_supabase',list(data.frame(id='p')))
  finish(list(a),NULL)
  stopifnot(nzchar(background_sync_status('u')$error))
})
check('worker categories give actionable messages without private details', {
  reset(); a <- job('sync_players_to_supabase',list(data.frame(id='p')))
  finish(list(a),list(list(ok=FALSE,issues=list(list(category='schema',table='player_history',code='PGRST204')))))
  stopifnot(grepl('database needs an update',background_sync_status('u')$error,fixed=TRUE),
    !grepl('player_history',background_sync_status('u')$error,fixed=TRUE))
  stopifnot(grepl('connection failed',persistence_failure_message(list(list(category='connection'))),fixed=TRUE),
    grepl('not configured',persistence_failure_message(list(list(category='configuration'))),fixed=TRUE),
    grepl('access was denied',persistence_failure_message(list(list(category='authorization'))),fixed=TRUE))
})
# Intercept the unqualified POST used by supabase_post_direct before enabling it.
requests <- 0L
response_status <- 400L
response_body <- '{"code":"PGRST204","message":"private row/token must never be logged"}'
POST <- function(...) {
  requests <<- requests+1L
  structure(list(status_code=response_status,url='https://offline.invalid',headers=list('content-type'='application/json'),
    content=charToRaw(response_body)),class='response')
}
get_sb_url <- function() 'https://offline.invalid'
get_sb_key <- function() 'synthetic-key'
options(futmondo.offline=FALSE)
check('write failures carry only sanitized table and response codes', {
  reset(); messages <- character()
  value <- withCallingHandlers(supabase_post_direct('player_history',list(value=123)),message=function(m) {
    messages <<- c(messages,conditionMessage(m));invokeRestart('muffleMessage')
  })
  issues <- getOption('futmondo.persistence_issues')
  stopifnot(value==400L,requests==1L,getOption('futmondo.persistence_failures')==1L,
    issues[[1]]$category=='schema',issues[[1]]$code=='PGRST204',
    !any(grepl('private row/token',messages,fixed=TRUE)))
})
check('HTTP authorization transient and constraint failures are distinguished', {
  for (case in list(list(403L,'42501','authorization'),list(503L,'PGRST000','connection'),list(409L,'23503','write'))) {
    reset(); response_status <- case[[1]]
    response_body <- jsonlite::toJSON(list(code=case[[2]]),auto_unbox=TRUE)
    suppressMessages(supabase_post_direct('players',list(id='p')))
    stopifnot(getOption('futmondo.persistence_issues')[[1]]$category==case[[3]])
  }
})
check('successful writes do not report persistence failure', {
  reset(); response_status <- 201L; response_body <- ''
  invisible(capture.output(value <- supabase_post_direct('players',list(id='p'))))
  stopifnot(value==201L,getOption('futmondo.persistence_failures')==0L,length(getOption('futmondo.persistence_issues'))==0L)
})
check('empty writes are harmless even without optional configuration', {
  reset(); get_sb_key <- function() ''; before <- requests
  supabase_post_direct('players',data.frame())
  stopifnot(requests==before,getOption('futmondo.persistence_failures')==0L)
  suppressMessages(supabase_post_direct('players',list(id='p')))
  stopifnot(requests==before,getOption('futmondo.persistence_issues')[[1]]$category=='configuration')
})
check('connection exceptions are sanitized', {
  reset(); get_sb_key <- function() 'synthetic-key'
  POST <- function(...) stop('private transport detail')
  messages <- character()
  value <- withCallingHandlers(supabase_post_direct('players',list(id='p')),message=function(m) {
    messages <<- c(messages,conditionMessage(m));invokeRestart('muffleMessage')
  })
  stopifnot(is.null(value),getOption('futmondo.persistence_issues')[[1]]$category=='connection',
    !any(grepl('private transport detail',messages,fixed=TRUE)))
})
check('isolated worker returns structured failures and resets diagnostics per job', {
  reset()
  .persistence_state$queue <- list(
    job('record_persistence_failure',list('player_history',400L,'PGRST204'),key='failure'),
    job('sync_players_to_supabase',list(data.frame()),key='empty'))
  .persistence_state$scheduled <- TRUE
  persistence_tick()
  child <- .persistence_state$process
  stopifnot(!is.null(child))
  child$wait(timeout=30000)
  if (child$is_alive()) { child$kill(); stop('Offline worker did not finish') }
  result <- child$get_result()
  stopifnot(length(result)==2L,!result[[1]]$ok,result[[1]]$issues[[1]]$category=='schema',
    result[[2]]$ok,length(result[[2]]$issues)==0L)
  persistence_tick()
  stopifnot(grepl('database needs an update',background_sync_status('u')$error,fixed=TRUE),
    background_sync_status('u')$pending==0L)
})
reset()
cat(sprintf('BACKGROUND SYNC: %d passed / 0 failed\n',n))
