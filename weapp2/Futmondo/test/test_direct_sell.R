#!/usr/bin/env Rscript
# The HAR supplies endpoint/schema evidence only. No captured credentials are used.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');library(shiny)})
source('Modules/Selected_Player_Module.R')
n <- 0L
check <- function(name,expr) {force(expr);n <<- n+1L;cat('PASS',name,'\n')}
auth <- c(token='synthetic-token',userid='synthetic-user')
entries <- jsonlite::fromJSON('app.futmondo.com.direct_sel.har',simplifyVector=FALSE)$log$entries
sale <- Filter(function(e) identical(e$request$method,'POST') && identical(e$request$url,DIRECT_SELL_URL),entries)
check('payload matches the captured endpoint contract without price or offer ID', {
  stopifnot(length(sale)==1L)
  captured <- jsonlite::fromJSON(sale[[1]]$request$postData$text,simplifyVector=FALSE)
  expected <- captured
  expected$header <- list(token=auth[['token']],userid=auth[['userid']])
  expected$query <- list(championshipId='c',userteamId='t',player_id='p')
  payload <- build_direct_sell_payload(auth,'c','t','p')
  stopifnot(identical(names(payload$query),names(captured$query)),
    identical(jsonlite::toJSON(payload,auto_unbox=TRUE),jsonlite::toJSON(expected,auto_unbox=TRUE)),
    jsonlite::fromJSON(sale[[1]]$response$content$text)$answer$code==API_CODE_OK)
})
summary <- list(data=list(id='p',value=1000),championship=list(owner=list(`_id`='t')),market=list(bids=list(),direct=list()))
get_player_summary <- function(...) summary
calls <- 0L
response <- function(code=API_CODE_OK,status=200L) structure(list(status_code=status,
  headers=list('Content-Type'='application/json'),content=charToRaw(jsonlite::toJSON(list(answer=list(code=code)),auto_unbox=TRUE)),url=DIRECT_SELL_URL),class='response')
futmondo_post <- function(url,body,...) {calls <<- calls+1L;stopifnot(identical(url,DIRECT_SELL_URL));response()}
check('owned unlisted player can be sold with no bid and fresh verification', {
  api_cache_env[[api_cache_key('player_summary_synthetic-user_c_t_p',auth)]] <- list(data=summary,time=Sys.time())
  result <- direct_sell_player(auth,'c','t','p')
  stopifnot(result$success,!result$uncertain,calls==1L,
    !exists(api_cache_key('player_summary_synthetic-user_c_t_p',auth),api_cache_env,inherits=FALSE))
})
check('rival free-agent listed stale and mismatched players cannot be sold', {
  original <- summary
  for(owner in c('rival','')) {
    summary$championship$owner$`_id` <- owner
    stopifnot(!direct_sell_player(auth,'c','t','p')$success,calls==1L)
  }
  summary <- original;summary$market$p <- 1000
  stopifnot(direct_sell_player(auth,'c','t','p')$code=='listed',calls==1L)
  summary <- original;attr(summary,'fetch_status') <- 'stale'
  stopifnot(direct_sell_player(auth,'c','t','p')$code=='unavailable',calls==1L)
  summary <- original;summary$data$id <- 'other'
  stopifnot(!direct_sell_player(auth,'c','t','p')$success,calls==1L)
  summary <- original;summary$market <- list()
  stopifnot(direct_sell_player(auth,'c','t','p')$code=='unavailable',calls==1L)
  summary <- original
  stopifnot(direct_sale_preflight(auth,'c','', 'p')$code=='invalid_context')
})
check('ownership is rechecked after confirmation and API rejection is not success', {
  stopifnot(direct_sale_preflight(auth,'c','t','p')$ok)
  summary$championship$owner$`_id` <- 'rival'
  stopifnot(!direct_sell_player(auth,'c','t','p')$success,calls==1L)
  summary$championship$owner$`_id` <- 't'
  futmondo_post <- function(...) response('api.error.invalid')
  result <- direct_sell_player(auth,'c','t','p')
  stopifnot(!result$success,!result$uncertain,result$code=='api.error.invalid')
  futmondo_post <- function(...) response(API_CODE_OK,500L)
  result <- direct_sell_player(auth,'c','t','p')
  stopifnot(!result$success,result$uncertain)
})
check('unconfirmed transport result does not retry or claim a sale', {
  attempts <- 0L
  futmondo_post <- function(...) {attempts <<- attempts+1L;stop('transport timeout')}
  result <- direct_sell_player(auth,'c','t','p')
  stopifnot(!result$success,result$uncertain,attempts==1L,grepl('Refresh',result$message))
})
find_handler <- function(x,event) {
  if(is.call(x) && identical(x[[1]],as.name('observeEvent')) && identical(paste(deparse(x[[2]]),collapse=''),event)) return(x[[3]])
  if(is.call(x) || is.expression(x) || is.pairlist(x)) for(y in as.list(x)) {
    if(missing(y)) next
    found <- find_handler(y,event);if(!is.null(found)) return(found)
  }
  NULL
}
check('confirmation is one-use and success refreshes via explicit sale event', {
  handler <- find_handler(parse('Modules/Selected_Player_Module.R'),'input$submit_direct_sell')
  env <- new.env(parent=globalenv());allowed <- TRUE;writes <- 0L;events <- list()
  env$consume_action_context <- function(action) {ok<-allowed && action=='direct_sell';allowed <<- FALSE;ok}
  env$selected_player <- function() data.frame(id='p',user_team_id='t')
  env$login_token <- auth;env$championship_id <- 'c';env$user_team_id <- 't';env$get_reactive_val <- identity
  env$direct_sell_player <- function(...) {writes <<- writes+1L;list(success=TRUE,uncertain=FALSE,message='Sold')}
  env$removeModal <- function(...) NULL;env$showNotification <- function(...) NULL;env$clear_api_cache <- function(...) NULL
  env$on_bid_updated <- function(...) events[[length(events)+1L]] <<- list(...)
  session <- MockShinySession$new()
  withReactiveDomain(session,eval(handler,env))
  tryCatch(withReactiveDomain(session,eval(handler,env)),shiny.silent.error=function(e)NULL)
  stopifnot(writes==1L,length(events)==1L,events[[1]]$action_type=='player_sold_direct',events[[1]]$player_id=='p')
  session$close()
})
cat(sprintf('DIRECT SELL: %d passed / 0 failed\n',n))
