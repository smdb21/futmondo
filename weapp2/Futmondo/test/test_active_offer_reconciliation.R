options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
at <- fm_time('2026-11-23T06:27:27Z')
market <- data.frame(id='player',bid_id='own-bid',bid_price=38016571)
job <- list(id='job',kind='manual',mode='bid',player_id='player',player_name='Fixture',
  max_price='38016571',reserved_amount=38016571,reserved_slot=1,
  dispatched_at=fm_clause_stamp(at),state='accepted',result_code='api.general.ok')
match_offer <- function(rows=market, balance_at=at+2, market_at=at+1)
  fm_clause_active_offer_matches(list(job),rows,balance_at,market_at)
stopifnot(identical(match_offer(),list(list(job_id='job',player_id='player',bid_id='own-bid',price=38016571))),
  !length(match_offer(balance_at=at)),!length(match_offer(market_at=at)),!length(match_offer(market_at=at+NA)),
  !length(match_offer(transform(market,bid_id=NA_character_))),!length(match_offer(transform(market,bid_price=1))),
  !length(match_offer(transform(market,id='other'))),!length(match_offer(rbind(market,market))),
  !length(match_offer(rbind(market,transform(market,id='other')))))
accepted <- as.character(fm_clause_pending_ui(list(job)))
job$state <- 'uncertain'
uncertain <- as.character(fm_clause_pending_ui(list(job)))
stopifnot(grepl('Accepted offers awaiting balance update: 38.016.571 EUR (1)',accepted,fixed=TRUE),
  !grepl('Scheduled clause purchases',accepted,fixed=TRUE),
  !grepl('Offers awaiting confirmation',accepted,fixed=TRUE),
  grepl('Offers awaiting confirmation: 38.016.571 EUR (1)',uncertain,fixed=TRUE))
cat('PASS exact saved own offer, source clocks, ambiguous rows and acceptance wording\n')

# A snapshot may change while the database checks the submitted versions.
# A no-op write must let the observer try the latest data without a source read.
ctx <- structure(list(account_id='fixture',championship_id='league',user_team_id='mine',
  player_id='',competition_id='competition',season='season',scoring_version='score'),class='futmondo_context')
auth <- c(userid='fixture',token='fixture')
job$context <- modifyList(ctx,list(player_id=job$player_id))
fm_clause_enabled <- function() TRUE
fm_worker_mode <- function(...) TRUE
resource_context <- function(...) ctx
fm_worker_descriptor <- function(handler,...) list(resource_key=handler)
worker_state <- function(data, stamp) {
  attr(data,'worker_acquisition_status') <- 'ok'
  list(data=data,observed_at=stamp,context=ctx,status='ok')
}
market_rv <- shiny::reactiveVal(worker_state(market,at+1))
balance_rv <- shiny::reactiveVal(worker_state(list(budget=100000000),at+2))
fm_worker_get <- function(account,key) {
  value <- if(key=='get_market_players') market_rv() else balance_rv()
  list(manifest=list(resource_key=key,version=as.character(as.numeric(value$observed_at)),observed_at=value$observed_at))
}
account <- fm_clause_account('fixture')
account$known <- TRUE
account$jobs <- list(job)
writes <- list()
resolver <- NULL
fm_clause_rpc <- function(name,payload,...) {
  stopifnot(name=='reconcile_active_offers')
  writes[[length(writes)+1L]] <<- payload
  promises::promise(function(resolve,reject) resolver <<- resolve)
}
fm_clause_refresh <- function(...) {
  account$jobs <- list()
  account$revision(shiny::isolate(account$revision())+1L)
}
shiny::testServer(function(input,output,session) {
  fm_clause_reconcile_saved_offers(session,function()auth,function()ctx$championship_id,
    function()ctx$user_team_id,function()stop('History should not be needed for a matching bid'),balance_rv,market_rv)
}, {
  flush <- function() for(i in seq_len(5)) {later::run_now(0); session$flushReact()}
  flush()
  stopifnot(length(writes)==1L)
  market_rv(worker_state(market,at+3))
  flush()
  stopifnot(length(writes)==1L)
  resolver(0)
  flush()
  stopifnot(length(writes)==2L,writes[[1]]$p_market_version!=writes[[2]]$p_market_version)
  resolver(1)
  flush()
  stopifnot(length(writes)==2L,!length(account$jobs))
})
cat('PASS saved offer observer coalesces writes and retries the latest persisted version after a no-op\n')

clause <- modifyList(job,list(id='clause',mode='clause',state='accepted',result_code=API_CODE_OK,
 original_owner='rival',actual_price=91657534,reserved_amount=91657534))
account$jobs <- list(clause)
account$saved_offers_attempted <- NULL
writes <- list()
history <- worker_state(data.frame(id='official',player_id=clause$player_id,
 buyer_team_id='mine',seller_team_id='rival',price=91657534,created=fm_clause_stamp(at+1),
 is_clause=TRUE),at+2)
history_rv <- shiny::reactiveVal(history)
original_get <- fm_worker_get
fm_worker_get <- function(account,key) {
 if(key!='get_championship_pressroom')return(original_get(account,key))
 value <- history_rv()
 list(manifest=list(resource_key=key,version='saved-history',observed_at=value$observed_at))
}
fm_clause_rpc <- function(name,payload,...) {
 stopifnot(name=='reconcile_completed_bids')
 writes[[length(writes)+1L]] <<- payload
 promises::promise(function(resolve,reject) resolver <<- resolve)
}
shiny::testServer(function(input,output,session) {
 fm_clause_reconcile_saved_offers(session,function()auth,function()ctx$championship_id,
  function()ctx$user_team_id,history_rv,balance_rv,function()stop('Clause settlement needs no active offers read'))
}, {
 flush <- function() for(i in seq_len(5)) {later::run_now(0); session$flushReact()}
 flush()
 stopifnot(length(writes)==1L,writes[[1]]$p_history_version=='saved-history',
  writes[[1]]$p_matches[[1]]$job_id=='clause',isTRUE(writes[[1]]$p_matches[[1]]$is_clause),
  fm_clause_reservations(account$jobs)$count==1)
 resolver(1)
 flush()
 stopifnot(length(writes)==1L,fm_clause_reservations(account$jobs)$count==0)
})
cat('PASS paid manual clause reuses saved history and balance to clear the cash and squad hold\n')
