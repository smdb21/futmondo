#!/usr/bin/env Rscript
# The actual card must not finish a slow-history check with already-expired funds.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
clock_now<-as.POSIXct('2026-11-05 12:00:00',tz='UTC')
Sys.time<-function()clock_now
get_sb_url<-function()'';get_sb_key<-function()''
get_locker_clause_payments<-function(...)observed_data(data.frame())
record_acquired_resource<-function(...)invisible(TRUE)
auth<-c(userid='card-freshness-fixture',token='synthetic')
requests<-character();advanced<-FALSE;capacity_calls<-0L
response<-function(url,body){
 route<-tail(strsplit(url,'/',fixed=TRUE)[[1]],1L);q<-jsonlite::fromJSON(body)$query
 requests<<-c(requests,route)
 answer<-switch(route,
  information=list(budget=1000,withheld=0,teamValue=2000,maxBid=2000,
   configuration=list(maxPlayersInRoster=20,mnmp=.5,mbmv=TRUE)),
  roster=list(list(id='owned',name='Owned',role='defensa',value=2000,change=0,points=5,teamId='club')),
  players=list(list(id='target',name='Target',role='defensa',value=1000,change=0,points=5,numberOfBids=0)),
  pressroom={
   if(!advanced){clock_now<<-clock_now+31;advanced<<-TRUE}
   if(q$from=='')list(news=list(list(`_id`='transfer',created='2026-11-04T12:00:00Z',
    `_player`=list(`_id`='owned'),`_buyer`=list(`_id`='mine'),price=2000)))else list(news=list())
  },stop(paste('Unexpected source route',route)))
 list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=answer),auto_unbox=TRUE)),times=list(total=0))
}
pending_history<-NULL
fm_async_read_http<-function(url,method='POST',body=NULL,...){
 if(grepl('/pressroom$',url)&&!advanced)return(promises::promise(function(resolve,reject){
  pending_history<<-function()resolve(response(url,body))
 }))
 promises::promise_resolve(response(url,body))
}
real_async<-async_adapter_fetch
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE,...){
 if(fn=='get_team_acquisition_capacity')capacity_calls<<-capacity_calls+1L
 if(fn%in%c('get_team_acquisition_capacity','get_user_team_info','get_players_from_team','get_championship_pressroom','get_market_players'))
  return(real_async(fn,args,context,owner,is_current,...))
 value<-switch(fn,
  get_player_summary=observed_data(list(championship=list(),points=list(list(round=1,points=0),list(round=2,points=-2)),prices=list())),
  get_lineup_from_team=list(pointSystem='fixture'),get_player_full_profile=list(profile=list()),
  get_player_matches=list(matches=list()),read_card_round_history=data.frame(),read_card_price_history=data.frame(),
  stop(paste('Unexpected card resource',fn)))
 promises::promise_resolve(value)
}
settle<-function(session){for(i in seq_len(60L)){later::run_now(0);session$flushReact()}}
selected<-shiny::reactiveVal(data.frame(id='target',name='Berenguer',role='midfielder',team='Club',
 value=1000,change=0,points=2,user_team_id='rival',price=NA_real_,market_inMarket=FALSE,
 clause_price=1200,clause_transferred=FALSE,clause_date=NA_character_))
clear_api_cache()
shiny::testServer(selected_player_Server,args=list(selected_player=selected,login_token=shiny::reactive(auth),
 championship_id=shiny::reactive('league'),user_team_id=shiny::reactive('mine')) ,{
 settle(session)
 stopifnot(is.function(pending_history),!advanced)
 pending_history();settle(session)
 state<-card_states$capacity();html<-output$action_buttons$html
 stopifnot(!identical(state$status,'loading'),advanced)
  stopifnot(capacity_calls==1L,identical(state$data$status,'ok'),evaluate_acquisition_preflight(state$data,'offer')$ok,
   !grepl('Checking availability|checks expired',html),!grepl('disabled',html,fixed=TRUE),
   state$data$valid_until>as.numeric(clock_now))
  cat('PASS actual card renders verified actions after 31s history with fresh source timestamps.\n')
  before<-requests;previous<-selected();selected(NULL);settle(session);selected(previous);settle(session)
  stopifnot(identical(requests,before),evaluate_acquisition_preflight(card_states$capacity()$data,'offer')$ok)
  cat('PASS reopening the fresh card adds zero requests.\n')
 cat('READS:',paste(names(table(requests)),as.integer(table(requests)),sep='=',collapse=', '),'\n')
 stopifnot(all(table(requests)[c('information','roster','players','pressroom')]==2L))
})
