#!/usr/bin/env Rscript
# A separate capacity request still repairs slow source prerequisites.
# Only the passive worker snapshot, unrelated card data, HTTP and clock are fake.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
start_time<-as.POSIXct('2026-11-15 12:00:00',tz='UTC')
clock_now<-start_time;Sys.time<-function()clock_now
fm_worker_configured<-function()TRUE
fm_worker_mode<-function(id)!isTRUE(getOption('futmondo.worker_live',FALSE))
get_sb_url<-function()'';get_sb_key<-function()''
get_locker_clause_payments<-function(...)observed_data(data.frame())
record_acquired_resource<-function(...)invisible(TRUE)
# Since 76dec9b an opened card may ask for newer saved observations ('read');
# answer "nothing newer". Enqueueing acquisition or writing still fails.
fm_worker_rpc<-function(name,...){
 if(identical(name,'read'))return(promises::promise_resolve(list(account=list(enabled=TRUE,auth_status='active'),resources=list(),jobs=list())))
 stop('Card must not enqueue acquisition or write the database')
}
futmondo_post<-function(...)stop('No synchronous source request is allowed by this fixture')
writes<-0L;buy_clause<-function(...){writes<<-writes+1L;stop('Verification must not submit a transaction')}
auth<-c(userid='card-native-capacity-fixture',token='synthetic')
counts<-integer();pending<-list();requests<-character();live_calls<-0L;source_stamps<-list()
native_store<-resource_store
resource_store<-function(key,data,context,resource,ttl=NULL){
 value<-native_store(key,data,context,resource,ttl)
 if(resource%in%c('team_info_league_mine','roster_facts_league_mine','market_league_mine','pressroom_league'))
  source_stamps[[resource]]<<-c(source_stamps[[resource]],as.numeric(attr(value,'observed_at')))
 value
}
response<-function(url,body){
 route<-tail(strsplit(url,'/',fixed=TRUE)[[1]],1L);q<-jsonlite::fromJSON(body)$query
 answer<-switch(route,
  information=list(budget=1000,withheld=0,teamValue=2000,maxBid=2000,
   configuration=list(maxPlayersInRoster=20,mnmp=.5,mbmv=TRUE)),
  roster=list(list(id='owned',name='Owned',role='defensa',value=2000,change=0,points=5,teamId='club')),
  players=list(list(id='target',name='Target',role='defensa',value=1000,change=0,points=5,numberOfBids=0)),
  pressroom=if(q$from=='')list(news=list(list(`_id`='event',created='2026-11-14T12:00:00Z',
   `_player`=list(`_id`='owned'),`_buyer`=list(`_id`='mine'),`_seller`=list(`_id`='rival'),price=2000)))else list(news=list()),
  stop(paste('Unexpected native card fixture route',route)))
 list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=answer),auto_unbox=TRUE)),times=list(total=0))
}
fm_async_read_http<-function(url,method='POST',body=NULL,...){
 route<-tail(strsplit(url,'/',fixed=TRUE)[[1]],1L)
 if(is.na(counts[route]))counts[route]<<-0L
 counts[route]<<-counts[route]+1L;ordinal<-counts[route];requests<<-c(requests,route)
 if((route=='pressroom'&&ordinal==1L)||(route=='players'&&ordinal==1L))
  return(promises::promise(function(resolve,reject){
   pending[[paste(route,ordinal,sep=':')]]<<-function()resolve(response(url,body))
  }))
 promises::promise_resolve(response(url,body))
}
native_async<-async_adapter_fetch
async_adapter_fetch<-function(fn,args=list(),context,worker_bypass=FALSE,intent=NULL,...){
 # Since 76dec9b manual checks pass intent='manual_action' instead of worker_bypass.
 worker_bypass<-isTRUE(worker_bypass)||identical(intent,'manual_action')
 if(fn=='get_team_acquisition_capacity'){
  if(worker_bypass){
   live_calls<<-live_calls+1L
   return(native_async(fn,args,context,worker_bypass=TRUE,...))
  }
  return(promises::promise_resolve(observed_data(list(status='ok',
   valid_until=as.numeric(start_time)-30,roster=list(status='ok',count=1,effective_count=1,cap=20),
   funds=list(spendable_budget=2000),outstanding=list(count=0),rules=list(minimum_bid_at_value=TRUE)),
   status='stale',observed_at=start_time-45)))
 }
 if(fn%in%c('get_user_team_info','get_players_from_team','get_championship_pressroom','get_market_players'))
  return(native_async(fn,args,context,worker_bypass=worker_bypass,...))
 value<-switch(fn,get_player_summary=list(data=list(id='target'),championship=list(owner=list(computer=TRUE)),market=list(p=1000,bids=list()),points=list(),prices=list()),
  get_lineup_from_team=list(pointSystem='fixture'),get_player_full_profile=list(profile=list()),
  get_player_matches=list(matches=list()),read_card_round_history=data.frame(),read_card_price_history=data.frame(),stop(fn))
 promises::promise_resolve(value)
}
selected<-shiny::reactiveVal(data.frame(id='target',name='Konaté',role='defensa',team='Real Madrid',
 value=1000,change=0,points=67,user_team_id='',price=1000,market_inMarket=TRUE,
 # Table rows carry their observation time; the card trusts only recent facts (9a27d10).
 observed_at=format(start_time-5,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')))
settle<-function(session)for(i in 1:80){later::run_now(0);session$flushReact()}
button<-function(html,id){
 match<-regmatches(html,regexpr(paste0('<button\\b[^>]*\\bid="[^"]*',id,'"[^>]*>'),html,perl=TRUE))
 if(!length(match))return(NULL)
 match
}
enabled<-function(html,id){tag<-button(html,id);!is.null(tag)&&!grepl('\\bdisabled(?:=|\\s|>)',tag,perl=TRUE)}
clear_api_cache()
shiny::testServer(selected_player_Server,args=list(selected_player=selected,login_token=shiny::reactive(auth),
 championship_id=shiny::reactive('league'),user_team_id=shiny::reactive('mine'),
 capacity_fetcher=function(...)stop('Opening the verified modal must not run a synchronous preflight')) ,{
 settle(session);stopifnot(live_calls==0L,enabled(output$action_buttons$html,'btn_bid_market'))
 session$setInputs(btn_bid_market=1L);settle(session)
 stopifnot(live_calls==0L,isTRUE(offer_modal_opened_RV()),!length(requests))
 ctx <- resource_context(auth, 'league', 'mine')
 capacity <- NULL
 promises::then(async_adapter_fetch('get_team_acquisition_capacity',
  list(login=auth,championship_id='league',user_team_id='mine'),ctx,intent='manual_action'),
  function(value) capacity <<- value)
 settle(session)
 stopifnot(live_calls==1L,is.function(pending[['pressroom:1']]))
 clock_now<<-clock_now+31;pending[['pressroom:1']]();settle(session)
 stopifnot(is.function(pending[['players:1']]),is.null(capacity))
 session$setInputs(btn_verify_availability=2L);settle(session)
 stopifnot(live_calls==1L)
 clock_now<<-clock_now+20;pending[['players:1']]();settle(session)
 html<-output$action_buttons$html
 stopifnot(capacity$status=='ok',evaluate_acquisition_preflight(capacity,'bid',1000)$ok,
  enabled(html,'btn_bid_market'),!grepl('Availability checks expired',html,fixed=TRUE),
  live_calls==1L,writes==0L,counts['information']==2L,counts['players']==1L,
  counts['pressroom']==2L,counts['roster']==2L)
 cat('PASS editor entry makes no source requests and the separate capacity adapter repairs expired evidence during a 20s market response\n')

 epoch<-as.numeric(start_time)
 stopifnot(identical(source_stamps[['team_info_league_mine']]-epoch,c(31,51)),
  identical(source_stamps[['market_league_mine']]-epoch,51),
  identical(source_stamps[['roster_facts_league_mine']]-epoch,c(0,31)),
  identical(source_stamps[['pressroom_league']]-epoch,0),
  capacity$valid_until==epoch+61,capacity$valid_until-as.numeric(clock_now)==10)
 cat('PASS original source clocks remain unchanged; strict capacity deadline is the retained roster expiry, only 10s after completion\n')

 before<-requests;session$setInputs(btn_bid_market=2L);settle(session)
 stopifnot(offer_modal_opened_RV(),identical(requests,before),live_calls==1L,writes==0L)
 cat('PASS reopening the market editor adds zero source reads and zero transactions\n')
 before<-requests;row<-selected();selected(NULL);settle(session);selected(row);settle(session)
 stopifnot(identical(requests,before),live_calls==1L,writes==0L,
  enabled(output$action_buttons$html,'btn_bid_market'))
 cat('PASS reopening the card adds zero financial dispatches or additional source requests\n')
})
