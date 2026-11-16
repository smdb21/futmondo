#!/usr/bin/env Rscript
# Explicit editor demand, dormant reuse and final preflight source deadlines.
# All reads are synthetic; no account, network, database write or purchase.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
clock<-as.POSIXct('2026-11-15 12:00:00',tz='UTC');Sys.time<-function()clock
get_sb_url<-function()'';get_sb_key<-function()''
fm_worker_configured<-function()TRUE;fm_worker_mode<-function(...)TRUE
fm_worker_resolve_context<-function(handler,args,context)context
record_acquired_resource<-function(...)invisible(TRUE)
auth<-c(userid='availability-window-fixture',token='synthetic')
saved<-shiny::reactiveVal(NULL);jobs<-list()
capacity<-function(seconds=15,status='ok')observed_data(list(status=status,valid_until=as.numeric(clock)+seconds,
 roster=list(status='ok',count=1,effective_count=1,cap=20),funds=list(spendable_budget=10000),
 outstanding=list(count=0),rules=list(minimum_bid_at_value=TRUE)),observed_at=clock)
async_adapter_fetch<-function(fn,args=list(),context,worker_bypass=FALSE,...){
 if(fn=='get_team_acquisition_capacity'){
  if(!worker_bypass)return(promises::promise_resolve(saved()$data))
  return(promises::promise(function(resolve,reject)jobs[[length(jobs)+1L]]<<-list(resolve=resolve,reject=reject)))
 }
 value<-switch(fn,get_player_summary=list(data=list(id=context$player_id),championship=list(owner=list(computer=TRUE)),market=list(p=1000,bids=list()),points=list(),prices=list()),
 get_lineup_from_team=list(pointSystem='fixture'),get_player_full_profile=list(profile=list()),
 get_player_matches=list(matches=list()),read_card_round_history=data.frame(),read_card_price_history=data.frame(),stop(fn))
 promises::promise_resolve(value)
}
fm_worker_rpc<-function(...)stop('No worker acquisition or database request is allowed')
player<-data.frame(id='target',name='Dani fixture',role='defensa',team='Club',value=1000,price=1000,
 change=0,points=2,user_team_id='',market_inMarket=TRUE)
selected<-shiny::reactiveVal(player)
settle<-function(session)for(i in 1:35){later::run_now(0);session$flushReact()}
blocked<-function(html,id){tag<-regmatches(html,regexpr(paste0('<button[^>]*id="[^"]*',id,'"[^>]*>'),html));!length(tag)||grepl('disabled',tag,fixed=TRUE)}
args<-list(selected_player=selected,login_token=shiny::reactive(auth),championship_id=shiny::reactive('league'),user_team_id=shiny::reactive('mine'))
# Editor demand attaches a stored entry, checking actual freshness exactly once.
clear_api_cache();saved(NULL)
preflight_calls<-0L;writes<-0L
buy_clause<-function(...){writes<<-writes+1L;stop('No purchase may bypass current preflight')}
args$capacity_fetcher<-function(...){preflight_calls<<-preflight_calls+1L;capacity(-1)}
shiny::testServer(selected_player_Server,args=args,{
 settle(session);stopifnot(!length(jobs),!blocked(output$action_buttons$html,'btn_bid_market'))
 session$setInputs(btn_bid_market=1L);settle(session);stopifnot(length(jobs)==1L)
 jobs[[1]]$resolve(capacity(2));settle(session)
 original<-card_states$capacity()$data
 stopifnot(!market_offer_dialog_RV()$pending,!blocked(output$market_offer_submit$html,'submit_bid'))
 session$setInputs(market_offer_closed=as.character(market_offer_dialog_RV()$generation));settle(session)
 clock<<-clock+90;selected(NULL);settle(session);selected(player);settle(session)
 stopifnot(length(jobs)==1L,!blocked(output$action_buttons$html,'btn_bid_market'),
  !evaluate_acquisition_preflight(original,'bid',1000)$ok,
  !grepl('Retry availability|expired before',output$action_buttons$html))
 session$setInputs(btn_bid_market=2L);settle(session)
 stopifnot(length(jobs)==2L,market_offer_dialog_RV()$pending,blocked(output$market_offer_submit$html,'submit_bid'))
 session$setInputs(btn_bid_market=3L);settle(session);stopifnot(length(jobs)==2L)
 jobs[[2]]$resolve(capacity());settle(session)
 stopifnot(!market_offer_dialog_RV()$pending,!blocked(output$market_offer_submit$html,'submit_bid'))
 clock<<-clock+16;session$setInputs(bid_amount=1000,submit_bid=1L);settle(session)
 stopifnot(preflight_calls==1L,writes==0L)
 cat('PASS target session survives 90s; editor demand coalesces expired-funds verification and final preflight blocks an expired purchase\n')
})
# Explicitly opening a failed editor retries once; card redraws never retry it.
clear_api_cache();jobs<-list();saved(NULL)
shiny::testServer(selected_player_Server,args=args,{
 settle(session);session$setInputs(btn_bid_market=1L);settle(session)
 jobs[[1]]$resolve(capacity(-1));settle(session)
 stopifnot(length(jobs)==1L,!market_offer_dialog_RV()$pending,
  blocked(output$market_offer_submit$html,'submit_bid'),!blocked(output$action_buttons$html,'btn_bid_market'))
 for(i in 1:5)settle(session);stopifnot(length(jobs)==1L)
 session$setInputs(market_offer_closed=as.character(market_offer_dialog_RV()$generation));settle(session)
 session$setInputs(btn_bid_market=2L);settle(session);stopifnot(length(jobs)==2L)
 jobs[[2]]$reject(resource_failure(category='network'));settle(session)
 stopifnot(length(jobs)==2L,blocked(output$market_offer_submit$html,'submit_bid'),writes==0L)
 cat('PASS expired/failed financial completion remains blocked without whole-card retry loops; explicit editor reopening retries once\n')
})
clear_api_cache();jobs<-list();saved(resource_state(capacity()))
ctx<-resource_context(auth,'league','mine')
shiny::testServer(function(input,output,session){
 source<-player_availability_source(session,'team',shiny::reactive(ctx),function()auth,stored=function()saved(),mode=function()TRUE)
 output$value<-shiny::renderText(source()$data$funds$spendable_budget)
},{
 settle(session);stopifnot(!length(jobs),output$value=='10000')
 original<-source()$data
 clock<<-clock+1;saved(resource_state(capacity(-1)));settle(session)
 stopifnot(identical(source()$data,original),!length(jobs))
 clock<<-clock+90;saved(resource_state(capacity(status='partial')));settle(session)
 stopifnot(identical(source()$data,original),!length(jobs),isTRUE(source()$session_verified))
 cat('PASS unusable worker publications cannot overwrite a successful session observation or renew its actual source deadline\n')
})
