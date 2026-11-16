#!/usr/bin/env Rscript
# Slow completion and nearly expired durable reuse through the real card/provider.
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
 value<-switch(fn,get_player_summary=list(championship=list(),points=list(),prices=list()),
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
clear_api_cache();saved(resource_state(capacity(2)))
preflight_calls<-0L;writes<-0L
buy_clause<-function(...){writes<<-writes+1L;stop('No purchase may bypass a current preflight')}
args$capacity_fetcher<-function(...){preflight_calls<<-preflight_calls+1L;capacity(-1)}
shiny::testServer(selected_player_Server,args=args,{
 settle(session)
 jobs[[1]]$resolve(capacity(2));settle(session)
 stopifnot(length(jobs)==1L,!blocked(output$action_buttons$html,'btn_bid_market'))
 original<-card_states$capacity()$data
 clock<<-clock+90
 row<-selected();selected(NULL);settle(session);selected(row);settle(session)
 stopifnot(length(jobs)==1L,!blocked(output$action_buttons$html,'btn_bid_market'),
  isTRUE(card_states$capacity()$session_verified),identical(card_states$capacity()$data,original),
  !evaluate_acquisition_preflight(original,'bid',1000)$ok,
  !grepl('Retry availability|expired before',output$action_buttons$html))
 # Opening an editor after expiry requests current checks; submission stays
 # disabled until those complete. Repeated clicks join the same pending read.
 session$setInputs(btn_bid_market=1L);settle(session)
 stopifnot(length(jobs)==2L,market_offer_dialog_RV()$pending,
  blocked(output$market_offer_submit$html,'submit_bid'))
 session$setInputs(btn_bid_market=2L);settle(session);stopifnot(length(jobs)==2L)
 jobs[[2]]$resolve(capacity());settle(session)
 stopifnot(!market_offer_dialog_RV()$pending,!blocked(output$market_offer_submit$html,'submit_bid'))
 clock<<-clock+16;session$setInputs(bid_amount=1000,submit_bid=1L);settle(session)
 stopifnot(preflight_calls==1L,writes==0L)
 cat('PASS one session check survives 90s/reopening; editor entry joins one current check and an expired final preflight blocks the purchase\n')
})
# Test the provider directly to inject real stored-publication revisions.
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
 clock<<-clock+1;saved(resource_state(capacity(status='partial')));settle(session)
 clock<<-clock+90;saved(resource_state(capacity(-1)));settle(session)
 stopifnot(identical(source()$data,original),!length(jobs),isTRUE(source()$session_verified))
 cat('PASS newer expired or partial stored publications preserve the verified session result without acquisition loops\n')
})
clear_api_cache();jobs<-list();saved(NULL)
shiny::testServer(selected_player_Server,args=args,{
 settle(session);stopifnot(length(jobs)==1L)
 heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,0);settle(session);stopifnot(heartbeat)
 clock<<-clock+31;jobs[[1]]$resolve(capacity(-1));settle(session)
 stopifnot(length(jobs)==2L,blocked(output$action_buttons$html,'btn_bid_market'),
  blocked(output$action_buttons$html,'btn_verify_availability'))
 jobs[[2]]$resolve(capacity());settle(session)
 stopifnot(!blocked(output$action_buttons$html,'btn_bid_market'),length(jobs)==2L,
  evaluate_acquisition_preflight(card_states$capacity()$data,'bid',1000)$ok)
 clock<<-clock+16
 row<-selected();row$points<-3;selected(row);settle(session)
 stopifnot(length(jobs)==2L)
 cat('PASS an expired initial live completion in worker mode repairs once, yields to heartbeat and enables actions; later ordinary expiry cannot poll\n')
})
clear_api_cache();jobs<-list();saved(NULL)
shiny::testServer(selected_player_Server,args=args,{
 settle(session);jobs[[1]]$resolve(capacity(-1));settle(session);stopifnot(length(jobs)==2L)
 jobs[[2]]$resolve(capacity(-1));settle(session)
 stopifnot(length(jobs)==2L,blocked(output$action_buttons$html,'btn_bid_market'),
  !blocked(output$action_buttons$html,'btn_verify_availability'))
 session$setInputs(btn_verify_availability=1L);settle(session);stopifnot(length(jobs)==3L)
 jobs[[3]]$reject(resource_failure(category='network'));settle(session)
 stopifnot(length(jobs)==3L,blocked(output$action_buttons$html,'btn_bid_market'))
 cat('PASS repeated expired completions stop after one repair; an explicit failed retry never creates another automatic attempt\n')
})

# Fresh table identity/listing facts can open the card independently of history.
clear_api_cache();jobs<-list();saved(NULL)
fresh_player<-observed_data(player,observed_at=clock);selected(fresh_player)
source_async<-async_adapter_fetch
async_adapter_fetch<-function(fn,...){
 if(fn=='get_player_summary')return(promises::promise(function(resolve,reject)NULL))
 source_async(fn,...)
}
shiny::testServer(selected_player_Server,args=args,{
 settle(session)
 stopifnot(!length(jobs),identical(card_states$summary()$status,'loading'),
  !blocked(output$action_buttons$html,'btn_bid_market'),
  !grepl('Retry availability|Checking availability|Data unavailable',output$action_buttons$html))
 session$setInputs(btn_bid_market=1L);settle(session)
 stopifnot(length(jobs)==1L,market_offer_dialog_RV()$pending,blocked(output$market_offer_submit$html,'submit_bid'))
 jobs[[1]]$resolve(capacity());settle(session)
 stopifnot(!market_offer_dialog_RV()$pending,!blocked(output$market_offer_submit$html,'submit_bid'))
 cat('PASS recent table ownership/listing facts add zero capacity reads and do not wait for history; explicit offer entry verifies funds before enabling Submit\n')
})
for(age in c(61,-1)){
 stale<-observed_data(player,observed_at=clock-age)
 stopifnot(!player_card_recent_table_facts(stale,clock))
}
unknown<-player;stopifnot(!player_card_recent_table_facts(unknown,clock))
missing<-fresh_player;missing$market_inMarket<-NA;stopifnot(!player_card_recent_table_facts(missing,clock))
old_market<-fresh_player;attr(old_market,'availability_market_observed_at')<-clock-61
stopifnot(!player_card_recent_table_facts(old_market,clock))
cat('PASS old/future/undated/unknown-market table facts cannot suppress verification; an older joined market timestamp bounds reuse\n')
future_market<-fresh_player;attr(future_market,'availability_market_observed_at')<-clock+1
stopifnot(!player_card_recent_table_facts(future_market,clock))
