#!/usr/bin/env Rscript
# Actual card wiring; delayed factual promises, no accounts, HTTP or transactions.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
clock<-base::Sys.time();Sys.time<-function()clock
get_sb_url<-function()'';get_sb_key<-function()''
fm_worker_configured<-function()TRUE;fm_worker_mode<-function(...)TRUE
fm_worker_resolve_context<-function(handler,args,context)context
fm_worker_rpc<-function(...)stop('No worker publication may be required')
auth<-c(userid='availability-switch-fixture',token='synthetic')
team<-shiny::reactiveVal('mine');refresh<-shiny::reactiveVal(0L)
row<-function(id)data.frame(id=id,name=paste('Player',id),role='defensa',team='Club',value=1000,price=1000,
 change=0,points=2,user_team_id='rival',market_inMarket=TRUE)
selected<-shiny::reactiveVal(row('p1'))
team_jobs<-list();summary_jobs<-list();reads<-list();writes<-0L
capacity<-function(){observed_data(list(status='ok',valid_until=as.numeric(clock)+15,
 roster=list(status='ok',count=1,effective_count=1,cap=20),funds=list(spendable_budget=10000),
 outstanding=list(count=0),rules=list(minimum_bid_at_value=TRUE)),observed_at=clock)}
summary<-function(id,bid=NULL)observed_data(list(data=list(id=id),championship=list(),points=list(),prices=list(),
 my_bid_id=bid,my_bid_price=if(is.null(bid))NULL else 1000),observed_at=clock)
async_adapter_fetch<-function(fn,args=list(),context,is_current=function()TRUE,worker_bypass=FALSE,...){
 reads[[length(reads)+1L]]<<-list(fn=fn,bypass=worker_bypass,context=context)
 if(fn=='get_team_acquisition_capacity'){
  if(!worker_bypass){old<-capacity();old$valid_until<-as.numeric(clock)-30;return(promises::promise_resolve(observed_data(old,status='stale',observed_at=clock-45)))}
  return(promises::promise(function(resolve,reject)team_jobs[[length(team_jobs)+1L]]<<-list(resolve=resolve,reject=reject,current=is_current,context=context)))
 }
 if(fn=='get_player_summary'){
  stopifnot(worker_bypass)
  return(promises::promise(function(resolve,reject)summary_jobs[[length(summary_jobs)+1L]]<<-list(resolve=resolve,reject=reject,current=is_current,context=context)))
 }
 if(fn%in%c('get_player_full_profile','get_player_matches'))return(promises::promise(function(resolve,reject)NULL))
 value<-switch(fn,get_lineup_from_team=list(pointSystem='fixture'),get_my_market_players=data.frame(),
  read_card_round_history=data.frame(),read_card_price_history=data.frame(),stop(fn))
 promises::promise_resolve(value)
}
buy_clause<-function(...){writes<<-writes+1L;stop('No transaction is permitted')}
settle<-function(session)for(i in 1:35){later::run_now(0);session$flushReact()}
blocked<-function(html,id){tag<-regmatches(html,regexpr(paste0('<button[^>]*id="[^"]*',id,'"[^>]*>'),html));!length(tag)||grepl('disabled',tag,fixed=TRUE)}
clear_api_cache()
shiny::testServer(selected_player_Server,args=list(selected_player=selected,login_token=shiny::reactive(auth),
 championship_id=shiny::reactive('league'),user_team_id=team,refresh_trigger=refresh),{
 settle(session);stopifnot(length(team_jobs)==1L,length(summary_jobs)==1L)
 selected(row('p2'));settle(session)
 stopifnot(length(team_jobs)==1L,length(summary_jobs)==2L,team_jobs[[1]]$current())
 summary_jobs[[1]]$resolve(summary('p1','old-player-bid'));team_jobs[[1]]$resolve(capacity());settle(session)
 stopifnot(blocked(output$action_buttons$html,'btn_bid_market'),!grepl('old-player-bid',output$action_buttons$html,fixed=TRUE))
 summary_jobs[[2]]$resolve(summary('p2'));settle(session)
 stopifnot(!blocked(output$action_buttons$html,'btn_bid_market'),length(team_jobs)==1L)
 cat('PASS switching during acquisition joins one team request; missing worker summaries fetch directly and an old player completion cannot authorize the new card\n')
 started<-proc.time()[['elapsed']]
 selected(row('p3'));settle(session)
 stopifnot(length(team_jobs)==1L,length(summary_jobs)==3L)
 summary_jobs[[3]]$resolve(summary('p3'));settle(session)
 stopifnot(!blocked(output$action_buttons$html,'btn_bid_market'),identical(card_states$profile()$status,'loading'))
 elapsed<-proc.time()[['elapsed']]-started
 selected(row('p2'));settle(session)
 stopifnot(length(team_jobs)==1L,length(summary_jobs)==3L,!blocked(output$action_buttons$html,'btn_bid_market'))
 cat(sprintf('PASS warm team reuse adds zero team/known-summary dispatches; a new summary enables actions while optional profile stays held (%.3fs local switch/flush)\n',elapsed))
 team('another');settle(session)
 stopifnot(length(team_jobs)==2L,length(summary_jobs)==4L,!team_jobs[[1]]$current())
 selected(NULL);settle(session);stopifnot(team_jobs[[2]]$current())
 team_jobs[[2]]$resolve(capacity());summary_jobs[[4]]$resolve(summary('p2'));settle(session)
 selected(row('p4'));settle(session)
 stopifnot(length(team_jobs)==2L,length(summary_jobs)==5L)
 summary_jobs[[5]]$resolve(summary('p4'));settle(session)
 stopifnot(!blocked(output$action_buttons$html,'btn_bid_market'))
 cat('PASS account/team scope rejects obsolete completions while a finite team request may finish after card closure and serve the next card\n')
 refresh(1L);settle(session)
 stopifnot(length(team_jobs)==3L,length(summary_jobs)==6L,blocked(output$action_buttons$html,'btn_bid_market'))
 session$setInputs(btn_verify_availability=1L);settle(session);stopifnot(length(team_jobs)==3L)
 team_jobs[[3]]$resolve(capacity());summary_jobs[[6]]$resolve(summary('p4'));settle(session)
 before<-length(team_jobs);session$setInputs(refresh_history=1L);settle(session)
 stopifnot(length(team_jobs)==before,length(summary_jobs)==7L,writes==0L)
 summary_jobs[[7]]$resolve(summary('p4'));settle(session)
 stopifnot(!blocked(output$action_buttons$html,'btn_bid_market'))
 cat('PASS explicit refresh checks once, duplicate retries join, and chart-history refresh does not restart team verification\n')
 clock<<-clock+90;selected(row('p5'));settle(session)
 stopifnot(length(team_jobs)==3L,length(summary_jobs)==8L,blocked(output$action_buttons$html,'btn_bid_market'))
 summary_jobs[[8]]$resolve(summary('p5'));settle(session)
 stopifnot(!blocked(output$action_buttons$html,'btn_bid_market'),writes==0L,
  !evaluate_acquisition_preflight(card_states$capacity()$data,'bid',1000)$ok)
 cat('PASS session verification survives original source expiry and switching; transaction preflight still rejects expired facts\n')
 selected(row('p6'));settle(session);stopifnot(length(summary_jobs)==9L,length(team_jobs)==3L)
 summary_jobs[[9]]$reject(resource_failure(resource='get_player_summary',category='network'));settle(session)
 stopifnot(!blocked(output$action_buttons$html,'btn_verify_availability'),blocked(output$action_buttons$html,'btn_bid_market'))
 session$setInputs(btn_verify_availability=2L);settle(session)
 stopifnot(length(summary_jobs)==10L,length(team_jobs)==3L)
 summary_jobs[[10]]$resolve(summary('p6'));settle(session)
 stopifnot(!blocked(output$action_buttons$html,'btn_bid_market'))
 selected(row('p7'));settle(session)
 stale<-summary('p7');attr(stale,'observed_at')<-clock-61
 summary_jobs[[11]]$resolve(stale);settle(session)
 stopifnot(blocked(output$action_buttons$html,'btn_bid_market'),!blocked(output$action_buttons$html,'btn_verify_availability'),length(team_jobs)==3L)
 session$setInputs(btn_verify_availability=3L);settle(session)
 stopifnot(length(summary_jobs)==12L,length(team_jobs)==3L)
 summary_jobs[[12]]$resolve(summary('p7'));settle(session)
 stopifnot(!blocked(output$action_buttons$html,'btn_bid_market'),writes==0L)
 cat('PASS failed or expired player summaries stay blocked and retry only that summary, without repeating fresh team checks\n')
})
# Owned players use their own shared factual listing gate, not team acquisition.
base_async<-async_adapter_fetch;listing_jobs<-list();team_jobs<-list();summary_jobs<-list()
async_adapter_fetch<-function(fn,args=list(),context,is_current=function()TRUE,worker_bypass=FALSE,...){
 if(fn=='get_my_market_players'){
  stopifnot(worker_bypass)
  return(promises::promise(function(resolve,reject)listing_jobs[[length(listing_jobs)+1L]]<<-list(resolve=resolve,reject=reject)))
 }
 base_async(fn,args,context,is_current=is_current,worker_bypass=worker_bypass,...)
}
own<-row('own1');own$user_team_id<-shiny::isolate(team());selected(own);refresh(0L)
shiny::testServer(selected_player_Server,args=list(selected_player=selected,login_token=shiny::reactive(auth),
 championship_id=shiny::reactive('league'),user_team_id=team,refresh_trigger=refresh),{
 settle(session);stopifnot(length(listing_jobs)==1L,!length(team_jobs))
 listing_jobs[[1]]$reject(resource_failure(resource='get_my_market_players',category='network'));settle(session)
 stopifnot(!blocked(output$action_buttons$html,'btn_verify_availability'))
 session$setInputs(btn_verify_availability=1L);settle(session)
 stopifnot(length(listing_jobs)==2L,!length(team_jobs))
 listing_jobs[[2]]$resolve(observed_data(data.frame(),observed_at=clock));settle(session)
 own2<-own;own2$id<-'own2';selected(own2);settle(session)
 stopifnot(length(listing_jobs)==2L,!length(team_jobs),player_availability_valid(card_states$listings()$data,'listings'))
 clock<<-clock+21;own3<-own;own3$id<-'own3';selected(own3);settle(session)
 stopifnot(length(listing_jobs)==3L,!length(team_jobs),blocked(output$action_buttons$html,'btn_verify_availability'))
 cat('PASS owned-card listing checks bypass worker waits, retry their own failures, share a verified empty list and respect the original 20-second market lifetime\n')
})
