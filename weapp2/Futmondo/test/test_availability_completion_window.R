#!/usr/bin/env Rscript
# A durable availability result retains its original source deadline.
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
settle <- function(session) for(i in 1:35) { later::run_now(0); session$flushReact() }
clear_api_cache();jobs<-list();saved(resource_state(capacity()))
ctx<-resource_context(auth,'league','mine')
shiny::testServer(function(input,output,session){
 source<-player_availability_source(session,'team',shiny::reactive(ctx),function()auth,stored=function()saved(),mode=function()TRUE,intent=function()'manual_action')
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
