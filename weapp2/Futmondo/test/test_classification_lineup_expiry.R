#!/usr/bin/env Rscript
# Exercise the real bound read and shared resource: a persisted source deadline
# must not restart when a recent database hydration creates a fresh outer state.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='expiry-fixture',token='synthetic')
fixture_now<-base::Sys.time();Sys.time<-function()fixture_now
calls<-0L;resolve_next<-NULL
async_adapter_fetch<-function(fn,args,context,...){
  stopifnot(fn=='get_round_lineup_snapshot');calls<<-calls+1L
  if(calls==1L)return(promises::promise_resolve(structure(
    list(round_id='r1',answer=list(),revision=1L,valid_until=as.numeric(fixture_now)+1),
    fetched_at=fixture_now,observed_at=fixture_now-299,fetch_status='ok')))
  promises::promise(function(resolve,reject)resolve_next<<-resolve)
}
settle<-function(session)for(i in 1:15){later::run_now(0);session$flushReact()}
clear_api_cache()
shiny::testServer(function(input,output,session){
  bound<-factual_read_adapter(session,'get_round_lineup_snapshot',function()auth,function()'league',function()'mine',state_only=TRUE)
  pulse<-reactiveVal(0L)
  snapshot<-reactive({pulse();bound(auth,'league','mine','r1')})
  output$status<-renderText(paste(snapshot()$status,snapshot()$freshness))
},{
  settle(session)
  first<-snapshot();stopifnot(calls==1L,first$status=='ok',first$freshness=='fresh',first$data$revision==1L)
  fixture_now<<-fixture_now+1
  stopifnot(as.numeric(difftime(fixture_now,first$fetched_at,units='secs'))<resource_policy('get_round_lineup_snapshot')$ttl)
  pulse(1L);settle(session)
  pending<-snapshot()
  stopifnot(calls==2L,pending$status=='loading',pending$freshness=='stale',pending$data$revision==1L)
  resolve_next(structure(list(round_id='r1',answer=list(),revision=2L,valid_until=as.numeric(fixture_now)+300),
    fetched_at=fixture_now,observed_at=fixture_now,fetch_status='ok'))
  settle(session)
  current<-snapshot();stopifnot(calls==2L,current$status=='ok',current$freshness=='fresh',current$data$revision==2L)
  pulse(2L);settle(session);stopifnot(calls==2L)
})
cat('PASS bound historical lineup refreshes exactly at original source deadline despite recent hydration; one refresh, stale pending data, warm reuse\n')
