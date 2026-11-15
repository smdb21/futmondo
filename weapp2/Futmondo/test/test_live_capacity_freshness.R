#!/usr/bin/env Rscript
# Real parsers, async adapters, caches and preflight; only HTTP and wall time are
# synthetic. This guards the explicit Verify path, not a prebuilt capacity mock.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
clock_now<-as.POSIXct('2026-11-15 12:00:00',tz='UTC');Sys.time<-function()clock_now
get_sb_url<-function()'';get_sb_key<-function()''
get_locker_clause_payments<-function(...)observed_data(data.frame())
record_acquired_resource<-function(...)invisible(TRUE)
auth<-c(userid='live-capacity-freshness-fixture',token='synthetic')
ctx<-resource_context(auth,'league','mine',session_id='live-fixture')
settle<-function()for(i in 1:80)later::run_now(0)
counts<-integer();pending<-list();mode<-'late-market';requests<-character()
response<-function(url,body,ordinal){
 route<-tail(strsplit(url,'/',fixed=TRUE)[[1]],1L);q<-jsonlite::fromJSON(body)$query
 roster_id<-if(mode=='new-candidate'&&ordinal==1L)'arrival'else'owned'
 event_id<-if(mode=='new-candidate')'arrival'else'owned'
 answer<-switch(route,
  information=list(budget=1000,withheld=0,teamValue=2000,maxBid=2000,
   configuration=list(maxPlayersInRoster=20,mnmp=.5,mbmv=TRUE)),
  roster=list(list(id=roster_id,name='Owned',role='defensa',value=2000,change=0,points=5,teamId='club')),
  players=list(list(id='target',name='Target',role='defensa',value=1000,change=0,points=5,numberOfBids=0)),
  pressroom=if(q$from=='')list(news=list(list(`_id`='event',created='2026-11-14T12:00:00Z',
   `_player`=list(`_id`=event_id),`_buyer`=list(`_id`='mine'),`_seller`=list(`_id`='rival'),price=2000)))else list(news=list()),
  summary=list(data=list(id=q$playerId),championship=list(owner=list(id='rival'),
   clause=list(transferred=TRUE,transferDate='2026-11-16T12:00:00Z')),points=list(),prices=list()),
  stop(paste('Unexpected native fixture route',route)))
 list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=answer),auto_unbox=TRUE)),times=list(total=0))
}
fm_async_read_http<-function(url,method='POST',body=NULL,...){
 route<-tail(strsplit(url,'/',fixed=TRUE)[[1]],1L)
 if(is.na(counts[route]))counts[route]<<-0L
 counts[route]<<-counts[route]+1L;ordinal<-counts[route];requests<<-c(requests,route)
 hold<-(route=='pressroom'&&ordinal==1L)||
  (mode%in%c('late-market','pathological')&&route=='players'&&ordinal==2L)||
  (mode=='new-candidate'&&route=='summary'&&ordinal==1L)||
  (mode=='pathological'&&route=='information'&&ordinal==3L)
 if(hold)return(promises::promise(function(resolve,reject){
  pending[[paste(route,ordinal,sep=':')]]<<-function()resolve(response(url,body,ordinal))
 }))
 promises::promise_resolve(response(url,body,ordinal))
}
start<-function(){
 result<-new.env(parent=emptyenv());result$done<-FALSE
 promises::then(async_adapter_fetch('get_team_acquisition_capacity',
  list(login=auth,championship_id='league',user_team_id='mine'),context=ctx,worker_bypass=TRUE),
  function(x){result$data<-x;result$done<-TRUE;NULL},
  function(e){result$error<-e;result$done<-TRUE;NULL})
 settle();result
}
reset<-function(next_mode){
 clear_api_cache();mode<<-next_mode;counts<<-integer();requests<<-character();pending<<-list()
 clock_now<<-as.POSIXct('2026-11-15 12:00:00',tz='UTC')
}
release<-function(key,seconds){
 stopifnot(is.function(pending[[key]]));clock_now<<-clock_now+seconds
 finish<-pending[[key]];pending[[key]]<<-NULL;finish();settle()
}
assert_fresh<-function(result){
 stopifnot(result$done,is.null(result$error),result$data$status=='ok',
  result$data$valid_until>as.numeric(clock_now),evaluate_acquisition_preflight(result$data,'bid',100)$ok)
}
reset('late-market');result<-start();stopifnot(!result$done)
release('pressroom:1',31);stopifnot(!result$done,is.function(pending[['players:2']]))
release('players:2',20);assert_fresh(result)
stopifnot(counts['information']==3L,counts['players']==2L,counts['pressroom']==2L,
 counts['roster']<=3L,length(requests)<=10L)
before<-requests;warm<-start();assert_fresh(warm);stopifnot(identical(requests,before))
cat('PASS native explicit live check repairs funds after a 20s final market read; immediate reuse adds zero HTTP reads\n')

reset('new-candidate');result<-start();release('pressroom:1',31)
stopifnot(!result$done,is.function(pending[['summary:1']]),counts['information']==1L)
release('summary:1',31);assert_fresh(result)
stopifnot(result$data$roster$pending_inbound_count==1L,result$data$roster$effective_count==2L,
 counts['summary']==1L,counts['pressroom']==2L,length(requests)<=12L)
cat('PASS refreshed roster discovers a new pending arrival; slow summary finishes before final funds acquisition and remains counted\n')

reset('pathological');result<-start();release('pressroom:1',31);release('players:2',20)
stopifnot(!result$done,is.function(pending[['information:3']]))
release('information:3',21)
stopifnot(result$done,is.null(result$error),result$data$status=='partial',
 result$data$valid_until<=as.numeric(clock_now),!evaluate_acquisition_preflight(result$data,'bid',100)$ok,
 counts['information']==3L,counts['players']==2L,length(requests)<=12L,!length(pending))
before<-requests;settle();stopifnot(identical(requests,before))
cat('PASS pathological alternating delays stop at the bounded repair limit with original expired deadlines and no retry loop\n')
