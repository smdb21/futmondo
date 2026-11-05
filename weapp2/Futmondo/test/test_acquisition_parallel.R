#!/usr/bin/env Rscript
# Controlled source promises and a virtual source clock; no HTTP or accounts.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('futmondo_functions.R'))
checks<-0L;check<-function(name,code){force(code);checks<<-checks+1L;cat('PASS',name,'\n')}
auth<-c(userid='parallel-capacity-fixture',token='synthetic')
ctx<-resource_context(auth,'league','team',session_id='fixture-session')
clock<-base::Sys.time();Sys.time<-function()clock
stamp<-function(data)observed_data(data,observed_at=clock)
info<-function()stamp(list(budget=1000,withheld=0,teamValue=2000,maxBid=2000,configuration=list(maxPlayersInRoster=20,mnmp=.5,mbmv=TRUE)))
roster<-function(ids='owned')stamp(data.frame(id=ids,value=rep(2000,length(ids))))
market<-function()stamp(data.frame(id=character(),bid_id=character(),bid_price=numeric()))
history<-function(ids=character())stamp(data.frame(id=if(length(ids))paste0('event-',ids)else character(),player_id=ids,
 buyer_team_id=rep('team',length(ids)),seller_team_id=rep('rival',length(ids)),
 created=rep(format(clock-100,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),length(ids))))
summary<-function()stamp(list(championship=list(owner=list(id='rival'),
 clause=list(transferred=TRUE,transferDate=format(clock+86400,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')))))
settle<-function()for(i in 1:20)later::run_now(0)
reset<-function(){clear_api_cache();jobs<<-list();active<<-0L;peak<<-0L;current<<-TRUE;clock<<-base::Sys.time()}
async_adapter_fetch<-function(fn,args=list(),context,is_current,priority,owner){
 stopifnot(priority=='interactive',owner==ctx$session_id,context$account_id==ctx$account_id)
 active<<-active+1L;peak<<-max(peak,active)
 promises::promise(function(resolve,reject){jobs[[length(jobs)+1L]]<<-list(fn=fn,args=args,resolve=resolve,reject=reject,done=FALSE)})
}
start<-function(){
 result<-new.env(parent=emptyenv());result$done<-FALSE
 p<-get_team_acquisition_capacity_async(auth,'league','team',ctx,is_current=function()current)
 promises::then(p,function(value){result$data<-value;result$done<-TRUE},function(error){result$error<-error;result$done<-TRUE})
 settle();result
}
complete<-function(index,data=NULL,error=NULL){
 stopifnot(!jobs[[index]]$done);jobs[[index]]$done<<-TRUE;active<<-active-1L
 if(is.null(error))jobs[[index]]$resolve(data)else jobs[[index]]$reject(error)
 settle()
}
next_job<-function(fn){which(vapply(jobs,function(x)!x$done&&identical(x$fn,fn),logical(1)))[1]}
complete_initial<-function(records=history(),own=roster()){
 complete(next_job('get_user_team_info'),info());complete(next_job('get_players_from_team'),own)
 complete(next_job('get_market_players'),market());complete(next_job('get_championship_pressroom'),records)
}
reset();result<-start()
check('four independent prerequisites dispatch together with interactive context',stopifnot(length(jobs)==4L,active==4L,peak==4L))
complete(next_job('get_user_team_info'),info());complete(next_job('get_players_from_team'),roster());complete(next_job('get_market_players'),market())
clock<-clock+31;complete(next_job('get_championship_pressroom'),history())
check('a long history read refreshes only expired mutable facts in parallel',{
 stopifnot(!result$done,length(jobs)==7L,active==3L,
  sum(vapply(jobs,function(x)x$fn=='get_championship_pressroom',logical(1)))==1L)
 complete(next_job('get_user_team_info'),info());complete(next_job('get_players_from_team'),roster());complete(next_job('get_market_players'),market())
 stopifnot(result$done,result$data$status=='ok',result$data$valid_until>as.numeric(clock)+14,
  evaluate_acquisition_preflight(result$data,'bid',100)$ok)
 before<-length(jobs);warm<-start();stopifnot(warm$done,length(jobs)==before)
})
reset();result<-start();complete_initial(history(paste0('incoming-',1:7)))
check('pending players use two summary slots and all seven are verified without serial prerequisite replay',{
 stopifnot(active==2L,length(jobs)==6L)
 for(i in 1:7){complete(next_job('get_player_summary'),summary());stopifnot(active<=2L)}
 stopifnot(result$done,length(jobs)==11L,result$data$roster$pending_inbound_count==7L,
  result$data$roster$effective_count==8L,result$data$roster$status=='ok')
})
reset();result<-start()
complete(next_job('get_user_team_info'),info());complete(next_job('get_players_from_team'),roster('arrival'));complete(next_job('get_market_players'),market())
clock<-clock+31;complete(next_job('get_championship_pressroom'),history('arrival'))
complete(next_job('get_user_team_info'),info());complete(next_job('get_players_from_team'),roster(character()));complete(next_job('get_market_players'),market())
check('a changed refreshed roster discovers and verifies new candidate IDs before completion',{
 stopifnot(!result$done,active==1L,jobs[[next_job('get_player_summary')]]$args$player_id=='arrival')
 complete(next_job('get_player_summary'),summary())
 stopifnot(result$done,result$data$roster$pending_inbound_count==1L,result$data$roster$effective_count==1L)
})
reset();result<-start();complete_initial(history('arrival'),roster(character()))
clock<-clock+31;complete(next_job('get_player_summary'),error=resource_failure(resource='get_player_summary',category='http',http_status=403L))
complete(next_job('get_user_team_info'),info());complete(next_job('get_players_from_team'),roster('arrival'));complete(next_job('get_market_players'),market())
check('removed candidates discard obsolete failures and failed summaries are not automatically replayed',{
 stopifnot(result$done,result$data$roster$status=='ok',is.null(attr(result$data,'fetch_error')),
  sum(vapply(jobs,function(x)x$fn=='get_player_summary',logical(1)))==1L)
})
reset();result<-start();complete_initial(history('arrival'),roster(character()))
clock<-clock+31;complete(next_job('get_player_summary'),error=resource_failure(resource='get_player_summary',category='http',http_status=403L))
complete(next_job('get_user_team_info'),info());complete(next_job('get_players_from_team'),roster(character()));complete(next_job('get_market_players'),market())
check('a still-required permanent summary failure stays blocked without an extra freshness retry',{
 stopifnot(result$done,!evaluate_acquisition_preflight(result$data,'bid',100)$ok,
  attr(result$data,'fetch_error')$http_status==403L,
  sum(vapply(jobs,function(x)x$fn=='get_player_summary',logical(1)))==1L)
})
reset();result<-start()
complete(next_job('get_user_team_info'),info());complete(next_job('get_players_from_team'),roster());complete(next_job('get_market_players'),market())
complete(next_job('get_championship_pressroom'),error=resource_failure(resource='pressroom',category='network'))
check('missing complete history settles with a failed preflight and its safe source diagnostic',{
 stopifnot(result$done,length(jobs)==4L,!evaluate_acquisition_preflight(result$data,'bid',100)$ok,
  attr(result$data,'fetch_error')$category=='network',result$data$roster$status=='unavailable')
})
reset();result<-start()
complete(next_job('get_user_team_info'),info());complete(next_job('get_players_from_team'),roster());complete(next_job('get_market_players'),market())
clock<-clock+31;complete(next_job('get_championship_pressroom'),history())
complete(next_job('get_user_team_info'),info());clock<-clock+20
complete(next_job('get_players_from_team'),roster());complete(next_job('get_market_players'),market())
check('the bounded final freshness pass cannot publish expired facts as verified or loop indefinitely',{
 stopifnot(result$done,length(jobs)==7L,result$data$status=='partial',
  !evaluate_acquisition_preflight(result$data,'bid',100)$ok,result$data$valid_until<=as.numeric(clock))
})
reset();result<-start();current<-FALSE;complete(next_job('get_user_team_info'),info())
check('context cancellation settles the promise and starts no dependent work or capacity cache',{
 stopifnot(result$done,inherits(result$error,'fm_async_cancelled'),length(jobs)==4L,
  !any(grepl('acq_capacity',ls(api_cache_env))))
})
rm('Sys.time',envir=.GlobalEnv)
cat(sprintf('PARALLEL ACQUISITION: %d passed / 0 failed\n',checks))
