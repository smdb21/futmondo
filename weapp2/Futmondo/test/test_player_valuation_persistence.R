#!/usr/bin/env Rscript
# Exercise the production asynchronous DB read/acknowledgement paths with fakes.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
ctx<-resource_context(account_id='price-fixture',championship_id='league',user_team_id='team',player_id='player',
 competition_id='competition',season='2025-2026',scoring_version='scoring',session_id='fixture-session')
profile<-list(player=list(id='player'),league=list(`_id`='competition',season=ctx$season),
 prices=data.frame(date=format(as.POSIXct('2026-09-01',tz='UTC')+0:30*86400,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),social=1e6+0:30*1000,classic=2e6+0:30*1000))
attr(profile,'observed_at')<-fm_time('2026-11-15T08:00:00Z')
get_sb_url<-function()'https://valuation.fixture.invalid';get_sb_key<-function()'synthetic-database-key'
futmondo_post<-function(...)stop('No source acquisition belongs to saved-history replay')
posts<-list();attempts<-0L;.persistence_retry_base_seconds<-0
fm_async_http<-function(url,method,body,headers,owner,...){
 attempts<<-attempts+1L;posts[[attempts]]<<-list(url=url,method=method,body=body)
 if(attempts==1L)return(promises::promise_reject(resource_failure(resource='database',category='http',http_status=503L)))
 promises::promise_resolve(list(status_code=201L))
}
options(futmondo.offline=FALSE)
stopifnot(persist_player_valuation_profile(profile,ctx),length(.persistence_state$queue)==1L,
 !length(.persistence_state$acknowledged))
deadline<-Sys.time()+3
while(background_sync_status(ctx$account_id)$pending&&Sys.time()<deadline){later::run_now(.005);Sys.sleep(.001)}
stopifnot(attempts==2L,identical(posts[[1]]$body,posts[[2]]$body),length(.persistence_state$acknowledged)==1L,
 is.null(background_sync_status(ctx$account_id)$error))
row<-jsonlite::fromJSON(posts[[2]]$body,simplifyVector=FALSE)[[1]]
stopifnot(row$payload$profile$complete,identical(row$payload$scope,player_valuation_scope(ctx)),
 !grepl('synthetic-database-key|token|password|authorization|cookie',posts[[2]]$body,ignore.case=TRUE))
stopifnot(persist_player_valuation_profile(profile,ctx),!length(.persistence_state$queue),attempts==2L)
cat('PASS source snapshot retries identically, acknowledges completed writes only, and skips acknowledged duplicates\n')
reads<-list();heartbeat<-FALSE
fm_async_read_http<-function(url,...){
 parsed<-httr::parse_url(url);table<-tail(strsplit(parsed$path,'/',fixed=TRUE)[[1]],1)
 reads[[length(reads)+1L]]<<-list(table=table,query=parsed$query)
 response<-if(table=='player_history')list()else{
  stopifnot(table=='rival_finance_evidence_latest',parsed$query$limit=='1',
   parsed$query$season=='eq.2025-2026',parsed$query$competition_id=='eq.competition')
  list(row)
 }
 promises::promise(function(resolve,reject)later::later(function()resolve(list(status_code=200L,
  content=charToRaw(jsonlite::toJSON(response,auto_unbox=TRUE,na='null',null='null',digits=NA)))),.025))
}
clear_api_cache();later::later(function()heartbeat<<-TRUE,.005)
done<-FALSE;result<-failure<-NULL
started<-proc.time()[['elapsed']]
promises::then(async_adapter_fetch('read_card_price_history',list(context=ctx),ctx),
 function(x){result<<-x;done<<-TRUE},function(e){failure<<-e;done<<-TRUE})
deadline<-Sys.time()+5
while(!done&&Sys.time()<deadline){later::run_now(.005);Sys.sleep(.001)}
stopifnot(done,is.null(failure),heartbeat,length(reads)==2L)
summary<-list(prices=data.frame(date=tail(profile$prices$date,7),price=tail(profile$prices$social,7)))
values<-normalize_card_valuation(result,summary,context=ctx)
stopifnot(nrow(values)==31L,all(values$value==profile$prices$social),
 identical(attr(attr(result,'valuation_profile'),'fetch_status'),'stale'))
cat(sprintf('PASS cold saved-history replay: %d DB reads, zero source reads, heartbeat before completion, %.3fs including 50ms injected DB delay\n',
 length(reads),proc.time()[['elapsed']]-started))
options(futmondo.offline=TRUE)
