#!/usr/bin/env Rscript
# Captured compact league-list contract, synthetic HTTP/database boundaries only.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='lineup-calendar-fixture',token='private-fixture-token');cid<-'fixture-league';lid<-'source-league'
active<-list(championships=list(list(id=cid,league=lid,userteam=list(id='own-team'))),
 rounds=list(list(`_id`='r12',number=12,championshipId=lid,beginProcess='2026-11-01T12:00:00Z')))
leagues<-list(list(`_id`=lid,rounds=lapply(1:38,function(i)list(`_id`=paste0('r',i),status=if(i==12)'next'else'future'))))
calendar<-list(rounds=lapply(38:1,function(i)list(`_id`=paste0('r',i),number=i,
 status=if(i<12)'done'else if(i==12)'playing'else'future',matches=list(list(private='discarded')))))
fixture<-new.env(parent=emptyenv());fixture$stored<-list();fixture$writes<-0L;fixture$db_reads<-0L;fixture$calendar_reads<-0L;fixture$mode<-'ok'
get_sb_url<-function()'https://database.fixture.invalid';get_sb_key<-function()'private-database-key'
persist_rival_finance_evidence<-function(evidence,context){
 stopifnot(length(evidence)==1L,context$account_id==auth[['userid']],context$championship_id==cid,
  !grepl('private-fixture-token|private-database-key|"matches"|"private"',jsonlite::toJSON(evidence,auto_unbox=TRUE,na='null',null='null')))
 fixture$stored<-evidence;fixture$writes<-fixture$writes+1L;TRUE
}
futmondo_post<-function(...)stop('Unexpected synchronous HTTP')
fm_async_http<-function(url,method='GET',body=NULL,...){
 if(method=='GET'){
  query<-httr::parse_url(url)$query
  stopifnot(grepl('/rest/v1/rival_finance_evidence_latest',url,fixed=TRUE),query$account_id==paste0('eq.',auth[['userid']]),
   query$championship_id==paste0('eq.',cid),query$season=='eq.unknown',query$evidence_type=='eq.eligibility',query$limit=='1',
   startsWith(query$source_id,'eq.calendar:v1:'))
  fixture$db_reads<-fixture$db_reads+1L
  return(promises::promise_resolve(list(status_code=200L,content=charToRaw(jsonlite::toJSON(unname(fixture$stored),auto_unbox=TRUE,na='null',null='null',digits=NA)),times=list(total=0))))
 }
 stopifnot(method=='POST')
 query<-jsonlite::fromJSON(as.character(body),simplifyVector=FALSE)$query
 if(identical(url,ACTIVE_CHAMPIONSHIPS_URL))answer<-active
 else if(grepl('/2/league/list$',url))answer<-leagues
 else if(grepl('/2/league/matches$',url)){
  stopifnot(identical(query,list(leagueId=lid)));fixture$calendar_reads<-fixture$calendar_reads+1L
  answer<-calendar
  if(fixture$mode=='missing')answer<-list()
  if(fixture$mode=='conflict')answer$rounds<-c(answer$rounds,list(list(`_id`='r1',number=99,status='done')))
 }else stop('Unexpected source route')
 promises::promise_resolve(list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=answer),auto_unbox=TRUE,null='null')),times=list(total=0)))
}
options(futmondo.offline=FALSE)
await<-function(p){
 done<-FALSE;answer<-error<-NULL
 promises::then(p,function(x){answer<<-x;done<<-TRUE},function(e){error<<-e;done<<-TRUE})
 deadline<-Sys.time()+4
 while(!done&&Sys.time()<deadline)later::run_now(.001)
 stopifnot(done);if(!is.null(error))stop(error);answer
}
read_rounds<-function()await(async_adapter_fetch('get_finished_rounds',list(login=auth,championship_id=cid),resource_context(auth,cid)))
clear_api_cache();rounds<-read_rounds()
stopifnot(isTRUE(attr(rounds,'catalog_complete')),attr(rounds,'catalog_status')=='ok',
 identical(rounds$round_number,as.numeric(1:38)),identical(which(rounds$is_finished%in%TRUE),1:11),
 fixture$calendar_reads==1L,fixture$db_reads==1L,fixture$writes==1L)
roster<-data.frame(id=c('a','b','missing'))
history<-data.frame(player_id=rep(c('a','b','missing'),each=6),round=rep(7:12,3),
 points=c(-2,0,4,6,7,1000,1,2,3,4,5,1000,NA,2,3,4,5,1000))
scores<-lineup_average_scores(roster,history,rounds,5L)
stopifnot(identical(attr(scores,'rounds'),as.numeric(7:11)),
 identical(scores$average[1:2],c(3,3)),is.na(scores$average[3]),scores$coverage[3]==4L)
cat('PASS actual round getter resolves compact future IDs using one bulk calendar read; exact 7:11 averages preserve zero/negative/unknown and exclude the ongoing round\n')
# Clear process state and restore a database-shaped nested JSON payload.
stamp<-fixture$stored[[1]]$observed_at
clear_api_cache();again<-read_rounds()
stopifnot(identical(again$round_number,rounds$round_number),identical(again$is_finished,rounds$is_finished),
 fixture$calendar_reads==1L,fixture$db_reads==2L,fixture$writes==1L,
 identical(as.numeric(attr(again,'observed_at')),as.numeric(fm_time(stamp))),
 abs(attr(again,'valid_until')-fixture$stored[[1]]$payload$valid_until)<1e-5)
# An ordinary calendar consumer does not require an active-round anchor and
# reuses the richer scoped observation without another read.
invisible(await(async_adapter_fetch('get_award_round_metadata',list(login=auth,championship_id=cid,league_id=lid),resource_context(auth,cid))))
stopifnot(fixture$calendar_reads==1L,fixture$db_reads==2L,fixture$writes==1L)
cat('PASS fresh durable calendar survives cleared memory, one bounded row read and no repeated source request or database write\n')
# A direct explicit refresh bypasses fresh persisted and process snapshots.
refreshed<-await(async_adapter_fetch('get_award_round_catalog',list(login=auth,championship_id=cid,refresh=TRUE),resource_context(auth,cid)))
stopifnot(refreshed$complete,fixture$calendar_reads==2L,fixture$writes==2L)
# A new current-round anchor cannot hydrate a still-fresh older calendar.
active$rounds[[1]]$`_id`<-'r13';active$rounds[[1]]$number<-13
calendar$rounds[[which(vapply(calendar$rounds,function(x)x$number==12,logical(1)))]]$status<-'done'
clear_api_cache();advanced<-read_rounds()
stopifnot(isTRUE(attr(advanced,'catalog_complete')),max(advanced$round_number[advanced$is_finished%in%TRUE])==12,
 fixture$calendar_reads==3L,fixture$writes==3L)
cat('PASS explicit refresh and changed active round acquire fresh source metadata despite a fresh database snapshot\n')
# Expired stored success must not hide a failed current acquisition.
fixture$stored[[1]]$payload$valid_until<-as.numeric(Sys.time())-1
fixture$mode<-'missing';clear_api_cache();failed<-read_rounds()
stopifnot(identical(attr(failed,'catalog_complete'),FALSE),attr(failed,'catalog_status')=='partial',
 grepl('could not be read',attr(failed,'catalog_reason'),fixed=TRUE),!any(failed$is_finished%in%TRUE),fixture$writes==3L)
fixture$stored<-list();fixture$mode<-'conflict';clear_api_cache();conflicting<-read_rounds()
stopifnot(identical(attr(conflicting,'catalog_complete'),FALSE),is.na(conflicting$round_number[1]),
 grepl('conflicting',attr(conflicting,'catalog_reason'),fixed=TRUE),fixture$writes==3L)
cat('PASS stale/unavailable and conflicting calendars stay partial, do not invent round ordinals and never replace persisted success\n')
# A malformed database payload falls through to source, and an explicitly
# unfinished older round is excluded without making completed history unknown.
fixture$mode<-'ok';fixture$stored<-list(rival_finance_evidence(auth[['userid']],cid,'unknown','eligibility',
 paste0('calendar:v1:',as.character(openssl::sha256(lid))),123,Sys.time()))
active$rounds[[1]]$`_id`<-'r12';active$rounds[[1]]$number<-12
for(number in c(10,12))calendar$rounds[[which(vapply(calendar$rounds,function(x)x$number==number,logical(1)))]]$status<-'playing'
reads_before<-fixture$calendar_reads;writes_before<-fixture$writes
clear_api_cache();postponed<-read_rounds()
selected<-lineup_average_scores(data.frame(id='a'),data.frame(player_id='a',round=6:12,points=1),postponed,5L)
stopifnot(isTRUE(attr(postponed,'catalog_complete')),identical(attr(selected,'rounds'),as.numeric(c(6:9,11))),
 selected$average==1,fixture$calendar_reads==reads_before+1L,fixture$writes==writes_before+1L)
cat('PASS malformed database data falls back to acquisition; a confirmed unfinished older round is excluded from a complete five-round window\n')
cat('LINEUP ROUND CATALOG: 5 groups passed / 0 failed\n')
