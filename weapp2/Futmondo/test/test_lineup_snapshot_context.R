#!/usr/bin/env Rscript
# Real snapshot validation/persistence with a stale shared-history scoring identity.
# All source and database transports are synthetic; no account/transaction access.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
real_lineup_api_read<-lineup_api_read
auth<-c(userid='lineup-context-account',token='synthetic-token')
active<-list(championships=list(list(id='league',league='competition',ps='press')),
 leagues=list(list(`_id`='competition',season='season')))
publish_active_league_contexts(auth,active)
ctx<-resource_context(auth,'league','team');current_scoring<-ctx$scoring_version
stopifnot(current_scoring!='unknown')
rounds<-data.frame(round_id='round',round_number=13,is_finished=FALSE,
 begin_process=format(Sys.time()+86400,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))
answers<-list(lineup=list(strategy='4-4-2',pointSystem='press',players=list(),bench=list(players=list())),
 roster=list(list(id='player',name='Player',role='portero',points=-2)),
 info=list(league=list(id='competition',season='season'),configuration=list()),
 strategies=list(strategies=list(list(st='4-4-2',pro=FALSE))))
entry<-list(context=ctx,season='season',scoring=fm_scoring_version('stats'))
reads<-writes<-source_reads<-0L;stored<-NULL;fail_write<-FALSE
get_sb_url<-function()'https://fixture.invalid';get_sb_key<-function()'synthetic-server-key'
persist_league_context<-function(...)invisible(TRUE)
lineup_api_read<-function(route,...){source_reads<<-source_reads+1L;promises::promise_resolve(answers[[switch(route,
 '/1/userteam/lineup'='lineup','/1/userteam/roster'='roster','/1/userteam/information'='info','/5/strategy/availables'='strategies')]])}
fm_async_read_http<-function(url,...){
 stopifnot(grepl('/team_lineup_snapshots',url,fixed=TRUE));reads<<-reads+1L
 rows<-if(is.null(stored))list()else list(stored)
 promises::promise_resolve(list(content=charToRaw(jsonlite::toJSON(rows,auto_unbox=TRUE,null='null'))))
}
fm_async_http<-function(url,method,body,headers,...){
 stopifnot(method=='POST',grepl('/team_lineup_snapshots',url,fixed=TRUE),!grepl('synthetic-token|synthetic-server-key',body))
 writes<<-writes+1L
 if(fail_write)return(promises::promise_reject(simpleError('Synthetic persistence outage')))
 stored<<-jsonlite::fromJSON(body,simplifyVector=FALSE)
 promises::promise_resolve(list(status_code=201L))
}
recent_player_points_source<-function(session,...){
 root<-session$rootScope();root$userData$shared_round_points<-list(entry=shiny::reactive(entry))
 list(history=shiny::reactive(data.frame()),status=shiny::reactive(list(pending=0L,remaining=0L,saving=0L,failed=0L)))
}
settle<-function(session)for(i in 1:35){later::run_now(0);session$flushReact()}
args<-list(is_module_active=function()TRUE,login_token=function()auth,
 championship_id=function()'league',user_team_id=function()'team',round_catalog=function()rounds,
 clause_transfers_source=function()list(outgoing_status='ok',outgoing_records=outgoing_clause_empty()))
shiny::testServer(lineup_Server,args=args,{
 settle(session)
 stopifnot(identical(context()$scoring_version,current_scoring),identical(source()$scoring_version,current_scoring),
  !nzchar(cache_issue()),writes==1L,reads==1L,source_reads==4L,
  identical(stored$scoring_version,current_scoring),identical(stored$payload$scoring_version,current_scoring),
  identical(entry$scoring,fm_scoring_version('stats')))
})
cat('PASS old shared-history scoring cannot replace independently known snapshot scope; actual source result is saved once\n')
stamp<-stored$observed_at
shiny::testServer(lineup_Server,args=args,{
 settle(session)
 stopifnot(!nzchar(cache_issue()),writes==1L,reads==2L,source_reads==4L,
  identical(source()$observed_at,stamp),lineup_roster_frame(source())$points==-2)
})
cat('PASS cold session reuses the persisted current-scope lineup with original observation time and zero extra source reads/writes\n')
# Persistence validation itself remains strict, independently of the corrected caller.
resolve<-function(p){result<-list(done=FALSE);promises::then(p,function(x){result<<-list(done=TRUE,value=x);NULL},function(e){result<<-list(done=TRUE,error=e);NULL});for(i in 1:30)later::run_now(0);stopifnot(result$done);result}
for(field in c('competition_id','season','scoring_version')){
 bad<-stored$payload;bad[[field]]<-'foreign'
 result<-resolve(lineup_snapshot_store(ctx,'round',bad))
 stopifnot(inherits(result$error,'error'),is.null(result$value),writes==1L)
}
cat('PASS foreign competition, season and scoring payloads remain rejected before any POST\n')
fail_write<-TRUE
result<-resolve(lineup_snapshot_store(ctx,'round',stored$payload));stopifnot(!is.null(result$error),is.null(result$value),writes==2L)
fail_write<-FALSE
result<-resolve(lineup_snapshot_store(ctx,'round',stored$payload));stopifnot(isTRUE(result$value),writes==3L)
cat('PASS failed persistence is unacknowledged and remains retryable\n')


# Production shape: source selector is blank, custom settings are complete and
# agree exactly with independently observed active-championship scoring metadata.
custom<-list(media=list(list(media='official',pct=100)),ps=NULL,stats='fixture')
active$championships[[1]]$ps<-'custom';active$championships[[1]]$custom<-custom
publish_active_league_contexts(auth,active);ctx<-resource_context(auth,'league','team')
current_scoring<-ctx$scoring_version
answers$lineup$pointSystem<-'';answers$lineup$custom<-list(stats='fixture',ps=NULL,media=list(list(pct=100,media='official')))
for(blank in list(NULL,character(),'')){
 raw<-answers$lineup;raw$pointSystem<-blank
 resolved<-lineup_resolve_scoring(raw,ctx)
 stopifnot(identical(resolved$pointSystem,'custom'),identical(fm_scoring_version(resolved$pointSystem,resolved$custom),current_scoring),
  identical(raw$pointSystem,blank))
}
for(field in c('account_id','championship_id','competition_id','season','scoring_version')){
 foreign<-ctx;foreign[[field]]<-'other'
 stopifnot(inherits(try(lineup_resolve_scoring(answers$lineup,foreign),silent=TRUE),'try-error'))
}
wrong<-answers$lineup;wrong$custom$media[[1]]$pct<-99
stopifnot(inherits(try(lineup_resolve_scoring(wrong,ctx),silent=TRUE),'try-error'))
wrong$custom<-NULL
stopifnot(inherits(try(lineup_resolve_scoring(wrong,ctx),silent=TRUE),'try-error'))
key<-fm_league_context_key(ctx$account_id,ctx$championship_id)
known_metadata<-.fm_league_contexts$items[[key]]
.fm_league_contexts$items[[key]]$complete<-FALSE
stopifnot(inherits(try(lineup_resolve_scoring(answers$lineup,ctx),silent=TRUE),'try-error'))
.fm_league_contexts$items[[key]]<-known_metadata
cat('PASS blank source selectors resolve only through complete exact-scope metadata and identical canonical custom settings\n')

lineup_api_read<-real_lineup_api_read
fm_worker_configured<-function()TRUE;fm_worker_mode<-function(account)TRUE
now<-Sys.time();worker_reads<-live_reads<-metadata_calls<-0L
clock_offsets<-c(lineup=-10,roster=-9,info=-8,strategies=-7)
valid_offsets<-c(lineup=90,roster=80,info=70,strategies=60)
worker_failure<-NULL
answer_name<-function(route)switch(route,'/1/userteam/lineup'='lineup','/1/userteam/roster'='roster',
 '/1/userteam/information'='info','/5/strategy/availables'='strategies')
fm_worker_fetch<-function(handler,args,context,...){
 stopifnot(handler=='get_worker_http_read',identical(context$scoring_version,current_scoring))
 worker_reads<<-worker_reads+1L;name<-answer_name(args$route)
 envelope<-list(answer=answers[[name]])
 attr(envelope,'observed_at')<-now+clock_offsets[[name]]
 attr(envelope,'valid_until')<-as.numeric(now+valid_offsets[[name]])
 attr(envelope,'worker_acquisition_status')<-'ok';attr(envelope,'worker_completeness')<-'unknown'
 attr(envelope,'fetch_status')<-if(valid_offsets[[name]]<0)'stale'else'ok'
 if(name=='lineup'&&!is.null(worker_failure)){
  if(worker_failure=='partial')attr(envelope,'worker_completeness')<-'partial'
  if(worker_failure=='failed')attr(envelope,'worker_acquisition_status')<-'unavailable'
  if(worker_failure=='missing-clock')attr(envelope,'observed_at')<-NULL
  if(worker_failure=='future-clock')attr(envelope,'observed_at')<-now+3600
 }
 promises::promise_resolve(envelope)
}
base_db_read<-fm_async_read_http
fm_async_read_http<-function(url,method='GET',body=NULL,headers=character(),...){
 if(grepl('/team_lineup_snapshots',url,fixed=TRUE))return(base_db_read(url,...))
 stopifnot(method=='POST',startsWith(url,'https://api.futmondo.com/'))
 live_reads<<-live_reads+1L;route<-sub('https://api.futmondo.com','',url,fixed=TRUE)
 promises::promise_resolve(list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=answers[[answer_name(route)]]),auto_unbox=TRUE,null='null'))))
}
real_remember<-remember_league_scoring
remember_league_scoring<-function(...){metadata_calls<<-metadata_calls+1L;real_remember(...)}
stored<-NULL;reads<-writes<-0L
metadata_clock<-league_context_metadata('league',auth[['userid']],allow_stale=TRUE)$observed_at
shiny::testServer(lineup_Server,args=args,{
 settle(session)
 stopifnot(!nzchar(cache_issue()),!nzchar(notice()),writes==1L,reads==1L,worker_reads==4L,live_reads==0L,
  identical(source()$scoring_version,current_scoring),isTRUE(source()$source$worker_replay),
  abs(as.numeric(fm_time(source()$observed_at))-as.numeric(now-10))<.00001,
  abs(as.numeric(fm_time(source()$expires_at))-as.numeric(now+60))<.00001,
  metadata_calls==0L,identical(league_context_metadata('league',auth[['userid']],allow_stale=TRUE)$observed_at,metadata_clock))
})
cat('PASS actual worker answer boundary preserves custom scope and oldest source clock/earliest absolute deadline; one snapshot POST and zero source/metadata writes\n')
observation<-stored$observed_at;deadline<-stored$expires_at
shiny::testServer(lineup_Server,args=args,{
 settle(session)
 stopifnot(!nzchar(cache_issue()),reads==2L,writes==1L,worker_reads==4L,live_reads==0L,
  identical(source()$observed_at,observation),identical(source()$expires_at,deadline))
})
cat('PASS second session reuses the fresh worker-derived persisted lineup without worker/source reacquisition or duplicate writes\n')

clock_offsets[]<-c(-100,-99,-98,-97);valid_offsets[]<-c(-50,-40,-30,-20)
stale<-resolve(lineup_fetch_bundle(auth,ctx,NULL,'fixture'))
stopifnot(is.null(stale$error),!lineup_snapshot_fresh(stale$value),stale$value$source$fetch_status=='stale',
 abs(as.numeric(fm_time(stale$value$observed_at))-as.numeric(now-100))<.00001,
 abs(as.numeric(fm_time(stale$value$expires_at))-as.numeric(now-50))<.00001,metadata_calls==0L)
stale_saved<-resolve(lineup_snapshot_store(ctx,'round',stale$value))
stopifnot(isTRUE(stale_saved$value),writes==2L,!lineup_snapshot_fresh(stored$payload),
 identical(stored$observed_at,stale$value$observed_at),identical(stored$expires_at,stale$value$expires_at))
cat('PASS stale worker observations retain their original expired clocks through successful persistence; replay cannot renew scoring metadata\n')
for(problem in c('partial','failed','missing-clock','future-clock')){
 worker_failure<-problem
 failed<-resolve(lineup_fetch_bundle(auth,ctx,NULL,'fixture'))
 stopifnot(!is.null(failed$error),is.null(failed$value),writes==2L,metadata_calls==0L)
}
worker_failure<-NULL
cat('PASS failed/partial source evidence and missing/future clocks cannot become successful snapshots or renew metadata\n')
prior_worker_reads<-worker_reads
live<-resolve(lineup_fetch_bundle(auth,ctx,NULL,'fixture',worker_live=TRUE))
stopifnot(is.null(live$error),live_reads==4L,worker_reads==prior_worker_reads,!isTRUE(live$value$source$worker_replay),
 lineup_snapshot_fresh(live$value),fm_time(live$value$observed_at)>=now,
 as.numeric(difftime(fm_time(live$value$expires_at),fm_time(live$value$observed_at),units='secs'))<=30.001,
 identical(league_context_metadata('league',auth[['userid']],allow_stale=TRUE)$observed_at,metadata_clock))
cat('PASS explicit live preflight bypasses all worker snapshots and preserves the 30-second live policy without inventing source scoring evidence\n')

# Bounded CPU comparison for a full 100-player source roster. Both paths use
# identical already-fetched rows; no cache/API/DB work is introduced by fallback.
benchmark_answers<-answers
benchmark_answers$roster<-lapply(1:100,function(i)list(id=paste0('player-',i),name=paste('Player',i),
 role='defensa',points=i-50,team=list(name='Club',logo='club.png'),buyPrice=100,value=200))
explicit_answers<-benchmark_answers;explicit_answers$lineup$pointSystem<-'custom'
counts_before<-c(reads,writes,worker_reads,live_reads,metadata_calls)
explicit_time<-system.time(for(i in 1:100)lineup_normalize_bundle(explicit_answers,ctx,NULL))[['elapsed']]
omitted_time<-system.time(for(i in 1:100)lineup_normalize_bundle(benchmark_answers,ctx,NULL))[['elapsed']]
stopifnot(identical(counts_before,c(reads,writes,worker_reads,live_reads,metadata_calls)))
cat(sprintf('MEASURE 100-player bundle: explicit selector %.3fms/normalization; missing selector with exact evidence %.3fms/normalization (100 runs each); zero added I/O\n',10*explicit_time,10*omitted_time))
cat('LINEUP SNAPSHOT CONTEXT: 10 groups passed / 0 failed\n')
