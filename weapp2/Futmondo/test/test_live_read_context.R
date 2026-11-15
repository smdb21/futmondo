# Native live adapters must retain the caller's authoritative league identity
# when bypassing stale worker values, including durable history delta reuse.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
fm_worker_configured<-function()TRUE
worker_mode_original<-fm_worker_mode
fm_worker_mode<-function(id)TRUE
now<-base::Sys.time()
auth<-c(userid='live-context-fixture',token='synthetic')
metadata<-list(account_id=auth[['userid']],championship_id='league',competition_id='competition',
 season='season',scoring_version='score',observed_at=format(now-600,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),
 expires_at=format(now-300,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),complete=TRUE,payload=list())
remember_league_context(metadata)
ctx<-resource_context(auth,'league','mine','target')
await<-function(p){
 done<-FALSE;value<-failure<-NULL
 promises::then(p,function(x){value<<-x;done<<-TRUE;NULL},function(e){failure<<-e;done<<-TRUE;NULL})
 until<-base::Sys.time()+5
 while(!done&&base::Sys.time()<until)later::run_now(.01)
 stopifnot(done)
 if(!is.null(failure))stop(failure)
 value
}
read_live<-function(fn,args=list(login=auth,championship_id='league',user_team_id='mine'))
 await(async_adapter_fetch(fn,args,ctx,worker_bypass=TRUE))
get_scope_probe<-function(login,championship_id,user_team_id){
 list(current=resource_context(login,championship_id,user_team_id,player_id='another-player'),
  archive=resource_context(login,championship_id,season='unknown',scoring_version='unknown',competition_id='unknown'),
  explicit=resource_context(login,championship_id,season='archive',scoring_version='archive-score',competition_id='archive-competition'),
  other_team=resource_context(login,championship_id,'another-team'),
  foreign_account=resource_context(account_id='other',championship_id=championship_id),
  foreign_league=resource_context(login,'other-league'),
  metadata=league_context_metadata(championship_id,login[['userid']]))
}
check('live bypass retains the known caller identity without renewing expired source metadata',{
 value<-read_live('get_scope_probe')
 stopifnot(value$current$competition_id=='competition',value$current$season=='season',value$current$scoring_version=='score',
  value$current$user_team_id=='mine',value$current$player_id=='another-player',
  !isTRUE(value$metadata$complete),is.null(value$metadata$observed_at),is.null(value$metadata$expires_at),
  identical(league_context_metadata('league',auth[['userid']],allow_stale=TRUE)$observed_at,metadata$observed_at))
})
check('explicit archives and foreign account/league/team/player scopes remain isolated',{
 stopifnot(value$archive$season=='unknown',value$archive$competition_id=='unknown',value$archive$scoring_version=='unknown',
  value$explicit$season=='archive',value$explicit$scoring_version=='archive-score',value$explicit$competition_id=='archive-competition',
  value$other_team$user_team_id=='another-team',value$other_team$player_id=='',
  value$foreign_account$season=='unknown',value$foreign_league$season=='unknown')
})
check('a newly conflicting authoritative context rejects the old live request',{
 newer<-metadata;newer$season<-'next-season';newer$observed_at<-format(now,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')
 newer$expires_at<-format(now+300,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')
 remember_league_context(newer)
 error<-tryCatch(read_live('get_scope_probe'),error=identity)
 stopifnot(inherits(error,'error'),error$category=='schema')
 .fm_league_contexts$items[[fm_league_context_key(auth[['userid']],'league')]]<-metadata
})

# Use the real baseline serializer and acknowledgement, then simulate a new
# process with no memory cache. The fixture database contains factual data only.
resource<-'pressroom_league'
source_date<-function(delta)format(now+delta,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')
baseline<-data.frame(id=c('saved-head','old-kept'),created=c(source_date(-60),source_date(-864000)),
 player_id=c('owned','old'),buyer_team_id=c('mine','mine'),seller_team_id=c('',''),price=c(100,50))
baseline<-observed_data(baseline,observed_at=now-500)
attr(baseline,'full_history_at')<-now-600
attr(baseline,'source_latest_at')<-now-60
writes<-list();database<-NULL;records<-list()
defer_persistence<-function(fn,args,context,on_complete){
 stopifnot(fn=='supabase_post_direct',args[[1]]=='complete_history_baselines')
 writes[[length(writes)+1L]]<<-list(args=args,context=context)
 database<<-as.data.frame(args[[2]],stringsAsFactors=FALSE)
 on_complete(TRUE)
 TRUE
}
record_acquired_resource<-function(resource,data,context){
 records[[length(records)+1L]]<<-list(resource=resource,context=context)
 if(resource=='pressroom_league')persist_completed_history_baseline(resource,data,context)
 invisible(TRUE)
}
stopifnot(persist_completed_history_baseline(resource,baseline,ctx),length(writes)==1L)
clear_api_cache()
rm(list=ls(.history_baseline_ack),envir=.history_baseline_ack)
get_sb_url<-function()'https://fixture.invalid'
get_sb_key<-function()'synthetic-key'
get_locker_clause_payments<-function(...)observed_data(data.frame())
queries<-list();source_reads<-0L;heartbeat<-FALSE
fm_async_read_http<-function(url,method='POST',body=NULL,headers=character(),...){
 if(method=='GET'){
  parsed<-httr::parse_url(url)
  stopifnot(parsed$path=='rest/v1/complete_history_baselines')
  queries[[length(queries)+1L]]<<-parsed$query
  fields<-c('account_id','championship_id','competition_id','season','scoring_version','resource')
  matched<-all(vapply(fields,function(field)identical(parsed$query[[field]],paste0('eq.',database[[field]][1])),logical(1)))
  payload<-if(matched)jsonlite::toJSON(database,dataframe='rows',auto_unbox=TRUE)else '[]'
 }else{
  stopifnot(grepl('/pressroom$',url),jsonlite::fromJSON(body)$query$from=='')
  source_reads<<-source_reads+1L
  # Crossing the correction-overlap boundary proves completeness without
  # paginating through the old persisted event or rechecking another season.
  news<-list(list(`_id`='new-head',created=source_date(-1),`_player`=list(`_id`='new'),
    `_buyer`=list(`_id`='mine'),price=200),
   list(`_id`='boundary',created=source_date(-172800),`_player`=list(`_id`='boundary-player'),
    `_buyer`=list(`_id`='mine'),price=25))
  payload<-jsonlite::toJSON(list(answer=list(news=news)),auto_unbox=TRUE)
 }
 promises::promise(function(resolve,reject)later::later(function()resolve(list(status_code=200L,
  content=charToRaw(payload),times=list(total=.01))),.01))
}
get_baseline_probe<-function(login,championship_id,user_team_id)
 read_completed_history_baseline(paste0('pressroom_',championship_id),resource_context(login,championship_id))
check('cold live baseline restoration uses the exact database scope and original observation times',{
 later::later(function()heartbeat<<-TRUE,.001)
 restored<-read_live('get_baseline_probe')
 stopifnot(heartbeat,length(queries)==1L,source_reads==0L,is.data.frame(restored),nrow(restored)==2L,
  identical(as.numeric(attr(restored,'observed_at')),as.numeric(fm_time(database$observed_at))),
  abs(as.numeric(attr(restored,'full_history_at'))-as.numeric(now-600))<.00001,
  queries[[1]]$competition_id=='eq.competition',queries[[1]]$season=='eq.season',queries[[1]]$scoring_version=='eq.score')
})
check('native live history reuses persisted coverage, retrieves one delta page, and persists the result once',{
 value<-read_live('get_championship_pressroom',list(login=auth,championship_id='league'))
 stopifnot(nrow(value)==4L,all(c('old-kept','new-head')%in%value$id),source_reads==1L,length(queries)==1L,
  length(writes)==2L,identical(attr(value,'fetch_status'),'ok'),
  abs(as.numeric(attr(value,'full_history_at'))-as.numeric(now-600))<.00001,
  writes[[2]]$context$season=='season',writes[[2]]$context$competition_id=='competition',
  writes[[2]]$context$scoring_version=='score')
 before_reads<-source_reads;before_writes<-length(writes)
 warm<-read_live('get_championship_pressroom',list(login=auth,championship_id='league'))
 stopifnot(source_reads==before_reads,length(writes)==before_writes,nrow(warm)==4L)
})
check('a fresh process restores the acknowledged merged baseline instead of repeating a full history scan',{
 clear_api_cache();rm(list=ls(.history_baseline_ack),envir=.history_baseline_ack)
 restored<-read_live('get_baseline_probe')
 stopifnot(nrow(restored)==4L,source_reads==1L,length(queries)==2L,length(writes)==2L,
  restored$price[restored$id=='old-kept']==50,
  queries[[2]]$season=='eq.season',queries[[2]]$scoring_version=='eq.score')
})
# Submission uses the real worker-live wrapper, not the asynchronous adapter's
# clone. Its cache identity must match the just-verified asynchronous result.
fm_worker_mode<-worker_mode_original
account<-fm_worker_account_state(auth[['userid']])
fm_worker_set_policy(account,list(enabled=TRUE,auth_status='active'))
source_http<-fm_async_read_http
fm_async_read_http<-function(url,method='POST',body=NULL,headers=character(),...){
 route<-tail(strsplit(url,'/',fixed=TRUE)[[1]],1L)
 if(method=='GET'||route=='pressroom')return(source_http(url,method,body,headers,...))
 source_reads<<-source_reads+1L
 answer<-switch(route,
  information=list(budget=1000,withheld=0,teamValue=2000,maxBid=2000,configuration=list(maxPlayersInRoster=20,mnmp=.5,mbmv=FALSE)),
  roster=lapply(c('owned','old','new','boundary-player'),function(id)list(id=id,name=id,role='defensa',value=500,change=0,points=0,teamId='club')),
  players=list(list(id='target',name='Target',role='defensa',value=200,change=0,points=0,numberOfBids=0)),
  stop(paste('Unexpected source route',route)))
 promises::promise_resolve(list(status_code=200L,
  content=charToRaw(jsonlite::toJSON(list(answer=answer),auto_unbox=TRUE)),times=list(total=0)))
}
futmondo_post<-function(...)stop('A fresh asynchronous verification must avoid a synchronous source request')
check('synchronous submission reuses the exact fresh async capacity despite expired league metadata',{
 team_ctx<-ctx;team_ctx$player_id<-''
 verified<-await(async_adapter_fetch('get_team_acquisition_capacity',
  list(login=auth,championship_id='league',user_team_id='mine'),team_ctx,worker_bypass=TRUE))
 stopifnot(evaluate_acquisition_preflight(verified,'bid',amount=150)$ok)
 before_reads<-source_reads;before_db<-length(queries)
 submitted<-get_acquisition_capacity(auth,'league','mine','target')
 stopifnot(evaluate_acquisition_preflight(submitted,'bid',amount=150)$ok,
  identical(submitted$valid_until,verified$valid_until),source_reads==before_reads,length(queries)==before_db,
  !isTRUE(getOption('futmondo.worker_live',FALSE)),
  identical(league_context_metadata('league',auth[['userid']],allow_stale=TRUE)$observed_at,metadata$observed_at))
})
check('top-level capacity cache rejects conflicting scope and reuses explicitly supplied known scope',{
 team_ctx<-ctx;team_ctx$player_id<-''
 before_reads<-source_reads;before_db<-length(queries)
 conflicting<-team_ctx;conflicting$season<-'other-season'
 error<-tryCatch(await(async_adapter_fetch('get_team_acquisition_capacity',
  list(login=auth,championship_id='league',user_team_id='mine'),conflicting,worker_bypass=TRUE)),error=identity)
 stopifnot(inherits(error,'error'),error$category=='schema',source_reads==before_reads,length(queries)==before_db)
 # A canonical worker context can arrive before local league metadata. It must
 # still reuse exactly its known-scope cache, never the unknown namespace.
 clear_league_contexts(auth[['userid']])
 reused<-await(async_adapter_fetch('get_team_acquisition_capacity',
  list(login=auth,championship_id='league',user_team_id='mine'),team_ctx,worker_bypass=TRUE))
 stopifnot(identical(reused$valid_until,verified$valid_until),source_reads==before_reads,length(queries)==before_db)
})
cat(sprintf('LIVE READ CONTEXT: %d groups passed / 0 failed\n',checks))
