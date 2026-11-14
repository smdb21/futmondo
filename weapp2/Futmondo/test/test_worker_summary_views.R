options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
source('worker/summary_views.R')
for(expr in parse('worker/helper.R'))if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
 as.character(expr[[2]])%in%c('fm_worker_run','fm_worker_discover','fm_worker_database_rows'))eval(expr,.GlobalEnv)
ctx<-resource_context(account_id='summary-fixture',championship_id='league',user_team_id='team',player_id='player',
 competition_id='competition',season='season',scoring_version='score')
stamp<-as.POSIXct(floor(as.numeric(Sys.time()))-3,origin='1970-01-01',tz='UTC')
summary<-observed_data(list(data=list(id='player',name='Player',role='MID',team='Club',logo='club'),
 championship=list(season='season',owner=list(id='team',name='Team'),clause=list(transferred=TRUE,transferDate='2026-11-20T20:00:00Z')),
 points=list(list(round=1,points=0,isFinished=TRUE),list(round=2,points=-2,isFinished=TRUE))),observed_at=stamp)
source_reads<-0L;writes<-list();saved_summary<-NULL;fail_persist<-FALSE
get_player_summary<-function(login,championship_id,user_team_id,player_id){source_reads<<-source_reads+1L;summary}
fm_pipe_exchange<-function(message){
 if(message$type=='resource')return(if(is.null(saved_summary))list(value=NULL)else list(value=fm_worker_encode(saved_summary),observed_at=rival_finance_stamp(stamp)))
 if(message$type=='persist'){
  writes[[length(writes)+1L]]<<-message
  if(fail_persist)return(list(ok=FALSE))
  return(list(ok=TRUE))
 }
 stop('Unexpected acquisition: ',message$type)
}
args<-list(championship_id='league',user_team_id='team',player_id='player')
d<-fm_worker_descriptor('get_player_summary',args,ctx)
result<-fm_worker_run(d)
children<-setNames(result$resources,vapply(result$resources,function(x)x$descriptor$handler,character(1)))
stopifnot(source_reads==1L,all(c('get_player_recent_points','get_clause_transfer_summary')%in%names(children)))
decode<-function(x)fm_worker_decode(jsonlite::fromJSON(fm_worker_json(x),simplifyVector=TRUE,simplifyDataFrame=FALSE,simplifyMatrix=FALSE))
points<-decode(children$get_player_recent_points$value)
clause<-decode(children$get_clause_transfer_summary$value)
stopifnot(isTRUE(attr(points,'worker_history_persisted')),identical(as.numeric(points$points$points),c(0,-2)),identical(as.numeric(points$checked_at),as.numeric(stamp)),
 identical(clause$championship$clause$transferDate,'2026-11-20T20:00:00Z'),
 attr(clause,'valid_until')==as.numeric(stamp)+300,
 children$get_player_recent_points$descriptor$resource_key==fm_worker_descriptor('get_player_recent_points',c(args,list(scoring_version='score')),ctx)$resource_key,
 children$get_clause_transfer_summary$descriptor$resource_key==fm_worker_descriptor('get_clause_transfer_summary',args,ctx)$resource_key)
tables<-vapply(writes,`[[`,character(1),'table')
stopifnot(identical(tables,c('player_match_observations','player_round_history_checks','rival_finance_evidence')),
 all(writes[[1]]$payload$points%in%c(0,-2)),writes[[2]]$payload$point_count==2L,
 abs(as.numeric(fm_time(writes[[2]]$payload$checked_at))-as.numeric(stamp))<1e-6)
cat('PASS one autonomous summary publishes exact recent-points/clause descriptors and acknowledges scores, completion marker and clause evidence; zero extra source reads\n')
# The shared transformation agrees with the getter and preserves negative/zero.
base_getter<-get_player_recent_points
metadata<-list(account_id='summary-fixture',championship_id='league',competition_id='competition',season='season',
 scoring_version='score',observed_at=rival_finance_stamp(stamp),expires_at=rival_finance_stamp(stamp+300),complete=TRUE,payload=list())
remember_league_context(metadata)
direct<-base_getter(c(userid='summary-fixture',token='synthetic'),'league','team','player','score')
stopifnot(!isTRUE(attr(direct,'worker_history_persisted')),identical(direct$observations,points$observations),identical(direct$points,points$points))
cat('PASS known-answer direct and worker transforms retain zero, negative scores and original source time\n')
# Cached nested summary reuse must still make display variants available.
get_worker_summary_parent<-function(login,championship_id,user_team_id,player_id){
 value<-get_player_summary(login,championship_id,user_team_id,player_id);list(id=value$data$id)
}
saved_summary<-summary;before<-source_reads;writes<-list()
parent<-fm_worker_descriptor('get_worker_summary_parent',args,ctx)
result<-fm_worker_run(parent)
stopifnot(source_reads==before,length(result$resources)==2L,length(writes)==3L)
cat('PASS a nested persisted summary produces both child views and acknowledged facts without source acquisition\n')
# Writes cannot be silently declared complete if an acknowledgement fails.
saved_summary<-NULL;fail_persist<-TRUE
stopifnot(inherits(try(fm_worker_run(d),silent=TRUE),'try-error'))
fail_persist<-FALSE
for(kind in c('player','season')){
 bad<-summary
 if(kind=='player')bad$data$id<-'foreign' else bad$championship$season<-'foreign'
 stopifnot(inherits(try(fm_worker_summary_views(bad,ctx),silent=TRUE),'try-error'))
}
for(kind in c('stale','partial')){
 bad<-summary;attr(bad,'fetch_status')<-kind
 stopifnot(!length(fm_worker_summary_views(bad,ctx)))
}
cat('PASS failed persistence, foreign player/season, stale or partial summaries cannot certify derived observations\n')
