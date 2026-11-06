#!/usr/bin/env Rscript
# Independent public-score fixtures copied as numbers only, without HAR requests,
# source IDs, credentials, or private response bodies. All I/O is synthetic.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
settle<-function(session){for(i in seq_len(20L)){later::run_now(0);session$flushReact()}}
http_attempts<-0L
for(method in c('GET','POST','PUT','PATCH','DELETE'))suppressMessages(trace(method,where=asNamespace('httr'),print=FALSE,
 tracer=quote({http_attempts<<-http_attempts+1L;stop('Network forbidden in independent recent-average test')})))

# Captures preserve source-list order; the first two round labels in the first
# and third examples are 2,1. These vectors must not acquire invented round IDs.
fixtures<-data.frame(id=paste0('source-',1:4),
 average.fitness=c('8,3,11,11,3','0,4,7,0,4','5,14,25,22,18','7,11'),
 average.averageLastFive=c(7.2,3,16.8,9))
check('four captured source suffixes independently give 7, 2, 20 and 9',{
 x<-add_recent_points_average(fixtures,data.frame(),2L)
 stopifnot(identical(x$recent_points_avg,c(7,2,20,9)),
  all(x$recent_points_status=='reported'),all(x$recent_points_count==2L),
  !any(c('round','round_number','season','checked_at')%in%names(x)))
})
check('zero and negative source scores keep their arithmetic meaning',{
 signed<-data.frame(id=c('zero','negative'),average.fitness=c('8,3,11,0,0','5,3,7,-4,0'))
 stopifnot(identical(add_recent_points_average(signed,data.frame(),2L)$recent_points_avg,c(0,-2)))
})
check('missing suffix slots stay missing and earlier unknowns never shift the suffix',{
 unknown<-data.frame(id=paste0('missing-',1:6),
  average.fitness=c('8,3,11,NA,3','8,3,11,3,','8,3,11,null,3','8,3,11,3','NA,3,11,7,3','8,3,Inf,7,3'))
 x<-add_recent_points_average(unknown,data.frame(),2L)
 stopifnot(identical(x$recent_points_avg,c(NA_real_,NA_real_,NA_real_,7,5,NA_real_)),
  is.na(add_recent_points_average(unknown[4,,drop=FALSE],data.frame(),5L)$recent_points_avg))
})
check('a five-score scalar alone cannot imply two scores and a vector cannot supply N6',{
 aggregate<-data.frame(id='aggregate',average.averageLastFive=7.2)
 stopifnot(is.na(add_recent_points_average(aggregate,data.frame(),2L)$recent_points_avg),
  add_recent_points_average(aggregate,data.frame(),5L)$recent_points_avg==7.2,
  all(is.na(add_recent_points_average(fixtures,data.frame(),6L)$recent_points_avg)))
})
check('complete explicit numbered history retains priority over source sequence',{
 explicit<-data.frame(player_id='source-1',round=c(9,10),points=c(-2,0))
 x<-add_recent_points_average(fixtures,explicit,2L)
 stopifnot(identical(x$recent_points_avg,c(-1,2,20,9)),
  x$recent_points_status[1]=='complete',all(x$recent_points_status[-1]=='reported'))
})

auth<-c(token='synthetic',userid='source-average-fixture')
scoring<-paste0(normalize_league_rules(list(),list(pointSystem='fixture'),'league')$scoring_version)
stamp<-Sys.time()-20
rows<-fixtures[1:2,,drop=FALSE]
rows$name<-c('Synthetic A','Synthetic B');rows$role<-'defensa';rows$team<-'Club'
rows$value<-c(2e6,4e6);rows$change<-0;rows$points<-0;rows$user_team_id<-''
rows$observed_at<-format(stamp,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC')
make_payload<-function(player,rounds=integer(),scores=numeric()){
 context<-resource_context(auth,'league','team',player,scoring_version=scoring)
 observation<-data.frame(player_id=rep(player,length(rounds)),championship_id=rep('league',length(rounds)),
  season=rep('unknown',length(rounds)),scoring_version=rep(scoring,length(rounds)),round=rounds,points=scores,
  score_status=rep('unknown',length(rounds)),observed_at=rep(format(stamp+10,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),length(rounds)),
  occurred_at=rep(NA_character_,length(rounds)),round_start_at=rep(NA_character_,length(rounds)))
 list(context=context,checked_at=stamp+10,observations=observation)
}
reset<-function(){
 clear_api_cache();calls<<-character();pending<<-new.env(parent=emptyenv());saves<<-list();observation_writes<<-list()
 .persistence_state$queue<-list();.persistence_state$active<-NULL
}
getReactableState<-function(outputId,name,session)switch(name,page=1L,pageSize=20L,NULL)
updateReactable<-function(...)invisible(NULL)
record_acquired_resource<-function(...)invisible(TRUE)
persist_recent_points_snapshot<-function(payload,context,on_complete){saves[[length(saves)+1L]]<<-list(payload=payload,ack=on_complete);TRUE}
defer_persistence<-function(fn,args,context,on_complete){observation_writes[[length(observation_writes)+1L]]<<-list(fn=fn,args=args);TRUE}
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE){
 calls<<-c(calls,paste0(fn,':',args$player_id %||% ''))
 if(fn=='get_player_recent_points')return(promises::promise(function(resolve,reject){pending[[args$player_id]]<-list(resolve=resolve,current=is_current)}))
 value<-switch(fn,read_league_round_dataset=dataset,get_championship_players=rows,
  get_lineup_from_team=list(pointSystem='fixture'),stop(paste('Unexpected fixture read',fn)))
 promises::promise_resolve(value)
}
module_args<-function()list(players_table_RV=shiny::reactive(rows),user_teams_RV=shiny::reactive(NULL),
 login_token=shiny::reactive(auth),championship_id=shiny::reactive('league'),
 user_team_id=shiny::reactive('team'),points_ratio_basis='value')
dataset<-list(points=data.frame(),checks=data.frame(),complete=TRUE)
reset()
shiny::testServer(players_table_Server,args=module_args(),{
 session$setInputs(recent_games_n=5L);settle(session)
 check('actual table shows N5 while its history request remains unresolved',{
  stopifnot(identical(players_table_enriched_RV()$recent_points_avg,c(7.2,3)),
   nrow(recent_points$history())==0L,length(saves)==0L,length(pending)>0L,
   sum(grepl('read_league_round_dataset',calls))==1L)
 })
 before<-calls
 session$setInputs(recent_games_n=2L);settle(session)
 check('actual N5 to N2 table and value ratios update with no additional reads',{
  shown<-players_table_enriched_RV();widget<-output$players_table
  stopifnot(identical(shown$recent_points_avg,c(7,2)),identical(shown$recent_points_ratio,c(3.5,0.5)),
   all(shown$recent_points_status=='reported'),identical(calls,before),!is.null(widget),
   nrow(recent_points$history())==0L,length(saves)==0L,length(observation_writes)==0L)
 })
 heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,0);settle(session)
 check('held source history leaves the event loop responsive',stopifnot(heartbeat))
 # Extend the captured five-score prefix with five synthetic explicit rounds.
 # The old unlabelled vector must not anchor newer round IDs or the other player.
 pending[['source-1']]$resolve(make_payload('source-1',c(2L,1L,3L:10L),c(8,3,11,11,3,-2,0,10,20,30)));settle(session)
 pending[['source-2']]$resolve(make_payload('source-2'));settle(session)
 check('real history acquisition still saves explicit scores and the successful empty check',{
  stopifnot(length(saves)==2L,identical(vapply(saves,function(x)nrow(x$payload$observations),integer(1)),c(10L,0L)),
   length(observation_writes)==0L,recent_points$status()$saving==2L,
   nrow(session$rootScope()$userData$shared_round_points$entry()$checks)==0L)
 })
 for(save in saves)save$ack(TRUE)
 settle(session)
 check('acknowledged explicit and empty histories keep source-only fallback truthful',{
  shown<-players_table_enriched_RV();saved_checks<-session$rootScope()$userData$shared_round_points$entry()$checks
  stopifnot(identical(shown$recent_points_avg,c(25,2)),identical(shown$recent_points_status,c('complete','reported')),
   recent_points$status()$saving==0L,nrow(saved_checks)==2L,
   setequal(saved_checks$result_status,c('scores','empty')),nrow(recent_points$history())==10L)
  dataset<<-list(points=dplyr::bind_rows(lapply(saves,function(save)save$payload$observations)),checks=saved_checks,complete=TRUE)
 })
 before<-calls
 session$setInputs(recent_games_n=8L);settle(session)
 check('N8 uses eight genuine explicit rounds and retains missing source history as missing',{
  shown<-players_table_enriched_RV()
  stopifnot(identical(shown$recent_points_avg,c(83/8,NA_real_)),
   identical(shown$recent_points_ratio,c(83/16,NA_real_)),
   identical(shown$recent_points_count,c(8L,0L)),identical(calls,before))
 })
 for(n in c(5L,2L,8L,2L)){session$setInputs(recent_games_n=n);settle(session)}
 check('later N changes reuse acquired history without new requests or saves',
  stopifnot(identical(calls,before),length(saves)==2L))
})
reset()
shiny::testServer(players_table_Server,args=module_args(),{
 session$setInputs(recent_games_n=2L);settle(session)
 check('a fresh module session reuses saved scores and empty checks without a source replay',{
  stopifnot(sum(grepl('read_league_round_dataset',calls))==1L,!any(grepl('get_player_recent_points',calls)),
   length(saves)==0L,length(observation_writes)==0L,
   identical(players_table_enriched_RV()$recent_points_avg,c(25,2)))
  before<-calls;session$setInputs(recent_games_n=8L);settle(session)
  stopifnot(identical(players_table_enriched_RV()$recent_points_avg,c(83/8,NA_real_)),identical(calls,before))
 })
})

# Measure the actual source-only calculation at league size, with a separate
# small real-module measurement above covering wiring and request behavior.
benchmark<-fixtures[rep(seq_len(nrow(fixtures)),length.out=527L),,drop=FALSE]
benchmark$id<-paste0('benchmark-',seq_len(nrow(benchmark)))
expected<-rep(c(7,2,20,9),length.out=527L)
windows<-rep(c(5L,2L,4L,2L),5L)
timing<-system.time({
 for(n in windows){result<-add_recent_points_average(benchmark,data.frame(),n)
  if(n==2L)stopifnot(identical(result$recent_points_avg,expected))}
})[['elapsed']]
check('527 players across 20 source-only N changes retain known answers',stopifnot(nrow(result)==527L,is.numeric(result$recent_points_avg)))
cat(sprintf('PERFORMANCE: 527 players x20 N changes: %.3fs total, %.3fs mean calculation; no API/database work in the calculation.\n',timing,timing/length(windows)))
check('all independent checks made zero network attempts',stopifnot(http_attempts==0L))
for(method in c('GET','POST','PUT','PATCH','DELETE'))suppressMessages(untrace(method,where=asNamespace('httr')))
cat(sprintf('INDEPENDENT SOURCE AVERAGES: %d passed / 0 failed\n',checks))
