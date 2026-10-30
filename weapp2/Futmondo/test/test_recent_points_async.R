options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
source('Modules/Players_Table_Module.R')
recent_average_exact<-add_recent_points_average
auth<-c(token='synthetic',userid='person')
checks<-0L
check<-function(name,code){force(code);checks<<-checks+1L;cat('PASS',name,'\n')}
settle<-function(session){for(i in 1:25){later::run_now(timeoutSecs=0);session$flushReact()}}
players<-data.frame(id=c('p1','p2'),average.averageLastFive=c(12.4,9))
scoring<-normalize_league_rules(list(),list(pointSystem='fixture'),'league')$scoring_version
matrix<-data.frame();attr(matrix,'complete')<-TRUE
calls<-character();queued<-new.env(parent=emptyenv());recorded<-list()
record_acquired_resource<-function(resource,data,context){recorded[[length(recorded)+1L]]<<-list(data=data,context=context)}
read_league_round_points<-function(championship_id,max_rows=50000L)matrix
get_championship_players<-function(login,championship_id)data.frame(id=paste0('p',1:4))
get_lineup_from_team<-function(login,championship_id,user_team_id)list(pointSystem='fixture')
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE){
 calls<<-c(calls,paste0(fn,':',args$player_id %||% ''))
 if(fn=='get_player_recent_points')return(promises::promise(function(resolve,reject){queued[[paste(args$championship_id,args$player_id,sep=':')]]<-list(resolve=resolve,current=is_current)}))
 promises::promise_resolve(do.call(get(fn,.GlobalEnv),args))
}
make_source<-function(player,league='league',rounds=3:10,points=c(1,-2,0,16,3,7,19,18),season='2026'){
 ctx<-resource_context(auth,league,'team',player,season,scoring)
 observed<-data.frame(player_id=player,championship_id=league,season=season,scoring_version=rep(scoring,length(rounds)),
  round=rounds,points=points,score_status='unknown',observed_at='2026-10-30T12:00:00Z')
 list(context=ctx,observations=observed,points=reconcile_player_points(data.frame(),observed,ctx),previous_points=data.frame())
}
server<-function(input,output,session){
 active<-reactiveVal(TRUE);league<-reactiveVal('league')
 source<-recent_player_points_source(session,reactive(auth),league,reactive('team'),reactive(players),active)
}
shiny::testServer(server,{
 settle(session)
 check('one matrix/catalog/lineup read and only two summary requests pending',{
  stopifnot(sum(grepl('read_league_round_points',calls))==1L,sum(grepl('get_championship_players',calls))==1L,
   sum(grepl('get_lineup_from_team',calls))==1L,sum(grepl('get_player_recent_points',calls))==2L,length(source$pending())==2L)
 })
 check('delayed requests leave event loop available',{
  heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,0);settle(session);stopifnot(heartbeat,nrow(source$history())==0L)
 })
 queued[['league:p1']]$resolve(make_source('p1'));settle(session)
 check('first source fills exact N3 and N8 from one observation set',{
  h<-source$history();a<-recent_average_exact(players,h,3);b<-recent_average_exact(players,h,8)
  stopifnot(isTRUE(all.equal(a$recent_points_avg[1],44/3)),b$recent_points_avg[1]==62/8,
   a$recent_points_count[1]==3L,b$recent_points_count[1]==8L,length(recorded)==1L)
 })
 check('bounded finite league fill proceeds beyond table rows',{
  stopifnot(exists('league:p3',queued),!exists('league:p4',queued),length(source$pending())==2L)
 })
 before<-calls;a<-recent_average_exact(players,source$history(),3);b<-recent_average_exact(players,source$history(),8);settle(session)
 check('changing N needs no repeat summary or matrix reads',stopifnot(identical(before,calls)))
 active(FALSE);settle(session)
 check('navigation makes outstanding work obsolete',stopifnot(!queued[['league:p2']]$current(),!queued[['league:p3']]$current()))
 before<-calls;queued[['league:p2']]$resolve(make_source('p2'));queued[['league:p3']]$resolve(make_source('p3'));settle(session)
 check('late responses are discarded with no subsequent requests or persistence',{
  stopifnot(identical(before,calls),length(source$sources())==0L,length(recorded)==1L,!exists('league:p4',queued))
 })
})
check('missing round has explicit coverage and no shorter-window average',{
 h<-data.frame(player_id='p1',round=c(3:7,9:10),points=c(1,-2,0,16,3,19,18))
 a<-recent_average_exact(players,h,8)
 stopifnot(is.na(a$recent_points_avg[1]),a$recent_points_count[1]==7L,a$recent_points_required[1]==8L)
})
check('true zero and negative points remain in the denominator',{
 h<-data.frame(player_id='p1',round=1:3,points=c(-2,0,8))
 stopifnot(recent_average_exact(players,h,3)$recent_points_avg[1]==2)
})
check('player without latest league round is incomplete',{
 h<-data.frame(player_id=c('p1','p1','p1','p2','p2','p2'),round=c(8,9,10,7,8,9),points=c(7,19,18,1,2,3))
 a<-recent_average_exact(players,h,3);stopifnot(a$recent_points_count[2]==2L,is.na(a$recent_points_avg[2]))
})
cat(sprintf('RECENT ROUND SOURCE: %d passed / 0 failed\n',checks))

# Integrate the full unchanged table/filter/card shell with delayed sources.
clear_api_cache();calls<-character();queued<-new.env(parent=emptyenv());recorded<-list()
fixture_rows<-data.frame(id=c('p1','p2'),name=c('One','Two'),role='Forward',role2=NA_character_,
 value=1e6,change=0,points=20,average.averageLastFive=c(12.4,9))
get_financial_snapshot<-function(...)stop('Unexpected financial snapshot on average path')
shiny::testServer(players_table_Server,args=list(players_table_RV=reactive(fixture_rows),
 user_teams_RV=reactive(data.frame()),login_token=reactive(auth),championship_id=reactive('league'),
 user_team_id=reactive('team'),is_module_active=reactive(TRUE)),{
 session$setInputs(recent_games_n=3);settle(session)
 check('full table renders while summaries are delayed with no finance read',{
  stopifnot(!is.null(output$players_table),all(is.na(players_table_filtered_RV()$recent_points_avg)),
   grepl('Loading round points',output$recent_points_status$html,fixed=TRUE),length(recent_points$pending())==2L)
 })
 queued[['league:p1']]$resolve(make_source('p1'));settle(session)
 check('full table updates N3 when first source arrives',{
  stopifnot(isTRUE(all.equal(players_table_filtered_RV()$recent_points_avg[1],44/3)))
 })
 before<-calls;session$setInputs(recent_games_n=8);settle(session)
 check('full table N8 reuses observations and renders known value',{
  stopifnot(players_table_filtered_RV()$recent_points_avg[1]==62/8,identical(before,calls),!is.null(output$players_table))
 })
})
cat(sprintf('INTEGRATED RECENT POINTS: %d passed / 0 failed\n',checks))

clear_api_cache();calls<-character();queued<-new.env(parent=emptyenv());recorded<-list()
.persistence_state$queue<-rep(list(list(owner='person',bytes=100L)),20)
shiny::testServer(server,{
 settle(session)
 check('write-queue backpressure pauses acquisition before owner limit',{
  stopifnot(sum(grepl('get_player_recent_points',calls))==0L,source$status()$saving==20L)
 })
 .persistence_state$queue<-list()
 .persistence_state$revision(isolate(.persistence_state$revision())+1L);settle(session)
 check('write-completion signal resumes two summary slots without polling',{
  stopifnot(sum(grepl('get_player_recent_points',calls))==2L,length(source$pending())==2L)
 })
 for(player in paste0('p',1:4)){
  queued[[paste0('league:',player)]]$resolve(make_source(player));settle(session)
 }
 check('finite fill rereads matrix once after saving settles',{
  stopifnot(sum(grepl('read_league_round_points',calls))==2L,source$status()$pending==0L,
   source$status()$remaining==0L,length(recorded)==4L)
 })
 before<-calls;settle(session)
 check('settled fill does not loop or issue repeated matrix reads',stopifnot(identical(calls,before)))
})
cat(sprintf('RECENT POINTS WITH BACKPRESSURE: %d passed / 0 failed\n',checks))

clear_api_cache();calls<-character();queued<-new.env(parent=emptyenv());recorded<-list()
shiny::testServer(server,{
 settle(session);session$close()
 for(player in c('p1','p2'))queued[[paste0('league:',player)]]$resolve(make_source(player))
 for(i in 1:20)later::run_now(timeoutSecs=0)
 check('session close discards responses before any reactive access or persistence',{
  stopifnot(length(recorded)==0L,!exists('league:p3',queued))
 })
})
check('500-player matrix averaging is bounded and known-answer correct',{
 players500<-data.frame(id=paste0('x',1:500))
 history500<-data.frame(player_id=rep(players500$id,each=38),round=rep(1:38,500),points=rep(1:38,500))
 elapsed<-system.time(result<-recent_average_exact(players500,history500,8))['elapsed']
 stopifnot(all(result$recent_points_avg==34.5),elapsed<1)
 cat(sprintf('500player x38round average: %.3fs\n',elapsed))
})
cat(sprintf('RECENT POINTS ASYNC: %d passed / 0 failed\n',checks))

check('latest unknown round remains in the requested window',{
 h<-data.frame(player_id='p1',round=8:9,points=c(7,19));attr(h,'latest_round')<-10
 a<-recent_average_exact(players,h,3)
 stopifnot(is.na(a$recent_points_avg[1]),a$recent_points_count[1]==2L)
})
clear_api_cache();calls<-character();queued<-new.env(parent=emptyenv());recorded<-list()
refresh_server<-function(input,output,session){
 refresh<-reactiveVal(0L)
 source<-recent_player_points_source(session,reactive(auth),reactive('league'),reactive('team'),reactive(players),reactive(TRUE),refresh)
}
shiny::testServer(refresh_server,{
 settle(session)
 for(player in paste0('p',1:4)){queued[[paste0('league:',player)]]$resolve(make_source(player));settle(session)}
 before<-sum(grepl('get_player_recent_points',calls));refresh(1L);settle(session)
 check('explicit refresh invalidates current-league source caches',{
  stopifnot(sum(grepl('get_player_recent_points',calls))==before+2L,length(source$pending())==2L)
 })
})
clear_api_cache();calls<-character();queued<-new.env(parent=emptyenv());recorded<-list()
matrix<-dplyr::bind_rows(lapply(paste0('p',1:4),function(player)make_source(player,rounds=1:10,points=1:10)$observations))
attr(matrix,'complete')<-TRUE
shiny::testServer(server,{
 settle(session);queued[['league:p1']]$resolve(make_source('p1',rounds=1:10,points=1:10));settle(session)
 check('current source context reuses complete saved series for other players',{
  stopifnot(!exists('league:p3',queued),!exists('league:p4',queued),
   recent_average_exact(players,source$history(),8)$recent_points_avg[2]==6.5)
 })
})
cat(sprintf('RECENT POINTS FINAL: %d passed / 0 failed\n',checks))
