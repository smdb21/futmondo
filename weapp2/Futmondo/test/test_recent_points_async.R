options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(token='synthetic',userid='person');checks<-0L
check<-function(name,code){force(code);checks<<-checks+1L;cat('PASS',name,'\n')}
settle<-function(session){for(i in 1:20){later::run_now(timeoutSecs=0);session$flushReact()}}
scoring<-normalize_league_rules(list(),list(pointSystem='fixture'),'league')$scoring_version
players<-data.frame(id=c('p1','p2'),average.averageLastFive=c(12.4,9))
make_source<-function(player,rounds=3:10,points=c(1,-2,0,16,3,7,19,18),season='unknown'){
 ctx<-resource_context(auth,'league','team',player,season,scoring)
 observed<-data.frame(player_id=rep(player,length(rounds)),championship_id=rep('league',length(rounds)),season=rep(season,length(rounds)),scoring_version=rep(scoring,length(rounds)),round=rounds,points=points,score_status=rep('unknown',length(rounds)),observed_at=rep(format(Sys.time(),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),length(rounds)),occurred_at=rep(NA_character_,length(rounds)),round_start_at=rep(NA_character_,length(rounds)))
 list(context=ctx,observations=observed,checked_at=Sys.time(),points=reconcile_player_points(data.frame(),observed,ctx),previous_points=data.frame())
}
marker<-function(player,status='scores',season='unknown',age=0)data.frame(player_id=player,championship_id='league',season=season,scoring_version=scoring,checked_at=format(Sys.time()-age,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),result_status=status,point_count=if(status=='empty')0L else 8L)
reset<-function(points=data.frame(),saved_checks=data.frame()){
 clear_api_cache();calls<<-character();queued<<-new.env(parent=emptyenv());recorded<<-list()
 dataset<<-list(points=points,checks=saved_checks,complete=TRUE)
 .persistence_state$queue<-list();.persistence_state$active<-NULL
}
read_league_round_dataset<-function(championship_id,max_rows=50000L)dataset
get_championship_players<-function(login,championship_id)data.frame(id=paste0('p',1:4))
get_lineup_from_team<-function(login,championship_id,user_team_id)list(pointSystem='fixture')
persist_recent_points_snapshot<-function(payload,context,on_complete){recorded[[length(recorded)+1L]]<<-list(payload=payload,context=context,ack=on_complete);TRUE}
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE){
 calls<<-c(calls,paste0(fn,':',args$player_id %||% ''))
 if(fn=='get_player_recent_points')return(promises::promise(function(resolve,reject){queued[[paste(args$championship_id,args$player_id,sep=':')]]<-list(resolve=resolve,current=is_current)}))
 promises::promise_resolve(do.call(get(fn,.GlobalEnv),args))
}
server<-function(input,output,session){
 active<-reactiveVal(TRUE);league<-reactiveVal('league');refresh<-reactiveVal(0L)
 source<-recent_player_points_source(session,reactive(auth),league,reactive('team'),reactive(players),active,refresh)
}
reset()
shiny::testServer(server,{
 settle(session)
 check('one league read, no probe requirement, at most two summaries',stopifnot(sum(grepl('read_league_round_dataset',calls))==1L,sum(grepl('get_player_recent_points',calls))==2L,length(source$pending())==2L))
 heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,0);settle(session)
 check('delayed API leaves event loop available',stopifnot(heartbeat,nrow(source$history())==0L))
 for(player in c('p1','p2')){queued[[paste0('league:',player)]]$resolve(make_source(player));settle(session)}
 check('N3 and N8 use the same local signed-score matrix',stopifnot(isTRUE(all.equal(add_recent_points_average(players,source$history(),3)$recent_points_avg[1],44/3)),add_recent_points_average(players,source$history(),8)$recent_points_avg[1]==62/8))
 before<-calls;add_recent_points_average(players,source$history(),3);add_recent_points_average(players,source$history(),8);settle(session)
 check('N changes issue zero new reads',stopifnot(identical(calls,before)))
 check('successful acquisition waits for persistence acknowledgement',stopifnot(nrow(session$userData$shared_round_points$entry()$checks)==0L,source$status()$saving==2L))
 recorded[[1]]$ack(FALSE);recorded[[2]]$ack(TRUE);settle(session)
 check('failed save creates no marker; successful acknowledgement does',stopifnot(identical(as.character(session$userData$shared_round_points$entry()$checks$player_id),'p2'),source$status()$saving==0L,grepl('could not be saved',source$status()$save_error,fixed=TRUE)))
 active(FALSE);settle(session)
 check('leaving view obsoletes outstanding requests',stopifnot(!queued[['league:p3']]$current(),!queued[['league:p4']]$current()))
 before<-calls;queued[['league:p3']]$resolve(make_source('p3'));settle(session)
 check('obsolete response does not persist or start followup',stopifnot(identical(before,calls),length(recorded)==2L))
 active(TRUE);settle(session)
 check('returning reuses completed work and league read',stopifnot(sum(calls=='get_player_recent_points:p1')==1L,sum(calls=='get_player_recent_points:p2')==1L,sum(grepl('read_league_round_dataset',calls))==1L))
})
saved<-dplyr::bind_rows(make_source('p1',rounds=c(8,10),points=c(7,NA_real_))$observations,make_source('p2')$observations,make_source('p3')$observations)
reset(saved,marker('p4','empty'))
shiny::testServer(server,{
 settle(session)
 check('fresh sparse legacy scores and empty marker require zero API calls',stopifnot(!any(grepl('get_player_recent_points',calls)),source$status()$remaining==0L,nrow(source$history())==10L,is.na(add_recent_points_average(players,source$history(),3)$recent_points_avg[1])))
 refresh(1L);settle(session)
 check('manual refresh targets current table IDs only',stopifnot(sum(grepl('get_player_recent_points',calls))==2L,exists('league:p1',queued),exists('league:p2',queued),!exists('league:p3',queued),!exists('league:p4',queued)))
})
reset(saved,dplyr::bind_rows(marker('p1'),marker('p2'),marker('p3'),marker('p4','empty')))
shiny::testServer(server,{
 settle(session)
 check('new root session uses saved matrix/checks without API replay',stopifnot(sum(grepl('read_league_round_dataset',calls))==1L,!any(grepl('get_player_recent_points',calls))))
})
reset()
child<-function(id,active,rows)moduleServer(id,function(input,output,session)recent_player_points_source(session,reactive(auth),reactive('league'),reactive('team'),rows,active))
shared_server<-function(input,output,session){
 first_active<-reactiveVal(TRUE);second_active<-reactiveVal(FALSE)
 first<-child('first',first_active,reactive(data.frame(id='p1')))
 second<-child('second',second_active,reactive(data.frame(id='p2')))
}
shiny::testServer(shared_server,{
 settle(session);queued[['league:p1']]$resolve(make_source('p1'));settle(session)
 first_active(FALSE);second_active(TRUE);settle(session)
 check('page namespaces share one league read and completed history',stopifnot(sum(grepl('read_league_round_dataset',calls))==1L,sum(calls=='get_player_recent_points:p1')==1L,length(session$userData$shared_round_points$clients())==2L))
 queued[['league:p2']]$resolve(make_source('p2'));settle(session)
 check('second page joins its players from the same dataset',stopifnot(nrow(second$history())==8L))
})
reset(saved,dplyr::bind_rows(marker('p1'),marker('p2'),marker('p3'),marker('p4','empty')))
account_server<-function(input,output,session){login<-reactiveVal(auth);league<-reactive({req(valid_login(login()));'league'});team<-reactive({req(valid_login(login()));'team'});source<-recent_player_points_source(session,login,league,team,reactive(players),reactive(TRUE))}
shiny::testServer(account_server,{
 settle(session);login(NULL);settle(session)
 check('logout clears shared private history and pending state',stopifnot(nrow(source$history())==0L,is.null(session$userData$shared_round_points$entry()),length(source$pending())==0L))
})
reset();.persistence_state$queue<-rep(list(list(owner='person',bytes=100L)),20)
shiny::testServer(server,{
 settle(session)
 check('write backpressure pauses acquisition',stopifnot(!any(grepl('get_player_recent_points',calls))))
 .persistence_state$queue<-list();.persistence_state$revision(isolate(.persistence_state$revision())+1L);settle(session)
 check('write completion resumes exactly two slots',stopifnot(sum(grepl('get_player_recent_points',calls))==2L))
})
check('freshness and exact context isolation preserve sparse coverage',{
 ctx<-make_source('p1')$context;ctx$player_id<-''
 stopifnot(setequal(recent_points_saved_players(saved,ctx,marker('p4','empty')),c('p1','p2','p3','p4')))
 foreign<-ctx;foreign$season<-'another';stopifnot(!length(recent_points_saved_players(saved,foreign,marker('p4','empty'))))
 old<-saved;old$observed_at<-format(Sys.time()-7200,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC');stopifnot(!length(recent_points_saved_players(old,ctx,marker('p4','empty',age=7200))))
})
check('500-player x38-round mean remains bounded',{
 p<-data.frame(id=paste0('x',1:500));h<-data.frame(player_id=rep(p$id,each=38),round=rep(1:38,500),points=rep(1:38,500))
 elapsed<-system.time(a<-add_recent_points_average(p,h,8))['elapsed'];stopifnot(all(a$recent_points_avg==34.5),elapsed<1);cat(sprintf('500player x38round: %.3fs\n',elapsed))
})
cat(sprintf('SHARED ROUND DATASET: %d passed / 0 failed\n',checks))

# Refresh changes while optional content is hidden must survive until activation.
reset(saved,dplyr::bind_rows(marker('p1'),marker('p2'),marker('p3'),marker('p4','empty')))
shiny::testServer(server,{
 settle(session);active(FALSE);refresh(1L);settle(session);active(TRUE);settle(session)
 check('refresh while inactive is consumed when visible IDs return',stopifnot(sum(grepl('get_player_recent_points',calls))==2L,!exists('league:p3',queued)))
})
reset()
get_championship_players<-function(login,championship_id)data.frame(id=paste0('p',1:30))
publication_server<-function(input,output,session){
 publications<-0L
 source<-recent_player_points_source(session,reactive(auth),reactive('league'),reactive('team'),reactive(players),reactive(TRUE),publish_interval=60)
 observe({session$userData$shared_round_points$dataset();publications<<-publications+1L})
}
shiny::testServer(publication_server,{
 settle(session)
 for(player in c('p1','p2')){queued[[paste0('league:',player)]]$resolve(make_source(player));settle(session)}
 before<-publications
 for(player in paste0('p',3:30)){queued[[paste0('league:',player)]]$resolve(make_source(player));settle(session)}
 check('off-table responses do not reassemble or republish shared matrix per player',stopifnot(publications==before,source$status()$remaining==0L))
})
cat(sprintf('SHARED ROUND DATASET FINAL: %d passed / 0 failed\n',checks))

reset()
get_championship_players<-function(login,championship_id)data.frame(id=paste0('p',1:4))
shiny::testServer(account_server,{
 settle(session);old<-queued[['league:p1']]
 login(c(token='synthetic-b',userid='person-b'));settle(session)
 old$resolve(make_source('p1'));settle(session)
 check('account replacement discards old pending results and root private data',{
  stopifnot(session$userData$shared_round_points$entry()$context$account_id=='person-b',
   nrow(source$history())==0L,length(recorded)==0L,
   sum(grepl('read_league_round_dataset',calls))==2L,!old$current())
 })
})
cat(sprintf('SHARED ROUND DATASET COMPLETE: %d passed / 0 failed\n',checks))
