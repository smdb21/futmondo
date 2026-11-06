#!/usr/bin/env Rscript
# Regression: a saved successful five-round check cannot suppress demand for
# missing older rounds. All source and persistence operations are synthetic.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
settle<-function(session){for(i in seq_len(20L)){later::run_now(0);session$flushReact()}}
http_attempts<-0L
for(method in c('GET','POST','PUT','PATCH','DELETE'))suppressMessages(trace(method,where=asNamespace('httr'),print=FALSE,
 tracer=quote({http_attempts<<-http_attempts+1L;stop('Network forbidden in wider-demand regression')})))
auth<-c(token='synthetic',userid='wider-demand-fixture')
score<-paste0(normalize_league_rules(list(),list(pointSystem='fixture'),'league')$scoring_version)
stamp<-Sys.time()-90*86400
scores<-c(1,-2,0,16,3,7,19,18,6,10)
players<-data.frame(id=c('needs-older','already-covered'),average.fitness='7,19,18,6,10',
 observed_at=format(Sys.time()-20,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'))
catalog<-rbind(players,transform(players[1,,drop=FALSE],id='offscreen'))
observations<-function(player,rounds=1:10,time=stamp){
 data.frame(player_id=rep(player,length(rounds)),championship_id=rep('league',length(rounds)),
  season=rep('unknown',length(rounds)),scoring_version=rep(score,length(rounds)),round=rounds,points=scores[rounds],
  score_status=rep('unknown',length(rounds)),observed_at=rep(format(time,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),length(rounds)),
  occurred_at=rep(NA_character_,length(rounds)),round_start_at=rep(NA_character_,length(rounds)))
}
payload<-function(player='needs-older',rounds=1:10){
 context<-resource_context(auth,'league','team',player,scoring_version=score)
 list(context=context,checked_at=Sys.time(),observations=observations(player,rounds,Sys.time()))
}
marker<-function(player,count=5L,time=stamp){
 data.frame(player_id=player,championship_id='league',season='unknown',scoring_version=score,
  checked_at=format(time,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),result_status='scores',point_count=count)
}
original_points<-dplyr::bind_rows(observations('needs-older',6:10),observations('already-covered'),observations('offscreen',6:10))
original_checks<-dplyr::bind_rows(marker('needs-older'),marker('already-covered',10L),marker('offscreen'))
reset<-function(points=original_points,saved_checks=original_checks){
 clear_api_cache();calls<<-character();pending<<-new.env(parent=emptyenv());saves<<-list()
 dataset<<-list(points=points,checks=saved_checks,complete=TRUE)
 .persistence_state$queue<-list();.persistence_state$active<-NULL
}
read_league_round_dataset<-function(...)dataset
get_championship_players<-function(...)catalog
get_lineup_from_team<-function(...)list(pointSystem='fixture')
persist_recent_points_snapshot<-function(payload,context,on_complete){
 saves[[length(saves)+1L]]<<-list(payload=payload,context=context,ack=on_complete);TRUE
}
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE){
 calls<<-c(calls,paste0(fn,':',args$player_id %||% ''))
 if(fn=='get_player_recent_points')return(promises::promise(function(resolve,reject){pending[[args$player_id]]<-list(resolve=resolve,current=is_current)}))
 promises::promise_resolve(do.call(get(fn,.GlobalEnv),args))
}
initial_n<-5L
server<-function(input,output,session){
 n<-reactiveVal(initial_n);active<-reactiveVal(TRUE)
 source<-recent_player_points_source(session,reactive(auth),reactive('league'),reactive('team'),
  reactive(players),active,required_rounds=n)
}
reset()
shiny::testServer(server,{
 settle(session)
 check('N5 reuses the stored five-round check and current agreeing source vector',{
  stopifnot(!any(grepl('get_player_recent_points',calls)),sum(grepl('read_league_round_dataset',calls))==1L,
   identical(add_recent_points_average(players,source$history(),5L)$recent_points_avg,c(12,12)))
 })
 n(8L);settle(session)
 check('N8 starts one targeted source read despite the older five-round success marker',{
  stopifnot(identical(calls[grepl('get_player_recent_points',calls)],'get_player_recent_points:needs-older'),
   length(source$pending())==1L,!exists('offscreen',pending),!exists('already-covered',pending),
   is.na(add_recent_points_average(players,source$history(),8L)$recent_points_avg[1]))
 })
 heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,0);settle(session)
 check('held history read remains nonblocking and does not get duplicated by N changes',{
  before<-calls;n(2L);settle(session);n(8L);settle(session)
  stopifnot(heartbeat,identical(calls,before))
 })
 pending[['needs-older']]$resolve(payload());settle(session)
 check('full returned history supplies exact signed N8 arithmetic and enters persistence',{
  stopifnot(identical(add_recent_points_average(players,source$history(),8L)$recent_points_avg,c(79/8,79/8)),
   length(saves)==1L,nrow(saves[[1]]$payload$observations)==10L,source$status()$saving==1L)
  old<-session$userData$shared_round_points$entry()$checks
  stopifnot(old$point_count[old$player_id=='needs-older']==5L)
 })
 saves[[1]]$ack(TRUE);settle(session)
 check('new successful coverage becomes durable only after acknowledgement',{
  saved_checks<-session$userData$shared_round_points$entry()$checks
  stopifnot(saved_checks$point_count[saved_checks$player_id=='needs-older']==10L,source$status()$saving==0L)
  durable_points<<-dplyr::bind_rows(original_points[original_points$player_id!='needs-older',,drop=FALSE],saves[[1]]$payload$observations)
  durable_checks<<-saved_checks
 })
 before<-calls
 for(wanted in c(2L,5L,8L,10L,8L)){n(wanted);settle(session)}
 active(FALSE);settle(session);active(TRUE);settle(session)
 check('repeated N changes and navigation reuse the acquired history',stopifnot(identical(calls,before),length(saves)==1L))
})
initial_n<-8L
reset(durable_points,durable_checks)
shiny::testServer(server,{
 settle(session)
 check('a new session reuses acknowledged eight-round coverage with zero source replay',{
  stopifnot(sum(grepl('read_league_round_dataset',calls))==1L,!any(grepl('get_player_recent_points',calls)),
   !length(saves),identical(add_recent_points_average(players,source$history(),8L)$recent_points_avg,c(79/8,79/8)))
 })
})
reset()
shiny::testServer(server,{
 settle(session)
 check('starting directly at N8 also requests only the uncovered visible player',
  stopifnot(identical(calls[grepl('get_player_recent_points',calls)],'get_player_recent_points:needs-older')))
 pending[['needs-older']]$resolve(payload(rounds=6:10));settle(session)
 saves[[1]]$ack(TRUE);settle(session)
 before<-calls
 for(wanted in c(5L,8L,10L,8L)){n(wanted);settle(session)}
 active(FALSE);settle(session);active(TRUE);settle(session)
 check('a genuinely sparse successful response remains missing without a same-session replay',{
  stopifnot(identical(calls,before),length(saves)==1L,
   is.na(add_recent_points_average(players,source$history(),8L)$recent_points_avg[1]),
   source$status()$saving==0L,source$status()$pending==0L)
 })
})
check('wider-window regression attempted zero external requests',stopifnot(http_attempts==0L))
for(method in c('GET','POST','PUT','PATCH','DELETE'))suppressMessages(untrace(method,where=asNamespace('httr')))
cat(sprintf('WIDER HISTORY DEMAND: %d passed / 0 failed\n',checks))
