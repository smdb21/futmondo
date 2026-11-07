#!/usr/bin/env Rscript
# Held and delayed synthetic sources exercise card work without external I/O.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L;check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
settle<-function(session){for(i in seq_len(20L)){later::run_now(0);session$flushReact()}}
auth<-shiny::reactiveVal(c(userid='card-work-fixture',token='synthetic'))
league<-shiny::reactiveVal('league');team<-shiny::reactiveVal('mine');selected<-shiny::reactiveVal(NULL)
pending<-list();calls<-list();box_updates<-0L;action_builds<-0L
# Count construction of the actual action renderer, without changing its result.
renderUI<-function(expr,...,env=parent.frame(),quoted=FALSE){
 expression<-if(quoted)expr else substitute(expr)
 if(identical(expression,quote(action_buttons)))action_builds<<-action_builds+1L
 shiny::renderUI(expression,...,env=env,quoted=TRUE)
}
trace('updateBox',where=asNamespace('shinydashboardPlus'),print=FALSE,
 tracer=quote(if(identical(id,'selected_player_box')).GlobalEnv$box_updates<-.GlobalEnv$box_updates+1L))
record_acquired_resource<-function(...)invisible(TRUE)
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE){
 index<-length(calls)+1L;calls[[index]]<<-list(fn=fn,context=context)
 promises::promise(function(resolve,reject){pending[[index]]<<-list(resolve=resolve,reject=reject,current=is_current)})
}
indices<-function(fn)which(vapply(calls,function(call)identical(call$fn,fn),logical(1)))
resolve<-function(fn,value){pending[[tail(indices(fn),1L)]]$resolve(value)}
player<-function(id,owner='')data.frame(id=id,name=paste('Player',id),role='defensa',team='Club',photo='fixture',
 value=1000,change=0,points=-2,user_team_id=owner,price=1000,market_inMarket=TRUE)
clear_api_cache()
shiny::testServer(selected_player_Server,args=list(selected_player=selected,login_token=auth,
 championship_id=league,user_team_id=team),{
 opened<-Sys.time();selected(player('p1'));settle(session)
 check('identity renders while every source remains pending',{
  stopifnot(grepl('Player p1',output$player_identity_summary$html,fixed=TRUE),length(calls)==6L,
   identical(card_states$lineup()$status,'loading'),identical(card_states$profile()$status,'loading'))
 })
 resolve('get_player_summary',observed_data(list(championship=list(season='2026'),points=list(list(round=1,points=-2)))))
 resolve('get_team_acquisition_capacity',list(status='ok',valid_until=as.numeric(Sys.time())+60,
  roster=list(status='ok',count=1,cap=20,effective_count=1,effective_remaining_slots=19),
  funds=list(spendable_budget=10000),outstanding=list(count=0),rules=list(minimum_bid_at_value=TRUE)))
 settle(session);available_ms<-as.numeric(difftime(Sys.time(),opened,units='secs'))*1000
 check('verified actions and summary scores render before optional history',{
  stopifnot(grepl('Make Market Offer',output$action_buttons$html,fixed=TRUE),
   !grepl('Checking availability|disabled',output$action_buttons$html),
   grepl('-2 pts',output$recent_round_points$html,fixed=TRUE),
   identical(card_states$lineup()$status,'loading'),identical(card_states$valuation()$status,'loading'))
 })
 before_actions<-action_builds
 # Each optional completion occurs in a separate event-loop turn.
 for(source in c('get_lineup_from_team','get_player_full_profile','get_player_matches','read_card_price_history','read_card_round_history')){
  done<-FALSE
  value<-switch(source,get_lineup_from_team=list(pointSystem='fixture'),
   get_player_full_profile=list(profile=list()),get_player_matches=list(matches=list()),data.frame())
  later::later(local({name<-source;answer<-value;function(){resolve(name,answer);done<<-TRUE}}),.04)
  deadline<-Sys.time()+2
  while(!done&&Sys.time()<deadline){later::run_now(.005);session$flushReact()}
  stopifnot(done);settle(session)
 }
 optional_ms<-as.numeric(difftime(Sys.time(),opened,units='secs'))*1000
 cat(sprintf('MEASURE card_available_ms=%.0f optional_complete_ms=%.0f box_updates=%d optional_action_rebuilds=%d source_reads=%d\n',
  available_ms,optional_ms,box_updates,action_builds-before_actions,length(calls)))
 check('optional completions do not rebuild the header or action controls',{
  stopifnot(box_updates==1L,action_builds==before_actions,
   grepl('-2 pts',output$recent_round_points$html,fixed=TRUE))
 })
 # Navigation retains a team read even while the clicked player changes.
 selected(player('own1','mine'));settle(session)
 listing<-tail(indices('get_my_market_players'),1L)
 selected(player('own2','mine'));settle(session)
 check('own-player navigation reuses the pending team lineup and listings',{
  stopifnot(length(indices('get_lineup_from_team'))==1L,length(indices('get_my_market_players'))==1L,
   pending[[listing]]$current(),calls[[listing]]$context$player_id=='')
 })
 league('other-league');settle(session)
 check('team reads remain isolated when the league changes',{
  stopifnot(length(indices('get_lineup_from_team'))==2L,length(indices('get_my_market_players'))==2L,
   !pending[[listing]]$current(),tail(calls[indices('get_my_market_players')],1L)[[1]]$context$championship_id=='other-league')
 })
 team('other-team');selected(player('own2','other-team'));settle(session)
 auth(c(userid='other-account',token='synthetic'));settle(session)
 check('team and account changes each acquire their own isolated team data',{
  stopifnot(length(indices('get_lineup_from_team'))==4L,length(indices('get_my_market_players'))==4L,
   tail(calls[indices('get_my_market_players')],1L)[[1]]$context$user_team_id=='other-team',
   tail(calls[indices('get_my_market_players')],1L)[[1]]$context$account_id=='other-account')
 })
 selected(NULL);settle(session)
 check('closing the card makes its pending team requests obsolete',{
  stopifnot(!tail(pending[indices('get_my_market_players')],1L)[[1]]$current())
 })
})
untrace('updateBox',where=asNamespace('shinydashboardPlus'))
cat(sprintf('PLAYER CARD LOADING WORK: %d passed / 0 failed\n',checks))
