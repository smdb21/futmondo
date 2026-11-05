#!/usr/bin/env Rscript
# Real Players table -> selected-card wiring. All source data and saves are synthetic.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='card-saving-fixture',token='synthetic')
score<-paste0(normalize_league_rules(list(),list(pointSystem='fixture'),'league')$scoring_version)
stamp<-Sys.time()
rows<-data.frame(id=paste0('p',1:7),name=c('Berenguer',paste('Player',2:7)),
 role='defensa',team='Club',value=1000,change=0,points=2,user_team_id='',price=1000,market_inMarket=TRUE)
writes<-list();reads<-character();pending<-new.env(parent=emptyenv())
getReactableState<-function(outputId,name,session)switch(name,page=1L,pageSize=10L,selected=session$input$fixture_selected,NULL)
updateReactable<-function(...)invisible(NULL)
record_acquired_resource<-function(...)invisible(TRUE)
persist_recent_points_snapshot<-function(payload,context,on_complete){writes[[length(writes)+1L]]<<-on_complete;TRUE}
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE){
 reads<<-c(reads,fn)
 if(fn%in%c('get_player_summary','get_team_acquisition_capacity'))return(promises::promise(function(resolve,reject){pending[[fn]]<-list(resolve=resolve,reject=reject,current=is_current)}))
 value<-switch(fn,
  read_league_round_dataset=list(points=data.frame(),checks=data.frame(),complete=TRUE),
  get_championship_players=rows,
  get_lineup_from_team=list(pointSystem='fixture'),
  get_player_recent_points={
   ctx<-context;ctx$scoring_version<-score
   list(context=ctx,checked_at=stamp,observations=data.frame(player_id=ctx$player_id,
    championship_id='league',season='unknown',scoring_version=score,round=1:3,points=c(0,-2,4),
    score_status='unknown',observed_at=format(stamp,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),
    occurred_at=NA_character_,round_start_at=NA_character_))
  },
  get_player_full_profile=list(profile=list()),get_player_matches=list(matches=list()),
  read_card_round_history=data.frame(),read_card_price_history=data.frame(),
  stop(paste('Unexpected synthetic read',fn)))
 promises::promise_resolve(value)
}
settle<-function(session){for(i in seq_len(20L)){later::run_now(0);session$flushReact()}}
checks<-0L;check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
clear_api_cache()
shiny::testServer(players_table_Server,args=list(players_table_RV=shiny::reactive(rows),
 user_teams_RV=shiny::reactive(NULL),login_token=shiny::reactive(auth),
 championship_id=shiny::reactive('league'),user_team_id=shiny::reactive('mine')) ,{
 session$setInputs(recent_games_n=3L);settle(session)
 check('seven acquired histories remain visibly pending while their saves are held',{
  stopifnot(length(writes)==7L,recent_points$status()$saving==7L,
   grepl('Saving history: 7 pending.',output$recent_points_status,fixed=TRUE))
 })
 session$setInputs(fixture_selected=1L);settle(session)
 check('table click opens the captured player and dispatches availability before any save completes',{
  stopifnot(selected_player_RV()$name=='Berenguer',
   grepl('Berenguer',output[['selected_player-player_identity_summary']]$html,fixed=TRUE),
   grepl('Checking availability',output[['selected_player-action_buttons']]$html,fixed=TRUE),
   sum(reads=='get_team_acquisition_capacity')==1L,recent_points$status()$saving==7L)
 })
 pending$get_player_summary$resolve(list(championship=list(),points=list(list(round=1,points=0),list(round=2,points=-2)),prices=list()))
 pending$get_team_acquisition_capacity$resolve(list(status='ok',valid_until=as.numeric(Sys.time())+15,
  roster=list(status='ok',count=1,cap=20,effective_count=1,effective_remaining_slots=19),
  funds=list(spendable_budget=10000),outstanding=list(count=0),rules=list(minimum_bid_at_value=TRUE)))
 settle(session)
 check('verified availability renders with all seven saves still pending',{
  html<-output[['selected_player-action_buttons']]$html
  stopifnot(!grepl('Checking availability',html,fixed=TRUE),grepl('Make Market Offer',html,fixed=TRUE),
   !grepl('disabled',html,fixed=TRUE),recent_points$status()$saving==7L)
 })
 before<-reads
 for(i in seq_along(writes)){writes[[i]](TRUE);settle(session);stopifnot(recent_points$status()$saving==7L-i)}
 check('save countdown settles without restarting the card or losing signed scores',{
  stopifnot(identical(reads,before),recent_points$status()$saving==0L,
   all(c(0,-2,4)%in%recent_points$history()$points),
   !grepl('Checking availability',output[['selected_player-action_buttons']]$html,fixed=TRUE))
 })
 session$setInputs(card_closed=1);settle(session)
 stopifnot(is.null(selected_player_RV()))
})
cat(sprintf('PLAYER CARD DURING SAVES: %d passed / 0 failed\n',checks))
