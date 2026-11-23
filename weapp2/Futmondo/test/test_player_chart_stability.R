#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
get_sb_url <- function() ''
get_sb_key <- function() ''
checks<-0L;check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
settle<-function(session){for(i in seq_len(20L)){later::run_now(0);session$flushReact()}}
check('initial card shell has no chart widget dependency',{
 shell<-htmltools::renderTags(selected_player_UI('shell'))
 deps<-vapply(shell$dependencies,function(dep)dep$name,character(1))
 stopifnot(!any(deps%in%c('htmlwidgets','plotly-binding')),grepl('player_chart_container',shell$html,fixed=TRUE))
})
auth<-c(token='synthetic-only',userid='progressive-fixture',user_name='Example')
# Delayed promises exercise real shared-resource scheduling, without live I/O.
pending<-new.env(parent=emptyenv());calls<-character()
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE,...){
 key<-paste(fn,context$player_id,sep=':');calls<<-c(calls,key)
 promises::promise(function(resolve,reject){pending[[key]]<-list(resolve=resolve,reject=reject,current=is_current)})
}
record_acquired_resource<-function(...)invisible(TRUE)
selected<-shiny::reactiveVal(data.frame(id='p1',name='Immediate Player',role='defensa',team='Club A',photo='fixture',
 value=1000,change=0,points=8,user_team_id='',price=1000,market_inMarket=TRUE))
clear_api_cache()
shiny::testServer(selected_player_Server,args=list(selected_player=selected,login_token=shiny::reactive(auth),
 championship_id=shiny::reactive('league'),user_team_id=shiny::reactive('team')),{
 settle(session)
 check('identity and safe eligibility display while all sources are pending',{
   stopifnot(grepl('Immediate Player',output$player_identity_summary$html,fixed=TRUE),
     grepl('Make Market Offer',output$action_buttons$html,fixed=TRUE),
     identical(card_states$summary()$status,'loading'),length(calls)==4L,!card_chart_ready())
 })
 check('chart waits for the current browser identity paint acknowledgment',{
   generation<-as.character(card_paint_generation())
   session$setInputs(identity_painted=list(player='p1',generation='obsolete'));settle(session);stopifnot(!card_chart_ready())
   session$setInputs(identity_painted=list(player='p1',generation=generation));settle(session)
   stopifnot(card_chart_ready(),is.null(output$player_chart_container))
 })
 summary<-list(championship=list(season='2026',competitionId='domestic'),points=list(list(round=8,points=-2,matchId='match8')))
 pending[['get_player_summary:p1']]$resolve(summary);settle(session)
 check('summary points render before stored history or profile resolves',{
   html<-output$recent_round_points$html
   stopifnot(grepl('-2 pts',html,fixed=TRUE),!grepl('Match association unavailable',html,fixed=TRUE),
     grepl('player_trend_plot',output$player_chart_container$html,fixed=TRUE),
     identical(card_states$profile()$status,'loading'),!any(grepl('read_card_round_history',calls)))
 })
 pending[['get_lineup_from_team:']]$resolve(list(pointSystem='fixture'));settle(session)
 check('scoped stored points start only after summary and scoring context settle',{
   stopifnot(sum(grepl('read_card_round_history',calls))==1L,identical(history_context()$season,'2026'))
 })
 pending[['get_player_matches:p1']]$resolve(list(season='2026',competitionId='domestic',matches=list(
   list(id='match8',r=8,info=list(date='2026-10-01'),h=list(name='Home',score=2),a=list(name='Away',score=1)))))
 pending[['get_player_full_profile:p1']]$reject(simpleError('synthetic unavailable'));settle(session)
 check('round result and points share one box despite profile failure',{
   html<-output$recent_round_points$html
   stopifnot(grepl('-2 pts',html,fixed=TRUE),grepl('Home 2–1 Away',html,fixed=TRUE),
     !grepl('player-round-status',html,fixed=TRUE),identical(card_states$profile()$status,'unavailable'))
 })
 check('same player record updates keep the mounted chart and its data',{
   before_generation<-card_paint_generation();before_chart<-chart_data()
   before_container<-output$player_chart_container$html
   updated<-selected();updated$name<-'Updated display name';selected(updated);settle(session)
   stopifnot(identical(card_paint_generation(),before_generation),card_chart_ready(),
     identical(chart_data(),before_chart),identical(output$player_chart_container$html,before_container))
 })

})
