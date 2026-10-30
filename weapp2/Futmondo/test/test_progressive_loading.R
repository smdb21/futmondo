#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
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
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE){
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
     grepl('Checking availability',output$action_buttons$html,fixed=TRUE),
     identical(card_states$summary()$status,'loading'),length(calls)==6L,!card_chart_ready())
 })
 check('chart waits for the current browser identity paint acknowledgment',{
   generation<-as.character(card_paint_generation())
   session$setInputs(identity_painted=list(player='p1',generation='obsolete'));settle(session);stopifnot(!card_chart_ready())
   session$setInputs(identity_painted=list(player='p1',generation=generation));settle(session)
   stopifnot(card_chart_ready(),grepl('player_trend_plot',output$player_chart_container$html,fixed=TRUE))
 })
 summary<-list(championship=list(season='2026',competitionId='domestic'),points=list(list(round=8,points=-2,matchId='match8')))
 pending[['get_player_summary:p1']]$resolve(summary);settle(session)
 check('summary points render before stored history or profile resolves',{
   html<-output$recent_round_points$html
   stopifnot(grepl('-2 pts',html,fixed=TRUE),grepl('Loading match details',html,fixed=TRUE),
     identical(card_states$profile()$status,'loading'),!any(grepl('read_card_round_history',calls)))
 })
 pending[['get_lineup_from_team:p1']]$resolve(list(pointSystem='fixture'));settle(session)
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
 # Cache a completed history read; then close and reopen the same context.
 pending[['read_card_round_history:p1']]$resolve(data.frame())
 pending[['read_card_price_history:p1']]$resolve(data.frame());settle(session)
 selected(NULL);settle(session)
 check('closing the card makes pending composite work obsolete',{
   stopifnot(!pending[['get_acquisition_capacity:p1']]$current())
 })
 before<-calls
 selected(data.frame(id='p1',name='Immediate Player',role='defensa',team='Club A',photo='fixture',value=1000,change=0,points=8,user_team_id='',price=1000,market_inMarket=TRUE));settle(session)
 check('reopening reuses completed fresh resources',{
   stopifnot(!card_chart_ready())
   added<-calls[(length(before)+1L):length(calls)]
   if(length(calls)==length(before))added<-character()
   stopifnot(!any(grepl('get_player_summary|read_card_round_history|read_card_price_history|get_player_matches',added)))
 })
 check('explicit history refresh invalidates inner source caches without unrelated reads',{
   base<-resource_context(auth,'league','team','p1')
   raw<-list(summary=list(key='player_summary_fixture_league_team_p1',ctx=base),
     lineup=list(key='lineup_league_team',ctx=resource_context(auth,'league','team')),
     matches=list(key='player_matches_p1',ctx=resource_context(auth,player_id='p1')),
     unrelated=list(key='player_summary_fixture_league_team_p2',ctx=resource_context(auth,'league','team','p2')))
   for(item in raw)resource_store(api_cache_key(item$key,context=item$ctx),list(fixture=TRUE),item$ctx,item$key)
   before<-calls;session$setInputs(refresh_history=1);settle(session)
   stopifnot(!exists(api_cache_key(raw$summary$key,context=base),api_cache_env,inherits=FALSE),
     !exists(api_cache_key(raw$lineup$key,context=raw$lineup$ctx),api_cache_env,inherits=FALSE),
     !exists(api_cache_key(raw$matches$key,context=raw$matches$ctx),api_cache_env,inherits=FALSE),
     exists(api_cache_key(raw$unrelated$key,context=raw$unrelated$ctx),api_cache_env,inherits=FALSE))
   added<-tail(calls,length(calls)-length(before))
   stopifnot(!any(grepl('get_player_full_profile|get_acquisition_capacity|get_my_market_players',added)))
 })
})
# Login checks exercise pending duplicate protection and a superseded response.
attempts<-0L;login_resolve<-NULL
fm_async_login<-function(...){attempts<<-attempts+1L;promises::promise(function(resolve,reject)login_resolve<<-resolve)}
shiny::testServer(login_Server,{
 session$setInputs(user_name='person@example.invalid',password='fixture',login_button=1);settle(session)
 session$setInputs(login_button=2);settle(session)
 check('pending authentication allows one attempt and no token yet',stopifnot(attempts==1L,isTRUE(login_pending_RV()),is.null(login_token_RV())))
 session$setInputs(logout_button=1);settle(session)
 login_resolve(auth);settle(session)
 check('logout discards a late authentication response',stopifnot(is.null(login_token_RV()),identical(session$userData$startup_stage(),'signed_out')))
})
# Today primary data is independent, and retry only touches failed resources.
clear_api_cache();calls<-character();pending<-new.env(parent=emptyenv())
players_table_Server<-function(...)invisible(NULL)
shiny::testServer(today_Server,args=list(is_module_active=shiny::reactive(TRUE),login_token=shiny::reactive(auth),
 championship_id=shiny::reactive('league'),user_team_id=shiny::reactive('team'),user_teams_RV=shiny::reactive(stop('optional teams read'))),{
 settle(session)
 check('Today dispatches only team information and market before optional history',{
   stopifnot(length(calls)==2L,any(grepl('get_user_team_info',calls)),any(grepl('get_market_players',calls)),!primary_settled())
 })
 pending[['get_user_team_info:']]$resolve(list(budget=0,teamValue=1000));settle(session)
 check('Today cash appears while market is still pending',{
   stopifnot(grepl('0 €',output$kpi_cash_box$html,fixed=TRUE),!primary_settled(),!any(grepl('pressroom',calls)))
 })
 pending[['get_market_players:']]$reject(simpleError('synthetic timeout'));settle(session)
 check('Today reaches partial and starts optional transfers after primary failure',{
   stopifnot(primary_settled(),identical(primary_status(),'partial'),grepl('Retry market',output$market_status$html,fixed=TRUE))
   try(output$recent_transfers,silent=TRUE);settle(session)
   stopifnot(any(grepl('pressroom',calls)))
 })
 before<-calls;retry_failed();settle(session)
 check('targeted retry does not refetch a successful balance or optional transfers',{
   added<-tail(calls,length(calls)-length(before))
   stopifnot(length(added)==1L,grepl('get_market_players',added))
 })
})
cat(sprintf('PROGRESSIVE LOADING: %d passed / 0 failed\n',checks))
