# Real root projection and Admin server, synthetic states only.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='settings-fixture',token='synthetic')
fm_admin_access<-function(...)NULL;fm_admin_timezones<-function(...)NULL
shared_mvp_source<-function(...)reactiveVal(list(coverage=data.frame(),loading=FALSE))
io<-0L;blocked<-function(...){io<<-io+1L;stop('Unexpected source/database I/O')}
for(name in c('futmondo_post','fm_async_http','fm_worker_rpc','supabase_upsert'))assign(name,blocked)
settle<-function(session)for(i in 1:15){later::run_now(0);session$flushReact()}
stamp<-as.POSIXct('2026-11-15 15:00:00',tz='UTC')
info<-observed_data(list(configuration=list(budget=0,playerMoveInDays=4)),observed_at=stamp)
root_exprs<-as.list(parse('server.R')[[1]][[3]])[-1L]
shiny::testServer(function(input,output,session){
 championship_id_RV<-reactiveVal('league')
 championships_RV<-reactiveVal(list(list(id='league',configuration=list(budget=100))))
 primary_team_state<-reactiveVal(resource_state(info))
 for(expr in root_exprs)if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
  identical(expr[[2]],as.name('league_settings_source')))eval(expr,envir=environment())
},{
 settle(session);snapshot<-league_settings_source()
 stopifnot(!snapshot$stale,snapshot$status=='ok',identical(snapshot$observed_at,stamp),snapshot$info$configuration$budget==0)
 primary_team_state(resource_state(info,status='loading'));settle(session)
 # Actual shared_resource previews carry stale freshness while a request is in flight.
 state<-primary_team_state();state$freshness<-'stale';primary_team_state(state);settle(session)
 snapshot<-league_settings_source();html<-as.character(admin_league_settings_UI(admin_league_settings(snapshot$league,snapshot$info),
  stale=snapshot$stale,status=snapshot$status,error=snapshot$error,observed_at=snapshot$observed_at))
 stopifnot(snapshot$stale,snapshot$status=='loading',!grepl('could not be refreshed|latest configuration is unavailable|fm-info-error',html),
  identical(snapshot$observed_at,stamp),grepl('0 EUR',html),grepl("Only league's admin",html,fixed=TRUE),grepl('Futmondo official app',html))
 cat('PASS actual root settings projection preserves observation time and zero values; pending refresh never becomes a source failure\n')
})
active<-reactiveVal(TRUE);allowed<-reactiveVal(TRUE)
snapshot<-reactiveVal(list(league=list(),info=info,status='stale',stale=TRUE,observed_at=stamp))
reloads<-0L
shiny::testServer(admin_Server,args=list(is_module_active=active,login_token=function()auth,
 championship_id=function()'league',user_team_id=function()'mine',user_teams_RV=function()data.frame(),is_admin=allowed,
 league_settings_source=snapshot,retry_league_settings=function(){reloads<<-reloads+1L;s<-isolate(snapshot());s$status<-'loading';snapshot(s)}),{
 settle(session);html<-output$league_settings$html
 stopifnot(grepl('Showing saved settings (out of date)',html,fixed=TRUE),!grepl('fm-info-error',html),grepl('Reload settings',html))
 session$setInputs(retry_league_settings=1);settle(session)
 stopifnot(reloads==1L,!grepl('out of date|could not be refreshed|fm-info-error',output$league_settings$html))
 session$setInputs(retry_league_settings=2);settle(session);stopifnot(reloads==1L)
 s<-isolate(snapshot());s$status<-'stale';s$error<-resource_failure(category='network',resource='get_user_team_info');snapshot(s);settle(session)
 html<-output$league_settings$html
 stopifnot(grepl('League settings could not be refreshed',html),grepl('Connection unavailable',html),grepl('fm-info-error',html),grepl('0 EUR',html))
 session$setInputs(retry_league_settings=3);settle(session);stopifnot(reloads==2L)
 s<-isolate(snapshot());s$status<-'ok';s$stale<-FALSE;s$error<-NULL;s$observed_at<-stamp+60;s$info$configuration$budget<-42;snapshot(s);settle(session)
 html<-output$league_settings$html
 stopifnot(grepl('42 EUR',html),!grepl('out of date|could not be refreshed|Updating league settings',html))
 active(FALSE);settle(session);session$setInputs(retry_league_settings=4);settle(session);stopifnot(reloads==2L)
 active(TRUE);allowed(FALSE);settle(session);session$setInputs(retry_league_settings=5);settle(session);stopifnot(reloads==2L,io==0L)
 cat('PASS scoped retry yields during loading, suppresses duplicate/inactive/unauthorized clicks, preserves real failures and recovers from newer configuration\n')
})
cat('ADMIN CONFIGURATION: 2 groups passed; no source/database I/O\n')
