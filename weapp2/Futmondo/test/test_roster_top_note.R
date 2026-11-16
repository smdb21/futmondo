# Real root-page note projections and worker adapters, with synthetic stored data.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
now<-base::Sys.time();Sys.time<-function()now
fixture_auth<-c(userid='roster-note-fixture',token='synthetic')
account<-fm_worker_account_state(fixture_auth[['userid']])
account$known<-TRUE;account$account<-list(enabled=TRUE,auth_status='active')
account$health<-list(heartbeat=rival_finance_stamp(now))
io<-0L;blocked<-function(...){io<<-io+1L;stop('Unexpected source/database acquisition')}
for(name in c('futmondo_post','fm_async_http','fm_worker_rpc','supabase_get','supabase_upsert'))assign(name,blocked)
store<-function(league='league',offset=0,missing=FALSE,future=FALSE){
 ctx<-resource_context(fixture_auth,league,'mine')
 for(handler in c('get_players_from_team','get_championship_pressroom')){
  args<-if(handler=='get_players_from_team')list(championship_id=league,user_team_id='mine',teams=NULL)else list(championship_id=league)
  d<-fm_worker_descriptor(handler,args,ctx)
  at<-now-1200+offset+if(handler=='get_championship_pressroom')60 else 0
  if(future)at<-now+600
  value<-observed_data(data.frame(id=character()),observed_at=at)
  if(missing)attr(value,'observed_at')<-as.POSIXct(NA_real_,origin='1970-01-01',tz='UTC')
  attr(value,'valid_until')<-as.numeric(now)-60
  fm_worker_put(ctx$account_id,d$resource_key,list(data=value,manifest=list(version=paste(league,offset,missing,future),checked_at=rival_finance_stamp(at))))
  revision<-fm_worker_resource_revision(account,d$resource_key);revision(shiny::isolate(revision())+1L)
 }
}
store();store('other',offset=120)
settle<-function(session)for(i in 1:20){later::run_now(0);session$flushReact()}
root_exprs<-as.list(parse('server.R')[[1]][[3]])[-1L]
worker_exprs<-as.list(body(fm_worker_session))[-1L]
shiny::testServer(function(input,output,session){
 login_token_RV<-shiny::reactiveVal(fixture_auth);login<-login_token_RV
 championship_id_RV<-shiny::reactiveVal('league');user_team_id_RV<-function()'mine';refresh_trigger<-function()0L
 for(expr in root_exprs)if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
  paste(deparse(expr[[2]]),collapse='')%in%c('clause_transfer_revision','roster_note_active','roster_sources_active','roster_source_readers','roster_source_note'))eval(expr,envir=environment())
 source_note<-function()roster_source_note()
 include<-FALSE
 for(expr in worker_exprs){
  lhs<-if(is.call(expr)&&identical(expr[[1]],as.name('<-')))paste(deparse(expr[[2]]),collapse='')else ''
  if(lhs=='status')include<-TRUE
  if(lhs=='output$worker_settings')break
  if(include)eval(expr,envir=environment())
 }
},{
 for(page in c('yourteam','today','players_in_championship')){
  session$setInputs(tabs=page);settle(session)
  stopifnot(grepl('worker_roster_source_times',output$worker_status$html,fixed=TRUE),
   grepl('Roster observed:',output$worker_roster_source_times,fixed=TRUE),
   grepl('Transactions observed:',output$worker_roster_source_times,fixed=TRUE),
   grepl(fm_format_datetime(now-1200),output$worker_roster_source_times,fixed=TRUE))
 }
 stopifnot(io==0L,!length(.fm_transport$tasks))
 cat('PASS My Team, Market and All Players restore original roster/transaction clocks from shared stored snapshots with zero I/O or clause-summary work\n')
 before<-output$worker_status;original<-output$worker_roster_source_times
 elapsed<-system.time(for(i in 1:10){store(offset=i*60);settle(session)})[['elapsed']]
 stopifnot(!identical(original,output$worker_roster_source_times),identical(before,output$worker_status),io==0L)
 cat(sprintf('PASS ten source-clock changes update only top-note text, preserving its DOM and issuing zero I/O (%.3fs)\n',elapsed))
 championship_id_RV('other');settle(session)
 stopifnot(grepl(fm_format_datetime(now-1080),output$worker_roster_source_times,fixed=TRUE))
 session$setInputs(tabs='rivals');settle(session);stopifnot(!nzchar(trimws(output$worker_roster_source_times)))
 session$setInputs(tabs='today');settle(session)
 store('other',missing=TRUE);settle(session);stopifnot(!nzchar(trimws(output$worker_roster_source_times)))
 store('other',future=TRUE);settle(session);stopifnot(!nzchar(trimws(output$worker_roster_source_times)))
 account$service_error<-'Synthetic saved-data read failure';fm_worker_notify(fixture_auth[['userid']]);settle(session)
 stopifnot(grepl('fm-info-error',output$worker_status$html,fixed=TRUE),grepl('Synthetic saved-data read failure',output$worker_status_message,fixed=TRUE))
 login_token_RV(NULL);settle(session);stopifnot(is.null(output$worker_status),!nzchar(trimws(output$worker_roster_source_times)),io==0L)
 cat('PASS league switch, unrelated page, unknown/future timestamps and logout clear old clocks; actual worker errors remain visible\n')
})
