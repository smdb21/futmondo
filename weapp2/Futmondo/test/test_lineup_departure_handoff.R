# Root-server readers, real worker adapters and shared transfer reconciliation.
# A confirmed empty result must survive Today -> My Team -> Lineup navigation.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
fixture_auth<-c(userid='departure-handoff-fixture',token='synthetic')
account<-fm_worker_account_state(fixture_auth[['userid']])
account$known<-TRUE;account$account<-list(enabled=TRUE,auth_status='active')
now<-Sys.time()
io<-0L
blocked<-function(...){io<<-io+1L;stop('Unexpected acquisition or persistence')}
for(name in c('futmondo_post','fm_async_http','fm_worker_rpc','supabase_get','supabase_upsert'))assign(name,blocked)
store<-function(league='league',auth=fixture_auth,offset=0L,partial=FALSE){
 ctx<-resource_context(auth,league,'mine')
 for(handler in c('get_players_from_team','get_championship_pressroom')){
  args<-if(handler=='get_players_from_team')list(championship_id=league,user_team_id='mine',teams=NULL)else list(championship_id=league)
  d<-fm_worker_descriptor(handler,args,ctx)
  data<-if(handler=='get_players_from_team')data.frame(id='owned',clause_transferred=FALSE)else
   data.frame(player_id=character(),buyer_team_id=character(),seller_team_id=character(),created=character(),is_clause=logical())
  data<-observed_data(data,observed_at=now-120+offset,status='stale')
  attr(data,'worker_acquisition_status')<-if(nrow(data))'ok'else'empty'
  attr(data,'worker_completeness')<-if(partial)'partial'else'complete';attr(data,'valid_until')<-as.numeric(now)-60+offset
  fm_worker_put(ctx$account_id,d$resource_key,list(data=data,manifest=list(version=paste(league,offset))))
  revision<-fm_worker_resource_revision(fm_worker_account_state(auth[['userid']]),d$resource_key)
  revision(shiny::isolate(revision())+1L)
 }
}
store();store('other')
settle<-function(session)for(i in 1:25){later::run_now(0);session$flushReact()}
root_exprs<-as.list(parse('server.R')[[1]][[3]])[-1L]
root_names<-c('clause_transfer_revision','roster_note_active','roster_sources_active','roster_note_readers',
 'roster_source_readers','roster_source_note','clause_transfers_RV')
for(first in c('today','lineup','yourteam'))shiny::testServer(function(input,output,session){
 login_token_RV<-shiny::reactiveVal(fixture_auth)
 championship_id_RV<-shiny::reactiveVal('league');user_team_id_RV<-function()'mine';refresh_trigger<-shiny::reactiveVal(0L)
 for(expr in root_exprs)if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
  paste(deparse(expr[[2]]),collapse='')%in%root_names)eval(expr,envir=environment())
 departure<-resource_display_source(session,function()clause_departure_round_state(clause_transfers_RV(),list(starts_at=now+86400)))
 output$eligibility<-shiny::renderText({shiny::req(identical(input$tabs,'lineup'));as.character(departure()$complete)})
 output$departing<-shiny::renderUI({shiny::req(identical(input$tabs,'yourteam'));s<-clause_transfers_RV()
  outgoing_clause_panel(list(status=s$outgoing_status,records=s$outgoing_records,error=s$outgoing_error %||% s$error),list(available=FALSE),session$ns)})
},{
 session$setInputs(tabs=first);settle(session)
 for(page in c('yourteam','lineup','today','lineup','players_in_championship','yourteam','lineup')){
  session$setInputs(tabs=page);settle(session)
  if(page=='yourteam')stopifnot(grepl('Leaving players (0)',output$departing$html,fixed=TRUE))
  if(page=='lineup'){
   stopifnot(output$eligibility=='TRUE',isTRUE(departure()$complete),!nrow(clause_transfers_RV()$outgoing_records))
  }
 }
 stopifnot(io==0L,!length(.fm_transport$tasks))
 cat('PASS confirmed zero departures survives navigation beginning on',first,'with zero source/database requests or writes\n')
 championship_id_RV('other');settle(session)
 stopifnot(departure()$complete,!nrow(clause_transfers_RV()$outgoing_records),io==0L)
 cat('PASS a league change restores only its own saved empty result\n')
 stats<-new.env(parent=emptyenv());stats$updates<-0L
 observer<-shiny::observe({departure();stats$updates<-stats$updates+1L})
 settle(session);before<-stats$updates
 elapsed<-system.time(for(i in 1:10){store('other',offset=i);settle(session)})[['elapsed']]
 stopifnot(stats$updates==before,io==0L)
 observer$destroy()
 cat(sprintf('PASS ten timestamp publications cause zero eligibility redraws or I/O (%.3fs)\n',elapsed))
 championship_id_RV(paste0('delayed-',first));settle(session)
 stopifnot(!departure()$complete,is.null(clause_transfers_RV()$error),clause_transfers_RV()$outgoing_status=='loading')
 store(paste0('delayed-',first));fm_worker_settle(account);settle(session)
 stopifnot(departure()$complete,io==0L)
 store(paste0('delayed-',first),partial=TRUE);settle(session)
 stopifnot(!departure()$complete,grepl('incomplete',clause_transfers_RV()$error))
 store(paste0('delayed-',first));settle(session);stopifnot(departure()$complete)
 cat('PASS missing data waits without certifying zero; delayed saved data recovers; partial evidence remains unverified\n')
 login_token_RV(NULL);settle(session);stopifnot(!departure()$complete)
 login_token_RV(fixture_auth);settle(session);stopifnot(departure()$complete)
 other_auth<-c(userid=paste0('other-',first),token='synthetic')
 other_account<-fm_worker_account_state(other_auth[['userid']]);other_account$known<-TRUE
 other_account$account<-list(enabled=TRUE,auth_status='active')
 login_token_RV(other_auth);settle(session);stopifnot(!departure()$complete)
 store(paste0('delayed-',first),auth=other_auth);fm_worker_settle(other_account);settle(session)
 stopifnot(departure()$complete,io==0L)
 cat('PASS logout and account replacement clear earlier eligibility until matching saved evidence arrives\n')
})
