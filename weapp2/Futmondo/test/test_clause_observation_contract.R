options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
now<-base::Sys.time();Sys.time<-function()now
auth<-c(userid='clause-observation-contract',token='synthetic')
ctx<-resource_context(auth,'league','mine')
observed<-function(x,at=now-1200,status='stale'){
 x<-observed_data(x,observed_at=at,status=status)
 attr(x,'worker_acquisition_status')<-'ok';attr(x,'worker_completeness')<-'complete'
 attr(x,'valid_until')<-as.numeric(at)+30;x
}
roster<-observed(data.frame(id='out'))
history<-observed(data.frame(id=c('buy','sell'),player_id=c('in','out'),player_name=c('Incoming','Leaving'),
 buyer_team_id=c('mine','buyer'),seller_team_id=c('seller','mine'),
 created=format(now-3600,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),is_clause=TRUE))
summary<-function(id,at=now-300,owner=if(id=='in')'seller'else'mine',transferred=TRUE){
 observed(list(data=list(id=id,name=id,role='defensa',team='Club'),championship=list(owner=list(id=owner),
  clause=list(transferred=transferred,transferDate=format(now+3600,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')))),at)
}
summaries<-list(`in`=summary('in'),out=summary('out'))
# Even recent-looking failed/partial evidence cannot resolve a later transfer.
for(kind in c('older','error','partial','incomplete')){
 bad<-summaries;bad[['in']]<-summary('in',at=if(kind=='older')now-7200 else now-5,owner='mine',transferred=FALSE)
 if(kind=='error')attr(bad[['in']],'fetch_error')<-resource_failure(category='network')
 if(kind=='partial')attr(bad[['in']],'worker_acquisition_status')<-'partial'
 if(kind=='incomplete')attr(bad[['in']],'worker_completeness')<-'partial'
 if(kind!='older'){attr(bad[['in']],'fetch_status')<-'ok';attr(bad[['in']],'valid_until')<-as.numeric(now)+25}
 result<-reconcile_pending_clause_transfers(roster,history,bad,'mine',now,observed_only=TRUE)
 stopifnot(nrow(result$incoming_records)==1L,nzchar(result$incoming_records$timing_error),is.na(result$pending_inbound_count))
}
cat('PASS a pre-transaction, failed or partial summary cannot falsely confirm arrival or cancellation\n')
passed<-summaries
for(id in names(passed))passed[[id]]$championship$clause$transferDate<-format(now-60,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')
result<-reconcile_pending_clause_transfers(roster,history,passed,'mine',now,observed_only=TRUE)
stopifnot(nrow(result$incoming_records)==1L,nrow(result$outgoing_records)==1L,
 result$incoming_records$overdue,result$outgoing_records$overdue,
 !nzchar(result$incoming_records$timing_error),!nzchar(result$outgoing_records$timing_error),
 result$status=='unavailable',is.na(result$pending_inbound_count),is.na(result$pending_outbound_count))
newer<-summaries;newer[['in']]<-summary('in',at=now-1,owner='mine',transferred=FALSE)
newer$out<-summary('out',at=now-1,owner='buyer',transferred=FALSE)
resolved<-reconcile_pending_clause_transfers(roster,history,newer,'mine',now,observed_only=TRUE)
stopifnot(!nrow(resolved$incoming_records),!nrow(resolved$outgoing_records))
cat('PASS elapsed schedules remain awaiting confirmation; only newer observed ownership/cancellation retires them, with action capacity unavailable\n')
state<-function(data){x<-resource_state(data);x$context<-ctx;x}
roster_state<-reactiveVal(state(roster));history_state<-reactiveVal(state(history))
factual_read_adapter<-function(session,handler,...)if(handler=='get_players_from_team')function(...)roster_state()else function(...)history_state()
incoming_clause_cached_summary<-function(login,championship_id,user_team_id,player_id,...)summaries[[player_id]]
reads<-writes<-0L
async_adapter_fetch<-function(...){reads<<-reads+1L;stop('Display must not acquire source data')}
persist_clause_transfer_summary<-function(...){writes<<-writes+1L;stop('Display must not write restored evidence')}
settle<-function(session)for(i in 1:15){later::run_now(0);session$flushReact()}
# Execute the actual module's display projections, including its separate clock
# outputs. Source adapters are synthetic; no source account or database is used.
module_body<-as.list(body(eval(body(players_in_teams_Server)[[2]][[3]])))[-1L]
wanted<-c('source_times','output$incoming_source_times','output$outgoing_source_times',
 'incoming_panel_display','outgoing_panel_display','output$incoming_players','output$outgoing_players')
shiny::testServer(function(input,output,session){
 incoming<-incoming_clause_source(session,function()auth,function()'league',function()'mine',function()TRUE)
 is_module_active<-function()TRUE;round_catalog<-function()data.frame();ns<-session$ns
 for(expr in module_body)if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
  paste(deparse(expr[[2]]),collapse='')%in%wanted)eval(expr,envir=environment())
 stats<-new.env(parent=emptyenv());stats$incoming<-0L;stats$outgoing<-0L
 observe({incoming_panel_display();stats$incoming<-stats$incoming+1L})
 observe({outgoing_panel_display();stats$outgoing<-stats$outgoing+1L})
},{
 settle(session)
 stopifnot(incoming()$status=='observed',incoming()$outgoing_status=='observed',is.null(incoming()$error),
  grepl('Last observed',output$incoming_players$html),grepl('Roster observed:',output$incoming_source_times),reads==0L,writes==0L)
 before<-as.list(stats);times<-output$incoming_source_times
 elapsed<-system.time(for(i in 1:10){
  roster_state(state(observed(roster,at=now-1200+i*60)))
  history_state(state(observed(history,at=now-1200+i*60)))
  settle(session)
 })[['elapsed']]
 stopifnot(identical(as.list(stats),before),output$incoming_source_times!=times,reads==0L,writes==0L)
 cat(sprintf('PASS ten new observation clocks update source-time text with zero incoming/leaving panel redraws, source requests or writes (%.3fs)\n',elapsed))
 # Ordinary freshness expiry is not a failed acquisition or a new transfer fact.
 fresh_roster<-observed(roster,at=now,status='ok');fresh_history<-observed(history,at=now,status='ok')
 roster_state(state(fresh_roster));history_state(state(fresh_history));settle(session);before<-as.list(stats)
 now<<-now+31
 attr(fresh_roster,'fetch_status')<-'stale';attr(fresh_history,'fetch_status')<-'stale'
 roster_state(state(fresh_roster));history_state(state(fresh_history));settle(session)
 stopifnot(identical(as.list(stats),before),is.null(incoming()$error),incoming()$status=='observed',
  attr(roster_state()$data,'observed_at')==now-31,attr(roster_state()$data,'valid_until')==as.numeric(now)-1)
 cat('PASS a successful roster expiring after30seconds keeps its observation intact with no warning loop or deadline renewal\n')
 broken<-roster_state();broken$error<-resource_failure(category='network')
 roster_state(broken);settle(session)
 stopifnot(incoming()$status=='partial',!is.null(incoming()$error),length(incoming()$source_errors)==1L,
  incoming()$observations_only,nrow(incoming()$records)==1L)
 roster_state(state(fresh_roster));settle(session)
 partial<-roster_state();attr(partial$data,'worker_completeness')<-'partial';partial$status<-'ok';partial$freshness<-'fresh';attr(partial$data,'fetch_status')<-'ok'
 roster_state(partial);settle(session)
 stopifnot(incoming()$status=='partial',grepl('incomplete',incoming()$error))
 wrong<-state(fresh_roster);wrong$context$season<-'different-season'
 wrong_history<-state(fresh_history);wrong_history$context$season<-'different-season'
 roster_state(wrong);history_state(wrong_history);settle(session)
 stopifnot(grepl('different account, league or scoring contexts',incoming()$error),incoming()$status=='partial')
 stopifnot(reads==0L,writes==0L)
 cat('PASS real source errors, partial proof and two matching old-scope snapshots retain truthful failures and never authorize current actions\n')
})
# Stale saved money remains visible; a real failure is not disguised as expiry.
team<-resource_state(observed(list(budget=100,withheld=0,teamValue=1000,configuration=list(mnmp=.5))))
market<-resource_state(observed(data.frame()))
finance<-round_finance_display(team,market)
stopifnot(finance$cash==100,finance$cash_stale,finance$class=='round-countdown-unavailable',
 grepl('Cached means the last saved amounts',finance$message),!grepl('Refresh',finance$message))
team$error<-resource_failure(category='network');failed<-round_finance_display(team,market)
stopifnot(failed$cash==100,grepl('could not load',failed$message),grepl('last saved amounts',failed$message))
team$status<-'ok';team$freshness<-'fresh';attr(team$data,'fetch_status')<-'ok';team$data$budget<--100
failed<-round_finance_display(team,market)
stopifnot(failed$class=='round-countdown-unavailable',grepl('could not load',failed$message),!grepl('Restore at least',failed$message))
cat('PASS cached money keeps exact saved figures and uncertain solvency; actual acquisition failures remain visible\n')
