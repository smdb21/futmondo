options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='observed-fixture',token='synthetic');now<-Sys.time();scope<-resource_context(auth,'league','mine')
roster<-observed_data(data.frame(id='out'),observed_at=now-90,status='stale');attr(roster,'worker_acquisition_status')<-'ok'
history<-observed_data(data.frame(id=c('buy','sell'),player_id=c('in','out'),player_name=c('Incoming','Leaving'),
 buyer_team_id=c('mine','buyer'),seller_team_id=c('seller','mine'),created=format(now-3600,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),is_clause=TRUE),observed_at=now-90,status='stale')
attr(history,'worker_acquisition_status')<-'ok'
summary<-function(id) {
 x<-observed_data(list(data=list(id=id,name=if(id=='in')'Incoming'else'Leaving',role='defensa',team='Club',logo='club'),
 championship=list(owner=list(id=if(id=='in')'seller'else'mine'),clause=list(transferred=TRUE,transferDate=format(now+3600,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')))),observed_at=now-5)
 attr(x,'valid_until')<-as.numeric(now)+295;x
}
summaries<-list(`in`=summary('in'),out=summary('out'))
x<-reconcile_pending_clause_transfers(roster,history,summaries,'mine',now,observed_only=TRUE)
stopifnot(nrow(x$incoming_records)==1L,nrow(x$outgoing_records)==1L,all(!nzchar(x$incoming_records$timing_error)),all(!nzchar(x$outgoing_records$timing_error)),
 x$status=='unavailable',x$outgoing_status=='partial',is.na(x$pending_inbound_count),is.na(x$pending_outbound_count),!length(x$pending_inbound_ids),!length(x$pending_outbound_ids),attr(roster,'fetch_status')=='stale')
strict<-reconcile_pending_clause_transfers(roster,history,summaries,'mine',now)
stopifnot(strict$status=='unavailable',is.na(strict$pending_inbound_count),!nrow(strict$incoming_records))
cat('PASS last-observed schedules are available without certifying current membership or acquisition capacity; strict reconciliation still rejects stale sources\n')
bad<-roster;attr(bad,'worker_acquisition_status')<-'partial'
stopifnot(!nrow(reconcile_pending_clause_transfers(bad,history,summaries,'mine',now,observed_only=TRUE)$incoming_records))
expired<-summaries;attr(expired[['in']],'valid_until')<-as.numeric(now)-1
stopifnot(nzchar(reconcile_pending_clause_transfers(roster,history,expired,'mine',now,observed_only=TRUE)$incoming_records$timing_error))
cat('PASS partial acquisitions and expired summaries without successful-source proof cannot supply observed timing\n')
last_seen<-summaries
for(id in names(last_seen)){attr(last_seen[[id]],'observed_at')<-now-1800;attr(last_seen[[id]],'valid_until')<-as.numeric(now)-1500;attr(last_seen[[id]],'fetch_status')<-'stale';attr(last_seen[[id]],'worker_acquisition_status')<-'ok'}
observed<-reconcile_pending_clause_transfers(roster,history,last_seen,'mine',now,observed_only=TRUE)
stopifnot(all(!nzchar(observed$incoming_records$timing_error)),all(!nzchar(observed$outgoing_records$timing_error)),is.na(observed$pending_inbound_count),observed$status=='unavailable',observed$incoming_records$observed_at==now-1800)
cat('PASS expired successful worker summaries remain historical observations with original timestamps and unavailable capacity\n')
roster_state<-resource_state(roster);roster_state$context<-scope
history_state<-resource_state(history);history_state$context<-scope
factual_read_adapter<-function(session,handler,...)if(handler=='get_players_from_team')function(...)roster_state else function(...)history_state
real_cached_summary<-incoming_clause_cached_summary
incoming_clause_cached_summary<-function(login,championship_id,user_team_id,player_id,...)summaries[[player_id]]
source_reads<-0L;async_adapter_fetch<-function(...){source_reads<<-source_reads+1L;stop('No source acquisition is allowed from stale display prerequisites')}
settle<-function(session)for(i in 1:20){later::run_now(0);session$flushReact()}
for(reload in 1:2)shiny::testServer(function(input,output,session){
 state<-incoming_clause_source(session,function()auth,function()'league',function()'mine',function()TRUE)
},{
 settle(session);s<-state()
 stopifnot(s$status=='observed',s$outgoing_status=='observed',isTRUE(s$observations_only),is.null(s$error),
  nrow(s$records)==1L,nrow(s$outgoing_records)==1L,all(!is.na(s$records$joins_at)),all(!is.na(s$outgoing_records$leaves_at)),source_reads==0L)
 html<-as.character(incoming_clause_panel(s,list(available=FALSE),session$ns))
 out_html<-as.character(outgoing_clause_panel(list(status=s$outgoing_status,records=s$outgoing_records,error=s$error,observations_only=s$observations_only,source_times=s$source_times),list(available=FALSE),session$ns))
 stopifnot(!grepl('Last observed transfer details',html,fixed=TRUE),grepl('Incoming',html,fixed=TRUE),grepl('Observed:',html,fixed=TRUE),
   !grepl('Last observed transfer details',out_html,fixed=TRUE),grepl('Leaving',out_html,fixed=TRUE),grepl('Observed:',out_html,fixed=TRUE),!grepl('Roster observed:',html,fixed=TRUE),
   !grepl('Transactions observed:',out_html,fixed=TRUE),!grepl('Refresh to retry',html,fixed=TRUE))
})
cat('PASS two cold page sessions restore stored transfer details as dated observations without false failure warnings, known dates and zero new source reads\n')

# The stored roster and transactions can arrive in an earlier batch than the
# derived player summaries. Exercise the real worker read and its per-key revisions.
incoming_clause_cached_summary<-real_cached_summary
fm_worker_configured<-function()TRUE
worker<-fm_worker_account_state(scope$account_id)
worker$bootstrap_complete<-TRUE
worker$account<-list(enabled=TRUE)
batches<-0L
fm_worker_rpc<-function(name,payload,owner=''){
 stopifnot(name=='read')
 batches<<-batches+1L
 promises::promise(function(resolve,reject){finish_read<<-resolve})
}
shiny::testServer(function(input,output,session){
 state<-incoming_clause_source(session,function()auth,function()'league',function()'mine',function()TRUE)
},{
 settle(session)
 stopifnot(state()$incoming_loading,state()$outgoing_loading)
 if(!worker$busy)fm_worker_poll(scope$account_id,force=TRUE)
 stopifnot(batches==1L)
 # This database response contains only the departure. A missing arrival is
 # now settled, and a later stored arrival must still invalidate the count.
 saved<-function(id){
   d<-fm_worker_descriptor('get_clause_transfer_summary',
     list(championship_id='league',user_team_id='mine',player_id=id),scope)
   wire<-charToRaw(fm_worker_json(fm_worker_encode(summaries[[id]])))
   compressed<-history_baseline_compress(wire)
   manifest<-list(account_id=scope$account_id,resource_key=d$resource_key,context=d$context,handler=d$handler,
     version='v1',json_bytes=length(wire),parts=1L,sha256=paste0(openssl::sha256(compressed)),
     observed_at=rival_finance_stamp(now-5),checked_at=rival_finance_stamp(now-5),
     valid_until=rival_finance_stamp(now+295),status='ok',completeness='complete')
   list(key=d$resource_key,manifest=manifest,chunks=list(jsonlite::base64_enc(compressed)))
 }
 reply<-function(id)list(account=worker$account,resources=list(saved(id)),jobs=list())
 finish_read(reply('out'));settle(session)
 stopifnot(!state()$incoming_loading,state()$status=='partial',
   !state()$outgoing_loading,state()$outgoing_status=='observed',nrow(state()$outgoing_records)==1L)
 fm_worker_poll(scope$account_id,force=TRUE)
 finish_read(reply('in'));settle(session)
 stopifnot(state()$status=='observed',!state()$incoming_loading,nrow(state()$records)==1L,
   source_reads==0L,batches==2L)
})
cat('PASS pending summary batch, settled missing arrival and subsequent stored arrival with zero source reads\n')
