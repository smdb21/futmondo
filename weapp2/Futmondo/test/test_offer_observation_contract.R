#!/usr/bin/env Rscript
# Real worker codec + display projections, synthetic database snapshots only.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
now<-Sys.time();auth<-c(userid='offer-observation-fixture',token='synthetic')
rows<-dplyr::bind_rows(normalize_received_offer(list(id='sys',price=500),'p'),
 normalize_received_offer(list(id='rival',price=300,userTeam=list(id='r',name='Rival')),'p'))
roster<-data.frame(id='p',name='Player',value=1000,points=1,role='defensa')
attr(roster,'received_offers')<-rows;attr(roster,'received_offers_status')<-'ok'
attr(roster,'received_offers_observed_at')<-now-100
restore<-function(value,status='ok',at=now-90){
 bytes<-charToRaw(fm_worker_json(fm_worker_encode(value)));compressed<-history_baseline_compress(bytes)
 fm_worker_restore(list(key='fixture-key',manifest=list(account_id=auth[['userid']],resource_key='fixture-key',
  handler='get_players_from_team',json_bytes=length(bytes),parts=1L,sha256=paste0(openssl::sha256(compressed)),
  observed_at=rival_finance_stamp(at),checked_at=rival_finance_stamp(at),valid_until=rival_finance_stamp(at+30),
  status=status,completeness='unknown'),chunks=list(jsonlite::base64_enc(compressed))),auth[['userid']])$data
}
saved<-restore(roster);original<-saved
view<-team_received_offers_state(resource_state(saved));html<-as.character(team_received_offers_panel(view,identity))
stopifnot(attr(saved,'fetch_status')=='stale',view$status=='observed',view$observations_only,nrow(view$records)==2L,
 identical(view$records$bid_price,c(500,300)),abs(as.numeric(view$observed_at)-as.numeric(attr(roster,'received_offers_observed_at')))<0.00001,
 grepl('2 observed',html,fixed=TRUE),grepl('Observed:',html,fixed=TRUE),grepl('Rival · Rival',html,fixed=TRUE),
 !grepl('fm-info-error|could not be verified|Refresh to retry',html),identical(saved,original))
cat('PASS expired successful database-worker snapshots retain all offers, original clock and explicit observation status without a false failure\n')
empty<-roster[0,,drop=FALSE];attr(empty,'received_offers')<-received_offers_empty();attr(empty,'received_offers_status')<-'empty'
x<-team_received_offers_state(resource_state(restore(empty,'empty')))
stopifnot(x$status=='observed',!nrow(x$records),grepl('No offers observed.',as.character(team_received_offers_panel(x,identity)),fixed=TRUE))
# Older explicit empty roster responses have no supplemental offer metadata.
attr(empty,'received_offers')<-NULL;attr(empty,'received_offers_status')<-NULL
x<-team_received_offers_state(resource_state(restore(empty,'empty')))
stopifnot(x$status=='observed',!nrow(x$records))
cat('PASS both explicit empty-offer snapshots and authoritative old empty rosters retain truthful observed zero\n')
for(kind in c('partial','incomplete','offer-partial','error','missing-proof','future')){
 bad<-saved
 if(kind=='partial')attr(bad,'worker_acquisition_status')<-'partial'
 if(kind=='incomplete')attr(bad,'worker_completeness')<-'partial'
 if(kind=='offer-partial')attr(bad,'received_offers_status')<-'partial'
 if(kind=='error')attr(bad,'received_offers_error')<-resource_failure(category='network')
 if(kind=='missing-proof')attr(bad,'received_offers')<-NULL
 if(kind=='future')attr(bad,'received_offers_observed_at')<-now+3600
 for(freshness in c('stale','ok')){
  attr(bad,'fetch_status')<-freshness
  state<-team_received_offers_state(resource_state(bad))
  stopifnot(!state$status%in%c('observed','ok','empty'),!state$observations_only,
   grepl('fm-info-error',as.character(team_received_offers_panel(state,identity)),fixed=TRUE))
 }
}
cat('PASS failures, partial acquisitions, incomplete or missing offer proof and future timestamps cannot become successful observations\n')
# Execute actual module projections: stamp-only updates must not rebuild offers.
body_nodes<-as.list(body(eval(body(players_in_teams_Server)[[2]][[3]])))[-1L]
wanted<-c('offers_state','offers_source_time','output$received_offers_source_time','offers_panel_display','output$received_offers')
reads<-writes<-0L
async_adapter_fetch<-function(...){reads<<-reads+1L;stop('Unexpected acquisition')}
supabase_get<-function(...){reads<<-reads+1L;stop('Unexpected database read')}
defer_persistence<-function(...){writes<<-writes+1L;stop('Restored data must not be rewritten')}
shiny::testServer(function(input,output,session){
 roster_state<-shiny::reactiveVal(resource_state(saved));is_module_active<-function()TRUE
 for(expr in body_nodes)if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&paste(deparse(expr[[2]]),collapse='')%in%wanted)eval(expr,envir=environment())
 stats<-new.env();stats$renders<-0L;shiny::observe({offers_panel_display();stats$renders<-stats$renders+1L})
},{
 settle<-function()for(i in 1:12){later::run_now(0);session$flushReact()}
 settle();before<-stats$renders;stamp<-output$received_offers_source_time
 elapsed<-system.time(for(i in 1:10){
  next_value<-saved
  # Keep stamps historic while crossing visible minute boundaries.
  attr(next_value,'received_offers_observed_at')<-now-1200+i*60
  attr(next_value,'observed_at')<-now-1200+i*60
  roster_state(resource_state(next_value));settle()
 })[['elapsed']]
 stopifnot(stats$renders==before,output$received_offers_source_time!=stamp,reads==0L,writes==0L)
 cat(sprintf('PASS ten saved-clock publications: zero repeated offer-panel renders, acquisitions or writes (%.3fs)\n',elapsed))
})
# The explicit empty source path records completeness for durable future replay.
clear_api_cache();get_sb_url<-function()'';get_sb_key<-function()''
record_acquired_resource<-function(...)invisible(TRUE)
futmondo_post<-function(url,...) {
 stopifnot(identical(url,ROSTER_URL))
 fm_httr_response(list(status_code=200L,content=charToRaw('{"answer":[]}'),times=list(total=0)),url)
}
x<-get_players_from_team(auth,'league','mine')
stopifnot(!nrow(x),identical(attr(x,'received_offers_status'),'empty'),roster_offer_rows_valid(attr(x,'received_offers')),
 as.numeric(attr(x,'received_offers_observed_at'))==as.numeric(attr(x,'observed_at')))
cat('PASS authoritative empty roster acquisition retains complete empty offer metadata for database persistence/reuse\n')

# Follow that newly acquired empty result through the actual persistence/reuse
# contract; only the database/queue boundaries are synthetic.
ctx<-resource_context(auth,'league','mine');queued<-list()
defer_persistence<-function(fn,args,context,...){queued[[length(queued)+1L]]<<-args;TRUE}
stopifnot(persist_roster_offers(x,ctx),length(queued)==1L)
record<-queued[[1L]][[2L]][[1L]]
stopifnot(record$payload$origin=='combined',record$payload$status=='complete',!length(record$payload$rows))
supabase_get<-function(...)as.data.frame(jsonlite::fromJSON(jsonlite::toJSON(list(record),auto_unbox=TRUE,na='null',null='null',digits=NA)))
futmondo_post<-function(...)stop('Fresh stored empty offers must be reused before HTTP')
clear_api_cache();reused<-get_roster_bids(auth,'league','mine')
stopifnot(!nrow(reused),attr(reused,'offers_from_saved'),roster_offer_rows_valid(attr(reused,'received_offers')),
 length(queued)==1L)
cat('PASS newly acquired empty offers are queued as one complete snapshot and fresh stored data is reused without HTTP or duplicate writes\n')
