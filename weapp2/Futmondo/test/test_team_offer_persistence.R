#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(label,expr){force(expr);checks<<-checks+1L;cat('[PASS]',label,'\n')}
context<-resource_context(account_id='offer-fixture',championship_id='league',user_team_id='mine')
now<-as.POSIXct('2026-11-07 12:00:00',tz='UTC')
rows<-data.frame(id=c('a','a','b','c'),bid_price=c(200,150,NA_real_,-5),
  bid_user=c('Futmondo','Rival','Unknown bidder','Futmondo'),bid_id=c('sys','rival','unknown','negative'),
  bidder_team_id=c('','rival-id','',''),bid_source=c('system','rival','unknown','system'))
data<-highest_received_offers(rows)
attr(data,'received_offers')<-rows;attr(data,'observed_at')<-now;attr(data,'fetch_status')<-'ok'
attr(data,'received_offers_status')<-'ok'
attr(data,'unrelated_secret')<-'must-not-persist'
queued<-list();saved<-NULL;reads<-0L
# Capture the existing queue boundary without issuing any external write.
defer_persistence<-function(fn,args,context,...){queued[[length(queued)+1L]]<<-list(fn=fn,args=args,context=context);TRUE}
supabase_get<-function(table_name,query_params){
  reads<<-reads+1L
  stopifnot(table_name=='rival_finance_evidence_latest',query_params$limit==2L,
    query_params$account_id=='eq.offer-fixture',query_params$championship_id=='eq.league')
  saved
}
wire<-function(record){
  # JSONB does not retain object-key order. Mimic a PostgREST round trip.
  record$payload$scope<-rev(record$payload$scope)
  as.data.frame(jsonlite::fromJSON(jsonlite::toJSON(list(record),auto_unbox=TRUE,na='null',null='null',digits=NA)))
}
check('complete snapshots use existing private queue and exclude unrelated fields',{
  stopifnot(persist_roster_offers(data,context),length(queued)==1L,
    queued[[1]]$fn=='supabase_post_direct',queued[[1]]$args[[1]]=='rival_finance_evidence',
    queued[[1]]$args[[4]],!grepl('must-not-persist',jsonlite::toJSON(queued[[1]]$args)))
  record<-queued[[1]]$args[[2]][[1]]
  stopifnot(record$evidence_type=='eligibility',startsWith(record$source_id,'rival-cache:received-offers-v1:'),
    length(record$payload$rows)==4L)
  saved<-wire(record)
})
check('fresh durable reads restore every offer and original source time',{
  restored<-read_saved_roster_offers(context,now+3)
  stopifnot(is.data.frame(restored),identical(attr(restored,'received_offers'),rows),
    identical(as.numeric(attr(restored,'observed_at')),as.numeric(now)),attr(restored,'offers_from_saved'),
    restored$bid_id[restored$id=='a']=='sys',is.na(restored$bid_price[restored$id=='b']))
  count<-length(queued);stopifnot(!persist_roster_offers(restored,context),length(queued)==count)
})
check('expiry and account, league, team, scoring and season isolation reject stale or foreign snapshots',{
  stopifnot(is.null(read_saved_roster_offers(context,now+15)),is.null(read_saved_roster_offers(context,now-1)))
  for(field in c('account_id','championship_id','user_team_id','season','scoring_version')){
    wrong<-context;wrong[[field]]<-'different'
    # This stub returns the same row regardless of requested scope; decoding must reject it.
    old_get<-supabase_get;supabase_get<-function(...)saved
    stopifnot(is.null(read_saved_roster_offers(wrong,now+1)))
    supabase_get<-old_get
  }
})
check('partial and failed sources are never saved; failed enqueue remains retryable',{
  bad<-data;attr(bad,'received_offers_status')<-'partial'
  count<-length(queued);stopifnot(!persist_roster_offers(bad,context),length(queued)==count)
  attr(bad,'received_offers_status')<-'ok';attr(bad,'fetch_status')<-'stale'
  stopifnot(!persist_roster_offers(bad,context))
  old_defer<-defer_persistence;attempts<-0L
  defer_persistence<-function(...){attempts<<-attempts+1L;FALSE}
  stopifnot(!persist_roster_offers(data,context),!persist_roster_offers(data,context),attempts==2L)
  defer_persistence<-old_defer
})
check('a verified empty list is durable and preserves its empty status',{
  empty<-received_offers_empty();attr(empty,'received_offers')<-received_offers_empty()
  attr(empty,'observed_at')<-now;attr(empty,'fetch_status')<-'empty'
  stopifnot(persist_roster_offers(empty,context))
  saved<-wire(tail(queued,1L)[[1]]$args[[2]][[1]])
  restored<-read_saved_roster_offers(context,now+1)
  stopifnot(is.data.frame(restored),nrow(restored)==0L,attr(restored,'fetch_status')=='empty')
})
check('acquisition hook persists complete combined roster offers outside async replay',{
  queued<-list();record_acquired_resource('roster_league_mine',data,context)
  stopifnot(length(queued)==1L,length(queued[[1]]$args[[2]][[1]]$payload$rows)==4L)
})
check('fresh DB snapshot is reused before any offer HTTP acquisition',{
  saved<-wire(queued[[1]]$args[[2]][[1]])
  count<-length(queued);Sys.time<-function()now+1
  futmondo_post<-function(...)stop('Offer HTTP must not run for a fresh durable snapshot')
  clear_api_cache()
  restored<-get_roster_bids(c(userid='offer-fixture',token='synthetic'),'league','mine')
  stopifnot(nrow(attr(restored,'received_offers'))==4L,attr(restored,'offers_from_saved'),
    length(queued)==count)
  rm(Sys.time)
})
check('manual invalidation rejects previously persisted offers',{
  saved<-wire(queued[[1]]$args[[2]][[1]])
  invalidate_saved_roster_offers(context)
  stopifnot(is.null(read_saved_roster_offers(context,now+1)))
})
cat(sprintf('Team offer persistence: %d checks passed.\n',checks))
