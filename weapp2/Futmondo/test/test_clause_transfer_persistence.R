#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(label,expr){force(expr);checks<<-checks+1L;cat('[PASS]',label,'\n')}
context<-resource_context(account_id='clause-fixture',championship_id='league',user_team_id='mine',player_id='p')
now<-as.POSIXct('2026-11-10 12:00:00',tz='UTC')
clause_clock<-now+1
clause_transfer_now<-function()clause_clock
summary<-observed_data(list(data=list(id='p',name='Player',role='defensa',team='Club',logo='club',password='must-not-persist'),
  championship=list(owner=list(id='mine',name='Your team',token='must-not-persist'),
    clause=list(transferred=TRUE,transferDate='2026-11-13T22:00:00Z',secret='must-not-persist')),
  token='must-not-persist',points=list(list(round=1,points=-2))),observed_at=now)
queued<-list();saved<-NULL;reads<-0L;source_reads<-0L
defer_persistence<-function(fn,args,context,on_complete=NULL,...){queued[[length(queued)+1L]]<<-list(fn=fn,args=args,context=context);TRUE}
supabase_get<-function(table_name,query_params){
  reads<<-reads+1L
  stopifnot(table_name=='rival_finance_evidence_latest',query_params$limit==1L,
    query_params$account_id=='eq.clause-fixture',query_params$championship_id=='eq.league')
  saved
}
wire<-function(record){record$payload$scope<-rev(record$payload$scope)
  as.data.frame(jsonlite::fromJSON(jsonlite::toJSON(list(record),auto_unbox=TRUE,na='null',null='null',digits=NA)))}
check('complete safe subset persists with scoped source timestamp and no authentication fields',{
  stopifnot(persist_clause_transfer_summary(summary,context),length(queued)==1L,
    queued[[1]]$fn=='supabase_post_direct',queued[[1]]$args[[1]]=='rival_finance_evidence',queued[[1]]$args[[4]])
  record<-queued[[1]]$args[[2]][[1]]
  stopifnot(startsWith(record$source_id,'rival-cache:clause-transfer-v1:'),record$evidence_type=='eligibility',
    !grepl('must-not-persist|password|token|secret|points',jsonlite::toJSON(record)),
    record$payload$completeness=='complete',fm_time(record$payload$valid_until)==now+60)
  saved<-wire(record)
})
check('JSONB replay preserves authoritative owner, schedule and original observation deadline',{
  restored<-read_saved_clause_transfer_summary(context,now+1)
  stopifnot(restored$data$id=='p',restored$championship$owner$id=='mine',
    restored$championship$clause$transferred,restored$championship$clause$transferDate=='2026-11-13T22:00:00Z',
    as.numeric(attr(restored,'observed_at'))==as.numeric(now),attr(restored,'valid_until')==as.numeric(now+60),attr(restored,'clause_from_saved'))
  count<-length(queued);stopifnot(!persist_clause_transfer_summary(restored,context),length(queued)==count)
})
check('expiry, future timestamps and every context dimension reject foreign snapshots',{
  stopifnot(is.null(read_saved_clause_transfer_summary(context,now+60)),is.null(read_saved_clause_transfer_summary(context,now-1)))
  original_get<-supabase_get;supabase_get<-function(...)saved
  for(field in c('account_id','championship_id','user_team_id','player_id','season','scoring_version')){
    wrong<-context;wrong[[field]]<-'different';stopifnot(is.null(read_saved_clause_transfer_summary(wrong,now+1)))
  }
  supabase_get<-original_get
})
check('missing or contradictory identity, pending date and source completeness cannot persist',{
  count<-length(queued)
  for(kind in c('player','owner','date','status','stale')) {
    bad<-summary
    if(kind=='player')bad$data$id<-'other'
    if(kind=='owner')bad$championship$owner<-list()
    if(kind=='date')bad$championship$clause$transferDate<-'broken'
    if(kind=='status')bad$championship$clause$transferred<-NULL
    if(kind=='stale')attr(bad,'fetch_status')<-'stale'
    stopifnot(!persist_clause_transfer_summary(bad,context),length(queued)==count)
  }
})
check('fresh durable replay avoids API acquisition; warm navigation avoids another database read',{
  clause_clock<-now+1
  Sys.time<-function()now+1
  get_player_summary<-function(...){source_reads<<-source_reads+1L;summary}
  clear_api_cache();before<-reads
  restored<-get_clause_transfer_summary(c(userid='clause-fixture',token='synthetic'),'league','mine','p')
  stopifnot(attr(restored,'clause_from_saved'),reads==before+1L,source_reads==0L)
  restored<-get_clause_transfer_summary(c(userid='clause-fixture',token='synthetic'),'league','mine','p')
  stopifnot(attr(restored,'clause_from_saved'),reads==before+1L,source_reads==0L)
  Sys.time<-function()now+60
  clause_clock<-now+60
  get_clause_transfer_summary(c(userid='clause-fixture',token='synthetic'),'league','mine','p')
  stopifnot(source_reads==1L,reads==before+2L)
  rm(Sys.time)
})
check('explicit refresh invalidates persisted and in-memory clause observations',{
  Sys.time<-function()now+2
  invalidate_saved_clause_transfers(context)
  stopifnot(is.null(read_saved_clause_transfer_summary(context,now+3)),is.null(cached_clause_transfer_summary(context,now+3)))
  rm(Sys.time)
})
check('failed or unacknowledged writes remain retryable without another source request',{
  attempts<-0L;original_defer<-defer_persistence
  defer_persistence<-function(...){attempts<<-attempts+1L;FALSE}
  stopifnot(!persist_clause_transfer_summary(summary,context),!persist_clause_transfer_summary(summary,context),attempts==2L)
  defer_persistence<-function(...,on_complete=NULL){attempts<<-attempts+1L;if(is.function(on_complete))on_complete(FALSE);TRUE}
  acknowledged<-NULL
  stopifnot(persist_clause_transfer_summary(summary,context,function(ok)acknowledged<<-ok),identical(acknowledged,FALSE),
    persist_clause_transfer_summary(summary,context),attempts==4L,source_reads==1L)
  defer_persistence<-original_defer
})
check('explicit clause cancellation is reusable without inventing a transfer time',{
  cancelled<-summary;cancelled$championship$clause<-list(transferred=FALSE)
  stopifnot(persist_clause_transfer_summary(cancelled,context))
  saved<-wire(tail(queued,1L)[[1]]$args[[2]][[1]])
  rm(list=ls(.clause_transfer_invalidated),envir=.clause_transfer_invalidated)
  restored<-read_saved_clause_transfer_summary(context,now+1)
  stopifnot(identical(restored$championship$clause$transferred,FALSE),identical(restored$championship$clause$transferDate,''))
})
check('a delayed durable read yields cooperatively and preserves its original deadline',{
  clause_clock<-now+1
  clear_api_cache();Sys.time<-function()now+1
  original_url<-get_sb_url;original_key<-get_sb_key;original_http<-fm_async_read_http
  get_sb_url<-function()'https://synthetic.invalid';get_sb_key<-function()'synthetic-key'
  pending<-NULL;completed<-FALSE;value<-NULL;failure<-NULL
  fm_async_read_http<-function(url,method,body=NULL,headers=character(),...){
    stopifnot(method=='GET',grepl('rival_finance_evidence_latest',url,fixed=TRUE))
    promises::promise(function(resolve,reject)pending<<-resolve)
  }
  promise<-async_adapter_fetch('get_clause_transfer_summary',list(login=c(userid='clause-fixture',token='synthetic'),
    championship_id='league',user_team_id='mine',player_id='p'),context=context)
  promises::then(promise,function(result){completed<<-TRUE;value<<-result},function(error){failure<<-error})
  for(i in seq_len(5L))later::run_now(0)
  stopifnot(is.function(pending),!completed,is.null(failure))
  pending(list(status_code=200L,content=charToRaw(jsonlite::toJSON(jsonlite::fromJSON(jsonlite::toJSON(saved,auto_unbox=TRUE,na='null',null='null'),simplifyVector=FALSE),auto_unbox=TRUE,na='null',null='null')),times=list(total=0)))
  for(i in seq_len(12L))later::run_now(0)
  stopifnot(completed,is.null(failure),attr(value,'clause_from_saved'),attr(value,'valid_until')==as.numeric(now+60))
  get_sb_url<-original_url;get_sb_key<-original_key;fm_async_read_http<-original_http;rm(Sys.time)
})
check('a durable observation expiring during a delayed read acquires a fresh summary once',{
  clear_api_cache();clause_clock<-now+59;Sys.time<-function()clause_clock
  original_url<-get_sb_url;original_key<-get_sb_key;original_http<-fm_async_read_http;original_summary<-get_player_summary
  get_sb_url<-function()'https://synthetic.invalid';get_sb_key<-function()'synthetic-key'
  get_player_summary<-function(...){source_reads<<-source_reads+1L;observed_data(summary,observed_at=clause_clock)}
  pending<-NULL;completed<-FALSE;value<-NULL;failure<-NULL;before<-source_reads;database_reads<-0L
  fm_async_read_http<-function(url,method,body=NULL,headers=character(),...){
    stopifnot(method=='GET',grepl('rival_finance_evidence_latest',url,fixed=TRUE));database_reads<<-database_reads+1L
    promises::promise(function(resolve,reject)pending<<-resolve)
  }
  promise<-async_adapter_fetch('get_clause_transfer_summary',list(login=c(userid='clause-fixture',token='synthetic'),
    championship_id='league',user_team_id='mine',player_id='p'),context=context)
  promises::then(promise,function(result){completed<<-TRUE;value<<-result},function(error){failure<<-error})
  for(i in seq_len(5L))later::run_now(0)
  stopifnot(is.function(pending),!completed,source_reads==before)
  clause_clock<-now+61
  pending(list(status_code=200L,content=charToRaw(jsonlite::toJSON(saved,auto_unbox=TRUE,na='null',null='null')),times=list(total=2)))
  for(i in seq_len(12L))later::run_now(0)
  stopifnot(completed,is.null(failure),!isTRUE(attr(value,'clause_from_saved')),source_reads==before+1L,database_reads==1L,
    as.numeric(attr(value,'observed_at'))==as.numeric(now+61))
  get_sb_url<-original_url;get_sb_key<-original_key;fm_async_read_http<-original_http;get_player_summary<-original_summary;rm(Sys.time)
})
cat(sprintf('Clause transfer persistence: %d checks passed. Cold valid replay: one scoped DB read, zero API reads; warm replay: zero reads.\n',checks))
