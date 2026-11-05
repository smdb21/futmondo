#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
clear_api_cache();options(futmondo.offline=FALSE)
ctx<-resource_context(account_id='queue-fixture',championship_id='league',session_id='queue-session')
options(futmondo.persistence_context=ctx)
rows<-data.frame(id=paste0('event',seq_len(310)),player_id='player',player_name='Player',
 buyer_team_id='team',buyer_team_name='Team',seller_team_id='',seller_team_name='Market',
 price=seq_len(310),created='2026-11-01T12:00:00Z',stringsAsFactors=FALSE)
rows$bids<-I(rep(list(list()),nrow(rows)));rows<-observed_data(rows,observed_at=fm_time('2026-11-05T12:00:00Z'))
stopifnot(persist_auction_history(rows,'league'),length(.persistence_state$queue)==1L)
job<-.persistence_state$queue[[1]];callback<-job$callbacks[[1]];state<-environment(callback)$state
stopifnot(identical(sort(ls(environment(callback))),c('hashes','state')),identical(parent.env(state),emptyenv()),
 nrow(state$rows)==310L,job$bytes>as.numeric(object.size(job$args))+as.numeric(object.size(state$rows)))
first_bytes<-job$bytes
# Simulate all four successful HTTP operations, then cancel before the next batch.
.persistence_state$queue<-list();persistence_record_result(job,TRUE)
stopifnot(length(.persistence_state$queue)==1L,nrow(state$rows)==210L,.persistence_state$queue[[1]]$bytes<first_bytes)
next_bytes<-.persistence_state$queue[[1]]$bytes
cancel_persistence_jobs(ctx$account_id,ctx$session_id)
later::run_now(.01)
stopifnot(length(.persistence_state$queue)==0L,is.null(.persistence_state$active),is.null(state$rows),is.null(state$signatures))
cat(sprintf('PASS continuation memory is counted and shrinks %d -> %d bytes; logout cancels queued remainder with no delayed revival.\n',first_bytes,next_bytes))
# Each duplicate callback can hold a distinct continuation, so count it too.
arguments<-list('players',data.frame(id='p',name='P'),'id',TRUE)
first_result<-second_result<-NULL
stopifnot(defer_persistence('supabase_post_direct',arguments,ctx,on_complete=function(ok)first_result<<-ok,retained_bytes=10*1024^2))
stopifnot(!defer_persistence('supabase_post_direct',arguments,ctx,on_complete=function(ok)second_result<<-ok,retained_bytes=7*1024^2),
 identical(second_result,FALSE),length(.persistence_state$queue)==1L,length(.persistence_state$queue[[1]]$callbacks)==1L)
cancel_persistence_jobs(ctx$account_id,ctx$session_id);later::run_now(.01)
stopifnot(identical(first_result,FALSE),length(.persistence_state$queue)==0L)
for(value in list(-1,NA_real_,Inf,numeric(),c(1,2),'1'))
 stopifnot(!defer_persistence('supabase_post_direct',arguments,ctx,retained_bytes=value))
stopifnot(!defer_persistence('supabase_post_direct',arguments,ctx,retained_bytes=17*1024^2),length(.persistence_state$queue)==0L)
cat('PASS duplicate continuations share the 16MiB queue budget and malformed retained-byte declarations are rejected.\n')
# A duplicate may attach while an HTTP operation is active; its memory must
# survive requeueing the remaining operations in that same ordered job.
get_sb_url<-function()'https://queue.fixture.invalid';get_sb_key<-function()'fixture-only'
respond<-NULL
fm_async_http<-function(...)promises::promise(function(resolve,reject)respond<<-resolve)
operations<-list(list('players',data.frame(id='p2',name='P2'),'id',TRUE),
 list('market_transactions',data.frame(player_id='p2',championship_id='league',source_event_id='e2',price=1),'championship_id,source_event_id',FALSE))
stopifnot(defer_persistence('supabase_pressroom_history_batch_direct',list(operations),ctx,on_complete=function(ok)NULL,retained_bytes=1024))
original_bytes<-.persistence_state$queue[[1]]$bytes
persistence_tick()
stopifnot(!is.null(.persistence_state$active),is.function(respond),
 defer_persistence('supabase_pressroom_history_batch_direct',list(operations),ctx,on_complete=function(ok)NULL,retained_bytes=2048),
 .persistence_state$active$bytes==original_bytes+2048)
respond(list(status_code=201L))
deadline<-Sys.time()+2
repeat{
 later::run_now(.001)
 jobs<-c(.persistence_state$queue,if(!is.null(.persistence_state$active))list(.persistence_state$active)else list())
 progressed<-Filter(function(job)identical(job$operation_index,2L),jobs)
 if(length(progressed))break
 if(Sys.time()>deadline)stop('next operation was not queued')
}
stopifnot(progressed[[1]]$bytes==original_bytes+2048,length(progressed[[1]]$callbacks)==2L)
cancel_persistence_jobs(ctx$account_id,ctx$session_id);later::run_now(.01)
stopifnot(length(.persistence_state$queue)==0L,is.null(.persistence_state$active))
cat('PASS callbacks attached during active HTTP keep their memory accounting through the remaining batch operations.\n')
options(futmondo.offline=TRUE,futmondo.persistence_context=NULL)
