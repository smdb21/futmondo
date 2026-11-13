options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
ctx<-resource_context(account_id='payment-ack-fixture',championship_id='league')
row<-data.frame(id='locker:payment',locker_event_id='payment',created='2026-11-01T12:00:00Z',delivery_at='',
 player_id='p',player_name='Fixture',buyer_team_id='mine',buyer_team_name='Buyer',
 seller_team_id='rival',seller_team_name='Seller',price=120,is_clause=TRUE)
stamp<-Sys.time();writes<-list();callbacks<-list()
persist_completed_history_baseline<-function(...)TRUE
persist_auction_history<-function(...)TRUE
defer_persistence<-function(fn,args,context,on_complete=NULL,...){
 stopifnot(fn=='supabase_post_direct',args[[1]]=='rival_finance_evidence')
 writes[[length(writes)+1L]]<<-args;callbacks[[length(callbacks)+1L]]<<-on_complete;TRUE
}
run<-function(context=ctx)record_acquired_resource('pressroom_league',observed_data(row,observed_at=stamp),context)
settle<-function()for(i in 1:5)later::run_now(0)
run();run();stopifnot(length(writes)==2L)
callbacks[[2]](TRUE);settle();run();stopifnot(length(writes)==2L)
cat('PASS unacknowledged clause facts retry; acknowledged unchanged payment facts do not append revisions\n')
key<-api_cache_key('history_write_ack_locker',context=ctx)
saved<-api_cache_env[[key]]$data$hashes
wire<-fm_worker_encode(saved);clear_api_cache()
restored<-fm_worker_decode(jsonlite::fromJSON(fm_worker_json(wire),simplifyVector=FALSE))
invisible(resource_store(key,list(hashes=restored),ctx,'history_write_ack_locker'))
run();stopifnot(length(writes)==2L)
cat('PASS typed durable acknowledgement survives cleared caches and suppresses the same immutable event write\n')
row$price<-130;run();stopifnot(length(writes)==3L)
callbacks[[3]](FALSE);settle();run();stopifnot(length(writes)==4L)
callbacks[[4]](TRUE);settle();run();stopifnot(length(writes)==4L)
stopifnot(writes[[4]][[2]][[1]]$payload$transfer$price==130)
cat('PASS revised event facts persist, failed acknowledgements remain retryable and successful revisions reuse\n')
foreign<-ctx;foreign$account_id<-'other-account'
run(foreign);stopifnot(length(writes)==5L)
cat('PASS acknowledgement fingerprints never cross account context\n')
# Completion callback certifies the full requested evidence set, not a truncated subset.
failed<-NULL
oversized<-writes[[1]][[2]][[1]];oversized$payload$extra<-paste(rep('x',270000),collapse='')
stopifnot(!persist_rival_finance_evidence(list(oversized),ctx,on_complete=function(ok)failed<<-ok),identical(failed,FALSE))
cat('PASS skipped oversized facts cannot produce a successful all-rows acknowledgement\n')
