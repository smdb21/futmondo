# Offline HTTP fixtures exercise real queue acknowledgement and retries.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
clear_api_cache()
get_sb_url<-function()'https://fixture.invalid'
get_sb_key<-function()'fixture'
options(futmondo.offline=FALSE)
ctx<-resource_context(account_id='fixture',championship_id='league',session_id='fixture')
options(futmondo.persistence_context=ctx)
rows<-data.frame(id=paste0('event',seq_len(310)),player_id='player',player_name='Player',
 buyer_team_id='team',buyer_team_name='Team',seller_team_id='',seller_team_name='Market',
 price=seq_len(310),created='2026-11-01T12:00:00Z',stringsAsFactors=FALSE)
rows$bids<-I(rep(list(list()),nrow(rows)))
rows<-observed_data(rows,observed_at=fm_time('2026-11-05T12:00:00Z'))
calls<-list();reject_table<-NULL
fm_async_http<-function(url,method,body,headers,owner){
 table<-tail(strsplit(sub('\\?.*$','',url),'/',fixed=TRUE)[[1]],1)
 payload<-jsonlite::fromJSON(body)
 calls[[length(calls)+1L]]<<-list(table=table,rows=payload)
 if(identical(table,reject_table))promises::promise_reject(simpleError('HTTP 503'))
 else promises::promise_resolve(list(status_code=201L))
}
drain<-function(){
 deadline<-Sys.time()+10
 repeat {later::run_now(.01);if(!length(.persistence_state$queue)&&is.null(.persistence_state$active)){
   later::run_now(.01);if(!length(.persistence_state$queue)&&is.null(.persistence_state$active))break}
   if(Sys.time()>deadline)stop('queue timeout')}
}
count_rows<-function(table)sum(vapply(Filter(function(x)x$table==table,calls),function(x)nrow(x$rows),integer(1)))
stopifnot(isTRUE(persist_auction_history(rows,'league')));drain()
stopifnot(count_rows('market_transactions')==310L,count_rows('auction_observations')==310L,count_rows('bid_observations')==310L)
initial_calls<-length(calls)
attr(rows,'observed_at')<-fm_time('2026-11-05T13:00:00Z')
stopifnot(isTRUE(persist_auction_history(rows,'league')));drain();stopifnot(length(calls)==initial_calls)
cat('PASS 310 saved transactions: unchanged refresh sends zero writes\n')
rows$price[5]<-999;calls<-list()
stopifnot(isTRUE(persist_auction_history(rows,'league')));drain()
stopifnot(count_rows('market_transactions')==1L,count_rows('bid_observations')==1L,calls[[2]]$rows$price==999)
cat('PASS a single correction writes only its transaction and auction/bid evidence\n')
rows$price[7]<-888;reject_table<-'market_transactions';calls<-list()
stopifnot(isTRUE(persist_auction_history(rows,'league')));drain()
stopifnot(identical(vapply(calls,`[[`,character(1),'table'),c('players','market_transactions')))
reject_table<-NULL;calls<-list()
stopifnot(isTRUE(persist_auction_history(rows,'league')));drain()
stopifnot(count_rows('market_transactions')==1L,count_rows('auction_observations')==1L)
cat('PASS failed saves never advance acknowledgement and retry safely\n')
other<-ctx;other$account_id<-'other';options(futmondo.persistence_context=other);calls<-list()
stopifnot(isTRUE(persist_auction_history(rows,'league')));drain();stopifnot(count_rows('market_transactions')==310L)
cat('PASS write reuse stays account scoped\n')
options(futmondo.offline=TRUE,futmondo.persistence_context=NULL)
