options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
clear_api_cache();clear_completed_history_baselines();record_acquired_resource<-function(...)TRUE
# Fifty days of observations, with a dense final six hours. This requires more
# than a wholly known first page to cover the one-day revision window.
start<-as.POSIXct('2026-08-01',tz='UTC')
times<-start+c(seq_len(800)*3600,800*3600+seq_len(400)*60)
item<-function(time,price=1)list(`_id`=sprintf('%08x0000000000000001',as.integer(time)),created=format(time,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),`_player`=list(`_id`='p',name='Player'),`_buyer`=list(`_id`='a',name='A'),price=price)
news<-lapply(times,function(time)item(time))
requests<-0L;mode<-'ok';fail_at<-Inf
fm_async_read_http<-function(url,method,body,headers,owner){
 query<-jsonlite::fromJSON(body)$query
 if(endsWith(url,'/news'))answer<-list(news=list())else{
  requests<<-requests+1L
  if(requests==fail_at)return(promises::promise_reject(simpleError('Connection failed')))
  rows<-Filter(function(x)!nzchar(query$from)||x$`_id`<query$from,news)
  if(mode=='repeat')rows<-news
  rows<-rows[order(vapply(rows,function(x)x$created,character(1)),decreasing=TRUE)];answer<-list(news=head(rows,150L))
 }
 promises::promise_resolve(list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=answer),auto_unbox=TRUE)),times=list(total=0)))
}
await<-function(p){done<-FALSE;out<-NULL;error<-NULL;promises::then(p,function(x){out<<-x;done<<-TRUE},function(e){error<<-e;done<<-TRUE});deadline<-Sys.time()+20;while(!done&&Sys.time()<deadline)later::run_now(.01);stopifnot(done);if(!is.null(error))stop(error);out}
auth<-c(userid='u',token='fixture');ctx<-resource_context(auth,'c',session_id='fixture');args<-list(login=auth,championship_id='c')
read<-function()await(async_adapter_fetch('get_championship_pressroom',args,ctx))
key<-api_cache_key('pressroom_c',context=resource_context(auth,'c'))
expire<-function(){api_cache_env[[key]]$time<-Sys.time()-301}
x<-read();full_reads<-requests
stopifnot(nrow(x)==1200L,identical(attr(x,'fetch_status'),'ok'),requests>5L,is.null(attr(x,'history_delta_ids')))
before<-requests;invisible(read());stopifnot(requests==before)
expire();news[[1000]]$price<-999
before<-requests;x<-read();overlap_reads<-requests-before
stopifnot(overlap_reads>1L,overlap_reads<full_reads,nrow(x)==1200L,x$price[x$id==news[[1000]]$`_id`]==999,identical(attr(x,'history_delta_ids'),news[[1000]]$`_id`))
# A new event and a late-arriving event within the overlap are both retained.
news[[1201]]<-item(tail(times,1)+3600,7)
news[[1202]]<-item(tail(times,1)-18000+1,8)
expire();before<-requests;x<-read()
stopifnot(nrow(x)==1202L,!anyDuplicated(x$id),requests-before<full_reads,setequal(attr(x,'history_delta_ids'),vapply(news[1201:1202],function(item)item$`_id`,character(1))))
# Ordinary Refresh invalidates display state, retaining the complete baseline.
invalidate_resources(resource_context(auth,'c'),pattern='pressroom')
before<-requests;x<-read();stopifnot(requests-before<full_reads,identical(attr(x,'history_delta_ids'),character()))
# Full audits remain available weekly for revisions beyond the overlap.
news[[1]]$price<-91
attr(.transaction_history_baselines[[key]]$data,'full_history_at')<-Sys.time()-8*86400
expire();before<-requests;x<-read()
stopifnot(requests-before>=full_reads,x$price[x$id==news[[1]]$`_id`]==91,identical(attr(x,'history_delta_ids'),news[[1]]$`_id`))
# A saturated repeated cursor cannot advance the last successful baseline.
previous<-read_completed_history_baseline('pressroom_c',resource_context(auth,'c'))
mode<-'repeat';expire();x<-read()
stopifnot(identical(attr(x,'fetch_status'),'partial'),identical(read_completed_history_baseline('pressroom_c',resource_context(auth,'c')),previous))
mode<-'ok';invalidate_resources(resource_context(auth,'c'),pattern='pressroom');invisible(read())
# A failed overlap also retains the prior complete baseline and watermark.
previous<-read_completed_history_baseline('pressroom_c',resource_context(auth,'c'))
news[[1203]]<-item(tail(times,1)+7200,9);expire();fail_at<-requests+2L
x<-suppressMessages(read())
stopifnot(identical(attr(x,'fetch_status'),'stale'),identical(read_completed_history_baseline('pressroom_c',resource_context(auth,'c')),previous))
fail_at<-Inf;expire();x<-read();stopifnot(nrow(x)==1203L)
# Malformed source amounts must not silently become zero or certify a baseline.
previous<-read_completed_history_baseline('pressroom_c',resource_context(auth,'c'))
news[[1203]]$price<-NULL;expire();x<-suppressMessages(read())
stopifnot(identical(attr(x,'fetch_status'),'stale'),identical(read_completed_history_baseline('pressroom_c',resource_context(auth,'c')),previous))
news[[1203]]$price<-9
clear_completed_history_baselines('u','another-session')
stopifnot(identical(read_completed_history_baseline('pressroom_c',resource_context(auth,'c')),previous))
cat(sprintf('PASS complete baseline, warm reuse, one-day overlap revisions/late arrivals, refresh reuse, weekly audit and unchanged failure watermark; full_reads=%d overlap_reads=%d warm_reads=0\n',full_reads,overlap_reads))
# Recreate a process-cold read from an actual encoded database snapshot. Only
# transport and the external database row are mocked; decoding and acquisition
# use their real production functions through the cooperative async adapter.
saved_url<-get_sb_url;saved_key<-get_sb_key;saved_defer<-defer_persistence;saved_http<-fm_async_read_http
get_sb_url<-function()'https://fixture.invalid';get_sb_key<-function()'synthetic-key'
database<-NULL;database_reads<-0L
defer_persistence<-function(fn,args,context,on_complete){
 stopifnot(fn=='supabase_post_direct',args[[1]]=='complete_history_baselines')
 database<<-as.data.frame(args[[2]],stringsAsFactors=FALSE);on_complete(TRUE);TRUE
}
stopifnot(isTRUE(persist_completed_history_baseline('pressroom_c',previous,resource_context(auth,'c'))),is.data.frame(database))
original_database<-database
fm_async_read_http<-function(url,method,body,headers,owner){
 if(grepl('/complete_history_baselines',url,fixed=TRUE)){
  database_reads<<-database_reads+1L;query<-httr::parse_url(url)$query
  stopifnot(method=='GET',query$account_id=='eq.u',query$championship_id=='eq.c',query$limit=='1')
  rows<-if(query$resource=='eq.pressroom_c')database else database[FALSE,,drop=FALSE]
  return(promises::promise_resolve(list(status_code=200L,content=charToRaw(jsonlite::toJSON(rows,dataframe='rows',auto_unbox=TRUE)),times=list(total=0))))
 }
 saved_http(url,method,body,headers,owner)
}
clear_api_cache();clear_completed_history_baselines();before<-requests
x<-read()
stopifnot(nrow(x)==1203L,identical(attr(x,'fetch_status'),'ok'),requests-before==3L,database_reads==2L,
 x$price[x$id==news[[1]]$`_id`]==91,identical(attr(x,'history_delta_ids'),character()))
# A process-cold source failure restores but cannot alter the acknowledged
# complete snapshot; a subsequent retry continues from that same baseline.
clear_api_cache();clear_completed_history_baselines();fail_at<-requests+1L;before_db<-database_reads
failure<-suppressMessages(tryCatch(read(),error=identity))
restored<-read_completed_history_baseline('pressroom_c',resource_context(auth,'c'))
stopifnot(inherits(failure,'futmondo_unavailable'),identical(database,original_database),nrow(restored)==1203L,
 as.numeric(attr(restored,'source_latest_at'))==as.numeric(attr(previous,'source_latest_at')),database_reads-before_db==2L)
fail_at<-Inf;before<-requests;invisible(read());stopifnot(requests-before==3L,identical(database,original_database))
get_sb_url<-saved_url;get_sb_key<-saved_key;defer_persistence<-saved_defer;fm_async_read_http<-saved_http
cat('PASS cold persisted baseline restores all 1,203 rows with 2 database reads and 3 overlap API pages; failed source leaves snapshot unchanged\n')
# Account and league keys do not reuse another private baseline.
stopifnot(is.null(read_completed_history_baseline('pressroom_c',resource_context(account_id='other',championship_id='c'))),is.null(read_completed_history_baseline('pressroom_other',resource_context(auth,'other'))))
# A newly configured known season cannot adopt an unknown-season snapshot or
# the display cache. Explicit known contexts retain their own complete history.
configured<-Sys.getenv('FUTMONDO_SEASON_CONTEXTS',unset=NA_character_)
Sys.setenv(FUTMONDO_SEASON_CONTEXTS='{"c":"season-one"}')
stopifnot(is.null(read_completed_history_baseline('pressroom_c',resource_context(auth,'c'))))
write_completed_history_baseline('pressroom_c',previous,resource_context(auth,'c'))
stopifnot(identical(read_completed_history_baseline('pressroom_c',resource_context(auth,'c')),previous))
Sys.setenv(FUTMONDO_SEASON_CONTEXTS='{"c":"season-two"}')
stopifnot(is.null(read_completed_history_baseline('pressroom_c',resource_context(auth,'c'))),identical(read_completed_history_baseline('pressroom_c',resource_context(auth,'c',season='season-one')),previous))
if(is.na(configured))Sys.unsetenv('FUTMONDO_SEASON_CONTEXTS')else Sys.setenv(FUTMONDO_SEASON_CONTEXTS=configured)
cat('PASS known/unknown season isolation without adopting the unscoped display cache\n')
# Retention is bounded independently of the normal resource cache.
for(i in seq_len(40))write_completed_history_baseline(paste0('test_',i),previous,resource_context(auth,paste0('c',i)))
stopifnot(length(ls(.transaction_history_baselines))<=32L,sum(vapply(as.list(.transaction_history_baselines),function(item)item$bytes,numeric(1)))<=16*1024^2)
cat('PASS account/league isolation and bounded complete-history retention\n')

# Native delivery time is the watermark even when the cash ledger uses the
# earlier clause payment date. Typed storage round-trips do not create deltas.
cutoff<-as.POSIXct('2026-11-05 10:00:00',tz='UTC')+.5
stopifnot(!transaction_history_passed_window(list(item(cutoff-.25)),cutoff),transaction_history_passed_window(list(item(cutoff-1)),cutoff))
original<-data.frame(id='a',price=1L,bids=I(list(list(list(bid=2L)))))
restored<-original;restored$price<-as.numeric(restored$price);restored$bids<-I(list(list(list(bid=2))))
stopifnot(identical(transaction_history_delta_ids(restored,original),character()))
clear_api_cache();clear_completed_history_baselines()
get_locker_clause_payments<-function(...)observed_data(data.frame(id='locker:payment',locker_event_id='payment',created='2026-11-01T00:00:00Z',player_name='Player',buyer_team_name='A',seller_team_name='B',price=12,is_clause=TRUE))
get_teams<-function(...)data.frame(teamid=c('a','b'),teamname=c('A','B'))
delivery<-as.POSIXct('2026-11-05',tz='UTC');news<-list(item(delivery,12));news[[1]]$`_seller`<-list(`_id`='b',name='B')
x<-read()
stopifnot(x$created=='2026-11-01T00:00:00Z',x$delivery_at==news[[1]]$created,as.numeric(attr(x,'source_latest_at'))==as.numeric(delivery))
expire();x<-read()
stopifnot(x$created=='2026-11-01T00:00:00Z',x$delivery_at==news[[1]]$created,as.numeric(attr(x,'source_latest_at'))==as.numeric(delivery),identical(attr(x,'history_delta_ids'),character()))
cat('PASS overlap boundary-second replay, storage type stability, and native delivery watermark independent of payment date\n')

# An exhausted source is a complete current snapshot, including a cleared feed;
# do not retain disappeared native rows merely because a baseline was available.
get_locker_clause_payments<-function(...)observed_data(data.frame())
news<-list();expire();x<-read()
stopifnot(nrow(x)==0L,identical(attr(x,'fetch_status'),'ok'),is.null(attr(x,'source_latest_at')),nrow(read_completed_history_baseline('pressroom_c',resource_context(auth,'c')))==0L)
cat('PASS exhausted or reset feed replaces the previous complete source snapshot\n')
