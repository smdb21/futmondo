#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('futmondo_functions.R'))
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
await<-function(p,seconds=3){done<-FALSE;out<-NULL;failure<-NULL;promises::then(p,function(x){out<<-x;done<<-TRUE;NULL},function(e){failure<<-e;done<<-TRUE;NULL});until<-Sys.time()+seconds;while(!done&&Sys.time()<until)later::run_now(.01);stopifnot(done);if(!is.null(failure))stop(failure);out}
settle<-function(p)tryCatch(await(p),error=identity)
flush<-function(seconds=.05){until<-Sys.time()+seconds;while(Sys.time()<until)later::run_now(.01)}
clean<-function(){stopifnot(length(.fm_transport$reads)==0L,length(.fm_transport$read_states)==0L)}
original_http<-fm_async_http
ok<-list(status_code=200L,content=charToRaw('{}'),times=list(total=0))
check('transient read recovers once, coalesces throughout backoff, and leaves the event loop responsive',{
 calls<-0L;beat<-FALSE
 fm_async_http<-function(...){calls<<-calls+1L;if(calls==1L)promises::promise_reject(resource_failure(category='http',http_status=503L))else promises::promise_resolve(ok)}
 started<-Sys.time();first<-fm_async_read_http('https://fixture.invalid/summary','POST',owner='retry')
 later::later(function()beat<<-TRUE,.02);flush(.06)
 second<-fm_async_read_http('https://fixture.invalid/summary','POST',owner='retry')
 stopifnot(beat,calls==1L,identical(first,second),as.numeric(Sys.time()-started)<.2)
 stopifnot(await(first)$status_code==200L,await(second)$status_code==200L,calls==2L);clean()
 cat('MEASURE failed_read_attempts=2 coalesced_consumers=2 heartbeat_during_backoff=TRUE\n')
})
check('persistent transient failures stop after two attempts',{
 calls<-0L;cause<-resource_failure(category='network')
 fm_async_http<-function(...){calls<<-calls+1L;promises::promise_reject(cause)}
 failure<-settle(fm_async_read_http('https://fixture.invalid/rest/v1/history'))
 stopifnot(calls==2L,identical(failure$reference,cause$reference));clean()
})
check('authentication, non-transient HTTP, parser, and capacity errors are never retried',{
 for(cause in list(resource_failure(category='http',http_status=401L),resource_failure(category='http',http_status=403L),resource_failure(category='http',http_status=400L),resource_failure(category='http'),resource_failure(category='parser'),resource_failure(category='read_budget'))){
  calls<-0L;fm_async_http<-function(...){calls<<-calls+1L;promises::promise_reject(cause)}
  failure<-settle(fm_async_read_http('https://fixture.invalid/roster','POST'))
  stopifnot(calls==1L,identical(failure$reference,cause$reference));clean()
 }
})
check('login and manual actions cannot be retried by the factual read coordinator',{
 for(route in c('login','buy','sell','bid','cancelbid','exerciserescission')){
  calls<-0L;fm_async_http<-function(...){calls<<-calls+1L;promises::promise_reject(resource_failure(category='network'))}
  failure<-settle(fm_async_read_http(paste0('https://fixture.invalid/',route),'POST'))
  stopifnot(calls==1L,inherits(failure,'error'));clean()
 }
 stopifnot(!fm_async_read_retry_allowed('https://fixture.invalid/summary','DELETE'))
})
check('session cancellation cancels a backoff without dispatching another request',{
 calls<-0L;fm_async_http<-function(...){calls<<-calls+1L;promises::promise_reject(resource_failure(category='network'))}
 pending<-fm_async_read_http('https://fixture.invalid/summary','POST',owner='cancel-retry');flush(.03)
 fm_transport_cancel('cancel-retry');failure<-settle(pending);flush(.3)
 stopifnot(inherits(failure,'fm_async_cancelled'),calls==1L);clean()
})
check('retrying read states count toward the capacity bound and release on cancellation',{
 calls<-0L;fm_async_http<-function(...){calls<<-calls+1L;promises::promise(function(resolve,reject){})}
 pending<-lapply(seq_len(32L),function(i)fm_async_read_http(paste0('https://fixture.invalid/summary?player=',i),'POST',owner='capacity'))
 extra<-settle(fm_async_read_http('https://fixture.invalid/roster','POST',owner='capacity'))
 stopifnot(calls==32L,extra$category=='read_budget',length(.fm_transport$read_states)==32L)
 results<-lapply(pending,function(p)promises::then(p,function(value)value,function(error)error))
 fm_transport_cancel('capacity');failures<-lapply(results,await)
 stopifnot(all(vapply(failures,inherits,logical(1),'fm_async_cancelled')));clean()
})
check('synchronous dispatch exceptions settle and clean up',{
 calls<-0L;fm_async_http<-function(...){calls<<-calls+1L;stop(resource_failure(category='parser'))}
 failure<-settle(fm_async_read_http('https://fixture.invalid/summary','POST'))
 stopifnot(calls==1L,failure$category=='parser');clean()
})
fm_async_http<-original_http
cat(sprintf('ASYNC READ RECOVERY: %d passed / 0 failed\n',n))
