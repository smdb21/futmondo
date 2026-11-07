#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('futmondo_functions.R'))
record_acquired_resource<-function(...)invisible(TRUE)
checks<-0L;check<-function(label,expr){force(expr);checks<<-checks+1L;cat('PASS',label,'\n')}
fixture_now<-base::Sys.time();Sys.time<-function()fixture_now
ctx<-resource_context(account_id='source-expiry-fixture',championship_id='league',user_team_id='mine')
await<-function(p){done<-FALSE;value<-NULL;failure<-NULL;promises::then(p,function(x){value<<-x;done<<-TRUE},function(e){failure<<-e;done<<-TRUE});until<-base::Sys.time()+3;while(!done&&base::Sys.time()<until)later::run_now(.01);stopifnot(done);if(!is.null(failure))stop(failure);value}
snapshot<-function(revision=1L,seconds=15)list(status='ok',revision=revision,valid_until=as.numeric(Sys.time())+seconds)
check('ordinary cache entries and fresh capacity snapshots retain warm reuse',{
 clear_api_cache();calls<-0L
 fetch<-function(){calls<<-calls+1L;snapshot()}
 first<-await(get_cached_resource_async('card_capacity',ctx,fetch))
 for(i in seq_len(10L))stopifnot(await(get_cached_resource_async('card_capacity',ctx,fetch))$data$revision==1L)
 stopifnot(calls==1L)
 ordinary<-function(){calls<<-calls+1L;list(value=42)}
 await(get_cached_resource_async('ordinary',ctx,ordinary));await(get_cached_resource_async('ordinary',ctx,ordinary))
 stopifnot(calls==2L)
})
check('source expiry at sixteen seconds overrides a twenty second outer cache TTL',{
 clear_api_cache();calls<-0L
 fetch<-function(){calls<<-calls+1L;snapshot(calls)}
 first<-await(get_cached_resource_async('card_capacity',ctx,fetch));fixture_now<-fixture_now+16
 stopifnot(resource_policy('card_capacity')$ttl==20)
 second<-await(get_cached_resource_async('card_capacity',ctx,fetch))
 stopifnot(calls==2L,second$data$revision==2L,second$data$valid_until>as.numeric(Sys.time()),second$status=='ok')
})
check('exact source deadline refreshes once and concurrent consumers coalesce',{
 clear_api_cache();calls<-0L;resolve_next<-NULL
 await(get_cached_resource_async('card_capacity',ctx,function()snapshot()))
 fixture_now<-fixture_now+15
 fetch<-function(){calls<<-calls+1L;promises::promise(function(resolve,reject)resolve_next<<-resolve)}
 first<-get_cached_resource_async('card_capacity',ctx,fetch);second<-get_cached_resource_async('card_capacity',ctx,fetch)
 stopifnot(calls==1L);resolve_next(snapshot(2L))
 stopifnot(await(first)$data$revision==2L,await(second)$data$revision==2L)
})
check('malformed supplied deadlines cannot be reused as fresh',{
 for(deadline in list(NULL,numeric(),NA_real_,NaN,Inf,-Inf,c(1,2),'2999999999',list(2999999999))){
  clear_api_cache();bad<-list(status='ok',valid_until=deadline)
  resource_store(api_cache_key('card_capacity',context=ctx),bad,ctx,'card_capacity')
  calls<-0L
  got<-await(get_cached_resource_async('card_capacity',ctx,function(){calls<<-calls+1L;snapshot(2L)}))
  stopifnot(calls==1L,got$data$revision==2L,got$status=='ok')
 }
})
check('failed refresh retains stale evidence and a truthful blocked state',{
 clear_api_cache();first<-await(get_cached_resource_async('card_capacity',ctx,function()snapshot()))
 fixture_now<-fixture_now+16
 got<-suppressMessages(await(get_cached_resource_async('card_capacity',ctx,function()stop(resource_failure(category='network',resource='information')))))
 stopifnot(got$status=='stale',got$freshness=='stale',got$data$valid_until==first$data$valid_until,
  !evaluate_acquisition_preflight(got$data,'bid',100)$ok)
})
check('synchronous cache reuse preserves the same composite source deadline',{
 clear_api_cache();calls<-0L
 read<-function()get_cached_data('acq_capacity_fixture',{calls<<-calls+1L;snapshot(calls)},context=ctx)
 first<-read();stopifnot(read()$revision==first$revision,calls==1L)
 fixture_now<-fixture_now+15;stopifnot(read()$revision==2L,calls==2L)
})
check('calendar frames retain their source deadline across outer cache reuse',{
 clear_api_cache();calls<-0L
 fetch<-function(){calls<<-calls+1L;rows<-data.frame(round_number=calls,is_finished=TRUE);attr(rows,'valid_until')<-as.numeric(Sys.time())+15;rows}
 first<-await(get_cached_resource_async('get_finished_rounds',ctx,fetch))
 stopifnot(await(get_cached_resource_async('get_finished_rounds',ctx,fetch))$data$round_number==1L,calls==1L)
 fixture_now<-fixture_now+15
 second<-await(get_cached_resource_async('get_finished_rounds',ctx,fetch))
 stopifnot(calls==2L,second$data$round_number==2L,
  identical(names(second$data),c('round_number','is_finished')),!resource_snapshot_unexpired(first$data))
})
cat(sprintf('RESOURCE SOURCE EXPIRY: %d groups passed; warm requests unchanged, one coalesced read on source expiry\n',checks))
