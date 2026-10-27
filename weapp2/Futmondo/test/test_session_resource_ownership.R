#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('factual_helpers.R');source('background_runtime.R')})
watch<-function(p){e<-new.env();e$done<-FALSE;e$promise<-promises::then(p,function(x){e$value<-x;e$done<-TRUE;NULL},function(x){e$error<-x;e$done<-TRUE;NULL});e}
drain<-function(...){checks<-list(...);end<-Sys.time()+3;while(!all(vapply(checks,function(x)isTRUE(x$done),logical(1)))&&Sys.time()<end)later::run_now(.02);stopifnot(all(vapply(checks,function(x)isTRUE(x$done),logical(1))))}
clear_api_cache();a<-resource_context(account_id='u',championship_id='L',user_team_id='T',session_id='A');b<-a;b$session_id<-'B'
raw_context<-resource_context(account_id='u',championship_id='L',user_team_id='T');raw_key<-api_cache_key('team_info_L_T',context=raw_context)
invisible(resource_store(raw_key,list(budget=5),raw_context,'team_info_L_T'))
calls<-0L;ra<-rb<-ja<-NULL
pa<-get_cached_resource_async('get_user_team_info:fixture',a,function(){calls<<-calls+1L;promises::promise(function(resolve,reject){ra<<-resolve;ja<<-reject})})
pa2<-get_cached_resource_async('get_user_team_info:fixture',a,function()stop('Duplicate same-session fetch'))
pb<-get_cached_resource_async('get_user_team_info:fixture',b,function(){calls<<-calls+1L;promises::promise(function(resolve,reject){rb<<-resolve})})
stopifnot(calls==2L,identical(pa,pa2));wa<-watch(pa);wb<-watch(pb)
clear_api_cache('u','A');stopifnot(exists(raw_key,api_cache_env,inherits=FALSE))
ja(simpleError('Session request cancelled'));rb(list(budget=7));drain(wa,wb)
stopifnot(wa$value$status=='unavailable',wb$value$status=='ok',wb$value$data$budget==7)
other<-resource_context(account_id='v',championship_id='L',user_team_id='T');other_key<-api_cache_key('team_info_L_T',context=other)
invisible(resource_store(other_key,list(budget=99),other,'team_info_L_T'))
invalidate_manual_resources(c(token='fixture',userid='u'),'L','T')
stopifnot(!exists(raw_key,api_cache_env,inherits=FALSE),exists(other_key,api_cache_env,inherits=FALSE))
cat('PASS same-session coalescing, independent sessions, scoped logout and cash invalidation\n')
get_cancelled_adapter_fixture<-function(login){tryCatch(futmondo_post('https://fixture.invalid/summary'),error=function(e)NULL);futmondo_post('https://fixture.invalid/lineup')}
original_transport<-fm_async_read_http;n<-0L
fm_async_read_http<-function(...){n<<-n+1L;promises::promise_reject(simpleError('Session request cancelled'))}
x<-watch(async_adapter_fetch('get_cancelled_adapter_fixture',list(login=c(token='fixture',userid='u')),a));drain(x)
stopifnot(n==1L,inherits(x$error,'error'));fm_async_read_http<-original_transport
cat('PASS cancellation stops composite adapter before next read\n')
parents<-data.frame(id='p',name='Player',slug='');tx<-data.frame(championship_id='L',source_event_id='event',observed_at='2026-01-01T00:00:00Z',price=1)
ops<-list(list('players',parents,'id',TRUE),list('market_transactions',tx,'championship_id,source_event_id',FALSE))
first<-persistence_job_scope('supabase_fact_batch_direct',list(ops));ops[[2]][[2]]$observed_at<-'2026-01-02T00:00:00Z';ops[[2]][[2]]$price<-2
stopifnot(identical(first,persistence_job_scope('supabase_fact_batch_direct',list(ops))))
cat('PASS batch failure identity survives refreshed times and corrections\n')
