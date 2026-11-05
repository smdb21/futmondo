#!/usr/bin/env Rscript
# Actual curl sockets against synthetic loopback data; no external accounts.
options(futmondo.offline=FALSE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R')})
checks<-0L;check<-function(label,expr){force(expr);checks<<-checks+1L;cat('PASS',label,'\n')}
await<-function(p,seconds=8){
 done<-FALSE;value<-NULL;failure<-NULL
 promises::then(p,function(x){value<<-x;done<<-TRUE;NULL},function(e){failure<<-e;done<<-TRUE;NULL})
 end<-Sys.time()+seconds;while(!done&&Sys.time()<end)later::run_now(.005)
 stopifnot(done);if(!is.null(failure))stop(failure);value
}
flush<-function(seconds=.03){end<-Sys.time()+seconds;while(Sys.time()<end)later::run_now(.005)}
settle_error<-function(p)tryCatch(await(p),error=identity)
handled<-function(p)promises::then(p,identity,function(error)error)
clean<-function(){flush(.02);stopifnot(!length(.fm_transport$tasks),!length(.fm_transport$reads),
 !length(.fm_transport$read_states),fm_transport_admitted()==0L,!isTRUE(.fm_transport$scheduled),is.null(.fm_transport$timer))}
main<-function(){
 port_file<-tempfile();server_log<-tempfile()
 system2('node',c('test/http_priority_fixture.cjs',shQuote(port_file)),wait=FALSE,stdout=server_log,stderr=server_log)
 end<-Sys.time()+3;while(!file.exists(port_file)&&Sys.time()<end)Sys.sleep(.01);stopifnot(file.exists(port_file))
 base<-paste0('http://127.0.0.1:',readLines(port_file,warn=FALSE))
 on.exit({try(httr::GET(paste0(base,'/shutdown'),httr::timeout(1)),silent=TRUE);unlink(c(port_file,server_log))},add=TRUE)
 url<-function(route,delay=20,run='default',extra='')paste0(base,'/',route,'?delay=',delay,'&run=',run,extra)
 stats<-function(run)jsonlite::fromJSON(httr::content(httr::GET(paste0(base,'/stats?run=',run),httr::timeout(2)),as='text',encoding='UTF-8'))
 invisible(promises::promise_resolve(NULL));flush(.01)
 check('libcurl internal queue consumes timeout; scheduler gives queued request its full network timeout',{
  pool<-curl::new_pool(total_con=4L,host_con=4L);results<-list();handles<-list()
  for(i in 1:5)local({id<-i;handle<-curl::new_handle(url=url(paste0('request-',i),if(i<=4)250 else 20,'old-timeout'))
   curl::handle_setopt(handle,timeout_ms=if(i<=4)1500L else 100L)
   handles[[i]]<<-handle
   curl::multi_add(handle,done=function(response)results[[id]]<<-TRUE,fail=function(error)results[[id]]<<-FALSE,pool=pool)
  })
  curl::multi_run(timeout=2,pool=pool)
  stopifnot(identical(results[[5]],FALSE),is.null(stats('old-timeout')$counts[['/request-5']]))
  blockers<-lapply(1:4,function(i)fm_async_http(url(paste0('block-',i),250,'new-timeout'),owner='timeout'))
  flush(.03);start<-Sys.time()
  target<-fm_async_http(url('target',20,'new-timeout'),owner='timeout',timeout=.1)
  stopifnot(await(target)$status_code==200L,as.numeric(Sys.time()-start)>.18)
  await(promises::promise_all(.list=blockers));clean()
  stopifnot(stats('new-timeout')$counts[['/target']]==1L,stats('new-timeout')$maxActive<=4L)
 })
 measure<-function(priority){
  run<-paste0('contention-',priority);start<-as.numeric(Sys.time())*1000;events<-list();completed<-NULL;beat<-FALSE
  gather<-function(p)promises::then(p,function(response){event<-jsonlite::fromJSON(rawToChar(response$content));events[[length(events)+1L]]<<-event;event})
  jobs<-lapply(c('recent-A','recent-B','save'),function(x)gather(fm_async_http(url(x,500,run),owner=run)))
  flush(.025);later::later(function(){beat<<-TRUE},.02)
  capacity<-function(i=1L)promises::then(gather(fm_async_read_http(url(paste0('capacity-',i),20,run),owner=run,priority=priority)),function(event){
   if(i<4L)capacity(i+1L)else{completed<<-as.numeric(Sys.time())*1000;event}
  })
  for(name in c('summary','lineup','profile','matches','valuation'))jobs[[length(jobs)+1L]]<-gather(fm_async_http(url(name,250,run),owner=run))
  jobs[[length(jobs)+1L]]<-capacity();await(promises::promise_all(.list=jobs));clean()
  frame<-dplyr::bind_rows(events);caps<-frame[grepl('capacity-',frame$route),]
  stopifnot(nrow(frame)==12L,max(frame$maxActive)<=4L,beat,sum(caps$delay)==80L)
  list(total=completed-start,first=min(caps$arrival)-start)
 }
 check('interactive sequential reads overtake optional work with unchanged request and connection counts',{
  ordinary<-measure('normal');priority<-measure('interactive')
  stopifnot(priority$total<ordinary$total*.6,priority$total<500)
  cat(sprintf('MEASURE capacity_normal_ms=%.0f capacity_interactive_ms=%.0f server_work_ms=80 requests_each=12 max_connections=4\n',ordinary$total,priority$total))
 })
 check('normal FIFO requests run after at most four interactive dispatches',{
  original_start<-fm_transport_start;order<-character();.fm_transport$interactive_run<-0L
  fm_transport_start<<-function(id){order<<-c(order,basename(sub('\\?.*$','',.fm_transport$tasks[[id]]$url)));original_start(id)}
  jobs<-c(lapply(1:3,function(i)fm_async_http(url(paste0('normal-',i),20,'fairness'),owner='fairness')),
   lapply(1:12,function(i)fm_async_http(url(paste0('high-',i),20,'fairness'),owner='fairness',priority='interactive')))
  await(promises::promise_all(.list=jobs));fm_transport_start<<-original_start;clean()
  stopifnot(identical(order[grepl('normal-',order)],paste0('normal-',1:3)),identical(which(grepl('normal-',order)),c(5L,10L,15L)))
 })
 check('coalescing promotes an already queued read without adding a request',{
  original_start<-fm_transport_start;order<-character();.fm_transport$interactive_run<-0L
  fm_transport_start<<-function(id){order<<-c(order,basename(sub('\\?.*$','',.fm_transport$tasks[[id]]$url)));original_start(id)}
  jobs<-lapply(1:4,function(i)fm_async_http(url(paste0('block-',i),150,'promotion'),owner='promotion'));flush(.03)
  for(i in 1:4)jobs[[length(jobs)+1L]]<-fm_async_http(url(paste0('optional-',i),150,'promotion'),owner='promotion')
  low<-fm_async_read_http(url('target',20,'promotion'),owner='promotion')
  high<-fm_async_read_http(url('target',20,'promotion'),owner='promotion',priority='interactive')
  stopifnot(identical(low,high),length(.fm_transport$read_states)==1L,
   all(vapply(Filter(function(task)!is.null(task$read_key),.fm_transport$tasks),function(task)task$priority=='interactive',logical(1))))
  jobs[[length(jobs)+1L]]<-low;await(promises::promise_all(.list=jobs));fm_transport_start<<-original_start;clean()
  stopifnot(order[5]=='target',stats('promotion')$counts[['/target']]==1L)
 })
 check('direct work plus logical reads share 32 admissions; active and queued cancellation settle once',{
  jobs<-c(lapply(1:16,function(i)handled(fm_async_http(url(paste0('direct-',i),300,'admission'),owner='admission'))),
   lapply(1:16,function(i)handled(fm_async_read_http(url(paste0('read-',i),300,'admission'),owner='admission'))))
  stopifnot(fm_transport_admitted()==32L,length(.fm_transport$tasks)==32L,length(.fm_transport$read_states)==16L)
  stopifnot(settle_error(fm_async_http(url('extra-direct'),owner='admission'))$category=='read_budget',
   settle_error(fm_async_read_http(url('extra-read'),owner='admission'))$category=='read_budget')
  flush(.03);stopifnot(sum(vapply(.fm_transport$tasks,function(task)isTRUE(task$active),logical(1)))==4L)
  fm_transport_cancel('admission');result<-await(promises::promise_all(.list=jobs));clean()
  stopifnot(length(result)==32L,all(vapply(result,inherits,logical(1),'fm_async_cancelled')),
   nrow(stats('admission')$events)<=4L)
 })
 check('retry backoff reserves capacity and promotion survives to the queued retry',{
  result<-handled(fm_async_read_http(url('flaky',20,'retry-bound','&fail_first=1'),owner='retry-bound'))
  flush(.08);stopifnot(length(.fm_transport$tasks)==0L,length(.fm_transport$read_states)==1L)
  promoted<-fm_async_read_http(url('flaky',20,'retry-bound','&fail_first=1'),owner='retry-bound',priority='interactive')
  jobs<-lapply(1:31,function(i)handled(fm_async_http(url(paste0('direct-',i),600,'retry-bound'),owner='retry-bound')))
  stopifnot(fm_transport_admitted()==32L)
  flush(.3)
  retry_tasks<-Filter(function(task)!is.null(task$read_key),.fm_transport$tasks)
  stopifnot(fm_transport_admitted()==32L,length(.fm_transport$tasks)==32L,length(retry_tasks)==1L,
   retry_tasks[[1]]$priority=='interactive',!retry_tasks[[1]]$active)
  fm_transport_cancel('retry-bound');stopifnot(inherits(await(result),'fm_async_cancelled'))
  await(promises::promise_all(.list=jobs));clean()
 })
 check('adapter priority covers every replay stage, supports override and preserves owner',{
  old_summary<-get_player_summary;old_url<-get_sb_url;old_key<-get_sb_key
  original_start<-fm_transport_start;priorities<-character();owners<-character()
  get_sb_url<<-function()base;get_sb_key<<-function()'synthetic'
  get_player_summary<<-eval(quote(function(){for(i in 1:4)supabase_get(paste0('stage-',i),list(run='adapter',delay=10));TRUE}),envir=.GlobalEnv)
  fm_transport_start<<-function(id){priorities<<-c(priorities,.fm_transport$tasks[[id]]$priority);owners<<-c(owners,.fm_transport$tasks[[id]]$owner);original_start(id)}
  ctx<-resource_context(account_id='fixture',session_id='adapter-owner')
  stopifnot(isTRUE(await(async_adapter_fetch('get_player_summary',context=ctx))),identical(priorities,rep('interactive',4L)),all(owners=='adapter-owner'))
  priorities<-character();stopifnot(isTRUE(await(async_adapter_fetch('get_player_summary',context=ctx,priority='normal'))),identical(priorities,rep('normal',4L)))
  fm_transport_start<<-original_start;get_player_summary<<-old_summary;get_sb_url<<-old_url;get_sb_key<<-old_key;clean()
 })
 check('synchronous dispatch failure cancels its handle and releases the next queued request',{
  original_metric<-resource_metric;fail_once<-TRUE
  resource_metric<<-function(key,amount=1L){
   if(key=='requests'&&fail_once){fail_once<<-FALSE;stop(resource_failure(category='parser'))}
   original_metric(key,amount)
  }
  failed<-handled(fm_async_http(url('failed-start',20,'start-failure'),owner='start-failure'))
  next_request<-fm_async_http(url('next-start',20,'start-failure'),owner='start-failure')
  stopifnot(inherits(await(failed),'error'),await(next_request)$status_code==200L)
  resource_metric<<-original_metric;clean()
  stopifnot(is.null(stats('start-failure')$counts[['/failed-start']]),stats('start-failure')$counts[['/next-start']]==1L)
 })
 check('HTTP errors remain single attempts for manual operations and release capacity',{
  before<-stats('manual')$events
  failure<-settle_error(fm_async_http(url('bid',10,'manual','&fail_first=1'),'POST',owner='manual',priority='interactive'))
  stopifnot(inherits(failure,'error'),failure$http_status==503L,stats('manual')$counts[['/bid']]==1L)
  stopifnot(await(fm_async_http(url('after-error')))$status_code==200L);clean()
 })
 cat(sprintf('ASYNC TRANSPORT PRIORITY: %d passed / 0 failed\n',checks))
}
main()
