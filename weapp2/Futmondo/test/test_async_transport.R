#!/usr/bin/env Rscript
options(futmondo.offline=FALSE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R')})
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
await<-function(p){done<-FALSE;out<-NULL;failure<-NULL;promises::then(p,function(x){out<<-x;done<<-TRUE},function(e){failure<<-e;done<<-TRUE});until<-Sys.time()+5;while(!done&&Sys.time()<until){later::run_now(.01);Sys.sleep(.001)};stopifnot(done);if(!is.null(failure))stop(failure);out}
port_file<-tempfile();server_log<-tempfile()
system2('node',c('test/http_fixture.cjs',shQuote(port_file)),wait=FALSE,stdout=server_log,stderr=server_log)
wait_until<-Sys.time()+3;while(!file.exists(port_file)&&Sys.time()<wait_until)Sys.sleep(.01)
stopifnot(file.exists(port_file));base<-paste0('http://127.0.0.1:',readLines(port_file,warn=FALSE))
invisible(promises::promise_resolve(NULL));later::run_now(0)
check('delayed socket preserves heartbeat and dispatch returns promptly',{
 beat<-FALSE;later::later(function()beat<<-TRUE,.02);started<-Sys.time()
 request<-fm_async_http(paste0(base,'/slow'),owner='fixture');dispatched<-as.numeric(difftime(Sys.time(),started,units='secs'))
 while(!beat&&as.numeric(difftime(Sys.time(),started,units='secs'))<.15)later::run_now(.01)
 stopifnot(dispatched<.1,beat,as.numeric(difftime(Sys.time(),started,units='secs'))<.18)
 stopifnot(await(request)$status_code==200L);cat(sprintf('MEASURE dispatch_seconds=%.4f heartbeat_before_response=TRUE\n',dispatched))
})
check('identical concurrent read transports coalesce',{
 before<-resource_metrics()$requests;first<-fm_async_read_http(paste0(base,'/slow'),owner='shared');second<-fm_async_read_http(paste0(base,'/slow'),owner='shared')
 stopifnot(await(first)$status_code==200,await(second)$status_code==200,resource_metrics()$requests-before==1)
})
check('real card keeps zero negative undated scores when database fails',{
 clear_api_cache();PLAYER_SUMMARY_URL<-paste0(base,'/1/player/summary');LINEUP_URL<-paste0(base,'/1/userteam/lineup')
 Sys.setenv(supabase_project_url=base,supabase_secret_key='fixture-only')
 auth<-c(userid='fixture',token='fixture-only');ctx<-resource_context(auth,'league','team','p',session_id='card-test');before<-resource_metrics()$requests
 saved_record<-record_acquired_resource;record_acquired_resource<-function(...)invisible(TRUE)
 args<-list(login=auth,championship_id='league',user_team_id='team',player_id='p')
 card<-await(async_adapter_fetch('get_player_card_history',args,ctx))
 stopifnot(identical(card$points$points,c(0,-2)),all(is.na(card$points$date)),all(card$points$score_status=='unknown'),card$status=='partial',is.list(card$summary),resource_metrics()$requests-before==4)
 warm<-resource_metrics()$requests;again<-await(async_adapter_fetch('get_player_card_history',args,ctx))
 stopifnot(identical(again$points$points,c(0,-2)),resource_metrics()$requests==warm)
 args$refresh<-TRUE;refreshed<-await(async_adapter_fetch('get_player_card_history',args,ctx))
 stopifnot(identical(refreshed$points$points,c(0,-2)),resource_metrics()$requests-warm==4)
 record_acquired_resource<-saved_record;cat('MEASURE cold_card_requests=4 warm_card_requests=0 explicit_refresh_requests=4\n')
})
check('cancellation rejects transfer and frees capacity',{
 request<-fm_async_http(paste0(base,'/cancel'),owner='cancel-test');fm_transport_cancel('cancel-test')
 got<-try(await(request),silent=TRUE);stopifnot(inherits(got,'try-error'),!any(vapply(.fm_transport$tasks,function(x)identical(x$owner,'cancel-test'),logical(1))))
})
try(httr::GET(paste0(base,'/shutdown'),httr::timeout(1)),silent=TRUE)
Sys.unsetenv(c('supabase_project_url','supabase_secret_key'));options(futmondo.offline=TRUE)
cat(sprintf('ASYNC TRANSPORT: %d passed / 0 failed\n',n))
