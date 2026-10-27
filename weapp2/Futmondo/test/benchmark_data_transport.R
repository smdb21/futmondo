#!/usr/bin/env Rscript
# Controlled comparison of synchronous I/O with the selected async mechanism.
options(futmondo.offline=FALSE)
suppressPackageStartupMessages(source('futmondo_functions.R'))
port_file<-tempfile();server_log<-tempfile();system2('node',c('test/http_fixture.cjs',shQuote(port_file)),wait=FALSE,stdout=server_log,stderr=server_log)
end<-Sys.time()+3;while(!file.exists(port_file)&&Sys.time()<end)Sys.sleep(.01);stopifnot(file.exists(port_file));base<-paste0('http://127.0.0.1:',readLines(port_file,warn=FALSE))
await<-function(p){done<-FALSE;value<-NULL;failure<-NULL;promises::then(p,function(x){value<<-x;done<<-TRUE},function(e){failure<<-e;done<<-TRUE});end<-Sys.time()+5;while(!done&&Sys.time()<end)later::run_now(.01);stopifnot(done);if(!is.null(failure))stop(failure);value}
invisible(promises::promise_resolve(NULL));later::run_now(0)
sync<-async<-numeric(5L);heartbeat<-logical(5L)
for(i in seq_len(5L)){start<-Sys.time();response<-httr::GET(paste0(base,'/slow'),httr::timeout(2));sync[i]<-as.numeric(difftime(Sys.time(),start,units='secs'));stopifnot(response$status_code==200L)}
for(i in seq_len(5L)){beat<-FALSE;later::later(function()beat<<-TRUE,.02);start<-Sys.time();p<-fm_async_http(paste0(base,'/slow'),owner='benchmark');async[i]<-as.numeric(difftime(Sys.time(),start,units='secs'));until<-Sys.time()+.1;while(!beat&&Sys.time()<until)later::run_now(.005);heartbeat[i]<-beat;stopifnot(await(p)$status_code==200L)}
invisible(try(httr::GET(paste0(base,'/shutdown'),httr::timeout(1)),silent=TRUE))
cat(jsonlite::toJSON(list(samples=5L,fixture_delay_ms=200L,synchronous_block_seconds=sync,async_dispatch_seconds=async,synchronous_median_seconds=median(sync),async_dispatch_median_seconds=median(async),heartbeat_before_response=heartbeat,async_transport_metrics=resource_metrics()),auto_unbox=TRUE,pretty=TRUE),'\n')
stopifnot(all(heartbeat),max(async)<.1)
