#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
stopifnot(recent_points_summary_valid(list(points_received=TRUE,points=list(list(round=1,points=0),list(round=2,points=-2),list(round=3)))),
 !recent_points_summary_valid(list(points_received=FALSE,points=list())),
 !recent_points_summary_valid(list(points=list(list(round=3,points=list(1))))),
 !recent_points_summary_valid(list(points=list(list(round=3,points=TRUE)))),
 !recent_points_summary_valid(list(points=list(list(round=3,points=c(1,2))))))
cat('PASS a known round with omitted score remains unavailable; missing source field and malformed score shapes reject.\n')
# Exercise the real summary parser and asynchronous endpoint against local HTTP.
options(futmondo.offline=FALSE)
port_file<-tempfile();server_log<-tempfile()
system2('node',c('test/http_fixture.cjs',shQuote(port_file)),wait=FALSE,stdout=server_log,stderr=server_log)
until<-Sys.time()+3;while(!file.exists(port_file)&&Sys.time()<until)Sys.sleep(.01)
stopifnot(file.exists(port_file));base<-paste0('http://127.0.0.1:',readLines(port_file,warn=FALSE))
PLAYER_SUMMARY_URL<-paste0(base,'/1/player/summary')
auth<-c(userid='recent-points-fixture',token='synthetic');ctx<-resource_context(auth,'league','team','p',scoring_version='fixture')
await<-function(p){done<-FALSE;value<-error<-NULL;promises::then(p,function(x){value<<-x;done<<-TRUE},function(e){error<<-e;done<<-TRUE});until<-Sys.time()+5;while(!done&&Sys.time()<until){later::run_now(.01);Sys.sleep(.001)};stopifnot(done);if(!is.null(error))stop(error);value}
payload<-await(async_adapter_fetch('get_player_recent_points',list(login=auth,championship_id='league',user_team_id='team',player_id='p',scoring_version='fixture'),ctx))
stopifnot(identical(payload$observations$round,c(1,2,3)),identical(payload$observations$points,c(0,-2,NA_real_)),
 identical(payload$observations$score_status,c('unknown','unknown','unavailable')),identical(payload$context$season,'2026'))
saved<-list();ack<-NULL
get_sb_url<-function()'https://fixture.invalid';get_sb_key<-function()'synthetic-key'
fm_async_http<-function(url,method,body,headers,owner){saved[[length(saved)+1L]]<<-list(table=basename(sub('\\?.*$','',url)),data=jsonlite::fromJSON(body));promises::promise_resolve(list(status_code=201L))}
stopifnot(persist_recent_points_snapshot(payload,payload$context,function(ok)ack<<-ok))
for(i in seq_len(30))later::run_now(0)
stopifnot(isTRUE(ack),
 identical(vapply(saved,`[[`,character(1),'table'),c('player_match_observations','player_round_history_checks')),
 identical(as.numeric(saved[[1]]$data$points),c(0,-2,NA_real_)),saved[[2]]$data$point_count==3L,saved[[2]]$data$result_status=='scores')
cat('PASS real HTTP summary preserves0/-2/unknown and ordered persistence saves all3 observations before its successful acquisition marker.\n')
invisible(try(httr::GET(paste0(base,'/shutdown'),httr::timeout(1)),silent=TRUE));options(futmondo.offline=TRUE)
# Preserve an exhausted transport failure instead of relabeling it as schema.
get_player_summary<-function(...){x<-list(points=list(),points_received=TRUE);attr(x,'fetch_status')<-'stale';attr(x,'fetch_error')<-resource_failure(resource='get_player_summary',category='http',http_status=503L);x}
failure<-tryCatch(get_player_recent_points(auth,'league','team','p'),error=function(e)e)
stopifnot(inherits(failure,'futmondo_unavailable'),failure$category=='http',failure$http_status==503L)
cat('PASS stale-summary source diagnostics reach bounded recovery with their original HTTP503 category.\n')
