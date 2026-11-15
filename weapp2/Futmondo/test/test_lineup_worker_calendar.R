#!/usr/bin/env Rscript
# Actual getter/worker encoding and durable hydration, with synthetic I/O only.
options(futmondo.offline=TRUE,futmondo.worker_helper=TRUE)
suppressPackageStartupMessages(source('global.R'))
for(expr in parse('worker/helper.R'))if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
 as.character(expr[[2]])%in%c('fm_worker_run','fm_worker_discover','fm_worker_database_rows'))eval(expr,.GlobalEnv)
checks<-0L
check<-function(name,code){force(code);checks<<-checks+1L;cat('PASS',name,'\n')}
now<-base::Sys.time();Sys.time<-function()now
stamp<-as.POSIXct(floor(as.numeric(now))-120,origin='1970-01-01',tz='UTC')
auth<-c(userid='calendar-fixture',token='synthetic')
context<-resource_context(auth,'league','team',competition_id='competition',season='source-season',scoring_version='source-score')
rounds<-data.frame(round_id=paste0('r',1:7),round_number=as.numeric(1:7),
 begin_process=format(now+c(-6:-1*86400,86400),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),
 is_finished=c(rep(TRUE,6),FALSE),stringsAsFactors=FALSE)
reads<-0L;bridge_reads<-0L;writes<-0L
fixture_catalog<-observed_data(list(rounds=rounds,status='ok',complete=TRUE,reason=''),observed_at=stamp)
attr(fixture_catalog,'valid_until')<-as.numeric(stamp)+60
get_award_round_catalog<-function(login,championship_id,saved=data.frame(),refresh=FALSE){reads<<-reads+1L;fixture_catalog}
fm_pipe_exchange<-function(message){
 if(message$type=='resource'){bridge_reads<<-bridge_reads+1L;return(list(value=NULL))}
 writes<<-writes+1L;stop('Unexpected worker operation: ',message$type)
}
descriptor<-fm_worker_descriptor('get_finished_rounds',list(championship_id='league'),context)
result<-fm_worker_run(descriptor)
value<-fm_worker_decode(result$value)
check('actual round getter publishes complete metadata through the worker envelope',{
 stopifnot(reads==1L,bridge_reads==1L,writes==0L,result$status=='ok',result$completeness=='complete',
  isTRUE(attr(value,'complete')),isTRUE(attr(value,'catalog_complete')),
  identical(value$round_number,rounds$round_number),
  identical(as.numeric(fm_time(result$observed_at)),as.numeric(stamp)),
  identical(as.numeric(fm_time(result$valid_until)),as.numeric(stamp)+60))
})
snapshot<-function(data,completeness='unknown',status='ok',handler='get_finished_rounds',observed=stamp){
 wire<-charToRaw(fm_worker_json(fm_worker_encode(data)));compressed<-history_baseline_compress(wire)
 list(key=descriptor$resource_key,manifest=list(account_id=auth[['userid']],resource_key=descriptor$resource_key,
  handler=handler,context=descriptor$context,json_bytes=length(wire),parts=1L,
  sha256=paste0(openssl::sha256(compressed)),observed_at=rival_finance_stamp(observed),
  checked_at=rival_finance_stamp(stamp+1),valid_until=rival_finance_stamp(stamp+60),status=status,completeness=completeness),
  chunks=list(jsonlite::base64_enc(compressed)))
}
legacy<-value;attr(legacy,'complete')<-NULL
state<-function(data)list(data=data,status='stale',freshness='stale',context=descriptor$context,error=NULL)
check('older database round snapshots reuse saved parser proof without renewing their clocks',{
 before<-legacy;attr(before,'worker_acquisition_status')<-'ok';attr(before,'worker_completeness')<-'unknown';attr(before,'fetch_status')<-'stale'
 stopifnot(grepl('incomplete or invalid',lineup_catalog_problem(before),fixed=TRUE))
 restored<-fm_worker_restore(snapshot(legacy),auth[['userid']]);data<-restored$data
 stopifnot(restored$manifest$completeness=='unknown',attr(data,'worker_completeness')=='complete',
  attr(data,'worker_completeness_source')=='catalog_complete',attr(data,'fetch_status')=='stale',
  !nzchar(lineup_catalog_problem(data,state(data),descriptor$context)),
  identical(as.numeric(attr(data,'observed_at')),as.numeric(stamp)),
  identical(as.numeric(attr(data,'fetched_at')),as.numeric(stamp)+1),
  identical(attr(data,'valid_until'),as.numeric(stamp)+60),reads==1L,bridge_reads==1L,writes==0L)
})
check('hydrated catalog preserves independently known last-N averages, zero, negatives and missing values',{
 data<-fm_worker_restore(snapshot(legacy),auth[['userid']])$data
 roster<-data.frame(id=c('known','unknown'))
 history<-data.frame(player_id='known',round=c(4,5,6,7),points=c(0,-2,5,1000))
 scores<-lineup_average_scores(roster,history,data,3L)
 stopifnot(identical(attr(scores,'rounds'),as.numeric(4:6)),scores$average[1]==1,is.na(scores$average[2]),
  identical(scores$coverage,c(3L,0L)))
})
check('legacy reconstruction requires explicit successful parser and transport evidence',{
 for(kind in c('false','absent','parser_partial','parser_status_absent','manifest_partial','manifest_unavailable',
  'missing_completeness','explicit_partial','different_handler','missing_time','future_time')){
  data<-legacy;args<-list(data=data)
  if(kind=='false')attr(args$data,'catalog_complete')<-FALSE
  if(kind=='absent')attr(args$data,'catalog_complete')<-NULL
  if(kind=='parser_partial')attr(args$data,'catalog_status')<-'partial'
  if(kind=='parser_status_absent')attr(args$data,'catalog_status')<-NULL
  if(kind=='manifest_partial')args$status<-'partial'
  if(kind=='manifest_unavailable')args$status<-'unavailable'
  if(kind=='missing_completeness')args['completeness']<-list(NULL)
  if(kind=='explicit_partial')args$completeness<-'partial'
  if(kind=='different_handler')args$handler<-'get_players_from_team'
  if(kind=='missing_time')args$observed<-as.POSIXct(NA)
  if(kind=='future_time')args$observed<-now+300
  restored<-fm_worker_restore(do.call(snapshot,args),auth[['userid']])$data
  stopifnot(!identical(attr(restored,'worker_completeness'),'complete'),nzchar(lineup_catalog_problem(restored,state(restored),descriptor$context)))
 }
})
check('malformed saved round rows cannot become complete observations',{
 for(kind in c('column','duplicate_id','duplicate_number','missing_finished_number','fraction','infinite','blank_id','type','oversized')){
  data<-legacy
  if(kind=='column')data$round_id<-NULL
  if(kind=='duplicate_id')data$round_id[2]<-data$round_id[1]
  if(kind=='duplicate_number')data$round_number[2]<-data$round_number[1]
  if(kind=='missing_finished_number')data$round_number[1]<-NA_real_
  if(kind=='fraction')data$round_number[1]<-1.5
  if(kind=='infinite')data$round_number[1]<-Inf
  if(kind=='blank_id')data$round_id[1]<-''
  if(kind=='type')data$is_finished<-as.character(data$is_finished)
  if(kind=='oversized'){data<-data[rep(seq_len(nrow(data)),18L),];attr(data,'catalog_complete')<-TRUE;attr(data,'catalog_status')<-'ok'}
  # The codec itself rejects non-finite wire values; all other cases reach
  # round-specific proof validation after a valid checksum/decompression.
  saved<-tryCatch(snapshot(data),error=identity)
  if(inherits(saved,'error')){stopifnot(kind=='infinite');next}
  restored<-fm_worker_restore(saved,auth[['userid']])$data
  stopifnot(!identical(attr(restored,'worker_completeness'),'complete'))
 }
})
check('context mismatch and known read failures still reject otherwise complete observations',{
 data<-fm_worker_restore(snapshot(legacy),auth[['userid']])$data
 for(field in c('account_id','championship_id','user_team_id','competition_id','season','scoring_version')){
  altered<-state(data);altered$context[[field]]<-paste0('other-',field)
  stopifnot(nzchar(lineup_catalog_problem(data,altered,descriptor$context)))
 }
 failure<-state(data);failure$error<-resource_failure(category='network')
 stopifnot(nzchar(lineup_catalog_problem(data,failure,descriptor$context)),
  inherits(try(fm_worker_restore(snapshot(legacy),'other-account'),silent=TRUE),'try-error'))
})
get_worker_calendar_parent<-function(login,championship_id)list(round_count=nrow(get_finished_rounds(login,championship_id)))
check('nested round acquisitions publish complete durable child metadata too',{
 before<-reads
 parent<-fm_worker_descriptor('get_worker_calendar_parent',list(championship_id='league'),context)
 produced<-fm_worker_run(parent)
 child<-Filter(function(x)identical(x$descriptor$handler,'get_finished_rounds'),produced$resources)
 stopifnot(reads==before+1L,length(child)==1L,child[[1]]$completeness=='complete',
  isTRUE(attr(fm_worker_decode(child[[1]]$value),'catalog_complete')),
  identical(as.numeric(fm_time(child[[1]]$observed_at)),as.numeric(stamp)),writes==0L)
})
check('future partial catalogs do not publish complete and restored reads perform no acquisition',{
 fixture_catalog$complete<-FALSE;fixture_catalog$status<-'partial';fixture_catalog$reason<-'Numbered calendar read failed.'
 partial<-fm_worker_run(descriptor)
 stopifnot(partial$completeness!='complete',!isTRUE(attr(fm_worker_decode(partial$value),'complete')))
 before<-c(reads,bridge_reads,writes);saved<-snapshot(legacy)
 elapsed<-system.time(for(i in seq_len(100L))fm_worker_restore(saved,auth[['userid']]))[['elapsed']]
 stopifnot(identical(c(reads,bridge_reads,writes),before))
 cat(sprintf('MEASURE 100 small durable hydrations %.3fs; added source/DB reads/writes=0\n',elapsed))
})
cat(sprintf('LINEUP WORKER CALENDAR: %d passed / 0 failed\n',checks))
