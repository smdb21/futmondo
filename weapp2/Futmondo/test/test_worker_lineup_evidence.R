options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
raw<-list(status='closed',negative=FALSE,players=lapply(1:11,function(i)
  list(id=paste0('p',i),details=setNames(as.list(1:500),paste0('field',1:500)))),bench=list(players=list()))
stamp<-as.POSIXct('2026-11-01T12:00:00Z',tz='UTC')
observation<-list(answer=raw,team_id='t',round_id='r',signature='revision')
attr(observation,'observed_at')<-stamp
rows<-rival_lineup_evidence(observation,'fixture','l','unknown')
wire_bytes<-nchar(jsonlite::toJSON(rows[[1]],auto_unbox=TRUE,null='null',na='null',digits=NA),type='bytes')
stopifnot(as.numeric(object.size(rows[[1]]))>256*1024,wire_bytes<256*1024)
ctx<-resource_context(account_id='fixture',championship_id='l',user_team_id='t',season='unknown')
writes<-list()
defer_persistence<-function(fn,args,context,on_complete=NULL,...){
  writes[[length(writes)+1L]]<<-args
  if(is.function(on_complete))on_complete(TRUE)
  TRUE
}
stopifnot(persist_rival_finance_evidence(rows,ctx),length(writes)==1L,
 identical(writes[[1]][[2]][[1]]$payload$raw_lineup,raw),
 identical(writes[[1]][[2]][[1]]$observed_at,rival_finance_stamp(stamp)))
cat('PASS a detailed lineup expanded beyond 256 KiB in R is durably queued in full when its factual JSON is only',wire_bytes,'bytes\n')
saved<-jsonlite::fromJSON(as.character(jsonlite::toJSON(writes[[1]][[2]],auto_unbox=TRUE,null='null',na='null',digits=NA)))
restored<-rival_evidence_decode(saved)
stopifnot(length(rival_evidence_rows(restored,'round_lineup'))==1L,
 identical(restored[[1]]$payload$signature,'revision'),
 identical(unlist(restored[[1]]$payload$players,use.names=FALSE),paste0('p',1:11)))
cat('PASS persisted lineup JSON decodes into reusable ownership/signature evidence without changing source time\n')
large<-rows;large[[1]]$payload$raw_lineup<-paste(rep('x',270000),collapse='')
foreign<-rows;foreign[[1]]$account_id<-'foreign'
stopifnot(!persist_rival_finance_evidence(large,ctx),!persist_rival_finance_evidence(foreign,ctx),length(writes)==1L)
cat('PASS the actual JSON-byte bound and account isolation still reject oversized/foreign records\n')

small<-rows[[1]];small$payload$raw_lineup<-NULL
batch_rows<-c(rep(list(small),50L),rows)
callbacks<-list();batches<-list()
defer_persistence<-function(fn,args,context,on_complete=NULL,...){
 batches[[length(batches)+1L]]<<-args[[2]];callbacks[[length(callbacks)+1L]]<<-on_complete;TRUE
}
stopifnot(persist_rival_finance_evidence(batch_rows,ctx),length(batches)==1L,length(batches[[1]])==50L)
callbacks[[1]](TRUE);later::run_now(0)
stopifnot(length(batches)==2L,length(batches[[2]])==1L,identical(batches[[2]][[1]]$payload$raw_lineup,raw))
cat('PASS JSON validation/writes advance in acknowledged batches of 50 rather than inspecting the whole history at once\n')

for(expr in parse('worker/helper.R'))if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
 as.character(expr[[2]])%in%c('fm_worker_run','fm_worker_discover'))eval(expr,.GlobalEnv)
get_rival_round_lineup_observation<-function(login,championship_id,user_team_id,round_id,signature)observation
get_teams<-function(login,championship_id)data.frame(teamid='t',teamname='Fixture')
resource_reads<-character()
fm_pipe_exchange<-function(message){
 stopifnot(message$type=='resource',message$descriptor$handler=='get_teams')
 resource_reads<<-c(resource_reads,message$descriptor$handler)
 list(value=NULL)
}
fm_worker_headless_finances<-function(env,auth,ctx,on_update=NULL){
 value<-env$get_rival_round_lineup_observation(auth,ctx$championship_id,ctx$user_team_id,'r','revision')
 stopifnot(identical(value$answer,raw))
 teams<-env$get_teams(auth,ctx$championship_id)
 list(loading=FALSE,team_count=nrow(teams))
}
descriptor<-fm_worker_descriptor('fm_worker_finances',list(championship_id='l',user_team_id='t'),ctx)
result<-fm_worker_run(descriptor)
stopifnot(identical(resource_reads,'get_teams'),length(result$resources)==1L,
 result$resources[[1]]$descriptor$handler=='get_teams',fm_worker_decode(result$value)$team_count==1L)
cat('PASS private task envelopes bypass redundant snapshot encoding while display resources retain shared durable publication\n')
