options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
for(expr in parse('worker/helper.R'))if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
 identical(expr[[2]],as.name('fm_worker_database_rows')))eval(expr,.GlobalEnv)
payload<-list(kind='round_lineup',negative=FALSE,status='closed',players=as.list(paste0('p',1:11)),
 bench=list(),raw_lineup=list(players=lapply(1:11,function(i)list(id=paste0('p',i),
 details=setNames(as.list(1:500),paste0('field',1:500))))))
row<-list(account_id='fixture',championship_id='l',competition_id='c',season='s',evidence_type='eligibility',
 source_id='lineup:t:r',observed_at='2026-11-01T00:00:00Z',payload=payload)
input<-rep(list(row),10L)
fast<-system.time(got<-fm_worker_database_rows('rival_finance_evidence_latest',input))
stopifnot(is.data.frame(got),nrow(got)==10L,is.list(got$payload),!is.data.frame(got$payload),
 identical(got$payload[[1]],payload),identical(got$observed_at,rep(row$observed_at,10L)))
decoded<-rival_evidence_decode(got)
stopifnot(length(decoded)==10L,identical(decoded[[1]]$payload,payload),decoded[[1]]$season=='s')
cat('PASS finance history preserves full nested JSON, empty arrays, source times and scope in a canonical outer frame\n')
projected<-fm_worker_database_rows('rival_finance_evidence_latest',list(row[c('payload','observed_at')]))
stopifnot(nrow(projected)==1L,identical(projected$payload[[1]],payload),
 is.data.frame(fm_worker_database_rows('rival_finance_evidence_latest',list())),
 nrow(fm_worker_database_rows('rival_finance_evidence_latest',list()))==0L)
stopifnot(inherits(try(fm_worker_database_rows('rival_finance_evidence_latest',list(rows=input)),silent=TRUE),'try-error'))
cat('PASS projected finance reads and genuine empty arrays work; malformed outer envelopes stay failures\n')
other<-fm_worker_database_rows('other_table',list(list(id='one',points=-2),list(id='two',points=NULL)))
stopifnot(is.data.frame(other),other$id[1]=='one',other$points[1]==-2,is.na(other$points[2]))
cat('PASS other database table contracts retain negative numbers and missing values\n')
legacy<-system.time(old<-jsonlite::fromJSON(as.character(jsonlite::toJSON(input,auto_unbox=TRUE,null='null',na='null',digits=NA))))
stopifnot(nrow(old)==nrow(got))
cat('BENCHMARK ten rich rows: direct outer frame',fast[['elapsed']],'seconds; prior JSON round trip',legacy[['elapsed']],'seconds\n')
