#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('data_contracts.R');source('data_service.R');source('history_repository.R')})
load_function<-function(name){expressions<-as.list(parse('supabase_connector.R'));match<-Filter(function(x)is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name(name)),expressions);eval(tail(match,1)[[1]],.GlobalEnv)}
load_function('read_league_round_dataset')
ctx<-resource_context(account_id='fixture-account',championship_id='L',user_team_id='T',season='2026',scoring_version='score')
stamp<-function(x)format(as.POSIXct(x,origin='1970-01-01',tz='UTC'),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')
points<-data.frame(row_kind='point',championship_id='L',player_id=rep(paste0('p',1:3),each=500),season='2026',scoring_version='score',round=rep(1:500,3),points=rep(c(-2,0,NA_real_,8,5),300),score_status='final',
 observed_at=stamp(as.numeric(as.POSIXct('2026-07-01',tz='UTC'))+seq_len(1500)*3600),occurred_at=NA_character_,round_start_at=NA_character_,checked_at=NA_character_,result_status=NA_character_,point_count=NA_integer_,stringsAsFactors=FALSE)
checks<-points[1:4,,drop=FALSE];checks$row_kind<-'check';checks$player_id<-paste0('p',1:4);checks$round<-NA_integer_;checks$points<-NA_real_;checks$observed_at<-NA_character_
checks$checked_at<-tail(points$observed_at,4);checks$result_status<-c('scores','scores','scores','empty');checks$point_count<-c(500L,500L,500L,0L)
fixture<-rbind(points,checks);calls<-list()
supabase_get<-function(table_name,query_params){
 stopifnot(table_name=='player_round_dataset');q<-query_params
 calls[[length(calls)+1L]]<<-list(query=q,rows=0L)
 rows<-fixture[fixture$championship_id==sub('^eq\\.','',q$championship_id),,drop=FALSE]
 if(!is.null(q$and)){
   stopifnot(startsWith(q$and,'(or(observed_at.gte.'),grepl(',checked_at.gte.',q$and,fixed=TRUE))
   cutoff<-fm_time(sub('^\\(or\\(observed_at\\.gte\\.([^,]+),.*$','\\1',q$and))
   keep<-(!is.na(fm_time(rows$observed_at))&fm_time(rows$observed_at)>=cutoff)|(!is.na(fm_time(rows$checked_at))&fm_time(rows$checked_at)>=cutoff)
   rows<-rows[keep,,drop=FALSE]
 }
 fields<-sub('\\.asc$','',strsplit(q$order,',',fixed=TRUE)[[1]])
 if(nrow(rows))rows<-rows[do.call(order,rows[fields]),,drop=FALSE]
 offset<-q$offset %||% 0L
 rows<-rows[seq_len(nrow(rows))>offset&seq_len(nrow(rows))<=offset+q$limit,,drop=FALSE]
 calls[[length(calls)]]$rows<<-nrow(rows);rows
}
full<-read_league_round_dataset('L',context=ctx);full_requests<-length(calls)
stopifnot(full$complete,nrow(full$points)==1500L,nrow(full$checks)==4L,full_requests==3L)
new_time<-stamp(max(fm_time(fixture$checked_at),na.rm=TRUE)+3600)
fixture$points[fixture$row_kind=='point'&fixture$player_id=='p1'&fixture$round==1]<--11
fixture$observed_at[fixture$row_kind=='point'&fixture$player_id=='p1'&fixture$round==1]<-new_time
fixture$checked_at[fixture$row_kind=='check'&fixture$player_id=='p1']<-new_time
new_empty<-checks[4,,drop=FALSE];new_empty$player_id<-'p5';new_empty$checked_at<-new_time
fixture<-rbind(fixture,new_empty);calls<-list()
delta<-read_league_round_dataset('L',context=ctx)
stopifnot(delta$complete,nrow(delta$points)==1500L,nrow(delta$checks)==5L,length(calls)==2L,
 delta$points$points[delta$points$player_id=='p1'&delta$points$round==1]==-11,
 all(is.na(delta$checks$round)),sum(delta$checks$player_id=='p1')==1L,
 delta$checks$checked_at[delta$checks$player_id=='p1']==new_time,
 all(delta$checks$point_count[delta$checks$player_id%in%c('p4','p5')]==0L),
 all(delta$checks$result_status[delta$checks$player_id%in%c('p4','p5')]=='empty'),
 !any(delta$points$player_id%in%c('p4','p5')),anyNA(delta$points$points),any(delta$points$points==0,na.rm=TRUE))
cat(sprintf('PASS league dataset: %d -> %d requests; overlapping observed/check timestamps preserve old-round correction, NULL-round check identity and explicit empty players.\n',full_requests,length(calls)))
calls<-list();other<-ctx;other$championship_id<-'other'
failed<-tryCatch({read_league_round_dataset('L',context=other);FALSE},error=function(e)TRUE)
stopifnot(failed,length(calls)==0L)
other<-ctx;other$account_id<-'another-account';invisible(read_league_round_dataset('L',context=other))
stopifnot(is.null(calls[[1]]$query$and))
cat('PASS mismatched league context fails before querying and a different account starts its own baseline.\n')

original_get<-supabase_get
failure<-resource_failure(resource='database',category='http',http_status=503L)
supabase_get<-function(...)stop(failure)
stale<-read_league_round_dataset('L',context=ctx)
stopifnot(!stale$complete,nrow(stale$points)==1500L,nrow(stale$checks)==5L,
 identical(attr(stale,'fetch_status'),'stale'),
 identical(attr(stale,'fetch_error')$reference,failure$reference),
 identical(attr(stale,'fetch_error')$http_status,503L))
supabase_get<-original_get
recovered<-read_league_round_dataset('L',context=ctx)
stopifnot(recovered$complete,identical(attr(recovered,'fetch_status'),'ok'),
 is.null(attr(recovered,'fetch_error')),nrow(recovered$points)==1500L,nrow(recovered$checks)==5L)
cat('PASS failed incremental dataset retains saved rows and safe source diagnostics; recovery clears failure metadata.\n')
