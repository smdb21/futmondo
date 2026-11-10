#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('data_contracts.R');source('data_service.R');source('history_repository.R')})
ctx<-resource_context(account_id='fixture-account',championship_id='L',user_team_id='T',player_id='P',season='2026',scoring_version='score')
stamp<-function(x)format(as.POSIXct(x,origin='1970-01-01',tz='UTC'),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')
prices<-data.frame(id=1:1200,player_id='P',championship_id='L',value=seq_len(1200)*100,
 observed_at=stamp(as.numeric(as.POSIXct('2026-08-01',tz='UTC'))+seq_len(1200)*3600),stringsAsFactors=FALSE)
prices$recorded_at<-prices$observed_at
prices$competition_id<-'unknown';prices$season<-'2026'
rounds<-data.frame(player_id='P',championship_id='L',season='2026',scoring_version='score',round=c(1,2,3),
 points=c(0,-4,8),score_status='final',round_start_at=NA_character_,occurred_at=NA_character_,observed_at=prices$observed_at[c(1,600,1200)])
rounds$competition_id<-'unknown';rounds$source_round_key<-paste0('number:',rounds$round)
rounds$round_id<-rounds$match_id<-NA_character_
calls<-list();server_cap<-500L;fail_request<-Inf
compare<-function(x)if(inherits(x,'POSIXt'))as.numeric(x)else if(is.numeric(x))x else as.character(x)
supabase_get<-function(table_name,query_params=list()){
 calls[[length(calls)+1L]]<<-list(table=table_name,query=query_params,rows=0L)
 if(length(calls)==fail_request)return(NULL)
 q<-query_params;rows<-switch(table_name,player_history=prices,player_match_observations=rounds,stop('Unexpected table'))
 for(field in intersect(c('championship_id','player_id','competition_id','season','scoring_version'),names(q))){
   wanted<-sub('^eq\\.','',q[[field]]);rows<-rows[rows[[field]]==wanted,,drop=FALSE]
 }
 if(!is.null(q$and)){
   clauses<-regmatches(q$and,gregexpr('[a-z_]+\\.gte\\.[^,)]+',q$and))[[1]]
   keep<-rep(FALSE,nrow(rows))
   for(clause in clauses){field<-sub('\\.gte\\..*$','',clause);value<-sub('^[a-z_]+\\.gte\\.','',clause)
     keep<-keep|(!is.na(fm_time(rows[[field]]))&fm_time(rows[[field]])>=fm_time(value))}
   rows<-rows[keep,,drop=FALSE]
 }
 order_fields<-sub('\\.asc$','',strsplit(q$order,',',fixed=TRUE)[[1]])
 if(nrow(rows))rows<-rows[do.call(order,lapply(rows[order_fields],compare)),,drop=FALSE]
 if(!is.null(q[['or']])){
   clauses<-regmatches(q[['or']],gregexpr('[a-z_]+\\.gt\\.[^,)]+',q[['or']]))[[1]]
   keep<-rep(FALSE,nrow(rows));equal<-rep(TRUE,nrow(rows))
   for(field in order_fields){
     clause<-clauses[startsWith(clauses,paste0(field,'.gt.'))];stopifnot(length(clause)==1L)
     value<-substring(clause,nchar(field)+5L)
     if(is.numeric(rows[[field]]))value<-as.numeric(value)
     keep<-keep|(equal&rows[[field]]>value);equal<-equal&rows[[field]]==value
   }
   rows<-rows[keep,,drop=FALSE]
 }
 rows<-head(rows,min(server_cap,q$limit));calls[[length(calls)]]$rows<<-nrow(rows);rows
}
reset_calls<-function(){calls<<-list();fail_request<<-Inf}
full<-read_card_price_history(ctx);full_requests<-length(calls);full_rows<-sum(vapply(calls,function(x)x$rows,integer(1)))
stopifnot(nrow(full)==1200L,isTRUE(attr(full,'complete')),!attr(full,'history_incremental'),full_requests==4L)
# A correction exactly on the overlap boundary is included, with a new observation.
boundary<-max(fm_time(prices$recorded_at))-86400
changed<-which(fm_time(prices$recorded_at)==boundary);stopifnot(length(changed)==1L)
prices$value[changed]<-765432
addition<-tail(prices,1);addition$id<-1201L;addition$recorded_at<-addition$observed_at<-stamp(max(fm_time(prices$recorded_at))+3600)
prices<-rbind(prices,addition)
reset_calls();delta<-read_card_price_history(ctx);delta_requests<-length(calls);delta_rows<-sum(vapply(calls,function(x)x$rows,integer(1)))
stopifnot(nrow(delta)==1201L,isTRUE(attr(delta,'complete')),isTRUE(attr(delta,'history_incremental')),
 identical(delta$value[delta$id==prices$id[changed]],765432),delta_requests==2L,delta_rows==26L,
 identical(calls[[1]]$query$and,paste0('(recorded_at.gte.',stamp(boundary),')')))
cat(sprintf('PASS stored prices: %d -> %d requests and %d -> %d transferred rows; overlap correction and deduplication preserved.\n',full_requests,delta_requests,full_rows,delta_rows))
reset_calls();invisible(read_card_round_history(ctx))
correction<-rounds[1,,drop=FALSE];correction$points<--8;correction$observed_at<-stamp(max(fm_time(rounds$observed_at))+3600)
rounds<-rbind(rounds,correction)
reset_calls();point_rows<-read_card_round_history(ctx);points<-reconcile_player_points(point_rows,data.frame(),ctx)
stopifnot(isTRUE(attr(point_rows,'history_incremental')),identical(points$points,c(-8,-4,8)),nrow(point_rows)==4L,
 grepl('observed_at.gte.',calls[[1]]$query$and,fixed=TRUE))
cat('PASS newest observation watermark retrieves corrections to old rounds, retaining zero/negative provenance.\n')
# Failed or partial deltas cannot replace the known complete baseline.
reset_calls();fail_request<-1L;stale<-read_card_price_history(ctx)
stopifnot(attr(stale,'fetch_status')=='stale',!attr(stale,'complete'),nrow(stale)==1201L)
reset_calls();server_cap<-10L;fail_request<-2L;partial<-read_card_price_history(ctx)
partial_boundary<-calls[[1]]$query$and
stopifnot(attr(partial,'fetch_status')=='partial',!attr(partial,'complete'),nrow(partial)==1201L)
reset_calls();server_cap<-500L;retry<-read_card_price_history(ctx)
stopifnot(isTRUE(attr(retry,'complete')),identical(calls[[1]]$query$and,partial_boundary))
cat('PASS failed/partial reads keep the complete baseline and do not advance the retry boundary.\n')
for(field in c('account_id','championship_id','user_team_id','player_id','competition_id','season','scoring_version')){
 other<-ctx;other[[field]]<-paste0(other[[field]],'-other');reset_calls();read_card_price_history(other)
 stopifnot(is.null(calls[[1]]$query$and))
}
cat('PASS account, league, team, player, competition, season and scoring contexts use separate baselines.\n')
reset_calls();limited<-read_card_price_history(ctx,max_rows=10L)
reset_calls();limited_again<-read_card_price_history(ctx,max_rows=10L)
stopifnot(nrow(limited)==10L,!attr(limited,'complete'),!attr(limited_again,'history_incremental'),is.null(calls[[1]]$query$and))
cat('PASS capped histories remain partial and cannot seed an incremental baseline.\n')
# A full audit repairs deletions/old edits even during continuous active use.
keys<-ls(api_cache_env);price_key<-keys[vapply(keys,function(k){entry<-api_cache_env[[k]];identical(entry$context,ctx)&&startsWith(entry$resource,'history_baseline_player_history_')},logical(1))]
stopifnot(length(price_key)==1L)
entry<-api_cache_env[[price_key]];attr(entry$data,'history_full_read_at')<-Sys.time()-86401;api_cache_env[[price_key]]<-entry
prices<-prices[prices$id!=1L,,drop=FALSE];prices$value[prices$id==2L]<-1234
reset_calls();audited<-read_card_price_history(ctx)
stopifnot(is.null(calls[[1]]$query$and),!attr(audited,'history_incremental'),!1L%in%audited$id,audited$value[audited$id==2L]==1234,
 api_cache_env[[price_key]]$expires_at>as.numeric(Sys.time())+6*86400)
cat('PASS daily full audit repairs old changes; retained baseline remains bounded by the shared cache.\n')
# Large/multi-timestamp dataset filters must not overwrite pre-existing bounds or cursor filters.
q<-list(order='round.asc,observed_at.asc',and='(round.lte.40)');captured<-NULL
old_pager<-history_read_pages
history_read_pages<-function(table,query,...){captured<<-query;x<-rounds;attr(x,'complete')<-TRUE;x}
invisible(read_history_incremental('multi-fixture',q,ctx,identity_fields=c('round','observed_at'),watermark_fields=c('observed_at','occurred_at')))
invisible(read_history_incremental('multi-fixture',q,ctx,identity_fields=c('round','observed_at'),watermark_fields=c('observed_at','occurred_at')))
stopifnot(startsWith(captured$and,'(round.lte.40,or(observed_at.gte.'),grepl('occurred_at.gte.',captured$and,fixed=TRUE))
history_read_pages<-old_pager
cat('PASS multi-watermark overlap composes with existing query bounds.\n')
# Replay the real cooperative adapter with delayed fixture HTTP responses.
source('async_transport.R')
get_sb_url<-function()'https://history.fixture.invalid';get_sb_key<-function()'fixture-only'
http_requests<-0L;heartbeats<-0L
fm_async_read_http<-function(url,method='GET',body=NULL,headers=character(),owner=''){
 http_requests<<-http_requests+1L
 promises::promise(function(resolve,reject){later::later(function(){
   parsed<-httr::parse_url(url);q<-parsed$query;q$limit<-as.integer(q$limit)
   result<-supabase_get(tail(strsplit(parsed$path,'/',fixed=TRUE)[[1]],1),q)
   resolve(list(status_code=200L,content=charToRaw(jsonlite::toJSON(result,dataframe='rows',auto_unbox=TRUE,na='null'))))
 },.025)})
}
await<-function(p){
 done<-FALSE;result<-error<-NULL
 promises::then(p,function(x){result<<-x;done<<-TRUE},function(e){error<<-e;done<<-TRUE})
 deadline<-Sys.time()+5
 while(!done&&Sys.time()<deadline){later::run_now(.005);Sys.sleep(.001)}
 stopifnot(done);if(!is.null(error))stop(error);result
}
clear_api_cache();reset_calls()
for(delay in c(.005,.015,.035,.055))later::later(function()heartbeats<<-heartbeats+1L,delay)
async_full<-await(async_adapter_fetch('read_card_price_history',list(context=ctx),context=ctx))
first_http<-http_requests
async_delta<-await(async_adapter_fetch('read_card_price_history',list(context=ctx),context=ctx))
stopifnot(isTRUE(attr(async_full,'complete')),isTRUE(attr(async_delta,'history_incremental')),
 identical(async_full$id,async_delta$id),first_http==4L,http_requests-first_http==2L,heartbeats==4L)
cat('PASS delayed asynchronous replay preserves the complete baseline, uses 4 -> 2 HTTP reads, and allows UI callbacks.\n')
