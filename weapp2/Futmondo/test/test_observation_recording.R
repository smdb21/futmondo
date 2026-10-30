#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R')})
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
writes<-list();supabase_post<-function(table_name,payload,...){writes[[length(writes)+1L]]<<-list(table=table_name,payload=payload);204L}
ctx<-resource_context(account_id='u',championship_id='c',user_team_id='t',player_id='p',season='2026',scoring_version='s')
check('catalog batches parents without invented balances',{
 writes<-list();rows<-lapply(1:3,function(i)list(id=paste0('c',i),name=paste('League',i),userteam=list(id=paste0('t',i),name=paste('Team',i))))
 record_acquired_resource('active_championship_objects',list(championships=rows),ctx)
 stopifnot(length(writes)==2L,nrow(writes[[1]]$payload)==3L,nrow(writes[[2]]$payload)==3L,!('budget'%in%names(writes[[2]]$payload)))
})
check('zero cash preserves source time and unknown points',{
 writes<-list();info<-observed_data(list(budget=0,teamValue=100),observed_at=fm_time('2026-10-01T10:00:00Z'))
 record_acquired_resource('team_info_c_t',info,ctx);out<-writes[[1]]$payload
 stopifnot(length(writes)==1L,out$budget==0,out$team_value==100,is.na(out$points),!('active_teams_count'%in%names(out)),fm_time(out$observed_at)==fm_time('2026-10-01T10:00:00Z'))
})
points<-data.frame(round_number=c(1,2),points=c(0,-2),date=NA_character_,score_status=c('final','provisional'),observed_at='2026-10-02T10:00:00Z',source='futmondo')
check('unchanged rounds skip writes while corrections persist',{
 writes<-list();view<-list(points=points,previous_points=points,context=ctx);record_acquired_resource('player_card_history',view,ctx);stopifnot(length(writes)==0L)
 view$points$points[2]<--3;record_acquired_resource('player_card_history',view,ctx)
 stopifnot(length(writes)==1L,writes[[1]]$table=='player_match_observations',nrow(writes[[1]]$payload)==1L,writes[[1]]$payload$points==-3)
})
check('finality correction persists unchanged score',{
 writes<-list();view<-list(points=points,previous_points=points,context=ctx);view$points$score_status[2]<-'final';record_acquired_resource('player_card_history',view,ctx)
 stopifnot(length(writes)==1L,writes[[1]]$payload$round==2,writes[[1]]$payload$points==-2,writes[[1]]$payload$score_status=='final')
})
check('unknown finality persists once and unchanged unknown observations deduplicate',{
 writes<-list();unknown<-points;unknown$score_status[2]<-'unknown'
 record_acquired_resource('player_card_history',list(points=unknown,previous_points=points,context=ctx),ctx)
 stopifnot(length(writes)==1L,writes[[1]]$payload$score_status=='unknown',writes[[1]]$payload$points==-2)
 writes<-list();record_acquired_resource('player_card_history',list(points=unknown,previous_points=unknown,context=ctx),ctx)
 stopifnot(length(writes)==0L)
})
check('bounded errors clear after successful entity retry',{
 .persistence_state$errors<-list();for(i in 1:70)persistence_record_result(list(owner='u',key=paste0('k',i)),FALSE);stopifnot(length(.persistence_state$errors$u)==64L)
 args<-list('player_history',list(player_id='p',championship_id='c',observed_at='old',value=0),'player_id,championship_id,observed_at');changed<-args;changed[[2]]$observed_at<-'new';changed[[2]]$value<-1
 key<-persistence_job_scope('supabase_post_direct',args);stopifnot(identical(key,persistence_job_scope('supabase_post_direct',changed)))
 persistence_record_result(list(owner='v',key='old',error_key=key),FALSE);persistence_record_result(list(owner='v',key='new',error_key=key),TRUE);stopifnot(is.null(.persistence_state$errors$v))
})
check('mutable TTL and catalog freshness match policy',stopifnot(resource_policy('team_info_c_t')$ttl==20,resource_policy('get_user_team_info')$ttl==20,resource_policy('championshipplayers_c')$ttl<=300,resource_policy('real_clubs_c')$ttl==900))
check('async parent failure blocks children and success unlocks dependent write',{
 old_http<-fm_async_http;old_url<-get_sb_url;old_key<-get_sb_key;get_sb_url<-function()'https://fixture.invalid';get_sb_key<-function()'fixture'
 operations<-list(list('players',data.frame(id='p',name='Player',slug=''),'id',TRUE),list('market_transactions',data.frame(player_id='p',championship_id='c',source_event_id='e',price=1),'championship_id,source_event_id',FALSE))
 for(succeed in c(FALSE,TRUE)){
  .persistence_state$queue<-list();.persistence_state$active<-NULL;.persistence_state$scheduled<-FALSE;.persistence_state$errors<-list();.persistence_state$acknowledged<-list();called<-character()
  fm_async_http<-function(url,...){called<<-c(called,tail(strsplit(sub('\\?.*$','',url),'/',fixed=TRUE)[[1]],1));if(succeed)promises::promise_resolve(list(status_code=201))else promises::promise_reject(simpleError('HTTP 503'))}
  options(futmondo.offline=FALSE);stopifnot(isTRUE(defer_persistence('supabase_fact_batch_direct',list(operations),ctx)))
  deadline<-Sys.time()+3;while(background_sync_status('u')$pending&&Sys.time()<deadline){later::run_now(.01);Sys.sleep(.001)}
  stopifnot(identical(called,if(succeed)c('players','market_transactions')else 'players'),background_sync_status('u')$pending==0L,identical(is.null(background_sync_status('u')$error),succeed))
 }
 fm_async_http<-old_http;get_sb_url<-old_url;get_sb_key<-old_key;options(futmondo.offline=TRUE)
})

check('unknown score corrections persist without repeated writes',{
 writes<-list();unknown<-points;unknown$points[1]<-NA_real_
 record_acquired_resource('player_card_history',list(points=unknown,previous_points=points,context=ctx),ctx)
 stopifnot(length(writes)==1L,nrow(writes[[1]]$payload)==1L,is.na(writes[[1]]$payload$points))
 writes<-list();record_acquired_resource('player_card_history',list(points=unknown,previous_points=unknown,context=ctx),ctx);stopifnot(length(writes)==0L)
 record_acquired_resource('player_card_history',list(points=points,previous_points=unknown,context=ctx),ctx)
 stopifnot(length(writes)==1L,writes[[1]]$payload$points==0)
})

cat(sprintf('OBSERVATION RECORDING: %d passed / 0 failed\n',n))
