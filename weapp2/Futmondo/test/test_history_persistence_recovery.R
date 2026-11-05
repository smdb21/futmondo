options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R')})
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
ctx<-resource_context(account_id='fixture-account',championship_id='fixture-league',player_id='fixture-player',season='fixture-season',scoring_version='fixture-score',session_id='fixture-session')
rows<-data.frame(player_id=ctx$player_id,championship_id=ctx$championship_id,season=ctx$season,scoring_version=ctx$scoring_version,
 round=1,points=-2,score_status='final',observed_at='2026-10-01T12:00:00Z',occurred_at=NA_character_,round_start_at=NA_character_)
payload<-list(observations=rows,checked_at=as.POSIXct('2026-10-01T12:00:00',tz='UTC'))
get_sb_url<-function()'https://fixture.invalid';get_sb_key<-function()'synthetic-key'
requests<-list();callbacks<-logical();.persistence_retry_base_seconds<-0
reset<-function(){
 if(is.function(.persistence_state$retry_timer)).persistence_state$retry_timer()
 .persistence_state$retry_timer<-NULL;.persistence_state$queue<-list();.persistence_state$active<-NULL
 .persistence_state$scheduled<-FALSE;.persistence_state$errors<-list();.persistence_state$acknowledged<-list()
 requests<<-list();callbacks<<-logical();.persistence_retry_base_seconds<<-0
}
drain<-function(){for(i in seq_len(40L))later::run_now(0)}
record<-function(ok)callbacks<<-c(callbacks,isTRUE(ok))
fm_async_http<-function(url,method,body,headers,owner){
 promises::promise(function(resolve,reject){requests[[length(requests)+1L]]<<-list(url=url,body=body,owner=owner,resolve=resolve,reject=reject)})
}
options(futmondo.offline=FALSE)
check('structured transient failures retry only the same immutable operation before its marker',{
 reset();stopifnot(persist_recent_points_snapshot(payload,ctx,record));drain()
 requests[[1]]$reject(resource_failure(resource='database',category='http',http_status=503L));drain()
 stopifnot(length(requests)==2L,identical(requests[[1]]$body,requests[[2]]$body),!length(callbacks))
 requests[[2]]$resolve(list(status_code=201L));drain()
 stopifnot(length(requests)==3L,grepl('/player_round_history_checks',requests[[3]]$url,fixed=TRUE),!length(callbacks))
 requests[[3]]$resolve(list(status_code=201L));drain()
 stopifnot(identical(callbacks,TRUE),is.null(.persistence_state$active),!length(.persistence_state$queue),is.null(background_sync_status(ctx$account_id)$error))
})
check('transient save attempts are bounded and never advance the marker after exhaustion',{
 reset();stopifnot(persist_recent_points_snapshot(payload,ctx,record));drain()
 for(i in 1:3){stopifnot(length(requests)==i);requests[[i]]$reject(resource_failure(resource='database',category='network'));drain()}
 stopifnot(length(requests)==3L,identical(callbacks,FALSE),!length(.persistence_state$queue),is.null(.persistence_state$active),
   all(!vapply(requests,function(request)grepl('/player_round_history_checks',request$url,fixed=TRUE),logical(1))))
})
check('permanent structured failures do not retry and a newer successful checkpoint clears the old error',{
 reset();stopifnot(persist_recent_points_snapshot(payload,ctx,record));drain()
 requests[[1]]$reject(resource_failure(resource='database',category='http',http_status=403L));drain()
 stopifnot(length(requests)==1L,identical(callbacks,FALSE),grepl('access was denied',background_sync_status(ctx$account_id)$error,fixed=TRUE))
 newer<-payload;newer$checked_at<-newer$checked_at+1;newer$observations$observed_at<-'2026-10-01T12:00:01Z'
 stopifnot(persist_recent_points_snapshot(newer,ctx,record));drain()
 requests[[2]]$resolve(list(status_code=201L));drain();requests[[3]]$resolve(list(status_code=201L));drain()
 stopifnot(identical(callbacks,c(FALSE,TRUE)),is.null(background_sync_status(ctx$account_id)$error))
})
check('synchronous transport errors release the active job and recover within the same retry budget',{
 reset();ordinary_http<-fm_async_http;attempts<-0L
 fm_async_http<-function(...){attempts<<-attempts+1L;if(attempts<3L)stop(resource_failure(resource='database',category='network'));ordinary_http(...)}
 stopifnot(persist_recent_points_snapshot(payload,ctx,record));drain()
 stopifnot(attempts==3L,length(requests)==1L,!length(callbacks))
 requests[[1]]$resolve(list(status_code=201L));drain();requests[[2]]$resolve(list(status_code=201L));drain()
 stopifnot(identical(callbacks,TRUE),is.null(.persistence_state$active));fm_async_http<-ordinary_http
})
check('serialization failure cannot strand unrelated later writes',{
 reset();bad<-list('fixture_table',list(not_json=new.env()),NULL,FALSE)
 stopifnot(defer_persistence('supabase_post_direct',bad,ctx,on_complete=record));drain()
 stopifnot(identical(callbacks,FALSE),is.null(.persistence_state$active),length(requests)==0L)
 stopifnot(persist_recent_points_snapshot(payload,ctx,record));drain()
 requests[[1]]$resolve(list(status_code=201L));drain();requests[[2]]$resolve(list(status_code=201L));drain()
 stopifnot(identical(callbacks,c(FALSE,TRUE)),is.null(.persistence_state$active))
})
check('logout cancels a delayed retry and its timer without another write',{
 reset();.persistence_retry_base_seconds<-30
 stopifnot(persist_recent_points_snapshot(payload,ctx,record));drain()
 requests[[1]]$reject(resource_failure(resource='database',category='http',http_status=429L));drain()
 stopifnot(is.function(.persistence_state$retry_timer),!length(callbacks))
 stopifnot(cancel_persistence_jobs(ctx$account_id,ctx$session_id)==1L);drain()
 stopifnot(is.null(.persistence_state$retry_timer),identical(callbacks,FALSE),length(requests)==1L,
   is.null(.persistence_state$active),!length(.persistence_state$queue))
})
check('retry backoff keeps the event loop responsive and admits unrelated ready writes',{
 reset();.persistence_retry_base_seconds<-0.03
 stopifnot(persist_recent_points_snapshot(payload,ctx,record));drain()
 requests[[1]]$reject(resource_failure(resource='database',category='http',http_status=503L));drain()
 other<-ctx;other$account_id<-'other-account';other$session_id<-'other-session'
 stopifnot(defer_persistence('supabase_post_direct',list('fixture_table',list(id='observed-fact'),NULL,FALSE),other,on_complete=record));drain()
 stopifnot(length(requests)==2L,grepl('/fixture_table',requests[[2]]$url,fixed=TRUE))
 requests[[2]]$resolve(list(status_code=201L));drain()
 heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,.001)
 deadline<-Sys.time()+1
 while(length(requests)<3L&&Sys.time()<deadline)later::run_now(.005)
 stopifnot(heartbeat,length(requests)==3L,identical(requests[[3]]$body,requests[[1]]$body))
 requests[[3]]$resolve(list(status_code=201L));drain();requests[[4]]$resolve(list(status_code=201L));drain()
 stopifnot(identical(callbacks,c(TRUE,TRUE)),is.null(.persistence_state$active),!length(.persistence_state$queue))
})
check('invalid cross-context history cannot obtain a success checkpoint',{
 reset();wrong<-payload;wrong$observations$player_id<-'another-player'
 stopifnot(!persist_recent_points_snapshot(wrong,ctx,record),!length(.persistence_state$queue),!length(requests))
})
check('an old failed mutable upsert cannot replay over a newer value',{
 reset();old<-list('players',list(id='p',name='Old',slug='p'),'id',FALSE)
 newer<-list('players',list(id='p',name='New',slug='p'),'id',FALSE)
 stopifnot(defer_persistence('supabase_post_direct',old,ctx,on_complete=record));drain()
 stopifnot(defer_persistence('supabase_post_direct',newer,ctx,on_complete=record))
 requests[[1]]$reject(resource_failure(resource='database',category='http',http_status=503L));drain()
 stopifnot(length(requests)==2L,identical(callbacks,FALSE),grepl('New',requests[[2]]$body,fixed=TRUE))
 requests[[2]]$resolve(list(status_code=201L));drain()
 stopifnot(identical(callbacks,c(FALSE,TRUE)),length(requests)==2L,!length(.persistence_state$queue))
})
check('malformed generic jobs are rejected or safely released before dispatch',{
 reset();stopifnot(!defer_persistence('supabase_post_direct',list('players'),ctx,on_complete=record),!length(.persistence_state$queue))
 .persistence_state$queue<-list(list(fn='supabase_post_direct',args=list(),owner=ctx$account_id,session_id=ctx$session_id,key='malformed',callbacks=list(record),bytes=0))
 persistence_tick();drain()
 stopifnot(identical(callbacks,FALSE),is.null(.persistence_state$active),!length(requests))
})
check('manual actions remain outside the retry queue',{
 reset();stopifnot(!defer_persistence('place_bid',list(),ctx),!defer_persistence('sell_player',list(),ctx),!length(.persistence_state$queue))
})
reset();options(futmondo.offline=TRUE)
cat(sprintf('HISTORY PERSISTENCE RECOVERY: %d groups passed / 0 failed\n',checks))
