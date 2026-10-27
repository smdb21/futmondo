#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R')})
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
reset<-function(){.persistence_state$queue<-list();.persistence_state$active<-NULL;.persistence_state$errors<-list();.persistence_state$acknowledged<-list();.persistence_state$scheduled<-FALSE}
ctx<-resource_context(account_id='u',session_id='s',championship_id='league')
args<-list('player_history',list(player_id='p',championship_id='league',value=100,observed_at='2026-10-01'),supabase_conflict_key('player_history'))
check('duplicate observation queues once and correction stays distinct',{
 reset();options(futmondo.offline=FALSE);stopifnot(isTRUE(defer_persistence('supabase_post_direct',args,ctx)),isTRUE(defer_persistence('supabase_post_direct',args,ctx)),length(.persistence_state$queue)==1L)
 corrected<-args;corrected[[2]]$value<-101;stopifnot(isTRUE(defer_persistence('supabase_post_direct',corrected,ctx)),length(.persistence_state$queue)==2L)
})
check('acknowledged content does not queue again',{
 job<-.persistence_state$queue[[1]];persistence_record_result(job,TRUE);.persistence_state$queue<-list();stopifnot(isTRUE(defer_persistence('supabase_post_direct',args,ctx)),length(.persistence_state$queue)==0L)
})
check('only factual writes enter queue',stopifnot(!defer_persistence('get_teams',list('token','league'),ctx)))
check('account bound preserves another account capacity',{
 reset();.persistence_state$queue<-lapply(seq_len(.persistence_max_jobs_per_owner),function(i)list(owner='u',session_id='s',key=paste0('u',i)))
 other<-ctx;other$account_id<-'v';stopifnot(!defer_persistence('supabase_post_direct',args,ctx),isTRUE(defer_persistence('supabase_post_direct',args,other)),!is.null(background_sync_status('u')$error),is.null(background_sync_status('v')$error))
})
check('logout removes matching session jobs only',{
 reset();.persistence_state$queue<-list(list(owner='u',session_id='s1',key='a'),list(owner='u',session_id='s2',key='b'),list(owner='v',session_id='s1',key='c'));stopifnot(cancel_persistence_jobs('u','s1')==1L,length(.persistence_state$queue)==2L)
})
check('configuration failure is scoped and distinct',{
 reset();persistence_record_result(list(owner='u',key='x'),FALSE,list(list(category='configuration')));stopifnot(grepl('not configured',background_sync_status('u')$error,fixed=TRUE),is.null(background_sync_status('v')$error))
})
options(futmondo.offline=TRUE);reset();cat(sprintf('BACKGROUND SYNC: %d passed / 0 failed\n',n))
