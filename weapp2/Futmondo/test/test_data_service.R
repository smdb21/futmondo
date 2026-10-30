#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R')})
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
auth<-c(userid='one',token='t');ctx<-resource_context(auth,'league','team','p','2026','score')
await<-function(p){done<-FALSE;result<-NULL;failure<-NULL;promises::then(p,function(x){result<<-x;done<<-TRUE},function(e){failure<<-e;done<<-TRUE});deadline<-Sys.time()+3;while(!done&&Sys.time()<deadline){later::run_now(.01);Sys.sleep(.001)};stopifnot(done);if(!is.null(failure))stop(failure);result}
check('cache keys isolate all context dimensions',{
 key<-api_cache_key('x',context=ctx)
 for(field in c('account_id','championship_id','user_team_id','player_id','season','scoring_version')){other<-ctx;other[[field]]<-'different';stopifnot(api_cache_key('x',context=other)!=key)}
 stopifnot(inherits(try(resource_context(),silent=TRUE),'try-error'))
})
check('return inside adapter publishes exactly once',{
 clear_api_cache();calls<-0L
 fetch<-function()get_cached_data('return-fixture',{calls<<-calls+1L;return(7)},context=ctx)
 stopifnot(fetch()==7,fetch()==7,calls==1L)
})
check('targeted invalidation preserves other league and resource',{
 clear_api_cache();other<-ctx;other$championship_id<-'other'
 get_cached_data('market',1,context=ctx);get_cached_data('catalog',2,context=ctx);get_cached_data('market',3,context=other)
 stopifnot(invalidate_resources(ctx,resources='market')==1L,exists(api_cache_key('catalog',context=ctx),api_cache_env),exists(api_cache_key('market',context=other),api_cache_env))
})
check('entry and byte caps permit continued navigation',{
 clear_api_cache();for(i in 1:300)get_cached_data(paste0('p',i),i,context=ctx)
 m<-resource_metrics();stopifnot(m$entries<=256,m$retained_bytes<=32*1024^2,m$evictions>0)
 stopifnot(await(get_cached_resource_async('after-cap',ctx,function()promises::promise_resolve('new')))$data=='new',resource_metrics()$entries<=256)
})
check('partial reconciliation preserves corrections zero negative and context',{
 row<-function(round,points,time,season='2026',score='score',status='final')data.frame(player_id='p',championship_id='league',season=season,scoring_version=score,round=round,points=points,observed_at=time,score_status=status,occurred_at=NA_character_)
 stored<-dplyr::bind_rows(row(1,0,'2026-10-01'),row(2,8,'2026-10-01'),row(2,9,'2026-10-02'),row(1,99,'2025-10-01',season='2025'),row(4,99,'2026-10-01',season='unknown'))
 live<-dplyr::bind_rows(row(2,7,'2026-10-01'),row(3,-2,'2026-10-03',status='provisional'),row(4,NA,'2026-10-03'))
 got<-reconcile_player_points(stored,live,ctx)
 stopifnot(identical(got$round_number,c(1,2,3)),identical(got$points,c(0,9,-2)),all(is.na(got$date)),got$score_status[3]=='provisional')
})
check('unknown finality and participation remain unknown',{
 got<-normalize_player_match_observations(list(match=list(r=list(number=9)),points=list(list(round=1,points=0),list(round=2,points=-2),list(round=3))),'p','league')
 stopifnot(all(got$score_status[1:2]=='unknown'),is.na(got$points[3]),all(is.na(got$participation)))
})
check('reconciliation preserves unknown and legacy labels through revision selection',{
 old<-data.frame(player_id='p',championship_id='league',season='2026',scoring_version='score',round=c(1,2),
   points=c(0,-2),score_status=c('provisional','provisional'),observed_at='2026-10-01')
 live<-old;live$round<-c(1,3);live$score_status<-c('unknown','unknown');live$observed_at<-'2026-10-02'
 got<-reconcile_player_points(old,live,ctx)
 stopifnot(identical(got$score_status,c('unknown','provisional','unknown')),identical(got$points,c(0,-2,-2)))
 malformed<-live;malformed$score_status<-c(NA,'unrecognized');got<-reconcile_player_points(data.frame(),malformed,ctx)
 stopifnot(all(got$score_status=='unknown'))
})
check('short server pages do not truncate history',{
 old<-supabase_get;requests<-0L;fixture<-data.frame(id=letters[1:5],value=1:5)
 supabase_get<-function(table_name,query_params){requests<<-requests+1L;offset<-query_params$offset %||% 0L;limit<-min(2L,query_params$limit);fixture[seq_len(nrow(fixture))>offset & seq_len(nrow(fixture))<=offset+limit,,drop=FALSE]}
 got<-history_read_pages('fixture',list(order='id.asc'),max_rows=20,page_size=500)
 stopifnot(nrow(got)==5,requests==4,isTRUE(attr(got,'complete')))
 limited<-history_read_pages('fixture',list(order='id.asc'),max_rows=3,page_size=500)
 stopifnot(nrow(limited)==3,!attr(limited,'complete'),attr(limited,'fetch_status')=='partial');supabase_get<-old
})
check('concurrent consumers share pending read and warm provenance',{
 clear_api_cache();calls<-0L
 fetch<-function(){calls<<-calls+1L;promises::promise(function(resolve,reject)later::later(function()resolve(42),.02))}
 first<-get_cached_resource_async('shared',ctx,fetch);second<-get_cached_resource_async('shared',ctx,fetch)
 a<-await(first);b<-await(second);c<-await(get_cached_resource_async('shared',ctx,fetch))
 stopifnot(calls==1L,a$data==42,b$data==42,c$data==42,identical(a$fetched_at,c$fetched_at))
})
check('invalidated response cannot republish old generation',{
 clear_api_cache();old<-get_cached_resource_async('late',ctx,function()promises::promise(function(resolve,reject)later::later(function()resolve('old'),.02)))
 invalidate_resources(ctx,resources='late');fresh<-get_cached_resource_async('late',ctx,function()promises::promise_resolve('new'))
 stopifnot(await(fresh)$data=='new',await(old)$status=='unavailable',api_cache_env[[api_cache_key('late',context=ctx)]]$data=='new')
})
cat(sprintf('DATA SERVICE: %d passed / 0 failed\n',n))
