#!/usr/bin/env Rscript
# Offline known-answer coverage for reusing controller calculations across saves.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(name,code){force(code);checks<<-checks+1L;cat('PASS',name,'\n')}
auth<-c(userid='cache-review',token='synthetic')
score<-paste0(normalize_league_rules(list(),list(pointSystem='fixture'),'league')$scoring_version)
stamp<-Sys.time()-20
rows<-data.frame(id=c('p1','p2'),average.fitness='2,5,12,12,6',
 observed_at=format(stamp,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'))
observations<-function(id,extra=FALSE){
 rounds<-if(extra)6:11 else 6:10
 data.frame(player_id=id,championship_id='league',season='unknown',scoring_version=score,
  round=rounds,points=if(extra)c(2,5,12,12,6,99)else c(2,5,12,12,6),score_status='unknown',
  observed_at=format(stamp+if(extra)10 else -5,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),
  occurred_at=NA_character_,round_start_at=NA_character_)
}
payload<-function(id)list(context=resource_context(auth,'league','team',id,scoring_version=score),
 checked_at=stamp+10,observations=observations(id,TRUE))
read_league_round_dataset<-function(...)list(points=dplyr::bind_rows(observations('p1'),observations('p2')),
 checks=data.frame(),complete=TRUE)
get_lineup_from_team<-function(...)list(pointSystem='fixture')
get_championship_players<-function(...)rows
calls<-character();pending<-new.env(parent=emptyenv());acks<-list()
async_adapter_fetch<-function(fn,args=list(),context,...){
 if(fn=='get_player_recent_points'){
  calls<<-c(calls,args$player_id)
  return(promises::promise(function(resolve,reject)pending[[args$player_id]]<-resolve))
 }
 promises::promise_resolve(do.call(get(fn,.GlobalEnv),args))
}
persist_recent_points_snapshot<-function(payload,context,on_complete){acks[[length(acks)+1L]]<<-on_complete;TRUE}
defer_persistence<-function(...,on_complete){on_complete(TRUE);TRUE}
settle<-function(session)for(i in 1:5){later::run_now(0);session$flushReact()}
clear_api_cache()
shiny::testServer(function(input,output,session){
 refresh<-reactiveVal(0L);n<-reactiveVal(4L)
 source<-recent_player_points_source(session,reactive(auth),reactive('league'),reactive('team'),
  reactive(rows[1,,drop=FALSE]),reactive(TRUE),refresh_trigger=refresh,required_rounds=n)
},{
 settle(session)
 check('agreeing saved histories start without source requests',stopifnot(!length(calls),source$status()$completed==2L))
 refresh(1L);settle(session);stopifnot(identical(calls,'p1'))
 pending[['p1']](payload('p1'));settle(session)
 check('a newly acquired round invalidates other fitness coverage before its save acknowledges',{
  stopifnot(identical(calls,c('p1','p2')),source$status()$completed==1L,
   source$status()$pending==1L,source$status()$saving==1L,length(acks)==1L)
 })
 pending[['p2']](payload('p2'));settle(session)
 check('completed acquisition is independent of save success and ACK cannot fabricate another acquisition',{
  stopifnot(source$status()$completed==2L,source$status()$saving==2L)
  before<-calls;acks[[1]](FALSE);acks[[2]](TRUE);settle(session)
  entry<-session$userData$shared_round_points$entry()
  stopifnot(identical(calls,before),source$status()$completed==2L,source$status()$saving==0L,
   !is.null(source$status()$save_error),identical(entry$checks$player_id,'p2'))
 })
})

# Future observation timestamps can become eligible without a matrix reload;
# cached ID sets must preserve the helper's existing now+60-second contract.
shiny::testServer(function(input,output,session){
 controller<-new_shared_round_points_controller(session)
},{
 helper<-get('saved_ids_for',environment(controller$entry))
 covered<-get('covered_for',environment(controller$entry))
 entry<-new.env(parent=emptyenv())
 entry$context<-resource_context(auth,'league','team',scoring_version=score)
 entry$season<-'unknown';entry$scoring<-score
 entry$points<-dplyr::bind_rows(observations('future'),observations('other'))
 entry$catalog_ids<-'future';entry$live<-list()
 entry$saved_ids_cache<-NULL;entry$coverage_cache<-NULL
 clock<-as.POSIXct('2026-11-05 23:59:30',tz='UTC')
 entry$points$observed_at<-format(clock+120,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')
 assign('Sys.time',function()clock,envir=.GlobalEnv)
 tryCatch({
  check('future IDs become eligible at their exact boundary and remain reusable across days',{
   stopifnot(!length(helper(entry)))
   clock<-clock+61
   stopifnot(identical(helper(entry),'future'))
   clock<-clock+90*86400
   stopifnot(identical(helper(entry),'future'))
  })
  check('known season changes invalidate cached saved IDs',{
   entry$season<-'another-season';stopifnot(!length(helper(entry)))
   entry$season<-'unknown';stopifnot(identical(helper(entry),'future'))
  })
  check('fractional round-window requests retain at most twenty exact coverage results',{
   for(n in seq(1,20,length.out=50L)){
    expected<-intersect(recent_points_covered_players(entry$points,entry$context,n),'future')
    stopifnot(identical(covered(entry,n),expected),length(entry$coverage_cache$by_n)<=20L)
   }
   stopifnot(length(entry$coverage_cache$by_n)==20L)
  })
  check('current player membership changes invalidate both ID caches without retaining excluded players',{
   entry$catalog_ids<-'other'
   stopifnot(identical(helper(entry),'other'),identical(covered(entry,4L),'other'),
    !'future'%in%unlist(entry$coverage_cache$by_n,use.names=FALSE))
   entry$catalog_ids<-c('future','other')
   stopifnot(setequal(helper(entry),entry$catalog_ids),setequal(covered(entry,4L),entry$catalog_ids))
  })
 },finally=rm('Sys.time',envir=.GlobalEnv))
})
cat(sprintf('ROUND HISTORY CACHE INVALIDATION: %d passed / 0 failed\n',checks))
