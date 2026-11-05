# Acknowledging already-loaded histories must not recalculate the league matrix.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='ack-fixture',token='synthetic')
scoring<-paste0(normalize_league_rules(list(),list(pointSystem='fixture'),'league')$scoring_version)
stamp<-Sys.time()-10;ids<-sprintf('p%04d',1:527)
values<-c(rep(c(0,-2,3),length.out=33),2,5,12,12,6)
players<-data.frame(id=ids,average.fitness='2,5,12,12,6',observed_at=format(stamp,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'))
observations<-function(player,time=stamp)data.frame(player_id=player,championship_id='league',season='unknown',scoring_version=scoring,
 round=1:38,points=values,score_status='unknown',observed_at=format(time,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),occurred_at=NA_character_,round_start_at=NA_character_)
checks<-data.frame(player_id=head(ids,520),championship_id='league',season='unknown',scoring_version=scoring,
 checked_at=format(stamp,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),result_status='scores',point_count=38L)
dataset<-list(points=dplyr::bind_rows(lapply(head(ids,520),observations)),checks=checks,complete=TRUE)
read_league_round_dataset<-function(...)dataset
get_championship_players<-function(...)players
get_lineup_from_team<-function(...)list(pointSystem='fixture')
pending<-new.env(parent=emptyenv());writes<-list();requests<-0L;publications<-0L
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE){
 if(fn=='get_player_recent_points'){
  requests<<-requests+1L
  return(promises::promise(function(resolve,reject)pending[[args$player_id]]<-resolve))
 }
 promises::promise_resolve(do.call(get(fn,.GlobalEnv),args))
}
persist_recent_points_snapshot<-function(payload,context,on_complete){writes[[length(writes)+1L]]<<-list(payload=payload,ack=on_complete);TRUE}
defer_persistence<-function(...,on_complete=NULL){if(is.function(on_complete))on_complete(TRUE);TRUE}
settle<-function(session){for(i in 1:8){later::run_now(0);session$flushReact()}}
expensive<-c(saved=0L,covered=0L,fitness=0L)
real_saved<-recent_points_saved_players
recent_points_saved_players<-function(stored,...){if(nrow(stored)>10000L)expensive[['saved']]<<-expensive[['saved']]+1L;real_saved(stored,...)}
real_covered<-recent_points_covered_players
recent_points_covered_players<-function(...){expensive[['covered']]<<-expensive[['covered']]+1L;real_covered(...)}
real_fitness<-recent_points_fitness_agrees
recent_points_fitness_agrees<-function(...){expensive[['fitness']]<<-expensive[['fitness']]+1L;real_fitness(...)}
server<-function(input,output,session){
 active<-reactiveVal(TRUE);n<-reactiveVal(8L)
 session$userData$shared_round_points<-new_shared_round_points_controller(session,publish_interval=60)
 source<-recent_player_points_source(session,reactive(auth),reactive('league'),reactive('team'),reactive(players),active,required_rounds=n)
 observe({session$userData$shared_round_points$dataset();publications<<-publications+1L})
}
shiny::testServer(server,{
 settle(session)
 while(length(source$pending())){
  id<-names(source$pending())[1]
  ctx<-resource_context(auth,'league','team',id,scoring_version=scoring)
  pending[[id]](list(context=ctx,checked_at=stamp+5,observations=observations(id,stamp+5)));settle(session)
 }
 stopifnot(requests==7L,length(writes)==7L,source$status()$saving==7L,source$status()$completed==527L)
 before<-expensive;before_publications<-publications;times<-numeric();ticks<-numeric();pulse_live<-TRUE;pulse_cancel<-NULL
 heartbeat<-function(){if(!pulse_live)return();ticks<<-c(ticks,as.numeric(Sys.time()));pulse_cancel<<-later::later(heartbeat,.001)}
 heartbeat()
 for(write in writes){
  timing<-system.time({write$ack(TRUE);settle(session)})
  times<-c(times,timing[['elapsed']])
 }
 later::run_now(.005);pulse_live<-FALSE;if(is.function(pulse_cancel))pulse_cancel()
 stopifnot(identical(expensive,before),requests==7L,publications==before_publications,
  source$status()$saving==0L,source$status()$completed==527L,nrow(session$userData$shared_round_points$entry()$checks)==527L,
  all(add_recent_points_average(players,source$history(),4)$recent_points_avg==8.75),max(diff(ticks))<1)
 cat(sprintf('PASS seven history ACKs at527 players x38 rounds: %.3fs total, %.3fs worst heartbeat gap; zero full-matrix scans, fitness recalculations, extra reads or publications.\n',sum(times),max(diff(ticks))))
 # Cache keys retain distinct requested windows and invalidate on new live rounds.
 env<-environment(session$userData$shared_round_points$entry);entry<-session$userData$shared_round_points$entry()
 covered<-get('covered_for',env);merge<-get('merge_live',env)
 cache_bytes<-as.numeric(object.size(entry$saved_ids_cache))+as.numeric(object.size(entry$coverage_cache))+as.numeric(object.size(entry$latest_round_cache))
 before<-expensive[['covered']];stopifnot(length(covered(entry,8L))==527L,length(covered(entry,8L))==527L,expensive[['covered']]==before)
 stopifnot(length(covered(entry,20L))==527L,expensive[['covered']]==before+1L)
 for(window in 1:20)covered(entry,window)
 max_cache_bytes<-as.numeric(object.size(entry$saved_ids_cache))+as.numeric(object.size(entry$coverage_cache))+as.numeric(object.size(entry$latest_round_cache))
 cat(sprintf('CACHE527: %.0f bytes for current window; %.0f bytes for all20 windows (ID sets and scalar latest only).\n',cache_bytes,max_cache_bytes))
 before<-expensive[['covered']]
 new_round<-observations(ids[1],stamp+6)[1,,drop=FALSE];new_round$round<-39L;new_round$points<-9
 merge(entry,new_round)
 stopifnot(identical(covered(entry,1L),ids[1]),expensive[['covered']]==before+1L)
 entry$season<-'another-season'
 stopifnot(!length(covered(entry,1L)),!length(get('saved_ids_for',env)(entry)))
 cat('PASS cached coverage distinguishes N, invalidates on new observations, and isolates a changed season.\n')
})
