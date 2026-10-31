# Offline confirmed oldest-to-newest fitness -> explicit league-round regression.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(name,code){force(code);checks<<-checks+1L;cat('PASS',name,'\n')}
settle<-function(session){for(i in 1:20){later::run_now(timeoutSecs=0);session$flushReact()}}
auth<-c(token='synthetic',userid='fitness-person')
scoring<-paste0(normalize_league_rules(list(),list(pointSystem='fixture'),'league')$scoring_version)
stamp<-Sys.time()-20
ctx<-resource_context(auth,'league','team','p1',scoring_version=scoring)
rows<-data.frame(id=paste0('p',1:63),average.fitness='2,5,12,12,6',observed_at=format(stamp,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'))
evidence<-recent_points_fitness_rows(rows)
observations<-function(player,rounds=6:10,points=c(2,5,12,12,6),time=stamp+10){
 data.frame(player_id=rep(player,length(rounds)),championship_id=rep('league',length(rounds)),season=rep('unknown',length(rounds)),scoring_version=rep(scoring,length(rounds)),
  round=rounds,points=points,score_status=ifelse(is.finite(points),'unknown','unavailable'),
  observed_at=rep(format(time,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),length(rounds)),occurred_at=rep(NA_character_,length(rounds)),round_start_at=rep(NA_character_,length(rounds)))
}
payload<-function(player='p1',rounds=6:10,points=c(2,5,12,12,6)){
 context<-ctx;context$player_id<-player
 list(context=context,checked_at=stamp+10,observations=observations(player,rounds,points))
}
check('confirmed sequence maps to 6..10; last four 8.75 and last three 10',{
 anchor<-recent_points_fitness_anchor(evidence[1,,drop=FALSE],payload(),ctx)
 mapped<-recent_points_fitness_observations(evidence[1,,drop=FALSE],anchor)
 stopifnot(anchor$round==10L,identical(mapped$round,6:10),identical(mapped$points,c(2,5,12,12,6)),
  add_recent_points_average(rows[1,,drop=FALSE],mapped,4)$recent_points_avg==8.75,
  add_recent_points_average(rows[1,,drop=FALSE],mapped,3)$recent_points_avg==10,
  all(as.numeric(fm_time(mapped$observed_at))==as.numeric(fm_time(evidence$observed_at[1]))))
})
check('older subsequence, stale source, conflicting context and flat evidence do not anchor',{
 old<-payload(rounds=6:11,points=c(2,5,12,12,6,99))
 repeated<-payload(rounds=1:10,points=rep(c(2,5,12,12,6),2))
 short<-evidence[1,,drop=FALSE];short$fitness<-'5,12,12,6'
 stale<-payload();stale$checked_at<-stamp-1
 wrong<-payload();wrong$context$season<-'other'
 flat<-evidence[1,,drop=FALSE];flat$fitness<-'0,0,0,0,0'
 stopifnot(is.null(recent_points_fitness_anchor(evidence[1,,drop=FALSE],old,ctx)),
  is.null(recent_points_fitness_anchor(evidence[1,,drop=FALSE],stale,ctx)),
  is.null(recent_points_fitness_anchor(evidence[1,,drop=FALSE],repeated,ctx)),
  is.null(recent_points_fitness_anchor(short,payload(),ctx)),
  is.null(recent_points_fitness_anchor(evidence[1,,drop=FALSE],wrong,ctx)),
  is.null(recent_points_fitness_anchor(flat,payload(points=rep(0,5)),ctx)),
  !length(recent_points_fitness_values('1,Inf,3')),
  nrow(recent_points_fitness_rows(transform(rows,observed_at=NA_character_)))==0L)
})
check('zero, negatives, NULL slots and season boundary keep their positions',{
 anchor<-recent_points_fitness_anchor(evidence[1,,drop=FALSE],payload(),ctx)
 mixed<-evidence[1,,drop=FALSE];mixed$fitness<-'0,-2,NA,9,1'
 mapped<-recent_points_fitness_observations(mixed,anchor)
 short<-mixed;short$fitness<-'0,-2'
 early<-anchor;early$round<-2L
 stopifnot(identical(mapped$points,c(0,-2,NA_real_,9,1)),mapped$round[3]==8L,
  mapped$score_status[3]=='unavailable',
  identical(recent_points_fitness_observations(short,early)$round,1:2),
  nrow(recent_points_fitness_observations(mixed,early))==0L)
})
check('newer NULL corrections and unchanged final scores are not overwritten',{
 anchor<-recent_points_fitness_anchor(evidence[1,,drop=FALSE],payload(),ctx)
 mapped<-recent_points_fitness_observations(evidence[1,,drop=FALSE],anchor)
 newer<-observations('p1',rounds=10,points=NA_real_,time=stamp+5)
 unchanged<-observations('p1',rounds=9,points=12,time=stamp-5);unchanged$score_status<-'final'
 remaining<-recent_points_fitness_changes(mapped,dplyr::bind_rows(newer,unchanged))
 stopifnot(!any(remaining$round%in%c(9,10)),nrow(remaining)==3L)
})
check('blocked-write buffers deduplicate repeated rounds and cap retained rows',{
 anchor<-recent_points_fitness_anchor(evidence[1,,drop=FALSE],payload(),ctx)
 mapped<-recent_points_fitness_observations(evidence[1,,drop=FALSE],anchor)
 replay<-mapped[rep(seq_len(nrow(mapped)),100L),,drop=FALSE]
 many<-mapped[rep(1L,10005L),,drop=FALSE];many$player_id<-paste0('bounded-',seq_len(nrow(many)))
 stopifnot(nrow(recent_points_fitness_buffer(replay))==5L,nrow(recent_points_fitness_buffer(many))==10000L)
})
get_championship_players<-function(...)rows
get_lineup_from_team<-function(...)list(pointSystem='fixture')
recorded<-list();writes<-list();calls<-character();pending<-new.env(parent=emptyenv())
saved<-dplyr::bind_rows(lapply(rows$id,function(id)observations(id,c(7,9,10),c(5,12,6),stamp-5)))
dataset<-list(points=saved,checks=data.frame(),complete=TRUE)
read_league_round_dataset<-function(...)dataset
persist_recent_points_snapshot<-function(payload,context,on_complete){recorded[[length(recorded)+1L]]<<-list(payload=payload,ack=on_complete);TRUE}
defer_persistence<-function(fn,args,context,on_complete){writes[[length(writes)+1L]]<<-list(fn=fn,args=args,context=context,ack=on_complete);TRUE}
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE){
 calls<<-c(calls,paste0(fn,':',args$player_id %||% ''))
 if(fn=='get_player_recent_points')return(promises::promise(function(resolve,reject){pending[[args$player_id]]<-list(resolve=resolve,current=is_current)}))
 promises::promise_resolve(do.call(get(fn,.GlobalEnv),args))
}
reset<-function(){clear_api_cache();calls<<-character();recorded<<-list();writes<<-list();pending<<-new.env(parent=emptyenv());.persistence_state$queue<-list();.persistence_state$active<-NULL}
server<-function(input,output,session){
 active<-reactiveVal(TRUE);n<-reactiveVal(4L);refresh<-reactiveVal(0L)
 source<-recent_player_points_source(session,reactive(auth),reactive('league'),reactive('team'),reactive(rows[1:3,,drop=FALSE]),active,refresh_trigger=refresh,required_rounds=n)
}
reset()
shiny::testServer(server,{
 settle(session)
 check('fresh partial legacy history starts one shared-snapshot probe',stopifnot(sum(grepl('get_player_recent_points',calls))==1L,sum(grepl('read_league_round_dataset',calls))==1L,length(source$pending())==1L))
 pending[['p1']]$resolve(payload());settle(session)
 check('one probe fills every catalog fitness snapshot and computes requested means',{
  h<-source$history();averages<-add_recent_points_average(rows[1:3,,drop=FALSE],h,4)
  stopifnot(all(averages$recent_points_avg==8.75),nrow(h)==15L,
   sum(grepl('get_player_recent_points',calls))==1L,length(recorded)==1L,
   nrow(session$userData$shared_round_points$entry()$checks)==0L)
 })
 check('fitness writes contain only missing rounds, bounded chunks, original time and no check marker',{
  points<-dplyr::bind_rows(lapply(writes,function(x)x$args[[2]]))
  stopifnot(nrow(points)==124L,length(writes)==2L,all(points$round%in%c(6,8)),
   all(vapply(writes,function(x)x$fn=='supabase_post_direct'&&x$args[[1]]=='player_match_observations'&&nrow(x$args[[2]])<=100L,logical(1))),
   all(as.numeric(fm_time(points$observed_at))==as.numeric(fm_time(evidence$observed_at[1]))))
 })
 for(write in writes)write$ack(TRUE)
 recorded[[1]]$ack(TRUE);settle(session)
 check('ACK creates only real endpoint check and unchanged navigation or N<=5 causes no reads',{
  before<-calls;active(FALSE);settle(session);active(TRUE);settle(session);n(3L);settle(session)
  stopifnot(identical(calls,before),nrow(session$userData$shared_round_points$entry()$checks)==1L,
   all(add_recent_points_average(rows[1:3,,drop=FALSE],source$history(),3)$recent_points_avg==10))
 })
 n(8L);settle(session)
 check('N greater than five can still fetch older histories for mapped players',stopifnot(sum(grepl('get_player_recent_points',calls))==3L,exists('p2',pending),exists('p3',pending)))
})
# Failed observation-only batches wait for explicit Refresh and retain their facts.
reset()
shiny::testServer(server,{
 settle(session);pending[['p1']]$resolve(payload());settle(session)
 failed<-writes[[1]]$args[[2]]
 writes[[1]]$ack(FALSE);writes[[2]]$ack(TRUE);recorded[[1]]$ack(TRUE);settle(session)
 before<-length(writes);settle(session)
 stopifnot(length(writes)==before,!is.null(source$status()$save_error),source$status()$saving==0L)
 refresh(1L);settle(session)
 check('failed fitness batch retries only after explicit Refresh and clears error on ACK',{
  stopifnot(length(writes)==before+1L,identical(writes[[before+1L]]$args[[2]],failed))
  writes[[before+1L]]$ack(TRUE);settle(session)
  stopifnot(is.null(source$status()$save_error))
 })
})
# An already agreeing matrix avoids even the anchor probe in another root session.
dataset$points<-dplyr::bind_rows(lapply(rows$id,function(id)observations(id,time=stamp+10)))
reset()
shiny::testServer(server,{
 settle(session)
 check('complete agreeing saved five-round window causes no probe or write in new session',stopifnot(!any(grepl('get_player_recent_points',calls)),!length(writes),!length(recorded)))
})
# An explicit empty endpoint response cannot assign any rounds to the vector.
dataset$points<-saved;reset()
shiny::testServer(server,{
 settle(session);pending[['p1']]$resolve(payload(rounds=integer(),points=numeric()));settle(session)
 check('empty probe never manufactures fitness round assignments',stopifnot(!length(writes),nrow(source$history())==9L,all(is.na(add_recent_points_average(rows[1:3,,drop=FALSE],source$history(),4)$recent_points_avg))))
})
cat(sprintf('FITNESS ROUND SNAPSHOT: %d passed / 0 failed\n',checks))
# Time the actual controller's bulk mapping/merge once at a representative size.
rows<-data.frame(id=paste0('p',1:500),average.fitness='2,5,12,12,6',observed_at=format(stamp,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'))
dataset$points<-dplyr::bind_rows(lapply(rows$id,function(id)observations(id,c(7,9,10),c(5,12,6),stamp-5)))
reset()
shiny::testServer(server,{
 settle(session)
 elapsed<-system.time({pending[['p1']]$resolve(payload());settle(session)})[['elapsed']]
 check('500-player fitness bulk mapping stays bounded and uses one probe',{
  stopifnot(sum(grepl('get_player_recent_points',calls))==1L,
   length(session$userData$shared_round_points$entry()$live)==500L,
   all(add_recent_points_average(rows[1:3,,drop=FALSE],source$history(),4)$recent_points_avg==8.75))
  cat(sprintf('500-player x5-round acquisition/map/merge/publication: %.3fs\n',elapsed))
 })
})
cat(sprintf('FITNESS ROUND SNAPSHOT FINAL: %d passed / 0 failed\n',checks))
