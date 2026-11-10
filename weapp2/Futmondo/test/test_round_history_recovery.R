#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='recovery-fixture',token='synthetic')
score<-paste0(normalize_league_rules(list(),list(pointSystem='fixture'),'league')$scoring_version)
stamp<-Sys.time()-10
ids<-sprintf('p%04d',seq_len(527))
players<-data.frame(id=ids)
base_context<-resource_context(auth,'league','team',scoring_version=score)
observations<-function(player,rounds=1:3,points=c(0,-2,NA_real_),time=stamp){
 data.frame(player_id=player,championship_id='league',season='unknown',scoring_version=score,
 round=rounds,points=points,score_status=ifelse(is.finite(points),'unknown','unavailable'),
 observed_at=format(time,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),occurred_at=NA_character_,round_start_at=NA_character_)
}
payload<-function(player,rounds=1:3,points=c(0,-2,NA_real_)){
 context<-base_context;context$player_id<-player
 list(context=context,checked_at=stamp+5,observations=observations(player,rounds,points,stamp+5))
}
checks_for<-function(players)data.frame(player_id=players,championship_id='league',season='unknown',scoring_version=score,
 checked_at=format(stamp,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),result_status='scores',point_count=3L)
settle<-function(session){for(i in seq_len(8)){later::run_now(0);session$flushReact()}}
real_cache<-get_cached_resource_async
outer_reject<-character();outer_counts<-list()
get_cached_resource_async<-function(resource,context,fetch,refresh=FALSE){
 if(resource=='player_summary_points'){
  id<-context$player_id;outer_counts[[id]]<<-(outer_counts[[id]] %||% 0L)+1L
  if(id%in%outer_reject&&outer_counts[[id]]==1L)return(promises::promise_reject(resource_failure(resource='get_player_recent_points',category='read_budget')))
 }
 real_cache(resource,context,fetch,refresh)
}
reset<-function(stored=512L,fitness=FALSE){
 clear_api_cache();calls<<-list();initial_calls<<-list();published_count<<-0L;pending<<-new.env(parent=emptyenv());writes<<-list();outer_counts<<-list();outer_reject<<-character()
 .persistence_state$queue<-list();.persistence_state$active<-NULL;hold_last<<-0L;write_count<<-0L
 players<<-data.frame(id=ids)
 if(fitness){
  players$average.fitness<<-'2,5,12,12,6';players$observed_at<<-format(stamp,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC')
  points<-dplyr::bind_rows(lapply(ids,function(id)observations(id,c(7,9,10),c(5,12,6),stamp-1)))
  markers<-data.frame()
 }else{
  points<-dplyr::bind_rows(lapply(head(ids,stored),observations));markers<-checks_for(head(ids,stored))
 }
 dataset<<-list(points=points,checks=markers,complete=TRUE)
}
read_league_round_dataset<-function(...)dataset
get_championship_players<-function(...)players
get_lineup_from_team<-function(...)list(pointSystem='fixture')
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE){
 if(fn=='get_player_recent_points'){
  id<-args$player_id;calls[[id]]<<-(calls[[id]] %||% 0L)+1L
  return(promises::promise(function(resolve,reject)pending[[id]]<-list(resolve=resolve,reject=reject,current=is_current)))
 }
 initial_calls[[fn]]<<-(initial_calls[[fn]] %||% 0L)+1L
 promises::promise_resolve(do.call(get(fn,.GlobalEnv),args))
}
persist_recent_points_snapshot<-function(payload,context,on_complete){
 write_count<<-write_count+1L;writes[[length(writes)+1L]]<<-list(payload=payload,ack=on_complete)
 if(hold_last==0L||write_count<=15L-hold_last)on_complete(TRUE)
 TRUE
}
defer_persistence<-function(fn,args,context,on_complete,...){on_complete(TRUE);TRUE}
server<-function(input,output,session){
 active<-reactiveVal(TRUE);refresh<-reactiveVal(0L);n<-reactiveVal(if('average.fitness'%in%names(players))4L else 3L)
 session$userData$shared_round_points<-new_shared_round_points_controller(session,publish_interval=60,retry_delays=c(0,0))
 source<-recent_player_points_source(session,reactive(auth),reactive('league'),reactive('team'),reactive(players),active,
  refresh_trigger=refresh,required_rounds=n)
 observe({session$userData$shared_round_points$dataset();published_count<<-published_count+1L})
}
reset();outer_reject<-ids[514];hold_last<-4L
timing<-system.time(shiny::testServer(server,{
 settle(session);max_pending<-0L;heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,0)
 while(length(source$pending())){
  max_pending<-max(max_pending,length(source$pending()))
  id<-names(source$pending())[1];job<-pending[[id]]
  if(id==ids[513]&&calls[[id]]==1L)job$reject(resource_failure(resource='get_player_recent_points',category='http',http_status=503L))else job$resolve(payload(id))
  settle(session)
 }
 status<-source$status()
 stopifnot(status$total==527L,status$completed==527L,status$remaining==0L,status$failed==0L,
  status$retrying==0L,status$saving==4L,max_pending<=2L,heartbeat,length(writes)==15L,
  calls[[ids[513]]]==2L,outer_counts[[ids[514]]]==2L,session$userData$shared_round_points$entry()$retry_reads==2L)
 for(write in tail(writes,4))write$ack(TRUE)
 settle(session)
 stopifnot(source$status()$saving==0L,nrow(session$userData$shared_round_points$entry()$checks)==527L,
  is.na(source$history()$points[source$history()$player_id==ids[513]&source$history()$round==3L]))
 warm_before<-list(reads=sum(unlist(outer_counts)),initial=initial_calls,published=published_count)
 warm_timing<<-system.time({active(FALSE);settle(session);active(TRUE);settle(session)})
 stopifnot(source$status()$completed==527L,sum(unlist(outer_counts))==warm_before$reads,
  identical(initial_calls,warm_before$initial),published_count==warm_before$published)
}));elapsed<-timing[['elapsed']]
cat(sprintf('PERF exact527 local: total CPU %.3fs, wall %.3fs; %d logical player reads (%d adapters), %d history publications; warm hide/show CPU %.3fs, wall %.3fs, zero additional initial/player reads or publications.\n',sum(timing[c('user.self','sys.self')]),timing[['elapsed']],sum(unlist(outer_counts)),sum(unlist(calls)),published_count,sum(warm_timing[c('user.self','sys.self')]),warm_timing[['elapsed']]))
cat(sprintf('PASS exact527:512 reused +15 acquired;503 and outer read-budget rejection recover,4 pending saves settle, unavailable score preserved (%.3fs).\n',elapsed))
reset(stored=526L)
shiny::testServer(server,{
 settle(session);pending[[ids[527]]]$reject(resource_failure(resource='get_player_recent_points',category='http',http_status=403L));settle(session)
 status<-source$status()
 stopifnot(status$completed==526L,status$failed==1L,status$pending==0L,status$remaining==0L,
  calls[[ids[527]]]==1L,identical(status$failure_details[[ids[527]]]$http_status,403L),
  grepl('^FM-HTTP-',status$failure_details[[ids[527]]]$reference),!length(writes))
})
cat('PASS permanent authorization failure settles its slot once and is not counted as a loaded player.\n')
reset(stored=526L)
shiny::testServer(server,{
 settle(session);bad<-payload(ids[527]);bad$observations<-data.frame(unexpected=1)
 pending[[ids[527]]]$resolve(bad);settle(session)
 stopifnot(source$status()$failed==1L,source$status()$pending==0L,source$status()$completed==526L,
  calls[[ids[527]]]==1L,!length(writes),source$status()$failure_details[[ids[527]]]$category=='schema')
})
cat('PASS malformed success payload clears pending, retains saved history and reports its schema diagnostic without retrying.\n')
reset(fitness=TRUE)
shiny::testServer(server,{
 settle(session);initial<-source$status()
 stopifnot(initial$total==527L,initial$completed==0L,initial$pending==1L,initial$remaining==526L)
 pending[[ids[1]]]$reject(resource_failure(resource='get_player_recent_points',category='network'));settle(session)
 stopifnot(length(calls)==1L,calls[[ids[1]]]==2L,source$status()$pending==1L,source$status()$failed==0L)
 pending[[ids[1]]]$resolve(payload(ids[1],6:10,c(2,5,12,12,6)));settle(session)
 stopifnot(source$status()$completed==527L,source$status()$remaining==0L,source$status()$pending==0L,
  source$status()$failed==0L,length(calls)==1L,all(add_recent_points_average(players,source$history(),4)$recent_points_avg==8.75))
})
cat('PASS527 fitness-held players remain counted; one failed anchor retries and completes the group with2 reads total.\n')
reset(stored=510L)
shiny::testServer(server,{
 settle(session)
 while(length(source$pending())){
  id<-names(source$pending())[1];pending[[id]]$reject(resource_failure(resource='get_player_recent_points',category='read_budget'));settle(session)
 }
 stopifnot(sum(unlist(calls))==49L,max(unlist(calls))<=3L,
  session$userData$shared_round_points$entry()$retry_reads==32L,source$status()$failed==17L,
  source$status()$pending==0L,source$status()$remaining==0L,source$status()$completed==510L)
})
cat('PASS repeated capacity failures stop at3 attempts/player and32 extra reads/context; all17 unresolved players remain explicit.\n')
# A pending backoff must never dispatch after the view is hidden.
reset(stored=526L)
slow_server<-function(input,output,session){
 active<-reactiveVal(TRUE)
 session$userData$shared_round_points<-new_shared_round_points_controller(session,publish_interval=60,retry_delays=c(.05,.05))
 source<-recent_player_points_source(session,reactive(auth),reactive('league'),reactive('team'),reactive(players),active)
}
shiny::testServer(slow_server,{
 settle(session);pending[[ids[527]]]$reject(resource_failure(resource='get_player_recent_points',category='network'));settle(session)
 active(FALSE);settle(session);before<-sum(unlist(calls))
 later::run_now(.08);settle(session)
 stopifnot(sum(unlist(calls))==before,length(source$pending())==0L)
})
cat('PASS hiding the view cancels scheduled recovery and clears pending work.\n')

# Both free slots must share one remaining retry allowance.
reset(stored=525L)
shiny::testServer(server,{
 settle(session);entry<-session$userData$shared_round_points$entry();entry$retry_reads<-31L
 for(id in tail(ids,2))pending[[id]]$reject(resource_failure(resource='get_player_recent_points',category='network'))
 settle(session)
 stopifnot(entry$retry_reads==32L,sum(unlist(calls))==3L,length(source$pending())==1L)
 id<-names(source$pending())[1];pending[[id]]$resolve(payload(id));settle(session)
 stopifnot(entry$retry_reads==32L,source$status()$completed==526L,source$status()$failed==1L)
})
cat('PASS simultaneous failures cannot exceed the shared retry allowance when two slots become free.\n')
# An initial factory throw must clear loading; Refresh retries just failed sources.
reset(stored=527L)
ordinary_adapter<-async_adapter_fetch;initial_failure<-TRUE
async_adapter_fetch<-function(fn,...){
 if(fn=='read_league_round_dataset'&&initial_failure){initial_calls[[fn]]<<-(initial_calls[[fn]] %||% 0L)+1L;stop(resource_failure(resource=fn,category='network'))}
 ordinary_adapter(fn,...)
}
shiny::testServer(server,{
 settle(session);entry<-session$userData$shared_round_points$entry()
 stopifnot(!any(entry$loading),identical(names(source$status()$initial_errors),'matrix'),length(source$pending())==2L)
 # Finish any reads already needed while the matrix was unavailable.
 for(id in names(source$pending()))pending[[id]]$resolve(payload(id))
 initial_failure<<-FALSE;before<-initial_calls;refresh(1L);settle(session)
 stopifnot(!any(entry$loading),!length(source$status()$initial_errors),entry$matrix_complete,
  initial_calls[['read_league_round_dataset']]==before[['read_league_round_dataset']]+1L,
  identical(initial_calls[['get_lineup_from_team']],before[['get_lineup_from_team']]),
  identical(initial_calls[['get_championship_players']],before[['get_championship_players']]))
 for(i in seq_len(3)){for(id in names(source$pending()))pending[[id]]$resolve(payload(id));settle(session)}
 stopifnot(source$status()$failed==0L)
 active(FALSE);settle(session)
})
async_adapter_fetch<-ordinary_adapter
cat('PASS a synchronous initial read failure settles loading and Refresh retries only that failed initial source.\n')
# A later retry must never reselect an already exhausted fitness anchor.
ids<-c('player-A','player-B','player-C');reset(fitness=TRUE)
shiny::testServer(server,{
 settle(session)
 for(i in 1:3){pending[[ids[1]]]$reject(resource_failure(resource='get_player_recent_points',category='network'));settle(session)}
 stopifnot(all(ids[2:3]%in%names(source$pending())))
 pending[[ids[2]]]$reject(resource_failure(resource='get_player_recent_points',category='network'));settle(session)
 pending[[ids[3]]]$resolve(payload(ids[3],6:10,c(2,5,12,12,6)));settle(session)
 stopifnot(calls[[ids[1]]]==3L,calls[[ids[2]]]==2L,ids[2]%in%names(source$pending()))
 pending[[ids[2]]]$resolve(payload(ids[2],6:10,c(2,5,12,12,6)));settle(session)
 stopifnot(source$status()$completed==3L,source$status()$failed==0L,source$status()$pending==0L,
  source$status()$remaining==0L,all(add_recent_points_average(players,source$history(),4)$recent_points_avg==8.75))
 n(8L);settle(session)
 # The probe established only five numbered rounds. A wider visible request
 # is allowed one full-source upgrade for each successful probe, but cannot
 # retry the exhausted first player.
 stopifnot(source$status()$completed==0L,source$status()$failed==1L,source$status()$pending==2L,
  calls[[ids[1]]]==3L,calls[[ids[2]]]==3L,calls[[ids[3]]]==2L)
 for(id in ids[2:3]){pending[[id]]$resolve(payload(id,3:10,c(0,-2,3,2,5,12,12,6)));settle(session)}
 stopifnot(source$status()$completed==2L,source$status()$failed==1L,source$status()$remaining==0L,
  source$status()$pending==0L,calls[[ids[1]]]==3L,
  is.na(add_recent_points_average(players,source$history(),8)$recent_points_avg[1]))
 before<-calls;active(FALSE);settle(session);active(TRUE);settle(session)
 stopifnot(identical(calls,before),source$status()$completed==2L,source$status()$failed==1L)
})
cat('PASS a later eligible fitness anchor retries after the first exhausts; reconstruction recovers N4 for all three but retains failed N8 missing history.\n')
# Season discovery must make previous unknown-season success eligible again.
ids<-c('player-A','player-B');season_payload<-function(id,season){x<-payload(id);x$context$season<-season;x$observations$season<-season;x}
reset(stored=1L);dataset<-list(points=data.frame(),checks=data.frame(),complete=TRUE)
shiny::testServer(server,{
 settle(session)
 pending[[ids[1]]]$resolve(payload(ids[1]));settle(session)
 pending[[ids[2]]]$resolve(season_payload(ids[2],'season-A'));settle(session)
 stopifnot(source$status()$completed==1L,source$status()$pending==1L,calls[[ids[1]]]==2L)
 pending[[ids[1]]]$resolve(season_payload(ids[1],'season-A'));settle(session)
 stopifnot(source$status()$completed==2L,source$status()$failed==0L,
  setequal(unique(source$history()$player_id),ids),all(source$history()$season=='season-A'))
})
cat('PASS unknown-season successes are reacquired after authoritative season discovery.\n')
reset(stored=1L);dataset<-list(points=data.frame(),checks=data.frame(),complete=TRUE)
shiny::testServer(server,{
 settle(session)
 pending[[ids[1]]]$resolve(season_payload(ids[1],'season-A'));settle(session)
 pending[[ids[2]]]$resolve(payload(ids[2]));settle(session)
 stopifnot(source$status()$completed==1L,source$status()$failed==1L,source$status()$pending==0L,
  identical(unique(source$history()$player_id),ids[1]),length(writes)==1L)
})
cat('PASS a late unknown-season response cannot count as completed or persist under a now-known season.\n')
reset(stored=1L);dataset<-list(points=data.frame(),checks=data.frame(),complete=TRUE)
shiny::testServer(server,{
 settle(session);bad<-payload(ids[1]);bad$context$user_team_id<-'different-team'
 pending[[ids[1]]]$resolve(bad);pending[[ids[2]]]$resolve(payload(ids[2]));settle(session)
 stopifnot(source$status()$completed==1L,source$status()$failed==1L,length(writes)==1L)
})
cat('PASS responses from another team are rejected before publication or persistence.\n')
# Failed lineup cannot start scores in an unverified scoring context.
reset(stored=1L);dataset<-list(points=data.frame(),checks=data.frame(),complete=TRUE)
ordinary_adapter<-async_adapter_fetch;lineup_failure<-TRUE
async_adapter_fetch<-function(fn,...){
 if(fn=='get_lineup_from_team'&&lineup_failure){initial_calls[[fn]]<<-(initial_calls[[fn]] %||% 0L)+1L;stop(resource_failure(resource=fn,category='network'))}
 ordinary_adapter(fn,...)
}
shiny::testServer(server,{
 settle(session);stopifnot(!length(calls),source$status()$completed==0L,source$status()$remaining==2L,
  identical(names(source$status()$initial_errors),'lineup'))
 lineup_failure<<-FALSE;before<-initial_calls;refresh(1L);settle(session)
 stopifnot(length(source$pending())==2L,!length(source$status()$initial_errors),
  initial_calls[['get_lineup_from_team']]==before[['get_lineup_from_team']]+1L,
  initial_calls[['read_league_round_dataset']]==(before[['read_league_round_dataset']] %||% 0L)+1L,
  identical(initial_calls[['get_championship_players']],before[['get_championship_players']]))
 for(id in ids)pending[[id]]$resolve(payload(id));settle(session)
 stopifnot(source$status()$completed==2L,source$status()$failed==0L)
})
async_adapter_fetch<-ordinary_adapter
cat('PASS initial scoring failure waits for its failed-only Refresh before any per-player acquisition.\n')

reset(stored=2L);dataset$complete<-FALSE
attr(dataset,'fetch_status')<-'stale';attr(dataset,'fetch_error')<-resource_failure(resource='database',category='http',http_status=503L)
shiny::testServer(server,{
 settle(session);stopifnot(!source$status()$matrix_complete,source$status()$initial_errors$matrix$http_status==503L)
 dataset$complete<<-TRUE;attr(dataset,'fetch_status')<<-NULL;attr(dataset,'fetch_error')<<-NULL
 before<-initial_calls;refresh(1L);settle(session)
 stopifnot(source$status()$matrix_complete,!length(source$status()$initial_errors),
  initial_calls[['read_league_round_dataset']]==before[['read_league_round_dataset']]+1L)
 active(FALSE);settle(session)
})
cat('PASS stale matrix diagnostics survive settlement and a failed-only Refresh recovers the database read.\n')
