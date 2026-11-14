options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
# Synthetic database envelopes and acknowledgements only; no account/source I/O.
for(method in c('GET','POST','PUT','PATCH','DELETE'))suppressMessages(trace(method,where=asNamespace('httr'),print=FALSE,tracer=quote(stop('Live HTTP forbidden'))))
auth<-c(userid='history-worker-fixture',token='synthetic')
scoring<-normalize_league_rules(list(),list(pointSystem='fixture'),'league')$scoring_version
remember_league_context(list(account_id=auth[['userid']],championship_id='league',competition_id='unknown',season='unknown',
 scoring_version=scoring,observed_at=rival_finance_stamp(),expires_at=rival_finance_stamp(Sys.time()+300),complete=FALSE,payload=list()))
ctx<-resource_context(auth,'league','team',scoring_version=scoring)
players<-data.frame(id=sprintf('p%04d',1:527))
stamp<-as.POSIXct(floor(as.numeric(Sys.time()))-3600,origin='1970-01-01',tz='UTC')
rows<-function(ids,points=c(0,-2,6),at=stamp) data.frame(player_id=rep(ids,each=3L),championship_id='league',
 season='unknown',scoring_version=scoring,round=rep(1:3,length(ids)),points=rep(points,length(ids)),score_status='unknown',
 observed_at=format(at,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),occurred_at=NA_character_,round_start_at=NA_character_)
checks<-data.frame(player_id=head(players$id,526),championship_id='league',season='unknown',scoring_version=scoring,
 checked_at=format(stamp,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),result_status='scores',point_count=3L)
dataset<-list(points=rows(head(players$id,526)),checks=checks,complete=TRUE)
source_payload<-function(points=c(0,-2,6),at=stamp){
 scope<-ctx;scope$player_id<-tail(players$id,1)
 structure(list(context=scope,observations=rows(scope$player_id,points,at),checked_at=at,status='partial'),worker_history_persisted=TRUE)
}
fm_worker_configured<-function()TRUE
account<-fm_worker_account_state(auth[['userid']]);account$known<-TRUE;account$account<-list(enabled=TRUE,auth_status='active')
rpc<-character();fm_worker_rpc<-function(name,...){rpc<<-c(rpc,name);stopifnot(name=='read');promises::promise_resolve(list(account=account$account,health=NULL,jobs=list(),resources=list()))}
restore<-function(handler,args,value,version='v1',expired=TRUE){
 descriptor<-fm_worker_descriptor(handler,args,ctx)
 wire<-charToRaw(fm_worker_json(fm_worker_encode(value)));compressed<-history_baseline_compress(wire)
 manifest<-list(account_id=auth[['userid']],resource_key=descriptor$resource_key,handler=handler,version=version,
  json_bytes=length(wire),parts=1L,sha256=paste0(openssl::sha256(compressed)),
  observed_at=rival_finance_stamp(stamp),checked_at=rival_finance_stamp(stamp),
  valid_until=rival_finance_stamp(if(expired)stamp+60 else Sys.time()+60),status=if(handler=='get_player_recent_points')'partial'else'ok',
  completeness=if(handler=='get_player_recent_points')'partial'else'unknown')
 item<-fm_worker_restore(list(key=descriptor$resource_key,manifest=manifest,chunks=list(jsonlite::base64_enc(compressed))),auth[['userid']])
 fm_worker_put(auth[['userid']],descriptor$resource_key,item)
 descriptor
}
base_args<-list(login=auth,championship_id='league',user_team_id='team')
lineup<-restore('get_lineup_from_team',base_args,list(pointSystem='fixture'))
catalog<-restore('get_championship_players',base_args[c('login','championship_id')],players)
player_args<-c(base_args,list(player_id=tail(players$id,1),scoring_version=scoring))
player<-restore('get_player_recent_points',player_args,source_payload())
real_async<-async_adapter_fetch;reads<-character();db_reads<-0L
async_adapter_fetch<-function(fn,args=list(),context,...){
 reads<<-c(reads,fn)
 if(fn=='read_league_round_dataset'){
  db_reads<<-db_reads+1L;return(promises::promise_resolve(dataset))
 }
 real_async(fn,args,context,...)
}
writes<-list();persist_recent_points_snapshot<-function(payload,context,on_complete){
 writes[[length(writes)+1L]]<<-list(payload=payload,ack=on_complete);TRUE
}
settle<-function(session)for(i in 1:20){later::run_now(0);session$flushReact()}
server<-function(input,output,session){
 active<-reactiveVal(TRUE)
 source<-recent_player_points_source(session,reactive(auth),reactive('league'),reactive('team'),reactive(players),active,
  required_rounds=function()3L)
}
time<-system.time(shiny::testServer(server,{
 heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,0);settle(session)
 state<-source$status()
 stopifnot(heartbeat,state$total==527L,state$completed==527L,state$remaining==0L,state$failed==0L,
  !length(state$initial_errors),state$matrix_complete,state$saving==0L,db_reads==1L,
  sum(reads=='get_player_recent_points')==1L,length(writes)==0L,
  all(c(lineup$resource_key,catalog$resource_key,player$resource_key)%in%fm_worker_live_keys(account)))
 average<-add_recent_points_average(players,source$history(),3)$recent_points_avg
 stopifnot(all(average==4/3),all(source$history()$points%in%c(0,-2,6)))
 stopifnot(source$status()$saving==0L,nrow(session$userData$shared_round_points$entry()$checks)==527L)
 cat('PASS 527 histories complete from expired database evidence: 526 reused, 1 already persisted worker result reused with its original timestamp; zero GUI writes; source HTTP=0\n')
 before<-list(reads=reads,rpc=rpc)
 for(i in 1:4)settle(session)
 stopifnot(identical(reads,before$reads),identical(rpc,before$rpc))
 cat('PASS settled reads retain live interests and idle flushes do not reread, requeue or acquire\n')
 # Simulate the same version publication used by the asynchronous DB poll.
 restore('get_player_recent_points',player_args,source_payload(c(0,-2,12),stamp+30),'v2',FALSE)
 version<-fm_worker_resource_revision(account,player$resource_key);version(isolate(version())+1L)
 settle(session)
 stopifnot(sum(reads=='get_player_recent_points')==2L,length(writes)==0L,
  source$history()$points[source$history()$player_id==tail(players$id,1)&source$history()$round==3]==12,
  source$status()$saving==0L,db_reads==1L)
 stopifnot(is.null(source$status()$save_error),source$status()$saving==0L)
 cat('PASS a new stored player version repairs scores without manual refresh, source HTTP or duplicate writes\n')
 restore('get_lineup_from_team',base_args,list(pointSystem='fixture'),'v2',FALSE)
 version<-fm_worker_resource_revision(account,lineup$resource_key);version(isolate(version())+1L)
 settle(session)
 stopifnot(sum(reads=='get_lineup_from_team')==1L,db_reads==1L,!length(source$status()$initial_errors))
 active(FALSE);settle(session)
 stopifnot(!length(fm_worker_live_keys(account)))
 before<-reads
 version(isolate(version())+1L);settle(session)
 stopifnot(identical(reads,before),length(session$userData$shared_round_points$entry()$worker_reads)<=128L)
 cat('PASS unchanged scoring publications avoid dataset rereads and inactive views release worker interests\n')
}))
cat(sprintf('PERF 527-player stored replay: CPU %.3fs / wall %.3fs; two targeted player reads, one dataset read, no source HTTP\n',sum(time[c('user.self','sys.self')]),time[['elapsed']]))
# An older worker snapshot without the explicit factual-write proof is still
# useful display evidence, but may not manufacture a completion marker.
clear_api_cache();account$descriptors<-list();account$waiters<-list();account$resource_revisions<-new.env(parent=emptyenv())
unproven<-source_payload();attr(unproven,'worker_history_persisted')<-NULL
invisible(restore('get_player_recent_points',player_args,unproven,'legacy-no-proof'))
writes<-list();reads<-character();db_reads<-0L
shiny::testServer(server,{
 settle(session)
 stopifnot(source$status()$completed==527L,length(writes)==0L,source$status()$saving==0L,
  nrow(session$userData$shared_round_points$entry()$checks)==526L)
 cat('PASS legacy stored history stays displayable with zero GUI writes and cannot invent an acknowledged completion marker\n')
})
invisible(restore('get_player_recent_points',player_args,source_payload(),'persisted-proof'))
# A real rejected initial worker waiter must recover when its stored version arrives.
clear_api_cache();rm(list=fm_worker_key(auth[['userid']],lineup$resource_key),envir=.fm_worker$values)
account$descriptors<-list();account$waiters<-list();account$resource_revisions<-new.env(parent=emptyenv())
writes<-list();reads<-character();db_reads<-0L
shiny::testServer(server,{
 settle(session)
 stopifnot(length(account$waiters)==1L,db_reads==0L,source$status()$completed==0L)
 waiting_id<-names(account$waiters)[1];waiting<-account$waiters[[waiting_id]]
 if(is.function(waiting$cancel))waiting$cancel()
 account$waiters[[waiting_id]]<-NULL
 waiting$reject(resource_failure(resource='get_lineup_from_team',category='network'));settle(session)
 stopifnot(source$status()$completed==0L,!is.null(source$status()$initial_errors$lineup),db_reads==0L)
 restore('get_lineup_from_team',base_args,list(pointSystem='fixture'),'recovered',FALSE)
 version<-fm_worker_resource_revision(account,lineup$resource_key);version(isolate(version())+1L);settle(session)
 stopifnot(!length(source$status()$initial_errors),source$status()$completed==527L,db_reads==1L,
  length(writes)==0L,source$status()$saving==0L)
 stopifnot(source$status()$saving==0L)
 cat('PASS a real initial worker rejection stays truthful, then recovers automatically from a new database version without Refresh\n')
})
clear_api_cache();account$descriptors<-list();account$waiters<-list();account$resource_revisions<-new.env(parent=emptyenv())
reads<-character();db_reads<-0L
old_context<-ctx;old_context$season<-'old-fixture-season';old_context$competition_id<-'old-fixture-competition'
invisible(restore('get_resolved_league_context',base_args,old_context))
shiny::testServer(function(input,output,session){
 session$userData$league_context_enabled<-TRUE
 source<-recent_player_points_source(session,reactive(auth),reactive('league'),reactive('team'),reactive(players),catalog_scope=FALSE)
},{
 settle(session)
 stopifnot(identical(reads,'get_resolved_league_context'),db_reads==0L,source$status()$completed==0L,
  !is.null(source$status()$initial_errors$context),session$userData$shared_round_points$entry()$season=='unknown')
 cat('PASS expired resolver metadata cannot establish an unknown current season or read an old archive\n')
})
