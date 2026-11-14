options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
# Production regression: bulk persisted history must not become527 individual
# snapshot waiters when catalogue metadata or the unlabelled fitness changes.
for(method in c('GET','POST','PUT','PATCH','DELETE'))suppressMessages(trace(method,where=asNamespace('httr'),print=FALSE,tracer=quote(stop('Live HTTP forbidden'))))
auth<-c(userid='history-worker-fixture',token='synthetic')
scoring<-normalize_league_rules(list(),list(pointSystem='fixture'),'league')$scoring_version
remember_league_context(list(account_id=auth[['userid']],championship_id='league',competition_id='fixture-competition',season='2026',
 scoring_version=scoring,observed_at=rival_finance_stamp(),expires_at=rival_finance_stamp(Sys.time()+300),complete=TRUE,payload=list()))
ctx<-resource_context(auth,'league','team',scoring_version=scoring)
players<-data.frame(id=sprintf('p%04d',1:527),average.fitness='0,0,0,0,0')
stamp<-as.POSIXct(floor(as.numeric(Sys.time()))-3600,origin='1970-01-01',tz='UTC')
rows<-function(ids,points=c(0,-2,6,1,8),at=stamp) data.frame(player_id=rep(ids,each=5L),championship_id='league',
 competition_id='fixture-competition',season='2026',scoring_version=scoring,round=rep(1:5,length(ids)),points=rep(points,length(ids)),score_status='unknown',
 observed_at=format(at,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),occurred_at=NA_character_,round_start_at=NA_character_)
checks<-function(ids,at=stamp)data.frame(player_id=ids,championship_id='league',competition_id='fixture-competition',season='2026',scoring_version=scoring,
 checked_at=format(at,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),result_status='scores',point_count=5L)
make_dataset<-function(ids=players$id)list(points=rows(ids),checks=checks(ids),complete=TRUE)
dataset<-make_dataset(head(players$id,432))
fm_worker_configured<-function()TRUE
account<-fm_worker_account_state(auth[['userid']]);account$known<-TRUE;account$account<-list(enabled=TRUE,auth_status='active')
rpc<-character();fm_worker_rpc<-function(name,...){rpc<<-c(rpc,name);stopifnot(name=='read');promises::promise_resolve(list(account=account$account,health=NULL,jobs=list(),resources=list()))}
restore<-function(handler,args,value,version='v1',at=stamp,expired=TRUE){
 descriptor<-fm_worker_descriptor(handler,args,ctx)
 wire<-charToRaw(fm_worker_json(fm_worker_encode(value)));compressed<-history_baseline_compress(wire)
 manifest<-list(account_id=auth[['userid']],resource_key=descriptor$resource_key,handler=handler,version=version,
  json_bytes=length(wire),parts=1L,sha256=paste0(openssl::sha256(compressed)),
  observed_at=rival_finance_stamp(at),checked_at=rival_finance_stamp(at),
  valid_until=rival_finance_stamp(if(expired)at+60 else Sys.time()+60),status='ok',completeness='unknown')
 item<-fm_worker_restore(list(key=descriptor$resource_key,manifest=manifest,chunks=list(jsonlite::base64_enc(compressed))),auth[['userid']])
 fm_worker_put(auth[['userid']],descriptor$resource_key,item)
 descriptor
}
base_args<-list(login=auth,championship_id='league',user_team_id='team')
lineup<-restore('get_lineup_from_team',base_args,list(pointSystem='fixture'))
catalog_args<-base_args[c('login','championship_id')]
catalog<-restore('get_championship_players',catalog_args,players,expired=FALSE)
real_async<-async_adapter_fetch;reads<-character();db_reads<-0L;db_error<-FALSE;held<-NULL
async_adapter_fetch<-function(fn,args=list(),context,...){
 reads<<-c(reads,fn)
 if(fn=='read_league_round_dataset'){
  db_reads<<-db_reads+1L
  if(db_error)return(promises::promise_reject(simpleError('Synthetic database interruption')))
  if(isTRUE(held))return(promises::promise(function(resolve,reject)held<<-list(resolve=resolve,reject=reject)))
  return(promises::promise_resolve(dataset))
 }
 stopifnot(fn!='get_player_recent_points')
 real_async(fn,args,context,...)
}
writes<-0L;persist_recent_points_snapshot<-function(...){writes<<-writes+1L;stop('GUI history writes forbidden')}
settle<-function(session)for(i in 1:20){later::run_now(0);session$flushReact()}
tick<-function(session,due=FALSE){
 entry<-shiny::isolate(session$userData$shared_round_points$entry())
 if(due&&!is.null(entry))entry$last_matrix_read<-as.numeric(Sys.time())-301
 account$status_revision(shiny::isolate(account$status_revision())+1L);settle(session)
}
server<-function(input,output,session){
 active<-reactiveVal(TRUE);refresh<-reactiveVal(0L)
 source<-recent_player_points_source(session,reactive(auth),reactive('league'),reactive('team'),reactive(players),active,refresh,
  required_rounds=function()5L)
 publications<-0L;observe({source$history();publications<<-publications+1L})
}
time<-system.time(shiny::testServer(server,{
 heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,0);settle(session)
 state<-source$status()
 stopifnot(heartbeat,state$total==527L,state$completed==432L,state$remaining==95L,state$pending==0L,
  state$failed==0L,!length(state$initial_errors),state$matrix_complete,db_reads==1L,
  !any(reads=='get_player_recent_points'),writes==0L,length(account$waiters)==0L,
  all(c(lineup$resource_key,catalog$resource_key)%in%fm_worker_live_keys(account)))
 cat('PASS432 saved histories remain loaded despite conflicting newer fitness;95 genuinely missing histories create zero individual reads or waiters\n')
 before<-list(reads=reads,rpc=rpc,publications=publications)
 for(i in 1:10)tick(session)
 stopifnot(identical(reads,before$reads),identical(rpc,before$rpc),publications==before$publications)
 cat('PASS ten idle worker updates perform no extra history reads or redraws inside the60-second boundary\n')
 dataset<<-make_dataset();tick(session,TRUE)
 stopifnot(source$status()$completed==527L,source$status()$remaining==0L,source$status()$pending==0L,
  db_reads==2L,length(account$waiters)==0L,writes==0L,
  all(add_recent_points_average(players,source$history(),5)$recent_points_avg==13/5))
 cat('PASS one asynchronous bulk reread discovers all95 new database histories, preserving zero and negative scores\n')
 # A fresh observation with identical values must not start source-oriented
 # probing or reset the progress to the14 players whose vector happens to agree.
 catalog_rows<-players;catalog_rows$average.fitness[1:14]<-'0,-2,6,1,8'
 restore('get_championship_players',catalog_args,catalog_rows,'v2',stamp+100,FALSE)
 version<-fm_worker_resource_revision(account,catalog$resource_key);version(isolate(version())+1L)
 settle(session)
 stopifnot(source$status()$completed==527L,source$status()$remaining==0L,db_reads==2L,
  !any(reads=='get_player_recent_points'),length(account$waiters)==0L)
 cat('PASS fresh catalogue with only14 matching vectors cannot reset527 acknowledged histories to14\n')
 # A delayed database read must retain the displayed matrix and event loop.
 held<<-TRUE;tick(session,TRUE)
 heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,0);settle(session)
 stopifnot(heartbeat,source$status()$completed==527L,nrow(source$history())==2635L,db_reads==3L)
 before<-publications;held$resolve(dataset);held<<-NULL;settle(session)
 stopifnot(publications==before)
 cat('PASS delayed and unchanged bulk reads retain history without blocking or republishing the table\n')
 corrected<-dataset
 corrected$points$points[corrected$points$player_id==players$id[1]&corrected$points$round==5]<--7
 corrected$points$observed_at[corrected$points$player_id==players$id[1]]<-rival_finance_stamp(stamp+120)
 dataset<<-corrected
 tick(session,TRUE)
 stopifnot(source$status()$completed==527L,
  add_recent_points_average(players,source$history(),5)$recent_points_avg[1]==-2/5)
 cat('PASS a real database score correction updates the average without reloading player histories\n')
 db_error<<-TRUE;tick(session,TRUE);db_error<<-FALSE
 stopifnot(source$status()$completed==527L,!source$status()$matrix_complete,
  !is.null(source$status()$initial_errors$matrix),nrow(source$history())==2635L)
 cat('PASS a database interruption keeps every saved fact and reports the failed refresh truthfully\n')
 full<-dataset;dataset<<-list(points=head(dataset$points,70),checks=head(dataset$checks,14),complete=FALSE)
 tick(session,TRUE)
 stopifnot(source$status()$completed==527L,!source$status()$matrix_complete,
  nrow(source$history())==2635L,!length(source$status()$initial_errors))
 cat('PASS an incomplete14-player reread cannot replace the527-player saved baseline\n')
 dataset<<-full;tick(session,TRUE)
 before<-db_reads;refresh(1L);settle(session)
 stopifnot(db_reads==before+1L,source$status()$completed==527L,!length(source$status()$initial_errors),
  !any(reads=='get_player_recent_points'),writes==0L)
 active(FALSE);settle(session);before<-db_reads;tick(session,TRUE)
 stopifnot(db_reads==before,!length(fm_worker_live_keys(account)))
 cat('PASS manual Refresh rereads the database once and hidden views stop reading; no source acquisition or writes\n')
}))
cat(sprintf('PERF527-player stored-history regression: CPU %.3fs / wall %.3fs; zero individual player reads, source HTTP or GUI writes\n',sum(time[c('user.self','sys.self')]),time[['elapsed']]))
# A new browser session starts with every persisted result, without replay.
clear_api_cache();account$descriptors<-list();account$waiters<-list();account$resource_revisions<-new.env(parent=emptyenv())
reads<-character();db_reads<-0L
shiny::testServer(server,{
 settle(session)
 stopifnot(source$status()$completed==527L,source$status()$remaining==0L,db_reads==1L,
  !any(reads=='get_player_recent_points'),writes==0L)
 cat('PASS a new session loads527 histories in one logical database read without per-player replay\n')
})
