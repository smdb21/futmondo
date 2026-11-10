#!/usr/bin/env Rscript
# Synthetic-only regression for exact saved-data scope across competitions.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(name,code){force(code);checks<<-checks+1L;cat('PASS',name,'\n')}
settle<-function(p){
 value<-NULL;failure<-NULL;done<-FALSE
 promises::then(p,function(x){value<<-x;done<<-TRUE;NULL},function(error){failure<<-error;done<<-TRUE;NULL})
 for(i in 1:50){later::run_now(0);if(done)break}
 stopifnot(done);list(value=value,error=failure)
}
auth<-c(userid='snapshot-account',token='synthetic-token')
ctx<-resource_context(auth,'snapshot-league','snapshot-team',season='2026-2027',scoring_version='scoring-A',competition_id='competition-A')
now<-Sys.time()-5
stamp<-function(value)format(value,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC')
snapshot<-list(version=2L,roster=list(list(id='p',name='Player',points=-2,club='Original Club',logo='original.png')),
 official=lineup_empty(),rules=list(allowed='4-4-2',multiposition=FALSE,bench_enabled=FALSE,captain_enabled=FALSE),
 source=list(wire=list(strategy='4-4-2',players=list()),locked=list()),competition_id='competition-A',season='2026-2027',scoring_version='scoring-A',
 observed_at=stamp(now),expires_at=stamp(now+30))
row<-list(account_id=ctx$account_id,championship_id=ctx$championship_id,user_team_id=ctx$user_team_id,target_round='round-1',
 competition_id=ctx$competition_id,season=ctx$season,scoring_version=ctx$scoring_version,payload=snapshot,
 observed_at=snapshot$observed_at,expires_at=snapshot$expires_at,complete=TRUE)
get_sb_url<-function()'https://fixture.invalid';get_sb_key<-function()'synthetic-database-secret'
reads<-character();writes<-list()
fm_async_read_http<-function(url,...){reads<<-c(reads,url);promises::promise_resolve(list(content=charToRaw(jsonlite::toJSON(list(row),auto_unbox=TRUE,null='null'))))}
fm_async_http<-function(url,method,body,...){writes[[length(writes)+1L]]<<-list(url=url,body=body);promises::promise_resolve(list(status_code=201L))}
check('known lineup context hydrates with exact competition/season/scoring filters',{
 result<-settle(lineup_snapshot_store(ctx,'round-1'))
 stopifnot(is.null(result$error),identical(result$value$competition_id,'competition-A'),
  identical(result$value$roster[[1]]$club,'Original Club'),result$value$roster[[1]]$points==-2,
  grepl('competition_id=eq.competition-A',tail(reads,1),fixed=TRUE),grepl('season=eq.2026-2027',tail(reads,1),fixed=TRUE),
  grepl('scoring_version=eq.scoring-A',tail(reads,1),fixed=TRUE),!length(writes))
})
check('unknown context never hydrates an arbitrary known lineup',{
 unknown<-ctx;unknown$competition_id<-unknown$season<-unknown$scoring_version<-'unknown'
 result<-settle(lineup_snapshot_store(unknown,'round-1'))
 stopifnot(!is.null(result$error),is.null(result$value),grepl('competition_id=eq.unknown',tail(reads,1),fixed=TRUE),
  grepl('season=eq.unknown',tail(reads,1),fixed=TRUE),grepl('scoring_version=eq.unknown',tail(reads,1),fixed=TRUE))
})
check('mismatched or legacy lineup metadata cannot hydrate or overwrite a known context',{
 for(field in c('account_id','championship_id','user_team_id','competition_id','season','scoring_version')){
  wrong<-ctx;wrong[[field]]<-'other'
  stopifnot(!is.null(settle(lineup_snapshot_store(wrong,'round-1'))$error))
 }
 old<-row;row$payload$version<-1L
 stopifnot(!is.null(settle(lineup_snapshot_store(ctx,'round-1'))$error));row<-old
 wrong_payload<-snapshot;wrong_payload$competition_id<-'other'
 stopifnot(!is.null(settle(lineup_snapshot_store(ctx,'round-1',wrong_payload))$error),!length(writes))
 result<-settle(lineup_snapshot_store(ctx,'round-1',snapshot));stopifnot(isTRUE(result$value),length(writes)==1L)
 stored<-jsonlite::fromJSON(writes[[1]]$body,simplifyVector=FALSE)
 stopifnot(identical(stored$competition_id,ctx$competition_id),!grepl('synthetic-token|synthetic-database-secret',writes[[1]]$body),
  grepl('target_round%2Ccompetition_id%2Cseason%2Cscoring_version',writes[[1]]$url,fixed=TRUE)||
  grepl('target_round,competition_id,season,scoring_version',writes[[1]]$url,fixed=TRUE))
})
check('lineup scoring fingerprints ignore equivalent object order and reject foreign source context',{
 a<-list(stats='ff',media=list(list(media='provider',pct=100)))
 b<-list(media=list(list(pct=100,media='provider')),stats='ff')
 answers<-list(lineup=list(strategy='4-4-2',players=list(),bench=list(players=list()),pointSystem='custom',custom=a),
  roster=list(),info=list(league=list(`_id`='competition-A',season='2026-2027'),configuration=list()),
  strategies=list(strategies=list(list(st='4-4-2',pro=FALSE))))
 local_ctx<-ctx;local_ctx$scoring_version<-'unknown'
 first<-lineup_normalize_bundle(answers,local_ctx,NULL);answers$lineup$custom<-b
 second<-lineup_normalize_bundle(answers,local_ctx,NULL)
 stopifnot(identical(first$scoring_version,second$scoring_version),identical(first$competition_id,'competition-A'),first$version==2L)
 answers$info$league$`_id`<-'competition-B'
 stopifnot(inherits(try(lineup_normalize_bundle(answers,local_ctx,NULL),silent=TRUE),'try-error'))
})
check('late direct lineup bundles cannot publish scoring metadata',{
 original_read<-lineup_api_read;original_remember<-remember_league_scoring;original_persist<-persist_league_context
 answers<-list(lineup=list(strategy='4-4-2',players=list(),bench=list(players=list()),pointSystem='press'),
  roster=list(),info=list(league=list(`_id`='competition-A',season='2026-2027'),configuration=list()),
  strategies=list(strategies=list(list(st='4-4-2',pro=FALSE))))
 api_reads<-0L;metadata_published<-0L;metadata_saved<-0L;valid_generation<-TRUE
 lineup_api_read<-function(route,...){api_reads<<-api_reads+1L;promises::promise_resolve(answers[[switch(route,
  '/1/userteam/lineup'='lineup','/1/userteam/roster'='roster','/1/userteam/information'='info','/5/strategy/availables'='strategies')]])}
 remember_league_scoring<-function(...){metadata_published<<-metadata_published+1L;list(complete=TRUE)}
 persist_league_context<-function(...){metadata_saved<<-metadata_saved+1L;TRUE}
 pending_bundle<-lineup_fetch_bundle(auth,ctx,NULL,'fixture',is_current=function()valid_generation)
 valid_generation<-FALSE;result<-settle(pending_bundle)
 stopifnot(!is.null(result$error),api_reads==4L,metadata_published==0L,metadata_saved==0L)
 valid_generation<-TRUE;result<-settle(lineup_fetch_bundle(auth,ctx,NULL,'fixture',is_current=function()valid_generation))
 stopifnot(is.null(result$error),api_reads==8L,metadata_published==1L,metadata_saved==1L)
 lineup_api_read<-original_read;remember_league_scoring<-original_remember;persist_league_context<-original_persist
})
resource<-'pressroom_snapshot-league'
feed<-data.frame(id='transfer-1',created=stamp(now-10),player_id='p',buyer_team_id='',seller_team_id='snapshot-team',price=10)
attr(feed,'fetch_status')<-'ok';attr(feed,'full_history_at')<-now;attr(feed,'source_latest_at')<-now-10;attr(feed,'observed_at')<-now
queued<-list();database<-NULL
supabase_get<-function(table_name,query_params){reads<<-c(reads,paste(names(query_params),unlist(query_params),collapse='|'));database}
defer_persistence<-function(fn,args,context,on_complete=NULL,...){queued[[length(queued)+1L]]<<-list(fn=fn,args=args,callback=on_complete);TRUE}
check('complete baselines persist competition and format and remain retryable until acknowledged',{
 rm(list=ls(.history_baseline_ack),envir=.history_baseline_ack)
 stopifnot(persist_completed_history_baseline(resource,feed,ctx),length(queued)==1L,
  queued[[1]]$args[[3]]=='account_id,championship_id,competition_id,season,scoring_version,resource')
 wire<-jsonlite::fromJSON(rawToChar(memDecompress(openssl::base64_decode(queued[[1]]$args[[2]]$payload),'gzip')),simplifyVector=FALSE)
 stopifnot(wire$format==2L,wire$scope$competition_id=='competition-A')
 queued[[1]]$callback(FALSE);stopifnot(persist_completed_history_baseline(resource,feed,ctx),length(queued)==2L)
 queued[[2]]$callback(TRUE);stopifnot(persist_completed_history_baseline(resource,feed,ctx),length(queued)==2L)
 database<-as.data.frame(queued[[2]]$args[[2]],stringsAsFactors=FALSE)
 restored<-read_persisted_history_baseline(resource,ctx)
 stopifnot(is.data.frame(restored),restored$price==10,restored$buyer_team_id=='')
})
check('another competition or old baseline envelope cannot supply completeness',{
 other<-ctx;other$competition_id<-'competition-B'
 stopifnot(is.null(read_persisted_history_baseline(resource,other)))
 old<-database
 bytes<-history_baseline_decompress(openssl::base64_decode(database$payload),database$json_bytes)
 wire<-jsonlite::fromJSON(rawToChar(bytes),simplifyVector=FALSE);wire$format<-1L
 raw<-charToRaw(as.character(jsonlite::toJSON(wire,auto_unbox=TRUE,null='null',digits=NA)));compressed<-history_baseline_compress(raw)
 database$payload<-openssl::base64_encode(compressed);database$payload_sha256<-history_baseline_hash(compressed);database$json_bytes<-length(raw)
 stopifnot(is.null(read_persisted_history_baseline(resource,ctx)));database<-old
})
check('finance, clause and historical lineup snapshot identities include competition, season and scoring',{
 for(field in c('competition_id','season','scoring_version')){
  other<-ctx;other[[field]]<-'other'
  stopifnot(rival_snapshot_id(rival_snapshot_scope(ctx))!=rival_snapshot_id(rival_snapshot_scope(other)),
   round_lineup_snapshot_id(ctx,'round-1')!=round_lineup_snapshot_id(other,'round-1'),
   roster_offer_snapshot_id(roster_offer_scope(ctx))!=roster_offer_snapshot_id(roster_offer_scope(other)))
  ctx$player_id<-'p';other$player_id<-'p'
  stopifnot(clause_transfer_snapshot_id(clause_transfer_scope(ctx))!=clause_transfer_snapshot_id(clause_transfer_scope(other)))
 }
})
check('lineup generation and retained drafts change with the resolved competition context',{
 metadata<-shiny::reactiveVal(list(competition='competition-A',season='2026-2027',scoring='scoring-A'))
 original_context<-resource_context;original_revision<-league_context_revision
 resource_context<-function(...){value<-original_context(...);value$competition_id<-metadata()$competition;value$season<-metadata()$season;value$scoring_version<-metadata()$scoring;value}
 league_context_revision<-function()metadata()
 lineup_snapshot_store<-function(...)promises::promise_resolve(NULL)
 pending<-list();bundle_reads<-0L
 lineup_fetch_bundle<-function(...){bundle_reads<<-bundle_reads+1L;promises::promise(function(resolve,reject)pending[[bundle_reads]]<<-resolve)}
 rounds<-data.frame(round_id='round-1',round_number=1,begin_process=stamp(Sys.time()+86400),is_finished=FALSE)
 shiny::testServer(lineup_Server,args=list(is_module_active=function()TRUE,login_token=function()auth,
  championship_id=function()'snapshot-league',user_team_id=function()'snapshot-team',round_catalog=function()rounds),{
  flush<-function(){for(i in 1:20){later::run_now(0);session$flushReact()}}
  flush();stopifnot(bundle_reads==1L,is.null(draft()));obsolete<-pending[[1]]
  metadata(list(competition='competition-B',season='2026-2027',scoring='scoring-A'));flush()
  stopifnot(bundle_reads==2L,is.null(draft()))
  obsolete(snapshot);flush();stopifnot(is.null(draft()))
  current_snapshot<-snapshot;current_snapshot$competition_id<-'competition-B';pending[[2]](current_snapshot);flush()
  stopifnot(identical(source()$competition_id,'competition-B'),!is.null(draft()))
 })
 resource_context<-original_context;league_context_revision<-original_revision
})
cat(sprintf('COMPETITION SNAPSHOTS: %d groups passed / 0 failed\n',checks))
