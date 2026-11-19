#!/usr/bin/env Rscript
# Calendar logos can be absent even when complete club crests are already saved.
# Synthetic account/HTTP only; exercise the actual durable codec and UI adapter.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
now<-Sys.time();auth<-c(userid='crest-fixture',token='synthetic')
publish_active_league_contexts(auth,list(championships=list(list(id='league',league='competition',ps='press')),
 leagues=list(list(`_id`='competition',season='source-season'))))
ctx<-resource_context(auth,'league','team')
descriptor<-fm_worker_descriptor('get_real_clubs',list(championship_id='league'),ctx)
scope<-ctx[c('account_id','championship_id','competition_id','season')]
games<-league_fixture_matches(list(list(`_id`='next',matches=list(
 list(`_id`='game',h=list(`_id`='home',name='Calendar home name'),
  a=list(`_id`='away',name='Calendar away name'),info=list(date=format(now+86400,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')))))))
rounds<-data.frame(round_id='next',round_number=13,begin_process=format(now+86400,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),is_finished=FALSE)
attr(rounds,'fixture_calendar')<-list(matches=games,scope=scope,status='ok',observed_at=now-120,valid_until=as.numeric(now)+60)
clubs<-data.frame(teamId=c('away','home','foreign'),team=c('Different away name','Different home name','Calendar home name'),
 logo=c('observed-away.png','observed-home.png','wrong-name-match.png'))
attr(clubs,'observed_at')<-now-180;attr(clubs,'valid_until')<-as.numeric(now)-30
snapshot<-function(rows){
 wire<-charToRaw(fm_worker_json(fm_worker_encode(rows)));compressed<-history_baseline_compress(wire)
 list(key=descriptor$resource_key,manifest=list(account_id=ctx$account_id,resource_key=descriptor$resource_key,
  handler=descriptor$handler,context=descriptor$context,version='fixture-version',json_bytes=length(wire),parts=1L,
  sha256=paste0(openssl::sha256(compressed)),observed_at=rival_finance_stamp(attr(rows,'observed_at')),
  checked_at=rival_finance_stamp(now-170),valid_until=rival_finance_stamp(as.POSIXct(attr(rows,'valid_until'),origin='1970-01-01')),
  status='ok',completeness='unknown'),chunks=list(jsonlite::base64_enc(compressed)))
}
saved<-snapshot(clubs)
restored<-fm_worker_restore(saved,ctx$account_id)
state<-resource_state(restored$data);state$context<-descriptor$context
view<-function(data=rounds,stored=state)lineup_fixture_view(data,lineup_target(data,now),scope,now,stored)
result<-view()
stopifnot(result$complete,result$rows$home_logo=='observed-home.png',result$rows$away_logo=='observed-away.png',
 result$rows$home_name=='Calendar home name',result$crest_stale,!result$stale,
 identical(as.numeric(result$observed_at),as.numeric(now-120)),abs(as.numeric(result$crest_observed_at)-as.numeric(now-180))<.001,
 !nzchar(attr(rounds,'fixture_calendar')$matches$rows$home_logo))
cat('PASS persisted catalogue resolves omitted match logos by club ID and keeps independent original clocks/names\n')
for(field in names(scope)){
 foreign<-state;foreign$context[[field]]<-'foreign'
 stopifnot(!nzchar(view(stored=foreign)$rows$home_logo))
}
for(kind in c('missing_time','future_time','missing_deadline','partial','failed','duplicate','name_only','wrong_type')){
 bad<-state
 if(kind=='missing_time')attr(bad$data,'observed_at')<-NULL
 if(kind=='future_time')attr(bad$data,'observed_at')<-now+60
 if(kind=='missing_deadline')attr(bad$data,'valid_until')<-NULL
 if(kind=='partial')attr(bad$data,'worker_completeness')<-'partial'
 if(kind=='failed')attr(bad$data,'worker_acquisition_status')<-'unavailable'
 if(kind=='duplicate')bad$data<-rbind(bad$data,bad$data[bad$data$teamId=='home',])
 if(kind=='name_only')bad$data$teamId[bad$data$teamId=='home']<-'unrelated'
 if(kind=='wrong_type')bad$data$teamId<-seq_len(nrow(bad$data))
 stopifnot(!nzchar(view(stored=bad)$rows$home_logo),nrow(view(stored=bad)$rows)==1L)
}
legacy<-rounds;attr(legacy,'fixture_calendar')$matches$version<-1L
attr(legacy,'fixture_calendar')$matches$rows[c('home_logo','away_logo')]<-NULL
stopifnot(view(legacy)$rows$home_logo=='observed-home.png')
direct<-rounds;attr(direct,'fixture_calendar')$matches$rows$home_logo<-'direct-home.png'
stopifnot(view(direct)$rows$home_logo=='direct-home.png',view(direct)$rows$away_logo=='observed-away.png')
attr(direct,'fixture_calendar')$matches$rows$away_logo<-'direct-away.png'
stopifnot(lineup_fixture_view(direct,lineup_target(direct,now),scope,now,stop('Unneeded club read'))$rows$home_logo=='direct-home.png')
cat('PASS scope, failed/partial/future observations, ambiguous IDs, legacy calendars and direct-logo precedence\n')

fm_worker_configured<-function()TRUE
worker<-fm_worker_account_state(ctx$account_id);worker$bootstrap_complete<-TRUE
worker$known<-TRUE;worker$account<-list(enabled=TRUE,auth_status='active')
db_reads<-0L;source_reads<-0L;held<-NULL
fm_worker_rpc<-function(name,payload,owner=''){
 stopifnot(name=='read');db_reads<<-db_reads+1L
 promises::promise(function(resolve,reject)held<<-resolve)
}
fm_async_http<-fm_async_read_http<-futmondo_post<-function(...){source_reads<<-source_reads+1L;stop('Unexpected source/DB HTTP')}
recent_player_points_source<-function(...)list(history=shiny::reactive(data.frame()),
 status=shiny::reactive(list(pending=0L,remaining=0L,saving=0L,failed=0L)))
lineup_snapshot_store<-function(context,target_round,payload=NULL,owner='')promises::promise_resolve(if(is.null(payload))NULL else TRUE)
lineup_fetch_bundle<-function(...)promises::promise_resolve(list(version=1L,roster=list(),official=lineup_empty(),
 rules=list(allowed='4-4-2',multiposition=TRUE,bench_enabled=TRUE,captain_enabled=FALSE),
 source=list(wire=list(strategy='4-4-2',players=list()),locked=list()),
 observed_at=rival_finance_stamp(now),expires_at=rival_finance_stamp(now+60)))
settle<-function(session)for(i in 1:30){later::run_now(0);session$flushReact()}
args<-list(is_module_active=function()TRUE,login_token=function()auth,championship_id=function()'league',
 user_team_id=function()'team',round_catalog=function()rounds,
 clause_transfers_source=function()list(outgoing_status='ok',outgoing_records=outgoing_clause_empty()))
for(run in 1:2){
 # A new process/session restores the same durable bytes, without a source read.
 rm(list=ls(.fm_worker$values),envir=.fm_worker$values);worker$requested_keys<-character()
 clear_api_cache();held<-NULL
 shiny::testServer(lineup_Server,args=args,{
  settle(session);before<-draft()
  # MockShinySession$getOutput waits for all outstanding promises. Inspect the
  # still-loading resource here; the browser test checks its visible match cards.
  stopifnot(is.function(held),is.null(fm_worker_get(ctx$account_id,descriptor$resource_key)))
  started<-proc.time()[['elapsed']]
  held(list(account=worker$account,resources=list(saved),jobs=list(),health=NULL));settle(session)
  html<-output$fixtures$html;nodes<-xml2::read_html(html)
  images<-xml2::xml_find_all(nodes,'.//img[@class="fm-lineup-fixture-crest"]')
  stopifnot(length(images)==2L,identical(xml2::xml_attr(images,'src'),paste0(TEAM_LOGO_URL,c('observed-home.png','observed-away.png'))),
   all(xml2::xml_attr(images,'loading')=='lazy'),identical(draft(),before),source_reads==0L,db_reads==run,
   grepl('Club crests updated',html,fixed=TRUE))
  for(i in 1:5)session$flushReact()
  stopifnot(db_reads==run,source_reads==0L,identical(output$fixtures$html,html))
  cat(sprintf('PASS session %d: delayed DB catalogue renders both crests; unchanged draft, no source calls; publication %.3fs\n',
   run,proc.time()[['elapsed']]-started))
 })
}
cat('LINEUP FIXTURE CRESTS: all checks passed\n')
