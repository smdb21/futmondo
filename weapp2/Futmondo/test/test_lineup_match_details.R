#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
now <- base::Sys.time()
Sys.time <- function() now
auth <- c(userid='match-fixture',token='synthetic')
set_context <- function()publish_active_league_contexts(auth,list(championships=list(list(id='league',league='competition',ps='stats')),
  leagues=list(list(`_id`='competition',season='source-season'))))
set_context()
ctx <- resource_context(auth,'league','team')
stamp <- function(value)format(value,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')
make_game <- function(id,time,st)list(`_id`=id,h=list(`_id`='home',name='Home club'),
  a=list(`_id`='away',name='Away club'),info=list(date=stamp(time)),st=st)
records <- list(list(`_id`='started',matches=list(make_game('final',now-14400,'F'),make_game('live',now-1800,'live'))),
  list(`_id`='future',matches=list(make_game('future-id',now+36000,'scheduled'))))
rounds <- data.frame(round_id=c('started','future'),round_number=13:14,
  begin_process=stamp(c(now-14400,now+36000)),is_finished=FALSE)
games <- league_fixture_matches(records)
attr(rounds,'fixture_calendar') <- list(matches=games,scope=ctx[c('account_id','championship_id','competition_id','season')],
  status='ok',observed_at=now-60,valid_until=as.numeric(now)+300)
played <- function(side,i){
  list(p=list(`_id`=paste0(side,i),name=paste(side,i),role='delantero'),
    s=list(st=if(i<=11)'st'else'bc',pos=if(i<=11)i else 0L,role='centrocampista',po=list(
      list(mode='stats',r='delantero',p=20),list(mode='stats',r='centrocampista',p=i-2),
      list(mode='ff',r='centrocampista',p=3.5),list(mode='as',r='delantero',p=6),
      list(mode='as',r='centrocampista',p=2))))
}
raw_game <- function(id,started=FALSE){
  row <- games$rows[games$rows$match_id==id,,drop=FALSE]
  list(`_id`=id,r=list(`_id`=row$round_id),st=if(started)'live'else'F',
    info=list(min=if(started)31L else 90L),uAt=stamp(now-5),
    h=list(id=list(`_id`='home'),score=0,ps=lapply(1:12,function(i)played('Home',i))),
    a=list(id=list(`_id`='away'),score=1,ps=lapply(1:11,function(i)played('Away',i))))
}
fixture <- games$rows[1,,drop=FALSE]
raw <- raw_game('final')
detail <- lineup_match_parse(raw,fixture,now)
custom <- list(stats='ff',media=list(list(media='as',pct=20)))
stopifnot(detail$complete,unname(detail$score[1])==0,detail$minute==90L,detail$source_updated_at==stamp(now-5),
  lineup_match_status(detail,fixture)=='Finished',lineup_match_points(detail$home[[1]],'stats')==-1,
  lineup_match_points(detail$home[[2]],'stats')==0,lineup_match_points(detail$home[[1]],'custom',custom)==4.7,
  is.na(lineup_match_points(detail$home[[1]],'press')),length(detail$home)==12L)
wrong <- raw;wrong$h$id$`_id` <- 'wrong'
stopifnot(is.null(lineup_match_parse(wrong,fixture,now)))
partial <- raw;partial$a$ps[[1]]$s$po <- NULL
stopifnot(!lineup_match_parse(partial,fixture,now)$complete)
refs <- lineup_match_rounds(rounds,now)
stopifnot(refs$latest$id=='started',refs$next_round$id=='future',
  lineup_match_status(NULL,games$rows[3,,drop=FALSE],now)=='Scheduled')

read_ids <- character();saved_sheet <- NULL;bad_live <- FALSE
fm_worker_saved_resource <- function(handler,args,context){
  stopifnot(identical(args,list(championship_id='league')),identical(context$account_id,ctx$account_id),
    identical(context$scoring_version,ctx$scoring_version))
  switch(handler,get_finished_rounds=rounds,get_lineup_match_details=saved_sheet,stop('Unexpected stored read'))
}
mvp_api_answer <- function(login,url,query){
  stopifnot(identical(url,'https://api.futmondo.com/2/match/fetchmatch'),identical(names(query),'matchId'))
  id <- query$matchId
  read_ids <<- c(read_ids,id)
  if(bad_live&&id=='live')return(list(status='unavailable'))
  structure(list(status='ok',answer=list(match=raw_game(id,id=='live'))),observed_at=now-10)
}
get_details <- attr(get_lineup_match_details,'worker_original')
batch <- get_details(auth,'league')
stopifnot(identical(read_ids,c('live','final')),isTRUE(attr(batch,'complete')),
  lineup_match_status(batch$matches$live,games$rows[2,,drop=FALSE])=='Reported match minute · 31′')
saved_sheet <- fm_worker_decode(jsonlite::fromJSON(fm_worker_json(fm_worker_encode(batch)),simplifyVector=FALSE))
stopifnot(isTRUE(all.equal(saved_sheet,batch)))
now <- now+120
bad_live <- TRUE
retry <- get_details(auth,'league')
stopifnot(identical(read_ids,c('live','final','live')),retry$matches$live$refresh_incomplete,
  !isTRUE(attr(retry,'complete')),isTRUE(all.equal(retry$matches$live$observed_at,batch$matches$live$observed_at)),
  isTRUE(all.equal(retry$matches$final,batch$matches$final)))
saved_sheet <- retry
now <- now+6*3600;bad_live <- FALSE;set_context()
updated <- get_details(auth,'league')
stopifnot(setequal(tail(read_ids,2),c('live','final')),isTRUE(attr(updated,'complete')),
  !isTRUE(updated$matches$live$refresh_incomplete))
saved_sheet <- fm_worker_decode(jsonlite::fromJSON(fm_worker_json(fm_worker_encode(updated)),simplifyVector=FALSE))
now <- now-6*3600;set_context()
original_rounds <- rounds
backfill <- league_fixture_matches(list(list(`_id`='started',matches=lapply(1:8,
  function(i)make_game(paste0('backfill-',i),now-10000,'F')))))
attr(rounds,'fixture_calendar')$matches <- backfill
mvp_api_answer <- function(login,url,query){
  read_ids <<- c(read_ids,query$matchId)
  list(status='unavailable')
}
read_ids <- character()
saved_sheet <- NULL
saved_sheet <- get_details(auth,'league')
stopifnot(length(read_ids)==6L,length(saved_sheet$attempts)==6L)
now <- now+120
invisible(get_details(auth,'league'))
stopifnot(identical(read_ids[7:8],c('backfill-7','backfill-8')),length(read_ids)==12L)
rounds <- original_rounds
cat('PASS final and in-progress contract, official role/custom arithmetic, zero and missing points, replay and retry\n')

# Open the real Lineup modal with a delayed stored-data read.
fm_worker_configured <- function()TRUE
worker <- fm_worker_account_state(ctx$account_id)
worker$bootstrap_complete <- TRUE;worker$known <- TRUE
worker$account <- list(enabled=TRUE,auth_status='active')
descriptor <- fm_worker_descriptor('get_lineup_match_details',list(championship_id='league'),ctx)
wire <- charToRaw(fm_worker_json(fm_worker_encode(batch)))
compressed <- history_baseline_compress(wire)
record <- list(key=descriptor$resource_key,manifest=list(account_id=ctx$account_id,resource_key=descriptor$resource_key,
  handler=descriptor$handler,context=descriptor$context,version='match-fixture',
  json_bytes=length(wire),parts=1L,sha256=paste0(openssl::sha256(compressed)),
  observed_at=stamp(now-10),checked_at=stamp(now-10),valid_until=stamp(now+100),
  status='ok',completeness='complete'),chunks=list(jsonlite::base64_enc(compressed)))
source_requests <- 0L;db_match_requests <- 0L;held <- NULL
fm_worker_rpc <- function(name,payload,owner=''){
  stopifnot(name=='read')
  if(!descriptor$resource_key%in%names(payload$p_versions))return(promises::promise_resolve(list(account=worker$account)))
  db_match_requests <<- db_match_requests+1L
  promises::promise(function(resolve,reject)held<<-resolve)
}
fm_async_http <- fm_async_read_http <- futmondo_post <- mvp_api_answer <- function(...){
  source_requests <<- source_requests+1L
  stop('Unexpected network acquisition')
}
recent_player_points_source <- function(...)list(history=shiny::reactive(data.frame()),
  status=shiny::reactive(list(pending=0L,remaining=0L,saving=0L,failed=0L)))
lineup_snapshot_store <- function(context,target_round,payload=NULL,owner='')
  promises::promise_resolve(if(is.null(payload))NULL else TRUE)
lineup_fetch_bundle <- function(...)promises::promise_resolve(list(version=1L,roster=list(),official=lineup_empty(),
  rules=list(allowed='4-4-2',multiposition=TRUE,bench_enabled=TRUE,captain_enabled=FALSE),
  source=list(wire=list(strategy='4-4-2',players=list()),locked=list()),
  observed_at=stamp(now),expires_at=stamp(now+60)))
settle <- function(session)for(i in 1:25){later::run_now(0);session$flushReact()}
shiny::testServer(lineup_Server,args=list(is_module_active=function()TRUE,login_token=function()auth,
  championship_id=function()'league',user_team_id=function()'team',round_catalog=function()rounds,
  clause_transfers_source=function()list(outgoing_status='ok',outgoing_records=outgoing_clause_empty())),{
  settle(session);before<-draft()
  stopifnot(grepl('Latest started round',output$fixtures$html),db_match_requests==0L)
  session$setInputs(match_open=list(id='future-id',round='future'));settle(session)
  stopifnot(grepl('Scheduled',output$match_header$html),db_match_requests==0L)
  session$setInputs(match_open=list(id='foreign',round='started'));settle(session)
  stopifnot(match_pick()$row$match_id=='future-id')
  session$setInputs(match_open=list(id='final',round='started'));settle(session)
  stopifnot(is.function(held),is.null(match_detail()$detail),db_match_requests==1L)
  held(list(account=worker$account,resources=list(record),jobs=list(),health=NULL));settle(session)
  stopifnot(grepl('Finished',output$match_header$html),grepl('0 – 1',output$match_header$html),
    grepl('Home 1',output$match_players$html),grepl('Away 1',output$match_players$html),
    grepl('>-1<',output$match_players$html),identical(draft(),before))
  session$setInputs(match_close=1);settle(session)
  session$setInputs(match_open=list(id='live',round='started'));settle(session)
  stopifnot(grepl('31′',output$match_header$html),db_match_requests==1L,source_requests==0L)
})
cat('PASS match cards and modal restore durable observations, reuse across cards and perform no source calls\n')

# The split fixture list and editable XI advance independently in the module.
base_time <- now
split_days <- base_time+c(1,3,3.1,6,66)*86400
split_rounds <- data.frame(round_id=c('r14','r15','r23'),round_number=c(14,15,23),
  begin_process=stamp(split_days[c(1,4,2)]),is_finished=FALSE)
split_games <- league_fixture_matches(list(
  list(`_id`='r23',matches=list(make_game('feb',split_days[5],'scheduled'),
    make_game('dec-a',split_days[2],'scheduled'),make_game('dec-b',split_days[3],'scheduled'))),
  list(`_id`='r14',matches=list(make_game('regular-14',split_days[1],'scheduled'))),
  list(`_id`='r15',matches=list(make_game('regular-15',split_days[4],'scheduled')))))
attr(split_rounds,'fixture_calendar') <- list(matches=split_games,status='ok',scope=ctx,
  observed_at=now-60,valid_until=as.numeric(now)+3600)
attr(split_rounds,'fixture_windows') <- lineup_fixture_windows(split_rounds,split_games)
attr(split_rounds,'lineup_active') <- list(target=list(id='r14',number=14,starts_at=split_days[1]))
split_state <- shiny::reactiveVal(split_rounds)
factual_read_adapter <- function(...)function(...)list(data=NULL)
shiny::testServer(lineup_Server,args=list(is_module_active=function()TRUE,login_token=function()auth,
  championship_id=function()'league',user_team_id=function()'team',round_catalog=split_state,
  clause_transfers_source=function()list(outgoing_status='ok',outgoing_records=outgoing_clause_empty())),{
  settle(session)
  html <- output$fixtures$html
  stopifnot(target()$id=='r14',grepl('Upcoming split round',html),grepl('Select a full XI',html),
    grepl('data-match-id="dec-a"',html,fixed=TRUE),grepl('data-match-id="dec-b"',html,fixed=TRUE),
    !grepl('data-match-id="feb"',html,fixed=TRUE))
  updated <- split_state()
  updated$begin_process[updated$round_id=='r14'] <- stamp(now-86400)
  attr(updated,'fixture_calendar')$matches$rows$kickoff[split_games$rows$round_id=='r14'] <- stamp(now-86400)
  attr(updated,'fixture_windows') <- lineup_fixture_windows(updated,attr(updated,'fixture_calendar')$matches)
  attr(updated,'lineup_active') <- list(target=list(id='r23',number=23,starts_at=split_days[2]))
  split_state(updated);settle(session)
  stopifnot(grepl('Next scheduled round 23',output$heading$html),target()$id=='r23',
    !isTRUE(target()$reopened),source_requests==0L)
  event_league <- '504e4f584d8bec9a67000079'
  event_round <- '6a4af7f2ae633549bd0f0471'
  # The replay fixture above already observed membership six hours ahead.
  now <<- base_time+7*3600
  publish_active_league_contexts(auth,list(championships=list(list(id='league',league=event_league,ps='stats')),
    leagues=list(list(`_id`=event_league,season='2025-2026'))))
  updated$round_id[3] <- event_round
  updated$first_kickoff <- updated$begin_process
  updated$first_kickoff[3] <- stamp(now-86400)
  attr(updated,'lineup_active') <- list(target=list(id=event_round,number=23,starts_at=split_days[5]))
  split_state(updated);settle(session)
  heading <- output$heading$html
  stopifnot(isTRUE(target()$reopened),grepl('10:00 Madrid time',heading),grepl('earns 0 points',heading),
    grepl('retain their points',heading),source_requests==0L)
})
cat('PASS Lineup shows two advanced fixtures during the preceding round and changes the full-XI editor only when Futmondo activates their round\n')
