#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
n<-0L
check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
player<-function(id,position,changed=FALSE,points=5)list(id=id,name=paste('Player',id),position=position,
  photo=paste0(id,'.png'),team=list(id='club',logo='club.png'),points=points,changed=changed)
raw<-list(strategy='3-6-1',status='done',negative=FALSE,
  players=list(player('keeper',10),player('incoming',4,TRUE),player('striker',0,points=0),player('lastdef',9,points=-2)),
  bench=list(players=list(player('fourth',3),player('outgoing',1,TRUE),player('first',0,points=NA_real_))))
auth<-c(userid='u',token='synthetic-private-token');ctx<-resource_context(auth,'league','a',season=league_season_context('league'))
check('official positions retain eleven places, 3-6-1 rows, holes and four ordered bench positions',{
  got<-normalize_round_lineup(raw)
  stopifnot(identical(got$rows,c(1L,6L,3L,1L)),length(got$players)==11L,length(got$bench)==4L,
    got$players[[1]]$id=='striker',is.null(got$players[[2]]),got$players[[5]]$id=='incoming',got$players[[11]]$id=='keeper',
    got$bench[[1]]$id=='first',got$bench[[2]]$id=='outgoing',is.null(got$bench[[3]]),got$bench[[4]]$id=='fourth',
    got$players[[1]]$points==0,got$players[[10]]$points==-2)
  html<-htmltools::renderTags(round_lineup_pitch_ui(raw))$html
  stopifnot(grepl('fm-lineup-in',html),grepl('fm-lineup-out',html),grepl('Empty position',html),
    grepl('https://static01.mondocore.com/futmondo/img/faces/64/incoming.png',html,fixed=TRUE),
    grepl('https://static02.mondocore.com/futmondo/img/teams/48/club.png',html,fixed=TRUE),
    got$players[[5]]$changed,got$bench[[2]]$changed)
})
check('empty lineups and zero-sized rows do not invent players; malformed slots fail',{
  blank<-list(strategy='5-5-0',players=list(),bench=list(players=list()))
  got<-normalize_round_lineup(blank)
  stopifnot(length(got$players)==11L,all(vapply(got$players,is.null,logical(1))),
    length(gregexpr('data-slot=',htmltools::renderTags(round_lineup_pitch_ui(blank))$html,fixed=TRUE)[[1]])==15L)
  for(bad in list(list(strategy='4-4-2'),list(strategy='broken',players=list()),
    list(strategy='4-4-2',players=list(player('x',11))),list(strategy='4-4-2',players=list(player('x',0),player('y',0)))))
    stopifnot(inherits(try(normalize_round_lineup(bad),silent=TRUE),'try-error'))
})
check('range endpoints alone define single-round selection and dropdown is removed',{
  stopifnot(classification_single_round(c(9,9))=='9',classification_single_round(c(1,9))=='all',
    classification_single_round(NULL)=='all',classification_single_round(c(NA,NA))=='all',
    !grepl('single_round_select|Inspect one round',htmltools::renderTags(classification_UI('classification'))$html))
})
check('rival evidence retains strategy, exact positions and changes alongside ownership IDs',{
  result<-rival_lineup_evidence(list(answer=raw,team_id='a',round_id='opaque-round',signature='revision'),'u','league',ctx$season)
  stopifnot(length(result)==1L,identical(result[[1]]$payload$raw_lineup,raw),
    'incoming'%in%unlist(result[[1]]$payload$players),'outgoing'%in%unlist(result[[1]]$payload$bench))
})
original_post<-futmondo_post
saved<-list();calls<-0L;db_reads<-0L
supabase_get<-function(table,query){
  db_reads<<-db_reads+1L
  stopifnot(table=='rival_finance_evidence_latest',query$account_id=='eq.u',query$championship_id=='eq.league',
    query$season==paste0('eq.',ctx$season),query$source_id==paste0('eq.',round_lineup_snapshot_id(ctx,'opaque-round')),query$limit==1L)
  if(!length(saved))return(data.frame())
  jsonlite::fromJSON(as.character(jsonlite::toJSON(saved,auto_unbox=TRUE,null='null',na='null',digits=NA)))
}
defer_persistence<-function(fn,args,context,...){
  stopifnot(fn=='supabase_post_direct',args[[1]]=='rival_finance_evidence');saved<<-args[[2]];TRUE
}
futmondo_post<-function(url,...){
  request<-jsonlite::fromJSON(list(...)$body,simplifyVector=FALSE);calls<<-calls+1L
  stopifnot(url=='https://api.futmondo.com/1/userteam/roundlineup',
    identical(request$query,list(championshipId='league',userteamId='a',round='opaque-round')))
  structure(list(status_code=200L,headers=list('Content-Type'='application/json; charset=utf-8'),
    content=charToRaw(jsonlite::toJSON(list(code=API_CODE_OK,answer=raw),auto_unbox=TRUE,null='null',na='null')),url=url),class='response')
}
check('first retrieval uses opaque round ID and persists full success without authentication',{
  clear_api_cache();got<-get_round_lineup_snapshot(auth,'league','a','opaque-round')
  stopifnot(calls==1L,db_reads==1L,length(saved)==1L,saved[[1]]$payload$completeness=='complete',
    !grepl('synthetic-private-token',jsonlite::toJSON(saved,auto_unbox=TRUE)),normalize_round_lineup(got$answer)$players[[5]]$id=='incoming')
})
check('memory reuse and persisted JSON reuse avoid repeat API calls after memory clears',{
  got<-get_round_lineup_snapshot(auth,'league','a','opaque-round');stopifnot(calls==1L,db_reads==1L)
  clear_api_cache();got<-get_round_lineup_snapshot(auth,'league','a','opaque-round')
  stopifnot(calls==1L,db_reads==2L,isTRUE(got$from_saved),normalize_round_lineup(got$answer)$bench[[2]]$id=='outgoing',
    normalize_round_lineup(got$answer)$players[[5]]$team$logo=='club.png')
})
check('wrong scope and expired snapshots refetch; failures preserve truthful stale data',{
  original<-saved;saved[[1]]$payload$scope$user_team_id<-'other'
  clear_api_cache();got<-get_round_lineup_snapshot(auth,'league','a','opaque-round');stopifnot(calls==2L,!isTRUE(got$from_saved))
  saved<-original;old<-Sys.time()-600
  saved[[1]]$observed_at<-saved[[1]]$payload$source_observed_at<-rival_finance_stamp(old)
  saved[[1]]$payload$valid_until<-rival_finance_stamp(old+300)
  clear_api_cache();got<-get_round_lineup_snapshot(auth,'league','a','opaque-round');stopifnot(calls==3L)
  saved[[1]]$observed_at<-saved[[1]]$payload$source_observed_at<-rival_finance_stamp(old)
  saved[[1]]$payload$valid_until<-rival_finance_stamp(old+300)
  futmondo_post<-function(...)stop('Connection failed')
  clear_api_cache();got<-get_round_lineup_snapshot(auth,'league','a','opaque-round')
  stopifnot(identical(attr(got,'fetch_status'),'stale'),!is.null(attr(got,'fetch_error')),normalize_round_lineup(got$answer)$players[[11]]$id=='keeper')
})
check('real async adapter replays one database read then one API read and persists only the successful result',{
  clear_api_cache();saved<-list();transport<-character();futmondo_post<-original_post
  get_sb_url<-function()'https://fixture.invalid';get_sb_key<-function()'synthetic-db-key'
  fm_async_http<-function(url,method='GET',body=NULL,headers=character(),...){
    transport<<-c(transport,method)
    response<-if(method=='GET')'[]'else as.character(jsonlite::toJSON(list(code=API_CODE_OK,answer=raw),auto_unbox=TRUE,null='null',na='null'))
    promises::promise_resolve(list(status_code=200L,content=charToRaw(response),times=list(total=0)))
  }
  done<-FALSE;answer<-NULL;failure<-NULL
  promises::then(async_adapter_fetch('get_round_lineup_snapshot',list(auth,'league','a','opaque-round'),ctx),
    function(value){answer<<-value;done<<-TRUE},function(error){failure<<-error;done<<-TRUE})
  deadline<-Sys.time()+5
  while(!done&&Sys.time()<deadline)later::run_now(0.01)
  stopifnot(done,is.null(failure),identical(transport,c('GET','POST')),length(saved)==1L,
    normalize_round_lineup(answer$answer)$players[[5]]$id=='incoming')
})
cat('LINEUP:',n,'passed / 0 failed\n')
