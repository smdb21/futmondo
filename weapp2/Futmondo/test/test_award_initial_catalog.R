#!/usr/bin/env Rscript
# Synthetic HTTP only; exercise the actual replay adapter, catalog and Admin.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
auth<-c(userid='initial-award-fixture',token='synthetic');cid<-'fantasy-championship';lid<-'source-league'
active<-list(championships=list(list(id=cid,league=lid,userteam=list(id='own-team'))),
  rounds=list(list(`_id`='r12',number=12,championshipId=lid)))
leagues<-list(list(`_id`=lid,rounds=lapply(1:38,function(i)list(`_id`=paste0('r',i),status='future'))))
calendar<-list(rounds=lapply(38:1,function(i)list(`_id`=paste0('r',i),number=i,
  status=if(i<12)'done'else if(i==12)'playing'else 'future',matches=list(list(private='discarded')))))
check('captured league-ID marker survives while foreign league metadata is rejected',{
  catalog<-normalize_mvp_catalog(active,leagues,cid)
  stopifnot(catalog$current_round_number==12,catalog$current_round_id=='r12',catalog$league_id==lid)
  foreign<-active;foreign$rounds[[1]]$championshipId<-'another-league'
  rejected<-normalize_mvp_catalog(foreign,leagues,cid)
  stopifnot(is.na(rejected$current_round_number),!nzchar(rejected$current_round_id),
    is.na(rejected$rounds$round_number[12]))
})
fixture<-new.env(parent=emptyenv());fixture$requests<-list();fixture$stored<-list();fixture$mode<-'ok'
read_rival_finance_evidence<-function(...)list(evidence=fixture$stored,complete=TRUE)
award_expected_scoring<-function(...)'unknown'
get_sb_url<-function()'';get_sb_key<-function()''
futmondo_post<-function(...)stop('Unexpected synchronous HTTP')
persist_official_award_snapshot<-function(result,login,championship_id,reference,context,on_complete){
  fixture$stored[[reference$round_id]]<-award_snapshot_evidence(result,login,championship_id,reference,context)
  on_complete(TRUE);TRUE
}
fm_async_http<-function(url,method='GET',body=NULL,headers=character(),owner='',...){
  stopifnot(method=='POST')
  query<-jsonlite::fromJSON(as.character(body),simplifyVector=FALSE)$query
  fixture$requests[[length(fixture$requests)+1L]]<-list(url=url,query=query)
  if(identical(url,ACTIVE_CHAMPIONSHIPS_URL))answer<-active
  else if(grepl('/2/league/list$',url))answer<-leagues
  else if(grepl('/2/league/matches$',url)){
    stopifnot(identical(query,list(leagueId=lid)))
    answer<-calendar
    if(fixture$mode=='missing')answer<-list()
    if(fixture$mode=='conflict')answer$rounds<-c(answer$rounds,list(list(`_id`='r1',number=99,status='done')))
  }else if(identical(url,DREAMTEAM_URL)){
    limit<-active$rounds[[1]]$number-1L
    stopifnot(identical(query$championshipId,cid),query$round%in%paste0('r',seq_len(limit)))
    answer<-list(players=lapply(1:11,function(i)list(id=paste0('p',i),name=paste('Player',i),points=i)),mvp='p1')
  }else stop('Unexpected HTTP route')
  response<-list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=answer),auto_unbox=TRUE,null='null')),times=list(total=0))
  promises::promise(function(resolve,reject)later::later(function()resolve(response),.015))
}
options(futmondo.offline=FALSE)
settle<-function(session,predicate,timeout=5){deadline<-Sys.time()+timeout;repeat{
  session$flushReact();later::run_now(.005);session$flushReact()
  if(isTRUE(shiny::isolate(predicate())))return(invisible(NULL))
  if(Sys.time()>deadline)stop('Initial awards fixture timed out')
}}
reads<-function(route)sum(vapply(fixture$requests,function(x)grepl(route,x$url),logical(1)))
check('initial Admin displays numbered historical rounds before any award request',{
  clear_api_cache();fixture$requests<-list();messages<-list()
  shiny::testServer(admin_Server,args=list(is_module_active=function()TRUE,login_token=function()auth,
    championship_id=function()cid,user_team_id=function()'own-team',user_teams_RV=function()data.frame(),is_admin=TRUE),{
    root<-session$rootScope();root$sendInputMessage<-function(id,message){messages[[id]]<<-message}
    heartbeat<-FALSE;later::later(function(){heartbeat<<-TRUE},.005)
    settle(session,function()!awards()$loading)
    snapshot<-awards();coverage<-snapshot$coverage
    stopifnot(heartbeat,identical(coverage$round_number,as.numeric(1:38)),sum(coverage$required)==11L,
      reads('league/matches$')==1L,reads('dreamteam$')==0L,reads('match/list$')==0L)
    choices<-messages[[session$ns('award_round')]]$options
    stopifnot(grepl('Round 11',choices,fixed=TRUE),
      !grepl('Round (',choices,fixed=TRUE),!grepl('Round 38',choices,fixed=TRUE))
    table<-output$awards_coverage
    stopifnot(!grepl('r38',table,fixed=TRUE),!grepl('Failures:',output$awards_progress,fixed=TRUE))
    session$setInputs(btn_sync_dreamteams=1)
    settle(session,function()!awards()$loading&&!awards()$running&&awards()$saved==11L)
    stopifnot(reads('dreamteam$')==11L,reads('league/matches$')==2L,reads('match/list$')==0L,
      awards()$failed==0L,awards()$complete,!nzchar(awards()$reason),length(fixture$stored)==11L)
  })
})
check('saved awards and bulk metadata are reused on a new consuming session',{
  before<-length(fixture$requests)
  shiny::testServer(function(input,output,session){source<-shared_mvp_source(session,function()auth,function()cid,function()TRUE)},{
    settle(session,function()!source()$loading)
    stopifnot(source()$complete,sum(source()$coverage$persistence=='Saved')==11L,
      source()$failed==0L,length(fixture$requests)==before)
  })
  cached<-get_cached_data(paste0('award_round_metadata_',cid),stop('Cache should be warm'),context=resource_context(auth,cid))
  stopifnot(nrow(cached$rows)==38L,!any(c('matches','private')%in%names(cached$rows)))
})
check('an explicit retrieval refreshes a warm calendar when the current round advances',{
  before_awards<-reads('dreamteam$');before_metadata<-reads('league/matches$')
  active$rounds[[1]]$`_id`<-'r13';active$rounds[[1]]$number<-13
  calendar$rounds[[which(vapply(calendar$rounds,function(x)x$number==12,logical(1)))]]$status<-'done'
  calendar$rounds[[which(vapply(calendar$rounds,function(x)x$number==13,logical(1)))]]$status<-'playing'
  shiny::testServer(function(input,output,session){source<-shared_mvp_source(session,function()auth,function()cid,function()TRUE)},{
    settle(session,function()!source()$loading)
    stopifnot(sum(source()$coverage$required)==11L)
    attr(source,'retrieve')('missing')
    settle(session,function()!source()$loading&&!source()$running)
    stopifnot(sum(source()$coverage$required)==12L,source()$saved==1L,source()$failed==0L,source()$complete,
      reads('dreamteam$')==before_awards+1L,reads('league/matches$')==before_metadata+1L,
      tail(fixture$requests,1)[[1]]$query$round=='r12')
  })
})
check('missing calendar metadata remains unavailable and conflicting identities do not assign ordinals',{
  fixture$mode<-'missing';clear_api_cache()
  completed<-NULL
  promises::then(async_adapter_fetch('get_award_round_catalog',list(login=auth,championship_id=cid),resource_context(auth,cid)),function(x)completed<<-x)
  deadline<-Sys.time()+3;while(is.null(completed)&&Sys.time()<deadline)later::run_now(.01)
  stopifnot(!is.null(completed),!completed$complete,nzchar(completed$reason),is.na(completed$rounds$round_number[1]),
    completed$current_round_number==13)
  fixture$mode<-'conflict';clear_api_cache();completed<-NULL
  promises::then(async_adapter_fetch('get_award_round_catalog',list(login=auth,championship_id=cid),resource_context(auth,cid)),function(x)completed<<-x)
  deadline<-Sys.time()+3;while(is.null(completed)&&Sys.time()<deadline)later::run_now(.01)
  stopifnot(!is.null(completed),!completed$complete,is.na(completed$rounds$round_number[1]),
    grepl('conflicting',completed$reason,fixed=TRUE),completed$rounds$round_number[38]==38)
})
check('unknown future finality alone needs no metadata request and produces no warning',{
  original_catalog<-get_finished_round_catalog
  known<-normalize_mvp_catalog(active,leagues,cid)
  known$rounds$round_number<-as.numeric(1:38)
  known$rounds$is_finished<-c(rep(TRUE,12),rep(NA,26))
  get_finished_round_catalog<-function(...)known
  before<-length(fixture$requests)
  result<-get_award_round_catalog(auth,cid)
  stopifnot(result$complete,!nzchar(result$reason),length(fixture$requests)==before)
  get_finished_round_catalog<-original_catalog
})
cat(sprintf('INITIAL ADMIN AWARDS: %d passed / 0 failed; one cold bulk calendar read, zero warm reads\n',n))
