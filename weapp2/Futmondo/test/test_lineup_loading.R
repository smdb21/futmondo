#!/usr/bin/env Rscript
# Exercise real acquisition, normalization and durable-store hydration at the HTTP boundary.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='fixture-account',token='private-fixture-token')
rounds<-data.frame(round_id='r13',round_number=13,is_finished=FALSE,
 begin_process=format(Sys.time()+86400,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))
# Observed /5/strategy/availables contract: standard entries omit pro;
# the official client tests pro === true to identify paid formations.
strategies<-list(strategies=lapply(seq_along(lineup_formations()),function(i){
 value<-list(st=lineup_formations()[i]);if(i>7L)value$pro<-TRUE;value
}),pro=FALSE,configuration=list(cpt=FALSE),bought=list())
answers<-list(
 lineup=list(strategy='3-5-2',multiposition=FALSE,pointSystem='fixture',
  players=list(list(id='p1',position=10,buyPrice=100,role='portero')),
  bench=list(enabled=TRUE,players=list())),
 roster=list(list(id='p1',name='Keeper',points=0,role='portero',team='Fixture Club',buyPrice=100,value=200)),
 information=list(configuration=list(cpt=FALSE),season='fixture-season'),
 availables=strategies)
api_reads<-db_reads<-db_writes<-0L;stored<-NULL
fm_async_read_http<-function(url,method='GET',body=NULL,headers=character(),...){
 if(grepl('/rest/v1/team_lineup_snapshots',url,fixed=TRUE)){
  stopifnot(method=='GET');db_reads<<-db_reads+1L
  result<-if(is.null(stored))'[]'else paste0('[',stored,']')
 }else{
  stopifnot(method=='POST',grepl('^https://api.futmondo.com/',url))
  route<-tail(strsplit(url,'/',fixed=TRUE)[[1]],1L);stopifnot(route%in%names(answers))
  api_reads<<-api_reads+1L
  result<-jsonlite::toJSON(list(answer=answers[[route]]),auto_unbox=TRUE,null='null')
 }
 promises::promise_resolve(list(status_code=200L,content=charToRaw(result)))
}
fm_async_http<-function(url,method,body,headers,...){
 stopifnot(grepl('/rest/v1/team_lineup_snapshots',url,fixed=TRUE),method=='POST',
  !grepl('private-fixture-token|private-database-key|"token"',body))
 stored<<-body;db_writes<<-db_writes+1L
 promises::promise_resolve(list(status_code=201L))
}
get_sb_url<-function()'https://database.fixture.invalid'
get_sb_key<-function()'private-database-key'
recent_player_points_source<-function(...)list(history=shiny::reactive(data.frame()),
 status=shiny::reactive(list(pending=0L,remaining=0L,saving=0L,failed=0L)))
settle<-function(session){for(i in 1:35){later::run_now(0);session$flushReact()}}
args<-list(is_module_active=function()TRUE,login_token=function()auth,
 championship_id=function()'fixture-league',user_team_id=function()'fixture-team',round_catalog=function()rounds)
shiny::testServer(lineup_Server,args=args,{
 settle(session)
 stopifnot(!loading(),notice()=='',cache_issue()=='',!is.null(source()),!is.null(draft()),
  draft()$strategy=='3-5-2',draft()$starters[11]=='p1',
  !is.object(source()$scoring_version),
  identical(source()$rules$allowed,lineup_formations()[1:7]),
  lineup_roster_frame(source())$points==0,api_reads==4L,db_reads==1L,db_writes==1L,
  grepl('Keeper',as.character(output$pitch$html),fixed=TRUE))
})
cat('PASS omitted premium flags load the pitch through all four real read/parser boundaries and save one snapshot\n')
observation<-jsonlite::fromJSON(stored,simplifyVector=FALSE)$payload$observed_at
shiny::testServer(lineup_Server,args=args,{
 settle(session)
 stopifnot(!loading(),notice()=='',cache_issue()=='',draft()$starters[11]=='p1',
  lineup_roster_frame(source())$points==0,identical(source()$observed_at,observation),api_reads==4L,db_reads==2L,db_writes==1L)
})
cat('PASS a new session reuses the persisted fresh snapshot with zero additional Futmondo reads or writes\n')
# Omission/null has source-defined meaning; malformed non-null flags still fail.
for(value in list('false',0,NA,list(TRUE),c(TRUE,FALSE))){
 malformed<-strategies;malformed$strategies[[1]]$pro<-value
 failure<-tryCatch(lineup_available_formations(malformed,'3-5-2'),error=identity)
 stopifnot(inherits(failure,'error'))
}
with_null<-strategies;with_null$strategies[[1]]['pro']<-list(NULL)
with_false<-strategies;with_false$strategies[[1]]$pro<-FALSE
stopifnot(identical(lineup_available_formations(with_null,'3-5-2'),lineup_formations()[1:7]),
 identical(lineup_available_formations(with_false,'3-5-2'),lineup_formations()[1:7]))
paid<-strategies;paid$bought<-list(list(st='4-6-0'))
stopifnot(setequal(lineup_available_formations(paid,'3-6-1'),c(lineup_formations()[1:7],'4-6-0','3-6-1')))
cat('PASS malformed flags remain rejected and unpurchased premium formations remain locked\n')
cat('LINEUP LOADING: 3 groups passed / 0 failed\n')
