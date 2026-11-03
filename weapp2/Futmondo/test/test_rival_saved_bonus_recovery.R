#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R');library(shiny)})
checks<-0L;check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
cid<-'fixture-league';account<-'fixture-account';stamp<-as.POSIXct('2026-11-02T12:00:00',tz='UTC')
e<-function(type,id,p)rival_finance_evidence(account,cid,'unknown',type,id,p,stamp)
teams<-data.frame(teamid=c('a','b','c'),teamname=c('A','B','C'),points=c(10,5,NA_real_))
version<-award_scoring_version('custom',list())
players<-lapply(seq_len(11),function(i)list(championship_id=cid,round_id='r1',round_number=1,player_id=paste0('p',i),player_name=paste('Player',i),points=i,is_mvp=i==1,is_finished=TRUE))
award<-e('awards','r1',list(round_id='r1',round_number=1,players=players,finality='final',scoring_version=version,pointSystem='custom',custom=list()))
rounds<-function(id,score)e('team_rounds',id,list(team_id=id,rounds=list(list(id='r1',number=1,points=score,status='closed',negative=FALSE),list(id='r2',number=2,points=0,status='running',negative=FALSE))))
raw<-list(award,rounds('a',10),rounds('b',5),e('team_rounds','c',list(team_id='c',rounds=list())))
wire<-jsonlite::toJSON(raw,auto_unbox=TRUE,na='null',null='null')
decoded<-rival_evidence_decode(as.data.frame(jsonlite::fromJSON(wire)))
awards<-award_saved_rows(rival_evidence_rows(decoded,'awards'),cid,'unknown',version,account)
rules<-list(initial_budget=100,money_per_point=2,ranking_pool=30,ranking_mode='top',users_to_rank=-1,dreamteam_reward=5,mvp_reward=10)
check('real simplified JSON shape restores saved XI and multiple rounds without list wrappers',{
 stopifnot(nrow(awards)==11L,sum(awards$is_mvp)==1L,nrow(rival_saved_rounds(decoded))==4L,
  identical(as.character(rival_saved_rounds(decoded)$round_id),rep(c('r1','r2'),2)))
 other<-raw;other[[1]]$account_id<-'another-account'
 stopifnot(nrow(award_saved_rows(rival_evidence_rows(rival_evidence_decode(as.data.frame(jsonlite::fromJSON(jsonlite::toJSON(other,auto_unbox=TRUE)))),'awards'),cid,'unknown',version,account))==0L,
  nrow(award_saved_rows(rival_evidence_rows(decoded,'awards'),cid,'unknown','incompatible-version',account))==0L)
 malformed<-raw;malformed[[1]]$payload$players[[11]]$player_id<-'p1'
 stopifnot(nrow(award_saved_rows(rival_evidence_rows(rival_evidence_decode(as.data.frame(jsonlite::fromJSON(jsonlite::toJSON(malformed,auto_unbox=TRUE)))),'awards'),cid,'unknown',version,account))==0L)
})
original_http<-fm_async_read_http;original_url<-get_sb_url;original_key<-get_sb_key
get_sb_url<-function()'https://database.synthetic.invalid';get_sb_key<-function()'synthetic-test-key'
http_calls<-character()
ranking_answer<-list(metadata=list(status='closed'),ranking=list(list(id='a',name='A',points=10,position=1),list(id='b',name='B',points=5,position=2)))
fm_async_read_http<-function(url,method='GET',body=NULL,headers=character(),owner=''){
 http_calls<<-c(http_calls,url)
 query<-httr::parse_url(url)$query
 value<-if(grepl('/rest/v1/rival_finance_evidence_latest',url,fixed=TRUE)){
  if(as.integer(query$offset %||% 0)==0L)raw else list()
 }else if(grepl('/activechampionships$',url)){
  list(answer=list(championships=list(list(id=cid,ps='custom',custom=list()))))
 }else if(identical(url,'https://api.futmondo.com/1/ranking/round')){
  sent<-jsonlite::fromJSON(body);stopifnot(identical(sent$query,list(championshipId=cid,roundNumber=1L,userteamId='a')))
  list(answer=ranking_answer)
 }else stop('Unexpected synthetic request')
 promises::promise_resolve(list(status_code=200L,content=charToRaw(jsonlite::toJSON(value,auto_unbox=TRUE,na='null',null='null')),times=c(total=.001)))
}
await<-function(p){done<-FALSE;value<-failure<-NULL;promises::then(p,function(x){value<<-x;done<<-TRUE},function(x){failure<<-x;done<<-TRUE});deadline<-Sys.time()+5;while(!done){later::run_now(.001);if(Sys.time()>deadline)stop('async timeout')};if(!is.null(failure))stop(failure);value}
check('actual async DB transport and saved award catalog reuse the corrected decoder',{
 clear_api_cache()
 ctx<-resource_context(account_id=account,championship_id=cid,session_id='offline-fixture')
 catalog<-await(async_adapter_fetch('get_mvp_saved_catalog',list(login=c(userid=account,token='synthetic'),championship_id=cid),ctx))
 stopifnot(nrow(catalog$saved)==11L,all(catalog$saved$persistence=='Saved'),isTRUE(catalog$saved_complete),identical(catalog$expected_scoring,version))
 before<-length(http_calls)
 saved<-await(async_adapter_fetch('get_rival_finance_saved',list(account_id=account,championship_id=cid,season='unknown'),ctx))
 stopifnot(length(saved$evidence)==4L,nrow(rival_saved_rounds(saved$evidence))==4L,
  sum(grepl('/rival_finance_evidence_latest',http_calls))==2L,length(http_calls)==before,!any(grepl("league_cash_defaults",http_calls,fixed=TRUE)))
})
source_rounds<-rival_saved_rounds(decoded,include_ranking=FALSE)
sig<-rival_ranking_signature(source_rounds,'r1',teams$teamid)
ranking_result<-NULL
check('actual async ranking route sends the captured query and produces persistable source evidence',{
 # Only the mocked read boundary is enabled; no real network path is exercised.
 old<-getOption('futmondo.offline');options(futmondo.offline=FALSE)
 ranking_result<-tryCatch(await(async_adapter_fetch('get_rival_round_ranking_observation',list(login=c(userid=account,token='synthetic'),championship_id=cid,user_team_id='a',round_id='r1',round_number=1,signature=sig,source_teams=teams$teamid),resource_context(account_id=account,championship_id=cid,session_id='offline-fixture'))),finally=options(futmondo.offline=old))
 evidence<-rival_ranking_evidence(ranking_result,account,cid,'unknown')
 stopifnot(length(evidence)==1L,evidence[[1]]$evidence_type=='eligibility',evidence[[1]]$payload$kind=='round_ranking',nrow(rival_round_ranking_rows(evidence[[1]]$payload))==2L)
})
ranked<-rival_ranking_evidence(ranking_result,account,cid,'unknown')
check('official participants restore position rewards without fabricating an absent team score',{
 history<-rival_historical_components(c(decoded,ranked),teams,rules,awards)
 income<-history$income
 stopifnot(identical(income$amount[income$bonus_type=='ranking'],c(20,10)),!any(history$rounds$team_id=='c'),
  !any(grepl('Ranking history incomplete',unlist(history$missing))))
 out<-build_rival_cash_dataset(teams,cid,c(decoded,ranked),awards,rules=rules,account_id=account)$team_finances
 stopifnot(identical(out$ranking_prize,c(20,10,0)),identical(out$estimated_cash,c(140,120,100)),
  is.na(out$points[3]),grepl('Per-round point history is unavailable',out$estimate_notes[3]))
 revised<-decoded;revised[[2]]$payload$rounds$points[1]<-11
 stopifnot(length(rival_saved_rankings(c(revised,ranked),teams$teamid))==0L)
})
check('malformed or open ranking responses cannot establish complete historical participants',{
 p<-ranked[[1]]$payload
 for(kind in c('open','duplicate','missing')){
  changed<-p
  if(kind=='open')changed$status<-'running'
  if(kind=='duplicate')changed$ranking[[2]]$id<-'a'
  if(kind=='missing')changed$ranking[[2]]$points<-NULL
  stopifnot(nrow(rival_round_ranking_rows(changed))==0L)
 }
})
check('shared recovery persists one missing ranking and reuses saved lineups across sessions',{
 original_adapter<-async_adapter_fetch;original_persist<-persist_rival_finance_evidence
 persisted<-c(decoded,list(e('movements','point-a',list(id='point-a',team_id='a',type='points',concept='1',money=20,date='2026-11-01T12:00:00Z'))));calls<-character();busy<-0L;maximum<-0L
 persist_rival_finance_evidence<-function(rows,context){persisted<<-rival_merge_evidence(persisted,rows);TRUE}
 async_adapter_fetch<-function(fn,args,context,...){
  calls<<-c(calls,fn);busy<<-busy+1L;maximum<<-max(maximum,busy)
  result<-switch(fn,get_rival_finance_saved=list(evidence=rival_evidence_decode(as.data.frame(jsonlite::fromJSON(jsonlite::toJSON(persisted,auto_unbox=TRUE,na='null',null='null')))),defaults=NULL),
   get_rival_finance_seed=list(info=NULL,rules=rules,pressroom=data.frame()),
   get_rival_team_finance_observation=list(team_id=args$user_team_id,info=NULL,movements=NULL,rounds=NULL),
   get_rival_round_ranking_observation=ranking_result,
   get_rival_round_lineup_observation=list(team_id=args$user_team_id,round_id=args$round_id,signature=args$signature,answer=list(status='closed',negative=FALSE,players=if(args$user_team_id=='a')lapply(1:11,function(i)list(id=paste0('p',i)))else list(list(id='x')),bench=list(players=list()))))
  promises::promise(function(resolve,reject)later::later(function(){busy<<-busy-1L;resolve(result)},.001))
 }
 settle<-function(session,source){deadline<-Sys.time()+6;repeat{later::run_now(.005);session$flushReact();if(!isTRUE(shiny::isolate(source()$finance_loading)))break;if(Sys.time()>deadline)stop('recovery queue timeout')}}
 for(run in 1:2){
  testServer(function(input,output,session){source<-shared_rival_finance_source(session,function()c(userid=account,token='synthetic'),function()cid,function()'a',function()teams,function()TRUE,function()list(rows=awards,complete=FALSE))},{
   session$flushReact();settle(session,source)
   value<-source();a<-value$team_finances[value$team_finances$teamid=='a',]
   stopifnot(a$ranking_prize==20,a$dreamteam_income==55,a$mvp_income==10,a$estimated_cash==205,
    maximum<=2L,!value$finance_loading)
   for(id in teams$teamid){ledger<-value$cash_ledgers[[id]];row<-value$team_finances[value$team_finances$teamid==id,];stopifnot(tail(ledger$running_balance,1L)==row$estimated_cash)}
  })
 }
 stopifnot(sum(calls=='get_rival_round_ranking_observation')==1L,sum(calls=='get_rival_round_lineup_observation')==2L)
 async_adapter_fetch<-original_adapter;persist_rival_finance_evidence<-original_persist
})
fm_async_read_http<-original_http;get_sb_url<-original_url;get_sb_key<-original_key
cat(sprintf('SAVED RIVAL BONUS RECOVERY: %d passed / 0 failed\n',checks))
