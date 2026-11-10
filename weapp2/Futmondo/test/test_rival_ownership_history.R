#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R');library(shiny)})
checks<-0L
check<-function(label,expr){force(expr);checks<<-checks+1L;cat('PASS',label,'\n')}
teams<-data.frame(teamid=c('a','b'),teamname=c('A','B'),stringsAsFactors=FALSE)
owners<-list(list(n='A',d='2026-01-01T00:00:00Z',p=10,l=FALSE),list(n='futmondo',d='2026-03-01T00:00:00Z',p=12,l=FALSE))
summary<-list(owners=owners,owners_received=TRUE)
rows<-rival_owner_history_rows(summary)
check('source owner fields are parsed independently of score-history completeness',{
 stopifnot(nrow(rows)==2L,identical(rows$owner_name,c('A','futmondo')),identical(rows$price,c(10,12)))
 for(value in list(list(owners=owners),list(owners_received=TRUE,owners=list()),list(owners_received=TRUE,owners=list(list(n='A',d='bad'))),list(owners_received=TRUE,owners=c(owners,owners[1])),list(owners_received=TRUE,owners=list(list(n='A',d='2026-01-01T00:00:00Z',l=TRUE)))))
  stopifnot(nrow(rival_owner_history_rows(value))==0L)
})
resolve<-function(value=rows,begin='2026-02-01T00:00:00Z',end='2026-02-10T00:00:00Z',observed='2026-04-01T00:00:00Z',directory=teams)
 rival_resolve_owner_interval(value,begin,end,observed,directory)
check('an unchanged uniquely mapped owner spans the whole evidenced interval',{
 stopifnot(resolve()$known,resolve()$team_id=='a',!resolve()$unowned)
 stopifnot(!resolve(begin='2025-12-01T00:00:00Z')$known,!resolve(begin='2026-01-01T00:00:00Z')$known,!resolve(end='2026-03-10T00:00:00Z')$known,
  !resolve(observed='2026-02-05T00:00:00Z')$known,!resolve(observed='2026-02-15T00:00:00Z')$known,!resolve(observed=rival_finance_stamp(Sys.time()+120))$known,!resolve(directory=rbind(teams,data.frame(teamid='c',teamname='A')))$known)
 unknown<-rows;unknown$owner_name[1]<-'Renamed A';stopifnot(!resolve(value=unknown)$known)
 stopifnot(resolve(begin='2026-03-02T00:00:00Z',end='2026-03-10T00:00:00Z')$unowned)
 stopifnot(!resolve(begin='2026-03-02T00:00:00Z',end='2026-03-10T00:00:00Z',
  directory=rbind(teams,data.frame(teamid='brand-name',teamname='futmondo')))$known)
})
# Real player-summary response path retains presence independently of empty history.
original_post<-futmondo_post;original_cache<-get_cached_data
get_cached_data<-function(key,expr,...)force(expr)
sent<-NULL;response_answer<-list(data=list(id='p'),owners=owners,historyComplete=TRUE)
futmondo_post<-function(url,body,...){sent<<-list(url=url,body=jsonlite::fromJSON(body));structure(list(url=url,status_code=200L,
 headers=list('Content-Type'='application/json'),all_headers=list(),cookies=data.frame(),content=charToRaw(jsonlite::toJSON(list(answer=response_answer),auto_unbox=TRUE))),class='response')}
check('real summary query and missing-versus-empty ownership arrays stay distinct',{
 live<-get_player_summary(c(userid='u',token='synthetic'),'league','a','p')
 stopifnot(live$owners_received,nrow(rival_owner_history_rows(live))==2L,
  identical(sent$url,PLAYER_SUMMARY_URL),identical(sent$body$query,list(championshipId='league',userteamId='a',playerId='p')))
 response_answer$owners<-NULL;stopifnot(!get_player_summary(c(userid='u',token='synthetic'),'league','a','p')$owners_received)
 response_answer$owners<-list();stopifnot(get_player_summary(c(userid='u',token='synthetic'),'league','a','p')$owners_received)
})
futmondo_post<-original_post;get_cached_data<-original_cache
stamp<-as.POSIXct('2026-04-01T00:00:00Z',tz='UTC')
ev<-function(type,id,p)rival_finance_evidence('u','league','unknown',type,id,p,stamp)
catalog_result<-list(status='ok',rows=data.frame(round_id='r1',begin_process='2026-02-01T00:00:00Z'),observed_at=rival_finance_stamp(stamp))
calendar<-rival_ownership_observation_evidence(catalog_result,'u','league','unknown',catalog=TRUE)
awards<-data.frame(round_id='r1',round_number=1,player_id=paste0('p',1:11),is_mvp=c(TRUE,rep(FALSE,10)),is_finished=TRUE)
histories<-lapply(teams$teamid,function(id)ev('team_rounds',id,list(team_id=id,rounds=list(list(id='r1',number=1,points=1,status='closed',negative=FALSE)))))
payment<-ev('movements','a:ranking',list(id='ranking',team_id='a',type='ranking',category='bonus',money=10,concept='1',date='2026-02-10T00:00:00Z'))
owner_record<-function(id)rival_ownership_observation_evidence(list(status='ok',player_id=id,rows=rows,observed_at=rival_finance_stamp(stamp)),'u','league','unknown')
records<-c(calendar,histories,list(payment),unlist(lapply(awards$player_id,owner_record),recursive=FALSE))
rules<-list(initial_budget=100,money_per_point=0,ranking_pool=0,ranking_mode='top',users_to_rank=-1,dreamteam_reward=5,mvp_reward=10)
build<-function(evidence)build_rival_cash_dataset(teams,'league',evidence,awards,rules=rules,account_id='u',reference_team_id='a')
check('returned ownership history persists as scoped evidence and drives the one cash ledger',{
 out<-build(records)
 stopifnot(out$team_finances$estimated_cash[out$team_finances$teamid=='a']==175,
  attr(out$cash_ledgers$a,'round_bonus_totals')$amount[attr(out$cash_ledgers$a,'round_bonus_totals')$bonus_type=='mvp']==10,
  attr(out$cash_ledgers$a,'round_bonus_totals')$amount[attr(out$cash_ledgers$a,'round_bonus_totals')$bonus_type=='dreamteam']==55)
 restored<-rival_evidence_decode(jsonlite::fromJSON(jsonlite::toJSON(records,auto_unbox=TRUE,null='null')))
 stopifnot(identical(build(restored)$cash_ledgers,out$cash_ledgers))
 for(field in c('account_id','championship_id','season')){
  wrong<-records;for(i in seq_along(wrong))if(identical(wrong[[i]]$payload$kind,'player_ownership_history'))wrong[[i]][[field]]<-'other'
  stopifnot(build(wrong)$team_finances$estimated_cash[1]==110)
 }
 for(change in c('identity','coverage','schema')){
  wrong<-records
  for(j in seq_along(wrong))if(identical(wrong[[j]]$payload$kind,'player_ownership_history')){
   if(change=='identity')wrong[[j]]$source_id<-'player-owners:foreign-player'
   if(change=='coverage')wrong[[j]]$payload$coverage<-'complete'
   if(change=='schema')wrong[[j]]$payload$unexpected<-'unsupported'
  }
  stopifnot(build(wrong)$team_finances$estimated_cash[1]==110)
 }
 conflict<-ev('eligibility','conflict',list(round_id='r1',player_id='p1',team_id='b',eligible=TRUE,verified=TRUE))
 stopifnot(is.na(attr(build(c(records,list(conflict)))$cash_ledgers$a,'round_bonus_totals')$amount[2]))
 no_start<-Filter(function(row)!identical(row$payload$kind,'award_ownership_calendar'),records)
 stopifnot(build(no_start)$team_finances$estimated_cash[1]==110)
 points_only<-records;points_only[[length(calendar)+length(histories)+1L]]$payload$type<-'points'
 stopifnot(build(points_only)$team_finances$estimated_cash[1]==110)
})
cat('Ownership history:',checks,'checks passed.\n')
# Exercise the actual shared queue, preserving its two-slot bound and durable reuse.
original_async<-async_adapter_fetch;original_defer<-defer_persistence
initial_records<-c(calendar,histories,list(payment),lapply(awards$player_id[-1],function(id)ev('eligibility',paste0('known:',id),list(round_id='r1',player_id=id,team_id='a',eligible=TRUE,verified=TRUE))))
requests<-character();persisted<-list();seed_records<-initial_records
async_adapter_fetch<-function(fn,args,context,...){
 requests<<-c(requests,fn)
 result<-switch(fn,
  read_rival_calculation_snapshot=NULL,
  get_rival_finance_saved=list(evidence=seed_records,complete=TRUE,pressroom=data.frame()),
  get_rival_finance_seed=list(info=NULL,rules=rules,pressroom=data.frame(),notifications=NULL),
  get_rival_team_finance_observation=list(team_id=args$user_team_id,info=NULL,movements=NULL,rounds=NULL),
  get_rival_round_lineup_observation=list(team_id=args$user_team_id,round_id=args$round_id,signature=args$signature,
   answer=list(players=lapply(if(args$user_team_id=='a')awards$player_id[-1]else 'b0',function(id)list(id=id)),bench=list(players=list()),status='closed',negative=FALSE)),
  get_rival_player_ownership_observation=list(status='ok',player_id=args$player_id,rows=rows,observed_at=rival_finance_stamp(stamp)),
  get_rival_ownership_round_catalog=catalog_result,
  stop(paste('unexpected task',fn)))
 promises::promise_resolve(result)
}
defer_persistence<-function(fn,args,context,on_complete=NULL,...){
 if(identical(args[[1]],'rival_finance_evidence'))persisted<<-rival_merge_evidence(persisted,args[[2]])
 if(is.function(on_complete))on_complete(TRUE)
 TRUE
}
run_queue<-function(expected_cash=175){
 result<-NULL
 testServer(function(input,output,session){
   value<-shared_rival_finance_source(session,function()c(userid='u',token='synthetic'),function()'league',function()'a',
     function()teams,function()TRUE,function()list(rows=awards,complete=TRUE))
  },{
   for(i in seq_len(60)){later::run_now(0);session$flushReact()}
   result<<-value()
   stopifnot(!result$finance_loading,result$team_finances$estimated_cash[result$team_finances$teamid=='a']==expected_cash)
  })
 result
}
check('shared queue retrieves a missing player once and saves the returned ownership history',{
 result<-run_queue()
 stopifnot(sum(requests=='get_rival_player_ownership_observation')==1L,
  length(rival_ownership_evidence_rows(persisted,'player_ownership_history'))==1L,
  !any(requests=='get_rival_ownership_round_catalog'))
})
check('a fresh shared session reuses the persisted source history without reacquisition',{
 seed_records<-rival_merge_evidence(initial_records,persisted)
 requests<-character();persisted<-list()
 result<-run_queue()
 stopifnot(!any(requests=='get_rival_player_ownership_observation'),
  !any(requests=='get_rival_ownership_round_catalog'))
})
check('missing persisted start times use one catalog task and retain its source metadata',{
 seed_records<-Filter(function(row)!identical(row$payload$kind,'award_ownership_calendar'),initial_records)
 requests<-character();persisted<-list();result<-run_queue()
 stopifnot(sum(requests=='get_rival_ownership_round_catalog')==1L,
  length(rival_ownership_evidence_rows(persisted,'award_ownership_calendar'))==1L)
})
check('a catalog without usable required intervals causes no player-history requests',{
 original_catalog<-catalog_result
 catalog_result$rows$round_id<-'other-round'
 seed_records<-Filter(function(row)!identical(row$payload$kind,'award_ownership_calendar'),initial_records)
 requests<-character();persisted<-list();result<-run_queue(expected_cash=160)
 stopifnot(sum(requests=='get_rival_ownership_round_catalog')==1L,
  !any(requests=='get_rival_player_ownership_observation'),grepl('lack an official round start',result$reason,fixed=TRUE))
 catalog_result<-original_catalog
})
check('a failed ownership write stays retryable on refresh without another player read',{
 seed_records<-initial_records;requests<-character();persisted<-list();owner_writes<-0L
 defer_persistence<-function(fn,args,context,on_complete=NULL,...){
  ownership<-identical(args[[1]],'rival_finance_evidence')&&any(vapply(args[[2]],function(row)identical(row$payload$kind,'player_ownership_history'),logical(1)))
  if(ownership)owner_writes<<-owner_writes+1L
  ok<-!ownership||owner_writes>1L
  if(ok&&identical(args[[1]],'rival_finance_evidence'))persisted<<-rival_merge_evidence(persisted,args[[2]])
  if(is.function(on_complete))on_complete(ok)
  TRUE
 }
 revision<-reactiveVal(0L)
 testServer(function(input,output,session){
  value<-shared_rival_finance_source(session,function()c(userid='u',token='synthetic'),function()'league',function()'a',
   function()teams,function()TRUE,function()list(rows=awards,complete=TRUE),revision)
 },{
  for(i in seq_len(60)){later::run_now(0);session$flushReact()}
  stopifnot(owner_writes==1L,grepl('could not be saved',value()$reason,fixed=TRUE))
  revision(1L);session$flushReact()
  for(i in seq_len(60)){later::run_now(0);session$flushReact()}
  stopifnot(owner_writes==2L,sum(requests=='get_rival_player_ownership_observation')==1L,
    !grepl('Historical ownership was retrieved but could not be saved',value()$reason,fixed=TRUE))
 })
})
check('delayed ownership responses retain a usable ledger and cause one final batch rebuild',local({
 original_builder<-build_rival_cash_dataset;on.exit(build_rival_cash_dataset<<-original_builder)
 builds<-0L
 build_rival_cash_dataset<<-function(...){builds<<-builds+1L;original_builder(...)}
 pending<-list();requests<-character();persisted<-list()
 seed_records<-c(calendar,histories,list(payment),lapply(awards$player_id[-c(1,2)],function(id)
  ev('eligibility',paste0('known:',id),list(round_id='r1',player_id=id,team_id='a',eligible=TRUE,verified=TRUE))))
 async_adapter_fetch<<-function(fn,args,context,...){
  requests<<-c(requests,fn)
  if(fn=='get_rival_player_ownership_observation')return(promises::promise(function(resolve,reject)
   pending[[args$player_id]]<<-list(resolve=resolve,reject=reject)))
  result<-switch(fn,read_rival_calculation_snapshot=NULL,
   get_rival_finance_saved=list(evidence=seed_records,complete=TRUE,pressroom=data.frame()),
   get_rival_finance_seed=list(info=NULL,rules=rules,pressroom=data.frame(),notifications=NULL),
   get_rival_team_finance_observation=list(team_id=args$user_team_id,info=NULL,movements=NULL,rounds=NULL),
   get_rival_round_lineup_observation=list(team_id=args$user_team_id,round_id=args$round_id,signature=args$signature,
    answer=list(players=lapply(if(args$user_team_id=='a')awards$player_id[-c(1,2)]else 'b0',function(id)list(id=id)),
      bench=list(players=list()),status='closed',negative=FALSE)),
   get_rival_ownership_round_catalog=catalog_result,stop(paste('unexpected task',fn)))
  promises::promise_resolve(result)
 }
 defer_persistence<<-function(fn,args,context,on_complete=NULL,...){
  if(identical(args[[1]],'rival_finance_evidence'))persisted<<-rival_merge_evidence(persisted,args[[2]])
  if(is.function(on_complete))on_complete(TRUE)
  TRUE
 }
 award_state<-reactiveVal(list(rows=awards,complete=TRUE))
 testServer(function(input,output,session){
  value<-shared_rival_finance_source(session,function()c(userid='u',token='synthetic'),function()'league',function()'a',
   function()teams,function()TRUE,award_state)
 },{
  settle<-function(){for(i in seq_len(40)){later::run_now(0);session$flushReact()}}
  settle()
  stopifnot(length(pending)==2L,session$userData$rival_finance_shared$ownership_pending==2L,value()$finance_loading,
    value()$team_finances$estimated_cash[value()$team_finances$teamid=='a']==155)
  before<-builds;old_inputs<-session$userData$rival_finance_shared$calculation_inputs
  pending$p1$resolve(list(status='ok',player_id='p1',rows=rows,observed_at=rival_finance_stamp(stamp)));settle()
  stopifnot(builds==before,session$userData$rival_finance_shared$ownership_pending==1L)
  # Force a status publication while the second promise is delayed. This must
  # retain the old inputs and data instead of silently accepting partial evidence.
  state_update<-award_state();state_update$reason<-"Synthetic status publication while ownership is pending"
  award_state(state_update);session$flushReact();later::run_now(0)
  stopifnot(builds==before,identical(session$userData$rival_finance_shared$calculation_inputs,old_inputs),
    value()$finance_loading,value()$team_finances$estimated_cash[value()$team_finances$teamid=='a']==155)
  pending$p2$resolve(list(status='ok',player_id='p2',rows=rows,observed_at=rival_finance_stamp(stamp)));settle()
  stopifnot(builds==before+1L,session$userData$rival_finance_shared$ownership_pending==0L,!value()$finance_loading,
    value()$team_finances$estimated_cash[value()$team_finances$teamid=='a']==175,
    !identical(session$userData$rival_finance_shared$calculation_inputs,old_inputs))
 })
}))
async_adapter_fetch<-original_async;defer_persistence<-original_defer
cat('Ownership history total:',checks,'checks passed.\n')
