#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R');library(shiny)})
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
account<-'lineup-race-fixture';cid<-'lineup-race-league';stamp<-as.POSIXct('2026-11-06T12:00:00',tz='UTC')
e<-function(type,id,p,at=stamp)rival_finance_evidence(account,cid,'unknown',type,id,p,at)
teams<-data.frame(teamid='a',teamname='A')
rules<-list(initial_budget=100,money_per_point=0,ranking_pool=0,ranking_mode='top',users_to_rank=-1,dreamteam_reward=5,mvp_reward=10)
round<-list(id='r1',number=1,points=1,status='closed',negative=FALSE)
base<-list(e('team_rounds','a',list(team_id='a',rounds=list(round))),
  e('movements','a:ranking',list(team_id='a',id='ranking',type='ranking',money=1,concept='1',date='2026-11-05T12:00:00Z')))
awards<-data.frame(round_id='r1',round_number=1,player_id='old-player',is_mvp=TRUE,is_finished=TRUE)
old_adapter<-async_adapter_fetch;old_persist<-persist_rival_finance_evidence
pending<-list();persisted<-list();calls<-character();inflight<-maximum<-0L
persist_rival_finance_evidence<-function(rows,context){persisted<<-c(persisted,rows);TRUE}
async_adapter_fetch<-function(fn,args,context,...){
  calls<<-c(calls,fn)
  if(fn=='get_rival_round_lineup_observation'){
    inflight<<-inflight+1L;maximum<<-max(maximum,inflight)
    return(promises::promise(function(resolve,reject)pending[[args$signature]]<<-list(args=args,resolve=resolve)))
  }
  result<-switch(fn,get_rival_finance_saved=list(evidence=base,complete=TRUE),
    get_rival_finance_seed=list(info=NULL,rules=rules,pressroom=data.frame()),
    get_rival_team_finance_observation=list(team_id=args$user_team_id,info=NULL,movements=NULL,rounds=NULL),
    stop(paste('Unexpected fixture call',fn)))
  promises::promise_resolve(result)
}
flush<-function(session){for(i in seq_len(30L)){later::run_now(0);session$flushReact()}}
complete<-function(signature,player,at){
  entry<-pending[[signature]];inflight<<-inflight-1L
  result<-list(team_id=entry$args$user_team_id,round_id=entry$args$round_id,signature=signature,
    answer=list(players=list(list(id=player)),bench=list(players=list()),negative=FALSE,status='closed'))
  attr(result,'observed_at')<-at;entry$resolve(result)
}
tryCatch({testServer(function(input,output,session){
  official<-reactiveVal(list(rows=awards,complete=TRUE))
  source<-shared_rival_finance_source(session,function()c(userid=account,token='synthetic'),function()cid,
    function()'a',function()teams,function()TRUE,official)
},{
  session$flushReact();flush(session)
  old_signature<-rival_round_signature(rival_team_round_rows(list(round),'a'),awards)
  stopifnot(length(pending)==1L,old_signature%in%names(pending),source()$finance_loading)
  revised<-awards;revised$player_id<-'new-player'
  official(list(rows=revised,complete=TRUE));flush(session)
  new_signature<-rival_round_signature(rival_team_round_rows(list(round),'a'),revised)
  stopifnot(length(pending)==2L,new_signature%in%names(pending),maximum==2L)
  complete(new_signature,'new-player',stamp+1);flush(session)
  current<-rival_evidence_rows(session$userData$rival_finance_shared$evidence,'round_lineup')
  stopifnot(length(current)==1L,identical(current[[1]]$payload$signature,new_signature))
  complete(old_signature,'old-player',stamp+2);flush(session)
  check('late superseded lineup cannot overwrite corrected eligibility or its stored revision',{
    value<-source();lineups<-rival_evidence_rows(session$userData$rival_finance_shared$evidence,'round_lineup')
    saved<-rival_evidence_rows(persisted,'round_lineup')
    stopifnot(!value$finance_loading,value$finance_pending==0L,
      !grepl('changed round',value$team_finances$estimate_notes,fixed=TRUE),
      value$team_finances$dreamteam_income==5,value$team_finances$mvp_income==10,
      length(lineups)==1L,identical(lineups[[1]]$payload$signature,new_signature),
      identical(lineups[[1]]$payload$players,list('new-player')),
      length(saved)==1L,identical(saved[[1]]$payload$signature,new_signature))
  })
  check('corrected lineup is reused without extra requests or a misleading retrieval failure',{
    before<-length(calls);official(list(rows=revised,complete=TRUE));flush(session)
    stopifnot(length(calls)==before,sum(calls=='get_rival_round_lineup_observation')==2L,
      !grepl('could not be retrieved',source()$reason,fixed=TRUE),maximum<=2L)
  })
})
# An official ranking can supply a round absent from the team's own history.
# Completion validation must retain that established acquisition fallback.
empty_rounds<-rival_team_round_rows(list(),'')
ranking<-e('round_ranking','r1',list(round_id='r1',round_number=1,status='closed',
  ranking=list(list(id='a',points=1)),source_teams=list('a'),
  signature=rival_ranking_signature(empty_rounds,'r1','a')))
base<-c(list(ranking),base[2])
pending<-list();persisted<-list();calls<-character();inflight<-maximum<-0L
clear_api_cache()
testServer(function(input,output,session){
  source<-shared_rival_finance_source(session,function()c(userid=account,token='synthetic'),function()cid,
    function()'a',function()teams,function()TRUE,function()list(rows=awards,complete=TRUE))
},{
  session$flushReact();flush(session)
  stopifnot(length(pending)==1L)
  complete(names(pending)[1],'old-player',stamp+3);flush(session)
  check('ranking-supplied current round still accepts and persists its matching lineup',{
    value<-source();saved<-rival_evidence_rows(persisted,'round_lineup')
    stopifnot(!value$finance_loading,value$team_finances$dreamteam_income==5,
      value$team_finances$mvp_income==10,length(saved)==1L,
      !grepl('changed round',value$team_finances$estimate_notes,fixed=TRUE))
  })
})
},finally={async_adapter_fetch<-old_adapter;persist_rival_finance_evidence<-old_persist})
cat(sprintf('Rival lineup revision race: %d passed / 0 failed\n',checks))
