#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R');library(shiny)})
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
cid<-'inactive-fixture';now<-as.POSIXct('2026-11-02T12:00:00',tz='UTC')
ids<-letters[1:10];teams<-data.frame(teamid=ids,teamname=toupper(ids))
rules<-list(initial_budget=100000000,money_per_point=0,ranking_pool=30000000,ranking_mode='flop',users_to_rank=-1,dreamteam_reward=0,mvp_reward=0)
e<-function(type,id,p,at=now)rival_finance_evidence('u',cid,'unknown',type,id,p,at)
scores<-list(c(4,9,8,7,6,5,3,2,1),c(1,9,8,7,6,5,4,3,2),c(2,9,8,7,6,5,4,3,1))
histories<-lapply(seq_len(9),function(i)e('team_rounds',ids[i],list(team_id=ids[i],rounds=lapply(seq_along(scores),function(j)
  list(id=c('r1','r2','r8')[j],number=c(1,2,8)[j],points=scores[[j]][i],status='closed',negative=FALSE)))))
inactive<-e('team_rounds','j',list(team_id='j',rounds=list()))
payment<-function(id,round,money,team='a',at=now)e('movements',paste(team,id,sep=':'),
  list(id=id,team_id=team,type='ranking',category='bonus',concept=as.character(round),money=money,date=rival_finance_stamp(at)),at)
payments<-list(payment('rank1',1,4000000),payment('rank2',2,6000000))
evidence<-c(histories,list(inactive),payments)
resolve<-function(rows=evidence)rival_ranking_participants(rows,ids,rules)
build<-function(rows=evidence)build_rival_cash_dataset(teams,cid,rows,rules=rules,account_id='u',now=now)
check('explicit nonparticipant histories and independent exact payments recover unpaid historical rounds',{
  p<-resolve();stopifnot(identical(p$team_ids,ids[1:9]),identical(p$excluded,'j'),setequal(p$verified_rounds,c('r1','r2')))
  out<-build();a<-out$cash_ledgers$a;ranking<-a[a$bonus_type=='ranking',]
  stopifnot(out$team_finances$ranking_prize[1]==15333333,out$team_finances$estimated_cash[1]==115333333,
    nrow(ranking)==3L,sum(!ranking$estimated)==2L,sum(ranking$estimated)==1L,
    ranking$amount[ranking$round_id=='r8']==5333333,
    grepl('matching recorded position payments in 2 rounds',ranking$source[ranking$round_id=='r8'],fixed=TRUE),
    !any(grepl('Ranking history incomplete',out$team_finances$estimate_notes)),
    !any(out$cash_ledgers$j$bonus_type=='ranking'),out$team_finances$estimated_cash[10]==100000000)
  stopifnot(sum(a$amount[a$included])==tail(a$running_balance,1),tail(a$running_balance,1)==out$team_finances$estimated_cash[1])
})
check('missing malformed nonempty and unknown-eligibility histories cannot establish nonparticipation',{
  variants<-list(c(histories,payments))
  for(value in list(NULL,'malformed',list(error=TRUE),list(list(bad='record')))){
    bad<-inactive;bad$payload$rounds<-value;variants<-c(variants,list(c(histories,list(bad),payments)))
  }
  active<-inactive;active$payload$rounds<-list(list(id='future',number=12,points=0,status='next',negative=FALSE))
  variants<-c(variants,list(c(histories,list(active),payments)))
  unknown<-evidence;unknown[[2]]$payload$rounds[[1]]$negative<-NA;unknown[[2]]$payload$rounds[[2]]$negative<-NA
  variants<-c(variants,list(unknown))
  for(rows in variants)stopifnot(!length(resolve(rows)$excluded))
})
check('one round replay partial payments and contradictions never prove a reduced denominator',{
  one<-c(histories,list(inactive),payments[1],payments[1])
  stopifnot(!length(resolve(one)$excluded))
  for(amount in c(1000000,6000001,NA_real_)){
    changed<-payments[[2]];changed$payload$money<-amount
    stopifnot(!length(resolve(c(histories,list(inactive),payments[1],list(changed)))$excluded))
  }
  stopifnot(!length(resolve(c(evidence,list(payment('inactive-payment',1,666667,'j'))))$excluded))
  out<-build(c(histories,list(inactive),payments[1]))
  stopifnot(out$team_finances$ranking_prize[2]==0,grepl('Ranking history incomplete',out$team_finances$estimate_notes[2]))
})
check('latest payment revisions and distinct split payments count once and remain reusable after JSON storage',{
  wrong<-payment('rank1',1,1,at=now-1)
  rows<-c(histories,list(inactive,wrong),payments,payments)
  merged<-rival_merge_evidence(list(),rows)
  stopifnot(length(resolve(merged)$verified_rounds)==2L,build(rows)$team_finances$ranking_prize[1]==15333333)
  split<-c(histories,list(inactive,payment('first',1.1,1000000),payment('second',1.2,3000000)),payments[2])
  stopifnot(length(resolve(split)$verified_rounds)==2L,build(split)$team_finances$ranking_prize[1]==15333333)
  saved<-jsonlite::fromJSON(jsonlite::toJSON(merged,auto_unbox=TRUE,null='null'),simplifyVector=TRUE)
  decoded<-rival_evidence_decode(saved)
  stopifnot(identical(resolve(decoded)$team_ids,ids[1:9]),build(decoded)$team_finances$ranking_prize[1]==15333333)
})
check('a missing active round remains incomplete and an official ranking retains priority',{
  partial<-evidence;partial[[2]]$payload$rounds<-partial[[2]]$payload$rounds[1:2]
  stopifnot(identical(resolve(partial)$excluded,'j'))
  out<-build(partial);stopifnot(out$team_finances$ranking_prize[3]==2666666,
    grepl('Ranking history incomplete for round 8',out$team_finances$estimate_notes[3],fixed=TRUE))
  source<-rival_saved_rounds(evidence,include_ranking=FALSE)
  ranking<-e('round_ranking','r8',list(round_id='r8',round_number=8,status='closed',
    ranking=lapply(seq_along(ids),function(i)list(id=ids[i],points=c(scores[[3]],0)[i])),
    signature=rival_ranking_signature(source,'r8',ids),source_teams=as.list(ids)))
  out<-build(c(evidence,list(ranking)));r8<-out$cash_ledgers$a[out$cash_ledgers$a$round_id=='r8'&out$cash_ledgers$a$bonus_type=='ranking',]
  stopifnot(nrow(r8)==1L,r8$amount==4363636,grepl('Official historical round ranking',r8$source,fixed=TRUE))
})
check('the shared queue skips redundant ranking reads and reuses supported participants across navigation',{
  local({
    adapter<-async_adapter_fetch;persist<-persist_rival_finance_evidence
    on.exit({async_adapter_fetch<<-adapter;persist_rival_finance_evidence<<-persist})
    calls<-character();persist_rival_finance_evidence<<-function(...)TRUE
    async_adapter_fetch<<-function(fn,args,context,...){
      calls<<-c(calls,fn)
      result<-switch(fn,get_rival_finance_saved=list(evidence=evidence,complete=TRUE),
        get_rival_finance_seed=list(rules=rules,pressroom=data.frame()),
        get_rival_team_finance_observation=list(team_id=args$user_team_id,info=NULL,movements=NULL,rounds=NULL),
        get_rival_round_lineup_observation=list(team_id=args$user_team_id,round_id=args$round_id,answer=NULL),
        get_rival_round_notification_page=list(status='ok',rows=data.frame(),next_cursor='',count=0L))
      if(fn=='get_rival_round_ranking_observation')stop('redundant ranking request')
      promises::promise_resolve(result)
    }
    testServer(function(input,output,session){
      active<-reactiveVal(TRUE)
      source<-shared_rival_finance_source(session,function()c(userid='u',token='synthetic'),function()cid,function()'a',
        function()teams,active,function()list(rows=data.frame(),complete=TRUE))
    },{
      for(i in seq_len(100L)){later::run_now(0);session$flushReact()}
      stopifnot(!source()$finance_loading,source()$team_finances$ranking_prize[1]==15333333,
        !any(calls=='get_rival_round_ranking_observation'))
      before<-length(calls);active(FALSE);session$flushReact();active(TRUE);session$flushReact()
      stopifnot(length(calls)==before)
    })
  })
})
cat(sprintf('Inactive rival participants: %d passed / 0 failed\n',n))
