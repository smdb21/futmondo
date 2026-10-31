#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R');library(shiny)})
n<-0L
check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
build_scoped<-build_rival_cash_dataset
build_rival_cash_dataset<-function(...,account_id='u')build_scoped(...,account_id=account_id)
cid<-rival_cash_league_id;now<-as.POSIXct('2026-10-31T12:00:00',tz='UTC')
teams<-data.frame(teamid=c('a','b'),teamname=c('A','B'),points=c(100,200))
e<-function(type,id,p,at=now,season='unknown')rival_finance_evidence('u',cid,season,type,id,p,at)
m<-function(id,money,type='bonus',bonus='',round='',player='',at=now)e('movements',paste('a',id,sep=':'),list(id=id,team_id='a',money=money,type=type,category='',concept=id,date=rival_finance_stamp(at),bonus_type=bonus,round_id=round,player_id=player),at)
check('movement decoder preserves signed/missing amounts and identities without interpreting descriptions',{
  x<-normalize_money_movements(list(list(`_id`='x',money=0,date='d',concept='MVP'),list(`_id`='y',money=-5)))
  stopifnot(identical(x$money,c(0,-5)),all(x$bonus_type==''),isTRUE(attr(x,'complete')))
  x<-normalize_money_movements(list(list(`_id`='x')));stopifnot(is.na(x$money),!attr(x,'complete'))
  stopifnot(!attr(normalize_money_movements(list(error=TRUE)),'complete'))
})
check('latest correction replaces one movement; repeated notification is not cash',{
  rows<-list(m('x',10),m('x',15,at=now+1),e('notifications','n',list(round_id='r',message='points published')))
  out<-build_rival_cash_dataset(teams,cid,evidence=rows,now=now,account_id='u')$team_finances
  stopifnot(out$estimated_cash[1]==300000015,out$estimated_cash[2]==300000000)
})
check('full ideal XI and separate MVP use historical eligibility, replaced selections remove former recipients',{
  rules<-list(initial_budget=100,dreamteam_reward=10,mvp_reward=20,money_per_point=0,ranking_pool=0)
  official<-data.frame(round_id='r',player_id=c('p','q'),is_mvp=c(TRUE,FALSE),is_finished=TRUE)
  eligible<-data.frame(round_id='r',player_id=c('p','q'),team_id=c('a','b'),eligible=TRUE,verified=TRUE)
  first<-build_rival_cash_dataset(teams,cid,official=official,rules=rules,eligibility=eligible,coverage_complete=TRUE)$team_finances
  stopifnot(identical(first$estimated_cash,c(130,110)))
  revised<-official[2,,drop=FALSE];revised$is_mvp<-TRUE
  last<-build_rival_cash_dataset(teams,cid,official=revised,rules=rules,eligibility=eligible,coverage_complete=TRUE)$team_finances
  stopifnot(identical(last$estimated_cash,c(100,130)))
  paid<-list(m('pay',35,bonus='mvp',round='r',player='p'))
  actual<-build_rival_cash_dataset(teams,cid,paid,official=official,rules=rules,eligibility=eligible)$team_finances
  stopifnot(actual$estimated_cash[1]==145) # 100 + recorded35 + only separate XI10
})
check('unclassified income blocks overlapping reward estimates and movement transfers never add pressroom twice',{
  official<-data.frame(round_id='r',player_id='p',is_mvp=TRUE,is_finished=TRUE)
  eligibility<-data.frame(round_id='r',player_id='p',team_id='a',eligible=TRUE,verified=TRUE)
  tx<-data.frame(id='tx',buyer_team_id='a',seller_team_id='',price=50,player_id='p')
  out<-build_rival_cash_dataset(teams,cid,list(m('buy',-50,'buy'),m('award',30)),official,tx,list(initial_budget=100,mvp_reward=20,dreamteam_reward=10),eligibility=eligibility)$team_finances
  stopifnot(out$estimated_cash[1]==80,grepl('overlapping',out$estimate_notes[1]))
})
check('partial point settlement replaces estimated component and extra payment adds once',{
  p<-list(team_id='a',round_id='r',verified=TRUE,points_credited=TRUE,credited_points=3,money_per_point=10,historical_rules=TRUE,round_settled=FALSE)
  out<-build_rival_cash_dataset(teams,cid,list(e('eligibility','points-r',p)),rules=list(initial_budget=100))$team_finances
  stopifnot(out$estimated_cash[1]==130)
  out<-build_rival_cash_dataset(teams,cid,list(e('eligibility','points-r',p),m('pay1',30,bonus='points',round='r'),m('pay2',10,bonus='points',round='r')),rules=list(initial_budget=100))$team_finances
  stopifnot(out$estimated_cash[1]==140,out$point_bonus[1]==40)
})
check('own observed cash remains zero or negative and mismatch is not silently balanced',{
  rows<-list(e('team','a',list(budget=0,observed_at=rival_finance_stamp(now))),e('team','b',list(budget=-5,observed_at=rival_finance_stamp(now))))
  out<-build_rival_cash_dataset(teams,cid,rows,rules=list(initial_budget=100),now=now)$team_finances
  stopifnot(identical(out$estimated_cash,c(0,-5)),all(out$confidence=='Verified'),out$cash_status[2]=='Confirmed debt',out$reconciliation_difference[1]==-100)
  stale<-build_rival_cash_dataset(teams,cid,rows,rules=list(initial_budget=100),now=now+3600)$team_finances
  stopifnot(all(stale$confidence=='Estimated'),all(stale$estimated_cash==100))
})
check('session shared async ledger reads database once across navigation and resets on account change',{
  oldadapter<-async_adapter_fetch;oldsaved<-get_rival_finance_saved;oldseed<-get_rival_finance_seed;oldteam<-get_rival_team_finance_observation
  reads<-0L;pending<-0L;maximum<-0L;team_reads<-0L;heartbeat<-FALSE
  async_adapter_fetch<-function(fn,args,context,...){
    pending<<-pending+1L;maximum<<-max(maximum,pending)
    promises::promise(function(resolve,reject)later::later(function(){
      pending<<-pending-1L;resolve(do.call(get(fn,.GlobalEnv),args))
    },.05))
  }
  get_rival_finance_saved<-function(...){reads<<-reads+1L;list(evidence=list(),defaults=NULL,complete=TRUE)}
  get_rival_finance_seed<-function(...)list(rules=list(initial_budget=100,season='unknown'),pressroom=data.frame())
  get_rival_team_finance_observation<-function(login,championship_id,user_team_id){team_reads<<-team_reads+1L;list(team_id=user_team_id,info=NULL,movements=NULL)}
  settle<-function(session,source){deadline<-Sys.time()+3;repeat{session$flushReact();later::run_now(timeoutSecs=.01);session$flushReact();if(!isTRUE(shiny::isolate(source()$loading)))break;if(Sys.time()>deadline)stop('timeout')}}
  testServer(function(input,output,session){enabled<-reactiveVal(FALSE);auth<-reactiveVal(c(userid='u',token='t'));cidv<-reactiveVal(cid);source<-shared_rival_finance_source(session,auth,cidv,function()'a',function()teams,enabled,function()list(rows=data.frame(),complete=FALSE))},{
    session$flushReact();stopifnot(reads==0)
    enabled(TRUE);session$flushReact()
    later::later(function(){heartbeat<<-TRUE},.005);later::run_now(timeoutSecs=.015)
    stopifnot(heartbeat,isTRUE(shiny::isolate(source()$loading)))
    settle(session,source);stopifnot(reads==1,team_reads==2,nrow(source()$team_finances)==2,maximum==2L)
    enabled(FALSE);session$flushReact();enabled(TRUE);settle(session,source);stopifnot(reads==1,team_reads==2)
    auth(c(userid='v',token='t'));settle(session,source);stopifnot(reads==2,team_reads==4)
    auth(NULL);session$flushReact();stopifnot(nrow(source()$team_finances)==0)
  })
  async_adapter_fetch<-oldadapter;get_rival_finance_saved<-oldsaved;get_rival_finance_seed<-oldseed;get_rival_team_finance_observation<-oldteam
})
check('multiple player records never multiply one team-round settlement',{
  p<-list(team_id='a',round_id='r',verified=TRUE,eligible=TRUE,points_credited=TRUE,credited_points=3,money_per_point=10,historical_rules=TRUE)
  p1<-p;p1$player_id<-'p';p2<-p;p2$player_id<-'q'
  rows<-list(e('eligibility','p-r',p1),e('eligibility','q-r',p2))
  out<-build_rival_cash_dataset(teams,cid,rows,rules=list(initial_budget=100))$team_finances
  stopifnot(out$estimated_cash[1]==130)
  p2$credited_points<-4
  out<-build_rival_cash_dataset(teams,cid,list(e('eligibility','p-r',p1),e('eligibility','q-r',p2)),rules=list(initial_budget=100))$team_finances
  stopifnot(out$estimated_cash[1]==100,grepl('Conflicting',out$estimate_notes[1]))
})
check('missing account fails closed before consuming private evidence',{
  stopifnot(inherits(try(build_scoped(teams,cid,list(m('private',10))),silent=TRUE),'try-error'))
})
cat(sprintf('Rival finance: %d passed / 0 failed\n',n))
