#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R');library(shiny)})
n<-0L
check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
cid<-rival_cash_league_id;now<-as.POSIXct('2026-10-31T12:00:00',tz='UTC')
e<-function(type,id,p,at=now)rival_finance_evidence('u',cid,'unknown',type,id,p,at)
teams<-data.frame(teamid=c('a','b','c'),teamname=c('A','B','C'),points=c(30,20,10))
rules<-list(initial_budget=300,money_per_point=2,ranking_pool=60,ranking_mode='top',users_to_rank=-1,dreamteam_reward=5,mvp_reward=10)
raw<-function(points,negative=FALSE,status='done')list(list(id='r1',number=1,points=points,status=status,negative=negative))
snapshot<-function(id,points,negative=FALSE,status='done',at=now)e('team_rounds',id,list(team_id=id,rounds=raw(points,negative,status)),at)
lineup<-function(id,players,negative=FALSE)e('round_lineup',paste(id,'r1',sep=':'),list(team_id=id,round_id='r1',players=as.list(players),bench=list(),negative=negative))
rows<-list(snapshot('a',30),snapshot('b',20),snapshot('c',10))
build<-function(evidence=rows,official=data.frame(),tx=data.frame(),ruleset=rules)build_rival_cash_dataset(teams,cid,evidence,official,tx,ruleset,coverage_complete=TRUE,account_id='u',now=now)
check('real round contract is stored in scoped full snapshots without synthetic settlement flags',{
  result<-rival_team_evidence(list(team_id='a',info=NULL,movements=NULL,rounds=raw(30)),'u',cid,'unknown')
  stopifnot(length(result)==1L,result[[1]]$evidence_type=='eligibility',result[[1]]$source_id=='rounds:a',result[[1]]$payload$kind=='team_rounds',
    rival_saved_rounds(result)$points==30)
})
check('only an explicitly successful empty round response can replace saved history',{
  blank<-function(answer)rival_team_evidence(list(team_id='a',info=NULL,movements=NULL,rounds=answer),'u',cid,'unknown')
  stopifnot(length(blank(NULL))==0L,length(blank(list()))==0L,length(blank(list(error=TRUE)))==0L,
    length(blank(list(list(id='r1'),list(invalid=TRUE))))==0L)
  validated<-structure(list(),valid_response=TRUE)
  empty<-blank(validated)
  stopifnot(length(empty)==1L,length(empty[[1]]$payload$rounds)==0L)
  empty[[1]]$observed_at<-rival_finance_stamp(now+1)
  saved<-rival_saved_rounds(rival_merge_evidence(rows,empty))
  stopifnot(!any(saved$team_id=='a'))
})
check('reader rejects top-level error envelopes instead of accepting their empty answer as a reset',{
  local({
    old_post<-futmondo_post;old_cache<-get_cached_data
    on.exit({futmondo_post<<-old_post;get_cached_data<<-old_cache})
    envelope<-list(error=TRUE,answer=list())
    get_cached_data<<-function(key,expr,...)eval(substitute(expr),parent.frame())
    futmondo_post<<-function(url,...){
      structure(list(status_code=200L,headers=list('Content-Type'='application/json; charset=utf-8'),
        content=charToRaw(jsonlite::toJSON(envelope,auto_unbox=TRUE)),url=url),class='response')
    }
    read<-function()get_user_team_rounds(c(userid='u',token='synthetic'),cid,'a')
    stopifnot(inherits(try(read(),silent=TRUE),'try-error'))
    envelope<-list(code='api.error.denied',answer=list())
    stopifnot(inherits(try(read(),silent=TRUE),'try-error'))
    envelope<-list(code=API_CODE_OK,answer=list())
    result<-read();stopifnot(length(result)==0L,isTRUE(attr(result,'valid_response')))
    envelope<-list(answer=list())
    result<-read();stopifnot(length(result)==0L,isTRUE(attr(result,'valid_response')))
  })
})
check('opening plus public transfer plus per-round points and top ranking reconstructs rival cash',{
  tx<-data.frame(id=c('t1','t2'),buyer_team_id=c('a',''),seller_team_id=c('','b'),price=c(50,10),player_id=c('p','q'))
  out<-build(tx=tx)$team_finances
  stopifnot(identical(out$reconstructed_cash,c(340,370,330)),identical(out$point_bonus,c(60,40,20)),identical(out$ranking_prize,c(30,20,10)),all(out$confidence=='Estimated'))
})
check('official distribution supports reverse ranking, ties, fixed positions and JS rounding',{
  stopifnot(identical(rival_ranking_allocations(c(30,20,10),60,'flop',-1),c(10,20,30)),
    identical(rival_ranking_allocations(c(30,30,10),60,'top',-1),c(25,25,10)),
    identical(rival_ranking_allocations(c(30,20,10),100,'top',3),c(50,30,20)),
    identical(rival_ranking_allocations(5:1,100,'top',5),c(40,30,15,10,5)),
    identical(rival_ranking_allocations(1,2.5,'top',-1),3))
})
check('negative starts receive no automatic premiums, open rounds never count as final income',{
  negative<-list(snapshot('a',30,TRUE),snapshot('b',20),snapshot('c',10))
  out<-build(negative)$team_finances
  stopifnot(out$point_bonus[1]==0,out$ranking_prize[1]==0,out$ranking_prize[2]==40,out$ranking_prize[3]==20)
  allopen<-list(snapshot('a',30,status='running'),snapshot('b',20,status='running'),snapshot('c',10,status='running'))
  out<-build(allopen)$team_finances
  # Open-only round history never turns into cumulative cash income.
  stopifnot(all(out$point_bonus==0),all(out$ranking_prize==0))
})
check('historical XI and separate MVP use starter or bench ownership and source negative flag',{
  official<-data.frame(round_id='r1',player_id=c('p','q'),is_mvp=c(TRUE,FALSE),is_finished=TRUE)
  out<-build(c(rows,list(lineup('a','p'),lineup('b','q'))),official)$team_finances
  stopifnot(identical(out$reconstructed_cash,c(405,365,330)),out$dreamteam_income[1]==5,out$mvp_income[1]==10)
  neg<-build(c(rows,list(lineup('a','p',TRUE))),official)$team_finances
  stopifnot(neg$dreamteam_income[1]==0,neg$mvp_income[1]==0)
})
check('lineup negative flag also gates points and ranking when the rounds list omits eligibility',{
  unknown<-rows;unknown[[1]]$payload$rounds[[1]]$negative<-NULL
  out<-build(c(unknown,list(lineup('a','p',TRUE))))$team_finances
  stopifnot(out$point_bonus[1]==0,out$ranking_prize[1]==0,out$reconstructed_cash[1]==300)
})
check('duplicate award evidence is idempotent and conflicting ownership or negative eligibility blocks credit',{
  official<-data.frame(round_id='r1',player_id='p',is_mvp=TRUE,is_finished=TRUE)
  legacy<-e('eligibility','legacy',list(round_id='r1',player_id='p',team_id='a',eligible=TRUE,verified=TRUE))
  out<-build(c(rows,list(lineup('a','p'),legacy,legacy)),official)$team_finances
  stopifnot(out$dreamteam_income[1]==5,out$mvp_income[1]==10)
  legacy$payload$eligible<-FALSE
  out<-build(c(rows,list(lineup('a','p'),legacy)),official)$team_finances
  stopifnot(out$dreamteam_income[1]==0,out$mvp_income[1]==0,grepl('Conflicting',out$estimate_notes[1]))
  legacy$payload$eligible<-TRUE;legacy$payload$team_id<-'b'
  out<-build(c(rows,list(lineup('a','p'),legacy)),official)$team_finances
  stopifnot(all(out$dreamteam_income==0),all(out$mvp_income==0))
})
check('repeated and postponed revisions replace reconstructed components rather than adding money twice',{
  out<-build(c(rows,list(snapshot('a',40,at=now+1),snapshot('a',40,at=now+2))))$team_finances
  stopifnot(out$point_bonus[1]==80,out$ranking_prize[1]==30,out$reconstructed_cash[1]==410)
  # A full revised list no longer contains r1: its old points disappear.
  replacement<-e('team_rounds','a',list(team_id='a',rounds=list(list(id='r2',number=2,points=8,status='done',negative=FALSE))),now+3)
  saved<-rival_saved_rounds(rival_merge_evidence(rows,list(replacement)))
  stopifnot(identical(saved$round_id[saved$team_id=='a'],'r2'))
})
check('recorded point payment and unclassified manual income never double count derived bonuses',{
  m<-e('movements','a:p',list(team_id='a',id='p',money=60,type='bonus',bonus_type='points',round_id='r1'))
  out<-build(c(rows,list(m)))$team_finances
  stopifnot(out$point_bonus[1]==60,out$reconstructed_cash[1]==390)
  m$payload$bonus_type<-''
  out<-build(c(rows,list(m)))$team_finances
  stopifnot(out$reconstructed_cash[1]==360,grepl('overlapping',out$estimate_notes[1]))
})
check('signed negative fees remain adjustments without blocking reconstructed round rewards',{
  fee<-e('movements','a:fee',list(team_id='a',id='fee',money=-7,type='fee',concept='coach'))
  out<-build(c(rows,list(fee)))$team_finances
  stopifnot(out$reconstructed_cash[1]==383,out$point_bonus[1]==60,out$ranking_prize[1]==30)
})
check('one historical lineup request uses captured public client contract and persists source IDs',{
  old<-mvp_api_answer
  mvp_api_answer<-function(login,url,query){
    stopifnot(url=='https://api.futmondo.com/1/userteam/roundlineup',identical(query,list(championshipId=cid,userteamId='a',round='r1')))
    list(status='ok',answer=list(players=list(list(id='p')),bench=list(players=list(list(id='q'))),negative=FALSE,status='done'))
  }
  result<-get_rival_round_lineup_observation(c(userid='u',token='t'),cid,'a','r1','sig')
  result<-rival_lineup_evidence(result,'u',cid,'unknown')
  mvp_api_answer<-old
  stopifnot(result[[1]]$evidence_type=='eligibility',result[[1]]$source_id=='lineup:a:r1',identical(result[[1]]$payload$players,list('p')),identical(result[[1]]$payload$bench,list('q')))
})
check('saved final lineups are reused and only changed round signatures enqueue new bounded requests',{
  local({
    old_adapter<-async_adapter_fetch;old_persist<-persist_rival_finance_evidence
    on.exit({async_adapter_fetch<<-old_adapter;persist_rival_finance_evidence<<-old_persist})
    persist_rival_finance_evidence<<-function(...)TRUE
    calls<-character();inflight<-0L;maximum<-0L
    awards<-data.frame(round_id='r1',player_id='p',is_mvp=TRUE,is_finished=TRUE)
    saved_lineup<-lineup('a','p');saved_lineup$payload$signature<-rival_round_signature(rival_team_round_rows(raw(30),'a'),awards)
    async_adapter_fetch<<-function(fn,args,context,...){
      calls<<-c(calls,fn);inflight<<-inflight+1L;maximum<<-max(maximum,inflight)
      promises::promise(function(resolve,reject)later::later(function(){
        inflight<<-inflight-1L
        result<-switch(fn,get_rival_finance_saved=list(evidence=c(rows,list(saved_lineup)),defaults=NULL),
          get_rival_finance_seed=list(info=NULL,rules=rules,pressroom=data.frame()),
          get_rival_team_finance_observation=list(team_id=args$user_team_id,info=NULL,movements=NULL),
          get_rival_round_lineup_observation=list(team_id=args$user_team_id,round_id=args$round_id,signature=args$signature,
            answer=list(players=list(list(id='p')),bench=list(players=list()),negative=FALSE,status='done')))
        resolve(result)
      },.001))
    }
    settle<-function(session,source){
      deadline<-Sys.time()+4
      repeat{later::run_now(.01);session$flushReact();if(!isTRUE(shiny::isolate(source()$finance_loading)))break;if(Sys.time()>deadline)stop('historical queue timed out')}
    }
    testServer(function(input,output,session){
      enabled<-reactiveVal(TRUE);official<-reactiveVal(list(rows=awards,complete=TRUE))
      source<-shared_rival_finance_source(session,function()c(userid='u',token='t'),function()cid,function()'a',function()teams,enabled,official)
    },{
      session$flushReact();settle(session,source)
      stopifnot(sum(calls=='get_rival_round_lineup_observation')==2L,maximum<=2L,!source()$finance_loading)
      before<-length(calls);enabled(FALSE);session$flushReact();enabled(TRUE);session$flushReact();settle(session,source)
      stopifnot(length(calls)==before)
      revised<-awards;revised$player_id<-'q';official(list(rows=revised,complete=TRUE));session$flushReact();settle(session,source)
      stopifnot(sum(calls=='get_rival_round_lineup_observation')==5L,maximum<=2L)
      before<-length(calls);official(list(rows=revised,complete=TRUE));session$flushReact();settle(session,source)
      stopifnot(length(calls)==before)
    })
  })
})
cat(sprintf('Rival reconstruction: %d passed / 0 failed\n',n))
