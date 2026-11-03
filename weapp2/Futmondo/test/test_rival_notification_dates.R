#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R');library(shiny)})
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
cid<-'notification-fixture';stamp<-'2026-11-02T12:00:00Z'
raw_notice<-function(id,round='r1',number='1',league=cid,type='round',action='close',date='2026-10-27T14:21:52.133Z')
 list(`_id`=id,type=type,action=action,created=date,updated=date,directObject=list(id=round,name=number),context=list(id=league,name='Fixture league'))
normalized<-function(...){x<-normalize_notifications(list(...));attr(x,'observed_at')<-fm_time(stamp);x}
check('official notification page transport uses oldest ID cursor and preserves round/created contract',{
 local({
  old<-fm_async_read_http;offline<-getOption('futmondo.offline');on.exit({fm_async_read_http<<-old;options(futmondo.offline=offline)})
  options(futmondo.offline=FALSE);calls<-0L
  fm_async_read_http<<-function(url,method='GET',body=NULL,headers=character(),owner='',...){
   request<-jsonlite::fromJSON(body);calls<<-calls+1L
   stopifnot(url=='https://api.futmondo.com/1/notification/list',request$query$to=='cursor-latest',method=='POST')
   promises::promise_resolve(list(status_code=200L,headers=list(),content=charToRaw(jsonlite::toJSON(list(answer=list(items=list(raw_notice('close-r1')))),auto_unbox=TRUE))))
  }
  result<-NULL;failure<-NULL;auth<-c(userid='u',token='fixture-only')
  p<-async_adapter_fetch('get_rival_round_notification_page',list(login=auth,championship_id=cid,user_team_id='a',cursor='cursor-latest'),resource_context(auth,cid,'a',session_id='notice-test'))
  promises::then(p,function(x)result<<-x,function(e)failure<<-e);deadline<-Sys.time()+3
  while(is.null(result)&&is.null(failure)&&Sys.time()<deadline)later::run_now(.01)
  if(!is.null(failure))stop(failure)
  stopifnot(calls==1L,result$status=='ok',result$cursor=='close-r1',result$notifications$player_id=='r1',
    result$notifications$player_name=='1',result$notifications$created_at=='2026-10-27T14:21:52.133Z')
  evidence<-rival_notification_evidence(result$notifications,'u',cid,'unknown',result$observed_at)
  stopifnot(rival_round_publication_dates(evidence,data.frame(round_id='r1',round_number=1))$date=='2026-10-27T14:21:52.133Z')
 })
})
check('malformed, repeated and empty pages stop without manufacturing a cursor or timestamp',{
 local({
  old<-notification_request;on.exit(notification_request<<-old)
  for(case in c('empty','repeated','malformed')){
   notification_request<<-function(...)fetch_result(list(items=switch(case,empty=list(),repeated=list(raw_notice('cursor')),malformed=list(list(type='round')))))
   x<-get_rival_round_notification_page(c(userid='u',token='fixture'),cid,'a','cursor')
   stopifnot(x$status==switch(case,empty='empty',repeated='repeated',malformed='unavailable'))
  }
  stopifnot(length(rival_notification_evidence(data.frame(),'u',cid,'unknown',NULL))==0L)
 })
})
teams<-data.frame(teamid=c('a','b'),teamname=c('A','B'),points=10)
rules<-list(initial_budget=100,money_per_point=2,ranking_pool=0,dreamteam_reward=0,mvp_reward=0,season='unknown')
saved_rounds<-lapply(c('a','b'),function(id)rival_finance_evidence('u',cid,'unknown','team_rounds',id,
 list(team_id=id,rounds=list(list(id='r1',number=1,points=5,status='closed',negative=FALSE),list(id='r2',number=2,points=5,status='closed',negative=FALSE))),stamp))
page<-function(rows,cursor,status='ok')list(notifications=rows,cursor=cursor,status=status,observed_at=stamp)
run_source<-function(saved,pages,configured=rules){
 old_adapter<-async_adapter_fetch;old_persist<-persist_rival_finance_evidence
 on.exit({async_adapter_fetch<<-old_adapter;persist_rival_finance_evidence<<-old_persist})
 history_calls<-character();stored<-saved;calls<-character();out<-NULL
 persist_rival_finance_evidence<<-function(rows,...){stored<<-rival_merge_evidence(stored,rows);invisible(TRUE)}
 async_adapter_fetch<<-function(fn,args,context,...){
  calls<<-c(calls,fn)
  result<-switch(fn,get_rival_finance_saved=list(evidence=saved,complete=TRUE),
   get_rival_finance_seed=list(info=NULL,rules=configured,pressroom=data.frame(),notifications=normalized(raw_notice('head',type='market',action='acceptBid',league='other'))),
   get_rival_team_finance_observation=list(team_id=args$user_team_id,info=NULL,movements=NULL,rounds=NULL),
   get_rival_round_notification_page={history_calls<<-c(history_calls,args$cursor);stopifnot(args$cursor%in%names(pages));pages[[args$cursor]]})
  promises::promise_resolve(result)
 }
 testServer(function(input,output,session){
  enabled<-reactiveVal(TRUE)
  source<-shared_rival_finance_source(session,function()c(userid='u',token='fixture'),function()cid,function()'a',function()teams,enabled,function()list(rows=data.frame(),complete=TRUE))
 },{
  for(i in seq_len(120L)){later::run_now(0);session$flushReact()}
  stopifnot(!source()$loading);out<<-source()
  before<-length(calls);enabled(FALSE);session$flushReact();enabled(TRUE);session$flushReact()
  for(i in seq_len(5L)){later::run_now(0);session$flushReact()}
  stopifnot(length(calls)==before)
 })
 list(result=out,stored=stored,cursors=history_calls)
}
check('only missing rounds page backwards; unrelated league advances cursor and saved dates eliminate replay',{
 pages<-list(head=page(normalized(raw_notice('pub-r2','r2','2'),raw_notice('older',league='other',type='market',action='acceptBid')),'older'),
  older=page(normalized(raw_notice('pub-r1')),'pub-r1'))
 first<-run_source(saved_rounds,pages)
 stopifnot(identical(first$cursors,c('head','older')),all(first$result$team_finances$estimated_cash==120))
 bonuses<-first$result$cash_ledgers$b[first$result$cash_ledgers$b$bonus_type=='points',]
 stopifnot(nrow(bonuses)==2L,all(bonuses$payment_date_estimated),all(nzchar(bonuses$date)))
 restored<-rival_evidence_decode(jsonlite::fromJSON(jsonlite::toJSON(first$stored,auto_unbox=TRUE,null='null')))
 second<-run_source(restored,list())
 stopifnot(length(second$cursors)==0L,identical(first$result$cash_ledgers$b$date,second$result$cash_ledgers$b$date))
})
check('usable shared payment anchors avoid notification history when no point rewards need a date',{
 configuration<-rules;configuration$money_per_point<-0;configuration$ranking_pool<-30
 configuration$ranking_mode<-'top';configuration$users_to_rank<--1
 payment<-function(id,round,concept)rival_finance_evidence('u',cid,'unknown','movements',id,
   list(id=id,team_id='a',type='ranking',category='bonus',concept=concept,money=20,date='2026-10-28T12:00:00Z',round_id=round),stamp)
 saved<-c(saved_rounds,list(payment('rank1','r1','1'),payment('rank2','r2','2')))
 anchored<-run_source(saved,list(),configuration)
 stopifnot(length(anchored$cursors)==0L)
 saved[[length(saved)-1L]]$payload$concept<-'1.1'
 fallback<-run_source(saved,list(head=page(normalized(raw_notice('pub-r1')),'pub-r1')),configuration)
 stopifnot(identical(fallback$cursors,'head'))
})
check('a failed older page preserves successful dates and amounts; empty history is saved and not rescanned',{
 partial<-run_source(saved_rounds,list(head=page(normalized(raw_notice('pub-r2','r2','2')),'older'),
  older=list(status='unavailable',notifications=data.frame(),cursor='older')))
 bonus<-partial$result$cash_ledgers$b[partial$result$cash_ledgers$b$bonus_type=='points',]
 stopifnot(identical(partial$cursors,c('head','older')),all(partial$result$team_finances$estimated_cash==120),
   nzchar(bonus$date[bonus$round_id=='r2']),bonus$date[bonus$round_id=='r1']=='')
 empty<-run_source(saved_rounds,list(head=page(normalize_notifications(list()),'head','empty')))
 stopifnot(length(empty$cursors)==1L,length(run_source(empty$stored,list())$cursors)==0L)
 repeated<-run_source(saved_rounds,list(head=page(normalized(raw_notice('head',type='market',action='acceptBid')),'head','repeated')))
 stopifnot(length(repeated$cursors)==1L)
})
check('twenty-page cap persists a continuation cursor for the next requested refresh',{
 pages<-lapply(seq_len(20L),function(i)page(normalized(raw_notice(paste0('cursor',i),type='market',action='acceptBid',league='other')),paste0('cursor',i)))
 names(pages)<-c('head',paste0('cursor',seq_len(19L)))
 first<-run_source(saved_rounds,pages)
 stopifnot(length(first$cursors)==20L)
 scans<-Filter(function(x)x$source_id=='round-publication-scan',rival_evidence_rows(first$stored,'notifications'))
 stopifnot(length(scans)==1L,scans[[1]]$payload$status=='limit',scans[[1]]$payload$cursor=='cursor20')
 second<-run_source(first$stored,list(cursor20=page(normalized(raw_notice('pub-r2','r2','2',type='rounds'),raw_notice('pub-r1')),'pub-r1')))
 stopifnot(identical(second$cursors,'cursor20'),all(nzchar(second$result$cash_ledgers$b$date[second$result$cash_ledgers$b$bonus_type=='points'])))
})
cat(sprintf('Rival notification dates: %d passed / 0 failed\n',n))
