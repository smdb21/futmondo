options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R');source('Modules/Players_Table_Module.R');source('Modules/Rivals_Module.R')})
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
stamp<-as.POSIXct('2026-11-03T12:00:00',tz='UTC')
e<-function(type,id,p,at=stamp)rival_finance_evidence('fixture-account','fixture-league','unknown',type,id,p,at)
scores<-c(82,68,60,89,90,80,77,60,72,75,95)
rounds<-function(id,pts=scores) e('team_rounds',id,list(team_id=id,rounds=lapply(seq_along(pts),function(i)list(id=paste0('r',i),number=i,points=pts[i],status=if(i==11)'running'else'closed',negative=FALSE))))
raw<-jsonlite::fromJSON('test/point_settlements.json',simplifyVector=FALSE)
payments<-lapply(raw,function(x)e('movements',x$`_id`,c(list(id=x$`_id`,team_id='a'),x[setdiff(names(x),'_id')])))
evidence<-c(list(rounds('a'),rounds('b')),payments)
rules<-list(initial_budget=100,money_per_point=70000,ranking_pool=0,dreamteam_reward=0,mvp_reward=0)
teams<-data.frame(teamid=c('a','b'),teamname=c('A','B'),points=c(NA_real_,NA_real_))
build<-function(ev=evidence)build_rival_cash_dataset(teams,'fixture-league',ev,rules=rules,account_id='fixture-account',reference_team_id='a')
check('known answers validate ten completed rounds and preserve actual instalments',{
 legacy<-e('eligibility','old-partial',list(team_id='b',round_id='r1',verified=TRUE,points_credited=TRUE,credited_points=1,money_per_point=70000,historical_rules=TRUE,round_settled=FALSE))
 x<-build(c(evidence,list(legacy)));v<-x$point_reconciliation
 stopifnot(sum(v$status=='validated')==10,v$recorded[v$round_id=='r1']==5740000,v$expected[v$round_id=='r6']==5600000,
  v$status[v$round_id=='r11']=='unverified_history')
 a<-x$cash_ledgers$a;b<-x$cash_ledgers$b
 stopifnot(sum(a$bonus_type=='points')==14,sum(b$bonus_type=='points')==10,
  b$date[b$round_id=='r1']=='2026-08-28T09:48:04.049Z',b$date[b$round_id=='r6']=='2026-10-22T13:47:08.654Z',
  b$amount[b$round_id=='r1']==5740000,!any(b$payment_date_estimated[b$bonus_type=='points']),all(b$payment_date_source[b$bonus_type=='points']=='league_point_payment'))
 for(id in teams$teamid)stopifnot(tail(x$cash_ledgers[[id]]$running_balance,1)==x$team_finances$estimated_cash[x$team_finances$teamid==id])
})
check('fresh history resolves round eleven without inferring scores from payments',{
 ev<-evidence
 for(i in 1:2){ev[[i]]$payload$rounds[[11]]$points<-105;ev[[i]]$payload$rounds[[11]]$status<-'closed'}
 x<-build(ev);stopifnot(all(x$point_reconciliation$status=='validated'),sum(x$cash_ledgers$b$bonus_type=='points')==11)
 ev[[1]]$payload$rounds[[11]]$points<-104
 x<-build(ev);stopifnot(x$point_reconciliation$status[x$point_reconciliation$round_id=='r11']=='mismatch',!any(x$cash_ledgers$b$round_id=='r11'))
})
check('revisions replace by ID; shuffled duplicates do not multiply payments',{
 x<-build(c(rev(evidence),payments));stopifnot(x$point_reconciliation$recorded[x$point_reconciliation$round_id=='r1']==5740000)
 changed<-payments[[1]];changed$observed_at<-rival_finance_stamp(stamp+1);changed$payload$money<-1
 x<-build(c(evidence,list(changed)));stopifnot(x$point_reconciliation$status[x$point_reconciliation$round_id=='r1']=='mismatch',!any(x$cash_ledgers$b$round_id=='r1'))
})
check('unknown eligibility blocks rivals; signed negative rewards survive',{
 ev<-evidence;ev[[2]]$payload$rounds[[1]]$negative<-NULL
 stopifnot(!any(build(ev)$cash_ledgers$b$round_id=='r1'))
 ev<-evidence;ev[[2]]$payload$rounds[[1]]$points<--2
 b<-build(ev)$cash_ledgers$b;stopifnot(b$amount[b$round_id=='r1']==-140000)
})
check('scope isolation never borrows reference payments from another context',{
 for(field in c('account_id','championship_id','season')){
  ev<-evidence;for(i in seq_along(ev)[-(1:2)])ev[[i]][[field]]<-'other'
  stopifnot(!any(build(ev)$cash_ledgers$b$bonus_type=='points'))
 }
})
check('payment arrivals replace one cumulative rival entry and never invent instalments',{
 snapshot<-function(id,points,at)e('team_rounds',id,list(team_id=id,rounds=list(
  list(id='r1',number=1,points=points,status='closed',negative=FALSE))),at)
 first<-payments[[which(vapply(payments,function(p)identical(p$payload$concept,'1'),logical(1)))[1]]]
 second<-payments[[which(vapply(payments,function(p)identical(p$payload$concept,'1.1'),logical(1)))[1]]]
 legacy<-e('eligibility','discarded-interim',list(team_id='b',round_id='r1',verified=TRUE,
  points_credited=TRUE,credited_points=1,money_per_point=70000,historical_rules=TRUE,round_settled=FALSE))
 notification<-e('notifications','published',list(type='round',action='close',player_id='r1',created_at='2026-08-20T03:47:00Z'))
 saved<-list(snapshot('a',28,stamp),snapshot('b',20,stamp),legacy,notification)
 stopifnot(!any(build(saved)$cash_ledgers$b$bonus_type=='points'))
 saved<-rival_merge_evidence(saved,list(first))
 x<-build(saved);initial<-x$cash_ledgers$b[x$cash_ledgers$b$bonus_type=='points',]
 stopifnot(nrow(initial)==1L,initial$amount==1400000,initial$date==first$payload$date)
 # A newly observed payment must not be reconciled by altering the old score.
 saved<-rival_merge_evidence(saved,list(second))
 x<-build(saved)
 stopifnot(x$point_reconciliation$status=='mismatch',!any(x$cash_ledgers$b$bonus_type=='points'),
  sum(x$cash_ledgers$a$bonus_type=='points')==2L)
 # Independently updated cumulative scores replace the earlier rival entry.
 saved<-rival_merge_evidence(saved,list(snapshot('a',82,stamp+1),snapshot('b',84,stamp+1)))
 for(replay in 1:3){
  saved<-rival_merge_evidence(saved,list(first,second,legacy))
  x<-build(saved);b<-x$cash_ledgers$b[x$cash_ledgers$b$bonus_type=='points',]
  stopifnot(nrow(b)==1L,b$id==initial$id,b$amount==5880000,b$date==second$payload$date,
    sum(x$cash_ledgers$a$bonus_type=='points')==2L,
    rivals_inflow_totals(x$cash_ledgers$b)$points==5880000,
    tail(x$cash_ledgers$b$running_balance,1)==5880100)
 }
 # Rebuilding a fresh dataset from saved evidence produces the same single row.
 restored<-unserialize(serialize(saved,NULL));again<-build(restored)
 stopifnot(identical(again$cash_ledgers$b,x$cash_ledgers$b))
})
cat(sprintf('POINT SETTLEMENTS: %d passed / 0 failed\n',n))
check('bounded asynchronous repair persists fresh round eleven and reuses it next session',{
 original_adapter<-async_adapter_fetch;original_persist<-persist_rival_finance_evidence
 persisted<-evidence;counts<-c(a=0L,b=0L);busy<-maximum<-0L;beats<-0L
 persist_rival_finance_evidence<-function(rows,context){persisted<<-rival_merge_evidence(persisted,rows);TRUE}
 async_adapter_fetch<-function(fn,args,context,...){
  busy<<-busy+1L;maximum<<-max(maximum,busy)
  result<-switch(fn,get_rival_finance_saved=list(evidence=persisted,defaults=NULL),
   get_rival_finance_seed=list(info=NULL,rules=rules,pressroom=data.frame()),
   get_rival_team_finance_observation={
    id<-args$user_team_id;counts[id]<<-counts[id]+1L
    r<-rounds(id)$payload$rounds
    if(counts[id]>=2L){r[[11]]$points<-if(run==3L&&id=='a')104 else 105;r[[11]]$status<-'closed'}
    attr(r,'observed_at')<-stamp+counts[id]
    list(team_id=id,info=NULL,movements=NULL,rounds=r)
   },stop(paste('Unexpected request',fn)))
  promises::promise(function(resolve,reject)later::later(function(){busy<<-busy-1L;resolve(result)},.02))
 }
 for(run in 1:3){
  if(run==3L){stopifnot(all(counts==3L));persisted<-evidence;counts<-c(a=0L,b=0L)}
  shiny::testServer(function(input,output,session){
   value<-shared_rival_finance_source(session,function()c(userid='fixture-account',token='synthetic'),function()'fixture-league',function()'a',function()teams,function()TRUE,function()list(rows=data.frame(),complete=FALSE))
  },{
   session$flushReact();deadline<-Sys.time()+10
   repeat{later::run_now(.005);beats<<-beats+1L;session$flushReact();if(!isTRUE(shiny::isolate(value()$finance_loading)))break;if(Sys.time()>deadline)stop('timeout')}
   v<-value();stopifnot(maximum<=2)
   if(run<3L)stopifnot(all(v$point_reconciliation$status=='validated'),sum(v$cash_ledgers$b$bonus_type=='points')==11)
   else stopifnot(v$point_reconciliation$status[v$point_reconciliation$round_id=='r11']=='mismatch',!any(v$cash_ledgers$b$round_id=='r11'),is.na(rivals_inflow_totals(v$cash_ledgers$b)$points))
   before<-counts;session$flushReact();stopifnot(identical(before,counts))
  })
 }
 stopifnot(all(counts==2L),beats>2L)
 async_adapter_fetch<-original_adapter;persist_rival_finance_evidence<-original_persist
})
cat(sprintf('POINT SETTLEMENTS FINAL: %d passed / 0 failed\n',n))
