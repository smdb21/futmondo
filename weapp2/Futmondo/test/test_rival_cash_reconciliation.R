#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R');source('Modules/Players_Table_Module.R');source('Modules/Rivals_Module.R')})
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
cid<-'fixture-league';now<-as.POSIXct('2026-11-01T12:00:00',tz='UTC')
teams<-data.frame(teamid=c('a','b'),teamname=c('A','B'),points=c(10,5))
rules<-list(initial_budget=100,money_per_point=0,ranking_pool=30,ranking_mode='top',users_to_rank=-1,dreamteam_reward=5,mvp_reward=10)
e<-function(type,id,p)rival_finance_evidence('u',cid,'unknown',type,id,p,now)
movement<-function(id,team,type,money,concept,date='2026-10-29T12:00:00Z',round='',bonus='',player='',event='')
 e('movements',paste(team,id,sep=':'),list(id=id,team_id=team,type=type,money=money,concept=concept,category=if(type=='ranking')'bonus'else'market',date=date,round_id=round,bonus_type=bonus,player_id=player,source_event_id=event))
notice<-function(id='publication',round='r1',number='1',date='2026-10-29T12:00:00Z',type='round',action='close',updated=date)
 e('notifications',id,list(id=id,type=type,action=action,player_id=round,player_name=number,created_at=date,updated_at=updated,championship_id=cid))
rounds<-lapply(c('a','b'),function(id)e('team_rounds',id,list(team_id=id,rounds=list(list(id='r1',number=1,points=if(id=='a')10 else 5,status='closed',negative=FALSE)))))
awards<-data.frame(round_id='r1',round_number=1,player_id='p',player_name='Player P',is_mvp=TRUE,is_finished=TRUE)
eligibility<-data.frame(round_id='r1',player_id='p',team_id='b',eligible=TRUE,verified=TRUE)
build<-function(rows,tx=data.frame(),official=awards)build_rival_cash_dataset(teams,cid,rows,official,tx,rules,eligibility=eligibility,account_id='u',now=now,coverage_complete=TRUE)
check('all observed movement types normalize before reward overlap checks',{
 raw<-normalize_money_movements(lapply(seq_along(c('buy','clause','sell','clause_sell','ranking','budget')),function(i)
   list(id=as.character(i),type=c('buy','clause','sell','clause_sell','ranking','budget')[i],category=if(i>=5)'bonus'else'market',money=c(-10,-20,30,40,5,100)[i],concept=if(i==5)'1.1'else'P')))
 x<-rival_normalize_movements(raw,data.frame(round_id='r1',round_number=1))
 stopifnot(identical(x$type,c('buy','buy','sell','sell','bonus','budget')),identical(x$source_type,c('buy','clause','sell','clause_sell','ranking','budget')),
   x$category[2]=='clause',x$category[4]=='clause',x$bonus_type[5]=='ranking',x$round_id[5]=='r1',x$settlement_label[5]=='1.1')
})
check('reconstructed award bonuses prefer observed position-payment dates and reconcile all surfaces',{
 out<-build(c(rounds,list(movement('rank-a','a','ranking',20,'1',date='2026-10-28T12:00:00Z'),notice())));l<-out$cash_ledgers$b
 bonus<-l[l$bonus_type%in%c('ranking','mvp','dreamteam'),]
 stopifnot(nrow(bonus)==3L,all(bonus$date=='2026-10-28T12:00:00Z'),all(bonus$estimated),all(bonus$payment_date_estimated),all(bonus$payment_date_source=='league_position_payment'),sum(bonus$amount)==25,
   all(c('Position reward','MVP reward','Best XI reward')%in%bonus$component),sum(grepl('Player P',bonus$calculation))==2L,
   inherits(rivals_cash_history_widget(l),'reactable'))
 for(id in c('a','b')){
   ledger<-out$cash_ledgers[[id]];i<-rivals_inflow_totals(ledger);o<-rivals_outflow_totals(ledger);summary<-out$team_finances[out$team_finances$teamid==id,]
   stopifnot(i$total-o$total==tail(ledger$running_balance,1),i$total-o$total==summary$estimated_cash,
     summary$total_inflow==i$total,summary$total_outflow==o$total)
 }
 stopifnot(out$team_finances$estimated_cash[1]==120,out$team_finances$estimated_cash[2]==125,
   out$cash_ledgers$a$date[out$cash_ledgers$a$bonus_type=='ranking']=='2026-10-28T12:00:00Z',
   !out$cash_ledgers$a$payment_date_estimated[out$cash_ledgers$a$bonus_type=='ranking'])
})
check('point estimates use their publication date rather than another reward payment anchor',{
 configuration<-rules;configuration$money_per_point<-2
 x<-build_rival_cash_dataset(teams,cid,c(rounds,list(notice(),movement('rank-a','a','ranking',20,'1',date='2026-10-28T12:00:00Z'))),awards,data.frame(),configuration,eligibility=eligibility,account_id='u',now=now)
 bonus<-x$cash_ledgers$b[x$cash_ledgers$b$estimated,]
 stopifnot(bonus$date[bonus$bonus_type=='points']=='2026-10-29T12:00:00Z',
   bonus$payment_date_source[bonus$bonus_type=='points']=='round_points_publication',
   all(bonus$date[bonus$bonus_type!='points']=='2026-10-28T12:00:00Z'),
   all(bonus$payment_date_source[bonus$bonus_type!='points']=='league_position_payment'),
   x$team_finances$estimated_cash[2]==135)
})
check('recorded round-level MVP payments replace estimates without requiring a player ID',{
 rows<-c(rounds,list(notice(date='2026-10-31T12:00:00Z'),movement('rank-a','a','ranking',20,'1'),movement('mvp-b','b','bonus',12,'Award',round='r1',bonus='mvp')))
 x<-build(rows);l<-x$cash_ledgers$b
 stopifnot(x$team_finances$estimated_cash[2]==127,sum(l$bonus_type=='mvp')==1L,l$amount[l$bonus_type=='mvp']==12,
   l$date[l$bonus_type=='mvp']=='2026-10-29T12:00:00Z',!l$payment_date_estimated[l$bonus_type=='mvp'])
})
check('typed rewards without round identity cannot be added again as reconstructed income',{
 rows<-c(rounds,list(movement('rank-a','a','ranking',20,'1'),movement('mvp-b','b','bonus',10,'Award',bonus='mvp')))
 x<-build(rows);l<-x$cash_ledgers$b
 stopifnot(sum(l$amount[l$bonus_type=='mvp'])==10,x$team_finances$estimated_cash[2]==125,
   sum(l$amount[l$bonus_type=='dreamteam'])==5,grepl('no round identity',x$team_finances$estimate_notes[2]))
 rows<-c(rounds,list(movement('rank-b','b','bonus',7,'No round',bonus='ranking')))
 x<-build(rows);stopifnot(sum(x$cash_ledgers$b$amount[x$cash_ledgers$b$bonus_type=='ranking'])==7,x$team_finances$estimated_cash[2]==122)
 configuration<-rules;configuration$money_per_point<-2
 rows<-c(rounds,list(movement('points-b','b','bonus',4,'No round',bonus='points')))
 x<-build_rival_cash_dataset(teams,cid,rows,awards,data.frame(),configuration,eligibility=eligibility,account_id='u',now=now)
 stopifnot(sum(x$cash_ledgers$b$amount[x$cash_ledgers$b$bonus_type=='points'])==4)
})
check('split settlement labels retain separate recorded money and never backdate a full latest selection',{
 rows<-c(rounds,list(movement('rank-a1','a','ranking',8,'1.1'),movement('rank-a2','a','ranking',12,'1.2',date='2026-10-31T12:00:00Z')))
 x<-build(rows);l<-x$cash_ledgers$b;bonus<-l[l$bonus_type%in%c('ranking','mvp','dreamteam'),]
 stopifnot(sum(x$cash_ledgers$a$amount[x$cash_ledgers$a$bonus_type=='ranking'])==20,
   all(bonus$date==''),sum(bonus$amount)==25,x$team_finances$estimated_cash[2]==125,
   all(c('1.1','1.2')%in%x$cash_ledgers$a$settlement_label))
})
check('split or repeated position payments fall back to the matching round notification once',{
 for(labels in list(c('1.1','1.2'),c('1','1'))){
  rows<-c(rounds,list(movement('rank-a1','a','ranking',8,labels[1]),movement('rank-a2','a','ranking',12,labels[2],date='2026-10-31T12:00:00Z'),notice(date='2026-11-01T12:00:00Z')))
  x<-build(rows);bonus<-x$cash_ledgers$b[x$cash_ledgers$b$estimated&x$cash_ledgers$b$bonus_type%in%c('ranking','mvp','dreamteam'),]
  stopifnot(nrow(bonus)==3L,sum(bonus$amount)==25,all(bonus$date=='2026-11-01T12:00:00Z'),
    all(bonus$payment_date_source=='round_points_publication'),x$team_finances$estimated_cash[2]==125)
 }
})
check('another team split or repeated settlement blocks an apparently simple reference anchor',{
 rows<-c(rounds,list(movement('rank-a','a','ranking',20,'1'),movement('rank-b','b','ranking',10,'1.2',date='2026-10-31T12:00:00Z')))
 x<-build(rows);bonus<-x$cash_ledgers$b[x$cash_ledgers$b$estimated&x$cash_ledgers$b$bonus_type%in%c('mvp','dreamteam'),]
 stopifnot(nrow(bonus)==2L,all(bonus$date==''))
 rows[[length(rows)]]$payload$concept<-'1'
 rows<-c(rows,list(movement('rank-b2','b','ranking',2,'1',date='2026-11-01T12:00:00Z')))
 x<-build(rows);bonus<-x$cash_ledgers$b[x$cash_ledgers$b$estimated&x$cash_ledgers$b$bonus_type%in%c('mvp','dreamteam'),]
 stopifnot(nrow(bonus)==2L,all(bonus$date==''))
})
check('latest repeated round publication dates a cumulative reward once; read updates do not move it',{
 rows<-c(rounds,list(notice('first',date='2026-10-27T12:00:00Z'),notice('latest',date='2026-10-31T12:00:00Z'),
   notice('latest',date='2026-10-31T12:00:00Z',updated='2026-11-02T10:00:00Z')))
 configuration<-rules;configuration$money_per_point<-2
 x<-build_rival_cash_dataset(teams,cid,rows,awards,data.frame(),configuration,eligibility=eligibility,account_id='u',now=now)
 bonus<-x$cash_ledgers$b[x$cash_ledgers$b$estimated,]
 stopifnot(nrow(bonus)==4L,all(bonus$date=='2026-10-31T12:00:00Z'),sum(bonus$amount)==35,
   x$team_finances$estimated_cash[2]==135,all(bonus$payment_date_estimated),
   all(grepl('cumulative round reward',bonus$source)))
 replay<-build_rival_cash_dataset(teams,cid,c(rows,rows),awards,data.frame(),configuration,eligibility=eligibility,account_id='u',now=now)
 stopifnot(identical(x$team_finances$estimated_cash,replay$team_finances$estimated_cash),
   identical(x$cash_ledgers$b$date,replay$cash_ledgers$b$date))
})
check('round identity, event kind and creation timestamp must match publication evidence',{
 catalog<-data.frame(round_id=c('r1','r2'),round_number=c(1,2))
 events<-list(notice('market',type='market'),notice('wrong-action',action='open'),notice('wrong-number',number='2'),
   notice('missing-created',date='',updated='2026-10-31T12:00:00Z'),notice('round-two',round='r2',number='2'))
 dates<-rival_round_publication_dates(events,catalog)
 stopifnot(nrow(dates)==1L,dates$round_id=='r2')
 x<-build(c(rounds,events));bonus<-x$cash_ledgers$b[x$cash_ledgers$b$estimated,]
 stopifnot(all(bonus$date==''),all(bonus$payment_date_source=='missing_round_publication'),
   grepl('Round 1 points-publication notification is missing',x$team_finances$estimate_notes[2]))
 foreign<-notice();foreign$account_id<-'another-account'
 x<-build(c(rounds,list(foreign)));stopifnot(all(x$cash_ledgers$b$date[x$cash_ledgers$b$estimated]==''))
 # A future decoder may expose directObject as explicit round fields. A unique
 # number maps only through the actual catalog, never a guessed ID sequence.
 numbered<-notice();numbered$payload$player_id<-''
 stopifnot(rival_round_publication_dates(list(numbered),catalog)$round_id=='r1')
})
check('saved notifications survive JSON storage and date reuse without notification or per-team requests',{
 rows<-c(rounds,list(notice()))
 saved<-jsonlite::fromJSON(jsonlite::toJSON(rows,auto_unbox=TRUE,null='null'),simplifyVector=TRUE)
 decoded<-rival_evidence_decode(saved)
 expected<-build(rows);restored<-build(decoded)
 stopifnot(identical(expected$team_finances$estimated_cash,restored$team_finances$estimated_cash),
   identical(expected$cash_ledgers$b$date,restored$cash_ledgers$b$date),
   all(restored$cash_ledgers$b$payment_date_estimated[restored$cash_ledgers$b$estimated]))
})
check('notification dates never invent point income when the point reward setting is absent',{
 configuration<-rules;configuration$money_per_point<-NULL
 x<-build_rival_cash_dataset(teams,cid,c(rounds,list(notice())),awards,data.frame(),configuration,eligibility=eligibility,account_id='u',now=now)
 stopifnot(!any(x$cash_ledgers$b$bonus_type=='points'),x$team_finances$point_bonus[2]==0,
   x$team_finances$estimated_cash[2]==125,grepl('Point reward rule unavailable',x$team_finances$estimate_notes[2]))
})
check('configured zero rewards create no reconstructed cashflow or missing-date issue; actual zero remains',{
 configuration<-rules;configuration$money_per_point<-0;configuration$ranking_pool<-0
 configuration$dreamteam_reward<-0;configuration$mvp_reward<-0
 x<-build_rival_cash_dataset(teams,cid,rounds,awards,data.frame(),configuration,eligibility=eligibility,account_id='u',now=now)
 stopifnot(all(x$team_finances$estimated_cash==100),
   all(vapply(x$cash_ledgers,function(ledger)!any(ledger$estimated&ledger$bonus_type%in%c('points','ranking','mvp','dreamteam')),logical(1))),
   !any(grepl('points-publication notification is missing',x$team_finances$estimate_notes)))
 paid<-c(rounds,list(movement('zero-point','b','bonus',0,'Zero reward',round='r1',bonus='points')))
 actual<-build_rival_cash_dataset(teams,cid,paid,awards,data.frame(),configuration,eligibility=eligibility,account_id='u',now=now)
 row<-actual$cash_ledgers$b[actual$cash_ledgers$b$id=='movement:zero-point',]
 stopifnot(nrow(row)==1L,row$amount==0,!row$estimated,row$date=='2026-10-29T12:00:00Z')
})
check('unique observed clause propagates to buyer and seller with one-second source jitter only',{
 tx<-data.frame(id=c('clause','ordinary'),buyer_team_id=c('a','b'),seller_team_id=c('b','a'),price=c(20,10),player_id=c('p','q'),player_name=c('P','Q'),created='2026-10-29T12:00:00.090Z')
 rows<-list(movement('buy','a','clause',-20,'P'),movement('sale','a','sell',10,'Q'))
 x<-build(rows,tx,awards[FALSE,]);l<-x$cash_ledgers$b
 stopifnot(identical(x$pressroom$is_clause,c(TRUE,FALSE)),rivals_inflow_totals(l)$clauses==20,rivals_outflow_totals(l)$market==10,
   x$team_finances$total_spent[1]==20,x$team_finances$total_sales[1]==10)
 tx$created[1]<-'2026-10-31T12:00:00Z';x<-build(rows,tx,awards[FALSE,]);stopifnot(is.na(x$pressroom$is_clause[1]))
 tx$created[1]<-'2026-10-29T12:00:00Z';tx<-rbind(tx,tx[1,]);tx$id[3]<-'ambiguous';x<-build(rows,tx,awards[FALSE,]);stopifnot(all(is.na(x$pressroom$is_clause[c(1,3)])))
})
check('unclassified transfer methods preserve numeric totals and signed zero/negative final cash',{
 for(price in c(100,105)){
   tx<-data.frame(id='unknown',buyer_team_id='b',seller_team_id='a',price=price)
   x<-build(list(),tx,awards[FALSE,]);l<-x$cash_ledgers$b;i<-rivals_inflow_totals(l);o<-rivals_outflow_totals(l)
   stopifnot(o$total==price,o$unknown==price,i$total==100,i$total-o$total==100-price,tail(l$running_balance,1)==100-price)
 }
})
check('real asynchronous lineup read accepts observed route and produces persistable historical eligibility',{
 local({
   original<-fm_async_read_http;old_offline<-getOption('futmondo.offline');on.exit({fm_async_read_http<<-original;options(futmondo.offline=old_offline)})
   options(futmondo.offline=FALSE);calls<-0L
   fm_async_read_http<<-function(url,method='GET',body=NULL,headers=character(),owner='fixture',...){
     calls<<-calls+1L;request<-jsonlite::fromJSON(body)
     stopifnot(url=='https://api.futmondo.com/1/userteam/roundlineup',method=='POST',request$query$round=='r1',request$query$userteamId=='b')
     promises::promise_resolve(list(status_code=200L,headers=list(`content-type`='application/json'),content=charToRaw('{"answer":{"players":[{"id":"p"}],"bench":{"players":[{"id":"q"}]},"negative":false,"status":"closed"}}')))
   }
   result<-NULL;failure<-NULL;auth<-c(userid='fixture',token='fixture-only')
   p<-async_adapter_fetch('get_rival_round_lineup_observation',list(login=auth,championship_id=cid,user_team_id='b',round_id='r1',signature='sig'),resource_context(auth,cid,'b',session_id='lineup-test'))
   promises::then(p,function(x)result<<-x,function(e)failure<<-e);deadline<-Sys.time()+3
   while(is.null(result)&&is.null(failure)&&Sys.time()<deadline)later::run_now(.01)
   if(!is.null(failure))stop(failure)
   records<-rival_lineup_evidence(result,'u',cid,'unknown')
   stopifnot(calls==1L,length(records)==1L,identical(records[[1]]$payload$players,list('p')),identical(records[[1]]$payload$bench,list('q')))
 })
})
cat(sprintf('Rival cash reconciliation: %d passed / 0 failed\n',n))
