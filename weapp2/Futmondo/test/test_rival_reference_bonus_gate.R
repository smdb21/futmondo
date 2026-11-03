options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R');source('Modules/Players_Table_Module.R');source('Modules/Rivals_Module.R')})
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
e<-function(type,id,p,at=as.POSIXct('2026-11-03',tz='UTC'))rival_finance_evidence('u','c','unknown',type,id,p,at)
teams<-data.frame(teamid=c('a','b'),teamname=c('A','B'))
rules<-list(initial_budget=100,money_per_point=2,ranking_pool=30,ranking_mode='top',users_to_rank=-1,dreamteam_reward=5,mvp_reward=10)
rounds<-lapply(c('a','b'),function(id)e('team_rounds',id,list(team_id=id,rounds=list(list(id='r8',number=8,points=if(id=='a')10 else 5,status='closed',negative=FALSE)))))
award<-data.frame(round_id='r8',round_number=8,player_id='p',player_name='P',is_mvp=TRUE,is_finished=TRUE)
eligibility<-data.frame(round_id='r8',player_id='p',team_id='b',eligible=TRUE,verified=TRUE)
payment<-function(kind,amount,label='8',id=kind,team='a',date='2026-10-13T08:33:13Z')e('movements',paste(team,id),list(id=id,team_id=team,type=kind,money=amount,concept=label,date=date))
build<-function(extra=list())build_rival_cash_dataset(teams,'c',c(rounds,extra),award,rules=rules,eligibility=eligibility,coverage_complete=TRUE,account_id='u',reference_team_id='a')
check('point payments and interim closed selections cannot unlock final bonuses',{
 notification<-e('notifications','n',list(type='round',action='close',player_id='r8',created_at='2026-10-13T08:33:13Z'))
 x<-build(list(payment('points',20),notification));b<-x$cash_ledgers$b
 stopifnot(tail(b$running_balance,1)==110,sum(b$amount[b$included & b$bonus_type=='points'])==10,
  !b$included[b$bonus_type=='ranking'],!any(b$included[b$bonus_type%in%c('dreamteam','mvp')]),
  b$payment_date_source[b$bonus_type=='ranking']=='reference_payment_pending',
  is.na(rivals_inflow_totals(b)$ranking),is.na(x$team_finances$mvp_income[2]))
 t<-attr(b,'round_bonus_totals');stopifnot(is.na(t$amount[t$bonus_type=='ranking']),is.na(t$amount[t$bonus_type=='dreamteam']),is.na(t$amount[t$bonus_type=='mvp']) )
 stopifnot(any(rivals_cash_history_rows(b)$date=='Payment not confirmed'))
})
check('position settlement unlocks position; award payments can date independent awards',{
 base<-list(payment('points',20),payment('ranking',20,'8.1'),payment('dreamTeam',5,'8.1',date='2026-10-14T09:00:01Z'))
 x<-build(base);b<-x$cash_ledgers$b
 stopifnot(tail(b$running_balance,1)==135,b$included[b$bonus_type=='mvp'],
   b$date[b$bonus_type=='ranking']=='2026-10-13T08:33:13Z',b$date[b$bonus_type=='dreamteam']=='2026-10-14T09:00:01Z',
   !any(b$payment_date_estimated[b$bonus_type%in%c('ranking','dreamteam')]))
 x<-build(c(base,list(payment('mvp',10,'8.1'))));stopifnot(tail(x$cash_ledgers$b$running_balance,1)==135)
})
check('final position settlement enables awards without any reference award receipt',{
 b<-build(list(payment('points',20),payment('ranking',20)))$cash_ledgers$b
 stopifnot(tail(b$running_balance,1)==135,all(b$included[b$bonus_type%in%c('dreamteam','mvp')]))
})
check('explicit rival transactions survive and cannot authorize another team or type',{
 x<-build(list(payment('mvp',10,team='b')));b<-x$cash_ledgers$b
 stopifnot(sum(b$amount[b$included & b$bonus_type=='mvp'])==10,!any(b$included & b$bonus_type=='ranking'))
})
check('split timestamps, duplicates and source revisions preserve one whole-round reward',{
 base<-list(payment('points',20),payment('ranking',10,'8',id='first'),payment('ranking',10,'8.1',id='last',date='2026-10-14T10:00:00Z'))
 x<-build(c(rev(base),base));b<-x$cash_ledgers$b
 stopifnot(sum(b$included & b$bonus_type=='ranking')==1,b$date[b$bonus_type=='ranking']=='2026-10-14T10:00:00Z')
 revised<-base[[3]];revised$observed_at<-rival_finance_stamp(as.POSIXct('2026-11-04',tz='UTC'));revised$payload$date<-''
 b<-build(c(base,list(revised)))$cash_ledgers$b;stopifnot(!b$included[b$bonus_type=='ranking'])
})
check('other-account reference payments cannot unlock a bonus',{
 p<-payment('ranking',20);p$account_id<-'other'
 b<-build(list(p))$cash_ledgers$b;stopifnot(!b$included[b$bonus_type=='ranking'])
})
check('independent awards still require final source selections and historical eligibility',{
 saved_award<-award;saved_eligibility<-eligibility
 award$is_finished<-FALSE
 b<-build(list(payment('points',20)))$cash_ledgers$b
 stopifnot(!any(b$included & b$bonus_type%in%c('dreamteam','mvp')))
 award<-saved_award;eligibility<-eligibility[FALSE,]
 b<-build(list(payment('points',20)))$cash_ledgers$b
 stopifnot(!any(b$included & b$bonus_type%in%c('dreamteam','mvp')))
 eligibility<-saved_eligibility
})
cat(sprintf('REFERENCE BONUS GATE: %d passed / 0 failed\n',n))
