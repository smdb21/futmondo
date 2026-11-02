#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({
  source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R')
  source('Modules/Players_Table_Module.R');source('Modules/Rivals_Module.R')
})
n<-0L
check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
now<-as.POSIXct('2026-10-31T12:00:00',tz='UTC');cid<-'fixture-league'
teams<-data.frame(teamid='a',teamname='A',points=5)
rules<-list(initial_budget=100,money_per_point=2,ranking_pool=0,ranking_mode='top',users_to_rank=-1,dreamteam_reward=5,mvp_reward=10)
e<-function(type,id,p,at=now)rival_finance_evidence('u',cid,'unknown',type,id,p,at)
m<-function(id,amount,type='buy',date='2026-10-01T10:00:00Z',bonus='',at=now){
  e('movements',paste0('a:',id),list(id=id,team_id='a',money=amount,type=type,concept=id,date=date,bonus_type=bonus,round_id=if(nzchar(bonus))'r1'else''),at)
}
history<-e('team_rounds','a',list(team_id='a',rounds=list(list(id='r1',number=1,points=5,status='done',negative=FALSE))))
eligibility<-data.frame(round_id='r1',player_id='p',team_id='a',eligible=TRUE,verified=TRUE)
awards<-data.frame(round_id='r1',player_id='p',is_mvp=TRUE,is_finished=TRUE)
movements<-list(m('a-buy',-30),m('b-sell',20,'sell'),m('fee',-5,'fee','2026-10-02T10:00:00Z'),
  m('points',10,'bonus','2026-10-03T10:00:00Z','points'))
build<-function(evidence=c(list(history),movements),official=awards,tx=data.frame(),configuration=rules){
  build_rival_cash_dataset(teams,cid,evidence,official,tx,configuration,eligibility=eligibility,
    coverage_complete=TRUE,now=now,account_id='u')
}
out<-build();ledger<-out$cash_ledgers$a
check('one ledger reconciles opening, signed transfers, fee, recorded points and separate XI/MVP components',{
  # 100 - 30 + 20 - 5 + 10 + 5 XI + 10 MVP = 110.
  stopifnot(out$team_finances$estimated_cash==110,out$team_finances$reconstructed_cash==110,
    tail(ledger$running_balance,1)==110,sum(ledger$amount)==110,
    sum(ledger$id=='movement:points')==1L,sum(ledger$component=='Point reward')==0L,
    identical(ledger$running_balance[ledger$section!='undated'],c(100,70,90,85,95)))
  card<-rivals_summary_values(list(),teams,out$team_finances,'a')
  stopifnot(card$purchases==30,card$sales==20,card$cash==110)
})
check('same-time transactions have stable per-row cash, including same-minute entries',{
  shuffled<-build(c(list(history),rev(movements)))$cash_ledgers$a
  stopifnot(identical(ledger$id,shuffled$id),identical(ledger$running_balance,shuffled$running_balance),
    ledger$running_balance[ledger$id=='movement:a-buy']==70,ledger$running_balance[ledger$id=='movement:b-sell']==90)
  display<-rivals_cash_history_rows(ledger)
  stopifnot(all(is.finite(display$running_balance)),inherits(rivals_cash_history_widget(ledger),'reactable'))
})
check('undated estimated rewards have no invented dates and are separate calculation subtotals',{
  undated<-ledger[ledger$section=='undated',,drop=FALSE]
  stopifnot(nrow(undated)==3L,sum(undated$amount)==15,all(undated$date==''),all(is.na(undated$timestamp)),all(undated$estimated),
    tail(ledger$running_balance[ledger$section!='undated'],1)==95,
    tail(undated$running_balance,1)==out$team_finances$estimated_cash,
    all(rivals_cash_history_rows(ledger,TRUE)$date=='Payment date unknown'),
    inherits(rivals_cash_history_widget(ledger,TRUE),'reactable'))
})
check('sorting, filtering and paging a projection do not recalculate cash from its subset',{
  display<-rivals_cash_history_rows(ledger)
  subset<-display[display$type=='sell',,drop=FALSE]
  stopifnot(nrow(subset)==1L,subset$running_balance==90,subset$money==20)
  reordered<-display[order(display$money,decreasing=TRUE),,drop=FALSE]
  stopifnot(identical(display$running_balance,reordered$running_balance[match(display$ledger_order,reordered$ledger_order)]),
    tail(ledger$running_balance,1)==110)
})
check('corrected movements and award selections replace earlier entries without duplicate cash',{
  revised<-build(c(list(history),movements,list(m('a-buy',-35,at=now+1),m('a-buy',-35,at=now+2))))
  stopifnot(revised$team_finances$estimated_cash==105,sum(revised$cash_ledgers$a$id=='movement:a-buy')==1L)
  removed<-build(c(list(history),movements),awards[FALSE,])
  stopifnot(removed$team_finances$estimated_cash==95,tail(removed$cash_ledgers$a$running_balance,1)==95,
    !any(removed$cash_ledgers$a$component%in%c('MVP reward','Ideal XI reward')))
})
check('public feed deduplicates known IDs, preserves system counterparties and every unknown-ID transfer',{
  tx<-data.frame(id=c('buy','buy',NA,NA),buyer_team_id=c('a','a','',''),seller_team_id=c('','','a','a'),
    price=c(99,30,20,5),player_name=c('P','P','Q','R'),created='2026-10-01T10:00:00Z')
  x<-build(list(history),awards[FALSE,],tx)
  # No movements: 100 - 30 + 20 + 5 + 10 points = 105.
  stopifnot(x$team_finances$estimated_cash==105,sum(x$cash_ledgers$a$source=='Pressroom transfer')==3L,
    identical(sort(x$cash_ledgers$a$amount[x$cash_ledgers$a$source=='Pressroom transfer']),c(-30,5,20)),
    tail(x$cash_ledgers$a$running_balance,1)==105)
})
check('missing amounts remain unknown while zero and negative balances are preserved',{
  x<-build(list(m('a-buy',-100),m('b-zero',0,'fee'),m('c-unknown',NA_real_,'fee'),m('d-fee',-5,'fee')),awards[FALSE,])
  l<-x$cash_ledgers$a
  stopifnot(identical(l$running_balance,c(100,0,0,0,-5)),is.na(l$amount[l$id=='movement:c-unknown']),
    x$team_finances$estimated_cash== -5,grepl('known-amount',x$team_finances$estimate_notes),
    tail(l$unknown_amounts,1)==1L)
  unknown<-rules;unknown$initial_budget<-NA_real_
  y<-build_rival_cash_dataset(teams,'another-league',rules=unknown,account_id='u')
  stopifnot(is.na(y$team_finances$estimated_cash),all(is.na(y$cash_ledgers$a$running_balance)))
})
check('recorded opening allocation replaces configured opening and undated recorded cash is not backdated',{
  x<-build(list(m('opening',50,'budget','2026-09-01T00:00:00Z'),m('a-buy',-10),m('fee',-5,'fee','')),awards[FALSE,])
  l<-x$cash_ledgers$a
  stopifnot(sum(l$section=='opening')==1L,l$amount[1]==50,l$id[1]=='movement:opening',
    x$team_finances$estimated_cash==35,l$running_balance[l$id=='movement:a-buy']==40,
    l$section[l$id=='movement:fee']=='undated',is.na(l$timestamp[l$id=='movement:fee']))
})
check('observed cash is separate and cannot insert a balancing transaction or replace reconstructed summary',{
  x<-build(c(list(history),movements,list(e('team','a',list(budget=0,observed_at=rival_finance_stamp(now))))))
  stopifnot(x$team_finances$observed_cash==0,x$team_finances$budget==0,x$team_finances$estimated_cash==110,
    x$team_finances$reconciliation_difference== -110,tail(x$cash_ledgers$a$running_balance,1)==110,
    identical(x$cash_ledgers$a$id,ledger$id))
  card<-rivals_summary_values(list(budget=0),teams,x$team_finances,'a')
  stopifnot(card$cash==110,card$observed_cash==0,card$difference== -110)
})
check('bulk transfer history keeps one bounded ledger',{
  tx<-data.frame(id=paste0('t',seq_len(1000)),buyer_team_id='a',seller_team_id='',price=1,
    created='2026-10-01T10:00:00Z')
  elapsed<-system.time(x<-build(list(),awards[FALSE,],tx))['elapsed']
  stopifnot(nrow(x$cash_ledgers$a)==1001L,x$team_finances$estimated_cash== -900)
  cat(sprintf('MEASURE 1000 transfers: %.3fs; ledger %.1f KiB\n',elapsed,as.numeric(object.size(x$cash_ledgers$a))/1024))
})
check('inflow includes opening cash and separates sales and bonuses without expenses or excluded entries',{
  rows<-data.frame(type=c('opening','sell','bonus','bonus','fee','adjustment','sell'),
    amount=c(300000000,30,15,-3,-5,7,99),included=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE))
  x<-rivals_inflow_totals(rows)
  stopifnot(x$sales==30,x$bonuses==15,x$other==7,x$total==300000052,!x$incomplete)
  rows$amount[2]<-NA_real_;x<-rivals_inflow_totals(rows)
  stopifnot(x$incomplete,x$total==300000022,is.na(rivals_inflow_totals(data.frame())$total))
})
check('outflow separates recorded methods and preserves unclassified purchases and other debits',{
  rows<-data.frame(type=c('opening','buy','buy','buy','fee','bonus','sell','buy'),
    category=c('','market','clause',NA,'','','','market'),
    amount=c(300,-30,-20,-10,-5,-3,40,-99),included=c(rep(TRUE,7),FALSE))
  x<-rivals_outflow_totals(rows)
  stopifnot(x$market==30,x$clauses==20,x$unknown==10,x$other==8,x$total==68,!x$incomplete)
  rows$amount[2]<-NA_real_;x<-rivals_outflow_totals(rows)
  stopifnot(x$incomplete,x$total==38,is.na(rivals_outflow_totals(data.frame())$total))
})
check('public market buys are classified but rival transfers do not imply a purchase method',{
  tx<-data.frame(id=c('market','private'),buyer_team_id='a',seller_team_id=c('','b'),price=c(30,20))
  x<-build(list(),awards[FALSE,],tx)
  totals<-rivals_outflow_totals(x$cash_ledgers$a)
  stopifnot(totals$market==30,totals$clauses==0,totals$unknown==20,totals$total==50,
    x$team_finances$estimated_cash==50)
})
cat(sprintf('Rival running balances: %d passed / 0 failed\n',n))
