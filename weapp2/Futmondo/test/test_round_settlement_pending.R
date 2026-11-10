#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
now<-as.POSIXct('2026-02-15T12:00:00Z',tz='UTC')
catalog<-data.frame(round_id=c('r1','r2'),round_number=1:2)
confirmed<-rival_finance_evidence('u','league','s','eligibility','round-settlement:r2',
  list(kind='round_settlement_status',version=1L,status='pending',verified=TRUE,round_id='r2',round_number=2),now-86400)
mov<-data.frame(round_id='r2',bonus_type='points',money=20,date='2026-02-15T00:00:00Z')
stopifnot(identical(rival_pending_settlement_rounds(list(confirmed),catalog,mov,now),2L))
# Neither points, elapsed time nor a zero final-category payment prove settlement.
stopifnot(identical(rival_pending_settlement_rounds(list(confirmed),catalog,mov,now+86400*60),2L))
zero<-mov;zero$bonus_type<-'ranking';zero$money<-0
stopifnot(identical(rival_pending_settlement_rounds(list(confirmed),catalog,zero,now),2L))
paid<-zero;paid$money<-7
stopifnot(!length(rival_pending_settlement_rounds(list(confirmed),catalog,paid,now)))
old<-paid;old$date<-'2026-02-13T00:00:00Z'
stopifnot(identical(rival_pending_settlement_rounds(list(confirmed),catalog,old,now),2L))
for(field in c('source_id','round_number')){
 invalid<-confirmed
 if(field=='source_id')invalid$source_id<-'round-settlement:foreign'else invalid$payload$round_number<-1
 stopifnot(!length(rival_pending_settlement_rounds(list(invalid),catalog,mov,now)))
}
cat('PASS explicit pending status, later final payment, point/zero/old-payment exclusion and source identity\n')
# The official dated calendar supplies pending proof even without a manual record.
calendar<-rival_ownership_observation_evidence(list(status='ok',
 rows=data.frame(round_id='r2',begin_process='2026-02-01T00:00:00Z',
   fixture_dates=I(list(c('2026-02-01T00:00:00Z','2026-02-20T00:00:00Z')))),
 observed_at='2026-02-10T00:00:00Z'),'u','league','s',catalog=TRUE)
stopifnot(identical(rival_pending_settlement_rounds(calendar,catalog,paid,now),2L),
  identical(rival_pending_settlement_rounds(calendar,catalog,paid,now+86400*10),2L))
later<-paid;later$date<-'2026-02-21T00:00:00Z'
stopifnot(!length(rival_pending_settlement_rounds(calendar,catalog,later,now+86400*10)))
cat('PASS future postponed fixture remains pending until an observed final-category payment\n')
# Round totals exclude the pending round while standings points stay visible.
ledger<-data.frame()
attr(ledger,'round_bonus_totals')<-data.frame(round_number=rep(1:2,each=4),
 bonus_type=rep(c('points','mvp','dreamteam','ranking'),2),amount=c(10,0,5,7,20,NA,NA,NA))
h<-data.frame(team_id='a',team_name='Alpha',round=1:2,points=c(5,10))
f<-list(cash_ledgers=list(a=ledger),pending_bonus_rounds=2L)
z<-classification_bonus_table(classification_window_table(h),h,f,c(1,2),expected_rounds=1:2)
stopifnot(z$points==15,z$points_bonus==10,z$mvp_bonus==0,z$best_xi_bonus==5,z$position_bonus==7,
 identical(attr(z,'bonus_pending_rounds'),2L),identical(attr(z,'bonus_completed_rounds'),1L))
cat('PASS selection preserves all standings points and known settled-round bonuses without inventing pending amounts\n')
