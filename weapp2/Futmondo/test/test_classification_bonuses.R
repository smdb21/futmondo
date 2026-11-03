#!/usr/bin/env Rscript
# Synthetic accounting fixtures; no network or real accounts.
source('test/test_rival_running_balances.R')
suppressPackageStartupMessages(source('Modules/Classification_Module.R'))
h<-data.frame(team_id='a',team_name='A',round=1,points=5)
standings<-classification_window_table(h)
x<-classification_bonus_table(standings,h,out,c(1,1))
stopifnot(x$points_bonus==10,x$mvp_bonus==10,x$best_xi_bonus==5,x$position_bonus==0)
# Independent selection answers: r1=(10,10,5,0), r2=(20,0,15,7).
f<-out
attr(f$cash_ledgers$a,'round_bonus_totals')<-data.frame(round_number=rep(1:2,each=4),bonus_type=rep(c('points','mvp','dreamteam','ranking'),2),amount=c(10,10,5,0,20,0,15,7))
h<-rbind(h,transform(h,round=2,points=10))
x<-classification_bonus_table(standings,h,f,c(1,2))
stopifnot(x$points_bonus==30,x$mvp_bonus==10,x$best_xi_bonus==20,x$position_bonus==7)
x<-classification_bonus_table(standings,h,f,c(1,1),'2')
stopifnot(x$points_bonus==20,x$mvp_bonus==0,x$best_xi_bonus==15,x$position_bonus==7)
attr(f$cash_ledgers$a,'round_bonus_totals')$amount[5]<-NA_real_
x<-classification_bonus_table(standings,h,f,c(1,2))
stopifnot(is.na(x$points_bonus),x$best_xi_bonus==20)
x<-classification_bonus_table(standings,h,f,c(1,1))
stopifnot(x$points_bonus==10)
stopifnot(is.na(classification_bonus_table(standings,h,list(),c(1,2))$points_bonus))
# Missing award ownership is unknown; a verified award belonging to another
# team is a genuine zero. Both use the same accounting builder as the UI.
unknown<-build_rival_cash_dataset(teams,cid,list(history),awards,rules=rules,coverage_complete=TRUE,account_id="u")
stopifnot(is.na(classification_bonus_table(standings,h[1,],unknown,c(1,1))$mvp_bonus))
other<-eligibility;other$team_id<-'b'
known<-build_rival_cash_dataset(teams,cid,list(history),awards,rules=rules,eligibility=other,coverage_complete=TRUE,account_id="u")
stopifnot(classification_bonus_table(standings,h[1,],known,c(1,1))$mvp_bonus==0)
cat('PASS classification bonus selection, known answers, missing evidence and confirmed zeros\n')
# Selection only reads a bounded in-memory projection (10 teams x 38 rounds).
large<-list(cash_ledgers=setNames(rep(list(f$cash_ledgers$a),10),as.character(1:10)))
for(id in names(large$cash_ledgers))attr(large$cash_ledgers[[id]],'round_bonus_totals')<-data.frame(round_number=rep(1:38,each=4),bonus_type=rep(c('points','mvp','dreamteam','ranking'),38),amount=1)
st<-data.frame(team_id=as.character(1:10));hh<-data.frame(round=1:38)
elapsed<-system.time(for(i in 1:100)classification_bonus_table(st,hh,large,c(1,38)))[['elapsed']]
cat(sprintf('MEASURE 100 selections, 10 teams x 38 rounds: %.3fs; metadata %.1f KiB\n',elapsed,as.numeric(object.size(large))/1024))
