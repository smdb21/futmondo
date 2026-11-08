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

# A complete final XI can contain players whose historical owner is unknown.
# Those XI gaps must not obscure an independently verified MVP recipient.
full_xi<-data.frame(round_id='r1',player_id=c('p',paste0('q',1:10)),
  is_mvp=c(TRUE,rep(FALSE,10)),is_finished=TRUE)
partial_owners<-build_rival_cash_dataset(teams,cid,list(history),full_xi,rules=rules,
  eligibility=eligibility,coverage_complete=FALSE,account_id='u')
one_round<-data.frame(team_id='a',team_name='A',round=1,points=5)
projection<-function(value)classification_bonus_table(classification_window_table(one_round),one_round,value,c(1,1))
x<-projection(partial_owners)
# Opening 100 + points 10 + verified XI member 5 + MVP 10 = known subtotal 125.
stopifnot(x$mvp_bonus==10,is.na(x$best_xi_bonus),x$points_bonus==10,
  partial_owners$team_finances$estimated_cash==125,
  tail(partial_owners$cash_ledgers$a$running_balance,1)==125)
other_owner<-eligibility;other_owner$team_id<-'b'
other_owners<-build_rival_cash_dataset(teams,cid,list(history),full_xi,rules=rules,
  eligibility=other_owner,coverage_complete=FALSE,account_id='u')
stopifnot(projection(other_owners)$mvp_bonus==0,is.na(projection(other_owners)$best_xi_bonus))
for(ownership in list(eligibility[FALSE,],rbind(eligibility,other_owner))){
  unknown_mvp<-build_rival_cash_dataset(teams,cid,list(history),full_xi,rules=rules,
    eligibility=ownership,coverage_complete=FALSE,account_id='u')
  stopifnot(is.na(projection(unknown_mvp)$mvp_bonus))
}
# Missing eligibility for another round cannot poison the selected round.
second_xi<-transform(full_xi,round_id='r2')
more_rounds<-build_rival_cash_dataset(teams,cid,list(history),rbind(full_xi,second_xi),rules=rules,
  eligibility=eligibility,coverage_complete=FALSE,account_id='u')
stopifnot(projection(more_rounds)$mvp_bonus==10,is.na(projection(more_rounds)$best_xi_bonus))
# The existing wire format carries component coverage through a process restart.
wire<-jsonlite::toJSON(rival_snapshot_encode(partial_owners$cash_ledgers$a),auto_unbox=TRUE,na='null',null='null',digits=NA)
restored_ledger<-rival_snapshot_decode(jsonlite::fromJSON(wire,simplifyVector=FALSE))
restored<-list(cash_ledgers=list(a=restored_ledger))
stopifnot(projection(restored)$mvp_bonus==10,is.na(projection(restored)$best_xi_bonus),
  identical(restored_ledger,partial_owners$cash_ledgers$a))
cat('PASS independent MVP coverage, unknown/conflicting owners, round isolation and persisted ledger reuse\n')
# Matching a partial recipient subtotal is insufficient to split a recorded
# combined payment. Its actual cash survives even when attribution is unknown.
combined_payment<-e('movements','a:native',list(id='native',team_id='a',type='dreamTeam',
  concept='1',money=15,date='2026-10-03T10:00:00Z'))
combined<-build_rival_cash_dataset(teams,cid,list(history,combined_payment),full_xi,rules=rules,
  eligibility=eligibility,coverage_complete=FALSE,account_id='u')
allocation<-attr(combined$cash_ledgers$a,'award_payment_allocations')
stopifnot(all(allocation$status=='unresolved'),is.na(projection(combined)$mvp_bonus),
  is.na(projection(combined)$best_xi_bonus),
  sum(combined$cash_ledgers$a$amount[combined$cash_ledgers$a$source_type=='dreamTeam'])==15,
  combined$team_finances$estimated_cash==125)
cat('PASS partial recipients cannot validate combined payment attribution; actual cash retained\n')

# Explicitly confirmed unowned players are different from missing evidence.
# Persisted confirmations close coverage without assigning a bonus to a team.
local({
  directory<-data.frame(teamid=c('a','b'),teamname=c('A','B'))
  histories<-lapply(directory$teamid,function(id)e('team_rounds',id,
    list(team_id=id,rounds=list(list(id='r1',number=1,points=5,status='closed',negative=FALSE)))))
  owners<-data.frame(round_id='r1',player_id=full_xi$player_id[1:7],
    team_id=c(rep('a',3),rep('b',4)),eligible=TRUE,verified=TRUE)
  confirmations<-lapply(full_xi$player_id[8:11],function(player)e('eligibility',paste('unowned','r1',player,sep=':'),
    list(round_id='r1',player_id=player,team_id='',eligible=FALSE,verified=TRUE,
      ownership_status='unowned',source='user_confirmed_unowned')))
  decoded<-rival_evidence_decode(jsonlite::fromJSON(jsonlite::toJSON(confirmations,auto_unbox=TRUE,null='null')))
  build_confirmed<-function(records)build_rival_cash_dataset(directory,cid,c(histories,records),full_xi,
    rules=rules,eligibility=owners,coverage_complete=FALSE,account_id='u')
  scores<-data.frame(team_id=c('a','b'),team_name=c('A','B'),round=1,points=5)
  project<-function(value)classification_bonus_table(classification_window_table(scores),scores,value,c(1,1))
  value<-build_confirmed(decoded);result<-project(value)
  stopifnot(identical(result$best_xi_bonus,c(15,20)),identical(result$mvp_bonus,c(10,0)),
    sum(result$best_xi_bonus)==35,identical(value$team_finances$estimated_cash,c(135,130)))
  for(kind in c('missing','round','account','league')){
    invalid<-decoded
    if(kind=='missing')invalid<-invalid[-1]
    if(kind=='round')invalid[[1]]$payload$round_id<-'r2'
    if(kind=='account')invalid[[1]]$account_id<-'other'
    if(kind=='league')invalid[[1]]$championship_id<-'other'
    stopifnot(all(is.na(project(build_confirmed(invalid))$best_xi_bonus)))
  }
  conflicting<-e('eligibility','other-owner',list(round_id='r1',player_id=full_xi$player_id[8],
    team_id='a',eligible=TRUE,verified=TRUE))
  stopifnot(all(is.na(project(build_confirmed(c(decoded,list(conflicting))))$best_xi_bonus)))
  cat('PASS explicit unowned confirmations persist and close XI coverage; absent, foreign or conflicting evidence stays unknown\n')
})
