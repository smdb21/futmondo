options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R')})
raw<-jsonlite::fromJSON('test/idless_bonus_movements.json',simplifyVector=FALSE)
mov<-normalize_money_movements(raw)
stopifnot(attr(mov,'complete'),sum(mov$identity_source=='derived_settlement')==17,
 all(mov$source_movement_id[mov$identity_source=='derived_settlement']==''),
 sum(mov$money[mov$type=='points'])==60060000,sum(mov$money)==52129125)
stamp<-as.POSIXct('2026-11-03 18:52:14',tz='UTC');attr(mov,'observed_at')<-stamp
capture<-function(m)rival_team_evidence(list(team_id='reference',info=NULL,movements=m,rounds=NULL),'account','league','unknown')
ev<-capture(mov)
stopifnot(length(rival_evidence_rows(ev,'movements'))==length(raw))
# Exercise the actual JSON persistence decoder before rebuilding cash.
restored<-rival_evidence_decode(jsonlite::fromJSON(jsonlite::toJSON(ev,auto_unbox=TRUE,null='null')))
build<-function(evidence)build_rival_cash_dataset(data.frame(teamid='reference',teamname='Reference'),'league',evidence,
 rules=list(initial_budget=300000000,money_per_point=70000,ranking_pool=30000000,dreamteam_reward=500000,mvp_reward=0),
 account_id='account',reference_team_id='reference')
x<-build(restored);ledger<-x$cash_ledgers$reference
stopifnot(x$team_finances$reconstructed_cash==52129125,
 sum(ledger$amount[ledger$bonus_type=='points'])==60060000,
 sum(ledger$amount[ledger$bonus_type=='dreamteam'])==3500000,
 !any(ledger$estimated & ledger$bonus_type%in%c('points','dreamteam')))
replay<-normalize_money_movements(rev(raw));attr(replay,'observed_at')<-stamp+1
again<-build(rival_merge_evidence(restored,capture(replay)))
stopifnot(again$team_finances$reconstructed_cash==52129125,nrow(again$cash_ledgers$reference)==nrow(ledger))
# A revision replaces the same natural settlement, while split labels stay distinct.
index<-which(vapply(raw,function(r)identical(r$type,'points'),logical(1)))[1]
changed<-raw;changed[[index]]$money<-changed[[index]]$money+1
revision<-normalize_money_movements(changed);attr(revision,'observed_at')<-stamp+2
stopifnot(identical(revision$id,mov$id),build(rival_merge_evidence(restored,capture(revision)))$team_finances$reconstructed_cash==52129126)
# Unidentified generic transactions do not acquire invented source identities.
bad<-normalize_money_movements(list(list(type='buy',money=-10,date='2026-11-03T00:00:00Z',concept='1')))
stopifnot(!attr(bad,'complete'),bad$id=='')
cat('PASS captured ID-less settlements survive parsing, persistence, replay and revisions; cash reconciles to 52,129,125 EUR\n')
