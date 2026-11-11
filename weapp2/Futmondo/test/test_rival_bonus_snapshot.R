#!/usr/bin/env Rscript
# A compact durable snapshot must retain the exact ledger projection, including NA.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
ctx<-list(account_id='snapshot-account',championship_id='snapshot-league',season='s',user_team_id='a')
teams<-data.frame(teamid=c('a','b'),teamname=c('Alpha','Beta'))
evidence<-lapply(teams$teamid,function(id)rival_finance_evidence(ctx$account_id,ctx$championship_id,ctx$season,
  'team_rounds',id,list(team_id=id,rounds=list(list(id='r1',number=1,points=if(id=='a')5 else 10,status='closed',negative=FALSE)))))
cache<-rival_calculation_cache()
calculated<-build_rival_cash_dataset(teams,ctx$championship_id,evidence,
  rules=list(initial_budget=100,money_per_point=2,ranking_pool=0,dreamteam_reward=0,mvp_reward=0),
  account_id=ctx$account_id,season=ctx$season,cache=cache)
attr(calculated$cash_ledgers$b,'round_bonus_totals')$amount[2]<-NA_real_
expected<-lapply(calculated$cash_ledgers,function(ledger)attr(ledger,'round_bonus_totals'))
# Force the same full-cache size fallback seen in the production snapshot.
cache$prepared$padding<-raw(.rival_snapshot_limits$json+1L)
payload<-rival_snapshot_payload(cache,calculated,ctx)
stopifnot(!is.null(payload))
wire<-jsonlite::fromJSON(rawToChar(history_baseline_decompress(openssl::base64_decode(payload$data),payload$json_bytes)),simplifyVector=FALSE)
stopifnot(identical(wire$cache$type,'null'))
row_frame<-function(payload){
  row<-rival_finance_evidence(ctx$account_id,ctx$championship_id,ctx$season,'eligibility',rival_snapshot_id(rival_snapshot_scope(ctx)),payload)
  jsonlite::fromJSON(jsonlite::toJSON(list(row),auto_unbox=TRUE,na='null',null='null',digits=NA))
}
reads<-0L
supabase_get<-function(table,query){reads<<-reads+1L;stopifnot(table=='rival_finance_evidence_latest',query$limit==1L);row_frame(payload)}
restored<-do.call(read_rival_calculation_snapshot,ctx)
stopifnot(isTRUE(restored$ledger_complete),reads==1L,is.null(restored$cache),identical(restored$team_finances,calculated$team_finances),
  identical(restored$cash_ledgers,calculated$cash_ledgers),
  identical(lapply(restored$cash_ledgers,function(ledger)attr(ledger,'round_bonus_totals')),expected))
rows<-data.frame(team_id=c('a','b'),team_name=c('Alpha','Beta'),round=1,points=c(5,10))
before<-classification_bonus_table(classification_window_table(rows),rows,calculated,c(1,1))
after<-classification_bonus_table(classification_window_table(rows),rows,restored,c(1,1))
stopifnot(identical(before,after),identical(after$points_bonus,c(20,10)),sum(after$best_xi_bonus)==0,
  sum(is.na(after$mvp_bonus))==1L)
cat('PASS oversized detailed cache retains exact numeric, zero and unknown bonuses through one durable read\n')
stopifnot(!rival_snapshot_bonus_valid(setNames(expected,c('a','foreign')),teams$teamid))
bad<-expected;bad$a<-rbind(bad$a,bad$a[1,,drop=FALSE]);stopifnot(!rival_snapshot_bonus_valid(bad,teams$teamid))
bad<-expected;bad$a$amount[1]<-Inf;stopifnot(!rival_snapshot_bonus_valid(bad,teams$teamid))
bad<-expected;bad$a$amount<-as.character(bad$a$amount);stopifnot(!rival_snapshot_bonus_valid(bad,teams$teamid))
cat('PASS foreign team, duplicate component, infinite amount and nonnumeric projection rejected\n')
# Pending round identity must survive the same compact fallback.
calculated$pending_bonus_rounds<-1L
payload<-rival_snapshot_payload(cache,calculated,ctx)
pending<-do.call(read_rival_calculation_snapshot,ctx)
stopifnot(identical(as.numeric(pending$pending_bonus_rounds),1),
  all(attr(pending$cash_ledgers$a,'round_bonus_totals')$settlement_pending))
bad<-expected;bad$a$settlement_pending<-"true";stopifnot(!rival_snapshot_bonus_valid(bad,teams$teamid))
cat('PASS pending settlement state survives compact storage and malformed flags are rejected\n')
# Legacy summary-only snapshots stay readable and truthfully have no bonus data.
legacy_summary<-wire$summary
legacy_summary$attributes$round_bonus_totals<-NULL
legacy_summary$attribute_order<-as.list(setdiff(unlist(legacy_summary$attribute_order),'round_bonus_totals'))
wire$summary<-legacy_summary
raw<-charToRaw(jsonlite::toJSON(wire,auto_unbox=TRUE,na='null',null='null',digits=NA))
compressed<-history_baseline_compress(raw)
payload$json_bytes<-length(raw);payload$data<-openssl::base64_encode(compressed);payload$sha256<-history_baseline_hash(compressed)
payload$ledger_archive<-NULL;payload$ledger_manifest<-NULL
legacy<-do.call(read_rival_calculation_snapshot,ctx)
stopifnot(!is.null(legacy),!legacy$ledger_complete,!length(legacy$cash_ledgers),identical(legacy$team_finances,calculated$team_finances))
cat('PASS legacy summary-only snapshot remains readable without fabricated bonuses\n')
