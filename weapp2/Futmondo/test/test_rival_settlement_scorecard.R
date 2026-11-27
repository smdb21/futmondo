options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source("global.R"))
teams <- data.frame(teamid=c("a","b","c"),teamname=c("A","B","C"))
rules <- list(initial_budget=100,money_per_point=10,ranking_pool=60,ranking_mode="flop",users_to_rank=-1)
receipt <- fm_time("2026-11-09T13:00:00Z")
observed <- receipt+3600
now <- observed+20*86400
e <- function(type,id,p,at=observed)rival_finance_evidence("acct","league","unknown",type,id,p,at)
histories <- function(scores,at)lapply(seq_along(scores),function(i){
  id <- teams$teamid[i]
  e("team_rounds",id,list(team_id=id,rounds=list(list(id="r12",number=12,
    points=scores[i],negative=FALSE,status="closed"))),at)
})
ranking <- function(scores,at)e("eligibility","classification:r12",list(kind="classification_round_ranking",
  round_id="r12",round_number=12,answer=list(metadata=list(status="V"),
    ranking=lapply(seq_along(scores),function(i)list(id=teams$teamid[i],points=scores[i])))),at)
payment <- function(kind,amount)e("movements",kind,list(id=kind,team_id="a",type=kind,
  concept="12",date=rival_finance_stamp(receipt),money=amount))
payments <- list(payment("points",200),payment("ranking",20))
old <- c(20,30,10)
new <- c(31,28,13)
early <- c(histories(old,observed),list(ranking(old,observed)),payments)
recent <- c(histories(new,now),list(ranking(new,now)),payments)
mov <- rival_normalize_movements(rival_cash_latest_movements(payments,"a"),data.frame(round_id="r12",round_number=12))
capture <- function(ev,at=observed)rival_new_payment_scorecards(ev,teams$teamid,"a",rules,mov,at)
card <- capture(early)
stopifnot(length(card)==1L,!length(capture(c(early,card))),!length(capture(recent,now)),
  !length(capture(c(histories(old,receipt-1),list(ranking(old,observed)),payments))))
get_cash <- function(evidence,cache=NULL,press=data.frame())build_rival_cash_dataset(teams,"league",evidence,
  rules=rules,account_id="acct",season="unknown",reference_team_id="a",now=now,cache=cache,pressroom=press)
restored <- c(recent,card)
result <- get_cash(restored)
# A gets its actual 220. B gets 300 points cash and 10 for position, C gets 100 and 30.
stopifnot(identical(result$team_finances$estimated_cash,c(320,410,230)),
  result$point_reconciliation$status=="validated",result$point_reconciliation$expected==200,
  result$cash_ledgers$b$round_points[result$cash_ledgers$b$bonus_type=="points"]==30,
  result$cash_ledgers$b$round_position[result$cash_ledgers$b$bonus_type=="ranking"]==1,
  all(result$cash_ledgers$b$date[result$cash_ledgers$b$bonus_type %in% c("points","ranking")]==rival_finance_stamp(receipt)))
for(id in teams$teamid)stopifnot(tail(result$cash_ledgers[[id]]$running_balance,1)==
  result$team_finances$estimated_cash[result$team_finances$teamid==id])
# Missing, stale, foreign, disqualified, conflicting or superseded evidence fails closed.
for(changed in c("absent","foreign","late","negative","missing_team","revised","installment")){
  bad <- restored
  if(changed=="absent")bad <- recent
  if(changed=="foreign")bad[[length(bad)]]$season <- "other"
  if(changed=="late")bad[[length(bad)]]$observed_at <- rival_finance_stamp(now)
  if(changed=="negative")bad[[length(bad)]]$payload$ranking[[2]]$negative <- TRUE
  if(changed=="missing_team")bad[[length(bad)]]$payload$ranking[[3]] <- NULL
  if(changed=="revised")bad[[6]]$payload$money <- 21
  if(changed=="installment"){
    extra <- payment("points",0);extra$source_id <- extra$payload$id <- "extra"
    extra$payload$concept <- "12.1";bad <- c(bad,list(extra))
  }
  stopifnot(get_cash(bad)$point_reconciliation$status=="mismatch")
}
# A November 12 private-info clock with a hidden budget cannot mask newer evidence.
private <- e("team","b",list(budget=NA_real_,observed_at=rival_finance_stamp(receipt-7*86400)))
ev <- c(restored,list(private))
cache <- rival_calculation_cache()
before <- get_cash(ev,cache)
after <- get_cash(ev,cache)
b <- after$team_finances[2,]
stopifnot(b$as_of==rival_finance_stamp(now),b$observed_cash_at=="",!b$observed_cash_fresh,
  identical(before,after),cache$team_builds==3,cache$hits==3)
# An updated Market purchase can be the newest evidence even without team history.
tx_at <- now-60
tx <- data.frame(id="tx",created=rival_finance_stamp(tx_at),buyer_team_id="b",seller_team_id="",
  player_id="p",player_name="Player",price=150,is_clause=FALSE)
partial <- get_cash(list(private),press=tx)$team_finances
stopifnot(partial$as_of[2]==rival_finance_stamp(tx_at),partial$estimated_cash[2]==-50,
  partial$as_of[3]=="",!partial$observed_cash_fresh[2])
preview <- rival_refresh_finance_row(b,teams[2,,drop=FALSE])
stopifnot(identical(preview$as_of,b$as_of))
cat("PASS archived scores reconcile both receipts, survive revisions and replay, preserve eligibility and drive ledger and freshness on warm and cold reads\n")
