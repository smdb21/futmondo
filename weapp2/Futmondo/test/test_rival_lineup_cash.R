options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source("global.R"))
lock <- "2026-11-20T20:00:00Z"
at <- "2026-11-25T12:00:00Z"
rounds <- data.frame(round_id="r",round_number=13L,is_finished=TRUE,status="closed")
fixtures <- data.frame(round_id="r",begin_process=lock,all_fixtures_dated=TRUE)
fixtures$fixture_dates <- I(list(c(lock,"2026-11-23T20:00:00Z")))
calendar <- list(fixtures=fixtures,rounds=rounds)
p <- list(team_id="a",round_id="r",status="closed",negative=FALSE,
  raw_lineup=list(budget=197900476,status="closed",negative=FALSE))
record <- rival_finance_evidence("account","league","season","round_lineup","a:r",p,fm_time(at))
select <- function(rec=record,mov=data.frame(),cal=calendar)rival_lineup_cash_checks(list(rec),cal,mov,fm_time(at))$a
check <- select()
stopifnot(check$cash==197900476,check$date==lock)
entries <- dplyr::bind_rows(rival_cash_entry("opening","Opening cash",300000000,"rules","opening"),
  rival_cash_entry("old","Older outgoings",-117892857,"market","buy","2026-11-19T12:00:00Z"),
  rival_cash_entry("later","Subsequent net activity",-199798788,"market","buy","2026-11-26T12:00:00Z"),
  rival_cash_entry("sale","Turrientes",1900000,"market","sell","2026-11-27T18:56:07Z"))
ledger <- rival_cash_ledger(entries,check)
stopifnot(identical(ledger$running_balance,c(300000000,182107143,197900476,-1898312,1688)),
  tail(rival_cash_ledger(entries)$running_balance,1)==-15791645,
  rival_cash_net_gain(ledger)==rival_cash_net_gain(rival_cash_ledger(entries)),
  is.na(ledger$amount[ledger$type=="cash observation"]),!ledger$included[ledger$type=="cash observation"])
ledger$round_number <- NA_integer_
view <- rivals_cash_history_rows(ledger)
stopifnot(view$running_balance[view$type=="cash observation"]==197900476,
  !grepl("excluded",view$concept[view$type=="cash observation"]))
for(kind in c("null","negative","zero","wrong_id","future","open","undated","tie","straddle","split")){
  changed <- record
  mov <- data.frame()
  if(kind=="null")changed$payload$raw_lineup$budget <- NULL
  if(kind=="negative")changed$payload$raw_lineup$budget <- -1
  if(kind=="zero")changed$payload$raw_lineup$budget <- 0
  if(kind=="wrong_id")changed$source_id <- "lineup:b:r"
  if(kind=="future")changed$observed_at <- "2026-11-27T12:00:00Z"
  if(kind=="open")changed$payload$raw_lineup$status <- "open"
  if(kind %in% c("straddle","split"))mov <- data.frame(round_id=if(kind=="split")"r"else"previous",
    bonus_type="points",settlement_label=c("1","1.1"),date=c("2026-11-19","2026-11-22"))
  if(kind %in% c("tie","undated")){
    e <- entries;e$date[2] <- if(kind=="tie")lock else ""
    stopifnot(!any(rival_cash_ledger(e,check)$type=="cash observation"))
  }else if(kind=="zero")stopifnot(select(changed)$cash==0)else stopifnot(is.null(select(changed,mov)))
}
neg <- record
neg$payload$negative <- neg$payload$raw_lineup$negative <- TRUE
neg$payload$raw_lineup$budget <- -123
stopifnot(select(neg)$cash == -123)
cat("PASS closed lineup cash, post-lock transfers, fallback, split settlements, UI, negative and zero budgets\n")

scope <- list(account_id="fixture",championship_id="cup",competition_id="competition",season="season")
publish_active_league_contexts(c(userid="fixture",token="synthetic"),list(
  championships=list(list(id="cup",league="competition",ps="stats")),
  leagues=list(list(`_id`="competition",season="season"))))
observe <- function(type,id,p)rival_finance_evidence("fixture","cup","season",type,id,p,fm_time(at),"competition")
cal <- observe("eligibility",paste0("calendar:v1:",as.character(openssl::sha256("competition"))),
  list(kind="league_round_calendar",version=2,completeness="complete",scope=c(scope,list(league_id="competition")),
  rows=rounds,fixture_rounds=fixtures))
lineup <- observe("round_lineup","a:r",p)
trade <- data.frame(id="s",created="2026-11-27T18:56:07Z",buyer_team_id="",seller_team_id="a",
  player_id="p",player_name="Player",price=1900000,is_clause=FALSE)
cache <- rival_calculation_cache()
build <- function(ev)build_rival_cash_dataset(data.frame(teamid="a",teamname="A"),"cup",ev,pressroom=trade,
  rules=list(initial_budget=100),account_id="fixture",season="season",now=fm_time("2026-11-27T19:00:00Z"),cache=cache)
first <- build(list(cal,lineup))
stopifnot(first$team_finances$estimated_cash==199800476,identical(first,build(list(cal,lineup))),cache$hits==1L)
lineup$season <- "other"
stopifnot(build(list(cal,lineup))$team_finances$estimated_cash==1900100)
cat("PASS normal accounting entry point, scoped calendar, cache hit and foreign season rejection\n")
