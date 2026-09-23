#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source("futmondo_functions.R"))
passed <- 0L
check <- function(name, code) { force(code); passed <<- passed + 1L; cat("PASS", name, "\n") }

check("temporary debt headroom reaches exactly half team value", {
  x <- acquisition_headroom(cash=-100, team_value=10000, withheld=200, commitments=300)
  stopifnot(x$debt_limit==5000, x$minimum_balance== -5000,
    x$projected_committed_balance== -400, x$spendable_budget==4600,
    x$reserved_amount==300)
  boundary <- acquisition_headroom(-5000,10000)
  stopifnot(boundary$spendable_budget==0, boundary$minimum_balance== -5000)
  same_reservation <- acquisition_headroom(cash=1000, team_value=10000, withheld=300, commitments=300)
  stopifnot(same_reservation$projected_committed_balance==700, same_reservation$spendable_budget==5700)
})
check("unknown or invalid finance fails closed", {
  for (x in list(acquisition_headroom(1,NA), acquisition_headroom(1,-1),
      acquisition_headroom(1,10,-1), acquisition_headroom(1,10,0,-1)))
    stopifnot(is.na(x$spendable_budget),is.na(x$debt_limit))
})
check("verified acquisition capacity includes debt and existing bids", {
  saved <- mget(c("get_user_team_info","get_players_from_team","get_market_players"),envir=.GlobalEnv)
  get_user_team_info <- function(...) list(budget=-100,withheld=200,maxBid=10000,teamValue=10000,
    configuration=list(maxPlayersInRoster=20))
  get_players_from_team <- function(...) data.frame(id="owned",value=10000)
  get_market_players <- function(...) data.frame(id="target",bid_id="mine",bid_price=300)
  clear_api_cache()
  cap <- get_acquisition_capacity(c(token="t",userid="u"),"c","team","target")
  stopifnot(cap$status=="ok",cap$funds$spendable_budget==4600,
    cap$funds$debt_limit==5000,cap$funds$minimum_balance== -5000,
    cap$funds$projected_committed_balance== -400, cap$funds$reserved_amount==300)
  finance <- get_financial_snapshot(c(token="t",userid="u"),"c","team")
  stopifnot(finance$commitments$total_amount == 300, finance$commitments$count == 1)
  stopifnot(evaluate_acquisition_preflight(cap,"modify",4900,300)$ok,
    !evaluate_acquisition_preflight(cap,"modify",4901,300)$ok)
  list2env(saved,envir=.GlobalEnv);clear_api_cache()
})
check("roster value verifies debt limit when team summary omits it", {
  saved <- mget(c("get_user_team_info","get_players_from_team","get_market_players"),envir=.GlobalEnv)
  get_user_team_info <- function(...) list(budget=0,withheld=0,maxBid=10000,
    configuration=list(maxPlayersInRoster=20))
  get_players_from_team <- function(...) data.frame(id=c("a","b"),value=c(4000,6000))
  get_market_players <- function(...) data.frame()
  clear_api_cache()
  cap <- get_acquisition_capacity(c(token="t",userid="fallback"),"c","team")
  stopifnot(cap$status=="ok",cap$funds$team_value==10000,cap$funds$spendable_budget==5000)
  finance <- get_financial_snapshot(c(token="t",userid="fallback"),"c","team")
  stopifnot(finance$status=="ok", finance$team_value==10000,
    finance$debt_limit==5000, finance$spendable_budget==5000)
  list2env(saved,envir=.GlobalEnv);clear_api_cache()
})
check("next round selects the earliest future start", {
  rounds <- data.frame(round_number=c(4,6,5),
    begin_process=c("2026-09-10T12:00:00Z","2026-09-20T12:00:00Z","2026-09-12T12:00:00Z"))
  next_round <- next_round_context(rounds,as.POSIXct("2026-09-11T12:00:00Z",tz="UTC"))
  stopifnot(next_round$available,next_round$round_number==5,
    format(next_round$starts_at,tz="UTC",usetz=TRUE)=="2026-09-12 12:00:00 UTC")
})
check("countdown is stable at boundary and handles unavailable dates", {
  now <- as.POSIXct("2026-09-11T12:00:00Z",tz="UTC")
  stopifnot(format_round_countdown(now+90061,now)=="1d 01h 01m 01s",
    format_round_countdown(now-1,now)=="0d 00h 00m 00s",
    format_round_countdown(NA,now)=="Start time unavailable",
    !next_round_context(data.frame(),now)$available)
})
check("current round selects latest started unfinished round", {
  rounds <- data.frame(round_number=c(4,5,6),
    begin_process=c("2026-09-01T12:00:00Z","2026-09-08T12:00:00Z","2026-09-15T12:00:00Z"),
    is_finished=c(TRUE,FALSE,FALSE))
  now <- as.POSIXct("2026-09-11T12:00:00Z",tz="UTC")
  current <- current_round_context(rounds,now)
  all_finished <- transform(rounds,is_finished=TRUE)
  no_current <- current_round_context(all_finished,now)
  stopifnot(current$available,current$round_number==5,isFALSE(no_current$available))
})
check("global UI exposes countdown and deadline solvency warning", {
  ui <- paste(readLines("ui.R",warn=FALSE),collapse="\n")
  server <- paste(readLines("server.R",warn=FALSE),collapse="\n")
  css <- paste(readLines("www/custom_style.css",warn=FALSE),collapse="\n")
  stopifnot(grepl('uiOutput("round_countdown")',ui,fixed=TRUE),
    grepl('output$round_countdown <- renderUI',server,fixed=TRUE),
    grepl('round_finance_RV <- reactive',server,fixed=TRUE),
    grepl('invalidateLater(60000, session)',server,fixed=TRUE),
    grepl('finance <- round_finance_RV()',server,fixed=TRUE),
    grepl('Active offers:',server,fixed=TRUE),
    grepl('commitment_snapshot$total_amount',server,fixed=TRUE),
    grepl('After offers:',server,fixed=TRUE),
    grepl('Can spend:',server,fixed=TRUE),
    grepl('projected_committed_balance',server,fixed=TRUE),
    grepl("before kickoff to score points",server,fixed=TRUE),
    grepl('tags$span("In progress")',server,fixed=TRUE),
    grepl("position: sticky",css,fixed=TRUE),grepl("@media (max-width: 767px)",css,fixed=TRUE))
})
check("countdown clock and financial refresh use separate intervals", {
  server <- paste(readLines("server.R",warn=FALSE),collapse="\n")
  finance_start <- regexpr("round_finance_RV <- reactive",server,fixed=TRUE)[1]
  countdown_start <- regexpr("output$round_countdown <- renderUI",server,fixed=TRUE)[1]
  stopifnot(finance_start > 0, countdown_start > finance_start,
    grepl("invalidateLater(60000, session)",substr(server,finance_start,countdown_start-1),fixed=TRUE),
    grepl("invalidateLater(1000, session)",substr(server,countdown_start,nchar(server)),fixed=TRUE))
})
cat(sprintf("DEBT AND ROUND COUNTDOWN: %d passed / 0 failed\n",passed))
