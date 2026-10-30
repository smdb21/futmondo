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
check("known cash survives loading or failed offers without a debt warning", {
  team <- resource_state(list(budget=180546496,withheld=0,teamValue=350438934))
  loading <- round_finance_display(team,resource_state(status="loading"))
  failed <- round_finance_display(team,resource_state(status="unavailable",error=simpleError("synthetic failure")))
  stopifnot(loading$cash==180546496,loading$offers_loading,is.na(loading$commitments),
    loading$class=="round-countdown-unavailable",loading$message=="Loading active offers…",
    failed$cash==180546496,is.na(failed$projected_committed_balance),
    failed$class=="round-countdown-unavailable",grepl("could not load",failed$message))
})
check("banner uses greater reservation once and retains exact confirmed boundaries", {
  market <- resource_state(data.frame(bid_id=c("own","own"),bid_price=c(200,200)))
  x <- round_finance_display(resource_state(list(budget=1000,withheld=300,teamValue=10000)),market)
  stopifnot(x$commitments==200,x$active_offer_count==1L,x$projected_committed_balance==700,
    x$spendable_budget==5700,x$class=="round-countdown-solvent")
  zero <- round_finance_display(resource_state(list(budget=300,withheld=300,teamValue=10000)),market)
  debt <- round_finance_display(resource_state(list(budget=-10)),resource_state(status="loading"))
  stopifnot(zero$class=="round-countdown-warning",grepl("1 EUR before kickoff",zero$message,fixed=TRUE),
    debt$class=="round-countdown-warning",grepl("active offers still need verification",debt$message,fixed=TRUE))
})
check("missing withheld and malformed or stale market never imply zero reservation", {
  no_withheld <- round_finance_display(resource_state(list(budget=1000,teamValue=10000)),resource_state(data.frame()))
  malformed <- round_finance_display(resource_state(list(budget=1000,withheld=0)),resource_state(data.frame(id="listing")))
  invalid <- round_finance_display(resource_state(list(budget=1000,withheld=0)),resource_state(data.frame(bid_id="own",bid_price=NA_real_)))
  stale <- round_finance_display(resource_state(observed_data(list(budget=1000,withheld=0),status="stale")),resource_state(data.frame()))
  stale_debt <- round_finance_display(resource_state(observed_data(list(budget=-10,withheld=0),status="stale")),resource_state(data.frame()))
  stopifnot(is.na(no_withheld$projected_committed_balance),no_withheld$class=="round-countdown-unavailable",
    is.na(malformed$commitments),is.na(invalid$commitments),stale$cash_stale,
    stale$class=="round-countdown-unavailable",stale_debt$class=="round-countdown-unavailable",
    is.na(stale$projected_committed_balance),grepl("Cached",stale$message))
})
check("real market loader marks valid absent-own-bid fields and reuses the result", {
  saved_post <- futmondo_post; reads <- 0L
  futmondo_post <- function(url, ...) {
    stopifnot(identical(url,MARKET_URL));reads <<- reads+1L
    payload <- jsonlite::toJSON(list(answer=list(list(id="listed",name="Player",value=1000,change=0,points=0))),auto_unbox=TRUE)
    structure(list(status_code=200L,headers=list("Content-Type"="application/json"),content=charToRaw(payload),url=url),class="response")
  }
  clear_api_cache()
  auth <- c(token="synthetic",userid="market-display-fixture")
  rows <- get_market_players(auth,"c","team")
  rows2 <- get_market_players(auth,"c","team")
  x <- round_finance_display(resource_state(list(budget=1000,withheld=0,teamValue=10000)),resource_state(rows))
  stopifnot(reads==1L,nrow(rows)==1L,is.na(rows$bid_id),is.na(rows$bid_price),
    x$commitments==0,x$class=="round-countdown-solvent",identical(rows,rows2))
  futmondo_post <- saved_post;clear_api_cache()
})
check("global UI shares primary states and keeps manual capacity checks separate", {
  ui <- paste(readLines("ui.R",warn=FALSE),collapse="\n")
  server <- paste(readLines("server.R",warn=FALSE),collapse="\n")
  css <- paste(readLines("www/custom_style.css",warn=FALSE),collapse="\n")
  stopifnot(grepl('uiOutput("round_countdown")',ui,fixed=TRUE),
    grepl('output$round_countdown <- renderUI',server,fixed=TRUE),
    grepl('round_finance_display(primary_team_state(), primary_market_state())',server,fixed=TRUE),
    grepl('primary_sources = primary_sources',server,fixed=TRUE),
    !grepl('get_financial_snapshot',server,fixed=TRUE),
    grepl('Active offers:',server,fixed=TRUE),grepl('After offers:',server,fixed=TRUE),grepl('Can spend:',server,fixed=TRUE),
    grepl('projected_committed_balance',server,fixed=TRUE),
    grepl('tags$span("In progress")',server,fixed=TRUE),
    grepl("position: sticky",css,fixed=TRUE),grepl("overflow-wrap: anywhere",css,fixed=TRUE))
})
check("countdown clock and primary financial refresh use separate intervals", {
  server <- paste(readLines("server.R",warn=FALSE),collapse="\n")
  finance_start <- regexpr("round_finance_version <- reactive",server,fixed=TRUE)[1]
  countdown_start <- regexpr("output$round_countdown <- renderUI",server,fixed=TRUE)[1]
  stopifnot(finance_start > 0, countdown_start > finance_start,
    grepl("invalidateLater(60000, session)",substr(server,finance_start,countdown_start-1),fixed=TRUE),
    grepl("invalidateLater(1000, session)",substr(server,countdown_start,nchar(server)),fixed=TRUE))
})
cat(sprintf("DEBT AND ROUND COUNTDOWN: %d passed / 0 failed\n",passed))
