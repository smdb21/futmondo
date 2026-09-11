#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source("intelligence_engine.R"))
deadline <- list(available=TRUE,starts_at=as.POSIXct("2026-09-20 16:00:00",tz="UTC"))
finance <- list(status="ok",projected_committed_balance=6000000)
roster <- data.frame(id=paste0("p",1:15),bid_price=c(3000000,2000000,rep(NA_real_,13)))
now <- clause_deadline_affordability(5000000,finance,roster,deadline)
stopifnot(now$status=="affordable_now",isTRUE(now$affordable),now$required_sales==0)
finance$projected_committed_balance <- 1000000
after_sales <- clause_deadline_affordability(5000000,finance,roster,deadline)
stopifnot(after_sales$status=="affordable_after_sales",isTRUE(after_sales$affordable),
  after_sales$required_sales==4000001,after_sales$sale_proceeds==5000000,
  identical(after_sales$sell_player_ids,c("p1","p2")))
short_roster <- roster[1:11,]
short_roster$bid_price <- c(3000000,rep(NA_real_,10))
blocked <- clause_deadline_affordability(5000000,finance,short_roster,deadline)
stopifnot(blocked$status=="unaffordable",isFALSE(blocked$affordable))
unverified <- clause_deadline_affordability(NULL,finance,roster,deadline)
stopifnot(unverified$status=="unverified",is.na(unverified$affordable))

players <- data.frame(id=c("cheap","costly"),name=c("Cheap clause","Costly clause"),
  value=c(4000000,3000000),clause_price=c(5000000,9000000),clause_transferred=FALSE,
  clause_date="2026-01-01",user_team_id="rival",fis_score=c(80,90),fis_tier="Buy",fis_summary="Good")
feed <- generate_command_center_feed(NULL,"champ","mine",data.frame(),players,
  market_candidates=data.frame(),clause_candidates=players,financial=finance,
  roster_df=roster,next_round=deadline)
clauses <- feed[feed$type=="Clause",,drop=FALSE]
stopifnot(identical(clauses$player_id,"cheap"),
  grepl("accepting observed offers worth 5.000.000 €",clauses$description,fixed=TRUE),
  grepl("positive deadline balance",clauses$description,fixed=TRUE))
cat("CLAUSE DEADLINE AFFORDABILITY: 5 passed / 0 failed\n")
