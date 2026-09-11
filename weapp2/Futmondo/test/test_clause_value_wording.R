#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source("intelligence_engine.R"))
low <- clause_value_comparison(5100000,6064210)
high <- clause_value_comparison(5100000,4400345)
very_high <- clause_value_comparison(5100000,5100000/2.519)
equal <- clause_value_comparison(5100000,5100000)
stopifnot(low$available,low$favorable,low$text=="15.9% below market value",
  high$available,isFALSE(high$favorable),high$text=="15.9% above market value",
  very_high$text=="151.9% above market value",equal$text=="at market value",
  !clause_value_comparison(5100000,NA_real_)$available)
players <- data.frame(id=c("low","middle","nacho"),name=c("Low Premium","Middle","Nacho Perez"),
  value=c(4000000,3000000,5100000/2.519),clause_price=5100000,
  clause_transferred=FALSE,clause_date="2026-01-01",user_team_id="rival",
  fis_score=c(80,90,99),fis_tier="Buy",fis_summary="Good player")
feed <- generate_command_center_feed(NULL,"champ","mine",data.frame(),players,
  market_candidates=data.frame(),clause_candidates=players)
clauses <- feed[feed$type=="Clause",,drop=FALSE]
stopifnot(identical(clauses$player_id,c("low","middle")),!"nacho" %in% clauses$player_id,
  grepl("27.5% above market value",clauses$description[1],fixed=TRUE),
  grepl("observed clause-to-value ratios",clauses$description[1],fixed=TRUE))
nacho <- players[players$id=="nacho",,drop=FALSE]
nacho_feed <- generate_command_center_feed(NULL,"champ","mine",data.frame(),nacho,
  market_candidates=data.frame(),clause_candidates=nacho)
description <- nacho_feed$description[nacho_feed$type=="Clause"]
stopifnot(length(description)==1L,grepl("5.100.000 €",description,fixed=TRUE),
  grepl("151.9% above market value",description,fixed=TRUE),
  grepl("observed clause-to-value ratios",description,fixed=TRUE),
  !grepl("discount",description,fixed=TRUE))
cat("CLAUSE VALUE WORDING AND RANKING: 3 passed / 0 failed\n")
