#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
source("intelligence_engine.R")
# A transient duplicate can carry different ratings for the same player.
players <- data.frame(
  id=c("same","same","other"), name=c("Conflict","Conflict","Stable"),
  user_team_id=c("mine","mine","mine"), fis_tier=c("Sell","Hold","Hold"),
  fis_score=c(30,60,65), fis_summary="", stringsAsFactors=FALSE
)
feed <- generate_command_center_feed(NULL,"league","mine",data.frame(),players,
  market_candidates=data.frame(),clause_candidates=data.frame())
conflict <- feed[feed$player_id=="same",,drop=FALSE]
stopifnot(nrow(conflict)==1L,identical(conflict$type,"Sell"))
stopifnot(any(feed$player_id=="other" & feed$type=="Hold"))
cat("COMMAND CENTER CONFLICTS: 2 passed / 0 failed\n")
