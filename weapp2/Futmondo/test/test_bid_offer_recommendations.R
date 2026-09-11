#!/usr/bin/env Rscript
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source("intelligence_engine.R"))

feed_for <- function(players) generate_command_center_feed(
  login = c(token = "offline", userid = "u"), championship_id = "c",
  user_team_id = "mine", user_teams_df = data.frame(), players_df = players,
  market_candidates = data.frame(), clause_candidates = data.frame())

base <- data.frame(id = c("missing", "valid"), name = c(NA, "Koke"),
  user_team_id = c("mine", "mine"), bid_price = c("NA", "900"),
  value = c(NA_real_, NA_real_), fis_score = c(50, 50), fis_tier = c("Hold", "Hold"),
  stringsAsFactors = FALSE)
feed <- feed_for(base)
bids <- feed[feed$type == "Bid", , drop = FALSE]
stopifnot(nrow(bids) == 1L, bids$title == "BID OFFER: Koke",
  !grepl("NA", bids$title, fixed = TRUE), !grepl("NA", bids$description, fixed = TRUE),
  bids$description == "Active bid of 900 EUR. Player valuation is unavailable; evaluate the offer manually.",
  bids$action_label == "Evaluate")

base$value[2] <- 1000
feed <- feed_for(base)
bids <- feed[feed$type == "Bid", , drop = FALSE]
stopifnot(nrow(bids) == 1L,
  bids$description == "Active bid of 900 EUR on player valued at 1000 EUR. Accept recommended.",
  bids$action_label == "Accept")
cat("BID OFFER RECOMMENDATIONS: 2 passed / 0 failed\n")
