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
  bids$description == "Active bid of 900 €. Player valuation is unavailable; evaluate the offer manually.",
  bids$action_label == "Evaluate")

base$value[2] <- 1000
base$buyPrice <- c(NA_real_, 700)
feed <- feed_for(base)
bids <- feed[feed$type == "Bid", , drop = FALSE]
stopifnot(nrow(bids) == 1L,
  bids$description == paste0("Active bid of 900 € on player valued at 1.000 €. Accept recommended. ",
    "Sale proceeds: 900 €; acquisition cost: 700 €; Net gain: 200 €."),
  bids$action_label == "Accept")

pressroom <- data.frame(id=c("buy1","sell1","buy2"),player_id="valid",
  buyer_team_id=c("mine","other","mine"),seller_team_id=c("other","mine","other"),
  price=c(500,650,750),created=c("2026-01-01","2026-02-01","2026-03-01"))
stopifnot(current_player_acquisition_cost(list(id="valid"),pressroom,"mine")==750,
  is.na(current_player_acquisition_cost(list(id="unknown"),pressroom,"mine")))
cat("BID OFFER RECOMMENDATIONS: 2 passed / 0 failed\n")
