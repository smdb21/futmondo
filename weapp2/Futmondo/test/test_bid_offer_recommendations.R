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
  bids$description == paste("Active bid of 900 €. Player valuation is unavailable; evaluate the offer manually.",
    "Squad position depth unavailable."),
  bids$action_label == "Evaluate")

base$value[2] <- 1000
base$buyPrice <- c(NA_real_, 700)
feed <- feed_for(base)
bids <- feed[feed$type == "Bid", , drop = FALSE]
stopifnot(nrow(bids) == 1L,
  grepl("Hold or negotiate", bids$description, fixed = TRUE),
  grepl("below the 110% acceptance threshold", bids$description, fixed = TRUE),
  grepl("Net gain: 200 €", bids$description, fixed = TRUE),
  bids$action_label == "Evaluate")

premium <- base[2, , drop=FALSE]
premium$bid_price <- 1200
premium$change <- -50
premium$average.average <- 4
feed <- feed_for(premium)
bids <- feed[feed$type == "Bid", , drop = FALSE]
stopifnot(nrow(bids) == 1L,
  grepl("Accept recommended", bids$description, fixed = TRUE),
  grepl("clears the 110% premium threshold", bids$description, fixed = TRUE),
  grepl("Scoring average: 4 points per match", bids$description, fixed = TRUE),
  bids$action_label == "Accept")

bartra <- data.frame(id="bartra",name="Bartra",user_team_id="mine",bid_price=13077175,
  value=13274287,buyPrice=21661732,change=197112,average.average=8,
  fis_score=72,fis_tier="Buy",stringsAsFactors=FALSE)
feed <- feed_for(bartra)
bids <- feed[feed$type == "Bid", , drop = FALSE]
stopifnot(nrow(bids) == 1L,bids$action_label == "Evaluate",
  grepl("98.5% of current value",bids$description,fixed=TRUE),
  grepl("net loss of 8.584.557 €",bids$description,fixed=TRUE),
  grepl("player is rated Buy",bids$description,fixed=TRUE),
  grepl("market value is rising by 197.112 €",bids$description,fixed=TRUE),
  grepl("Scoring average: 8 points per match",bids$description,fixed=TRUE),
  !grepl("Accept recommended",bids$description,fixed=TRUE))


sell_offer <- data.frame(id="sell",name="Konaté",user_team_id="mine",bid_price=11234778,
  value=15000000,buyPrice=14000000,change=-250000,average.average=2.7,
  fis_score=42.8,fis_tier="Sell",fis_summary="",stringsAsFactors=FALSE)
sell_feed <- feed_for(sell_offer)
sell_card <- sell_feed[sell_feed$type=="Sell",,drop=FALSE]
stopifnot(nrow(sell_card)==1L,sell_card$action_label=="Accept Offer",
  sell_card$action_code=="accept_offer",
  grepl("Accept despite a net loss of 2.765.222 €",sell_card$description,fixed=TRUE),
  grepl("rated Sell (FIS 42.8/100)",sell_card$description,fixed=TRUE),
  grepl("scoring average is 2.7 points per match",sell_card$description,fixed=TRUE),
  grepl("market value is falling by 250.000 €",sell_card$description,fixed=TRUE),
  grepl("acquisition cost: 14.000.000 €",sell_card$description,fixed=TRUE),
  nrow(sell_feed[sell_feed$type=="Bid",,drop=FALSE])==0L)


# NA owner/FIS rows can appear while an accepted offer refreshes the roster.
# They must never become synthetic Sell or Hold cards.
transient <- rbind(sell_offer, data.frame(id=NA_character_,name=NA_character_,user_team_id=NA_character_,
  bid_price=NA_real_,value=NA_real_,buyPrice=NA_real_,change=NA_real_,average.average=NA_real_,
  fis_score=NA_real_,fis_tier=NA_character_,fis_summary=NA_character_))
transient_feed <- feed_for(transient)
stopifnot(!any(is.na(transient_feed$player_id)),
  !any(grepl("SELL: NA|HOLD: NA", transient_feed$title)))

pressroom <- data.frame(id=c("buy1","sell1","buy2"),player_id="valid",
  buyer_team_id=c("mine","other","mine"),seller_team_id=c("other","mine","other"),
  price=c(500,650,750),created=c("2026-01-01","2026-02-01","2026-03-01"))
stopifnot(current_player_acquisition_cost(list(id="valid"),pressroom,"mine")==750,
  is.na(current_player_acquisition_cost(list(id="unknown"),pressroom,"mine")))
cat("BID OFFER RECOMMENDATIONS: 6 passed / 0 failed\n")

# Buy candidates also consider change/value relative to the whole league. A
# high-growth Hold player is eligible only when its rise is in the upper league
# quartile; an otherwise identical player with an ordinary rise is not.
league <- data.frame(
  id = paste0("p", 1:5), name = paste0("Player ", 1:5),
  value = rep(1000000, 5), change = c(10000, 20000, 30000, 40000, 100000),
  fis_score = c(60, 60, 60, 60, 60), fis_tier = rep("Hold", 5),
  fis_summary = rep("Stable rating.", 5), stringsAsFactors = FALSE
)
growth_feed <- generate_command_center_feed(
  login = NULL, championship_id = "c", user_team_id = "mine", user_teams_df = data.frame(),
  players_df = league, market_candidates = league, clause_candidates = data.frame()
)
growth_buy <- growth_feed[growth_feed$type == "Buy", , drop = FALSE]
stopifnot(identical(growth_buy$player_id[1], "p5"),
  setequal(growth_buy$player_id, c("p4", "p5")),
  grepl("Value is up 10% today", growth_buy$description[1], fixed = TRUE),
  identical(player_value_growth_ratio(league)[5], 0.1))
cat("BUY VALUE GROWTH: 1 passed / 0 failed\n")
