#!/usr/bin/env Rscript
# Focused offline semantic tests; no API, credentials, packages, or persistence.
source("prediction_engine.R", local = TRUE)
results <- logical()
check <- function(label, expression) {
  error <- tryCatch({ force(expression); NULL }, error = identity)
  results[label] <<- is.null(error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", label,
      if (!is.null(error)) paste0(": ", conditionMessage(error)) else "", "\n", sep = "")
}
near <- function(a, b) isTRUE(all.equal(a, b, check.attributes = FALSE))
cutoff <- as.POSIXct("2026-08-10 12:00:00", tz = "UTC")
day <- 86400
auction <- data.frame(championship_id = "league", auction_id = c("a", "b", "c", "d"),
  player_id = "player", settled_at = as.numeric(cutoff) - c(4, 3, 2, 1) * day,
  observed_at = as.numeric(cutoff) - c(4, 3, 2, 1) * day + 60,
  winner_id = c("m1", "m2", "m1", "m2"), winning_bid = c(110, 108, 115, 111),
  reference_value = 100, reference_observed_at = as.numeric(cutoff) - c(4, 3, 2, 1) * day - 3600,
  visibility = "complete", stringsAsFactors = FALSE)
auction$eligible_manager_ids <- I(rep(list(c("m1", "m2", "new")), 4))
auction$bids <- I(list(list(list(manager_id = "m1", amount = 105), list(manager_id = "m2", amount = 104)),
  list(), list(list(manager_id = "m2", amount = 112)), list()))
normal <- normalize_prediction_auctions(auction, cutoff)
model <- fit_rival_bid_model(normal$auctions, normal$bids, as_of = cutoff)

check("winner price overrides repeated nested winner", {
  stopifnot(nrow(normal$bids) == 6,
    normal$bids$amount[normal$bids$auction_id == "a" & normal$bids$manager_id == "m1"] == 110,
    sum(normal$bids$is_winner) == 4)
})
check("repeated captures do not multiply labels or opportunities", {
  duplicated <- normalize_prediction_auctions(rbind(auction, auction), cutoff)
  fitted <- fit_rival_bid_model(duplicated$auctions, duplicated$bids, as_of = cutoff)
  stopifnot(nrow(duplicated$auctions) == 4, nrow(duplicated$bids) == 6,
    near(fitted$managers, model$managers))
})
check("empty observed bids are distinct from missing bids", {
  x <- auction
  x$bids <- I(list(NULL, list(), list(), list()))
  n <- normalize_prediction_auctions(x, cutoff)
  stopifnot(n$auctions$visibility[n$auctions$auction_id == "a"] == "unknown",
    n$auctions$visibility[n$auctions$auction_id == "b"] == "complete")
})
check("eligible non-bidders enter denominator only for complete opportunities", {
  m1 <- model$managers[model$managers$manager_id == "m1", ]
  m2 <- model$managers[model$managers$manager_id == "m2", ]
  new <- model$managers[model$managers$manager_id == "new", ]
  stopifnot(m1$eligible_opportunities == 4, m1$observed_participations == 2,
    m2$observed_participations == 4, new$observed_participations == 0,
    new$participation_probability > 0)
})
check("unknown visibility does not invent non-bids or participation precision", {
  x <- normal$auctions; x$visibility <- "unknown"
  m <- fit_rival_bid_model(x, normal$bids, as_of = cutoff)
  stopifnot(all(m$managers$eligible_opportunities == 0),
    all(is.na(m$managers$participation_probability)), all(m$managers$observed_bids >= 0))
})
check("missing eligible-manager coverage prevents negative training labels", {
  x <- normal$auctions; x$eligible_manager_ids <- NULL
  m <- fit_rival_bid_model(x, normal$bids, as_of = cutoff)
  stopifnot(all(is.na(m$managers$participation_probability)))
})
check("aliases accept persistence auction and bidder schemas", {
  a <- normal$auctions; names(a)[names(a) == "auction_id"] <- "event_id"
  b <- normal$bids; names(b)[names(b) == "manager_id"] <- "bidder_id"
  stopifnot(near(fit_rival_bid_model(a, b, as_of = cutoff)$managers, model$managers))
})
check("future bid captures and reference prices cannot leak into fit", {
  a <- normal$auctions; a$reference_observed_at <- a$settled_at + 1
  m <- fit_rival_bid_model(a, normal$bids, as_of = cutoff)
  stopifnot(m$metadata$premium_samples == 0, all(is.na(m$managers$log_ratio_mean)))
  b <- normal$bids; b$observed_at <- as.numeric(cutoff) + 1
  m <- fit_rival_bid_model(normal$auctions, b, as_of = cutoff)
  stopifnot(m$metadata$bids == 0)
})
check("cold start uses pooled amount distribution and interval, not individual certainty", {
  m <- fit_rival_bid_model(normal$auctions, normal$bids, manager_ids = "cold", as_of = cutoff)
  cold <- m$managers[m$managers$manager_id == "cold", ]
  stopifnot(cold$premium_samples == 0, cold$method == "pooled_prior",
    cold$participation_low < cold$participation_high,
    cold$conditional_bid_ratio_low < cold$conditional_bid_ratio_high)
})
curve <- predict_auction_win_curve(model, c(0, 80, 100, 110, 120, 150), 100, user_id = "new")
check("win curve is monotonic, bounded, and has scenario envelopes", {
  stopifnot(all(diff(curve$p_win) >= 0), all(curve$p_win >= 0 & curve$p_win <= 1),
    all(curve$p_win_low <= curve$p_win), all(curve$p_win_high >= curve$p_win),
    curve$p_win[1] == 0)
})
check("user is excluded from rival product", {
  reduced <- model; reduced$managers <- reduced$managers[reduced$managers$manager_id != "m1", ]
  a <- predict_auction_win_curve(model, c(90, 110, 130), 100, user_id = "m1")
  b <- predict_auction_win_curve(reduced, c(90, 110, 130), 100)
  stopifnot(near(a, b))
})
check("visible active bid is a strict lower bound on winning bid", {
  active <- data.frame(manager_id = "m1", amount = 115)
  x <- predict_auction_win_curve(model, c(100, 115, 116, 150), 100, active_bids = active)
  stopifnot(all(x$p_win[1:2] == 0), x$p_win[4] > x$p_win[3])
})
check("unavailable evidence abstains from win probabilities", {
  x <- normal$auctions; x$visibility <- "unknown"
  m <- fit_rival_bid_model(x, normal$bids, as_of = cutoff)
  stopifnot(all(is.na(predict_auction_win_curve(m, c(100, 120), 100)$p_win)),
    all(is.na(predict_auction_win_curve(model, 100, NA_real_)$p_win)))
})

players <- data.frame(id = c("p1", "p2", "p3"), role = "MF", status = c("ok", "injured", NA),
  value = c(110, 200, 300), observed_at = as.numeric(cutoff),
  average.average = c(6, 7, NA), average.matches = c(3, 3, NA))
history <- data.frame(player_id = rep(c("p1", "p2"), each = 3), round_id = rep(1:3, 2),
  points = c(2, 6, 10, 4, 5, 6), observed_at = as.numeric(cutoff) - rep(c(21, 14, 7), 2) * day,
  played = TRUE, role = "MF")
history$occurred_at <- history$observed_at - 3600
forecast <- forecast_fantasy_points(players, history, cutoff)
check("point forecasts shrink recency-weighted match observations and expose uncertainty", {
  p <- forecast[forecast$player_id == "p1" & forecast$horizon == 1, ]
  stopifnot(p$n_observations == 3, p$expected_points > 5, p$expected_points < 7,
    p$lower < p$expected_points, p$upper > p$expected_points, p$method == "shrunk_round_history")
})
check("injured next round is excluded and unknown is never relabeled healthy", {
  stopifnot(forecast$expected_points[forecast$player_id == "p2" & forecast$horizon == 1] == 0,
    is.na(forecast$expected_points[forecast$player_id == "p2" & forecast$horizon == 3]),
    all(forecast$availability[forecast$player_id == "p3"] == "unknown"))
})
check("three-round totals are points, not dimensionless ratings", {
  x <- forecast[forecast$player_id == "p1", ]
  stopifnot(near(x$expected_points[x$horizon == 3], 3 * x$expected_points[x$horizon == 1]))
})
check("missing history differs from explicitly did not play", {
  missing <- history; missing$points[1] <- NA; missing$played[1] <- NA
  dnp <- missing; dnp$played[1] <- FALSE
  m <- forecast_fantasy_points(players, missing, cutoff, 1)
  d <- forecast_fantasy_points(players, dnp, cutoff, 1)
  stopifnot(m$n_observations[1] == 2, d$n_observations[1] == 3,
    d$expected_points[1] < m$expected_points[1])
})
check("future match outcomes and captures cannot change forecasts", {
  extra <- history[1, ]; extra$round_id <- 99; extra$points <- 1000; extra$observed_at <- as.numeric(cutoff) + 1
  stopifnot(near(forecast_fantasy_points(players, rbind(history, extra), cutoff), forecast))
  extra$observed_at <- as.numeric(cutoff) - 1
  h <- rbind(history, extra); h$captured_at <- h$observed_at; h$captured_at[nrow(h)] <- as.numeric(cutoff) + 1
  stopifnot(near(forecast_fantasy_points(players, h, cutoff), forecast))
})
check("aggregate fallback requires timestamp and does not invent uncertainty", {
  f <- forecast_fantasy_points(players[1, ], as_of = cutoff, horizons = 1)
  stopifnot(f$expected_points == 6, is.na(f$lower), f$method == "aggregate_average_baseline")
  p <- players[1, ]; p$observed_at <- NULL
  stopifnot(is.na(forecast_fantasy_points(p, as_of = cutoff, horizons = 1)$expected_points))
})
check("repeat round revisions count once and use last known revision", {
  revised <- history[1, ]; revised$points <- 4; revised$observed_at <- as.numeric(cutoff) - day
  f <- forecast_fantasy_points(players, rbind(history, revised), cutoff, 1)
  stopifnot(f$n_observations[1] == 3, f$expected_points[1] != forecast$expected_points[1])
})

prices <- data.frame(player_id = rep(c("p1", "p2"), each = 5), value = c(100, 102, 101, 105, 110, 210, 205, 208, 202, 200),
  observed_at = as.numeric(cutoff) - rep(4:0, 2) * day)
resale <- forecast_resale_values(players, prices, cutoff, execution_ratio = 0.9)
check("resale forecasts expose 1, 3, 7 day intervals and conservative net execution proceeds", {
  stopifnot(nrow(resale) == 9, identical(unique(resale$horizon), c(1, 3, 7)),
    all(resale$lower_value <= resale$expected_value), all(resale$upper_value >= resale$lower_value),
    near(resale$conservative_proceeds, resale$lower_value * 0.9))
})
check("future prices and captures cannot leak into resale forecasts", {
  extra <- prices[1, ]; extra$value <- 100000; extra$observed_at <- as.numeric(cutoff) + 1
  stopifnot(near(forecast_resale_values(players, rbind(prices, extra), cutoff, execution_ratio = 0.9), resale))
  h <- rbind(prices, extra); h$observed_at[nrow(h)] <- as.numeric(cutoff) - 1
  h$captured_at <- h$observed_at; h$captured_at[nrow(h)] <- as.numeric(cutoff) + 1
  stopifnot(near(forecast_resale_values(players, h, cutoff, execution_ratio = 0.9), resale))
})
check("valuation is not executable proceeds without an explicit execution assumption", {
  f <- forecast_resale_values(players, prices, cutoff)
  stopifnot(all(is.finite(f$expected_value)), all(is.na(f$conservative_proceeds)))
})
check("price cold start uses no-change baseline and cannot justify profit trading", {
  f <- forecast_resale_values(players, as_of = cutoff, execution_ratio = 0.9)
  stopifnot(all(f$method == "no_change_baseline"), all(is.na(f$conservative_proceeds)),
    near(f$expected_value, rep(players$value, each = 3)))
})
check("unknown or future current valuations do not masquerade as past data", {
  p <- players; p$observed_at <- as.numeric(cutoff) + 1
  f <- forecast_resale_values(p, as_of = cutoff)
  stopifnot(all(is.na(f$expected_value)))
})
check("fee handling deducts once from conservative sale proceeds", {
  f <- forecast_resale_values(players, prices, cutoff, execution_ratio = 0.9, fees = 2)
  stopifnot(all(f$fees == 2))
  stopifnot(near(f$conservative_proceeds, pmax(0, resale$conservative_proceeds - 2)))
})
check("sparse intraday snapshots do not inflate daily return counts", {
  h <- prices[1, ]; h <- h[rep(1, 10), ]; h$observed_at <- as.numeric(cutoff) - seq(100, 1000, 100)
  f <- forecast_resale_values(players[1, ], h, cutoff)
  stopifnot(all(f$n_observations == 0), all(f$method == "no_change_baseline"))
})
check("profit-optimal bid can prefer lower win chance and respects every cap", {
  c <- data.frame(bid = c(50, 80, 110), p_win = c(0.5, 0.9, 1))
  bid <- select_profit_bid(c, 100, 200, max_bid = 150)
  stopifnot(bid$recommended_bid == 50, bid$expected_profit == 25,
    select_profit_bid(c, 100, 40)$recommended_bid == 0,
    select_profit_bid(c, 100, 200, max_bid = 40)$recommended_bid == 0)
})
check("no bid wins over loss and unknown economics", {
  c <- data.frame(bid = c(90, 100), p_win = c(0.8, 0.9))
  stopifnot(select_profit_bid(c, 80, 100)$action == "no_bid",
    select_profit_bid(c, NA_real_, 100)$action == "no_bid",
    select_profit_bid(c, 200, NA_real_)$action == "no_bid",
    select_profit_bid(c, 200, -1)$action == "no_bid")
})

roster <- data.frame(id = paste0("r", 1:12), role = c("GK", rep("DF", 4), rep("MF", 4), rep("FW", 3)), value = 10)
legal <- function(x) list(feasible = nrow(x) >= 11 && any(x$role == "GK"))
check("joint basket reserves cash against all purchases winning", {
  buy <- data.frame(id = c("n1", "n2"), role = "MF", price = c(60, 60))
  x <- evaluate_trade_basket(roster, buy, verified_cash = 100, legal_check = legal)
  stopifnot(!x$valid, x$cash_remaining == -20, "insufficient_cash_for_all_wins" %in% x$reasons)
})
check("committed funds count once and exact zero remaining cash is valid", {
  buy <- data.frame(id = "n1", role = "MF", price = 80)
  x <- evaluate_trade_basket(roster, buy, verified_cash = 100, committed_cash = 20, legal_check = legal)
  stopifnot(x$valid, x$cash_remaining == 0)
})
check("sale of only goalkeeper invalidates legal XI despite positive cash profit", {
  sale <- data.frame(id = "r1", proceeds = 1000, completed = TRUE)
  x <- evaluate_trade_basket(roster, sales = sale, verified_cash = 0, legal_check = legal)
  stopifnot(!x$valid, "no_legal_starting_xi" %in% x$reasons)
})
check("unsettled sale proceeds cannot fund purchases", {
  sale <- data.frame(id = "r12", proceeds = 1000, completed = FALSE)
  x <- evaluate_trade_basket(roster, sales = sale, verified_cash = 0, legal_check = legal)
  stopifnot(!x$valid, "sale_proceeds_not_yet_settled" %in% x$reasons)
})
check("heterogeneous schemas preserve players and allow a verified legal profit trade", {
  buy <- data.frame(id = "n1", role = "MF", price = 50, extra = "new")
  sale <- data.frame(id = "r12", proceeds = 60, completed = TRUE)
  x <- evaluate_trade_basket(roster, buy, sale, verified_cash = 0, max_roster = 12, legal_check = legal)
  stopifnot(x$valid, nrow(x$roster) == 12, x$cash_remaining == 10, "extra" %in% names(x$roster))
})
check("duplicate ownership, capacity, and legality failures are explicit", {
  own <- data.frame(id = "r1", price = 1)
  stopifnot(!evaluate_trade_basket(roster, own, verified_cash = 10, legal_check = legal)$valid)
  buy <- data.frame(id = "new", role = "MF", price = 1)
  stopifnot(!evaluate_trade_basket(roster, buy, verified_cash = 10, max_roster = 12, legal_check = legal)$valid,
    !evaluate_trade_basket(roster, verified_cash = 10, legal_check = function(x) stop("failed"))$valid,
    !evaluate_trade_basket(roster, verified_cash = 10)$valid)
})
check("empty inputs produce empty forecasts without plausible fake scores", {
  stopifnot(nrow(forecast_fantasy_points(data.frame())) == 0,
    nrow(forecast_resale_values(data.frame())) == 0,
    nrow(normalize_prediction_auctions(data.frame())$bids) == 0,
    nrow(fit_rival_bid_model(data.frame(), data.frame())$managers) == 0)
})

check("explicit current managers exclude retired opponents while preserving pooled training", {
  m <- fit_rival_bid_model(normal$auctions, normal$bids, manager_ids = c("m1", "cold"), as_of = cutoff)
  stopifnot(setequal(m$managers$manager_id, c("m1", "cold")), m$metadata$bids == 6)
})
check("known active participants override unknown historical participation", {
  a <- normal$auctions; a$visibility <- "unknown"
  m <- fit_rival_bid_model(a, normal$bids, as_of = cutoff)
  active <- data.frame(manager_id = c("m1", "m2"), amount = c(110, 108))
  x <- predict_auction_win_curve(m, c(100, 120, 150), 100, user_id = "new", active_bids = active)
  stopifnot(x$p_win[1] == 0, is.finite(x$p_win[2]), x$p_win[3] > 0)
})
check("an unseen active rival cannot be silently ignored", {
  active <- data.frame(manager_id = "never_seen", amount = 200)
  x <- predict_auction_win_curve(model, c(100, 300), 100, active_bids = active)
  stopifnot(all(is.na(x$p_win)))
})
check("future availability cannot contaminate a historical point forecast", {
  p <- players; p$observed_at <- as.numeric(cutoff) + 1
  f <- forecast_fantasy_points(p, history, cutoff, horizons = 1)
  stopifnot(all(f$availability == "unknown"), f$expected_points[f$player_id == "p2"] > 0)
})
backtest <- rolling_bid_backtest(normal$auctions, normal$bids, min_train_dates = 2, as_of = cutoff)
check("rolling backtest freezes training before whole settlement-day folds", {
  stopifnot(backtest$status == "evaluated", nrow(backtest$folds) == 2,
    all(backtest$folds$train_max_settled_at < backtest$folds$cutoff),
    all(backtest$folds$train_dates >= 2), nrow(backtest$participation) == 6,
    !any(backtest$participation$auction_id %in% c("a", "b")))
})
check("backtest reports held-out accuracy with observed denominators", {
  m <- backtest$metrics
  stopifnot(all(c("participation_brier", "participation_log_loss", "premium_log_mae",
    "premium_80pct_coverage", "fixed_grid_win_brier") %in% m$metric),
    all(is.finite(m$value)), all(m$observations > 0))
  p <- backtest$participation
  stopifnot(near(m$value[m$metric == "participation_brier"], mean((p$prediction - p$observed)^2)))
})
check("later outcomes cannot change earlier fold predictions", {
  b <- normal$bids; b$amount[b$auction_id == "d"] <- 5000
  changed <- rolling_bid_backtest(normal$auctions, b, min_train_dates = 2, as_of = cutoff)
  stopifnot(near(backtest$participation$prediction, changed$participation$prediction),
    near(backtest$premiums$prediction, changed$premiums$prediction),
    near(backtest$wins$prediction, changed$wins$prediction))
})
check("late-imported history cannot fabricate a historical backtest", {
  a <- normal$auctions; a$observed_at <- as.numeric(cutoff)
  b <- normal$bids; b$observed_at <- as.numeric(cutoff)
  x <- rolling_bid_backtest(a, b, min_train_dates = 2, as_of = cutoff)
  stopifnot(x$status == "insufficient_history", nrow(x$metrics) == 0)
})
check("partial test visibility contributes no negative participation or win labels", {
  a <- normal$auctions; a$visibility[a$auction_id %in% c("c", "d")] <- "partial"
  x <- rolling_bid_backtest(a, normal$bids, min_train_dates = 2, as_of = cutoff)
  stopifnot(nrow(x$participation) == 0, nrow(x$wins) == 0, nrow(x$premiums) > 0)
})

eval_start <- as.POSIXct('2026-07-01 00:00:00',tz='UTC')
eval_players <- data.frame(id=c('p1','p2'),role=c('MF','FW'),status='injured',
  average.average=999,average.matches=20,observed_at=eval_start+86400*90)
point_history <- data.frame(player_id=rep(c('p1','p2'),6),round_id=rep(paste0('r',1:6),each=2),
  round_start_at=eval_start+rep(0:5,each=2)*7*86400,
  observed_at=eval_start+(rep(0:5,each=2)*7+1)*86400,
  captured_at=eval_start+(rep(0:5,each=2)*7+1)*86400+60,
  points=c(2,3,4,5,1,4,7,2,8,5,3,6),score_status='final')
point_eval <- rolling_point_backtest(eval_players,point_history,min_train_rounds=2,as_of=eval_start+86400*45)
check('point validation splits entire rounds before first kickoff and ignores future snapshots', {
  stopifnot(point_eval$status=='evaluated',nrow(point_eval$folds)==4,nrow(point_eval$predictions)==8,
    all(point_eval$folds$train_max_observed_at<point_eval$folds$cutoff),
    all(point_eval$predictions$prediction>0),all(point_eval$predictions$prediction<10),
    point_eval$metrics$observations[point_eval$metrics$metric=='mae']==8)
})
check('point validation compares predictions with a past-only last-five baseline', {
  x<-point_eval$predictions
  stopifnot(near(x$baseline[1:2],c(3,4)),
    near(point_eval$metrics$value[point_eval$metrics$metric=='mae'],mean(abs(x$prediction-x$observed))))
})
check('changing held-out final round points changes errors but not frozen predictions', {
  h<-point_history;h$points[h$round_id=='r6']<-1000
  x<-rolling_point_backtest(eval_players,h,2,eval_start+86400*45)
  stopifnot(near(x$predictions$prediction,point_eval$predictions$prediction),
    x$metrics$value[x$metrics$metric=='mae']>point_eval$metrics$value[point_eval$metrics$metric=='mae'])
})
check('point evaluation refuses missing kickoff timestamps and late-imported history', {
  h<-point_history;h$round_start_at<-NULL
  stopifnot(rolling_point_backtest(eval_players,h)$status=='insufficient_timing')
  h<-point_history;h$captured_at<-eval_start+86400*40
  stopifnot(rolling_point_backtest(eval_players,h,2,eval_start+86400*45)$status=='insufficient_history')
})
eval_prices <- data.frame(player_id='p1',value=100*exp((0:14)*.01),
  observed_at=eval_start+(0:14)*86400+43200,captured_at=eval_start+(0:14)*86400+43260)
price_eval <- rolling_resale_backtest(eval_players,eval_prices,min_train_dates=4,
  as_of=eval_start+86400*16,horizons=c(1,3))
check('resale validation uses historical daily folds and separately reports horizons', {
  stopifnot(price_eval$status=='evaluated',nrow(price_eval$predictions)>0,
    all(price_eval$folds$train_max_observed_at<price_eval$folds$cutoff),
    setequal(price_eval$metrics$horizon,c(1,3)),
    all(price_eval$predictions$outcome_at>=price_eval$predictions$target_at),
    all(price_eval$predictions$outcome_at-price_eval$predictions$target_at<=12*3600))
})
check('resale baseline is last available value and targets need observed prices', {
  x<-price_eval$predictions[1,]
  stopifnot(near(x$baseline,tail(eval_prices$value[eval_prices$observed_at<x$cutoff],1)),
    rolling_resale_backtest(eval_players,eval_prices,4,eval_start+86400*16,tolerance_hours=0)$status=='insufficient_history')
})
check('late captures cannot fabricate resale validation and future targets cannot change earlier predictions', {
  h<-eval_prices;h$captured_at<-eval_start+86400*15
  stopifnot(rolling_resale_backtest(eval_players,h,4,eval_start+86400*16)$status=='insufficient_history')
  h<-eval_prices;h$value[nrow(h)]<-10000
  x<-rolling_resale_backtest(eval_players,h,4,eval_start+86400*16,horizons=c(1,3))
  stopifnot(near(x$predictions$prediction,price_eval$predictions$prediction))
})

check('observed bidders outside declared eligibility invalidate non-bid training labels', {
 a<-normal$auctions;a$eligible_manager_ids<-I(rep(list('absent_manager'),nrow(a)))
 x<-fit_rival_bid_model(a,normal$bids,as_of=cutoff)
 stopifnot(all(is.na(x$managers$participation_probability)),all(x$managers$eligible_opportunities==0))
})
check('persisted round aliases deduplicate final revisions and exclude provisional scores', {
 h<-history;h$round<-h$round_id;h$round_id<-NULL;h$is_final<-TRUE
 extra<-h[1,,drop=FALSE];extra$round<-'future-provisional';extra$points<-1000;extra$is_final<-FALSE
 stopifnot(near(forecast_fantasy_points(players,rbind(h,extra),cutoff),forecast_fantasy_points(players,h,cutoff)))
})

cat(sprintf("\nPrediction engine: %d passed / %d failed\n", sum(results), sum(!results)))
prediction_engine_all_passed <- all(results)
if (!getOption("prediction_engine_no_quit", FALSE)) quit(status = if (prediction_engine_all_passed) 0 else 1)
