#!/usr/bin/env Rscript
options(futmondo.offline=TRUE);source("data_contracts.R");source("factual_helpers.R")
n<-0L;check<-function(name,code){force(code);n<<-n+1L;cat("PASS",name,"\n")}
check("manual bid minimum uses observed values",stopifnot(market_bid_minimum(list(value=100,price=150))==150,market_bid_minimum(list(value=200,price=150))==200,is.na(market_bid_minimum(list(value=NA_real_,price=NA_real_)))))
tx<-data.frame(id=c("buy1","sell1","buy2"),player_id="p",buyer_team_id=c("mine","","mine"),seller_team_id=c("","mine",""),price=c(100,150,200),created=c("2026-10-01T00:00:00Z","2026-10-03T00:00:00Z","2026-10-04T00:00:00Z"))
check("acquisition follows ownership cycle",stopifnot(current_player_acquisition_cost(list(id="p"),tx,"mine")==200,current_player_acquisition_cost(list(id="p",buyPrice=250),tx,"mine")==250,is.na(current_player_acquisition_cost(list(id="unknown"),tx,"mine"))))
check("realized profit preserves system counterparties",{ledger<-realized_trade_ledger(rbind(tx,tx[1,]),"mine");stopifnot(nrow(ledger)==1L,ledger$proceeds==150,ledger$cost_basis==100,ledger$realized_profit==50,ledger$counterparty=="Futmondo (System)")})
check("observed holding duration is retained",{summary<-summarize_manager_transactions("mine",tx[1:2,]);stopifnot(summary$total_trades==2L,summary$avg_holding_days==2,is.na(summary$avg_overpayment_pct),summary$premium_observations==0L)})
check("position identity respects verified multiposition",{player<-data.frame(role="defensa",role2="delantero");stopifnot(identical(analytics_player_positions(player,FALSE)[[1]],"DEF"),identical(analytics_player_positions(player,TRUE)[[1]],c("DEF","FWD")))})
check("clause eligibility requires current facts",{player<-data.frame(clause_price=100,clause_transferred=FALSE,clause_date="2026-01-01T00:00:00Z");stopifnot(today_is_clause_open(player,now=as.POSIXct("2026-10-01",tz="UTC")));player$clause_transferred<-NA;stopifnot(!today_is_clause_open(player,now=as.POSIXct("2026-10-01",tz="UTC")))})
check("clause comparison is factual arithmetic",stopifnot(clause_value_comparison(75,100)$percent==25,clause_value_comparison(150,100)$percent==50,!clause_value_comparison(100,NA_real_)$available))
cat("Factual helpers:",n,"checks passed.\n")
