#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(shiny);library(plotly);source('intelligence_engine.R');source('prediction_engine.R');source('Modules/Intelligence_Module.R')})
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('[PASS]',name,'\n')}
now<-Sys.time()
ctx_a<-list(championship_id='leagueA',season='2026',scoring_version='mediaA')
ctx_b<-list(championship_id='leagueB',season='2026',scoring_version='mediaB')
roster<-data.frame(id=paste0('p',1:14),name=paste0('Player ',1:14),
 role=c('GK','GK',rep('DF',5),rep('MF',4),rep('FW',3)),status='ok',value=1e6,
 average.average=5,average.averageLastFive=5,average.matches=5,points=25,observed_at=now)
a_history<-data.frame(player_id=rep(roster$id,each=3),round_id=rep(c('r1','r2','r3'),14),
 points=10,role=rep(roster$role,each=3),observed_at=now-rep(c(21,14,7),14)*86400,
 championship_id='leagueA',season='2026',scoring_version='mediaA',is_final=TRUE)
b_history<-a_history;b_history$championship_id<-'leagueB';b_history$scoring_version<-'mediaB';b_history$points<-2
histories<-rbind(a_history,b_history)
check('same player and round identifiers retain distinct league scoring',{
 a<-forecast_fantasy_points(roster,histories,now,horizons=1,context=ctx_a)
 b<-forecast_fantasy_points(roster,histories,now,horizons=1,context=ctx_b)
 stopifnot(all(a$expected_points==10),all(b$expected_points==2),all(a$championship_id=='leagueA'),all(b$scoring_version=='mediaB'))
 stopifnot(inherits(try(forecast_fantasy_points(roster,histories,now),silent=TRUE),'try-error'))
})
check('the same league never pools different scoring versions or seasons',{
 changed<-a_history;changed$scoring_version<-'other';changed$points<-999
 previous<-a_history;previous$season<-'2025';previous$points<-555
 f<-forecast_fantasy_points(roster,rbind(a_history,changed,previous),now,horizons=1,context=ctx_a)
 stopifnot(all(f$expected_points==10),all(f$n_observations==3))
})
auctions<-data.frame(auction_id=c('a1','a2'),championship_id='leagueA',player_id='m-first',
 settled_at=now-c(3,2)*86400,observed_at=now-c(3,2)*86400+60,
 visibility='complete',reference_value=1e6,reference_observed_at=now-c(3,2)*86400-60)
auctions$eligible_manager_ids<-I(rep(list(c('self','rival')),2))
bids<-data.frame(auction_id=c('a1','a2'),championship_id='leagueA',bidder_id='rival',amount=c(1.4e6,1.6e6),is_winner=TRUE,
 settled_at=auctions$settled_at,observed_at=auctions$observed_at)
auctions_b<-auctions;auctions_b$championship_id<-'leagueB'
bids_b<-bids;bids_b$championship_id<-'leagueB';bids_b$amount<-c(.85e6,.95e6)
all_auctions<-rbind(auctions,auctions_b);all_bids<-rbind(bids,bids_b)
check('rival amount effects remain league-specific even with shared manager and auction IDs',{
 a<-fit_rival_bid_model(all_auctions,all_bids,as_of=now,context=ctx_a)
 b<-fit_rival_bid_model(all_auctions,all_bids,as_of=now,context=ctx_b)
 stopifnot(a$metadata$auctions==2,b$metadata$bids==2,
  a$managers$conditional_bid_ratio_median[a$managers$manager_id=='rival']>1.4,
  b$managers$conditional_bid_ratio_median[b$managers$manager_id=='rival']<1,
  inherits(try(fit_rival_bid_model(all_auctions,all_bids,as_of=now),silent=TRUE),'try-error'))
})
prices_a<-data.frame(player_id='m-first',value=1e6*exp(.01*(0:6)),observed_at=now-(6:0)*86400,championship_id='leagueA')
prices_b<-prices_a;prices_b$championship_id<-'leagueB';prices_b$value<-2e6*exp(-.01*(0:6))
all_prices<-rbind(prices_a,prices_b)
check('resale histories and current values cannot cross leagues',{
 p<-data.frame(id='m-first')
 a<-forecast_resale_values(p,all_prices,now,horizons=1,context=ctx_a)
 b<-forecast_resale_values(p,all_prices,now,horizons=1,context=ctx_b)
 stopifnot(a$expected_value<1.2e6,b$expected_value>1.7e6,a$n_observations==6,b$n_observations==6)
})
check('auction reference joins retain league identity when player IDs overlap',{
 a<-data.frame(auction_id='event',championship_id='leagueA',player_id='shared',settled_at=now,observed_at=now)
 h<-data.frame(player_id='shared',championship_id=c('leagueA','leagueB'),value=c(100,999),observed_at=now-c(120,60))
 x<-intelligence_prepare_auctions(a,data.frame(),h)
 stopifnot(x$auctions$reference_value==100)
})
check('chronological evaluators keep all held-out observations in the selected league',{
 a<-rolling_bid_backtest(all_auctions,all_bids,min_train_dates=1,as_of=now,context=ctx_a)
 stopifnot(nrow(a$folds)==1,a$folds$train_auctions==1,a$folds$test_auctions==1)
 h<-histories;h$round_start_at<-h$observed_at-86400
 a<-rolling_point_backtest(roster,h,min_train_rounds=1,as_of=now,context=ctx_a)
 b<-rolling_point_backtest(roster,h,min_train_rounds=1,as_of=now,context=ctx_b)
 stopifnot(a$status=='evaluated',b$status=='evaluated',all(a$predictions$observed==10),all(b$predictions$observed==2))
 p<-data.frame(id='m-first')
 a<-rolling_resale_backtest(p,all_prices,min_train_dates=2,as_of=now,horizons=1,tolerance_hours=24,context=ctx_a)
 b<-rolling_resale_backtest(p,all_prices,min_train_dates=2,as_of=now,horizons=1,tolerance_hours=24,context=ctx_b)
 stopifnot(a$status=='evaluated',b$status=='evaluated',all(a$predictions$observed<1.3e6),all(b$predictions$observed>1.7e6))
})
check('transfer scenarios and joint baskets reject foreign league rows',{
 a<-roster;a$championship_id<-'leagueA'
 purchase<-a[14,,drop=FALSE];purchase$id<-'foreign';purchase$championship_id<-'leagueB';purchase$price<-100
 scenario<-simulate_transfer_scenario(a,2e6,buy_player_ids='foreign',market_df=purchase)
 basket<-evaluate_trade_basket(a,purchases=purchase,verified_cash=2e6)
 stopifnot(scenario$status=='invalid',!basket$valid,'cross_context_transactions'%in%basket$reasons)
})
check('optimizer ignores another league forecast with reused player IDs',{
 a<-roster;a$championship_id<-'leagueA'
 foreign<-data.frame(player_id=a$id,expected_points=999,horizon=1,championship_id='leagueB')
 result<-optimize_starting_xi(a,forecast_df=foreign)
 stopifnot(result$feasible,result$expected_points==55)
})
champ<-reactiveVal('leagueA');persisted<-list()
market<-roster[13:14,,drop=FALSE];market$id<-c('m-first','m-second');market$price<-1e6
loaders<-list(
 get_financial_snapshot=function(championship_id,...) {
  a<-championship_id=='leagueA'
  rules<-list(formations=if(a)'4-3-3' else '4-4-2',captain_enabled=a,captain_multiplier=2,multiposition=a,
   bench_enabled=TRUE,bench_size=4,money_per_point=if(a)500000 else 1000,season='2026',scoring_version=if(a)'mediaA' else 'mediaB')
  list(status='ok',cash=2e6,spendable_budget=2e6,legal_bid_limit=2e6,roster_cap=20,rules=rules,lineup_rules=rules)
 },
 get_market_players=function(...)market,get_players_from_team=function(...)roster,
 read_player_price_history=function(...)all_prices,read_model_auctions=function(...)all_auctions,
 read_bid_observations=function(...)all_bids,read_player_match_history=function(...)histories,
 get_teams=function(...)data.frame(id=c('self','rival'),teamname=c('Me','Rival')),
 persist_forecast=function(...){persisted[[length(persisted)+1L]]<<-list(...);TRUE})
check('league switches reset overlapping selections and scenario controls while changing league rules',{
 testServer(intelligence_Server,args=list(is_module_active=reactive(TRUE),login_token=reactive(c(token='offline',userid='account')),
  championship_id=champ,user_team_id=reactive('self'),loaders=loaders),{
  session$flushReact()
  session$setInputs(player_id='m-second',resale_percent=90,fees=12345,horizon=3)
  stopifnot(selected()$id=='m-second',scenario_settings()$fees==12345,
    lineup()$formation=='4-3-3',abs(lineup()$expected_points-120)<1e-8,lineup()$captain_id%in%roster$id,
    loaded()$finance$rules$money_per_point==500000)
  champ('leagueB');session$flushReact()
  stopifnot(selected()$id=='m-first',scenario_settings()$fees==0,scenario_settings()$resale_percent==95,
    scenario_settings()$horizon==7,lineup()$formation=='4-4-2',abs(lineup()$expected_points-22)<1e-8,
    is.na(lineup()$captain_id),loaded()$finance$rules$money_per_point==1000,
    loaded()$model$metadata$auctions==2,all(loaded()$matches$championship_id=='leagueB'))
 })
})
check('persisted forecasts include the matching league season scoring and bonus settings',{
 for(league in c('leagueA','leagueB')) {
  records<-Filter(function(x)x$championship_id==league,persisted)
  stopifnot(length(records)>0)
  version<-if(league=='leagueA')'mediaA' else 'mediaB';reward<-if(league=='leagueA')500000 else 1000
  stopifnot(all(vapply(records,function(x)x$user_id=='account'&&x$season=='2026'&&x$scoring_version==version,logical(1))),
    all(vapply(records,function(x)x$prediction$league_rules$money_per_point==reward,logical(1))))
 }
})
cat('MULTI-LEAGUE ANALYTICS:',checks,'passed / 0 failed\n')
