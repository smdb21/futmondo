#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(shiny);library(plotly);source('intelligence_engine.R');source('prediction_engine.R');source('Modules/Intelligence_Module.R')})
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('[PASS]',name,'\n')}
now<-as.POSIXct('2026-08-30 12:00:00',tz='UTC')
auctions<-data.frame(auction_id=paste0('a',1:12),championship_id='league',player_id='market',settled_at=now-86400*(12:1),
 observed_at=now-86400*(12:1)+60,visibility='complete',reference_value=1e6,reference_observed_at=now-86400*(12:1)-60)
auctions$eligible_manager_ids<-I(rep(list(c('self','rival')),12))
bids<-data.frame(auction_id=auctions$auction_id,championship_id='league',bidder_id=rep(c('self','rival'),6),amount=seq(1e6,1.11e6,length.out=12),is_winner=TRUE,settled_at=auctions$settled_at,observed_at=auctions$observed_at)
prices<-data.frame(player_id='market',value=1e6*exp(.025*(0:20)),observed_at=Sys.time()-86400*(20:0))
roster<-data.frame(id=paste0('p',1:12),name=paste0('P',1:12),role=c('Goalkeeper','Goalkeeper',rep('Defender',4),rep('Midfielder',3),rep('Forward',3)),status='ok',value=1e6,average.average=5,average.averageLastFive=5,average.matches=5,points=25,change=0)
market<-roster[12,,drop=FALSE];market$id<-'market';market$name<-'Market player';market$value<-tail(prices$value,1);market$price<-1e6
finance<-list(status='ok',cash=2e6,spendable_budget=2e6,legal_bid_limit=2e6,roster_cap=25,lineup_rules=list(formations='4-3-3'))
persisted<-list()
loaders<-list(persist_forecast=function(...) {persisted[[length(persisted)+1L]]<<-list(...);TRUE},get_market_players=function(...)market,get_players_from_team=function(...)roster,
 get_financial_snapshot=function(...)finance,read_player_price_history=function(...)prices,
 read_model_auctions=function(...)data.frame(),read_auction_observations=function(...)auctions,read_bid_observations=function(...)bids,
 get_teams=function(...)data.frame(id=c('self','rival'),teamname=c('My team','Rival team')),
 read_player_match_history=function(...)data.frame(),get_championship_pressroom=function(...)data.frame())
check('auction adapter never fills missing visibility or eligibility',{
 x<-intelligence_prepare_auctions(data.frame(id='a',created='2026-08-01T00:00:00Z',player_id='market'),data.frame(),prices,'league')
 stopifnot(x$auctions$visibility=='unknown',length(x$auctions$eligible_manager_ids[[1]])==0,is.na(x$auctions$observed_at))
})
check('auction adapter chooses only earlier historical values',{
 h<-data.frame(player_id='p',value=c(100,999),observed_at=c('2026-08-01T10:00:00Z','2026-08-03T10:00:00Z'))
 a<-data.frame(id='a',created='2026-08-02T10:00:00Z',player_id='p')
 x<-intelligence_prepare_auctions(a,data.frame(),h)
 stopifnot(x$auctions$reference_value==100)
})
check('historical references exclude prices captured after auction settlement',{
 h<-data.frame(player_id='p',value=c(100,999),observed_at=c('2026-08-02T12:00:00Z','2026-08-02T20:00:00Z'),
   captured_at=c('2026-08-02T13:00:00Z','2026-08-04T10:00:00Z'))
 a<-data.frame(id='a',created='2026-08-03T10:00:00Z',player_id='p')
 stopifnot(intelligence_prepare_auctions(a,data.frame(),h)$auctions$reference_value==100)
})
check('stale price references cannot normalize later auction outcomes',{
 h<-data.frame(player_id='p',value=100,observed_at='2026-08-01T10:00:00Z')
 a<-data.frame(id='a',created='2026-08-03T10:00:00Z',player_id='p')
 stopifnot(is.na(intelligence_prepare_auctions(a,data.frame(),h)$auctions$reference_value))
})
check('dashboard UI contains usable visible controls',{
 html<-as.character(intelligence_UI('intel'))
 stopifnot(grepl('intel-player_id',html,fixed=TRUE),grepl('Rival bids',html,fixed=TRUE),grepl('Profit planner',html,fixed=TRUE),grepl('Best XI',html,fixed=TRUE))
})
check('live module flow uses injectable read-only data and renders models',{
 testServer(intelligence_Server,args=list(is_module_active=reactive(TRUE),login_token=reactive(c(token='offline',userid='account-self')),championship_id=reactive('league'),user_team_id=reactive('self'),loaders=loaders),{
  session$setInputs(player_id='market',resale_percent=95,fees=0,horizon=7)
  d<-loaded();stopifnot(nrow(d$model$managers)==2,nrow(selected())==1,nrow(curve())>0,
    all(is.finite(curve()$p_win)),lineup()$feasible,nrow(resale())==3)
  stopifnot(nchar(output$rivals)>0,nchar(output$resale)>0,nchar(output$lineup)>0,length(output$coverage)>0,length(output$profit_summary)>0,length(output$lineup_summary)>0)
  p<-profit();stopifnot(p$recommended_bid<=2e6,nchar(output$bench)>0,nchar(output$points_horizons)>0)
  stopifnot(length(persisted)>0,all(vapply(persisted,function(x)x$user_id=='account-self',logical(1))),
    all(vapply(persisted,function(x)x$championship_id=='league',logical(1))),
    all(c('fantasy_points','resale_value','rival_bid')%in%vapply(persisted,function(x)x$forecast_type,character(1))))
  point_records<-Filter(function(x)x$forecast_type=='fantasy_points',persisted)
  stopifnot(setequal(vapply(point_records,function(x)x$horizon,numeric(1)),c(1,3)))
 })
})
check('unverified finances suppress profit recommendations',{
 unknown<-loaders;unknown$get_financial_snapshot<-function(...)list(status='unavailable')
 testServer(intelligence_Server,args=list(is_module_active=reactive(TRUE),login_token=reactive('offline'),championship_id=reactive('league'),user_team_id=reactive('self'),loaders=unknown),{
  session$setInputs(player_id='market',resale_percent=95,fees=0,horizon=7)
  stopifnot(profit()$action=='no_bid',profit()$recommended_bid==0)
 })
})
check('empty storage and market render useful empty states',{
 empty<-loaders;empty$get_market_players<-function(...)data.frame();empty$read_auction_observations<-function(...)data.frame();empty$read_bid_observations<-function(...)data.frame()
 testServer(intelligence_Server,args=list(is_module_active=reactive(TRUE),login_token=reactive('offline'),championship_id=reactive('league'),user_team_id=reactive('self'),loaders=empty),{
  session$setInputs(resale_percent=95,fees=0,horizon=7)
  stopifnot(nrow(selected())==0,profit()$action=='no_bid',nchar(output$rivals)>0)
 })
})
check('forecast persistence failures preserve usable analysis',{
 broken<-loaders;broken$persist_forecast<-function(...)stop('storage offline')
 testServer(intelligence_Server,args=list(is_module_active=reactive(TRUE),login_token=reactive(c(token='offline',userid='account')),
   championship_id=reactive('league'),user_team_id=reactive('self'),loaders=broken),{
   session$setInputs(player_id='market',resale_percent=95,fees=0,horizon=7)
   stopifnot(lineup()$feasible,nrow(resale())==3,nchar(output$rivals)>0)
 })
})
check('unknown account identity never persists under a team identifier',{
 count<-length(persisted)
 testServer(intelligence_Server,args=list(is_module_active=reactive(TRUE),login_token=reactive(c(token='offline')),
   championship_id=reactive('league'),user_team_id=reactive('self'),loaders=loaders),{
   session$setInputs(player_id='market',resale_percent=95,fees=0,horizon=7)
   stopifnot(lineup()$feasible)
 })
 stopifnot(length(persisted)==count)
})
cat('INTELLIGENCE MODULE:',checks,'passed / 0 failed\n')
