#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');
  source('intelligence_engine.R');source('prediction_engine.R');source('insights_runtime.R');
  source('portfolio_engine.R');source('automation_runtime.R');source('Modules/Notifications_Module.R')})
checks<-0L
check<-function(name,code){force(code);checks<<-checks+1L;cat('[PASS]',name,'\n')}
now<-as.POSIXct('2026-09-07 12:00:00',tz='UTC')
roster<-data.frame(id=paste0('p',1:11),name=paste('Player',1:11),role=c('GK',rep('DEF',4),rep('MID',3),rep('FWD',3)),
  status='ok',value=100,average.average=5,average.matches=10,points=50,observed_at=now)
check('actual rules adapter permits provisional XI without authorizing actions',{
  rules<-normalize_league_rules(list(configuration=list(maxPlayersInRoster=20)),list(multiposition=FALSE),'league')
  xi<-optimize_starting_xi(roster,rules=rules)
  stopifnot(xi$feasible,nrow(xi$starting_xi)==11,!xi$legality_verified,xi$legality_status=='provisional',length(xi$assumptions)>0)
  rules$club_limit_status<-'limited';rules$club_limit<-NA_real_
  stopifnot(!optimize_starting_xi(roster,rules=rules)$feasible)
  rules$club_limit_status<-'unrestricted';rules$verified<-TRUE
  stopifnot(optimize_starting_xi(roster,rules=rules)$legality_verified)
})
check('missing observation times remain unknown and stale data keeps original time',{
  d<-data.frame(id='p',value=100)
  stopifnot(is.na(preserve_observation_time(d)$observed_at))
  attr(d,'observed_at')<-now-86400;attr(d,'fetch_status')<-'stale'
  out<-preserve_observation_time(d)
  stopifnot(fm_time(out$observed_at)==now-86400,attr(out,'fetch_status')=='stale')
})
check('price persistence uses source time and skips stale repeats',{
  saved<-supabase_post;writes<-list();supabase_post<-function(table_name,payload,...) {writes[[length(writes)+1L]]<<-payload;204L}
  d<-roster;d$observed_at<-format(now-86400,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')
  log_player_history(d,'league')
  stopifnot(length(writes)==1L,all(fm_time(writes[[1]]$observed_at)==now-86400))
  attr(d,'fetch_status')<-'stale';log_player_history(d,'league');stopifnot(length(writes)==1L)
  supabase_post<-saved
})
check('past auction replay selects the revision known at its cutoff',{
  saved<-supabase_read_all
  supabase_read_all<-function(...)data.frame(championship_id='c',auction_id='a',amount=c(100,200),
    observed_at=format(now+c(-86400,86400),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))
  d<-read_observation_revisions('fixture','c',now,c('championship_id','auction_id'))
  stopifnot(nrow(d)==1L,d$amount==100)
  supabase_read_all<-saved
})
check('old and new rounds fetched together still receive different form weights',{
  summary<-list(points=list(list(round=1,points=0,finished=TRUE),list(round=2,points=10,finished=TRUE)))
  h<-normalize_player_match_observations(summary,'p1','c','s','2026',now)
  f<-forecast_fantasy_points(roster[1,,drop=FALSE],h,now,1)
  stopifnot(f$expected_points>5,all(is.na(h$occurred_at)),f$method=='shrunk_round_order')
  h$points<-rev(h$points);g<-forecast_fantasy_points(roster[1,,drop=FALSE],h,now,1)
  stopifnot(g$expected_points<5)
})
check('fair collection rotates beyond the first thirty players',{
  ids<-sprintf('p%03d',1:95);a<-collection_player_batch(ids);b<-collection_player_batch(ids,tail(a,1))
  c<-collection_player_batch(ids,tail(b,1));d<-collection_player_batch(ids,tail(c,1))
  stopifnot(length(intersect(a,b))==0L,length(unique(c(a,b,c,d)))==95)
})
check('sale execution learns distinct system offers and excludes future observations',{
  d<-data.frame(offer_id=paste0('o',1:5),amount=95:99,market_value=100,observed_at=now-1,counterparty='Futmondo (System)')
  e<-fit_sale_execution(rbind(d,d),now);stopifnot(e$n==5,e$mean==.97)
  d$observed_at[5]<-now+1;stopifnot(!is.finite(fit_sale_execution(d,now)$mean))
})
check('profitable points-reducing transfers are retained',{
  r<-roster;r$expected_sale_proceeds<-100;r$executable_sale_price<-100
  m<-r[2,,drop=FALSE];m$id<-'new';m$average.average<-1;m$points<-10;m$price<-50;m$expected_sale_proceeds<-200
  rec<-recommend_transfers(r,m,current_budget=100)
  stopifnot(nrow(rec)>0,all(rec$expected_profit>0),any(rec$delta_expected_points<0))
})
check('joint planner reserves all bids and selects the more profitable affordable purchase',{
  m<-roster[2:3,,drop=FALSE];m$id<-c('a','b')
  candidates<-data.frame(player_id=c('a','b'),price=60,expected_profit=c(20,30),downside_profit=-10)
  p<-plan_profit_portfolio(roster,m,candidates,list(spendable_budget=100,roster_cap=20))
  stopifnot(nrow(p$purchases)==1L,p$purchases$player_id=='b',p$cash_remaining==40,p$lineup$feasible)
  q<-plan_profit_portfolio(roster,m,candidates,list(spendable_budget=120,roster_cap=20))
  stopifnot(nrow(q$purchases)==2L,q$cash_remaining==0,q$expected_profit==50)
})
check('sale prerequisites cannot remove the only goalkeeper',{
  sales<-data.frame(player_id='p1',proceeds=200,hold_proceeds=100)
  p<-plan_profit_portfolio(roster,roster[FALSE,],data.frame(),list(spendable_budget=0,roster_cap=20),sales)
  stopifnot(nrow(p$sales)==0L,p$lineup$feasible)
})
check('realized profit pairs ownership cycles and preserves unknown acquisition cost',{
  d<-data.frame(id=1:5,player_id=c('p','p','p','p','q'),price=c(100,120,150,140,50),
    buyer_team_id=c('me','','me','',''),seller_team_id=c('','me','','me','me'),
    created=format(now+1:5,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))
  l<-realized_trade_ledger(rbind(d,d),'me')
  stopifnot(nrow(l)==3L,identical(l$realized_profit,c(20,-10,NA_real_)),all(l$counterparty=='Futmondo (System)'))
})
check('relisting and changed offers get distinct stable automation identities',{
  p<-list(id='policy',policy=list(amount=10))
  a<-automation_opportunity_key(p,'p','bid',list(listing_expires_at='first',amount=10))
  b<-automation_opportunity_key(p,'p','bid',list(listing_expires_at='second',amount=10))
  stopifnot(a!=b,a==automation_opportunity_key(p,'p','bid',list(listing_expires_at='first',amount=10)))
})
check('all-league alerts retain real league names and account scope',{
  old_active<-get_active_championships;old_alerts<-fetch_user_smart_alerts
  get_active_championships<-function(...)list(championships=list(list(id='a',name='Alpha',userteam=list(id='ta')),list(id='b',name='Beta',userteam=list(id='tb'))))
  fetch_user_smart_alerts<-function(user_team_id,championship_id,user_id) {
    stopifnot(user_id=='u');data.frame(id=championship_id,championship_id=championship_id,is_read=FALSE)
  }
  d<-fetch_account_insights_alerts(c(token='t',userid='u'))
  stopifnot(nrow(d)==2L,setequal(d$league_name,c('Alpha','Beta')))
  get_active_championships<-old_active;fetch_user_smart_alerts<-old_alerts
})
check('outcome matching requires the correct future horizon',{
  r<-list(cutoff=now-86400,horizon=1,player_id='p',forecast_type='resale_value')
  prices<-data.frame(player_id='p',value=c(90,110),observed_at=c(now-1,now+1))
  stopifnot(is.null(forecast_observed_outcome(r,prices,NULL,NULL,now)))
  out<-forecast_observed_outcome(r,prices,NULL,NULL,now+2);stopifnot(out$value==110)
})
check('terminal theme uses shared accessible foreground and widget backgrounds',{
  t<-fm_theme_tokens();stopifnot(t$bg=='#050805',t$text=='#7CFF9B',grepl('monospace',t$font))
  luminance<-function(hex){c<-grDevices::col2rgb(hex)/255;c<-ifelse(c<=.04045,c/12.92,((c+.055)/1.055)^2.4);sum(c*c(.2126,.7152,.0722))}
  stopifnot((luminance(t$text)+.05)/(luminance(t$surface)+.05)>4.5)
  p<-plotly::plotly_build(fm_plot_layout(plotly::plot_ly(x=1:2,y=1:2,type='scatter',mode='lines')))
  stopifnot(p$x$layout$paper_bgcolor==t$surface,p$x$layout$font$color==t$text)
  stopifnot(fm_reactable_theme()$style$fontFamily==t$font)
  stopifnot(!grepl('fonts.googleapis',paste(readLines('www/custom_style.css'),collapse='')))
})
check('rival outcomes match listing lifecycles without conflating relisted players',{
  r<-list(cutoff=now-86400,horizon=1,player_id='p',forecast_type='rival_bid',prediction=list(listing_id='second'))
  auctions<-data.frame(auction_id=c('a','b'),listing_id=c('first','second'),settled_at=now-1,winning_amount=c(10,20),winner_id='rival')
  stopifnot(forecast_observed_outcome(r,NULL,NULL,auctions,now)$winning_bid==20)
  r$prediction$listing_id<-'third';stopifnot(is.null(forecast_observed_outcome(r,NULL,NULL,auctions,now)))
})
check('delayed acquisitions cannot fill an immediate XI',{
  r<-roster;r$acquisition_effective_at<-NA_character_
  r$acquisition_effective_at[1]<-format(now+86400,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')
  stopifnot(!optimize_starting_xi(r,rules=list(deadline=now))$feasible)
})
source('Modules/Players_in_Teams_Module.R')
check('same membership still reports formation captain bench and position changes',{
  submitted<-list(players=data.frame(id=c('a','b'),assigned_position=c('DEF','MID')),
    formation='4-3-3',captain_id='a',bench=list(players=data.frame(id=c('x','y'))))
  optimized<-list(feasible=TRUE,starting_xi=data.frame(id=c('a','b'),pos_group=c('MID','DEF')),
    formation='3-4-3',captain_id='b',bench_order=c('y','x'))
  result<-squad_lineup_comparison(submitted,optimized)
  stopifnot(!length(result$add),!length(result$remove),length(result$changes)==5L)
})
check('settlement ingestion requires evidence, league identity and acknowledged writes',{
  a<-list(auction_id='a',player_id='p',observed_at=now,outcome='unsold')
  e<-list(settlement_verified=TRUE,source='verified fixture')
  stopifnot(!record_verified_auction_outcome('c','l',a,data.frame(),list()))
  missing_time<-a;missing_time$observed_at<-NULL
  stopifnot(!record_verified_auction_outcome('c','l',missing_time,data.frame(),e))
  b<-data.frame(championship_id='foreign',auction_id='a',player_id='p',bidder_id='r')
  stopifnot(!record_verified_auction_outcome('c','l',a,b,e))
  old<-supabase_post;supabase_post<-function(...)500L
  stopifnot(!record_verified_auction_outcome('c','l',a,data.frame(),e))
  supabase_post<-function(...)204L
  stopifnot(record_verified_auction_outcome('c','l',a,data.frame(),e));supabase_post<-old
})
check('saved evaluations keep horizons separate and refreshes do not inflate samples',{
  records<-data.frame(id=c('a','b','c'),model_version='v',player_id='p',horizon=c(1,1,7),forecast_type='resale_value',
    cutoff=c('2026-09-01T01:00:00Z','2026-09-01T02:00:00Z','2026-09-01T01:00:00Z'))
  records$prediction<-list(list(expected_value=100,baseline=100),list(expected_value=101,baseline=100),list(expected_value=200,baseline=100))
  records$outcome<-list(list(value=110),list(value=110),list(value=220))
  old_read<-supabase_read_all;old_post<-supabase_post;written<-NULL
  supabase_read_all<-function(...)records
  supabase_post<-function(table_name,payload,...) {written<<-payload;204L}
  evaluate_saved_forecasts('u','c',now)
  stopifnot(written$metrics$observations==2,written$metrics$horizon_1_mae==10,written$metrics$horizon_7_mae==20,!written$approved)
  supabase_read_all<-old_read;supabase_post<-old_post
})
check('unrelated successful background jobs do not clear a failed job',{
  source('background_runtime.R')
  .persistence_state$batch<-list(list(owner='u',key='failed'),list(owner='u',key='success'))
  .persistence_state$process<-list(is_alive=function()FALSE,get_result=function()list(FALSE,TRUE))
  persistence_tick()
  stopifnot(nzchar(background_sync_status('u')$error),is.null(background_sync_status('other')$error))
})
check('observation schedules run independently across leagues and retain collection failures',{
  names<-c('read_automation_rows','decrypt_automation_session','collect_account_observations','reconcile_forecast_outcomes',
    'evaluate_saved_forecasts','run_chronological_evaluations','supabase_patch')
  saved<-mget(names,envir=.GlobalEnv);updates<-list()
  read_automation_rows<-function(table,query) {
    if(table=='observation_subscriptions')return(list(list(user_id='u',championship_id='a',user_team_id='ta'),list(user_id='u',championship_id='b',user_team_id='tb')))
    stopifnot(table=='automation_sessions');list(list(encrypted_session='fixture'))
  }
  decrypt_automation_session<-function(...)c(token='fixture',userid='u')
  collect_account_observations<-function(login,championship_id,user_team_id)championship_id=='b'
  reconcile_forecast_outcomes<-evaluate_saved_forecasts<-run_chronological_evaluations<-function(...)TRUE
  supabase_patch<-function(table,update,filters){updates[[filters$championship_id]]<<-update;204L}
  stopifnot(schedule_observations(now)==2L,nzchar(updates[['eq.a']]$last_error),updates[['eq.b']]$last_error=='')
  list2env(saved,envir=.GlobalEnv)
})
cat('REVIEW COMPLETION:',checks,'passed / 0 failed\n')
