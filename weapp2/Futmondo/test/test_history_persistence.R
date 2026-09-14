#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R');source('intelligence_engine.R');source('Modules/Notifications_Module.R')})
n<-0L
check<-function(name,code){force(code);n<<-n+1L;cat('PASS',name,'\n')}
check('collection logs avoid conflicting number separators and player-name debug output',{
  server_source<-paste(readLines('server.R',warn=FALSE),collapse='\n')
  roster_source<-paste(readLines('futmondo_functions.R',warn=FALSE),collapse='\n')
  connector_source<-paste(readLines('supabase_connector.R',warn=FALSE),collapse='\n')
  stopifnot(grepl('decimal.mark = ","',server_source,fixed=TRUE),
    !grepl('print(player$name)',roster_source,fixed=TRUE),
    grepl('Insight alerts skipped for this collection',connector_source,fixed=TRUE))
})
check('UTC, SQL and millisecond timestamps agree',{
  t<-fm_time(c('2026-09-01T12:00:00Z','2026-09-01 12:00:00+00:00','2026-09-01T14:00:00+02:00'))
  stopifnot(length(unique(as.numeric(t)))==1L,as.numeric(fm_time(as.numeric(t[1])*1000))==as.numeric(t[1]))
})
check('history filters future, provisional and changed scoring then deduplicates rounds',{
  old<-supabase_read_all
  supabase_read_all<-function(...)data.frame(player_id='p',round=c(1,1,1,2,2,3),points=c(1,3,9,0,100,50),
    score_status=c('final','final','final','final','provisional','final'),season='2026',
    scoring_version=c('old','new','new','new','new','new'),observed_at=c('2026-08-01T00:00:00Z','2026-08-02T00:00:00Z','2026-08-03T00:00:00Z','2026-08-04T00:00:00Z','2026-08-05T00:00:00Z','2026-09-01T00:00:00Z'))
  d<-read_player_match_history('c',as.POSIXct('2026-08-10',tz='UTC'))
  stopifnot(nrow(d)==2L,identical(d$points,c(9,0)),all(d$is_final),all(d$scoring_version=='new'))
  supabase_read_all<-old
})
check('forecast IDs are stable, scoped and sensitive to cutoff',{
  old<-supabase_post;records<-list()
  supabase_post<-function(table_name,payload,...){records[[length(records)+1L]]<<-payload;TRUE}
  args<-list(user_id='u',championship_id='c',player_id='p',model_version='v1',forecast_type='points',
    cutoff=as.POSIXct('2026-09-01',tz='UTC'),horizon=1L,prediction=list(mean=0,lower=-2,upper=4))
  do.call(persist_forecast,args);do.call(persist_forecast,args);args$user_id<-'v';do.call(persist_forecast,args)
  stopifnot(records[[1]]$id==records[[2]]$id,records[[1]]$id!=records[[3]]$id,records[[1]]$prediction$mean==0)
  supabase_post<-old
})
check('outcome writes constrain account league and unevaluated row',{
  old<-supabase_patch;filters<-NULL
  supabase_patch<-function(table_name,payload,filters){assign('captured_filters',filters,envir=.GlobalEnv);TRUE}
  stopifnot(record_forecast_outcome('u','c','f',list(points=0)),captured_filters$user_id=='eq.u',
    captured_filters$championship_id=='eq.c',captured_filters$evaluated_at=='is.null')
  supabase_patch<-old
})
check('background status cannot disclose another account failures',{
  .persistence_state$errors<-list(a='failure-a',b='failure-b')
  .persistence_state$queue<-list(list(owner='a'),list(owner='b'))
  stopifnot(background_sync_status('a')$error=='failure-a',background_sync_status('a')$pending==1,
    is.null(background_sync_status('c')$error),background_sync_status('c')$pending==0)
  .persistence_state$queue<-list();.persistence_state$errors<-list()
})
check('league rules preserve activated roles, optional bench and configured weights',{
  rules<-normalize_league_rules(list(configuration=list(cpt=TRUE)),
    list(multiposition=TRUE,bench=list(config=list(enabled=TRUE,mode='automatic')),
      custom=list(media=list(list(pct=80),list(pct=70)))))
  stopifnot(rules$multiposition,rules$captain_multiplier==2,rules$bench_size==4,
    rules$scoring$media[[1]]$pct==80,!rules$verified,!rules$solvency_verified)
})
check('zero balance and owned auction deadlines produce scoped actionable alerts',{
  now<-as.POSIXct('2026-09-01',tz='UTC')
  market<-data.frame(id=c('p','q'),name=c('Own bid','No bid'),bid_id=c('bid',NA),
    expirationDate=format(now+300,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))
  d<-build_insight_alerts('u','c','t',list(status='ok',cash=0),market=market,now=now)
  stopifnot(all(d$user_id=='u'),sum(d$alert_type=='auction_deadline')==1L,any(d$alert_type=='solvency'))
})
check('unknown commitments and stale snapshots cannot authorize spending',{
  old_info<-get_user_team_info;old_cap<-get_acquisition_capacity;old_lineup<-get_lineup_from_team
  get_user_team_info<-function(...)list(budget=1000,withheld=0,maxBid=2000,configuration=list())
  get_lineup_from_team<-function(...)list(multiposition=FALSE)
  get_acquisition_capacity<-function(...)list(status='partial',roster=list(count=15,cap=20),
    outstanding=list(count=NA,total_amount=NA,completeness='partial'))
  clear_api_cache();fin<-get_financial_snapshot(c(token='t',userid='u'),'c','t')
  stopifnot(is.na(fin$spendable_budget),fin$status=='partial')
  fetch<-function(login,fail=FALSE,ttl=300)get_cached_data('stale-finance',{
    if(fail)stop('failure');list(status='ok',spendable_budget=1000)},ttl)
  fetch(c(token='t',userid='u'));stale<-fetch(c(token='t',userid='u'),TRUE,-1)
  stopifnot(stale$status=='partial',is.na(stale$spendable_budget),
    !evaluate_acquisition_preflight(stale,'buy',100)$ok)
  get_user_team_info<-old_info;get_acquisition_capacity<-old_cap;get_lineup_from_team<-old_lineup
})
check('league-specific seasons and ratings never overwrite shared player identity',{
  stopifnot(league_season_context('a',configured='{"a":"2026-27","b":"2026"}')=='2026-27',
    league_season_context('b',configured='{"a":"2026-27","b":"2026"}')=='2026',
    league_season_context('c',configured='{"a":"2026-27"}')=='unknown')
  old<-supabase_post;rows<-list()
  supabase_post<-function(table_name,payload,...) {rows[[table_name]]<<-payload;TRUE}
  players<-data.frame(id='p',name='Player',slug='player',role='defensa',role2='delantero',
    primary_role='defensa',status='injured',rating=2,value=1000)
  attr(players,'observed_at')<-Sys.time()
  sync_players_to_supabase(players);log_player_daily_snapshots(players,'a')
  stopifnot(!any(c('role','role2','status','rating')%in%names(rows$players)),
    rows$player_daily_snapshots$championship_id=='a',rows$player_daily_snapshots$role=='defensa')
  supabase_post<-old
})
check('manual acquisition rejects an API ceiling even with cash available',{
  cap<-list(status='ok',roster=list(count=12,cap=20),outstanding=list(count=0),
    funds=list(spendable_budget=1000,api_bid_limit=100))
  stopifnot(!evaluate_acquisition_preflight(cap,'bid',200)$ok,
    evaluate_acquisition_preflight(cap,'bid',50)$ok)
  cap$funds$api_bid_limit<-NA_real_
  stopifnot(!evaluate_acquisition_preflight(cap,'bid',50)$ok)
})
cat(sprintf('HISTORY/PERSISTENCE: %d passed / 0 failed\n',n))
