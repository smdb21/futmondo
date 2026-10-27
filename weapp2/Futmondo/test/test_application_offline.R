# Full application lifecycle with synthetic fixtures and blocked network I/O.
options(futmondo.offline = TRUE, warn = 1)
offline_http_attempts <- 0L
for (method in c('GET','POST','PUT','PATCH','DELETE')) {
  suppressMessages(trace(method, where=asNamespace('httr'), print=FALSE,
    tracer=quote({offline_http_attempts <<- offline_http_attempts + 1L; stop('Unexpected HTTP in offline lifecycle test')})))
}
app_checks <- 0L
app_check <- function(name, code) {
  force(code); app_checks <<- app_checks + 1L; cat('[PASS]', name, '\n')
}
app_check('actual application startup', {
  source('global.R',local=.GlobalEnv)
  source('ui.R',local=.GlobalEnv)
  app_server <- source('server.R',local=.GlobalEnv)$value
  stopifnot(is.function(app_server),offline_http_attempts==0L)
})
# Every ID/name here is synthetic. Fixtures include zero scores, a multiposition
# player, valid seventeen-player squad, published rounds and an unread event.
app_auth <- c(token='offline-token',userid='offline-user',user_name='offline@example.invalid')
app_teams <- data.frame(teamid=c('team1','team2'),id=c('team1','team2'),teamname=c('Your Team','Rival Team'),
  name=c('Your Team','Rival Team'),points=c(30,25),position=1:2,teamValue=c(34e6,32e6),is_active=TRUE)
app_roster <- data.frame(id=paste0('p',1:17),name=paste('Player',1:17),
  role=c(rep('portero',2),rep('defensa',5),rep('centrocampista',6),rep('delantero',4)),
  role2=NA_character_,team=rep(c('Club A','Club B','Club C'),length.out=17),teamId=rep(1:3,length.out=17),
  points=seq(0,80,length.out=17),value=2e6,change=10000,buyPrice=1.8e6,
  average.average=seq(2,6,length.out=17),average.matches=10,average.averageLastFive=seq(2,6,length.out=17),
  status='ok',photo='fixture',slug=paste0('player-',1:17),userteamId='team1',user_team_id='team1',userTeam='Your Team',
  isClause=FALSE,clause_price=4e6,clause_date=NA_character_,clause_transferred=FALSE,
  market_inMarket=FALSE,effective_market_price=NA_real_,price=NA_real_,bid_price=NA_real_,numberOfBids=0,
  computer=FALSE,fav=FALSE,observed_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))
app_roster$role2[7] <- 'centrocampista'
app_market <- app_roster[15:17,,drop=FALSE]
app_market$id <- paste0('m',1:3);app_market$name<-paste('Market Player',1:3)
app_market$userteamId<-NA_character_;app_market$user_team_id<-NA_character_;app_market$userTeam<-NA_character_
app_market$computer<-TRUE;app_market$market_inMarket<-TRUE;app_market$price<-2e6;app_market$effective_market_price<-2e6
app_cfg <- list(budget=30e6,moneyPerPoint=123,moneyPerRanking=456,rankingMode='flop',
                mvpPlayer=789,dreamTeamPlayer=321,maxPlayersInRoster=20)
app_financial <- list(status='ok',cash=0,withheld=0,spendable_budget=0,legal_bid_limit=10e6,
  roster_count=17,roster_cap=20,observed_at=Sys.time(),configuration=app_cfg,commitments=list(),
  lineup_rules=list(roster_cap=20))
app_champ <- list(id='champ',name='Offline League',userteam=list(id='team1',name='Your Team'))
app_rounds <- data.frame(round_id=c('r1','r2'),round_number=1:2,begin_process=c('2026-08-15T00:00:00Z','2026-08-22T00:00:00Z'),is_finished=TRUE)
login <- function(...) app_auth
get_championships <- function(...) unlist(app_champ)
get_active_championships <- function(...) list(championships=list(app_champ))
get_teams <- function(...) app_teams
get_players_from_team <- function(login,championship_id,user_team_id,...) {
  d<-app_roster
  if(as.character(user_team_id)!='team1') {d$id<-paste0('r',1:17);d$userteamId<-'team2';d$user_team_id<-'team2';d$userTeam<-'Rival Team'}
  d
}
get_market_players <- function(...) app_market
get_roster_bids <- function(...) data.frame()
get_my_market_players <- function(...) app_roster[FALSE,,drop=FALSE]
get_championship_players <- function(...) dplyr::bind_rows(app_roster,app_market)
get_financial_snapshot <- function(...) app_financial
get_user_team_info <- function(...) list(budget=0,withheld=0,maxBid=10e6,teamValue=34e6,configuration=app_cfg)
get_lineup_from_team <- function(...) list(players=app_roster[c(1,3:6,8:10,14:16),,drop=FALSE],bench=list(players=app_roster[c(2,7,11:13,17),,drop=FALSE]),lineup_config=list(),budget=0)
get_finished_rounds <- function(...) app_rounds
get_user_team_rounds <- function(login,championship_id,user_team_id,...) {
  if(as.character(user_team_id)=='team1') list(list(round='r1',points=10),list(round='r2',points=20)) else list(list(round='r1',points=20),list(round='r2',points=5))
}
get_round_dreamteam <- function(...) list(mvp='p1',players=list(list(id='p1',name='Player 1',role=1,points=9)))
get_championship_pressroom <- function(...) data.frame(id=character(),created=character(),player_id=character(),player_name=character(),buyer_team_id=character(),seller_team_id=character(),price=numeric())
get_user_team_moneymovements <- function(...) data.frame()
get_league_standings_history <- function(...) data.frame()
get_player_historical_data <- function(...) data.frame()
get_player_summary <- function(...) list(my_bid_id=NULL,my_bid_price=NULL,points=list(),match=list())
read_player_price_history <- function(...) data.frame()
read_player_match_history <- function(...) data.frame()
read_auction_observations <- function(...) data.frame()
read_bid_observations <- function(...) data.frame()
supabase_get <- function(...) data.frame()
get_notification_unread <- function(...) 1L
get_notifications <- function(...) normalize_notifications(list(list(`_id`='notice1',created='2026-08-22T00:00:00Z',updated='2026-08-22T00:00:00Z',type='market',action='acceptBid',readed=FALSE,directObject=list(id='p1',name='Player 1'),subject=list(name='Futmondo'),context=list(id='champ',name='Offline League'))))
get_table_row_counts <- function(...) data.frame(table='players',rows=0)
calculate_league_finances <- function(...) list(team_finances=data.frame(teamid=app_teams$teamid,teamname=app_teams$teamname,
  initial_budget=30e6,total_spent=30e6,total_sales=0,budget=c(0,2e6),team_value=app_teams$teamValue,
  net_profit_loss=0,squad_size=17,points=app_teams$points,point_bonus=0,ranking_prize=0),all_purchases=data.frame())
# Infrastructure already respects offline mode; make all persistence boundaries
# deterministic too, so the fixture test never launches a background process.
defer_persistence <- function(...) invisible(TRUE)
for (name in c('sync_championship_to_supabase','sync_user_teams_to_supabase','log_user_team_history',
 'sync_real_clubs_to_supabase','sync_players_to_supabase','log_player_history','sync_pressroom_transactions_to_supabase',
 'log_player_daily_snapshots')) assign(name,function(...) invisible(TRUE),envir=.GlobalEnv)


get_player_card_history<-function(...)list(valuation=data.frame(),points=data.frame(round_number=c(1,2),points=c(0,-2),score_status="final"),status="ok",freshness="fresh",source="fixture")
get_player_full_profile<-function(...)NULL
get_player_matches<-function(...)NULL
app_settle<-function(session){for(i in seq_len(40L)){later::run_now(timeoutSecs=0);session$flushReact()};invisible(TRUE)}
app_check('full server login, navigation, refresh and logout',{
 shiny::testServer(app_server,{
  session$setInputs(tabs='login',`login-user_name`='offline@example.invalid',`login-password`='fixture',`login-login_button`=1)
  session$elapse(100);app_settle(session);stopifnot(valid_login(login_token_RV()))
  tabs<-list(today=c('today-kpi_cash_box','today-kpi_valuation_box','today-market_players-players_table','today-recent_transfers'),
   yourteam=c('players_in_teams-team_value_box','players_in_teams-players_table_in_teams-players_table'),
   market='market-market_players_table-players_table',players_in_championship='players_in_championship-championship_players_table-players_table',
   rivals=c('rivals-league_finances_table','rivals-league_finances_plot','rivals-team_valuation_history_plot'),
   plots=c('plots-standings_plot','plots-values_plot','plots-cash_plot','plots-rank_plot','plots-owned_club_plot'),
   classification=c('classification-classification_table','classification-rank_evolution_plot','classification-dreamteam_box_ui'),
   round_mvps='round_mvps-mvp_cards',notifications=c('notifications-status','notifications-items'))
  for(tab in names(tabs)){
   session$setInputs(tabs=tab);app_settle(session)
   for(id in tabs[[tab]])stopifnot(!is.null(output[[id]]))
   cat('[PASS] rendered tab:',tab,'\n')
  }
  session$setInputs(tabs='today',refresh_all=1);app_settle(session);stopifnot(!is.null(output[['today-kpi_cash_box']]))
  stopifnot(!grepl('Predictions|Automation',output$menu$html))
  session$setInputs(`login-logout_button`=1);session$elapse(100);app_settle(session);stopifnot(is.null(login_token_RV()))
 })
})
app_check('rendered login never embeds server credentials',{
 previous<-Sys.getenv(c('user_name','password'));Sys.setenv(user_name='SENTINEL-UI-USER',password='SENTINEL-UI-PASSWORD')
 stopifnot(!grepl('SENTINEL-UI-',as.character(login_UI('login')),fixed=TRUE));do.call(Sys.setenv,as.list(previous))
})
app_check('countdown ticks reuse fetched round catalog',{
 original_rounds<-get_finished_rounds;round_reads<-0L
 get_finished_rounds<-function(...){round_reads<<-round_reads+1L;app_rounds};clear_api_cache()
 shiny::testServer(app_server,{
  session$setInputs(tabs='login',`login-user_name`='offline@example.invalid',`login-password`='fixture',`login-login_button`=1)
  session$elapse(100);app_settle(session);clock<-output$round_countdown;before<-round_reads
  session$elapse(3100);app_settle(session);clock<-output$round_countdown;stopifnot(round_reads==before)
 });get_finished_rounds<-original_rounds
})
app_check('official notifications escape text and retain failed acknowledgements',{
 original_notifications<-get_notifications;original_unread<-get_notification_unread
 notification_read<-FALSE;allow_ack<-FALSE;fail_fetch<-FALSE;clear_api_cache()
 get_notifications<-function(...){
  if(fail_fetch)return(NULL)
  normalize_notifications(list(list(`_id`='notice1',created='2026-08-22T00:00:00Z',updated='2026-08-22T00:00:00Z',type='market',action='acceptBid',readed=notification_read,directObject=list(id='p1',name='<script>alert(1)</script>'),subject=list(name='Futmondo'),context=list(id='champ',name='Offline League'))))
 }
 get_notification_unread<-function(...)if(notification_read)0L else 1L
 mark_notification_read<-function(...){if(!allow_ack)return(FALSE);notification_read<<-TRUE;TRUE}
 shiny::testServer(notifications_Server,args=list(is_module_active=reactive(TRUE),login_token=reactive(app_auth),championship_id=reactive('champ'),user_team_id=reactive('team1'),user_teams_RV=reactive(app_teams),refresh_trigger=reactiveVal(0L)),{
  session$setInputs(read_filter='all',source_filter='Futmondo',event_filter='All',current_league=TRUE);app_settle(session);html<-output$items$html
  stopifnot(grepl('&lt;script&gt;',html,fixed=TRUE),!grepl('<script>alert',html,fixed=TRUE))
  session$setInputs(mark_read='notice1');app_settle(session);stopifnot(!notification_read,!official()$is_read[1])
  allow_ack<<-TRUE;session$setInputs(mark_read='');session$setInputs(mark_read='notice1');app_settle(session);stopifnot(notification_read,official()$is_read[1])
  fail_fetch<<-TRUE;session$setInputs(refresh=1);app_settle(session);stopifnot(stale(),nrow(official())==1L)
 });get_notifications<-original_notifications;get_notification_unread<-original_unread
})
app_check('manual listing confirmation cannot cross league boundaries',{
 champ<-reactiveVal('champ');selected<-reactiveVal(app_roster[1,,drop=FALSE]);mutations<-0L
 original_listing<-put_player_on_market;put_player_on_market<-function(...){mutations<<-mutations+1L;list(success=TRUE)}
 shiny::testServer(selected_player_Server,args=list(selected_player=selected,login_token=reactive(app_auth),championship_id=champ,user_team_id=reactive('team1')),{
  app_settle(session);session$setInputs(btn_put_on_market=1);stopifnot(length(modal_context_RV())==1L)
  champ('different-league');app_settle(session);session$setInputs(sale_price_input=2000000,submit_put_on_market=1)
  stopifnot(mutations==0L,length(modal_context_RV())==0L)
  champ('champ');app_settle(session);session$setInputs(btn_put_on_market=2);session$setInputs(submit_put_on_market=2);stopifnot(mutations==1L)
 });put_player_on_market<-original_listing
})
app_check('admin handlers reject forged operations and require confirmation',{
 resets<-0L;verification<-0L;supabase_reset_database<-function(...){resets<<-resets+1L;list(players='deleted')};init_supabase_db<-function(...){verification<<-verification+1L;TRUE}
 shiny::testServer(admin_Server,args=list(is_module_active=reactive(TRUE),login_token=reactive(app_auth),championship_id=reactive('champ'),user_team_id=reactive('team1'),user_teams_RV=reactive(app_teams),is_admin=reactive(FALSE)),{
  session$setInputs(btn_reset_db=1,btn_confirm_reset=1,btn_verify_db=1,btn_sync_dreamteams=1);stopifnot(resets==0L,verification==0L)
 })
 shiny::testServer(admin_Server,args=list(is_module_active=reactive(TRUE),login_token=reactive(app_auth),championship_id=reactive('champ'),user_team_id=reactive('team1'),user_teams_RV=reactive(app_teams),is_admin=reactive(TRUE)),{
  session$setInputs(btn_confirm_reset=1);stopifnot(resets==0L)
  session$setInputs(btn_reset_db=1);session$setInputs(btn_confirm_reset=2);stopifnot(resets==1L)
 })
})
app_check('removed feature interfaces are absent',{
 html<-paste(as.character(today_UI('today')),as.character(players_in_teams_UI('team')),as.character(selected_player_UI('card')))
 stopifnot(!grepl('Smart Bid|Lineup Optimizer|Transfer Sandbox|Automatically hire|recommendation_types',html),!file.exists('prediction_engine.R'),!file.exists('automation_runtime.R'))
})
app_check('one notification click completes cold async player loading',{
 original_catalog<-get_championship_players;get_championship_players<-function(...)app_roster
 clear_api_cache(app_auth[["userid"]]);league<-reactiveVal('champ')
 shiny::testServer(notifications_Server,args=list(is_module_active=reactive(TRUE),login_token=reactive(app_auth),championship_id=league,user_team_id=reactive('team1'),user_teams_RV=reactive(app_teams)),{
  session$setInputs(read_filter='all',source_filter='Futmondo',event_filter='All',current_league=FALSE);app_settle(session)
  session$setInputs(open_player='notice1');app_settle(session)
  stopifnot(selected()$id=='p1',detail_context()$championship_id=='champ',is.null(pending_player()))
  session$setInputs(open_player='');session$setInputs(open_player='notice1');league('other-league');app_settle(session)
  stopifnot(is.null(selected()),is.null(detail_context()),is.null(pending_player()))
 });get_championship_players<-original_catalog
})
app_check('unavailable history cannot suppress live roster rows',{
 originals<-mget(c('get_financial_snapshot','get_finished_rounds','read_player_match_history'),envir=.GlobalEnv);reads<-0L
 get_financial_snapshot<-function(...){x<-app_financial;x$lineup_rules<-list(season='2026',scoring_version='known');x}
 get_finished_rounds<-function(...)app_rounds;read_player_match_history<-function(...){reads<<-reads+1L;NULL};clear_api_cache(app_auth[["userid"]])
 shiny::testServer(players_table_Server,args=list(players_table_RV=reactive(app_roster),user_teams_RV=reactive(app_teams),login_token=reactive(app_auth),championship_id=reactive('champ'),user_team_id=reactive('team1')),{
  app_settle(session);stopifnot(reads>0L,setequal(players_table_filtered_RV()$id,app_roster$id))
 });for(name in names(originals))assign(name,originals[[name]],envir=.GlobalEnv)
})
app_check('same-context loading preserves selection but league changes clear it',{
 original_state<-getReactableState;getReactableState<-function(outputId,name,session)session$input$fixture_selection
 loading<-reactiveVal(FALSE);league<-reactiveVal('champ');rows<-reactive({if(loading())shiny::validate(shiny::need(FALSE,'Loading fixture'));app_roster})
 shiny::testServer(players_table_Server,args=list(players_table_RV=rows,user_teams_RV=reactive(app_teams),login_token=reactive(app_auth),championship_id=league,user_team_id=reactive('team1')),{
  app_settle(session);session$setInputs(fixture_selection=1L);app_settle(session);stopifnot(isTRUE(selected_player_ready_RV()),selected_player_RV()$id=='p1')
  loading(TRUE);app_settle(session);stopifnot(selected_player_RV()$id=='p1')
  loading(FALSE);app_settle(session);stopifnot(selected_player_RV()$id=='p1')
  league('other-league');app_settle(session);stopifnot(is.null(selected_player_RV()),is.null(selected_player_snapshot_RV()))
 });getReactableState<-original_state
})
app_check('no HTTP or live database mutation attempts',stopifnot(offline_http_attempts==0L))
cat('Offline lifecycle:',app_checks,'checks plus 9 tabs passed.\n')
for(method in c('GET','POST','PUT','PATCH','DELETE'))suppressMessages(untrace(method,where=asNamespace('httr')))
