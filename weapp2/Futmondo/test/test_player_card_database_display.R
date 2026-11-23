#!/usr/bin/env Rscript
# Real snapshot codec + card, with synthetic persisted data and held manual checks.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
now<-as.POSIXct('2026-11-16 12:00:00',tz='UTC');Sys.time<-function()now
get_sb_url<-function()'';get_sb_key<-function()''
fm_worker_configured<-function()TRUE
record_acquired_resource<-function(...)invisible(TRUE)
persist_player_valuation_profile<-function(...)invisible(TRUE)
futmondo_post<-function(...)stop('Browsing must not query Futmondo')
supabase_get<-function(...)stop('Unexpected database path')
buy_clause<-function(...)stop('No transaction is authorized')
auth<-c(userid='card-database-display',token='synthetic')
ctx<-resource_context(auth,'league','mine','player')
args<-list(championship_id='league',user_team_id='mine')
team_ctx<-ctx;team_ctx$player_id<-''
database<-list();version<-0L
snapshot<-function(handler,arguments,value,context=ctx,age=120,ttl=60,status='ok',complete='unknown'){
  value<-observed_data(value,observed_at=now-age)
  wire<-charToRaw(fm_worker_json(fm_worker_encode(value)));compressed<-history_baseline_compress(wire)
  d<-fm_worker_descriptor(handler,arguments,context);version<<-version+1L
  m<-list(account_id=ctx$account_id,resource_key=d$resource_key,context=d$context,handler=handler,
    version=as.character(version),json_bytes=length(wire),parts=1L,sha256=paste0(openssl::sha256(compressed)),
    observed_at=rival_finance_stamp(now-age),checked_at=rival_finance_stamp(now-age),
    valid_until=rival_finance_stamp(now-age+ttl),status=status,completeness=complete)
  database[[d$resource_key]]<<-list(key=d$resource_key,manifest=m,chunks=list(jsonlite::base64_enc(compressed)))
  invisible(d)
}
summary<-function(owner='rival',price=150)list(data=list(id='player'),championship=list(
  owner=list(id=owner,name='Saved owner',computer=FALSE)),market=list(p=price,bids=list()),points=list(),prices=list())
target<-snapshot('get_player_summary',c(args,list(player_id='player')),summary())
snapshot('get_lineup_from_team',args,list(pointSystem='fixture'),team_ctx)
snapshot('get_player_full_profile',list(player_id='player'),list(profile=list()))
snapshot('get_player_matches',list(player_id='player'),list(matches=list()))
listings<-snapshot('get_my_market_players',args,data.frame(id='player',price=150),team_ctx)
restore<-function(key){
  item<-fm_worker_restore(database[[key]],ctx$account_id)
  fm_worker_put(ctx$account_id,key,item)
  account<-fm_worker_account_state(ctx$account_id)
  revision<-fm_worker_resource_revision(account,key);revision(shiny::isolate(revision())+1L)
  invisible(item)
}
restore_all<-function(){
  clear_api_cache();.fm_worker$accounts[[ctx$account_id]]<-NULL
  account<-fm_worker_account_state(ctx$account_id);account$known<-TRUE;account$bootstrap_complete<-TRUE
  account$account<-list(enabled=TRUE,auth_status='active')
  invisible(lapply(names(database),restore))
}
row<-data.frame(id='player',name='Fixture',role='delantero',team='Club',value=200,
  price=NA_real_,change=0,points=-2,user_team_id='old-owner',market_inMarket=FALSE)
saved<-fm_worker_restore(database[[target$resource_key]],ctx$account_id)$data
projected<-player_card_summary_row(row,saved,observed=TRUE)
stopifnot(!is.null(projected),projected$user_team_id=='rival',projected$price==150,
  projected$points==-2,attr(projected,'observed_at')==now-120,
  attr(projected,'valid_until')==as.numeric(now)-60,
  !player_availability_valid(saved,'summary'),!player_card_recent_table_facts(projected),
  identical(attr(projected,'fetch_status'),'stale'))
for(kind in c('partial','failed','future','missing-clock','missing-owner','missing-market','wrong-player','error')){
  bad<-saved
  switch(kind,partial={attr(bad,'worker_completeness')<-'partial'},failed={attr(bad,'worker_acquisition_status')<-'unavailable'},
    future={attr(bad,'observed_at')<-now+60},`missing-clock`={attr(bad,'observed_at')<-NULL},
    `missing-owner`={bad$championship$owner<-NULL},`missing-market`={bad$market<-list()},
    `wrong-player`={bad$data$id<-'other'},error={attr(bad,'fetch_error')<-resource_failure(category='network')})
  stopifnot(is.null(player_card_summary_row(row,bad,observed=TRUE)))
}
cat('PASS expired successful snapshots retain exact ownership/price/clocks/negative points; incomplete, failed, mismatched and future evidence is rejected\n')

real_async<-async_adapter_fetch;source_reads<-character();history_reads<-0L;pending<-list();fail_history<-FALSE
async_adapter_fetch<-function(fn,args=list(),context,intent='display',...){
  if(identical(intent,'manual_action')){
    source_reads<<-c(source_reads,fn)
    return(promises::promise(function(resolve,reject)pending[[fn]]<<-list(resolve=resolve,reject=reject)))
  }
  if(fn=='read_card_round_history'){
    history_reads<<-history_reads+1L
    if(fail_history)return(promises::promise_reject(resource_failure(category='network')))
    data<-data.frame(player_id=context$player_id,championship_id=context$championship_id,
      competition_id=context$competition_id,season=context$season,scoring_version=context$scoring_version,
      round=c(1,2),points=c(0,-2),score_status='unknown',observed_at=rival_finance_stamp(now-120))
    return(promises::promise_resolve(observed_data(data,observed_at=now-120)))
  }
  if(fn=='read_card_price_history')return(promises::promise_resolve(data.frame()))
  real_async(fn,args,context,intent=intent,...)
}
fm_worker_rpc<-function(...)stop('No acquisition/queue writes from browsing')
settle<-function(session)for(i in 1:25){later::run_now(0);session$flushReact()}
blocked<-function(html,id){tag<-regmatches(html,regexpr(paste0('<button[^>]*id="[^"]*',id,'"[^>]*>'),html));!length(tag)||grepl('disabled',tag,fixed=TRUE)}
selected<-shiny::reactiveVal(row)
restore_all()
started<-proc.time()[['elapsed']]
shiny::testServer(selected_player_Server,args=list(selected_player=selected,login_token=reactive(auth),
  championship_id=reactive('league'),user_team_id=reactive('mine')),{
  settle(session)
  html<-output$action_buttons$html;history<-output$recent_round_points$html
  stopifnot(player_target_ready(),!player_card_recent_table_facts(selected_player()),selected_player()$user_team_id=='rival',
    !blocked(html,'btn_bid_market'),!grepl('Retry availability|btn_verify_availability|not verified',html),
    !grepl('refresh failed',history),grepl('awaiting a newer background observation',history,fixed=TRUE),
    identical(card_history()$points$points,c(0,-2)),!length(source_reads))
  session$setInputs(btn_verify_availability=1L);settle(session)
  session$setInputs(refresh_history=1L);settle(session)
  selected(NULL);settle(session);selected(row);settle(session)
  stopifnot(!length(source_reads),!grepl('Retry availability',output$action_buttons$html,fixed=TRUE))
  cat('PASS opening, Refresh, reopening and an obsolete Retry input remain database-only with no false failure or Retry control\n')
  # A new saved summary updates facts automatically; the old owner cannot linger.
  snapshot('get_player_summary',c(args,list(player_id='player')),summary('new-rival',175),age=90)
  restore(target$resource_key);settle(session)
  stopifnot(selected_player()$user_team_id=='new-rival',selected_player()$price==175,!length(source_reads))
  cat('PASS newer durable observations update the open card automatically without source requests\n')
  session$setInputs(btn_bid_market=1L);settle(session)
  stopifnot(!length(source_reads), isTRUE(offer_modal_opened_RV()),
    market_offer_dialog_RV()$player$price == 175)
  cat('PASS explicit offer opens from the saved player observation without source reads\n')
})
cat(sprintf('METRIC initial card plus refresh/update and saved editor %.3fs\n',proc.time()[['elapsed']]-started))

# A new process/session restores database bytes, never the display-only marker.
restore_all();source_reads<-character();selected(row);league<-shiny::reactiveVal('league')
shiny::testServer(selected_player_Server,args=list(selected_player=selected,login_token=reactive(auth),
  championship_id=league,user_team_id=reactive('mine')),{
  settle(session);stopifnot(selected_player()$user_team_id=='new-rival',!player_card_recent_table_facts(selected_player()),!length(source_reads))
  league('another-league');settle(session)
  stopifnot(!player_target_ready(),selected_player()$user_team_id=='old-owner',!length(source_reads))
  league('league');settle(session)
  stopifnot(selected_player()$user_team_id=='new-rival',!length(source_reads))
  selected(transform(row,id='missing'));settle(session)
  stopifnot(!player_target_ready(),!grepl('Retry availability|btn_verify_availability',output$action_buttons$html),
    grepl('saved data',output$action_buttons$html,fixed=TRUE),!length(source_reads))
  selected(row);settle(session)
  fail_history<<-TRUE;session$setInputs(refresh_history=1L);settle(session)
  stopifnot(length(card_history()$errors)>0L,grepl('Connection unavailable',output$recent_round_points$html,fixed=TRUE),
    !length(source_reads))
  cat('PASS restarted cards reuse durable data with league/player isolation; missing data stays unknown and real database failures remain visible\n')
})
restore_all();fail_history<-FALSE
snapshot('get_player_summary',c(args,list(player_id='player')),summary('mine'),age=120)
restore(target$resource_key)
selected(transform(row,user_team_id='mine'))
shiny::testServer(selected_player_Server,args=list(selected_player=selected,login_token=reactive(auth),
  championship_id=reactive('league'),user_team_id=reactive('mine')),{
  settle(session);stopifnot(!grepl('Retry availability|btn_verify_availability|not verified',output$action_buttons$html),!length(source_reads))
  cat('PASS own-player listings also reuse successful saved observations without a browsing retry\n')
})
# Clause pricing can survive a change to System ownership in the source summary.
clause_summary <- function(owner, opens) {
  value <- summary(owner)
  value$championship$owner$computer <- !nzchar(owner)
  value$championship$clause <- list(price=300, transferred=FALSE, date=opens)
  value
}
publish_clause <- function(owner, opens) {
  snapshot('get_player_summary', c(args, list(player_id='player')), clause_summary(owner, opens), age=120L-version)
  restore(target$resource_key)
}
past <- '2026-11-15T12:00:00Z'
future <- '2026-11-17T12:00:00Z'
publish_clause('rival', past)
selected(row)
action <- shiny::reactiveVal(NULL)
shiny::testServer(selected_player_Server, args=list(selected_player=selected, login_token=reactive(auth),
  championship_id=reactive('league'), user_team_id=reactive('mine'), open_action=action), {
  settle(session)
  stopifnot(!blocked(output$action_buttons$html, 'btn_pay_clause'))
  publish_clause('rival', future); settle(session)
  stopifnot(grepl('Release Clause Locked', output$action_buttons$html, fixed=TRUE),
    !grepl('btn_pay_clause', output$action_buttons$html, fixed=TRUE))
  remember_action_context('clause')
  for (opens in c(past, future)) {
    publish_clause('', opens); settle(session)
    html <- output$action_buttons$html
    stopifnot(player_target_ready(), selected_player()$clause_price==300,
      grepl('Ownership: Free Agent', html, fixed=TRUE), !blocked(html, 'btn_bid_market'),
      !grepl('btn_pay_clause|Release Clause Locked|schedule_clause|test_locked_clause', html),
      !length(source_reads))
  }
  publish_clause('', past); settle(session)
  session$setInputs(submit_clause=1L); settle(session)
  stopifnot(is.null(modal_context_RV()$clause), !length(source_reads))
  action('clause_buyout')
  session$setInputs(btn_pay_clause=1L); settle(session)
  stopifnot(!isTRUE(clause_modal_opened_RV()), is.null(modal_context_RV()$clause), !length(source_reads))
  cat('PASS saved listed free agents keep market offers and reject stale clause entry points; rival clauses still render open and locked states\n')
})
elapsed<-system.time(for(i in 1:1000)player_availability_observed(saved,'summary'))[['elapsed']]
cat(sprintf('METRIC 1000 display-evidence validations %.3fs; no I/O, timers or additional cache\n',elapsed))


# Replay the conflicting player league label through the same durable snapshot
# codec and asynchronous adapter used by a real card.
active <- list(championships=list(list(id='league',league='domestic',ps='fixture')),
  leagues=list(list(`_id`='domestic',season='2026-2027')))
publish_active_league_contexts(auth,active)
scoped <- resource_context(auth,'league','mine','player')
scoped_team <- scoped;scoped_team$player_id <- ''
fixture <- function(id, round, date) list(id=id,r=round,info=list(date=date),
  h=list(id='home',name='Home',score=2),a=list(id='away',name='Away',score=1))
first <- fixture('game-1',1,'2026-09-01T14:00:00Z')
second <- fixture('game-2',2,'2026-09-08T14:00:00Z')
schedule <- data.frame(round_id=c('r1','r2'),round_number=1:2,is_finished=TRUE)
attr(schedule,'fixture_calendar') <- list(status='ok',observed_at=now-300,valid_until=as.numeric(now)-60,
  scope=as.list(scoped[c('account_id','championship_id','competition_id','season')]),
  matches=league_fixture_matches(list(list(`_id`='r1',matches=list(first)),list(`_id`='r2',matches=list(second)))))
snapshot('get_finished_rounds',list(championship_id='league'),schedule,scoped_team,complete='complete')
snapshot('get_player_matches',list(player_id='player'),
  list(league=list(`_id`='domestic',season='2025-2026'),matches=list(second)),scoped)
scoped_summary <- summary()
scoped_summary$prices <- list(list(date='2026-09-03T00:00:00Z',price=200))
snapshot('get_player_summary',c(args,list(player_id='player')),scoped_summary,scoped)
snapshot('get_lineup_from_team',args,list(pointSystem='fixture'),scoped_team)
restore_all();selected(row)
shiny::testServer(selected_player_Server,args=list(selected_player=selected,login_token=reactive(auth),
  championship_id=reactive('league'),user_team_id=reactive('mine')),{
  for (pass in 1:2) {
    settle(session)
    html <- output$recent_round_points$html
    rounds <- card_round_details()
    stopifnot(grepl('Home 2–1 Away',html,fixed=TRUE),grepl('-2 pts',html,fixed=TRUE),
      grepl('0 pts',html,fixed=TRUE),grepl('One round has no verified player fixture',html,fixed=TRUE),
      !grepl('separate round axis|no verified points link',html),
      identical(as.numeric(rounds$dates),as.numeric(fm_time(c(first$info$date,second$info$date)))),
      identical(attr(rounds$dates,'round_calendar'),c(TRUE,FALSE)),!length(source_reads))
    session$setInputs(refresh_history=pass)
  }
  cat('PASS saved match IDs repair the league-season mismatch, match and calendar dates stay distinct, and Refresh reuses persisted data\n')
})
