# Actual root application with synthetic factual reads and held market responses.
options(futmondo.offline=TRUE)
for(expr in parse('test/test_application_offline.R')){
 if(is.call(expr)&&identical(expr[[1]],as.name('app_check'))&&identical(expr[[2]],'full server login, navigation, refresh and logout'))break
 eval(expr,envir=.GlobalEnv)
}
Sys.setenv(admin='offline@example.invalid')
fm_timezone_store<-function(...)promises::promise_resolve(NULL)
real_async<-async_adapter_fetch;pending<-list();hold_market<-FALSE;calls<-character()
async_adapter_fetch<-function(fn,args=list(),context,...){
 calls<<-c(calls,fn)
 if(identical(fn,'get_market_players')&&hold_market)return(promises::promise(function(resolve,reject){pending[[length(pending)+1L]]<<-list(resolve=resolve,reject=reject)}))
 real_async(fn,args,context,...)
}
login_fixture<-function(session){
 app_settle(session)
 session$setInputs(tabs='login',`login-user_name`='offline@example.invalid',`login-password`='fixture',`login-login_button`=1)
 session$elapse(100);app_settle(session)
}
fm_page_store <- function(user_id, page=NULL, ...) promises::promise_resolve(if(is.null(page)) list(page='rivals') else TRUE)
app_rounds <- data.frame(round_id='next', round_number=12L, begin_process=Sys.time()+86400, is_finished=FALSE)
app_roster$change <- c(42000, -12000, 4000, rep(0, 14))
app_market$bid_id <- c("first", "second", "second")
app_market$bid_price <- c(200000, 150000, 150000)
get_market_players <- function(...) app_market
empty_history <- get_championship_pressroom
get_championship_pressroom <- function(...) data.frame(id=c("buy", "sell"), player_id=c("arrival", "p1"),
  player_name=c("Arrival", "Leaving"), buyer_team_id=c("team1", "team2"), seller_team_id=c("team2","team1"),
  created=format(Sys.time()-100, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"), is_clause=TRUE)
incoming_clause_cached_summary <- function(login, championship_id, user_team_id, player_id, ...) {
  observed_data(list(data=list(id=player_id), championship=list(
    owner=list(id=if(player_id=="p1") "team1" else "team2"),
    clause=list(transferred=TRUE, transferDate=format(Sys.time()+86400,"%Y-%m-%dT%H:%M:%SZ",tz="UTC")))))
}
clear_api_cache()
shiny::testServer(app_server,{
 login_fixture(session)
 stopifnot(valid_login(login_token_RV()),startup_stage()=='ready',is.null(output$startup_status),
  grepl('Round 12',output$home_round$html),grepl('fm-browser-countdown',output$home_round$html),
  grepl('Time until kickoff', output$home_round$html, fixed=TRUE),
  !grepl('Showing the saved schedule', output$home_round$html, fixed=TRUE),
  grepl('0 €',output$home_balance$html),grepl('17 players',output$home_squad$html),
  grepl('Continue to Rivals',output$home_continue$html),grepl('data-fm-page="admin"',output$home_destinations$html),
  sum(calls=='get_market_players')==1L, sum(calls=='get_championship_pressroom')==1L)
 balance <- xml2::read_html(output$home_balance$html)
 stale_card <- as.character(home_observation(list(status='stale', observed_at=as.POSIXct('2001-02-03', tz='UTC')), '17 players'))
 stopifnot(grepl('May be out of date', stale_card, fixed=TRUE), !grepl('Last observed|2001', stale_card))
 trend <- xml2::xml_find_first(balance, '//*[@class="fm-team-value-trend"]')
 squad <- xml2::read_html(output$home_squad$html)
 count_text <- function(direction) {
   node <- xml2::xml_find_first(squad, paste0('.//*[@class="fm-team-trend-', direction, '"]'))
   trimws(gsub('[[:space:]]+', ' ', xml2::xml_text(node)))
 }
 value <- xml2::xml_find_first(trend, 'preceding-sibling::*[1][@class="fm-home-squad-value"]')
 stopifnot(grepl('Squad value', xml2::xml_text(value), fixed=TRUE),
   xml2::xml_text(xml2::xml_find_first(value, 'strong')) == '34.000.000 €',
   grepl('24h squad trend', xml2::xml_text(trend), fixed=TRUE),
   grepl('+34.000', xml2::xml_text(trend), fixed=TRUE),
   !grepl('fm-team-trend-counts', output$home_balance$html, fixed=TRUE),
   grepl('2 rising$', count_text('up')), grepl('1 falling$', count_text('down')),
   grepl('Players value',output$home_squad$html,fixed=TRUE),
   grepl('2 · 350.000 €', output$home_balance$html, fixed=TRUE),
   identical(home_transfer_counts(),list(incoming=1L, leaving=1L)),
   sum(calls == 'get_players_from_team') == 1L)
 before<-length(calls);elapsed<-system.time(for(i in 1:10){session$setInputs(fixture_pulse=i);app_settle(session)})[['elapsed']]
 stopifnot(length(calls)==before)
 local({
   # Advance the same one-shot callback used at kickoff without a real wait.
   after_kickoff <- app_rounds$begin_process[1] + 1
   assign('Sys.time', function() after_kickoff, envir=.GlobalEnv)
   on.exit(rm('Sys.time', envir=.GlobalEnv))
   stopifnot(is.function(session$env$home_round_timer))
   advance_home_round()
   session$flushReact()
   stopifnot(grepl('Awaiting round status', output$home_round$html, fixed=TRUE),
     !grepl('fm-browser-countdown', output$home_round$html, fixed=TRUE),
     is.null(session$env$home_round_timer), length(calls)==before)
 })
 session$setInputs(tabs='admin');app_settle(session)
 stopifnot(grepl('30.000.000 EUR',output[['admin-league_settings']]$html,fixed=TRUE))
 session$setInputs(tabs='login');app_settle(session)
 stopifnot(startup_stage()=='ready',is.null(output$startup_status),grepl('Continue to Admin',output$home_continue$html))
 cat(sprintf('PASS Home shows independent factual values from shared roster, offer and transfer reads; ten unrelated inputs add no reads (%.3fs)\n',elapsed))
})
clear_api_cache();hold_market<-TRUE;pending<-list();calls<-character()
get_championship_pressroom <- empty_history
app_roster$change[1] <- NA_real_
shiny::testServer(app_server,{
 login_fixture(session);stopifnot(startup_stage()=='ready', length(pending)==1L,
   grepl('Loading…', output$home_balance$html,fixed=TRUE),
   identical(home_transfer_counts(),list(incoming=0L, leaving=0L)))
 balance <- xml2::read_html(output$home_balance$html)
 stopifnot(grepl('0 €', output$home_balance$html, fixed=TRUE),
   grepl('17 players', output$home_squad$html, fixed=TRUE),
   xml2::xml_text(xml2::xml_find_first(balance, '//*[@class="fm-team-value-trend"]//strong')) == 'Unavailable',
   length(xml2::xml_find_all(xml2::read_html(output$home_squad$html), '//*[@class="fm-team-trend-counts"]//span[text()="Unavailable"]')) == 2L)
 session$setInputs(tabs='today');app_settle(session)
 stopifnot(startup_stage()=='loading_today',length(pending)==1L)
 session$setInputs(tabs='login');app_settle(session)
 stopifnot(startup_stage()=='ready',is.null(output$startup_status),grepl('0 €',output$home_balance$html))
 pending[[1]]$resolve(app_market);app_settle(session)
 stopifnot(startup_stage()=='ready',is.null(output$startup_status))
 cat('PASS held market only blocks Today, and returning Home is immediate\n')
})
clear_api_cache();pending<-list()
shiny::testServer(app_server,{
 login_fixture(session);session$setInputs(tabs='today');app_settle(session)
 pending[[1]]$reject(resource_failure(category='network',resource='get_market_players'));app_settle(session)
 stopifnot(startup_stage()=='partial',grepl('retry_startup',output$startup_status$html,fixed=TRUE))
 session$setInputs(retry_startup=1);app_settle(session);stopifnot(length(pending)==2L)
 pending[[2]]$resolve(app_market);app_settle(session)
 stopifnot(startup_stage()=='ready',is.null(output$startup_status),offline_http_attempts==0L)
 cat('PASS a failed Today market read remains actionable and recovers after scoped retry\n')
})
clear_api_cache();Sys.unsetenv('admin')
get_active_championships<-function(...)list(championships=list(),leagues=list())
shiny::testServer(app_server,{
 login_fixture(session)
 stopifnot(startup_stage()=='empty', grepl('Unavailable',output$home_balance$html),
  grepl('Unavailable',output$home_squad$html), grepl('Unavailable',output$home_round$html),
  !grepl('data-fm-page="admin"',output$home_destinations$html))
 cat('PASS an account without a playable league keeps Home navigation and shows truthful empty metrics\n')
})
cat('WELCOME PRIMARY: 4 groups passed; no live HTTP or database mutations\n')
