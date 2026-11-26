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
get_market_players <- function(...) app_market
clear_api_cache()
shiny::testServer(app_server,{
 login_fixture(session)
 stopifnot(valid_login(login_token_RV()),startup_stage()=='ready',is.null(output$startup_status),
  grepl('Round 12',output$home_round$html),grepl('fm-browser-countdown',output$home_round$html),
  grepl('0 EUR',output$home_balance$html),grepl('17 players',output$home_squad$html),
  grepl('Continue to Rivals',output$home_continue$html),grepl('data-fm-page="admin"',output$home_destinations$html),
  !any(c('get_market_players','get_championship_pressroom') %in% calls))
 before<-length(calls);elapsed<-system.time(for(i in 1:10){session$setInputs(fixture_pulse=i);app_settle(session)})[['elapsed']]
 stopifnot(length(calls)==before)
 session$setInputs(tabs='admin');app_settle(session)
 stopifnot(grepl('30.000.000 EUR',output[['admin-league_settings']]$html,fixed=TRUE))
 session$setInputs(tabs='login');app_settle(session)
 stopifnot(startup_stage()=='ready',is.null(output$startup_status),grepl('Continue to Admin',output$home_continue$html))
 cat(sprintf('PASS Home shows independent factual values with no market/pressroom read; ten unrelated inputs add no reads (%.3fs)\n',elapsed))
})
clear_api_cache();hold_market<-TRUE;pending<-list();calls<-character()
shiny::testServer(app_server,{
 login_fixture(session);stopifnot(startup_stage()=='ready', !length(pending))
 session$setInputs(tabs='today');app_settle(session)
 stopifnot(startup_stage()=='loading_today',length(pending)==1L)
 session$setInputs(tabs='login');app_settle(session)
 stopifnot(startup_stage()=='ready',is.null(output$startup_status),grepl('0 EUR',output$home_balance$html))
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
