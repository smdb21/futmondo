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
 session$setInputs(tabs='login',`login-user_name`='offline@example.invalid',`login-password`='fixture',`login-login_button`=1)
 session$elapse(100);app_settle(session)
}
clear_api_cache()
shiny::testServer(app_server,{
 login_fixture(session)
 stopifnot(valid_login(login_token_RV()),startup_stage()=='ready',is.null(output$startup_status),
  primary_team_state()$status=='ok',primary_market_state()$status=='ok')
 before<-length(calls);elapsed<-system.time(for(i in 1:10){session$setInputs(fixture_pulse=i);app_settle(session)})[['elapsed']]
 stopifnot(startup_stage()=='ready',is.null(output$startup_status),length(calls)==before)
 session$setInputs(tabs='admin');app_settle(session)
 stopifnot(grepl('30.000.000 EUR',output[['admin-league_settings']]$html,fixed=TRUE))
 session$setInputs(tabs='login');app_settle(session);stopifnot(startup_stage()=='ready',is.null(output$startup_status))
 cat(sprintf('PASS Welcome settles with actual loaded primary resources, preserves Admin data, and ten unrelated inputs add no reads (%.3fs)\n',elapsed))
})
clear_api_cache();hold_market<-TRUE;pending<-list()
shiny::testServer(app_server,{
 login_fixture(session)
 stopifnot(startup_stage()=='loading_today',length(pending)==1L,grepl("Loading today",output$startup_status$html,fixed=TRUE))
 pending[[1]]$resolve(app_market);app_settle(session)
 stopifnot(startup_stage()=='ready',is.null(output$startup_status))
 cat('PASS a genuinely held market read shows loading, then clears on Welcome when it resolves\n')
})
clear_api_cache();pending<-list()
shiny::testServer(app_server,{
 login_fixture(session);stopifnot(startup_stage()=='loading_today',length(pending)==1L)
 pending[[1]]$reject(resource_failure(category='network',resource='get_market_players'));app_settle(session)
 stopifnot(startup_stage()=='partial',grepl('Some sections could not load',output$startup_status$html,fixed=TRUE),
  grepl('retry_startup',output$startup_status$html,fixed=TRUE))
 session$setInputs(retry_startup=1);app_settle(session);stopifnot(length(pending)==2L)
 pending[[2]]$resolve(app_market);app_settle(session)
 stopifnot(startup_stage()=='ready',is.null(output$startup_status),offline_http_attempts==0L)
 cat('PASS real market failure becomes actionable partial state on Welcome and scoped retry recovers\n')
})
cat('WELCOME PRIMARY: 3 groups passed; no live HTTP or database mutations\n')
