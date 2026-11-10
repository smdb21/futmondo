#!/usr/bin/env Rscript
# Actual server, synthetic login, and HTTP blocked by the shared lifecycle fixture.
expressions<-parse('test/test_application_offline.R')
for(expr in expressions){
 if(is.call(expr)&&identical(expr[[1]],as.name('app_check'))&&identical(expr[[2]],'full server login, navigation, refresh and logout'))break
 eval(expr,envir=.GlobalEnv)
}
get_active_championships<-function(...)list(championships=list(),leagues=list())
shiny::testServer(app_server,{
 session$setInputs(tabs='login',`login-user_name`='offline@example.invalid',`login-password`='fixture',`login-login_button`=1L)
 session$elapse(100);app_settle(session)
 stopifnot(valid_login(login_token_RV()),length(championships_RV())==0L)
 unavailable<-tryCatch({championship_RV();NULL},error=function(e)e)
 stopifnot(inherits(unavailable,'shiny.silent.error'),grepl('No active leagues',conditionMessage(unavailable)))
})
cat('PASS an authenticated account with no leagues settles with an explicit empty-league state.\n')
clear_api_cache();clear_league_contexts(app_auth[['userid']])
get_active_championships<-function(...)list(championships=list(list(id='no-team-game',name='No team fixture',league='real',ps='stats')),leagues=list(list(`_id`='real',season='source-season')))
shiny::testServer(app_server,{
 session$setInputs(tabs='login',`login-user_name`='offline@example.invalid',`login-password`='fixture',`login-login_button`=1L)
 session$elapse(100);app_settle(session)
 stopifnot(valid_login(login_token_RV()),length(championships_RV())==1L)
 unavailable<-tryCatch({championship_RV();NULL},error=function(e)e)
 stopifnot(inherits(unavailable,'shiny.silent.error'),grepl('no playable team',conditionMessage(unavailable)))
})
stopifnot(offline_http_attempts==0L)
cat('PASS a source-listed league without a playable team exposes its specific state and issues no live requests.\n')
