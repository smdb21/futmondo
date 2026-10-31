#!/usr/bin/env Rscript
# Real server/module wiring; all requests use synthetic offline fixtures.
expressions<-parse('test/test_application_offline.R')
for(expr in expressions){
 if(is.call(expr)&&identical(expr[[1]],as.name('app_check'))&&identical(expr[[2]],'full server login, navigation, refresh and logout'))break
 eval(expr,envir=.GlobalEnv)
}
checks<-0L;check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
if(identical(Sys.getenv('FUTMONDO_BASELINE_SERVER'),'true'))app_server<-eval(parse(text=system2('git',c('show','HEAD:weapp2/Futmondo/server.R'),stdout=TRUE)))
original_async<-async_adapter_fetch;calls<-character()
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE){calls<<-c(calls,fn);original_async(fn,args,context,owner,is_current)}
clear_api_cache()
shiny::testServer(app_server,{
 session$setInputs(tabs='login',`login-user_name`='offline@example.invalid',`login-password`='fixture',`login-login_button`=1)
 session$elapse(100);app_settle(session);session$setInputs(tabs='today');app_settle(session)
 invisible(output[['today-market_players-players_table']]);before<-calls
 session$elapse(180000);app_settle(session);invisible(output$round_countdown)
 added<-if(length(calls)>length(before))tail(calls,length(calls)-length(before))else character()
 check('idle Today clock does not refetch market team or round data',stopifnot(!any(added%in%c('get_market_players','get_user_team_info','get_finished_rounds'))))
 for(tab in c('admin','round_mvps')){
  session$setInputs(tabs=tab);app_settle(session);before<-calls
  session$elapse(180000);app_settle(session);invisible(output$round_countdown)
  added<-if(length(calls)>length(before))tail(calls,length(calls)-length(before))else character()
  check(paste('idle',tab,'does not read hidden table sources or player histories'),stopifnot(!any(added%in%c('get_market_players','get_user_team_info','get_finished_rounds','get_championship_players','get_player_recent_points'))))
 }
 before<-calls;session$setInputs(tabs='today',refresh_all=1);app_settle(session)
 added<-tail(calls,length(calls)-length(before))
 check('explicit Refresh still reloads shared primary resources',stopifnot(sum(added=='get_market_players')==1L,sum(added=='get_user_team_info')==1L))
})
async_adapter_fetch<-original_async
original_points<-recent_player_points_source;original_update<-updateReactable;original_state<-getReactableState
history_fixture<-reactiveVal(data.frame(player_id='p1',round=1:5,points=1:5));status_fixture<-reactiveVal(list(total=17L,remaining=2L,pending=0L,saving=0L,failed=0L,matrix_complete=TRUE))
recent_player_points_source<-function(...)list(history=history_fixture,status=status_fixture)
updates<-list()
updateReactable<-function(outputId,data=NULL,...){if(!is.null(data))updates[[length(updates)+1L]]<<-list(data=data,args=list(...));invisible(NULL)}
getReactableState<-function(outputId,name,session){switch(name,page=2L,pageSize=5L,selected=session$input$fixture_selected,NULL)}
rows_fixture<-reactiveVal(app_roster)
shiny::testServer(players_table_Server,args=list(players_table_RV=rows_fixture,user_teams_RV=reactive(app_teams),login_token=reactive(app_auth),championship_id=reactive('champ'),user_team_id=reactive('team1')),{
 session$setInputs(recent_games_n=5L);app_settle(session);initial<-output$players_table;stopifnot(length(updates)==0L)
 status_fixture(list(total=17L,remaining=1L,pending=0L,saving=1L,failed=0L,matrix_complete=TRUE));app_settle(session)
 check('history progress alone does not rebuild or update the table',stopifnot(identical(initial,output$players_table),length(updates)==0L))
 history_fixture(data.frame(player_id='p1',round=1:5,points=2:6));app_settle(session)
 check('new averages update data in place and preserve the current page',stopifnot(identical(initial,output$players_table),length(updates)==1L,updates[[1]]$data$recent_points_avg[1]==4,updates[[1]]$args$page==2L))
 rows<-rows_fixture();rows$points<-as.numeric(rows$points);attr(rows,'observed_at')<-Sys.time();rows$observed_at<-'2026-10-31T00:00:00Z';rows_fixture(rows);app_settle(session)
 check('source timestamps with unchanged visible values send no table update',stopifnot(length(updates)==1L,identical(initial,output$players_table)))
 session$setInputs(fixture_selected=1L);app_settle(session)
 rows<-rows[c(2,1,3:nrow(rows)),,drop=FALSE];rows_fixture(rows);app_settle(session)
 check('non-average row changes rebuild R-formatted cells and preserve captured card identity',stopifnot(!identical(initial,output$players_table),selected_player_RV()$id=='p1',length(updates)==1L))
 previous<-output$players_table;session$setInputs(recent_games_n=3L);app_settle(session)
 check('changing N updates the average header and schema',stopifnot(!identical(previous,output$players_table)))
})
recent_player_points_source<-original_points;updateReactable<-original_update;getReactableState<-original_state
check('inactive history client never demands player rows',{
 active_fixture<-reactiveVal(FALSE);row_reads<-0L
 shiny::testServer(function(input,output,session){source<-recent_player_points_source(session,reactive(app_auth),reactive('champ'),reactive('team1'),function(){row_reads<<-row_reads+1L;app_roster},active_fixture)},{
  app_settle(session);stopifnot(row_reads==0L)
  active_fixture(TRUE);app_settle(session);stopifnot(row_reads>0L)
  active_fixture(FALSE);app_settle(session);before<-row_reads;session$elapse(180000);app_settle(session);stopifnot(row_reads==before)
 })
})
check('offline fixture makes no HTTP calls',stopifnot(offline_http_attempts==0L))
for(method in c('GET','POST','PUT','PATCH','DELETE'))suppressMessages(untrace(method,where=asNamespace('httr')))
cat(sprintf('REFRESH STABILITY: %d passed / 0 failed\n',checks))
