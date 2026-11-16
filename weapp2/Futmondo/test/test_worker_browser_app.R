options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
fm_worker_can_connect<-function()TRUE
fm_worker_encrypt<-function(...)structure('synthetic',credential_id='fixture')
account<-NULL;connects<-0L
fm_worker_rpc<-function(name,payload,owner=''){
 if(name%in%c('bootstrap','read'))return(promises::promise_resolve(list(account=account,resources=list(),jobs=list(),health=list(heartbeat=rival_finance_stamp()))))
 if(name=='connect'){
  connects<<-connects+1L
  account<<-list(account_id='browser-fixture',enabled=TRUE,auth_status='active',generation=account$generation %||% 1L,timezone='Europe/Madrid',
   quiet_start=0L,quiet_end=0L,excluded_leagues=list())
  return(promises::promise_resolve(account))
 }
 if(name=='preferences'){account$excluded_leagues<<-payload$p_excluded;return(promises::promise_resolve(account))}
 if(name=='disconnect'){account$enabled<<-FALSE;account$generation<<-account$generation+1L;return(promises::promise_resolve(account))}
 stop('Unexpected database operation')
}
Sys.setenv(FUTMONDO_WORKER_KEY=paste(rep('ab',32),collapse=''))
ui<-shiny::fluidPage(shiny::tags$head(shiny::includeScript('www/worker-state.js')),
 shiny::uiOutput('worker_status'),shiny::uiOutput('worker_settings'),
 shiny::textOutput('login_state'),
 shiny::actionButton('heartbeat','Heartbeat'),shiny::textOutput('heartbeat_value'),
 shiny::tags$span(class='fm-browser-countdown',`data-deadline`=as.numeric(Sys.time())+600,`data-server-now`=as.numeric(Sys.time()),'10:00'))
server<-function(input,output,session){
 auth<-shiny::reactiveVal(c(userid='browser-fixture',token='synthetic',user_name='fixture@example.invalid'))
 connects<<-0L
 fm_worker_session(session,auth,leagues=function()list(list(id='league-a',name='League A'),list(id='league-b',name='League B')))
 session$userData$disconnect_login<-function()auth(NULL)
 output$login_state<-shiny::renderText(if(valid_login(auth()))'signed in'else'signed out')
 shiny::observeEvent(input$fixture_stats,session$sendCustomMessage('fixture-stats',list(connects=connects,enabled=account$enabled,excluded=as.list(account$excluded_leagues))))
 output$heartbeat_value<-shiny::renderText(input$heartbeat)
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=as.integer(Sys.getenv('FUTMONDO_WORKER_TEST_PORT','3897')),launch.browser=FALSE)
