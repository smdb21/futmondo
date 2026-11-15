# Synthetic durable store only; HTTP acquisition stays disabled.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
Sys.setenv(admin='admin@example.invalid')
fixture_rows<-list(alice=list(user_id='alice',display_name='Alice',timezone='Europe/Madrid',source='user'),
 bob=list(user_id='bob',display_name='Bob',timezone='Asia/Tokyo',source='admin'))
fixture_calls<-0L
fixture_store<-function(user_id=NULL,timezone=NULL,label='',mode='get',token=NULL,owner='',after='',search=''){
 fixture_calls<<-fixture_calls+1L
 if(mode=='list')return(promises::promise_resolve(unname(fixture_rows)))
 row<-fixture_rows[[user_id]]
 if(mode=='set'||is.null(row)){
  row<-list(user_id=user_id,display_name=if(is.null(row))label else row$display_name,timezone=timezone,source=mode)
  fixture_rows[[user_id]]<<-row
 }
 promises::promise_resolve(row)
}
ui<-shiny::fluidPage(shiny::tags$head(shiny::includeScript('www/worker-state.js')),
 shiny::uiOutput('timezone_preference_status'),shiny::uiOutput('timezone_settings'),
 shiny::actionButton('signout','Sign out / in'),shiny::textOutput('dates'),
 shiny::uiOutput('incoming'),shiny::uiOutput('notices'),
 shiny::h2('Admin'),shiny::uiOutput(shiny::NS('admin','timezone_admin')),
 shiny::textOutput('stats'),shiny::actionButton('heartbeat','Heartbeat'),shiny::textOutput('beats'))
server<-function(input,output,session){
 auth<-shiny::reactiveVal(c(userid='admin',token='synthetic',user_name='admin@example.invalid'))
 fm_timezone_preferences(session,auth,fixture_store)
 shiny::moduleServer('admin',function(input,output,session)fm_admin_timezones(session,auth,function()!is.null(auth()),fixture_store))
 shiny::observeEvent(input$signout,if(is.null(auth()))auth(c(userid='admin',token='synthetic',user_name='admin@example.invalid'))else auth(NULL))
 output$dates<-shiny::renderText(fm_format_datetime('2026-11-15T06:02:48Z'))
 output$notices<-shiny::renderUI(shiny::p(notification_datetime_labels('2026-11-15T06:02:48Z')))
 output$incoming<-shiny::renderUI(clause_transfer_panel(list(status='ok',records=data.frame()),
  list(available=TRUE,round_number=13,starts_at=fm_time('2026-11-15T06:02:48Z')),identity))
 output$stats<-shiny::renderText({input$heartbeat;fixture_calls})
 output$beats<-shiny::renderText(input$heartbeat %||% 0)
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=3891,launch.browser=FALSE)
