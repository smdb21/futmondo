# Synthetic durable personal preferences only; no live HTTP or database writes.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fixture_rows<-list();fixture_counts<-list()
ui<-shiny::fluidPage(shiny::tags$head(shiny::includeScript('www/worker-state.js')),
 shiny::h2('Your display settings'),
 shiny::uiOutput('timezone_preference_status'),shiny::uiOutput('timezone_settings'),
 shiny::actionButton('signout','Sign out / in'),shiny::actionButton('switch_user','Switch account'),
 shiny::textOutput('dates'),shiny::uiOutput('incoming'),shiny::uiOutput('notices'),
 shiny::textOutput('stats'),shiny::actionButton('heartbeat','Heartbeat'),shiny::textOutput('beats'))
server<-function(input,output,session){
 key<-shiny::parseQueryString(shiny::isolate(session$clientData$url_search))$case %||% 'personal-fixture'
 second<-paste0(key,'-second')
 if(is.null(fixture_rows[[key]])){
  fixture_rows[[key]]<<-list(user_id=key,timezone='Asia/Tokyo',source='browser')
  fixture_rows[[second]]<<-list(user_id=second,timezone='Asia/Tokyo',source='user')
  fixture_counts[[key]]<<-list(reads=0L,writes=0L)
 }
 current_id<-shiny::reactiveVal(key)
 token<-function(id)c(userid=id,token='synthetic',user_name=paste0(id,'@example.invalid'))
 auth<-shiny::reactiveVal(token(key))
 fixture_store<-function(user_id=NULL,timezone=NULL,mode='get',token=NULL,owner=''){
  stopifnot(identical(user_id,unname(token[['userid']])),mode%in%c('get','set'))
  counts<-fixture_counts[[key]];field<-if(mode=='get')'reads'else'writes';counts[[field]]<-counts[[field]]+1L;fixture_counts[[key]]<<-counts
  if(mode=='get')return(promises::promise_resolve(fixture_rows[[user_id]]))
  promises::promise(function(resolve,reject)later::later(function(){
   row<-list(user_id=user_id,timezone=timezone,source='user');fixture_rows[[user_id]]<<-row;resolve(row)
  },.6))
 }
 fm_timezone_preferences(session,auth,fixture_store)
 shiny::observeEvent(input$signout,if(is.null(auth()))auth(token(current_id()))else auth(NULL))
 shiny::observeEvent(input$switch_user,{current_id(if(current_id()==key)second else key);auth(token(current_id()))})
 output$dates<-shiny::renderText(fm_format_datetime('2026-11-15T06:02:48Z'))
 output$notices<-shiny::renderUI(shiny::p(notification_datetime_labels('2026-11-15T06:02:48Z')))
 output$incoming<-shiny::renderUI(clause_transfer_panel(list(status='ok',records=data.frame()),
  list(available=TRUE,round_number=13,starts_at=fm_time('2026-11-15T06:02:48Z')),identity))
 output$stats<-shiny::renderText({input$heartbeat;fm_display_timezone();jsonlite::toJSON(fixture_counts[[key]],auto_unbox=TRUE)})
 output$beats<-shiny::renderText(input$heartbeat %||% 0)
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=3891,launch.browser=FALSE)
