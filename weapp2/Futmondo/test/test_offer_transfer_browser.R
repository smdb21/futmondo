options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
players_table_UI<-function(...)NULL
io<-0L;blocked<-function(...){io<<-io+1L;stop('Unexpected source/database call')}
for(name in c('futmondo_post','fm_async_http','fm_worker_rpc','supabase_get','defer_persistence'))assign(name,blocked)
now<-Sys.time();source_stamp<-now-1200
# Explicit named assignment avoids depending on the record schema's column order.
record<-incoming_clause_empty()[NA_integer_,,drop=FALSE]
record$player_id<-'in';record$player_name<-'Incoming player';record$seller_team_id<-'seller'
record$purchased_at<-now-3600;record$joins_at<-now+86400;record$observed_at<-now-1200
record$role<-'delantero';record$team<-'Club';record$logo<-'';record$timing_error<-'';record$overdue<-FALSE
out<-outgoing_clause_empty()[NA_integer_,,drop=FALSE]
for(field in intersect(names(record),names(out)))out[[field]]<-record[[field]]
out$player_id<-'out';out$player_name<-'Leaving player';out$seller_team_id<-'mine';out$buyer_team_id<-'buyer';out$leaves_at<-now+43200
roster<-observed_data(data.frame(id=c('p','q','r'),name=c('Marco Asensio','Brais Méndez','Isco'),
  team=c('Real Madrid','Real Sociedad','Betis'),logo=c('real-madrid.png','',''),buyPrice=c(9.5e6,54911066,NA_real_),
  value=1000,points=1),status='stale',observed_at=now-1200)
attr(roster,'worker_acquisition_status')<-'ok';attr(roster,'worker_completeness')<-'unknown'
attr(roster,'received_offers_status')<-'ok';attr(roster,'received_offers_observed_at')<-now-1200
attr(roster,'received_offers')<-rbind(
  normalize_received_offer(list(id='rival-bid',price=12e6,userTeam=list(id='r',name='Rival with a long <club> name')),'p'),
  normalize_received_offer(list(id='system-bid',price=25271066),'q'),
  normalize_received_offer(list(id='unknown-profit',price=11e6),'r'))

ui<-shiny::fluidPage(shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),
 shiny::includeCSS('www/custom_style.css'),shiny::includeScript('www/info-disclosures.js')),
 shiny::selectInput('tabs','Page',c('My Team'='yourteam','Market'='today','All Players'='players_in_championship')),
 shiny::uiOutput('worker_status'),players_in_teams_UI('styles'),shiny::div(id='incoming',incoming_clause_panel(list(status='observed',records=record,observations_only=TRUE),list(available=TRUE,round_number=13,starts_at=now+172800),identity)),
 shiny::div(id='outgoing',outgoing_clause_panel(list(status='observed',records=out,observations_only=TRUE),list(available=TRUE,round_number=13,starts_at=now+172800),identity)),
 shiny::uiOutput('offers'),shiny::textOutput('opened_offer'),shiny::actionButton('advance','Advance observed clock'),shiny::actionButton('heartbeat','Heartbeat'),shiny::textOutput('beats'),shiny::textOutput('calls'))
server<-function(input,output,session){
 state<-shiny::reactiveVal(resource_state(roster))
 status<-shiny::reactive(list(message='Background sync is enabled.',error=FALSE,checked_at=source_stamp))
 source_note<-function(){
  if(!input$tabs%in%c('yourteam','today','players_in_championship'))return('')
  at<-team_received_offers_state(state())$observed_at
  incoming_clause_source_times(list(roster=at,transactions=at))
 }
 # Mount the actual shared note outputs without account settings/polling.
 include<-FALSE
 for(expr in as.list(body(fm_worker_session))[-1L]){
  lhs<-if(is.call(expr)&&identical(expr[[1]],as.name('<-')))paste(deparse(expr[[2]]),collapse='')else ''
  if(lhs=='status_visible')include<-TRUE
  if(lhs=='output$worker_settings')break
  if(include)eval(expr,envir=environment())
 }
 panel<-resource_display_source(session,function()team_received_offers_panel(team_received_offers_state(state()),identity,
  source_note=shiny::textOutput('offer_stamp',inline=TRUE)))
 output$offers<-shiny::renderUI(panel());output$offer_stamp<-shiny::renderText(paste('Observed:',fm_format_datetime(team_received_offers_state(state())$observed_at)))
 shiny::outputOptions(output,'offer_stamp',suspendWhenHidden=FALSE)
 offers<-attr(roster,'received_offers')
 opened_offer<-shiny::reactiveVal('')
 for(i in seq_len(nrow(offers)))local({
  row<-offers[i,,drop=FALSE]
  shiny::observeEvent(input[[team_received_offer_input(row)]],opened_offer(row$bid_id))
 })
 output$opened_offer<-shiny::renderText(opened_offer())
 shiny::observeEvent(input$advance,{next_roster<-roster;attr(next_roster,'received_offers_observed_at')<-source_stamp+600;state(resource_state(next_roster))})
 output$beats<-shiny::renderText(input$heartbeat %||% 0);output$calls<-shiny::renderText({input$heartbeat;io})
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=3892,launch.browser=FALSE)
