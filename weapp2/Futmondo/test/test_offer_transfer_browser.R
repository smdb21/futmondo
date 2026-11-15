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
roster<-observed_data(data.frame(id='p',name='Offer player',value=1000,points=1),status='stale',observed_at=now-1200)
attr(roster,'worker_acquisition_status')<-'ok';attr(roster,'worker_completeness')<-'unknown'
attr(roster,'received_offers_status')<-'ok';attr(roster,'received_offers_observed_at')<-now-1200
attr(roster,'received_offers')<-normalize_received_offer(list(id='bid',price=500,userTeam=list(id='r',name='Rival')),'p')
ui<-shiny::fluidPage(shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),
 shiny::includeCSS('www/custom_style.css'),shiny::includeScript('www/info-disclosures.js')),
 players_in_teams_UI('styles'),shiny::div(id='incoming',incoming_clause_panel(list(status='observed',records=record,observations_only=TRUE),list(available=TRUE,round_number=13,starts_at=now+172800),identity)),
 shiny::div(id='outgoing',outgoing_clause_panel(list(status='observed',records=out,observations_only=TRUE),list(available=TRUE,round_number=13,starts_at=now+172800),identity)),
 shiny::uiOutput('offers'),shiny::actionButton('advance','Advance observed clock'),shiny::actionButton('heartbeat','Heartbeat'),shiny::textOutput('beats'),shiny::textOutput('calls'))
server<-function(input,output,session){
 state<-shiny::reactiveVal(resource_state(roster));panel<-resource_display_source(session,function()team_received_offers_panel(team_received_offers_state(state()),identity,
  source_note=shiny::textOutput('offer_stamp',inline=TRUE)))
 output$offers<-shiny::renderUI(panel());output$offer_stamp<-shiny::renderText(paste('Observed:',fm_format_datetime(team_received_offers_state(state())$observed_at)))
 shiny::observeEvent(input$advance,{next_roster<-roster;attr(next_roster,'received_offers_observed_at')<-source_stamp+600;state(resource_state(next_roster))})
 output$beats<-shiny::renderText(input$heartbeat %||% 0);output$calls<-shiny::renderText({input$heartbeat;io})
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=3892,launch.browser=FALSE)
