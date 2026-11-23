#!/usr/bin/env Rscript
# Offline shared controls in both layouts; external I/O is a test failure.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
blocked_io<-function(...)stop('Unexpected external I/O in position-filter fixture')
for(name in c('futmondo_post','fm_async_http','fm_worker_rpc','supabase_read','supabase_upsert','supabase_write'))assign(name,blocked_io)
selected_player_Server<-function(...)NULL
recent_player_points_source<-function(...)list(history=function()data.frame(),status=function()NULL)
rows<-data.frame(id=c('gk','def','mid','fwd','dual','unknown'),name=c('Keeper','Defender','Midfielder','Forward','Dual','Unknown'),
 role=c('Goalkeeper','Defender','Midfielder','Forward','Midfielder',NA),role2=c('',NA,'Defender','','Forward',NA),value=1e6,
 clause_price=c(120e6,135e6,328e6,135.5e6,0,NA))
ui<-shinydashboard::dashboardPage(shinydashboard::dashboardHeader(title='Position filters'),
 shinydashboard::dashboardSidebar(disable=TRUE),shinydashboard::dashboardBody(
 shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),shiny::includeCSS('www/custom_style.css')),
 shiny::actionButton('heartbeat','Heartbeat'),shiny::textOutput('heartbeat_value'),
 players_table_UI('plain',box_title='Standard layout',filter_by_max_clause_value=TRUE),
 players_table_UI('squad',box_title='Squad layout',show_position_breakdown=TRUE)))
server<-function(input,output,session){
 for(id in c('plain','squad'))players_table_Server(id,players_table_RV=function()rows,
  user_teams_RV=function()NULL,optional_ready=function()FALSE)
 output$heartbeat_value<-shiny::renderText(input$heartbeat %||% 0L)
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',
 port=as.integer(Sys.getenv('FUTMONDO_POSITION_TEST_PORT','3933')),launch.browser=FALSE)
