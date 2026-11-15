#!/usr/bin/env Rscript
# Real shared player table with synthetic rows; no credentials or external I/O.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
blocked_io<-function(...)stop('Unexpected external I/O in sticky-table fixture')
for(name in c('futmondo_post','fm_async_http','fm_worker_rpc','supabase_read','supabase_upsert','supabase_write'))assign(name,blocked_io)
selected_player_Server<-function(...)NULL
recent_player_points_source<-function(...)list(history=function()data.frame(),status=function()NULL)
rows<-data.frame(id=paste0('p',1:12),name=paste('Synthetic player',1:12),role='Defender / Midfielder',
 team='Athletic Club',logo='athletic',value=2e6,points=1:12,change=10000,
 average.fitness='2,5,12,12,6',average.averageLastFive=7.4,fav=FALSE,
 price=3e6,clause_price=4e6,rating=7,played=12,userTeam='Fixture team')
ui<-shinydashboard::dashboardPage(shinydashboard::dashboardHeader(title='Table fixture'),
 shinydashboard::dashboardSidebar(disable=TRUE),shinydashboard::dashboardBody(
 shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),shiny::includeCSS('www/custom_style.css')),
 shiny::actionButton('change_rows','Update rows'),shiny::actionButton('heartbeat','Heartbeat'),
 shiny::textOutput('heartbeat_value'),players_table_UI('fixture',filter_by_own_team=FALSE)))
server<-function(input,output,session){
 data<-shiny::reactiveVal(rows)
 players_table_Server('fixture',players_table_RV=data,user_teams_RV=shiny::reactive(NULL),
  own_squad=TRUE,departing_player_ids=shiny::reactive('p1'),points_ratio_basis='value')
 shiny::observeEvent(input$change_rows,{updated<-rows;updated$points<-updated$points+1;data(updated)})
 output$heartbeat_value<-shiny::renderText(input$heartbeat %||% 0L)
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',
 port=as.integer(Sys.getenv('FUTMONDO_STICKY_TEST_PORT','3917')),launch.browser=FALSE)
