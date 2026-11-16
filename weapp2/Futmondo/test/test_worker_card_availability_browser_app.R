options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
get_sb_url<-function()'';get_sb_key<-function()''
fm_worker_configured<-function()FALSE
record_acquired_resource<-function(...)invisible(TRUE)
futmondo_post<-function(...)stop('Unexpected synchronous source request')
buy_clause<-function(...)stop('No real purchase is authorized')
fixture<-new.env(parent=emptyenv());fixture$offset<-0
Sys.time<-function()base::Sys.time()+fixture$offset
rows<-function(age=120,id='dani-fixture'){
 catalog<-observed_data(data.frame(id=id,name=if(id=='dani-fixture')'Dani fixture'else if(id=='system-fixture')'System fixture'else'Stale fixture',
 role='centrocampista',role2=NA_character_,team='Club',value=1000,change=0,points=-2,user_team_id='',computer=TRUE),observed_at=Sys.time()-age)
 market<-observed_data(data.frame(id=id,price=1000),observed_at=Sys.time())
 unify_columns(calculate_player_changes(translate_player_positions(championship_market_prices(catalog,resource_state(market)))))
}
async_adapter_fetch<-function(fn,args=list(),context,is_current=function()TRUE,...){
 fixture$reads<-c(fixture$reads,fn)
 if(fn=='get_player_summary')return(promises::promise(function(resolve,reject){
  fixture$summaries[[length(fixture$summaries)+1L]]<-list(resolve=resolve,reject=reject,id=context$player_id)
 }))
 if(fn=='get_team_acquisition_capacity')return(promises::promise(function(resolve,reject){
  fixture$finances[[length(fixture$finances)+1L]]<-list(resolve=resolve,reject=reject)
 }))
 if(fn%in%c('get_lineup_from_team','get_player_full_profile','get_player_matches','read_card_round_history','read_card_price_history'))
  return(promises::promise(function(resolve,reject)NULL))
 stop(paste('Unexpected fixture resource',fn))
}
ui<-shiny::fluidPage(shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),
 shiny::includeCSS('www/custom_style.css'),shiny::includeScript('www/info-disclosures.js')),
 shiny::actionButton('heartbeat','Heartbeat'),shiny::textOutput('heartbeat_value'),
 shiny::actionButton('finish_summary','Complete target source'),shiny::actionButton('finish_funds','Complete funds'),
 shiny::actionButton('old_table','Load old table'),shiny::actionButton('system_table','Load System player'),shiny::actionButton('advance','Advance time'),
 players_table_UI('table',box_title='Player fixture',filter_by_position=FALSE,filter_by_team=FALSE,
 filter_by_value=FALSE,filter_by_change_value=FALSE,filter_by_active_clause=FALSE,
 filter_by_is_favorite=FALSE,filter_by_is_from_futmondo=FALSE))
server<-function(input,output,session){
 clear_api_cache();fixture$reads<-character();fixture$summaries<-list();fixture$finances<-list();fixture$offset<-0
 data<-shiny::reactiveVal(rows())
 players_table_Server('table',data,shiny::reactive(NULL),shiny::reactive(c(userid=paste0('selection-browser-',session$token),token='synthetic')),
 shiny::reactive('league'),shiny::reactive('mine'),optional_ready=function()FALSE)
 output$heartbeat_value<-shiny::renderText(input$heartbeat)
 shiny::observeEvent(input$finish_summary,{
  job<-tail(fixture$summaries,1)[[1]]
  if(job$id=='system-fixture')job$resolve(observed_data(list(data=list(id=job$id,computer=TRUE),championship=list(pro=FALSE,clause=list()),
   market=list(pr=FALSE,p=123456,bids=list()),points=list(),prices=list())))else
  job$resolve(observed_data(list(data=list(id=job$id),championship=list(owner=list(id='rival',name='Verified owner',computer=FALSE)),
   market=list(bids=list()),points=list(),prices=list())))
 })
 shiny::observeEvent(input$finish_funds,{
  job<-tail(fixture$finances,1)[[1]]
  job$resolve(observed_data(list(status='ok',valid_until=as.numeric(Sys.time())+15,
   roster=list(status='ok',count=1,effective_count=1,cap=20),funds=list(spendable_budget=10000),
   outstanding=list(count=0),rules=list(minimum_bid_at_value=TRUE))))
 })
 shiny::observeEvent(input$old_table,data(rows(301,'stale-fixture')))
 shiny::observeEvent(input$system_table,data(rows(301,'system-fixture')))
 shiny::observeEvent(input$advance,fixture$offset<-fixture$offset+90)
 shiny::observeEvent(input$fixture_stats,session$sendCustomMessage('fixture-stats',list(
  financial=length(fixture$finances),summaries=length(fixture$summaries),reads=as.list(fixture$reads))))
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=as.integer(Sys.getenv('FUTMONDO_SELECTION_TEST_PORT','3914')),launch.browser=FALSE)
