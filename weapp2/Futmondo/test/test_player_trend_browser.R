# Render the actual card plot expression with synthetic history and no source I/O.
source('test/test_player_trend_plot.R')
for(name in c('futmondo_post','fm_async_http','supabase_get','supabase_post'))
 assign(name,function(...)stop('Unexpected external I/O in chart fixture'))
fixture<-list(status='ok',freshness='fresh',
 valuation=data.frame(recorded_at=format(as.POSIXct('2026-11-01',tz='UTC')+0:123*86400,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),value=99e6+sin(0:123/12)*1e6),
 points=data.frame(round_number=1:4,points=c(4,0,-2,3),date=as.POSIXct('2026-11-01',tz='UTC')+0:3*7*86400,score_status=c('final','final','final','provisional')))
ui<-shiny::fluidPage(shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),
 shiny::includeScript('www/theme-switch.js')),
 shiny::div(style='max-width:800px;margin:auto;padding:16px;',
 shiny::selectInput('scenario','History',choices=c('both','value','points','undated')),
 shiny::actionButton('heartbeat','Heartbeat'),shiny::textOutput('heartbeat_value'),
 plotly::plotlyOutput('chart',height='340px'),plotly::plotlyOutput('neutral',height='220px')))
server<-function(input,output,session){
 output$chart<-plotly::renderPlotly({
  history<-fixture
  if(input$scenario=='value')history$points<-data.frame()
  if(input$scenario=='points')history$valuation<-data.frame()
  if(input$scenario=='undated')history$points$date<-as.POSIXct(rep(NA_character_,4),tz='UTC')
  env<-new.env(parent=globalenv())
  env$selected_player<-function()data.frame(id='fixture',points=5,value=99e6)
  env$card_history<-function()history;env$card_chart_ready<-function()TRUE
  env$chart_data<-function()list(history=history,matches=normalize_player_matches(list()))
  env$card_states<-list(matches=function()list(data=list()))
  eval(plot_body,env)
 })
 output$neutral<-plotly::renderPlotly(fm_plot_layout(plotly::plot_ly(x=1:4,y=1:4,type='scatter',mode='lines'),
   xaxis=list(title='Round'),yaxis=list(title='Neutral axis')))
 output$heartbeat_value<-shiny::renderText(input$heartbeat %||% 0L)
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=3921,launch.browser=FALSE)
