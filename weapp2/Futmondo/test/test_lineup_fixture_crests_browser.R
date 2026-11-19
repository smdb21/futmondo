# Synthetic rendered-UI boundary; the actual worker adapter/codec is covered by
# test_lineup_fixture_crests.R. No source accounts or external API requests.
for(expr in parse('test/test_lineup_browser.R')){
 if(is.call(expr)&&identical(expr[[1]],quote(shiny::runApp)))next
 eval(expr,.GlobalEnv)
}
attr(fixture_rounds,'fixture_calendar')$matches$rows$home_logo<-''
attr(fixture_rounds,'fixture_calendar')$matches$rows$away_logo<-''
fixture_clubs<-data.frame(teamId=c(paste0('home-',1:10),paste0('away-',1:10)),
 logo=rep(c('fixture-home.png','fixture-away.png'),each=10))
attr(fixture_clubs,'observed_at')<-Sys.time()-90
attr(fixture_clubs,'valid_until')<-as.numeric(Sys.time())+900
attr(fixture_clubs,'worker_acquisition_status')<-'ok'
fixture_scope<-attr(fixture_rounds,'fixture_calendar')$scope
factual_read_adapter<-function(session,handler,...){
 stopifnot(handler=='get_real_clubs')
 session$userData$fixture_clubs<-shiny::reactiveVal(list(data=NULL,status='loading',context=fixture_scope))
 function(...)session$userData$fixture_clubs()
}
base_server<-server
server<-function(input,output,session){
 base_server(input,output,session)
 shiny::observeEvent(input$publish_clubs,{
  session$userData$fixture_clubs(list(data=fixture_clubs,status='ok',context=fixture_scope))
 })
}
ui<-shiny::fluidPage(shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),
 shiny::includeCSS('www/custom_style.css'),shiny::includeScript('www/info-disclosures.js')),
 shiny::textOutput('fixture_counts'),fm_info_page_UI('Lineup',lineup_UI('lineup')))
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',
 port=as.integer(Sys.getenv('FUTMONDO_EDITOR_TEST_PORT','8558')),launch.browser=FALSE)
