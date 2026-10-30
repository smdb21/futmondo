# Offline browser acceptance fixture: actual app with delayed synthetic reads.
expressions <- parse("test/test_application_offline.R")
for(expr in expressions) {
  if(is.call(expr) && identical(expr[[1]],as.name("app_check")) &&
     identical(expr[[2]],"full server login, navigation, refresh and logout")) break
  eval(expr,envir=.GlobalEnv)
}
fixture_calls <- new.env(parent=emptyenv())
fixture_delay <- function(value,seconds) promises::promise(function(resolve,reject) {
  later::later(function()resolve(value),seconds)
})
fm_async_login <- function(...)fixture_delay(app_auth,.8)
get_player_summary <- function(...)observed_data(list(championship=list(),
  points=list(list(round=1,points=0,isFinished=TRUE),
              list(round=2,points=-2)),prices=list()),observed_at=Sys.time())
get_player_matches <- function(...)list(matches=list(
  list(r=1,h=list(name="Club A",score=0),a=list(name="Club B",score=0),info=list(date="2026-08-15")),
  list(r=2,h=list(name="Club C with a deliberately long club name",score=2),a=list(name="Club A",score=1),info=list(date="2026-08-22"))))
get_player_full_profile <- function(...)list(profile=list(height=0,weight=0,nationality="Synthetic"),prices=list())
read_card_round_history <- function(...)structure(data.frame(),complete=TRUE)
read_card_price_history <- function(...)structure(data.frame(),complete=TRUE)
get_acquisition_capacity <- function(...)list(status="ok",roster=list(count=17,cap=20,status="ok"),
  outstanding=list(count=0),funds=list(spendable_budget=10000000))
fixture_original_async <- async_adapter_fetch
async_adapter_fetch <- function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE) {
  name<-if(is.character(fn))fn else deparse(substitute(fn))
  fixture_calls[[name]]<-(fixture_calls[[name]] %||% 0L)+1L
  delay<-switch(name,get_active_championships=.8,get_user_team_info=.6,get_market_players=1.2,
    get_player_summary=.6,get_lineup_from_team=.6,get_player_matches=2,get_player_full_profile=3,
    read_card_round_history=2,read_card_price_history=2,get_acquisition_capacity=3,
    get_financial_snapshot=4,get_championship_pressroom=4,.1)
  promises::then(fixture_delay(NULL,delay),function(unused)
    fixture_original_async(name,args,context,owner,is_current))
}
browser_ui <- source("ui.R",local=.GlobalEnv)$value
browser_server <- function(input,output,session) {
  clear_api_cache()
  app_server(input,output,session)
  shiny::observeEvent(input$fixture_snapshot,{
    session$sendCustomMessage("fixture-stats",list(reads=as.list(fixture_calls),timings=resource_timings(),http=offline_http_attempts))
  })
}
shiny::runApp(shiny::shinyApp(ui=browser_ui,server=browser_server),host="127.0.0.1",port=38766,launch.browser=FALSE)
