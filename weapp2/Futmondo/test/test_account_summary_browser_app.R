# Production root UI with synthetic account data and blocked HTTP/persistence.
expressions <- parse('test/test_application_offline.R')
for(expr in expressions) {
  if(is.call(expr) && identical(expr[[1]],as.name('app_check')) &&
     identical(expr[[2]],'full server login, navigation, refresh and logout')) break
  eval(expr,envir=.GlobalEnv)
}
fm_page_store <- function(user_id,page=NULL,...) promises::promise_resolve(if(is.null(page))NULL else TRUE)
fixture_reads <- character(); fixture_mutations <- 0L
original_async <- async_adapter_fetch
async_adapter_fetch <- function(fn,...) {
  fixture_reads <<- c(fixture_reads,fn)
  original_async(fn,...)
}
fm_async_hire_coach <- function(...) {fixture_mutations <<- fixture_mutations+1L;stop('Unexpected coach hire')}
get_user_team_info <- function(...) list(budget=10000000,withheld=200000,maxBid=15000000,
  teamValue=34000000,staff=list(coach=1),configuration=list(mnmp=.5,maxPlayersInRoster=18,enableCoach='automatic'))
app_market$bid_id <- c('own-offer',NA_character_,NA_character_)
app_market$bid_price <- c(200000,NA_real_,NA_real_)
app_rounds <- data.frame(round_id=c('closed','next'),round_number=11:12,
  begin_process=format(Sys.time()+c(-7,1)*86400,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),
  status=c('closed','next'),is_finished=c(TRUE,FALSE))
app_roster$change <- c(30000, 20000, -10000, rep(0, 14))
fixture_server <- function(input,output,session) {
  app_server(input,output,session)
  observeEvent(input$fixture_stats,session$sendCustomMessage('fixture-stats',list(
    reads=fixture_reads,mutations=fixture_mutations,http=offline_http_attempts)))
}
shiny::runApp(shiny::shinyApp(ui=source('ui.R')$value,server=fixture_server),
 host='127.0.0.1',port=as.integer(Sys.getenv('FUTMONDO_SUMMARY_TEST_PORT','3892')),launch.browser=FALSE)
