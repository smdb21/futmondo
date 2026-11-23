# Synthetic saved observations only; no authenticated requests or database writes.
options(futmondo.offline=TRUE)
source('global.R')
bind_factual_reads <- function(...) invisible(NULL)
counts <- new.env(parent=emptyenv())
read_fixture <- function(key, value) { counts[[key]] <- (counts[[key]] %||% 0L)+1L; value }
get_classification_round_catalog <- function(...) read_fixture('catalog',list(rounds=data.frame(round_id=c('r6','r62','r7'),round_number=c(6,6.2,7))))
get_user_team_rounds <- function(login,championship_id,user_team_id) read_fixture(paste0('rounds_',user_team_id),list(rounds=list(
  list(id='r6',points=if(user_team_id=='a')10 else 8),list(id='r62',points=5),list(id='r7',points=-3))))
get_league_standings_history <- function(...) read_fixture('history',data.frame(teamname=rep(c('Alpha','Beta'),each=3),
 recorded_at=rep(c('2026-11-12T12:00:00Z','2026-11-13T12:00:00Z','2026-11-14T12:00:00Z'),2),
 points=c(10,20,18,9,15,14),team_value=c(100,105,103,95,100,110)))
get_championship_players <- function(...) read_fixture('players',data.frame(id=paste0('p',1:6),user_team_id='a',
 teamId=rep(c('x','y','z'),c(3,2,1)),team=rep(c('Athletic Club','Real Sociedad','Real Madrid'),c(3,2,1)),logo=rep('fixture-crest.png',6)))
get_financial_snapshot <- function(...) read_fixture('finances',list(configuration=list()))
get_championship_pressroom <- function(...) read_fixture('pressroom',data.frame(id='transaction'))
rivals_buying_power_values <- function(...) data.frame(team=c('Alpha','Beta'),value=c(1e6,2e6))
ui <- shiny::fluidPage(shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),
 shiny::includeCSS('www/custom_style.css'), shiny::includeScript('www/theme-switch.js')),
 plots_UI('plots'), shiny::actionButton('measure','Measure saved reads'),shiny::textOutput('counts'))
server <- function(input,output,session) {
 plots_Server('plots',function()TRUE,function()list(token='fixture',userid='fixture'),function()'league',function()'a',
  function()data.frame(teamid=c('a','b'),teamname=c('Alpha','Beta')))
 output$counts <- shiny::renderText({input$measure;paste(names(counts),unlist(as.list(counts)),collapse=';')})
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=8569,launch.browser=FALSE)
