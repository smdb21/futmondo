# Synthetic browser app for test_classification_lineup_browser.cjs.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
bind_factual_reads <- function(...)invisible(NULL)
shared_mvp_source <- function(...)reactive(list())
shared_rival_finance_source <- function(...)reactive(list(loading=FALSE))
get_classification_round_catalog <- function(...)list(rounds=data.frame(round_id=c(paste0('round-',1:8),'round-nine','round-ten','round-eleven','future-round'),round_number=1:12,is_finished=c(rep(TRUE,11),FALSE)))
get_finished_rounds <- function(...)get_classification_round_catalog(...)$rounds
get_user_team_rounds <- function(login,championship_id,user_team_id)list(list(round=9,points=72),list(round=11,points=if(user_team_id=='own')105 else 90))
get_user_team_info <- function(...)list(configuration=list())
get_round_dreamteam <- function(...)list()
fixture_reads <- 0L
get_round_lineup_snapshot <- function(login,championship_id,user_team_id,round_id){
  fixture_reads <<- fixture_reads+1L
  slots <- setdiff(0:10,4)
  players <- lapply(slots,function(i)list(id=paste(user_team_id,i,sep='-'),position=i,name=paste(if(user_team_id=='own')'Own'else'Rival',i),points=if(i==2)0 else i,changed=i==1))
  bench <- lapply(c(3,0,2),function(i)list(id=paste0('bench-',i),position=i,name=paste('Reserve',i),points=i,changed=i==2))
  list(round_id=round_id,answer=list(strategy=if(round_id=='round-eleven')'3-6-1'else'4-3-3',players=players,bench=list(players=bench),status='done'))
}
ui <- shinydashboard::dashboardPage(shinydashboard::dashboardHeader(title='Lineup fixture'),shinydashboard::dashboardSidebar(disable=TRUE),
  shinydashboard::dashboardBody(tags$head(tags$style(HTML(fm_theme_css())),includeCSS('www/custom_style.css')),
    textOutput('requests'),classification_UI('classification')))
server <- function(input,output,session){
  classification_Server('classification',reactive(TRUE),reactive(c(userid='fixture-account',token='synthetic')),reactive('fixture-league'),reactive('own'),
    reactive(data.frame(teamid=c('own','rival'),teamname=c('Your Team','Rival Team'),points=c(177,162))))
  output$requests <- renderText({invalidateLater(250);paste('Lineup reads:',fixture_reads)})
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=as.integer(Sys.getenv('FUTMONDO_LINEUP_TEST_PORT','8501')),launch.browser=FALSE)
