# Synthetic source/database boundaries for the actual player-card UI.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured <- function() TRUE
fm_worker_mode <- function(id) TRUE
record_acquired_resource <- function(...) invisible(TRUE)
fm_worker_rpc <- function(...) stop('Unexpected database request or acquisition enqueue')
fixture <- new.env(parent=emptyenv())
fixture_auth <- c(userid='availability-browser-fixture',token='synthetic')
fixture_capacity <- function(valid=15,status='ok') observed_data(list(status=status,
 valid_until=as.numeric(Sys.time())+valid,
 roster=list(status='ok',count=10,effective_count=10,cap=20),
 funds=list(spendable_budget=10000000),outstanding=list(count=0),
 rules=list(minimum_bid_at_value=FALSE),diagnostics=if(status=='partial')'reported budget unavailable' else character()))
async_adapter_fetch <- function(fn,args=list(),context,worker_bypass=FALSE,is_current=function()TRUE,...) {
 fixture$reads <- c(fixture$reads,fn)
 if(fn=='get_team_acquisition_capacity') {
  if(worker_bypass) {
   fixture$live <- fixture$live+1L
   return(promises::promise(function(resolve,reject) fixture$pending <- list(resolve=resolve,reject=reject,current=is_current)))
  }
  if(isTRUE(fixture$passive_hold))return(promises::promise(function(resolve,reject)fixture$pending_passive <- list(resolve=resolve,reject=reject)))
  return(promises::promise_resolve(observed_data(fixture_capacity(-30),status='stale')))
 }
 if(fn=='get_player_summary'&&isTRUE(fixture$summary_hold)){
  stopifnot(worker_bypass)
  fixture$summary_live<-fixture$summary_live+1L
  return(promises::promise(function(resolve,reject)fixture$summary_pending<-list(resolve=resolve,player_id=context$player_id)))
 }
 value <- switch(fn,get_player_summary=list(championship=list(),points=list(),prices=list()),
  get_lineup_from_team=list(pointSystem='fixture'),get_player_full_profile=list(profile=list()),
  get_player_matches=list(matches=list()),read_card_round_history=data.frame(),read_card_price_history=data.frame(),stop(fn))
 promises::promise_resolve(value)
}
buy_clause <- function(...) stop('No transaction is authorized by this fixture')
ui <- shiny::fluidPage(shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),
 shiny::includeCSS('www/custom_style.css'),shiny::includeScript('www/info-disclosures.js')),
 shiny::fluidRow(shiny::column(12,shiny::actionButton('heartbeat','Heartbeat'),shiny::textOutput('heartbeat_value'),
 shiny::actionButton('finish_success','Complete live check'),shiny::actionButton('finish_failure','Fail live check'),
 shiny::actionButton('finish_partial','Partial live check'),shiny::actionButton('reset_card','Refresh card'),
 shiny::actionButton('next_card','Next player'),shiny::actionButton('hold_next_summary','Hold next player summary'),
 shiny::actionButton('finish_summary','Complete player summary'),
 shiny::actionButton('hold_saved','Hold saved capacity'),
 shiny::textOutput('fixture_status'))),selected_player_UI('card'))
server <- function(input,output,session) {
 clear_api_cache();fixture$reads <- character();fixture$live <- 0L;fixture$pending <- NULL
 fixture$passive_hold <- FALSE;fixture$pending_passive <- NULL;fixture$summary_hold<-FALSE;fixture$summary_pending<-NULL;fixture$summary_live<-0L;refresh <- shiny::reactiveVal(0L)
 auth <- fixture_auth;auth[['userid']] <- paste0('availability-browser-',session$token)
 session$onSessionEnded(function()fm_worker_detach(auth[['userid']],session$token))
 selected <- shiny::reactiveVal(data.frame(id='p',name='Óskarsson',role='delantero',team='Real Sociedad',
  value=3132405,price=3132405,change=217886,points=49,last_points=35,user_team_id='',market_inMarket=TRUE))
 selected_player_Server('card',selected,login_token=shiny::reactive(auth),championship_id=shiny::reactive('league'),
  user_team_id=shiny::reactive('mine'),refresh_trigger=refresh,capacity_fetcher=function(...)stop('Unexpected synchronous preflight'))
 output$heartbeat_value <- shiny::renderText(input$heartbeat)
 output$fixture_status <- shiny::renderText({input$heartbeat;paste('Live checks:',fixture$live)})
 shiny::observeEvent(input$finish_success,{stopifnot(!is.null(fixture$pending));fixture$pending$resolve(fixture_capacity());fixture$pending <- NULL})
 shiny::observeEvent(input$finish_failure,{stopifnot(!is.null(fixture$pending));fixture$pending$reject(resource_failure(resource='get_team_acquisition_capacity',category='network'));fixture$pending <- NULL})
 shiny::observeEvent(input$finish_partial,{stopifnot(!is.null(fixture$pending));fixture$pending$resolve(fixture_capacity(status='partial'));fixture$pending <- NULL})
 shiny::observeEvent(input$reset_card,{next_player <- selected();next_player$id <- paste0('reset',input$reset_card);selected(next_player);refresh(refresh()+1L)})
 shiny::observeEvent(input$next_card,{next_player<-selected();next_player$id<-paste0('next',input$next_card);selected(next_player)})
 shiny::observeEvent(input$hold_next_summary,{fixture$summary_hold<-TRUE;next_player<-selected();next_player$id<-paste0('summary',input$hold_next_summary);selected(next_player)})
 shiny::observeEvent(input$finish_summary,{stopifnot(!is.null(fixture$summary_pending));fixture$summary_hold<-FALSE
  fixture$summary_pending$resolve(observed_data(list(data=list(id=fixture$summary_pending$player_id),championship=list(),points=list(),prices=list())))
  fixture$summary_pending<-NULL})
 shiny::observeEvent(input$hold_saved,{fixture$passive_hold <- TRUE;refresh(refresh()+1L)})
 shiny::observeEvent(input$fixture_stats,session$sendCustomMessage('fixture-stats',list(live=fixture$live,reads=length(fixture$reads),passive_pending=!is.null(fixture$pending_passive),summary_live=fixture$summary_live,summary_pending=!is.null(fixture$summary_pending))))
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=3907,launch.browser=FALSE)
