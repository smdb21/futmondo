# Actual worker descriptors, compressed persistence codec and page scripts.
# All records and RPCs are synthetic. Browsing source HTTP is forbidden.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured <- function() TRUE
fm_worker_can_connect <- function() FALSE
fixture_auth <- c(userid='plots-browser-worker',token='synthetic')
fixture_account <- list(enabled=TRUE,auth_status='active',generation=1L)
fixture_held <- TRUE
fixture_version <- 1L
fixture_reads <- 0L
fixture_source_calls <- 0L
fixture_payloads <- new.env(parent=emptyenv())
fixture_full_pressroom_reads <- 0L
futmondo_post <- function(...) {fixture_source_calls <<- fixture_source_calls+1L;stop('Unexpected source HTTP')}
fm_worker_rpc <- function(name,payload,owner='') {
 stopifnot(name=='read')
 fixture_reads <<- fixture_reads+1L
 s <- fm_worker_account_state(fixture_auth[['userid']])
 resources <- list(); jobs <- list()
 for(key in names(payload$p_versions)) {
  d <- s$descriptors[[key]]$descriptor
  if(is.null(d)) next
  if(d$handler=='get_championship_pressroom')fixture_full_pressroom_reads <<- fixture_full_pressroom_reads+1L
  held <- fixture_held && (d$handler=='get_championship_transfer_amounts' ||
    (d$handler=='get_user_team_rounds' && d$args$user_team_id=='t2'))
  jobs[[length(jobs)+1L]] <- list(key=key,pending=held,running=FALSE,error=NULL)
  if(held && d$handler=='get_championship_transfer_amounts')
   return(promises::promise(function(resolve,reject) {
    wait_for_release <- function() {
     if (fixture_held) later::later(wait_for_release,.05)
     else promises::then(fm_worker_rpc(name,payload,owner),resolve,reject)
    }
    later::later(wait_for_release,.05)
   }))
  if(held) next
  version <- as.character(fixture_version)
  if(identical(payload$p_versions[[key]],version)) next
  saved <- fixture_payloads[[key]]
  if (is.null(saved)) {
   value <- switch(d$handler,
   get_teams=data.frame(teamid=paste0('t',1:11),teamname=paste('Team',1:11)),
   get_classification_round_catalog=list(rounds=data.frame(round_id=paste0('r',1:14),round_number=1:14,
     is_finished=(1:14)!=8, partially_played=(1:14)==8, status='closed')),
   get_user_team_rounds=if(d$args$user_team_id=='t11') structure(list(),valid_response=TRUE) else lapply(1:14,function(n)
    list(id=paste0('r',n),number=n,status='closed',points=n*10+as.integer(sub('t','',d$args$user_team_id)))),
   get_classification_round_ranking={
    answer <- list(metadata=list(status=if(d$args$round_number%%2==0)'V' else 'P'),
     ranking=lapply(1:10,function(i)list(id=paste0('t',i),name=paste('Team',i),points=d$args$round_number*10+i)))
    list(round_id=d$args$round_id,round_number=d$args$round_number,answer=answer,
     rows=classification_ranking_rows(answer,d$args$round_id,d$args$round_number))
   },
   get_league_standings_history=data.frame(teamname=rep(paste('Team',1:10),each=14),
    recorded_at=rep(format(as.Date('2026-11-01')+0:13,'%Y-%m-%dT12:00:00Z'),10),team_value=seq_len(140)*1000),
   get_championship_players=data.frame(id=paste0('p',1:60),user_team_id='t1',
    teamId=rep(paste0('club',1:20),each=3),team=rep(paste('Club',1:20),each=3),logo='fixture-crest.png'),
   get_financial_snapshot=list(configuration=list()),get_championship_transfer_amounts=data.frame(
    created=c('2026-01-31T22:00:00Z','2026-01-31T23:00:00Z','2026-02-01T23:00:00Z'),
    price=c(100,200,300)),
   stop('Unexpected fixture handler: ',d$handler))
   wire <- charToRaw(fm_worker_json(fm_worker_encode(value)))
   compressed <- history_baseline_compress(wire)
   # The database returns already persisted chunks on subsequent reads.
   saved <- list(bytes=length(wire),sha256=paste0(openssl::sha256(compressed)),
    chunks=list(jsonlite::base64_enc(compressed)))
   fixture_payloads[[key]] <- saved
  }
  manifest <- list(account_id=fixture_auth[['userid']],resource_key=key,context=d$context,handler=d$handler,
   version=version,json_bytes=saved$bytes,parts=1L,sha256=saved$sha256,
   observed_at=rival_finance_stamp(Sys.time()-10),checked_at=rival_finance_stamp(),
   valid_until=rival_finance_stamp(Sys.time()+300),status='ok',completeness='complete')
  resource <- list(key=key,manifest=manifest,chunks=saved$chunks)
  resources[[length(resources)+1L]] <- resource
 }
 promises::promise(function(resolve,reject)later::later(function() resolve(list(
  account=fixture_account,health=NULL,resources=resources,jobs=jobs)),.2))
}
get_league_standings_history <- function(championship_id) data.frame(teamname=rep(paste('Team',1:10),each=14),recorded_at=rep(format(as.Date('2026-11-01')+0:13,'%Y-%m-%dT12:00:00Z'),10),team_value=seq_len(140)*1000)
ui <- shiny::fluidPage(shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),
 shiny::includeCSS('www/custom_style.css'),shiny::includeScript('www/theme-switch.js'),shiny::includeScript('www/info-disclosures.js')),
 fm_info_page_UI('Trends',plots_UI('plots')),
 shiny::actionButton('release','Release held history'),shiny::actionButton('publish','Publish same saved facts'),
 shiny::actionButton('pulse','Heartbeat'),shiny::textOutput('heartbeat'),shiny::textOutput('stats'))
server <- function(input,output,session) {
 clear_api_cache()
 if(exists(fixture_auth[['userid']],.fm_worker$accounts,inherits=FALSE))rm(list=fixture_auth[['userid']],envir=.fm_worker$accounts)
 session$onSessionEnded(function()fm_worker_detach(fixture_auth[['userid']],session$token))
 keys <- ls(.fm_worker$values)
 if(length(keys))rm(list=keys,envir=.fm_worker$values)
 fixture_held <<- TRUE;fixture_version <<- 1L;fixture_reads <<- 0L;fixture_full_pressroom_reads <<- 0L
 s <- fm_worker_account_state(fixture_auth[['userid']]);s$bootstrap_complete <- TRUE
 fm_worker_set_policy(s,fixture_account)
 auth <- reactive(fixture_auth)
 read_teams <- factual_read_adapter(session,'get_teams',auth,function()'league',function()'t1')
 teams <- reactive(read_teams(auth(),'league'))
 plots_Server('plots',reactive(TRUE),auth,reactive('league'),reactive('t1'),teams)
 poll_fixture <- function() {
  if (s$busy) later::later(poll_fixture, .05) else fm_worker_poll(fixture_auth[['userid']], session$token, TRUE)
 }
 observeEvent(input$release,{
  fixture_held <<- FALSE
 })
 observeEvent(input$publish,{
  fixture_version <<- fixture_version+1L
  poll_fixture()
 })
 output$heartbeat <- renderText(input$pulse %||% 0L)
 output$stats <- renderText({input$pulse;paste(fixture_reads,fixture_full_pressroom_reads,fixture_source_calls)})
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=8571,launch.browser=FALSE)
