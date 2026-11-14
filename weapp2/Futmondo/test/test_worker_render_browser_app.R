# Actual Today cards and shared read adapters, with synthetic durable worker records.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
Sys.setenv(FUTMONDO_WORKER_KEY=paste(rep('ab',32),collapse=''))
fixture_auth<-c(userid='render-fixture',token='synthetic',user_name='fixture@example.invalid')
fixture_account<-list(account_id='render-fixture',enabled=TRUE,auth_status='active',generation=1L,
 timezone='UTC',quiet_start=0L,quiet_end=7L,excluded_leagues=list())
fixture_version<-1L;fixture_cash<-20000001;fixture_reads<-0L;fixture_enqueues<-0L;fixture_resources<-list()
fixture_stamp<-Sys.time()
fm_worker_rpc<-function(name,payload,owner=''){
 if(name=='renew')return(promises::promise_resolve(fixture_account))
 if(name=='enqueue'){fixture_enqueues<<-fixture_enqueues+1L;stop('GUI attempted acquisition')}
 stopifnot(name=='read');fixture_reads<<-fixture_reads+1L
 # Real acquisitions create new versions/source clocks even when facts match.
 fixture_version<<-fixture_version+1L;fixture_stamp<<-Sys.time()
 s<-fm_worker_account_state(fixture_auth[['userid']])
 resources<-list();jobs<-list()
 for(key in names(payload$p_versions)){
  descriptor<-s$descriptors[[key]]$descriptor
  if(is.null(descriptor))next
  version<-as.character(fixture_version)
  if(!identical(payload$p_versions[[key]],version)){
   value<-switch(descriptor$handler,
    get_user_team_info=list(budget=fixture_cash,withheld=0,teamValue=34000000,configuration=list(mnmp=.5)),
    get_market_players=data.frame(id=character(),value=numeric()),
    get_championship_pressroom=data.frame(id=character(),created=character(),player_id=character(),
     player_name=character(),buyer_team_id=character(),seller_team_id=character(),price=numeric()),
    stop('Unexpected fixture handler: ',descriptor$handler))
   wire<-charToRaw(fm_worker_json(fm_worker_encode(value)));compressed<-history_baseline_compress(wire)
   manifest<-list(account_id=fixture_auth[['userid']],resource_key=key,context=descriptor$context,handler=descriptor$handler,
    version=version,json_bytes=length(wire),parts=1L,sha256=paste0(openssl::sha256(compressed)),
    observed_at=rival_finance_stamp(fixture_stamp),checked_at=rival_finance_stamp(fixture_stamp),
    valid_until=rival_finance_stamp(fixture_stamp+3600),status='ok',completeness='complete')
   resources[[length(resources)+1L]]<-list(key=key,manifest=manifest,chunks=list(jsonlite::base64_enc(compressed)))
  }
  jobs[[length(jobs)+1L]]<-list(key=key,pending=FALSE,running=FALSE,error=NULL)
 }
 promises::promise_resolve(list(account=fixture_account,health=list(heartbeat=rival_finance_stamp()),resources=resources,jobs=jobs))
}
# The regression concerns primary cards and their real read dependencies; no table
# or finance acquisition is needed to measure these outputs.
players_table_UI<-function(...)NULL
players_table_Server<-function(...)NULL
ui<-shiny::fluidPage(shiny::tags$head(shiny::includeScript('www/worker-state.js')),
 shiny::selectInput('tabs','Page',choices=c(Today='today')),shiny::uiOutput('round_countdown'),
 shiny::uiOutput('worker_status'),today_UI('today'),
 shiny::actionButton('publish','Publish new balance'),shiny::actionButton('refresh_saved','Read saved data'),shiny::actionButton('heartbeat','Heartbeat'),
 shiny::textOutput('heartbeat_value'))
server<-function(input,output,session){
 clear_api_cache();fixture_version<<-1L;fixture_cash<<-20000001;fixture_stamp<<-Sys.time()
 s<-fm_worker_account_state(fixture_auth[['userid']]);fm_worker_set_policy(s,fixture_account)
 auth<-shiny::reactiveVal(fixture_auth)
 fm_worker_session(session,auth)
 login_token_RV<-auth
 team_read<-factual_read_adapter(session,'get_user_team_info',auth,function()'league',function()'team',state_only=TRUE)
 market_read<-factual_read_adapter(session,'get_market_players',auth,function()'league',function()'team',state_only=TRUE)
 primary_team_state<-shiny::reactive(team_read(auth(),'league','team'))
 primary_market_state<-shiny::reactive(market_read(auth(),'league','team'))
 today<-today_Server('today',function()TRUE,auth,function()'league',function()'team',function()data.frame(),
  primary_sources=list(team_state=primary_team_state,market_state=primary_market_state,retry_team=function()NULL,retry_market=function()NULL))
 round_catalog_RV<-shiny::reactive(data.frame(round_number=14,begin_process=format(Sys.time()+7*86400,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),is_finished=FALSE))
 # Execute the actual app header/readiness definitions, not a copied facsimile.
 wanted<-c('startup_primary_ready','round_finance_RV','round_finance_checked','output$round_finance_checked','round_heading_state','output$round_countdown')
 for(expr in as.list(parse('server.R')[[1]][[3]])[-1L])if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
  paste(deparse(expr[[2]]),collapse='')%in%wanted)eval(expr,envir=environment())
 shiny::observeEvent(input$publish,{
  fixture_cash<<-fixture_cash+1;fixture_stamp<<-Sys.time()
  fm_worker_poll(fixture_auth[['userid']],session$token,TRUE)
 })
 shiny::observeEvent(input$refresh_saved,fm_worker_invalidate_pattern(resource_context(fixture_auth,'league','team'),pattern='.*'))
 shiny::observeEvent(input$fixture_stats,session$sendCustomMessage('fixture-stats',list(enqueues=fixture_enqueues,reads=fixture_reads)))
 output$heartbeat_value<-shiny::renderText(input$heartbeat)
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=3898,launch.browser=FALSE)
