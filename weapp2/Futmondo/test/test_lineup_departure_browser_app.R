# Real Lineup, root prerequisite wiring and information panel; synthetic DB only.
for(expr in parse('test/test_lineup_error_browser.R')){
 if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&identical(expr[[2]],as.name('ui')))break
 eval(expr,.GlobalEnv)
}
fm_worker_configured<-function()TRUE
counts$worker_reads<-0L;counts$blocked_io<-0L
worker_read_revision<-shiny::reactiveVal(0L)
blocked<-function(...){counts$blocked_io<-counts$blocked_io+1L;stop('Unexpected source I/O')}
for(name in c('futmondo_post','supabase_get','supabase_upsert'))assign(name,blocked)
saved_resources<-lapply(c('get_players_from_team','get_championship_pressroom'),function(handler){
 args<-if(handler=='get_players_from_team')list(championship_id='league',user_team_id='team',teams=NULL)else list(championship_id='league')
 descriptor<-fm_worker_descriptor(handler,args,ctx)
 data<-if(handler=='get_players_from_team')data.frame(id=paste0('p',1:24),clause_transferred=FALSE)else
  data.frame(player_id=character(),buyer_team_id=character(),seller_team_id=character(),created=character(),is_clause=logical())
 data<-observed_data(data,observed_at=stamp)
 wire<-charToRaw(fm_worker_json(fm_worker_encode(data)));compressed<-history_baseline_compress(wire)
 list(key=descriptor$resource_key,manifest=list(account_id=ctx$account_id,resource_key=descriptor$resource_key,
  handler=handler,context=descriptor$context,version=paste0('saved-',handler),json_bytes=length(wire),parts=1L,
  sha256=paste0(openssl::sha256(compressed)),observed_at=rival_finance_stamp(stamp),
  checked_at=rival_finance_stamp(stamp+1),valid_until=rival_finance_stamp(stamp+60),
  status=if(nrow(data))'ok'else'empty',completeness='complete'),chunks=list(jsonlite::base64_enc(compressed)))
})
fm_worker_rpc<-function(name,payload,owner=''){
 stopifnot(name=='read');counts$worker_reads<-counts$worker_reads+1L
 worker_read_revision(shiny::isolate(worker_read_revision())+1L)
 promises::promise_resolve(list(account=list(enabled=TRUE,auth_status='active'),health=NULL,jobs=list(),
  resources=Filter(function(x)x$key%in%names(payload$p_descriptors),saved_resources)))
}
root_exprs<-as.list(parse('server.R')[[1]][[3]])[-1L]
root_names<-c('clause_transfer_revision','roster_note_active','roster_sources_active','roster_source_readers',
 'roster_source_note','clause_transfers_RV','refresh_clause_transfers')
ui<-shiny::fluidPage(shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),
 shiny::includeCSS('www/custom_style.css'),shiny::includeScript('www/info-disclosures.js'),
 shiny::tags$style('#diagnostics{overflow-wrap:anywhere}')),
 shiny::actionButton('publish','Read saved observations'),shiny::actionButton('heartbeat','Heartbeat'),
 shiny::textOutput('heartbeat_value'),shiny::textOutput('diagnostics'),
 shiny::tabsetPanel(id='tabs',selected='today',
  shiny::tabPanel('Today',value='today',fm_info_page_UI('Today',shiny::p('Saved observations are available.'))),
  shiny::tabPanel('My Team',value='yourteam',fm_info_page_UI('My Team',shiny::uiOutput('departing'))),
  shiny::tabPanel('Lineup',value='lineup',fm_info_page_UI('Lineup',lineup_UI('lineup')))))
server<-function(input,output,session){
 # A new browser session begins with a cold memory cache and restores DB data.
 keys<-ls(.fm_worker$values);if(length(keys))rm(list=keys,envir=.fm_worker$values)
 state<-fm_worker_account_state(auth[['userid']]);state$known<-TRUE
 state$account<-list(enabled=TRUE,auth_status='active');state$descriptors<-list()
 login_token_RV<-shiny::reactiveVal(auth);championship_id_RV<-shiny::reactiveVal('league')
 user_team_id_RV<-function()'team';refresh_trigger<-shiny::reactiveVal(0L)
 for(expr in root_exprs)if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
  paste(deparse(expr[[2]]),collapse='')%in%root_names)eval(expr,envir=environment())
 session$onFlushed(function()fm_worker_poll(auth[['userid']],session$token,TRUE),once=TRUE)
 shiny::observeEvent(input$publish,fm_worker_poll(auth[['userid']],session$token,TRUE))
 output$departing<-shiny::renderUI({shiny::req(input$tabs=='yourteam');s<-clause_transfers_RV()
  outgoing_clause_panel(list(status=s$outgoing_status,records=s$outgoing_records,error=s$outgoing_error %||% s$error),
   next_round_context(calendar),session$ns)})
 lineup_Server('lineup',shiny::reactive(input$tabs=='lineup'),login_token_RV,championship_id_RV,user_team_id_RV,
  shiny::reactive(calendar),clause_transfers_source=clause_transfers_RV,clause_transfers_refresh=refresh_clause_transfers,
  round_catalog_state=shiny::reactive(list(data=calendar,context=ctx,status='stale',freshness='stale',error=NULL)))
 output$heartbeat_value<-shiny::renderText(input$heartbeat %||% 0L)
 output$diagnostics<-shiny::renderText({worker_read_revision();s<-clause_transfers_RV()
  jsonlite::toJSON(c(as.list(counts),list(complete=clause_departure_round_state(s,lineup_target(calendar))$complete,
   outgoing=s$outgoing_status,observed=as.numeric(s$source_times$roster %||% NA_real_))),auto_unbox=TRUE,na='null')})
 shiny::outputOptions(output,'diagnostics',suspendWhenHidden=FALSE)
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',
 port=as.integer(Sys.getenv('FUTMONDO_DEPARTURE_TEST_PORT','3911')),launch.browser=FALSE)
