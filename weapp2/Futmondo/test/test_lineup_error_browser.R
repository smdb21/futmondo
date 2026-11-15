#!/usr/bin/env Rscript
# Synthetic real-module replay of the two reported Lineup errors. No live I/O.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
if(identical(Sys.getenv('FUTMONDO_LINEUP_BASELINE'),'true')){
 baseline<-Sys.getenv('FUTMONDO_LINEUP_BASELINE_ROOT','/tmp/futmondo-lineup-errors-baseline')
 source(file.path(baseline,'lineup_api.R'))
 for(e in parse(file.path(baseline,'worker_runtime.R')))
  if(is.call(e)&&identical(e[[1]],as.name('<-'))&&identical(e[[2]],as.name('fm_worker_restore')))eval(e,.GlobalEnv)
}
auth<-c(userid='browser-lineup-account',token='synthetic')
custom<-list(media=list(as=1),ps=list(media=1,stats=1),stats=list(goal=5))
active<-list(championships=list(list(id='league',league='competition',ps='custom',custom=custom)),
 leagues=list(list(`_id`='competition',season='season')))
publish_active_league_contexts(auth,active)
ctx<-resource_context(auth,'league','team')
roster<-lapply(seq_len(24),function(i)list(id=paste0('p',i),name=paste('Player',i),
 role=c(rep('portero',2),rep('defensa',6),rep('centrocampista',8),rep('delantero',8))[i],
 role2='',status='ok',points=if(i<=2)-3+i else i*10,value=2000000,buyPrice=1000000,
 team=list(name='Fixture Club',logo='')))
starters<-c(17,18,9,10,11,12,3,4,5,6,1)
source_player<-function(i,position){row<-roster[[i]];row$position<-as.integer(position);row}
answers<-list(
 lineup=list(strategy='4-4-2',pointSystem='',custom=custom,
  players=lapply(seq_along(starters),function(i)source_player(starters[i],i-1L)),
  bench=list(enabled=TRUE,players=lapply(seq_along(c(7,13,14,19)),function(i)source_player(c(7,13,14,19)[i],i-1L))),multiposition=FALSE),
 roster=roster,info=list(league=list(id='competition',season='season'),configuration=list(cpt=FALSE)),
 strategies=list(strategies=lapply(lineup_formations(),function(x)list(st=x,pro=FALSE))))
now<-Sys.time();stamp<-now-120
# Keep the four browser variants independent of runner speed while retaining
# an unchanged synthetic observation clock across every durable reload.
answers<-lapply(answers,function(answer){
 answer<-observed_data(answer,observed_at=now)
 attr(answer,'valid_until')<-as.numeric(now)+3600
 attr(answer,'worker_replay')<-TRUE;attr(answer,'worker_acquisition_status')<-'ok'
 attr(answer,'worker_completeness')<-'complete';answer
})
rounds<-data.frame(round_id=paste0('r',1:13),round_number=as.numeric(1:13),
 is_finished=c(rep(TRUE,12),FALSE),begin_process=format(now+c(-12:-1*86400,86400),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))
attr(rounds,'catalog_complete')<-TRUE;attr(rounds,'catalog_status')<-'ok';attr(rounds,'catalog_reason')<-''
wire<-charToRaw(fm_worker_json(fm_worker_encode(rounds)));compressed<-history_baseline_compress(wire)
d<-fm_worker_descriptor('get_finished_rounds',list(championship_id='league'),ctx)
saved_calendar<-list(key=d$resource_key,manifest=list(account_id=auth[['userid']],resource_key=d$resource_key,
 handler='get_finished_rounds',context=d$context,json_bytes=length(wire),parts=1L,
 sha256=paste0(openssl::sha256(compressed)),observed_at=rival_finance_stamp(stamp),
 checked_at=rival_finance_stamp(stamp+1),valid_until=rival_finance_stamp(stamp+60),status='ok',completeness='unknown'),
 chunks=list(jsonlite::base64_enc(compressed)))
calendar<-fm_worker_restore(saved_calendar,auth[['userid']])$data
history<-expand.grid(player_id=paste0('p',1:24),round=1:12,stringsAsFactors=FALSE)
history$points<-as.numeric(sub('p','',history$player_id));history$points[history$player_id=='p1']<--2
history$points[history$player_id=='p2']<--1
counts<-new.env(parent=emptyenv());counts$source<-counts$db_read<-counts$db_write<-counts$submit<-0L
stored<-NULL
get_sb_url<-function()'https://fixture.invalid';get_sb_key<-function()'synthetic-key'
persist_league_context<-function(...)invisible(TRUE)
lineup_api_read<-function(route,...){counts$source<-counts$source+1L;promises::promise_resolve(answers[[switch(route,
 '/1/userteam/lineup'='lineup','/1/userteam/roster'='roster','/1/userteam/information'='info','/5/strategy/availables'='strategies')]])}
fm_async_read_http<-function(url,...){stopifnot(grepl('team_lineup_snapshots',url,fixed=TRUE));counts$db_read<-counts$db_read+1L
 promises::promise_resolve(list(content=charToRaw(jsonlite::toJSON(if(is.null(stored))list()else list(stored),auto_unbox=TRUE,null='null'))))}
fm_async_http<-function(url,method,body,...){stopifnot(method=='POST',grepl('team_lineup_snapshots',url,fixed=TRUE));counts$db_write<-counts$db_write+1L
 stored<<-jsonlite::fromJSON(body,simplifyVector=FALSE);promises::promise_resolve(list(status_code=201L))}
lineup_submit<-function(...){counts$submit<-counts$submit+1L;stop('No transactions allowed in browser fixture')}
recent_player_points_source<-function(session,...){
 root<-session$rootScope();root$userData$shared_round_points<-list(entry=shiny::reactive(list(context=ctx,season=ctx$season,scoring=ctx$scoring_version)))
 list(history=shiny::reactive(history),status=shiny::reactive(list(pending=0L,remaining=0L,failed=0L,saving=0L,save_error=NULL)))}
ui<-shiny::fluidPage(shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),
 shiny::includeCSS('www/custom_style.css'),shiny::includeScript('www/info-disclosures.js'),
 shiny::tags$style('#fixture_counts{overflow-wrap:anywhere}')),
 shiny::actionButton('invalid_calendar','Simulate incomplete calendar'),shiny::actionButton('valid_calendar','Restore calendar'),
 shiny::actionButton('heartbeat','Heartbeat'),shiny::textOutput('heartbeat_value'),shiny::textOutput('fixture_counts'),lineup_UI('lineup'))
server<-function(input,output,session){
 current<-shiny::reactiveVal(calendar)
 shiny::observeEvent(input$invalid_calendar,{bad<-calendar;attr(bad,'worker_completeness')<-'partial';attr(bad,'catalog_complete')<-FALSE;current(bad)})
 shiny::observeEvent(input$valid_calendar,current(calendar))
 lineup_Server('lineup',shiny::reactive(TRUE),shiny::reactive(auth),shiny::reactive('league'),shiny::reactive('team'),current,
  history_refresh_trigger=shiny::reactive(0L),clause_transfers_source=function()list(outgoing_status='ok',outgoing_records=outgoing_clause_empty()),
  round_catalog_state=shiny::reactive(list(data=current(),context=ctx,status='stale',freshness='stale',error=NULL)))
 output$heartbeat_value<-shiny::renderText(input$heartbeat %||% 0L)
 output$fixture_counts<-shiny::renderText({shiny::invalidateLater(100,session)
  jsonlite::toJSON(c(as.list(counts),list(stored=!is.null(stored),observed_at=if(is.null(stored))''else stored$observed_at)),auto_unbox=TRUE)})
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=as.integer(Sys.getenv('FUTMONDO_LINEUP_ERROR_PORT','3909')),launch.browser=FALSE)
