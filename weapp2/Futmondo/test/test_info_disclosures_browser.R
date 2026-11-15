#!/usr/bin/env Rscript
# Synthetic browser fixture for production disclosure CSS/JS and real panels.
# No credentials, source reads, database writes or transactions are permitted.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fixture_io<-new.env(parent=emptyenv());fixture_io$attempts<-0L
blocked_io<-function(...){fixture_io$attempts<-fixture_io$attempts+1L;stop('Unexpected source/database acquisition in disclosure fixture')}
for(name in c('futmondo_post','fm_async_http','fm_worker_rpc','supabase_read','supabase_upsert','supabase_write'))assign(name,blocked_io)
get_team_image_name<-function(...)'' # Synthetic club text; no external crest request.
long_detail<-paste(rep('LongDiagnosticReferenceWithoutBreaks',25),collapse='')
ui<-shiny::fluidPage(shiny::tags$head(
 shiny::tags$style(shiny::HTML(fm_theme_css())),shiny::includeCSS('www/custom_style.css'),
 shiny::includeScript('www/info-disclosures.js')),
 shiny::h2('Information fixture'),
 shiny::div(id='neutral',fm_info('Saved results are reused before another acquisition.')),
 shiny::div(id='long_error',fm_info(long_detail,error=TRUE)),
 shiny::uiOutput('clock_panel'),shiny::uiOutput('replaced_panel'),
 shiny::div(id='empty_dynamic',fm_info(shiny::textOutput('deferred_text',inline=TRUE))),
 shiny::uiOutput('pending_panel'),shiny::uiOutput('settled_error_panel'),
 reactable::reactableOutput('tooltip_table'),shiny::textOutput('selected_row'),
 shiny::div(id='retry_panel',fm_info('Could not save the preference.',shiny::actionButton('retry','Retry'),error=TRUE)),
 shiny::div(id='real_offers',team_received_offers_panel(list(records=data.frame(bid_source=character()),status='stale',error=NULL),identity)),
 shiny::div(id='real_rivals',rivals_status_message('6 league teams loaded.')),
 shiny::div(id='real_chart',player_trend_information(list(status='partial',points=data.frame(),valuation=data.frame()),list(value=1000000,points=12))),
 shiny::div(id='login_feedback_fixture',fm_info('Could not authenticate.',error=TRUE,class='login-feedback login-feedback-error')),
 shiny::fluidRow(shiny::column(12,
  shiny::actionButton('advance_stamp','Advance source clock'),
  shiny::actionButton('replace_panel','Publish new detail'),
  shiny::actionButton('emit_error','Notification error'),
  shiny::actionButton('emit_warning','Notification warning'),
  shiny::actionButton('validate_error','Validation error'),
  shiny::actionButton('validate_loading','Validation loading'),
  shiny::actionButton('runtime_error','Output error'),
  shiny::actionButton('heartbeat','Heartbeat'),shiny::actionButton('show_deferred','Fill empty information'),
  shiny::actionButton('clear_deferred','Empty information'),shiny::actionButton('wait_deferred','Wait for data'),
  shiny::actionButton('fail_deferred','Wait then fail'),shiny::actionButton('toggle_departure','Toggle departure'))),
 shiny::textOutput('validation'),shiny::textOutput('loading_validation'),shiny::textOutput('runtime_error_output'),shiny::textOutput('heartbeat_value'),
 shiny::div(style='display:none',shiny::textOutput('counts')))
server<-function(input,output,session){
 departing<-shiny::reactiveVal(TRUE)
 output$tooltip_table<-reactable::renderReactable({
  rows<-data.frame(name=c('Departing name','Staying name'),team='Club',logo='')
  columns<-get_reactable_columns_for_players(rows);columns[['.selection']]<-reactable::colDef(show=FALSE)
  reactable::reactable(rows,columns=columns,selection='single',onClick='select',defaultPageSize=2,
   rowClass=players_table_departure_class(),meta=list(departingPlayerIndexes=list(0L)))
 })
 output$selected_row<-shiny::renderText(paste(reactable::getReactableState('tooltip_table','selected',session=session),collapse=','))
 shiny::observeEvent(input$toggle_departure,{departing(!departing());reactable::updateReactable('tooltip_table',meta=list(departingPlayerIndexes=if(departing())list(0L)else list()),session=session)})
 deferred<-shiny::reactiveVal('');waiting<-shiny::reactiveVal(TRUE);failure<-shiny::reactiveVal(FALSE)
 output$deferred_text<-shiny::renderText(deferred())
 shiny::outputOptions(output,'deferred_text',suspendWhenHidden=FALSE)
 output$pending_panel<-shiny::renderUI(fm_info('Data is ready.',pending=waiting()))
 output$settled_error_panel<-shiny::renderUI(fm_info(if(failure())'Confirmed source failure.'else 'Waiting for source.',error=TRUE,pending=waiting()||!failure()))
 shiny::observeEvent(input$show_deferred,{deferred('Ready source information');waiting(FALSE)})
 shiny::observeEvent(input$clear_deferred,deferred(''))
 shiny::observeEvent(input$wait_deferred,{waiting(TRUE);later::later(function(){waiting(FALSE)},.4)})
 shiny::observeEvent(input$fail_deferred,{failure(FALSE);waiting(TRUE);later::later(function(){failure(TRUE);waiting(FALSE)},.4)})
 stamp<-shiny::reactiveVal(1L);version<-shiny::reactiveVal(1L);retries<-shiny::reactiveVal(0L);clock_panels<-0L
 output$clock_panel<-shiny::renderUI({clock_panels<<-clock_panels+1L;fm_info('Latest update: ',shiny::textOutput('source_clock',inline=TRUE))})
 output$source_clock<-shiny::renderText(paste('fixture-clock',stamp()))
 shiny::outputOptions(output,'source_clock',suspendWhenHidden=FALSE)
 output$replaced_panel<-shiny::renderUI(fm_info(paste('Detail version',version())))
 shiny::observeEvent(input$advance_stamp,stamp(stamp()+1L))
 shiny::observeEvent(input$replace_panel,version(version()+1L))
 shiny::observeEvent(input$retry,retries(retries()+1L))
 shiny::observeEvent(input$emit_error,shiny::showNotification('Synthetic request failed. No acquisition was performed.',type='error',id='fixture-error',duration=NULL))
 shiny::observeEvent(input$emit_warning,shiny::showNotification('Synthetic verification warning.',type='warning',id='fixture-warning',duration=NULL))
 output$validation<-shiny::renderText({if(isTRUE((input$validate_error %||% 0L)>0L))shiny::validate(shiny::need(FALSE,'Synthetic validation failed.'));''})
 output$loading_validation<-shiny::renderText({if(isTRUE((input$validate_loading %||% 0L)>0L))shiny::validate(shiny::need(FALSE,'Synthetic loading is pending.'),errorClass='fm-loading');''})
 output$runtime_error_output<-shiny::renderText({if(isTRUE((input$runtime_error %||% 0L)>0L))stop('Synthetic output error.');''})
 output$heartbeat_value<-shiny::renderText(input$heartbeat %||% 0L)
 output$counts<-shiny::renderText(jsonlite::toJSON(list(retries=retries(),io_attempts=fixture_io$attempts,clock_panels=clock_panels),auto_unbox=TRUE))
 shiny::outputOptions(output,'counts',suspendWhenHidden=FALSE)
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',
 port=as.integer(Sys.getenv('FUTMONDO_INFO_TEST_PORT','3908')),launch.browser=FALSE)
