#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
ui<-shiny::fluidPage(shiny::tags$head(
 shiny::tags$style(shiny::HTML(fm_theme_css())),shiny::includeCSS('www/custom_style.css'),
 shiny::includeScript('www/info-disclosures.js')),
 shiny::div(`data-fm-info-global`='true',shiny::uiOutput('finance')),
 shiny::div(id='shiny-tab-market',class='tab-pane active',
  fm_info_page_UI("Today's market",
   shiny::h2("Today's market"),
   fm_info('Market guide stays available.',source='Market guide'),
   shiny::uiOutput('healthy'),shiny::div(class='fm-coach-control',shiny::checkboxInput('automatically_hire_coach','Automatically hire coach',TRUE),shiny::uiOutput('failed')),shiny::uiOutput('secondary'),shiny::uiOutput('optional'),
   shiny::uiOutput('action_validation'),reactable::reactableOutput('widget_error'))),
 shiny::div(id='shiny-tab-team',class='tab-pane',style='display:none',
  fm_info_page_UI('My Team',fm_info('Team-specific information.',source='Roster'))),
 shiny::div(id='shiny-tab-empty',class='tab-pane',style='display:none',fm_info_page_UI('Empty page',fm_info('',source='Empty'),fm_info('Waiting',pending=TRUE))),
 shiny::actionButton('toggle_error','Toggle failure'),shiny::actionButton('toggle_second','Toggle second failure'),shiny::actionButton('toggle_widget_failure','Toggle table failure'),shiny::actionButton('advance_stamp','Advance clock'),
 shiny::actionButton('start_pending','Delayed source'),shiny::actionButton('open_dialog','Open dialog'),
 shiny::actionButton('action_error','Action error'),shiny::actionButton('switch_context','Switch context'),
 shiny::actionButton('remove_source','Remove source'),shiny::actionButton('heartbeat','Heartbeat'),
 shiny::tags$button(id='go_team',type='button',onclick="$('#shiny-tab-market').hide().removeClass('active');$('#shiny-tab-team').show().addClass('active');",'Team page'),
 shiny::tags$button(id='go_market',type='button',onclick="$('#shiny-tab-team').hide().removeClass('active');$('#shiny-tab-market').show().addClass('active');",'Market page'),
 shiny::textOutput('heartbeat_value'),shiny::div(style='display:none',shiny::textOutput('counts')))
server<-function(input,output,session){
 session$userData$information_context_revision<-shiny::reactiveVal(0L)
 attempts<-0L
 blocked<-function(...){attempts<<-attempts+1L;stop('Unexpected I/O')}
 for(name in c('futmondo_post','fm_async_http','fm_worker_rpc','supabase_read','supabase_upsert'))assign(name,blocked,.GlobalEnv)
 widget_failed<-shiny::reactiveVal(FALSE)
 error<-shiny::reactiveVal(FALSE);second_error<-shiny::reactiveVal(FALSE);pending<-shiny::reactiveVal(FALSE);stamp<-shiny::reactiveVal(1L)
 context<-shiny::reactiveVal('A');retry_count<-shiny::reactiveVal(0L);dialog_count<-shiny::reactiveVal(0L)
 finance_renders<-0L;healthy_renders<-0L
 output$finance<-shiny::renderUI({finance_renders<<-finance_renders+1L;fm_info(paste('Finance context',context()),source='Balance')})
 output$healthy<-shiny::renderUI({healthy_renders<<-healthy_renders+1L
  fm_info('Successful source. ',shiny::textOutput('source_clock',inline=TRUE),source='Background sync')})
 output$source_clock<-shiny::renderText(paste('Observed clock',stamp()))
 shiny::outputOptions(output,'source_clock',suspendWhenHidden=FALSE)
 output$failed<-shiny::renderUI(fm_info(if(error())paste('Failed source.',paste(rep('LongDiagnostic',40),collapse=''))else 'Source recovered.',
  shiny::actionButton('retry','Retry source'),error=error(),pending=pending(),source='Market prices'))
 output$secondary<-shiny::renderUI(if(second_error())fm_info('Another source failed.',error=TRUE,source='Round awards'))
 output$widget_error<-reactable::renderReactable({if(widget_failed())shiny::validate(shiny::need(FALSE,'Widget source failed.'));reactable::reactable(data.frame(Player='One'))})
 output$optional<-shiny::renderUI(shiny::div(id='optional_source',fm_info('Temporary source.',source='Optional source')))
 output$action_validation<-shiny::renderUI(fm_info('Enter an amount to continue.',error=TRUE,inline=TRUE))
 shiny::observeEvent(input$toggle_error,error(!error()))
 shiny::observeEvent(input$toggle_second,second_error(!second_error()))
 shiny::observeEvent(input$toggle_widget_failure,widget_failed(!widget_failed()))
 shiny::observeEvent(input$advance_stamp,stamp(stamp()+1L))
 shiny::observeEvent(input$retry,{retry_count(retry_count()+1L);error(FALSE)})
 shiny::observeEvent(input$start_pending,{pending(TRUE);later::later(function(){if(!session$isClosed())pending(FALSE)},.45)})
 shiny::observeEvent(input$remove_source,shiny::removeUI('#optional_source'))
 shiny::observeEvent(input$switch_context,{session$userData$information_context_revision(session$userData$information_context_revision()+1L);session$sendCustomMessage('fm-information-reset',list());context(if(context()=='A')'B'else'A')})
 shiny::observeEvent(input$open_dialog,{
  dialog_count(dialog_count()+1L)
  shiny::showModal(shiny::modalDialog(title='Player details',
    fm_info('Player facts are available.',source='Player profile'),
    fm_info(shiny::p('Points are reported by Futmondo. Final confirmation is not provided for some rounds.'),
      source='',class='player-points-explanation'),
    shiny::uiOutput('dialog_info'),footer=shiny::modalButton('Close')))
 })
 output$dialog_info<-shiny::renderUI(fm_info(paste('Dialog generation',dialog_count()),shiny::actionButton('dialog_retry','Retry player source'),error=error(),source='Player history'))
 shiny::observeEvent(input$dialog_retry,retry_count(retry_count()+1L))
 shiny::observeEvent(input$action_error,shiny::showNotification('Purchase failed. Check the published price.',type='error',duration=NULL,id='action-error'))
 output$heartbeat_value<-shiny::renderText(input$heartbeat %||% 0L)
 output$counts<-shiny::renderText(jsonlite::toJSON(list(retries=retry_count(),io_attempts=attempts,
  finance_renders=finance_renders,healthy_renders=healthy_renders),auto_unbox=TRUE))
 shiny::outputOptions(output,'counts',suspendWhenHidden=FALSE)
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',
 port=as.integer(Sys.getenv('FUTMONDO_INFO_TEST_PORT','3908')),launch.browser=FALSE)
