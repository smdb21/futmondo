#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
for(value in list(NULL,'',' \n ',NA_character_,list(),shiny::tagList(),shiny::tags$small(),shiny::tags$div(shiny::tags$span(' ')),shiny::HTML('<span>&nbsp;</span>')))
 stopifnot(is.null(fm_info(value)))
stopifnot(is.null(fm_info('Waiting for data',pending=TRUE)),is.null(fm_info('Pending source',error=TRUE,pending=TRUE)),
 grepl('fm-info-error',as.character(fm_info('Confirmed failure',error=TRUE)),fixed=TRUE),
 grepl('fm-info-empty',as.character(fm_info(shiny::tags$small(shiny::textOutput('pending_stamp',inline=TRUE)))),fixed=TRUE))
cat('PASS recursively empty content has no disclosure, dynamic placeholders start hidden, and pending errors cannot flash red\n')
for(outgoing in c(FALSE,TRUE)){
 state<-list(status='loading',error='Prior failure while retrying',observations_only=TRUE,
  records=if(outgoing)outgoing_clause_empty()else incoming_clause_empty())
 html<-as.character(clause_transfer_panel(state,list(available=FALSE),identity,outgoing=outgoing))
 stopifnot(!grepl('class="fm-info',html,fixed=TRUE))
 # An empty settled source-clock wrapper is not information.
 state$status<-'observed';state$error<-NULL
 html<-as.character(clause_transfer_panel(state,list(available=TRUE,round_number=1,starts_at=Sys.time()+86400),identity,
  outgoing=outgoing))
 stopifnot(!grepl('class="fm-info',html,fixed=TRUE))
}
html<-as.character(team_received_offers_panel(list(status='loading',records=received_offers_empty(),error=NULL),identity))
stopifnot(!grepl('class="fm-info',html,fixed=TRUE),is.null(rivals_status_message('Pending source',loading=TRUE,error=TRUE)))
cat('PASS incoming/leaving/offers/rival waiting states hide icons; empty source wrappers never create empty boxes\n')
# Exercise a pending read, then successful data, another pending refresh, and a
# settled failure in the actual notification module. No real I/O is performed.
bind_factual_reads<-function(...)NULL;selected_player_Server<-function(...)NULL
record_round_publication_signal<-function(...)NULL
phase<-shiny::reactiveVal('waiting');reads<-0L
get_notifications<-function(...){reads<<-reads+1L
 if(phase()=='waiting')shiny::validate(shiny::need(FALSE,'Loading data…'),errorClass='fm-loading')
 if(phase()=='failed')stop(resource_failure(category='network'))
 observed_data(normalize_notifications(list()),observed_at=Sys.time())
}
get_notification_unread<-function(...)0L
shiny::testServer(notifications_Server,args=list(is_module_active=shiny::reactive(TRUE),login_token=shiny::reactive(c(userid='fixture',token='synthetic')),
 championship_id=shiny::reactive('league'),user_team_id=shiny::reactive('team'),user_teams_RV=shiny::reactive(NULL),refresh_trigger=phase),{
 settle<-function()for(i in 1:12){later::run_now(0);session$flushReact()}
 settle();stopifnot(is.null(output$status),shiny::isolate(pending()),!shiny::isolate(stale()))
 phase('ready');settle();stopifnot(grepl('Updated',output$status$html,fixed=TRUE),!shiny::isolate(pending()))
 phase('waiting');settle();stopifnot(is.null(output$status),shiny::isolate(pending()))
 phase('failed');settle();stopifnot(grepl('fm-info-error',output$status$html,fixed=TRUE),!shiny::isolate(pending()),shiny::isolate(stale()))
})
cat('PASS notification waiting/refresh stays hidden; successful data and confirmed failures become inspectable only after settling\n')
elapsed<-system.time(for(i in 1:1000)fm_info(shiny::tags$div(shiny::tags$span('Source detail'))))[['elapsed']]
cat(sprintf('MEASURE 1000 nested information renders %.3fs; no source/database calls\n',elapsed))
