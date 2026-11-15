#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
expected <- '12/11/2026 13:34 CET'
inputs <- c('2026-11-12T12:34:20.989Z','2026-11-12T13:34:20.989+01:00',
 '2026-11-12T01:34:20+02:00',NA,'','not a date','2026-11-12T25:00:00Z',
 '2026-11-12garbage','2026-02-30T12:00:00Z')
stopifnot(identical(notification_datetime_labels(inputs),
 c(expected,expected,'12/11/2026 00:34 CET',rep('Date unavailable',6))),
 identical(notification_datetime_labels(character()),character()),
 notification_datetime_labels(fm_time(inputs[1]))==expected)
cat('PASS known account-zone/offset/day-rollover labels and missing/malformed dates\n')
bind_factual_reads <- function(...)NULL
selected_player_Server <- function(...)NULL
record_round_publication_signal <- function(...)NULL
notices <- normalize_notifications(list(list(`_id`='notice',created=inputs[1],updated=inputs[1],
 type='market',action='acceptBid',readed=FALSE,directObject=list(id='p',name='<player>'),
 context=list(id='league',name='OHY CAMPEÓN'))))
attr(notices,'observed_at') <- fm_time(inputs[1]); before <- notices
get_notifications <- function(...)notices
get_notification_unread <- function(...)1L
auth <- c(token='synthetic',userid='offline',user_name='offline')
shiny::testServer(notifications_Server,args=list(is_module_active=shiny::reactive(TRUE),
 login_token=shiny::reactive(auth),championship_id=shiny::reactive('league'),
 user_team_id=shiny::reactive('team'),user_teams_RV=shiny::reactive(NULL)),{
 session$setInputs(read_filter='all',source_filter='Futmondo',event_filter='All',current_league=TRUE)
 for(i in seq_len(5)){later::run_now(0);session$flushReact()}
 html <- output$items$html; doc <- xml2::read_html(html,encoding='UTF-8')
 header <- xml2::xml_find_first(doc,"//*[@class='fm-notification-meta']")
 label <- gsub('[[:space:]]+',' ',trimws(xml2::xml_text(header)))
 stopifnot(label=='OHY CAMPEÓN · 12/11/2026 13:34 CET',
  xml2::xml_attr(xml2::xml_find_first(header,'.//time'),'datetime')==inputs[1],
  !grepl('Futmondo|LigaRadar',label),grepl('&lt;player&gt;',html,fixed=TRUE),
  grepl(expected,output$status$html,fixed=TRUE),identical(official(),before))
})
cat('PASS actual header omits source, retains ISO/source data, escapes text and formats refresh time\n')
elapsed <- system.time(notification_datetime_labels(rep(inputs[1],1000)))[['elapsed']]
cat(sprintf('MEASURE 1000 timestamp labels: %.3f seconds\n',elapsed))
