#!/usr/bin/env Rscript
# Synthetic score/hover contract; no accounts, HTTP or database calls.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
players<-data.frame(id=c('p','missing'))
history<-data.frame(player_id='p',round=c(11,13,12),points=c(4,5,12))
check('exact same N-round window supplies average and descending hover',{
 result<-add_recent_points_average(players,history,3)
 stopifnot(result$recent_points_avg[1]==7,
  identical(result$recent_points_details,c('- round 13: 5\n- round 12: 12\n- round 11: 4','')),
  is.na(result$recent_points_avg[2]))
 short<-add_recent_points_average(players,history,2)
 stopifnot(short$recent_points_avg[1]==8.5,
  short$recent_points_details[1]=='- round 13: 5\n- round 12: 12')
})
check('revisions signed scores decimals and explicit latest-round anchor agree',{
 revised<-rbind(history,data.frame(player_id='p',round=13,points=-2.5))
 revised$points[revised$round==12]<-0
 stopifnot(add_recent_points_average(players,revised,3)$recent_points_details[1]==
  '- round 13: -2.5\n- round 12: 0\n- round 11: 4')
 attr(revised,'latest_round')<-12L
 anchored<-add_recent_points_average(players,revised,2)
 stopifnot(anchored$recent_points_avg[1]==2,
  anchored$recent_points_details[1]=='- round 12: 0\n- round 11: 4')
 attr(revised,'latest_round')<-14L
 incomplete<-add_recent_points_average(players,revised,2)
 stopifnot(is.na(incomplete$recent_points_avg[1]),incomplete$recent_points_details[1]=='')
})
check('unlabelled and aggregate fallbacks never invent round numbers',{
 source<-data.frame(id='p',average.fitness='4,3,2,-2,0',average.averageLastFive=1.4)
 short<-add_recent_points_average(source,data.frame(),2)
 stopifnot(short$recent_points_avg==-1,
  short$recent_points_details=='Round numbers unavailable; latest scores first:\n- score: 0\n- score: -2')
 scalar<-add_recent_points_average(data.frame(id='p',average.averageLastFive=7),data.frame(),5)
 stopifnot(scalar$recent_points_details=='Individual round scores are unavailable in the reported average.')
 explicit<-add_recent_points_average(source,history,2)
 stopifnot(explicit$recent_points_details=='- round 13: 5\n- round 12: 12')
 stopifnot(length(add_recent_points_average(players[FALSE,,drop=FALSE],history,2)$recent_points_details)==0L)
})
check('actual numeric pill formatter attaches title as safe text and leaves other pills alone',{
 js<-paste0("const assert = require('node:assert/strict'); const React = {createElement: (tag, props, text) => ({tag, props, text})}; const cell = ",
  as.character(players_table_recent_average_column(3)$cell),
  "; const title = '- round 13: -2.5\\n- round 12: 0'; const result = cell({value: -1.25, row: {recent_points_details: title}}); ",
  "assert.equal(result.props.title, title); assert.equal(result.props.className, 'fm-metric-badge'); assert.equal(result.text, '-1.3'); ",
  "assert.equal(cell({value: null}), ''); assert.equal(cell({value: 0, row: {recent_points_details: '<unsafe>'}}).props.title, '<unsafe>'); const plain = ",
  as.character(players_table_metric_cell(0)),"; assert.equal(plain({value: 2}).props.title, undefined);")
 stopifnot(system2('node',c('-e',shQuote(js)))==0L)
})
selected_player_Server<-function(...)NULL
scores<-shiny::reactiveVal(history)
recent_player_points_source<-function(...)list(history=scores,status=function()NULL)
updates<-list()
updateReactable<-function(outputId,data=NULL,...){if(!is.null(data))updates[[length(updates)+1L]]<<-list(data=data,args=list(...));invisible(NULL)}
getReactableState<-function(outputId,name,session){switch(name,page=1L,pageSize=5L,selected=NULL,NULL)}
settle<-function(session){for(i in seq_len(5)){later::run_now(0);session$flushReact()}}
shiny::testServer(players_table_Server,args=list(players_table_RV=shiny::reactive(data.frame(id='p',name='Player')),
 user_teams_RV=shiny::reactive(NULL)),{
 session$setInputs(recent_games_n=3L);settle(session)
 initial<-output$players_table;schema<-table_schema()
 widget<-jsonlite::fromJSON(initial,simplifyVector=FALSE)$x$tag$attribs
 column<-Filter(function(x)x$id=='recent_points_details',widget$columns)[[1]]
 check('hover payload is hidden and excluded from client search/filtering',{
  stopifnot(isFALSE(column$show),isFALSE(column$searchable),isFALSE(column$filterable),
   table_display_data()$recent_points_details=='- round 13: 5\n- round 12: 12\n- round 11: 4')
 })
 # Swap two scores without changing average: title must refresh in the same widget.
 revised<-history;revised$points[revised$round==13]<-12;revised$points[revised$round==12]<-5
 before<-length(updates);scores(revised);settle(session)
 check('history-only detail correction updates in place even with unchanged average',{
  stopifnot(identical(table_schema(),schema),identical(output$players_table,initial),length(updates)==before+1L,
   updates[[length(updates)]]$data$recent_points_avg==7,
   updates[[length(updates)]]$data$recent_points_details=='- round 13: 12\n- round 12: 5\n- round 11: 4')
 })
})
cat(sprintf('RECENT POINTS HOVER: %d passed / 0 failed\n',checks))
