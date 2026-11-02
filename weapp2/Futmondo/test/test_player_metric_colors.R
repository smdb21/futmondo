#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
selected_player_Server<-function(...)NULL
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
check('only fully duplicate price columns are hidden, preserving distinct or missing asking prices',{
 rows<-data.frame(price=c(10,NA,30),market_price=c(10,NA,30),effective_market_price=c(10,NA,30))
 stopifnot(identical(players_table_duplicate_price_columns(rows),c("market_price","effective_market_price")))
 rows$effective_market_price[3]<-35
 stopifnot(identical(players_table_duplicate_price_columns(rows),"market_price"))
 rows$effective_market_price<-c(10,0,30)
 stopifnot(identical(players_table_duplicate_price_columns(rows),"market_price"))
})
check('source ranges include zero/negative and independent finite metric domains',{
 ranges<-players_table_metric_ranges(data.frame(recent_points_avg=c(-4,0,8,NA,Inf),recent_points_ratio=c(-2,0,2,NA,-Inf)))
 stopifnot(identical(ranges,list(recent_points_avg=list(min=-4,max=8),recent_points_ratio=list(min=-2,max=2))))
})
check('equal empty and all-missing domains remain explicit without adding ratio',{
 stopifnot(identical(players_table_metric_ranges(data.frame(recent_points_avg=c(0,0)))$recent_points_avg,list(min=0,max=0)),
  identical(players_table_metric_ranges(data.frame(recent_points_avg=c(NA_real_,Inf)))$recent_points_avg,list(min=NULL,max=NULL)),
  identical(players_table_metric_ranges(data.frame(recent_points_avg=numeric()))$recent_points_avg,list(min=NULL,max=NULL)),
  identical(players_table_metric_ranges(NULL),list()))
})
check('actual generated client style applies palette and accessible text consistently',{
 column<-players_table_recent_average_column(3L)
 stopifnot(inherits(column$style,'JS_EVAL'),identical(as.character(column$style),as.character(players_table_metric_style())))
 fixture<-tempfile(fileext='.json');jsonlite::write_json(list(style=as.character(column$style)),fixture,auto_unbox=TRUE)
 stopifnot(system2('node',c('test/test_player_metric_colors.cjs',shQuote(fixture)))==0L);unlink(fixture)
})
scores<-shiny::reactiveVal(data.frame(player_id=rep(c('a','b','c'),each=3),round=rep(1:3,3),points=c(0,0,0,0,5,10,10,20,30)))
recent_player_points_source<-function(...)list(history=scores,status=function()NULL)
source_rows<-shiny::reactiveVal(data.frame(id=c('a','b','c'),name=c('A','B','C'),role=c('Defender','Forward','Midfielder'),points=c(-2,0,100),value=c(1e6,2e6,4e6)))
updates<-list()
updateReactable<-function(outputId,data=NULL,...){if(!is.null(data))updates[[length(updates)+1L]]<<-list(data=data,args=list(...));invisible(NULL)}
getReactableState<-function(outputId,name,session){switch(name,page=1L,pageSize=5L,selected=NULL,NULL)}
enrichments<-0L;original_average<-add_recent_points_average
add_recent_points_average<-function(...){enrichments<<-enrichments+1L;original_average(...)}
settle<-function(session){for(i in seq_len(5)){later::run_now(0);session$flushReact()}}
shiny::testServer(players_table_Server,args=list(players_table_RV=source_rows,user_teams_RV=shiny::reactive(NULL),points_ratio_basis='value'),{
 session$setInputs(recent_games_n=3L);settle(session)
 initial<-output$players_table;global_meta<-table_color_meta();before<-enrichments
 check('initial widget carries independent source ranges and all metric column styles',{
  widget<-jsonlite::fromJSON(initial,simplifyVector=FALSE)$x$tag$attribs
  stopifnot(identical(global_meta,list(playerMetricRanges=list(recent_points_avg=list(min=0,max=20),recent_points_ratio=list(min=0,max=5),points=list(min=-2,max=100)))),
    widget$meta$playerMetricRanges$recent_points_avg$max==20)
  columns<-widget$columns
  metric_columns<-Filter(function(column)column$id%in%c('recent_points_avg','recent_points_ratio','points'),columns)
  stopifnot(length(metric_columns)==3L,all(vapply(metric_columns,function(column)identical(column$style,as.character(players_table_metric_style())),logical(1))))
 })
 session$setInputs(position_filter='Forward');settle(session);filtered_widget<-output$players_table
 check('app filter keeps global ranges without recalculating enrichment',{
  stopifnot(nrow(table_display_data())==1L,table_display_data()$name=='B',identical(table_color_meta(),global_meta),enrichments==before)
 })
 schema<-table_schema();visible<-table_display_data();before_updates<-length(updates)
 history<-scores();history$points[history$player_id=='c'&history$round==3]<-60;scores(history);settle(session)
 check('hidden history change updates data and metadata without rebuilding visible widget',{
  stopifnot(identical(table_display_data(),visible),identical(table_schema(),schema),identical(output$players_table,filtered_widget),
    length(updates)==before_updates+1L,identical(updates[[length(updates)]]$data,visible),
    updates[[length(updates)]]$args$meta$playerMetricRanges$recent_points_avg$max==30,
    updates[[length(updates)]]$args$meta$playerMetricRanges$recent_points_ratio$max==7.5)
 })
 session$setInputs(recent_games_n=2L);settle(session);invisible(output$players_table)
 check('N changes recompute global averages ratio and ranges',{
  stopifnot(table_display_data()$recent_points_avg==7.5,table_display_data()$recent_points_ratio==3.75,
    table_color_meta()$playerMetricRanges$recent_points_avg$max==40,table_color_meta()$playerMetricRanges$recent_points_ratio$max==10)
 })
 rows<-source_rows();rows$value[rows$id=='c']<-8e6;source_rows(rows);settle(session)
 check('hidden source change refreshes only its affected independent range',{
  stopifnot(table_color_meta()$playerMetricRanges$recent_points_avg$max==40,table_color_meta()$playerMetricRanges$recent_points_ratio$max==5)
 })
})
add_recent_points_average<-original_average
cat(sprintf('PLAYER METRIC COLORS: %d passed / 0 failed\n',checks))
