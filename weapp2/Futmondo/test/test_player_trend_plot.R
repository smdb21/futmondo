#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({library(shiny);library(shinydashboard);library(shinydashboardPlus);library(dplyr);library(plotly)})
source("data_contracts.R");source("theme.R");source("Modules/Selected_Player_Module.R")
n<-0L;check<-function(name,code){force(code);n<<-n+1L;cat("PASS",name,"\n")}
find_output<-function(node,target){
 if(is.call(node)&&identical(node[[1]],as.name("<-"))&&identical(paste(deparse(node[[2]]),collapse=""),paste0("output$",target)))return(node[[3]][[2]])
 if(is.call(node)||is.expression(node)||is.pairlist(node))for(child in as.list(node)){if(missing(child))next;found<-find_output(child,target);if(!is.null(found))return(found)};NULL
}
tree<-parse("Modules/Selected_Player_Module.R");plot_body<-find_output(tree,"player_trend_plot");recent_body<-find_output(tree,"recent_round_points")
empty<-function()list(valuation=data.frame(),points=data.frame(),status="empty",freshness="fresh")
render_fixture<-function(history=empty(),metric="points",total=12,value=1000000){
 env<-new.env(parent=globalenv());env$selected_player<-function()data.frame(id="p",points=total,value=value);env$card_history<-function()history;env$input<-list(player_chart_metric=metric)
 plotly::plotly_build(eval(plot_body,env))$x
}
check("zero negative and large values have padded axes",{for(values in list(0,-2,c(-2,0,5),c(99e6,100e6))){bounds<-player_trend_axis_range(values);stopifnot(all(is.finite(bounds)),all(values>bounds[1]),all(values<bounds[2]))}})
check("invalid axis inputs stay finite",stopifnot(identical(player_trend_axis_range(c(NA,Inf,-Inf)),c(0,1))))
known<-empty();known$points<-data.frame(round_number=1:4,points=c(4,0,-2,3),date=as.POSIXct(rep(NA_character_,4),tz="UTC"),score_status=c("final","final","final","provisional"))
check("round scores retain zero negatives missing dates and status",{chart<-render_fixture(known);stopifnot(identical(as.numeric(unlist(chart$data[[1]]$x)),as.numeric(1:4)),identical(as.numeric(unlist(chart$data[[1]]$y)),c(4,0,-2,3)),grepl("provisional",tail(chart$data[[1]]$text,1),fixed=TRUE))})
check("recent panel uses the same factual source",{env<-new.env(parent=globalenv());env$selected_player<-function()data.frame(id="p",points=5,value=100);env$card_history<-function()known;html<-as.character(eval(recent_body,env));stopifnot(grepl("-2 pts",html,fixed=TRUE),grepl("0 pts",html,fixed=TRUE),grepl("provisional",html,fixed=TRUE))})
check("cumulative totals never become round scores",{chart<-render_fixture(total=37);stopifnot(grepl("Total points: 37",chart$layout$annotations[[1]]$text,fixed=TRUE));text<-paste(readLines("Modules/Selected_Player_Module.R"),collapse="\n");stopifnot(!grepl("build_player_points_trace|get_player_historical_data",text))})
check("unknown valuation has no fabricated curve",{chart<-render_fixture(metric="valuation",value=1234567);stopifnot(grepl("Valuation history unavailable",chart$layout$annotations[[1]]$text,fixed=TRUE),grepl("1.234.567",chart$layout$annotations[[1]]$text,fixed=TRUE))})
check("valuation keeps only actual finite dated observations",{history<-known;history$valuation<-data.frame(recorded_at=c("2026-10-01T00:00:00Z","2026-10-02T00:00:00Z","bad"),value=c(99e6,100e6,1));chart<-render_fixture(history,"valuation");stopifnot(identical(as.numeric(unlist(chart$data[[1]]$y)),c(99e6,100e6)))})
check("stored undated final scores remain available",{rows<-data.frame(round=c(1,2,3),points=c(0,-2,5),score_status=c("final","final","provisional"));trace<-player_match_points_trace(rows);stopifnot(trace$has_points,identical(trace$points_df$points,c(0,-2)),all(is.na(trace$points_df$date)))})
check("card exposes refresh without advice",{html<-as.character(selected_player_UI("card"));stopifnot(grepl("refresh_history",html,fixed=TRUE),!grepl("Smart Bid|fis_panel|smart_bid_widget",html))})
cat("Player trend plot:",n,"checks passed.\n")
