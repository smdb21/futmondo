#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({library(shiny);library(shinydashboard);library(shinydashboardPlus);library(dplyr);library(plotly)})
source("futmondo_functions.R");source("theme.R");source("Modules/Selected_Player_Module.R")
n<-0L;check<-function(name,code){force(code);n<<-n+1L;cat("PASS",name,"\n")}
find_output<-function(node,target){
 if(is.call(node)&&identical(node[[1]],as.name("<-"))&&identical(paste(deparse(node[[2]]),collapse=""),paste0("output$",target)))return(node[[3]][[2]])
 if(is.call(node)||is.expression(node)||is.pairlist(node))for(child in as.list(node)){if(missing(child))next;found<-find_output(child,target);if(!is.null(found))return(found)};NULL
}
tree<-parse("Modules/Selected_Player_Module.R");plot_body<-find_output(tree,"player_trend_plot");recent_body<-find_output(tree,"recent_round_points")
empty<-function()list(valuation=data.frame(),points=data.frame(),status="empty",freshness="fresh")
render_fixture<-function(history=empty(),total=12,value=1000000,matches=list()){
 env<-new.env(parent=globalenv());env$selected_player<-function()data.frame(id="p",points=total,value=value);env$card_history<-function()history;env$card_chart_ready<-function()TRUE
 env$card_states<-list(matches=function()list(data=matches))
 plotly::plotly_build(eval(plot_body,env))$x
}
check("zero negative and large values have padded axes",{for(values in list(0,-2,c(-2,0,5),c(99e6,100e6))){bounds<-player_trend_axis_range(values);stopifnot(all(is.finite(bounds)),all(values>bounds[1]),all(values<bounds[2]))}})
check("invalid axis inputs stay finite",stopifnot(identical(player_trend_axis_range(c(NA,Inf,-Inf)),c(0,1))))
known<-empty();known$points<-data.frame(round_number=1:4,points=c(4,0,-2,3),date=as.POSIXct(rep(NA_character_,4),tz="UTC"),score_status=c("final","final","final","provisional"))
check("round scores retain zero negatives missing dates and status",{chart<-render_fixture(known);stopifnot(identical(as.numeric(unlist(chart$data[[1]]$x)),as.numeric(1:4)),identical(as.numeric(unlist(chart$data[[1]]$y)),c(4,0,-2,3)),grepl("provisional",tail(chart$data[[1]]$text,1),fixed=TRUE))})
check("recent panel preserves points with a single finality explanation",{
 env<-new.env(parent=globalenv());env$selected_player<-function()data.frame(id="p",points=5,value=100)
 env$card_history<-function()known
 env$card_states<-list(matches=function()resource_state(list(matches=list())),summary=function()resource_state(list(points=list())))
 html<-as.character(eval(recent_body,env))
 stopifnot(grepl("-2 pts",html,fixed=TRUE),grepl("0 pts",html,fixed=TRUE),
   grepl("About these points",html,fixed=TRUE),!grepl("player-round-status",html,fixed=TRUE))
})
check("unknown point finality is preserved in chart provenance",{
 history<-known;history$points$score_status[4]<-"unknown";chart<-render_fixture(history)
 stopifnot(grepl("unknown",tail(chart$data[[1]]$text,1),fixed=TRUE),identical(as.numeric(unlist(chart$data[[1]]$y)),c(4,0,-2,3)))
})
check("cumulative totals never become round scores",{chart<-render_fixture(total=37);stopifnot(grepl("Total points: 37",chart$layout$annotations[[1]]$text,fixed=TRUE));text<-paste(readLines("Modules/Selected_Player_Module.R"),collapse="\n");stopifnot(!grepl("build_player_points_trace|get_player_historical_data",text))})
check("unknown valuation has no fabricated curve",{chart<-render_fixture(value=1234567);stopifnot(grepl("Valuation history unavailable",chart$layout$annotations[[1]]$text,fixed=TRUE),grepl("1.234.567",chart$layout$annotations[[1]]$text,fixed=TRUE))})
combined<-known;combined$valuation<-data.frame(recorded_at=c("2026-10-01T00:00:00Z","2026-10-02T00:00:00Z","bad"),value=c(99e6,100e6,1))
check("valuation keeps only actual finite dated observations",{chart<-render_fixture(combined);stopifnot(identical(as.numeric(unlist(chart$data[[1]]$y)),c(99e6,100e6)))})
check("both series have independent vertical scales and distinct styles",{
 history<-combined;history$points$date<-as.POSIXct("2026-10-01",tz="UTC")+0:3*86400
 chart<-render_fixture(history);value<-chart$data[[1]];points<-chart$data[[2]]
 stopifnot(length(chart$data)==2L,value$yaxis=="y",points$yaxis=="y2",points$xaxis=="x",
   value$line$dash=="solid",points$line$dash=="dash",value$line$color!=points$line$color,
   chart$layout$yaxis2$overlaying=="y",chart$layout$yaxis2$side=="right",identical(chart$layout$showlegend,FALSE),
   all(c(99e6,100e6)>chart$layout$yaxis$range[1]),all(c(99e6,100e6)<chart$layout$yaxis$range[2]),
   all(c(-2,4)>chart$layout$yaxis2$range[1]),all(c(-2,4)<chart$layout$yaxis2$range[2]))
})
check("dense histories use sparse automatic dates without an empty dtick",{
 history<-known
 history$valuation<-data.frame(recorded_at=format(as.POSIXct("2026-07-01",tz="UTC")+0:123*86400,"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),value=22e6+sin(0:123/12)*1e6)
 history$points$date<-as.POSIXct("2026-07-01",tz="UTC")+0:3*7*86400
 chart<-render_fixture(history)
 stopifnot(chart$layout$xaxis$tickmode=="auto",chart$layout$xaxis$nticks==4,
   is.null(chart$layout$xaxis$dtick),chart$layout$xaxis$tickformat=="%d %b",chart$layout$xaxis$tickangle==0,
   identical(chart$layout$xaxis$showgrid,FALSE),identical(chart$layout$showlegend,FALSE),
   length(chart$data[[1]]$y)==124L)
})
check("undated scores stay visible with an explicitly independent round axis",{
 chart<-render_fixture(combined)
 stopifnot(chart$data[[2]]$xaxis=="x2",identical(as.numeric(chart$data[[2]]$x),as.numeric(1:4)),
   chart$layout$xaxis2$overlaying=="x",grepl("not aligned with dates",chart$layout$annotations[[1]]$text,fixed=TRUE))
})
check("existing scoped match dates align points without a new read",{
 history<-combined;history$context<-list(player_id="p",season="2026",competition_id="league")
 history$points$source<-"futmondo"
 matches<-data.frame(round=1:4,occurred_at=paste0("2026-10-0",1:4,"T12:00:00Z"),season="2026",competition_id="league")
 dates<-player_trend_points_dates(history$points,matches,history$context)
 stopifnot(identical(as.numeric(dates),as.numeric(fm_time(matches$occurred_at))))
 matches$season<-"2025";stopifnot(all(is.na(player_trend_points_dates(history$points,matches,history$context))))
})
check("stored undated final scores remain available",{rows<-data.frame(round=c(1,2,3),points=c(0,-2,5),score_status=c("final","final","provisional"));trace<-player_match_points_trace(rows);stopifnot(trace$has_points,identical(trace$points_df$points,c(0,-2)),all(is.na(trace$points_df$date)))})
check("card exposes refresh and one combined chart",{html<-as.character(selected_player_UI("card"));stopifnot(grepl("refresh_history",html,fixed=TRUE),!grepl("Smart Bid|fis_panel|smart_bid_widget|player_chart_metric",html))})
cat("Player trend plot:",n,"checks passed.\n")
