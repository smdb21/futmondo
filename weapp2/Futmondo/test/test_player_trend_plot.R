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
 grouped <- build_player_recent_rounds(history$points, normalize_player_matches(matches), history$context %||% list())
 env$chart_data<-function()list(history=history,dates=player_trend_points_dates(history$points, grouped$rounds))
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
 env$card_round_details<-function() {
   rounds<-build_player_recent_rounds(known$points, data.frame(), list(), limit=Inf)
   rounds$dates<-player_trend_points_dates(known$points, rounds$rounds)
   rounds
 }
 html<-as.character(eval(recent_body,env))
 stopifnot(grepl("-2 pts",html,fixed=TRUE),grepl("0 pts",html,fixed=TRUE),
   grepl("Points are reported by Futmondo.",html,fixed=TRUE),
   lengths(regmatches(html, gregexpr("fm-info-error", html, fixed=TRUE)))==1L,
   grepl("4 rounds have no verified player fixture.",html,fixed=TRUE),
   grepl('data-fm-info-source=""',html,fixed=TRUE),!grepl("About these points|LigaRadar|player-round-status",html))
})
check("unknown point finality is preserved in chart provenance",{
 history<-known;history$points$score_status[4]<-"unknown";chart<-render_fixture(history)
 stopifnot(grepl("unknown",tail(chart$data[[1]]$text,1),fixed=TRUE),identical(as.numeric(unlist(chart$data[[1]]$y)),c(4,0,-2,3)))
})
check("cumulative totals stay in collapsed information and never become round scores",{chart<-render_fixture(total=37);info<-as.character(player_trend_information(empty(),data.frame(points=37,value=1000000)));stopifnot(!length(chart$layout$annotations),grepl("Total points: 37",info,fixed=TRUE),grepl("<details",info,fixed=TRUE));text<-paste(readLines("Modules/Selected_Player_Module.R"),collapse="\n");stopifnot(!grepl("build_player_points_trace|get_player_historical_data",text))})
check("unknown valuation has no fabricated curve and diagnostics are collapsed",{chart<-render_fixture(value=1234567);info<-as.character(player_trend_information(empty(),data.frame(points=12,value=1234567)));stopifnot(!length(chart$layout$annotations),grepl("Valuation history unavailable",info,fixed=TRUE),grepl("1.234.567",info,fixed=TRUE),grepl("fm-info-error",info,fixed=TRUE),!grepl("<details[^>]* open",info))})
combined<-known;combined$valuation<-data.frame(recorded_at=c("2026-10-01T00:00:00Z","2026-10-02T00:00:00Z","bad"),value=c(99e6,100e6,1))
check("valuation keeps only actual finite dated observations",{chart<-render_fixture(combined);stopifnot(identical(as.numeric(unlist(chart$data[[1]]$y)),c(99e6,100e6)))})
check("both series have independent vertical scales and distinct styles",{
 history<-combined;history$points$date<-as.POSIXct("2026-10-01",tz="UTC")+0:3*86400
 chart<-render_fixture(history);value<-chart$data[[1]];points<-chart$data[[2]]
 stopifnot(length(chart$data)==2L,value$yaxis=="y",points$yaxis=="y2",points$xaxis=="x",
   value$mode=="lines",value$line$dash=="solid",points$line$dash=="solid",points$line$color=="#F28C28",value$line$color!=points$line$color,
   chart$layout$yaxis2$overlaying=="y",chart$layout$yaxis2$side=="right",identical(chart$layout$showlegend,FALSE),
   chart$layout$yaxis$tickfont$color==value$line$color,chart$layout$yaxis$title$font$color==value$line$color,
   chart$layout$yaxis2$tickfont$color==points$line$color,chart$layout$yaxis2$title$font$color==points$line$color,
   all(c(99e6,100e6)>chart$layout$yaxis$range[1]),all(c(99e6,100e6)<chart$layout$yaxis$range[2]),
   all(c(-2,4)>chart$layout$yaxis2$range[1]),all(c(-2,4)<chart$layout$yaxis2$range[2]))
})
check("postponed rounds connect chronologically with score provenance intact",{
 history<-combined
 history$points$date<-as.POSIXct(c("2026-10-20","2026-10-01","2026-10-10","2026-10-10"),tz="UTC")
 # Input order and round order both differ from chronological order.
 history$points<-history$points[c(4,1,3,2),]
 chart<-render_fixture(history);trace<-chart$data[[2]]
 stopifnot(identical(as.character(trace$x),fm_plot_datetime(sort(history$points$date))),
   identical(as.numeric(trace$y),c(0,-2,3,4)),
   all(startsWith(trace$text,paste0("Round ",c(2,3,4,1),"<br>"))),
   grepl("provisional",trace$text[3],fixed=TRUE),trace$line$dash=="solid")
})
check("dense histories label the first and fifteenth with smaller date text",{
 history<-known
 history$valuation<-data.frame(recorded_at=format(as.POSIXct("2026-07-01",tz="UTC")+0:123*86400,"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),value=22e6+sin(0:123/12)*1e6)
 history$points$date<-as.POSIXct("2026-07-01",tz="UTC")+0:3*7*86400
 chart<-render_fixture(history)
 stopifnot(chart$layout$xaxis$tickmode=="array",chart$layout$xaxis$tickfont$size==10,
   identical(as.character(chart$layout$xaxis$tickvals),c("2026-07-01","2026-07-15","2026-08-01","2026-08-15",
     "2026-09-01","2026-09-15","2026-10-01","2026-10-15","2026-11-01","2026-11-15")),
   is.null(chart$layout$xaxis$dtick),chart$layout$xaxis$tickformat=="%d %b",chart$layout$xaxis$tickangle==90,
   identical(chart$layout$xaxis$showgrid,FALSE),identical(chart$layout$showlegend,FALSE),
   length(chart$data[[1]]$y)==124L,grepl("2026",chart$data[[1]]$text[1],fixed=TRUE))
})
check("undated scores stay visible with an explicitly independent round axis",{
 chart<-render_fixture(combined)
 stopifnot(chart$data[[2]]$xaxis=="x2",identical(as.numeric(chart$data[[2]]$x),as.numeric(1:4)),
   chart$layout$xaxis2$overlaying=="x",!length(chart$layout$annotations),
   chart$layout$xaxis2$tickfont$color==chart$data[[2]]$line$color,
   chart$layout$xaxis2$title$font$color==chart$data[[2]]$line$color,
   grepl("separate round axis", player_round_history_notice(combined,list(rounds=list(),unassociated=data.frame(),
     dates=combined$points$date),list(status="ok")),fixed=TRUE))
})
check("existing scoped match dates align points without a new read",{
 history<-combined;history$context<-list(player_id="p",season="2026",competition_id="league")
 history$points$source<-"futmondo"
 matches<-data.frame(round=1:4,occurred_at=paste0("2026-10-0",1:4,"T12:00:00Z"),season="2026",competition_id="league")
 dates<-player_trend_points_dates(history$points,build_player_recent_rounds(history$points,matches,history$context)$rounds)
 stopifnot(identical(as.numeric(dates),as.numeric(fm_time(matches$occurred_at))))
 matches$season<-"2025";stopifnot(all(is.na(player_trend_points_dates(history$points,
   build_player_recent_rounds(history$points,matches,history$context)$rounds))))
})
check("calendar match identity repairs stale league metadata without accepting conflicting fixtures", {
 ctx<-list(account_id="account",championship_id="championship",competition_id="domestic",season="2026-2027")
 fixture<-function(id, round, time="2026-09-13T14:15:00Z") {
   list(id=id,r=round,info=list(date=time),h=list(name="Home",id="h",score=2),a=list(name="Away",id="a",score=1))
 }
 native<-fixture("match-5",5)
 absent_date<-fixture("match-6",6,"")
 wrong_comp<-fixture("match-7",7);wrong_comp$competitionId<-"foreign"
 wrong_season<-fixture("match-8",8);wrong_season$season<-"2025-2026"
 duplicate<-fixture("ambiguous-match",10)
 answer<-list(league=list(`_id`="domestic",season="2025-2026"),matches=list(
   native,absent_date,wrong_comp,wrong_season,duplicate,fixture("outside",5)))
 calendar<-lapply(5:11,function(i)list(`_id`=paste0("round-",i),
   matches=list(fixture(if(i>=10)"ambiguous-match"else paste0("match-",i),i))))
 schedule<-data.frame(round_id=paste0("round-",5:11),round_number=5:11,is_finished=c(rep(TRUE,6),FALSE))
 attr(schedule,"fixture_calendar")<-list(status="ok",scope=ctx,observed_at=Sys.time()-300,valid_until=as.numeric(Sys.time())-60,matches=league_fixture_matches(calendar))
 evidence<-player_card_fixture_evidence(answer,schedule,ctx)
 repaired<-evidence$matches[evidence$matches$season==ctx$season,]
 stopifnot(identical(repaired$match_id,c("match-6","match-5")),
   all(repaired$occurred_at=="2026-09-13T14:15:00Z"))
 scores<-data.frame(round_number=c(5,9,11),points=c(-2,0,3))
 grouped<-build_player_recent_rounds(scores,evidence$matches,ctx)
 dates<-player_trend_points_dates(scores,grouped$rounds,evidence$dates)
 stopifnot(identical(grouped$rounds[[3]]$fixtures$match_id,"match-5"),
   identical(attr(dates,"round_calendar"),c(FALSE,TRUE,FALSE)),
   dates[1]==dates[2],is.na(dates[3]))
 for (field in names(ctx)) {
   foreign<-ctx;foreign[[field]]<-"wrong"
   blocked<-player_card_fixture_evidence(answer,schedule,foreign)
   stopifnot(!nrow(blocked$dates),identical(blocked$matches,normalize_player_matches(answer)))
 }
})
check("stored undated final scores remain available",{rows<-data.frame(round=c(1,2,3),points=c(0,-2,5),score_status=c("final","final","provisional"));trace<-player_match_points_trace(rows);stopifnot(trace$has_points,identical(trace$points_df$points,c(0,-2)),all(is.na(trace$points_df$date)))})
check("card exposes refresh and one combined chart",{html<-as.character(selected_player_UI("card"));stopifnot(grepl("refresh_history",html,fixed=TRUE),!grepl("Smart Bid|fis_panel|smart_bid_widget|player_chart_metric",html))})
check("shared plot theme preserves series colors and retains neutral defaults",{
 chart<-plotly::plotly_build(fm_plot_layout(plotly::plot_ly(),xaxis=list(title="Date"),
   yaxis=list(color="#00ff88"),yaxis2=list(color="#F4C95D"),xaxis2=list(tickfont=list(color="#123456"))))$x
 stopifnot(chart$layout$xaxis$tickfont$color==fm_theme_tokens()$muted,
   chart$layout$yaxis$tickfont$color=="#00ff88",chart$layout$yaxis2$tickfont$color=="#F4C95D",
   chart$layout$xaxis2$tickfont$color=="#123456")
})
cat("Player trend plot:",n,"checks passed.\n")
