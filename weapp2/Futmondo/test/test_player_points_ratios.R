#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
selected_player_Server<-function(...)NULL
scores<-shiny::reactiveVal(data.frame(player_id='p',round=1:3,points=c(5,10,15)))
recent_player_points_source<-function(...)list(history=function()scores(),status=function()NULL)
settle<-function(session){for(i in seq_len(5)){later::run_now(0);session$flushReact()}}
rows<-data.frame(id='p',name='Player',value=2e6,price=9e6,clause_price=4e6)
for(basis in c('value','clause_price')){
 shiny::testServer(players_table_Server,args=list(players_table_RV=shiny::reactive(rows),user_teams_RV=shiny::reactive(NULL),points_ratio_basis=basis),{
  session$setInputs(recent_games_n=3L);settle(session)
  d<-table_display_data();divisor<-if(basis=='value')2 else 4
  stopifnot(identical(names(d)[1:3],c('name','recent_points_avg','recent_points_ratio')),
    is.numeric(d$recent_points_ratio),d$recent_points_avg==10,d$recent_points_ratio==10/divisor,!is.null(output$players_table))
  session$setInputs(recent_games_n=2L);settle(session)
  stopifnot(table_display_data()$recent_points_ratio==12.5/divisor)
  previous_schema<-table_schema()
  scores(data.frame(player_id='p',round=1:3,points=c(5,10,20)));settle(session)
  stopifnot(table_display_data()$recent_points_ratio==15/divisor,identical(table_schema(),previous_schema))
 });scores(data.frame(player_id='p',round=1:3,points=c(5,10,15)))
}
x<-players_table_points_ratio(data.frame(recent_points_avg=c(10,0,-2,NA,10,10,10),value=c(2e6,2e6,2e6,2e6,0,NA,-1)),'value')
stopifnot(identical(x$recent_points_ratio,c(5,0,-1,NA_real_,NA_real_,NA_real_,NA_real_)),
 length(players_table_points_ratio(data.frame(recent_points_avg=numeric(),value=numeric()),'value')$recent_points_ratio)==0L)
cat('Player points ratios: both page bases, N changes, in-place history updates and invalid denominators passed.\n')
