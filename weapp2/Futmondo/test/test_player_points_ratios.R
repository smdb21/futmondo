#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
selected_player_Server<-function(...)NULL
scores<-shiny::reactiveVal(data.frame(player_id='p',round=1:3,points=c(5,10,15)))
recent_player_points_source<-function(...)list(history=function()scores(),status=function()NULL)
settle<-function(session){for(i in seq_len(5)){later::run_now(0);session$flushReact()}}
rows<-data.frame(id='p',name='Player',value=2e6,price=9e6,clause_price=4e6,change=250000)
for(basis in c('value','clause_price')){
 player_rows<-shiny::reactiveVal(rows)
 shiny::testServer(players_table_Server,args=list(players_table_RV=player_rows,user_teams_RV=shiny::reactive(NULL),points_ratio_basis=basis),{
  session$setInputs(recent_games_n=3L);settle(session)
  d<-table_display_data();divisor<-if(basis=='value')2 else 4
  stopifnot(identical(names(d)[1:4],c('name','recent_points_avg','recent_points_ratio','recent_points_trend')),
    is.numeric(d$recent_points_ratio),is.numeric(d$recent_points_trend),d$recent_points_avg==10,
    d$recent_points_ratio==10/divisor,d$recent_points_trend==2500000,!is.null(output$players_table))
  session$setInputs(recent_games_n=2L);settle(session)
  stopifnot(table_display_data()$recent_points_ratio==12.5/divisor,table_display_data()$recent_points_trend==3125000)
  previous_schema<-table_schema()
  scores(data.frame(player_id='p',round=1:3,points=c(5,10,20)));settle(session)
  stopifnot(table_display_data()$recent_points_ratio==15/divisor,table_display_data()$recent_points_trend==3750000,
    identical(table_schema(),previous_schema))
  changed<-player_rows();changed$change<--100000;player_rows(changed);settle(session)
  stopifnot(table_display_data()$recent_points_ratio==15/divisor,table_display_data()$recent_points_trend== -1500000)
 });scores(data.frame(player_id='p',round=1:3,points=c(5,10,15)))
}
x<-players_table_points_ratio(data.frame(recent_points_avg=c(10,0,-2,NA,10,10,10),value=c(2e6,2e6,2e6,2e6,0,NA,-1)),'value')
stopifnot(identical(x$recent_points_ratio,c(5,0,-1,NA_real_,NA_real_,NA_real_,NA_real_)),
 length(players_table_points_ratio(data.frame(recent_points_avg=numeric(),value=numeric()),'value')$recent_points_ratio)==0L)
cat('Player points metrics: both page bases, raw-euro products, N/source changes, in-place history updates and invalid denominators passed.\n')

# Independent known answers retain signed/zero arithmetic and never manufacture
# a zero from an unknown operand, even when the other operand is zero.
products<-players_table_points_trend(data.frame(
 recent_points_avg=c(10,0,-2,NA,1,2,-3,NaN,Inf,5,0,NA,1e308),
 change=c(250000,250000,250000,250000,0,-100000,-100000,1,1,NA,NA,0,1e308)))
stopifnot(identical(products$recent_points_trend,
 c(2500000,0,-500000,NA_real_,0,-200000,300000,rep(NA_real_,6))),
 all(is.na(players_table_points_trend(data.frame(recent_points_avg=c(1,1),change=c(Inf,-Inf)))$recent_points_trend)),
 is.na(players_table_points_trend(data.frame(recent_points_avg=10))$recent_points_trend),
 is.na(players_table_points_trend(data.frame(change=250000))$recent_points_trend),
 identical(players_table_points_trend(data.frame(recent_points_avg=numeric(),change=numeric()))$recent_points_trend,numeric()))
shiny::testServer(players_table_Server,args=list(players_table_RV=shiny::reactive(rows),user_teams_RV=shiny::reactive(NULL)),{
 session$setInputs(recent_games_n=3L);settle(session)
 stopifnot(!any(c('recent_points_ratio','recent_points_trend')%in%names(table_display_data())),
   !any(c('recent_points_ratio','recent_points_trend')%in%names(table_color_meta()$playerMetricRanges)))
})
cat('Trend product known answers: positive/signed/zero, missing/absent/empty/nonfinite/overflow, and opt-in scope passed.\n')

# Reproduce N5 -> N2 when the table has source scores but numbered history is empty.
scores(data.frame())
rows <- data.frame(id='p',name='Player',value=2e6,clause_price=4e6,
  change=200000,average.fitness='2,5,12,12,6',average.averageLastFive=7.4)
for(basis in c('value','clause_price')){
 shiny::testServer(players_table_Server,args=list(players_table_RV=shiny::reactive(rows),user_teams_RV=shiny::reactive(NULL),points_ratio_basis=basis),{
  session$setInputs(recent_games_n=5L);settle(session)
  stopifnot(table_display_data()$recent_points_avg==7.4,table_display_data()$recent_points_trend==1480000)
  session$setInputs(recent_games_n=2L);settle(session)
  divisor<-if(basis=='value')2 else 4
  stopifnot(table_display_data()$recent_points_avg==9,table_display_data()$recent_points_ratio==9/divisor,
    table_display_data()$recent_points_trend==1800000)
  widget<-jsonlite::fromJSON(output$players_table,simplifyVector=FALSE)$x$tag$attribs
  data<-if(is.character(widget$data))jsonlite::fromJSON(widget$data)else widget$data
  stopifnot(data$recent_points_avg==9,data$recent_points_ratio==9/divisor,data$recent_points_trend==1800000)
  session$setInputs(recent_games_n=6L);settle(session)
  stopifnot(is.na(table_display_data()$recent_points_avg),is.na(table_display_data()$recent_points_ratio),
    is.na(table_display_data()$recent_points_trend))
  session$setInputs(recent_games_n=2L);settle(session)
  scores(data.frame(player_id='p',round=9:10,points=c(20,30)));settle(session)
  stopifnot(table_display_data()$recent_points_avg==25,table_display_data()$recent_points_ratio==25/divisor,
    table_display_data()$recent_points_trend==5000000)
 });scores(data.frame())
}
cat('Source recent scores: N5 -> N2 widget values and both ratios, unavailable N6, and explicit-history replacement passed.\n')

# Wider windows use all eight labelled source scores, including zero and negatives.
scores(data.frame(player_id='p',round=1:10,points=c(99,99,1,-2,0,16,3,7,19,18)))
rows <- data.frame(id='p',name='Player',role='Defender',value=2e6,clause_price=4e6,
  change=200000,average.fitness='16,3,7,19,18',average.averageLastFive=12.6)
for(basis in c('value','clause_price')){
 shiny::testServer(players_table_Server,args=list(players_table_RV=shiny::reactive(rows),user_teams_RV=shiny::reactive(NULL),points_ratio_basis=basis),{
  divisor<-if(basis=='value')2 else 4
  for(n in c(5L,2L,8L,2L,8L)){
   session$setInputs(recent_games_n=n);settle(session)
   expected<-switch(as.character(n),'5'=12.6,'2'=18.5,'8'=7.75)
   stopifnot(table_display_data()$recent_points_avg==expected,
    table_display_data()$recent_points_ratio==expected/divisor,
    table_display_data()$recent_points_trend==switch(as.character(n),'5'=2520000,'2'=3700000,'8'=1550000),
    identical(names(table_display_data())[1:5],c('name','role','recent_points_avg','recent_points_ratio','recent_points_trend')))
  }
 })
}
cat('Eight-round known answer: 7.75 with signed/zero scores, both ratio bases and Position after Name passed.\n')
