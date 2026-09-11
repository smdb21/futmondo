#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(shiny);library(reactable)})
source("Modules/Players_Table_Module.R")
players <- data.frame(id=c("p1","p2","p3"),`average.averageLastFive`=c(9,8,7),check.names=FALSE)
history <- data.frame(player_id=c(rep("p1",4),rep("p2",2)),round=c(1,2,3,4,3,4),points=c(2,4,8,10,0,6))
last_two <- add_recent_points_average(players,history,2)
stopifnot(identical(last_two$recent_points_avg,c(9,3,NA_real_)))
last_three <- add_recent_points_average(players,history,3)
stopifnot(isTRUE(all.equal(last_three$recent_points_avg[1],22/3)),last_three$recent_points_avg[2]==3,is.na(last_three$recent_points_avg[3]))
last_five <- add_recent_points_average(players,history,5)
stopifnot(last_five$recent_points_avg[1]==6,last_five$recent_points_avg[2]==3,last_five$recent_points_avg[3]==7)
revised <- rbind(history,data.frame(player_id="p1",round=4,points=14))
# The history reader normally deduplicates revisions; the helper consumes its finalized rows in order.
revised <- revised[!duplicated(revised[c("player_id","round")],fromLast=TRUE),]
stopifnot(add_recent_points_average(players,revised,1)$recent_points_avg[1]==14)
empty <- add_recent_points_average(players,data.frame(),4)
stopifnot(all(is.na(empty$recent_points_avg)))
ui_html <- as.character(players_table_UI("table",filter_by_active_clause=FALSE,
  filter_by_is_favorite=FALSE,filter_by_is_from_futmondo=FALSE))
stopifnot(grepl("recent_games_n",ui_html,fixed=TRUE),grepl("Average points: latest games",ui_html,fixed=TRUE))
cat("RECENT POINTS AVERAGE: 6 passed / 0 failed\n")
