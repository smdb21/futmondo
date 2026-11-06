#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(shiny);library(reactable)})
source("data_contracts.R")
source("Modules/Players_Table_Module.R")
players <- data.frame(id=c("p1","p2","p3"),`average.averageLastFive`=c(9,8,7),check.names=FALSE)
history <- data.frame(player_id=c(rep("p1",4),rep("p2",2)),round=c(1,2,3,4,3,4),points=c(2,4,8,10,0,6))
last_two <- add_recent_points_average(players,history,2)
stopifnot(identical(last_two$recent_points_avg,c(9,3,NA_real_)))
last_three <- add_recent_points_average(players,history,3)
stopifnot(isTRUE(all.equal(last_three$recent_points_avg[1],22/3)),is.na(last_three$recent_points_avg[2]),last_three$recent_points_count[2]==2L,is.na(last_three$recent_points_avg[3]))
last_five <- add_recent_points_average(players,history,5)
stopifnot(last_five$recent_points_avg[1]==9,last_five$recent_points_avg[2]==8,last_five$recent_points_avg[3]==7)
revised <- rbind(history,data.frame(player_id="p1",round=4,points=14))
# The matrix reader deduplicates revisions; the helper uses explicit rounds.
revised <- revised[!duplicated(revised[c("player_id","round")],fromLast=TRUE),]
stopifnot(add_recent_points_average(players,revised,1)$recent_points_avg[1]==14)
empty <- add_recent_points_average(players,data.frame(),4)
stopifnot(all(is.na(empty$recent_points_avg)))
ui_html <- as.character(players_table_UI("table",filter_by_active_clause=FALSE,
  filter_by_is_favorite=FALSE,filter_by_is_from_futmondo=FALSE))
stopifnot(grepl("recent_games_n",ui_html,fixed=TRUE),grepl("Average points: latest rounds",ui_html,fixed=TRUE))
cat("RECENT POINTS AVERAGE: 6 passed / 0 failed\n")

# Finality describes confirmation, not whether a reported score can be averaged.
observed <- data.frame(player_id=rep("p1",3),round=c(8,10,9),
  points=c(7,18,19),score_status=c("unknown","unknown","provisional"))
stopifnot(isTRUE(all.equal(add_recent_points_average(players,observed,3)$recent_points_avg[1],44/3)))
signed<-data.frame(player_id=rep("p1",3),round=c(1,2,3),points=c(-2,0,8))
stopifnot(add_recent_points_average(players,signed,3)$recent_points_avg[1]==2)
sparse<-data.frame(player_id="p1",round=c(3,4,6,7,8,9,10),points=c(1,-2,16,3,7,19,18))
coverage<-add_recent_points_average(players,sparse,8)
stopifnot(is.na(coverage$recent_points_avg[1]),coverage$recent_points_count[1]==7L,
  coverage$recent_points_required[1]==8L)
cat("RECENT OBSERVED SCORES: 3 additional checks passed\n")

# Display stays numeric and sortable; unavailable values are blank, never zero.
average_column <- players_table_recent_average_column(4L)
stopifnot(is.numeric(last_three$recent_points_avg),identical(average_column$sortNALast,TRUE),
  inherits(average_column$cell,"JS_EVAL"),identical(average_column$name,"Avg Last 4"))
# Evaluate the actual browser formatter with fixed signed/missing known answers.
formatter_script<-paste0("const React = {createElement: (tag, props, text) => text}; const cell = ",as.character(average_column$cell),"; const values = [null, 0, -2.125, 3.25]; const expected = ['', '0', '-2.1', '3.3']; if (JSON.stringify(values.map(value => cell({value}))) !== JSON.stringify(expected)) process.exit(1);")
stopifnot(system2("node",c("-e",shQuote(formatter_script)))==0L)
cat("NUMERIC AVERAGE DISPLAY: 1 additional check passed\n")

# N5 used to work through the scalar fallback while N2 ignored source scores.
source_players <- data.frame(id=c("p1","p2","p3","p4","p5"),
  average.fitness=c("2,5,12,12,6","4,3,2,-2,0","1,2,3,NA,8","1,2,3,8,","9"),
  average.averageLastFive=c(7.4,1.4,NA,NA,9))
source_two <- add_recent_points_average(source_players,data.frame(),2)
stopifnot(identical(source_two$recent_points_avg,c(9,-1,NA_real_,NA_real_,NA_real_)),
  identical(source_two$recent_points_count,c(2L,2L,0L,0L,0L)),
  identical(source_two$recent_points_status,c("reported","reported","missing","missing","missing")),
  all(is.na(add_recent_points_average(source_players,data.frame(),6)$recent_points_avg)))
# Only the requested suffix needs scores; unavailable slots never shift position.
stopifnot(add_recent_points_average(data.frame(id="p",average.fitness="NA,2,3,8,4"),data.frame(),2)$recent_points_avg==6,
  is.na(add_recent_points_average(data.frame(id="p",average.fitness="2,3,broken,8,4"),data.frame(),2)$recent_points_avg),
  is.na(add_recent_points_average(data.frame(id="p",average.averageLastFive=7),data.frame(),2)$recent_points_avg))
# Full explicit history takes priority over the unlabelled source sequence.
known <- data.frame(player_id="p1",round=9:10,points=c(20,30))
preferred <- add_recent_points_average(source_players,known,2)
stopifnot(preferred$recent_points_avg[1]==25,preferred$recent_points_status[1]=="complete",
  preferred$recent_points_avg[2]==-1)
cat("SOURCE RECENT WINDOW: 3 additional known-answer checks passed\n")
