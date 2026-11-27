#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source("global.R"))
now <- Sys.time()
auth <- c(userid="xi-fixture",token="synthetic")
comp <- "504e4f584d8bec9a67000079"
ctx <- resource_context(auth,"league",competition_id=comp,season="2026",scoring_version="stats")
stamp <- function(x)format(x,"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
games <- league_fixture_matches(list(list(`_id`="round",matches=list(list(`_id`="fixture",st="P",
  h=list(`_id`="home",name="Atlético de Madrid"),a=list(`_id`="away",name="Athletic Club"),
  info=list(date=stamp(now+1800)))))))
fixtures <- games$rows
roster <- data.frame(id=c("f1","f12","f13","a1","who"),teamId=c("home","home","home","away","away"),
  name=c("Angel Garcia 1","Angel Garcia 12","Angel Garcia 13","Angel Garcia 1","unknown"),number=c(1,12,13,1,NA))
args <- list(championship_id="league")
rounds <- data.frame(round_id="round")
attr(rounds,"fixture_calendar") <- list(matches=games,status="ok",scope=ctx)
if("--wire" %in% commandArgs(TRUE)){
  cat(as.character(jsonlite::toJSON(list(descriptor=fm_worker_descriptor("get_club_lineups",args,ctx),
    rounds=fm_worker_encode(rounds),catalog=fm_worker_encode(roster)),auto_unbox=TRUE,na="null",null="null",digits=NA)))
  quit(save="no")
}
reads <- character()
saved <- function(fn,args,context){
  reads <<- c(reads,fn)
  switch(fn,get_championship_players=roster,get_player_full_profile=list(player=list(id=args$player_id),
    profile=list(name="With Fullname",dob="1999-01-01",number=33)),stop("unexpected read"))
}
checks <- 0
fetch <- function(m,players,clubs){
  checks <<- checks+1
  stopifnot(length(players)==5)
  if(checks==2)stopifnot(players[[5]]$full_name=="With Fullname",clubs$home$team_id==1)
  list(status="confirmed",observed=as.numeric(now),fixture_id=42,players=list(f1="starting",f12="benched",f13="absent"),
    clubs=list(home=list(team_id=1,season_id=77,links=list(f12=112))),unresolved=list("who"))
}
collect <- function(prior=NULL,clock=now,rows=fixtures,check=fetch)
  club_xi_collect(rows,prior,ctx,clock,saved,check)
player <- function(id)list(id=id,teamId="home")
badge <- function(f,id="f12",clock=now)club_xi_label(f,player(id),fixtures,clock)
result <- collect()
stopifnot(identical(reads,"get_championship_players"),checks==1,
  identical(badge(result)$text,"On club bench"),badge(result)$warning,badge(result,"f13")$warning,
  !badge(result,"f1")$warning,!badge(result,"missing")$warning,badge(result,clock=now+1600)$warning)
restore <- function(x)fm_worker_decode(jsonlite::fromJSON(fm_worker_json(fm_worker_encode(x)),simplifyVector=FALSE))
replayed <- collect(restore(result),now+90)
stopifnot(isTRUE(all.equal(replayed$matches,result$matches)),checks==1,length(reads)==1,abs(replayed$next_check-as.numeric(now)-180)<0.001)
retry <- collect(replayed,now+181)
stopifnot(checks==2,length(reads)==3,retry$profiles$who$number==33)
outage <- collect(retry,now+365,check=function(...)list(status="unavailable"))
stopifnot(!badge(outage)$warning,identical(outage$matches$fixture$observed,retry$matches$fixture$observed))
future <- fixtures;future$kickoff <- stamp(now+86400)
empty <- collect(result,rows=future)
stopifnot(!length(empty$matches),checks==2)
season <- future[rep(1,380),];season$match_id <- paste0("match",seq_len(nrow(season)))
season$kickoff <- stamp(now+86400+seq_len(nrow(season))*7200)
reads <- character()
elapsed <- system.time(for(i in 1:20)invisible(collect(rows=season)))[["elapsed"]]
stopifnot(!length(reads))
cat(sprintf("PASS verified, unresolved, stale and rescheduled XIs, profile reuse and durable replay. Quiet 380-fixture calendar %.1f ms/pass with zero reads.\n",elapsed*50))
