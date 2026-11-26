#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source("global.R"))
stamp <- function(day) ifelse(is.na(day), "", sprintf("2026-12-%02dT18:00:00Z",day))
game <- function(id,day,status) list(`_id`=id, st=status, info=list(date=stamp(day)),
  h=list(id="home",name="Home"), a=list(id="away",name="Away"))
round_record <- function(number,status,...) list(`_id`=paste0("r",number),number=number,status=status,matches=list(...))
catalog <- function(records,day) {
  ids <- vapply(records,function(r)r$`_id`,character(1))
  base <- data.frame(round_id=ids, round_number=vapply(records,function(r)r$number,numeric(1)),
    status=vapply(records,function(r)r$status,character(1)),begin_process="")
  base$is_finished <- base$status=="closed"
  matches <- league_fixture_matches(records)
  metadata <- list(rows=base, fixture_matches=matches, fixture_rounds=award_fixture_calendar(records))
  attr(metadata,"observed_at") <- fm_time(stamp(day))
  rounds <- award_join_calendar(base,metadata)$rounds
  attr(rounds,"fixture_calendar") <- list(matches=matches,status="ok",scope=list(current_round_id="r23"))
  attr(rounds,"fixture_windows") <- lineup_fixture_windows(rounds,matches)
  rounds
}
at <- function(rounds,day) home_round_context(rounds,fm_time(stamp(day)))
read_round <- function(record,number,status) {
  stopifnot(identical(record$current,list(round_number=number,status=status)))
}

# Dated fixtures, source round numbers and the editable lineup marker each
# follow a different order. Exercise the same join used for the worker snapshot.
early <- round_record(23,"closed",game("early",2,"F"),game("rest",26,"P"))
regular <- round_record(15,"running",game("fri",4,"F"),game("sun",6,"P"))
upcoming <- round_record(16,"next",game("next",12,"P"))
rounds <- catalog(list(early,upcoming,regular),5)
x <- at(rounds,5)
read_round(x,15,"In progress")
stopifnot(x$upcoming$round_number==16,x$upcoming$starts_at==fm_time(stamp(12)),x$changes_at==fm_time(stamp(6)))
regular$status <- "closed"
regular$matches[[2]]$st <- "F"
rounds <- catalog(list(early,upcoming,regular),7)
read_round(at(rounds,7),15,"Closed")
stopifnot(at(rounds,7)$upcoming$round_number==16)
replayed <- fm_worker_decode(jsonlite::fromJSON(fm_worker_json(fm_worker_encode(rounds)),simplifyVector=FALSE))
stopifnot(identical(at(rounds,7),at(replayed,7)))
cat("PASS an advanced round yields to the ordinary playing and then closed round across durable replay\n")

rounds <- catalog(list(early),3)
x <- at(rounds,3)
read_round(x,23,"Awaiting remaining fixtures")
stopifnot(x$upcoming$round_number==23, x$upcoming$starts_at==fm_time(stamp(26)), x$changes_at==x$upcoming$starts_at)
early$status <- "running"
rounds <- catalog(list(early),26)
read_round(at(rounds,26),23,"In progress")
early$matches[[2]]$st <- "F"
rounds <- catalog(list(early),27)
read_round(at(rounds,27),23,"Awaiting round closure")
early$status <- "closed"
rounds <- catalog(list(early),27)
read_round(at(rounds,27),23,"Closed")
stopifnot(is.null(at(rounds,27)$upcoming))
cat("PASS an isolated postponed kickoff reappears at its actual date; final games and official closure are distinct\n")

undated <- round_record(14,"running",game("played",1,"F"),game("postponed",NA_integer_,"P"))
rounds <- catalog(list(undated,upcoming),3)
read_round(at(rounds,3),14,"Awaiting remaining fixtures")
stopifnot(at(rounds,3)$upcoming$round_number==16)
upcoming$status <- "closed"
upcoming$matches[[1]]$st <- "F"
rounds <- catalog(list(undated,upcoming),13)
read_round(at(rounds,13),16,"Closed")
cat("PASS a postponed match without a date neither acquires a kickoff nor holds Home on an older round\n")

# In an overlapping schedule one closed matchday can sit inside another.
# A stale 'running' flag must lose precedence after its own window passes.
rounds <- catalog(list(
  round_record(10,"running",game("a",1,"F"),game("b",3,"P"),game("c",6,"P")),
  round_record(11,"closed",game("d",2,"F"),game("e",4,"F"))),4)
read_round(at(rounds,5),10,"In progress")
read_round(at(rounds,8),11,"Closed")

legacy <- data.frame(round_id=c("a","b"),round_number=c(5,6),
  begin_process=c(stamp(4),""),is_finished=c(TRUE,FALSE),status=c("closed","running"))
read_round(at(legacy,8),6,"In progress")
older_running <- legacy
older_running$round_number[2] <- 4
read_round(at(older_running,8),5,"Closed")
legacy$begin_process[1] <- ""
legacy$status[2] <- "future"
read_round(at(legacy,8),5,"Closed")
legacy$is_finished[1] <- FALSE
legacy$status[1] <- "next"
legacy$begin_process[1] <- stamp(4)
read_round(at(legacy,8),5,"Awaiting round status")
stopifnot(is.null(at(data.frame(),8)$current),is.null(at(legacy,3)$current))
ambiguous <- data.frame(round_id=c("a","b"),round_number=c(5,6),begin_process="",is_finished=TRUE,status="closed")
stopifnot(is.null(at(ambiguous,8)$current))
cat("PASS overlapping matchdays, legacy calendars and unknown status keep independent chronological and status evidence\n")

season <- lapply(1:38,function(i) {
  first <- fm_time("2026-08-01T18:00:00Z")+(i-1L)*7*86400
  games <- lapply(1:10,function(j) {
    value <- game(paste(i,j,sep="-"),1,if (i<14L || (i==14L && j==1L)) "F" else "P")
    value$info$date <- format(first+(j-1L)*3*3600,"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
    value
  })
  list(`_id`=paste0("r",i),number=i,status=if (i<14L) "closed" else if (i==14L) "running" else "future",matches=games)
})
full <- catalog(season,3)
now <- fm_time(season[[14]]$matches[[2]]$info$date)
read_round(home_round_context(full,now),14,"In progress")
elapsed <- system.time(for (i in 1:100) home_round_context(full,now))[["elapsed"]]/100
cat(sprintf("MEASURE 38 rounds / 380 matches: %.2f ms per warm Home projection, no I/O\n",1000*elapsed))
