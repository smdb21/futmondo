options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source('global.R'))
now <- Sys.time()
roster <- data.frame(id = paste0('p', 1:18), name = paste('Player', 1:18),
  role = c('portero', rep('defensa', 6), rep('centrocampista', 6), rep('delantero', 5)),
  role2 = '', status = 'ok', available = TRUE)
arrivals <- data.frame(player_id = c('keeper2','keeper1','at-start','after-start','owned'),
  player_name = c('Second keeper', 'First keeper', 'At kickoff', 'After kickoff','Already owned'),
  role = rep('portero',5), status = 'ok', joins_at = now + c(30,30,60,61,10), timing_error = '')
roster$id[1] <- 'owned'
target <- list(id = 'round', starts_at = now + 60)
candidates <- lineup_arrival_candidates(list(status='observed',records=arrivals), roster, target)
stopifnot(isTRUE(candidates$complete), identical(candidates$rows$player_id,c('keeper1','keeper2')))
invalid <- arrivals
invalid$joins_at[1] <- NA
stopifnot(!lineup_arrival_candidates(list(status='observed',records=invalid),roster,target)$complete)
scores <- data.frame(id = c(roster$id,'keeper1','keeper2'), average = c(seq_len(nrow(roster)),50,60))
rules <- list(allowed=lineup_formations(),bench_enabled=TRUE,multiposition=TRUE,captain_enabled=TRUE)
preview <- lineup_schedule_preview(roster,candidates$rows,scores,rules,lineup_empty(),character(),FALSE,FALSE)
stopifnot(length(preview)==1L, tail(preview[[1]]$lineup$starters,1)=='keeper2',
  identical(preview[[1]]$due_at,fm_clause_stamp(now+30)))
unavailable <- candidates$rows
unavailable$available <- FALSE
immediate <- lineup_schedule_preview(roster,unavailable,scores,rules,lineup_empty(),character(),FALSE,FALSE)
stopifnot(length(immediate)==1L,is.null(immediate[[1]]$due_at),immediate[[1]]$player_id=='')
cat('PASS strict kickoff eligibility, already-owned filtering, deterministic simultaneous arrivals and unavailable players\n')

scarce <- roster[c(1,2:6,8:12,14:15),]
early <- candidates$rows[1,,drop=FALSE]
forward <- transform(early, player_id='new-forward', player_name='Incoming forward', role='delantero', joins_at=now+40)
coverage_scores <- rbind(scores,data.frame(id='new-forward',average=80))
rules$allowed <- '4-4-2'
stopifnot(is.null(lineup_schedule_preview(scarce,early,coverage_scores,rules,lineup_empty(),
  character(),FALSE,TRUE)))
strict <- lineup_schedule_preview(scarce,rbind(early,forward),coverage_scores,rules,lineup_empty(),
  character(),FALSE,TRUE)
stopifnot(length(strict)==1L,strict[[1]]$player_id=='new-forward',
  identical(strict[[1]]$due_at,fm_clause_stamp(forward$joins_at)),
  'keeper1'%in%strict[[1]]$lineup$starters,
  length(lineup_bench_coverage(lineup_arrival_roster(scarce,rbind(early,forward)),strict[[1]]$lineup$bench,TRUE))==3L)
late <- transform(forward,player_id='late-bench',joins_at=now+45)
unused <- transform(forward,player_id='unused',joins_at=now+50)
bench_scores <- rbind(coverage_scores,data.frame(id=c('late-bench','unused'),average=c(14.5,-10)))
plan <- lineup_schedule_preview(scarce,rbind(unused,early,late,forward),bench_scores,rules,lineup_empty(),
  character(),FALSE,TRUE)
stopifnot(length(plan)==1L,'late-bench'%in%plan[[1]]$lineup$bench,plan[[1]]$player_id=='late-bench',
  identical(plan[[1]]$due_at,fm_clause_stamp(late$joins_at)))
cat('PASS latest selected substitute sets the submission time even after full coverage is feasible\n')
rules$allowed <- lineup_formations()

runs <- replicate(20,system.time(lineup_schedule_preview(roster,candidates$rows,scores,
  rules,lineup_empty(),character(),FALSE,FALSE))[['elapsed']])
cat('18 owned + 2 incoming, all formations, one lineup median ms:', median(runs)*1000, '\n')
