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
keepers <- vapply(preview,function(stage)tail(stage$lineup$starters,1),character(1))
stopifnot(identical(keepers,c('owned','keeper1','keeper2')),is.null(preview[[1]]$due_at),
  identical(preview[[2]]$due_at,preview[[3]]$due_at),
  length(unique(vapply(preview,function(stage)lineup_identity(stage$lineup),character(1))))==3)
unavailable <- candidates$rows
unavailable$available <- c(FALSE,TRUE)
preview <- lineup_schedule_preview(roster,unavailable,scores,rules,lineup_empty(),character(),FALSE,FALSE)
stopifnot(identical(vapply(preview,function(stage)tail(stage$lineup$starters,1),character(1)),c('owned','owned','keeper2')))
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
cat('PASS early stages without full cover are skipped, and the first feasible stage keeps earlier signings\n')
rules$allowed <- lineup_formations()

runs <- replicate(20,system.time(lineup_schedule_preview(roster,candidates$rows,scores,
  rules,lineup_empty(),character(),FALSE,FALSE))[['elapsed']])
cat('18 owned + 2 incoming, all formations, three stages median ms:', median(runs)*1000, '\n')
