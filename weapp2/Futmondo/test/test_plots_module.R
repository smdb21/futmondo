#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
source('global.R')
checks <- 0L
check <- function(label, expr) { force(expr); checks <<- checks + 1L; cat('[PASS]', label, '\n') }
check('owned real-team aggregation excludes free agents and deduplicates IDs', {
  p <- data.frame(id=c('p1','p1','p2','p3','p4'), user_team_id=c('u1','u1','','u2','u3'), teamId=c('a','a','b','','b'), team=c('Alpha','Alpha','Beta','','Beta'), stringsAsFactors=FALSE)
  d <- plots_owned_by_real_team(p)
  stopifnot(identical(d$real_team, c('Alpha','Beta')), identical(d$owned_players, c(1L,1L)))
})
check('owned real-team aggregation is descending with stable name tie-break', {
  p <- data.frame(id=c('a1','a2','b1'), userteamId='owner', teamId=c('a','a','b'), team=c('Alpha','Alpha','Beta'))
  d <- plots_owned_by_real_team(p)
  stopifnot(identical(d$real_team, c('Alpha','Beta')), identical(d$owned_players, c(2L,1L)))
})
check('slider handles follow published round identities as the domain changes', {
  rounds <- c(14,23,15,15.2)
  stopifnot(identical(plots_round_handles(rounds, NULL), c(0L,5L)),
    identical(plots_round_handles(rounds,c('23','15')),c(2L,3L)),
    identical(plots_round_handles(c(12,rounds),c('23','15')),c(3L,4L)),
    identical(plots_round_handles(c(rounds,16),c('15.2','Latest published')),c(4L,6L)),
    identical(plots_round_handles(23,c('23','23')),c(1L,1L)),
    identical(plots_round_handles(rounds,c('15','23')),c(0L,5L)))
})
check('plots UI exposes five factual chart tabs and one points/ranks switch', {
  html <- as.character(plots_UI('plots'))
  stopifnot(!grepl('rank_plot|rank_range|Rank progression',html), grepl('data-value="Points"', html, fixed=TRUE))
  pages <- fm_navigation_pages()
  stopifnot(identical(pages$label[pages$tab == "plots"], "Trends"), grepl("<h2>Trends</h2>", html, fixed=TRUE))
  for (id in c('standings_plot','values_plot','cash_plot','transfers_plot','show_ranks','owned_club_plot')) stopifnot(grepl(id, html, fixed=TRUE))
})
check('round totals preserve subrounds, revisions, negative and missing points', {
  rows <- data.frame(team_id=c(rep('a',5),rep('b',3)), team_name=c(rep('Alpha',5),rep('Beta',3)),
    round=c(6,6.2,7,8,6.2,6,7,8), points=c(10,99,-3,0,5,8,4,2))
  d <- plots_round_totals(rows)
  a <- d[d$team_id=='a',]; b <- d[d$team_id=='b',]
  stopifnot(identical(a$round,c(6,6.2,7,8)), identical(a$points,c(10,15,12,12)),
    identical(b$points,c(8,NA_real_,NA_real_,NA_real_)), identical(a$rank,c(1,NA_real_,NA_real_,NA_real_)))
  interval <- plots_interval_points(d[d$round %in% c(7,8),])
  stopifnot(identical(interval$points[interval$team_name=='Beta'],c(0,4,6)),
    identical(interval$points[interval$team_name=='Alpha'],c(0,-3,-3)))
  p <- plotly::plotly_build(plots_round_chart(d))
  stopifnot(p$x$layout$xaxis$type=='category', identical(unlist(p$x$layout$xaxis$categoryarray),c('6','6.2','7','8')),
    length(p$x$data)==2, identical(attr(d, 'missing_rounds'), 6.2))
})
check('valuation date axis and hover omit time and year', {
  d <- data.frame(teamname='Alpha', recorded_at=c('2026-11-12T12:00:00Z','2026-11-13T12:00:00Z'),team_value=c(100,110))
  p <- plotly::plotly_build(plots_history(d,'team_value','Squad valuation (EUR)'))
  stopifnot(p$x$layout$xaxis$type=='date',p$x$layout$xaxis$tickformat=='%b-%d',
    grepl('Nov-12',p$x$data[[1]]$hovertemplate[1],fixed=TRUE),
    !grepl('2026|12:00',p$x$data[[1]]$hovertemplate[1]))
})
check('ownership chart has source crests, counts, blue bars and toolbar clearance', {
  p <- data.frame(id=c('a','b','c'),user_team_id='owner',teamId=c('a','a','b'),team=c('Alpha','Alpha','Beta'),logo=c('fixture-a','fixture-a','fixture-b'))
  chart <- plotly::plotly_build(plots_owned_chart(p))
  stopifnot(length(chart$x$layout$images)==2, chart$x$layout$margin$t>=60,
    chart$x$data[[1]]$marker$color=='#608BC1',chart$x$data[[1]]$textposition=='outside',
    identical(as.integer(chart$x$data[[1]]$text),c(1L,2L)))
})
check('chart controls have descriptions, wider metric column and no round inspector', {
  html <- as.character(plots_UI('plots'))
  stopifnot(!grepl('rank_round|Inspect one round',html),grepl('col-sm-8',html),
    grepl('transfer-only balance estimates',html),grepl('subround',html))
})
check('catalog coverage and absent teams never become invented totals', {
 rows <- data.frame(team_id=c('a','a','b'),team_name=c('Alpha','Alpha','Beta'),round=c(1,3,NA_real_),points=c(10,5,NA_real_))
 attr(rows,'finished_rounds') <- c(1,2,3)
 totals <- plots_round_totals(rows)
 stopifnot(identical(totals$round,c(1,2,3,1,2,3)),identical(totals$points,c(10,NA_real_,NA_real_,NA_real_,NA_real_,NA_real_)))
})
check('early J23 skips future rounds and preserves the chronological cumulative curve', {
 rounds <- data.frame(round_id=paste0('r',14:23),round_number=14:23,is_finished=c(TRUE,TRUE,rep(NA,7),FALSE),
   status=c('closed','closed',rep('future',7),'V'),partially_played=c(rep(FALSE,9),TRUE),
   first_kickoff=format(as.Date('2026-12-01')+c(0,4,6:12,2)))
 scores <- data.frame(team_id='a',team_name='Alpha',round=c(14,14.2,23,15),points=c(8,-2,0,4))
 rounds$latest_fixture_at <- as.numeric(as.POSIXct(rounds$first_kickoff,tz='UTC'))
 rounds$latest_fixture_at[rounds$round_number==23] <- as.numeric(as.POSIXct('2027-02-07',tz='UTC'))
 attr(scores,'played_rounds') <- plots_round_numbers(scores,rounds)
 stopifnot(identical(attr(scores,'played_rounds'),c(14,14.2,23,15)))
 totals <- plots_round_totals(scores)
 chart <- plotly::plotly_build(plots_round_chart(totals))
 stopifnot(identical(totals$points,c(8,6,6,10)),identical(totals$rank,rep(1,4)),
   identical(unlist(chart$x$layout$xaxis$categoryarray),c('14','14.2','23','15')))
 scores$points[scores$round%in%c(15,23)] <- NA_real_
 stopifnot(identical(plots_ranking_targets(scores,rounds)$round_id,'r15'))
})
check('saved rankings restore scores and distinguish nonparticipation from unknown', {
  rows <- data.frame(team_id=c('a','a','b','b','c'), team_name=c('Alpha','Alpha','Beta','Beta','Gamma'),
    round=c(1,2,1,2,2), points=c(10,5,8,NA,20))
  ranking <- function(id, number, teams, scores, status='V') {
    answer <- list(metadata=list(status=status),ranking=lapply(seq_along(teams),function(i)
      list(id=teams[i],name=teams[i],points=scores[i])))
    list(round_id=id,round_number=number,answer=answer,rows=classification_ranking_rows(answer,id,number))
  }
  native <- data.frame(team_id=c('a','b','a','c'), round_id=rep(c('r1','r2'),each=2),
    points=c(10,8,5,20), status='closed')
  r1 <- data.frame(round_id='r1',round_number=1,is_finished=NA)
  r2 <- data.frame(round_id='r2',round_number=2,is_finished=TRUE)
  stored_1 <- ranking('r1',1,c('a','b'),c(10,8),'P')
  stored_2 <- ranking('r2',2,c('a','b','c'),c(5,0,20))
  recovered <- list(plots_saved_round_ranking(stored_1,r1,native), plots_saved_round_ranking(stored_2,r2,native))
  stopifnot(all(vapply(recovered,nrow,integer(1))==c(2L,3L)))
  phase <- stored_2
  phase$answer$metadata$status <- 'closed'
  stopifnot(is.null(plots_saved_round_ranking(phase,transform(r2,is_finished=FALSE),native)),
    is.null(plots_saved_round_ranking(phase,transform(r2,partially_played=TRUE),native)))
  result <- plots_round_totals(rows,recovered)
  stopifnot(identical(result$points,c(10,15,8,8,NA_real_,20)),
    identical(result$rank,c(1,2,2,3,NA_real_,1)), !length(attr(result,'missing_rounds')))
  stopifnot(is.null(plots_saved_round_ranking(stored_1,transform(r1,round_id='foreign'),native)),
    is.null(plots_saved_round_ranking(stored_1,r1,transform(native,status='active'))),
    is.null(plots_saved_round_ranking(structure(stored_1,worker_completeness='partial'),r1,native)),
    is.null(plots_saved_round_ranking(ranking('r1',1,c('a','b'),c(9,8)),r1,native)),
    is.null(plots_saved_round_ranking(ranking('r1',1,c('a','b'),c(10,NA)),r1,native)),
    is.null(plots_saved_round_ranking(ranking('r1',1,c('a','a'),c(10,8)),r1,native)))
  unknown <- plots_round_totals(rows,recovered[1])
  stopifnot(identical(attr(unknown,'missing_rounds'),2),
    is.na(unknown$points[unknown$team_id=='b' & unknown$round==2]))
  former <- rbind(recovered[[1]],transform(recovered[[1]][1,],team_id='former'))
  result <- plots_round_totals(rows,list(former,recovered[[2]]))
  stopifnot(identical(attr(result,'other_participants'),1),all(is.na(result$rank[result$round==1])))
  # A delayed team snapshot can omit a round already confirmed closed.
  delayed <- rows[rows$round==1,]
  attr(delayed, "finished_rounds") <- c(1,2)
  attr(delayed, "round_numbers") <- c(1,2,3)
  settled <- plots_round_totals(delayed, list(recovered[[2]][1:2,]))
  stopifnot(identical(settled$round,c(1,2,1,2)), identical(settled$points,c(10,15,8,8)))
})
check('postponed round 8 uses the published stage for cumulative ranks', {
  catalog <- data.frame(round_id=paste0('r',7:9), round_number=7:9,
    is_finished=c(TRUE,FALSE,TRUE), partially_played=c(FALSE,TRUE,FALSE), status='closed')
  rows <- data.frame(team_id=c('a','a','a','b','b','b','c'), team_name=c(rep('A',3),rep('B',3),'C'),
    round=c(7:9,7:9,9), points=c(7,-2,3,4,0,5,6))
  native <- data.frame(team_id=c('a','b'), round_id='r8', status='closed', points=c(-2,0))
  snapshot <- list(round_id='r8', round_number=8, answer=list(metadata=list(status='V')),
    rows=data.frame(team_id=c('a','b'),round_id='r8',round=8,points=c(-2,0)))
  targets <- plots_ranking_targets(rows,catalog)
  stage <- targets[targets$round_number==8,]
  stopifnot(nrow(stage)==1L, isTRUE(stage$partially_played))
  ranking <- plots_saved_round_ranking(snapshot,stage,native,c('a','b','c'))
  closed7 <- data.frame(team_id=c('a','b'), round=7, points=c(7,4))
  totals <- plots_round_totals(rows,list(closed7,ranking))
  stopifnot(identical(totals$points,c(7,5,8,4,4,9,NA_real_,NA_real_,6)),
    identical(totals$rank,c(1,1,2,2,2,1,NA_real_,NA_real_,3)), !length(attr(totals,'missing_rounds')))
  interval <- plots_interval_points(totals[totals$round %in% 8:9,])
  stopifnot(identical(interval$points[interval$team_name=='C'], c(0,NA_real_,6)),
    identical(interval$points[interval$team_name=='B'], c(0,0,5)))
  uncertain <- plots_saved_round_ranking(snapshot,stage,native,c('a','b'))
  stopifnot(identical(attr(plots_round_totals(rows,list(closed7,uncertain)),'missing_rounds'),8),
    is.null(plots_saved_round_ranking(snapshot,stage,native[1,])),
    is.null(plots_saved_round_ranking(snapshot,stage,transform(native,status='active'))),
    is.null(plots_saved_round_ranking(snapshot,transform(stage,status='playing'),native)))
})
check('points axes omit empty dtick while rank axes keep integer positions', {
  rows <- data.frame(team_id='a',team_name='Alpha',round=1:13,points=seq(70,130,length.out=13))
  totals <- plots_round_totals(rows)
  points <- plotly::plotly_build(plots_round_chart(totals))
  ranks <- plotly::plotly_build(plots_round_chart(totals,'rank'))
  stopifnot(!'dtick' %in% names(points$x$layout$yaxis),
    points$x$layout$yaxis$tickmode=='auto',points$x$layout$yaxis$nticks==6,
    ranks$x$layout$yaxis$dtick==1,ranks$x$layout$yaxis$autorange=='reversed',
    max(points$x$data[[1]]$y)>1000)
})
check('transfer histogram groups exact amounts in the display timezone across calendar boundaries', {
  history <- data.frame(
    created = c('2026-01-31T22:59:00Z', '2026-01-31T23:00:00Z', '2026-02-01T22:58:00Z',
      '2026-02-01T23:00:00Z', '2026-03-01T23:00:00Z', '2026-03-29T00:59:00Z',
      '2026-03-29T01:00:00Z', 'bad-date', '2026-04-01T10:00:00Z'),
    price = c(100, 200, 300, 400, 0, 5, 6, 20, NA_real_)
  )
  projection <- pressroom_transfer_amounts(transform(history, player_id = 'fixture', bids = 12L))
  stopifnot(identical(projection, history), identical(names(projection), c('created','price')))
  rows <- plots_transfer_days(projection, 'Europe/Madrid')
  day <- plots_transfer_periods(rows, 'day')
  week <- plots_transfer_periods(rows, 'week')
  month <- plots_transfer_periods(rows, 'month')
  stopifnot(isTRUE(attr(rows, 'incomplete')), nrow(rows) == 7L,
    identical(as.character(day$start), c('2026-01-31','2026-02-01','2026-02-02','2026-03-02','2026-03-29')),
    identical(day$amount, c(100,500,400,0,11)),
    identical(as.character(week$start), c('2026-01-26','2026-02-02','2026-03-02','2026-03-23')),
    identical(week$amount, c(600,400,0,11)),
    identical(as.character(month$end), c('2026-02-01','2026-03-01','2026-04-01')),
    identical(month$amount, c(100,900,11)), identical(month$transfers, c(1L,3L,3L)),
    nrow(plots_transfer_periods(rows[FALSE, ], 'month')) == 0L,
    !isTRUE(attr(plots_transfer_days(history[FALSE, ]), 'incomplete')),
    is.null(plots_transfer_days(data.frame(id = 'malformed')))
  )
  utc <- plots_transfer_periods(plots_transfer_days(history, 'UTC'), 'month')
  stopifnot(identical(utc$amount, c(300,700,11)))
  chart <- plotly::plotly_build(plots_transfer_chart(month))
  trace <- chart$x$data[[1]]
  stopifnot(identical(trace$type, 'bar'), identical(as.numeric(trace$y), c(100,900,11)),
    identical(as.numeric(trace$width), c(31,28,31) * 86400000 * 0.9), chart$x$layout$xaxis$type == 'date',
    grepl('28 Feb 2026.*900 EUR.*3 transfers', trace$hovertemplate[2]))
})
cat('Plots module:', checks, 'checks passed.\n')
