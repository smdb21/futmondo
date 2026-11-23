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
check('plots UI exposes five factual chart tabs', {
  html <- as.character(plots_UI('plots'))
  for (id in c('standings_plot','values_plot','cash_plot','rank_plot','owned_club_plot')) stopifnot(grepl(id, html, fixed=TRUE))
})
check('round totals preserve subrounds, revisions, negative and missing points', {
  rows <- data.frame(team_id=c(rep('a',5),rep('b',3)), team_name=c(rep('Alpha',5),rep('Beta',3)),
    round=c(6,6.2,7,8,6.2,6,7,8), points=c(10,99,-3,0,5,8,4,2))
  d <- plots_round_totals(rows)
  a <- d[d$team_id=='a',]; b <- d[d$team_id=='b',]
  stopifnot(identical(a$round,c(6,6.2,7,8)), identical(a$points,c(10,15,12,12)),
    identical(b$points,c(8,NA_real_,NA_real_,NA_real_)), identical(a$rank,c(1,NA_real_,NA_real_,NA_real_)))
  p <- plotly::plotly_build(plots_round_chart(d))
  stopifnot(p$x$layout$xaxis$type=='category', identical(unlist(p$x$layout$xaxis$categoryarray),c('6','6.2','7','8')),
    length(p$x$data)==2, !is.null(attr(plots_round_chart(d),'fm_info')))
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
 attr(rows,'round_numbers') <- c(1,2,3,4)
 totals <- plots_round_totals(rows)
 stopifnot(identical(totals$round,c(1,2,3,1,2,3)),identical(totals$points,c(10,NA_real_,NA_real_,NA_real_,NA_real_,NA_real_)))
})
cat('Plots module:', checks, 'checks passed.\n')
