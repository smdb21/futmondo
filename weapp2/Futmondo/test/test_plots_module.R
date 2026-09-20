#!/usr/bin/env Rscript
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
check('plots UI exposes six chart tabs', {
  html <- as.character(plots_UI('plots'))
  for (id in c('standings_plot','values_plot','cash_plot','rank_plot','bid_plot','owned_club_plot')) stopifnot(grepl(id, html, fixed=TRUE))
})
cat('Plots module:', checks, 'checks passed.\n')
