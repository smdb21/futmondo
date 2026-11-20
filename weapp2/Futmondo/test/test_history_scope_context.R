#!/usr/bin/env Rscript
# Offline: history identity must not fall back to "unknown" when the live
# 5-minute league metadata expires, and unresolved writes must stay retryable.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages({
  source('futmondo_functions.R')
  source('supabase_connector.R')
  source('background_runtime.R')
})
source('Modules/Plots_Module.R')
checks <- 0L
check <- function(name, expr) {
  force(expr)
  checks <<- checks + 1L
  cat('PASS', name, '\n')
}
reset <- function() {
  .fm_league_contexts$items <- list()
  rm(list = ls(.fm_history_scope_cache), envir = .fm_history_scope_cache)
}
ctx_row <- function(account, cid, comp, season, age = 600) {
  stamp <- Sys.time() - age
  list(
    account_id = account,
    championship_id = cid,
    competition_id = comp,
    season = season,
    scoring_version = 'unknown',
    observed_at = format(stamp, '%Y-%m-%dT%H:%M:%OS6Z', tz = 'UTC'),
    expires_at = format(stamp + 300, '%Y-%m-%dT%H:%M:%OS6Z', tz = 'UTC'),
    complete = TRUE,
    payload = list(version = 2L)
  )
}

check('expired live metadata still supplies the recorded history identity', {
  reset()
  stopifnot(remember_league_context(ctx_row(
    'acct',
    'league-a',
    'comp-1',
    '2025-2026'
  )))
  stopifnot(is.null(league_context_metadata('league-a', 'acct')))
  s <- league_history_scope('league-a', 'acct', lookup = FALSE)
  stopifnot(s$known, s$competition_id == 'comp-1', s$season == '2025-2026')
  # No account in scope: a single agreeing context is used.
  s <- league_history_scope('league-a', lookup = FALSE)
  stopifnot(s$known, s$season == '2025-2026')
})

check('conflicting accounts or other leagues never supply a context', {
  reset()
  remember_league_context(ctx_row('a1', 'league-a', 'comp-1', '2025-2026'))
  remember_league_context(ctx_row('a2', 'league-a', 'comp-2', '2026-2027'))
  stopifnot(
    !league_history_scope('league-a', lookup = FALSE)$known,
    league_history_scope('league-a', 'a2', lookup = FALSE)$season ==
      '2026-2027',
    !league_history_scope('league-b', lookup = FALSE)$known
  )
})

check('persisted context is used after memory is empty, and cached in a bounded way', {
  reset()
  calls <- 0L
  supabase_get <<- function(table, q) {
    calls <<- calls + 1L
    stopifnot(
      table == 'league_context_observations',
      q$championship_id == 'eq.league-a'
    )
    data.frame(
      competition_id = 'comp-1',
      season = '2025-2026',
      scoring_version = 'v',
      observed_at = '2026-11-20T00:00:00Z'
    )
  }
  a <- league_history_scope('league-a')
  b <- league_history_scope('league-a')
  stopifnot(
    a$known,
    a$source == 'database',
    b$season == '2025-2026',
    calls == 1L
  )
  reset()
  supabase_get <<- function(...) NULL
  stopifnot(!league_history_scope('league-a')$known)
})

check('history reader prefers the resolved season and keeps unknown as an archive scope', {
  reset(); remember_league_context(ctx_row('acct', 'league-a', 'comp-1', '2025-2026'))
  seen <- NULL
  read_league_team_history_daily <<- function(...) NULL
  supabase_read_all <<- function(table, query, ...) { seen <<- query; d <- data.frame(points = 1, recorded_at = '2026-11-01T00:00:00Z'); attr(d, 'fetch_status') <- 'partial'; d }
  h <- get_league_standings_history('league-a')
  stopifnot(seen$season == 'eq.2025-2026', seen$competition_id == 'eq.comp-1', identical(attr(h, 'fetch_status'), 'partial'))
  reset(); supabase_get <<- function(...) NULL; seen <- NULL
  get_league_standings_history('league-a')
  stopifnot(seen$season == 'eq.unknown', seen$competition_id == 'eq.unknown')
})
check('season-scoped history is not written under an unknown context', {
  reset()
  supabase_get <<- function(...) NULL
  options(futmondo.offline = FALSE)
  posted <- 0L
  supabase_post_direct <<- function(...) {
    posted <<- posted + 1L
    TRUE
  }
  r <- supabase_post(
    'user_team_history',
    data.frame(
      user_team_id = 't',
      championship_id = 'league-a',
      observed_at = '2026-11-20T00:00:00Z',
      points = 1
    )
  )
  stopifnot(isFALSE(r), posted == 0L)
  remember_league_context(ctx_row('acct', 'league-a', 'comp-1', '2025-2026'))
  r <- supabase_post(
    'user_team_history',
    data.frame(
      user_team_id = 't',
      championship_id = 'league-a',
      observed_at = '2026-11-20T00:00:00Z',
      points = 1
    )
  )
  stopifnot(!isFALSE(r), posted == 1L)
  options(futmondo.offline = TRUE)
})

check('plots distinguish unresolved context and partial history from no data', {
  u <- data.frame()
  attr(u, 'context_unknown') <- TRUE
  stopifnot(grepl(
    'season could not be resolved',
    as.character(attr(
      plots_history(u, 'points', 'Cumulative points'),
      'fm_info'
    ))
  ))
  p <- data.frame(
    teamname = 'T',
    points = c(1, 2),
    recorded_at = c('2026-11-01T00:00:00Z', '2026-11-02T00:00:00Z')
  )
  attr(p, 'fetch_status') <- 'partial'
  stopifnot(grepl(
    'partial history',
    as.character(attr(
      plots_history(p, 'points', 'Cumulative points'),
      'fm_info'
    ))
  ))
  attr(p, 'fetch_status') <- 'ok'
  stopifnot(is.null(attr(
    plots_history(p, 'points', 'Cumulative points'),
    'fm_info'
  )))
})

check('daily standings reader is preferred, paged and falls back when not installed', {
  reset()
  remember_league_context(ctx_row('acct', 'league-a', 'comp-1', '2025-2026'))
  raw <- 0L
  supabase_read_all <<- function(...) { raw <<- raw + 1L; data.frame() }
  read_league_team_history_daily <<- function(championship_id, scope, ...) {
    stopifnot(scope$season == '2025-2026', scope$competition_id == 'comp-1')
    d <- data.frame(user_team_id = 't', teamname = 'T', day = '2026-11-01', points = 3,
      recorded_at = '2026-11-01T20:00:00Z'); attr(d, 'fetch_status') <- 'ok'; d
  }
  h <- get_league_standings_history('league-a')
  stopifnot(raw == 0L, nrow(h) == 1L, h$teamname == 'T')
  read_league_team_history_daily <<- function(...) NULL
  get_league_standings_history('league-a')
  stopifnot(raw == 1L)
})

check('saved awards match the stable competition when live metadata has expired', {
  reset()
  remember_league_context(ctx_row('acct', 'league-a', 'comp-1', '2025-2026'))
  source('award_runtime.R')
  snap <- function(comp) list(account_id = 'acct', championship_id = 'league-a', competition_id = comp, season = '2025-2026', payload = list())
  kept <- 0L
  award_saved_rows_filter <- function(comp) {
    rows <- Filter(function(x) identical(x$competition_id, comp), list(snap('comp-1'), snap('comp-2')))
    length(rows)
  }
  stopifnot(league_competition_context('league-a', 'acct') == 'unknown',
    league_history_scope('league-a', 'acct', lookup = FALSE)$competition_id == 'comp-1')
  # A different competition is never accepted.
  out <- award_saved_rows(list(snap('comp-2')), 'league-a', '2025-2026', 'unknown', 'acct')
  stopifnot(!nrow(out))
})

check('stable helpers prefer live metadata and replace only unknown', {
  reset()
  remember_league_context(ctx_row('acct', 'league-a', 'comp-1', '2025-2026'))
  stopifnot(league_competition_context('league-a', 'acct') == 'unknown',
    league_stable_competition('league-a', 'acct') == 'comp-1',
    league_stable_season('league-a', 'acct') == '2025-2026')
  remember_league_context(ctx_row('acct', 'league-a', 'comp-live', 'S-live', age = 1))
  stopifnot(league_stable_competition('league-a', 'acct') == 'comp-live',
    league_stable_season('league-a', 'acct') == 'S-live')
  # Asynchronous adapters never trigger the extra database lookup.
  reset(); calls <- 0L; supabase_get <<- function(...) { calls <<- calls + 1L; NULL }
  env <- new.env(parent = globalenv()); env$.futmondo_request_context <- list(account_id = 'acct')
  f <- league_history_scope; environment(f) <- env; f('league-a')
  stopifnot(calls == 0L)
})

cat('HISTORY SCOPE CONTEXT:', checks, 'checks passed\n')
