#!/usr/bin/env Rscript
# Focused offline chart regression: no authentication or database/API writes.
suppressPackageStartupMessages({library(shiny); library(dplyr); library(plotly)})
source('data_contracts.R')
source('theme.R')
source('Modules/Selected_Player_Module.R')
format_table_currency <- function(x) paste0(format(x, scientific = FALSE, trim = TRUE), ' EUR')
checks <- 0L
check <- function(name, code) {
  force(code)
  checks <<- checks + 1L
  cat('[PASS]', name, '\n')
}

# Exercise the actual render body without triggering unrelated player-card
# observers. Network reads are replaced by deterministic fixtures.
find_plot_body <- function(node) {
  if (is.call(node) && identical(node[[1]], as.name('<-')) &&
      identical(paste(deparse(node[[2]]), collapse = ''), 'output$player_trend_plot')) {
    return(node[[3]][[2]])
  }
  if (is.call(node) || is.expression(node) || is.pairlist(node)) {
    for (child in as.list(node)) {
      if (missing(child)) next
      found <- find_plot_body(child)
      if (!is.null(found)) return(found)
    }
  }
  NULL
}
plot_body <- find_plot_body(parse('Modules/Selected_Player_Module.R'))
stopifnot(!is.null(plot_body))
render_fixture <- function(history, rounds = NULL, aggregate_points = NA_real_, chart_metric = "valuation", summary = NULL) {
  env <- new.env(parent = globalenv())
  env$selected_player <- function() data.frame(id = 'player', value = 100000000, change = 0, points = aggregate_points)
  env$login_token <- 'fixture'; env$championship_id <- 'league'; env$input <- list(player_chart_metric = chart_metric)
  env$get_reactive_val <- identity
  env$get_player_historical_data <- function(...) history
  env$get_finished_rounds <- function(...) rounds
  env$get_player_round_points_history <- function(...) data.frame()
  env$get_player_summary <- function(...) summary
  plotly::plotly_build(eval(plot_body, env))$x
}
assert_visible <- function(values, bounds) {
  stopifnot(length(bounds) == 2L, all(is.finite(bounds)), diff(bounds) > 0,
            all(values > bounds[1]), all(values < bounds[2]))
  fraction <- (values - bounds[1]) / diff(bounds)
  stopifnot(all(fraction >= 0.08), all(fraction <= 0.92))
}

check('high nearly constant valuations have space above and below their markers', {
  values <- c(99000000, 100000000)
  bounds <- player_trend_axis_range(values)
  assert_visible(values, bounds)
  stopifnot(bounds[1] > 90000000)
})
check('single, constant and zero-only series receive usable centered ranges', {
  for (values in list(100000000, c(8, 8), c(0, 0))) {
    bounds <- player_trend_axis_range(values)
    assert_visible(values, bounds)
    stopifnot(mean(bounds) == values[1])
  }
})
check('invalid observations cannot poison the axis range', {
  stopifnot(identical(player_trend_axis_range(c('8', 'NA', 'bad', 'Inf')), c(6, 10)),
            identical(player_trend_axis_range(c(NA, Inf, -Inf)), c(0, 1)),
            identical(player_trend_axis_range(NULL), c(0, 1)))
})

history <- data.frame(recorded_at = c('2026-08-01T10:00:00Z', '2026-08-08T10:00:00Z'),
                      value = c(99000000, 100000000), points = c(98, 99))
rounds <- data.frame(round_number = 1:2, begin_process = history$recorded_at, is_finished = TRUE)
check('valuation and points chart modes use independently padded numeric axes', {
  valuation <- render_fixture(history, rounds)
  points <- render_fixture(history, rounds, chart_metric = 'points')
  stopifnot(length(valuation$data) == 1L, valuation$data[[1]]$yaxis == 'y', !valuation$layout$yaxis$autorange,
            length(points$data) == 1L, points$data[[1]]$mode == 'lines+markers', !points$layout$yaxis$autorange)
  assert_visible(valuation$data[[1]]$y, valuation$layout$yaxis$range)
  assert_visible(points$data[[1]]$y, points$layout$yaxis$range)
})
check('render handles numeric strings and excludes nonfinite valuation rows', {
  strings <- rbind(history, transform(history[1, ], recorded_at = '2026-08-03T10:00:00Z'))
  strings$value <- c('99000000', '100000000', 'Inf')
  chart <- render_fixture(strings, rounds)
  stopifnot(is.numeric(chart$data[[1]]$y), length(chart$data[[1]]$y) == 2L)
  assert_visible(chart$data[[1]]$y, chart$layout$yaxis$range)
})
check('zero-point markers remain visible and an unavailable points series stays hidden', {
  zero <- history; zero$points <- 0
  chart <- render_fixture(zero, rounds, chart_metric = 'points')
  assert_visible(chart$data[[1]]$y, chart$layout$yaxis$range)
  unavailable <- render_fixture(history, chart_metric = 'points')
  stopifnot(unavailable$layout$annotations[[1]]$text == 'No round-by-round points recorded yet')
})
check('aggregate points do not produce a misleading no-points annotation', {
  chart <- render_fixture(NULL, NULL, aggregate_points = 37, chart_metric = 'points')
  stopifnot(chart$layout$annotations[[1]]$text == 'Total points: 37. Round-by-round history is unavailable.')
})
check('empty history fallback has a usable valuation range', {
  chart <- render_fixture(NULL)
  stopifnot(length(chart$data) == 1L, length(chart$data[[1]]$y) == 7L)
  assert_visible(chart$data[[1]]$y, chart$layout$yaxis$range)
})

check('stored finalized match observations are the primary round-points source', {
  rows <- data.frame(round=c(1,2,2,3),points=c(4,8,9,NA),score_status=c('final','final','final','provisional'),
    occurred_at=c('2026-08-01T10:00:00Z',NA,'2026-08-09T10:00:00Z','2026-08-15T10:00:00Z'),
    round_start_at=c(NA,'2026-08-08T10:00:00Z',NA,NA),observed_at=c('2026-08-01T11:00:00Z','2026-08-08T11:00:00Z','2026-08-09T11:00:00Z','2026-08-15T11:00:00Z'))
  trace <- player_match_points_trace(rows)
  stopifnot(trace$has_points, identical(trace$points_df$round_number,c(1,2)), identical(trace$points_df$points,c(4,9)))
})
check('player-summary fallback returns only explicitly finished rounds', {
  summary <- list(points=list(list(round=1,points=4),list(round=2,points=8)))
  rounds <- data.frame(round_number=1:2,begin_process=c('2026-08-01T10:00:00Z','2026-08-08T10:00:00Z'),is_finished=c(TRUE,FALSE))
  trace <- player_summary_points_trace(summary, rounds)
  stopifnot(trace$has_points, identical(trace$points_df$round_number, 1), identical(trace$points_df$points, 4))
})

check('player-summary fallback uses prior scores when historic round boundaries are absent', {
  summary <- list(match=list(r=list(number=3)), points=list(list(round=1,points=4),list(round=2,points=8),list(round=3,points=2)))
  trace <- player_summary_points_trace(summary, NULL)
  stopifnot(trace$has_points, identical(trace$points_df$round_number, c(1,2)), identical(trace$points_df$points, c(4,8)))
})

check('points chart renders the summary fallback without historic round boundaries', {
  summary <- list(match=list(r=list(number=3)), points=list(list(round=1,points=4),list(round=2,points=8),list(round=3,points=2)))
  chart <- render_fixture(NULL, NULL, aggregate_points=14, chart_metric='points', summary=summary)
  stopifnot(length(chart$data)==1L, identical(as.numeric(unlist(chart$data[[1]]$x)),c(1,2)), identical(as.numeric(unlist(chart$data[[1]]$y)),c(4,8)))
})

check('latest round points are limited and returned newest first', {
  history_recent <- data.frame(
    recorded_at=sprintf('2026-08-%02dT10:00:00Z',seq(1,8)),
    value=1000000,points=seq(1,8))
  rounds_recent <- data.frame(round_number=seq(1,8),begin_process=history_recent$recorded_at,is_finished=TRUE)
  recent <- latest_player_round_points(history_recent,rounds_recent,limit=5)
  stopifnot(identical(recent$round_number,c(8,7,6,5,4)),identical(recent$points,c(8,7,6,5,4)))
  unavailable <- latest_player_round_points(history_recent,NULL,limit=5)
  stopifnot(nrow(unavailable)==0L)
})

check('empty latest-round panel keeps a known aggregate score visible', {
  stopifnot(
    recent_round_points_empty_text(10) == 'Total points: 10. Completed-round breakdown is unavailable.',
    recent_round_points_empty_text(0) == 'No completed-round points recorded yet',
    recent_round_points_empty_text(NA_real_) == 'No completed-round points recorded yet'
  )
})


cat(sprintf('Player trend plot: %d checks passed.\n', checks))
