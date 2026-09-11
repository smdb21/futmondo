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
render_fixture <- function(history, rounds = NULL) {
  env <- new.env(parent = globalenv())
  env$selected_player <- function() data.frame(id = 'player', value = 100000000, change = 0)
  env$login_token <- 'fixture'; env$championship_id <- 'league'
  env$get_reactive_val <- identity
  env$get_player_historical_data <- function(...) history
  env$get_finished_rounds <- function(...) rounds
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
check('rendered valuation and points traces use independently padded numeric axes', {
  chart <- render_fixture(history, rounds)
  stopifnot(length(chart$data) == 2L, chart$data[[1]]$yaxis == 'y', chart$data[[2]]$yaxis == 'y2',
            chart$data[[2]]$mode == 'markers', chart$layout$yaxis2$overlaying == 'y',
            chart$layout$yaxis2$side == 'right', !chart$layout$yaxis$autorange,
            !chart$layout$yaxis2$autorange)
  assert_visible(chart$data[[1]]$y, chart$layout$yaxis$range)
  assert_visible(chart$data[[2]]$y, chart$layout$yaxis2$range)
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
  chart <- render_fixture(zero, rounds)
  assert_visible(chart$data[[2]]$y, chart$layout$yaxis2$range)
  unavailable <- render_fixture(history)
  stopifnot(length(unavailable$data) == 1L, !unavailable$layout$yaxis2$visible,
            unavailable$layout$annotations[[1]]$text == 'No points recorded yet')
})
check('empty history fallback has a usable valuation range', {
  chart <- render_fixture(NULL)
  stopifnot(length(chart$data) == 1L, length(chart$data[[1]]$y) == 7L)
  assert_visible(chart$data[[1]]$y, chart$layout$yaxis$range)
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

cat(sprintf('Player trend plot: %d checks passed.\n', checks))
