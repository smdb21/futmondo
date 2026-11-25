#!/usr/bin/env Rscript
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source('global.R'))
n <- 0L
check <- function(name, expr) { force(expr); n <<- n + 1L; cat('PASS', name, '\n') }
answer <- list(players = lapply(1:11, function(i) list(id = paste0('p', i), name = paste('Player', i),
  role = c('portero', rep('defensa', 4), rep('centrocampista', 4), rep('delantero', 2))[i],
  points = c(0, -2, NA, 4:11)[i])), mvp = 'p2')
reference <- list(round_id = 'r1', round_number = 1, is_finished = TRUE)
first <- normalize_official_dreamteam(answer, 'league', reference)$rows
second <- first; second$round_id <- 'r2'; second$round_number <- 2
unknown <- first; unknown$round_id <- 'unknown'; unknown$round_number <- NA_real_
provisional <- first; provisional$round_id <- 'provisional'; provisional$is_finished <- FALSE
unverified <- first; unverified$round_id <- 'unverified'; unverified$is_finished <- NA
foreign <- first; foreign$championship_id <- 'other'; foreign$player_name <- 'Foreign player'
rows <- rbind(first, second, unknown, provisional, unverified, foreign)
check('all available official selections are scoped, newest first, with source scores', {
  mvps <- round_mvp_rows(rows, 'league')
  stopifnot(identical(mvps$round_id, c('r2', 'r1', 'provisional', 'unverified', 'unknown')))
  for (i in seq_len(nrow(mvps))) {
    xi <- round_best_xi_rows(rows, 'league', mvps[i, , drop = FALSE])
    stopifnot(nrow(xi) == 11L, all(xi$round_id == mvps$round_id[i]),
      !any(xi$player_name == 'Foreign player'), sum(xi$is_mvp) == 1L,
      identical(xi$display_role, c('GK', rep('DEF', 4), rep('MID', 4), rep('FWD', 2))),
      xi$points[1] == 0, xi$points[2] == -2, is.na(xi$points[3]))
  }
})
check('round finality does not add a rival cash warning', {
  stopifnot(!length(rivals_calculation_coverage('Final settlement not confirmed. Unfinished or unverified award selections are excluded from reconstructed cash.')))
})
check('immutable IDs separate equal display numbers; legacy numbers still work', {
  repeated <- second; repeated$round_number <- 1
  together <- rbind(first, repeated)
  mvps <- round_mvp_rows(together, 'league')
  stopifnot(nrow(mvps) == 2L, nrow(round_best_xi_rows(together, 'league', mvps[2, ])) == 11L)
  legacy <- first; legacy$round_id <- NULL
  stopifnot(nrow(round_best_xi_rows(legacy, 'league', round_mvp_rows(legacy, 'league'))) == 11L)
  stopifnot(nrow(round_best_xi_rows(first[-1, ], 'league', mvps[1, ])) == 0L)
  duplicate <- first; duplicate$player_id[11] <- duplicate$player_id[1]
  stopifnot(nrow(round_best_xi_rows(duplicate, 'league', mvps[1, ])) == 0L)
})
check('saved full-XI snapshot retains every player and missing scores', {
  auth <- c(userid = 'fixture', token = 'synthetic')
  result <- normalize_official_dreamteam(answer, 'league', reference)
  ctx <- resource_context(auth, 'league', season = 'fixture-season')
  snapshot <- award_snapshot_evidence(result, auth, 'league', reference, ctx)
  saved <- award_saved_rows(list(snapshot), 'league', 'fixture-season', account_id = 'fixture')
  xi <- round_best_xi_rows(saved, 'league', round_mvp_rows(saved, 'league'))
  stopifnot(nrow(xi) == 11L, xi$points[1] == 0, xi$points[2] == -2, is.na(xi$points[3]))
})
check('module renders full lineups and MVP highlights from one shared snapshot', {
  original <- shared_mvp_source
  reads <- 0L
  shared_mvp_source <- function(...) { reads <<- reads + 1L; reactiveVal(list(rows = rows, reason = 'Coverage notice')) }
  shiny::testServer(round_mvps_Server, args = list(is_module_active = function() TRUE,
    login_token = function() NULL, championship_id = function() 'league'), {
    html <- output$mvp_cards$html
    dom <- xml2::read_html(html)
    stopifnot(length(xml2::xml_find_all(dom, '//table/tbody/tr')) == 55L,
      length(xml2::xml_find_all(dom, '//*[@class="round-best-xi-mvp"]')) == 5L,
      grepl('Coverage notice', html, fixed = TRUE), grepl('Unavailable', html, fixed = TRUE),
      grepl('>-2<', html, fixed = TRUE), !grepl('Foreign player', html, fixed = TRUE), reads == 1L)
    session$flushReact(); stopifnot(reads == 1L)
  })
  shared_mvp_source <- original
})
check('source names are escaped and unknown roles remain explicit', {
  unsafe <- first; unsafe$player_name[1] <- '<script>bad()</script>'; unsafe$player_role[1] <- ''
  xi <- round_best_xi_rows(unsafe, 'league', round_mvp_rows(unsafe, 'league'))
  html <- htmltools::renderTags(round_award_card(round_mvp_rows(unsafe, 'league'), xi))$html
  stopifnot(grepl('&lt;script&gt;', html, fixed = TRUE), !grepl('<script>', html, fixed = TRUE),
    xi$display_role[xi$player_id == 'p1'] == 'Unavailable')
})
season <- dplyr::bind_rows(lapply(1:38, function(i) { x <- first; x$round_id <- paste0('r', i); x$round_number <- i; x }))
elapsed <- system.time({
  mvps <- round_mvp_rows(season, 'league')
  cards <- lapply(seq_len(nrow(mvps)), function(i) round_award_card(mvps[i, ], round_best_xi_rows(season, 'league', mvps[i, ])))
  htmltools::renderTags(tagList(cards))
})[['elapsed']]
cat(sprintf('PERFORMANCE 38 rounds / 418 lineup rows: %.3f s render, zero added requests\n', elapsed))
check('award totals use stable identities, verified finality and one scoring context', {
  a <- transform(first, season = 'season', scoring_version = 'standard', persistence = 'Saved')
  b <- transform(a, round_id = 'r2', round_number = 1, is_mvp = player_id == 'p1')
  c <- transform(b, round_id = 'r3', round_number = NA_real_)
  c$player_id[11] <- 'p12'
  # The duplicate name and the name change must not split or merge player IDs.
  a$player_name[1:2] <- 'Alex'
  b$player_name[1:2] <- 'Alex'
  c$player_name[1:2] <- 'Alex'
  b$player_role[b$player_id == 'p2'] <- 'centrocampista'
  c$player_role[c$player_id == 'p12'] <- NA_character_
  c$player_position[c$player_id == 'p12'] <- '3' # formation slot
  pending <- transform(a, round_id = 'pending', is_finished = FALSE)
  pending$player_role[pending$player_id == 'p2'] <- 'delantero'
  unknown <- transform(a, round_id = 'unknown', is_finished = NA)
  short <- transform(a[-1, ], round_id = 'short')
  duplicate <- transform(a, round_id = 'duplicate')
  duplicate$player_id[11] <- duplicate$player_id[1]
  ambiguous <- transform(a, round_id = 'ambiguous', is_mvp = player_id %in% c('p1', 'p2'))
  unsaved <- transform(a, round_id = 'save', persistence = 'Save failed')
  foreign <- transform(a, championship_id = 'elsewhere', player_id = paste0('foreign', player_id))
  result <- round_award_player_stats(rbind(a, b, c, pending, unknown, short, duplicate, ambiguous, unsaved, foreign), 'league')
  p <- result$players
  stopifnot(result$rounds == 3L, result$unconfirmed_rounds == 2L, result$incomplete_rounds == 3L,
    result$unsaved_rounds == 1L, nrow(p) == 12L, sum(p$best_xi) == 33L, sum(p$mvp) == 3L,
    p$award_positions[p$player_id == 'p1'] == 'GK', p$award_positions[p$player_id == 'p2'] == 'DEF / MID',
    is.na(p$award_positions[p$player_id == 'p12']),
    sum(p$player_name == 'Alex') == 2L, p$mvp[p$player_id == 'p1'] == 2L,
    p$mvp[p$player_id == 'p2'] == 1L, p$best_xi[p$player_id == 'p11'] == 2L,
    p$best_xi[p$player_id == 'p12'] == 1L, !any(startsWith(p$player_id, 'foreign')))
  renamed <- transform(a, round_id = 'new', round_number = 8, player_name = paste('New', player_id))
  latest <- round_award_player_stats(rbind(renamed, a), 'league')$players
  stopifnot(latest$player_name[latest$player_id == 'p1'] == 'New p1')
  legacy <- a
  legacy$player_role <- NULL
  stopifnot(all(is.na(round_award_player_stats(legacy, 'league')$players$award_positions)))
  legacy$round_id <- NULL
  stopifnot(round_award_player_stats(legacy, 'league')$rounds == 1L)
  legacy$persistence <- NULL
  stopifnot(round_award_player_stats(legacy, 'league')$unsaved_rounds == 1L)
  for (field in c('season', 'scoring_version')) {
    other <- b
    other[[field]] <- 'other'
    mixed <- round_award_player_stats(rbind(a, other), 'league')
    stopifnot(mixed$rounds == 0L, nzchar(mixed$scope_issue))
  }
})
check('statistics tab uses confirmed rounds and the shared snapshot, including on updates', {
  original <- shared_mvp_source
  shared_calls <- 0L
  saved <- rows
  saved$persistence <- 'Saved'
  state <- reactiveVal(list(status = 'partial', rows = saved, reason = 'A historical round is missing.'))
  shared_mvp_source <- function(...) { shared_calls <<- shared_calls + 1L; state }
  shiny::testServer(round_mvps_Server, args = list(is_module_active = function() TRUE,
    login_token = function() NULL, championship_id = function() 'league'), {
    session$setInputs(award_tab = 'statistics')
    dom <- xml2::read_html(output$award_statistics$html)
    figures <- xml2::xml_find_all(dom, '//figure')
    stats <- xml2::xml_text(xml2::xml_find_all(dom, '//dl//dd'))
    table <- jsonlite::fromJSON(xml2::xml_text(xml2::xml_find_first(dom, '//script[@data-for]')))$x$tag$attribs$data
    stopifnot(identical(stats, c('3', '11', '1')), length(figures) == 2L,
      length(xml2::xml_find_all(figures[1], './/li')) == 5L,
      length(xml2::xml_find_all(figures[2], './/li')) == 1L,
      grepl('6 more players tied at 3', xml2::xml_text(figures[1]), fixed = TRUE),
      grepl('2 selections await confirmation', xml2::xml_text(dom), fixed = TRUE),
      grepl('A historical round is missing.', xml2::xml_text(dom), fixed = TRUE),
      length(table$player_name) == 11L, sum(table$mvp) == 3L, all(table$share == 1))
    session$setInputs(award_tab = 'rounds')
    session$setInputs(award_tab = 'statistics')
    state(list(rows = data.frame(), status = 'partial', reason = '',
      coverage = data.frame(required = FALSE)))
    session$flushReact()
    stopifnot(grepl('No completed round awards yet.', output$award_statistics$html, fixed = TRUE),
      !grepl('award-chart', output$award_statistics$html, fixed = TRUE))
    state(list(rows = data.frame(), status = 'loading', reason = ''))
    session$flushReact()
    stopifnot(is.null(output$award_statistics), shared_calls == 1L)
  })
  shared_mvp_source <- original
})
fixture <- tags$html(tags$head(tags$meta(name = 'viewport', content = 'width=device-width, initial-scale=1'),
  tags$style(HTML(paste(':root { --fm-muted: #aaa; --fm-border: #345; --fm-surface: #0b120d; --fm-text: #eee; --fm-warning: #ffc107; } body { margin: 0; padding: 16px; background: #050805; font-family: monospace; } * { box-sizing: border-box; }',
    paste(readLines('www/custom_style.css', warn = FALSE), collapse = '\n'))))),
  tags$body(tags$div(class = 'round-mvps-grid', cards[1:2])))
htmltools::save_html(fixture, '/tmp/futmondo-round-awards.html')
cat(sprintf('Round awards: %d passed / 0 failed\n', n))
