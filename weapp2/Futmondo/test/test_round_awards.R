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
check('Statistics opens first with player grouping and a club switch', {
  dom <- xml2::read_html(as.character(round_mvps_UI('awards')))
  tabs <- xml2::xml_find_all(dom, '//ul[@id="awards-award_tab"]/li/a')
  toggle <- xml2::xml_find_all(dom, '//div[@data-value="statistics"]//input[@id="awards-group_by_club"]')
  stopifnot(identical(xml2::xml_text(tabs), c('Statistics', 'By round')),
    identical(xml2::xml_attr(tabs, 'data-value'), c('statistics', 'rounds')),
    xml2::xml_attr(xml2::xml_parent(tabs[[1]]), 'class') == 'active',
    length(toggle) == 1L, is.na(xml2::xml_attr(toggle, 'checked')))
})
check('club statistics count each appearance at its recorded club and each represented round once', {
  a <- transform(first, persistence = 'Saved', award_team = c(rep('East', 6), rep('West', 4), 'South'))
  b <- transform(a, round_id = 'r2', award_team = c('East', rep('West', 9), ''))
  # The MVP moves from East to West. One West appearance records only its club crest.
  a$award_logo <- paste0(a$award_team, '.png')
  b$award_logo <- ifelse(nzchar(b$award_team), paste0(b$award_team, '.png'), '')
  b$award_team[3] <- ''
  incomplete <- transform(a[-1, ], round_id = 'partial', award_team = 'Incomplete XI')
  state <- reactiveVal(list(rows = rbind(a, b, incomplete), status = 'ok', reason = ''))
  original_source <- shared_mvp_source
  original_adapter <- factual_read_adapter
  source_reads <- 0L
  shared_mvp_source <- function(...) { source_reads <<- source_reads + 1L; state }
  factual_read_adapter <- function(...) function(...) list(status = 'ready',
    data = data.frame(id = a$player_id, team = 'Present-day club', logo = 'present.png'))
  shiny::testServer(round_mvps_Server, args = list(is_module_active = function() TRUE,
    login_token = function() c(userid = 'fixture', token = 'synthetic'), championship_id = function() 'league'), {
    widget_data <- function(dom) {
      script <- xml2::xml_find_first(dom, '//script[@data-for]')
      jsonlite::fromJSON(xml2::xml_text(script))$x$tag$attribs$data
    }
    player_table <- widget_data(xml2::read_html(output$award_statistics$html))
    stopifnot(length(player_table$player_name) == 11L, sum(player_table$best_xi) == 22L)
    session$setInputs(group_by_club = TRUE)
    dom <- xml2::read_html(output$award_statistics$html)
    clubs <- widget_data(dom)
    idx <- match(c('East', 'West', 'South'), clubs$club)
    figures <- xml2::xml_find_all(dom, '//figure')
    bars <- xml2::xml_find_all(figures, './/*[@class="award-chart-track"]/span')
    widths <- as.numeric(sub('^width: *([0-9.]+)%.*$', '\\1', xml2::xml_attr(bars, 'style')))
    stopifnot(length(clubs$club) == 3L, !anyNA(idx),
      identical(as.integer(clubs$best_xi[idx]), c(7L, 13L, 1L)),
      identical(as.integer(clubs$mvp[idx]), c(1L, 1L, 0L)),
      identical(clubs$share[idx], c(1, 1, 0.5)),
      identical(xml2::xml_text(xml2::xml_find_all(dom, '//dl//dt')),
        c('Confirmed rounds', 'Identified Best XI clubs', 'Clubs with an MVP')),
      identical(xml2::xml_text(xml2::xml_find_all(dom, '//dl//dd')), c('2', '3', '2')),
      grepl('1 selection is incomplete', xml2::xml_text(dom), fixed = TRUE),
      grepl('Club unknown for 1 Best XI appearance (0 MVP wins).', xml2::xml_text(dom), fixed = TRUE),
      length(figures) == 2L, length(widths) == 5L, all(widths > 0 & widths <= 100))
    session$setInputs(group_by_club = FALSE)
    stopifnot(identical(widget_data(xml2::read_html(output$award_statistics$html)), player_table),
      source_reads == 1L)
    legacy <- rbind(a, b)
    legacy[legacy$is_mvp, c('award_team', 'award_logo')] <- ''
    state(list(rows = legacy, status = 'ok', reason = ''))
    session$setInputs(group_by_club = TRUE)
    dom <- xml2::read_html(output$award_statistics$html)
    clubs <- widget_data(dom)
    stopifnot(sum(clubs$best_xi) == 19L, sum(clubs$mvp) == 0L, source_reads == 1L,
      grepl('Club unknown for 3 Best XI appearances (2 MVP wins).', xml2::xml_text(dom), fixed = TRUE),
      grepl('No MVP wins have a recorded club.', xml2::xml_text(dom), fixed = TRUE))
    legacy$award_team <- legacy$award_logo <- ''
    state(list(rows = legacy, status = 'ok', reason = ''))
    session$flushReact()
    dom <- xml2::read_html(output$award_statistics$html)
    stopifnot(grepl('Club unknown for 22 Best XI appearances (2 MVP wins).', xml2::xml_text(dom), fixed = TRUE),
      length(xml2::xml_find_all(dom, '//figure | //script[@data-for]')) == 0L)
  })
  shared_mvp_source <- original_source
  factual_read_adapter <- original_adapter
})
check('award clicks select a scoped ID, reuse the card, and retain historical players', {
  original_card <- selected_player_Server
  original_source <- shared_mvp_source
  original_adapter <- factual_read_adapter
  opened <- 0L
  card_player <- NULL
  selected_player_Server <- function(id, selected_player, ...) {
    opened <<- opened + 1L
    card_player <<- selected_player
  }
  saved <- transform(first, player_name = 'Shared name', persistence = 'Saved')
  shared_mvp_source <- function(...) reactiveVal(list(rows = saved, reason = ''))
  catalog <- reactiveVal(data.frame(id = c('p2', 'p1'), name = c('Second', 'First'), points = c(21, 20)))
  factual_read_adapter <- function(...) function(...) list(data = catalog())
  league <- reactiveVal('league')
  shiny::testServer(round_mvps_Server, args = list(is_module_active = function() TRUE,
    login_token = function() c(userid = 'fixture', token = 'synthetic'),
    championship_id = league, user_team_id = function() 'team'), {
    dom <- xml2::read_html(output$award_statistics$html)
    scope <- xml2::xml_attr(xml2::xml_find_first(dom, '//*[@data-award-scope]'), 'data-award-scope')
    click <- function(id) session$setInputs(award_open = list(scope = scope, id = id))
    stopifnot(opened == 0L, !is.na(scope))
    click('unknown')
    stopifnot(opened == 0L)
    click('p2')
    stopifnot(opened == 1L, card_player()$id == 'p2', card_player()$name == 'Second', card_player()$points == 21)
    session$setInputs(award_closed = 1L)
    stopifnot(is.null(card_player()))
    click('p2')
    session$setInputs(award_closed = 1L)
    stopifnot(opened == 1L, card_player()$id == 'p2')
    click('p11')
    stopifnot(card_player()$id == 'p11', card_player()$name == 'Shared name', is.null(card_player()$points))
    catalog(data.frame(id = 'p11', name = 'Current name', points = 42))
    stopifnot(card_player()$name == 'Current name', card_player()$points == 42)
    league('other')
    session$flushReact()
    click('p2')
    stopifnot(is.null(card_player()), opened == 1L)
  })
  selected_player_Server <- original_card
  shared_mvp_source <- original_source
  factual_read_adapter <- original_adapter
})
fixture <- tags$html(tags$head(tags$meta(name = 'viewport', content = 'width=device-width, initial-scale=1'),
  tags$style(HTML(paste(':root { --fm-muted: #aaa; --fm-border: #345; --fm-surface: #0b120d; --fm-text: #eee; --fm-warning: #ffc107; } body { margin: 0; padding: 16px; background: #050805; font-family: monospace; } * { box-sizing: border-box; }',
    paste(readLines('www/custom_style.css', warn = FALSE), collapse = '\n'))))),
  tags$body(tags$div(class = 'round-mvps-grid', cards[1:2])))
htmltools::save_html(fixture, '/tmp/futmondo-round-awards.html')
cat(sprintf('Round awards: %d passed / 0 failed\n', n))
