#!/usr/bin/env Rscript
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source('global.R'))
players <- data.frame(id = paste0('p', 11:1), name = 'Catalog name',
  team = 'Sevilla', logo = 'source-club.png')
xi <- data.frame(player_id = paste0('p', 1:11), player_name = paste('Award player', 1:11),
  points = 1:11, is_mvp = 1:11 == 2, display_role = 'DEF')
players$team[players$id == 'p1'] <- 'Athletic Club'
players$logo[players$id == 'p1'] <- 'athletic-authoritative.png'
players$logo[players$id == 'p3'] <- NA
xi$player_name[4] <- '<unsafe>'
mvp <- data.frame(round_number = 1, player_name = 'Award player 2', points = 2)
html <- htmltools::renderTags(round_award_card(mvp, xi, players))$html
dom <- xml2::read_html(html)
cells <- xml2::xml_find_all(dom, '//tbody/tr/td[2]')
stopifnot(length(cells) == 11L,
  xml2::xml_attr(xml2::xml_find_first(cells[[1]], './/img'), 'src') == paste0(TEAM_LOGO_URL, 'athletic-authoritative.png'),
  xml2::xml_attr(xml2::xml_find_first(cells[[1]], './/img'), 'alt') == 'Athletic Club',
  xml2::xml_attr(xml2::xml_find_first(cells[[3]], './/img'), 'src') == paste0(TEAM_LOGO_URL, 'sevilla.png'),
  xml2::xml_text(xml2::xml_find_first(cells[[1]], './span/span')) == 'Award player 1',
  xml2::xml_name(xml2::xml_children(xml2::xml_find_first(cells[[1]], './span'))[[1]]) == 'img',
  length(xml2::xml_find_all(dom, '//tbody//img[@loading="lazy" and @decoding="async"]')) == 11L,
  grepl('&lt;unsafe&gt;', html, fixed = TRUE), !grepl('<unsafe>', html, fixed = TRUE),
  !grepl('Catalog name', html, fixed = TRUE))
cat('PASS crests precede official names, map by ID, prefer source logos, and escape names\n')
missing <- htmltools::renderTags(round_award_card(mvp, xi, players[-1, ]))$html
stopifnot(grepl('Award player 11', missing, fixed = TRUE),
  length(xml2::xml_find_all(xml2::read_html(missing), '//tbody//img')) == 10L)
cat('PASS unmatched players retain their official names without invented crests\n')
rows <- transform(xi, round_number = 1, round_id = 'r1', player_role = 'defensa', is_finished = TRUE, persistence = 'Saved',
  award_team = '', award_logo = '')
rows$award_team[2] <- 'Earlier club'
rows$award_logo[2] <- 'https://club.example/earlier.webp'
rows$player_role[2] <- 'defensa/delantero'
rows$player_role[1] <- NA_character_
players$role <- 'portero'
players$role2 <- 'centrocampista'
shared_mvp_source <- function(...) reactiveVal(list(rows = rows, reason = ''))
state <- reactiveVal(list(data = players, status = 'ready'))
reads <- 0L
factual_read_adapter <- function(...) function(...) { reads <<- reads + 1L; state() }
shiny::testServer(round_mvps_Server, args = list(is_module_active = function() TRUE,
  login_token = function() c(userid = 'fixture', token = 'synthetic'), championship_id = function() 'league'), {
  rendered <- output$mvp_cards$html
  stopifnot(length(xml2::xml_find_all(xml2::read_html(rendered), '//tbody//img')) == 11L, reads == 1L)
  session$setInputs(award_tab = 'statistics')
  stats_dom <- xml2::read_html(output$award_statistics$html)
  widget <- jsonlite::fromJSON(xml2::xml_text(xml2::xml_find_first(stats_dom, '//script[@data-for]')), simplifyVector = FALSE)$x$tag$attribs
  names_in_order <- unlist(widget$data$player_name)
  mvp_chart <- xml2::xml_find_first(stats_dom, '//figure[contains(@class,"award-chart-mvp")]//li')
  crest <- xml2::xml_find_first(mvp_chart, './/img')
  mvp <- match('Award player 2', names_in_order)
  fallback <- match('Award player 1', names_in_order)
  stopifnot(xml2::xml_attr(crest, 'src') == 'https://club.example/earlier.webp',
    xml2::xml_attr(crest, 'alt') == 'Earlier club',
    xml2::xml_text(xml2::xml_find_first(mvp_chart, './/small')) == 'DEF / FWD',
    widget$data$crest[[mvp]] == xml2::xml_attr(crest, 'src'), widget$data$club[[mvp]] == 'Earlier club',
    widget$data$position[[mvp]] == 'DEF / FWD', widget$data$club[[fallback]] == 'Athletic Club',
    widget$data$position[[fallback]] == 'GK / MID', widget$data$position_title[[fallback]] == 'Current catalog positions',
    '<unsafe>' %in% names_in_order, !isTRUE(widget$columns[[1]]$html))
  session$flushReact(); stopifnot(reads == 1L)
  state(list(data = NULL, status = 'loading')); session$flushReact()
  rendered <- output$mvp_cards$html
  stopifnot(grepl('earlier.webp', output$award_statistics$html, fixed = TRUE),
    !grepl('athletic-authoritative.png', output$award_statistics$html, fixed = TRUE),
    !grepl('fm-info-error|Loading player team crests', rendered),
    length(xml2::xml_find_all(xml2::read_html(rendered), '//tbody/tr')) == 11L)
  state(list(data = NULL, status = 'error', error = resource_failure(category = 'connection'))); session$flushReact()
  rendered <- output$mvp_cards$html
  stopifnot(grepl('Player team crests:', rendered, fixed = TRUE),
    length(xml2::xml_find_all(xml2::read_html(rendered), '//tbody/tr')) == 11L)
  state(list(data = players, status = 'ready')); session$flushReact()
  stopifnot(length(xml2::xml_find_all(xml2::read_html(output$mvp_cards$html), '//tbody//img')) == 11L,
    grepl('athletic-authoritative.png', output$award_statistics$html, fixed = TRUE))
})
cat('PASS one shared catalog read, stable redraws, lineups during loading, visible failure and reactive recovery\n')
elapsed <- system.time(for (i in 1:38) htmltools::renderTags(round_award_card(mvp, xi, players)))[['elapsed']]
cat(sprintf('PERFORMANCE 38 rounds / 418 player cells with crests: %.3f seconds\n', elapsed))
