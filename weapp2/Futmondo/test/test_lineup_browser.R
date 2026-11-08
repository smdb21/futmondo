# Offline browser fixture for the editable Lineup module. No account requests.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source('global.R'))
for (file in c('lineup_runtime.R', 'lineup_api.R', 'lineup_store.R', 'Modules/Lineup_Module.R')) source(file)

fixture_roster <- data.frame(
  id = paste0('p', 1:24), name = paste('Player', 1:24),
  role = c(rep('portero', 2), rep('defensa', 6), rep('centrocampista', 8), rep('delantero', 8)),
  role2 = '', status = 'ok', photo = '', club = 'Fixture Club', logo = 'fixture-club.png',
  points = 100 + 1:24, buyPrice = 1000000, value = 2000000, stringsAsFactors = FALSE)
fixture_roster$role2[8] <- 'centrocampista'
fixture_roster$status[24] <- 'injured'
fixture_rounds <- data.frame(round_id = paste0('r', 1:6), round_number = 1:6,
  is_finished = c(rep(TRUE, 5), FALSE),
  begin_process = format(Sys.time() + c(-5:-1 * 86400, 3600), '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC'))
fixture_history <- expand.grid(player_id = fixture_roster$id, round = 1:5, stringsAsFactors = FALSE)
fixture_history$points <- as.numeric(sub('p', '', fixture_history$player_id))
fixture_history$points[fixture_history$player_id == 'p1'] <- c(100, 100, 100, 0, 0)
fixture_history$points[fixture_history$player_id == 'p2'] <- c(0, 0, 0, 50, 50)
fixture_official <- list(strategy = '4-4-2',
  starters = c('p17', 'p18', 'p9', 'p10', 'p11', 'p12', 'p3', 'p4', 'p5', 'p6', 'p1'),
  bench = c('p7', 'p13', 'p14', 'p19'), captain = 'p18')
fixture_initial <- fixture_official
fixture_reads <- fixture_submissions <- fixture_db_reads <- fixture_db_writes <- 0L
fixture_current_wire <- NULL
fixture_wire <- function(draft) {
  rows <- list()
  for (kind in c('starters', 'bench')) for (i in seq_along(draft[[kind]])) {
    id <- draft[[kind]][i]
    if (nzchar(id)) rows[[length(rows) + 1L]] <- list(`_id` = id, position = i - 1L,
      mp = FALSE, pr = 1000000, b = kind == 'bench', cpt = identical(draft$captain, id))
  }
  list(strategy = draft$strategy, players = rows)
}
lineup_fetch_bundle <- function(auth, context, target, owner) {
  fixture_reads <<- fixture_reads + 1L
  stamp <- Sys.time()
  result <- list(version = 1L,
    roster = lapply(seq_len(nrow(fixture_roster)), function(i) as.list(fixture_roster[i, ])),
    official = fixture_official,
    rules = list(allowed = setdiff(lineup_formations(), '5-2-3'), multiposition = TRUE,
      bench_enabled = TRUE, captain_enabled = TRUE),
    source = list(wire = if (is.null(fixture_current_wire)) fixture_wire(fixture_official) else fixture_current_wire,
      status = '', block_delayed = FALSE, locked = list(),
      target_id = target$id, target_number = target$number,
      target_starts = format(target$starts_at, '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC')),
    season = '2026-27', scoring_version = 'fixture-scoring',
    observed_at = format(stamp, '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC'),
    expires_at = format(stamp + 30, '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC'))
  promises::promise_resolve(result)
}
lineup_snapshot_store <- function(context, target_round, payload = NULL, owner = '') {
  if (is.null(payload)) fixture_db_reads <<- fixture_db_reads + 1L else fixture_db_writes <<- fixture_db_writes + 1L
  promises::promise_resolve(if (is.null(payload)) NULL else TRUE)
}
lineup_submit <- function(auth, cid, tid, current, modified, owner, changes = NULL) {
  fixture_submissions <<- fixture_submissions + 1L
  value <- lineup_empty(modified$strategy)
  for (player in modified$players) {
    key <- if (isTRUE(player$b)) 'bench' else 'starters'
    value[[key]][player$position + 1L] <- player$`_id`
    if (isTRUE(player$cpt)) value$captain <- player$`_id`
  }
  fixture_official <<- value
  fixture_current_wire <<- modified
  promises::promise_resolve(list(answer = list(code = API_CODE_OK)))
}
recent_player_points_source <- function(...) list(history = shiny::reactive(fixture_history),
  status = shiny::reactive(list(pending = 0L, remaining = 0L, failed = 0L, saving = 0L, save_error = NULL)))

ui <- shiny::fluidPage(shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),
  shiny::includeCSS('www/custom_style.css')),
  shiny::textOutput('fixture_counts'), lineup_UI('lineup'))
server <- function(input, output, session) {
  fixture_official <<- fixture_initial
  fixture_current_wire <<- NULL
  lineup_Server('lineup', shiny::reactive(TRUE),
    shiny::reactive(c(userid = 'fixture-account', token = 'fixture-token')),
    shiny::reactive('fixture-league'), shiny::reactive('fixture-team'),
    shiny::reactive(fixture_rounds), history_refresh_trigger = shiny::reactive(0L))
  output$fixture_counts <- shiny::renderText({
    shiny::invalidateLater(100, session)
    paste(fixture_reads, fixture_submissions, fixture_db_reads, fixture_db_writes, sep = '/')
  })
}
shiny::runApp(shiny::shinyApp(ui, server), host = '127.0.0.1',
  port = as.integer(Sys.getenv('FUTMONDO_EDITOR_TEST_PORT', '8542')), launch.browser = FALSE)
