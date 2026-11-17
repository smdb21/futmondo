#!/usr/bin/env Rscript
# Exercise the deployed classification path with synthetic persisted data only.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source('global.R'))
auth <- c(userid = 'bonus-db-fixture', token = 'synthetic')
ctx <- resource_context(auth, 'league', 'a', competition_id = 'competition',
  season = 'season', scoring_version = 'score')
remember_league_context(list(account_id = ctx$account_id, championship_id = ctx$championship_id,
  competition_id = ctx$competition_id, season = ctx$season, scoring_version = ctx$scoring_version,
  observed_at = rival_finance_stamp(), expires_at = rival_finance_stamp(Sys.time() + 3600), complete = TRUE, payload = list()))
teams <- data.frame(teamid = c('a', 'b'), teamname = c('Alpha', 'Beta'), points = c(15, 10))
evidence <- lapply(teams$teamid, function(id) rival_finance_evidence(ctx$account_id, 'league', 'season',
  'team_rounds', id, list(team_id = id, rounds = list(
    list(id = 'r1', number = 1, points = 5, status = 'closed', negative = FALSE),
    list(id = 'r2', number = 2, points = 10, status = 'closed', negative = FALSE)))))
cache <- rival_calculation_cache()
calculated <- build_rival_cash_dataset(teams, 'league', evidence,
  rules = list(initial_budget = 100, money_per_point = 2, ranking_pool = 0, dreamteam_reward = 0, mvp_reward = 0),
  account_id = ctx$account_id, season = ctx$season, cache = cache)
totals <- data.frame(round_number = rep(1:2, each = 4),
  bonus_type = rep(c('points', 'mvp', 'dreamteam', 'ranking'), 2), amount = c(10, 10, 5, 0, 20, 0, 15, 7))
attr(calculated$cash_ledgers$a, 'round_bonus_totals') <- totals
attr(calculated$cash_ledgers$b, 'round_bonus_totals') <- transform(totals, amount = 0)
cache$prepared$padding <- raw(.rival_snapshot_limits$json + 1L)
make_row <- function(value = calculated) {
  payload <- rival_snapshot_payload(cache, value, ctx)
  stopifnot(!is.null(payload), !is.null(payload$ledger_archive))
  row <- rival_finance_evidence(ctx$account_id, 'league', 'season', 'eligibility',
    rival_snapshot_id(rival_snapshot_scope(ctx)), payload, competition_id = ctx$competition_id)
  # Actual PostgREST projection omits the larger ledger archive/manifest.
  c(row[setdiff(names(row), 'payload')], payload[c('kind', 'encoding', 'data', 'sha256', 'json_bytes')])
}
row <- make_row()
# Fixed time makes worker-tick coalescing checks independent of minute edges.
clock <- base::Sys.time()
Sys.time <- function() clock
fm_worker_configured <- function() TRUE
get_sb_url <- function() 'https://database.synthetic.invalid'
get_sb_key <- function() 'synthetic-key'
requests <- list(); pending <- list()
fm_async_read_http <- function(url, method = 'GET', body = NULL, headers = character(), owner = '') {
  stopifnot(method == 'GET', grepl('/rest/v1/rival_finance_evidence_latest', url, fixed = TRUE))
  query <- httr::parse_url(url)$query
  stopifnot(query$limit == '1', query$account_id == paste0('eq.', ctx$account_id),
    query$championship_id == 'eq.league', query$competition_id == 'eq.competition', query$season == 'eq.season',
    grepl('data:payload->>data', query$select, fixed = TRUE), !grepl('ledger', query$select, fixed = TRUE),
    query$source_id == paste0('eq.', rival_snapshot_id(rival_snapshot_scope(ctx))))
  requests[[length(requests) + 1L]] <<- query
  promises::promise(function(resolve, reject) pending[[length(pending) + 1L]] <<- list(resolve = resolve, reject = reject))
}
respond <- function(i, rows = list(row)) pending[[i]]$resolve(list(status_code = 200L,
  content = charToRaw(jsonlite::toJSON(rows, auto_unbox = TRUE, null = 'null', na = 'null', digits = NA)), times = c(total = .001)))
settle <- function(session) for (i in 1:20) { later::run_now(.001); session$flushReact() }
# Other already-mounted standings sources are held constant. The finance
# controller must never be constructed on this production display path.
bind_factual_reads <- function(...) invisible(NULL)
shared_mvp_source <- function(...) reactive(list())
shared_rival_finance_source <- function(...) stop('Classification started the acquisition controller')
get_classification_round_catalog <- function(...) list(rounds = data.frame(
  round_id = c('r1', 'r2'), round_number = 1:2, is_finished = TRUE, status = 'closed'))
get_user_team_rounds <- function(...) list(list(id = 'r1', points = 5), list(id = 'r2', points = 10))
get_round_dreamteam <- function(...) list()
refresh <- reactiveVal(0L)
args <- list(is_module_active = reactive(TRUE), login_token = reactive(auth),
  championship_id = reactive('league'), user_team_id = reactive('a'), user_teams_RV = reactive(teams),
  refresh_trigger = refresh, league_settings_source = reactive(list(league = list(configuration = list()))))
clear_api_cache()
shiny::testServer(classification_Server, args = args, {
  session$setInputs(classification_view = 'partial', round_range_slider = c(1, 2))
  settle(session)
  stopifnot(length(requests) == 1L, nrow(standings_RV()) == 2L,
    grepl('Loading', output$classification_table, fixed = TRUE))
  # A held DB request must not block another Shiny input/standings render.
  session$setInputs(round_range_slider = c(1, 1)); settle(session)
  stopifnot(length(requests) == 1L, all(standings_RV()$points == 5))
  respond(1L); settle(session)
  a <- standings_RV()[standings_RV()$team_id == 'a', ]
  stopifnot(a$points_bonus == 10, a$mvp_bonus == 10, a$best_xi_bonus == 5, a$position_bonus == 0,
    !grepl('Loading', output$classification_table, fixed = TRUE))
  session$setInputs(round_range_slider = c(1, 2)); settle(session)
  a <- standings_RV()[standings_RV()$team_id == 'a', ]
  stopifnot(a$points_bonus == 30, a$mvp_bonus == 10, a$best_xi_bonus == 20, a$position_bonus == 7,
    length(requests) == 1L, !isTRUE(finance_data_RV()$loading))
  # Existing five-second worker ticks don't repeatedly read the DB.
  for (i in 1:3) { fm_worker_notify(auth[['userid']]); settle(session) }
  stopifnot(length(requests) == 1L)
  # Refresh is one DB reread. Existing numeric totals remain mounted.
  refresh(1L); settle(session)
  stopifnot(length(requests) == 2L, standings_RV()$mvp_bonus[1] == 10,
    !grepl('Loading', output$classification_table, fixed = TRUE))
  partial <- calculated
  attr(partial$cash_ledgers$a, 'round_bonus_totals')$amount[2] <- NA_real_
  respond(2L, list(make_row(partial))); settle(session)
  stopifnot(is.na(standings_RV()$mvp_bonus[1]), standings_RV()$best_xi_bonus[1] == 20,
    grepl('MVP (rounds 1)', output$classification_status$html, fixed = TRUE),
    !grepl('Loading', output$classification_table, fixed = TRUE))
})
cat('PASS delayed single DB query, responsive standings, known totals/zero/NA, range reuse and DB-only Refresh\n')

# Reopening after all process caches are cleared must restore from the database.
clear_api_cache(); refresh(0L)
shiny::testServer(classification_Server, args = args, {
  session$setInputs(classification_view = 'partial', round_range_slider = c(1, 2)); settle(session)
  stopifnot(length(requests) == 3L)
  respond(3L); settle(session)
  stopifnot(standings_RV()$best_xi_bonus[1] == 20)
  refresh(2L); settle(session)
  pending[[4L]]$reject(simpleError('Synthetic database outage')); settle(session)
  stopifnot(standings_RV()$best_xi_bonus[1] == 20, nzchar(finance_data_RV()$finance_error))
})
cat('PASS database reuse after memory reset and retained saved totals on failed DB refresh\n')

# The existing status tick drives bounded DB refresh without a new timer.
clear_api_cache()
enabled <- reactiveVal(TRUE)
shiny::testServer(function(input, output, session) {
  feed <- classification_saved_bonus_source(session, function() auth, function() 'league',
    function() 'a', enabled)
}, {
  settle(session); stopifnot(length(requests) == 5L)
  respond(5L); settle(session)
  stopifnot(!feed()$loading)
  clock <<- clock + 60
  fm_worker_notify(auth[['userid']]); settle(session)
  stopifnot(length(requests) == 6L)
  respond(6L, list()); settle(session)
  stopifnot(!feed()$loading, !length(feed()$cash_ledgers), grepl('No saved', feed()$reason))
  enabled(FALSE); settle(session)
  clock <<- clock + 60
  fm_worker_notify(auth[['userid']]); settle(session)
  stopifnot(length(requests) == 6L)
})
cat('PASS at most one DB reread per minute, no inactive reads and absent rows finish with explicit missing state\n')

# A late response from the old account cannot populate the new account's page.
clear_api_cache()
selected_auth <- reactiveVal(auth)
shiny::testServer(function(input, output, session) {
  feed <- classification_saved_bonus_source(session, selected_auth, function() 'league',
    function() 'a', function() TRUE)
}, {
  settle(session); stopifnot(length(requests) == 7L)
  ctx$account_id <<- 'another-bonus-account'
  remember_league_context(list(account_id = ctx$account_id, championship_id = 'league',
    competition_id = 'competition', season = 'season', scoring_version = 'score',
    observed_at = rival_finance_stamp(), expires_at = rival_finance_stamp(Sys.time() + 3600), complete = TRUE, payload = list()))
  selected_auth(c(userid = ctx$account_id, token = 'synthetic')); settle(session)
  stopifnot(length(requests) == 8L, !length(feed()$cash_ledgers))
  respond(7L); settle(session)
  stopifnot(isTRUE(feed()$loading), !length(feed()$cash_ledgers))
  respond(8L, list()); settle(session)
  stopifnot(!feed()$loading, !length(feed()$cash_ledgers))
})
ctx$account_id <- auth[['userid']]
cat('PASS account change discards a held old-account database response\n')

# Query projection validates scope, hash and numeric coverage just like full reads.
reads <- 0L
supabase_get <- function(...) { reads <<- reads + 1L; jsonlite::fromJSON(jsonlite::toJSON(list(row), auto_unbox = TRUE, digits = NA)) }
read_args <- ctx[c('account_id', 'championship_id', 'season', 'user_team_id', 'competition_id', 'scoring_version')]
saved <- do.call(read_rival_bonus_snapshot, read_args)
stopifnot(reads == 1L, identical(attr(saved$cash_ledgers$a, 'round_bonus_totals'), totals),
  all(vapply(saved$cash_ledgers, nrow, integer(1)) == 0L), !saved$loading)
valid <- row
for (field in c('account_id', 'championship_id', 'competition_id', 'season', 'source_id', 'sha256')) {
  row <- valid; row[[field]] <- 'foreign'
  stopifnot(!length(do.call(read_rival_bonus_snapshot, read_args)$cash_ledgers))
}
row <- valid
elapsed <- system.time(for (i in 1:100) do.call(read_rival_bonus_snapshot, read_args))[['elapsed']]
cat(sprintf('PASS compact scope/integrity validation; 100 local compact reads/decode/projections %.3fs; zero source requests/writes\n', elapsed))
