#!/usr/bin/env Rscript
# Deterministic pricing and manual-action checks; no credentials or network.
suppressPackageStartupMessages({library(shiny); library(dplyr); library(reactable)})
options(futmondo.offline = TRUE)
source('data_contracts.R')
source('Modules/Players_in_Teams_Module.R')
source('Modules/Selected_Player_Module.R')
checks <- 0L
check <- function(name, code) { force(code); checks <<- checks + 1L; cat('[PASS]', name, '\n') }
now <- as.POSIXct('2026-11-10 12:00:00', tz = 'UTC')
roster <- data.frame(id = c('p1', 'p2', 'p3', 'p4'), name = c('<Player>', 'Player 2', 'Player 3', 'Player 4'),
  value = c(100, 201, 2e6, NA), clause_price = c(100, 201, 2e6, NA),
  clause_transferred = c(FALSE, FALSE, TRUE, FALSE),
  clause_date = c('2026-12-01T00:00:00Z', '2026-10-01T00:00:00Z', '2026-12-01T00:00:00Z', NA),
  market_inMarket = c(FALSE, TRUE, FALSE, FALSE), stringsAsFactors = FALSE)

check('all six premiums use exact whole-euro increases for both bases', {
  fixture <- roster[3, , drop = FALSE]; fixture$clause_transferred <- FALSE
  for (pct in c(0.5, 1, 2, 4, 5, 10)) for (mode in c('value', 'closed_clause')) {
    entries <- bulk_market_listing_plan(fixture, mode, now = now, premium_pct = pct)$entries
    expected <- switch(as.character(pct), '0.5'=10000, '1'=20000, '2'=40000, '4'=80000, '5'=100000, '10'=200000)
    stopifnot(entries$base_price == 2e6, entries$increase == expected, entries$price == 2e6 + expected)
  }
})
check('rounding adds no spurious euro and totals sum individual rounded prices', {
  plan <- bulk_market_listing_plan(roster, 'value', now = now, premium_pct = 10)
  stopifnot(identical(plan$entries$price, c(110, 222, 2200000)),
    identical(plan$entries$increase, c(10, 21, 200000)), plan$skipped == 1L)
  fractional <- bulk_market_listing_plan(roster[1:2, ], 'value', premium_pct = 0.5)
  stopifnot(identical(fractional$entries$price, c(101, 203)), sum(fractional$entries$increase) == 3)
  custom <- bulk_market_listing_plan(roster[3, ], 'value', premium_pct = 1.25)
  stopifnot(custom$entries$increase == 25000, custom$entries$price == 2025000)
  decimal <- bulk_market_listing_plan(data.frame(id='p', value=10000), premium_pct=0.07)
  stopifnot(decimal$entries$increase == 7, decimal$entries$price == 10007)
  fraction <- bulk_market_listing_plan(data.frame(id='p', value=10000), premium_pct=0.070000001)
  stopifnot(fraction$entries$increase == 8)
  tiny <- bulk_market_listing_plan(data.frame(id='p', value=100), premium_pct=1e-20)
  stopifnot(tiny$entries$increase == 1)
})
check('zero and legacy defaults stay explicit, closed clauses retain eligibility', {
  zero <- bulk_market_listing_plan(roster, 'value', premium_pct = 0)
  stopifnot(identical(zero$entries$price, c(100, 201, 2e6)), all(zero$entries$increase == 0), zero$skipped == 1L)
  clause <- bulk_market_listing_plan(roster, 'closed_clause', now = now, premium_pct = 10)
  stopifnot(identical(clause$entries$player_id, 'p1'), clause$entries$price == 110, clause$skipped == 3L)
  stopifnot(bulk_market_listing_plan(roster, 'closed_clause', now = now)$entries$price == 105,
    identical(bulk_market_listing_plan(roster)$entries$price, zero$entries$price))
})
check('invalid premiums and unavailable values never become a substituted price', {
  for (pct in list(NULL, numeric(), NA, NaN, Inf, -0.5, 1000.01, '', 'invalid', c(1, 2))) {
    plan <- bulk_market_listing_plan(roster, premium_pct = pct)
    stopifnot(!nrow(plan$entries), identical(plan$message, 'Enter a premium between 0 and 1000%.'))
  }
  bad <- data.frame(id = c('zero', 'negative', 'unknown', ''), value = c(0, -5, NA, 100))
  stopifnot(!nrow(bulk_market_listing_plan(bad, premium_pct = 10)$entries))
})
check('preview uses the plan exactly, escapes names and discloses skipped players', {
  plan <- bulk_market_listing_plan(roster, 'value', premium_pct = 10)
  html <- htmltools::renderTags(bulk_market_listing_preview(plan, roster))$html
  stopifnot(grepl('Total increase: +200.031 €', html, fixed = TRUE),
    grepl('Base: 2.000.301 €', html, fixed = TRUE), grepl('Listing: 2.200.332 €', html, fixed = TRUE),
    grepl('Extra: +10 €', html, fixed = TRUE), grepl('Listing: 110 €', html, fixed = TRUE),
    grepl('&lt;Player&gt;', html, fixed = TRUE), !grepl('<Player>', html, fixed = TRUE),
    grepl('1 player(s) skipped', html, fixed = TRUE))
  invalid <- bulk_market_listing_plan(roster, premium_pct = NULL)
  html <- htmltools::renderTags(bulk_market_listing_preview(invalid, roster))$html
  stopifnot(grepl('Enter a premium', html, fixed = TRUE), !grepl('Total increase', html, fixed = TRUE))
})

# Exercise the actual event body with deterministic manual-action boundaries.
find_event <- function(x) {
  if (is.call(x) && identical(x[[1]], as.name('observeEvent')) &&
      identical(paste(deparse(x[[2]]), collapse=''), 'input$submit_put_all_on_market')) return(x[[3]])
  if (is.call(x) || is.expression(x) || is.pairlist(x)) for (y in as.list(x)) {
    if (missing(y)) next
    result <- find_event(y); if (!is.null(result)) return(result)
  }
  NULL
}
handler <- find_event(parse('Modules/Players_in_Teams_Module.R'))
stopifnot(!is.null(handler))
make_fixture <- function(pct = 10, expected_context = list(team='team')) {
  env <- new.env(parent = globalenv())
  env$login_token <- 'fixture'; env$championship_id <- 'champ'; env$user_team_id <- 'team'
  env$get_reactive_val <- identity
  env$input <- list(bulk_listing_price_mode = 'value', bulk_listing_clause_premium = pct)
  env$current_context <- function() list(team='team')
  env$bulk_context <- local({context <- expected_context; function(value) {if (!missing(value)) context <<- value; context}})
  env$players_table_RV <- function() roster[1:2, ]
  env$calls <- list(); env$invalidations <- 0L; env$refreshes <- 0L
  record <- function(kind, login, champ, team, player, price) {
    env$calls[[length(env$calls) + 1L]] <- list(kind=kind, player=player, price=price)
    list(success=TRUE)
  }
  env$put_player_on_market <- function(...) record('new', ...)
  env$update_player_market_listing <- function(...) record('update', ...)
  env$invalidate_manual_resources <- function(...) {env$invalidations <- env$invalidations + 1L}
  env$refresh_trigger <- function(value) {if (!missing(value)) env$refreshes <- env$refreshes + 1L; env$refreshes}
  env$removeModal <- function(...) NULL
  env
}
session <- shiny::MockShinySession$new()
check('confirmed prices match preview with one context consumption and one refresh', {
  fixture <- make_fixture()
  expected <- bulk_market_listing_plan(roster[1:2, ], 'value', premium_pct = 10)$entries
  shiny::withReactiveDomain(session, eval(handler, fixture))
  stopifnot(identical(vapply(fixture$calls, `[[`, numeric(1), 'price'), expected$price),
    identical(vapply(fixture$calls, `[[`, character(1), 'kind'), c('new', 'update')),
    is.null(fixture$bulk_context()), fixture$invalidations == 1L, fixture$refreshes == 1L)
  shiny::withReactiveDomain(session, eval(handler, fixture))
  stopifnot(length(fixture$calls) == 2L, fixture$refreshes == 1L)
})
check('invalid input is correctable in-place and stale team confirmation cannot submit', {
  fixture <- make_fixture(NULL)
  shiny::withReactiveDomain(session, eval(handler, fixture))
  stopifnot(!length(fixture$calls), !is.null(fixture$bulk_context()), fixture$invalidations == 0L)
  fixture$input$bulk_listing_clause_premium <- 0.5
  shiny::withReactiveDomain(session, eval(handler, fixture))
  stopifnot(identical(vapply(fixture$calls, `[[`, numeric(1), 'price'), c(101, 203)))
  fixture <- make_fixture(expected_context = list(team='old-team'))
  shiny::withReactiveDomain(session, eval(handler, fixture))
  stopifnot(!length(fixture$calls), fixture$invalidations == 0L)
})
check('closed-clause submission sends exactly the eligible preview prices', {
  fixture <- make_fixture(0.5)
  fixture$input$bulk_listing_price_mode <- 'closed_clause'
  closed_roster <- roster[1:2, ]
  closed_roster$clause_date <- format(Sys.time() + 86400, '%Y-%m-%dT%H:%M:%SZ', tz='UTC')
  fixture$players_table_RV <- function() closed_roster
  expected <- bulk_market_listing_plan(closed_roster, 'closed_clause', premium_pct=0.5)$entries
  shiny::withReactiveDomain(session, eval(handler, fixture))
  stopifnot(identical(vapply(fixture$calls, `[[`, numeric(1), 'price'), expected$price),
    length(fixture$calls) == 2L, fixture$invalidations == 1L, fixture$refreshes == 1L)
})
session$close()

check('repeated squad previews have bounded local cost', {
  fixture <- roster[rep(3, 30), ]; fixture$id <- paste0('p', seq_len(30))
  elapsed <- system.time(for (i in seq_len(100)) {
    plan <- bulk_market_listing_plan(fixture, premium_pct = c(0.5, 1, 2, 4, 5, 10)[(i %% 6) + 1])
    html <- htmltools::renderTags(bulk_market_listing_preview(plan, fixture))$html
  })[['elapsed']]
  stopifnot(nrow(plan$entries) == 30L, nchar(html) < 50000L)
  cat(sprintf('100 synthetic 30-player previews: %.3fs total; %.1f KiB HTML.\n', elapsed, nchar(html)/1024))
})
cat(sprintf('Bulk market premium: %d checks passed.\n', checks))
