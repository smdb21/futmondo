#!/usr/bin/env Rscript
# Focused offline checks for verified per-player market listing actions.
suppressPackageStartupMessages({library(shiny); library(dplyr); library(reactable)})
source('data_contracts.R')
source('Modules/Players_in_Teams_Module.R')
source('futmondo_functions.R')
checks <- 0L
check <- function(name, code) { force(code); checks <<- checks + 1L; cat('[PASS]', name, '\n') }

roster <- data.frame(
  id = c('value', 'closed', 'open', 'missing'), value = c(100, 200, 300, NA),
  clause_price = c(NA, 1000, 2000, NA),
  clause_transferred = c(NA, FALSE, FALSE, FALSE),
  clause_date = c(NA, '2026-10-01T00:00:00Z', '2026-01-01T00:00:00Z', NA),
  market_inMarket = c(FALSE, TRUE, FALSE, FALSE), stringsAsFactors = FALSE
)
now <- as.POSIXct('2026-09-13 00:00:00', tz = 'UTC')
check('bulk value pricing lists every player with a valid valuation', {
  plan <- bulk_market_listing_plan(roster, 'value', now = now)
  stopifnot(identical(plan$entries$player_id, c('value', 'closed', 'open')),
            identical(plan$entries$price, c(100, 200, 300)), plan$skipped == 1L)
})
check('closed clause pricing applies premium and excludes open or missing clauses', {
  plan <- bulk_market_listing_plan(roster, 'closed_clause', clause_premium_pct = 5, now = now)
  stopifnot(identical(plan$entries$player_id, 'closed'), identical(plan$entries$price, 1050),
            isTRUE(plan$entries$already_listed), plan$skipped == 3L)
})
check('listing update withdraws then lists at the replacement price', {
  old_cancel <- cancel_player_sell; old_list <- put_player_on_market
  on.exit({cancel_player_sell <<- old_cancel; put_player_on_market <<- old_list}, add = TRUE)
  calls <- list()
  cancel_player_sell <<- function(...) { calls[[length(calls) + 1L]] <<- 'cancel'; list(success = TRUE) }
  put_player_on_market <<- function(...){ calls[[length(calls) + 1L]] <<- 'list'; list(success = TRUE) }
  result <- update_player_market_listing(list(), 'c', 't', 'p', 100)
  stopifnot(isTRUE(result$success), identical(calls, list('cancel', 'list')))
})
check('listing update reports a failed replacement after withdrawal', {
  old_cancel <- cancel_player_sell; old_list <- put_player_on_market
  on.exit({cancel_player_sell <<- old_cancel; put_player_on_market <<- old_list}, add = TRUE)
  cancel_player_sell <<- function(...) list(success = TRUE)
  put_player_on_market <<- function(...) list(success = FALSE, message = 'Rejected')
  result <- update_player_market_listing(list(), 'c', 't', 'p', 100)
  stopifnot(!isTRUE(result$success), identical(result$message, 'Rejected'))
})
cat(sprintf('Market listing actions: %d checks passed.\n', checks))
