#!/usr/bin/env Rscript
# Offline regression: Today must use its existing finance snapshot for both KPIs.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({
  library(shiny)
  source('futmondo_functions.R')
  source('factual_helpers.R')
  source('Modules/Selected_Player_Module.R')
  source('Modules/Today_Module.R')
})
# The shared table has separate coverage; isolate the Today KPI dependency here.
players_table_Server <- function(...) invisible(NULL)
get_market_players <- function(...) data.frame()
get_championship_pressroom <- function(...) data.frame()
finance_reads <- 0L
team_reads <- 0L
fixture_snapshot <- NULL
get_financial_snapshot <- function(...) {
  finance_reads <<- finance_reads + 1L
  fixture_snapshot
}
auth <- c(userid='today-fixture',token='offline-only')
check_snapshot <- function(snapshot, cash_label, value_label) {
  clear_api_cache()
  fixture_snapshot <<- snapshot
  finance_reads <<- 0L
  team_reads <<- 0L
  testServer(today_Server, args=list(
    is_module_active=reactive(TRUE), login_token=reactive(auth),
    championship_id=reactive('league'), user_team_id=reactive('team'),
    user_teams_RV=reactive({team_reads <<- team_reads + 1L; stop('Teams unavailable')}),
    refresh_trigger=reactive(0L)), {
    cash <- valuation <- NULL
    deadline <- Sys.time()+3
    while ((is.null(cash)||is.null(valuation)) && Sys.time()<deadline) {
      session$flushReact(); later::run_now(0.01)
      cash <- tryCatch(output$kpi_cash_box, error=function(e)NULL)
      valuation <- tryCatch(output$kpi_valuation_box, error=function(e)NULL)
    }
    stopifnot(!is.null(cash), !is.null(valuation),
      grepl(cash_label, paste(unlist(cash),collapse=''), fixed=TRUE),
      grepl(value_label, paste(unlist(valuation),collapse=''), fixed=TRUE),
      finance_reads==1L, team_reads==0L)
    invisible(output$kpi_cash_box); invisible(output$kpi_valuation_box)
    session$flushReact()
    stopifnot(finance_reads==1L, team_reads==0L)
  })
}
check_snapshot(list(status='partial',cash=142637644,team_value=34000000),
  '142.637.644 €','34.000.000 €')
cat('PASS partial finance renders both KPIs despite unavailable league teams; one shared read, zero teams reads\n')
check_snapshot(list(status='partial',cash=0,team_value=NA_real_), '0 €', 'Unavailable')
cat('PASS unknown team value stays unavailable while zero cash remains visible\n')
check_snapshot(list(status='ok',cash=-50,team_value=0), '-50 €', '0 €')
cat('PASS negative cash and genuine zero team value remain visible\n')
cat('TODAY AVAILABILITY: 3 passed / 0 failed\n')
