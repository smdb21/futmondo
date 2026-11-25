#!/usr/bin/env Rscript
# Offline regression: Today’s cash uses the shared primary team information.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({
  source('global.R')
})
fm_worker_configured <- function() FALSE
# The shared table has separate coverage; isolate the Today KPI dependency here.
players_table_Server <- function(...) invisible(NULL)
get_market_players <- function(...) data.frame()
get_championship_pressroom <- function(...) data.frame()
finance_reads <- 0L
team_reads <- 0L
fixture_snapshot <- NULL
get_user_team_info <- function(...) {
  finance_reads <<- finance_reads + 1L
  fixture_snapshot
}
auth <- c(userid='today-fixture',token='offline-only')
check_snapshot <- function(snapshot, cash_label) {
  clear_api_cache()
  fixture_snapshot <<- list(budget=snapshot$cash,teamValue=snapshot$team_value)
  finance_reads <<- 0L
  team_reads <<- 0L
  testServer(today_Server, args=list(
    is_module_active=reactive(TRUE), login_token=reactive(auth),
    championship_id=reactive('league'), user_team_id=reactive('team'),
    user_teams_RV=reactive({team_reads <<- team_reads + 1L; stop('Teams unavailable')}),
    refresh_trigger=reactive(0L)), {
    deadline <- Sys.time()+3
    repeat {
      session$flushReact(); later::run_now(0.01)
      cash <- tryCatch(output$kpi_cash_box, error=function(e)NULL)
      ready <- grepl(cash_label, paste(unlist(cash),collapse=''), fixed=TRUE)
      if (ready || Sys.time()>=deadline) break
    }
    stopifnot(ready, finance_reads==1L, team_reads==0L)
    invisible(output$kpi_cash_box)
    session$flushReact()
    stopifnot(finance_reads==1L, team_reads==0L)
  })
}
stopifnot(!grepl('kpi_valuation_box',as.character(today_UI('today')),fixed=TRUE))
check_snapshot(list(cash=142637644,team_value=34000000), '142.637.644 €')
cat('PASS Today shows cash only, with one shared read and zero league-team reads\n')
check_snapshot(list(cash=0,team_value=NA_real_), '0 €')
cat('PASS zero cash remains visible when the team value is unknown\n')
check_snapshot(list(cash=-50,team_value=0), '-50 €')
cat('PASS negative cash remains visible\n')
cat('TODAY AVAILABILITY: 3 passed / 0 failed\n')
