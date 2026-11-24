#!/usr/bin/env Rscript
# Real shared adapters, delayed independent reads; no accounts or HTTP.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth <- c(userid='plots-loading-fixture',token='synthetic')
clear_api_cache()
pending <- new.env(parent=emptyenv())
calls <- character()
async_adapter_fetch <- function(fn,args=list(),context,...) {
 key <- paste(fn,args$user_team_id %||% '',sep=':')
 calls <<- c(calls,key)
 promises::promise(function(resolve,reject) pending[[key]] <- list(resolve=resolve,reject=reject))
}
settle <- function(session) for(i in 1:15) {later::run_now(0);session$flushReact()}
shiny::testServer(plots_Server,args=list(
 is_module_active=reactive(TRUE),login_token=reactive(auth),championship_id=reactive('league'),
 user_team_id=reactive('a'),user_teams_RV=reactive(data.frame(teamid=c('a','b','c'),teamname=c('Alpha','Beta','Gamma')))),{
 session$setInputs(plot_tab='Standings')
 settle(session)
 stopifnot(all(paste0('get_user_team_rounds:',c('a','b','c')) %in% calls),
   'get_classification_round_catalog:' %in% calls,isTRUE(round_state()$loading),
   grepl('Loading saved round scores',as.character(round_status()),fixed=TRUE))
 cat('PASS catalog and every team register together while all reads are pending\n')
 # Explicit native round numbers can render before the optional ID catalog.
 pending[['get_user_team_rounds:b']]$resolve(list(list(number=1,points=0),list(number=2,points=5),list(number=2.2,points=-2)))
 pending[['get_user_team_rounds:c']]$resolve(list(list(number=1,points=4),list(number=2,points=1),list(number=2.2,points=2)))
 settle(session)
 totals <- round_totals()
 stopifnot(identical(totals$points[totals$team_id=='b'],c(0,5,3)),
   all(is.na(totals$points[totals$team_id=='a'])),all(is.na(totals$rank)),isTRUE(round_state()$loading))
 built <- plotly::plotly_build(standings_plot())
 stopifnot(any(vapply(built$x$data,function(x)any(is.finite(x$y)),logical(1))))
 cat('PASS verified curves render while one team and the catalog remain pending; unknown is not zero\n')
 pending[['get_classification_round_catalog:']]$resolve(list(rounds=data.frame(
   round_id=c('r1','r2','r22'),round_number=c(1,2,2.2))))
 pending[['get_user_team_rounds:a']]$reject(simpleError('Synthetic saved history failure'))
 settle(session)
 stopifnot(!round_state()$loading,length(round_state()$errors)==1L,
   grepl('Some saved round data could not be read',as.character(round_status()),fixed=TRUE),
   identical(round_totals()$points[round_totals()$team_id=='b'],c(0,5,3)))
 cat('PASS failed team history preserves other curves and produces an explicit failure\n')
 before <- calls
 session$setInputs(rank_range=c(2,2.2),plot_tab='Rank progression')
 settle(session)
 rank <- plotly::plotly_build(rank_plot())
 stopifnot(identical(calls,before),all(unlist(rank$x$layout$xaxis$categoryarray)%in%c('2','2.2')))
 cat('PASS round filtering reuses reads and cumulative totals\n')
})

# Older databases can have a full baseline before the date/amount projection
# exists. The application must recover even when the normal worker read fails.
fm_worker_configured <- function() TRUE
league_context_metadata <- function(championship_id, account_id, allow_stale = FALSE) {
  if (identical(championship_id, 'league'))
    list(competition_id='competition', season='season', scoring_version='score')
}
clear_api_cache()
calls <- character()
read_args <- list()
async_adapter_fetch <- function(fn, args=list(), context, ...) {
  calls <<- c(calls,fn)
  read_args[[fn]] <<- args
  if (fn == 'get_championship_transfer_amounts') return(promises::promise_resolve(list()))
  if (fn == 'get_championship_pressroom') return(promises::promise_reject(simpleError('Synthetic worker failure')))
  if (fn == 'read_persisted_history_baseline')
    return(promises::promise_resolve(observed_data(data.frame(
      created=c('2026-11-14T12:00:00Z','2026-11-15T12:00:00Z'),price=c(125,375)))))
  promises::promise_resolve(data.frame())
}
shiny::testServer(plots_Server,args=list(
 is_module_active=reactive(TRUE),login_token=reactive(auth),championship_id=reactive('league'),
 user_team_id=reactive('a'),user_teams_RV=reactive(data.frame(teamid='a',teamname='Alpha'))),{
 session$setInputs(plot_tab='Money movements',transfer_period='week')
 settle(session)
 state <- transfer_chart_history()
 scope <- read_args[['read_persisted_history_baseline']]$context
 stopifnot(is.null(state$error), !state$loading, identical(attr(state$data,'fetch_status'),'stale'),
   identical(scope$account_id,auth[['userid']]), identical(scope$championship_id,'league'),
   identical(scope$competition_id,'competition'), identical(scope$season,'season'),
   identical(scope$scoring_version,'score'), identical(plots_transfer_periods(transfer_days(),'week')$amount,500))
 before <- calls
 session$setInputs(transfer_period='day')
 settle(session)
 stopifnot(identical(calls,before), identical(plots_transfer_periods(transfer_days(),'day')$amount,c(125,375)))
 cat('PASS failed worker history recovers through one exact-scope audited baseline read\n')
})
