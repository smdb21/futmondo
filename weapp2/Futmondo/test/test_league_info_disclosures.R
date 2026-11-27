#!/usr/bin/env Rscript
# Offline UI contract: diagnostics survive collapsed, with no extra source reads.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
details <- function(tag) xml2::xml_find_all(xml2::read_html(as.character(tag)), '//details[contains(@class,"fm-info")]')
loading <- xml2::read_html(as.character(rivals_status_message('Retrieving saved balances.', loading=TRUE)))
failed <- details(rivals_status_message('Source read failed.', error=TRUE))
stopifnot(length(xml2::xml_find_all(loading,'//details'))==0L, length(failed)==1L,
  xml2::xml_text(xml2::xml_find_first(loading,'//*[@role="status"]'))=='Retrieving saved balances.',
  is.na(xml2::xml_attr(failed,'open')),
  grepl('fm-info-error',xml2::xml_attr(failed,'class'),fixed=TRUE),
  trimws(xml2::xml_text(xml2::xml_find_first(failed,'./summary')))=='i',
  grepl('Source read failed.',xml2::xml_text(xml2::xml_find_first(failed,'./div')),fixed=TRUE))
cat('PASS rival loading is visible inline and settled errors retain their disclosure\n')
updates <- rivals_update_details(data.frame(teamname=c('Alpha','Beta'),
  as_of=c('2026-11-14T12:00:00Z',NA_character_)), 'UTC')
status <- details(rivals_status_message('2 league teams loaded.', details=updates))
stopifnot(grepl('Alpha:14/11/202612:00UTC',gsub('[[:space:]]+','',xml2::xml_text(status)),fixed=TRUE),
  grepl('Beta:Updatetimeunavailable',gsub('[[:space:]]+','',xml2::xml_text(status)),fixed=TRUE))
cat('PASS per-team update timestamps share the full-width collapsed status\n')


p <- plots_empty('Stored history request failed.')
stopifnot(length(details(attr(p,'fm_info')))==1L,
  is.null(p$prepend), is.null(p$append),
  !grepl('Stored history request failed.', plotly::plotly_json(p, jsonedit=FALSE),fixed=TRUE))
cat('PASS empty chart diagnostics are retained outside the plot payload\n')

calls <- new.env(parent=emptyenv())
record <- function(name, result) { calls[[name]] <- (calls[[name]] %||% 0L)+1L; result }
bind_factual_reads <- function(...) invisible(NULL)
get_league_standings_history <- function(...) record('history',data.frame())
get_financial_snapshot <- function(...) record('finance',list(configuration=list(budget=100)))
get_championship_pressroom <- function(...) record('transfers',data.frame())
get_classification_round_catalog <- function(...) list(rounds=data.frame())
get_user_team_rounds <- function(...) record('rounds',list())
get_championship_players <- function(...) record('players',data.frame())
shiny::testServer(plots_Server, args=list(is_module_active=function()TRUE,
  login_token=function()c(userid='fixture',token='synthetic'), championship_id=function()'league',
  user_team_id=function()'team', user_teams_RV=function()data.frame(teamid='team',teamname='Team')), {
  session$flushReact()
  tabs <- c("Points"="standings", "Squad values"="values", "Cash & transfers"="cash",
    "Owned by real team"="owned_club")
  for (tab in names(tabs)) {
    session$setInputs(plot_tab=tab)
    stopifnot(length(details(output$chart_status$html))==1L,
      !is.null(output[[paste0(tabs[[tab]],'_plot')]]))
  }
  before <- as.list(calls)
  session$setInputs(plot_tab="Points", show_ranks=TRUE)
  stopifnot(length(details(output$chart_status$html))==1L, !is.null(output$standings_plot))
  for(i in seq_len(5L)) {
    session$flushReact()
    invisible(output$chart_status); invisible(output$standings_plot)
  }
  # Squad values and Cash each enter the tab-scoped valuation reader once.
  stopifnot(identical(before,as.list(calls)), calls$history==2L,
    calls$finance==1L,calls$transfers==1L,calls$rounds==1L,calls$players==1L)
})
cat('PASS paired plot/status consumers share one result and add zero repeated reads\n')
