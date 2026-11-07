#!/usr/bin/env Rscript
# Exercise the real button observers offline, including browser input updates.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source("global.R"))
bind_factual_reads <- function(...) invisible(NULL)
shared_mvp_source <- function(...) reactive(list())
shared_rival_finance_source <- function(...) reactive(list())
calls <- character()
available <- reactiveVal(list(
  catalog = data.frame(round_id = c("r3", "r1"), round_number = c(3, 1)),
  history = list(list(round = 1, points = 5), list(round = 4, points = 12),
                 list(round = 3, points = 8))))
get_classification_round_catalog <- function(...) {
  calls <<- c(calls, "catalog")
  list(rounds=available()$catalog)
}
get_user_team_rounds <- function(...) {
  calls <<- c(calls, "history")
  available()$history
}
get_user_team_info <- function(...) list(configuration = list())
get_financial_snapshot <- function(...) list(configuration = list())
get_round_dreamteam <- function(...) {
  calls <<- c(calls, "dreamteam")
  list()
}
html <- htmltools::renderTags(classification_UI("classification"))$html
stopifnot(grepl('id="classification-latest_round"', html, fixed = TRUE),
          grepl('id="classification-general_classification"', html, fixed = TRUE))
shiny::testServer(classification_Server, args = list(
  is_module_active = reactive(TRUE), login_token = reactive("fixture"),
  championship_id = reactive("league"), user_team_id = reactive("a"),
  user_teams_RV = reactive(data.frame(teamid = "a", teamname = "Alpha", points = 25))), {
  messages <- list()
  root <- session$rootScope()
  root$sendInputMessage <- function(inputId, message) {
    messages[[sub("^.*-", "", inputId)]] <<- message
  }
  session$setInputs(round_range_slider = c(1, 4))
  before <- calls
  # The latest history round can arrive before the catalog includes it.
  session$setInputs(latest_round = 1)
  stopifnot(identical(as.numeric(messages$round_range_slider$value), c(4, 4)),
            identical(calls, before))
  session$setInputs(round_range_slider = c(4, 4))
  stopifnot(filtered_rows_RV()$points == 12, filtered_rows_RV()$rounds == 1)
  # General classification must restore the full range after a one-round selection.
  session$setInputs(round_range_slider = c(1, 1))
  before <- calls
  session$setInputs(general_classification = 1)
  stopifnot(identical(as.numeric(messages$round_range_slider$value), c(1, 4)),
            identical(calls, before))
  session$setInputs(round_range_slider = c(1, 4))
  stopifnot(filtered_rows_RV()$points == 25, filtered_rows_RV()$rounds == 3)
  session$setInputs(round_range_slider = c(1, 3))
  stopifnot(filtered_rows_RV()$points == 13)
  # With only one available round, both presets select that round.
  available(list(catalog = data.frame(round_id = "r3", round_number = 3),
                 history = list(list(round = 3, points = 8))))
  session$flushReact()
  session$setInputs(latest_round = 2)
  stopifnot(identical(as.numeric(messages$round_range_slider$value), c(3, 3)))
  session$setInputs(general_classification = 2)
  stopifnot(identical(as.numeric(messages$round_range_slider$value), c(3, 3)))
  # Empty history cannot generate an invalid slider range.
  available(list(catalog = data.frame(), history = list()))
  session$flushReact()
  messages <- list()
  session$setInputs(latest_round = 3, general_classification = 3)
  stopifnot(length(messages) == 0L)
})
cat("PASS classification presets: latest/general ranges, single-range reset, manual filters, edge cases, no extra reads\n")
