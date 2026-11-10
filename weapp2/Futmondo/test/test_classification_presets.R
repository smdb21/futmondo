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
  history = list(
    list(round = 1, points = 5), list(round = 4, points = 12),
    list(round = 3, points = 8)
  )
))
get_classification_round_catalog <- function(...) {
  calls <<- c(calls, "catalog")
  list(rounds = available()$catalog)
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
stopifnot(
  grepl("General", html, fixed = TRUE), grepl("classification-round_buttons", html, fixed = TRUE),
  !grepl("classification-latest_round|classification-general_classification", html)
)
shiny::testServer(classification_Server, args = list(
  is_module_active = reactive(TRUE), login_token = reactive("fixture"), championship_id = reactive("league"),
  user_team_id = reactive("a"), user_teams_RV = reactive(data.frame(teamid = "a", teamname = "Alpha", points = 25))
), {
  messages <- list()
  root <- session$rootScope()
  root$sendInputMessage <- function(inputId, message) messages[[sub("^.*-", "", inputId)]] <<- message
  session$setInputs(classification_view = "rounds", round_range_slider = c(1, 4))
  click <- function(n, context = round_buttons_context_RV()) session$setInputs(round_button = list(number = n, context = context, nonce = runif(1)))
  stopifnot(selected_round_RV() == 4, single_round_RV() == "4", filtered_rows_RV()$points == 12)
  buttons <- output$round_buttons$html
  stopifnot(
    grepl('data-round="4"', buttons, fixed = TRUE),
    regexpr('data-round="4"', buttons, fixed = TRUE) < regexpr('data-round="3"', buttons, fixed = TRUE),
    regexpr('data-round="3"', buttons, fixed = TRUE) < regexpr('data-round="1"', buttons, fixed = TRUE)
  )
  before <- calls[calls %in% c("catalog", "history")]
  click(1)
  stopifnot(selected_round_RV() == 1, filtered_rows_RV()$points == 5)
  click(99)
  click(3, "different-league")
  stopifnot(selected_round_RV() == 1)
  session$setInputs(classification_view = "partial", round_range_slider = c(1, 3))
  stopifnot(single_round_RV() == "all", filtered_rows_RV()$points == 13, !general_selection_RV())
  session$setInputs(round_range_slider = c(1, 4))
  stopifnot(general_selection_RV(), standings_RV()$points == 25)
  session$setInputs(round_range_slider = c(3, 3))
  stopifnot(single_round_RV() == "all", standings_RV()$points == 8)
  session$setInputs(classification_view = "rounds")
  stopifnot(selected_round_RV() == 1, standings_RV()$points == 5)
  stopifnot(identical(calls[calls %in% c("catalog", "history")], before))
  click(4)
  available(list(
    catalog = data.frame(round_id = c("r1", "r3", "r4", "r5"), round_number = c(1, 3, 4, 5)),
    history = list(list(round = 1, points = 5), list(round = 3, points = 8), list(round = 4, points = 12), list(round = 5, points = 6))
  ))
  session$flushReact()
  stopifnot(selected_round_RV() == 5, standings_RV()$points == 6)
  click(1)
  available(list(
    catalog = data.frame(round_id = c("r1", "r6"), round_number = c(1, 6)),
    history = list(list(round = 1, points = 5), list(round = 6, points = 9))
  ))
  session$flushReact()
  stopifnot(selected_round_RV() == 1, standings_RV()$points == 5)
  available(list(catalog = data.frame(round_id = "r3", round_number = 3), history = list(list(round = 3, points = 8))))
  session$flushReact()
  stopifnot(selected_round_RV() == 3, standings_RV()$points == 8)
  available(list(catalog = data.frame(), history = list()))
  session$flushReact()
  stopifnot(is.null(selected_round_RV()), single_round_RV() == "all")
})
cat("PASS round navigation: newest-first buttons, latest following, older selection, partial range independence, one-round partial, stale/invalid clicks, no extra source reads\n")
