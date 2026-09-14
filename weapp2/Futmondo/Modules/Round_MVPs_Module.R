library(shiny)
library(dplyr)

round_mvp_rows <- function(rows, championship_id = NULL) {
  required <- c("round_number", "player_id", "player_name", "player_role", "points", "is_mvp", "is_finished")
  empty <- data.frame(round_number = numeric(), player_id = character(), player_name = character(),
                      player_role = character(), points = numeric(), stringsAsFactors = FALSE)
  if (is.null(rows) || !is.data.frame(rows) || !nrow(rows) || !all(required %in% names(rows))) return(empty)
  out <- rows
  if (!is.null(championship_id) && "championship_id" %in% names(out)) {
    out <- out[as.character(out$championship_id) == as.character(championship_id)[1], , drop = FALSE]
  }
  as_flag <- function(x) tolower(trimws(as.character(x))) %in% c("true", "t", "1")
  out <- out[as_flag(out$is_mvp) & as_flag(out$is_finished), required[required != "is_mvp" & required != "is_finished"], drop = FALSE]
  if (!nrow(out)) return(empty)
  out$round_number <- suppressWarnings(as.numeric(out$round_number))
  out$points <- suppressWarnings(as.numeric(out$points))
  out <- out[is.finite(out$round_number), , drop = FALSE]
  if (!nrow(out)) return(empty)
  out <- out[!duplicated(out$round_number), , drop = FALSE]
  out[order(out$round_number, decreasing = TRUE), , drop = FALSE]
}

round_mvps_UI <- function(id) {
  ns <- NS(id)
  fluidRow(box(width = 12, title = "Round MVPs", status = "primary", solidHeader = TRUE,
    p(class = "round-mvps-intro", "Official MVPs from completed rounds."),
    uiOutput(ns("mvp_cards"))))
}

round_mvps_Server <- function(id, is_module_active, login_token, championship_id, refresh_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    mvp_snapshot <- reactive({
      req(is_module_active() == TRUE, login_token(), championship_id())
      if (!is.null(refresh_trigger)) refresh_trigger()
      tryCatch(list(status = "ok", rows = get_round_mvps(championship_id())),
        error = function(e) list(status = "unavailable", rows = NULL))
    })
    completed_rounds <- reactive({
      req(is_module_active() == TRUE, login_token(), championship_id())
      tryCatch(get_finished_rounds(login_token(), championship_id()), error = function(e) NULL)
    })
    output$mvp_cards <- renderUI({
      snapshot <- mvp_snapshot()
      if (!identical(snapshot$status, "ok")) {
        return(tags$p(class = "round-mvps-empty", "MVP data is unavailable. Refresh and try again."))
      }
      rows <- round_mvp_rows(snapshot$rows, championship_id())
      if (!nrow(rows)) {
        rounds <- completed_rounds()
        has_completed_round <- is.data.frame(rounds) && nrow(rounds) &&
          "is_finished" %in% names(rounds) && any(rounds$is_finished %in% TRUE)
        message <- if (isTRUE(has_completed_round)) {
          "No MVP records are saved for completed rounds. In Admin, use Sync Round Dream Teams to import them."
        } else {
          "No completed rounds are available yet. MVPs are published after a round closes."
        }
        return(tags$p(class = "round-mvps-empty", message))
      }
      tags$div(class = "round-mvps-grid", lapply(seq_len(nrow(rows)), function(i) {
        row <- rows[i, , drop = FALSE]
        tags$article(class = "round-mvp-card",
          tags$div(class = "round-mvp-round", paste("Round", format(row$round_number, trim = TRUE, scientific = FALSE))),
          tags$div(class = "round-mvp-player", row$player_name),
          tags$div(class = "round-mvp-meta", paste(na.omit(c(row$player_role, if (is.finite(row$points)) paste0(format(row$points, trim = TRUE), " points"))), collapse = " · ")))
      }))
    })
  })
}
