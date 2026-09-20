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
  columns <- c(required[required != "is_mvp" & required != "is_finished"],intersect("round_id",names(out)))
  out <- out[as_flag(out$is_mvp) & as_flag(out$is_finished), columns, drop = FALSE]
  if (!nrow(out)) return(empty)
  out$round_number <- suppressWarnings(as.numeric(out$round_number))
  out$points <- suppressWarnings(as.numeric(out$points))
  # Preserve valid official selections even when the catalog omits display numbers.
  if ("round_id" %in% names(rows)) {
    out <- out[!duplicated(out$round_id),,drop=FALSE]
  } else out <- out[is.finite(out$round_number) & !duplicated(out$round_number),,drop=FALSE]
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
    mvp_snapshot <- shared_mvp_source(session,login_token,championship_id,is_module_active,refresh_trigger)
    output$mvp_cards <- renderUI({
      snapshot <- mvp_snapshot()
      rows <- round_mvp_rows(snapshot$rows, championship_id())
      notice <- if(nzchar(snapshot$reason)) tags$p(class="round-mvps-empty",snapshot$reason)
      if (!nrow(rows)) {
        return(notice %||% tags$p("MVP data is unavailable; the reason could not be determined."))
      }
      tagList(notice,tags$div(class = "round-mvps-grid", lapply(seq_len(nrow(rows)), function(i) {
        row <- rows[i, , drop = FALSE]
        tags$article(class = "round-mvp-card",
          tags$div(class = "round-mvp-round", if(is.finite(row$round_number)) paste("Round", format(row$round_number, trim = TRUE, scientific = FALSE)) else "Round number unavailable"),
          tags$div(class = "round-mvp-player", row$player_name),
          tags$div(class = "round-mvp-meta", paste(na.omit(c(row$player_role, if (is.finite(row$points)) paste0(format(row$points, trim = TRUE), " points"))), collapse = " · ")))
      })))
    })
  })
}
