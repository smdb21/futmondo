library(shiny)
library(dplyr)

round_mvp_rows <- function(rows, championship_id = NULL) {
  required <- c("round_number", "player_id", "player_name", "player_role", "points", "is_mvp", "is_finished")
  empty <- data.frame(round_number = numeric(), player_id = character(), player_name = character(),
                      player_role = character(), points = numeric(), stringsAsFactors = FALSE)
  if (is.null(rows) || !is.data.frame(rows) || !nrow(rows) || !all(required %in% names(rows))) return(empty)
  out <- rows
  if (!is.null(championship_id) && "championship_id" %in% names(out)) {
    out <- out[as.character(out$championship_id) %in% as.character(championship_id)[1], , drop = FALSE]
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

# Select by immutable round identity: display numbers may be missing or repeated.
round_best_xi_rows <- function(rows, championship_id, round) {
  required <- c("round_number", "player_id", "player_name", "player_role", "points", "is_mvp", "is_finished")
  if (!is.data.frame(rows) || !all(required %in% names(rows))) return(data.frame())
  if ("championship_id" %in% names(rows)) {
    rows <- rows[as.character(rows$championship_id) %in% as.character(championship_id)[1], , drop = FALSE]
  }
  if ("round_id" %in% names(rows) && "round_id" %in% names(round)) {
    rows <- rows[rows$round_id %in% round$round_id, , drop = FALSE]
  } else {
    rows <- rows[rows$round_number %in% round$round_number, , drop = FALSE]
  }
  as_flag <- function(x) tolower(trimws(as.character(x))) %in% c("true", "t", "1")
  rows <- rows[as_flag(rows$is_finished), , drop = FALSE]
  rows$is_mvp <- as_flag(rows$is_mvp)
  if (nrow(rows) != 11L || anyNA(rows$player_id) || any(!nzchar(rows$player_id)) ||
      anyDuplicated(rows$player_id) || sum(rows$is_mvp) != 1L) return(data.frame())
  rows$points <- suppressWarnings(as.numeric(rows$points))
  positions <- analytics_player_positions(data.frame(role = rows$player_role))
  rows$display_role <- vapply(seq_len(nrow(rows)), function(i) {
    if (length(positions[[i]])) positions[[i]][1] else mvp_text(rows$player_role[i], "Unavailable")
  }, character(1))
  rows$display_role[!nzchar(rows$display_role)] <- "Unavailable"
  rows[order(match(rows$display_role, c("GK", "DEF", "MID", "FWD")), na.last = TRUE), , drop = FALSE]
}

round_award_card <- function(mvp, xi) {
  tags$article(class = "round-mvp-card",
    tags$div(class = "round-mvp-round", if (is.finite(mvp$round_number))
      paste("Round", format(mvp$round_number, trim = TRUE, scientific = FALSE)) else "Round number unavailable"),
    tags$div(class = "round-mvp-player", tags$span(class = "round-mvp-badge", "MVP"), " ", mvp$player_name),
    tags$div(class = "round-mvp-meta", if (is.finite(mvp$points))
      paste(format(mvp$points, trim = TRUE), "points") else "Points unavailable"),
    tags$h4(class = "round-best-xi-title", "Best XI"),
    if (!nrow(xi)) tags$p(class = "round-mvps-empty", "The official Best XI selection is incomplete.") else
      tags$table(class = "round-best-xi-table", `aria-label` = "Official Best XI",
        tags$thead(tags$tr(tags$th(scope = "col", "Role"), tags$th(scope = "col", "Player"), tags$th(scope = "col", "Points"))),
        tags$tbody(lapply(seq_len(nrow(xi)), function(i) {
          tags$tr(class = if (xi$is_mvp[i]) "round-best-xi-mvp",
            tags$td(xi$display_role[i]),
            tags$td(xi$player_name[i], if (xi$is_mvp[i]) tagList(" ", tags$span(class = "round-mvp-badge", "MVP"))),
            tags$td(if (is.finite(xi$points[i])) format(xi$points[i], trim = TRUE) else "Unavailable"))
        }))))
}

round_mvps_UI <- function(id) {
  ns <- NS(id)
  fluidRow(box(width = 12, title = "Round awards", status = "primary", solidHeader = TRUE,
    p(class = "round-mvps-intro", "Official MVPs and Best XI selections from completed rounds."),
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
        return(notice %||% tags$p("Round awards are unavailable; the reason could not be determined."))
      }
      tagList(notice,tags$div(class = "round-mvps-grid", lapply(seq_len(nrow(rows)), function(i) {
        row <- rows[i, , drop = FALSE]
        round_award_card(row, round_best_xi_rows(snapshot$rows, championship_id(), row))
      })))
    })
  })
}
