source("round_lineup_runtime.R")
library(reactable)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(plotly)

classification_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(
      width = 12, title = "Matchday results", status = "primary", solidHeader = TRUE,
      tabsetPanel(
        id = ns("classification_view"), selected = "rounds",
        tabPanel("Round classifications",
          value = "rounds",
          div(
            class = "fm-classification-rounds", role = "group", `aria-label` = "Select a played round",
            uiOutput(ns("round_buttons"), inline = TRUE)
          )
        ),
        tabPanel("General",
          value = "partial",
          p("Change slider to see partial classifications."),
          div(
            class = "fm-classification-range",
            sliderInput(ns("round_range_slider"), "Points between rounds", min = 1, max = 38, value = c(1, 38))
          )
        )
      )
    )),
    fluidRow(box(width = 12, title = "Standings", uiOutput(ns("classification_status")), reactableOutput(ns("classification_table")))),
    conditionalPanel(
      condition = "input.classification_view !== 'partial'", ns = ns,
      fluidRow(box(
        width = 12, title = "Dream team and configured rewards",
        uiOutput(ns("configured_rewards_ui")), uiOutput(ns("dreamteam_box_ui"))
      ))
    )
  )
}

# Normalize observed per-team round records. Missing points stay NA; opaque
# round IDs are resolved using the championship round catalog, never guessed.
classification_round_rows <- function(answer, team_id, team_name, rounds = data.frame()) {
  empty <- data.frame(team_id = character(), team_name = character(), round = numeric(), points = numeric())
  if (is.null(answer) || !length(answer) || isTRUE(answer$error)) {
    return(empty)
  }
  records <- if (!is.null(answer$rounds)) {
    answer$rounds
  } else {
    answer
  }
  if (is.data.frame(records)) {
    records <- split(records, seq_len(nrow(records)))
  }
  if (!is.list(records)) {
    return(empty)
  }
  pick <- function(x, keys) {
    for (key in keys) {
      if (!is.null(x[[key]]) && length(x[[key]]) == 1L && !is.list(x[[key]])) {
        return(x[[key]])
      }
    }
    NA
  }
  result <- bind_rows(lapply(records, function(record) {
    if (!is.list(record)) {
      return(NULL)
    }
    raw_round <- pick(record, c("round_number", "roundNumber", "number", "round", "id", "_id"))
    n <- suppressWarnings(as.numeric(raw_round))
    if (!is.finite(n) && nrow(rounds) && all(c("round_id", "round_number") %in% names(rounds))) {
      n <- rounds$round_number[match(as.character(raw_round), as.character(rounds$round_id))]
    }
    if (length(n) != 1L || !is.finite(n)) {
      return(NULL)
    }
    points <- suppressWarnings(as.numeric(pick(record, c("points", "score"))))
    data.frame(team_id = as.character(team_id), team_name = as.character(team_name), round = n, points = points)
  }))
  if (!nrow(result)) {
    return(empty)
  }
  result[!duplicated(paste(result$team_id, result$round), fromLast = TRUE), , drop = FALSE]
}

# A pending bound read is a Shiny validation condition. Capture it as data so
# one optional source cannot cancel every output or the round-filter observer.
classification_read_state <- function(read) {
  tryCatch(list(data = read(), loading = FALSE, error = NULL), error = function(error) {
    pending <- inherits(error, "shiny.silent.error") &&
      (!nzchar(conditionMessage(error)) || grepl("Loading|Refreshing", conditionMessage(error)))
    list(data = NULL, loading = pending, error = if (pending) {
      NULL
    } else {
      error
    })
  })
}
classification_empty_rows <- function() data.frame(team_id = character(), team_name = character(), round = numeric(), points = numeric())
classification_empty_teams <- function() data.frame(teamid = character(), teamname = character())

# Scheduled catalog rows are identities, not evidence of published scores.
classification_available_rounds <- function(catalog, history) {
  rounds <- catalog$rounds
  current <- fm_number(catalog$current_round_number)
  observed <- history$round
  if (is.finite(current)) {
    observed <- observed[observed <= current]
  }
  closed <- if (is.data.frame(rounds) && all(c("round_number", "is_finished") %in% names(rounds))) {
    rounds$round_number[rounds$is_finished %in% TRUE]
  } else {
    numeric()
  }
  playing <- if (is.data.frame(rounds) && all(c("round_number", "status") %in% names(rounds))) {
    rounds$round_number[tolower(rounds$status) %in% c("active", "playing", "in_progress", "running")]
  } else {
    numeric()
  }
  numbers <- sort(unique(c(observed, closed, playing, if (isTRUE(catalog$current_verified) && is.finite(current)) {
    current
  })))
  # Numbered preceding rounds are required coverage even when their completion
  # flags are missing. This is a filter domain, never financial finality.
  if (length(numbers) && is.data.frame(rounds) && "round_number" %in% names(rounds)) {
    numbers <- sort(unique(c(numbers, rounds$round_number[is.finite(rounds$round_number) & rounds$round_number <= max(numbers)])))
  }
  numbers[is.finite(numbers) & numbers >= 1 & numbers == floor(numbers)]
}

classification_merge_ranking <- function(history, ranking, teams) {
  if (is.null(ranking) || !is.data.frame(ranking$rows) || !nrow(ranking$rows)) {
    return(history)
  }
  observed <- ranking$rows
  # Keep absent source scores unknown even when the directory has cumulative
  # points. Replacing by team/round also prevents counting live points twice.
  directory <- data.frame(team_id = as.character(teams$teamid), team_name = as.character(teams$teamname))
  names <- directory$team_name[match(observed$team_id, directory$team_id)]
  observed$team_name[!is.na(names)] <- names[!is.na(names)]
  absent <- !directory$team_id %in% observed$team_id
  if (any(absent)) {
    observed <- bind_rows(observed, data.frame(
      team_id = directory$team_id[absent],
      team_name = directory$team_name[absent], round_id = ranking$round_id, round = ranking$round_number,
      points = NA_real_, score_status = "unavailable"
    ))
  }
  keys <- paste(observed$team_id, observed$round)
  bind_rows(history[!paste(history$team_id, history$round) %in% keys, , drop = FALSE], observed)
}

classification_window_table <- function(rows, window = NULL, single = "all", expected_rounds = NULL) {
  if (is.null(rows) || !nrow(rows)) {
    return(data.frame())
  }
  teams <- unique(rows[, c("team_id", "team_name"), drop = FALSE])
  expected <- expected_rounds %||% unique(rows$round)
  if (!is.null(single) && length(single) == 1L && single != "all") {
    rows <- rows[rows$round == suppressWarnings(as.numeric(single)), , drop = FALSE]
    expected <- suppressWarnings(as.numeric(single))
  } else {
    if (length(window) == 2L && all(is.finite(window))) {
      rows <- rows[rows$round >= window[1] & rows$round <= window[2], , drop = FALSE]
      expected <- expected[expected >= window[1] & expected <= window[2]]
    }
  }
  if (!length(expected)) {
    return(data.frame())
  }
  # Compare the same published rounds for every team. An absent record is
  # missing evidence, not a zero-point round or permission to sum fewer rounds.
  expected <- length(unique(expected))
  totals <- rows %>%
    group_by(team_id, team_name) %>%
    summarise(
      points = if (all(is.finite(points)) && n_distinct(round) == expected) {
        sum(points)
      } else {
        NA_real_
      },
      rounds = n_distinct(round), .groups = "drop"
    )
  teams %>%
    left_join(totals, by = c("team_id", "team_name")) %>%
    mutate(
      rounds = ifelse(is.na(rounds), 0L, rounds),
      rank = rank(-points, ties.method = "min", na.last = "keep")
    ) %>%
    arrange(rank, team_name)
}

# Remaining players apply only to one unfinished round, never to a range.
classification_remaining_table <- function(standings, history, single, rounds) {
  if (length(single) != 1L || single == "all") {
    return(standings)
  }
  selected <- history[history$round == as.numeric(single), , drop = FALSE]
  statuses <- selected$score_status
  catalog <- rounds[rounds$round_number %in% as.numeric(single), , drop = FALSE]
  finished <- any(statuses %in% "final") || any(catalog$is_finished %in% TRUE)
  unfinished <- any(statuses %in% "provisional") || any(catalog$is_finished %in% FALSE) ||
    any(tolower(catalog$status) %in% c("active", "playing", "in_progress", "running"))
  if (finished || !unfinished) {
    return(standings)
  }
  standings$players_left <- if ("players_left" %in% names(selected)) {
    selected$players_left[match(standings$team_id, selected$team_id)]
  } else {
    rep(NA_real_, nrow(standings))
  }
  standings
}

# Keep points numeric for sorting; add the remaining count only when rendering.
classification_points_cell <- function(points, left = NULL) {
  if (!is.finite(points)) {
    return("-")
  }
  label <- as.character(points)
  if (is.null(left)) {
    return(label)
  }
  if (is.finite(left)) {
    return(paste0(label, " (", left, ")"))
  }
  tags$span(
    title = "Players left unavailable: the round ranking source has not supplied a valid count.",
    `aria-label` = paste(label, "points; players left unavailable"), paste0(label, " (?)")
  )
}

#' Project selected completed-round bonuses from the shared ledger
#'
#' Sum coverage-aware ledger amounts without I/O or a second cash calculation.
#' Explicitly ongoing rounds are pending and do not erase completed-round totals.
#' Missing completed-round evidence stays unknown, including missing ownership.
#' @param standings Data frame with team_id, retaining its existing columns.
#' @param history Observed team-round scores, optionally including score_status.
#' @param finances Shared finance result containing cash_ledgers with numeric
#'   round_bonus_totals attributes; amounts are in euros.
#' @param window Inclusive round-number endpoints, or NULL for all rounds.
#' @param single One round number or "all" (the default) for the selected window.
#' @param expected_rounds Published round numbers required for complete coverage.
#'   Defaults to the observed history's rounds.
#' @param rounds Optional catalog with round_number and status.
#'   Explicit playing status identifies pending rounds even if ranking reads fail.
#' @return Standings with four numeric bonus columns and bonus_pending_rounds
#'   and bonus_completed_rounds attributes. No completed rounds yields NA, not zero.
#' @keywords internal
classification_bonus_table <- function(standings, history, finances, window = NULL,
                                       single = "all", expected_rounds = NULL,
                                       rounds = data.frame()) {
  kinds <- c(points_bonus = "points", mvp_bonus = "mvp", best_xi_bonus = "dreamteam", position_bonus = "ranking")
  selected <- sort(unique(expected_rounds %||% history$round))
  if (length(single) == 1L && single != "all") {
    selected <- selected[selected == as.numeric(single)]
  } else {
    if (length(window) == 2L && all(is.finite(window))) {
      selected <- selected[selected >= window[1] & selected <= window[2]]
    }
  }
  pending <- finances$pending_bonus_rounds %||% numeric()
  if ("score_status" %in% names(history)) {
    pending <- c(pending, history$round[history$score_status %in% "provisional"])
  }
  if (all(c("round_number", "status") %in% names(rounds))) {
    playing <- tolower(rounds$status) %in% c("active", "playing", "in_progress", "running")
    # Calendar status is overlaid from current match metadata; an older
    # compact catalog's is_finished flag must not override explicit playing.
    pending <- c(pending, rounds$round_number[playing])
  }
  pending <- selected[selected %in% pending]
  selected <- setdiff(selected, pending)
  for (column in names(kinds)) {
    standings[[column]] <- vapply(as.character(standings$team_id), function(id) {
      totals <- attr(finances$cash_ledgers[[id]], "round_bonus_totals")
      if (!length(selected) || !is.data.frame(totals) || !nrow(totals)) {
        return(NA_real_)
      }
      rows <- totals[totals$bonus_type %in% kinds[[column]] & totals$round_number %in% selected, , drop = FALSE]
      if (nrow(rows) != length(selected) || !setequal(rows$round_number, selected) || any(!is.finite(rows$amount))) {
        return(NA_real_)
      }
      sum(rows$amount)
    }, numeric(1))
  }
  attr(standings, "bonus_pending_rounds") <- pending
  attr(standings, "bonus_completed_rounds") <- selected
  standings
}

#' Explain missing completed-round coverage using the same saved ledger
#' @param finances Shared finance result with per-team cash ledgers.
#' @param rounds Completed round numbers in the displayed selection.
#' @param team_ids Displayed team identities.
#' @return Named list of missing round numbers per bonus component.
#' @keywords internal
classification_bonus_gaps <- function(finances, rounds, team_ids) {
  kinds <- c("points", "mvp", "dreamteam", "ranking")
  result <- setNames(rep(list(numeric()), length(kinds)), kinds)
  for (id in intersect(as.character(team_ids), names(finances$cash_ledgers))) {
    totals <- attr(finances$cash_ledgers[[id]], "round_bonus_totals")
    # A ledger without round metadata cannot identify component-specific gaps.
    # Keep that absence separate from the observed participants' coverage.
    if (!is.data.frame(totals) || !nrow(totals)) next
    for (kind in kinds) {
      rows <- totals[totals$bonus_type %in% kind, , drop = FALSE]
      known <- rows$round_number[is.finite(rows$amount) &
        !duplicated(rows$round_number) & !duplicated(rows$round_number, fromLast = TRUE)]
      result[[kind]] <- sort(unique(c(result[[kind]], setdiff(rounds, known))))
    }
  }
  result
}

classification_dreamteam_rows <- function(answer) {
  if (is.null(answer) || isTRUE(answer$error) || !length(answer$players)) {
    return(data.frame())
  }
  players <- answer$players
  if (is.data.frame(players)) {
    players <- split(players, seq_len(nrow(players)))
  }
  scalar <- function(x, default = NA_character_) {
    if (length(x) == 1L && !is.list(x)) {
      as.character(x)
    } else {
      default
    }
  }
  mvp <- answer$mvp
  if (is.list(mvp)) {
    mvp <- if (!is.null(mvp$id)) {
      mvp$id
    } else {
      mvp[["_id"]]
    }
  }
  bind_rows(lapply(players, function(p) {
    id <- scalar(if (!is.null(p$id)) {
      p$id
    } else {
      p[["_id"]]
    })
    data.frame(
      player_id = id, player = scalar(p$name), role = scalar(p$role),
      points = suppressWarnings(as.numeric(scalar(p$points))),
      mvp = !is.na(id) && id %in% as.character(mvp), stringsAsFactors = FALSE
    )
  }))
}

classification_Server <- function(id, is_module_active, login_token, championship_id, user_team_id,
                                  user_teams_RV, refresh_trigger = NULL, league_settings_source = NULL) {
  moduleServer(id, function(input, output, session) {
    bind_factual_reads(c("get_classification_round_catalog", "get_classification_round_ranking", "get_user_team_rounds", "get_user_team_info", "get_round_dreamteam", "get_round_lineup_snapshot"), session, login_token, championship_id, user_team_id, is_active = is_module_active, refresh_trigger = refresh_trigger)

    mvp_snapshot <- shared_mvp_source(session, login_token, championship_id, is_module_active, refresh_trigger)
    finances_RV <- shared_rival_finance_source(
      session, login_token, championship_id,
      user_team_id, user_teams_RV, is_module_active, mvp_snapshot, refresh_trigger
    )

    catalog_state_RV <- reactive({
      req(is_module_active() == TRUE, login_token(), championship_id())
      if (!is.null(refresh_trigger)) {
        refresh_trigger()
      }
      result <- classification_read_state(function() get_classification_round_catalog(login_token(), championship_id()))
      catalog <- result$data
      if (!is.null(catalog) && (!is.data.frame(catalog$rounds) || !nrow(catalog$rounds))) {
        result$error <- resource_failure(simpleError(mvp_text(catalog$reason, "Round catalog unavailable")),
          resource = "get_classification_round_catalog", category = if (identical(catalog$status, "connection")) {
            "network"
          } else {
            "schema"
          }
        )
      }
      result
    })
    catalog_RV <- reactive(catalog_state_RV()$data %||% empty_round_catalog())
    rounds_RV <- reactive(catalog_RV()$rounds)
    teams_state_RV <- reactive({
      req(is_module_active() == TRUE, login_token(), championship_id())
      classification_read_state(user_teams_RV)
    })
    teams_RV <- reactive({
      teams <- teams_state_RV()$data
      if (is.data.frame(teams) && all(c("teamid", "teamname") %in% names(teams))) {
        teams
      } else {
        classification_empty_teams()
      }
    })
    historical_state_RV <- reactive({
      req(is_module_active() == TRUE, login_token(), championship_id())
      if (!is.null(refresh_trigger)) {
        refresh_trigger()
      }
      teams <- teams_RV()
      rounds <- rounds_RV()
      reads <- lapply(seq_len(nrow(teams)), function(i) {
        result <- classification_read_state(function() get_user_team_rounds(login_token(), championship_id(), as.character(teams$teamid[i])))
        result$data <- classification_round_rows(result$data, teams$teamid[i], teams$teamname[i], rounds)
        result
      })
      list(
        data = bind_rows(c(list(classification_empty_rows()), lapply(reads, `[[`, "data"))),
        loading = isTRUE(teams_state_RV()$loading) || any(vapply(reads, function(x) isTRUE(x$loading), logical(1))),
        errors = Filter(Negate(is.null), c(list(teams_state_RV()$error), lapply(reads, `[[`, "error")))
      )
    })
    historical_rows_RV <- reactive(historical_state_RV()$data)
    finances_state_RV <- reactive(classification_read_state(finances_RV))
    finance_data_RV <- reactive(finances_state_RV()$data %||% list(loading = isTRUE(finances_state_RV()$loading)))
    active_ranking_RV <- reactive({
      catalog <- catalog_RV()
      rounds <- catalog$rounds
      number <- fm_number(catalog$current_round_number)
      if (!is.data.frame(rounds) || !nrow(rounds)) {
        return(NULL)
      }
      # A numbered playing row is authoritative even when an older saved
      # catalog has no lineup marker. Do not infer a current round from order.
      playing <- if ("status" %in% names(rounds)) {
        rounds$round_number[tolower(rounds$status) %in% c("active", "playing", "in_progress", "running")]
      } else {
        numeric()
      }
      playing <- unique(playing[is.finite(playing)])
      if (!is.finite(number) && length(playing) == 1L) {
        number <- playing
      }
      if (!is.finite(number)) {
        return(NULL)
      }
      # The lineup marker may already name next week's round. Only use explicit
      # calendar identities, and check at most that marker plus its predecessor.
      candidates <- rounds[is.finite(rounds$round_number) & rounds$round_number <= number, , drop = FALSE]
      candidates <- head(candidates[order(candidates$round_number, decreasing = TRUE), , drop = FALSE], 2L)
      if ("status" %in% names(candidates)) {
        candidates <- candidates[order(!tolower(candidates$status) %in% c("active", "playing", "in_progress", "running")), , drop = FALSE]
      }
      failure <- NULL
      loading <- FALSE
      for (i in seq_len(nrow(candidates))) {
        result <- tryCatch(get_classification_round_ranking(
          login_token(), championship_id(), user_team_id(),
          candidates$round_id[i], candidates$round_number[i]
        ), error = function(e) {
          if (inherits(e, "shiny.silent.error") && grepl("Loading", conditionMessage(e), fixed = TRUE)) {
            loading <<- TRUE
          } else {
            failure <<- e
          }
          NULL
        })
        if (!is.null(result) && is.data.frame(result$rows) && nrow(result$rows)) {
          return(result)
        }
        # A failed known live round remains unavailable. A completed prior
        # round cannot silently replace it as the latest matchday.
        if ("status" %in% names(candidates) && tolower(candidates$status[i]) %in% c("active", "playing", "in_progress", "running")) {
          break
        }
      }
      list(rows = data.frame(), loading = loading, error = failure)
    })
    round_rows_RV <- reactive({
      catalog <- catalog_RV()
      ranking <- active_ranking_RV()
      history <- historical_rows_RV()
      current <- if (!is.null(ranking) && nrow(ranking$rows)) {
        ranking$round_number
      } else {
        fm_number(catalog$current_round_number)
      }
      if (is.finite(current)) {
        history <- history[history$round <= current, , drop = FALSE]
      }
      classification_merge_ranking(history, ranking, teams_RV())
    })
    round_numbers_RV <- reactive({
      catalog <- catalog_RV()
      ranking <- active_ranking_RV()
      if (!is.null(ranking) && nrow(ranking$rows)) {
        catalog$current_round_number <- ranking$round_number
        catalog$current_verified <- TRUE
      }
      numbers <- classification_available_rounds(catalog, round_rows_RV())
      if (!is.null(ranking) && nrow(ranking$rows)) {
        numbers <- numbers[numbers <= ranking$round_number]
      }
      numbers
    })
    partial_view_RV <- reactive(identical(input$classification_view, "partial"))
    picked_round_RV <- reactiveVal(NULL)
    selected_round_RV <- reactive({
      numbers <- round_numbers_RV()
      if (!length(numbers)) {
        return(NULL)
      }
      picked <- picked_round_RV()
      if (length(picked) == 1L && picked %in% numbers) {
        picked
      } else {
        max(numbers)
      }
    })
    single_round_RV <- reactive({
      selected <- selected_round_RV()
      if (partial_view_RV() || !length(selected)) {
        "all"
      } else {
        as.character(selected)
      }
    })
    round_window_RV <- reactive({
      if (partial_view_RV()) {
        input$round_range_slider
      } else {
        rep(selected_round_RV(), 2L)
      }
    })
    round_buttons_context_RV <- reactive({
      auth <- login_token()
      paste(if (valid_login(auth)) {
        auth[["userid"]]
      } else {
        ""
      }, championship_id(), sep = ":")
    })
    observeEvent(round_buttons_context_RV(), picked_round_RV(NULL), ignoreInit = TRUE)
    previous_domain <- NULL
    observeEvent(round_numbers_RV(), {
      numbers <- round_numbers_RV()
      if (!length(numbers)) {
        return()
      }
      window <- isolate(input$round_range_slider)
      # A full-range selection follows new rounds; a custom partial range stays put.
      if (is.null(previous_domain) || length(window) != 2L || identical(as.numeric(window), as.numeric(range(previous_domain)))) {
        window <- range(numbers)
      } else {
        window <- pmax(min(numbers), pmin(max(numbers), window))
      }
      previous_domain <<- numbers
      updateSliderInput(session, "round_range_slider", min = min(numbers), max = max(c(numbers, min(numbers) + 1)), value = window)
    })
    observeEvent(input$round_button, {
      click <- input$round_button
      if (!is.list(click) || !identical(mvp_text(click$context), round_buttons_context_RV())) {
        return()
      }
      number <- fm_number(click$number)
      numbers <- round_numbers_RV()
      if (!is.finite(number) || !number %in% numbers) {
        return()
      }
      # Selecting the newest round follows the next published/playing round.
      picked_round_RV(if (number == max(numbers)) {
        NULL
      } else {
        number
      })
    })
    output$round_buttons <- renderUI({
      numbers <- sort(round_numbers_RV(), decreasing = TRUE)
      if (!length(numbers)) {
        return(p("Loading played rounds…"))
      }
      tagList(lapply(numbers, function(number) {
        selected <- identical(as.numeric(number), as.numeric(selected_round_RV()))
        tags$button(
          type = "button", class = paste("btn fm-round-button", if (selected) {
            "active"
          }),
          `data-round` = number, `aria-pressed` = if (selected) {
            "true"
          } else {
            "false"
          },
          onclick = paste0(
            "Shiny.setInputValue(", jsonlite::toJSON(session$ns("round_button"), auto_unbox = TRUE),
            ",{number:", number, ",context:", jsonlite::toJSON(round_buttons_context_RV(), auto_unbox = TRUE),
            ",nonce:Date.now()},{priority:'event'});"
          ), paste("Round", number)
        )
      }))
    })
    filtered_rows_RV <- reactive({
      classification_window_table(round_rows_RV(), round_window_RV(), single_round_RV(), round_numbers_RV())
    })
    general_selection_RV <- reactive({
      numbers <- round_numbers_RV()
      window <- round_window_RV()
      (is.null(single_round_RV()) || single_round_RV() == "all") &&
        partial_view_RV() && (!length(numbers) || length(window) != 2L ||
        (window[1] <= min(numbers) && window[2] >= max(numbers)))
    })
    standings_RV <- reactive({
      req(is_module_active() == TRUE)
      rows <- filtered_rows_RV()
      if (general_selection_RV()) {
        teams <- teams_RV()
        # Live-only rows cannot stand in for official cumulative standings.
        if (!nrow(teams) || !"points" %in% names(teams)) {
          return(data.frame())
        }
        # The official cumulative source remains authoritative when historical
        # team-round lists omit whole rounds; never label a partial sum General.
        if ("points" %in% names(teams)) {
          rows <- data.frame(
            team_id = as.character(teams$teamid), team_name = teams$teamname,
            points = suppressWarnings(as.numeric(teams$points)),
            rank = rank(-suppressWarnings(as.numeric(teams$points)), ties.method = "min", na.last = "keep")
          )
        }
      }
      if (!nrow(rows)) {
        return(rows)
      }
      rows <- classification_bonus_table(rows, round_rows_RV(), finance_data_RV(), round_window_RV(), single_round_RV(), round_numbers_RV(), rounds_RV())
      classification_remaining_table(rows, round_rows_RV(), single_round_RV(), rounds_RV())
    })
    empty_standings_message_RV <- reactive({
      teams <- teams_state_RV()
      if (general_selection_RV() && (!nrow(teams_RV()) || !"points" %in% names(teams_RV()))) {
        return(
          if (isTRUE(teams$loading)) {
            "Loading official cumulative standings…"
          } else {
            resource_failure_message(teams$error)
          }
        )
      }
      catalog <- catalog_state_RV()
      ranking <- active_ranking_RV()
      history <- historical_state_RV()
      failures <- Filter(Negate(is.null), c(list(catalog$error, ranking$error), history$errors))
      if (length(failures)) {
        return(resource_failure_message(failures[[1]]))
      }
      if (isTRUE(catalog$loading) || isTRUE(ranking$loading) || isTRUE(history$loading)) {
        return("Loading official round standings…")
      }
      "No published results in this selection."
    })
    output$classification_status <- renderUI({
      req(is_module_active() == TRUE, login_token(), championship_id())
      catalog <- catalog_state_RV()
      ranking <- active_ranking_RV()
      teams <- teams_state_RV()
      history <- historical_state_RV()
      failures <- Filter(Negate(is.null), c(list(catalog$error, ranking$error, finances_state_RV()$error), history$errors))
      rows <- standings_RV()
      if (!nrow(rows)) {
        return(p(empty_standings_message_RV()))
      }
      stale <- identical(attr(active_ranking_RV(), "fetch_status"), "stale")
      finances <- finance_data_RV()
      bonus_loading <- isTRUE(finances$loading) || isTRUE(finances$finance_loading)
      gaps <- classification_bonus_gaps(finances, attr(rows, "bonus_completed_rounds"), rows$team_id)
      labels <- c(points = "Points", mvp = "MVP", dreamteam = "Best XI", ranking = "Position")
      gap_labels <- vapply(names(gaps)[lengths(gaps) > 0L], function(kind) {
        paste0(labels[[kind]], " (rounds ", paste(gaps[[kind]], collapse = ", "), ")")
      }, character(1))
      missing_bonus <- any(vapply(rows[, intersect(c("points_bonus", "mvp_bonus", "best_xi_bonus", "position_bonus"), names(rows)), drop = FALSE], anyNA, logical(1)))
      tagList(
        p(if (single_round_RV() != "all") {
          "Click a team to see its formation and ordered substitutes for this round."
        } else {
          "Partial classification for the selected range of rounds."
        }),
        if (partial_view_RV() && length(attr(rows, "bonus_pending_rounds"))) {
          pending <- paste(attr(rows, "bonus_pending_rounds"), collapse = ", ")
          if (length(attr(rows, "bonus_completed_rounds"))) {
            p(paste0("Bonus totals cover completed rounds only. Unsettled rounds excluded: ", pending, "."))
          } else {
            p(paste0("Bonus totals are pending for unsettled rounds: ", pending, "."))
          }
        },
        if (bonus_loading && missing_bonus) {
          p("Bonus totals are loading; available standings remain visible.")
        } else if (missing_bonus && length(attr(rows, "bonus_completed_rounds"))) {
          p(paste0("Some bonus totals lack complete historical evidence",
            if (length(gap_labels)) paste0(": ", paste(gap_labels, collapse = "; ")) else "", "."))
        },
        if (nzchar(mvp_text(finances$finance_error))) p(finances$finance_error),
        if (nzchar(mvp_text(finances$reason))) p(finances$reason),
        if (length(failures)) {
          p(resource_failure_message(failures[[1]]))
        },
        if (isTRUE(history$loading)) {
          p("Historical round scores are loading; available live scores are shown.")
        },
        if (stale) {
          p("Showing saved round standings while the latest source request is unavailable.")
        }
      )
    })
    lineup_context_RV <- reactive({
      round <- single_round_RV()
      catalog <- rounds_RV()
      index <- match(suppressWarnings(as.numeric(round)), catalog$round_number)
      auth <- login_token()
      paste(
        if (valid_login(auth)) {
          auth[["userid"]]
        } else {
          ""
        }, championship_id(), round,
        if (length(index) == 1L && !is.na(index)) {
          catalog$round_id[index]
        } else {
          ""
        },
        sep = ":"
      )
    })
    selected_lineup_RV <- reactiveVal(NULL)
    observeEvent(lineup_context_RV(),
      {
        # Background publications can invalidate this reactive with the same ID.
        selected <- selected_lineup_RV()
        if (!is.null(selected) && !identical(selected$context, lineup_context_RV())) {
          selected_lineup_RV(NULL)
          removeModal(session = session)
        }
      },
      ignoreInit = TRUE
    )
    observeEvent(input$lineup_team_click, {
      click <- input$lineup_team_click
      if (!is.list(click) || single_round_RV() == "all" || !identical(mvp_text(click$context), lineup_context_RV())) {
        return()
      }
      rows <- standings_RV()
      i <- match(mvp_text(click$id), as.character(rows$team_id))
      catalog <- rounds_RV()
      r <- match(as.numeric(single_round_RV()), catalog$round_number)
      if (is.na(i) || is.na(r) || !nzchar(mvp_text(catalog$round_id[r]))) {
        return()
      }
      selected_lineup_RV(list(
        team_id = as.character(rows$team_id[i]), team_name = rows$team_name[i],
        round_id = as.character(catalog$round_id[r]), round_number = single_round_RV(), context = lineup_context_RV()
      ))
      showModal(modalDialog(
        title = paste(rows$team_name[i], "· Round", single_round_RV()),
        div(class = "player-card-close-shortcut fm-lineup-close-shortcut", modalButton("Close lineup")),
        uiOutput(session$ns("round_lineup_ui")), size = "l", easyClose = TRUE,
        footer = modalButton("Close")
      ), session = session)
    })
    output$round_lineup_ui <- renderUI({
      selected <- selected_lineup_RV()
      req(!is.null(selected), identical(selected$context, lineup_context_RV()), is_module_active() == TRUE)
      result <- round_lineup_read_state(function() get_round_lineup_snapshot(login_token(), championship_id(), selected$team_id, selected$round_id))
      if (is.null(result$data)) {
        return(p(if (isTRUE(result$loading)) {
          "Loading the official round lineup…"
        } else {
          resource_failure_message(result$error)
        }))
      }
      tagList(
        if (identical(attr(result$data, "fetch_status"), "stale")) {
          p("Showing the saved lineup while the latest source is being checked or is unavailable.")
        },
        round_lineup_pitch_ui(result$data$answer)
      )
    })
    output$classification_table <- renderReactable({
      rows <- standings_RV()
      if (!nrow(rows)) {
        return(reactable(data.frame(Message = empty_standings_message_RV())))
      }
      rows <- rows[order(!is.finite(rows$points), rows$rank, rows$team_name, na.last = TRUE), , drop = FALSE]
      bonus_columns <- c("points_bonus", "mvp_bonus", "best_xi_bonus", "position_bonus")
      pending_only <- length(attr(rows, "bonus_pending_rounds")) > 0L &&
        !length(attr(rows, "bonus_completed_rounds"))
      bonus_na <- if (pending_only) {
        "Pending"
      } else if (isTRUE(finance_data_RV()$loading)) {
        "Loading…"
      } else {
        "Unavailable"
      }
      latest <- single_round_RV() != "all" && as.numeric(single_round_RV()) == max(round_numbers_RV())
      hide_bonuses <- latest && !any(vapply(
        rows[, intersect(bonus_columns, names(rows)), drop = FALSE],
        function(values) any(is.finite(values)), logical(1)
      ))
      ids <- as.character(rows$team_id)
      own_id <- fm_scalar(user_team_id(), "")
      click <- if (single_round_RV() != "all") {
        htmlwidgets::JS(paste0("function(rowInfo){if(!rowInfo)return;var ids=", jsonlite::toJSON(ids), ";Shiny.setInputValue(", jsonlite::toJSON(session$ns("lineup_team_click"), auto_unbox = TRUE), ",{id:ids[rowInfo.index],context:", jsonlite::toJSON(lineup_context_RV(), auto_unbox = TRUE), ",nonce:Date.now()},{priority:'event'});}"))
      } else {
        NULL
      }
      remaining <- rows$players_left
      rows <- rows[, intersect(c("rank", "team_name", "points", "rounds", "points_bonus", "mvp_bonus", "best_xi_bonus", "position_bonus"), names(rows)), drop = FALSE]
      reactable(rows,
        compact = TRUE, striped = TRUE, highlight = single_round_RV() != "all", onClick = click, defaultSorted = "rank", defaultColDef = colDef(na = "Unavailable", sortNALast = TRUE, vAlign = "center", headerVAlign = "center"),
        rowClass = function(index) {
          if (nzchar(own_id) && ids[index] %in% own_id) {
            "fm-own-team-row"
          } else {
            NULL
          }
        },
        columns = c(
          if ("rounds" %in% names(rows)) {
            list(rounds = colDef(show = FALSE))
          },
          list(
            rank = colDef(name = "Rank", minWidth = 65, na = "-"), team_name = colDef(name = "Team", minWidth = 150, cell = function(value, index) {
              if (single_round_RV() == "all") {
                return(value)
              }
              tags$button(
                type = "button", class = "btn btn-link fm-lineup-open", title = paste("Open round lineup for", value),
                onclick = paste0(
                  "event.stopPropagation();Shiny.setInputValue(", jsonlite::toJSON(session$ns("lineup_team_click"), auto_unbox = TRUE),
                  ",{id:", jsonlite::toJSON(ids[index], auto_unbox = TRUE), ",context:", jsonlite::toJSON(lineup_context_RV(), auto_unbox = TRUE),
                  ",nonce:Date.now()},{priority:'event'});"
                ), value
              )
            }), points = colDef(
              name = if (is.null(remaining)) {
                "Points"
              } else {
                "Points (left)"
              },
              cell = function(value, index) {
                classification_points_cell(value, if (!is.null(remaining)) {
                  remaining[index]
                })
              }
            ),
            points_bonus = colDef(name = "Points bonus (€)", show = !hide_bonuses, na = bonus_na, minWidth = 140, format = colFormat(separators = TRUE, digits = 0)),
            mvp_bonus = colDef(name = "MVP bonus (€)", show = !hide_bonuses, na = bonus_na, minWidth = 140, format = colFormat(separators = TRUE, digits = 0)),
            best_xi_bonus = colDef(name = "Best XI bonus (€)", show = !hide_bonuses, na = bonus_na, minWidth = 140, format = colFormat(separators = TRUE, digits = 0)),
            position_bonus = colDef(name = "Position bonus (€)", show = !hide_bonuses, na = bonus_na, minWidth = 140, format = colFormat(separators = TRUE, digits = 0))
          )
        )
      )
    })
    output$rank_evolution_plot <- renderPlotly({
      rows <- round_rows_RV()
      if (!nrow(rows)) {
        return(plot_ly() %>% fm_plot_layout(annotations = list(list(text = "No published round history", showarrow = FALSE))))
      }
      numbers <- sort(unique(rows$round))
      history <- bind_rows(lapply(numbers, function(n) {
        d <- classification_window_table(rows, c(min(numbers), n))
        d$round <- n
        d
      }))
      range <- round_window_RV()
      if (length(range) == 2L) {
        history <- history[history$round >= range[1] & history$round <= range[2], , drop = FALSE]
      }
      if (!is.null(single_round_RV()) && single_round_RV() != "all") {
        history <- history[history$round == as.numeric(single_round_RV()), , drop = FALSE]
      }
      plot_ly(history, x = ~round, y = ~rank, color = ~team_name, type = "scatter", mode = "lines+markers") %>%
        fm_plot_layout(xaxis = list(title = "Round", dtick = 1), yaxis = list(title = "Cumulative rank", autorange = "reversed", dtick = 1))
    })
    output$configured_rewards_ui <- renderUI({
      req(is_module_active() == TRUE, login_token(), championship_id(), user_team_id())
      if (!is.null(refresh_trigger)) {
        refresh_trigger()
      }
      # Reuse the configuration already loaded for League settings. Standalone
      # callers can still acquire it through the scoped team-information cache.
      snapshot <- NULL
      settings <- classification_read_state(function() {
        if (is.function(league_settings_source)) {
          league_settings_source()
        } else {
          list(info = get_user_team_info(login_token(), championship_id(), user_team_id()))
        }
      })
      if (is.null(settings$data)) {
        return(p(if (isTRUE(settings$loading)) {
          "Loading league settings…"
        } else {
          resource_failure_message(settings$error)
        }))
      }
      snapshot <- settings$data
      if (is.function(league_settings_source)) {
        configuration <- utils::modifyList(
          snapshot$league$configuration %||% list(),
          snapshot$info$configuration %||% list()
        )
        if (!length(configuration)) {
          return(p(
            if (identical(snapshot$status, "loading")) {
              "Loading league settings…"
            } else {
              resource_failure_message(snapshot$error)
            }
          ))
        }
        info <- list(configuration = configuration)
      } else {
        info <- snapshot$info
      }
      rules <- normalize_league_rules(info)
      tagList(
        p(
          "Configured rewards — points: ", ui_financial_amount(rules$money_per_point),
          "; ranking pool: ", ui_financial_amount(rules$ranking_pool),
          "; ranking mode: ", if (length(rules$ranking_mode) && !is.na(rules$ranking_mode)) {
            rules$ranking_mode
          } else {
            "Unavailable"
          },
          "; MVP: ", ui_financial_amount(rules$mvp_reward), "; dream-team player: ", ui_financial_amount(rules$dreamteam_reward)
        ),
        if (isTRUE(snapshot$stale)) {
          p("Showing saved league settings while the latest configuration is unavailable.")
        }
      )
    })
    output$dreamteam_box_ui <- renderUI({
      req(is_module_active() == TRUE, login_token(), championship_id())
      if (!is.null(refresh_trigger)) {
        refresh_trigger()
      }
      if (is.null(single_round_RV()) || single_round_RV() == "all") {
        return(p("Select one round to see its published dream team and MVP."))
      }
      catalog <- rounds_RV()
      index <- match(as.numeric(single_round_RV()), catalog$round_number)
      if (is.na(index)) {
        return(p("Round identity unavailable."))
      }
      dreamteam <- classification_read_state(function() get_round_dreamteam(login_token(), championship_id(), catalog$round_id[index]))
      if (is.null(dreamteam$data)) {
        return(p(if (isTRUE(dreamteam$loading)) {
          "Loading the published dream team…"
        } else {
          resource_failure_message(dreamteam$error)
        }))
      }
      players <- classification_dreamteam_rows(dreamteam$data)
      if (!nrow(players)) {
        return(p("The dream team has not been published or is unavailable."))
      }
      tags$ul(lapply(seq_len(nrow(players)), function(i) {
        tags$li(
          players$player[i], " — ", if (is.finite(players$points[i])) {
            players$points[i]
          } else {
            "Unavailable"
          }, " points",
          if (isTRUE(players$mvp[i])) {
            strong(" · MVP")
          }
        )
      }))
    })
  })
}
