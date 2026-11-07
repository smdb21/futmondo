library(reactable)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(plotly)

classification_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width = 12, title = "Matchday results", status = "primary", solidHeader = TRUE,
      div(class = "form-group",
          actionButton(ns("latest_round"), "Latest round"),
          actionButton(ns("general_classification"), "General classification")),
      fluidRow(column(6, sliderInput(ns("round_range_slider"), "Points between rounds", min = 1, max = 38, value = c(1, 38))),
               column(6, selectInput(ns("single_round_select"), "Inspect one round", choices = c("All rounds" = "all")))))),
    fluidRow(box(width = 12, title = "Standings", uiOutput(ns("classification_status")), reactableOutput(ns("classification_table")))),
    fluidRow(box(width = 12, title = "Dream team and configured rewards",
      uiOutput(ns("configured_rewards_ui")), uiOutput(ns("dreamteam_box_ui"))))
  )
}

# Normalize observed per-team round records. Missing points stay NA; opaque
# round IDs are resolved using the championship round catalog, never guessed.
classification_round_rows <- function(answer, team_id, team_name, rounds = data.frame()) {
  empty <- data.frame(team_id = character(), team_name = character(), round = numeric(), points = numeric())
  if (is.null(answer) || !length(answer) || isTRUE(answer$error)) return(empty)
  records <- if (!is.null(answer$rounds)) answer$rounds else answer
  if (is.data.frame(records)) records <- split(records, seq_len(nrow(records)))
  if (!is.list(records)) return(empty)
  pick <- function(x, keys) {
    for (key in keys) if (!is.null(x[[key]]) && length(x[[key]]) == 1L && !is.list(x[[key]])) return(x[[key]])
    NA
  }
  result <- bind_rows(lapply(records, function(record) {
    if (!is.list(record)) return(NULL)
    raw_round <- pick(record, c("round_number", "roundNumber", "number", "round", "id", "_id"))
    n <- suppressWarnings(as.numeric(raw_round))
    if (!is.finite(n) && nrow(rounds) && all(c("round_id", "round_number") %in% names(rounds))) {
      n <- rounds$round_number[match(as.character(raw_round), as.character(rounds$round_id))]
    }
    if (length(n) != 1L || !is.finite(n)) return(NULL)
    points <- suppressWarnings(as.numeric(pick(record, c("points", "score"))))
    data.frame(team_id = as.character(team_id), team_name = as.character(team_name), round = n, points = points)
  }))
  if (!nrow(result)) return(empty)
  result[!duplicated(paste(result$team_id, result$round), fromLast = TRUE), , drop = FALSE]
}

# Scheduled catalog rows are identities, not evidence of published scores.
classification_available_rounds <- function(catalog, history) {
  rounds <- catalog$rounds
  current <- fm_number(catalog$current_round_number)
  observed <- history$round
  if(is.finite(current))observed <- observed[observed<=current]
  closed <- if(is.data.frame(rounds)&&all(c("round_number","is_finished")%in%names(rounds)))
    rounds$round_number[rounds$is_finished%in%TRUE]else numeric()
  playing <- if(is.data.frame(rounds)&&all(c("round_number","status")%in%names(rounds)))
    rounds$round_number[tolower(rounds$status)%in%c("active","playing","in_progress")]else numeric()
  numbers <- sort(unique(c(observed,closed,playing,if(isTRUE(catalog$current_verified)&&is.finite(current))current)))
  # Numbered preceding rounds are required coverage even when their completion
  # flags are missing. This is a filter domain, never financial finality.
  if(length(numbers)&&is.data.frame(rounds)&&"round_number"%in%names(rounds))
    numbers <- sort(unique(c(numbers,rounds$round_number[is.finite(rounds$round_number)&rounds$round_number<=max(numbers)])))
  numbers[is.finite(numbers)&numbers>=1&numbers==floor(numbers)]
}

classification_merge_ranking <- function(history, ranking, teams) {
  if(is.null(ranking)||!is.data.frame(ranking$rows)||!nrow(ranking$rows))return(history)
  observed <- ranking$rows
  # Keep absent source scores unknown even when the directory has cumulative
  # points. Replacing by team/round also prevents counting live points twice.
  directory <- data.frame(team_id=as.character(teams$teamid),team_name=as.character(teams$teamname))
  names <- directory$team_name[match(observed$team_id,directory$team_id)]
  observed$team_name[!is.na(names)] <- names[!is.na(names)]
  absent <- !directory$team_id%in%observed$team_id
  if(any(absent))observed <- bind_rows(observed,data.frame(team_id=directory$team_id[absent],
    team_name=directory$team_name[absent],round_id=ranking$round_id,round=ranking$round_number,
    points=NA_real_,score_status="unavailable"))
  keys <- paste(observed$team_id,observed$round)
  bind_rows(history[!paste(history$team_id,history$round)%in%keys,,drop=FALSE],observed)
}

classification_window_table <- function(rows, window = NULL, single = "all", expected_rounds = NULL) {
  if (is.null(rows) || !nrow(rows)) return(data.frame())
  teams <- unique(rows[, c("team_id", "team_name"), drop = FALSE])
  expected <- expected_rounds %||% unique(rows$round)
  if (!is.null(single) && length(single) == 1L && single != "all") {
    rows <- rows[rows$round == suppressWarnings(as.numeric(single)), , drop = FALSE]
    expected <- suppressWarnings(as.numeric(single))
  } else if (length(window) == 2L && all(is.finite(window))) {
    rows <- rows[rows$round >= window[1] & rows$round <= window[2], , drop = FALSE]
    expected <- expected[expected>=window[1]&expected<=window[2]]
  }
  if (!length(expected)) return(data.frame())
  # Compare the same published rounds for every team. An absent record is
  # missing evidence, not a zero-point round or permission to sum fewer rounds.
  expected <- length(unique(expected))
  totals <- rows %>% group_by(team_id, team_name) %>%
    summarise(points = if (all(is.finite(points)) && n_distinct(round) == expected) sum(points) else NA_real_,
              rounds = n_distinct(round), .groups = "drop")
  teams %>% left_join(totals, by = c("team_id", "team_name")) %>%
    mutate(rounds = ifelse(is.na(rounds), 0L, rounds),
           rank = rank(-points, ties.method = "min", na.last = "keep")) %>% arrange(rank, team_name)
}

# Project the shared ledger's coverage-aware round totals without I/O.
classification_bonus_table <- function(standings, history, finances, window=NULL, single="all",expected_rounds=NULL) {
  kinds<-c(points_bonus="points",mvp_bonus="mvp",best_xi_bonus="dreamteam",position_bonus="ranking")
  selected<-sort(unique(expected_rounds %||% history$round))
  if(length(single)==1L && single!="all") selected<-selected[selected==as.numeric(single)]
  else if(length(window)==2L && all(is.finite(window))) selected<-selected[selected>=window[1]&selected<=window[2]]
  provisional <- "score_status"%in%names(history)&&any(history$round%in%selected&history$score_status%in%c("provisional","unavailable"))
  for(column in names(kinds)) standings[[column]]<-vapply(as.character(standings$team_id),function(id){
    totals<-attr(finances$cash_ledgers[[id]],"round_bonus_totals")
    if(provisional||!length(selected)||!is.data.frame(totals)||!nrow(totals))return(NA_real_)
    rows<-totals[totals$bonus_type%in%kinds[[column]] & totals$round_number%in%selected,,drop=FALSE]
    if(nrow(rows)!=length(selected)||!setequal(rows$round_number,selected)||any(!is.finite(rows$amount)))return(NA_real_)
    sum(rows$amount)
  },numeric(1))
  standings
}

classification_dreamteam_rows <- function(answer) {
  if (is.null(answer) || isTRUE(answer$error) || !length(answer$players)) return(data.frame())
  players <- answer$players
  if (is.data.frame(players)) players <- split(players, seq_len(nrow(players)))
  scalar <- function(x, default = NA_character_) if (length(x) == 1L && !is.list(x)) as.character(x) else default
  mvp <- answer$mvp
  if (is.list(mvp)) mvp <- if (!is.null(mvp$id)) mvp$id else mvp[["_id"]]
  bind_rows(lapply(players, function(p) {
    id <- scalar(if (!is.null(p$id)) p$id else p[["_id"]])
    data.frame(player_id = id, player = scalar(p$name), role = scalar(p$role),
      points = suppressWarnings(as.numeric(scalar(p$points))),
      mvp = !is.na(id) && id %in% as.character(mvp), stringsAsFactors = FALSE)
  }))
}

classification_Server <- function(id, is_module_active, login_token, championship_id, user_team_id,
                                  user_teams_RV, refresh_trigger = NULL, league_settings_source = NULL) {
  moduleServer(id, function(input, output, session) {
    bind_factual_reads(c("get_classification_round_catalog","get_classification_round_ranking","get_user_team_rounds","get_user_team_info","get_round_dreamteam"),session,login_token,championship_id,user_team_id,is_active=is_module_active,refresh_trigger=refresh_trigger)

    mvp_snapshot <- shared_mvp_source(session,login_token,championship_id,is_module_active,refresh_trigger)
    finances_RV <- shared_rival_finance_source(session,login_token,championship_id,
      user_team_id,user_teams_RV,is_module_active,mvp_snapshot,refresh_trigger)

    catalog_RV <- reactive({
      req(is_module_active() == TRUE, login_token(), championship_id())
      if (!is.null(refresh_trigger)) refresh_trigger()
      tryCatch(get_classification_round_catalog(login_token(), championship_id()), error = function(e) empty_round_catalog())
    })
    rounds_RV <- reactive(catalog_RV()$rounds)
    historical_rows_RV <- reactive({
      req(is_module_active() == TRUE, login_token(), championship_id())
      if (!is.null(refresh_trigger)) refresh_trigger()
      teams <- user_teams_RV(); req(teams)
      bind_rows(lapply(seq_len(nrow(teams)), function(i) {
        ans <- tryCatch(get_user_team_rounds(login_token(), championship_id(), as.character(teams$teamid[i])), error = function(e) NULL)
        classification_round_rows(ans, teams$teamid[i], teams$teamname[i], rounds_RV())
      }))
    })
    active_ranking_RV <- reactive({
      catalog <- catalog_RV();rounds <- catalog$rounds
      number <- fm_number(catalog$current_round_number)
      if(!is.data.frame(rounds)||!nrow(rounds)||!is.finite(number))return(NULL)
      # The lineup marker may already name next week's round. Only use explicit
      # calendar identities, and check at most that marker plus its predecessor.
      candidates <- rounds[is.finite(rounds$round_number)&rounds$round_number<=number,,drop=FALSE]
      candidates <- head(candidates[order(candidates$round_number,decreasing=TRUE),,drop=FALSE],2L)
      if("status"%in%names(candidates))candidates <- candidates[order(!tolower(candidates$status)%in%c("active","playing","in_progress")),,drop=FALSE]
      failure <- NULL;loading <- FALSE
      for(i in seq_len(nrow(candidates))){
        result <- tryCatch(get_classification_round_ranking(login_token(),championship_id(),user_team_id(),
          candidates$round_id[i],candidates$round_number[i]),error=function(e){
            if(inherits(e,"shiny.silent.error")&&grepl("Loading",conditionMessage(e),fixed=TRUE))loading<<-TRUE
            else failure<<-e
            NULL
          })
        if(!is.null(result)&&is.data.frame(result$rows)&&nrow(result$rows))return(result)
        # A failed known live round remains unavailable. A completed prior
        # round cannot silently replace it as the latest matchday.
        if("status"%in%names(candidates)&&tolower(candidates$status[i])%in%c("active","playing","in_progress"))break
      }
      list(rows=data.frame(),loading=loading,error=failure)
    })
    round_rows_RV <- reactive({
      history <- historical_rows_RV();catalog <- catalog_RV()
      ranking <- active_ranking_RV()
      current <- if(!is.null(ranking)&&nrow(ranking$rows))ranking$round_number else fm_number(catalog$current_round_number)
      if(is.finite(current))history <- history[history$round<=current,,drop=FALSE]
      classification_merge_ranking(history,ranking,user_teams_RV())
    })
    round_numbers_RV <- reactive({
      catalog <- catalog_RV();ranking <- active_ranking_RV()
      if(!is.null(ranking)&&nrow(ranking$rows)){catalog$current_round_number<-ranking$round_number;catalog$current_verified<-TRUE}
      numbers <- classification_available_rounds(catalog,round_rows_RV())
      if(!is.null(ranking)&&nrow(ranking$rows))numbers<-numbers[numbers<=ranking$round_number]
      numbers
    })
    selection_mode <- reactiveVal("latest")
    sent_selection <- NULL
    send_selection <- function(numbers,mode) {
      window <- if(mode=="latest")rep(max(numbers),2)else range(numbers)
      sent_selection <<- list(window=as.numeric(window),single="all")
      updateSliderInput(session,"round_range_slider",value=window)
      updateSelectInput(session,"single_round_select",selected="all")
    }
    observeEvent(round_numbers_RV(), {
      numbers <- round_numbers_RV()
      if (!length(numbers)) return()
      updateSliderInput(session, "round_range_slider", min = min(numbers), max = max(c(numbers, min(numbers) + 1)))
      updateSelectInput(session, "single_round_select", choices = c("All rounds" = "all", stats::setNames(as.character(numbers), paste("Round", numbers))))
      mode <- isolate(selection_mode())
      if(mode%in%c("latest","general"))send_selection(numbers,mode)
    })
    observeEvent(input$latest_round, {
      selection_mode("latest")
      numbers <- round_numbers_RV()
      if(length(numbers))send_selection(numbers,"latest")
    })
    observeEvent(input$general_classification, {
      selection_mode("general")
      numbers <- round_numbers_RV()
      if(length(numbers))send_selection(numbers,"general")
    })
    observeEvent(input$round_range_slider, {
      if(!is.null(sent_selection)&&identical(as.numeric(input$round_range_slider),sent_selection$window))return()
      selection_mode("manual")
    },ignoreInit=TRUE,priority=-1)
    observeEvent(input$single_round_select, {
      if(!is.null(sent_selection)&&identical(input$single_round_select,sent_selection$single))return()
      selection_mode("manual")
    },ignoreInit=TRUE,priority=-1)
    filtered_rows_RV <- reactive({
      classification_window_table(round_rows_RV(), input$round_range_slider, input$single_round_select,round_numbers_RV())
    })
    standings_RV <- reactive({
      req(is_module_active() == TRUE)
      rows <- filtered_rows_RV()
      numbers <- round_numbers_RV();window <- input$round_range_slider
      general <- (is.null(input$single_round_select)||input$single_round_select=="all")&&
        !identical(selection_mode(),"latest")&&(!length(numbers)||length(window)!=2L||
          (window[1]<=min(numbers)&&window[2]>=max(numbers)))
      if (general) {
        teams <- user_teams_RV(); req(teams)
        # The official cumulative source remains authoritative when historical
        # team-round lists omit whole rounds; never label a partial sum General.
        if("points"%in%names(teams))rows <- data.frame(team_id=as.character(teams$teamid),team_name=teams$teamname,
          points=suppressWarnings(as.numeric(teams$points)),
          rank=rank(-suppressWarnings(as.numeric(teams$points)),ties.method="min",na.last="keep"))
      }
      if (!nrow(rows)) return(rows)
      classification_bonus_table(rows, round_rows_RV(), finances_RV(), input$round_range_slider, input$single_round_select,round_numbers_RV())
    })
    output$classification_status <- renderUI({
      ranking <- active_ranking_RV()
      if (!nrow(round_rows_RV())) return(p(if(isTRUE(ranking$loading))"Loading official round standings…"
        else if(!is.null(ranking$error))resource_failure_message(ranking$error)
        else "Round scores are unavailable; General classification uses official cumulative totals."))
      rows <- standings_RV()
      if (!nrow(rows)) return(p(if(isTRUE(ranking$loading))"Loading official round standings…"
        else if(!is.null(ranking$error))resource_failure_message(ranking$error)
        else "No published results in this selection."))
      bonus_columns <- c("points_bonus", "mvp_bonus", "best_xi_bonus", "position_bonus")
      missing_bonus <- any(!is.finite(as.matrix(rows[, bonus_columns, drop = FALSE])))
      selected <- round_rows_RV()
      if(length(input$single_round_select)==1L&&input$single_round_select!="all")selected<-selected[selected$round==as.numeric(input$single_round_select),,drop=FALSE]
      else if(length(input$round_range_slider)==2L)selected<-selected[selected$round>=input$round_range_slider[1]&selected$round<=input$round_range_slider[2],,drop=FALSE]
      live <- nrow(selected)&&"score_status"%in%names(selected)&&any(selected$score_status%in%c("provisional","unavailable"))
      stale <- identical(attr(active_ranking_RV(),"fetch_status"),"stale")
      tagList(p("Points and reconstructed bonus totals for the selected rounds."),
        if(live)p("Live round points are provisional. Rewards remain unavailable until the round and its payments are confirmed."),
        if(!is.null(ranking$error))p(resource_failure_message(ranking$error)),
        if(stale)p("Showing saved round standings while the latest source request is unavailable."),
        if (any(!is.finite(rows$points))) p("Some selected round scores are missing; affected point totals are unavailable."),
        if (missing_bonus) p(if (isTRUE(finances_RV()$loading)) "Bonus totals are loading."
          else "Some bonus totals lack complete round or award records and are shown as Unavailable."))
    })
    output$classification_table <- renderReactable({
      rows <- standings_RV()
      if (!nrow(rows)) return(reactable(data.frame(Message = "No results available.")))
      rows <- rows[, intersect(c("rank", "team_name", "points", "rounds", "points_bonus", "mvp_bonus", "best_xi_bonus", "position_bonus"), names(rows)), drop = FALSE]
      reactable(rows, compact = TRUE, striped = TRUE, defaultSorted = "rank", defaultColDef = colDef(na = "Unavailable"),
                columns = list(rank = colDef(name = "Rank", minWidth = 65), team_name = colDef(name = "Team", minWidth = 150), points = colDef(name = "Points"),
                  points_bonus = colDef(name = "Points bonus (€)", minWidth = 140, format = colFormat(separators = TRUE, digits = 0)),
                  mvp_bonus = colDef(name = "MVP bonus (€)", minWidth = 140, format = colFormat(separators = TRUE, digits = 0)),
                  best_xi_bonus = colDef(name = "Best XI bonus (€)", minWidth = 140, format = colFormat(separators = TRUE, digits = 0)),
                  position_bonus = colDef(name = "Position bonus (€)", minWidth = 140, format = colFormat(separators = TRUE, digits = 0))))
    })
    output$rank_evolution_plot <- renderPlotly({
      rows <- round_rows_RV()
      if (!nrow(rows)) return(plot_ly() %>% fm_plot_layout(annotations = list(list(text = "No published round history", showarrow = FALSE))))
      numbers <- sort(unique(rows$round))
      history <- bind_rows(lapply(numbers, function(n) {
        d <- classification_window_table(rows, c(min(numbers), n)); d$round <- n; d
      }))
      range <- input$round_range_slider
      if (length(range) == 2L) history <- history[history$round >= range[1] & history$round <= range[2], , drop = FALSE]
      if (!is.null(input$single_round_select) && input$single_round_select != "all") history <- history[history$round == as.numeric(input$single_round_select), , drop = FALSE]
      plot_ly(history, x = ~round, y = ~rank, color = ~team_name, type = "scatter", mode = "lines+markers") %>%
        fm_plot_layout(xaxis = list(title = "Round", dtick = 1), yaxis = list(title = "Cumulative rank", autorange = "reversed", dtick = 1))
    })
    output$configured_rewards_ui <- renderUI({
      req(is_module_active() == TRUE, login_token(), championship_id(), user_team_id())
      if (!is.null(refresh_trigger)) refresh_trigger()
      # Reuse the configuration already loaded for League settings. Standalone
      # callers can still acquire it through the scoped team-information cache.
      snapshot <- NULL
      if (is.function(league_settings_source)) {
        snapshot <- league_settings_source()
        configuration <- utils::modifyList(snapshot$league$configuration %||% list(),
          snapshot$info$configuration %||% list())
        if (!length(configuration)) validate(need(FALSE,
          if (identical(snapshot$status, "loading")) "Loading league settings…"
          else resource_failure_message(snapshot$error)))
        info <- list(configuration = configuration)
      } else info <- get_user_team_info(login_token(), championship_id(), user_team_id())
      rules <- normalize_league_rules(info)
      tagList(p("Configured rewards — points: ", ui_financial_amount(rules$money_per_point),
        "; ranking pool: ", ui_financial_amount(rules$ranking_pool),
        "; ranking mode: ", if (length(rules$ranking_mode) && !is.na(rules$ranking_mode)) rules$ranking_mode else "Unavailable",
        "; MVP: ", ui_financial_amount(rules$mvp_reward), "; dream-team player: ", ui_financial_amount(rules$dreamteam_reward)),
        if (isTRUE(snapshot$stale)) p("Showing saved league settings while the latest configuration is unavailable."))
    })
    output$dreamteam_box_ui <- renderUI({
      req(is_module_active() == TRUE, login_token(), championship_id())
      if (!is.null(refresh_trigger)) refresh_trigger()
      if (is.null(input$single_round_select) || input$single_round_select == "all") return(p("Select one round to see its published dream team and MVP."))
      catalog <- rounds_RV(); index <- match(as.numeric(input$single_round_select), catalog$round_number)
      if (is.na(index)) return(p("Round identity unavailable."))
      ans <- get_round_dreamteam(login_token(), championship_id(), catalog$round_id[index])
      players <- classification_dreamteam_rows(ans)
      if (!nrow(players)) return(p("The dream team has not been published or is unavailable."))
      tags$ul(lapply(seq_len(nrow(players)), function(i) tags$li(
        players$player[i], " — ", if (is.finite(players$points[i])) players$points[i] else "Unavailable", " points",
        if (isTRUE(players$mvp[i])) strong(" · MVP"))))
    })
  })
}
