library(reactable)

players_in_championship_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("market_prices_status")),
    players_table_UI(
      id = ns("championship_players_table"),
      box_title = "All Players in Championship",
      filter_by_position = TRUE,
      filter_by_team = TRUE,
      filter_by_value = TRUE,
      filter_by_active_clause = TRUE,
      default_active_clause = TRUE,
      filter_by_max_clause_value = TRUE,
      filter_by_clause_under_available_funds = TRUE,
      filter_by_own_team = TRUE
    )
  )
}


# Join observed asking prices only; catalog valuation and clause price are distinct.
championship_market_prices <- function(players, market_state) {
  if (!is.data.frame(players)) return(players)
  players$effective_market_price <- rep(NA_real_, nrow(players))
  players$market_inMarket <- rep(NA, nrow(players))
  market <- market_state$data
  attr(players,"availability_owner_valid_until")<-player_availability_deadline(players,"championshipplayers")
  attr(players,"availability_market_valid_until")<-player_availability_deadline(market,"market")
  attr(players,"availability_market_observed_at")<-market_state$observed_at %||% attr(market,"observed_at")
  attr(players,"availability_complete")<-market_state$status%in%c("ok","empty")&&
    identical(market_state$completeness,"complete")&&is.null(market_state$error)
  if (!is.data.frame(market) || !"id" %in% names(players)) return(players)
  if (!nrow(market)) {
    if (identical(market_state$status, "empty") && identical(market_state$completeness, "complete") && is.null(market_state$error))
      players$market_inMarket[] <- FALSE
    return(players)
  }
  if (!all(c("id", "price") %in% names(market))) return(players)
  ids <- as.character(market$id)
  valid <- !is.na(ids) & nzchar(ids) & !duplicated(ids) & !duplicated(ids, fromLast=TRUE)
  matched <- match(as.character(players$id), ids[valid])
  prices <- suppressWarnings(as.numeric(as.character(market$price[valid])))
  players$effective_market_price <- prices[matched]
  players$effective_market_price[!is.finite(players$effective_market_price) | players$effective_market_price <= 0] <- NA_real_
  players$market_inMarket[!is.na(matched)] <- TRUE
  # Absence is established only by a successful, complete current market list.
  if (market_state$status %in% c("ok", "empty") && identical(market_state$completeness, "complete") && is.null(market_state$error) && all(valid))
    players$market_inMarket[is.na(matched)] <- FALSE
  players
}

players_in_championship_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger = NULL, clause_transfers_source = NULL) {
  moduleServer(id, function(input, output, session) {
    bind_factual_reads(c("get_championship_players","get_financial_snapshot"),session,login_token,championship_id,user_team_id,is_active=is_module_active,refresh_trigger=refresh_trigger)

    market_reader <- factual_read_adapter(session,"get_market_players",login_token,championship_id,user_team_id,
      is_active=is_module_active,refresh_trigger=refresh_trigger,state_only=TRUE)
    market_state <- reactive({
      req(is_module_active(), login_token(), championship_id(), user_team_id())
      market_reader(login=login_token(), championship_id=championship_id(), user_team_id=user_team_id())
    })
    market_prices_status <- resource_display_source(session,function() {
      state <- market_state()
      if (identical(state$status,"loading")) return(tags$div(class="text-muted",role="status","Market prices loading…"))
      if (is.null(state$data) || !is.null(state$error))
        return(fm_info(paste("Market prices:",resource_failure_message(state$error)),error=TRUE))
      if (identical(state$freshness,"stale")) return(fm_info("Market prices show the last stored observation.",error=TRUE))
      fm_info("Market prices apply to listed players. Clause transfer dates apply to pending transfers, which Active Clause Only excludes.")
    })
    output$market_prices_status <- renderUI(market_prices_status())

    # observers ----

    # reactives ----


    ## players_table_RV ----
    players_table_RV <- reactive({
      req(is_module_active() == TRUE)
      req(login_token())
      req(championship_id())
      if (!is.null(refresh_trigger)) refresh_trigger() # Cache invalidation dependency
      championship_id <- championship_id()
      players_table <- get_championship_players(
        login = login_token(),
        championship_id = championship_id
      )
      players_table <- championship_market_prices(players_table, market_state())
      players_table <- players_table %>%
        translate_player_positions()
      players_table <- players_table %>%
        calculate_player_changes()
      players_table <- players_table %>%
        unify_columns()
      return(players_table)
    })
    available_funds_RV <- reactive({
      req(is_module_active() == TRUE, login_token(), championship_id(), user_team_id())
      if (!is.null(refresh_trigger)) refresh_trigger()
      finance <- tryCatch(get_financial_snapshot(login_token(), championship_id(), user_team_id()), error = function(e) NULL)
      if (!is.list(finance) || !identical(finance$status, "ok")) return(NA_real_)
      funds <- suppressWarnings(as.numeric(finance$spendable_budget))
      if(exists('fm_clause_enabled',mode='function')&&fm_clause_enabled()&&is.null(finance$scheduled_reservations)){
        reserved<-fm_clause_reservations(fm_clause_jobs(resource_context(login_token(),championship_id(),user_team_id())))$amount
        funds<-if(is.finite(reserved))max(0,funds-reserved)else NA_real_
      }
      if (length(funds) == 1L && is.finite(funds) && funds >= 0) funds else NA_real_
    })

    incoming_player_ids <- if(is.function(clause_transfers_source))resource_display_source(session,
      function()sort(unique(as.character(clause_transfers_source()$records$player_id))))else NULL

    # Module ----
    ##  players_table_Server Module ----
    selected_player_RV <- players_table_Server(
      id = "championship_players_table",
      players_table_RV = players_table_RV,
      user_teams_RV = user_teams_RV,
      login_token = login_token,
      championship_id = championship_id,
      user_team_id = user_team_id,
      available_funds_RV = available_funds_RV,
      points_ratio_basis = "clause_price",
      allow_scheduled_clauses = TRUE,
      filter_by_own_team = TRUE,
      incoming_player_ids = incoming_player_ids,
      refresh_trigger = refresh_trigger,
      is_module_active = is_module_active
    )

    return(selected_player_RV)
  })
}
