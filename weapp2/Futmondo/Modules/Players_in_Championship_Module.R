library(reactable)

players_in_championship_UI <- function(id) {
  ns <- NS(id)
  tagList(
    players_table_UI(
      id = ns("championship_players_table"),
      box_title = "All Players in Championship",
      filter_by_position = TRUE,
      filter_by_team = TRUE,
      filter_by_value = TRUE,
      filter_by_active_clause = TRUE,
      filter_by_max_clause_value = TRUE,
      filter_by_clause_under_available_funds = TRUE
    )
  )
}


players_in_championship_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
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
      if (length(funds) == 1L && is.finite(funds) && funds >= 0) funds else NA_real_
    })

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
      refresh_trigger = refresh_trigger
    )

    return(selected_player_RV)
  })
}
