market_UI <- function(id) {
  ns <- NS(id)
  tagList(
    players_table_UI(
      id = ns("market_players_table"),
      box_title = "Players in Market",
      filter_by_position = TRUE,
      filter_by_team = TRUE,
      filter_by_is_favorite = FALSE,
      filter_by_players_with_bid = TRUE
    )
  )
}
market_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      # reactives ----
      ## market_players_RV ----
      market_players_RV <- reactive({
        req(is_module_active() == TRUE)
        if (!is.null(refresh_trigger)) refresh_trigger() # Cache invalidation dependency
        players_table <- get_market_players(login = login_token(), championship_id = championship_id(), user_team_id = user_team_id())
        players_table <- players_table %>%
          translate_player_positions()
        players_table <- players_table %>%
          calculate_player_changes()
        players_table <- players_table %>%
          unify_columns()
        return(players_table)
      })

      # Module ----
      ##  players_table_Server Module ----
      selected_player_RV <- players_table_Server(
        id = "market_players_table",
        players_table_RV = market_players_RV,
        user_teams_RV = user_teams_RV,
        login_token = login_token,
        championship_id = championship_id,
        user_team_id = user_team_id
      )
    }
  )
}
