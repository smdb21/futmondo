library(reactable)

players_in_teams_UI <- function(id) {
  ns <- NS(id)
  tagList(
    players_table_UI(
      id = ns("players_table_in_teams"), box_title = "Players in Team",
      filter_by_position = TRUE,
      filter_by_team = FALSE,
      filter_by_is_favorite = FALSE,
      filter_by_is_from_futmondo = FALSE
    )
  )
}


players_in_teams_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV) {
  moduleServer(id, function(input, output, session) {
    # observers ----

    # reactives ----
    ## players_table_RV ----
    players_table_RV <- reactive({
      req(is_module_active() == TRUE)
      req(login_token())
      req(championship_id())
      req(user_team_id())
      championship_id <- championship_id()
      user_team_id <- user_team_id()
      players_table <- get_players_from_team(
        login = login_token(),
        championship_id = championship_id,
        user_team_id = user_team_id,
        teams = NULL
      )
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
      id = "players_table_in_teams",
      players_table_RV = players_table_RV,
      user_teams_RV = user_teams_RV
    )

    return(selected_player_RV)
  })
}
