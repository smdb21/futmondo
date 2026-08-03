library(reactable)

players_in_teams_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("team_value_box")),
    players_table_UI(
      id = ns("players_table_in_teams"), box_title = "Players in Team",
      filter_by_position = TRUE,
      filter_by_team = FALSE,
      filter_by_is_favorite = FALSE,
      filter_by_is_from_futmondo = FALSE
    )
  )
}


players_in_teams_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    # renders ----
    output$team_value_box <- renderUI({
      players_table <- players_table_RV()
      req(players_table)

      user_teams <- user_teams_RV()

      # Safeguard against missing or empty user teams data
      if (is.null(user_teams) || nrow(user_teams) == 0 || !"points" %in% colnames(user_teams)) {
        return(
          tagList(
            box(
              title = "Championship Overview",
              width = 12,
              status = "warning",
              solidHeader = TRUE,
              "No standings or team data is currently available for this championship."
            )
          )
        )
      }

      user_teams <- user_teams %>%
        dplyr::mutate(points = as.numeric(points))

      team_points <- user_teams %>%
        dplyr::arrange(desc(points)) %>%
        dplyr::select(teamid, teamname, points) %>%
        dplyr::mutate(position = row_number())
      team_info_table <- team_points %>%
        dplyr::filter(teamid == user_team_id())

      if (nrow(team_info_table) == 0) {
        return(
          tagList(
            box(
              title = "Championship Overview",
              width = 12,
              status = "warning",
              solidHeader = TRUE,
              "Your user team was not found in the championship participant list."
            )
          )
        )
      }

      team_points <- team_points %>%
        dplyr::mutate(diff_points = points - team_info_table$points[1])
      team_position <- team_points %>%
        dplyr::filter(teamid == user_team_id()) %>%
        dplyr::pull(position)
      if (team_position > 1) {
        previous_team <- team_points %>%
          dplyr::filter(position == team_position - 1)
      } else {
        previous_team <- NULL
      }
      if (team_position < nrow(user_teams)) {
        next_team <- team_points %>%
          dplyr::filter(position == team_position + 1)
      } else {
        next_team <- NULL
      }
      # add st, nd, rd, th to position
      team_position <- get_ordinal_position(team_position)

      total_teams <- nrow(user_teams_RV())
      team_position <- paste0(team_position, " of ", total_teams)
      team_name <- team_info_table$teamname[1]
      user_name <- team_info_table$name

      # Safeguards for empty roster calculations
      val_sum <- sum(players_table$value, na.rm = TRUE)
      val_mean <- if (nrow(players_table) > 0) mean(players_table$value, na.rm = TRUE) else 0

      team_value <- val_sum %>%
        # format it as currency in eur
        scales::label_currency(prefix = "€", suffix = "M", scale = 1e-6)(.)
      average_player_value <- val_mean %>%
        # format it as currency in eur
        scales::label_currency(prefix = "€", suffix = "M", scale = 1e-6)(.)
      team_value_block <- descriptionBlock(
        header = team_value,
        number = NULL,
        numberColor = "black",
        text = "Team value"
      )
      team_value_change <- sum(players_table$change, na.rm = TRUE)
      team_value_change_pct <- if (nrow(players_table) > 0 && (val_sum - team_value_change) != 0) {
        team_value_change / (val_sum - team_value_change) * 100
      } else {
        0
      }
      team_value_change_pct <- round(team_value_change_pct, 2)
      team_value_change_icon <- if (team_value_change > 0) {
        icon("caret-up")
      } else if (team_value_change < 0) {
        icon("caret-down")
      } else {
        NULL
      }
      team_change_value_block <- descriptionBlock(
        header = team_value_change %>% format_currency(),
        number = paste0(team_value_change_pct, "%"),  
        numberColor = "green",
        numberIcon = team_value_change_icon,
        text = "Team value change"
      )
      team_players_value_block <- descriptionBlock(
        header = average_player_value,
        number = NULL,
        numberColor = "black",
        text = "Avg player value"
      )
      team_position_block <- descriptionBlock(
        header = team_position,
        number = NULL,
        numberColor = "black",
        text = "General Position"
      )
      team_points_block <- descriptionBlock(
        header = team_info_table$points,
        number = NULL,
        text = paste0(team_info_table$teamname, " (", get_ordinal_position(team_info_table$position), ")")
      )
      if (!is.null(previous_team)) {
        previous_team_block <- descriptionBlock(
          header = previous_team$points,
          number = paste0("+", previous_team$diff_points),
          numberColor = "red",
          numberIcon = icon("angle-up"),
          text = paste0(previous_team$teamname, " (", get_ordinal_position(previous_team$position), ")")
        )
      } else {
        previous_team_block <- NULL
      }
      if (!is.null(next_team)) {
        next_team_block <- descriptionBlock(
          header = next_team$points,
          number = next_team$diff_points,
          numberColor = "green",
          numberIcon = icon("angle-down"),
          text = paste0(next_team$teamname, " (", get_ordinal_position(next_team$position), ")")
        )
      } else {
        next_team_block <- NULL
      }
      team_value_box <- box(
        title = "Value",
        width = 5,
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        fluidRow(
          column(4, team_value_block),
          column(4, team_change_value_block),
          column(4, team_players_value_block)
        )
      )
      team_position_box <- box(
        title = "Classification",
        width = 2,
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        team_position_block
      )
      team_points_box <- box(
        title = "Points",
        width = 5,
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        fluidRow(
          column(4, previous_team_block),
          column(4, team_points_block),
          column(4, next_team_block)
        )
      )
      ret <- tagList(
        team_position_box,
        team_points_box,
        team_value_box
      )
      return(ret)
    })
    # observers ----

    # reactives ----
    ## players_table_RV ----
    players_table_RV <- reactive({
      req(is_module_active() == TRUE)
      req(login_token())
      req(championship_id())
      req(user_team_id())
      if (!is.null(refresh_trigger)) refresh_trigger() # Cache invalidation dependency
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

get_ordinal_position <- function(position) {
  position <- case_when(
    position == 1 ~ paste0(position, "st"),
    position == 2 ~ paste0(position, "nd"),
    position == 3 ~ paste0(position, "rd"),
    TRUE ~ paste0(position, "th")
  )
  return(position)
}
