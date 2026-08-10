library(reactable)

players_in_teams_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("team_value_box")),
    uiOutput(ns("charts_row")), # Dynamic Plot B & Plot C Container
    div(
      style = "margin-bottom: 15px; display: flex; justify-content: flex-end;",
      actionButton(
        inputId = ns("btn_put_all_on_market"),
        label = tagList(icon("tags"), " Put All Players on Market"),
        class = "btn btn-offer-money"
      )
    ),
    players_table_UI(
      id = ns("players_table_in_teams"), box_title = "Players in Team",
      filter_by_position = TRUE,
      filter_by_team = FALSE,
      filter_by_is_favorite = FALSE,
      filter_by_is_from_futmondo = FALSE,
      show_position_breakdown = TRUE
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
    get_reactive_val <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.reactive(x) || is.function(x)) {
        tryCatch(x(), error = function(e) NULL)
      } else {
        x
      }
    }

    # ---- Put All Players on Market Modal ----
    observeEvent(input$btn_put_all_on_market, {
      showModal(modalDialog(
        title = tagList(icon("tags"), " Put All Players on Market"),
        p("Are you sure you want to list ALL your squad players on the transfer market simultaneously?"),
        p(style = "color: #64748b; font-size: 12px;", "Other users and the computer will be able to place bids on all your players."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("submit_put_all_on_market"), "Confirm Market Listing", class = "btn btn-offer-money")
        ),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Put All Players on Market ----
    observeEvent(input$submit_put_all_on_market, {
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(login, champ_id, team_id)

      res <- put_all_on_market(
        login = login,
        championship_id = champ_id,
        team_id = team_id
      )

      is_success <- if (is.list(res)) isTRUE(res$success) else isTRUE(res)

      removeModal()

      if (is_success) {
        shiny::showNotification(
          "All squad players listed on the transfer market successfully!",
          type = "message",
          duration = 5
        )
        clear_api_cache()
        if (!is.null(refresh_trigger)) {
          tryCatch(refresh_trigger(), error = function(e) NULL)
        }
      } else {
        err_msg <- if (is.list(res) && !is.null(res$message) && res$message != "") res$message else "Bulk listing failed. Please try again."
        shiny::showNotification(
          paste0("Failed to list squad on market: ", err_msg),
          type = "error",
          duration = 6
        )
      }
    })

    # reactives ----
    # Render Interactive Charts Grid
    output$charts_row <- renderUI({
      req(is_module_active() == TRUE)
      ns <- session$ns

      fluidRow(
        column(width = 6,
               box(
                 title = "League Standings Evolution (Points)",
                 width = 12,
                 status = "primary",
                 solidHeader = TRUE,
                 plotly::plotlyOutput(ns("standings_evolution_plot"), height = "300px")
               )
        ),
        column(width = 6,
               box(
                 title = "League Buying Power (Liquid Cash)",
                 width = 12,
                 status = "primary",
                 solidHeader = TRUE,
                 plotly::plotlyOutput(ns("league_finances_plot"), height = "300px")
               )
        )
      )
    })

    # Plot B: Standings Evolution Scatter Plot
    output$standings_evolution_plot <- plotly::renderPlotly({
      req(is_module_active() == TRUE)

      champ_id <- if (!is.null(championship_id)) championship_id() else NULL
      history_df <- NULL
      if (!is.null(champ_id)) {
        tryCatch({
          history_df <- get_league_standings_history(champ_id)
        }, error = function(e) {
          print(paste0("[Plot B] Standings fetch warning: ", e$message))
        })
      }

      has_points <- !is.null(history_df) && nrow(history_df) > 0 && any(!is.na(history_df$points) & history_df$points > 0)

      if (!has_points) {
        # Return empty-state Plotly canvas with centered message box
        return(
          plotly::plot_ly() %>%
            plotly::layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor = "rgba(0,0,0,0)",
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  x = 0.5,
                  y = 0.5,
                  xref = "paper",
                  yref = "paper",
                  text = "<b>No matchday points recorded yet.</b><br><span style='font-size: 12px; color: #64748b;'>The points evolution timeline will display automatically once matchday scores are logged.</span>",
                  showarrow = FALSE,
                  font = list(size = 14, color = "#334155"),
                  align = "center",
                  bgcolor = "#f8fafc",
                  bordercolor = "#cbd5e1",
                  borderwidth = 1,
                  borderpad = 16
                )
              )
            )
        )
      }

      # Format dates if points exist
      history_df$date <- as.POSIXct(history_df$recorded_at, format = "%Y-%m-%dT%H:%M:%S")
      if (any(is.na(history_df$date))) {
        history_df$date <- as.POSIXct(history_df$recorded_at)
      }

      history_df <- history_df %>% dplyr::arrange(date)

      # Render multi-line spline points progression
      plotly::plot_ly(data = history_df, x = ~date, y = ~points, color = ~teamname, type = "scatter", mode = "lines+markers",
                      line = list(width = 2, shape = "spline"),
                      marker = list(size = 5),
                      hoverinfo = "text",
                      text = ~paste0("Team: ", teamname, "<br>Date: ", format(date, "%d-%m-%y"), "<br>Points: ", points)) %>%
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "", gridcolor = "#f1f5f9", zeroline = FALSE, tickformat = "%d-%m"),
          yaxis = list(title = "Cumulative Points", gridcolor = "#f1f5f9", zeroline = FALSE),
          legend = list(orientation = "h", x = 0.5, y = -0.25, xanchor = "center"),
          margin = list(l = 50, r = 20, t = 10, b = 40)
        )
    })

    # Plot C: League Finances horizontal bar chart
    output$league_finances_plot <- plotly::renderPlotly({
      req(is_module_active() == TRUE)
      req(login_token(), championship_id(), user_teams_RV())

      champ_id <- championship_id()
      login <- login_token()
      teams <- user_teams_RV()

      # Calculate current league finances with liquid cash balances
      finances_res <- tryCatch({
        calculate_league_finances(
          login = login,
          championship_id = champ_id,
          user_teams_df = teams,
          initial_budget = 300000000
        )
      }, error = function(e) {
        print(paste0("[Plot C] Finances calculation warning: ", e$message))
        NULL
      })

      teams_df <- if (!is.null(finances_res) && "team_finances" %in% names(finances_res)) finances_res$team_finances else NULL

      # Fallback to Supabase get_user_teams_finances or user_teams_RV
      if (is.null(teams_df) || nrow(teams_df) == 0) {
        teams_df <- tryCatch(get_user_teams_finances(champ_id), error = function(e) NULL)
      }
      if (is.null(teams_df) || nrow(teams_df) == 0) {
        teams_df <- teams
      }

      if (is.null(teams_df) || nrow(teams_df) == 0) {
        return(NULL)
      }

      team_names <- if ("teamname" %in% colnames(teams_df)) teams_df$teamname else if ("name" %in% colnames(teams_df)) teams_df$name else "Unknown"
      team_budgets <- if ("budget" %in% colnames(teams_df)) as.numeric(teams_df$budget) else 0

      plot_df <- data.frame(
        team = as.character(team_names),
        budget = as.numeric(team_budgets),
        stringsAsFactors = FALSE
      ) %>% dplyr::arrange(budget)

      colors <- ifelse(plot_df$budget >= 0, "#10b981", "#ef4444")
      line_colors <- ifelse(plot_df$budget >= 0, "#059669", "#b91c1c")

      plotly::plot_ly(
        data = plot_df,
        x = ~budget,
        y = ~reorder(team, budget),
        type = "bar",
        orientation = "h",
        marker = list(color = colors, line = list(color = line_colors, width = 1)),
        hoverinfo = "text",
        text = ~paste0("<b>", team, "</b><br>Liquid Cash: ", format_table_currency(budget))
      ) %>%
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "Liquid Cash Budget (€)", gridcolor = "#f1f5f9", zeroline = TRUE, zerolinecolor = "#cbd5e1"),
          yaxis = list(title = "", gridcolor = "#f1f5f9", zeroline = FALSE),
          margin = list(l = 120, r = 20, t = 10, b = 40)
        )
    })

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

      # Background Sync Roster Snapshot to Supabase
      tryCatch({
        sync_real_clubs_to_supabase(players_table)
        sync_players_to_supabase(players_table)
        log_player_history(players_table, championship_id)
      }, error = function(e) {
        print(paste0("[Supabase] Roster sync warning: ", e$message))
      })

      return(players_table)
    })

    # Module ----
    ##  players_table_Server Module ----
    selected_player_RV <- players_table_Server(
      id = "players_table_in_teams",
      players_table_RV = players_table_RV,
      user_teams_RV = user_teams_RV,
      login_token = login_token,
      championship_id = championship_id,
      user_team_id = user_team_id
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
