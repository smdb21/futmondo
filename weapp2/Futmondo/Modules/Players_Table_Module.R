library(reactable)

players_table_UI <- function(id, box_title = NULL,
                             solidHeader = TRUE,
                             status = "primary",
                             filter_by_position = TRUE,
                             filter_by_team = TRUE,
                             filter_by_value = TRUE,
                             filter_by_change_value = TRUE,
                             default_minimum_change_value = NA,
                             filter_by_active_clause = TRUE,
                             filter_by_is_favorite = TRUE,
                             filter_by_is_from_futmondo = TRUE,
                             filter_by_players_with_bid = FALSE,
                              show_position_breakdown = FALSE) {
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      width = 12,
      solidHeader = solidHeader,
      status = status,
      title = box_title,

      # Top Filter Bar Grid
      if (show_position_breakdown) {
        fluidRow(
          column(width = 7,
                 fluidRow(
                   style = "background: #f8fafc; padding: 15px; border-radius: 8px; margin: 0 0 20px 0; border: 1px solid #e2e8f0;",
                   if (filter_by_position) {
                     column(width = 4, selectInput(inputId = ns("position_filter"), label = "Position", choices = c("All", "Goalkeeper", "Defender", "Midfielder", "Forward"), selected = "All", width = "100%"))
                   },
                   if (filter_by_team) {
                     column(width = 4, selectInput(inputId = ns("team_filter"), label = "User Team Owner", choices = c("All"), width = "100%"))
                   },
                   if (filter_by_value) {
                     tagList(
                       column(width = 4, numericInput(inputId = ns("min_value_filter"), label = "Min Val (M)", min = 0, max = 1000, value = 0, step = 10, width = "100%")),
                       column(width = 4, numericInput(inputId = ns("max_value_filter"), label = "Max Val (M)", min = 0, max = 1000, value = 1000, step = 10, width = "100%"))
                     )
                   },
                   if (filter_by_change_value) {
                     column(width = 4, numericInput(inputId = ns("change_value_filter"), label = "Min Trend (M)", min = 0, max = 1, value = default_minimum_change_value, step = 0.05, width = "100%"))
                   }
                 )
          ),
          column(width = 5,
                 uiOutput(ns("position_breakdown_ui"))
          )
        )
      } else {
        fluidRow(
          style = "background: #f8fafc; padding: 15px; border-radius: 8px; margin: 0 0 20px 0; border: 1px solid #e2e8f0;",
          if (filter_by_position) {
            column(width = 3, selectInput(inputId = ns("position_filter"), label = "Position", choices = c("All", "Goalkeeper", "Defender", "Midfielder", "Forward"), selected = "All", width = "100%"))
          },
          if (filter_by_team) {
            column(width = 3, selectInput(inputId = ns("team_filter"), label = "User Team Owner", choices = c("All"), width = "100%"))
          },
          if (filter_by_value) {
            tagList(
              column(width = 2, numericInput(inputId = ns("min_value_filter"), label = "Min Val (M)", min = 0, max = 1000, value = 0, step = 10, width = "100%")),
              column(width = 2, numericInput(inputId = ns("max_value_filter"), label = "Max Val (M)", min = 0, max = 1000, value = 1000, step = 10, width = "100%"))
            )
          },
          if (filter_by_change_value) {
            column(width = 2, numericInput(inputId = ns("change_value_filter"), label = "Min Trend (M)", min = 0, max = 1, value = default_minimum_change_value, step = 0.05, width = "100%"))
          }
        )
      },

      # Checkboxes Inline Grid
      if (filter_by_active_clause || filter_by_is_favorite || filter_by_is_from_futmondo || filter_by_players_with_bid) {
        fluidRow(
          style = "padding: 0 15px; margin-bottom: 20px; display: flex; flex-wrap: wrap; gap: 20px; align-items: center;",
          if (filter_by_active_clause) {
            div(checkboxInput(inputId = ns("active_clause_filter"), label = "Active Clause Only", value = FALSE), style = "font-weight: 500;")
          },
          if (filter_by_is_favorite) {
            div(checkboxInput(inputId = ns("is_favorite_filter"), label = "Favorites Only", value = FALSE), style = "font-weight: 500;")
          },
          if (filter_by_is_from_futmondo) {
            div(checkboxInput(inputId = ns("is_from_futmondo_filter"), label = "Free Agents Only", value = FALSE), style = "font-weight: 500;")
          },
          if (filter_by_players_with_bid) {
            tagList(
              div(checkboxInput(inputId = ns("players_you_bid_filter"), label = "Your Bids Only", value = FALSE), style = "font-weight: 500;"),
              div(checkboxInput(inputId = ns("players_with_bid_filter"), label = "Bidded Only", value = FALSE), style = "font-weight: 500;")
            )
          }
        )
      },

      # Table Container
      div(
        reactableOutput(ns("players_table")),
        style = "overflow-x: auto; font-size:85%; width: 100%;"
      )
    )
  )
}


players_table_Server <- function(id, players_table_RV, user_teams_RV, login_token = NULL, championship_id = NULL, user_team_id = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    table_refresh_trigger <- reactiveVal(0)
    # observers ----
    # observe user_teams_RV to update market_player_team_filter ----
    observeEvent(user_teams_RV(), {
      teams <- user_teams_RV()
      req(teams)
      user_team_names <- teams$teamname %>% sort()
      # remove the ones that are not in players_table_RV
      user_team_names <- user_team_names[user_team_names %in% players_table_RV()$userTeam]
      team_choices <- c("All", "Free")
      if (length(user_team_names)) {
        team_choices <- c(team_choices, user_team_names)
      }
      updateSelectInput(session, inputId = "team_filter", choices = team_choices, selected = "All")
    })
    
    # observe selected_player_RV() to open popup with           selected_player_UI(id = "selected_player")
    observeEvent(
      selected_player_RV(),
      {
        req(selected_player_RV())
        showModal(modalDialog(
          # title = "Selected player",
          selected_player_UI(id = ns("selected_player")),
          easyClose = TRUE,
          size = "l"
        ))
      },
      ignoreNULL = TRUE
    )
    # Modules ----
    selected_player_Server(
      id = "selected_player",
      selected_player = selected_player_RV,
      login_token = login_token,
      championship_id = championship_id,
      user_team_id = user_team_id,
      on_bid_updated = function() {
        table_refresh_trigger(table_refresh_trigger() + 1)
      }
    )
    # reactives ----
    ## selected_player_RV
    selected_player_RV <- reactive({
      selected_idx <- getReactableState(outputId = "players_table", name = "selected", session = session)
      req(selected_idx)
      selected_player <- players_table_filtered_RV()[selected_idx, ]
    })
    
    ## players_table_filtered_RV ----
    players_table_filtered_RV <- reactive({
      table_refresh_trigger()
      players_table <- players_table_RV()
      if (is.null(players_table)) {
        return(NULL)
      }
      # players_table <- players_table %>%
      #   translate_player_positions()
      # players_table <- players_table %>%
      #   calculate_player_changes()
      # players_table <- players_table %>%
      #   unify_columns()
      if (!is.null(input$position_filter)) {
        if (input$position_filter != "All") {
          players_table <- players_table %>%
            dplyr::filter(role == input$position_filter | role2 == input$position_filter)
        }
      }
      if (!is.null(input$team_filter)) {
        if (input$team_filter != "All") {
          if (input$team_filter == "Free") {
            players_table <- players_table %>%
              dplyr::filter(is.na(userTeam))
          } else {
            players_table <- players_table %>%
              dplyr::filter(userTeam == input$team_filter)
          }
        }
      }
      if (!is.null(input$min_value_filter)) {
        players_table <- players_table %>%
          dplyr::filter(value >= input$min_value_filter * 1000000)
      }
      if (!is.null(input$max_value_filter)) {
        players_table <- players_table %>%
          dplyr::filter(value <= input$max_value_filter * 1000000)
      }
      if (!is.null(input$active_clause_filter)) {
        if (input$active_clause_filter) {
          players_table <- players_table %>%
            dplyr::filter(isClause)
        }
      }
      # is_favorite_filter
      if (!is.null(input$is_favorite_filter)) {
        if (input$is_favorite_filter) {
          players_table <- players_table %>%
            dplyr::filter(fav == TRUE)
        }
      }
      if (!is.null(input$is_from_futmondo_filter)) {
        if (input$is_from_futmondo_filter) {
          if ("computer" %in% colnames(players_table)) {
            players_table <- players_table %>%
              dplyr::filter(computer == TRUE)
          } else {
            warning("is_from_futmondo_filter is active but 'computer' column is missing from players_table.")
          }
        }
      }
      if (!is.null(input$change_value_filter) && !is.na(input$change_value_filter)) {
        players_table <- players_table %>%
          dplyr::filter(change >= input$change_value_filter * 1000000)
      }
      if (!is.null(input$players_you_bid_filter)) {
        if (input$players_you_bid_filter) {
          players_table <- players_table %>%
            dplyr::filter(bid_price > 0)
        }
      }
      if (!is.null(input$players_with_bid_filter)) {
        if (input$players_with_bid_filter) {
          players_table <- players_table %>%
            # Number of Bids > 0
            dplyr::filter(numberOfBids > 0)
        }
      }
      return(players_table)
    })
    
    # renders ----
    ## render players_table ----
    output$players_table <- renderReactable({
      req(players_table_filtered_RV())
      players_table <- players_table_filtered_RV() %>%
        reorder_player_table_columns() %>%
        dplyr::select(!any_of(cfg_player_columns_to_hide))
      
      print(paste0(nrow(players_table), " players in table"))
      # players_table <- players_table %>%
      #   dplyr::select(
      #     any_of(starts_with("change")), any_of(starts_with("market_")), any_of(starts_with("bid_price")), any_of(starts_with("clause_")), any_of(c("name", "role", "role2", "points", "value", "status", "team", "rating", "change", "average", "total")))
      #
      table_columns <- get_reactable_columns_for_players(players_table)
      reactable(players_table,
                columns = table_columns,
                searchable = TRUE,
                filterable = TRUE,
                defaultPageSize = 20,
                pagination = TRUE,
                striped = TRUE,
                bordered = TRUE,
                highlight = TRUE,
                compact = TRUE,
                fullWidth = FALSE,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(5, 10, 20, 50),
                showPagination = TRUE,
                selection = "single",
                borderless = TRUE,
                onClick = "select",
                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
                )
      )
    })
    
    # Position Breakdown UI Renderer
    output$position_breakdown_ui <- renderUI({
      df <- players_table_RV()
      if (is.null(df) || nrow(df) == 0) return(NULL)

      gk <- sum(df$role == "Goalkeeper" | df$role2 == "Goalkeeper" | df$role == "portero" | df$role2 == "portero", na.rm = TRUE)
      df_cnt <- sum(df$role == "Defender" | df$role2 == "Defender" | df$role == "defensa" | df$role2 == "defensa", na.rm = TRUE)
      md <- sum(df$role == "Midfielder" | df$role2 == "Midfielder" | df$role == "centrocampista" | df$role2 == "centrocampista", na.rm = TRUE)
      fw <- sum(df$role == "Forward" | df$role2 == "Forward" | df$role == "delantero" | df$role2 == "delantero", na.rm = TRUE)
      total_squad <- nrow(df) # Exact actual player count (distinct players)

      div(
        class = "squad-breakdown-card",
        div(class = "squad-breakdown-title",
            span(icon("users"), " Squad Position Breakdown"),
            span(class = "badge", style = "background-color: #334155; color: #fff; font-size: 11px; padding: 3px 8px;", paste0("Total: ", total_squad, " Players"))
        ),
        div(style = "display: flex; gap: 8px; flex-wrap: wrap; justify-content: space-between; margin-top: 8px;",
            span(class = "badge-gk squad-pos-badge", paste0("Goalkeepers: ", gk)),
            span(class = "badge-df squad-pos-badge", paste0("Defenders: ", df_cnt)),
            span(class = "badge-md squad-pos-badge", paste0("Midfielders: ", md)),
            span(class = "badge-fw squad-pos-badge", paste0("Forwards: ", fw))
        )
      )
    })

    return(selected_player_RV)
  })
}
