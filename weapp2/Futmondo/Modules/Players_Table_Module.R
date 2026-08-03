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
                             filter_by_players_with_bid = FALSE) {
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      width = 12,
      solidHeader = solidHeader,
      status = status,
      title = box_title,
      # enclose table in a div so that is smaller
      div(
        reactableOutput(ns("players_table")),
        style = "overflow-x: auto;font-size:80%; rowHeight: 75%"
      ),
      sidebar = shinydashboardPlus::boxSidebar(
        id = ns("filters_sidebar"),
        icon = shiny::icon("filter"),
        # light-blue in HEX,
        background = "#3c8dbc",
        width = 35,
        # depending on parameters show filters:
        tagList(
          if (filter_by_position) {
            selectInput(inputId = ns("position_filter"), label = "By position", choices = c("All", "Goalkeeper", "Defender", "Midfielder", "Forward"), selected = "All")
          },
          if (filter_by_team) {
            selectInput(
              inputId = ns("team_filter"), label = "By team", choices = c("All") # to be updated in server
            )
          },
          if (filter_by_value) {
            print(paste0("Adding value filter in ", id))
            tagList(
              numericInput(inputId = ns("min_value_filter"), label = "Minimum value (M)", min = 0, max = 1000, value = 0, step = 10),
              numericInput(inputId = ns("max_value_filter"), label = "Maximum value (M)", min = 0, max = 1000, value = 1000, step = 10)
            )
          },
          if (filter_by_change_value) {
            print(paste0("Adding change value filter in ", id))
            numericInput(inputId = ns("change_value_filter"), label = "Minimum change (M)", min = 0, max = 1, value = default_minimum_change_value, step = 0.05)
          },
          if (filter_by_active_clause) {
            checkboxInput(inputId = ns("active_clause_filter"), label = "Only players with active clause", value = FALSE)
          },
          if (filter_by_is_favorite) {
            checkboxInput(inputId = ns("is_favorite_filter"), label = "Only favorite players", value = FALSE)
          },
          if (filter_by_is_from_futmondo) {
            checkboxInput(inputId = ns("is_from_futmondo_filter"), label = "Only free agent players", value = FALSE)
          },
          if (filter_by_players_with_bid) {
            tagList(
              checkboxInput(inputId = ns("players_you_bid_filter"), label = "Only players you bid", value = FALSE),
              checkboxInput(inputId = ns("players_with_bid_filter"), label = "Only players with bid", value = FALSE)
            )
          }
        )
      )
    )
  )
}


players_table_Server <- function(id, players_table_RV, user_teams_RV) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
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
    selected_player_Server(id = "selected_player", selected_player = selected_player_RV)
    # reactives ----
    ## selected_player_RV
    selected_player_RV <- reactive({
      selected_idx <- getReactableState(outputId = "players_table", name = "selected", session = session)
      req(selected_idx)
      selected_player <- players_table_filtered_RV()[selected_idx, ]
    })
    
    ## players_table_filtered_RV ----
    players_table_filtered_RV <- reactive({
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
    
    return(selected_player_RV)
  })
}
