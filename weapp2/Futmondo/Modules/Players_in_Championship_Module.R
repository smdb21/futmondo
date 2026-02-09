library(reactable)

players_in_championship_UI <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      width = 12,
      solidHeader = TRUE,
      # enclose table in a div so that is smaller
      div(
        reactableOutput(ns("players_table")),
        style = "overflow-x: auto;font-size:80%; rowHeight: 75%"
      )
    )
  )
}


players_in_championship_Server <- function(id, login_token, championship_id) {
  moduleServer(id, function(input, output, session) {
    # observers ----

    # reactives ----

    ## selected_player_RV
    selected_player_RV <- reactive({
      selected_idx <- getReactableState(outputId = "players_table", name = "selected", session = session)
      req(selected_idx)
      selected_player <- players_table_RV()[selected_idx, ]
    })

    ## players_table_RV ----
    players_table_RV <- reactive({
      req(login_token())
      req(championship_id())
      championship_id <- championship_id()
      players_table <- get_championship_players(
        login = login_token(),
        championship_id = championship_id
      )
      players_table <- players_table %>%
        translate_player_positions()
      players_table <- players_table %>%
        calculate_player_changes()

      return(players_table)
    })

    # renders ----
    ## render players_table ----
    output$players_table <- renderReactable({
      req(players_table_RV())
      players_table <- players_table_RV()

      print(paste0(colnames(players_table)))
      # browser()
      
      players_table <- players_table %>%
        dplyr::select(name, role, role2, points, value, status, #team, 
                      rating, change, any_of(starts_with("change")), #average, 
                      #total, 
                      any_of(starts_with("market_")), any_of(starts_with("bid_price")), any_of(starts_with("clause_")))

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
