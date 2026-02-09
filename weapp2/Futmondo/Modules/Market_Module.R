market_UI <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      title = "Players in market",
      width = 12,
      div(
        reactableOutput(outputId = ns("market_players_table")),
        style = "overflow-x: auto;font-size:80%; rowHeight: 75%"
      ),
      sidebar = shinydashboardPlus::boxSidebar(
        id = "filters_sidebar",
        icon = shiny::icon("filter"),
        # light-blue in HEX,
        background = "#3c8dbc", 
        width = 35,
        selectInput(inputId = ns("market_player_position_filter"), label = "By position", choices = c("All", "Goalkeeper", "Defender", "Midfielder", "Forward"), selected = "All"),
        # filter by userTeam
        selectInput(
          inputId = ns("market_player_team_filter"), label = "By team", choices = c("All") # to be updated in server
        )
      )
    )
  )
}
market_Server <- function(id, login_token, championship_id, user_team_id) {
  moduleServer(
    id,
    function(input, output, session) {
      # reactives ----
      ## market_players_RV ----
      market_players_RV <- reactive({
        market_players <- get_market_players(login = login_token(), championship_id = championship_id(), user_team_id = user_team_id())
        market_players <- market_players %>%
          translate_player_positions()
        market_players <- market_players %>%
          calculate_player_changes()
        return(market_players)
      })
      ## market_players_filtered_RV ----
      market_players_filtered_RV <- reactive({
        market_players <- market_players_RV()
        req(market_players)
        if (input$market_player_position_filter != "All") {
          market_players <- market_players %>%
            dplyr::filter(role == input$market_player_position_filter | role2 == input$market_player_position_filter)
        }
        return(market_players)
      })
      # renders ----
      ## render market_players_table
      output$market_players_table <- renderReactable({
        market_players <- market_players_filtered_RV()
        req(market_players)
        market_players <- market_players %>%
          dplyr::select(name, role, role2, points, value, status, any_of(starts_with("change")), any_of(starts_with("average")), numberOfBids, userTeam, team)
        table_columns <- get_reactable_columns_for_players(market_players)
        reactable(market_players,
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
          onClick = "select"
        )
      })
    }
  )
}
