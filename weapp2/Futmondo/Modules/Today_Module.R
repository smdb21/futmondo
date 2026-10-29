# Factual overview; actions use the shared manual player-card flow.
today_UI <- function(id) {
  ns <- NS(id)
  tagList(h2("Today"), p("Current league balances, market listings and completed transfers."),
    fluidRow(uiOutput(ns("kpi_cash_box")), uiOutput(ns("kpi_valuation_box"))),
    players_table_UI(ns("market_players"), box_title="Current market listings", filter_by_is_from_futmondo=TRUE),
    h3("Recent transfers"), tableOutput(ns("recent_transfers")))
}
today_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger=NULL) {
  moduleServer(id, function(input, output, session) {
    bind_factual_reads(c("get_financial_snapshot","get_market_players","get_championship_pressroom"),session,login_token,championship_id,user_team_id,
      is_active=is_module_active,refresh_trigger=refresh_trigger)

    finance <- reactive({
      req(is_module_active(), valid_login(login_token()), championship_id(), user_team_id())
      if (is.function(refresh_trigger)) refresh_trigger()
      get_financial_snapshot(login_token(), championship_id(), user_team_id())
    })
    market <- reactive({
      req(is_module_active(), valid_login(login_token()), championship_id(), user_team_id())
      if (is.function(refresh_trigger)) refresh_trigger()
      rows <- get_market_players(login_token(), championship_id(), user_team_id())
      if (is.data.frame(rows) && nrow(rows)) rows <- unify_columns(calculate_player_changes(translate_player_positions(rows)))
      rows
    })
    output$kpi_cash_box <- renderUI({
      req(is_module_active()); snapshot <- finance()
      shinydashboard::valueBox(player_card_money(snapshot$cash), "Current balance", icon=icon("wallet"), color="green", width=6)
    })
    output$kpi_valuation_box <- renderUI({
      req(is_module_active())
      amount <- fm_number(finance()$team_value)
      shinydashboard::valueBox(player_card_money(amount), "Observed team value", icon=icon("users"), color="blue", width=6)
    })
    output$recent_transfers <- renderTable({
      req(is_module_active(), valid_login(login_token()))
      if (is.function(refresh_trigger)) refresh_trigger()
      rows <- get_championship_pressroom(login_token(), championship_id())
      if (!is.data.frame(rows) || !nrow(rows)) return(data.frame(Status="No completed transfers available."))
      if ("created" %in% names(rows)) rows <- rows[order(rows$created,decreasing=TRUE),,drop=FALSE]
      head(rows[,intersect(c("created","player_name","price","buyer_team_id","seller_team_id"),names(rows)),drop=FALSE],20L)
    })
    players_table_Server("market_players", market, user_teams_RV, login_token, championship_id,
      user_team_id, refresh_trigger=refresh_trigger, is_module_active=is_module_active)
  })
}
