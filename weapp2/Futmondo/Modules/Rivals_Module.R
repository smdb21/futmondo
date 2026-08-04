library(reactable)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)

rivals_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # Plot D: League Squad Value Evolution (Historical values of all teams) - placed ON TOP
    fluidRow(
      column(width = 12,
             box(
               title = "League Squad Value Evolution (Historical Valuation)",
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               plotly::plotlyOutput(ns("team_valuation_history_plot"), height = "300px")
             )
      )
    ),

    # Scouting Target Selection row - placed BELOW Historical Valuation
    fluidRow(
      column(width = 12,
             box(
               title = "Scouting Target Selection",
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               selectInput(
                 inputId = ns("rival_team_select"),
                 label = "Select League Team to Scout:",
                 choices = NULL,
                 width = "100%"
               )
             )
      )
    ),

    # Financial Standings Overview Cards row
    uiOutput(ns("rival_financial_summary_box")),

    # Rival Squad Players table
    players_table_UI(
      id = ns("rival_squad_table"),
      box_title = "Scouted Player Roster",
      filter_by_position = TRUE,
      filter_by_team = FALSE,
      filter_by_value = TRUE,
      filter_by_change_value = TRUE,
      filter_by_active_clause = TRUE,
      filter_by_is_favorite = FALSE,
      filter_by_is_from_futmondo = FALSE
    )
  )
}

rivals_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Observers ----
    
    # Update Rival Team drop-down list from user_teams_RV
    observeEvent(user_teams_RV(), {
      teams <- user_teams_RV()
      req(teams)
      
      # Map team IDs to team names for choices selection
      choices <- setNames(teams$teamid, teams$teamname)
      updateSelectInput(session, "rival_team_select", choices = choices)
    })
    
    # Reactives ----
    
    # Selected rival team ID
    selected_rival_team_id <- reactive({
      input$rival_team_select
    })
    
    # Detailed financial statistics of selected rival
    rival_financial_summary_box_RV <- reactive({
      req(is_module_active() == TRUE)
      req(login_token())
      req(championship_id())
      req(selected_rival_team_id())
      
      info <- get_user_team_info(
        login = login_token(),
        championship_id = championship_id(),
        user_team_id = selected_rival_team_id()
      )
      return(info)
    })
    
    # Scouted Player list of the rival
    rival_players_table_RV <- reactive({
      req(is_module_active() == TRUE)
      req(selected_rival_team_id())
      
      players_table <- get_players_from_team(
        login = login_token(),
        championship_id = championship_id(),
        user_team_id = selected_rival_team_id()
      )
      
      # Return empty data frame gracefully if roster is empty
      if (is.null(players_table) || nrow(players_table) == 0) {
        empty_df <- data.frame(
          id = character(0), name = character(0), role = character(0), role2 = character(0),
          value = numeric(0), change = numeric(0), points = numeric(0), buyPrice = numeric(0),
          clause_price = numeric(0), isClause = logical(0), clause_ratio = numeric(0),
          stringsAsFactors = FALSE
        )
        return(empty_df)
      }
      
      players_table <- players_table %>%
        translate_player_positions() %>%
        calculate_player_changes() %>%
        unify_columns()
      
      # Compute clause-to-valuation ratio defensively (Scout indicator)
      if (nrow(players_table) > 0 && "clause_price" %in% colnames(players_table) && "value" %in% colnames(players_table)) {
        players_table <- players_table %>%
          dplyr::mutate(clause_ratio = clause_price / value)
      } else {
        players_table$clause_ratio <- NA_real_
      }
      
      return(players_table)
    })
    
    # Render Financial Details Summary card rows
    output$rival_financial_summary_box <- renderUI({
      req(is_module_active() == TRUE)
      info <- rival_financial_summary_box_RV()
      req(info)
      
      cash <- format_table_currency(info$budget)
      withheld <- format_table_currency(info$withheld)
      val_sum <- format_table_currency(info$teamValue)
      
      pos <- if (is.null(info$position) || is.na(info$position)) "-" else get_ordinal_position(info$position)
      total_teams <- nrow(user_teams_RV())
      rank_text <- paste0(pos, " of ", total_teams)
      
      fluidRow(
        column(width = 3,
               box(
                 title = "Standings Position",
                 width = 12,
                 status = "primary",
                 solidHeader = TRUE,
                 div(style = "text-align: center; padding: 10px;",
                     h3(style = "font-weight: 700; color: #0f172a; margin: 0; font-size: 20px;", rank_text),
                     p(style = "color: #64748b; font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Rank Position"))
               )
        ),
        column(width = 3,
               box(
                 title = "Liquid Cash Budget",
                 width = 12,
                 status = "success",
                 solidHeader = TRUE,
                 div(style = "text-align: center; padding: 10px;",
                     h3(style = "font-weight: 700; color: #10b981; margin: 0; font-size: 20px;", cash),
                     p(style = "color: #64748b; font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Cash on Hand"))
               )
        ),
        column(width = 3,
               box(
                 title = "Active Bid Funds",
                 width = 12,
                 status = "warning",
                 solidHeader = TRUE,
                 div(style = "text-align: center; padding: 10px;",
                     h3(style = "font-weight: 700; color: #f59e0b; margin: 0; font-size: 20px;", withheld),
                     p(style = "color: #64748b; font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Locked in Pending Bids"))
               )
        ),
        column(width = 3,
               box(
                 title = "Squad Valuation",
                 width = 12,
                 status = "danger",
                 solidHeader = TRUE,
                 div(style = "text-align: center; padding: 10px;",
                     h3(style = "font-weight: 700; color: #ef4444; margin: 0; font-size: 20px;", val_sum),
                     p(style = "color: #64748b; font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Roster Player Values"))
               )
        )
      )
    })
    
    # Plot D: League Squad Value Evolution (Historical valuations of all teams)
    output$team_valuation_history_plot <- plotly::renderPlotly({
      req(is_module_active() == TRUE)

      champ_id <- if (!is.null(championship_id)) championship_id() else NULL
      history_df <- NULL
      if (!is.null(champ_id)) {
        tryCatch({
          history_df <- get_league_standings_history(champ_id)
        }, error = function(e) {
          print(paste0("[Plot D] Valuation history fetch warning: ", e$message))
        })
      }

      # Fallback baseline timeline if DB is empty or unconfigured (pre-season)
      if (is.null(history_df) || nrow(history_df) == 0 || !"team_value" %in% colnames(history_df)) {
        teams <- user_teams_RV()
        if (is.null(teams) || nrow(teams) == 0) {
          teams <- data.frame(teamname = c("Team Alpha", "Team Beta"), teamValue = c(10000000, 8000000), stringsAsFactors = FALSE)
        }

        today <- Sys.time()
        dates <- seq(today - as.difftime(6, units="days"), today, by="1 day")

        history_df <- lapply(1:nrow(teams), function(i) {
          data.frame(
            teamname = teams$teamname[i],
            team_value = rep(as.numeric(if ("teamValue" %in% colnames(teams)) teams$teamValue[i] else 10000000), length(dates)),
            recorded_at = as.character(dates),
            stringsAsFactors = FALSE
          )
        }) %>% bind_rows()
      }

      # Format dates
      history_df$date <- as.POSIXct(history_df$recorded_at, format = "%Y-%m-%dT%H:%M:%S")
      if (any(is.na(history_df$date))) {
        history_df$date <- as.POSIXct(history_df$recorded_at)
      }

      history_df <- history_df %>% dplyr::arrange(date)

      # Render multi-line spline squad valuations progression
      plotly::plot_ly(data = history_df, x = ~date, y = ~team_value, color = ~teamname, colors = "Set3", type = "scatter", mode = "lines+markers",
                      line = list(width = 2, shape = "spline"),
                      marker = list(size = 5),
                      hoverinfo = "text",
                      text = ~paste0("Team: ", teamname, "<br>Date: ", format(date, "%d-%m-%y"), "<br>Valuation: ", format_table_currency(team_value))) %>%
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "", gridcolor = "#f1f5f9", zeroline = FALSE, tickformat = "%d-%m"),
          yaxis = list(title = "Squad Valuation (EUR)", gridcolor = "#f1f5f9", zeroline = FALSE, tickformat = "s"),
          legend = list(orientation = "h", x = 0.5, y = -0.25, xanchor = "center"),
          margin = list(l = 60, r = 20, t = 10, b = 40)
        )
    })

    # Nested table module server call
    selected_player_RV <- players_table_Server(
      id = "rival_squad_table",
      players_table_RV = rival_players_table_RV,
      user_teams_RV = user_teams_RV,
      login_token = login_token,
      championship_id = championship_id,
      user_team_id = user_team_id # Passed logged-in team ID so buying executes on your behalf
    )
    
    return(selected_player_RV)
  })
}