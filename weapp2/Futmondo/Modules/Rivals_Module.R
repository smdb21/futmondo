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

# League Financial Standings Overview & Scouting Target Selection
    fluidRow(
      column(width = 12,
             box(
               title = "League Financial Standings & Scouting Target Selection",
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               p(style = "color: #64748b; font-size: 13px; font-weight: 500; margin-bottom: 12px;",
                 icon("hand-pointer"), " Select a user team from the table below to scout their squad and financial details."),
               reactable::reactableOutput(ns("league_finances_table"))
             )
      )
    ),

    # Plot E: League Buying Power (Liquid Cash Standings)
    fluidRow(
      column(width = 12,
             box(
               title = "League Buying Power (Liquid Cash Standings)",
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               plotly::plotlyOutput(ns("league_finances_plot"), height = "300px")
             )
      )
    ),

    # Scouted Rival Details (Summary cards + Player Roster Table)
    uiOutput(ns("scouted_rival_details_ui"))
  )
}

rivals_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Observers ----

    # Reactives ----

    # Selected rival team ID derived from table selection
    selected_rival_team_id <- reactive({
      finances_data <- league_finances_RV()
      req(finances_data, finances_data$team_finances)
      df <- finances_data$team_finances
      if (is.null(df) || nrow(df) == 0) return(NULL)

      # Read selected row index from reactable state
      selected_idx <- getReactableState("league_finances_table", "selected", session = session)

      if (!is.null(selected_idx) && is.numeric(selected_idx) && selected_idx >= 1 && selected_idx <= nrow(df)) {
        return(as.character(df$teamid[selected_idx]))
      } else {
        return(NULL)
      }
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
    
    # League Finances reactive
    league_finances_RV <- reactive({
      req(is_module_active() == TRUE)
      req(login_token())
      req(championship_id())
      teams <- user_teams_RV()
      req(teams)

      finances <- calculate_league_finances(
        login = login_token(),
        championship_id = championship_id(),
        user_teams_df = teams,
        initial_budget = 300000000
      )
      return(finances)
    })

    # Render League Financial Standings Table
    output$league_finances_table <- reactable::renderReactable({
      req(is_module_active() == TRUE)
      finances_data <- league_finances_RV()
      req(finances_data)
      df <- finances_data$team_finances
      req(df)
      if (nrow(df) == 0) return(NULL)

      df_display <- df %>%
        dplyr::select(
          Team = teamname,
          `Initial Budget` = initial_budget,
          `Spent on Clauses` = total_spent,
          `Money Left` = budget,
          `Squad Value` = team_value,
          `Net Profit/Loss` = net_profit_loss,
          `Squad Size` = squad_size,
          Points = points
        )

      reactable::reactable(
        df_display,
        compact = TRUE,
        striped = TRUE,
        highlight = TRUE,
        bordered = FALSE,
        selection = "single",
        onClick = "select",
        defaultPageSize = 10,
        columns = list(
          Team = colDef(name = "User Team", align = "left", style = list(fontWeight = "600", color = "#0f172a")),
          `Initial Budget` = colDef(align = "right", cell = function(val) format_table_currency(val)),
          `Spent on Clauses` = colDef(align = "right", cell = function(val) format_table_currency(val)),
          `Money Left` = colDef(
            align = "right",
            cell = function(val) {
              formatted <- format_table_currency(val)
              color_style <- if (val >= 0) "color: #10b981; font-weight: 700;" else "color: #ef4444; font-weight: 700;"
              shiny::tags$span(style = color_style, formatted)
            }
          ),
          `Squad Value` = colDef(align = "right", cell = function(val) format_table_currency(val)),
          `Net Profit/Loss` = colDef(
            align = "right",
            cell = function(val) {
              if (is.na(val) || !is.numeric(val)) return("")
              sign_pfx <- if (val > 0) "+" else ""
              color_style <- if (val > 0) "color: #10b981; font-weight: 600;" else if (val < 0) "color: #ef4444; font-weight: 600;" else "color: #64748b;"
              formatted <- paste0(sign_pfx, format_table_currency(val))
              shiny::tags$span(style = color_style, formatted)
            }
          ),
          `Squad Size` = colDef(align = "center"),
          Points = colDef(align = "center", style = list(fontWeight = "600"))
        )
      )
    })

    # Render Financial Details Summary card rows for selected rival
    output$rival_financial_summary_box <- renderUI({
      req(is_module_active() == TRUE)
      info <- rival_financial_summary_box_RV()
      roster <- rival_players_table_RV()

      # Compute total spent and squad value from roster
      total_spent <- 0
      squad_val <- 0
      if (!is.null(roster) && nrow(roster) > 0) {
        if ("buyPrice" %in% colnames(roster)) {
          total_spent <- sum(suppressWarnings(as.numeric(roster$buyPrice)), na.rm = TRUE)
        }
        if ("value" %in% colnames(roster)) {
          squad_val <- sum(suppressWarnings(as.numeric(roster$value)), na.rm = TRUE)
        }
      }

      # Determine budget (Money Left)
      budget_val <- 300000000 - total_spent
      if (!is.null(info) && !is.null(info$budget) && is.numeric(info$budget) && info$budget > 0) {
        budget_val <- info$budget
      }
      if (!is.null(info) && !is.null(info$teamValue) && is.numeric(info$teamValue) && info$teamValue > 0) {
        squad_val <- info$teamValue
      }

      cash <- format_table_currency(budget_val)
      spent_fmt <- format_table_currency(total_spent)
      val_sum <- format_table_currency(squad_val)
      net_gain <- squad_val - total_spent
      net_fmt <- format_table_currency(net_gain)

      pos <- if (!is.null(info) && !is.null(info$position) && !is.na(info$position)) get_ordinal_position(info$position) else "-"
      total_teams <- if (!is.null(user_teams_RV())) nrow(user_teams_RV()) else 0
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
                 title = "Money Left (Budget)",
                 width = 12,
                 status = "success",
                 solidHeader = TRUE,
                 div(style = "text-align: center; padding: 10px;",
                     h3(style = "font-weight: 700; color: #10b981; margin: 0; font-size: 20px;", cash),
                     p(style = "color: #64748b; font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Calculated Funds Left"))
               )
        ),
        column(width = 3,
               box(
                 title = "Spent on Clauses",
                 width = 12,
                 status = "warning",
                 solidHeader = TRUE,
                 div(style = "text-align: center; padding: 10px;",
                     h3(style = "font-weight: 700; color: #f59e0b; margin: 0; font-size: 20px;", spent_fmt),
                     p(style = "color: #64748b; font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Roster Clauses Cost"))
               )
        ),
        column(width = 3,
               box(
                 title = "Squad Valuation & Gain",
                 width = 12,
                 status = "danger",
                 solidHeader = TRUE,
                 div(style = "text-align: center; padding: 10px;",
                     h3(style = "font-weight: 700; color: #ef4444; margin: 0; font-size: 20px;", val_sum),
                     p(style = "color: #64748b; font-size: 11px; text-transform: uppercase; margin-top: 5px;", paste0("Net Gain: ", net_fmt)))
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

    # Plot E: League Finances horizontal bar chart
    output$league_finances_plot <- plotly::renderPlotly({
      req(is_module_active() == TRUE)
      req(login_token(), championship_id(), user_teams_RV())

      champ_id <- championship_id()
      login <- login_token()
      teams <- user_teams_RV()

      finances_res <- tryCatch({
        calculate_league_finances(
          login = login,
          championship_id = champ_id,
          user_teams_df = teams,
          initial_budget = 300000000
        )
      }, error = function(e) {
        print(paste0("[Rivals] Finances calculation warning: ", e$message))
        NULL
      })

      teams_df <- if (!is.null(finances_res) && "team_finances" %in% names(finances_res)) finances_res$team_finances else NULL

      if (is.null(teams_df) || nrow(teams_df) == 0) {
        teams_df <- tryCatch(get_user_teams_finances(champ_id), error = function(e) NULL)
      }
      if (is.null(teams_df) || nrow(teams_df) == 0) {
        teams_df <- teams
      }

      if (is.null(teams_df) || nrow(teams_df) == 0) return(NULL)

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

    # Render Scouted Rival Details (or empty state prompt if no selection)
    output$scouted_rival_details_ui <- renderUI({
      req(is_module_active() == TRUE)
      rival_id <- selected_rival_team_id()

      if (is.null(rival_id) || rival_id == "") {
        div(
          style = "text-align: center; padding: 40px 20px; background: #f8fafc; border: 2px dashed #cbd5e1; border-radius: 12px; margin-top: 20px; margin-bottom: 25px;",
          shiny::tags$i(class = "fa-solid fa-user-ninja", style = "font-size: 36px; color: #94a3b8; margin-bottom: 12px;"),
          h4(style = "font-weight: 700; color: #334155; margin: 0 0 6px 0;", "No User Team Selected"),
          p(style = "color: #64748b; font-size: 14px; margin: 0;",
            icon("hand-pointer", style = "color: #3b82f6; margin-right: 4px;"),
            "Click on any user row in the table above to scout their squad roster and financial details."
          )
        )
      } else {
        tagList(
          uiOutput(ns("rival_financial_summary_box")),
          players_table_UI(
            id = ns("rival_squad_table"),
            box_title = "Scouted Player Roster & Purchase Breakdown",
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