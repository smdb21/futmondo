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

    # Safe date parsing helper
    parse_safe_datetime <- function(date_vec) {
      if (is.null(date_vec) || length(date_vec) == 0) return(as.POSIXct(character(0)))
      date_str <- as.character(date_vec)

      # Clean ISO 8601 formatting (e.g., "2024-03-15T10:30:00.000Z" -> "2024-03-15 10:30:00")
      clean_str <- gsub("T", " ", date_str)
      clean_str <- gsub("Z", "", clean_str)
      clean_str <- gsub("\\..*", "", clean_str) # strip fractional seconds

      parsed <- suppressWarnings(as.POSIXct(clean_str, format = "%Y-%m-%d %H:%M:%S"))

      # Fallback for plain YYYY-MM-DD
      na_idx <- is.na(parsed)
      if (any(na_idx)) {
        parsed[na_idx] <- suppressWarnings(as.POSIXct(clean_str[na_idx], format = "%Y-%m-%d"))
      }

      # Fallback for any remaining NAs
      na_idx <- is.na(parsed)
      if (any(na_idx)) {
        parsed[na_idx] <- Sys.time()
      }

      return(parsed)
    }

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
          `Squad Investment` = total_spent,
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
          `Squad Investment` = colDef(align = "right", cell = function(val) format_table_currency(val)),
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

      # Retrieve transaction movements safely
      tx_raw <- tryCatch({ rival_moneymovements_raw_RV() }, error = function(e) NULL)

      # Get pressroom-driven totals from league_finances_RV for the selected rival
      money_out <- total_spent
      money_in <- 0
      rival_id <- selected_rival_team_id()
      if (!is.null(rival_id)) {
        finances_data <- tryCatch({ league_finances_RV() }, error = function(e) NULL)
        if (!is.null(finances_data) && !is.null(finances_data$team_finances) && nrow(finances_data$team_finances) > 0) {
          rival_row <- finances_data$team_finances[finances_data$team_finances$teamid == rival_id, ]
          if (nrow(rival_row) > 0) {
            money_out <- if ("total_spent" %in% colnames(rival_row)) as.numeric(rival_row$total_spent[1]) else money_out
            money_in <- if ("total_sales" %in% colnames(rival_row)) as.numeric(rival_row$total_sales[1]) else 0
            if ("budget" %in% colnames(rival_row) && is.numeric(rival_row$budget[1]) && !is.na(rival_row$budget[1])) {
              budget_val <- as.numeric(rival_row$budget[1])
            }
          }
        }
      }

      # Fallback: if league_finances did not provide pressroom data, use raw moneymovements
      if (money_out == 0 || money_in == 0) {
        if (!is.null(tx_raw) && nrow(tx_raw) > 0) {
          if (money_out == 0) {
            money_out <- sum(abs(tx_raw$money[tx_raw$money < 0 | tx_raw$type == "buy"]), na.rm = TRUE)
          }
          if (money_in == 0) {
            money_in <- sum(tx_raw$money[tx_raw$type == "sell" | (tx_raw$money > 0 & tx_raw$type != "budget")], na.rm = TRUE)
          }
        }
      }

      cash <- format_table_currency(budget_val)
      spent_fmt <- format_table_currency(total_spent)
      money_out_fmt <- format_table_currency(money_out)
      money_in_fmt <- format_table_currency(money_in)
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
                   title = "Squad Investment",
                   width = 12,
                   status = "warning",
                   solidHeader = TRUE,
                   div(style = "text-align: center; padding: 10px;",
                       h3(style = "font-weight: 700; color: #f59e0b; margin: 0; font-size: 20px;", money_out_fmt),
                       p(style = "color: #64748b; font-size: 11px; text-transform: uppercase; margin-top: 5px;",
                         tags$span(style = "color: #ef4444; font-weight: 600;", paste0("Out: ", money_out_fmt)),
                         tags$span(style = "color: #94a3b8;", " \u2022 "),
                         tags$span(style = "color: #10b981; font-weight: 600;", paste0("In: ", money_in_fmt)))
                       )
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
          tabsetPanel(id = ns("rival_tabs"), type = "pills",
            tabPanel(title = "Player Roster & Clauses", icon = icon("users"),
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
            ),
            tabPanel(title = "Transaction & Financial History", icon = icon("receipt"),
              uiOutput(ns("rival_transactions_tab_ui"))
            )
          )
        )
      }
    })

    # ---- Transaction History Reactives ----

    # Raw money movements: fetch via API, compute running balance, build fallback if empty
    is_fallback <- reactiveVal(FALSE)

    rival_moneymovements_raw_RV <- reactive({
      req(is_module_active() == TRUE)
      req(login_token())
      req(championship_id())
      req(selected_rival_team_id())

      movements <- tryCatch({
        get_user_team_moneymovements(
          login = login_token(),
          championship_id = championship_id(),
          user_team_id = selected_rival_team_id()
        )
      }, error = function(e) {
        print(paste0("[Rivals] moneymovements fetch error: ", e$message))
        NULL
      })

      # Check if API returned empty or errored
      if (is.null(movements) || nrow(movements) == 0) {
        is_fallback(TRUE)

        # Try pressroom feed as a first fallback
        rival_id <- selected_rival_team_id()
        pressroom_df <- tryCatch({
          get_championship_pressroom(login = login_token(), championship_id = championship_id())
        }, error = function(e) NULL)

        if (!is.null(pressroom_df) && nrow(pressroom_df) > 0 && !is.null(rival_id) && rival_id != "") {
          rival_tx <- pressroom_df[pressroom_df$buyer_team_id == rival_id | pressroom_df$seller_team_id == rival_id, ]
          if (nrow(rival_tx) > 0) {
            fallback_df <- lapply(seq_len(nrow(rival_tx)), function(idx) {
              row <- rival_tx[idx, ]
              is_buy <- (as.character(row$buyer_team_id) == as.character(rival_id))
              tx_type <- if (is_buy) "buy" else "sell"
              tx_money <- if (is_buy) -as.numeric(row$price) else as.numeric(row$price)
              p_name <- if (!is.null(row$player_name) && as.character(row$player_name) != "") as.character(row$player_name) else "Player"
              data.frame(
                id = paste0("pressroom_", row$id),
                concept = paste0(p_name, if (is_buy) " (Purchased)" else " (Sold)"),
                type = tx_type,
                category = "market",
                money = tx_money,
                date = as.character(row$created),
                stringsAsFactors = FALSE
              )
            }) %>% bind_rows()

            fallback_df$timestamp <- parse_safe_datetime(fallback_df$date)
            fallback_df <- fallback_df %>% dplyr::arrange(timestamp)
            fallback_df$running_balance <- 300000000 + cumsum(fallback_df$money)
            fallback_df$timestamp <- NULL

            return(fallback_df)
          }
        }

        # Pressroom had no items for this rival -- fall back to current squad roster buyPrice
        roster <- rival_players_table_RV()
        if (is.null(roster) || nrow(roster) == 0) {
          return(data.frame(
            id = character(0), concept = character(0), type = character(0),
            category = character(0), money = numeric(0), date = character(0),
            running_balance = numeric(0), stringsAsFactors = FALSE
          ))
        }

        # Each player with buyPrice > 0 becomes a purchase transaction
        fallback_df <- roster %>%
          dplyr::filter(!is.na(buyPrice) & buyPrice > 0) %>%
          dplyr::select(buyPrice, name) %>%
          dplyr::mutate(
            id = paste0("fallback_buy_", seq_len(n())),
            concept = name,
            type = "buy",
            category = "market",
            money = -buyPrice,
            date = as.character(Sys.time())
          ) %>%
          dplyr::select(id, concept, type, category, money, date)

        if (nrow(fallback_df) == 0) {
          return(data.frame(
            id = character(0), concept = character(0), type = character(0),
            category = character(0), money = numeric(0), date = character(0),
            running_balance = numeric(0), stringsAsFactors = FALSE
          ))
        }

        # Sort ascending by timestamp (chronological) and compute running balance
        fallback_df <- fallback_df %>%
          dplyr::arrange(date) %>%
          dplyr::mutate(running_balance = cumsum(money))

        return(fallback_df)
      }

      is_fallback(FALSE)

      # Parse ISO dates
      movements$timestamp <- parse_safe_datetime(movements$date)

      # Sort ascending by timestamp (chronological) for running balance calculation
      movements <- movements %>%
        dplyr::arrange(timestamp)

      # Calculate running balance on the FULL chronological dataset
      movements$running_balance <- cumsum(movements$money)

      # Drop the helper column before returning
      movements$timestamp <- NULL

      return(movements)
    })

    # Filtered money movements: apply date range, type, and category filters
    rival_moneymovements_filtered_RV <- reactive({
      raw <- rival_moneymovements_raw_RV()
      if (is.null(raw) || nrow(raw) == 0) return(raw)

      filtered <- raw

      # Parse dates for filtering
      filtered$timestamp <- parse_safe_datetime(filtered$date)

      # Date range filter
      date_range <- input$tx_date_range
      if (!is.null(date_range) && length(date_range) == 2 && !is.na(date_range[1]) && !is.na(date_range[2])) {
        start_dt <- parse_safe_datetime(paste0(as.character(date_range[1]), " 00:00:00"))
        end_dt <- parse_safe_datetime(paste0(as.character(date_range[2]), " 23:59:59"))
        filtered <- filtered %>%
          dplyr::filter(!is.na(timestamp) & timestamp >= start_dt & timestamp <= end_dt)
      }

      # Type filter
      type_filter <- input$tx_type_filter
      if (!is.null(type_filter) && type_filter != "All") {
        filtered <- filtered %>%
          dplyr::filter(type == type_filter)
      }

      # Category filter
      category_filter <- input$tx_category_filter
      if (!is.null(category_filter) && category_filter != "All") {
        filtered <- filtered %>%
          dplyr::filter(category == category_filter)
      }

      # Drop helper column
      filtered$timestamp <- NULL

      # Sort descending by date for display (newest first)
      filtered <- filtered %>%
        dplyr::arrange(desc(date))

      return(filtered)
    })

    # Reset filters observer
    observeEvent(input$tx_reset_filters, {
      updateDateRangeInput(session, "tx_date_range", start = NULL, end = NULL)
      updateSelectInput(session, "tx_type_filter", selected = "All")
      updateSelectInput(session, "tx_category_filter", selected = "All")
    })

    # Render Tab 2 UI: Transaction History
    output$rival_transactions_tab_ui <- renderUI({
      is_fb <- is_fallback()

      # Compute period summary from filtered data
      filtered_tx <- rival_moneymovements_filtered_RV()
      total_inflow <- 0
      total_outflow <- 0
      net_flow <- 0
      if (!is.null(filtered_tx) && nrow(filtered_tx) > 0) {
        total_inflow <- sum(filtered_tx$money[filtered_tx$money > 0], na.rm = TRUE)
        total_outflow <- sum(filtered_tx$money[filtered_tx$money < 0], na.rm = TRUE)
        net_flow <- sum(filtered_tx$money, na.rm = TRUE)
      }

      # Build fallback callout
      callout_banner <- NULL
      if (is_fb) {
        callout_banner <- div(
          style = "background: #fff7ed; border: 1px solid #fbbf24; border-left: 4px solid #f59e0b; border-radius: 6px; padding: 12px 16px; margin-bottom: 16px; font-size: 13px; color: #92400e;",
          icon("info-circle", style = "color: #f59e0b; margin-right: 8px; vertical-align: middle;"),
          "Information restricted by Futmondo: The Futmondo API restricts direct access to private financial transactions for rival teams. The transactions shown below have been calculated from current squad purchases."
        )
      }

      # Build period summary cards
      net_flow_status <- if (net_flow >= 0) "primary" else "warning"
      net_flow_color <- if (net_flow >= 0) "#3b82f6" else "#f59e0b"

      period_summary_cards <- fluidRow(
        column(width = 4,
          box(
            title = "Total Inflow",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            div(style = "text-align: center; padding: 10px;",
              h3(style = "font-weight: 700; color: #10b981; margin: 0; font-size: 18px;",
                format_table_currency(total_inflow)
              ),
              p(style = "color: #64748b; font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Inflow in Period")
            )
          )
        ),
        column(width = 4,
          box(
            title = "Total Outflow",
            width = 12,
            status = "danger",
            solidHeader = TRUE,
            div(style = "text-align: center; padding: 10px;",
              h3(style = "font-weight: 700; color: #ef4444; margin: 0; font-size: 18px;",
                format_table_currency(total_outflow)
              ),
              p(style = "color: #64748b; font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Outflow in Period")
            )
          )
        ),
        column(width = 4,
          box(
            title = "Net Cash Flow",
            width = 12,
            status = net_flow_status,
            solidHeader = TRUE,
            div(style = "text-align: center; padding: 10px;",
              h3(style = paste0("font-weight: 700; color: ", net_flow_color, "; margin: 0; font-size: 18px;"),
                format_table_currency(net_flow)
              ),
              p(style = "color: #64748b; font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Net Cash Flow")
            )
          )
        )
      )

      tagList(
        # Fallback callout banner
        callout_banner,

        # Filter Bar
        fluidRow(
          style = "background: #f8fafc; padding: 15px; border-radius: 8px; margin-bottom: 16px; border: 1px solid #e2e8f0;",
          column(width = 4,
            dateRangeInput(
              ns("tx_date_range"),
              "Date Range",
              start = NULL,
              end = NULL,
              format = "dd/mm/yyyy",
              language = "en",
              width = "100%"
            )
          ),
          column(width = 3,
            selectInput(
              ns("tx_type_filter"),
              "Transaction Type",
              choices = c("All" = "All", "Purchases" = "buy", "Sales" = "sell", "Bonuses / Rewards" = "bonus", "Initial Budget" = "budget"),
              selected = "All",
              width = "100%"
            )
          ),
          column(width = 3,
            selectInput(
              ns("tx_category_filter"),
              "Category",
              choices = c("All" = "All", "Market" = "market", "Rounds" = "round", "Bonuses" = "bonus"),
              selected = "All",
              width = "100%"
            )
          ),
          column(width = 2,
            actionButton(
              ns("tx_reset_filters"),
              "Reset Filters",
              icon = icon("rotate-left"),
              style = "margin-top: 24px;"
            )
          )
        ),

        # Period Summary Cards
        period_summary_cards,

        # Transaction Table
        reactable::reactableOutput(ns("rival_transactions_table"))
      )
    })

    # Render Transaction Table
    output$rival_transactions_table <- reactable::renderReactable({
      filtered <- rival_moneymovements_filtered_RV()
      if (is.null(filtered) || nrow(filtered) == 0) {
        return(reactable::reactable(
          data.frame(Status = "No transaction movements recorded for this team yet."),
          columns = list(Status = colDef(name = "Transaction Log Status", align = "center")),
          compact = TRUE,
          bordered = FALSE
        ))
      }

      reactable::reactable(
        filtered,
        compact = TRUE,
        striped = TRUE,
        highlight = TRUE,
        bordered = FALSE,
        defaultPageSize = 15,
        columns = list(
          date = colDef(
            name = "Date",
            align = "left",
            cell = function(date_val) {
              ts <- parse_safe_datetime(date_val)
              format(ts, "%d/%m/%Y %H:%M")
            }
          ),
          type = colDef(
            name = "Type / Category",
            align = "center",
            cell = function(type_val, category_val) {
              badge_color <- "#64748b"
              badge_label <- type_val
              if (type_val == "buy") {
                badge_color <- "#ef4444"
                badge_label <- "Buy"
              } else if (type_val == "sell") {
                badge_color <- "#10b981"
                badge_label <- "Sell"
              } else if (type_val == "bonus") {
                badge_color <- "#3b82f6"
                badge_label <- "Bonus"
              } else if (type_val == "budget") {
                badge_color <- "#64748b"
                badge_label <- "Budget"
              }
              shiny::tags$span(
                style = paste0("background: ", badge_color, "; color: #fff; padding: 2px 8px; border-radius: 4px; font-size: 11px; font-weight: 600;"),
                badge_label
              )
            }
          ),
          concept = colDef(
            name = "Concept / Description",
            align = "left",
            style = list(fontWeight = "500")
          ),
          money = colDef(
            name = "Amount",
            align = "right",
            cell = function(money_val) {
              formatted <- format_table_currency(money_val)
              if (money_val > 0) {
                shiny::tags$span(style = "color: #10b981; font-weight: 600;", paste0("+", formatted))
              } else if (money_val < 0) {
                shiny::tags$span(style = "color: #ef4444; font-weight: 600;", formatted)
              } else {
                shiny::tags$span(style = "color: #64748b;", formatted)
              }
            }
          ),
          running_balance = colDef(
            name = "Money Left After Transaction",
            align = "right",
            cell = function(rb_val) {
              formatted <- format_table_currency(rb_val)
              shiny::tags$span(style = "font-weight: 700; color: #0f172a;", formatted)
            }
          )
        )
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