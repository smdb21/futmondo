library(reactable)

players_in_teams_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("team_value_box")),
    tabsetPanel(
      id = ns("squad_sub_tabs"),
      type = "pills",
      # ================================================================
      # TAB 1: Squad Roster
      # ================================================================
      tabPanel(
        "Squad Roster",
        icon = icon("users"),
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
          show_position_breakdown = TRUE,
          hide_bid_column = FALSE
        ),
        uiOutput(ns("charts_row")) # Dynamic Plot B & Plot C Container
      ),
      # ================================================================
      # TAB 2: Lineup Optimizer
      # ================================================================
      tabPanel(
        "Lineup Optimizer",
        icon = icon("chess"),
        # Control bar row
        fluidRow(
          column(
            width = 6,
            selectInput(
              ns("opt_formation"),
              "Tactical Formation",
              choices = c("4-3-3", "4-4-2", "3-5-2", "3-4-3", "4-5-1", "5-3-2", "5-4-1"),
              selected = "4-3-3"
            )
          ),
          column(
            width = 6,
            selectInput(
              ns("opt_mode"),
              "Strategy Mode",
              choices = c(
                "Max FIS (Balanced)" = "max_fis",
                "Safe XI (Floor & Consistency)" = "safe",
                "Upside XI (Ceiling & xG)" = "upside",
                "Form XI (Hot Streak)" = "form",
                "Fixture XI (Easy Matchups)" = "fixture"
              ),
              selected = "max_fis"
            )
          )
        ),
        # Optimizer KPI summary row
        uiOutput(ns("optimizer_kpi_row")),
        # Soccer Pitch container
        uiOutput(ns("soccer_pitch_ui")),
        # Starting XI and Bench tables in collapsible panels
        box(
          title = "Starting XI",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          reactableOutput(ns("starting_xi_table"))
        ),
        box(
          title = "Bench",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          reactableOutput(ns("bench_table"))
        )
      ),
      # ================================================================
      # TAB 3: Transfer Sandbox
      # ================================================================
      tabPanel(
        "Transfer Sandbox",
        icon = icon("calculator"),
        # Top Scenario KPI summary
        uiOutput(ns("sandbox_kpi_row")),
        fluidRow(
          # Left column
          column(
            width = 6,
            box(
              title = "Scenario Builder",
              width = 12,
              status = "warning",
              solidHeader = TRUE,
              collapsible = FALSE,
              selectizeInput(
                ns("sandbox_sells"),
                "Select Players to Sell",
                choices = NULL,
                multiple = TRUE,
                options = list(maxItems = 5, placeholder = "Choose squad players to sell...")
              ),
              selectizeInput(
                ns("sandbox_buys"),
                "Select Market Targets to Buy",
                choices = NULL,
                multiple = TRUE,
                options = list(maxItems = 5, placeholder = "Choose market players to buy...")
              ),
              div(
                style = "margin-top: 10px;",
                actionButton(ns("sandbox_reset"), "Reset Sandbox", class = "btn btn-default")
              ),
              # Recommended Swaps feed
              uiOutput(ns("sandbox_recommendations_ui"))
            )
          ),
          # Right column
          column(
            width = 6,
            box(
              title = "Projected Squad Preview",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              collapsible = FALSE,
              reactableOutput(ns("sandbox_projected_table"))
            )
          )
        )
      )
    )
  )
}


players_in_teams_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- helpers ----
    get_reactive_val <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.reactive(x) || is.function(x)) {
        tryCatch(x(), error = function(e) NULL)
      } else {
        x
      }
    }

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

      # Ensure market_inMarket column is always present
      if (!"market_inMarket" %in% colnames(players_table)) {
        players_table$market_inMarket <- FALSE
      }
      # Check which roster players are currently listed on the market
      my_mkt_df <- tryCatch({
        get_market_players(
          login = login_token(),
          championship_id = championship_id,
          user_team_id = user_team_id
        )
      }, error = function(e) {
        print(paste0("[Market check] Fetch warning: ", e$message))
        NULL
      })
      if (!is.null(my_mkt_df) && nrow(my_mkt_df) > 0 && "id" %in% colnames(my_mkt_df)) {
        mkt_ids <- as.character(my_mkt_df$id)
        players_table$market_inMarket <- as.character(players_table$id) %in% mkt_ids
      }

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

    ## Market Players RV (for Transfer Sandbox) ----
    market_players_RV <- reactive({
      req(is_module_active() == TRUE)
      req(login_token())
      req(championship_id())
      req(user_team_id())
      if (!is.null(refresh_trigger)) refresh_trigger()
      tryCatch({
        mkt_df <- get_market_players(
          login = login_token(),
          championship_id = championship_id(),
          user_team_id = user_team_id()
        )
        if (!is.null(mkt_df) && nrow(mkt_df) > 0) {
          mkt_df <- mkt_df %>%
            translate_player_positions() %>%
            calculate_player_changes() %>%
            unify_columns()
        }
        mkt_df
      }, error = function(e) {
        print(paste0("[Market Players RV] Fetch warning: ", e$message))
        NULL
      })
    })

    ## Liquid Cash Value (shared across tabs) ----
    liquid_cash_RV <- reactive({
      players_table <- players_table_RV()
      req(players_table)
      user_login <- get_reactive_val(login_token)
      user_champ_id <- get_reactive_val(championship_id)
      user_tid <- get_reactive_val(user_team_id)

      user_finances <- tryCatch({
        if (!is.null(user_login) && !is.null(user_champ_id) && !is.null(user_tid)) {
          get_user_team_info(login = user_login, championship_id = user_champ_id, user_team_id = user_tid)
        } else {
          NULL
        }
      }, error = function(e) NULL)

      total_spent <- 0
      if (!is.null(players_table) && nrow(players_table) > 0 && "buyPrice" %in% colnames(players_table)) {
        total_spent <- sum(suppressWarnings(as.numeric(players_table$buyPrice)), na.rm = TRUE)
      }

      liquid_cash_val <- 300000000 - total_spent
      if (!is.null(user_finances) && !is.null(user_finances$budget) && is.numeric(user_finances$budget) && user_finances$budget > 0) {
        liquid_cash_val <- user_finances$budget
      }
      liquid_cash_val
    })

    # renders ----
    output$team_value_box <- renderUI({
      req(is_module_active() == TRUE)
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
        scales::label_currency(prefix = "E", suffix = "M", scale = 1e-6)(.)
      average_player_value <- val_mean %>%
        scales::label_currency(prefix = "E", suffix = "M", scale = 1e-6)(.)
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

      # ---- Financials: Liquid Cash, Total Spent, Total Earned ----
      user_login <- get_reactive_val(login_token)
      user_champ_id <- get_reactive_val(championship_id)
      user_tid <- get_reactive_val(user_team_id)

      user_finances <- tryCatch({
        if (!is.null(user_login) && !is.null(user_champ_id) && !is.null(user_tid)) {
          get_user_team_info(login = user_login, championship_id = user_champ_id, user_team_id = user_tid)
        } else {
          NULL
        }
      }, error = function(e) NULL)

      roster_players <- players_table_RV()
      total_spent <- 0
      if (!is.null(roster_players) && nrow(roster_players) > 0 && "buyPrice" %in% colnames(roster_players)) {
        total_spent <- sum(suppressWarnings(as.numeric(roster_players$buyPrice)), na.rm = TRUE)
      }

      liquid_cash_val <- 300000000 - total_spent
      if (!is.null(user_finances) && !is.null(user_finances$budget) && is.numeric(user_finances$budget) && user_finances$budget > 0) {
        liquid_cash_val <- user_finances$budget
      }

      # total_volume_earned = liquid_cash_val + total_spent
      total_volume_earned <- liquid_cash_val + total_spent

      # total_volume_spent = total purchases from pressroom (or total_spent from roster)
      total_volume_spent <- total_spent

      # ---- Build 2x2 Grid Layout ----

      # Row 1, Col 1: Classification & Standings
      classification_standings_box <- box(
        title = "Classification & Standings",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        team_position_block,
        fluidRow(
          column(4, previous_team_block),
          column(4, team_points_block),
          column(4, next_team_block)
        )
      )

      # Row 1, Col 2: Squad Valuation & Market Trends
      squad_valuation_box <- box(
        title = "Squad Valuation & Market Trends",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        fluidRow(
          column(4, team_value_block),
          column(4, team_change_value_block),
          column(4, team_players_value_block)
        )
      )

      # Row 2, Col 1: Available Liquid Cash & Total Volume Earned
      liquid_cash_box <- box(
        title = "Available Liquid Cash & Total Volume Earned",
        width = 6,
        status = "success",
        solidHeader = TRUE,
        collapsible = FALSE,
        descriptionBlock(
          header = format_table_currency(liquid_cash_val),
          number = NULL,
          numberColor = "green",
          text = "Available Budget"
        ),
        div(
          style = "margin-top: 8px; font-size: 13px; color: #047857; text-align: center;",
          tagList(
            icon("sack-dollar"),
            paste0(" Total Volume Earned: ", format_table_currency(total_volume_earned))
          )
        )
      )

      # Row 2, Col 2: Squad Investment & Total Volume Spent
      squad_investment_box <- box(
        title = "Squad Investment & Total Volume Spent",
        width = 6,
        status = "warning",
        solidHeader = TRUE,
        collapsible = FALSE,
        descriptionBlock(
          header = format_table_currency(total_spent),
          number = NULL,
          numberColor = "orange",
          text = "Current Squad Cost"
        ),
        div(
          style = "margin-top: 8px; font-size: 13px; color: #b45309; text-align: center;",
          tagList(
            icon("money-bill-transfer"),
            paste0(" Total Volume Spent: ", format_table_currency(total_volume_spent))
          )
        )
      )

      ret <- tagList(
        fluidRow(
          classification_standings_box,
          squad_valuation_box
        ),
        fluidRow(
          liquid_cash_box,
          squad_investment_box
        )
      )
      return(ret)
    })

    # ================================================================
    # TAB 1: Squad Roster renders
    # ================================================================

    # Render Interactive Charts Grid
    output$charts_row <- renderUI({
      req(is_module_active() == TRUE)
      ns <- session$ns

      fluidRow(
        column(width = 12,
          box(
            title = "League Standings Evolution (Points)",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotly::plotlyOutput(ns("standings_evolution_plot"), height = "300px")
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

    # ================================================================
    # TAB 2: Lineup Optimizer renders
    # ================================================================

    # Optimizer reactive
    optimizer_result_RV <- reactive({
      req(is_module_active() == TRUE)
      squad_df <- players_table_RV()
      req(squad_df)
      formation <- input$opt_formation
      mode <- input$opt_mode

      tryCatch({
        optimize_starting_xi(squad_df = squad_df, formation = formation, mode = mode)
      }, error = function(e) {
        print(paste0("[Optimizer] Error: ", e$message))
        list(
          starting_xi = data.frame(),
          bench = data.frame(),
          formation = formation,
          mode = mode,
          total_score = 0,
          avg_fis = 0,
          feasible = FALSE,
          formation_counts = c(GK = 1, DEF = 0, MID = 0, FWD = 0)
        )
      })
    })

    # Optimizer KPI summary row
    output$optimizer_kpi_row <- renderUI({
      req(is_module_active() == TRUE)
      opt <- optimizer_result_RV()
      req(opt)

      fluidRow(
        column(
          width = 3,
          descriptionBlock(
            header = as.character(opt$formation),
            number = NULL,
            numberColor = "black",
            text = "Formation"
          )
        ),
        column(
          width = 3,
          descriptionBlock(
            header = round(opt$total_score, 1),
            number = NULL,
            numberColor = "blue",
            text = "Total Opt Score"
          )
        ),
        column(
          width = 3,
          descriptionBlock(
            header = round(opt$avg_fis, 1),
            number = NULL,
            numberColor = "green",
            text = "Avg FIS"
          )
        ),
        column(
          width = 3,
          descriptionBlock(
            header = if (opt$feasible) "Yes" else "Partial",
            number = NULL,
            numberColor = if (opt$feasible) "green" else "orange",
            text = "Feasible"
          )
        )
      )
    })

    # Soccer Pitch UI
    output$soccer_pitch_ui <- renderUI({
      req(is_module_active() == TRUE)
      opt <- optimizer_result_RV()
      req(opt)

      xi <- opt$starting_xi
      formation <- opt$formation

      if (is.null(xi) || nrow(xi) == 0) {
        return(
          div(
            class = "soccer-pitch",
            style = "position: relative; width: 100%; max-width: 600px; margin: 0 auto; padding: 20px; background: #166534; border-radius: 8px; min-height: 400px;",
            div(
              class = "pitch-halfway-line",
              style = "position: absolute; top: 50%; left: 0; right: 0; height: 2px; background: rgba(255,255,255,0.4);"
            ),
            div(
              class = "pitch-center-circle",
              style = "position: absolute; top: 50%; left: 50%; width: 80px; height: 80px; border: 2px solid rgba(255,255,255,0.4); border-radius: 50%; transform: translate(-50%, -50%);"
            ),
            div(style = "text-align: center; color: white; margin-top: 180px; font-size: 14px;", "No players available for lineup optimization.")
          )
        )
      }

      # Parse formation to get row counts
      formation_map <- list(
        "4-3-3" = list(FWD = 3, MID = 3, DEF = 4, GK = 1),
        "4-4-2" = list(FWD = 2, MID = 4, DEF = 4, GK = 1),
        "3-5-2" = list(FWD = 2, MID = 5, DEF = 3, GK = 1),
        "3-4-3" = list(FWD = 3, MID = 4, DEF = 3, GK = 1),
        "4-5-1" = list(FWD = 1, MID = 5, DEF = 4, GK = 1),
        "5-3-2" = list(FWD = 2, MID = 3, DEF = 5, GK = 1),
        "5-4-1" = list(FWD = 1, MID = 4, DEF = 5, GK = 1)
      )

      counts <- formation_map[[formation]]
      if (is.null(counts)) counts <- formation_map[["4-3-3"]]

      # Helper: get player card HTML
      make_player_card <- function(player_row, row_label, col_index, total_in_row) {
        name <- if (!is.null(player_row$name) && nzchar(as.character(player_row$name))) as.character(player_row$name) else "?"
        role <- if (!is.null(player_row$role) && nzchar(as.character(player_row$role))) as.character(player_row$role) else ""
        val <- if (!is.null(player_row$value)) format_table_currency(suppressWarnings(as.numeric(player_row$value))) else "-"
        fis <- if (!is.null(player_row$fis_score)) round(suppressWarnings(as.numeric(player_row$fis_score)), 1) else "-"

        # Calculate horizontal position
        left_pct <- if (total_in_row <= 1) 50 else ((col_index - 0.5) / total_in_row) * 100

        tags$div(
          class = "pitch-player-card",
          style = paste0("position: absolute; left: ", left_pct, "%; transform: translateX(-50%); text-align: center; width: 90px; z-index: 2;"),
          tags$div(
            style = "background: rgba(255,255,255,0.92); border-radius: 6px; padding: 4px 6px; font-size: 11px; box-shadow: 0 2px 4px rgba(0,0,0,0.2);",
            tags$span(class = "pitch-player-name", style = "display: block; font-weight: 700; color: #0f172a; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 80px;", name),
            tags$span(class = "pitch-player-role", style = "display: block; font-size: 9px; color: #64748b;", paste0(row_label, " #", col_index)),
            tags$span(class = "pitch-player-val", style = "display: block; font-size: 9px; color: #047857;", val),
            tags$span(class = "pitch-player-fis", style = "display: block; font-size: 9px; font-weight: 600; color: #2563eb;", paste0("FIS: ", fis))
          )
        )
      }

      # Assign players to rows by position group
      xi$pos_group <- if ("pos_group" %in% colnames(xi)) xi$pos_group else {
        map_position <- function(role_val) {
          if (is.na(role_val) || role_val == "" || role_val == "Unknown") return("Unknown")
          r <- tolower(trimws(as.character(role_val)))
          if (r %in% c("goalkeeper", "portero", "gk")) return("GK")
          if (r %in% c("defender", "defensa", "df")) return("DEF")
          if (r %in% c("midfielder", "centrocampista", "md")) return("MID")
          if (r %in% c("forward", "delantero", "fw")) return("FWD")
          "Unknown"
        }
        role_vec <- if ("role" %in% colnames(xi)) as.character(xi$role) else rep("Unknown", nrow(xi))
        role2_vec <- if ("role2" %in% colnames(xi)) as.character(xi$role2) else rep("", nrow(xi))
        vapply(seq_len(nrow(xi)), function(i) {
          primary <- map_position(role_vec[i])
          if (primary != "Unknown") return(primary)
          secondary <- map_position(role2_vec[i])
          if (secondary != "Unknown") return(secondary)
          "Unknown"
        }, character(1))
      }

      # Sort within each group by opt_score descending for consistent display
      xi <- xi[order(xi$pos_group, -xi$opt_score), ]

      # Build rows from top (FWD) to bottom (GK)
      row_order <- c("FWD", "MID", "DEF", "GK")
      row_top_pcts <- c(FWD = 10, MID = 35, DEF = 65, GK = 92)

      cards <- list()
      for (grp in row_order) {
        grp_players <- xi[xi$pos_group == grp, , drop = FALSE]
        n_in_grp <- nrow(grp_players)
        if (n_in_grp == 0) next
        top_pct <- row_top_pcts[[grp]]
        for (j in seq_len(n_in_grp)) {
          cards[[length(cards) + 1]] <- tags$div(
            class = "pitch-row",
            style = paste0("position: absolute; top: ", top_pct, "%; left: 0; right: 0; height: 30px;"),
            make_player_card(grp_players[j, ], grp, j, n_in_grp)
          )
        }
      }

      div(
        class = "soccer-pitch",
        style = "position: relative; width: 100%; max-width: 600px; margin: 0 auto; padding: 20px; background: #166534; border-radius: 8px; min-height: 450px; overflow: hidden;",
        div(
          class = "pitch-halfway-line",
          style = "position: absolute; top: 50%; left: 0; right: 0; height: 2px; background: rgba(255,255,255,0.4);"
        ),
        div(
          class = "pitch-center-circle",
          style = "position: absolute; top: 50%; left: 50%; width: 80px; height: 80px; border: 2px solid rgba(255,255,255,0.4); border-radius: 50%; transform: translate(-50%, -50%);"
        ),
        do.call(tagList, cards)
      )
    })

    # Starting XI table
    output$starting_xi_table <- renderReactable({
      req(is_module_active() == TRUE)
      opt <- optimizer_result_RV()
      req(opt)
      xi <- opt$starting_xi

      if (is.null(xi) || nrow(xi) == 0) {
        return(reactable(data.frame(), defaultColDef = colDef(cell = function() "No data")))
      }

      reactable(
        xi,
        columns = list(
          name = colDef(name = "Player", minWidth = 120),
          role = colDef(name = "Position", minWidth = 100),
          fis_score = colDef(name = "FIS", format = colFormat(digits = 1), minWidth = 60),
          opt_score = colDef(name = "Opt Score", format = colFormat(digits = 1), minWidth = 80),
          value = colDef(name = "Value", format = colFormat(separator = ",", digitGroupSeparator = "."), minWidth = 100),
          pos_group = colDef(name = "Group", minWidth = 60)
        ),
        highlight = TRUE,
        compact = TRUE,
        bordered = TRUE
      )
    })

    # Bench table
    output$bench_table <- renderReactable({
      req(is_module_active() == TRUE)
      opt <- optimizer_result_RV()
      req(opt)
      bench <- opt$bench

      if (is.null(bench) || nrow(bench) == 0) {
        return(reactable(data.frame(), defaultColDef = colDef(cell = function() "No bench players")))
      }

      reactable(
        bench,
        columns = list(
          name = colDef(name = "Player", minWidth = 120),
          role = colDef(name = "Position", minWidth = 100),
          fis_score = colDef(name = "FIS", format = colFormat(digits = 1), minWidth = 60),
          opt_score = colDef(name = "Opt Score", format = colFormat(digits = 1), minWidth = 80),
          value = colDef(name = "Value", format = colFormat(separator = ",", digitGroupSeparator = "."), minWidth = 100),
          pos_group = colDef(name = "Group", minWidth = 60)
        ),
        highlight = TRUE,
        compact = TRUE,
        bordered = TRUE
      )
    })

    # ================================================================
    # TAB 3: Transfer Sandbox renders
    # ================================================================

    # Update sandbox sell choices when roster loads
    observe({
      req(is_module_active() == TRUE)
      squad_df <- players_table_RV()
      req(squad_df)
      if ("id" %in% colnames(squad_df) && "name" %in% colnames(squad_df)) {
        choices <- setNames(
          as.character(squad_df$id),
          paste0(squad_df$name, " (", squad_df$role, ")")
        )
        updateSelectizeInput(session, "sandbox_sells", choices = choices, selected = character(0))
      }
    })

    # Update sandbox buy choices when market data loads
    observe({
      req(is_module_active() == TRUE)
      mkt_df <- market_players_RV()
      req(mkt_df)
      if ("id" %in% colnames(mkt_df) && "name" %in% colnames(mkt_df)) {
        choices <- setNames(
          as.character(mkt_df$id),
          paste0(mkt_df$name, " (", mkt_df$role, ")")
        )
        updateSelectizeInput(session, "sandbox_buys", choices = choices, selected = character(0))
      }
    })

    # Reset sandbox button
    observeEvent(input$sandbox_reset, {
      tryCatch({
        updateSelectizeInput(session, "sandbox_sells", selected = character(0))
        updateSelectizeInput(session, "sandbox_buys", selected = character(0))
        shiny::showNotification("Transfer sandbox reset.", type = "message", duration = 2)
      }, error = function(e) {
        print(paste0("[Sandbox Reset] Error: ", e$message))
      })
    })

    # Sandbox scenario reactive
    sandbox_scenario_RV <- reactive({
      req(is_module_active() == TRUE)
      squad_df <- players_table_RV()
      req(squad_df)
      current_budget <- liquid_cash_RV()
      req(current_budget)

      sell_ids <- input$sandbox_sells
      buy_ids <- input$sandbox_buys

      # Ensure character vectors
      sell_ids <- if (is.null(sell_ids) || length(sell_ids) == 0) character(0) else as.character(sell_ids)
      buy_ids <- if (is.null(buy_ids) || length(buy_ids) == 0) character(0) else as.character(buy_ids)

      mkt_df <- tryCatch({ market_players_RV() }, error = function(e) NULL)

      tryCatch({
        simulate_transfer_scenario(
          squad_df = squad_df,
          current_budget = current_budget,
          sell_player_ids = sell_ids,
          buy_player_ids = buy_ids,
          market_df = mkt_df
        )
      }, error = function(e) {
        print(paste0("[Sandbox Scenario] Error: ", e$message))
        list(
          projected_squad = data.frame(),
          total_sell_proceeds = 0,
          total_buy_cost = 0,
          projected_budget = current_budget,
          initial_total_val = 0,
          projected_total_val = 0,
          initial_avg_fis = 0,
          projected_avg_fis = 0,
          delta_avg_fis = 0,
          is_budget_valid = TRUE
        )
      })
    })

    # Sandbox KPI row
    output$sandbox_kpi_row <- renderUI({
      req(is_module_active() == TRUE)
      scenario <- sandbox_scenario_RV()
      req(scenario)

      delta_icon <- if (scenario$delta_avg_fis > 0) icon("caret-up") else if (scenario$delta_avg_fis < 0) icon("caret-down") else NULL
      budget_color <- if (scenario$is_budget_valid) "green" else "red"

      fluidRow(
        column(
          width = 3,
          descriptionBlock(
            header = format_table_currency(scenario$total_sell_proceeds),
            number = NULL,
            numberColor = "green",
            text = "Sell Proceeds"
          )
        ),
        column(
          width = 3,
          descriptionBlock(
            header = format_table_currency(scenario$total_buy_cost),
            number = NULL,
            numberColor = "orange",
            text = "Buy Cost"
          )
        ),
        column(
          width = 3,
          descriptionBlock(
            header = format_table_currency(scenario$projected_budget),
            number = NULL,
            numberColor = budget_color,
            text = "Projected Budget"
          )
        ),
        column(
          width = 3,
          descriptionBlock(
            header = paste0(scenario$delta_avg_fis, " pts"),
            number = NULL,
            numberColor = if (scenario$delta_avg_fis >= 0) "green" else "red",
            numberIcon = delta_icon,
            text = "Delta Avg FIS"
          )
        )
      )
    })

    # Sandbox projected squad table
    output$sandbox_projected_table <- renderReactable({
      req(is_module_active() == TRUE)
      scenario <- sandbox_scenario_RV()
      req(scenario)
      proj <- scenario$projected_squad

      if (is.null(proj) || nrow(proj) == 0) {
        return(reactable(data.frame(), defaultColDef = colDef(cell = function() "No projected squad data")))
      }

      reactable(
        proj,
        columns = list(
          name = colDef(name = "Player", minWidth = 120),
          role = colDef(name = "Position", minWidth = 100),
          fis_score = colDef(name = "FIS", format = colFormat(digits = 1), minWidth = 60),
          value = colDef(name = "Value", format = colFormat(separator = ",", digitGroupSeparator = "."), minWidth = 100)
        ),
        highlight = TRUE,
        compact = TRUE,
        bordered = TRUE,
        defaultSorted = list(fis_score = "desc")
      )
    })

    # Transfer recommendations
    output$sandbox_recommendations_ui <- renderUI({
      req(is_module_active() == TRUE)
      squad_df <- players_table_RV()
      req(squad_df)
      mkt_df <- tryCatch({ market_players_RV() }, error = function(e) NULL)
      current_budget <- tryCatch({ liquid_cash_RV() }, error = function(e) 0)

      recs <- tryCatch({
        recommend_transfers(
          squad_df = squad_df,
          market_df = mkt_df,
          current_budget = current_budget,
          max_transfers = 5
        )
      }, error = function(e) {
        print(paste0("[Transfer Recs] Error: ", e$message))
        data.frame()
      })

      if (is.null(recs) || nrow(recs) == 0) {
        return(
          div(
            style = "margin-top: 15px; padding: 10px; background: #f8fafc; border-radius: 6px; text-align: center; color: #64748b; font-size: 13px;",
            "No transfer recommendations available. Add players to sell or buy to see suggestions."
          )
        )
      }

      # Build recommendation cards
      cards_list <- list()
      for (i in seq_len(nrow(recs))) {
        r <- recs[i, ]
        net_cost_str <- if (r$net_cost > 0) paste0("+", format_table_currency(r$net_cost)) else format_table_currency(r$net_cost)
        delta_fis_str <- if (r$delta_fis > 0) paste0("+", round(r$delta_fis, 1)) else as.character(round(r$delta_fis, 1))

        cards_list[[i]] <- div(
          style = "margin-top: 10px; padding: 10px; background: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 6px; display: flex; justify-content: space-between; align-items: center;",
          div(
            style = "flex: 1;",
            tags$strong(style = "font-size: 13px; color: #0f172a;",
              paste0("Sell: ", r$sell_name, " -> Buy: ", r$buy_name)
            ),
            tags$br(),
            tags$span(style = "font-size: 11px; color: #64748b;",
              paste0("Net: ", net_cost_str, " | FIS Delta: ", delta_fis_str, " | ROI: ", r$roi_pct, "%")
            )
          ),
          actionButton(
            ns(paste0("rec_apply_", i)),
            "Apply",
            class = "btn btn-sm btn-success",
            style = "margin-left: 10px; white-space: nowrap;"
          )
        )
      }

      div(
        style = "margin-top: 15px;",
        tags$h5(style = "margin-bottom: 5px; color: #0f172a;", "Top Transfer Recommendations"),
        do.call(tagList, cards_list)
      )
    })

    # Recommendation apply observers
    observeEvent(input[["rec_apply_1"]], {
      tryCatch({
        apply_recommendation(session, 1)
      }, error = function(e) print(paste0("[Rec Apply 1] Error: ", e$message)))
    })
    observeEvent(input[["rec_apply_2"]], {
      tryCatch({
        apply_recommendation(session, 2)
      }, error = function(e) print(paste0("[Rec Apply 2] Error: ", e$message)))
    })
    observeEvent(input[["rec_apply_3"]], {
      tryCatch({
        apply_recommendation(session, 3)
      }, error = function(e) print(paste0("[Rec Apply 3] Error: ", e$message)))
    })
    observeEvent(input[["rec_apply_4"]], {
      tryCatch({
        apply_recommendation(session, 4)
      }, error = function(e) print(paste0("[Rec Apply 4] Error: ", e$message)))
    })
    observeEvent(input[["rec_apply_5"]], {
      tryCatch({
        apply_recommendation(session, 5)
      }, error = function(e) print(paste0("[Rec Apply 5] Error: ", e$message)))
    })

    # Helper: apply a recommendation to sandbox selectors
    apply_recommendation <- function(session, idx) {
      req(is_module_active() == TRUE)
      squad_df <- players_table_RV()
      req(squad_df)
      mkt_df <- tryCatch({ market_players_RV() }, error = function(e) NULL)
      current_budget <- tryCatch({ liquid_cash_RV() }, error = function(e) 0)

      recs <- tryCatch({
        recommend_transfers(
          squad_df = squad_df,
          market_df = mkt_df,
          current_budget = current_budget,
          max_transfers = 5
        )
      }, error = function(e) data.frame())

      if (is.null(recs) || nrow(recs) < idx) return()

      r <- recs[idx, ]
      sell_id <- as.character(r$sell_id)
      buy_id <- as.character(r$buy_id)

      # Get current selections
      current_sells <- input$sandbox_sells
      current_buys <- input$sandbox_buys
      current_sells <- if (is.null(current_sells)) character(0) else as.character(current_sells)
      current_buys <- if (is.null(current_buys)) character(0) else as.character(current_buys)

      # Add to selections if not already present
      if (!sell_id %in% current_sells) {
        current_sells <- c(current_sells, sell_id)
      }
      if (!buy_id %in% current_buys) {
        current_buys <- c(current_buys, buy_id)
      }

      updateSelectizeInput(session, "sandbox_sells", selected = current_sells)
      updateSelectizeInput(session, "sandbox_buys", selected = current_buys)

      shiny::showNotification(
        paste0("Applied: Sell ", r$sell_name, " -> Buy ", r$buy_name),
        type = "message",
        duration = 3
      )
    }

    # observers ----
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

        # Optimistically mark all squad players as on market for instant UI update
        curr_table <- tryCatch({ players_table_RV() }, error = function(e) NULL)
        if (!is.null(curr_table) && nrow(curr_table) > 0) {
          curr_table$market_inMarket <- TRUE
          tryCatch({ players_table_RV(curr_table) }, error = function(e) NULL)
        }

        # Trigger reactive refresh with cache-cleared fresh API data
        if (!is.null(refresh_trigger) && is.function(refresh_trigger)) {
          tryCatch(refresh_trigger(refresh_trigger() + 1), error = function(e) NULL)
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

    # Module ----
    ##  players_table_Server Module ----
    selected_player_RV <- players_table_Server(
      id = "players_table_in_teams",
      players_table_RV = players_table_RV,
      user_teams_RV = user_teams_RV,
      login_token = login_token,
      championship_id = championship_id,
      user_team_id = user_team_id,
      hide_bid_column = FALSE
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