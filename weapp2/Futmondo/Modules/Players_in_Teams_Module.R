library(reactable)

players_in_teams_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("team_value_box")),
    squad_coach_UI(ns("coach")),
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
              choices = c("Best available formation" = "auto", "4-3-3", "4-4-2", "3-5-2", "3-4-3", "4-5-1", "5-3-2", "5-4-1"),
              selected = "auto"
            )
          ),
          column(
            width = 6,
            selectInput(
              ns("opt_mode"),
              "Strategy Mode",
              choices = c(
                "Expected points (Baseline)" = "max_fis",
                "Lower forecast range (when available)" = "safe",
                "Upper forecast range (when available)" = "upside",
                "Recent average points" = "form"
              ),
              selected = "max_fis"
            )
          )
        ),
        # Optimizer KPI summary row
        uiOutput(ns("optimizer_kpi_row")),
        uiOutput(ns("submitted_lineup_comparison")),
        uiOutput(ns("lineup_notes")),
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
        uiOutput(ns("sandbox_price_status")),
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
                actionButton(ns("sandbox_reset"), "Reset Sandbox", class = "btn btn-default"),
                textInput(ns("scenario_name"), "Scenario name", placeholder = "Next round options"),
                actionButton(ns("scenario_save"), "Save scenario"),
                selectInput(ns("scenario_saved"), "Saved scenarios", choices = character()),
                actionButton(ns("scenario_load"), "Load scenario")
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
              reactableOutput(ns("sandbox_projected_table")),
              h4("Resulting starting XI"),
              uiOutput(ns("sandbox_lineup_status")),
              reactableOutput(ns("sandbox_lineup_table"))
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
    squad_coach_Server("coach", login_token, championship_id, user_team_id)

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
      players_table <- preserve_observation_time(players_table)
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
      # Reconcile only against the authenticated manager's own listings.
      my_mkt_df <- tryCatch({
        get_my_market_players(
          login = login_token(),
          championship_id = championship_id,
          user_team_id = user_team_id
        )
      }, error = function(e) {
        print(paste0("[Market check] Fetch warning: ", e$message))
        NULL
      })
      if (!is.null(my_mkt_df) && "id" %in% colnames(my_mkt_df)) {
        mkt_ids <- as.character(my_mkt_df$id)
        players_table$market_inMarket <- as.character(players_table$id) %in% mkt_ids
      }

      players_table <- preserve_observation_time(players_table)

      # Queue persistence after the fetched snapshot is available
      tryCatch({
        defer_persistence(sync_real_clubs_to_supabase, list(players_table))
        defer_persistence(sync_players_to_supabase, list(players_table))
        defer_persistence(log_player_history, list(players_table, championship_id))
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
        mkt_df <- preserve_observation_time(mkt_df)
        if (!is.null(mkt_df) && nrow(mkt_df) > 0) {
          mkt_df <- mkt_df %>%
            translate_player_positions() %>%
            calculate_player_changes() %>%
            unify_columns()
        }
        mkt_df <- preserve_observation_time(mkt_df)
        mkt_df
      }, error = function(e) {
        print(paste0("[Market Players RV] Fetch warning: ", e$message))
        NULL
      })
    })

    financial_snapshot_RV <- reactive({
      req(login_token(), championship_id(), user_team_id())
      if (!is.null(refresh_trigger)) refresh_trigger()
      tryCatch(get_financial_snapshot(login_token(), championship_id(), user_team_id()),
               error = function(e) list(status = "unavailable", cash = NA_real_,
                                        spendable_budget = NA_real_, configuration = list()))
    })
    liquid_cash_RV <- reactive({ financial_snapshot_RV()$cash })
    transfer_budget_RV <- reactive({ financial_snapshot_RV()$spendable_budget })
    submitted_lineup_RV <- reactive({
      req(is_module_active() == TRUE, login_token(), championship_id(), user_team_id())
      if (!is.null(refresh_trigger)) refresh_trigger()
      tryCatch(get_lineup_from_team(login_token(), championship_id(), user_team_id()), error = function(e) NULL)
    })
    league_rules_RV <- reactive({
      snapshot <- financial_snapshot_RV()
      if (is.list(snapshot$lineup_rules)) return(snapshot$lineup_rules)
      normalize_league_rules(list(configuration = snapshot$configuration), submitted_lineup_RV())
    })

    point_history_RV <- reactive({
      req(is_module_active() == TRUE, championship_id())
      if (!is.null(refresh_trigger)) refresh_trigger()
      rules <- league_rules_RV()
      history <- tryCatch(read_player_match_history(championship_id(), final_only = TRUE,
        season=rules$season, scoring_version=rules$scoring_version), error = function(e) data.frame())
      if (is.data.frame(history) && nrow(history) && "round" %in% names(history)) {
        # A round number alone is reused across seasons and scoring variants.
        season <- if ("season" %in% names(history)) history$season else "unknown"
        scoring <- if ("scoring_version" %in% names(history)) history$scoring_version else "unknown"
        history$round_id <- paste(season, scoring, history$round, sep = ":")
      }
      history
    })
    point_forecasts_RV <- reactive({
      req(is_module_active() == TRUE)
      players <- dplyr::bind_rows(players_table_RV(), market_players_RV())
      players <- players[!duplicated(as.character(players$id)), , drop = FALSE]
      rules <- league_rules_RV()
      context <- list(championship_id=championship_id(),season=rules$season,scoring_version=rules$scoring_version)
      for (key in names(context)) players[[key]] <- context[[key]]
      forecast_fantasy_points(players, history = point_history_RV(), horizons = 1, context=context)
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

      # A real zero/negative balance is meaningful; unavailable stays unavailable.
      financial <- financial_snapshot_RV()
      liquid_cash_val <- financial$cash
      roster_players <- players_table_RV()
      costs <- if ("buyPrice" %in% names(roster_players)) suppressWarnings(as.numeric(roster_players$buyPrice)) else NA_real_
      total_spent <- if (length(costs) && all(is.finite(costs))) sum(costs) else NA_real_

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
        title = "Cash & Spending Capacity",
        width = 6,
        status = "success",
        solidHeader = TRUE,
        collapsible = FALSE,
        descriptionBlock(
          header = ui_financial_amount(liquid_cash_val),
          number = NULL,
          numberColor = "green",
          text = "Cash balance"
        ),
        div(
          style = "margin-top: 8px; font-size: 13px; color: var(--fm-text); text-align: center;",
          tagList(
            icon("sack-dollar"),
            paste0(" Spendable: ", ui_financial_amount(financial$spendable_budget), " | Legal bid limit: ", ui_financial_amount(financial$legal_bid_limit))
          )
        )
      )

      # Row 2, Col 2: Squad Investment & Total Volume Spent
      squad_investment_box <- box(
        title = "Current Squad Investment",
        width = 6,
        status = "warning",
        solidHeader = TRUE,
        collapsible = FALSE,
        descriptionBlock(
          header = ui_financial_amount(total_spent),
          number = NULL,
          numberColor = "orange",
          text = "Current Squad Cost"
        ),
        div(
          style = "margin-top: 8px; font-size: 13px; color: var(--fm-text); text-align: center;",
          tagList(
            icon("money-bill-transfer"),
            "Purchase cost of players currently in the squad; excludes players already sold."
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
            fm_plot_layout(
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
                  text = "<b>No matchday points recorded yet.</b><br><span style='font-size: 12px; color: var(--fm-muted);'>The points evolution timeline will display automatically once matchday scores are logged.</span>",
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
        fm_plot_layout(
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
      formation <- if (is.null(input$opt_formation)) "auto" else input$opt_formation
      mode <- if (is.null(input$opt_mode)) "max_fis" else input$opt_mode

      tryCatch({
        optimize_starting_xi(squad_df = squad_df, formation = formation, mode = mode,
                            rules = league_rules_RV(),
                            forecast_df = point_forecasts_RV())
      }, error = function(e) {
        print(paste0("[Optimizer] Error: ", e$message))
        list(
          starting_xi = data.frame(),
          bench = data.frame(),
          formation = formation,
          mode = mode,
          total_score = NA_real_,
          expected_points = NA_real_,
          avg_fis = NA_real_,
          diagnostics = "The lineup could not be verified.",
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
            header = if (length(opt$expected_points) == 1L && is.finite(opt$expected_points)) round(opt$expected_points, 1) else "Unavailable",
            number = NULL,
            numberColor = "blue",
            text = "Expected points (baseline)"
          )
        ),
        column(
          width = 3,
          descriptionBlock(
            header = if (length(opt$avg_fis) == 1L && is.finite(opt$avg_fis)) round(opt$avg_fis, 1) else "Unavailable",
            number = NULL,
            numberColor = "green",
            text = "Avg FIS"
          )
        ),
        column(
          width = 3,
          descriptionBlock(
            header = if (isTRUE(opt$legality_verified)) "Verified" else if (isTRUE(opt$feasible)) "Provisional" else "Invalid",
            number = NULL,
            numberColor = if (isTRUE(opt$feasible)) "green" else "orange",
            text = "Feasible"
          )
        )
      )
    })

    output$submitted_lineup_comparison <- renderUI({
      comparison <- squad_lineup_comparison(submitted_lineup_RV(), optimizer_result_RV())
      if (comparison$status != "ok") return(div(class = "alert alert-info", comparison$message))
      roster <- players_table_RV()
      names_for <- function(ids) paste(roster$name[match(ids, as.character(roster$id))], collapse = ", ")
      div(class = "alert alert-info",
          strong("Compared with your submitted XI: "),
          paste0(length(comparison$add), " player changes. "),
          if (length(comparison$add)) span("Add: ", names_for(comparison$add), ". Remove: ", names_for(comparison$remove), "."),
          lapply(comparison$changes,function(change) p(change)),
          p("Recommendation only. Check the official round deadline and submit your team in Futmondo."))
    })

    output$lineup_notes <- renderUI({
      req(is_module_active() == TRUE)
      opt <- optimizer_result_RV()
      roster <- players_table_RV()
      label <- function(ids) {
        if (!length(ids)) return("None")
        names <- as.character(roster$name[match(ids, as.character(roster$id))])
        missing <- is.na(names) | !nzchar(names)
        names[missing] <- as.character(ids)[missing]
        paste(names, collapse = ", ")
      }
      captain <- opt$captain_id
      tagList(
        if (length(opt$diagnostics)) div(class = "alert alert-warning", paste(opt$diagnostics, collapse = " ")),
        if (isTRUE(opt$feasible)) p(
          if (length(captain) == 1L && !is.na(captain)) span(strong("Captain: "), label(captain), ". "),
          if (length(opt$bench_order)) span(strong("Bench order: "), label(opt$bench_order), ". Substitutions follow this order within each position. "),
          "Point forecasts are advisory. Availability and league settings are checked against the current snapshot."))
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
            style = "position: relative; width: 100%; max-width: 600px; margin: 0 auto; padding: 20px; background: var(--fm-surface); border-radius: 8px; min-height: 400px;",
            div(
              class = "pitch-halfway-line",
              style = "position: absolute; top: 50%; left: 0; right: 0; height: 2px; background: var(--fm-surface);"
            ),
            div(
              class = "pitch-center-circle",
              style = "position: absolute; top: 50%; left: 50%; width: 80px; height: 80px; border: 2px solid rgba(255,255,255,0.4); border-radius: 50%; transform: translate(-50%, -50%);"
            ),
            div(style = "text-align: center; color: var(--fm-text); margin-top: 180px; font-size: 14px;", "No players available for lineup optimization.")
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
          style = paste0("position: absolute; left: ", left_pct, "%; transform: translateX(-50%); text-align: center; width: calc(100% / ", total_in_row + 1, " - 4px); max-width: 90px; z-index: 2;"),
          tags$div(
            style = "background: var(--fm-surface); border-radius: 6px; padding: 4px 6px; font-size: 11px; box-shadow: 0 2px 4px rgba(0,0,0,0.2);",
            tags$span(class = "pitch-player-name", style = "display: block; font-weight: 700; color: var(--fm-text); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 80px;", name),
            tags$span(class = "pitch-player-role", style = "display: block; font-size: 9px; color: var(--fm-muted);", paste0(row_label, " #", col_index)),
            tags$span(class = "pitch-player-val", style = "display: block; font-size: 9px; color: var(--fm-text);", val),
            tags$span(class = "pitch-player-fis", style = "display: block; font-size: 9px; font-weight: 600; color: var(--fm-text);", paste0("FIS: ", fis))
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
        style = "position: relative; width: 100%; max-width: 600px; margin: 0 auto; padding: 20px; background: var(--fm-surface); border-radius: 8px; min-height: 450px; overflow: hidden;",
        div(
          class = "pitch-halfway-line",
          style = "position: absolute; top: 50%; left: 0; right: 0; height: 2px; background: var(--fm-surface);"
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

      rows <- squad_lineup_display_rows(xi, opt$captain_id, opt$bench_order, starting = TRUE)
      reactable(rows, defaultColDef = colDef(na = "Unavailable"),
        columns = list(player = colDef(name = "Player"), position = colDef(name = "Assigned position"),
          expected_points = colDef(name = "Expected points", format = colFormat(digits = 1)),
          lower = colDef(name = "Lower estimate", format = colFormat(digits = 1)),
          upper = colDef(name = "Upper estimate", format = colFormat(digits = 1)),
          value = colDef(name = "Market value", cell = ui_financial_amount),
          selection = colDef(name = "Selection")), highlight = TRUE, compact = TRUE, bordered = TRUE)
    })

    # The full reserve roster remains visible; only configured, eligible bench
    # selections receive an explicit substitution order.
    output$bench_table <- renderReactable({
      req(is_module_active() == TRUE)
      opt <- optimizer_result_RV(); req(opt)
      rows <- squad_lineup_display_rows(opt$bench, opt$captain_id, opt$bench_order)
      if (!nrow(rows)) return(reactable(data.frame(Message = "No reserve players.")))
      reactable(rows, defaultColDef = colDef(na = "Unavailable"),
        columns = list(player = colDef(name = "Player"), position = colDef(name = "Position"),
          expected_points = colDef(name = "Expected points", format = colFormat(digits = 1)),
          lower = colDef(name = "Lower estimate", format = colFormat(digits = 1)),
          upper = colDef(name = "Upper estimate", format = colFormat(digits = 1)),
          value = colDef(name = "Market value", cell = ui_financial_amount),
          selection = colDef(name = "Bench order")), highlight = TRUE, compact = TRUE, bordered = TRUE)
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

    scenario_version <- reactiveVal(0L)
    saved_scenarios_RV <- reactive({
      req(login_token(), championship_id(), user_team_id())
      scenario_version()
      tryCatch(read_transfer_scenarios(login_token()[["userid"]], championship_id(), user_team_id()),
               error = function(e) data.frame())
    })
    observeEvent(saved_scenarios_RV(), {
      d <- saved_scenarios_RV()
      choices <- if (is.data.frame(d) && nrow(d)) stats::setNames(as.character(d$id), d$name) else character()
      updateSelectInput(session, "scenario_saved", choices = choices)
    })
    observeEvent(input$scenario_save, {
      name <- trimws(input$scenario_name)
      if (!length(name) || !nzchar(name)) {
        showNotification("Enter a scenario name.", type = "warning"); return()
      }
      payload <- list(sell_ids = as.character(input$sandbox_sells), buy_ids = as.character(input$sandbox_buys),
                      formation = input$opt_formation, mode = input$opt_mode)
      ok <- tryCatch(save_transfer_scenario(login_token()[["userid"]], championship_id(), user_team_id(), name, payload), error = function(e) FALSE)
      showNotification(if (isTRUE(ok)) "Scenario saved." else "Scenario could not be saved.", type = if (isTRUE(ok)) "message" else "error")
      if (isTRUE(ok)) scenario_version(isolate(scenario_version()) + 1L)
    })
    observeEvent(input$scenario_load, {
      d <- saved_scenarios_RV(); req(is.data.frame(d), nrow(d), input$scenario_saved)
      idx <- match(input$scenario_saved, as.character(d$id)); req(!is.na(idx))
      item <- d$scenario[[idx]]
      if (is.character(item)) item <- tryCatch(jsonlite::fromJSON(item), error = function(e) NULL)
      req(is.list(item))
      # Keep selections: fresh validation reports players who are no longer available.
      restore_choices <- function(players, ids) {
        choices <- if (is.data.frame(players) && nrow(players)) stats::setNames(as.character(players$id), players$name) else character()
        missing <- setdiff(as.character(ids), unname(choices))
        c(choices, stats::setNames(missing, paste("Unavailable player", missing)))
      }
      updateSelectizeInput(session, "sandbox_sells", choices = restore_choices(players_table_RV(), item$sell_ids), selected = as.character(item$sell_ids))
      updateSelectizeInput(session, "sandbox_buys", choices = restore_choices(market_players_RV(), item$buy_ids), selected = as.character(item$buy_ids))
      updateSelectInput(session, "opt_formation", selected = item$formation)
      updateSelectInput(session, "opt_mode", selected = item$mode)
      showNotification("Scenario loaded and recalculated using current data.", type = "message")
    })

    # Sandbox scenario reactive
    sandbox_scenario_RV <- reactive({
      req(is_module_active() == TRUE)
      squad_df <- players_table_RV()
      req(squad_df)
      current_budget <- transfer_budget_RV()

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
          market_df = mkt_df,
          rules = league_rules_RV(),
          forecast_df = point_forecasts_RV()
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
          is_budget_valid = FALSE,
          is_lineup_valid = FALSE,
          status = "error",
          diagnostics = "Scenario could not be verified."
        )
      })
    })

    output$sandbox_price_status <- renderUI({
      scenario <- sandbox_scenario_RV()
      if (!is.null(scenario$status) && scenario$status == "ok" && !isTRUE(scenario$prices_verified))
        p(class = "text-muted", "Transfer prices are estimates. Actual offers, auction outcomes and retained funds can change the result.")
    })

    # Sandbox KPI row
    output$sandbox_kpi_row <- renderUI({
      req(is_module_active() == TRUE)
      scenario <- sandbox_scenario_RV()
      req(scenario)

      if (!is.null(scenario$status) && scenario$status != "ok") {
        return(div(class = "alert alert-warning", strong("Scenario is not valid. "),
                   paste(unlist(scenario$diagnostics), collapse = " ")))
      }
      delta <- scenario$delta_avg_fis
      if (length(delta) != 1L || !is.finite(delta)) delta <- 0
      delta_icon <- if (delta > 0) icon("caret-up") else if (delta < 0) icon("caret-down") else NULL
      budget_color <- if (isTRUE(scenario$is_budget_valid)) "green" else "red"

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
            header = paste0(scenario$delta_avg_fis, " FIS"),
            number = NULL,
            numberColor = if (delta >= 0) "green" else "red",
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
        proj[, intersect(c("name", "role", "fis_score", "value"), names(proj)), drop = FALSE],
        columns = list(
          name = colDef(name = "Player", minWidth = 120),
          role = colDef(name = "Position", minWidth = 100),
          fis_score = colDef(name = "FIS", format = colFormat(digits = 1), minWidth = 60),
          value = colDef(name = "Market value", cell = ui_financial_amount, minWidth = 100)
        ),
        highlight = TRUE,
        compact = TRUE,
        bordered = TRUE,
        defaultSorted = list(fis_score = "desc")
      )
    })

    output$sandbox_lineup_status <- renderUI({
      req(is_module_active() == TRUE)
      scenario <- sandbox_scenario_RV()
      lineup <- scenario$projected_lineup
      if (is.null(lineup) || !isTRUE(lineup$feasible))
        return(p("A legal starting XI could not be verified for this scenario."))
      p("Formation: ", lineup$formation, ". Expected points: ",
        if (length(lineup$expected_points) == 1L && is.finite(lineup$expected_points)) round(lineup$expected_points, 1) else "Unavailable",
        ". This scenario uses estimated transfer prices.")
    })
    output$sandbox_lineup_table <- renderReactable({
      req(is_module_active() == TRUE)
      lineup <- sandbox_scenario_RV()$projected_lineup
      if (is.null(lineup) || !isTRUE(lineup$feasible)) return(reactable(data.frame(Message = "No verified XI.")))
      rows <- squad_lineup_display_rows(lineup$starting_xi, lineup$captain_id, lineup$bench_order, starting = TRUE)
      reactable(rows[, c("player", "position", "expected_points", "selection"), drop = FALSE],
        defaultColDef = colDef(na = "Unavailable"), compact = TRUE,
        columns = list(expected_points = colDef(name = "Expected points", format = colFormat(digits = 1))))
    })

    # Shared snapshot keeps displayed and applied recommendations identical.
    transfer_recommendations_RV <- reactive({
      req(is_module_active() == TRUE)
      squad_df <- players_table_RV()
      req(squad_df)
      mkt_df <- tryCatch({ market_players_RV() }, error = function(e) NULL)
      current_budget <- tryCatch({ transfer_budget_RV() }, error = function(e) NA_real_)

      if(exists('profit_portfolio_inputs',mode='function')) {
        prepared <- tryCatch({
          execution<-fit_sale_execution(read_sale_observations(login_token()[['userid']],championship_id()))
          rules<-league_rules_RV()
          profit_portfolio_inputs(squad_df,mkt_df,read_player_price_history(championship_id()),
            get_roster_bids(login_token(),championship_id(),user_team_id()),execution,
            context=list(championship_id=championship_id(),season=rules$season,scoring_version=rules$scoring_version))
        },error=function(e)NULL)
        if(!is.null(prepared)){squad_df<-prepared$roster;mkt_df<-prepared$market}
      }

      recs <- tryCatch({
        recommend_transfers(
          squad_df = squad_df,
          market_df = mkt_df,
          current_budget = current_budget,
          max_transfers = 5,
          rules = league_rules_RV(),
          forecast_df = point_forecasts_RV()
        )
      }, error = function(e) {
        print(paste0("[Transfer Recs] Error: ", e$message))
        data.frame()
      })

      recs
    })
    output$sandbox_recommendations_ui <- renderUI({
      req(is_module_active() == TRUE)
      recs <- transfer_recommendations_RV()
      if (is.null(recs) || nrow(recs) == 0) {
        return(
          div(
            style = "margin-top: 15px; padding: 10px; background: var(--fm-surface); border-radius: 6px; text-align: center; color: var(--fm-muted); font-size: 13px;",
            "No supported profitable transfer yet. Recommendations need observed sale offers and resale evidence."
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
          style = "margin-top: 10px; padding: 10px; background: var(--fm-surface); border: 1px solid #bbf7d0; border-radius: 6px; display: flex; justify-content: space-between; align-items: center;",
          div(
            style = "flex: 1;",
            tags$strong(style = "font-size: 13px; color: var(--fm-text);",
              paste0("Sell: ", r$sell_name, " -> Buy: ", r$buy_name)
            ),
            tags$br(),
            tags$span(style = "font-size: 11px; color: var(--fm-muted);",
              paste0("Net cost: ", net_cost_str, " | Expected profit: ", format_table_currency(r$expected_profit),
                " | XI change: ", round(r$delta_expected_points, 1), " points")
            )
          ),
          tags$button(type="button",class="btn btn-sm btn-success", "Preview",
            onclick=paste0("Shiny.setInputValue(",jsonlite::toJSON(ns("apply_recommendation"),auto_unbox=TRUE),",",
              jsonlite::toJSON(list(account=login_token()[['userid']],league=championship_id(),team=user_team_id(),
                sell_id=as.character(r$sell_id),buy_id=as.character(r$buy_id)),auto_unbox=TRUE),",{priority:'event'});"))

        )
      }

      div(
        style = "margin-top: 15px;",
        tags$h5(style = "margin-bottom: 5px; color: var(--fm-text);", "Top Transfer Recommendations"),
        do.call(tagList, cards_list)
      )
    })

    observeEvent(input$apply_recommendation, {
      event <- input$apply_recommendation
      req(is.list(event),identical(event$account,as.character(login_token()[['userid']])),
        identical(event$league,as.character(championship_id())),identical(event$team,as.character(user_team_id())))
      apply_recommendation(session,event$sell_id,event$buy_id)
    })

    apply_recommendation <- function(session, sell_id, buy_id) {
      req(is_module_active() == TRUE)
      recs <- transfer_recommendations_RV()
      req(is.data.frame(recs),nrow(recs)>0,length(sell_id)==1L,length(buy_id)==1L)
      r <- recs[recs$sell_id==sell_id & recs$buy_id==buy_id,,drop=FALSE]
      req(nrow(r)==1L)

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
        p("Choose how to price every player. Listings are sent one by one and only verified successes are refreshed."),
        radioButtons(ns("bulk_listing_price_mode"), "Listing price", choices = c(
          "Current market value" = "value",
          "Closed clause plus premium" = "closed_clause"
        ), selected = "value"),
        conditionalPanel(
          condition = sprintf("input['%s'] === 'closed_clause'", ns("bulk_listing_price_mode")),
          numericInput(ns("bulk_listing_clause_premium"), "Premium above closed clause (%)", value = 5, min = 0, max = 1000, step = 1)
        ),
        p(style = "color: var(--fm-muted); font-size: 12px;", "The clause option skips players without a valid closed clause. Other users and Futmondo can bid on listed players."),
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

      mode <- if (identical(as.character(input$bulk_listing_price_mode), "closed_clause")) "closed_clause" else "value"
      premium <- suppressWarnings(as.numeric(input$bulk_listing_clause_premium))
      if (!is.finite(premium)) premium <- 5
      roster <- tryCatch(players_table_RV(), error = function(e) NULL)
      plan <- bulk_market_listing_plan(roster, mode = mode, clause_premium_pct = premium)
      if (!nrow(plan$entries)) {
        shiny::showNotification(plan$message, type = "warning", duration = 6)
        return()
      }

      outcomes <- lapply(seq_len(nrow(plan$entries)), function(i) {
        player <- plan$entries[i, , drop = FALSE]
        tryCatch({
          if (isTRUE(player$already_listed)) {
            update_player_market_listing(login, champ_id, team_id, player$player_id, player$price)
          } else {
            put_player_on_market(login, champ_id, team_id, player$player_id, player$price)
          }
        }, error = function(e) list(success = FALSE, code = "error", message = conditionMessage(e)))
      })
      succeeded <- vapply(outcomes, function(x) if (is.list(x)) isTRUE(x$success) else isTRUE(x), logical(1))
      success_count <- sum(succeeded)
      failure_count <- length(succeeded) - success_count
      removeModal()

      if (success_count > 0L) {
        clear_api_cache()
        if (!is.null(refresh_trigger) && is.function(refresh_trigger)) {
          tryCatch(refresh_trigger(refresh_trigger() + 1), error = function(e) NULL)
        }
      }
      detail <- if (plan$skipped > 0L) paste0(" ", plan$skipped, " player(s) skipped because no eligible price was available.") else ""
      if (failure_count == 0L) {
        shiny::showNotification(paste0(success_count, " player(s) listed successfully.", detail), type = "message", duration = 6)
      } else {
        shiny::showNotification(paste0(success_count, " player(s) listed; ", failure_count, " failed.", detail, " Refresh to review the failed listings."), type = "warning", duration = 8)
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
      hide_bid_column = FALSE,
      refresh_trigger = refresh_trigger
    )

    return(selected_player_RV)
  })
}

# Build verified per-player prices for bulk market listings. Closed clauses
# have a positive price, are not transferred, and have a future lock date.
bulk_market_listing_plan <- function(roster, mode = c("value", "closed_clause"), clause_premium_pct = 5, now = Sys.time()) {
  mode <- match.arg(mode)
  empty <- data.frame(player_id = character(), price = numeric(), already_listed = logical(), stringsAsFactors = FALSE)
  if (!is.data.frame(roster) || !nrow(roster) || !"id" %in% names(roster))
    return(list(entries = empty, skipped = 0L, message = "Your squad is unavailable. Refresh before listing players."))
  ids <- trimws(as.character(roster$id))
  listed <- if ("market_inMarket" %in% names(roster)) as.logical(roster$market_inMarket) else rep(FALSE, nrow(roster))
  listed[is.na(listed)] <- FALSE
  premium <- suppressWarnings(as.numeric(clause_premium_pct)[1])
  if (!is.finite(premium) || premium < 0) premium <- 5
  if (mode == "value") {
    prices <- if ("value" %in% names(roster)) suppressWarnings(as.numeric(as.character(roster$value))) else rep(NA_real_, nrow(roster))
    message <- "No players have a valid market value to list."
  } else {
    clauses <- if ("clause_price" %in% names(roster)) suppressWarnings(as.numeric(as.character(roster$clause_price))) else rep(NA_real_, nrow(roster))
    transferred <- if ("clause_transferred" %in% names(roster)) as.logical(roster$clause_transferred) else rep(NA, nrow(roster))
    dates <- if ("clause_date" %in% names(roster)) as.character(roster$clause_date) else rep(NA_character_, nrow(roster))
    parsed <- suppressWarnings(as.POSIXct(gsub("Z$", "", gsub("T", " ", dates)), tz = "UTC"))
    closed <- is.finite(clauses) & clauses > 0 & !is.na(transferred) & !transferred & !is.na(parsed) & parsed > now
    prices <- ifelse(closed, ceiling(clauses * (1 + premium / 100)), NA_real_)
    message <- "No players have a valid closed clause to use for this listing method."
  }
  keep <- !is.na(ids) & nzchar(ids) & is.finite(prices) & prices > 0
  entries <- data.frame(player_id = ids[keep], price = prices[keep], already_listed = listed[keep], stringsAsFactors = FALSE)
  list(entries = entries, skipped = as.integer(sum(!keep)), message = message)
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
# Compare immutable player IDs; unavailable lineups never imply an empty XI.
squad_lineup_comparison <- function(submitted, optimized) {
  if (is.null(submitted) || !is.data.frame(submitted$players))
    return(list(status = "unavailable", message = "Submitted lineup is unavailable."))
  if (!isTRUE(optimized$feasible))
    return(list(status = "invalid", message = "The suggested lineup does not meet all constraints."))
  key <- intersect(c("id", "player_id", "_player", "_id", "player.id"), names(submitted$players))
  if (!length(key)) return(list(status = "unavailable", message = "Submitted player identities are unavailable."))
  current <- as.character(submitted$players[[key[1]]])
  target <- as.character(optimized$starting_xi$id)
  changes <- list()
  cfg <- submitted$lineup_config %||% submitted
  old_formation <- fm_scalar(cfg$formation,'')
  if(nzchar(old_formation)&&!identical(old_formation,optimized$formation))
    changes<-c(changes,list(paste('Formation:',old_formation,'→',optimized$formation)))
  captain <- cfg$captain_id %||% cfg$captain
  if(is.list(captain))captain<-captain$id %||% captain$`_id`
  captain <- fm_scalar(captain,'')
  recommended <- fm_scalar(optimized$captain_id,'')
  if(nzchar(captain)&&nzchar(recommended)&&captain!=recommended)
    changes<-c(changes,list(paste('Captain:',captain,'→',recommended,'; highest projected captain contribution.')))
  bench <- submitted$bench$players
  if(is.data.frame(bench)&&'id'%in%names(bench)&&length(optimized$bench_order) &&
     !identical(as.character(bench$id),as.character(optimized$bench_order)))
    changes<-c(changes,list(paste('Bench order:',paste(optimized$bench_order,collapse=', '),'; ordered by projected points among eligible reserves.')))
  position <- intersect(c('pos_group','assigned_position','position'),names(submitted$players))
  if(length(position)&&'pos_group'%in%names(optimized$starting_xi)) {
    old<-as.character(submitted$players[[position[1]]]);new<-optimized$starting_xi$pos_group[match(current,target)]
    known<-old%in%c('GK','DEF','MID','FWD') & !is.na(new)&old!=new
    for(i in which(known))changes<-c(changes,list(paste(current[i],'position:',old[i],'→',new[i])))
  }
  all<-dplyr::bind_rows(optimized$starting_xi,optimized[['bench']])
  for(id in setdiff(target,current)) {
    row<-all[all$id==id,,drop=FALSE]
    label<-if('name'%in%names(row))as.character(row$name[1]) else id
    points<-if('expected_points'%in%names(row))fm_number(row$expected_points[1]) else NA_real_
    changes<-c(changes,list(paste('Add',label,'· projected points:',if(is.finite(points))round(points,1) else 'unavailable','· improves the chosen feasible assignment.')))
  }
  for(id in setdiff(current,target)) {
    row<-all[all$id==id,,drop=FALSE]
    reason<-if(nrow(row)&&any(analytics_known_unavailable(row)))'reported unavailable' else 'lower contribution in the selected formation'
    changes<-c(changes,list(paste('Remove',id,'·',reason)))
  }
  list(status = "ok", add = setdiff(target, current), remove = setdiff(current, target),changes=changes)
}

# Keep user-facing lineup tables limited to selection evidence, never raw API
# records. Missing forecasts remain missing rather than becoming zero points.
squad_lineup_display_rows <- function(players, captain_id = NA_character_, bench_order = character(), starting = FALSE) {
  if (!is.data.frame(players) || !nrow(players)) return(data.frame())
  text <- function(key, default = NA_character_) if (key %in% names(players)) as.character(players[[key]]) else rep(default, nrow(players))
  number <- function(key) suppressWarnings(as.numeric(text(key)))
  ids <- text("id")
  positions <- text("pos_group")
  missing <- is.na(positions) | !nzchar(positions)
  positions[missing] <- text("role")[missing]
  selection <- rep(if (starting) "Starting XI" else "Reserve", nrow(players))
  if (starting) {
    selection[!is.na(ids) & ids %in% captain_id[!is.na(captain_id)]] <- "Captain"
  } else {
    order <- match(ids, bench_order)
    selection[!is.na(order)] <- paste("Bench", order[!is.na(order)])
  }
  data.frame(player = text("name"), position = positions, expected_points = number("expected_points"),
    lower = number("forecast_lower"), upper = number("forecast_upper"), value = number("value"),
    selection = selection, stringsAsFactors = FALSE)
}


# Preference only until a successful per-round hiring contract is captured.
# See docs/coach_hiring.md. No network call or automation job is dispatched.
squad_coach_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class="well well-sm",
    shiny::h4("Automatic coach"),
    shiny::uiOutput(ns("preference")),
    shiny::uiOutput(ns("status")))
}

squad_coach_Server <- function(id, login_token, championship_id, user_team_id) {
  shiny::moduleServer(id, function(input, output, session) {
    preferences <- shiny::reactiveVal(list())
    saved <- shiny::reactiveVal(list())
    context <- shiny::reactive({
      auth <- login_token()
      if(!valid_login(auth)) return(NULL)
      league <- fm_scalar(championship_id(), "")
      team <- fm_scalar(user_team_id(), "")
      if(!nzchar(league) || !nzchar(team)) return(NULL)
      account <- as.character(auth[["userid"]])
      key <- as.character(openssl::sha256(charToRaw(jsonlite::toJSON(
        list(account,league,team),auto_unbox=TRUE))))
      list(account_id=account, championship_id=league, user_team_id=team,
        input_id=paste0("auto_hire_",key), key=key)
    })
    shiny::observeEvent(context(), {
      ctx <- context(); if(is.null(ctx)) return()
      current <- shiny::isolate(preferences())
      if(is.null(current[[ctx$key]]) && exists('read_league_preference',mode='function')) {
        stored <- read_league_preference(ctx$account_id,ctx$championship_id,ctx$user_team_id)
        if(is.logical(stored)&&length(stored)==1L&&!is.na(stored)) {
          current[[ctx$key]] <- stored;preferences(current)
          state<-shiny::isolate(saved());state[[ctx$key]]<-TRUE;saved(state)
        }
      }
    },priority=100)
    preference <- shiny::reactive({
      ctx <- context()
      if(is.null(ctx)) return(NULL)
      enabled <- preferences()[[ctx$key]]
      list(account_id=ctx$account_id,championship_id=ctx$championship_id,
        user_team_id=ctx$user_team_id,enabled=if(is.null(enabled)) TRUE else enabled,
        execution_available=FALSE,reason="hiring_endpoint_unverified")
    })
    output$preference <- shiny::renderUI({
      ctx <- context()
      if(is.null(ctx)) return(shiny::p("Log in and select a league to set this preference."))
      enabled <- shiny::isolate(preference()$enabled)
      shiny::checkboxInput(session$ns(ctx$input_id),
        "Automatically hire the coach each round",value=enabled)
    })
    shiny::observe({
      ctx <- context()
      if(is.null(ctx)) return()
      # A different input identity per account/league/team prevents delayed
      # events from the previous league from changing the current preference.
      value <- input[[ctx$input_id]]
      if(!is.logical(value) || length(value)!=1L || is.na(value)) return()
      current <- shiny::isolate(preferences())
      if(!identical(current[[ctx$key]],value)) {
        current[[ctx$key]] <- value
        preferences(current)
        ok <- if(exists('save_league_preference',mode='function'))
          save_league_preference(ctx$account_id,ctx$championship_id,ctx$user_team_id,value) else FALSE
        state <- shiny::isolate(saved());state[[ctx$key]] <- isTRUE(ok);saved(state)
      }
    })
    output$status <- shiny::renderUI({
      current <- preference()
      if(is.null(current)) return(NULL)
      shiny::tagList(
        shiny::p(class=if(current$enabled) "text-warning" else "text-muted",
          if(current$enabled) "Pending: automatic hiring is not connected yet. Hire the coach in Futmondo for now."
          else "Automatic coach hiring is off for this league."),
        shiny::tags$small(class="text-muted", if(isTRUE(saved()[[context()$key]]))
          "Preference saved for this account and league." else
          "Preference is active in this session; database saving is unavailable."))
    })
    preference
  })
}
