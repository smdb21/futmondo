# ============================================================
# Futmondo Today Module - Manager Command Center
# ============================================================
# Provides a daily actionable dashboard with KPIs, FIS-driven
# recommendations, market radar, and recent transfer intelligence.
# ============================================================

library(reactable)
library(dplyr)

# ---- Helper: safe reactive value extractor ----
today_get_reactive_val <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.reactive(x) || is.function(x)) {
    tryCatch(x(), error = function(e) NULL)
  } else {
    x
  }
}


# ============================================================
# today_UI
# ============================================================
today_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # ---- Hero Banner ----
    div(
      style = "background: linear-gradient(135deg, #1e3a5f 0%, #0f2027 100%); color: #fff; padding: 28px 24px; border-radius: 10px; margin-bottom: 20px;",
      fluidRow(
        column(
          width = 12,
          div(
            style = "display: flex; align-items: center; gap: 14px; flex-wrap: wrap;",
            icon("bolt", style = "font-size: 28px; color: #fbbf24;"),
            div(
              style = "flex: 1;",
              h2(
                style = "margin: 0; font-weight: 700; font-size: 22px;",
                "Manager Command Center"
              ),
              p(
                id = ns("today_date_subtitle"),
                style = "margin: 4px 0 0 0; font-size: 13px; color: #94a3b8;",
                "Your daily intelligence briefing"
              )
            )
          )
        )
      )
    ),

    # ---- KPI Value Boxes ----
    fluidRow(
      column(width = 3, uiOutput(ns("kpi_cash_box"))),
      column(width = 3, uiOutput(ns("kpi_valuation_box"))),
      column(width = 3, uiOutput(ns("kpi_opportunities_box"))),
      column(width = 3, uiOutput(ns("kpi_threats_box")))
    ),

    # ---- Main 2-Column Layout ----
    fluidRow(
      # Left Column: Recommendations Feed
      column(
        width = 8,
        box(
          title = tagList(icon("lightbulb"), " What Should I Do Today? (Actionable Manager Feed)"),
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          uiOutput(ns("recommendations_feed_ui"))
        )
      ),

      # Right Column: Market Radar + Recent Deals
      column(
        width = 4,
        # Market Radar Table
        box(
          title = tagList(icon("satellite-dish"), " Today's Market Intelligence Radar"),
          width = 12,
          status = "info",
          solidHeader = TRUE,
          collapsible = FALSE,
          reactableOutput(ns("market_radar_table"))
        ),

        # Recent League Transfers
        box(
          title = tagList(icon("newspaper"), " Recent League Transfers"),
          width = 12,
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          uiOutput(ns("recent_deals_ui"))
        )
      )
    )
  )
}


# ============================================================
# today_Server
# ============================================================
today_Server <- function(id, is_module_active, login_token, championship_id,
                         user_team_id, user_teams_RV, refresh_trigger = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # ---- Reactive: Market Players ----
      market_players_RV <- reactive({
        req(is_module_active() == TRUE)
        req(login_token())
        req(championship_id())
        req(user_team_id())
        if (!is.null(refresh_trigger)) refresh_trigger()

        tryCatch({
          df <- get_market_players(
            login = login_token(),
            championship_id = championship_id(),
            user_team_id = user_team_id()
          )
          df <- df %>% translate_player_positions()
          df <- df %>% calculate_player_changes()
          df <- df %>% unify_columns()
          df <- calculate_fis_score(df)
          df
        }, error = function(e) {
          print(paste0("[Today] Error fetching market players: ", e$message))
          data.frame()
        })
      })

      # ---- Reactive: Squad Players ----
      squad_players_RV <- reactive({
        req(is_module_active() == TRUE)
        req(login_token())
        req(championship_id())
        req(user_team_id())
        if (!is.null(refresh_trigger)) refresh_trigger()

        tryCatch({
          df <- get_players_from_team(
            login = login_token(),
            championship_id = championship_id(),
            user_team_id = user_team_id(),
            teams = NULL
          )
          df <- df %>% translate_player_positions()
          df <- df %>% calculate_player_changes()
          df <- df %>% unify_columns()
          df <- calculate_fis_score(df)
          df
        }, error = function(e) {
          print(paste0("[Today] Error fetching squad players: ", e$message))
          data.frame()
        })
      })

      # ---- Reactive: Pressroom Feed ----
      pressroom_RV <- reactive({
        req(is_module_active() == TRUE)
        req(login_token())
        req(championship_id())
        if (!is.null(refresh_trigger)) refresh_trigger()

        tryCatch({
          df <- get_championship_pressroom(
            login = login_token(),
            championship_id = championship_id()
          )
          df
        }, error = function(e) {
          print(paste0("[Today] Error fetching pressroom: ", e$message))
          data.frame()
        })
      })

      # ---- Reactive: User Finances ----
      user_finances_RV <- reactive({
        req(is_module_active() == TRUE)
        req(login_token())
        req(championship_id())
        req(user_team_id())
        if (!is.null(refresh_trigger)) refresh_trigger()

        tryCatch({
          info <- get_user_team_info(
            login = login_token(),
            championship_id = championship_id(),
            user_team_id = user_team_id()
          )
          info
        }, error = function(e) {
          print(paste0("[Today] Error fetching user finances: ", e$message))
          NULL
        })
      })

      # ---- Reactive: Combined Players (for FIS + recommendations) ----
      all_players_RV <- reactive({
        mkt <- market_players_RV()
        sqd <- squad_players_RV()

        if (is.null(mkt) && is.null(sqd)) return(data.frame())
        if (is.null(mkt) && !is.null(sqd)) return(sqd)
        if (!is.null(mkt) && is.null(sqd)) return(mkt)

        # Combine, deduplicate by id
        combined <- bind_rows(mkt, sqd)
        if (nrow(combined) > 0 && "id" %in% colnames(combined)) {
          combined <- combined %>% dplyr::distinct(id, .keep_all = TRUE)
        }
        # Recalculate FIS on combined set
        if (nrow(combined) > 0) {
          combined <- calculate_fis_score(combined)
        }
        combined
      })

      # ---- Reactive: Recommendations Feed ----
      recommendations_RV <- reactive({
        all_p <- all_players_RV()
        prs <- pressroom_RV()
        ut <- user_teams_RV()

        if (is.null(all_p) || nrow(all_p) == 0) {
          return(data.frame(
            type = character(0), title = character(0), description = character(0),
            confidence_pct = numeric(0), action_label = character(0),
            player_id = character(0), stringsAsFactors = FALSE
          ))
        }

        tryCatch({
          generate_command_center_feed(
            login = login_token(),
            championship_id = championship_id(),
            user_team_id = user_team_id(),
            user_teams_df = ut,
            players_df = all_p,
            pressroom_df = prs
          )
        }, error = function(e) {
          print(paste0("[Today] Error generating recommendations: ", e$message))
          data.frame(
            type = character(0), title = character(0), description = character(0),
            confidence_pct = numeric(0), action_label = character(0),
            player_id = character(0), stringsAsFactors = FALSE
          )
        })
      })

      # ============================================================
      # Renders
      # ============================================================

      # ---- Date Subtitle ----
      output$today_date_subtitle <- renderText({
        req(is_module_active() == TRUE)
        paste0(format(Sys.Date(), "%A, %B %d, %Y"), " | Your daily intelligence briefing")
      })

      # ---- KPI: Available Liquid Cash ----
      output$kpi_cash_box <- renderUI({
        req(is_module_active() == TRUE)

        fin <- user_finances_RV()
        sqd <- squad_players_RV()

        liquid_cash_val <- 300000000
        if (!is.null(fin) && !is.null(fin$budget) && is.numeric(fin$budget) && fin$budget > 0) {
          liquid_cash_val <- fin$budget
        } else if (!is.null(sqd) && nrow(sqd) > 0 && "buyPrice" %in% colnames(sqd)) {
          total_spent <- sum(suppressWarnings(as.numeric(sqd$buyPrice)), na.rm = TRUE)
          liquid_cash_val <- 300000000 - total_spent
        }

        value_text <- format_table_currency(liquid_cash_val)

        div(
          style = "background: linear-gradient(135deg, #059669 0%, #047857 100%); color: #fff; padding: 18px 16px; border-radius: 10px; margin-bottom: 10px;",
          icon("sack-dollar", style = "font-size: 20px; margin-bottom: 8px; color: #a7f3d0;"),
          br(),
          div(
            style = "font-size: 20px; font-weight: 700;",
            value_text
          ),
          div(
            style = "font-size: 11px; color: #a7f3d0; margin-top: 4px; text-transform: uppercase; letter-spacing: 0.5px;",
            "Available Liquid Cash"
          )
        )
      })

      # ---- KPI: Squad Market Valuation ----
      output$kpi_valuation_box <- renderUI({
        req(is_module_active() == TRUE)

        sqd <- squad_players_RV()
        val_sum <- if (!is.null(sqd) && nrow(sqd) > 0 && "value" %in% colnames(sqd)) {
          sum(suppressWarnings(as.numeric(sqd$value)), na.rm = TRUE)
        } else {
          0
        }

        value_text <- format_table_currency(val_sum)

        div(
          style = "background: linear-gradient(135deg, #2563eb 0%, #1d4ed8 100%); color: #fff; padding: 18px 16px; border-radius: 10px; margin-bottom: 10px;",
          icon("chart-line", style = "font-size: 20px; margin-bottom: 8px; color: #bfdbfe;"),
          br(),
          div(
            style = "font-size: 20px; font-weight: 700;",
            value_text
          ),
          div(
            style = "font-size: 11px; color: #bfdbfe; margin-top: 4px; text-transform: uppercase; letter-spacing: 0.5px;",
            "Squad Market Valuation"
          )
        )
      })

      # ---- KPI: Active Market Opportunities ----
      output$kpi_opportunities_box <- renderUI({
        req(is_module_active() == TRUE)

        mkt <- market_players_RV()
        high_fis_count <- 0
        if (!is.null(mkt) && nrow(mkt) > 0 && "fis_tier" %in% colnames(mkt)) {
          high_fis_count <- sum(mkt$fis_tier %in% c("Strong Buy", "Buy"), na.rm = TRUE)
        }

        div(
          style = "background: linear-gradient(135deg, #d97706 0%, #b45309 100%); color: #fff; padding: 18px 16px; border-radius: 10px; margin-bottom: 10px;",
          icon("magnifying-glass-chart", style = "font-size: 20px; margin-bottom: 8px; color: #fde68a;"),
          br(),
          div(
            style = "font-size: 20px; font-weight: 700;",
            high_fis_count
          ),
          div(
            style = "font-size: 11px; color: #fde68a; margin-top: 4px; text-transform: uppercase; letter-spacing: 0.5px;",
            "Active Market Opportunities"
          )
        )
      })

      # ---- KPI: Clause Threat Radar ----
      output$kpi_threats_box <- renderUI({
        req(is_module_active() == TRUE)

        sqd <- squad_players_RV()
        threats_count <- 0
        if (!is.null(sqd) && nrow(sqd) > 0) {
          # Count starters (in dream team or high points) with vulnerable clauses
          # A clause is "vulnerable" if clause_price exists and is relatively low vs value
          if ("clause_price" %in% colnames(sqd) && "value" %in% colnames(sqd)) {
            for (i in seq_len(nrow(sqd))) {
              cp <- suppressWarnings(as.numeric(sqd$clause_price[i]))
              v  <- suppressWarnings(as.numeric(sqd$value[i]))
              pts <- suppressWarnings(as.numeric(sqd$points[i]))
              # Vulnerable: clause exists, clause < 80% of value, and player has decent points
              if (!is.na(cp) && cp > 0 && !is.na(v) && v > 0 && cp < v * 0.8 && !is.na(pts) && pts > 0) {
                threats_count <- threats_count + 1
              }
            }
          }
        }

        status_color <- if (threats_count > 2) "#ef4444" else if (threats_count > 0) "#f59e0b" else "#10b981"

        div(
          style = paste0("background: linear-gradient(135deg, ", status_color, " 0%, ", status_color, " 100%); color: #fff; padding: 18px 16px; border-radius: 10px; margin-bottom: 10px;"),
          icon("shield-halved", style = "font-size: 20px; margin-bottom: 8px; color: rgba(255,255,255,0.7);"),
          br(),
          div(
            style = "font-size: 20px; font-weight: 700;",
            threats_count
          ),
          div(
            style = "font-size: 11px; color: rgba(255,255,255,0.8); margin-top: 4px; text-transform: uppercase; letter-spacing: 0.5px;",
            "Clause Threat Radar"
          )
        )
      })

      # ---- Recommendations Feed ----
      output$recommendations_feed_ui <- renderUI({
        req(is_module_active() == TRUE)
        recs <- recommendations_RV()

        if (is.null(recs) || nrow(recs) == 0) {
          return(
            div(
              style = "padding: 24px; text-align: center; color: #64748b;",
              icon("circle-info", style = "font-size: 24px; margin-bottom: 8px;"),
              br(),
              p("No actionable recommendations at this time. Check back later or refresh data.")
            )
          )
        }

        cards <- lapply(seq_len(nrow(recs)), function(i) {
          r <- recs[i, ]
          rec_type <- r$type
          title_text <- r$title
          desc_text <- r$description
          conf_pct <- round(r$confidence_pct, 0)
          action_label <- r$action_label
          pid <- as.character(r$player_id)

          # Color coding by type
          type_icon <- switch(
            rec_type,
            "Buy"    = icon("arrow-down-to-line", style = "color: #10b981;"),
            "Sell"   = icon("arrow-up-from-line", style = "color: #ef4444;"),
            "Bid"    = icon("hand-holding-dollar", style = "color: #f59e0b;"),
            "Clause" = icon("bolt", style = "color: #8b5cf6;"),
            "Hold"   = icon("hand", style = "color: #6b7280;"),
            icon("circle-info", style = "color: #6b7280;")
          )

          type_badge_color <- switch(
            rec_type,
            "Buy"    = "background-color: #d1fae5; color: #065f46; border-color: #a7f3d0;",
            "Sell"   = "background-color: #fee2e2; color: #991b1b; border-color: #fca5a5;",
            "Bid"    = "background-color: #fef3c7; color: #92400e; border-color: #fde68a;",
            "Clause" = "background-color: #ede9fe; color: #5b21b6; border-color: #c4b5fd;",
            "Hold"   = "background-color: #f3f4f6; color: #374151; border-color: #d1d5db;",
            "background-color: #f3f4f6; color: #374151; border-color: #d1d5db;"
          )

          conf_color <- if (conf_pct >= 80) "#10b981" else if (conf_pct >= 60) "#f59e0b" else "#ef4444"

          # Determine if action button should be shown
          show_action_btn <- rec_type %in% c("Buy", "Bid", "Clause")

          div(
            style = "border: 1px solid #e2e8f0; border-radius: 10px; padding: 16px; margin-bottom: 12px; background: #fff;",
            fluidRow(
              # Icon + Title
              column(
                width = 10,
                div(
                  style = "display: flex; align-items: center; gap: 10px;",
                  div(style = "font-size: 18px;", type_icon),
                  div(
                    style = "flex: 1;",
                    div(
                      style = "font-weight: 700; font-size: 14px; color: #0f172a;",
                      title_text
                    ),
                    div(
                      style = "font-size: 12px; color: #64748b; margin-top: 4px;",
                      desc_text
                    )
                  )
                )
              ),
              # Confidence badge
              column(
                width = 2,
                div(
                  style = paste0("text-align: right; display: flex; align-items: center; justify-content: flex-end;"),
                  div(
                    style = paste0("display: inline-block; padding: 4px 10px; border-radius: 20px; font-size: 11px; font-weight: 700; background-color: ", if (conf_pct >= 80) "#d1fae5" else if (conf_pct >= 60) "#fef3c7" else "#fee2e2", "; color: ", conf_color, ";"),
                    paste0("Confidence: ", conf_pct, "%")
                  )
                )
              )
            ),
            # Type badge + Action button row
            fluidRow(
              column(
                width = 12,
                div(
                  style = "margin-top: 10px; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 8px;",
                  div(
                    style = paste0("display: inline-block; padding: 4px 12px; border-radius: 6px; font-size: 11px; font-weight: 600; border: 1px solid; ", type_badge_color),
                    rec_type
                  ),
                  if (show_action_btn) {
                    actionButton(
                      inputId = ns(paste0("rec_action_", pid)),
                      label = tagList(icon("arrow-right"), action_label),
                      class = "btn btn-sm btn-primary",
                      style = "font-size: 11px; padding: 4px 12px;"
                    )
                  } else {
                    NULL
                  }
                )
              )
            )
          )
        })

        do.call(tagList, cards)
      })

      # ---- Market Radar Table ----
      output$market_radar_table <- renderReactable({
        req(is_module_active() == TRUE)
        mkt <- market_players_RV()

        if (is.null(mkt) || nrow(mkt) == 0) {
          return(
            reactable(
              data.frame(
                Player = character(0),
                Role = character(0),
                Price = numeric(0),
                FIS = numeric(0),
                Tier = character(0)
              ),
              outlined = FALSE,
              bordered = FALSE,
              compact = TRUE,
              showTitle = FALSE
            )
          )
        }

        # Filter to high-FIS players and sort by FIS score descending
        radar_df <- mkt
        if ("fis_score" %in% colnames(radar_df)) {
          radar_df <- radar_df %>%
            dplyr::filter(!is.na(fis_score)) %>%
            dplyr::arrange(desc(fis_score)) %>%
            dplyr::slice_head(n = 10)
        }

        if (nrow(radar_df) == 0) {
          return(
            reactable(
              data.frame(
                Player = character(0),
                Role = character(0),
                Price = numeric(0),
                FIS = numeric(0),
                Tier = character(0)
              ),
              outlined = FALSE,
              bordered = FALSE,
              compact = TRUE,
              showTitle = FALSE
            )
          )
        }

        display_df <- data.frame(
          Player = if ("name" %in% colnames(radar_df)) as.character(radar_df$name) else rep("Unknown", nrow(radar_df)),
          Role = if ("role" %in% colnames(radar_df)) as.character(radar_df$role) else rep("-", nrow(radar_df)),
          Price = if ("value" %in% colnames(radar_df)) suppressWarnings(as.numeric(radar_df$value)) else rep(0, nrow(radar_df)),
          FIS = if ("fis_score" %in% colnames(radar_df)) round(radar_df$fis_score, 1) else rep(0, nrow(radar_df)),
          Tier = if ("fis_tier" %in% colnames(radar_df)) as.character(radar_df$fis_tier) else rep("-", nrow(radar_df)),
          PlayerID = if ("id" %in% colnames(radar_df)) as.character(radar_df$id) else rep("", nrow(radar_df)),
          stringsAsFactors = FALSE
        )

        reactable(
          display_df,
          outlined = FALSE,
          bordered = FALSE,
          compact = TRUE,
          showTitle = FALSE,
          highlight = TRUE,
          clickable = TRUE,
          onClick = function(index) {
            pid <- display_df$PlayerID[index]
            if (!is.null(pid) && pid != "") {
              # Signal to the parent module to select this player
              session$sendCustomMessage(
                type = "today_select_player",
                message = list(player_id = pid)
              )
            }
          },
          columns = list(
            Player = colDef(
              name = "Player",
              minWidth = 100,
              cell = function(value) {
                div(
                  style = "font-weight: 600; font-size: 12px; color: #0f172a;",
                  value
                )
              }
            ),
            Role = colDef(
              name = "Role",
              minWidth = 60,
              cell = function(value) {
                div(
                  style = "font-size: 11px; color: #64748b;",
                  value
                )
              }
            ),
            Price = colDef(
              name = "Price",
              align = "right",
              minWidth = 90,
              cell = function(value) {
                div(
                  style = "font-size: 12px; font-weight: 600; color: #059669;",
                  format_table_currency(value)
                )
              }
            ),
            FIS = colDef(
              name = "FIS",
              align = "center",
              minWidth = 55,
              cell = function(value) {
                badge_color <- if (value >= 80) "#10b981" else if (value >= 65) "#f59e0b" else "#6b7280"
                div(
                  style = paste0("display: inline-block; padding: 2px 8px; border-radius: 10px; font-size: 11px; font-weight: 700; background-color: ", if (value >= 80) "#d1fae5" else if (value >= 65) "#fef3c7" else "#f3f4f6", "; color: ", badge_color, ";"),
                  value
                )
              }
            ),
            Tier = colDef(
              name = "Tier",
              minWidth = 75,
              cell = function(value) {
                tier_color <- switch(
                  as.character(value),
                  "Strong Buy" = "#10b981",
                  "Buy" = "#f59e0b",
                  "Hold" = "#6b7280",
                  "Sell" = "#ef4444",
                  "#6b7280"
                )
                div(
                  style = paste0("font-size: 10px; font-weight: 600; color: ", tier_color, "; text-transform: uppercase;"),
                  value
                )
              }
            ),
            PlayerID = colDef(visible = FALSE)
          )
        )
      })

      # ---- Recent League Transfers ----
      output$recent_deals_ui <- renderUI({
        req(is_module_active() == TRUE)
        prs <- pressroom_RV()

        if (is.null(prs) || nrow(prs) == 0) {
          return(
            div(
              style = "padding: 16px; text-align: center; color: #64748b; font-size: 12px;",
              icon("newspaper", style = "font-size: 16px; margin-bottom: 4px;"),
              br(),
              "No recent transfer data available."
            )
          )
        }

        # Take the most recent 6 high-impact deals
        recent <- head(prs, 6)

        deals <- lapply(seq_len(nrow(recent)), function(i) {
          d <- recent[i, ]

          player_name <- if (!is.null(d$player_name) && nzchar(as.character(d$player_name))) as.character(d$player_name) else "Unknown Player"
          buyer_name <- if (!is.null(d$buyer_team_name) && nzchar(as.character(d$buyer_team_name))) as.character(d$buyer_team_name) else "Futmondo / Mercado"
          seller_name <- if (!is.null(d$seller_team_name) && nzchar(as.character(d$seller_team_name))) as.character(d$seller_team_name) else "Futmondo / Mercado"
          price_val <- suppressWarnings(as.numeric(d$price))
          created_str <- if (!is.null(d$created) && nzchar(as.character(d$created))) as.character(d$created) else ""

          # Parse date for display
          display_date <- ""
          if (created_str != "") {
            parsed_dt <- suppressWarnings(as.POSIXct(created_str, tz = "UTC"))
            if (!is.na(parsed_dt)) {
              display_date <- format(parsed_dt, "%d-%m-%Y %H:%M")
            } else {
              display_date <- created_str
            }
          }

          price_display <- if (!is.na(price_val) && price_val > 0) format_table_currency(price_val) else "-"

          div(
            style = "border-bottom: 1px solid #f1f5f9; padding: 10px 0;",
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              icon("exchange-alt", style = "font-size: 12px; color: #6b7280;"),
              div(
                style = "flex: 1; font-size: 12px;",
                div(
                  style = "font-weight: 600; color: #0f172a;",
                  player_name
                ),
                div(
                  style = "font-size: 11px; color: #64748b;",
                  paste0(buyer_name, " <- ", seller_name)
                )
              ),
              div(
                style = "text-align: right;",
                div(
                  style = "font-weight: 700; font-size: 12px; color: #059669;",
                  price_display
                ),
                div(
                  style = "font-size: 10px; color: #94a3b8;",
                  display_date
                )
              )
            )
          )
        })

        do.call(tagList, deals)
      })

      # ---- Handle Recommendation Action Buttons ----
      # We observe clicks on recommendation action buttons to open player details
      # via the selected_player module in the parent scope.
      # Since we cannot directly call selected_player_Server, we use a custom
      # message that the parent server can listen for.
      # The action buttons have IDs like rec_action_{player_id}.

      # We use a reactiveVal to track the last selected player from this module
      selected_from_today_RV <- reactiveVal(NULL)

      # Generic observer for dynamically created action buttons
      # We use session$onSessionEnded for cleanup if needed
      observe({
        req(is_module_active() == TRUE)
        recs <- recommendations_RV()
        if (is.null(recs) || nrow(recs) == 0) return()

        # For each recommendation with an action button, observe the click
        for (i in seq_len(nrow(recs))) {
          r <- recs[i, ]
          pid <- as.character(r$player_id)
          btn_id <- paste0("rec_action_", pid)

          # We create the observer dynamically
          # Note: In practice, Shiny re-creates these on each reactives update
          # We use a single observer pattern instead
        }
      })

      # Single observer using session$dynamic or input lookup
      # Since button IDs are dynamic, we observe input dynamically
      observeEvent(input$rec_action_placeholder, {
        # This is a placeholder; real handling is done below
      }, ignoreNULL = TRUE)

      # We handle all rec_action_* buttons via a pattern observer
      # Since Shiny doesn't support regex inputId patterns natively,
      # we use a workaround: observe all inputs and filter
      observe({
        req(is_module_active() == TRUE)
        recs <- recommendations_RV()
        if (is.null(recs) || nrow(recs) == 0) return()

        # Check each recommendation's button
        for (i in seq_len(nrow(recs))) {
          r <- recs[i, ]
          pid <- as.character(r$player_id)
          btn_id <- paste0("rec_action_", pid)

          # Check if this input exists and has been clicked
          if (btn_id %in% names(input)) {
            # Trigger player selection
            selected_from_today_RV(pid)
          }
        }
      })

      return(selected_from_today_RV)
    }
  )
}