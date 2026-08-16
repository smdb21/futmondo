library(reactable)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)

# ---- Table Definitions ----
get_table_definitions <- function() {
  data.frame(
    table_name = c(
      "championships",
      "real_clubs",
      "players",
      "user_teams",
      "user_team_history",
      "player_history",
      "market_transactions",
      "round_dream_team"
    ),
    description = c(
      "Active championships and league metadata",
      "Real-world football clubs with logos",
      "Full player catalog from the API",
      "User-managed teams within a championship",
      "Historical snapshot of user team standings",
      "Historical snapshot of player valuations",
      "Market transfer and clause transactions",
      "Best 11 (Dream Team) and MVP player accolades per round"
    ),
    primary_key = c(
      "id (text)",
      "id (text)",
      "id (text)",
      "id (text)",
      "id (bigint)",
      "id (bigint)",
      "id (bigint)",
      "id (BIGSERIAL)"
    ),
    stringsAsFactors = FALSE
  )
}

admin_UI <- function(id) {
  ns <- NS(id)

  tagList(
    # ---- Telemetry KPI row ----
    fluidRow(
      column(width = 3,
             box(
               title = "Connection Status",
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               collapsible = FALSE,
               div(style = "text-align: center; padding: 10px 0;",
                   div(style = "font-size: 32px; margin-bottom: 4px;",
                       icon("server", style = "color: #3b82f6;")
                   ),
                   div(style = "font-size: 13px; color: #64748b;",
                       span("Supabase", style = "font-weight: 600; color: #0f172a;")
                   ),
                   div(style = "font-size: 11px; color: #94a3b8;",
                       span("Live API endpoint")
                   )
               )
             )
      ),
      column(width = 3,
             box(
               title = "Admin Email",
               width = 12,
               status = "info",
               solidHeader = TRUE,
               collapsible = FALSE,
               div(style = "text-align: center; padding: 10px 0;",
                   div(style = "font-size: 32px; margin-bottom: 4px;",
                       icon("user-shield", style = "color: #8b5cf6;")
                   ),
                   div(style = "font-size: 13px; color: #64748b;",
                       span("Authorized Operator", style = "font-weight: 600; color: #0f172a;")
                   ),
                   div(style = "font-size: 11px; color: #94a3b8;",
                       span("From .Renviron")
                   )
               )
             )
      ),
      column(width = 3,
             box(
               title = "Total Tables",
               width = 12,
               status = "success",
               solidHeader = TRUE,
               collapsible = FALSE,
               div(style = "text-align: center; padding: 10px 0;",
                   div(style = "font-size: 32px; margin-bottom: 4px;",
                       icon("table", style = "color: #10b981;")
                   ),
                   div(style = "font-size: 13px; color: #64748b;",
                       span("8 Tables", style = "font-weight: 600; color: #0f172a;")
                   ),
                   div(style = "font-size: 11px; color: #94a3b8;",
                       span("Schema verified")
                   )
               )
             )
      ),
      column(width = 3,
             box(
               title = "Total DB Records",
               width = 12,
               status = "warning",
               solidHeader = TRUE,
               collapsible = FALSE,
               div(style = "text-align: center; padding: 10px 0;",
                   div(style = "font-size: 32px; margin-bottom: 4px;",
                       icon("database", style = "color: #f59e0b;")
                   ),
                   div(style = "font-size: 13px; color: #64748b;",
                       span("Dynamic Count", style = "font-weight: 600; color: #0f172a;")
                   ),
                   div(style = "font-size: 11px; color: #94a3b8;",
                       span("Refresh to update")
                   )
               )
             )
      )
    ),

    # ---- Per-table KPI row ----
    fluidRow(
      column(width = 12,
             box(
               title = "Table Record Counts",
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               fluidRow(
                 column(width = 3,
                        div(style = "background: #f8fafc; padding: 12px; border-radius: 6px; border: 1px solid #e2e8f0; text-align: center; margin-bottom: 8px;",
                            div(style = "font-size: 11px; color: #64748b; font-weight: 600; text-transform: uppercase;", "Championships"),
                            div(style = "font-size: 22px; font-weight: 700; color: #3b82f6;",
                                textOutput(ns("kpi_championships"))
                            )
                        )
                 ),
                 column(width = 3,
                        div(style = "background: #f8fafc; padding: 12px; border-radius: 6px; border: 1px solid #e2e8f0; text-align: center; margin-bottom: 8px;",
                            div(style = "font-size: 11px; color: #64748b; font-weight: 600; text-transform: uppercase;", "Real Clubs"),
                            div(style = "font-size: 22px; font-weight: 700; color: #10b981;",
                                textOutput(ns("kpi_real_clubs"))
                            )
                        )
                 ),
                 column(width = 3,
                        div(style = "background: #f8fafc; padding: 12px; border-radius: 6px; border: 1px solid #e2e8f0; text-align: center; margin-bottom: 8px;",
                            div(style = "font-size: 11px; color: #64748b; font-weight: 600; text-transform: uppercase;", "Players"),
                            div(style = "font-size: 22px; font-weight: 700; color: #8b5cf6;",
                                textOutput(ns("kpi_players"))
                            )
                        )
                 ),
                 column(width = 3,
                        div(style = "background: #f8fafc; padding: 12px; border-radius: 6px; border: 1px solid #e2e8f0; text-align: center; margin-bottom: 8px;",
                            div(style = "font-size: 11px; color: #64748b; font-weight: 600; text-transform: uppercase;", "User Teams"),
                            div(style = "font-size: 22px; font-weight: 700; color: #f59e0b;",
                                textOutput(ns("kpi_user_teams"))
                            )
                        )
                 )
               ),
               fluidRow(
                 column(width = 3,
                        div(style = "background: #f8fafc; padding: 12px; border-radius: 6px; border: 1px solid #e2e8f0; text-align: center; margin-bottom: 8px;",
                            div(style = "font-size: 11px; color: #64748b; font-weight: 600; text-transform: uppercase;", "Team History"),
                            div(style = "font-size: 22px; font-weight: 700; color: #06b6d4;",
                                textOutput(ns("kpi_user_team_history"))
                            )
                        )
                 ),
                 column(width = 3,
                        div(style = "background: #f8fafc; padding: 12px; border-radius: 6px; border: 1px solid #e2e8f0; text-align: center; margin-bottom: 8px;",
                            div(style = "font-size: 11px; color: #64748b; font-weight: 600; text-transform: uppercase;", "Player History"),
                            div(style = "font-size: 22px; font-weight: 700; color: #ec4899;",
                                textOutput(ns("kpi_player_history"))
                            )
                        )
                 ),
column(width = 3,
                         div(style = "background: #f8fafc; padding: 12px; border-radius: 6px; border: 1px solid #e2e8f0; text-align: center; margin-bottom: 8px;",
                             div(style = "font-size: 11px; color: #64748b; font-weight: 600; text-transform: uppercase;", "Market Transactions"),
                             div(style = "font-size: 22px; font-weight: 700; color: #ef4444;",
                                 textOutput(ns("kpi_market_transactions"))
                             )
                         )
                  ),
                  column(width = 3,
                         div(style = "background: #f8fafc; padding: 12px; border-radius: 6px; border: 1px solid #e2e8f0; text-align: center; margin-bottom: 8px;",
                             div(style = "font-size: 11px; color: #64748b; font-weight: 600; text-transform: uppercase;", "Round Dream Team"),
                             div(style = "font-size: 22px; font-weight: 700; color: #d97706;",
                                 textOutput(ns("kpi_round_dream_team"))
                             )
                         )
                  )
                ),
                fluidRow(
                  column(width = 6,
                         div(style = "background: #f8fafc; padding: 12px; border-radius: 6px; border: 1px solid #e2e8f0; text-align: center; margin-bottom: 8px;",
                             div(style = "font-size: 11px; color: #64748b; font-weight: 600; text-transform: uppercase;", "Grand Total"),
                             div(style = "font-size: 22px; font-weight: 700; color: #0f172a;",
                                 textOutput(ns("kpi_total_records"))
                             )
                         )
                  )
                )
             )
      )
    ),

    # ---- Main content: left table + right operations ----
    fluidRow(
      # Left column: Database Tables Overview
      column(width = 8,
             box(
               title = "Database Tables Overview",
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               collapsible = FALSE,
               reactable::reactableOutput(ns("tables_stats_table")),
               div(style = "margin-top: 12px; text-align: right;",
                   actionButton(
                     inputId = ns("btn_refresh_stats"),
                     label = "Refresh Stats",
                     icon = icon("rotate"),
                     class = "btn-primary"
                   )
               )
             )
      ),

      # Right column: Database Operations & Maintenance
      column(width = 4,
             box(
               title = "Database Operations & Maintenance",
               width = 12,
               status = "warning",
               solidHeader = TRUE,
               collapsible = FALSE,

# Populate Entire Database button
                div(style = "margin-bottom: 16px;",
                    actionButton(
                      inputId = ns("btn_populate_db"),
                      label = "Populate Entire Database",
                      icon = icon("cloud-arrow-down"),
                      class = "btn-success"
                    ),
                    p(style = "color: #64748b; font-size: 11px; margin-top: 6px;",
                      "Fetches and syncs all data from the Futmondo API into every Supabase table."
                    )
                ),

                # Verify Schema button
                div(style = "margin-bottom: 16px;",
                    actionButton(
                      inputId = ns("btn_verify_db"),
                      label = "Verify Tables Schema",
                      icon = icon("shield-halved"),
                      class = "btn-default"
                    ),
                    p(style = "color: #64748b; font-size: 11px; margin-top: 6px;",
                      "Checks that all 7 required tables exist and respond via the Supabase REST API."
                    )
                ),

               # Sync Round Dream Teams button
                div(style = "margin-bottom: 16px;",
                    actionButton(
                      inputId = ns("btn_sync_dreamteams"),
                      label = "Sync Round Dream Teams",
                      icon = icon("trophy"),
                      class = "btn-info"
                    ),
                    p(style = "color: #64748b; font-size: 11px; margin-top: 6px;",
                      "Verifies and syncs the Best 11 (Dream Team) and MVP accolades for all finished matchdays, reconciling delayed matches."
                    )
                ),

                # Danger Zone
               div(style = "border: 2px solid #fecaca; border-radius: 8px; padding: 16px; background: #fef2f2;",
                   div(style = "display: flex; align-items: center; gap: 8px; margin-bottom: 12px;",
                       icon("triangle-exclamation", style = "color: #ef4444; font-size: 20px;"),
                       span(style = "font-weight: 700; color: #991b1b; font-size: 14px;", "Danger Zone")
                   ),
                   p(style = "color: #991b1b; font-size: 12px; margin-bottom: 12px;",
                     "Resetting the database will permanently delete all records across every table. This action cannot be undone."
                   ),
                   actionButton(
                     inputId = ns("btn_reset_db"),
                     label = "Reset Entire Database",
                     icon = icon("trash-can"),
                     class = "btn-danger"
                   )
               )
             )
      )
    )
  )
}

admin_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Reactive: table row counts ----
    row_counts_df <- reactiveVal(NULL)

    load_row_counts <- function() {
      tryCatch({
        df <- get_table_row_counts()
        row_counts_df(df)
      }, error = function(e) {
        print(paste0("[Admin] Error loading row counts: ", e$message))
        row_counts_df(NULL)
      })
    }

    # Load counts when the module becomes active
    observe({
      if (isTRUE(is_module_active())) {
        load_row_counts()
      }
    })

    # Load counts on manual refresh
    observeEvent(input$btn_refresh_stats, {
      load_row_counts()
    })

    # ---- Helper: get count for a table ----
    get_count <- function(tbl_name) {
      df <- row_counts_df()
      if (is.null(df) || nrow(df) == 0) return("N/A")
      row <- df[df$table_name == tbl_name, , drop = FALSE]
      if (nrow(row) == 0 || is.na(row$row_count)) return("N/A")
      formatC(row$row_count, format = "f", big.mark = ",", digits = 0)
    }

    # ---- KPI text outputs ----
    output$kpi_championships <- renderText({ get_count("championships") })
    output$kpi_real_clubs <- renderText({ get_count("real_clubs") })
    output$kpi_players <- renderText({ get_count("players") })
    output$kpi_user_teams <- renderText({ get_count("user_teams") })
    output$kpi_user_team_history <- renderText({ get_count("user_team_history") })
    output$kpi_player_history <- renderText({ get_count("player_history") })
    output$kpi_market_transactions <- renderText({ get_count("market_transactions") })
    output$kpi_round_dream_team <- renderText({ get_count("round_dream_team") })
    output$kpi_total_records <- renderText({
      df <- row_counts_df()
      if (is.null(df) || nrow(df) == 0) return("N/A")
      total <- sum(df$row_count, na.rm = TRUE)
      if (any(is.na(df$row_count))) return("N/A")
      formatC(total, format = "f", big.mark = ",", digits = 0)
    })

    # ---- Tables Stats Table ----
    output$tables_stats_table <- reactable::renderReactable({
      req(is_module_active() == TRUE)

      table_defs <- get_table_definitions()
      rc <- row_counts_df()

      # Merge row counts into table definitions
      if (!is.null(rc) && nrow(rc) > 0) {
        table_defs <- table_defs %>%
          dplyr::left_join(rc, by = "table_name")
      } else {
        table_defs$row_count <- NA_integer_
      }

      # Determine live status
      table_defs$live_status <- ifelse(
        !is.na(table_defs$row_count) & table_defs$row_count >= 0,
        "Active",
        "Unknown"
      )

      reactable::reactable(
        table_defs,
        compact = TRUE,
        striped = TRUE,
        highlight = TRUE,
        bordered = FALSE,
        defaultPageSize = 7,
        searchable = FALSE,
        sortable = TRUE,
        columns = list(
          table_name = colDef(
            name = "Table Name",
            align = "left",
            cell = function(val) {
              shiny::tags$span(style = "font-weight: 600; color: #0f172a;", val)
            }
          ),
          row_count = colDef(
            name = "Row Count",
            align = "center",
            cell = function(val) {
              if (is.na(val)) {
                shiny::tags$span(style = "color: #94a3b8;", "N/A")
              } else {
                formatted <- formatC(val, format = "f", big.mark = ",", digits = 0)
                shiny::tags$span(
                  style = "background: #eff6ff; color: #1d4ed8; padding: 2px 10px; border-radius: 12px; font-weight: 600; font-size: 13px;",
                  formatted
                )
              }
            }
          ),
          primary_key = colDef(
            name = "Primary Key",
            align = "center",
            cell = function(val) {
              shiny::tags$span(style = "color: #64748b; font-size: 12px;", val)
            }
          ),
          description = colDef(
            name = "Description",
            align = "left",
            cell = function(val) {
              shiny::tags$span(style = "color: #334155; font-size: 12px;", val)
            }
          ),
          live_status = colDef(
            name = "Live Status",
            align = "center",
            cell = function(val) {
              if (val == "Active") {
                shiny::tags$span(
                  style = "color: #10b981; font-weight: 600; font-size: 12px;",
                  icon("circle-check", style = "margin-right: 4px;"),
                  val
                )
              } else {
                shiny::tags$span(
                  style = "color: #f59e0b; font-weight: 600; font-size: 12px;",
                  icon("circle-exclamation", style = "margin-right: 4px;"),
                  val
                )
              }
            }
          )
        )
      )
    })

    # ---- Verify Schema ----
    observeEvent(input$btn_verify_db, {
      result <- tryCatch({
        ok <- init_supabase_db(verbose = TRUE)
        if (ok) {
          showNotification(
            "All 7 tables verified successfully. Schema is intact.",
            type = "message",
            duration = 6
          )
        } else {
          showNotification(
            "Schema verification completed with warnings. Check the console for details.",
            type = "warning",
            duration = 8
          )
        }
      }, error = function(e) {
        showNotification(
          paste0("Schema verification error: ", e$message),
          type = "error",
          duration = 8
        )
        NULL
      })
    })

    # ---- Populate Entire Database ----
    observeEvent(input$btn_populate_db, {
      # Check prerequisites
      if (is.null(login_token()) || login_token() == "") {
        showNotification(
          "Cannot populate database: no login token available. Please log in first.",
          type = "error",
          duration = 8
        )
        return()
      }
      if (is.null(championship_id()) || championship_id() == "") {
        showNotification(
          "Cannot populate database: no championship ID available. Please select an active championship first.",
          type = "error",
          duration = 8
        )
        return()
      }

      showNotification(
        "Populating database tables from Futmondo API...",
        type = "message",
        duration = 10
      )

      tryCatch({
        populate_entire_database(
          login = login_token(),
          championship_id = championship_id(),
          verbose = TRUE
        )

        showNotification(
          "Database population complete. Refreshing stats...",
          type = "message",
          duration = 6
        )

        # Immediately refresh the row counts so the dashboard updates
        load_row_counts()
      }, error = function(e) {
        showNotification(
          paste0("Database population failed: ", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # ---- Sync Round Dream Teams ----
    observeEvent(input$btn_sync_dreamteams, {
      # Check prerequisites
      if (is.null(login_token()) || login_token() == "") {
        showNotification(
          "Cannot sync dream teams: no login token available. Please log in first.",
          type = "error",
          duration = 8
        )
        return()
      }
      if (is.null(championship_id()) || championship_id() == "") {
        showNotification(
          "Cannot sync dream teams: no championship ID available. Please select an active championship first.",
          type = "error",
          duration = 8
        )
        return()
      }

      showNotification(
        "Syncing round dream teams and MVP accolades...",
        type = "message",
        duration = 10
      )

      tryCatch({
        result <- sync_all_championship_dreamteams(
          login = login_token(),
          championship_id = championship_id(),
          verbose = TRUE
        )

        summary_msg <- if (!is.null(result) && length(result) > 0) {
          paste0("Dream team sync complete. ", paste(names(result), result, sep = ": ", collapse = "; "))
        } else {
          "Dream team sync complete."
        }

        showNotification(
          summary_msg,
          type = "message",
          duration = 8
        )

        # Refresh row counts so the dashboard updates
        load_row_counts()
      }, error = function(e) {
        showNotification(
          paste0("Dream team sync failed: ", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # ---- Reset Database: confirmation modal ----
    observeEvent(input$btn_reset_db, {
      showModal(
        modalDialog(
          title = div(
            style = "display: flex; align-items: center; gap: 8px;",
            icon("triangle-exclamation", style = "color: #ef4444;"),
            "Confirm Database Reset"
          ),
          p(style = "color: #334155; font-size: 14px;",
            "This will permanently delete ALL records from every table in the Supabase database."
          ),
          p(style = "color: #991b1b; font-weight: 700; font-size: 13px;",
            "This action cannot be undone."
          ),
          div(
            style = "background: #fef2f2; border: 1px solid #fecaca; border-radius: 6px; padding: 12px; margin-top: 12px;",
            p(style = "color: #991b1b; font-size: 12px; margin: 0;",
              "Affected tables: championships, real_clubs, players, user_teams, user_team_history, player_history, market_transactions"
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              inputId = "btn_confirm_reset",
              label = "Yes, Reset Everything",
              icon = icon("trash-can"),
              class = "btn-danger"
            )
          ),
          easyClose = TRUE,
          size = "s"
        )
      )
    })

    # ---- Reset Database: execute ----
    observeEvent(input$btn_confirm_reset, {
      tryCatch({
        results <- supabase_reset_database(force = TRUE)

        # Build summary message
        summary_lines <- character(0)
        for (tbl in names(results)) {
          summary_lines <- c(summary_lines, paste0("  ", tbl, ": ", results[[tbl]]))
        }
        summary_text <- paste0("Database reset complete.\n", paste(summary_lines, collapse = "\n"))

        showNotification(
          summary_text,
          type = "message",
          duration = 12
        )

        # Close modal
        removeModal()

        # Trigger immediate stats refresh so counts show 0
        load_row_counts()
      }, error = function(e) {
        showNotification(
          paste0("Database reset failed: ", e$message),
          type = "error",
          duration = 10
        )
        removeModal()
      })
    })
  })
}