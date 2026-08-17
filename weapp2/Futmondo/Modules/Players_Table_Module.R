library(reactable)

players_table_UI <- function(id, box_title = NULL,
                             solidHeader = TRUE,
                             status = "primary",
                             filter_by_position = TRUE,
                             filter_by_team = TRUE,
                             filter_by_value = TRUE,
                             filter_by_change_value = TRUE,
                             default_minimum_change_value = NA,
                             filter_by_active_clause = TRUE,
                             filter_by_is_favorite = TRUE,
                             filter_by_is_from_futmondo = TRUE,
filter_by_players_with_bid = FALSE,
                             show_position_breakdown = FALSE,
                             hide_bid_column = FALSE) {
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      width = 12,
      solidHeader = solidHeader,
      status = status,
      title = box_title,

      # Top Filter Bar Grid
      if (show_position_breakdown) {
        fluidRow(
          column(width = 7,
                 fluidRow(
                   style = "background: #f8fafc; padding: 15px; border-radius: 8px; margin: 0 0 20px 0; border: 1px solid #e2e8f0;",
if (filter_by_position) {
                      column(width = 4, div(title = "Filter players by primary or secondary position (Goalkeeper, Defender, Midfielder, Forward)", selectInput(inputId = ns("position_filter"), label = "Position", choices = c("All", "Goalkeeper", "Defender", "Midfielder", "Forward"), selected = "All", width = "100%")))
                    },
                    if (filter_by_team) {
                      column(width = 4, div(title = "Filter players by current user team owner or free agents", selectInput(inputId = ns("team_filter"), label = "User Team Owner", choices = c("All"), width = "100%")))
                    },
                    if (filter_by_value) {
                      tagList(
                        column(width = 4, div(title = "Filter players by minimum market valuation in millions of EUR", numericInput(inputId = ns("min_value_filter"), label = "Min Val (M)", min = 0, max = 1000, value = 0, step = 10, width = "100%"))),
                        column(width = 4, div(title = "Filter players by maximum market valuation in millions of EUR", numericInput(inputId = ns("max_value_filter"), label = "Max Val (M)", min = 0, max = 1000, value = 1000, step = 10, width = "100%")))
                      )
                    },
                    if (filter_by_change_value) {
                      column(width = 4, div(title = "Filter players by minimum 24-hour market trend in millions of EUR", numericInput(inputId = ns("change_value_filter"), label = "Min Trend (M)", min = 0, max = 1, value = default_minimum_change_value, step = 0.05, width = "100%")))
                    }
                 )
          ),
          column(width = 5,
                 uiOutput(ns("position_breakdown_ui"))
          )
        )
      } else {
        fluidRow(
          style = "background: #f8fafc; padding: 15px; border-radius: 8px; margin: 0 0 20px 0; border: 1px solid #e2e8f0;",
          if (filter_by_position) {
            column(width = 3, div(title = "Filter players by primary or secondary position (Goalkeeper, Defender, Midfielder, Forward)", selectInput(inputId = ns("position_filter"), label = "Position", choices = c("All", "Goalkeeper", "Defender", "Midfielder", "Forward"), selected = "All", width = "100%")))
          },
          if (filter_by_team) {
            column(width = 3, div(title = "Filter players by current user team owner or free agents", selectInput(inputId = ns("team_filter"), label = "User Team Owner", choices = c("All"), width = "100%")))
          },
          if (filter_by_value) {
            tagList(
              column(width = 2, div(title = "Filter players by minimum market valuation in millions of EUR", numericInput(inputId = ns("min_value_filter"), label = "Min Val (M)", min = 0, max = 1000, value = 0, step = 10, width = "100%"))),
              column(width = 2, div(title = "Filter players by maximum market valuation in millions of EUR", numericInput(inputId = ns("max_value_filter"), label = "Max Val (M)", min = 0, max = 1000, value = 1000, step = 10, width = "100%")))
            )
          },
          if (filter_by_change_value) {
            column(width = 2, div(title = "Filter players by minimum 24-hour market trend in millions of EUR", numericInput(inputId = ns("change_value_filter"), label = "Min Trend (M)", min = 0, max = 1, value = default_minimum_change_value, step = 0.05, width = "100%")))
          },
          div(
            class = "col-sm-2 col-xs-6",
            title = "Filter by Futmondo Intelligence Score rating tier",
            selectInput(
              inputId = ns("fis_tier_filter"),
              label = "FIS Rating",
              choices = c("All" = "All", "Strong Buy" = "Strong Buy", "Buy" = "Buy", "Hold" = "Hold", "Sell" = "Sell"),
              selected = "All",
              width = "100%"
            )
          )
        )
      },

      # Checkboxes Inline Grid
      if (filter_by_active_clause || filter_by_is_favorite || filter_by_is_from_futmondo || filter_by_players_with_bid) {
        fluidRow(
          style = "padding: 0 15px; margin-bottom: 20px; display: flex; flex-wrap: wrap; gap: 20px; align-items: center;",
          if (filter_by_active_clause) {
            div(checkboxInput(inputId = ns("active_clause_filter"), label = "Active Clause Only", value = FALSE), style = "font-weight: 500;", title = "Show only players whose buyout clause is currently payable (lock period has expired)")
          },
          if (filter_by_is_favorite) {
            div(checkboxInput(inputId = ns("is_favorite_filter"), label = "Favorites Only", value = FALSE), style = "font-weight: 500;", title = "Show only players you have starred as favorite")
          },
          if (filter_by_is_from_futmondo) {
            div(checkboxInput(inputId = ns("is_from_futmondo_filter"), label = "Free Agents Only", value = FALSE), style = "font-weight: 500;", title = "Show only unowned players without a user team (free agents / market)")
          },
          if (filter_by_players_with_bid) {
            tagList(
              div(checkboxInput(inputId = ns("players_you_bid_filter"), label = "Your Bids Only", value = FALSE), style = "font-weight: 500;", title = "Show only players you have placed an active bid on"),
              div(checkboxInput(inputId = ns("players_with_bid_filter"), label = "Bidded Only", value = FALSE), style = "font-weight: 500;", title = "Show only players with active bids from any league team")
            )
          },
          div(actionButton(ns("btn_clear_filters"), icon("eraser"), "Clear Filters", class = "btn btn-sm btn-outline-secondary"), style = "margin-left: 10px;", title = "Reset all table filters to default values")
        )
      },

      # Table Container
      div(
        reactableOutput(ns("players_table")),
        style = "overflow-x: auto; font-size:85%; width: 100%;"
      )
    )
  )
}


players_table_Server <- function(id, players_table_RV, user_teams_RV, login_token = NULL, championship_id = NULL, user_team_id = NULL, hide_bid_column = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    table_refresh_trigger <- reactiveVal(0)

    local_bids_override <- reactiveVal(list())
    bulk_market_override <- reactiveVal(FALSE)

    handle_bid_updated <- function(player_id = NULL, new_bid_price = NULL, is_cancel = FALSE) {
      if (!is.null(player_id) && player_id == "ALL") {
        bulk_market_override(TRUE)
      } else if (!is.null(player_id) && player_id != "") {
        current_overrides <- local_bids_override()
        current_overrides[[as.character(player_id)]] <- list(
          price = if (isTRUE(is_cancel)) NA_real_ else suppressWarnings(as.numeric(new_bid_price)),
          is_cancel = isTRUE(is_cancel)
        )
        local_bids_override(current_overrides)
      }
      table_refresh_trigger(table_refresh_trigger() + 1)
    }

    # Safe date parsing helper
    parse_safe_datetime <- function(date_vec) {
      if (is.null(date_vec) || length(date_vec) == 0) return(as.POSIXct(character(0)))
      date_str <- as.character(date_vec)
      clean_str <- gsub("T", " ", date_str)
      clean_str <- gsub("Z", "", clean_str)
      clean_str <- gsub("\\..*", "", clean_str)
      parsed <- suppressWarnings(as.POSIXct(clean_str, format = "%Y-%m-%d %H:%M:%S"))
      na_idx <- is.na(parsed)
      if (any(na_idx)) {
        parsed[na_idx] <- suppressWarnings(as.POSIXct(clean_str[na_idx], format = "%Y-%m-%d"))
      }
      return(parsed)
    }

    # observers ----
    # observe user_teams_RV to update market_player_team_filter ----
    observeEvent(user_teams_RV(), {
      teams <- user_teams_RV()
      req(teams)
      user_team_names <- teams$teamname %>% sort()
      # remove the ones that are not in players_table_RV
      user_team_names <- user_team_names[user_team_names %in% players_table_RV()$userTeam]
      team_choices <- c("All", "Free")
      if (length(user_team_names)) {
        team_choices <- c(team_choices, user_team_names)
      }
      updateSelectInput(session, inputId = "team_filter", choices = team_choices, selected = "All")
    })

    # observe clear filters button ----
    observeEvent(input$btn_clear_filters, {
      tryCatch({
        updateSelectInput(session, inputId = "position_filter", selected = "All")
        updateSelectInput(session, inputId = "team_filter", selected = "All")
        updateNumericInput(session, inputId = "min_value_filter", value = 0)
        updateNumericInput(session, inputId = "max_value_filter", value = 1000)
        updateNumericInput(session, inputId = "change_value_filter", value = NA)
        updateCheckboxInput(session, inputId = "active_clause_filter", value = FALSE)
        updateCheckboxInput(session, inputId = "is_favorite_filter", value = FALSE)
        updateCheckboxInput(session, inputId = "is_from_futmondo_filter", value = FALSE)
        updateCheckboxInput(session, inputId = "players_you_bid_filter", value = FALSE)
        updateCheckboxInput(session, inputId = "players_with_bid_filter", value = FALSE)
        updateSelectInput(session, inputId = "fis_tier_filter", selected = "All")
      }, error = function(e) {
        warning(paste0("[Players Table] Clear filters error: ", e$message))
      })
    })
    
    # observe selected_player_RV() to open popup with           selected_player_UI(id = "selected_player")
    observeEvent(
      selected_player_RV(),
      {
        req(selected_player_RV())
        showModal(modalDialog(
          # title = "Selected player",
          selected_player_UI(id = ns("selected_player")),
          footer = div(style = "text-align: center; width: 100%;",
                       modalButton("Close")),
          easyClose = TRUE,
          size = "l"
        ))
      },
      ignoreNULL = TRUE
    )
    # Modules ----
    selected_player_Server(
      id = "selected_player",
      selected_player = selected_player_RV,
      login_token = login_token,
      championship_id = championship_id,
      user_team_id = user_team_id,
      on_bid_updated = handle_bid_updated
    )
    # reactives ----
    ## selected_player_RV
    selected_player_RV <- reactive({
      selected_idx <- getReactableState(outputId = "players_table", name = "selected", session = session)
      req(selected_idx)
      selected_player <- players_table_filtered_RV()[selected_idx, ]
    })
    
    ## players_table_filtered_RV ----
    players_table_filtered_RV <- reactive({
      table_refresh_trigger()
      players_table <- players_table_RV()
      if (is.null(players_table) || nrow(players_table) == 0) {
        return(NULL)
      }

      # Ensure FIS scores are calculated if fis_score is not present
      if (!"fis_score" %in% colnames(players_table)) {
        tryCatch({
          players_table <- calculate_fis_score(players_table)
        }, error = function(e) {
          warning(paste0("[Players Table] FIS score calculation failed: ", e$message))
        })
      }

      # Apply local in-memory bid overrides if present
      overrides <- local_bids_override()
      if (length(overrides) > 0 && "id" %in% colnames(players_table)) {
        for (pid in names(overrides)) {
          ov <- overrides[[pid]]
          match_idx <- which(as.character(players_table$id) == pid)
          if (length(match_idx) > 0) {
            if (isTRUE(ov$is_cancel)) {
              if ("bid_price" %in% colnames(players_table)) {
                players_table$bid_price[match_idx] <- NA_real_
              }
              if ("market_inMarket" %in% colnames(players_table)) {
                players_table$market_inMarket[match_idx] <- FALSE
              }
              if ("effective_market_price" %in% colnames(players_table)) {
                players_table$effective_market_price[match_idx] <- NA_real_
              }
              if ("numberOfBids" %in% colnames(players_table) && !is.na(players_table$numberOfBids[match_idx])) {
                current_bids <- suppressWarnings(as.numeric(players_table$numberOfBids[match_idx]))
                if (!is.na(current_bids)) {
                  players_table$numberOfBids[match_idx] <- max(0, current_bids - 1)
                }
              }
            } else {
              old_bid <- if ("bid_price" %in% colnames(players_table)) players_table$bid_price[match_idx] else NA_real_
              if (!"bid_price" %in% colnames(players_table)) {
                players_table$bid_price <- NA_real_
              }
              players_table$bid_price[match_idx] <- ov$price

              if ("numberOfBids" %in% colnames(players_table)) {
                old_bid_val <- suppressWarnings(as.numeric(old_bid))
                if (is.na(old_bid_val) || is.null(old_bid_val) || old_bid_val == 0) {
                  current_bids <- suppressWarnings(as.numeric(players_table$numberOfBids[match_idx]))
                  current_bids <- if (is.na(current_bids)) 0 else current_bids
                  players_table$numberOfBids[match_idx] <- current_bids + 1
                }
              }
            }
          }
        }
      }

      # Apply bulk market listed override if active
      if (isTRUE(bulk_market_override()) && !is.null(players_table) && nrow(players_table) > 0) {
        players_table$market_inMarket <- TRUE
        if (!"effective_market_price" %in% colnames(players_table)) {
          players_table$effective_market_price <- NA_real_
        }
        if ("value" %in% colnames(players_table)) {
          players_table$effective_market_price <- ifelse(
            is.na(players_table$effective_market_price) | players_table$effective_market_price <= 0,
            players_table$value,
            players_table$effective_market_price
          )
        }
      }

      # players_table <- players_table %>%
      #   translate_player_positions()
      # players_table <- players_table %>%
      #   calculate_player_changes()
      # players_table <- players_table %>%
      #   unify_columns()
      if (!is.null(input$position_filter)) {
        if (input$position_filter != "All") {
          players_table <- players_table %>%
            dplyr::filter(role == input$position_filter | role2 == input$position_filter)
        }
      }
      if (!is.null(input$team_filter)) {
        if (input$team_filter != "All") {
          if (input$team_filter == "Free") {
            players_table <- players_table %>%
              dplyr::filter(is.na(userTeam))
          } else {
            players_table <- players_table %>%
              dplyr::filter(userTeam == input$team_filter)
          }
        }
      }
      if (!is.null(input$min_value_filter)) {
        players_table <- players_table %>%
          dplyr::filter(value >= input$min_value_filter * 1000000)
      }
      if (!is.null(input$max_value_filter)) {
        players_table <- players_table %>%
          dplyr::filter(value <= input$max_value_filter * 1000000)
      }
      if (!is.null(input$active_clause_filter)) {
        if (input$active_clause_filter) {
          now_time <- Sys.time()
          players_table <- players_table %>%
            dplyr::filter(
              isClause & (
                is.na(clause_date) | clause_date == "" | parse_safe_datetime(clause_date) <= now_time
              )
            )
        }
      }
      # is_favorite_filter
      if (!is.null(input$is_favorite_filter)) {
        if (input$is_favorite_filter) {
          players_table <- players_table %>%
            dplyr::filter(fav == TRUE)
        }
      }
      if (!is.null(input$is_from_futmondo_filter)) {
        if (input$is_from_futmondo_filter) {
          if ("computer" %in% colnames(players_table)) {
            players_table <- players_table %>%
              dplyr::filter(computer == TRUE)
          } else {
            warning("is_from_futmondo_filter is active but 'computer' column is missing from players_table.")
          }
        }
      }
      if (!is.null(input$change_value_filter) && !is.na(input$change_value_filter)) {
        players_table <- players_table %>%
          dplyr::filter(change >= input$change_value_filter * 1000000)
      }
      if (!is.null(input$players_you_bid_filter)) {
        if (input$players_you_bid_filter) {
          players_table <- players_table %>%
            dplyr::filter(bid_price > 0)
        }
      }
      if (!is.null(input$players_with_bid_filter)) {
        if (input$players_with_bid_filter) {
          players_table <- players_table %>%
            # Number of Bids > 0
            dplyr::filter(numberOfBids > 0)
        }
      }
      # FIS Tier filter
      if (!is.null(input$fis_tier_filter) && input$fis_tier_filter != "All") {
        if ("fis_tier" %in% colnames(players_table)) {
          players_table <- players_table %>%
            dplyr::filter(fis_tier == input$fis_tier_filter)
        }
      }
      return(players_table)
    })
    
    # renders ----
    ## render players_table ----
    output$players_table <- renderReactable({
      req(players_table_filtered_RV())
      cols_to_hide <- cfg_player_columns_to_hide
      if (isTRUE(hide_bid_column)) {
        cols_to_hide <- c(cols_to_hide, "bid_price")
      }
      players_table <- players_table_filtered_RV() %>%
        reorder_player_table_columns() %>%
        dplyr::select(!any_of(cols_to_hide))
      
      print(paste0(nrow(players_table), " players in table"))
      # players_table <- players_table %>%
      #   dplyr::select(
      #     any_of(starts_with("change")), any_of(starts_with("market_")), any_of(starts_with("bid_price")), any_of(starts_with("clause_")), any_of(c("name", "role", "role2", "points", "value", "status", "team", "rating", "change", "average", "total")))
      #
      table_columns <- get_reactable_columns_for_players(players_table)
      reactable(players_table,
                columns = table_columns,
                searchable = TRUE,
                filterable = TRUE,
                resizable = TRUE,
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
    
    # Position Breakdown UI Renderer
    output$position_breakdown_ui <- renderUI({
      df <- players_table_RV()
      if (is.null(df) || nrow(df) == 0) return(NULL)

      combined_roles <- paste(
        if ("role" %in% colnames(df)) ifelse(is.na(df$role), "", as.character(df$role)) else "",
        if ("role2" %in% colnames(df)) ifelse(is.na(df$role2), "", as.character(df$role2)) else "",
        sep = " "
      )

      gk <- sum(grepl("portero|goalkeeper|^1$|\\bpor\\b|\\bgkp\\b|\\bgk\\b", combined_roles, ignore.case = TRUE), na.rm = TRUE)
      df_cnt <- sum(grepl("defensa|defender|^2$|\\bdef\\b|\\bdf\\b", combined_roles, ignore.case = TRUE), na.rm = TRUE)
      md <- sum(grepl("centrocampista|midfielder|mediocentro|^3$|\\bmed\\b|\\bcen\\b|\\bmid\\b|\\bmf\\b|\\bmc\\b", combined_roles, ignore.case = TRUE), na.rm = TRUE)
      fw <- sum(grepl("delantero|forward|extremo|atacante|^4$|\\bdel\\b|\\bfwd\\b|\\bfw\\b|\\batt\\b", combined_roles, ignore.case = TRUE), na.rm = TRUE)
      total_squad <- nrow(df) # Exact actual player count (distinct players)

      div(
        class = "squad-breakdown-card",
        div(class = "squad-breakdown-title",
            span(icon("users"), " Squad Position Breakdown"),
            span(class = "badge", style = "background-color: #334155; color: #fff; font-size: 11px; padding: 3px 8px;", paste0("Total: ", total_squad, " Players"))
        ),
        div(style = "display: flex; gap: 8px; flex-wrap: wrap; justify-content: space-between; margin-top: 8px;",
            span(class = "badge-gk squad-pos-badge", paste0("Goalkeepers: ", gk)),
            span(class = "badge-df squad-pos-badge", paste0("Defenders: ", df_cnt)),
            span(class = "badge-md squad-pos-badge", paste0("Midfielders: ", md)),
            span(class = "badge-fw squad-pos-badge", paste0("Forwards: ", fw))
        )
      )
    })

    return(selected_player_RV)
  })
}
