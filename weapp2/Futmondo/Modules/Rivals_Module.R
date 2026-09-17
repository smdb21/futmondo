library(reactable)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)

# ---- Pure helper: safe ISO datetime parser (top-level for testability) ----
# Mirrors the module's internal parse_safe_datetime so the pure helpers below
# can parse pressroom `created` timestamps deterministically.
rivals_parse_datetime <- function(date_vec) {
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
  # Invalid / unparseable timestamps stay NA (never substituted with Sys.time()).
  parsed
}

# ---- Pure helper: per-team buying-power values for the league plot ----
# Transfer-only balance estimates use ALL transfers through the END date
# (not reset at slider start), starting from an explicitly supplied budget.
# Rewards and other account adjustments are not inferred from transfers. Investment and Transaction Volume stay within the [start, end]
# range. Returns a data frame (team_id, team, value, range_label) where
# range_label describes the window used (for labels/tooltips).
rivals_buying_power_values <- function(pressroom_df, teams, metric = "cash",
                                       start_date = NULL, end_date = NULL,
                                       initial_budget = NA_real_) {
  team_ids <- if (!is.null(teams) && "teamid" %in% colnames(teams)) as.character(teams$teamid) else character(0)
  team_names_map <- if (!is.null(teams) && "teamname" %in% colnames(teams)) setNames(as.character(teams$teamname), as.character(teams$teamid)) else character(0)

  plot_df <- data.frame(
    team_id = team_ids,
    team = ifelse(team_ids %in% names(team_names_map), team_names_map[team_ids], team_ids),
    stringsAsFactors = FALSE
  )

  if (!nrow(plot_df)) {
    plot_df$value <- numeric(); plot_df$range_label <- character()
    return(plot_df)
  }
  if (is.null(pressroom_df) || !is.data.frame(pressroom_df) || nrow(pressroom_df) == 0) {
    plot_df$value <- if (metric == "cash") initial_budget else 0
    plot_df$range_label <- if (metric == "cash") "all transfers through end date" else "within selected range"
    return(plot_df)
  }

  parsed <- suppressWarnings(as.POSIXct(as.character(pressroom_df$created), tz = "UTC"))

  # Cash: no start filter (balance from all transfers through end).
  # Investment / Volume: [start, end] range.
  mask <- !is.na(parsed)
  if (metric == "cash") {
    if (!is.null(end_date)) mask <- mask & parsed <= end_date
  } else {
    if (!is.null(start_date)) mask <- mask & parsed >= start_date
    if (!is.null(end_date)) mask <- mask & parsed <= end_date
  }
  sub <- pressroom_df[mask, , drop = FALSE]

  purchases <- rep(0, nrow(plot_df))
  sales <- rep(0, nrow(plot_df))

  if (nrow(sub) > 0) {
    if ("buyer_team_id" %in% colnames(sub) && "price" %in% colnames(sub)) {
      buys_agg <- sub %>%
        dplyr::filter(!is.na(buyer_team_id) & nzchar(as.character(buyer_team_id))) %>%
        dplyr::group_by(buyer_team_id = as.character(buyer_team_id)) %>%
        dplyr::summarise(total = sum(suppressWarnings(as.numeric(price)), na.rm = TRUE), .groups = "drop")
      if (nrow(buys_agg) > 0) {
        m <- match(plot_df$team_id, buys_agg$buyer_team_id)
        purchases <- ifelse(!is.na(m), buys_agg$total[m], 0)
      }
    }
    if ("seller_team_id" %in% colnames(sub) && "price" %in% colnames(sub)) {
      sells_agg <- sub %>%
        dplyr::filter(!is.na(seller_team_id) & nzchar(as.character(seller_team_id))) %>%
        dplyr::group_by(seller_team_id = as.character(seller_team_id)) %>%
        dplyr::summarise(total = sum(suppressWarnings(as.numeric(price)), na.rm = TRUE), .groups = "drop")
      if (nrow(sells_agg) > 0) {
        m <- match(plot_df$team_id, sells_agg$seller_team_id)
        sales <- ifelse(!is.na(m), sells_agg$total[m], 0)
      }
    }
  }

  if (metric == "cash") {
    plot_df$value <- initial_budget - purchases + sales
    plot_df$range_label <- "all transfers through end date"
  } else if (metric == "investment") {
    plot_df$value <- purchases
    plot_df$range_label <- "within selected range"
  } else {
    plot_df$value <- purchases + sales
    plot_df$range_label <- "within selected range"
  }

  plot_df
}

# ---- Pure helper: build the "Pivot by Player" buy/sell ledger ----
# Pairs buys to sells by player_id (falling back to player_name when no id is
# exposed). Uses raw timestamps for matching/sorting (ISO 8601 sorts
# chronologically as a string). Returns a data frame WITHOUT a "Sold" column;
# a buy with no later sell has NA sell fields. Helper columns (PlayerID) are
# present for pairing and should be hidden in the table.
rivals_build_pivot_ledger <- function(pressroom_df, rival_id) {
  empty <- data.frame(
    Player = character(0), PlayerID = character(0),
    Buy_Date = character(0), Buy_Type = character(0), Bought_Price = numeric(0),
    Sell_Date = character(0), Sell_Type = character(0), Sold_Price = numeric(0), Net_PL = numeric(0),
    stringsAsFactors = FALSE
  )
  if (is.null(pressroom_df) || !is.data.frame(pressroom_df) || nrow(pressroom_df) == 0) return(empty)
  if (is.null(rival_id) || rival_id == "") return(empty)

  # NA-safe team filtering (NA == rival_id yields NA, which would select an
  # NA row when used as a row index).
  buys <- pressroom_df[!is.na(pressroom_df$buyer_team_id) & pressroom_df$buyer_team_id == rival_id, , drop = FALSE]
  sells <- pressroom_df[!is.na(pressroom_df$seller_team_id) & pressroom_df$seller_team_id == rival_id, , drop = FALSE]
  if (nrow(buys) == 0) return(empty)

  key_of <- function(df) {
    if ("player_id" %in% colnames(df) && all(nzchar(as.character(df$player_id)))) {
      as.character(df$player_id)
    } else if ("player_name" %in% colnames(df)) {
      as.character(df$player_name)
    } else {
      rep(NA_character_, nrow(df))
    }
  }
  name_of <- function(df) {
    if ("player_name" %in% colnames(df)) as.character(df$player_name) else rep("Unknown", nrow(df))
  }

  buys$key <- key_of(buys)
  buys$display_name <- name_of(buys)
  buys$buy_ts <- rivals_parse_datetime(buys$created)
  # Skip buys with invalid (NA) timestamps rather than ordering on fabricated now.
  buys <- buys[!is.na(buys$buy_ts), , drop = FALSE]
  if (nrow(buys) == 0) return(empty)
  buys <- buys[order(buys$buy_ts), , drop = FALSE]

  sells$key <- key_of(sells)
  sells$sell_ts <- rivals_parse_datetime(sells$created)
  # Skip sells with invalid (NA) timestamps rather than matching on fabricated now.
  sells <- sells[!is.na(sells$sell_ts), , drop = FALSE]
  sells <- sells[order(sells$sell_ts), , drop = FALSE]

  all_keys <- unique(buys$key)
  all_keys <- all_keys[!is.na(all_keys) & nzchar(all_keys)]
  if (length(all_keys) == 0) return(empty)

  ledger_rows <- lapply(all_keys, function(pk) {
    p_buys <- buys[buys$key == pk, , drop = FALSE]
    p_sells <- sells[sells$key == pk, , drop = FALSE]
    if (nrow(p_buys) == 0) return(NULL)

    used_sells <- integer(0)
    rows <- lapply(seq_len(nrow(p_buys)), function(bi) {
      b <- p_buys[bi, ]
      buy_price <- suppressWarnings(as.numeric(b$price))
      buy_date_raw <- as.character(b$created)
      buy_type <- if ("type" %in% colnames(b)) as.character(b$type) else "transfer"

      # First unused sell (by raw timestamp) strictly after this buy.
      sell_match_idx <- NA_integer_
      if (nrow(p_sells) > 0) {
        for (si in seq_len(nrow(p_sells))) {
          if (si %in% used_sells) next
          if (p_sells$sell_ts[si] > b$buy_ts) {
            sell_match_idx <- si
            break
          }
        }
      }

      if (!is.na(sell_match_idx)) {
        used_sells <<- c(used_sells, sell_match_idx)
        s <- p_sells[sell_match_idx, ]
        sell_price <- suppressWarnings(as.numeric(s$price))
        sell_date_raw <- as.character(s$created)
        sell_type <- if ("type" %in% colnames(s)) as.character(s$type) else "transfer"
        data.frame(
          Player = b$display_name,
          PlayerID = pk,
          Buy_Date = buy_date_raw,
          Buy_Type = buy_type,
          Bought_Price = buy_price,
          Sell_Date = sell_date_raw,
          Sell_Type = sell_type,
          Sold_Price = sell_price,
          Net_PL = sell_price - buy_price,
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          Player = b$display_name,
          PlayerID = pk,
          Buy_Date = buy_date_raw,
          Buy_Type = buy_type,
          Bought_Price = buy_price,
          Sell_Date = NA_character_,
          Sell_Type = NA_character_,
          Sold_Price = NA_real_,
          Net_PL = NA_real_,
          stringsAsFactors = FALSE
        )
      }
    })

    valid <- Filter(Negate(is.null), rows)
    if (length(valid) == 0) return(NULL)
    bind_rows(valid)
  })

  ledger <- Filter(Negate(is.null), ledger_rows)
  if (length(ledger) == 0) return(empty)
  ledger <- bind_rows(ledger)

  # Sort by raw buy timestamp, newest first (ISO 8601 sorts chronologically).
  # All ledger buys carry valid timestamps (invalid ones were skipped above),
  # but keep NA handling explicit for safety.
  ledger$buy_ts <- rivals_parse_datetime(ledger$Buy_Date)
  ledger <- ledger[order(ledger$buy_ts, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  ledger$buy_ts <- NULL
  ledger
}

# ---- Pure helper: reorder the transaction log into display column order ----
# Visible columns first (date, type, concept, money, running_balance), then the
# hidden helper columns (id, category, timestamp, batch_*). All fields are
# preserved and row order/values are unchanged; only the column order changes
# so the reactable renders "Date" first.
rivals_tx_display_df <- function(filtered) {
  if (is.null(filtered) || !is.data.frame(filtered) || nrow(filtered) == 0) return(filtered)
  visible_cols <- c("date", "type", "concept", "money", "running_balance")
  helper_cols <- c("id", "category", "timestamp", "batch_key", "is_batch_header", "batch_final_balance")
  present <- colnames(filtered)
  ordered_cols <- c(
    intersect(visible_cols, present),
    intersect(helper_cols, present),
    setdiff(present, c(visible_cols, helper_cols))
  )
  filtered[, ordered_cols, drop = FALSE]
}

rivals_UI <- function(id) {
  ns <- NS(id)
tagList(
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
               p(style = "color: var(--fm-muted); font-size: 13px; font-weight: 500; margin-bottom: 12px;",
                 icon("hand-pointer"), " Select a user team from the table below to scout their squad and financial details."),
               reactable::reactableOutput(ns("league_finances_table"))
             )
      )
    ),


    # Scouted Rival Details (Summary cards + Player Roster Table)
    uiOutput(ns("scouted_rival_details_ui")),

    # Plot D: League Squad Value Evolution (Historical Valuation) - placed at bottom
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
     )
  )
}

rivals_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger = NULL) {
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

    # Season start date helper -- programmatically detects the reset date
    get_season_start_date <- function(raw_movements = NULL) {
      # --- Case 1: raw_movements is empty or NULL ---
      if (is.null(raw_movements) || nrow(raw_movements) == 0) {
        return(Sys.Date() - 180)
      }

      parsed_dates <- parse_safe_datetime(raw_movements$date)

      # --- Case 2: look for explicit budget-reset rows ---
      # A budget reset is signalled by type == "budget", or by category == "bonus"
      # with money >= 100,000,000 (initial budget allocation).
      budget_mask <- FALSE
      if ("type" %in% colnames(raw_movements)) {
        budget_mask <- as.character(raw_movements$type) == "budget"
      }
      bonus_large_mask <- FALSE
      if ("category" %in% colnames(raw_movements) && "money" %in% colnames(raw_movements)) {
        bonus_large_mask <- as.character(raw_movements$category) == "bonus" &
                            suppressWarnings(as.numeric(raw_movements$money)) >= 100000000
      }
      reset_mask <- budget_mask | bonus_large_mask

      if (any(reset_mask, na.rm = TRUE)) {
        # Use the MOST RECENT budget-reset timestamp as the exact season start.
        reset_dates <- parsed_dates[reset_mask]
        reset_dates <- reset_dates[!is.na(reset_dates)]
        if (length(reset_dates) > 0) {
          return(as.Date(max(reset_dates)))
        }
      }

      # --- Case 3: no explicit budget row -- use earliest transaction in the split ---
      valid_dates <- parsed_dates[!is.na(parsed_dates)]
      if (length(valid_dates) > 0) {
        return(as.Date(min(valid_dates)))
      }

      # --- Final fallback ---
      return(Sys.Date() - 180)
    }

    rival_context_RV <- reactive(list(user=fm_scalar(login_token()[["userid"]]),
      championship=fm_scalar(championship_id()),team=fm_scalar(user_team_id())))
    selected_rival_id_RV <- reactiveVal(NULL)
    selected_rival_context_RV <- reactiveVal(NULL)
    observeEvent(rival_context_RV(), {
      selected_rival_id_RV(NULL);selected_rival_context_RV(NULL)
      tryCatch(updateReactable("league_finances_table",selected=NA_integer_,session=session),error=function(e)NULL)
    },ignoreNULL=FALSE,priority=100)
    observeEvent(getReactableState("league_finances_table","selected",session=session), {
      idx<-getReactableState("league_finances_table","selected",session=session)
      df<-league_finances_RV()$team_finances
      req(length(idx)==1L,is.numeric(idx),is.data.frame(df),idx>=1,idx<=nrow(df))
      selected_rival_context_RV(rival_context_RV());selected_rival_id_RV(as.character(df$teamid[idx]))
    },ignoreNULL=TRUE)
    selected_rival_team_id <- reactive({
      if(!identical(selected_rival_context_RV(),rival_context_RV())) return(NULL)
      id<-selected_rival_id_RV()
      df<-league_finances_RV()$team_finances
      if(is.null(id) || !is.data.frame(df) || !id %in% df$teamid) return(NULL)
      id
    })

    # Detailed financial statistics of selected rival
    rival_financial_summary_box_RV <- reactive({
      req(is_module_active() == TRUE)
      if (!is.null(refresh_trigger)) refresh_trigger()
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
      if (!is.null(refresh_trigger)) refresh_trigger()
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
      if (!is.null(refresh_trigger)) refresh_trigger()
      req(login_token())
      req(championship_id())
      teams <- user_teams_RV()
      req(teams)

      snapshot <- tryCatch(get_financial_snapshot(login_token(), championship_id(), user_team_id()), error = function(e) NULL)
      initial <- normalize_league_rules(list(configuration = snapshot$configuration))$initial_budget
      finances <- calculate_league_finances(
        login = login_token(),
        championship_id = championship_id(),
        user_teams_df = teams,
        initial_budget = initial
      )
      return(finances)
    })

    # Render League Financial Standings Table
    output$league_finances_table <- reactable::renderReactable({
      req(is_module_active() == TRUE)
      if (!is.null(refresh_trigger)) refresh_trigger()
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
              if (length(val) != 1L || !is.finite(val)) return("Unavailable")
              formatted <- format_table_currency(val)
              color_style <- if (val >= 0) "color: var(--fm-text); font-weight: 700;" else "color: var(--fm-danger); font-weight: 700;"
              shiny::tags$span(style = color_style, formatted)
            }
          ),
          `Squad Value` = colDef(align = "right", cell = function(val) format_table_currency(val)),
          `Net Profit/Loss` = colDef(
            align = "right",
            cell = function(val) {
              if (is.na(val) || !is.numeric(val)) return("")
              sign_pfx <- if (val > 0) "+" else ""
              color_style <- if (val > 0) "color: var(--fm-text); font-weight: 600;" else if (val < 0) "color: var(--fm-danger); font-weight: 600;" else "color: var(--fm-muted);"
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
      if (!is.null(refresh_trigger)) refresh_trigger()
      info <- rival_financial_summary_box_RV()
      roster <- rival_players_table_RV()
      rival_id <- selected_rival_team_id()

      # Compute squad value from roster
      squad_val <- 0
      if (!is.null(roster) && nrow(roster) > 0) {
        if ("value" %in% colnames(roster)) {
          squad_val <- sum(suppressWarnings(as.numeric(roster$value)), na.rm = TRUE)
        }
      }

      # Retrieve transaction movements safely
      tx_raw <- tryCatch({ rival_moneymovements_raw_RV() }, error = function(e) NULL)

      # Derive money_out, money_in, total_spent, budget_val from reconstructed finances
      money_out <- 0
      money_in <- 0
      total_spent <- 0
      budget_val <- NA_real_

      if (!is.null(tx_raw) && nrow(tx_raw) > 0) {
        # money_out = absolute value of all negative money (purchases)
        money_out <- sum(abs(tx_raw$money[tx_raw$money < 0]), na.rm = TRUE)

        # money_in = all positive money excluding initial budget row
        non_budget_rows <- tx_raw[tx_raw$type != "budget", ]
        if (nrow(non_budget_rows) > 0) {
          money_in <- sum(non_budget_rows$money[non_budget_rows$money > 0], na.rm = TRUE)
        }

        # total_spent = money_out (purchases only)
        total_spent <- money_out

      } else {
        # No transaction data -- use roster-based calculation
        if (!is.null(roster) && nrow(roster) > 0) {
          if ("buyPrice" %in% colnames(roster)) {
            total_spent <- sum(suppressWarnings(as.numeric(roster$buyPrice)), na.rm = TRUE)
            money_out <- total_spent
          }
        }
        budget_val <- NA_real_
      }

      # Override budget and squad_val with API-provided values if available and valid
      if (!is.null(info) && length(info$budget) == 1L && is.numeric(info$budget) && is.finite(info$budget)) {
        budget_val <- info$budget
      }
      if (!is.null(info) && !is.null(info$teamValue) && is.numeric(info$teamValue) && info$teamValue > 0) {
        squad_val <- info$teamValue
      }

      # net_gain = squad_val - total_spent
      net_gain <- squad_val - total_spent

      # Compute net_transfer_pnl from pressroom (sum of sell_price - buy_price for completed trades)
      net_transfer_pnl <- 0
      if (!is.null(rival_id) && rival_id != "") {
        tryCatch({
          champ_id <- championship_id()
          login <- login_token()
          pressroom_df <- get_championship_pressroom(login = login, championship_id = champ_id)
          if (!is.null(pressroom_df) && nrow(pressroom_df) > 0) {
            buys <- pressroom_df[pressroom_df$buyer_team_id == rival_id, ]
            sells <- pressroom_df[pressroom_df$seller_team_id == rival_id, ]
            if (!is.null(buys) && nrow(buys) > 0 && "price" %in% colnames(buys)) {
              net_transfer_pnl <- net_transfer_pnl - sum(suppressWarnings(as.numeric(buys$price)), na.rm = TRUE)
            }
            if (!is.null(sells) && nrow(sells) > 0 && "price" %in% colnames(sells)) {
              net_transfer_pnl <- net_transfer_pnl + sum(suppressWarnings(as.numeric(sells$price)), na.rm = TRUE)
            }
          }
        }, error = function(e) {
          print(paste0("[Rivals] net_transfer_pnl computation warning: ", e$message))
        })
      }

      pnl_color <- if (net_transfer_pnl > 0) "#10b981" else if (net_transfer_pnl < 0) "#ef4444" else "#64748b"
      pnl_sign <- if (net_transfer_pnl > 0) "+" else ""

      cash <- ui_financial_amount(budget_val)
      spent_fmt <- format_table_currency(total_spent)
      money_out_fmt <- format_table_currency(money_out)
      money_in_fmt <- format_table_currency(money_in)
      val_sum <- format_table_currency(squad_val)
      net_fmt <- format_table_currency(net_gain)
      pnl_fmt <- paste0(pnl_sign, format_table_currency(net_transfer_pnl))

      pos <- if (!is.null(info) && !is.null(info$position) && !is.na(info$position)) get_ordinal_position(info$position) else "-"
      total_teams <- if (!is.null(user_teams_RV())) nrow(user_teams_RV()) else 0
      rank_text <- paste0(pos, " of ", total_teams)

      tagList(
        # Row 1: 4 boxes
        fluidRow(
          column(width = 3,
                 box(
                   title = "Standings Position",
                   width = 12,
                   status = "primary",
                   solidHeader = TRUE,
                   div(style = "text-align: center; padding: 10px;",
                       h3(style = "font-weight: 700; color: var(--fm-text); margin: 0; font-size: 20px;", rank_text),
                       p(style = "color: var(--fm-muted); font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Rank Position"))
                 )
          ),
          column(width = 3,
                 box(
                   title = "Money Left (Budget)",
                   width = 12,
                   status = "success",
                   solidHeader = TRUE,
                   div(style = "text-align: center; padding: 10px;",
                       h3(style = "font-weight: 700; color: var(--fm-text); margin: 0; font-size: 20px;", cash),
                       p(style = "color: var(--fm-muted); font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Official Cash Balance"))
                 )
          ),
          column(width = 3,
                 box(
                   title = "Observed Transfer Cash Flow",
                   width = 12,
                   status = if (net_transfer_pnl > 0) "success" else "warning",
                   solidHeader = TRUE,
                   div(style = "text-align: center; padding: 10px;",
                       h3(style = paste0("font-weight: 700; color: ", pnl_color, "; margin: 0; font-size: 20px;"), pnl_fmt),
                       p(style = "color: var(--fm-muted); font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Sales minus purchases"))
                 )
          ),
          column(width = 3,
                 box(
                   title = "Squad Investment",
                   width = 12,
                   status = "warning",
                   solidHeader = TRUE,
                   div(style = "text-align: center; padding: 10px;",
                       h3(style = "font-weight: 700; color: var(--fm-warning); margin: 0; font-size: 20px;", money_out_fmt),
                       p(style = "color: var(--fm-muted); font-size: 11px; text-transform: uppercase; margin-top: 5px;",
                         tags$span(style = "color: var(--fm-danger); font-weight: 600;", paste0("Spent: ", money_out_fmt)),
                         tags$span(style = "color: var(--fm-muted);", " . "),
                         tags$span(style = "color: var(--fm-text); font-weight: 600;", paste0("Inflow: ", money_in_fmt)))
                       )
                 )
          )
        ),
        # Row 2: 1 wide box
        fluidRow(
          column(width = 12,
                 box(
                   title = "Squad Valuation & Net Gain",
                   width = 12,
                   status = "danger",
                   solidHeader = TRUE,
                   div(style = "text-align: center; padding: 10px;",
                       h3(style = "font-weight: 700; color: var(--fm-danger); margin: 0; font-size: 20px;", val_sum),
                       p(style = "color: var(--fm-muted); font-size: 11px; text-transform: uppercase; margin-top: 5px;", paste0("Net Gain: ", net_fmt)))
                 )
          )
        )
      )
    })
    
    # Plot D: League Squad Value Evolution (Historical valuations of all teams)
    output$team_valuation_history_plot <- plotly::renderPlotly({
      req(is_module_active() == TRUE)
      if (!is.null(refresh_trigger)) refresh_trigger()

      champ_id <- if (!is.null(championship_id)) championship_id() else NULL
      history_df <- NULL
      if (!is.null(champ_id)) {
        tryCatch({
          history_df <- get_league_standings_history(champ_id)
        }, error = function(e) {
          print(paste0("[Plot D] Valuation history fetch warning: ", e$message))
        })
      }

      # A current valuation is one observation, never an invented price history.
      if (is.null(history_df) || nrow(history_df) == 0 || !"team_value" %in% names(history_df)) {
        teams <- user_teams_RV()
        if (is.null(teams) || !nrow(teams) || !"teamValue" %in% names(teams)) {
          return(plotly::plot_ly() %>% fm_plot_layout(annotations = list(list(
            text = "Valuation history unavailable", showarrow = FALSE))))
        }
        history_df <- data.frame(teamname = teams$teamname,
          team_value = suppressWarnings(as.numeric(teams$teamValue)),
          recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC"), stringsAsFactors = FALSE)
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
        fm_plot_layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "", gridcolor = "#f1f5f9", zeroline = FALSE, tickformat = "%d-%m"),
          yaxis = list(title = "Squad Valuation (EUR)", gridcolor = "#f1f5f9", zeroline = FALSE, tickformat = "s"),
          legend = list(orientation = "h", x = 0.5, y = -0.25, xanchor = "center"),
          margin = list(l = 60, r = 20, t = 10, b = 40)
        )
    })

    # Plot E: League Buying Power horizontal bar chart
    output$league_finances_plot <- plotly::renderPlotly({
      req(is_module_active() == TRUE)
      if (!is.null(refresh_trigger)) refresh_trigger()
      req(login_token(), championship_id(), user_teams_RV())

      champ_id <- championship_id()
      login <- login_token()
      teams <- user_teams_RV()
      snapshot <- tryCatch(get_financial_snapshot(login, champ_id, user_team_id()), error = function(e) NULL)
      initial <- normalize_league_rules(list(configuration = snapshot$configuration))$initial_budget

      pressroom_df <- tryCatch({
        get_championship_pressroom(login = login, championship_id = champ_id)
      }, error = function(e) NULL)

      date_range <- input$buying_power_date_slider
      metric <- input$buying_power_metric
      if (is.null(metric)) metric <- "cash"

      if (is.null(pressroom_df) || nrow(pressroom_df) == 0 || is.null(date_range) || length(date_range) != 2) {
        # Fallback: show liquid cash from finances
        finances_res <- tryCatch({
          calculate_league_finances(
            login = login,
            championship_id = champ_id,
            user_teams_df = teams,
            initial_budget = initial
          )
        }, error = function(e) NULL)

        teams_df <- if (!is.null(finances_res) && "team_finances" %in% names(finances_res)) finances_res$team_finances else teams
        if (is.null(teams_df) || nrow(teams_df) == 0) return(NULL)

        team_names <- if ("teamname" %in% colnames(teams_df)) teams_df$teamname else if ("name" %in% colnames(teams_df)) teams_df$name else "Unknown"
        team_budgets <- if ("budget" %in% colnames(teams_df)) as.numeric(teams_df$budget) else rep(NA_real_, nrow(teams_df))

        plot_df <- data.frame(
          team = as.character(team_names),
          value = as.numeric(team_budgets),
          stringsAsFactors = FALSE
        ) %>% dplyr::arrange(value)

        colors <- ifelse(plot_df$value >= 0, "#10b981", "#ef4444")
        line_colors <- ifelse(plot_df$value >= 0, "#059669", "#b91c1c")

        return(plotly::plot_ly(
          data = plot_df,
          x = ~value,
          y = ~reorder(team, value),
          type = "bar",
          orientation = "h",
          marker = list(color = colors, line = list(color = line_colors, width = 1)),
          hoverinfo = "text",
          text = ~paste0("<b>", team, "</b><br>Liquid Cash: ", format_table_currency(value))
        ) %>%
          fm_plot_layout(
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            xaxis = list(title = "Liquid Cash (EUR)", gridcolor = "#f1f5f9", zeroline = TRUE, zerolinecolor = "#cbd5e1"),
            yaxis = list(title = "", gridcolor = "#f1f5f9", zeroline = FALSE),
            margin = list(l = 120, r = 20, t = 10, b = 40)
          )
        )
      }

      start_date <- as.POSIXct(date_range[1], tz = "UTC")
      end_date <- as.POSIXct(date_range[2], tz = "UTC")

      # Per-team values via the pure helper. Liquid Cash uses ALL transfers
      # through the end date (balance not reset at slider start); investment
      # and volume use the [start, end] range. range_label drives the tooltip.
      plot_df <- rivals_buying_power_values(
        pressroom_df = pressroom_df,
        teams = teams,
        metric = metric,
        start_date = start_date,
        end_date = end_date,
        initial_budget = initial
      )

      if (metric == "cash") {
        x_title <- "Transfer-only balance estimate (EUR)"
        tooltip_label <- "Transfer-only estimate; excludes rewards and adjustments"
      } else if (metric == "investment") {
        x_title <- "Squad Purchases (EUR)"
        tooltip_label <- "Squad Purchases"
      } else {
        x_title <- "Transaction Volume (EUR)"
        tooltip_label <- "Transaction Volume"
      }

      plot_df <- plot_df %>% dplyr::arrange(value)

      colors <- ifelse(plot_df$value >= 0, "#10b981", "#ef4444")
      line_colors <- ifelse(plot_df$value >= 0, "#059669", "#b91c1c")

      plotly::plot_ly(
        data = plot_df,
        x = ~value,
        y = ~reorder(team, value),
        type = "bar",
        orientation = "h",
        marker = list(color = colors, line = list(color = line_colors, width = 1)),
        hoverinfo = "text",
        text = ~paste0("<b>", team, "</b><br>", tooltip_label, ": ", format_table_currency(value), "<br><i>", range_label, "</i>")
      ) %>%
        fm_plot_layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = x_title, gridcolor = "#f1f5f9", zeroline = metric == "cash", zerolinecolor = "#cbd5e1"),
          yaxis = list(title = "", gridcolor = "#f1f5f9", zeroline = FALSE),
          margin = list(l = 120, r = 20, t = 10, b = 40)
        )
    })

    # Render Scouted Rival Details (or empty state prompt if no selection)
    output$scouted_rival_details_ui <- renderUI({
      req(is_module_active() == TRUE)
      if (!is.null(refresh_trigger)) refresh_trigger()
      rival_id <- selected_rival_team_id()

      if (is.null(rival_id) || rival_id == "") {
        div(
          style = "text-align: center; padding: 40px 20px; background: var(--fm-surface); border: 2px dashed #cbd5e1; border-radius: 12px; margin-top: 20px; margin-bottom: 25px;",
          shiny::tags$i(class = "fa-solid fa-user-ninja", style = "font-size: 36px; color: var(--fm-muted); margin-bottom: 12px;"),
          h4(style = "font-weight: 700; color: var(--fm-text); margin: 0 0 6px 0;", "No User Team Selected"),
          p(style = "color: var(--fm-muted); font-size: 14px; margin: 0;",
            icon("hand-pointer", style = "color: var(--fm-text); margin-right: 4px;"),
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
            ),
            tabPanel(title = "League Cash & Observed Transfers", icon = icon("chart-column"),
              box(
                title = "League Cash and Observed Transfers",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = FALSE,
                fluidRow(
                  column(width = 7,
                    sliderInput(
                      inputId = ns("buying_power_date_slider"),
                      label = "Date Range Window:",
                      min = as.Date(paste0(format(Sys.Date(), "%Y"), "-07-31")),
                      max = Sys.Date(),
                      value = c(as.Date(paste0(format(Sys.Date(), "%Y"), "-07-31")), Sys.Date()),
                      timeFormat = "%d/%m/%Y",
                      width = "100%"
                    )
                  ),
                  column(width = 5,
                    div(style = "margin-top: 5px;",
                      radioButtons(
                        inputId = ns("buying_power_metric"),
                        label = "Display Metric:",
                        choices = c("Transfer-only balance estimate" = "cash",
                          "Squad Purchases" = "investment",
                          "Transaction Volume" = "volume"),
                        selected = "cash",
                        inline = TRUE
                      )
                    )
                  )
                ),
                plotly::plotlyOutput(ns("league_finances_plot"), height = "300px")
              )
            )
          )
        )
      }
    })

    # ---- Transaction History Reactives ----

    # Raw money movements: fetch via API, compute running balance, build full reconstructed fallback if empty
    is_fallback <- reactiveVal(FALSE)

    rival_moneymovements_raw_RV <- reactive({
      req(is_module_active() == TRUE)
      if (!is.null(refresh_trigger)) refresh_trigger()
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

      # Check if movements has valid transaction data
      is_valid_movements <- !is.null(movements) && nrow(movements) > 0 &&
                            any(nzchar(movements$date)) &&
                            any(nzchar(movements$concept) | movements$type == "budget")

      # ---- PRIMARY PATH: API returned valid data ----
      if (is_valid_movements) {
        is_fallback(FALSE)

        # Parse ISO dates
        movements$timestamp <- parse_safe_datetime(movements$date)

        # Sort ascending by timestamp (chronological) for running balance calculation
        movements <- movements %>%
          dplyr::arrange(timestamp)

        # Calculate running balance on the FULL chronological dataset
        # A visible movement page does not prove a complete account ledger.
        # Keep historical balances unavailable without a verified opening balance.
        movements$running_balance <- rep(NA_real_, nrow(movements))

        # Batch consolidation: group by minute, compute batch_final_balance and is_batch_header
        movements$batch_key <- format(movements$timestamp, "%Y-%m-%d %H:%M")
        movements <- movements %>%
          dplyr::group_by(batch_key) %>%
          dplyr::mutate(
            batch_final_balance = running_balance[dplyr::n()],
            is_batch_header = dplyr::row_number() == dplyr::n()
          ) %>%
          dplyr::ungroup()

        # Order strictly descending (newest first), batch header row first within each timestamp group
        movements <- movements %>%
          dplyr::arrange(desc(timestamp), desc(is_batch_header))

        return(movements)
      }

      # Restricted money movements do not authorize inventing rewards, budgets,
      # dates or account balances. Show only settled transfers visible in feed.
      is_fallback(TRUE)
      pressroom <- tryCatch(get_championship_pressroom(login_token(), championship_id()), error = function(e) NULL)
      rivals_observed_transfers(pressroom, selected_rival_team_id())

    })

    # Filtered money movements: passthrough to raw data (filters removed)
    rival_moneymovements_filtered_RV <- reactive({
      tryCatch({
        rival_moneymovements_raw_RV()
      }, error = function(e) {
        NULL
      })
    })

    

    # Render Tab 2 UI: Transaction History
    output$rival_transactions_tab_ui <- renderUI({
      # Compute period summary from raw data
      filtered_tx <- rival_moneymovements_filtered_RV()
      total_inflow <- 0
      total_outflow <- 0
      net_flow <- 0
      if (!is.null(filtered_tx) && nrow(filtered_tx) > 0) {
        total_inflow <- sum(filtered_tx$money[filtered_tx$money > 0], na.rm = TRUE)
        total_outflow <- sum(filtered_tx$money[filtered_tx$money < 0], na.rm = TRUE)
        net_flow <- sum(filtered_tx$money, na.rm = TRUE)
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
              h3(style = "font-weight: 700; color: var(--fm-text); margin: 0; font-size: 18px;",
                format_table_currency(total_inflow)
              ),
              p(style = "color: var(--fm-muted); font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Inflow in Period")
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
              h3(style = "font-weight: 700; color: var(--fm-danger); margin: 0; font-size: 18px;",
                format_table_currency(total_outflow)
              ),
              p(style = "color: var(--fm-muted); font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Outflow in Period")
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
              p(style = "color: var(--fm-muted); font-size: 11px; text-transform: uppercase; margin-top: 5px;", "Net Cash Flow")
            )
          )
        )
      )

      tagList(
        # Pivot checkbox row
        fluidRow(
          style = "margin-bottom: 12px;",
          column(width = 12,
            checkboxInput(
              ns("tx_pivot_by_player"),
              "Pivot by Player (Buy/Sell Ledger)",
              value = FALSE
            )
          )
        ),

        if (is_fallback()) p(class = "alert alert-info", "Only observed transfers are available. Rewards, account adjustments and historical balances are unavailable."),
        # Period Summary Cards
        period_summary_cards,

        # Transaction Table
        reactable::reactableOutput(ns("rival_transactions_table"))
      )
    })

# Render Transaction Table
    output$rival_transactions_table <- reactable::renderReactable({
      if (isTRUE(input$tx_pivot_by_player)) {
        # Pivot by player: build the paired buy/sell ledger via the pure
        # helper (pairs by player_id, fallback name; raw dates; no Sold column).
        rival_id <- selected_rival_team_id()
        pressroom_df <- tryCatch({
          get_championship_pressroom(login = login_token(), championship_id = championship_id())
        }, error = function(e) NULL)

        ledger <- rivals_build_pivot_ledger(pressroom_df, rival_id)

        if (!is.null(ledger) && nrow(ledger) > 0) {
          return(reactable::reactable(
            ledger,
            compact = TRUE,
            striped = TRUE,
            highlight = TRUE,
            bordered = FALSE,
            defaultPageSize = 15,
            columns = list(
              Player = colDef(name = "Player", align = "left", style = list(fontWeight = "600")),
              Bought_Price = colDef(
                name = "Bought Price",
                align = "right",
                cell = function(val, Buy_Date, Buy_Type) {
                  d <- format(rivals_parse_datetime(Buy_Date), "%d/%m/%Y")
                  title_text <- paste0("Date: ", d, "\nType: ", Buy_Type)
                  shiny::tags$span(
                    style = "color: var(--fm-danger); font-weight: 600;",
                    title = title_text,
                    format_table_currency(val)
                  )
                }
              ),
              Sold_Price = colDef(
                name = "Sold Price",
                align = "right",
                cell = function(val, Sell_Date, Sell_Type) {
                  if (is.na(val)) {
                    shiny::tags$span(style = "color: var(--fm-muted);", "-")
                  } else {
                    d <- format(rivals_parse_datetime(Sell_Date), "%d/%m/%Y")
                    title_text <- paste0("Date: ", d, "\nType: ", Sell_Type)
                    shiny::tags$span(
                      style = "color: var(--fm-text); font-weight: 600;",
                      title = title_text,
                      format_table_currency(val)
                    )
                  }
                }
              ),
              Net_PL = colDef(
                name = "Net P/L",
                align = "right",
                cell = function(val) {
                  if (is.na(val)) {
                    shiny::tags$span(style = "color: var(--fm-muted);", "-")
                  } else if (val > 0) {
                    shiny::tags$span(style = "color: var(--fm-text); font-weight: 700;", paste0("+", format_table_currency(val)))
                  } else if (val < 0) {
                    shiny::tags$span(style = "color: var(--fm-danger); font-weight: 700;", format_table_currency(val))
                  } else {
                    shiny::tags$span(style = "color: var(--fm-muted);", format_table_currency(val))
                  }
                }
              ),
              # Helper columns (pairing keys + raw dates/types) are hidden.
              PlayerID = colDef(show = FALSE),
              Buy_Date = colDef(show = FALSE),
              Buy_Type = colDef(show = FALSE),
              Sell_Date = colDef(show = FALSE),
              Sell_Type = colDef(show = FALSE)
            )
          ))
        }

        # Fallback if no pressroom data
        return(reactable::reactable(
          data.frame(Status = "No pressroom transaction data available for pivot view."),
          columns = list(Status = colDef(name = "Pivot Status", align = "center")),
          compact = TRUE,
          bordered = FALSE
        ))
      }

      # Standard chronological transaction log
      filtered <- rival_moneymovements_filtered_RV()
      if (is.null(filtered) || nrow(filtered) == 0) {
        return(reactable::reactable(
          data.frame(Status = "No transaction movements recorded for this team yet."),
          columns = list(Status = colDef(name = "Transaction Log Status", align = "center")),
          compact = TRUE,
          bordered = FALSE
        ))
      }

      # Reorder columns so "Date" renders first (visible order), followed by
      # the hidden helper columns. Values, filtering, and the timestamp-based
      # default sort are all preserved.
      display_df <- rivals_tx_display_df(filtered)
      reactable::reactable(
        display_df,
        compact = TRUE,
        striped = TRUE,
        highlight = TRUE,
        bordered = FALSE,
        defaultPageSize = 15,
        # Timestamp-based default ordering (chronological, newest first) so
        # sorting is correct across years (the raw `date` string is not used).
        defaultSorted = list(timestamp = "desc"),
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
                style = paste0("background: ", badge_color, "; color: var(--fm-text); padding: 2px 8px; border-radius: 4px; font-size: 11px; font-weight: 600;"),
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
                shiny::tags$span(style = "color: var(--fm-text); font-weight: 600;", paste0("+", formatted))
              } else if (money_val < 0) {
                shiny::tags$span(style = "color: var(--fm-danger); font-weight: 600;", formatted)
              } else {
                shiny::tags$span(style = "color: var(--fm-muted);", formatted)
              }
            }
          ),
          running_balance = colDef(
            name = "Money Left After Transaction",
            align = "right",
            cell = function(rb_val, rowInfo) {
              is_hdr <- if (!is.null(rowInfo) && "is_batch_header" %in% names(rowInfo)) rowInfo$is_batch_header else TRUE
              if (isTRUE(is_hdr)) {
                formatted <- ui_financial_amount(rb_val)
                shiny::tags$span(style = "font-weight: 700; color: var(--fm-text);", formatted)
              } else {
                shiny::tags$span(style = "color: var(--fm-muted);", "-")
              }
            }
          ),
          # Helper / internal columns are hidden from the transaction log.
          id = colDef(show = FALSE),
          category = colDef(show = FALSE),
          timestamp = colDef(show = FALSE),
          batch_key = colDef(show = FALSE),
          is_batch_header = colDef(show = FALSE),
          batch_final_balance = colDef(show = FALSE)
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
      user_team_id = user_team_id, # Actions execute on the logged-in team only.
      refresh_trigger = refresh_trigger
    )
    
    return(selected_player_RV)
  })
}
# Restricted account histories can be supplemented with observed pressroom
# transfers, but never with fabricated budget, reward, date or balance records.
rivals_observed_transfers <- function(pressroom_df, rival_id) {
  empty <- data.frame(id=character(),concept=character(),type=character(),category=character(),
                      money=numeric(),date=character(),running_balance=numeric(),
                      timestamp=as.POSIXct(character()),batch_key=character(),is_batch_header=logical(),batch_final_balance=numeric())
  if (!is.data.frame(pressroom_df) || !nrow(pressroom_df) || length(rival_id)!=1L || is.na(rival_id) || !nzchar(rival_id)) return(empty)
  required <- c("buyer_team_id","seller_team_id","created","price")
  if (!all(required %in% names(pressroom_df))) return(empty)
  buyer <- as.character(pressroom_df$buyer_team_id)
  seller <- as.character(pressroom_df$seller_team_id)
  involved <- (!is.na(buyer) & buyer == rival_id) | (!is.na(seller) & seller == rival_id)
  rows <- pressroom_df[involved,,drop=FALSE]
  if (!nrow(rows)) return(empty)
  if ("id" %in% names(rows)) {
    ids <- as.character(rows$id)
    known <- !is.na(ids) & nzchar(ids)
    rows <- rows[!(known & duplicated(ids)),,drop=FALSE]
  }
  timestamps <- rivals_parse_datetime(rows$created)
  amounts <- suppressWarnings(as.numeric(rows$price))
  usable <- !is.na(timestamps) & is.finite(amounts) & amounts >= 0
  rows <- rows[usable,,drop=FALSE]; timestamps <- timestamps[usable]; amounts <- amounts[usable]
  if (!nrow(rows)) return(empty)
  buying <- !is.na(rows$buyer_team_id) & as.character(rows$buyer_team_id)==rival_id
  names <- if ("player_name"%in%names(rows)) as.character(rows$player_name) else rep("Player",nrow(rows))
  names[is.na(names) | !nzchar(names)] <- "Player"
  ids <- if ("id"%in%names(rows)) as.character(rows$id) else rep(NA_character_,nrow(rows))
  result <- data.frame(id=ids,concept=paste0(names,ifelse(buying," (Purchased)"," (Sold)")),
    type=ifelse(buying,"buy","sell"),category="market",money=ifelse(buying,-amounts,amounts),
    date=as.character(rows$created),running_balance=NA_real_,timestamp=timestamps,
    batch_key=format(timestamps,"%Y-%m-%d %H:%M"),is_batch_header=TRUE,batch_final_balance=NA_real_,stringsAsFactors=FALSE)
  result[order(result$timestamp,decreasing=TRUE),,drop=FALSE]
}
