library(reactable)

# Transparent 1x1 GIF to prevent broken-image alt-text rendering on pre-load
SPACER_GIF <- "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"

selected_player_UI <- function(id) {
  ns <- NS(id)
  tagList(
    userBox(
      id = ns("selected_player_box"),
      width = 12,
      title = userDescription(
        title = "Player Name",
        subtitle = "Position & Team",
        type = 1,
        image = SPACER_GIF
      ),
      status = "primary",
      gradient = TRUE,
      background = "light-blue",
      boxToolSize = "xl",
      collapsible = FALSE,
      footer = tagList(
        fluidRow(
          column(4, uiOutput(ns("player_points_description_box"))),
          column(4, uiOutput(ns("player_last_points_description_box"))),
          column(4, uiOutput(ns("player_value_description_box")))
        ),
        # Interactive Purchase Row
        fluidRow(
          column(12, align = "center", style = "margin-top: 15px; display: flex; justify-content: center; gap: 15px; flex-wrap: wrap;",
                 uiOutput(ns("buy_market_ui")),
                 uiOutput(ns("buy_clause_ui")))
        ),
        # Plotly History Chart Row (Plot A)
        fluidRow(
          style = "margin-top: 25px; padding-top: 20px; border-top: 1px solid #f1f5f9;",
          column(12,
                 h4(style = "font-weight: 600; color: #0f172a; margin-bottom: 15px;", "Historical Valuation & Performance"),
                 plotly::plotlyOutput(ns("player_trend_plot"), height = "280px")
          )
        )
      )
    ),
    # Action buttons area (market bid / clause purchase)
    div(
      id = ns("action_buttons_container"),
      style = "margin-top: 12px; display: flex; flex-wrap: wrap; gap: 8px; justify-content: center;"
    )
  )
}


selected_player_Server <- function(id, selected_player, login_token = NULL, championship_id = NULL, user_team_id = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Main observer: populate box + action buttons ----
    observeEvent(
      {
        selected_player()
      },
      {
        sp <- selected_player()
        req(sp)

        print(paste0("Selected player: ", sp$name))
        player_name <- sp$name
        role_text <- sp$role
        if (!is.na(sp$role2) && sp$role2 != "") {
          role_text <- paste(role_text, sp$role2, sep = ", ")
        }

        # Dynamically build integrated team logo emblem & name
        team_logo <- NULL
        if ("team" %in% colnames(sp) && !is.na(sp$team) && sp$team != "") {
          team_image_name <- get_team_image_name(sp$team)
          
          logo_tag <- if (team_image_name != "") {
            img(src = paste0(TEAM_LOGO_URL, team_image_name, ".png"), 
                style = "height: 18px; width: auto; object-fit: contain; background: transparent;", 
                alt = sp$team,
                onerror = "this.style.display='none';")
          } else {
            NULL
          }
          
          team_logo <- shiny::tags$div(
            style = "margin-top: 6px; display: flex; align-items: center; gap: 8px; font-weight: 500; font-size: 13px; color: #cbd5e1;",
            logo_tag,
            sp$team
          )
        }

        sub_title_markup <- tagList(
          shiny::tags$span(style = "display: block; font-weight: 600; font-size: 13px; color: #94a3b8; text-transform: uppercase; letter-spacing: 0.5px;", role_text),
          team_logo
        )

        shinydashboardPlus::updateBox(
          id = "selected_player_box",
          action = "update",
          options = list(
            title = shinydashboardPlus::userDescription(
              title = player_name,
              subtitle = sub_title_markup,
              type = 1,
              image = paste0(PHOTO_URL, "/", sp$photo)
            ),
            status = "red",
            background = NULL,
            width = 12
          )
        )

        # ---- Build action buttons ----
        action_buttons <- tagList()

        # "Bid on Market" button: show when player has a market_price
        if ("market_price" %in% colnames(sp) && !is.na(sp$market_price) && !is.null(sp$market_price)) {
          action_buttons <- tagList(
            action_buttons,
            actionButton(
              ns("btn_bid_market"),
              label = tagList(icon("shopping-cart"), " Bid on Market"),
              class = "btn btn-buy-market"
            )
          )
        }

        # "Pay Clause" button: show when player has an active release clause
        if ("clause_price" %in% colnames(sp) && !is.na(sp$clause_price) && !is.null(sp$clause_price) &&
            ("isClause" %in% colnames(sp) && isTRUE(sp$isClause))) {
          clause_label <- paste0(" Pay Clause: ", format_currency(sp$clause_price))
          action_buttons <- tagList(
            action_buttons,
            actionButton(
              ns("btn_pay_clause"),
              label = tagList(icon("fire"), clause_label),
              class = "btn btn-buy-clause"
            )
          )
        }

        output$action_buttons <- renderUI(action_buttons)
        insertUI(
          selector = paste0("#", ns("action_buttons_container")),
          ui = action_buttons,
          multiple = TRUE
        )
      }
    )

    # ---- Clear stale inserted buttons on re-selection ----
    observe({
      selected_player()
      # Remove previously inserted buttons before re-adding
      removeUI(selector = paste0("#", ns("action_buttons_container"), " > *"))
    })

    # ---- Bid on Market: modal ----
    observeEvent(input$btn_bid_market, {
      sp <- selected_player()
      req(sp)
      market_price <- sp$market_price

      showModal(modalDialog(
        title = tagList(icon("shopping-cart"), " Place Bid on Market"),
        p(strong(sp$name)),
        p("Current market price: ", strong(format_currency(market_price))),
        numericInput(
          ns("bid_amount"),
          label = "Your bid amount:",
          value = market_price,
          min = 1,
          step = 1
        ),
        footer = tagList(
          modalButton(),
          actionButton(ns("submit_bid"), "Submit Bid", class = "btn btn-buy-market")
        ),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Bid ----
    observeEvent(input$submit_bid, {
      sp <- selected_player()
      req(sp, login_token(), championship_id(), user_team_id())

      bid_amount <- input$bid_amount
      if (is.null(bid_amount) || bid_amount <= 0) {
        shiny::showNotification("Please enter a valid bid amount.", type = "warning")
        return()
      }

      login <- login_token()
      champ_id <- championship_id()
      team_id <- user_team_id()

      player_id <- sp$id
      player_slug <- if ("slug" %in% colnames(sp) && !is.na(sp$slug)) sp$slug else sp$name

      success <- buy_clause(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id,
        player_slug = player_slug,
        price = bid_amount,
        isClause = FALSE
      )

      removeModal()

      if (success) {
        tryCatch({
          log_market_transaction(
            player_id = player_id,
            championship_id = champ_id,
            buyer_team_id = team_id,
            seller_team_id = if ("user_team_id" %in% colnames(sp)) sp$user_team_id else NULL,
            price = bid_amount,
            is_clause = FALSE
          )
        }, error = function(e) {
          print(paste0("[Supabase] Bid log warning: ", e$message))
        })
        shiny::showNotification(
          paste0("Bid of ", format_currency(bid_amount), " placed successfully on ", sp$name, "!"),
          type = "message",
          duration = 5
        )
        clear_api_cache()
      } else {
        shiny::showNotification(
          "Bid failed. Please try again.",
          type = "error",
          duration = 5
        )
      }
    })

    # ---- Pay Clause: modal ----
    observeEvent(input$btn_pay_clause, {
      sp <- selected_player()
      req(sp)
      clause_price <- sp$clause_price

      showModal(modalDialog(
        title = tagList(icon("fire"), " Confirm Clause Purchase"),
        p(strong(sp$name)),
        p("This will purchase the player for their release clause."),
        p("Clause price: ", strong(format_currency(clause_price))),
        p(style = "color: #f59e0b; font-size: 13px;", "Are you sure you want to proceed?"),
        footer = tagList(
          modalButton(),
          actionButton(ns("submit_clause"), "Confirm Purchase", class = "btn btn-buy-clause")
        ),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Clause Purchase ----
    observeEvent(input$submit_clause, {
      sp <- selected_player()
      req(sp, login_token(), championship_id(), user_team_id())

      clause_price <- sp$clause_price

      login <- login_token()
      champ_id <- championship_id()
      team_id <- user_team_id()

      player_id <- sp$id
      player_slug <- if ("slug" %in% colnames(sp) && !is.na(sp$slug)) sp$slug else sp$name

      success <- buy_clause(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id,
        player_slug = player_slug,
        price = clause_price,
        isClause = TRUE
      )

      removeModal()

      if (success) {
        tryCatch({
          log_market_transaction(
            player_id = player_id,
            championship_id = champ_id,
            buyer_team_id = team_id,
            seller_team_id = if ("user_team_id" %in% colnames(sp)) sp$user_team_id else NULL,
            price = clause_price,
            is_clause = TRUE
          )
        }, error = function(e) {
          print(paste0("[Supabase] Clause log warning: ", e$message))
        })
        shiny::showNotification(
          paste0("Successfully purchased ", sp$name, " for ", format_currency(clause_price), "!"),
          type = "message",
          duration = 5
        )
        clear_api_cache()
      } else {
        shiny::showNotification(
          "Clause purchase failed. Please try again.",
          type = "error",
          duration = 5
        )
      }
    })

    ## render player_trend_plot (Plot A) ----
    output$player_trend_plot <- plotly::renderPlotly({
      sp <- selected_player()
      req(sp)

      champ_id <- if (!is.null(championship_id)) championship_id() else NULL
      player_id <- sp$id

      history_df <- NULL
      if (!is.null(champ_id) && !is.null(player_id)) {
        tryCatch({
          history_df <- get_player_historical_data(player_id, champ_id)
        }, error = function(e) {
          print(paste0("[Plot A] Error loading history: ", e$message))
        })
      }

      # Fallback simulated data if DB is empty or unconfigured (pre-season)
      if (is.null(history_df) || nrow(history_df) == 0) {
        today <- Sys.time()
        dates <- seq(today - as.difftime(6, units="days"), today, by="1 day")
        val_today <- if ("value" %in% colnames(sp)) as.numeric(sp$value) else 1000000
        val_change <- if ("change" %in% colnames(sp)) as.numeric(sp$change) else 0

        history_df <- data.frame(
          recorded_at = as.character(dates),
          value = seq(val_today - val_change, val_today, length.out = length(dates)),
          points = c(rep(0, length(dates) - 1), if ("points" %in% colnames(sp)) as.integer(sp$points) else 0),
          stringsAsFactors = FALSE
        )
      }

      # Format dates
      history_df$date <- as.POSIXct(history_df$recorded_at, format = "%Y-%m-%dT%H:%M:%S")
      if (any(is.na(history_df$date))) {
        history_df$date <- as.POSIXct(history_df$recorded_at)
      }

      history_df <- history_df %>% dplyr::arrange(date)

      # Build interactive plotly dual-axis chart
      plotly::plot_ly(data = history_df) %>%
        plotly::add_lines(
          x = ~date, y = ~value,
          name = "Valuation (EUR)",
          line = list(color = "#f59e0b", width = 3, shape = "spline"),
          fill = "tozeroy", fillcolor = "rgba(245, 158, 11, 0.05)",
          hoverinfo = "text",
          text = ~paste0("Date: ", format(date, "%d-%m-%y"), "<br>Valuation: ", format_table_currency(value))
        ) %>%
        plotly::add_bars(
          x = ~date, y = ~points,
          name = "Points",
          yaxis = "y2",
          marker = list(color = "rgba(16, 185, 129, 0.6)", line = list(color = "#10b981", width = 1)),
          hoverinfo = "text",
          text = ~paste0("Date: ", format(date, "%d-%m-%y"), "<br>Points: ", points)
        ) %>%
        plotly::layout(
          hovermode = "x unified",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(
            title = "",
            gridcolor = "#f1f5f9",
            zeroline = FALSE,
            tickformat = "%d-%m"
          ),
          yaxis = list(
            title = "Valuation (EUR)",
            gridcolor = "#f1f5f9",
            zeroline = FALSE,
            tickformat = "s"
          ),
          yaxis2 = list(
            title = "Points",
            overlaying = "y",
            side = "right",
            zeroline = FALSE,
            showgrid = FALSE
          ),
          legend = list(orientation = "h", x = 0.5, y = -0.25, xanchor = "center"),
          margin = list(l = 50, r = 50, t = 10, b = 40)
        )
    })

    ## render player_points_description_box ----
    output$player_points_description_box <- renderUI({
      sp <- selected_player()
      req(sp)
      ret <- tagList()
      points <- sp$points

      clean_points <- if (is.null(points) || is.na(points) || points == "NaN" || points == "") {
        "0"
      } else {
        as.character(points)
      }

      block <- descriptionBlock(
        header = clean_points,
        number = NULL,
        numberColor = "black",
        text = "Total Points"
      )
      ret <- tagList(
        ret,
        block
      )
      return(ret)
    })

    ## render player_last_points_description_box ----
    output$player_last_points_description_box <- renderUI({
      sp <- selected_player()
      req(sp)
      ret <- tagList()
      total_last_points <- sp$average.total
      avg_last_points <- sp$average.averageLastFive

      clean_total <- if (is.null(total_last_points) || is.na(total_last_points) || total_last_points == "NaN" || total_last_points == "") {
        "0"
      } else {
        as.character(total_last_points)
      }

      # Handle NaN / "NaN" / NA in averages safely -- omit "Avg:" if unavailable
      if (is.null(avg_last_points) || is.na(avg_last_points) || avg_last_points == "NaN" || avg_last_points == "") {
        header_text <- clean_total
      } else {
        header_text <- paste0(clean_total, " (Avg: ", round(as.numeric(avg_last_points), 1), ")")
      }

      block <- descriptionBlock(
        header = header_text,
        number = NULL,
        numberColor = "black",
        text = "Last 5 matches"
      )
      ret <- tagList(
        ret,
        block
      )
      return(ret)
    })

    ## render player_value_description_box ----
    output$player_value_description_box <- renderUI({
      sp <- selected_player()
      req(sp)
      value <- sp$value
      change <- sp$change
      change_pct <- sp$change_by_value * 100
      if (change > 0) {
        icon <- icon("caret-up")
        number_color = "green"
      } else if (change < 0) {
        icon <- icon("caret-down")
        number_color = "red"
      } else {
        icon <- NULL
        number_color = "black"
      }
      descriptionBlock(
        header = format_currency(value),
        number = paste0(add_sign(format_currency(change)), " (", round(change_pct, 2), "%)"),
        numberColor = number_color,
        numberIcon = icon,
        text = "Value"
      )
    })

    ## render selected_player_table ----
    output$selected_player_table <- renderReactable({
      sp <- selected_player()
      req(sp)
      sp <- sp %>%
        dplyr::select(photo, name, role, role2, points, value)
      reactable(sp,
        searchable = TRUE,
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
        columns = list(
          photo = colDef(
            name = "Photo",
            cell = function(img, index) {
              player_name <- sp[index, "name"]
              image <- img(src = paste0(PHOTO_URL, "/", img), style = "height: 24px;", alt = player_name)
              tagList(
                div(style = "display: inline-block; width: 45px;", image)
              )
            }
          )
        )
      )
    })
  })
}

add_sign <- function(x) {
  # Handle formatted currency strings like "-10.000 \u20ac" without double signs
  if (is.character(x)) {
    # Already starts with + or -, return as-is
    if (grepl("^[+\\-]", x)) {
      return(x)
    }
    # Positive number without sign: prepend +
    return(paste0("+", x))
  }
  # Numeric path
  if (x > 0) {
    return(paste0("+", x))
  } else {
    return(as.character(x))
  }
}