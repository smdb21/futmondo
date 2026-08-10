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
          column(12, align = "center", style = "margin-top: 15px; display: flex; justify-content: center; gap: 12px; flex-wrap: wrap;",
                 uiOutput(ns("action_buttons")))
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
    )
  )
}


selected_player_Server <- function(id, selected_player, login_token = NULL, championship_id = NULL, user_team_id = NULL, on_bid_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    active_bid_info_RV <- reactiveVal(NULL)

    # ---- Safe reactive value extractor ----
    get_reactive_val <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.reactive(x) || is.function(x)) {
        tryCatch(x(), error = function(e) NULL)
      } else {
        x
      }
    }

    # ---- Main observer: populate box + action buttons ----
    observeEvent(
      {
        selected_player()
      },
      {
        sp <- selected_player()
        req(sp)

        login <- get_reactive_val(login_token)
        champ_id <- get_reactive_val(championship_id)
        team_id <- get_reactive_val(user_team_id)

        my_bid_id <- NULL
        my_bid_price <- NULL

        if (!is.null(login) && !is.null(champ_id) && !is.null(sp$id)) {
          summary_res <- tryCatch({
            get_player_summary(login = login, championship_id = champ_id, user_team_id = team_id, player_id = sp$id)
          }, error = function(e) NULL)

          if (!is.null(summary_res)) {
            my_bid_id <- summary_res$my_bid_id
            my_bid_price <- summary_res$my_bid_price
          }
        }

        # Fallback to sp$bid_price if summary API didn't return my_bid_price
        if (is.null(my_bid_price) && "bid_price" %in% colnames(sp) && !is.na(sp$bid_price) && suppressWarnings(as.numeric(sp$bid_price)) > 0) {
          my_bid_price <- suppressWarnings(as.numeric(sp$bid_price))
          if ("bid_id" %in% colnames(sp) && !is.na(sp$bid_id)) {
            my_bid_id <- as.character(sp$bid_id)
          }
        }

        if (!is.null(my_bid_price) && my_bid_price > 0) {
          active_bid_info_RV(list(id = my_bid_id, price = my_bid_price))
        } else {
          active_bid_info_RV(NULL)
        }

        print(paste0("Selected player: ", sp$name))
        player_name <- sp$name
        role_text <- sp$role

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

        current_user_team <- get_reactive_val(user_team_id)
        player_owner_team <- if ("user_team_id" %in% colnames(sp)) sp$user_team_id else NULL
        is_own_player <- (!is.null(current_user_team) && !is.null(player_owner_team) && current_user_team == player_owner_team)
        has_active_bid <- !is.null(active_bid_info_RV())

        if (is_own_player) {
          # ---- Player belongs to user's OWN squad ----
          is_listed_on_market <- FALSE
          current_asking_price <- NA_real_

          if ("effective_market_price" %in% colnames(sp) && !is.na(sp$effective_market_price) && suppressWarnings(as.numeric(sp$effective_market_price)) > 0) {
            current_asking_price <- suppressWarnings(as.numeric(sp$effective_market_price))
            is_listed_on_market <- TRUE
          } else if ("market_price" %in% colnames(sp) && !is.na(sp$market_price) && suppressWarnings(as.numeric(sp$market_price)) > 0) {
            current_asking_price <- suppressWarnings(as.numeric(sp$market_price))
            is_listed_on_market <- TRUE
          } else if ("price" %in% colnames(sp) && !is.na(sp$price) && suppressWarnings(as.numeric(sp$price)) > 0) {
            current_asking_price <- suppressWarnings(as.numeric(sp$price))
            is_listed_on_market <- TRUE
          } else if ("market_inMarket" %in% colnames(sp) && isTRUE(as.logical(sp$market_inMarket))) {
            is_listed_on_market <- TRUE
          } else if (!is.null(login) && !is.null(champ_id) && !is.null(team_id)) {
            my_mkt_players <- tryCatch({
              get_my_market_players(login = login, championship_id = champ_id, user_team_id = team_id)
            }, error = function(e) NULL)
            if (!is.null(my_mkt_players) && nrow(my_mkt_players) > 0 && "id" %in% colnames(my_mkt_players)) {
              if (as.character(sp$id) %in% as.character(my_mkt_players$id)) {
                is_listed_on_market <- TRUE
                match_p <- my_mkt_players[which(as.character(my_mkt_players$id) == as.character(sp$id)), ]
                if ("price" %in% colnames(match_p) && !is.na(match_p$price)) {
                  current_asking_price <- suppressWarnings(as.numeric(match_p$price))
                }
              }
            }
          }

          own_badge <- div(
            style = "display: inline-block; padding: 8px 16px; background-color: #e0f2fe; color: #0369a1; border: 1px solid #bae6fd; border-radius: 8px; font-weight: 600; font-size: 12px; margin: 5px;",
            tagList(icon("shield-halved"), " Player in Your Squad")
          )

          if (is_listed_on_market) {
            price_text <- if (!is.na(current_asking_price) && current_asking_price > 0) paste0(" (Asking: ", format_currency(current_asking_price), ")") else ""
            listed_badge <- div(
              style = "display: inline-block; padding: 8px 16px; background-color: #fef3c7; color: #b45309; border: 1px solid #fde68a; border-radius: 8px; font-weight: 600; font-size: 12px; margin: 5px;",
              tagList(icon("tags"), paste0(" Listed on Market", price_text))
            )
            btn_update_sale <- actionButton(
              ns("btn_put_on_market"),
              label = tagList(icon("pen-to-square"), " Update Listing Price"),
              class = "btn btn-offer-money"
            )
            btn_cancel_sell <- actionButton(
              ns("btn_cancel_sell"),
              label = tagList(icon("tag"), " Remove from Market"),
              class = "btn btn-cancel-bid"
            )
            action_buttons <- tagList(own_badge, listed_badge, btn_update_sale, btn_cancel_sell)
          } else {
            btn_put_on_market <- actionButton(
              ns("btn_put_on_market"),
              label = tagList(icon("tags"), " Put on Market for Sale"),
              class = "btn btn-offer-money"
            )
            action_buttons <- tagList(own_badge, btn_put_on_market)
          }
        } else if (has_active_bid) {
          # ---- Player belongs to rival/market AND current user has an active BUY bid ----
          bid_info <- active_bid_info_RV()
          banner <- div(
            style = "width: 100%; text-align: center; margin-bottom: 10px; padding: 10px 16px; background-color: #d1fae5; color: #047857; border: 1px solid #a7f3d0; border-radius: 8px; font-weight: 700; font-size: 14px;",
            tagList(icon("hand-holding-dollar"), paste0(" Your Active Bid: ", format_currency(bid_info$price)))
          )
          btn_modify <- actionButton(
            ns("btn_modify_bid"),
            label = tagList(icon("pen-to-square"), " Update Bid"),
            class = "btn btn-buy-market"
          )
          btn_cancel <- actionButton(
            ns("btn_cancel_bid"),
            label = tagList(icon("trash-can"), " Cancel Bid"),
            class = "btn btn-cancel-bid"
          )
          action_buttons <- tagList(banner, btn_modify, btn_cancel)
        } else {
          # ---- Player belongs to rival/market AND current user has NO active bid ----
          # Extract effective market price (checking effective_market_price, market_price, and price)
          eff_market_price <- NA_real_
          if ("effective_market_price" %in% colnames(sp) && !is.na(sp$effective_market_price) && suppressWarnings(as.numeric(sp$effective_market_price)) > 0) {
            eff_market_price <- suppressWarnings(as.numeric(sp$effective_market_price))
          } else if ("market_price" %in% colnames(sp) && !is.na(sp$market_price) && suppressWarnings(as.numeric(sp$market_price)) > 0) {
            eff_market_price <- suppressWarnings(as.numeric(sp$market_price))
          } else if ("price" %in% colnames(sp) && !is.na(sp$price) && suppressWarnings(as.numeric(sp$price)) > 0) {
            eff_market_price <- suppressWarnings(as.numeric(sp$price))
          }

          # Extract release clause parameters
          clause_price_val <- if ("clause_price" %in% colnames(sp) && !is.na(sp$clause_price)) suppressWarnings(as.numeric(sp$clause_price)) else 0
          is_clause_transferred <- if ("clause_transferred" %in% colnames(sp) && !is.na(sp$clause_transferred)) isTRUE(as.logical(sp$clause_transferred)) else FALSE

          # Check clause date lock against current time
          is_clause_date_locked <- FALSE
          clause_date_formatted <- ""
          if ("clause_date" %in% colnames(sp) && !is.na(sp$clause_date) && sp$clause_date != "") {
            clause_time <- suppressWarnings(as.POSIXct(sp$clause_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
            if (is.na(clause_time)) {
              clause_time <- suppressWarnings(as.POSIXct(sp$clause_date, tz = "UTC"))
            }
            if (!is.na(clause_time)) {
              clause_date_formatted <- format(clause_time, "%d-%m-%Y %H:%M")
              if (clause_time > Sys.time()) {
                is_clause_date_locked <- TRUE
              }
            }
          }

          # Determine if release clause is OPEN for buyout
          is_clause_open <- (clause_price_val > 0) && !is_clause_transferred && !is_clause_date_locked

          # Option 1: "Make Market Offer" button when player is listed on market
          if (!is.na(eff_market_price) && eff_market_price > 0) {
            action_buttons <- tagList(
              action_buttons,
              actionButton(
                ns("btn_bid_market"),
                label = tagList(icon("hand-holding-dollar"), " Make Market Offer"),
                class = "btn btn-buy-market"
              )
            )
          }

          # Option 2: "Offer to Owner" button when player is owned by a rival user team
          if (!is.null(player_owner_team) && player_owner_team != "" && (is.na(eff_market_price) || eff_market_price <= 0)) {
            owner_name <- if ("userTeam" %in% colnames(sp) && !is.na(sp$userTeam)) sp$userTeam else if ("teamname" %in% colnames(sp) && !is.na(sp$teamname)) sp$teamname else "Owner"
            action_buttons <- tagList(
              action_buttons,
              actionButton(
                ns("btn_offer_owner"),
                label = tagList(icon("hand-holding-dollar"), paste0(" Offer to ", owner_name)),
                class = "btn btn-offer-money"
              )
            )
          }

          # Option 3: "Buy Release Clause" button when clause is OPEN
          if (is_clause_open) {
            clause_label <- paste0(" Buy Clause: ", format_currency(clause_price_val))
            action_buttons <- tagList(
              action_buttons,
              actionButton(
                ns("btn_pay_clause"),
                label = tagList(icon("bolt"), clause_label),
                class = "btn btn-buy-clause"
              )
            )
          } else if (clause_price_val > 0) {
            # Release clause exists but is currently LOCKED
            lock_reason <- if (clause_date_formatted != "") paste0("until ", clause_date_formatted) else "transferred/cooldown"
            locked_badge <- div(
              style = "display: inline-block; padding: 8px 16px; background-color: #fef3c7; color: #b45309; border: 1px solid #fde68a; border-radius: 8px; font-weight: 600; font-size: 12px; margin: 5px;",
              tagList(icon("lock"), paste0(" Release Clause Locked ", lock_reason, " (", format_currency(clause_price_val), ")"))
            )
            action_buttons <- tagList(action_buttons, locked_badge)
          }
        }

        output$action_buttons <- renderUI(action_buttons)
      }
    )

    # ---- Modify Active Bid Modal ----
    observeEvent(input$btn_modify_bid, {
      sp <- selected_player()
      req(sp)
      bid_info <- active_bid_info_RV()
      req(bid_info)

      showModal(modalDialog(
        title = tagList(icon("pen-to-square"), " Update Your Active Bid"),
        p(strong(sp$name)),
        p("Current Active Bid: ", strong(format_currency(bid_info$price))),
        numericInput(
          ns("new_bid_amount"),
          label = "New Bid Amount (EUR):",
          value = bid_info$price,
          min = 1,
          step = 10000
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("submit_modify_bid"), "Submit Updated Bid", class = "btn btn-buy-market")
        ),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Modify Active Bid ----
    observeEvent(input$submit_modify_bid, {
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      bid_info <- active_bid_info_RV()
      req(sp, login, champ_id, team_id, bid_info)

      new_price <- input$new_bid_amount
      if (is.null(new_price) || new_price <= 0) {
        shiny::showNotification("Please enter a valid bid amount.", type = "warning")
        return()
      }

      bid_id <- bid_info$id
      player_id <- sp$id

      success <- modify_bid(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id,
        bid_id = bid_id,
        new_price = new_price
      )

      removeModal()

      if (success) {
        tryCatch({
          log_market_transaction(
            player_id = player_id,
            championship_id = champ_id,
            buyer_team_id = team_id,
            seller_team_id = if ("user_team_id" %in% colnames(sp)) sp$user_team_id else NULL,
            price = new_price,
            is_clause = FALSE
          )
        }, error = function(e) NULL)
        shiny::showNotification(
          paste0("Active bid updated to ", format_currency(new_price), " for ", sp$name, "!"),
          type = "message",
          duration = 5
        )
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(player_id = player_id, new_bid_price = new_price, is_cancel = FALSE), error = function(e) NULL)
        }
        clear_api_cache()
      } else {
        shiny::showNotification(
          "Failed to update bid. Please try again.",
          type = "error",
          duration = 5
        )
      }
    })

    # ---- Cancel Active Bid Modal ----
    observeEvent(input$btn_cancel_bid, {
      sp <- selected_player()
      req(sp)
      bid_info <- active_bid_info_RV()
      req(bid_info)

      showModal(modalDialog(
        title = tagList(icon("trash-can"), " Cancel Active Bid"),
        p(strong(sp$name)),
        p("Are you sure you want to cancel your active bid of ", strong(format_currency(bid_info$price)), "?"),
        p(style = "color: #ef4444; font-size: 13px;", "This will withdraw your offer from the transfer market."),
        footer = tagList(
          modalButton("Keep Bid"),
          actionButton(ns("submit_cancel_bid"), "Confirm Cancel Bid", class = "btn btn-cancel-bid")
        ),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Cancel Active Bid ----
    observeEvent(input$submit_cancel_bid, {
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      bid_info <- active_bid_info_RV()
      req(sp, login, champ_id, team_id, bid_info)

      bid_id <- bid_info$id

      success <- cancel_bid(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        bid_id = bid_id
      )

      removeModal()

      if (success) {
        shiny::showNotification(
          paste0("Active bid on ", sp$name, " cancelled successfully!"),
          type = "message",
          duration = 5
        )
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(player_id = player_id, new_bid_price = NA_real_, is_cancel = TRUE), error = function(e) NULL)
        }
        clear_api_cache()
      } else {
        shiny::showNotification(
          "Failed to cancel bid. Please try again.",
          type = "error",
          duration = 5
        )
      }
    })

    # ---- Option 1: Market Offer Modal ----
    observeEvent(input$btn_bid_market, {
      sp <- selected_player()
      req(sp)
      market_price <- if ("effective_market_price" %in% colnames(sp) && !is.na(sp$effective_market_price)) suppressWarnings(as.numeric(sp$effective_market_price)) else if ("market_price" %in% colnames(sp) && !is.na(sp$market_price)) suppressWarnings(as.numeric(sp$market_price)) else if ("price" %in% colnames(sp) && !is.na(sp$price)) suppressWarnings(as.numeric(sp$price)) else 1000000

      showModal(modalDialog(
        title = tagList(icon("hand-holding-dollar"), " Place Market Offer"),
        p(strong(sp$name)),
        p("Current market price: ", strong(format_currency(market_price))),
        numericInput(
          ns("bid_amount"),
          label = "Your offer amount (EUR):",
          value = market_price,
          min = 1,
          step = 10000
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("submit_bid"), "Submit Market Offer", class = "btn btn-buy-market")
        ),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Market Offer ----
    observeEvent(input$submit_bid, {
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp, login, champ_id, team_id)

      bid_amount <- input$bid_amount
      if (is.null(bid_amount) || bid_amount <= 0) {
        shiny::showNotification("Please enter a valid offer amount.", type = "warning")
        return()
      }

      player_id <- sp$id
      player_slug <- if ("slug" %in% colnames(sp) && !is.na(sp$slug)) sp$slug else sp$name

      res <- buy_clause(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id,
        player_slug = player_slug,
        price = bid_amount,
        isClause = FALSE
      )

      is_success <- if (is.list(res)) isTRUE(res$success) else isTRUE(res)

      removeModal()

      if (is_success) {
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
          paste0("Market offer of ", format_currency(bid_amount), " submitted successfully for ", sp$name, "!"),
          type = "message",
          duration = 5
        )
        clear_api_cache()
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(player_id = player_id, new_bid_price = bid_amount, is_cancel = FALSE), error = function(e) NULL)
        }
      } else {
        err_msg <- if (is.list(res) && !is.null(res$message) && res$message != "") res$message else "Please verify your funds and try again."
        shiny::showNotification(
          paste0("Offer failed: ", err_msg),
          type = "error",
          duration = 6
        )
      }
    })

    # ---- Option 2: Direct Offer to Owner Modal ----
    observeEvent(input$btn_offer_owner, {
      sp <- selected_player()
      req(sp)
      default_offer <- if ("value" %in% colnames(sp) && !is.na(sp$value) && sp$value > 0) sp$value else 1000000
      owner_name <- if ("userTeam" %in% colnames(sp) && !is.na(sp$userTeam)) sp$userTeam else if ("teamname" %in% colnames(sp) && !is.na(sp$teamname)) sp$teamname else "Owner"
      clause_info <- if ("clause_price" %in% colnames(sp) && !is.na(sp$clause_price) && sp$clause_price > 0) paste0(" (Release Clause: ", format_currency(sp$clause_price), ")") else ""

      showModal(modalDialog(
        title = tagList(icon("hand-holding-dollar"), paste0(" Offer Money to ", owner_name)),
        p(strong(sp$name), clause_info),
        p("Current Market Valuation: ", strong(format_currency(default_offer))),
        numericInput(
          ns("owner_offer_amount"),
          label = "Your purchase offer amount (EUR):",
          value = default_offer,
          min = 1,
          step = 10000
        ),
        p(style = "color: #64748b; font-size: 12px;", "This offer will be submitted to the player owner and tracked in market transaction history."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("submit_owner_offer"), "Submit Offer", class = "btn btn-offer-money")
        ),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Direct Offer to Owner ----
    observeEvent(input$submit_owner_offer, {
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp, login, champ_id, team_id)

      offer_amount <- input$owner_offer_amount
      if (is.null(offer_amount) || offer_amount <= 0) {
        shiny::showNotification("Please enter a valid offer amount.", type = "warning")
        return()
      }

      player_id <- sp$id
      player_slug <- if ("slug" %in% colnames(sp) && !is.na(sp$slug)) sp$slug else sp$name

      res <- buy_clause(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id,
        player_slug = player_slug,
        price = offer_amount,
        isClause = FALSE
      )

      is_success <- if (is.list(res)) isTRUE(res$success) else isTRUE(res)

      removeModal()

      if (is_success) {
        tryCatch({
          log_market_transaction(
            player_id = player_id,
            championship_id = champ_id,
            buyer_team_id = team_id,
            seller_team_id = if ("user_team_id" %in% colnames(sp)) sp$user_team_id else NULL,
            price = offer_amount,
            is_clause = FALSE
          )
        }, error = function(e) {
          print(paste0("[Supabase] Direct offer log warning: ", e$message))
        })
        shiny::showNotification(
          paste0("Direct offer of ", format_currency(offer_amount), " submitted successfully for ", sp$name, "!"),
          type = "message",
          duration = 5
        )
        clear_api_cache()
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(player_id = player_id, new_bid_price = offer_amount, is_cancel = FALSE), error = function(e) NULL)
        }
      } else {
        err_msg <- if (is.list(res) && !is.null(res$message) && res$message != "") res$message else "Please verify your funds and try again."
        shiny::showNotification(
          paste0("Offer failed: ", err_msg),
          type = "error",
          duration = 6
        )
      }
    })

    # ---- Option 3: Release Clause Buyout Modal ----
    observeEvent(input$btn_pay_clause, {
      sp <- selected_player()
      req(sp)
      clause_price <- sp$clause_price

      showModal(modalDialog(
        title = tagList(icon("bolt"), " Confirm Release Clause Buyout"),
        p(strong(sp$name)),
        p("This will instantly purchase the player for their official release clause."),
        p("Clause price: ", strong(format_currency(clause_price))),
        p(style = "color: #ef4444; font-size: 13px; font-weight: 600;", "Are you sure you want to trigger this clause buyout?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("submit_clause"), "Confirm Clause Buyout", class = "btn btn-buy-clause")
        ),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Clause Purchase ----
    observeEvent(input$submit_clause, {
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp, login, champ_id, team_id)

      player_id <- sp$id
      player_slug <- if ("slug" %in% colnames(sp) && !is.na(sp$slug)) sp$slug else sp$name

      res <- buy_clause(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id,
        player_slug = player_slug,
        price = clause_price,
        isClause = TRUE
      )

      is_success <- if (is.list(res)) isTRUE(res$success) else isTRUE(res)

      removeModal()

      if (is_success) {
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
          paste0("Release clause buyout of ", format_currency(clause_price), " executed successfully for ", sp$name, "!"),
          type = "message",
          duration = 5
        )
        clear_api_cache()
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(), error = function(e) NULL)
        }
      } else {
        err_msg <- if (is.list(res) && !is.null(res$message) && res$message != "") res$message else "Please verify your funds and try again."
        shiny::showNotification(
          paste0("Clause buyout failed: ", err_msg),
          type = "error",
          duration = 6
        )
      }
    })

    # ---- Put Single Player on Market Modal ----
    observeEvent(input$btn_put_on_market, {
      sp <- selected_player()
      req(sp)
      default_price <- if ("value" %in% colnames(sp) && !is.na(sp$value) && sp$value > 0) sp$value else 1000000

      showModal(modalDialog(
        title = tagList(icon("tags"), paste0(" List ", sp$name, " on Market")),
        p(strong(sp$name)),
        p("Current Market Valuation: ", strong(format_currency(default_price))),
        numericInput(
          ns("sale_price_input"),
          label = "Asking Listing Price (EUR):",
          value = default_price,
          min = 1,
          step = 10000
        ),
        p(style = "color: #64748b; font-size: 12px;", "This player will be listed on the transfer market for other users and computer to place bids."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("submit_put_on_market"), "Confirm Market Listing", class = "btn btn-offer-money")
        ),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Put Player on Market ----
    observeEvent(input$submit_put_on_market, {
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp, login, champ_id, team_id)

      sale_price <- input$sale_price_input
      if (is.null(sale_price) || sale_price <= 0) {
        shiny::showNotification("Please enter a valid listing price.", type = "warning")
        return()
      }

      player_id <- sp$id

      res <- put_player_on_market(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id,
        price = sale_price
      )

      is_success <- if (is.list(res)) isTRUE(res$success) else isTRUE(res)

      removeModal()

      if (is_success) {
        shiny::showNotification(
          paste0(sp$name, " listed on the transfer market for ", format_currency(sale_price), "!"),
          type = "message",
          duration = 5
        )
        clear_api_cache()
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(player_id = player_id, new_bid_price = sale_price, is_cancel = FALSE), error = function(e) NULL)
        }
      } else {
        err_msg <- if (is.list(res) && !is.null(res$message) && res$message != "") res$message else "Listing failed. Please try again."
        shiny::showNotification(
          paste0("Failed to list player: ", err_msg),
          type = "error",
          duration = 6
        )
      }
    })

    # ---- Remove Player from Market Modal ----
    observeEvent(input$btn_cancel_sell, {
      sp <- selected_player()
      req(sp)

      showModal(modalDialog(
        title = tagList(icon("tag"), paste0(" Remove ", sp$name, " from Market")),
        p(strong(sp$name)),
        p("Are you sure you want to withdraw this player from the transfer market?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("submit_cancel_sell"), "Confirm Remove from Market", class = "btn btn-cancel-bid")
        ),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Remove Player from Market ----
    observeEvent(input$submit_cancel_sell, {
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp, login, champ_id, team_id)

      player_id <- sp$id

      res <- cancel_player_sell(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id
      )

      is_success <- if (is.list(res)) isTRUE(res$success) else isTRUE(res)

      removeModal()

      if (is_success) {
        shiny::showNotification(
          paste0(sp$name, " removed from the transfer market!"),
          type = "message",
          duration = 5
        )
        clear_api_cache()
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(player_id = player_id, new_bid_price = NA_real_, is_cancel = TRUE), error = function(e) NULL)
        }
      } else {
        err_msg <- if (is.list(res) && !is.null(res$message) && res$message != "") res$message else "Withdrawal failed. Please try again."
        shiny::showNotification(
          paste0("Failed to remove player: ", err_msg),
          type = "error",
          duration = 6
        )
      }
    })

    ## render player_trend_plot (Plot A) ----
    output$player_trend_plot <- plotly::renderPlotly({
      sp <- selected_player()
      req(sp)

      champ_id <- get_reactive_val(championship_id)
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