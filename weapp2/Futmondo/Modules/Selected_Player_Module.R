library(reactable)

# Transparent 1x1 GIF to prevent broken-image alt-text rendering on pre-load
SPACER_GIF <- "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"

# Stable human-readable euro amounts for player-card banners.
player_card_money <- function(value) {
  amount <- suppressWarnings(as.numeric(value))
  if (length(amount) != 1L || !is.finite(amount)) return("Unavailable")
  paste0(format(round(amount), big.mark = ".", decimal.mark = ",",
    scientific = FALSE, trim = TRUE), " €")
}

player_card_owner_id <- function(player) {
  if (!is.data.frame(player) || !nrow(player)) return("")
  for (field in c("owner_team_id", "user_team_id", "userteamId", "userTeamId",
                  "userTeam._id", "userTeam.id", "userteam._id", "userteam.id")) {
    if (!field %in% names(player)) next
    value <- player[[field]][1]
    if (!is.atomic(value) || length(value) != 1L || is.na(value)) next
    value <- trimws(as.character(value))
    if (nzchar(value)) return(value)
  }
  ""
}

player_card_is_own_player <- function(player, current_team_id = NULL) {
  row <- if (is.data.frame(player) && nrow(player)) player[1,,drop=FALSE] else data.frame()
  owner <- player_card_owner_id(row)
  current <- if (is.null(current_team_id) || length(current_team_id)!=1L || is.na(current_team_id)) "" else trimws(as.character(current_team_id))
  !is.na(owner) && nzchar(owner) && nzchar(current) && identical(owner,current)
}

player_card_can_buy_clause <- function(player,current_team_id,clause_open) {
  isTRUE(clause_open) && !player_card_is_own_player(player,current_team_id)
}

# Compact ownership/market wording for the player-card header. A free agent is
# not assumed to be listed: market status requires an explicit flag or asking price.
player_card_location_status <- function(player, current_team_id = NULL) {
  row <- if (is.data.frame(player) && nrow(player)) player[1, , drop = FALSE] else data.frame()
  text_at <- function(name) {
    value <- if (name %in% names(row)) as.character(row[[name]][1]) else ""
    if (length(value) != 1L || is.na(value)) "" else trimws(value)
  }
  owner_id <- player_card_owner_id(row)
  current_id <- if (is.null(current_team_id) || length(current_team_id) != 1L || is.na(current_team_id)) "" else trimws(as.character(current_team_id))
  owner_name <- text_at("userTeam")
  if (!nzchar(owner_name)) owner_name <- text_at("user_team_name")
  if ("market_inMarket" %in% names(row) && !is.na(row$market_inMarket[1])) {
    on_market <- isTRUE(as.logical(row$market_inMarket[1]))
  } else {
    asking <- NA_real_
    for (field in c("effective_market_price", "market_price", "price")) {
      amount <- if (field %in% names(row)) suppressWarnings(as.numeric(row[[field]][1])) else NA_real_
      if (length(amount) == 1L && is.finite(amount) && amount > 0) { asking <- amount; break }
    }
    on_market <- is.finite(asking)
  }
  ownership <- if (!nzchar(owner_id)) "Free Agent" else if (nzchar(current_id) && identical(owner_id, current_id)) {
    "Your Squad"
  } else paste0("Rival Owned", if (nzchar(owner_name)) paste0(": ", owner_name) else "")
  list(ownership=ownership,on_market=on_market,
    market=if (on_market) "Listed on Market" else "Not listed on Market")
}

player_card_location_label <- function(player, current_team_id = NULL) {
  status <- player_card_location_status(player,current_team_id)
  paste0("Ownership: ",status$ownership," · Market: ",status$market)
}

selected_player_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # The modal footer is below a long card. Keep a large, keyboard-accessible
    # dismissal control at the top of the scrollable player-card content.
    div(
      class = "player-card-close-shortcut",
      tags$button(type = "button", class = "btn btn-default",
        `data-dismiss` = "modal", `aria-label` = "Close player card",
        icon("times"), span("Close player card"))
    ),
    userBox(
      id = ns("selected_player_box"),
      width = 12,
      title = userDescription(title = "Player", subtitle = "", type = 1, image = SPACER_GIF),
      status = "primary",
      gradient = TRUE,
      background = "light-blue",
      boxToolSize = "xl",
      collapsible = FALSE,
      footer = tagList(
        uiOutput(ns("player_identity_summary")),
        fluidRow(
          column(4, uiOutput(ns("player_points_description_box"))),
          column(4, uiOutput(ns("player_last_points_description_box"))),
          column(4, uiOutput(ns("player_value_description_box")))
        ),
        fluidRow(
          column(12, uiOutput(ns("recent_round_points")))
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
                 div(class = "player-chart-heading",
                   h4(style = "font-weight: 600; color: var(--fm-text); margin: 0;", "Historical Valuation & Performance"),
                   radioButtons(ns("player_chart_metric"), label = NULL,
                     choices = c("Valuation" = "valuation", "Round points" = "points"),
                     selected = "valuation", inline = TRUE)
                 ),
                 plotly::plotlyOutput(ns("player_trend_plot"), height = "280px")
          )
        ),
        # Real-competition profile, price history, and completed match metadata.
        fluidRow(
          style = "margin-top: 20px; padding-top: 15px; border-top: 1px solid #f1f5f9;",
          column(12, uiOutput(ns("player_external_details")))
        ),
        # FIS 5-Pillar Breakdown Panel
        fluidRow(
          style = "margin-top: 20px; padding-top: 15px; border-top: 1px solid #f1f5f9;",
          column(12, uiOutput(ns("fis_panel")))
        ),
        # Smart Bid & Auction Intelligence Widget
        fluidRow(
          style = "margin-top: 20px; padding-top: 15px; border-top: 1px solid #f1f5f9;",
          column(12, uiOutput(ns("smart_bid_widget")))
        )
      )
    )
  )
}


# ---- Pure helper: market-offer open decision ----
# Single decision point shared by the "Make Market Offer" button and the
# external "market_bid" action event (e.g. Today's "Place Bid" recommendation).
# Given a selected player row and a preflight result, decide whether the
# regular market-offer modal should open.
#   - no valid player            -> open = FALSE, reason = "no_player"
#   - preflight not ok           -> open = FALSE, reason = <preflight reason>
#   - valid player + preflight ok-> open = TRUE,  reason = "ok"
# Returns list(open = TRUE/FALSE, reason = "ok" | "no_player" | <preflight reason>).
market_offer_decision <- function(sp, preflight) {
  if (is.null(sp) || is.null(sp$id) || !nzchar(as.character(sp$id))) {
    return(list(open = FALSE, reason = "no_player"))
  }
  if (is.null(preflight) || !isTRUE(preflight$ok)) {
    reason <- if (!is.null(preflight) && !is.null(preflight$reason)) preflight$reason else "unavailable"
    return(list(open = FALSE, reason = reason))
  }
  return(list(open = TRUE, reason = "ok"))
}


selected_player_Server <- function(id, selected_player, login_token = NULL, championship_id = NULL, user_team_id = NULL, on_bid_updated = NULL, open_action = NULL, capacity_fetcher = NULL) {
  # Keep the caller's reactive lazy: parent tables define it after this module.
  player_source <- function() selected_player()
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    raw_selected_player <- player_source
    selected_player <- reactive({
      row <- raw_selected_player()
      if (is.data.frame(row) && nrow(row) == 1L) {
        row$user_team_id <- player_card_owner_id(row)
      }
      row
    })

    active_bid_info_RV <- reactiveVal(NULL)

    # Store the latest smart bid result for the "Use Smart Bid" button
    smart_bid_cache_RV <- reactiveVal(NULL)

    # Records whether the regular market-offer modal was opened by the shared
    # helper (TRUE) or blocked by preflight / missing player (FALSE). Exposed
    # so the open_action routing can be asserted in tests without a live UI.
    offer_modal_opened_RV <- reactiveVal(FALSE)

    # Records whether the release-clause buyout confirmation modal was opened
    # by the shared helper (TRUE) or blocked by the open-state recheck /
    # preflight / missing player (FALSE). Exposed so the "clause_buyout"
    # open_action routing can be asserted in tests without a live UI.
    clause_modal_opened_RV <- reactiveVal(FALSE)
    # Test seam for direct and external active-bid update routing.
    modify_modal_opened_RV <- reactiveVal(FALSE)


    # ---- Safe reactive value extractor ----
    get_reactive_val <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.reactive(x) || is.function(x)) {
        tryCatch(x(), error = function(e) NULL)
      } else {
        x
      }
    }

    # A confirmation belongs to the exact account, league, team and player
    # that opened it. Switching context invalidates even an already queued click.
    modal_context_RV <- reactiveVal(list())
    player_action_context_RV <- reactive({
      auth <- get_reactive_val(login_token)
      sp <- tryCatch(selected_player(),error=function(e)NULL)
      if(!valid_login(auth) || is.null(sp) || !nzchar(fm_scalar(sp$id,""))) return(NULL)
      list(user=fm_scalar(auth[["userid"]]),championship=fm_scalar(get_reactive_val(championship_id)),
        team=fm_scalar(get_reactive_val(user_team_id)),player=fm_scalar(sp$id))
    })
    observeEvent(player_action_context_RV(), {
      active_bid_info_RV(NULL);smart_bid_cache_RV(NULL);modal_context_RV(list())
      offer_modal_opened_RV(FALSE);clause_modal_opened_RV(FALSE);modify_modal_opened_RV(FALSE)
      removeModal()
    },ignoreNULL=FALSE,priority=110)
    remember_action_context <- function(action) {
      context <- player_action_context_RV();req(!is.null(context))
      values <- modal_context_RV();values[[action]]<-context;modal_context_RV(values)
      invisible(TRUE)
    }
    consume_action_context <- function(action) {
      context <- player_action_context_RV();values<-modal_context_RV()
      if(is.null(context) || !identical(values[[action]],context)) return(FALSE)
      values[[action]]<-NULL;modal_context_RV(values)
      TRUE
    }

    # ---- Acquisition capacity preflight (shared by all acquisition paths) ----
    # Single internal preflight used by market bid, direct-owner offer, clause
    # buyout, and bid modification. It verifies roster capacity, verified
    # spendable funds, and (for modify) the existing own bid, then fails closed
    # when verification is unavailable/ambiguous.
    #
    # mode: "bid" | "offer" | "clause" | "modify"
    # amount: numeric amount to spend (NULL at modal-open time)
    # existing_bid_amount: the user's current bid on the target (for "modify")
    #
    # Returns list(ok, reason = "ok"|"unavailable"|"capacity"|"funds"|"minimum_bid", message).
    run_acquisition_preflight <- function(sp, mode, amount = NULL, existing_bid_amount = NULL) {
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)

      if (is.null(sp) || is.null(sp$id) || is.null(login) || is.null(champ_id) || is.null(team_id)) {
        return(list(ok = FALSE, reason = "unavailable",
                    message = "Acquisition verification unavailable: player, login, or championship context missing."))
      }

      # capacity_fetcher is an optional test seam: when a function is supplied
      # it is used in place of the network get_acquisition_capacity call, so
      # preflight behavior can be exercised deterministically (no network write).
      fetch_capacity <- if (!is.null(capacity_fetcher) && is.function(capacity_fetcher)) capacity_fetcher else get_acquisition_capacity
      capacity <- tryCatch(
        fetch_capacity(
          login = login,
          championship_id = champ_id,
          user_team_id = team_id,
          target_player_id = as.character(sp$id)
        ),
        error = function(e) NULL
      )

      evaluate_acquisition_preflight(
        capacity = capacity,
        mode = mode,
        amount = amount,
        existing_bid_amount = existing_bid_amount,
        minimum_bid = if (mode %in% c("bid", "modify")) market_bid_minimum(sp) else NULL
      )
    }

    # Show a preflight failure notification, distinguishing unavailable
    # verification from capacity/funds rejections.
    show_preflight_failure <- function(res) {
      msg <- if (!is.null(res$message) && nzchar(as.character(res$message))) as.character(res$message) else "Acquisition blocked."
      shiny::showNotification(msg, type = "error", duration = 6)
    }

    # ---- Live price preview helper ----
    render_input_price_preview <- function(val) {
      if (is.null(val) || is.na(val) || !is.numeric(val) || val <= 0) {
        div(
          style = "margin-top: 6px; color: var(--fm-danger); font-size: 12px; font-weight: 600; display: flex; align-items: center; gap: 4px;",
          shiny::tags$i(class = "fa-solid fa-circle-exclamation"),
          "Please enter a valid numerical price greater than 0 €."
        )
      } else {
        div(
          style = "margin-top: 6px; color: var(--fm-text); font-size: 14px; font-weight: 700;",
          player_card_money(val)
        )
      }
    }

    output$new_bid_amount_preview <- renderUI({ render_input_price_preview(input$new_bid_amount) })
    output$bid_amount_preview <- renderUI({ render_input_price_preview(input$bid_amount) })
    output$owner_offer_amount_preview <- renderUI({ render_input_price_preview(input$owner_offer_amount) })
    output$sale_price_input_preview <- renderUI({ render_input_price_preview(input$sale_price_input) })

    output$player_identity_summary <- renderUI({
      sp <- selected_player()
      req(sp)
      player_name <- if ("name" %in% names(sp) && !is.na(sp$name)) as.character(sp$name) else "Player"
      role <- if ("role" %in% names(sp) && !is.na(sp$role)) as.character(sp$role) else "Position unavailable"
      team <- if ("team" %in% names(sp) && !is.na(sp$team) && nzchar(as.character(sp$team))) as.character(sp$team) else "Team unavailable"
      photo <- if ("photo" %in% names(sp) && !is.na(sp$photo) && nzchar(as.character(sp$photo))) paste0(PHOTO_URL, "/", sp$photo) else SPACER_GIF
      team_logo_field <- if ("logo" %in% names(sp) && !is.na(sp$logo) && nzchar(as.character(sp$logo))) as.character(sp$logo) else NULL
      team_image_name <- if (identical(team, "Team unavailable")) "" else get_team_image_name(team, logo = team_logo_field)
      team_logo <- if (nzchar(team_image_name)) {
        img(src = paste0(TEAM_LOGO_URL, team_image_name, ".png"),
          class = "player-card-identity-team-logo", alt = paste0(team, " logo"),
          onerror = "this.style.display=\"none\";")
      } else NULL
      location <- player_card_location_label(sp, get_reactive_val(user_team_id))
      div(class = "player-card-identity",
        img(src = photo, class = "player-card-identity-photo", alt = player_name,
          onerror = paste0("this.src='", SPACER_GIF, "';")),
        div(class = "player-card-identity-details",
          div(class = "player-card-identity-name", player_name),
          div(class = "player-card-identity-meta",
            team_logo,
            span(paste(role, "·", team))),
          div(class = "player-card-identity-location", icon("location-dot"), " ", location)
        )
      )
    })

    # ---- Main observer: populate box + action buttons ----
    observeEvent(
      {
        selected_player()
      },
      {
        sp <- selected_player()
        req(sp)
        smart_bid_cache_RV(NULL)

        login <- get_reactive_val(login_token)
        champ_id <- get_reactive_val(championship_id)
        team_id <- get_reactive_val(user_team_id)

        my_bid_id <- NULL
        my_bid_price <- NULL

        if (!is.null(login) && !is.null(champ_id) && !is.null(team_id) && !is.null(sp$id)) {
          summary_res <- tryCatch({
            get_player_summary(login = login, championship_id = champ_id, user_team_id = team_id, player_id = sp$id)
          }, error = function(e) NULL)

          # Only trust summary-level my_bid_id/my_bid_price: get_player_summary
          # now returns them ONLY when a bid's immutable team ID matches our
          # team (NULL otherwise). No sp$bid_price fallback -- that field is the
          # market's current bid and may belong to a rival, so it must never be
          # treated as our own active bid (the modify flow would then target a
          # rival's bid).
          if (!is.null(summary_res)) {
            my_bid_id <- summary_res$my_bid_id
            my_bid_price <- summary_res$my_bid_price
          }
        }

        # Expose an active bid only when we have a verified own-bid ID + price.
        if (!is.null(my_bid_id) && nzchar(as.character(my_bid_id)) &&
            !is.null(my_bid_price) && is.finite(my_bid_price) && my_bid_price > 0) {
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
          team_logo_field <- if ("logo" %in% colnames(sp) && !is.na(sp$logo)) as.character(sp$logo) else NULL
          team_image_name <- get_team_image_name(sp$team, logo = team_logo_field)
          
          logo_tag <- if (team_image_name != "") {
            img(src = paste0(TEAM_LOGO_URL, team_image_name, ".png"), 
                style = "height: 18px; width: auto; object-fit: contain; background: transparent;", 
                alt = sp$team,
                onerror = "this.style.display='none';")
          } else {
            NULL
          }
          
          team_logo <- shiny::tags$div(
            style = "margin-top: 6px; display: flex; align-items: center; gap: 8px; font-weight: 500; font-size: 13px; color: var(--fm-text);",
            logo_tag,
            sp$team
          )
        }

        sub_title_markup <- tagList(
          shiny::tags$span(style = "display: block; font-weight: 600; font-size: 13px; color: var(--fm-muted); text-transform: uppercase; letter-spacing: 0.5px;", role_text),
          team_logo
        )

        shinydashboardPlus::updateBox(
          id = "selected_player_box",
          action = "update",
          options = list(
            title = shinydashboardPlus::userDescription(
              title = player_name, subtitle = sub_title_markup, type = 1,
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
        is_own_player <- player_card_is_own_player(sp,current_user_team)
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
            style = "display: inline-block; padding: 8px 16px; background-color: var(--fm-surface); color: var(--fm-text); border: 1px solid #bae6fd; border-radius: 8px; font-weight: 600; font-size: 12px; margin: 5px;",
            tagList(icon("shield-halved"), " Player in Your Squad")
          )

          if (is_listed_on_market) {
            price_text <- if (!is.na(current_asking_price) && current_asking_price > 0) paste0(" (Asking: ", player_card_money(current_asking_price), ")") else ""
            listed_badge <- div(
              style = "display: inline-block; padding: 8px 16px; background-color: var(--fm-surface); color: var(--fm-text); border: 1px solid #fde68a; border-radius: 8px; font-weight: 600; font-size: 12px; margin: 5px;",
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

          action_buttons <- tagList(action_buttons, actionButton(ns("btn_direct_sell"),
            label=tagList(icon("money-bill-transfer")," Sell directly to Futmondo"),
            class="btn btn-danger", disabled=if(is_listed_on_market) "disabled" else NULL,
            title=if(is_listed_on_market) "Remove the existing listing first." else "Sell to Futmondo without waiting for an offer."))

          # Check for received offer on squad player
          has_received_offer <- FALSE
          rec_offer_price <- NA_real_
          rec_offer_bidder <- "Futmondo"

          if ("bid_price" %in% colnames(sp) && !is.na(sp$bid_price) && suppressWarnings(as.numeric(sp$bid_price)) > 0) {
            has_received_offer <- TRUE
            rec_offer_price <- suppressWarnings(as.numeric(sp$bid_price))
            if ("bid_user" %in% colnames(sp) && !is.na(sp$bid_user) && sp$bid_user != "") {
              rec_offer_bidder <- as.character(sp$bid_user)
            }
          }

          if (has_received_offer) {
            offer_banner <- div(
              style = "width: 100%; text-align: center; margin-bottom: 10px; padding: 10px 16px; background-color: var(--fm-surface); color: var(--fm-text); border: 1px solid #a7f3d0; border-radius: 8px; font-weight: 700; font-size: 14px;",
              tagList(icon("hand-holding-dollar"), paste0(" Received Offer: ", player_card_money(rec_offer_price), " from ", rec_offer_bidder))
            )
            btn_accept_offer <- actionButton(
              ns("btn_accept_offer"),
              label = tagList(icon("circle-check"), " Accept Offer"),
              class = "btn btn-offer-money"
            )
            btn_reject_offer <- actionButton(
              ns("btn_reject_offer"),
              label = tagList(icon("circle-xmark"), " Reject Offer"),
              class = "btn btn-cancel-bid"
            )
            action_buttons <- tagList(action_buttons, offer_banner, btn_accept_offer, btn_reject_offer)
          }
        } else if (has_active_bid) {
          # ---- Player belongs to rival/market AND current user has an active BUY bid ----
          bid_info <- active_bid_info_RV()
          banner <- div(
            style = "width: 100%; text-align: center; margin-bottom: 10px; padding: 10px 16px; background-color: var(--fm-surface); color: var(--fm-text); border: 1px solid #a7f3d0; border-radius: 8px; font-weight: 700; font-size: 14px;",
            tagList(icon("hand-holding-dollar"), paste0(" Your Active Bid: ", player_card_money(bid_info$price)))
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

          # Determine owner name
          owner_name <- if ("userTeam" %in% colnames(sp) && !is.na(sp$userTeam) && nzchar(trimws(as.character(sp$userTeam)))) {
            trimws(as.character(sp$userTeam))
          } else if ("user" %in% colnames(sp) && !is.na(sp$user) && nzchar(trimws(as.character(sp$user)))) {
            trimws(as.character(sp$user))
          } else if ("teamname" %in% colnames(sp) && !is.na(sp$teamname) && nzchar(trimws(as.character(sp$teamname)))) {
            trimws(as.character(sp$teamname))
          } else if ("owner_teamname" %in% colnames(sp) && !is.na(sp$owner_teamname) && nzchar(trimws(as.character(sp$owner_teamname)))) {
            trimws(as.character(sp$owner_teamname))
          } else {
            NULL
          }

          location_status <- player_card_location_status(sp,current_user_team)
          ownership_badge <- div(
            style = "display: inline-block; padding: 8px 16px; background-color: var(--fm-surface); color: var(--fm-text); border: 1px solid #cbd5e1; border-radius: 8px; font-weight: 600; font-size: 13px; margin: 5px;",
            tagList(icon("users"),paste0(" Ownership: ",location_status$ownership)))
          market_badge <- div(
            style = "display: inline-block; padding: 8px 16px; background-color: var(--fm-surface); color: var(--fm-text); border: 1px solid #fde68a; border-radius: 8px; font-weight: 600; font-size: 13px; margin: 5px;",
            tagList(icon("tags"),paste0(" Market: ",location_status$market)))
          action_buttons <- tagList(ownership_badge,market_badge,action_buttons)

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
          bid_gate <- run_acquisition_preflight(sp,"bid")
          clause_gate <- run_acquisition_preflight(sp,"clause")
          blocked_reason <- if(!isTRUE(bid_gate$ok)) bid_gate$message else if(!isTRUE(clause_gate$ok)) clause_gate$message else NULL
          if(!is.null(blocked_reason)) action_buttons <- tagList(action_buttons,
            div(class="acquisition-capacity-warning",icon("triangle-exclamation")," ",blocked_reason))

          if (!is.na(eff_market_price) && eff_market_price > 0) {
            action_buttons <- tagList(
              action_buttons,
              actionButton(
                ns("btn_bid_market"),
                label = tagList(icon("hand-holding-dollar"), " Make Market Offer"),
                class = "btn btn-buy-market", disabled=if(!isTRUE(bid_gate$ok)) "disabled" else NULL,
                title=if(!isTRUE(bid_gate$ok)) bid_gate$message else NULL
              )
            )
          }

          # Option 2: "Offer to Owner" button when player is owned by a rival user team
          if (!is.null(player_owner_team) && !is.na(player_owner_team) && player_owner_team != "" && (is.na(eff_market_price) || eff_market_price <= 0)) {
            owner_name <- if ("userTeam" %in% colnames(sp) && !is.na(sp$userTeam)) sp$userTeam else if ("teamname" %in% colnames(sp) && !is.na(sp$teamname)) sp$teamname else "Owner"
            action_buttons <- tagList(
              action_buttons,
              actionButton(
                ns("btn_offer_owner"),
                label = tagList(icon("hand-holding-dollar"), paste0(" Offer to ", owner_name)),
                class = "btn btn-offer-money", disabled=if(!isTRUE(bid_gate$ok)) "disabled" else NULL,
                title=if(!isTRUE(bid_gate$ok)) bid_gate$message else NULL
              )
            )
          }

          # Option 3: "Buy Release Clause" button when clause is OPEN
          if (player_card_can_buy_clause(sp,current_user_team,is_clause_open)) {
            clause_label <- paste0(" Buy Clause: ", player_card_money(clause_price_val))
            action_buttons <- tagList(
              action_buttons,
              actionButton(
                ns("btn_pay_clause"),
                label = tagList(icon("bolt"), clause_label),
                class = "btn btn-buy-clause", disabled=if(!isTRUE(clause_gate$ok)) "disabled" else NULL,
                title=if(!isTRUE(clause_gate$ok)) clause_gate$message else NULL
              )
            )
          } else if (!is_own_player && clause_price_val > 0) {
            # Release clause exists but is currently LOCKED
            lock_reason <- if (clause_date_formatted != "") paste0("until ", clause_date_formatted) else "transferred/cooldown"
            locked_badge <- div(
              style = "display: inline-block; padding: 8px 16px; background-color: var(--fm-surface); color: var(--fm-text); border: 1px solid #fde68a; border-radius: 8px; font-weight: 600; font-size: 12px; margin: 5px;",
              tagList(icon("lock"), paste0(" Release Clause Locked ", lock_reason, " (", player_card_money(clause_price_val), ")"))
            )
            action_buttons <- tagList(action_buttons, locked_badge)
          }
        }

        output$action_buttons <- renderUI(action_buttons)
      }
    )

    # ---- Shared helper: verify and open the active-bid update modal ----
    open_modify_bid_modal <- function() {
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp, login, champ_id, team_id)

      # Reverify immutable own-team bid evidence immediately before opening.
      summary_res <- tryCatch(
        get_player_summary(login = login, championship_id = champ_id,
                           user_team_id = team_id, player_id = sp$id),
        error = function(e) NULL
      )
      bid_id <- if (!is.null(summary_res)) summary_res$my_bid_id else NULL
      bid_price <- if (!is.null(summary_res)) suppressWarnings(as.numeric(summary_res$my_bid_price)) else NA_real_
      if (is.null(bid_id) || !nzchar(as.character(bid_id)) ||
          length(bid_price) != 1L || !is.finite(bid_price) || bid_price <= 0) {
        active_bid_info_RV(NULL)
        modify_modal_opened_RV(FALSE)
        shiny::showNotification(
          "Your active bid could not be verified and may no longer exist. Please refresh and try again.",
          type = "warning", duration = 5
        )
        return(invisible(FALSE))
      }
      active_bid_info_RV(list(id = as.character(bid_id), price = bid_price))
      bid_info <- active_bid_info_RV()

      pf <- run_acquisition_preflight(sp, "modify", amount = NULL,
                                      existing_bid_amount = bid_info$price)
      if (!isTRUE(pf$ok)) {
        show_preflight_failure(pf)
        modify_modal_opened_RV(FALSE)
        return(invisible(FALSE))
      }

      remember_action_context("modify")
      modify_modal_opened_RV(TRUE)
      showModal(modalDialog(
        title = tagList(icon("pen-to-square"), " Update Your Active Bid"),
        p(strong(sp$name)),
        p("Current Active Bid: ", strong(player_card_money(bid_info$price))),
        p("Minimum market bid: ", strong(player_card_money(market_bid_minimum(sp)))),
        numericInput(
          ns("new_bid_amount"),
          label = "New Bid Amount (EUR):",
          value = max(bid_info$price, market_bid_minimum(sp)),
          min = market_bid_minimum(sp),
          step = 10000
        ),
        uiOutput(ns("new_bid_amount_preview")),
        footer = div(style = "text-align: center; width: 100%; display: flex; justify-content: center; gap: 10px;",
                     modalButton("Cancel"),
                     actionButton(ns("submit_modify_bid"), "Submit Updated Bid", class = "btn btn-buy-market")),
        easyClose = TRUE,
        size = "s"
      ))
      invisible(TRUE)
    }

    observeEvent(input$btn_modify_bid, {
      open_modify_bid_modal()
    })

    # ---- Submit Modify Active Bid ----
    observeEvent(input$submit_modify_bid, {
      req(consume_action_context("modify"))
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      bid_info <- active_bid_info_RV()
      req(sp, login, champ_id, team_id, bid_info)

      new_price <- input$new_bid_amount
      if (is.null(new_price) || is.na(new_price) || !is.numeric(new_price) || new_price <= 0) {
        shiny::showNotification("Please enter a valid numerical price greater than 0 €.", type = "error")
        return()
      }

      # Preflight before write: verify capacity + funds (delta vs existing bid).
      pf <- run_acquisition_preflight(sp, "modify", amount = new_price, existing_bid_amount = bid_info$price)
      if (!isTRUE(pf$ok)) {
        show_preflight_failure(pf)
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
          paste0("Active bid updated to ", player_card_money(new_price), " for ", sp$name, "!"),
          type = "message",
          duration = 5
        )
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(action_type = "bid_modified", player_id = player_id, new_bid_price = new_price, is_cancel = FALSE), error = function(e) NULL)
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
      remember_action_context("cancel")
      sp <- selected_player()
      req(sp)
      bid_info <- active_bid_info_RV()
      req(bid_info)

      showModal(modalDialog(
        title = tagList(icon("trash-can"), " Cancel Active Bid"),
        p(strong(sp$name)),
        p("Are you sure you want to cancel your active bid of ", strong(player_card_money(bid_info$price)), "?"),
        p(style = "color: var(--fm-danger); font-size: 13px;", "This will withdraw your offer from the transfer market."),
        footer = div(style = "text-align: center; width: 100%; display: flex; justify-content: center; gap: 10px;",
                     modalButton("Keep Bid"),
                     actionButton(ns("submit_cancel_bid"), "Confirm Cancel Bid", class = "btn btn-cancel-bid")),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Cancel Active Bid ----
    observeEvent(input$submit_cancel_bid, {
      req(consume_action_context("cancel"))
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      bid_info <- active_bid_info_RV()
      req(sp, login, champ_id, team_id, bid_info)

      player_id <- sp$id
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
          tryCatch(on_bid_updated(action_type = "bid_cancelled", player_id = player_id, new_bid_price = NA_real_, is_cancel = TRUE), error = function(e) NULL)
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

    # ---- Use Smart Bid: pre-fill the market offer modal ----
    observeEvent(input$btn_use_smart_bid, {
      remember_action_context("bid")
      cached <- smart_bid_cache_RV()
      if (is.null(cached) || !isTRUE(cached$can_compete) || !isTRUE(cached$funds_verified) || length(cached$recommended_bid) != 1L || !is.finite(cached$recommended_bid) || cached$recommended_bid <= 0) {
        shiny::showNotification("Smart bid data not available. Please refresh.", type = "error")
        return()
      }

      sp <- selected_player()
      req(sp)

      # Recheck the cached recommendation against current minimum and funds.
      pf <- run_acquisition_preflight(sp, "bid", amount = cached$recommended_bid)
      if (!isTRUE(pf$ok)) {
        show_preflight_failure(pf)
        return()
      }

      recommended_val <- cached$recommended_bid
      market_price <- market_bid_minimum(sp)

      showModal(modalDialog(
        title = tagList(icon("chart-line"), " Place Market Offer (Smart Bid)"),
        p(strong(sp$name)),
        p("Minimum market bid: ", strong(player_card_money(market_price))),
        p("Recommended Smart Bid: ", strong(player_card_money(recommended_val))),
        numericInput(
          ns("bid_amount"),
          label = "Your offer amount (EUR):",
          value = recommended_val,
          min = market_price,
          step = 10000
        ),
        uiOutput(ns("bid_amount_preview")),
        footer = div(style = "text-align: center; width: 100%; display: flex; justify-content: center; gap: 10px;",
                      modalButton("Cancel"),
                      actionButton(ns("submit_bid"), "Submit Market Offer", class = "btn btn-buy-market")),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Shared helper: open the regular market-offer modal ----
    # Single code path for the "Make Market Offer" button and for external
    # "market_bid" action events (e.g. Today's "Place Bid" recommendation).
    # Honors the same run_acquisition_preflight behavior as the button path.
    open_market_offer_modal <- function() {
      sp <- selected_player()
      req(sp)

      # Preflight: a new offer must have roster capacity available. The open
      # decision is routed through the pure market_offer_decision helper (the
      # single decision point shared with the external "market_bid" event).
      pf <- run_acquisition_preflight(sp, "bid", amount = NULL)
      decision <- market_offer_decision(sp, pf)
      if (!isTRUE(decision$open)) {
        if (!identical(decision$reason, "no_player")) {
          show_preflight_failure(pf)
        }
        offer_modal_opened_RV(FALSE)
        return(invisible(FALSE))
      }

      market_price <- market_bid_minimum(sp)

      remember_action_context("bid")
      offer_modal_opened_RV(TRUE)
      showModal(modalDialog(
        title = tagList(icon("hand-holding-dollar"), " Place Market Offer"),
        p(strong(sp$name)),
        p("Minimum market bid: ", strong(player_card_money(market_price))),
        numericInput(
          ns("bid_amount"),
          label = "Your offer amount (EUR):",
          value = market_price,
          min = market_price,
          step = 10000
        ),
        uiOutput(ns("bid_amount_preview")),
        footer = div(style = "text-align: center; width: 100%; display: flex; justify-content: center; gap: 10px;",
                      modalButton("Cancel"),
                      actionButton(ns("submit_bid"), "Submit Market Offer", class = "btn btn-buy-market")),
        easyClose = TRUE,
        size = "s"
      ))
      invisible(TRUE)
    }

    # ---- Option 1: Market Offer Modal ----
    observeEvent(input$btn_bid_market, {
      open_market_offer_modal()
    })

    # ---- Shared received-offer confirmation modal ----
    open_accept_offer_modal <- function() {
      sp <- selected_player()
      req(sp)
      rec_offer_price <- if ("bid_price" %in% names(sp)) suppressWarnings(as.numeric(sp$bid_price)) else NA_real_
      if (length(rec_offer_price) != 1L || !is.finite(rec_offer_price) || rec_offer_price <= 0) {
        shiny::showNotification("This received offer is no longer available. Please refresh and try again.", type = "warning")
        return(invisible(FALSE))
      }
      rec_offer_bidder <- if ("bid_user" %in% colnames(sp) && !is.na(sp$bid_user) && sp$bid_user != "") as.character(sp$bid_user) else "Futmondo"
      remember_action_context("accept")
      showModal(modalDialog(
        title = tagList(icon("circle-check"), paste0(" Accept Offer for ", sp$name)),
        p(strong(sp$name)),
        p("Are you sure you want to ACCEPT the received offer of ", strong(player_card_money(rec_offer_price)), " from ", strong(rec_offer_bidder), "?"),
        p(style = "color: var(--fm-text); font-size: 13px; font-weight: 600;", "The player will be sold and funds added to your budget immediately."),
        footer = div(style = "text-align: center; width: 100%; display: flex; justify-content: center; gap: 10px;",
                     modalButton("Cancel"),
                     actionButton(ns("submit_accept_offer"), "Confirm Accept Offer", class = "btn btn-offer-money")),
        easyClose = TRUE,
        size = "s"
      ))
      invisible(TRUE)
    }

    # ---- Open acquisition modals from an external stable action event ----
    #   - "modify_bid"  (Today's "Update Bid" recommendation) -> re-verifies
    #     immutable own-bid evidence and opens the existing update modal.
    # `open_action` is an optional reactive returning a stable action code:
    #   - "market_bid"   (Today's "Place Bid" recommendation) -> the SAME
    #     market-offer helper as the button path (identical preflight).
    #   - "clause_buyout" (Today's "Exercise Clause" recommendation) -> the
    #     SAME clause-buyout confirmation helper as the button path. It
    #     rechecks the strict open-clause state, and only the clause price is
    #     ever shown/executed (never a comparison price). It NEVER opens the
    #     market bid modal.
    # Guarded against stale startup reactive values: only fires when the
    # action is an executable stable code and a selected player is valid.
    if (!is.null(open_action) && is.reactive(open_action)) {
      observeEvent(open_action(), {
        act <- open_action()
        if (!act %in% c("market_bid", "modify_bid", "clause_buyout", "accept_offer")) return()
        sp <- selected_player()
        req(sp)
        if (identical(act, "modify_bid")) {
          open_modify_bid_modal()
        } else if (identical(act, "market_bid")) {
          open_market_offer_modal()
        } else if (identical(act, "clause_buyout")) {
          open_clause_buyout_modal(sp)
        } else {
          open_accept_offer_modal()
        }
      }, ignoreNULL = TRUE)
    }

    # ---- Submit Market Offer ----
    observeEvent(input$submit_bid, {
      req(consume_action_context("bid"))
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp, login, champ_id, team_id)

      bid_amount <- input$bid_amount
      if (is.null(bid_amount) || is.na(bid_amount) || !is.numeric(bid_amount) || bid_amount <= 0) {
        shiny::showNotification("Please enter a valid numerical price greater than 0 €.", type = "error")
        return()
      }

      # Preflight before write: verify capacity + verified spendable funds.
      pf <- run_acquisition_preflight(sp, "bid", amount = bid_amount)
      if (!isTRUE(pf$ok)) {
        show_preflight_failure(pf)
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
          paste0("Market offer of ", player_card_money(bid_amount), " submitted successfully for ", sp$name, "!"),
          type = "message",
          duration = 5
        )
        clear_api_cache()
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(action_type = "bid_placed", player_id = player_id, new_bid_price = bid_amount, is_cancel = FALSE), error = function(e) NULL)
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
      remember_action_context("owner_offer")
      sp <- selected_player()
      req(sp)

      # Preflight: a new direct offer must have roster capacity available.
      pf <- run_acquisition_preflight(sp, "offer", amount = NULL)
      if (!isTRUE(pf$ok)) {
        show_preflight_failure(pf)
        return()
      }

      default_offer <- if ("value" %in% colnames(sp) && !is.na(sp$value) && sp$value > 0) sp$value else 1000000
      owner_name <- if ("userTeam" %in% colnames(sp) && !is.na(sp$userTeam)) sp$userTeam else if ("teamname" %in% colnames(sp) && !is.na(sp$teamname)) sp$teamname else "Owner"
      clause_info <- if ("clause_price" %in% colnames(sp) && !is.na(sp$clause_price) && sp$clause_price > 0) paste0(" (Release Clause: ", player_card_money(sp$clause_price), ")") else ""

      showModal(modalDialog(
        title = tagList(icon("hand-holding-dollar"), paste0(" Offer Money to ", owner_name)),
        p(strong(sp$name), clause_info),
        p("Current Market Valuation: ", strong(player_card_money(default_offer))),
        numericInput(
          ns("owner_offer_amount"),
          label = "Your purchase offer amount (EUR):",
          value = default_offer,
          min = 1,
          step = 10000
        ),
        uiOutput(ns("owner_offer_amount_preview")),
        p(style = "color: var(--fm-muted); font-size: 12px;", "This offer will be submitted to the player owner and tracked in market transaction history."),
        footer = div(style = "text-align: center; width: 100%; display: flex; justify-content: center; gap: 10px;",
                     modalButton("Cancel"),
                     actionButton(ns("submit_owner_offer"), "Submit Offer", class = "btn btn-offer-money")),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Direct Offer to Owner ----
    observeEvent(input$submit_owner_offer, {
      req(consume_action_context("owner_offer"))
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp, login, champ_id, team_id)

      offer_amount <- input$owner_offer_amount
      if (is.null(offer_amount) || is.na(offer_amount) || !is.numeric(offer_amount) || offer_amount <= 0) {
        shiny::showNotification("Please enter a valid numerical price greater than 0 €.", type = "error")
        return()
      }

      # Preflight before write: verify capacity + verified spendable funds.
      pf <- run_acquisition_preflight(sp, "offer", amount = offer_amount)
      if (!isTRUE(pf$ok)) {
        show_preflight_failure(pf)
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
          paste0("Direct offer of ", player_card_money(offer_amount), " submitted successfully for ", sp$name, "!"),
          type = "message",
          duration = 5
        )
        clear_api_cache()
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(action_type = "owner_offer_placed", player_id = player_id, new_bid_price = offer_amount, is_cancel = FALSE), error = function(e) NULL)
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

    # ---- Shared helper: open the release-clause buyout confirmation modal ----
    # Single code path for the "Buy Release Clause" button and for external
    # "clause_buyout" action events (e.g. Today's "Exercise Clause"
    # recommendation). It RECHECKS the strict open-clause state before
    # showing (finite positive price, explicitly FALSE clause_transferred,
    # parseable clause_date <= now) and honors the same
    # run_acquisition_preflight behavior as the button path. Only the
    # recomputed clause_price is shown and later executed -- no comparison
    # price (e.g. max(market, clause)) is ever used. Never opens the market
    # bid modal.
    open_clause_buyout_modal <- function(sp) {
      req(sp)

      current_team <- get_reactive_val(user_team_id)
      if (player_card_is_own_player(sp,current_team)) {
        shiny::showNotification(
          "This player already belongs to your squad; you cannot buy your own release clause.",
          type="warning",duration=5
        )
        clause_modal_opened_RV(FALSE)
        return(invisible(FALSE))
      }

      # Recheck strict open state (fail closed): a stale / locked /
      # transferred clause must not open the buyout confirmation.
      if (!isTRUE(today_is_clause_open(sp))) {
        shiny::showNotification(
          "This release clause is not currently open (price, transfer lock, or clause date no longer valid). Please refresh and try again.",
          type = "warning",
          duration = 5
        )
        clause_modal_opened_RV(FALSE)
        return(invisible(FALSE))
      }

      # Recompute the clause price locally (single source of truth).
      clause_price <- if ("clause_price" %in% colnames(sp) && !is.na(sp$clause_price)) suppressWarnings(as.numeric(sp$clause_price)) else 0

      # Preflight: clause buyout requires a free roster slot.
      pf <- run_acquisition_preflight(sp, "clause", amount = NULL)
      if (!isTRUE(pf$ok)) {
        show_preflight_failure(pf)
        clause_modal_opened_RV(FALSE)
        return(invisible(FALSE))
      }

      remember_action_context("clause")
      clause_modal_opened_RV(TRUE)
      showModal(modalDialog(
        title = tagList(icon("bolt"), " Confirm Release Clause Buyout"),
        p(strong(sp$name)),
        p("This will instantly purchase the player for their official release clause."),
        p("Clause price: ", strong(player_card_money(clause_price))),
        p(style = "color: var(--fm-danger); font-size: 13px; font-weight: 600;", "Are you sure you want to trigger this clause buyout?"),
        footer = div(style = "text-align: center; width: 100%; display: flex; justify-content: center; gap: 10px;",
                     modalButton("Cancel"),
                     actionButton(ns("submit_clause"), "Confirm Clause Buyout", class = "btn btn-buy-clause")),
        easyClose = TRUE,
        size = "s"
      ))
      invisible(TRUE)
    }

    # ---- Option 3: Release Clause Buyout Modal ----
    observeEvent(input$btn_pay_clause, {
      sp <- selected_player()
      req(sp)
      open_clause_buyout_modal(sp)
    })

    # ---- Submit Clause Purchase ----
    observeEvent(input$submit_clause, {
      req(consume_action_context("clause"))
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp, login, champ_id, team_id)

      # Recheck strict open state before the write (fail closed): the clause
      # may have locked / transferred between modal-open and submit.
      if (!isTRUE(today_is_clause_open(sp))) {
        removeModal()
        shiny::showNotification(
          "This release clause is no longer open (price, transfer lock, or clause date no longer valid). Please refresh and try again.",
          type = "error",
          duration = 5
        )
        return()
      }

      # Recompute the clause price locally (do not rely on modal-scope state).
      # Only this clause price is sent to the clause endpoint -- no
      # comparison price (e.g. max(market, clause)) is ever transmitted.
      clause_price <- if ("clause_price" %in% colnames(sp) && !is.na(sp$clause_price)) suppressWarnings(as.numeric(sp$clause_price)) else 0
      if (is.na(clause_price) || clause_price <= 0) {
        shiny::showNotification("Release clause price is not available for this player.", type = "error")
        return()
      }

      # Preflight before write: verify roster slot + verified spendable funds.
      pf <- run_acquisition_preflight(sp, "clause", amount = clause_price)
      if (!isTRUE(pf$ok)) {
        show_preflight_failure(pf)
        return()
      }

      player_id <- sp$id
      player_slug <- if ("slug" %in% colnames(sp) && !is.na(sp$slug)) sp$slug else sp$name

      res <- buy_roster_clause(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id,
        player_slug = player_slug,
        price = clause_price
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
          paste0("Release clause buyout of ", player_card_money(clause_price), " executed successfully for ", sp$name, "!"),
          type = "message",
          duration = 5
        )
        clear_api_cache()
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(action_type = "clause_paid"), error = function(e) NULL)
        }
      } else {
        if(is.list(res)&&identical(res$code,"api.market.max_number_players_in_roster")) clear_api_cache()
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
      remember_action_context("list")
      sp <- selected_player()
      req(sp)
      default_price <- if ("value" %in% colnames(sp) && !is.na(sp$value) && sp$value > 0) sp$value else 1000000

      showModal(modalDialog(
        title = tagList(icon("tags"), paste0(" List ", sp$name, " on Market")),
        p(strong(sp$name)),
        p("Current Market Valuation: ", strong(player_card_money(default_price))),
        numericInput(
          ns("sale_price_input"),
          label = "Asking Listing Price (EUR):",
          value = default_price,
          min = 1,
          step = 10000
        ),
        uiOutput(ns("sale_price_input_preview")),
        p(style = "color: var(--fm-muted); font-size: 12px;", "This player will be listed on the transfer market for other users and computer to place bids."),
        footer = div(style = "text-align: center; width: 100%; display: flex; justify-content: center; gap: 10px;",
                     modalButton("Cancel"),
                     actionButton(ns("submit_put_on_market"), "Confirm Market Listing", class = "btn btn-offer-money")),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Put Player on Market ----
    observeEvent(input$submit_put_on_market, {
      req(consume_action_context("list"))
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp, login, champ_id, team_id)

      sale_price <- input$sale_price_input
      if (is.null(sale_price) || is.na(sale_price) || !is.numeric(sale_price) || sale_price <= 0) {
        shiny::showNotification("Please enter a valid numerical price greater than 0 €.", type = "error")
        return()
      }

      player_id <- sp$id

      already_listed <- ("market_inMarket" %in% names(sp) && isTRUE(as.logical(sp$market_inMarket))) ||
        ("effective_market_price" %in% names(sp) && is.finite(suppressWarnings(as.numeric(sp$effective_market_price))) && suppressWarnings(as.numeric(sp$effective_market_price)) > 0)
      res <- if (already_listed) {
        update_player_market_listing(login, champ_id, team_id, player_id, sale_price)
      } else {
        put_player_on_market(login, champ_id, team_id, player_id, sale_price)
      }

      is_success <- if (is.list(res)) isTRUE(res$success) else isTRUE(res)

      removeModal()

      if (is_success) {
        shiny::showNotification(
          paste0(sp$name, " listed on the transfer market for ", player_card_money(sale_price), "!"),
          type = "message",
          duration = 5
        )
        clear_api_cache()
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(action_type = "player_listed", player_id = player_id, new_bid_price = sale_price, is_cancel = FALSE), error = function(e) NULL)
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

    # ---- Direct sale to Futmondo: no received offer is required ----
    observeEvent(input$btn_direct_sell, {
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp,login,champ_id,team_id,player_card_is_own_player(sp,team_id))
      check <- direct_sale_preflight(login,champ_id,team_id,sp$id)
      if(!isTRUE(check$ok)) {
        showNotification(check$message,type="error");return()
      }
      remember_action_context("direct_sell")
      showModal(modalDialog(title=paste0("Sell ",sp$name," directly to Futmondo?"),
        p("This immediately sells the player to Futmondo / Mercado without waiting for an offer."),
        if(is.finite(check$value)) p(paste0("Current market value (reference only): ",player_card_money(check$value))),
        p("Futmondo determines the sale proceeds. A guaranteed sale price is not available. The player will leave your squad, which may affect your lineup."),
        footer=tagList(modalButton("Cancel"),actionButton(ns("submit_direct_sell"),"Confirm direct sale",class="btn btn-danger")),
        easyClose=FALSE,size="s"))
    })

    observeEvent(input$submit_direct_sell, {
      req(consume_action_context("direct_sell"))
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp,login,champ_id,team_id,player_card_is_own_player(sp,team_id))
      result <- direct_sell_player(login,champ_id,team_id,sp$id)
      removeModal()
      showNotification(result$message,type=if(isTRUE(result$success)) "message" else "error",duration=8)
      if(isTRUE(result$success) || isTRUE(result$uncertain)) {
        clear_api_cache(login[["userid"]])
        if(is.function(on_bid_updated)) tryCatch(on_bid_updated(
          action_type=if(isTRUE(result$success)) "player_sold_direct" else "refresh",
          player_id=sp$id,new_bid_price=NA_real_,is_cancel=FALSE),error=function(e)NULL)
      }
    })

    # ---- Remove Player from Market Modal ----
    observeEvent(input$btn_cancel_sell, {
      remember_action_context("delist")
      sp <- selected_player()
      req(sp)

      showModal(modalDialog(
        title = tagList(icon("tag"), paste0(" Remove ", sp$name, " from Market")),
        p(strong(sp$name)),
        p("Are you sure you want to withdraw this player from the transfer market?"),
        footer = div(style = "text-align: center; width: 100%; display: flex; justify-content: center; gap: 10px;",
                     modalButton("Cancel"),
                     actionButton(ns("submit_cancel_sell"), "Confirm Remove from Market", class = "btn btn-cancel-bid")),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Remove Player from Market ----
    observeEvent(input$submit_cancel_sell, {
      req(consume_action_context("delist"))
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
          tryCatch(on_bid_updated(action_type = "listing_cancelled", player_id = player_id, new_bid_price = NA_real_, is_cancel = TRUE), error = function(e) NULL)
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

    # ---- Accept Received Offer Modal ----
    observeEvent(input$btn_accept_offer, open_accept_offer_modal())

    # ---- Submit Accept Received Offer ----
    observeEvent(input$submit_accept_offer, {
      req(consume_action_context("accept"))
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp, login, champ_id, team_id)

      player_id <- as.character(sp$id)
      bid_id <- NULL

      if ("bid_id" %in% colnames(sp) && !is.na(sp$bid_id) && sp$bid_id != "") {
        bid_id <- as.character(sp$bid_id)
      } else if ("bid__id" %in% colnames(sp) && !is.na(sp$bid__id) && sp$bid__id != "") {
        bid_id <- as.character(sp$bid__id)
      } else {
        sum_res <- tryCatch({
          get_player_summary(login = login, championship_id = champ_id, user_team_id = team_id, player_id = player_id)
        }, error = function(e) NULL)
        if (!is.null(sum_res) && !is.null(sum_res$bids) && is.list(sum_res$bids) && length(sum_res$bids) > 0) {
          first_b <- sum_res$bids[[1]]
          if (is.list(first_b) && !is.null(first_b[["id"]])) {
            bid_id <- as.character(first_b[["id"]])
          }
        }
      }

      if (is.null(bid_id) || bid_id == "") {
        shiny::showNotification("Could not identify the received bid ID. Please try again.", type = "error")
        return()
      }

      res <- accept_bid(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id,
        bid_id = bid_id
      )

      is_success <- if (is.list(res)) isTRUE(res$success) else isTRUE(res)

      removeModal()

      if (is_success) {
        shiny::showNotification(
          paste0("Offer accepted! ", sp$name, " sold successfully."),
          type = "message",
          duration = 5
        )
        clear_api_cache()
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(action_type = "offer_accepted", player_id = player_id, new_bid_price = NA_real_, is_cancel = TRUE), error = function(e) NULL)
        }
      } else {
        err_msg <- if (is.list(res) && !is.null(res$message) && res$message != "") res$message else "Accept failed. Please try again."
        shiny::showNotification(
          paste0("Failed to accept offer: ", err_msg),
          type = "error",
          duration = 6
        )
      }
    })

    # ---- Reject Received Offer Modal ----
    observeEvent(input$btn_reject_offer, {
      remember_action_context("reject")
      sp <- selected_player()
      req(sp)
      rec_offer_price <- suppressWarnings(as.numeric(sp$bid_price))
      rec_offer_bidder <- if ("bid_user" %in% colnames(sp) && !is.na(sp$bid_user) && sp$bid_user != "") as.character(sp$bid_user) else "Futmondo"

      showModal(modalDialog(
        title = tagList(icon("circle-xmark"), paste0(" Reject Offer for ", sp$name)),
        p(strong(sp$name)),
        p("Are you sure you want to REJECT the received offer of ", strong(player_card_money(rec_offer_price)), " from ", strong(rec_offer_bidder), "?"),
        footer = div(style = "text-align: center; width: 100%; display: flex; justify-content: center; gap: 10px;",
                     modalButton("Cancel"),
                     actionButton(ns("submit_reject_offer"), "Confirm Reject Offer", class = "btn btn-cancel-bid")),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Reject Received Offer ----
    observeEvent(input$submit_reject_offer, {
      req(consume_action_context("reject"))
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp, login, champ_id, team_id)

      player_id <- as.character(sp$id)
      bid_id <- NULL

      if ("bid_id" %in% colnames(sp) && !is.na(sp$bid_id) && sp$bid_id != "") {
        bid_id <- as.character(sp$bid_id)
      } else if ("bid__id" %in% colnames(sp) && !is.na(sp$bid__id) && sp$bid__id != "") {
        bid_id <- as.character(sp$bid__id)
      } else {
        sum_res <- tryCatch({
          get_player_summary(login = login, championship_id = champ_id, user_team_id = team_id, player_id = player_id)
        }, error = function(e) NULL)
        if (!is.null(sum_res) && !is.null(sum_res$bids) && is.list(sum_res$bids) && length(sum_res$bids) > 0) {
          first_b <- sum_res$bids[[1]]
          if (is.list(first_b) && !is.null(first_b[["id"]])) {
            bid_id <- as.character(first_b[["id"]])
          }
        }
      }

      if (is.null(bid_id) || bid_id == "") {
        shiny::showNotification("Could not identify the received bid ID. Please try again.", type = "error")
        return()
      }

      res <- reject_bid(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id,
        bid_id = bid_id
      )

      is_success <- if (is.list(res)) isTRUE(res$success) else isTRUE(res)

      removeModal()

      if (is_success) {
        shiny::showNotification(
          paste0("Offer rejected for ", sp$name, "."),
          type = "message",
          duration = 5
        )
        clear_api_cache()
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(action_type = "offer_rejected", player_id = player_id, new_bid_price = NA_real_, is_cancel = TRUE), error = function(e) NULL)
        }
      } else {
        err_msg <- if (is.list(res) && !is.null(res$message) && res$message != "") res$message else "Reject failed. Please try again."
        shiny::showNotification(
          paste0("Failed to reject offer: ", err_msg),
          type = "error",
          duration = 6
        )
      }
    })

    ## render player_trend_plot (Plot A) ----
    output$player_trend_plot <- plotly::renderPlotly({
      sp <- selected_player()
      req(sp)

      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      player_id <- sp$id

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

      history_df <- NULL
      if (!is.null(champ_id) && !is.null(player_id)) {
        tryCatch({
          history_df <- get_player_historical_data(player_id, champ_id)
        }, error = function(e) {
          print(paste0("[Plot A] Error loading history: ", e$message))
        })
      }

      # ---- Valuation series (with pre-season simulated fallback) ----
      val_df <- NULL
      if (is.data.frame(history_df) && nrow(history_df) > 0 && "value" %in% colnames(history_df)) {
        val_df <- history_df
      } else {
        # Fallback simulated valuation only when DB is empty/unconfigured.
        # NOTE: no points are fabricated here (points trace is built separately).
        today <- Sys.time()
        dates <- seq(today - as.difftime(6, units="days"), today, by="1 day")
        val_today <- if ("value" %in% colnames(sp)) suppressWarnings(as.numeric(sp$value[1])) else NA_real_
        val_change <- if ("change" %in% colnames(sp)) suppressWarnings(as.numeric(sp$change[1])) else NA_real_
        if (!is.finite(val_today)) val_today <- 1000000
        if (!is.finite(val_change)) val_change <- 0

        val_df <- data.frame(
          recorded_at = as.character(dates),
          value = seq(val_today - val_change, val_today, length.out = length(dates)),
          stringsAsFactors = FALSE
        )
      }

      val_df$date <- as.POSIXct(parse_safe_datetime(val_df$recorded_at))
      val_df$value <- suppressWarnings(as.numeric(as.character(val_df$value)))
      val_df <- val_df %>% dplyr::filter(!is.na(date), is.finite(value)) %>% dplyr::arrange(date)
      if (!nrow(val_df)) {
        current_value <- if ("value" %in% colnames(sp)) suppressWarnings(as.numeric(sp$value[1])) else NA_real_
        if (!is.finite(current_value)) current_value <- 1000000
        val_df <- data.frame(date = Sys.time(), value = current_value)
      }

      # ---- Points series: one marker per completed round; graceful no-points ----
      # Fetch finished rounds defensively; points are only rendered when round
      # boundary data is available (never from daily snapshots alone).
      finished_rounds_df <- NULL
      if (!is.null(login) && !is.null(champ_id)) {
        tryCatch({
          finished_rounds_df <- get_finished_rounds(login, champ_id)
        }, error = function(e) {
          print(paste0("[Plot A] Error loading finished rounds: ", e$message))
        })
      }
      stored_round_history <- if (!is.null(champ_id) && !is.null(player_id)) tryCatch(
        get_player_round_points_history(player_id, champ_id), error=function(e) NULL) else NULL
      points_trace <- player_match_points_trace(stored_round_history)
      if (!points_trace$has_points) points_trace <- build_player_points_trace(history_df, finished_rounds_df, sp)
      if (!points_trace$has_points && !is.null(login) && !is.null(champ_id) && !is.null(player_id)) {
        summary <- tryCatch(get_player_summary(login, champ_id, get_reactive_val(user_team_id), player_id), error = function(e) NULL)
        summary_trace <- player_summary_points_trace(summary, finished_rounds_df)
        if (summary_trace$has_points) points_trace <- summary_trace
      }

      chart_metric <- if (!is.null(input$player_chart_metric) && identical(as.character(input$player_chart_metric), "points")) "points" else "valuation"
      if (identical(chart_metric, "points")) {
        if (isTRUE(points_trace$has_points)) {
          return(plotly::plot_ly(data = points_trace$points_df) %>%
            plotly::add_trace(type = "scatter", mode = "lines+markers", x = ~round_number, y = ~points,
              name = "Round points", line = list(color = "#10b981", width = 2.5),
              marker = list(size = 8, color = "#10b981"), hoverinfo = "text",
              text = ~paste0("Jornada ", round_number, "<br>Points: ", points)) %>%
            fm_plot_layout(hovermode = "x unified", paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor = "rgba(0,0,0,0)", xaxis = list(title = "Round", dtick = 1, gridcolor = "#f1f5f9"),
              yaxis = list(title = "Points", autorange = FALSE, range = player_trend_axis_range(points_trace$points_df$points), gridcolor = "#f1f5f9"),
              showlegend = FALSE, margin = list(l = 50, r = 20, t = 10, b = 40)))
        }
        aggregate_points <- suppressWarnings(as.numeric(as.character(sp$points)))
        no_points_text <- if (length(aggregate_points) == 1L && is.finite(aggregate_points) && aggregate_points > 0) {
          paste0("Total points: ", format(aggregate_points, trim = TRUE, scientific = FALSE),
            ". Round-by-round history is unavailable.")
        } else "No round-by-round points recorded yet"
        return(plotly::plot_ly() %>% fm_plot_layout(paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)", xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
          annotations = list(list(text = no_points_text, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE, align = "center", font = list(color = "#94a3b8", size = 13))),
          margin = list(l = 20, r = 20, t = 10, b = 20)))
      }

      # Valuation mode keeps its own readable scale. Points have a dedicated
      # chart selected through the switch above rather than a competing axis.
      chart <- plotly::plot_ly(data = val_df) %>%
        plotly::add_trace(
          type = "scatter",
          mode = "lines+markers",
          x = ~date, y = ~value,
          name = "Market Valuation (\u20ac)",
          fill = "tozeroy",
          fillcolor = "rgba(59, 130, 246, 0.08)",
          line = list(color = "#3b82f6", width = 2.5),
          yaxis = "y",
          hoverinfo = "text",
          text = ~paste0("Date: ", format(date, "%d-%m-%y"), "<br>Valuation: ", player_card_money(value))
        )

      if (identical(chart_metric, "points") && points_trace$has_points) {
        # One marker per completed round (no interpolated line).
        chart <- chart %>%
          plotly::add_trace(
            data = points_trace$points_df,
            type = "scatter",
            mode = "markers",
            x = ~date, y = ~points,
            name = "Points",
            inherit = FALSE,
            marker = list(size = 8, color = "#10b981", symbol = "circle"),
            yaxis = "y2",
            hoverinfo = "text",
            text = ~paste0("Jornada ", round_number, " (", format(date, "%d-%m-%y"), ")<br>Points: ", points)
          )
        yaxis2_cfg <- list(
          title = "Points",
          overlaying = "y",
          side = "right",
          showgrid = FALSE,
          autorange = FALSE,
          range = player_trend_axis_range(points_trace$points_df$points)
        )
        annotations_cfg <- NULL
      } else {
        # Valuation mode does not need a second points axis or an unavailable
        # annotation; the Points chart displays that state when selected.
        if (identical(chart_metric, "valuation")) {
          yaxis2_cfg <- list(visible = FALSE)
          annotations_cfg <- NULL
        } else {
        # Keep aggregate points and round-by-round observations distinct. A
        # player can have a current total while finalized per-round data has
        # not yet been collected.
        yaxis2_cfg <- list(visible = FALSE)
        aggregate_points <- suppressWarnings(as.numeric(as.character(sp$points)))
        no_points_text <- if (length(aggregate_points) == 1L && is.finite(aggregate_points) && aggregate_points > 0) {
          paste0("Total points: ", format(aggregate_points, trim = TRUE, scientific = FALSE),
            ". Round-by-round history is unavailable.")
        } else {
          "No round-by-round points recorded yet"
        }
        annotations_cfg <- list(list(
          text = no_points_text,
          x = 0.98, y = 0.98, xref = "paper", yref = "paper",
          showarrow = FALSE, align = "right",
          font = list(color = "#94a3b8", size = 12),
          bgcolor = "rgba(248,250,252,0.85)",
          bordercolor = "#e2e8f0",
          borderpad = 4
        ))
        }
      }

      chart %>%
        fm_plot_layout(
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
            title = "Valuation (\u20ac)",
            tickformat = "s",
            autorange = FALSE,
            range = player_trend_axis_range(val_df$value),
            gridcolor = "#f1f5f9"
          ),
          yaxis2 = yaxis2_cfg,
          annotations = annotations_cfg,
          legend = list(orientation = "h", x = 0.5, y = -0.25, xanchor = "center"),
          margin = list(l = 50, r = 50, t = 10, b = 40)
        )
    })

    ## render recent completed-round points ----
    output$recent_round_points <- renderUI({
      sp <- selected_player(); req(sp)
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      history <- if (!is.null(champ_id) && !is.null(sp$id)) tryCatch(
        get_player_historical_data(sp$id,champ_id),error=function(e)NULL) else NULL
      rounds <- if (!is.null(login) && !is.null(champ_id)) tryCatch(
        get_finished_rounds(login,champ_id),error=function(e)NULL) else NULL
      stored_round_history <- if (!is.null(champ_id) && !is.null(sp$id)) tryCatch(
        get_player_round_points_history(sp$id, champ_id), error=function(e) NULL) else NULL
      stored_trace <- player_match_points_trace(stored_round_history)
      recent <- if (is.list(stored_trace) && isTRUE(stored_trace$has_points) && is.data.frame(stored_trace$points_df)) {
        tail(stored_trace$points_df, 5L)[order(tail(stored_trace$points_df, 5L)$round_number, decreasing=TRUE), , drop=FALSE]
      } else latest_player_round_points(history,rounds,limit=5L)
      if (!is.data.frame(recent) || !all(c("round_number", "points") %in% names(recent))) {
        recent <- data.frame(round_number=numeric(), points=numeric())
      }
      if (!nrow(recent) && !is.null(login) && !is.null(champ_id) && !is.null(sp$id)) {
        summary <- tryCatch(get_player_summary(login, champ_id, get_reactive_val(user_team_id), sp$id), error = function(e) NULL)
        summary_trace <- player_summary_points_trace(summary, rounds)
        if (summary_trace$has_points) recent <- tail(summary_trace$points_df, 5L)[order(tail(summary_trace$points_df, 5L)$round_number, decreasing=TRUE), , drop=FALSE]
      }
      empty_text <- recent_round_points_empty_text(sp$points)
      div(class="player-recent-rounds",
        h4(icon("futbol")," Latest round points"),
        if (!nrow(recent)) span(class="player-recent-rounds-empty",empty_text) else
          div(class="player-recent-rounds-list",lapply(seq_len(nrow(recent)),function(i)
            div(class="player-recent-round-chip",
              span(class="player-recent-round-label",paste("Round",recent$round_number[i])),
              strong(paste0(format(recent$points[i],trim=TRUE,scientific=FALSE)," pts")))))
      )
    })

    ## render profile and match context from captured player endpoints ----
    output$player_external_details <- renderUI({
      sp <- selected_player(); req(sp)
      login <- get_reactive_val(login_token)
      if (!valid_login(login) || is.null(sp$id) || !nzchar(as.character(sp$id))) {
        return(div(class="player-external-details", h4(icon("address-card"), " Player profile & matches"),
          span(class="player-external-unavailable", "Player profile is unavailable until a valid session is available.")))
      }
      profile_answer <- tryCatch(get_player_full_profile(login, sp$id), error=function(e) NULL)
      matches_answer <- tryCatch(get_player_matches(login, sp$id), error=function(e) NULL)
      profile_data <- normalize_player_full_profile(profile_answer)
      matches <- normalize_player_matches(matches_answer)
      profile <- profile_data$profile
      profile_bits <- c()
      if (nzchar(profile$nationality)) profile_bits <- c(profile_bits, profile$nationality)
      if (is.finite(profile$height_cm)) profile_bits <- c(profile_bits, paste0(format(profile$height_cm, trim=TRUE), " cm"))
      if (is.finite(profile$weight_kg)) profile_bits <- c(profile_bits, paste0(format(profile$weight_kg, trim=TRUE), " kg"))
      if (nzchar(profile$preferred_foot)) profile_bits <- c(profile_bits, profile$preferred_foot)
      prices <- profile_data$price_history
      latest_price <- if (nrow(prices)) prices[nrow(prices),,drop=FALSE] else NULL
      price_text <- if (!is.null(latest_price) && is.finite(latest_price$social[1])) {
        paste0("Profile valuation: ", player_card_money(latest_price$social[1]), " · ", nrow(prices), " saved values")
      } else "Profile valuation history is unavailable."
      div(class="player-external-details",
        h4(icon("address-card"), " Player profile & matches"),
        if (is.null(profile_answer)) div(class="player-external-unavailable", "Player profile is unavailable. Please refresh to retry.") else
          div(class="player-profile-summary",
            if (length(profile_bits)) span(paste(profile_bits, collapse=" · ")) else span("Profile details are unavailable."),
            span(class="player-profile-price", price_text)
          ),
        h5("Recent matches"),
        if (is.null(matches_answer)) div(class="player-external-unavailable", "Match history is unavailable. Please refresh to retry.") else if (!nrow(matches))
          div(class="player-external-unavailable", "No completed matches are available.") else
          div(class="player-match-list", lapply(seq_len(min(5L,nrow(matches))), function(i) {
            match <- matches[i,,drop=FALSE]
            score <- if (is.finite(match$home_score) && is.finite(match$away_score)) paste0(format(match$home_score,trim=TRUE), "–", format(match$away_score,trim=TRUE)) else "Score unavailable"
            date <- if (nzchar(match$occurred_at)) sub("T.*", "", match$occurred_at) else "Date unavailable"
            div(class="player-match-card",
              span(class="player-match-round", paste("Round", format(match$round,trim=TRUE))),
              strong(paste(match$home, score, match$away)),
              span(class="player-match-date", date)
            )
          }))
      )
    })

    ## render player_points_description_box ----
    output$player_points_description_box <- renderUI({
      sp <- selected_player()
      req(sp)
      ret <- tagList()
      points <- sp$points

      clean_points <- if (is.null(points) || is.na(points) || points == "NaN" || points == "") {
        "Unavailable"
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
        "Unavailable"
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
      if (length(change) == 1L && is.finite(change) && change > 0) {
        icon <- icon("caret-up")
        number_color = "green"
      } else if (length(change) == 1L && is.finite(change) && change < 0) {
        icon <- icon("caret-down")
        number_color = "red"
      } else {
        icon <- NULL
        number_color = "black"
      }
      change_text <- if (length(change) == 1L && is.finite(change)) {
        add_sign(player_card_money(change))
      } else {
        "Unavailable"
      }
      descriptionBlock(
        header = player_card_money(value),
        number = paste0(change_text, " (", round(change_pct, 2), "%)"),
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

    ## render FIS 5-Pillar Breakdown Panel ----
    output$fis_panel <- renderUI({
      sp <- selected_player()
      req(sp)

      # Reuse the exact cohort-independent score already displayed in the table.
      # Preserve a row as a row: double transposition previously destroyed columns.
      fis_result <- selected_player_fis_row(sp)

      if (is.null(fis_result) || nrow(fis_result) == 0) {
        return(div(style = "color: var(--fm-muted); font-size: 13px;", "FIS data unavailable."))
      }

      fis_score_val <- suppressWarnings(as.numeric(fis_result$fis_score[1]))
      fis_tier_val <- if (!is.na(fis_result$fis_tier[1]) && nzchar(as.character(fis_result$fis_tier[1]))) as.character(fis_result$fis_tier[1]) else "N/A"
      fis_summary_val <- if (!is.na(fis_result$fis_summary[1]) && nzchar(as.character(fis_result$fis_summary[1]))) as.character(fis_result$fis_summary[1]) else ""

      # Pillar values (0-100)
      perf_val <- suppressWarnings(as.numeric(fis_result$perf[1]))
      form_val <- suppressWarnings(as.numeric(fis_result$form[1]))
      eff_val <- suppressWarnings(as.numeric(fis_result$efficiency[1]))
      mom_val <- suppressWarnings(as.numeric(fis_result$momentum[1]))
      fix_val <- suppressWarnings(as.numeric(fis_result$fixture_risk[1]))

      # Tier badge styling
      badge_bg <- if (fis_tier_val == "Strong Buy") {
        "#dcfce7"
      } else if (fis_tier_val == "Buy") {
        "#e0f2fe"
      } else if (fis_tier_val == "Hold") {
        "#fef3c7"
      } else {
        "#fee2e2"
      }
      badge_text <- if (fis_tier_val == "Strong Buy") {
        "#166534"
      } else if (fis_tier_val == "Buy") {
        "#0369a1"
      } else if (fis_tier_val == "Hold") {
        "#92400e"
      } else {
        "#991b1b"
      }

      # Confidence pill color
      coverage <- suppressWarnings(as.numeric(fis_result$data_coverage[1]))
      conf_pct <- if (length(coverage) == 1L && is.finite(coverage)) round(100 * coverage) else NA_real_
      conf_color <- if (!is.finite(conf_pct)) "#64748b" else if (conf_pct >= 80) "#16a34a" else if (conf_pct >= 65) "#2563eb" else if (conf_pct >= 45) "#d97706" else "#dc2626"

      # Helper to render a single pillar bar
      render_pillar <- function(label, value, missing_label = "Unavailable") {
        available <- length(value) == 1L && is.finite(value)
        v <- if (available) round(value, 1) else 0
        bar_color <- if (v >= 70) "#16a34a" else if (v >= 50) "#2563eb" else if (v >= 30) "#d97706" else "#dc2626"
        div(
          style = "margin-bottom: 8px;",
          div(
            style = "display: flex; justify-content: space-between; font-size: 12px; font-weight: 600; margin-bottom: 3px;",
            span(label),
            span(style = paste0("color: ", bar_color, ";"), if (available) paste0(v, "/100") else missing_label)
          ),
          div(
            style = "height: 8px; background: var(--fm-surface); border-radius: 4px; overflow: hidden;",
            div(
              style = paste0("height: 100%; width: ", max(0, min(v, 100)), "%; background: ", bar_color, "; border-radius: 4px; transition: width 0.3s;")
            )
          )
        )
      }

      div(
        style = "background: var(--fm-surface); border: 1px solid #e2e8f0; border-radius: 8px; padding: 16px;",
        div(
          style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px; flex-wrap: wrap;",
          div(
            style = paste0("font-size: 22px; font-weight: 800; color: var(--fm-text);"),
            "Descriptive rating ", if (!is.na(fis_score_val)) round(fis_score_val, 1) else "N/A"
          ),
          span(
            style = paste0("display: inline-block; padding: 3px 10px; border-radius: 12px; font-weight: 700; font-size: 12px; background: ", badge_bg, "; color: ", badge_text, ";"),
            fis_tier_val
          ),
          span(
            style = paste0("display: inline-block; padding: 3px 10px; border-radius: 12px; font-weight: 600; font-size: 11px; background: ", conf_color, "; color: var(--fm-text);"),
            if (is.finite(conf_pct)) paste0("Data coverage: ", conf_pct, "%") else "Data coverage unavailable"
          )
        ),
        if (nzchar(fis_summary_val)) {
          p(
            style = "margin: 0 0 14px 0; font-size: 13px; color: var(--fm-muted); font-style: italic; line-height: 1.4;",
            fis_summary_val
          )
        },
        div(
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 12px;",
          render_pillar("Performance", perf_val),
          render_pillar("Form", form_val),
          render_pillar("Points/EUR Efficiency", eff_val),
          render_pillar("Price Momentum", mom_val),
          render_pillar("Availability & Fitness", fix_val, "Not reported")
        ),
        if (!is.finite(fix_val)) {
          p(style = "font-size: 12px; color: var(--fm-muted); margin-top: 10px;",
            "Futmondo has not reported an availability status for this player.")
        }
      )
    })

    ## render Smart Bid & Auction Intelligence Widget ----
    output$smart_bid_widget <- renderUI({
      sp <- selected_player()
      req(sp)

      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)

      current_user_team <- get_reactive_val(user_team_id)
      player_owner_team <- if ("user_team_id" %in% colnames(sp)) sp$user_team_id else NULL
      is_own_player <- (length(current_user_team) == 1L && !is.na(current_user_team) &&
                        length(player_owner_team) == 1L && !is.na(player_owner_team) &&
                        identical(as.character(current_user_team), as.character(player_owner_team)))

      # Only show for non-owned players
      if (is_own_player) {
        return(NULL)
      }

      # Verified acquisition capacity (roster + verified spendable funds +
      # target's competing bids). Cached per (userid, championship, team, target).
      capacity <- tryCatch({
        if (!is.null(login) && !is.null(champ_id) && !is.null(team_id)) {
          get_acquisition_capacity(
            login = login,
            championship_id = champ_id,
            user_team_id = team_id,
            target_player_id = as.character(sp$id)
          )
        } else {
          NULL
        }
      }, error = function(e) NULL)

      # Pressroom transaction history (cached) for value/competition context.
      pressroom_df <- tryCatch({
        if (!is.null(login) && !is.null(champ_id)) {
          get_championship_pressroom(login = login, championship_id = champ_id)
        } else {
          NULL
        }
      }, error = function(e) NULL)

      # Market high bid = highest competing bid on the target (from capacity).
      mhb <- if (!is.null(capacity) && is.list(capacity$target) &&
                 is.numeric(capacity$target$highest_bid) && is.finite(capacity$target$highest_bid) &&
                 capacity$target$highest_bid > 0) {
        capacity$target$highest_bid
      } else {
        NULL
      }

      # Compute smart bid. user_cash is intentionally NA (unverified); the
      # verified spendable funds come from the capacity snapshot, so the
      # recommendation is bounded by real available budget (no 300M hardcode).
      smart_bid_result <- tryCatch({
        calculate_smart_bid(
          player_row = sp,
          championship_id = if (!is.null(champ_id)) as.character(champ_id) else "",
          pressroom_df = pressroom_df,
          user_teams_df = NULL,
          user_cash = NA,
          market_high_bid = mhb,
          capacity = capacity
        )
      }, error = function(e) {
        print(paste0("[Smart Bid Widget] Error computing smart bid: ", e$message))
        list(error = e$message)
      })

      if (is.null(smart_bid_result) || !is.null(smart_bid_result$error)) {
        return(div(style = "color: var(--fm-muted); font-size: 13px;", "Smart bid data unavailable."))
      }

      # Cache the smart bid result for the "Use Smart Bid" button
      smart_bid_cache_RV(smart_bid_result)

      fair_value <- smart_bid_result$fair_value
      min_winning <- smart_bid_result$min_winning_bid
      recommended <- smart_bid_result$recommended_bid
      max_rational <- smart_bid_result$max_rational_bid
      roi_pct <- smart_bid_result$expected_roi_pct
      if (length(roi_pct) != 1L || !is.finite(roi_pct)) roi_pct <- NA_real_
      comp_level <- smart_bid_result$competition_level
      competitors <- smart_bid_result$likely_competitors
      conf_pct <- smart_bid_result$confidence_pct
      if (length(conf_pct) != 1L || !is.finite(conf_pct)) conf_pct <- NA_real_

      # Competition level styling
      comp_color <- if (comp_level == "High") {
        "#dc2626"
      } else if (comp_level == "Medium") {
        "#d97706"
      } else if (comp_level == "Low") {
        "#16a34a"
      } else {
        "#64748b"
      }

      # ROI color
      roi_color <- if (!is.na(roi_pct) && roi_pct > 0) "#16a34a" else if (!is.na(roi_pct) && roi_pct < 0) "#dc2626" else "#64748b"

      # Competitors list
      comp_list_html <- ""
      if (!is.null(competitors) && length(competitors) > 0) {
        comp_items <- vapply(competitors, function(c_name) {
          c_str <- if (is.na(c_name) || c_name == "") "Futmondo / Mercado" else as.character(c_name)
          paste0("<li style='margin-bottom: 4px;'>", htmltools::htmlEscape(c_str), "</li>")
        }, character(1))
        comp_list_html <- paste0("<ul style='margin: 0; padding-left: 18px; font-size: 13px; color: var(--fm-muted);'>", paste(comp_items, collapse = ""), "</ul>")
      } else {
        comp_list_html <- "<p style='margin: 0; font-size: 13px; color: var(--fm-muted);'>No competitor data available.</p>"
      }

      div(
        style = "background: var(--fm-surface); border: 1px solid #e2e8f0; border-radius: 8px; padding: 16px;",
        h4(
          style = "font-weight: 700; color: var(--fm-text); margin-bottom: 12px; font-size: 15px;",
          tagList(icon("chart-line"), " Smart Bid &amp; Auction Intelligence")
        ),
        div(
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 12px; margin-bottom: 14px;",
          # Estimated Fair Value
          div(
            style = "background: var(--fm-surface); border: 1px solid #e2e8f0; border-radius: 6px; padding: 10px; text-align: center;",
            div(style = "font-size: 11px; color: var(--fm-muted); font-weight: 600; text-transform: uppercase;", "Estimated Fair Value"),
            div(style = "font-size: 18px; font-weight: 800; color: var(--fm-text);", player_card_money(fair_value))
          ),
          # Expected Winning Range
          div(
            style = "background: var(--fm-surface); border: 1px solid #e2e8f0; border-radius: 6px; padding: 10px; text-align: center;",
            div(style = "font-size: 11px; color: var(--fm-muted); font-weight: 600; text-transform: uppercase;", "Heuristic bid bounds"),
            div(style = "font-size: 14px; font-weight: 700; color: var(--fm-text);", paste0("Minimum ", player_card_money(min_winning), " · Ceiling ", player_card_money(max_rational)))
          ),
          # Recommended Smart Bid
          div(
            style = "background: var(--fm-surface); border: 2px solid #3b82f6; border-radius: 6px; padding: 10px; text-align: center;",
            div(style = "font-size: 11px; color: var(--fm-text); font-weight: 600; text-transform: uppercase;", "Recommended Smart Bid"),
            div(style = "font-size: 20px; font-weight: 800; color: var(--fm-text);",
                if (isTRUE(smart_bid_result$can_compete)) player_card_money(recommended) else "No bid")
          ),
          # Expected ROI
          div(
            style = "background: var(--fm-surface); border: 1px solid #e2e8f0; border-radius: 6px; padding: 10px; text-align: center;",
            div(style = "font-size: 11px; color: var(--fm-muted); font-weight: 600; text-transform: uppercase;", "Model-implied value gap"),
            div(style = paste0("font-size: 18px; font-weight: 800; color: ", roi_color, ";"), if (is.finite(roi_pct)) paste0(roi_pct, "%") else "Unavailable")
          )
        ),
        # Competition Level and Confidence
        div(
          style = "display: flex; gap: 16px; flex-wrap: wrap; margin-bottom: 14px; align-items: center;",
          div(
            style = "font-size: 13px; font-weight: 600;",
            "Competition: ",
            span(
              style = paste0("display: inline-block; padding: 2px 8px; border-radius: 8px; background: ", comp_color, "; color: var(--fm-text); font-weight: 700; font-size: 11px;"),
              comp_level
            )
          ),
          div(
            style = "font-size: 13px; font-weight: 600;",
            "Max Rational Bid: ",
            span(style = "color: var(--fm-danger); font-weight: 800;", player_card_money(max_rational))
          ),
          div(
            style = "font-size: 13px; font-weight: 600;",
            "Method: ",
            span("Heuristic; winning probability not calibrated")
          )
        ),
        if (!isTRUE(smart_bid_result$can_compete)) {
          p(style = "font-size: 13px; color: var(--fm-muted);", smart_bid_result$message)
        },
        # Use Smart Bid button
          div(
            style = "font-size: 13px; font-weight: 600;",
            "Verified Spendable: ",
            span(style = "font-weight: 800;", player_card_money(smart_bid_result$spendable_funds))
          ),
          if (is.finite(smart_bid_result$api_bid_limit)) div(
            style = "font-size: 13px; font-weight: 600;",
            "Futmondo Bid Limit: ",
            span(style = "font-weight: 800;", player_card_money(smart_bid_result$api_bid_limit))
          ),
        div(
          style = "margin-bottom: 14px; text-align: center;",
          actionButton(
            ns("btn_use_smart_bid"),
            label = tagList(icon("bolt"), " Use Smart Bid"),
            class = "btn btn-primary",
            disabled = !isTRUE(smart_bid_result$can_compete) || !isTRUE(smart_bid_result$funds_verified)
          )
        ),
        # Competitor Prediction Section
        div(
          style = "border-top: 1px solid #e2e8f0; padding-top: 12px;",
          h5(
            style = "font-weight: 700; color: var(--fm-text); margin-bottom: 8px; font-size: 13px;",
            tagList(icon("users-gear"), " Historical manager interest")
          ),
          shiny::HTML(comp_list_html)
        )
      )
    })
  })
}

# Keep every marker inside its axis, including constant or zero-only series.
# Explicit bounds prevent the valuation fill's zero baseline from compressing
# expensive players against the top of the chart.
player_trend_axis_range <- function(values) {
  values <- suppressWarnings(as.numeric(as.character(values)))
  values <- values[is.finite(values)]
  if (!length(values)) return(c(0, 1))
  bounds <- range(values)
  padding <- max(diff(bounds) * 0.20, abs(bounds) * 0.04, 2)
  bounds + c(-padding, padding)
}

# Pure helper: build the points trace for the player trend chart.
#
# Points are bucketed by FINISHED ROUND, not by calendar day. Each historical
# snapshot is mapped to the first completed round whose boundary window it
# falls within (with no reuse). Bucketing rule:
#   - Parse/sort the valid round `begin_process` boundaries ascending.
#   - For round i, the eligible window is (prior_boundary, boundary_i]
#     (prior_boundary is -Inf for the first round).
#   - The latest eligible snapshot is chosen for that round; a round with no
#     eligible snapshot is skipped.
#
# Returns a graceful no-points state (has_points = FALSE) when there is no
# history, no finished-round data, or no valid points -- it never fabricates a
# zero line, and it never renders points from daily snapshots when no
# finished-round data is available.
#
# Parameters:
#   history_df          -- data frame from get_player_historical_data() with
#                          `recorded_at` and `points` columns (may be NULL/empty)
#   finished_rounds_df  -- data frame from get_finished_rounds() with
#                          `round_number` and `begin_process` columns
#                          (may be NULL/empty)
#   sp                  -- selected player row (reserved for signature symmetry)
#
# Returns:
#   list(points_df = data.frame(date = POSIXct, points = numeric,
#                               round_number = numeric),
#        has_points = logical)
build_player_points_trace <- function(history_df, finished_rounds_df = NULL, sp = NULL) {
  empty <- list(
    points_df = data.frame(
      date = as.POSIXct(character(0)), points = numeric(0),
      round_number = numeric(0), stringsAsFactors = FALSE
    ),
    has_points = FALSE
  )

  if (is.null(history_df) || !is.data.frame(history_df) || nrow(history_df) == 0) {
    return(empty)
  }
  if (!("recorded_at" %in% colnames(history_df)) || !("points" %in% colnames(history_df))) {
    return(empty)
  }

  # No finished-round data -> do NOT render points from daily snapshots.
  if (is.null(finished_rounds_df) || !is.data.frame(finished_rounds_df) || nrow(finished_rounds_df) == 0) {
    return(empty)
  }
  if (!("round_number" %in% colnames(finished_rounds_df)) || !("begin_process" %in% colnames(finished_rounds_df))) {
    return(empty)
  }

  # Finished-only: when is_finished is present, keep strictly the finished
  # rounds (is_finished == TRUE). An unfinished round must never produce a
  # marker, even if a snapshot falls within its boundary window. NA is treated
  # as not-finished (fail closed).
  if ("is_finished" %in% colnames(finished_rounds_df)) {
    fin <- as.logical(finished_rounds_df$is_finished)
    finished_rounds_df <- finished_rounds_df[!is.na(fin) & fin, , drop = FALSE]
    if (nrow(finished_rounds_df) == 0) {
      return(empty)
    }
  }

  # Parse/sort valid round boundaries ascending.
  bp_str <- as.character(finished_rounds_df$begin_process)
  bp_clean <- gsub("T", " ", gsub("Z", "", gsub("\\..*", "", bp_str)))
  round_bp <- suppressWarnings(as.POSIXct(bp_clean, format = "%Y-%m-%d %H:%M:%S"))
  round_num <- as.numeric(finished_rounds_df$round_number)
  valid_round <- !is.na(round_bp)
  if (!any(valid_round)) {
    return(empty)
  }
  rounds <- data.frame(bp = round_bp[valid_round], round_number = round_num[valid_round], stringsAsFactors = FALSE)
  rounds <- rounds[order(rounds$bp, rounds$round_number), , drop = FALSE]

  # Parse snapshot dates and points.
  date_str <- as.character(history_df$recorded_at)
  clean_str <- gsub("T", " ", date_str)
  clean_str <- gsub("Z", "", clean_str)
  clean_str <- gsub("\\..*", "", clean_str)
  parsed <- suppressWarnings(as.POSIXct(clean_str, format = "%Y-%m-%d %H:%M:%S"))
  na_idx <- is.na(parsed)
  if (any(na_idx)) {
    parsed[na_idx] <- suppressWarnings(as.POSIXct(clean_str[na_idx], format = "%Y-%m-%d"))
  }
  pts <- suppressWarnings(as.numeric(history_df$points))

  # Valid snapshots: parseable date, finite points >= 0.
  valid_snap <- !is.na(parsed) & !is.na(pts) & is.finite(pts) & (pts >= 0)
  if (!any(valid_snap)) {
    return(empty)
  }
  snap_date <- parsed[valid_snap]
  snap_pts <- pts[valid_snap]
  n_snap <- length(snap_date)

  # Bucket each snapshot into the first completed round whose boundary window
  # it falls within: window for round i is (prior_boundary, boundary_i]. The
  # windows are disjoint, so a snapshot is used at most once (no reuse).
  used <- rep(FALSE, n_snap)
  trace_rows <- list()
  prev_bp <- -Inf
  for (i in seq_len(nrow(rounds))) {
    cur_bp <- rounds$bp[i]
    eligible <- which(!used & snap_date > prev_bp & snap_date <= cur_bp)
    prev_bp <- cur_bp
    if (length(eligible) == 0) next
    latest <- eligible[which.max(snap_date[eligible])]
    used[latest] <- TRUE
    trace_rows[[length(trace_rows) + 1]] <- data.frame(
      date = snap_date[latest],
      points = snap_pts[latest],
      round_number = rounds$round_number[i],
      stringsAsFactors = FALSE
    )
  }

  if (length(trace_rows) == 0) {
    return(empty)
  }
  points_df <- do.call(rbind, trace_rows)
  points_df <- points_df[order(points_df$date), , drop = FALSE]
  rownames(points_df) <- NULL

  list(points_df = points_df, has_points = TRUE)
}

# Convert stored final per-round observations into the card chart series.
player_match_points_trace <- function(rows) {
  empty <- list(points_df=data.frame(date=as.POSIXct(character(0)), points=numeric(), round_number=numeric(), stringsAsFactors=FALSE), has_points=FALSE)
  required <- c("round", "points")
  if (!is.data.frame(rows) || !nrow(rows) || !all(required %in% names(rows))) return(empty)
  final <- if ("score_status" %in% names(rows)) as.character(rows$score_status) == "final" else rep(TRUE, nrow(rows))
  points <- suppressWarnings(as.numeric(rows$points)); rounds <- suppressWarnings(as.numeric(rows$round))
  stamps <- if ("occurred_at" %in% names(rows)) as.character(rows$occurred_at) else rep(NA_character_,nrow(rows))
  if ("round_start_at" %in% names(rows)) stamps[is.na(stamps) | !nzchar(stamps)] <- as.character(rows$round_start_at[is.na(stamps) | !nzchar(stamps)])
  if ("observed_at" %in% names(rows)) stamps[is.na(stamps) | !nzchar(stamps)] <- as.character(rows$observed_at[is.na(stamps) | !nzchar(stamps)])
  stamp_clean <- gsub("Z$", "", gsub("T", " ", stamps))
  dates <- suppressWarnings(as.POSIXct(stamp_clean, tz="UTC"))
  keep <- final & is.finite(points) & is.finite(rounds) & !is.na(dates)
  if (!any(keep)) return(empty)
  out <- data.frame(date=dates[keep], points=points[keep], round_number=rounds[keep], stringsAsFactors=FALSE)
  out <- out[order(out$round_number,out$date),,drop=FALSE]
  out <- out[!duplicated(out$round_number,fromLast=TRUE),,drop=FALSE]
  list(points_df=out, has_points=nrow(out)>0L)
}

# Extract final round scores directly from the cached player-summary response.
# This fallback is used when daily snapshots have not yet been persisted.
player_summary_points_trace <- function(summary, finished_rounds_df = NULL) {
  empty <- list(points_df = data.frame(date=as.POSIXct(character(0)), points=numeric(), round_number=numeric(), stringsAsFactors=FALSE), has_points=FALSE)
  points <- if (is.list(summary)) summary$points else NULL
  if (is.null(points) || !length(points)) return(empty)
  if (is.data.frame(points)) points <- split(points, seq_len(nrow(points)))
  has_round_boundaries <- is.data.frame(finished_rounds_df) && nrow(finished_rounds_df) &&
    all(c("round_number", "begin_process") %in% names(finished_rounds_df))
  rounds <- if (has_round_boundaries) {
    finished <- if ("is_finished" %in% names(finished_rounds_df)) !is.na(as.logical(finished_rounds_df$is_finished)) & as.logical(finished_rounds_df$is_finished) else rep(FALSE, nrow(finished_rounds_df))
    finished_rounds_df[finished, , drop=FALSE]
  } else data.frame()
  current_round <- suppressWarnings(as.numeric(summary$match$r$number %||% NA_real_))
  dates <- if (nrow(rounds)) suppressWarnings(as.POSIXct(gsub("Z$", "", gsub("T", " ", as.character(rounds$begin_process))), tz="UTC")) else as.POSIXct(character(0))
  out <- lapply(as.list(points), function(point) {
    round <- suppressWarnings(as.numeric(point$round)); score <- suppressWarnings(as.numeric(point$points))
    if (!is.finite(round) || !is.finite(score)) return(NULL)
    idx <- if (nrow(rounds)) which(rounds$round_number == round) else integer()
    if (nrow(rounds) && length(idx) != 1L) return(NULL)
    if (!nrow(rounds) && is.finite(current_round) && round >= current_round) return(NULL)
    # The points plot is indexed by round. Keep a deterministic timestamp for
    # trace consumers that still expect a date column.
    date <- if (length(idx) == 1L) dates[idx] else as.POSIXct("1970-01-01", tz="UTC") + round * 86400
    data.frame(date=date, points=score, round_number=round, stringsAsFactors=FALSE)
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(empty)
  frame <- do.call(rbind, out)
  frame <- frame[order(frame$round_number), , drop=FALSE]
  list(points_df=frame, has_points=TRUE)
}

# Return the latest completed-round point observations, newest first.
latest_player_round_points <- function(history_df,finished_rounds_df,limit=5L) {
  trace <- build_player_points_trace(history_df,finished_rounds_df)
  limit <- suppressWarnings(as.integer(limit)[1])
  if (!isTRUE(trace$has_points) || !is.finite(limit) || limit<1L)
    return(trace$points_df[0,,drop=FALSE])
  points <- tail(trace$points_df,limit)
  points[order(points$round_number,decreasing=TRUE,na.last=TRUE),,drop=FALSE]
}

# Explain an empty completed-round breakdown without hiding a known aggregate total.
recent_round_points_empty_text <- function(aggregate_points) {
  total <- suppressWarnings(as.numeric(as.character(aggregate_points)[1]))
  if (length(total) == 1L && is.finite(total) && total > 0) {
    paste0("Total points: ", format(total, trim=TRUE, scientific=FALSE),
      ". Completed-round breakdown is unavailable.")
  } else {
    "No completed-round points recorded yet"
  }
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
# Preserve the selected player's score and shape; calculate only when absent.
selected_player_fis_row <- function(player) {
  if (is.null(player)) return(NULL)
  row <- if (is.data.frame(player)) player[1, , drop = FALSE] else as.data.frame(player, stringsAsFactors = FALSE)
  if (nrow(row) == 0L) return(NULL)
  if (!"fis_score" %in% names(row)) row <- calculate_fis_score(row)
  for (field in c("perf", "form", "efficiency", "momentum", "fixture_risk", "data_coverage")) if (!field %in% names(row)) row[[field]] <- NA_real_
  for (field in c("fis_tier", "fis_summary")) if (!field %in% names(row)) row[[field]] <- ""
  row
}
