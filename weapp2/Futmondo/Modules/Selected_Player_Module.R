library(reactable)
if(!exists("assemble_player_card_history",mode="function"))source("history_repository.R")

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

# The first valid player context follows the loading shell and must not dismiss
# it. Once a player context has existed, any later change invalidates its modal.
player_card_context_changed <- function(previous_context, current_context) {
  !is.null(previous_context) && !identical(previous_context, current_context)
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
    # Delay widget construction until actual identity content has painted. The
    # event fires before the value applies, so two animation frames follow it.
    tags$script(HTML(paste0(
      "(function(){var sent=false,id='",ns("player_identity_summary"),"';",
      "function painted(){if(sent)return;var root=document.getElementById(id);",
      "var identity=root&&root.querySelector('.player-card-identity');if(!identity)return;",
      "sent=true;var player=identity.getAttribute('data-player-id'),generation=identity.getAttribute('data-paint-generation');",
      "requestAnimationFrame(function(){requestAnimationFrame(function(){",
      "Shiny.setInputValue('",ns("identity_painted"),"',{player:player,generation:generation,nonce:Math.random()},{priority:'event'});});});}",
      "$(document).off('shiny:value.fmCardPaint",gsub("[^a-zA-Z0-9]","",ns("")),"')",
      ".on('shiny:value.fmCardPaint",gsub("[^a-zA-Z0-9]","",ns("")),"',function(event){if(event.name===id)setTimeout(painted,0);});",
      "setTimeout(painted,0);})();"
    ))),
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
                     selected = "valuation", inline = TRUE),
                   actionButton(ns("refresh_history"), "Refresh history", icon=icon("rotate"))
                 ),
                 uiOutput(ns("player_chart_container"))
          )
        ),
        # Real-competition profile, price history, and completed match metadata.
        fluidRow(
          style = "margin-top: 20px; padding-top: 15px; border-top: 1px solid #f1f5f9;",
          column(12, uiOutput(ns("player_external_details")))
        )
      )
    )
  )
}


# ---- Pure helper: market-offer open decision ----
# Single decision point shared by the "Make Market Offer" button and the
# external "market_bid" action event from another player view.
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


selected_player_Server <- function(id, selected_player, login_token = NULL, championship_id = NULL, user_team_id = NULL, on_bid_updated = NULL, open_action = NULL, capacity_fetcher = NULL, refresh_trigger = NULL) {
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
    previous_player_action_context_RV <- reactiveVal(NULL)
    observeEvent(player_action_context_RV(), {
      current_context <- player_action_context_RV()
      previous_context <- isolate(previous_player_action_context_RV())
      active_bid_info_RV(NULL);modal_context_RV(list())
      offer_modal_opened_RV(FALSE);clause_modal_opened_RV(FALSE);modify_modal_opened_RV(FALSE)
      if (player_card_context_changed(previous_context, current_context)) removeModal()
      previous_player_action_context_RV(current_context)
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
      if (is.null(sp)) {
        return(div(class = "player-card-loading",
          icon("spinner", class = "fa-spin"), span("Loading player details…")))
      }
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
      resource_timing("card_identity",isolate(card_started()))
      location <- player_card_location_label(sp, get_reactive_val(user_team_id))
      div(class = "player-card-identity", `data-player-id`=as.character(sp$id),`data-paint-generation`=as.character(card_paint_generation()),
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
        list(selected_player(),card_history(),card_states$capacity(),card_states$listings())
      },
      {
        sp <- selected_player()
        req(sp)

        login <- get_reactive_val(login_token)
        champ_id <- get_reactive_val(championship_id)
        team_id <- get_reactive_val(user_team_id)

        my_bid_id <- NULL
        my_bid_price <- NULL

        if (!is.null(login) && !is.null(champ_id) && !is.null(team_id) && !is.null(sp$id)) {
          summary_res <- card_history()$summary

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
            my_mkt_players <- card_states$listings()$data
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
          bid_gate <- passive_acquisition_gate(sp,"bid")
          clause_gate <- passive_acquisition_gate(sp,"clause")
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

        readiness<-if(is_own_player)card_states$listings() else card_states$capacity()
        if(!is_own_player && identical(card_states$summary()$status,"loading"))readiness<-resource_state(status="loading")
        if(!is_own_player && identical(card_states$summary()$status,"unavailable"))readiness<-card_states$summary()
        if(identical(readiness$status,"loading")) {
          disable<-function(tag){
            if(inherits(tag,"shiny.tag")){
              if(identical(tag$name,"button"))tag$attribs$disabled<-"disabled"
              tag$children<-lapply(tag$children,disable)
            } else if(is.list(tag))tag<-lapply(tag,disable)
            tag
          }
          action_buttons<-tagList(div(role="status",icon("spinner",class="fa-spin")," Checking availability…"),disable(action_buttons))
        } else if(is.null(readiness$data)) {
          action_buttons<-tagList(div(class="acquisition-capacity-warning",resource_failure_message(readiness$error)))
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
        invalidate_manual_resources(login,champ_id,team_id,sp$id)
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
        invalidate_manual_resources(login,champ_id,team_id,sp$id)
      } else {
        shiny::showNotification(
          "Failed to cancel bid. Please try again.",
          type = "error",
          duration = 5
        )
      }
    })

    # ---- Shared helper: open the regular market-offer modal ----
    # Single code path for the "Make Market Offer" button and for external
    # "market_bid" action events from another player view.
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
        invalidate_manual_resources(login,champ_id,team_id,sp$id)
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

      default_offer <- if ("value" %in% colnames(sp) && !is.na(sp$value) && sp$value > 0) sp$value else NA_real_
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
        invalidate_manual_resources(login,champ_id,team_id,sp$id)
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
        invalidate_manual_resources(login,champ_id,team_id,sp$id)
        if (!is.null(on_bid_updated) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(action_type = "clause_paid"), error = function(e) NULL)
        }
      } else {
        if(is.list(res)&&identical(res$code,"api.market.max_number_players_in_roster")) invalidate_manual_resources(login,champ_id,team_id,sp$id)
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
      default_price <- if ("value" %in% colnames(sp) && !is.na(sp$value) && sp$value > 0) sp$value else NA_real_

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
        invalidate_manual_resources(login,champ_id,team_id,sp$id)
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
        invalidate_manual_resources(login,champ_id,team_id,sp$id)
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
        invalidate_manual_resources(login,champ_id,team_id,sp$id)
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
        invalidate_manual_resources(login,champ_id,team_id,sp$id)
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
        invalidate_manual_resources(login,champ_id,team_id,sp$id)
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

    history_refresh <- reactive(paste(if(is.function(refresh_trigger))refresh_trigger()else 0L,input$refresh_history %||% 0L,sep=":"))
    observeEvent(input$refresh_history,{
      sp<-selected_player();auth<-get_reactive_val(login_token)
      req(sp,valid_login(auth))
      base<-resource_context(auth,get_reactive_val(championship_id),get_reactive_val(user_team_id),sp$id)
      invalidate_resources(base,pattern="^player_summary_|^player_card_history$")
      invalidate_resources(resource_context(auth,base$championship_id,base$user_team_id),pattern="^lineup_")
      invalidate_resources(resource_context(auth,player_id=sp$id),pattern="^player_matches_")
    },ignoreInit=TRUE,priority=120)
    card_context <- reactive({
      sp<-selected_player();if(is.null(sp)||!valid_login(get_reactive_val(login_token)))return(NULL)
      resource_context(get_reactive_val(login_token),get_reactive_val(championship_id),get_reactive_val(user_team_id),sp$id,session_id=session$token)
    })
    history_context <- reactive({
      ctx<-card_context();if(is.null(ctx))return(NULL)
      summary<-card_states$summary()$data;lineup<-card_states$lineup()$data
      ctx$season<-league_season_context(ctx$championship_id,summary$championship$season %||% summary$championship$seasonId)
      ctx$scoring_version<-normalize_league_rules(list(),lineup,ctx$championship_id)$scoring_version
      ctx$competition_id<-fm_scalar(summary$championship$competitionId %||% summary$championship$competition$id,"")
      ctx
    })
    card_read <- function(resource,handler,args,context=card_context,active=function()TRUE,refresh=history_refresh) {
      shared_resource(session,resource,context=context,
        active=function()!is.null(card_context())&&isTRUE(active()),refresh=refresh,
        fetch=function(ctx) {
          captured<-args(ctx)
          async_adapter_fetch(handler,captured,context=ctx,is_current=function()shiny::isolate({
            current<-context();!is.null(current)&&identical(current,ctx)&&isTRUE(active())
          }))
        })
    }
    auth_args<-function(ctx)list(login=get_reactive_val(login_token),championship_id=ctx$championship_id,user_team_id=ctx$user_team_id)
    player_args<-function(ctx)c(auth_args(ctx),list(player_id=ctx$player_id))
    global_refresh<-function()if(is.function(refresh_trigger))refresh_trigger()else 0L
    card_states<-list()
    card_states$summary<-card_read("card_summary","get_player_summary",player_args)
    card_states$lineup<-card_read("card_lineup","get_lineup_from_team",auth_args)
    card_states$profile<-card_read("card_profile","get_player_full_profile",function(ctx)list(login=get_reactive_val(login_token),player_id=ctx$player_id),refresh=global_refresh)
    card_states$matches<-card_read("card_matches","get_player_matches",function(ctx)list(login=get_reactive_val(login_token),player_id=ctx$player_id))
    context_ready<-function()!identical(card_states$summary()$status,"loading")&&!identical(card_states$lineup()$status,"loading")
    card_states$points<-card_read("card_points","read_card_round_history",function(ctx)list(context=ctx),context=history_context,active=context_ready)
    card_states$valuation<-card_read("card_valuation","read_card_price_history",function(ctx)list(context=ctx))
    card_states$capacity<-card_read("card_capacity","get_acquisition_capacity",function(ctx)c(auth_args(ctx),list(target_player_id=ctx$player_id)),
      active=function()!player_card_is_own_player(selected_player(),get_reactive_val(user_team_id)),refresh=global_refresh)
    card_states$listings<-card_read("card_listings","get_my_market_players",auth_args,
      active=function()player_card_is_own_player(selected_player(),get_reactive_val(user_team_id)),refresh=global_refresh)
    passive_acquisition_gate<-function(sp,mode){
      state<-card_states$capacity()
      if(identical(state$status,"loading"))return(list(ok=FALSE,reason="loading",message="Checking availability…"))
      evaluate_acquisition_preflight(state$data,mode=mode,minimum_bid=if(mode=="bid")market_bid_minimum(sp)else NULL)
    }
    card_started<-reactiveVal(Sys.time());timed_summary<-FALSE;timed_history<-FALSE
    card_chart_ready<-reactiveVal(FALSE)
    card_paint_generation<-reactiveVal(0L)
    observeEvent(card_context(),{
      card_started(Sys.time());timed_summary<<-FALSE;timed_history<<-FALSE;card_chart_ready(FALSE);card_paint_generation(isolate(card_paint_generation())+1L)

    },ignoreNULL=FALSE,priority=100)
    observe({
      req(!is.null(card_context()))
      if(!timed_summary&&!identical(card_states$summary()$status,"loading")){timed_summary<<-TRUE;resource_timing("card_summary",isolate(card_started()))}
      states<-lapply(card_states[c("summary","lineup","points","valuation")],function(read)read())
      if(!timed_history&&all(vapply(states,function(state)!identical(state$status,"loading"),logical(1)))){timed_history<<-TRUE;resource_timing("card_history",isolate(card_started()))}
    })
    card_history <- reactive({
      ctx<-history_context()
      if(is.null(ctx))return(list(summary=NULL,points=data.frame(),valuation=data.frame(),status="loading"))
      result<-assemble_player_card_history(card_states$summary()$data,card_states$lineup()$data,
        card_states$points()$data,card_states$valuation()$data,ctx)
      states<-lapply(card_states[c("summary","lineup","points","valuation")],function(read)read())
      if(any(vapply(states,function(state)identical(state$status,"loading"),logical(1))))result$status<-"loading"
      result
    })
    observe({
      history<-card_history()
      if(isTRUE(context_ready())&&!identical(card_states$points()$status,"loading")&&is.list(history$context)&&exists("record_acquired_resource",mode="function")){
        tryCatch(record_acquired_resource("player_card_history",history,history$context),error=function(e)NULL)
      }
    })
    observeEvent(input$identity_painted,{
      sp<-selected_player()
      if(!is.null(sp)&&identical(fm_scalar(input$identity_painted$player,""),as.character(sp$id))&&
         identical(fm_scalar(input$identity_painted$generation,""),as.character(card_paint_generation())))card_chart_ready(TRUE)
    })
    output$player_chart_container <- renderUI({
      req(selected_player())
      if(!isTRUE(card_chart_ready()))return(div(class="player-card-loading",role="status","Loading chart…"))
      plotly::plotlyOutput(ns("player_trend_plot"),height="280px")
    })
    output$player_trend_plot <- plotly::renderPlotly({
      req(card_chart_ready())
      sp <- selected_player(); req(sp); history <- card_history()
      metric <- if (identical(input$player_chart_metric,"points")) "points" else "valuation"
      empty_chart <- function(message) plotly::plot_ly() %>% fm_plot_layout(
        paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
        xaxis=list(visible=FALSE),yaxis=list(visible=FALSE),
        annotations=list(list(text=message,x=.5,y=.5,xref="paper",yref="paper",showarrow=FALSE)))
      if (identical(history$status,"loading") && !nrow(history$points) && !nrow(history$valuation)) return(empty_chart("Loading player history…"))
      if (metric=="points") {
        points <- history$points
        if (!is.data.frame(points) || !all(c("round_number","points") %in% names(points))) points <- data.frame()
        if (nrow(points)) {
          points$round_number <- suppressWarnings(as.numeric(points$round_number));points$points <- suppressWarnings(as.numeric(points$points))
          points <- points[is.finite(points$round_number)&is.finite(points$points),,drop=FALSE]
        }
        if (!nrow(points)) return(empty_chart(recent_round_points_empty_text(sp$points)))
        points <- points[order(points$round_number),,drop=FALSE]
        points$label <- paste0("Round ",points$round_number,"<br>Points: ",points$points,
          if ("score_status" %in% names(points)) paste0("<br>",points$score_status) else "")
        return(plotly::plot_ly(points) %>% plotly::add_trace(type="scatter",mode="lines+markers",
          x=~round_number,y=~points,text=~label,hoverinfo="text",name="Round points",
          line=list(color="#10b981",width=2.5),marker=list(size=8)) %>%
          fm_plot_layout(paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
            xaxis=list(title="Round",dtick=1),yaxis=list(title="Points",autorange=FALSE,
              range=player_trend_axis_range(points$points)),showlegend=FALSE))
      }
      values <- history$valuation
      if (!is.data.frame(values) || !all(c("recorded_at","value") %in% names(values))) values <- data.frame()
      if (nrow(values)) {
        values$date <- fm_time(values$recorded_at);values$value <- suppressWarnings(as.numeric(values$value))
        values <- values[!is.na(values$date)&is.finite(values$value),,drop=FALSE]
      }
      if (!nrow(values)) return(empty_chart(paste0("Valuation history unavailable. Current value: ",player_card_money(sp$value))))
      values <- values[order(values$date),,drop=FALSE]
      plotly::plot_ly(values) %>% plotly::add_trace(type="scatter",mode="lines+markers",x=~date,y=~value,
        name="Observed value",line=list(color="#00ff88",width=2.5)) %>%
        fm_plot_layout(paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
          xaxis=list(title="Date"),yaxis=list(title="Value (EUR)",autorange=FALSE,
            range=player_trend_axis_range(values$value)),showlegend=FALSE)
    })
    render_match <- function(match) {
      score<-if(is.finite(match$home_score)&&is.finite(match$away_score))paste0(format(match$home_score,trim=TRUE),"–",format(match$away_score,trim=TRUE))else "Score unavailable"
      date<-if(nzchar(match$occurred_at))sub("T.*","",match$occurred_at)else "Date unavailable"
      div(class="player-round-fixture",strong(paste(match$home,score,match$away)),span(class="player-match-date",date))
    }
    output$recent_round_points <- renderUI({
      sp<-selected_player();req(sp);history<-card_history();match_state<-card_states$matches()
      matches<-normalize_player_matches(match_state$data)
      rounds<-build_player_recent_rounds(history$points,matches,history$context %||% list(),current_player_responses=TRUE)
      div(class="player-recent-rounds",h4(icon("futbol")," Recent rounds"),
        div(class="player-round-boxes",lapply(rounds$rounds,function(round){
          div(class="player-match-card player-round-box",
            div(class="player-round-heading",span(paste("Round",round$round_number)),
              strong(if(is.finite(round$points))paste0(round$points," pts")else "Points unavailable")),
            if(nrow(round$fixtures))lapply(seq_len(nrow(round$fixtures)),function(i)render_match(round$fixtures[i,,drop=FALSE]))else
              span(class="player-match-date",if(identical(match_state$status,"loading"))"Loading match details…"else "Match association unavailable"))
        })),
        if(!length(rounds$rounds))span(class="player-recent-rounds-empty",if(identical(history$status,"loading"))"Loading round points…"else recent_round_points_empty_text(sp$points)),
        if(is.null(card_states$summary()$data)&&!identical(card_states$summary()$status,"loading"))span(class="player-history-status",resource_failure_message(card_states$summary()$error)),
        if(nrow(rounds$unassociated))tagList(
          p(class="player-history-status","Match records · points association unavailable"),
          div(class="player-round-boxes",lapply(seq_len(nrow(rounds$unassociated)),function(i){
            match<-rounds$unassociated[i,,drop=FALSE]
            div(class="player-match-card player-round-box",span(class="player-match-round",paste("Round",match$round)),render_match(match))
          }))),
        if(is.null(match_state$data)&&!identical(match_state$status,"loading"))span(class="player-history-status",resource_failure_message(match_state$error)),
        if(length(rounds$rounds))tags$details(class="player-points-explanation",
          tags$summary("About these points"),p("Points are reported by Futmondo. Final confirmation is not provided for some rounds.")),
        if(identical(history$freshness,"stale"))span(class="player-history-status","Showing saved history; refresh failed."))
    })
    output$player_external_details <- renderUI({
      req(selected_player());state<-card_states$profile();answer<-state$data
      if(is.null(answer))return(div(class="player-external-details",h4(icon("address-card")," Player profile"),
        span(if(identical(state$status,"loading"))"Loading profile…"else resource_failure_message(state$error))))
      data<-normalize_player_full_profile(answer);profile<-data$profile;bits<-character()
      if(nzchar(profile$nationality))bits<-c(bits,profile$nationality)
      if(is.finite(profile$height_cm)&&profile$height_cm>0)bits<-c(bits,paste0(format(profile$height_cm,trim=TRUE)," cm"))
      if(is.finite(profile$weight_kg)&&profile$weight_kg>0)bits<-c(bits,paste0(format(profile$weight_kg,trim=TRUE)," kg"))
      if(nzchar(profile$preferred_foot))bits<-c(bits,profile$preferred_foot)
      prices<-data$price_history;latest<-if(nrow(prices))tail(prices,1L)else NULL
      div(class="player-external-details",h4(icon("address-card")," Player profile"),
        div(class="player-profile-summary",if(length(bits))span(paste(bits,collapse=" · ")),
          span(class="player-profile-price",if(!is.null(latest)&&is.finite(latest$social[1]))
            paste0("Profile valuation: ",player_card_money(latest$social[1])," · ",nrow(prices)," saved values")else "Profile valuation history is unavailable.")))
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
  dates <- fm_time(stamps)
  keep <- !is.na(final) & final & is.finite(points) & is.finite(rounds)
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
  dates <- if (nrow(rounds)) fm_time(rounds$begin_process) else as.POSIXct(character(0))
  out <- lapply(as.list(points), function(point) {
    round <- suppressWarnings(as.numeric(point$round)); score <- suppressWarnings(as.numeric(point$points))
    if (!is.finite(round) || !is.finite(score)) return(NULL)
    idx <- if (nrow(rounds)) which(rounds$round_number == round) else integer()
    if (nrow(rounds) && length(idx) != 1L) return(NULL)
    if (!nrow(rounds) && is.finite(current_round) && round >= current_round) return(NULL)
    # The points plot is indexed by round. Keep a deterministic timestamp for
    # trace consumers that still expect a date column.
    date <- if (length(idx) == 1L && !is.na(dates[idx])) dates[idx] else as.POSIXct(NA_character_, tz="UTC")
    data.frame(date=date, points=score, round_number=round, stringsAsFactors=FALSE)
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(empty)
  frame <- do.call(rbind, out)
  frame <- frame[order(frame$round_number), , drop=FALSE]
  list(points_df=frame, has_points=TRUE)
}

# Return the latest completed-round point observations, newest first.

# Explain an empty completed-round breakdown without hiding a known aggregate total.
recent_round_points_empty_text <- function(aggregate_points) {
  total <- suppressWarnings(as.numeric(as.character(aggregate_points)[1]))
  if (length(total) == 1L && is.finite(total)) {
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
