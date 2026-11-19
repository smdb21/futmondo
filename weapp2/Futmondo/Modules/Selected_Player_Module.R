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

# Translate offer failures without changing submission or retry behavior.
offer_error_message <- function(result) {
 text<-function(x)if(is.character(x)&&length(x)==1L&&!is.na(x))trimws(x)else ""
 code<-if(is.list(result))text(result$code)else ""
 message<-if(is.list(result))text(result$message)else ""
 if(identical(code,"api.market.max_number_players_in_roster")||identical(message,"api.market.max_number_players_in_roster"))
  return("Your squad has reached the league's maximum size. Pending incoming players can also count toward this limit. Complete a sale to free a place, then refresh before making another offer.")
 if(nzchar(message)&&!grepl("^api[.]",message)&&!identical(code,"error"))return(message)
 if(grepl("^api[.]",code)||grepl("^api[.]",message))
  return("LigaRadar rejected this offer. Refresh the player's details and check the league's buying rules before trying again.")
 "The offer could not be confirmed. Refresh and check your active offers before trying again, so you do not submit it twice."
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

# Direct bid fields in the account/team-scoped market response describe our
# offer. Selected player rows can contain rival/received bids and are not used.
player_card_active_bid <- function(summary, capacity, player_id, now = Sys.time()) {
  valid_bid <- function(id, price) {
    id <- fm_scalar(id, ""); price <- fm_number(price)
    if (!nzchar(id) || !is.finite(price) || price <= 0) return(NULL)
    list(id=id, price=price)
  }
  deadline <- capacity$own_bid_valid_until %||% capacity$valid_until
  rows <- capacity$target_rows
  if (!acquisition_data_unverified(capacity) && is.numeric(deadline) &&
      length(deadline)==1L && is.finite(deadline) && deadline>as.numeric(now) &&
      is.data.frame(rows) && "id" %in% names(rows)) {
    rows <- rows[!is.na(rows$id) & as.character(rows$id)==fm_scalar(player_id,""),,drop=FALSE]
    if (nrow(rows)) {
      if (nrow(rows)!=1L) return(NULL)
      return(valid_bid(rows$bid_id, rows$bid_price))
    }
  }
  # Off-market owner offers may instead have immutable own-team evidence in
  # the summary. Its age and status must remain independently verifiable.
  if (is.list(summary) && !acquisition_data_unverified(summary) &&
      acquisition_source_deadline(summary,60,now)>as.numeric(now))
    return(valid_bid(summary$my_bid_id, summary$my_bid_price))
  NULL
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
  paste0("Ownership: ",status$ownership," · ",status$market)
}

# Recent table observations already establish ownership and market state.
# Unknown timestamps/flags never become evidence of a free agent or listing.
player_card_recent_table_facts <- function(player,now=Sys.time()) {
  if(!is.data.frame(player)||nrow(player)!=1L||acquisition_data_unverified(player)||
     !"id"%in%names(player)||!nzchar(fm_scalar(player$id,""))||
     !any(c("owner_team_id","user_team_id","userteamId","userTeamId","userTeam._id","userTeam.id","userteam._id","userteam.id")%in%names(player))||
     !"market_inMarket"%in%names(player)||is.na(player$market_inMarket[1]))return(FALSE)
  owner_fields<-intersect(c("owner_team_id","user_team_id","userteamId","userTeamId","userTeam._id","userTeam.id","userteam._id","userteam.id"),names(player))
  if(!any(vapply(owner_fields,function(field)!is.na(player[[field]][1]),logical(1)))||
     !tolower(as.character(player$market_inMarket[1]))%in%c("true","false")||
     identical(attr(player,"availability_complete"),FALSE))return(FALSE)
  if("computer"%in%names(player)&&isFALSE(suppressWarnings(as.logical(player$computer[1])))&&!nzchar(player_card_owner_id(player)))return(FALSE)
  stamp<-fm_time(if("observed_at"%in%names(player))player$observed_at[1]else attr(player,"observed_at"))
  market_stamp<-fm_time(attr(player,"availability_market_observed_at"))
  if(!is.null(attr(player,"availability_complete"))&&(length(market_stamp)!=1L||is.na(market_stamp)))return(FALSE)
  if(length(stamp)!=1L||is.na(stamp)||stamp>now||(length(market_stamp)==1L&&!is.na(market_stamp)&&market_stamp>now))return(FALSE)
  owner_deadline<-attr(player,"availability_owner_valid_until") %||% player_availability_deadline(player,"player_summary")
  market_deadline<-attr(player,"availability_market_valid_until") %||%
    if(length(market_stamp)==1L&&!is.na(market_stamp))as.numeric(market_stamp)+resource_policy("market")$ttl else owner_deadline
  deadlines<-c(owner_deadline,market_deadline)
  length(deadlines)==2L&&all(is.finite(deadlines))&&all(as.numeric(now)<deadlines)
}

# Project only evidenced target facts from the existing official summary.
# An absent owner/market contract never turns into a free agent or no listing.
player_card_summary_row <- function(player,summary,observed=FALSE) {
  eligible<-if(isTRUE(observed))player_availability_observed(summary,"summary")else !acquisition_data_unverified(summary)
  if(!is.data.frame(player)||nrow(player)!=1L||!is.list(summary)||!eligible||
     !identical(fm_scalar(summary$data$id %||% summary$data$`_id`,""),fm_scalar(player$id,"")))return(NULL)
  owner<-summary$championship$owner;market<-summary$market
  # Futmondo omits championship.owner for some System players (including
  # Dani Lorenzo). data.computer=TRUE is explicit ownership evidence.
  if(is.null(owner)&&isTRUE(summary$data$computer))owner<-list(computer=TRUE)
  if(!is.list(owner)||!is.list(market))return(NULL)
  owner_id<-fm_scalar(owner$`_id` %||% owner$id,"")
  if(!nzchar(owner_id)&&!isTRUE(owner$computer))return(NULL)
  listing_fields<-c("pr","p","vom","pom")
  listed<-any(listing_fields%in%names(market));price<-if(listed)fm_number(market$p)else NA_real_
  if((listed&&(!is.finite(price)||price<=0))||(!listed&&!"bids"%in%names(market)))return(NULL)
  if(isTRUE(owner$computer))owner_id<-""
  owner_name<-if(nzchar(owner_id))fm_scalar(owner$name,"")else ""
  for(field in intersect(c("owner_team_id","user_team_id","userteamId","userTeamId","userTeam._id","userTeam.id","userteam._id","userteam.id"),names(player)))player[[field]]<-owner_id
  player$user_team_id<-owner_id;player$owner_team_id<-owner_id
  for(field in c("userTeam","user_team_name","teamname"))player[[field]]<-owner_name
  player$market_inMarket<-listed
  for(field in c("effective_market_price","market_price","price"))player[[field]]<-price
  clause<-summary$championship$clause
  player$clause_price<-fm_number(clause$price)
  player$clause_transferred<-if(is.logical(clause$transferred)&&length(clause$transferred)==1L)clause$transferred else NA
  player$clause_date<-fm_scalar(clause$date,NA_character_)
  player$clause_transferDate<-fm_scalar(clause$transferDate,NA_character_)
  stamp<-attr(summary,"observed_at")
  # Clear source-row clocks from the older table projection; provenance now
  # belongs to this matching summary. Do not renew the summary's deadline.
  if("observed_at"%in%names(player))player$observed_at<-stamp
  player<-observed_data(player,status=attr(summary,"fetch_status"),observed_at=stamp)
  attr(player,"fetch_error")<-NULL
  attr(player,"valid_until")<-player_availability_deadline(summary,"player_summary")
  attr(player,"availability_owner_valid_until")<-player_availability_deadline(summary,"player_summary")
  attr(player,"availability_market_valid_until")<-attr(player,"availability_owner_valid_until")
  attr(player,"availability_market_observed_at")<-stamp
  attr(player,"availability_complete")<-TRUE
  player
}


selected_player_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # renderUI can insert HTML asynchronously after shiny:value. Observe only
    # the identity output and acknowledge each painted generation, including
    # replacements after reopening; a one-shot event can strand the chart.
    tags$script(HTML(paste0(
      "(function(){var id='",ns("player_identity_summary"),"',key='fmCardPaint",gsub("[^a-zA-Z0-9]","",ns("")),"';",
      "var previous=$.data(document,key);if(previous)previous();",
      "var sent='',scheduled=false,disposed=false,root=null,observer=null;",
      "$.data(document,key,function(){disposed=true;if(observer)observer.disconnect();});",
      "function identityKey(identity){return JSON.stringify([identity.getAttribute('data-player-id'),identity.getAttribute('data-paint-generation')]);}",
      "function painted(){if(disposed)return;var current=document.getElementById(id);",
      "if(current!==root){if(observer)observer.disconnect();root=current;",
      "if(root){observer=new MutationObserver(painted);observer.observe(root,{childList:true,subtree:true,attributes:true,attributeFilter:['data-player-id','data-paint-generation']});}}",
      "var identity=root&&root.querySelector('.player-card-identity');if(!identity||scheduled)return;",
      "var signature=identityKey(identity);if(signature===sent)return;scheduled=true;",
      "requestAnimationFrame(function(){requestAnimationFrame(function(){",
      "scheduled=false;if(disposed)return;",
      "if(!identity.isConnected||document.getElementById(id)!==root||root.querySelector('.player-card-identity')!==identity||identityKey(identity)!==signature){painted();return;}",
      "Shiny.setInputValue('",ns("identity_painted"),"',{player:identity.getAttribute('data-player-id'),generation:identity.getAttribute('data-paint-generation'),nonce:Math.random()},{priority:'event'});sent=signature;});});}",
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
                   actionButton(ns("refresh_history"), "Refresh history", icon=icon("rotate"))
                 ),
                 uiOutput(ns("player_chart_info")),
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


selected_player_Server <- function(id, selected_player, login_token = NULL, championship_id = NULL, user_team_id = NULL, on_bid_updated = NULL, open_action = NULL, capacity_fetcher = NULL, refresh_trigger = NULL, allow_scheduled_clauses = FALSE) {
  # Keep the caller's reactive lazy: parent tables define it after this module.
  player_source <- function() selected_player()
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    raw_selected_player <- player_source
    selected_player <- reactive({
      row <- raw_selected_player()
      if (is.data.frame(row) && nrow(row) == 1L) {
        ctx<-card_context();if(is.null(ctx))return(row)
        scope<-player_availability_scope(ctx,"summary");if(is.null(scope))return(row)
        identity<-list(scope=scope,refresh=global_refresh(),
          invalidated=player_availability_invalidated(ctx,"summary"))
        owner_fields<-c("owner_team_id","user_team_id","userteamId","userTeamId","userTeam._id","userTeam.id","userteam._id","userteam.id")
        if(!any(owner_fields%in%names(row))&&!isTRUE(row$computer))attr(row,"availability_complete")<-FALSE
        row$user_team_id <- player_card_owner_id(row)
        state<-card_states$summary()
        previous<-player_availability_remember_target(session,identity)
        latest<-function(value){
          stamps<-c(fm_time(attr(value,"observed_at") %||% if(is.data.frame(value)&&"observed_at"%in%names(value))value$observed_at[1]else NULL),fm_time(attr(value,"availability_market_observed_at")))
          stamps<-stamps[!is.na(stamps)];if(length(stamps))max(as.numeric(stamps))else -Inf
        }
        baseline<-if(player_card_recent_table_facts(row))row else NULL
        if(!is.null(previous)&&(is.null(baseline)||latest(previous)>latest(baseline)))baseline<-previous
        saved<-isTRUE(availability_mode())&&player_availability_observed(state$data,"summary")
        updated<-if(saved||isTRUE(state$session_verified))player_card_summary_row(row,state$data,observed=saved)else NULL
        if(!is.null(updated)){
          stamp<-fm_time(attr(updated,"observed_at"))
          if(length(stamp)==1L&&!is.na(stamp)&&as.numeric(stamp)>latest(baseline))
            return(player_availability_remember_target(session,identity,updated))
        }
        if(!is.null(baseline))return(player_availability_remember_target(session,identity,baseline))
      }
      row
    })
    player_target_ready<-function(player=selected_player())isTRUE(attr(player,"availability_session_verified"))||player_card_recent_table_facts(player)
    # Display may use an older successful database observation; an explicitly
    # opened editor must establish current target facts before enabling Submit.
    player_target_current<-function(player=selected_player())if(isTRUE(availability_mode()))player_card_recent_table_facts(player)else player_target_ready(player)

    active_bid_info_RV <- reactiveVal(NULL)
    modal_bid_info_RV <- reactiveVal(list())


    # Includes the immediately shown verification shell. It cannot submit until
    # the shared capacity read verifies this exact opening's context.
    offer_modal_opened_RV <- reactiveVal(FALSE)
    market_offer_dialog_RV <- reactiveVal(NULL)
    market_offer_generation <- 0L

    # Records whether the release-clause buyout confirmation modal was opened
    # by the shared helper (TRUE) or blocked by the open-state recheck /
    # preflight / missing player (FALSE). Exposed so the "clause_buyout"
    # open_action routing can be asserted in tests without a live UI.
    clause_modal_opened_RV <- reactiveVal(FALSE)
    clause_diagnostic_RV <- reactiveVal(FALSE)
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
      sp <- tryCatch(raw_selected_player(),error=function(e)NULL)
      if(!valid_login(auth) || is.null(sp) || !nzchar(fm_scalar(sp$id,""))) return(NULL)
      league_context_revision()
      ctx<-resource_context(auth,get_reactive_val(championship_id),get_reactive_val(user_team_id),sp$id)
      list(user=ctx$account_id,championship=ctx$championship_id,team=ctx$user_team_id,player=ctx$player_id,
        competition=ctx$competition_id,season=ctx$season,scoring=ctx$scoring_version)
    })
    previous_player_action_context_RV <- reactiveVal(NULL)
    observeEvent(player_action_context_RV(), {
      current_context <- player_action_context_RV()
      previous_context <- isolate(previous_player_action_context_RV())
      active_bid_info_RV(NULL);modal_context_RV(list());modal_bid_info_RV(list())
      market_offer_dialog_RV(NULL)
      clause_diagnostic_RV(FALSE)
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

    # Re-read the team-scoped source for manual actions. Never write using a
    # replacement offer or an amount that changed after confirmation opened.
    verify_active_bid <- function(sp, expected = NULL) {
      login <- get_reactive_val(login_token)
      champ <- get_reactive_val(championship_id)
      team <- get_reactive_val(user_team_id)
      req(valid_login(login),nzchar(fm_scalar(champ,"")),nzchar(fm_scalar(team,"")))
      ctx <- resource_context(login,champ,team)
      invalidate_resources(ctx,pattern="^market_")
      rows <- tryCatch(get_market_players(login,champ,team),error=function(e)NULL)
      bid <- NULL
      if (is.data.frame(rows) && !acquisition_data_unverified(rows)) {
        summary <- NULL
        if (!"id" %in% names(rows) || !any(as.character(rows$id) %in% as.character(sp$id))) {
          invalidate_resources(resource_context(login,champ,team,sp$id),pattern="^player_summary_")
          summary <- tryCatch(get_player_summary(login,champ,team,sp$id),error=function(e)NULL)
        }
        bid <- player_card_active_bid(summary,list(target_rows=rows,
          own_bid_valid_until=acquisition_source_deadline(rows,20)),sp$id)
      }
      if (is.null(bid) || (!is.null(expected) && !identical(bid,expected))) {
        shiny::showNotification("Your active offer could not be verified or has changed. Reopen the player before confirming another action.",
          type="warning",duration=6)
        return(NULL)
      }
      bid
    }
    remember_bid <- function(action,bid) {
      values <- modal_bid_info_RV();values[[action]] <- bid;modal_bid_info_RV(values)
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

      minimum<-if(mode %in% c("bid","modify"))market_bid_minimum(sp,capacity$rules$minimum_bid_at_value)else NULL
      result<-evaluate_acquisition_preflight(
        capacity = capacity,
        mode = if(mode=='clause'&&exists('fm_clause_enabled',mode='function')&&fm_clause_enabled())'offer'else mode,
        amount = amount,
        existing_bid_amount = existing_bid_amount,
        minimum_bid = minimum
      )
      result$minimum_bid<-minimum
      result
    }

    complete_acquisition<-function(result,callback){if(exists('fm_clause_complete',mode='function'))fm_clause_complete(result,callback)else callback(result)}

    perform_acquisition <- function(handler,args,mode,diagnostic=FALSE) {
      if(!exists('fm_clause_enabled',mode='function')||!fm_clause_enabled())return(do.call(get(handler,.GlobalEnv),args))
      action<-player_action_context_RV();sp<-selected_player()
      ctx<-resource_context(args$login,args$championship_id,args$team_id,args$player_id)
      current<-function()!session$isClosed()&&identical(isolate(player_action_context_RV()),action)
      result<-fm_clause_execute_async(handler,args,ctx,sp,current,session$token,diagnostic,action_mode=mode)
      structure(list(result=result,current=current),class='fm_clause_async_action')
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
        fm_info("Please enter a valid numerical price greater than 0 €.",error=TRUE,inline=TRUE)
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
        return(fm_info("Loading player details…",class="player-card-loading",pending=TRUE))
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

    # Identity is already available in the selected row. Optional history and
    # availability completions must not reconstruct the surrounding player box.
    observeEvent(selected_player(), {
        sp <- selected_player()
        req(sp)

        login <- get_reactive_val(login_token)
        champ_id <- get_reactive_val(championship_id)
        team_id <- get_reactive_val(user_team_id)

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

    })

    # Actions need ownership, current bids, and verified acquisition/listing
    # state. Stored points, valuation, profile, and scoring are independent.
    observeEvent(
      list(selected_player(), card_states$summary(), card_states$capacity(), card_states$listings(), {
        auth<-get_reactive_val(login_token)
        if(exists('fm_clause_enabled',mode='function')&&fm_clause_enabled()&&valid_login(auth))
          fm_clause_jobs(resource_context(auth,get_reactive_val(championship_id),get_reactive_val(user_team_id)))else NULL
      }),
      {
        sp <- selected_player()
        req(sp)
        login <- get_reactive_val(login_token)
        champ_id <- get_reactive_val(championship_id)
        team_id <- get_reactive_val(user_team_id)
        active_bid_info_RV(player_card_active_bid(card_states$summary()$data,
          card_states$capacity()$data,sp$id))
        action_buttons <- tagList();target_checked<-FALSE

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
            label=tagList(icon("money-bill-transfer")," Sell directly to LigaRadar"),
            class="btn btn-danger", disabled=isTRUE(is_listed_on_market),
            title=if(is_listed_on_market) "Remove the existing listing first." else "Sell to LigaRadar without waiting for an offer."))

          # Check for received offer on squad player
          has_received_offer <- FALSE
          rec_offer_price <- NA_real_
          rec_offer_bidder <- offer_bidder_label(sp$bid_user, sp$bid_source, sp$bidder_team_id)

          if ("bid_price" %in% colnames(sp) && !is.na(sp$bid_price) && suppressWarnings(as.numeric(sp$bid_price)) > 0) {
            has_received_offer <- TRUE
            rec_offer_price <- suppressWarnings(as.numeric(sp$bid_price))
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
            tagList(icon("tags"),paste0(" ",location_status$market)))
          action_buttons <- tagList(ownership_badge,market_badge,action_buttons)

          # Extract release clause parameters
          clause_price_val <- if ("clause_price" %in% colnames(sp) && !is.na(sp$clause_price)) suppressWarnings(as.numeric(sp$clause_price)) else 0
          is_clause_transferred <- if ("clause_transferred" %in% colnames(sp) && !is.na(sp$clause_transferred)) isTRUE(as.logical(sp$clause_transferred)) else FALSE

          # Check clause date lock against current time
          is_clause_date_locked <- FALSE
          clause_date_formatted <- ""
          if ("clause_date" %in% colnames(sp) && !is.na(sp$clause_date) && sp$clause_date != "") {
            clause_time <- fm_time(sp$clause_date)
            if (!is.na(clause_time)) {
              clause_date_formatted <- fm_format_datetime(clause_time)
              if (clause_time > Sys.time()) {
                is_clause_date_locked <- TRUE
              }
            }
          }

          # Determine if release clause is OPEN for buyout
          clause_status_known<-"clause_transferred"%in%names(sp)&&!is.na(sp$clause_transferred[1])
          is_clause_open <- (clause_price_val > 0) && clause_status_known && !is_clause_transferred && !is_clause_date_locked

          # Option 1: "Make Market Offer" button when player is listed on market
          # Off-market owner offers have no listing minimum to verify.
          bid_gate <- passive_acquisition_gate(sp,if(!is.na(eff_market_price) && eff_market_price>0)"bid"else"offer")
          clause_gate <- passive_acquisition_gate(sp,"clause")
          target_checked<-player_target_ready(sp)
          can_open_editor<-target_checked

          if (!is.na(eff_market_price) && eff_market_price > 0) {
            action_buttons <- tagList(
              action_buttons,
              actionButton(
                ns("btn_bid_market"),
                label = tagList(icon("hand-holding-dollar"), " Make Market Offer"),
                class = "btn btn-buy-market", disabled=!isTRUE(bid_gate$ok)&&!(can_open_editor&&bid_gate$reason%in%c("unavailable","loading")),
                title=if(!isTRUE(bid_gate$ok))if(can_open_editor)"Current availability will be checked before submission."else bid_gate$message else NULL
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
                class = "btn btn-offer-money", disabled=!isTRUE(bid_gate$ok)&&!(can_open_editor&&bid_gate$reason%in%c("unavailable","loading")),
                title=if(!isTRUE(bid_gate$ok))if(can_open_editor)"Current availability will be checked before submission."else bid_gate$message else NULL
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
                class = "btn btn-buy-clause", disabled=!isTRUE(clause_gate$ok)&&!(can_open_editor&&clause_gate$reason%in%c("unavailable","loading")),
                title=if(!isTRUE(clause_gate$ok))if(can_open_editor)"Current availability will be checked before submission."else clause_gate$message else NULL
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
          if(isTRUE(allow_scheduled_clauses)&&exists('fm_clause_schedule_ui',mode='function')){
            ctx<-resource_context(get_reactive_val(login_token),get_reactive_val(championship_id),current_user_team,sp$id)
            player<-fm_clause_row_player(sp)
            action_buttons<-tagList(action_buttons,fm_clause_schedule_ui(ns,player,current_user_team,ctx))
            if(fm_clause_diagnostic(get_reactive_val(login_token))&&fm_clause_eligible(player,current_user_team))
              action_buttons<-tagList(action_buttons,actionButton(ns('test_locked_clause'),'Test locked clause (one real request)',class='btn btn-warning',style='white-space:normal;max-width:100%;'))
          }
        }

        summary_state<-card_states$summary()
        summary_unverified<-!player_target_ready(sp)
        readiness<-if(is_own_player)card_states$listings() else if(!summary_unverified)resource_state(data=sp)else summary_state
        stored_mode<-isTRUE(availability_mode())
        listings_ready<-is_own_player&&(player_availability_valid(readiness$data,"listings")||
          (stored_mode&&player_availability_observed(readiness$data,"listings")))
        if(is_own_player&&!listings_ready&&!identical(readiness$status,"loading")&&!is.null(readiness$data))
          readiness<-resource_state(status="unverified",error=readiness$error)
        if(!is_own_player && has_active_bid && !summary_unverified)readiness<-resource_state(data=active_bid_info_RV())
        if(!is_own_player && !has_active_bid && summary_unverified && identical(card_states$summary()$status,"loading"))readiness<-resource_state(status="loading")
        if(!is_own_player && !has_active_bid && !identical(summary_state$status,"loading")&&summary_unverified)
          readiness<-if(is.null(summary_state$data))summary_state else resource_state(status="unverified",error=summary_state$error)
        {
          disable<-function(tag){
            if(inherits(tag,"shiny.tag")){
              if(identical(tag$name,"button"))tag$attribs$disabled<-"disabled"
              tag$children<-lapply(tag$children,disable)
            } else if(is.list(tag))tag<-lapply(tag,disable)
            tag
          }
        }
        if(!is_own_player&&!summary_unverified)readiness<-resource_state(data=sp)
        if(identical(readiness$status,"loading")&&!target_checked) {
          action_buttons<-tagList(fm_info(if(stored_mode)"Loading saved availability…"else"Checking availability…",pending=TRUE),disable(action_buttons))
        } else if(identical(readiness$status,"unverified")) {
          action_buttons<-tagList(fm_info(if(!is.null(readiness$error))resource_failure_message(readiness$error)else
            if(stored_mode)"Player availability is missing or incomplete in the saved data. See Background sync for acquisition status."else
            if(is_own_player)"Your market listings are not verified. Retry availability to check them."else
              "Player availability summary is not verified. Retry availability to check it.",error=!is.null(readiness$error)),disable(action_buttons))
        } else if(is.null(readiness$data)) {
          action_buttons<-tagList(fm_info(if(stored_mode&&is.null(readiness$error))
            "Player availability has not been saved yet. See Background sync for acquisition status."else
            resource_failure_message(readiness$error),error=!is.null(readiness$error)))
        }
        # Keep recovery reachable even when the saved capacity is missing or
        # loading: the ordinary readiness gate disables purchase controls only.
        if(!stored_mode && !is_own_player && !has_active_bid && worker_capacity_verification_available()) {
          pending <- summary_unverified&&identical(summary_state$status,"loading")
          if(summary_unverified)
            action_buttons <- tagList(action_buttons,actionButton(ns("btn_verify_availability"),
              if(pending) "Checking availability…" else "Retry availability",
              class="btn btn-default",disabled=pending))
        }
        if(!stored_mode&&is_own_player&&!player_availability_valid(card_states$listings()$data,"listings")){
          pending<-identical(card_states$listings()$status,"loading")
          action_buttons<-tagList(action_buttons,actionButton(ns("btn_verify_availability"),
            if(pending)"Checking availability…"else"Retry availability",class="btn btn-default",disabled=pending))
        }
        output$action_buttons <- renderUI(action_buttons)
        if(!timed_availability&&!identical(readiness$status,"loading")){timed_availability<<-TRUE;resource_timing("availability_actions",isolate(card_started()))}
      }
    )

    # ---- Shared helper: verify and open the active-bid update modal ----
    open_modify_bid_modal <- function() {
      sp <- selected_player()
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(sp, login, champ_id, team_id)

      bid_info <- verify_active_bid(sp)
      if (is.null(bid_info)) {
        active_bid_info_RV(NULL)
        modify_modal_opened_RV(FALSE)
        return(invisible(FALSE))
      }
      active_bid_info_RV(bid_info)

      pf <- run_acquisition_preflight(sp, "modify", amount = NULL,
                                      existing_bid_amount = bid_info$price)
      if (!isTRUE(pf$ok)) {
        show_preflight_failure(pf)
        modify_modal_opened_RV(FALSE)
        return(invisible(FALSE))
      }

      remember_action_context("modify")
      remember_bid("modify",bid_info)
      modify_modal_opened_RV(TRUE)
      showModal(modalDialog(
        title = tagList(icon("pen-to-square"), " Update Your Active Bid"),
        p(strong(sp$name)),
        p("Current Active Bid: ", strong(player_card_money(bid_info$price))),
        p("Minimum market bid: ", strong(player_card_money(pf$minimum_bid))),
        numericInput(
          ns("new_bid_amount"),
          label = "New Bid Amount (EUR):",
          value = max(bid_info$price, pf$minimum_bid),
          min = pf$minimum_bid,
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
      bid_info <- modal_bid_info_RV()[["modify"]]
      req(sp, login, champ_id, team_id, bid_info)

      if(!exists("fm_clause_enabled",mode="function")||!fm_clause_enabled())bid_info <- verify_active_bid(sp,bid_info)
      req(bid_info)

      new_price <- input$new_bid_amount
      if (is.null(new_price) || is.na(new_price) || !is.numeric(new_price) || new_price <= 0) {
        shiny::showNotification("Please enter a valid numerical price greater than 0 €.", type = "error")
        return()
      }

      # Preflight before write: verify capacity + funds (delta vs existing bid).
      pf <- if(exists("fm_clause_enabled",mode="function")&&fm_clause_enabled())list(ok=TRUE)else run_acquisition_preflight(sp, "modify", amount = new_price, existing_bid_amount = bid_info$price)
      if (!isTRUE(pf$ok)) {
        show_preflight_failure(pf)
        return()
      }

      bid_id <- bid_info$id
      player_id <- sp$id

      result <- perform_acquisition("modify_bid", list(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id,
        bid_id = bid_id,
        new_price = new_price
      ), "modify")
      complete_acquisition(result,function(res){
      success<-if(is.list(res))isTRUE(res$success)else isTRUE(res)

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
          if(is.list(res)&&nzchar(fm_scalar(res$message,"")))res$message else "Failed to update bid. Verify the current offer before trying again.",
          type = "error",
          duration = 5
        )
      }
      })
    })

    # ---- Cancel Active Bid Modal ----
    observeEvent(input$btn_cancel_bid, {
      sp <- selected_player()
      req(sp)
      bid_info <- verify_active_bid(sp)
      req(bid_info)
      remember_action_context("cancel")
      remember_bid("cancel",bid_info)

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
      bid_info <- modal_bid_info_RV()[["cancel"]]
      req(sp, login, champ_id, team_id, bid_info)

      bid_info <- verify_active_bid(sp,bid_info)
      req(bid_info)

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
    # Opening reuses verified card facts locally; submission independently runs
    # the existing current-state preflight. Never block this click on network I/O.
    market_offer_preflight <- function(sp, state) {
      if(!player_target_current(sp))return(list(ok=FALSE,reason="unavailable",message="Current player ownership and market status could not be verified."))
      if(player_card_is_own_player(sp,get_reactive_val(user_team_id))||!isTRUE(sp$market_inMarket)||
         !is.finite(fm_number(sp$effective_market_price %||% sp$price)))
        return(list(ok=FALSE,reason="unavailable",message="This player is not available for a market offer."))
      capacity <- state$data
      deadline <- capacity$valid_until
      if (!identical(state$status,"ok") || identical(state$freshness,"stale") ||
          !is.numeric(deadline) || length(deadline)!=1L || !is.finite(deadline) ||
          deadline<=as.numeric(Sys.time())) {
        return(list(ok=FALSE,reason="unavailable",message=if(!is.null(state$error))
          resource_failure_message(state$error) else acquisition_unavailable_message(capacity)))
      }
      minimum <- market_bid_minimum(sp,capacity$rules$minimum_bid_at_value)
      result <- evaluate_acquisition_preflight(capacity,"bid",minimum_bid=minimum)
      result$minimum_bid <- minimum
      result
    }
    output$market_offer_body <- renderUI({
      dialog <- market_offer_dialog_RV();req(dialog)
      if (isTRUE(dialog$pending)) return(fm_info("Checking availability…",pending=TRUE))
      if (!isTRUE(dialog$preflight$ok)) return(fm_info(dialog$preflight$message,error=TRUE,inline=TRUE))
      market_price <- dialog$preflight$minimum_bid
      tagList(p("Minimum market bid: ",strong(player_card_money(market_price))),
        numericInput(ns("bid_amount"),label="Your offer amount (EUR):",value=market_price,min=market_price,step=10000),
        tags$div(role="group", `aria-label`="Choose an offer premium",
          style="display:flex;flex-wrap:wrap;gap:6px;margin-bottom:12px;",
          lapply(c(0.5, 1, 2, 4, 5, 10), function(pct) tags$button(
            type="button", class="btn btn-default", paste0("+", pct, "%"),
            `data-target-input`=ns("bid_amount"),
            # Add the whole-euro premium separately to avoid 100 * 1.10
            # becoming 110.000000000000014 and rounding up an extra euro.
            `data-amount`=format(market_price + ceiling(market_price * pct / 100),
              scientific=FALSE, trim=TRUE),
            onclick="var field=document.getElementById(this.dataset.targetInput);if(field){field.value=this.dataset.amount;field.dispatchEvent(new Event('change',{bubbles:true}));}"))),
        p("Percentages are above the minimum market bid, rounded up to whole euros."),
        uiOutput(ns("bid_amount_preview")))
    })
    output$market_offer_submit <- renderUI({
      dialog <- market_offer_dialog_RV();req(dialog)
      actionButton(ns("submit_bid"),"Submit Market Offer",class="btn btn-buy-market",
        disabled=isTRUE(dialog$pending)||!isTRUE(dialog$preflight$ok))
    })
    observeEvent(input$market_offer_closed,{
      dialog <- market_offer_dialog_RV()
      if (!is.null(dialog) && identical(fm_scalar(input$market_offer_closed,""),as.character(dialog$generation))) {
        market_offer_dialog_RV(NULL);offer_modal_opened_RV(FALSE)
        values <- modal_context_RV();values$bid <- NULL;modal_context_RV(values)
      }
    })
    open_market_offer_modal <- function() {
      sp <- selected_player()
      context <- player_action_context_RV();req(sp,!is.null(context))
      state <- card_states$capacity();deadline <- state$data$valid_until
      if(player_target_current(sp)&&(player_card_is_own_player(sp,get_reactive_val(user_team_id))||!isTRUE(sp$market_inMarket))){
        show_preflight_failure(market_offer_preflight(sp,state));offer_modal_opened_RV(FALSE)
        return(invisible(FALSE))
      }
      pending <- !player_target_current(sp) || identical(state$status,"loading") || is.null(state$data) ||
        identical(state$freshness,"stale") || !is.numeric(deadline) || length(deadline)!=1L ||
        !is.finite(deadline) || deadline<=as.numeric(Sys.time())
      pf <- if (!pending) market_offer_preflight(sp,state) else NULL
      if (!pending && !isTRUE(market_offer_decision(sp,pf)$open)) {
        show_preflight_failure(pf)
        offer_modal_opened_RV(FALSE)
        return(invisible(FALSE))
      }
      values <- modal_context_RV();values$bid <- NULL;modal_context_RV(values)
      market_offer_generation <<- market_offer_generation+1L
      generation <- market_offer_generation
      auth<-get_reactive_val(login_token)
      live_check<-FALSE
      market_offer_dialog_RV(list(context=context,player=sp,generation=generation,pending=pending,preflight=pf,live_check=live_check))
      if (!pending) remember_action_context("bid")
      offer_modal_opened_RV(TRUE)
      showModal(modalDialog(
        title = tagList(icon("hand-holding-dollar"), " Place Market Offer"),
        div(id=ns("market_offer_marker"),p(strong(sp$name)),uiOutput(ns("market_offer_body"))),
        tags$script(HTML(paste0("(function(){var marker=document.getElementById('",ns("market_offer_marker"),
          "');var modal=marker&&marker.closest('.modal');if(modal)$(modal).one('hidden.bs.modal',function(){Shiny.setInputValue('",
          ns("market_offer_closed"),"','",generation,"',{priority:'event'});});})();"))),
        footer = div(style = "text-align: center; width: 100%; display: flex; justify-content: center; gap: 10px;",
                      modalButton("Cancel"),
                      uiOutput(ns("market_offer_submit"),inline=TRUE)),
        easyClose = TRUE,
        size = "s"
      ))
      # Join the card's team verification. A pending request is never duplicated
      # by opening an offer; only a settled expired/failed result needs retry.
      if(pending)verify_card_availability(current=TRUE)
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
      rec_offer_bidder <- offer_bidder_label(sp$bid_user, sp$bid_source, sp$bidder_team_id)
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
      pf <- if(exists("fm_clause_enabled",mode="function")&&fm_clause_enabled())list(ok=TRUE)else run_acquisition_preflight(sp, "bid", amount = bid_amount)
      if (!isTRUE(pf$ok)) {
        show_preflight_failure(pf)
        return()
      }

      player_id <- sp$id
      player_slug <- if ("slug" %in% colnames(sp) && !is.na(sp$slug)) sp$slug else sp$name

      result <- perform_acquisition("buy_clause", list(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id,
        player_slug = player_slug,
        price = bid_amount,
        isClause = FALSE
      ), "bid")
      complete_acquisition(result,function(res){

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
        err_msg <- offer_error_message(res)
        shiny::showNotification(
          tags$div(class = "fm-offer-error", paste0("Offer failed: ", err_msg)),
          type = "error",
          duration = NULL
        )
      }
      })
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
      pf <- if(exists("fm_clause_enabled",mode="function")&&fm_clause_enabled())list(ok=TRUE)else run_acquisition_preflight(sp, "offer", amount = offer_amount)
      if (!isTRUE(pf$ok)) {
        show_preflight_failure(pf)
        return()
      }

      player_id <- sp$id
      player_slug <- if ("slug" %in% colnames(sp) && !is.na(sp$slug)) sp$slug else sp$name

      result <- perform_acquisition("buy_clause", list(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id,
        player_slug = player_slug,
        price = offer_amount,
        isClause = FALSE
      ), "offer")
      complete_acquisition(result,function(res){

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
        err_msg <- offer_error_message(res)
        shiny::showNotification(
          tags$div(class = "fm-offer-error", paste0("Offer failed: ", err_msg)),
          type = "error",
          duration = NULL
        )
      }
      })
    })

    if(isTRUE(allow_scheduled_clauses)&&exists('fm_clause_card_observers',mode='function'))
      fm_clause_card_observers(input,session,selected_player,function()get_reactive_val(login_token),
        function()get_reactive_val(championship_id),function()get_reactive_val(user_team_id),
        remember_action_context,consume_action_context,player_action_context_RV)

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
    open_clause_buyout_modal <- function(sp, diagnostic=FALSE) {
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
      diagnostic<-isTRUE(diagnostic)&&isTRUE(allow_scheduled_clauses)&&exists('fm_clause_diagnostic',mode='function')&&
        fm_clause_diagnostic(get_reactive_val(login_token))&&fm_clause_eligible(fm_clause_row_player(sp),current_team)
      if (!isTRUE(today_is_clause_open(sp))&&!diagnostic) {
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
      clause_diagnostic_RV(diagnostic)
      clause_modal_opened_RV(TRUE)
      showModal(modalDialog(
        title = tagList(icon("bolt"), " Confirm Release Clause Buyout"),
        p(strong(sp$name)),
        p(if(diagnostic) 'DIAGNOSTIC: This sends one real purchase request while the clause is reported locked. If Futmondo accepts it, the player may be purchased and your balance may become negative. No automatic retry will occur.' else 'This purchases the player for their official release clause. Delivery follows your league settings.'),
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

    observeEvent(input$test_locked_clause, {
      req(isTRUE(allow_scheduled_clauses),fm_clause_diagnostic(get_reactive_val(login_token)))
      open_clause_buyout_modal(selected_player(),diagnostic=TRUE)
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
      diagnostic<-isTRUE(clause_diagnostic_RV())&&isTRUE(allow_scheduled_clauses)&&
        exists('fm_clause_diagnostic',mode='function')&&fm_clause_diagnostic(login)
      clause_diagnostic_RV(FALSE)
      if (!isTRUE(today_is_clause_open(sp))&&!diagnostic) {
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
      pf <- if(exists("fm_clause_enabled",mode="function")&&fm_clause_enabled())list(ok=TRUE)else run_acquisition_preflight(sp, "clause", amount = clause_price)
      if (!isTRUE(pf$ok)) {
        show_preflight_failure(pf)
        return()
      }

      if(diagnostic&&(!exists("fm_clause_enabled",mode="function")||!fm_clause_enabled())){
        diagnostic_read_options<-options(futmondo.worker_live=TRUE);on.exit(options(diagnostic_read_options),add=TRUE)
        invalidate_resources(resource_context(login,champ_id,team_id,sp$id),pattern='player_summary')
        current<-tryCatch(fm_clause_player(get_player_summary(login,champ_id,team_id,sp$id)),error=function(e)NULL)
        if(!fm_clause_eligible(current,team_id)||!identical(current$owner_id,player_card_owner_id(sp))||current$price>clause_price){
          removeModal();showNotification('Diagnostic stopped: the current owner, price or future lock changed.',type='error');return()
        }
        clause_price<-current$price
      }
      player_id <- sp$id
      player_slug <- if ("slug" %in% colnames(sp) && !is.na(sp$slug)) sp$slug else sp$name

      diagnostic_options<-options(futmondo.clause_diagnostic=if(diagnostic)login[['userid']]else NULL)
      on.exit(options(diagnostic_options),add=TRUE)
      result <- perform_acquisition("buy_roster_clause", list(
        login = login,
        championship_id = champ_id,
        team_id = team_id,
        player_id = player_id,
        player_slug = player_slug,
        price = clause_price
      ), "clause",diagnostic=diagnostic)
      complete_acquisition(result,function(res){

      is_success <- if (is.list(res)) isTRUE(res$success) else isTRUE(res)

      if(diagnostic)showNotification(paste0('Locked-clause diagnostic response: ',fm_clause_safe_code(res$code),
        '. No retry was sent. Share this code; never share credentials or a raw response.'),type=if(isTRUE(res$success))'warning'else'message',duration=NULL)

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
      showModal(modalDialog(title=paste0("Sell ",sp$name," directly to LigaRadar?"),
        p("This immediately sells the player to LigaRadar / Mercado without waiting for an offer."),
        if(is.finite(check$value)) p(paste0("Current market value (reference only): ",player_card_money(check$value))),
        p("LigaRadar determines the sale proceeds. A guaranteed sale price is not available. The player will leave your squad, which may affect your lineup."),
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
      rec_offer_bidder <- offer_bidder_label(sp$bid_user, sp$bid_source, sp$bidder_team_id)

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

    history_refresh_trigger <- round_history_refresh_trigger(session)
    history_refresh <- reactive(paste(history_refresh_trigger(),input$refresh_history %||% 0L,sep=":"))
    observeEvent(input$refresh_history,{
      sp<-selected_player();auth<-get_reactive_val(login_token)
      req(sp,valid_login(auth))
      base<-resource_context(auth,get_reactive_val(championship_id),get_reactive_val(user_team_id),sp$id)
      invalidate_resources(base,pattern="^player_summary_|^player_card_history$")
      invalidate_resources(base,resources="card_profile",notify_availability=FALSE)
      invalidate_resources(resource_context(auth,base$championship_id,base$user_team_id),pattern="^lineup_")
      invalidate_resources(resource_context(auth,player_id=sp$id),pattern="^player_matches_|^player_full_profile_")
    },ignoreInit=TRUE,priority=120)
    card_context <- reactive({
      league_context_revision()
      sp<-raw_selected_player();if(is.null(sp)||!valid_login(get_reactive_val(login_token)))return(NULL)
      resource_context(get_reactive_val(login_token),get_reactive_val(championship_id),get_reactive_val(user_team_id),sp$id,session_id=session$token)
    })
    history_context <- reactive({
      ctx<-card_context();if(is.null(ctx))return(NULL)
      if(isTRUE(session$rootScope()$userData$league_context_enabled)){resolved<-card_states$resolved()$data;req(!is.null(resolved),resolved$competition_id!="unknown",resolved$season!="unknown")}
      summary<-card_states$summary()$data;lineup<-card_states$lineup()$data
      ctx$season<-league_season_context(ctx$championship_id,summary$championship$season %||% summary$championship$seasonId)
      ctx$scoring_version<-normalize_league_rules(list(),lineup,ctx$championship_id)$scoring_version
      supplied_competition<-fm_scalar(summary$championship$competitionId %||% summary$championship$competition$id,"")
      if(nzchar(supplied_competition)&&ctx$competition_id!="unknown"&&!identical(supplied_competition,ctx$competition_id))stop("Player summary competition mismatch")
      if(nzchar(supplied_competition))ctx$competition_id<-supplied_competition
      ctx
    })
    card_read <- function(resource,handler,args,context=card_context,active=function()TRUE,refresh=history_refresh) {
      resolved_context<-function(){
        ctx<-context();if(is.null(ctx))return(NULL)
        fm_worker_resolve_context(handler,args(ctx),ctx)
      }
      worker_replay<-function(){
        source_fn<-get0(handler,envir=.GlobalEnv,mode="function")
        if(!fm_worker_configured()||!startsWith(handler,"get_")||
          !is.function(source_fn)||!"login"%in%names(formals(source_fn)))return(NULL)
        ctx<-resolved_context();if(is.null(ctx))return(NULL)
        mode<-fm_worker_mode(ctx$account_id);if(identical(mode,FALSE))return(list(enabled=FALSE))
        descriptor<-fm_worker_descriptor(handler,args(ctx),ctx)
        state<-fm_worker_account_state(ctx$account_id)
        revision<-fm_worker_resource_revision(state,descriptor$resource_key)()
        list(enabled=mode,revision=revision,ready=isTRUE(mode)&&!is.null(fm_worker_get(ctx$account_id,descriptor$resource_key)))
      }
      shared_resource(session,resource,context=resolved_context,
        active=function()!is.null(card_context())&&isTRUE(active()),refresh=refresh,replay=worker_replay,
        fetch=function(ctx) {
          captured<-args(ctx)
          async_adapter_fetch(handler,captured,context=ctx,is_current=function()shiny::isolate({
            current<-resolved_context();!is.null(current)&&identical(current,ctx)&&isTRUE(active())
          }))
        })
    }
    auth_args<-function(ctx)list(login=get_reactive_val(login_token),championship_id=ctx$championship_id,user_team_id=ctx$user_team_id)
    player_args<-function(ctx)c(auth_args(ctx),list(player_id=ctx$player_id))
    global_refresh<-function()if(is.function(refresh_trigger))refresh_trigger()else 0L
    # These reads describe the team, not the clicked player. Keep one pending
    # read while navigating cards, with account/league/team/session isolation.
    team_card_context<-reactive({ctx<-card_context();if(!is.null(ctx))ctx$player_id<-"";ctx})
    availability_team_context<-reactive({
      league_context_revision()
      auth<-get_reactive_val(login_token);league<-get_reactive_val(championship_id);team<-get_reactive_val(user_team_id)
      if(!valid_login(auth)||!nzchar(fm_scalar(league,""))||!nzchar(fm_scalar(team,"")))return(NULL)
      ctx<-resource_context(auth,league,team,session_id=session$rootScope()$token)
      fm_worker_resolve_context("get_team_acquisition_capacity",auth_args(ctx),ctx)
    })
    availability_mode<-function(){ctx<-availability_team_context();if(!is.null(ctx)&&fm_worker_configured())fm_worker_mode(ctx$account_id)else FALSE}
    availability_login<-function()get_reactive_val(login_token)
    availability_opening<-reactive({sp<-raw_selected_player();if(!is.null(sp))fm_scalar(sp$id,"")else NULL})
    availability_stored<-function(handler,ctx,args){
      if(is.null(ctx)||!fm_worker_configured()||!isTRUE(fm_worker_mode(ctx$account_id)))return(NULL)
      descriptor<-fm_worker_descriptor(handler,args,ctx);account<-fm_worker_account_state(ctx$account_id)
      fm_worker_resource_revision(account,descriptor$resource_key)()
      saved<-fm_worker_get(ctx$account_id,descriptor$resource_key)
      if(is.null(saved))NULL else {attr(saved$data,'availability_from_saved')<-TRUE;resource_state(saved$data)}
    }
    availability_requested<-reactiveVal(NULL)
    availability_required<-function()!is.null(market_offer_dialog_RV())&&identical(availability_requested(),availability_team_context())
    card_states<-list()
    card_states$resolved<-card_read("card_resolved_context","get_resolved_league_context",auth_args,context=team_card_context,active=function()isTRUE(session$rootScope()$userData$league_context_enabled))
    summary_availability_context<-reactive({
      ctx<-card_context();if(is.null(ctx))return(NULL)
      ctx$session_id<-session$rootScope()$token
      fm_worker_resolve_context("get_player_summary",player_args(ctx),ctx)
    })
    card_states$summary<-player_availability_source(session,"summary",summary_availability_context,availability_login,
      refresh=function()paste(global_refresh(),history_refresh(),sep=":"),active=function()!is.null(card_context()),
      stored=function(){ctx<-summary_availability_context();availability_stored("get_player_summary",ctx,if(!is.null(ctx))player_args(ctx)else list())},
      current_context=availability_team_context,mode=availability_mode,opening=availability_opening)
    availability_admitted<-function()!is.null(card_context())
    card_states$lineup<-card_read("card_lineup","get_lineup_from_team",auth_args,context=team_card_context,active=availability_admitted)
    card_states$profile<-card_read("card_profile","get_player_full_profile",function(ctx)list(login=get_reactive_val(login_token),player_id=ctx$player_id))
    card_states$matches<-card_read("card_matches","get_player_matches",function(ctx)list(login=get_reactive_val(login_token),player_id=ctx$player_id),active=availability_admitted)
    context_ready<-function()!identical(card_states$summary()$status,"loading")&&!identical(card_states$lineup()$status,"loading")
    card_states$points<-card_read("card_points","read_card_round_history",function(ctx)list(context=ctx),context=history_context,active=context_ready)
    card_states$valuation<-card_read("card_valuation","read_card_price_history",function(ctx)list(context=ctx),context=history_context,active=context_ready)
    card_states$capacity<-player_availability_source(session,"team",availability_team_context,availability_login,
      refresh=global_refresh,
      active=function()availability_required()&&!is.null(card_context())&&!player_card_is_own_player(selected_player(),get_reactive_val(user_team_id)),
      stored=function(){
        ctx<-availability_team_context();saved<-availability_stored("get_team_acquisition_capacity",ctx,if(!is.null(ctx))auth_args(ctx)else list())
        saved
      },current_context=availability_team_context,mode=availability_mode,opening=availability_opening,intent=function()'manual_action')
    manual_capacity_verification<-attr(card_states$capacity,"verification")
    # Kept as the card's existing recovery seam; it now works in either mode.
    worker_capacity_verification_available<-function()!is.null(availability_team_context())
    verify_card_availability<-function(current=FALSE){
      if(player_card_is_own_player(selected_player(),get_reactive_val(user_team_id))){
        if(!player_availability_valid(isolate(card_states$listings())$data,"listings"))attr(card_states$listings,"retry")()
        return(invisible(NULL))
      }
      if(if(isTRUE(current))!player_target_current()else!player_target_ready())attr(card_states$summary,"ensure")()
      if(isTRUE(current)){
        availability_requested(availability_team_context())
        attr(card_states$capacity,"ensure")()
      }
      invisible(NULL)
    }
    observeEvent(input$btn_verify_availability,{
      if(!isTRUE(availability_mode()))verify_card_availability()
    })
    observeEvent(list(card_states$capacity(),selected_player(),card_states$summary()),{
      dialog <- isolate(market_offer_dialog_RV());state <- card_states$capacity()
      if (is.null(dialog) || isTRUE(dialog$live_check) || !isTRUE(dialog$pending) ||
          !identical(dialog$context,isolate(player_action_context_RV()))) return()
      target<-selected_player()
      target_unavailable<-player_target_current(target)&&(player_card_is_own_player(target,get_reactive_val(user_team_id))||!isTRUE(target$market_inMarket))
      if(!target_unavailable&&identical(state$status,"loading"))return()
      if(!player_target_current(target)&&identical(card_states$summary()$status,"loading"))return()
      dialog$player<-target
      dialog$pending <- FALSE
      dialog$preflight <- market_offer_preflight(dialog$player,state)
      market_offer_dialog_RV(dialog)
      if (isTRUE(dialog$preflight$ok)) remember_action_context("bid")
    },ignoreInit=TRUE)
    card_states$listings<-player_availability_source(session,"listings",availability_team_context,availability_login,
      active=function()!is.null(card_context())&&player_card_is_own_player(selected_player(),get_reactive_val(user_team_id)),refresh=global_refresh,
      stored=function(){ctx<-availability_team_context();availability_stored("get_my_market_players",ctx,if(!is.null(ctx))auth_args(ctx)else list())},
      current_context=availability_team_context,mode=availability_mode,opening=availability_opening)
    passive_acquisition_gate<-function(sp,mode){
      state<-card_states$capacity()
      if(identical(state$status,"loading"))return(list(ok=FALSE,reason="loading",message=NULL))
      capacity<-state$data
      if(exists('fm_clause_enabled',mode='function')&&fm_clause_enabled()){
        ctx<-resource_context(get_reactive_val(login_token),get_reactive_val(championship_id),get_reactive_val(user_team_id),sp$id)
        capacity<-fm_clause_adjust_capacity(capacity,fm_clause_jobs(ctx))
      }
      evaluate_acquisition_preflight(capacity,mode=if(mode=='clause'&&exists('fm_clause_enabled',mode='function')&&fm_clause_enabled())'offer'else mode,minimum_bid=if(mode=="bid")market_bid_minimum(sp,capacity$rules$minimum_bid_at_value)else NULL)
    }
    card_started<-reactiveVal(Sys.time());timed_summary<-FALSE;timed_history<-FALSE;timed_availability<-FALSE
    card_chart_ready<-reactiveVal(FALSE)
    card_paint_generation<-reactiveVal(0L)
    chart_context<-NULL
    chart_data<-reactiveVal(NULL)
    observeEvent(card_context(),{
      current<-card_context()
      if(identical(current,chart_context)) return()
      chart_context<<-current
      chart_data(NULL)
      card_started(Sys.time());timed_summary<<-FALSE;timed_history<<-FALSE;timed_availability<<-FALSE;card_chart_ready(FALSE);card_paint_generation(isolate(card_paint_generation())+1L)

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
        card_states$points()$data,card_states$valuation()$data,ctx,profile=card_states$profile()$data)
      states<-lapply(card_states[c("summary","lineup","points","valuation")],function(read)read())
      if(any(vapply(states,function(state)identical(state$status,"loading"),logical(1))))result$status<-"loading"
      result$errors<-Filter(Negate(is.null),lapply(states,function(state)state$error))
      result
    })
    observe({
      ctx<-history_context();profile<-card_states$profile()$data
      if(!is.null(ctx)&&!is.null(profile))persist_player_valuation_profile(profile,ctx,
        previous=attr(card_states$valuation()$data,"valuation_profile"))
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
      if(!isTRUE(card_chart_ready()))return(NULL)
      plotly::plotlyOutput(ns("player_trend_plot"),height="340px")
    })
    output$player_chart_info <- renderUI({
      sp<-selected_player();req(sp)
      if(!isTRUE(card_chart_ready()))return(fm_info("Loading chart…",pending=TRUE))
      player_trend_information(card_history(),sp,normalize_player_matches(card_states$matches()$data))
    })
    # Publish only chart inputs: availability/status changes must not redraw the
    # widget. Keep the last curve while a refresh has no replacement data.
    observe({
      req(card_chart_ready())
      history<-card_history()
      matches<-normalize_player_matches(card_states$matches()$data)
      previous<-isolate(chart_data())
      if(identical(history$status,"loading") && !nrow(history$points) &&
         !nrow(history$valuation) && !is.null(previous)) return()
      chart_data(list(history=list(points=history$points,valuation=history$valuation,
        context=history$context,status=if(identical(history$status,"loading") && !nrow(history$points) && !nrow(history$valuation)) "loading" else "ready"),matches=matches))
    })
    output$player_trend_plot <- plotly::renderPlotly({
      req(card_chart_ready())
      data<-chart_data();req(!is.null(data));history<-data$history
      empty_chart <- function() plotly::plot_ly() %>% fm_plot_layout(
        paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
        xaxis=list(visible=FALSE),yaxis=list(visible=FALSE),
        annotations=list())
      if (identical(history$status,"loading") && !nrow(history$points) && !nrow(history$valuation)) return(empty_chart())
      points <- history$points
      if (!is.data.frame(points) || !all(c("round_number","points") %in% names(points))) points <- data.frame()
      if (nrow(points)) {
        points$round_number <- suppressWarnings(as.numeric(points$round_number));points$points <- suppressWarnings(as.numeric(points$points))
        points <- points[is.finite(points$round_number)&is.finite(points$points),,drop=FALSE]
        points <- points[order(points$round_number),,drop=FALSE]
      }
      values <- history$valuation
      if (!is.data.frame(values) || !all(c("recorded_at","value") %in% names(values))) values <- data.frame()
      if (nrow(values)) {
        values$display_date <- fm_format_datetime(values$recorded_at); values$date <- fm_plot_datetime(values$recorded_at);values$value <- suppressWarnings(as.numeric(values$value))
        values <- values[!is.na(values$date)&is.finite(values$value),,drop=FALSE]
        values <- values[order(values$date),,drop=FALSE]
      }
      if (!nrow(values) && !nrow(points)) return(empty_chart())

      # Reuse the already requested match details to date rounds; never use
      # observation/save timestamps as match dates or discard undated scores.
      points_dates <- player_trend_points_dates(points,data$matches,history$context %||% list())
      dated_points <- nrow(points)>0L && all(!is.na(points_dates))
      separate_round_axis <- nrow(values)>0L && nrow(points)>0L && !dated_points
      chart <- plotly::plot_ly()
      if (nrow(values)) chart <- chart %>% plotly::add_trace(data=values,type="scatter",mode="lines",x=~date,y=~value,
        name="Valuation",yaxis="y",line=list(color="#00ff88",width=2.5,dash="solid"),
        text=~display_date,hovertemplate="%{text}<br>Valuation: %{y:,.0f} EUR<extra></extra>")
      if (nrow(points)) {
        points$plot_x <- if(dated_points)fm_plot_datetime(points_dates) else points$round_number
        points$label <- paste0("Round ",points$round_number,"<br>Points: ",points$points,
          if(dated_points)paste0("<br>",fm_format_datetime(points_dates))else "",
          if("score_status" %in% names(points))paste0("<br>",points$score_status)else "")
        # Postponed fixtures can put round numbers out of date order. Plotly
        # connects rows as supplied, so order the complete rows by the axis
        # actually displayed, keeping scores and hover provenance together.
        points <- points[order(points$plot_x,points$round_number),,drop=FALSE]
        chart <- chart %>% plotly::add_trace(data=points,type="scatter",mode="lines+markers",
          x=~plot_x,y=~points,text=~label,hoverinfo="text",name="Points",yaxis="y2",
          xaxis=if(separate_round_axis)"x2"else "x",
          line=list(color="#F28C28",width=2.5,dash="solid"),marker=list(size=7,color="#F28C28"))
      }
      date_axis <- nrow(values)>0L || dated_points
      horizontal_axis <- list(title=if(date_axis)list(text="Date",standoff=20)else "Round",
        type=if(date_axis)"date"else "linear",tickfont=list(size=10),tickangle=if(date_axis)90 else 0,showgrid=FALSE)
      round_spacing <- if(nrow(points))max(1,ceiling(diff(range(points$round_number))/3))else 1
      if(date_axis) {
        # Use displayed calendar dates so month lengths and account timezones
        # cannot shift the first/midmonth ticks as a fixed 15-day interval would.
        dates <- as.Date(substr(c(values$date,if(dated_points)points$plot_x),1,10))
        months <- seq(as.Date(format(min(dates),"%Y-%m-01")),
          as.Date(format(max(dates),"%Y-%m-01")),by="month")
        horizontal_axis$tickmode <- "array"
        horizontal_axis$tickvals <- as.character(sort(c(months,months+14)))
        horizontal_axis$tickformat <- "%d %b"
      } else {
        horizontal_axis$tickmode <- "linear";horizontal_axis$dtick <- round_spacing
      }
      chart %>% fm_plot_layout(
        meta=list(fm_player_valuation=TRUE),
        xaxis=horizontal_axis,
        xaxis2=if(separate_round_axis)list(title=list(text="Round",font=list(color="#F28C28")),color="#F28C28",overlaying="x",side="top",
          tickmode="linear",dtick=round_spacing,tickfont=list(size=10),tickangle=0,showgrid=FALSE)else list(visible=FALSE),
        yaxis=list(title=list(text="Value (EUR)",font=list(color="#00ff88")),visible=nrow(values)>0L,autorange=FALSE,
          range=player_trend_axis_range(values$value),tickformat=".3s",color="#00ff88"),
        yaxis2=list(title=list(text="Points",font=list(color="#F28C28")),visible=nrow(points)>0L,overlaying="y",side="right",autorange=FALSE,
          range=player_trend_axis_range(points$points),showgrid=FALSE,color="#F28C28",tickformat="~g"),
        showlegend=FALSE,
        margin=list(l=55,r=50,b=95,t=if(separate_round_axis)60 else 20),
        annotations=list())
    })
    render_match <- function(match) {
      score<-if(is.finite(match$home_score)&&is.finite(match$away_score))paste0(format(match$home_score,trim=TRUE),"–",format(match$away_score,trim=TRUE))else "Score unavailable"
      date<-if(nzchar(match$occurred_at))fm_format_datetime(match$occurred_at)else "Date unavailable"
      div(class="player-round-fixture",strong(paste(match$home,score,match$away)),span(class="player-match-date",date))
    }
    output$recent_round_points <- renderUI({
      sp<-selected_player();req(sp);history<-card_history();match_state<-card_states$matches()
      matches<-normalize_player_matches(match_state$data)
      rounds<-build_player_recent_rounds(history$points,matches,history$context %||% list(),limit=50L,current_player_responses=TRUE)
      tags$details(class="player-recent-rounds",
        tags$summary(icon("futbol")," Recent rounds"),
        div(class="player-round-boxes",lapply(rounds$rounds,function(round){
          div(class="player-match-card player-round-box",
            div(class="player-round-heading",span(paste("Round",round$round_number)),
              strong(if(is.finite(round$points))paste0(round$points," pts")else "Points unavailable")),
            if(nrow(round$fixtures))lapply(seq_len(nrow(round$fixtures)),function(i)render_match(round$fixtures[i,,drop=FALSE]))else
              fm_info(if(identical(match_state$status,"loading"))"Loading match details…"else "Match association unavailable",error=!identical(match_state$status,"loading"),pending=identical(match_state$status,"loading")))
        })),
        if(!length(rounds$rounds))fm_info(if(identical(history$status,"loading"))"Loading round points…"else recent_round_points_empty_text(sp$points),error=!identical(history$status,"loading"),pending=identical(history$status,"loading")),
        if(is.null(card_states$summary()$data)&&!identical(card_states$summary()$status,"loading"))fm_info(
          if(isTRUE(availability_mode())&&is.null(card_states$summary()$error))"Player history has not been saved yet."else
          resource_failure_message(card_states$summary()$error),error=!is.null(card_states$summary()$error)),
        if(nrow(rounds$unassociated))tagList(
          fm_info("Match records · points association unavailable",error=TRUE),
          div(class="player-round-boxes",lapply(seq_len(nrow(rounds$unassociated)),function(i){
            match<-rounds$unassociated[i,,drop=FALSE]
            div(class="player-match-card player-round-box",span(class="player-match-round",paste("Round",match$round)),render_match(match))
          }))),
        if(is.null(match_state$data)&&!identical(match_state$status,"loading"))fm_info(resource_failure_message(match_state$error),error=TRUE),
        if(length(rounds$rounds))fm_info(p("Points are reported by Futmondo. Final confirmation is not provided for some rounds."),source="",class="player-points-explanation"),
        if(length(history$errors))fm_info(paste(if(nrow(history$points)||nrow(history$valuation))
          "Showing saved history."else "Player history could not be read.",resource_failure_message(history$errors[[1L]])),error=TRUE)else
          if(identical(history$freshness,"stale"))fm_info(if(isTRUE(availability_mode()))
            "Showing saved history; awaiting a newer background observation."else "Showing the last saved history."))
    })
    output$player_external_details <- renderUI({
      req(selected_player());state<-card_states$profile();answer<-state$data
      if(is.null(answer))return(div(class="player-external-details",h4(icon("address-card")," Player profile"),
        fm_info(if(identical(state$status,"loading"))"Loading profile…"else resource_failure_message(state$error),error=!identical(state$status,"loading"),pending=identical(state$status,"loading"))))
      data<-normalize_player_full_profile(answer);profile<-data$profile;bits<-character()
      if(nzchar(profile$nationality))bits<-c(bits,profile$nationality)
      if(is.finite(profile$height_cm)&&profile$height_cm>0)bits<-c(bits,paste0(format(profile$height_cm,trim=TRUE)," cm"))
      if(is.finite(profile$weight_kg)&&profile$weight_kg>0)bits<-c(bits,paste0(format(profile$weight_kg,trim=TRUE)," kg"))
      if(nzchar(profile$preferred_foot))bits<-c(bits,profile$preferred_foot)
      prices<-data$price_history;latest<-if(nrow(prices))tail(prices,1L)else NULL
      div(class="player-external-details",h4(icon("address-card")," Player profile"),
        div(class="player-profile-summary",if(length(bits))span(paste(bits,collapse=" · ")),
          if(!is.null(latest)&&is.finite(latest$social[1]))tagList(
            span(class="player-profile-price",paste0("Profile valuation: ",player_card_money(latest$social[1]))),
            fm_info(paste(nrow(prices),"saved values")))else fm_info("Profile valuation history is unavailable.",error=TRUE)))
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

# Match association applies the same context checks as the recent-round cards.
# For rounds with several fixtures, use the last actual fixture date.
player_trend_points_dates <- function(points,matches,context) {
  dates <- fm_time(points$date %||% rep(NA_character_,nrow(points)))
  if (!nrow(points) || !anyNA(dates)) return(dates)
  rounds <- build_player_recent_rounds(points,matches,context,limit=nrow(points),current_player_responses=TRUE)$rounds
  for (round in rounds) {
    indices <- which(points$round_number==round$round_number & is.na(dates))
    fixture_dates <- fm_time(round$fixtures$occurred_at %||% character())
    fixture_dates <- fixture_dates[!is.na(fixture_dates)]
    if(length(indices) && length(fixture_dates))dates[indices] <- max(fixture_dates)
  }
  dates
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

# Chart diagnostics use existing history/match state; expanding them never fetches.
player_trend_information <- function(history, player, matches=data.frame()) {
  if(identical(history$status,"loading"))return(fm_info("Loading player history…",pending=TRUE))
  points<-history$points;values<-history$valuation
  if(!is.data.frame(points)||!all(c("round_number","points")%in%names(points)))points<-data.frame()
  if(nrow(points))points<-points[is.finite(suppressWarnings(as.numeric(points$round_number)))&
    is.finite(suppressWarnings(as.numeric(points$points))),,drop=FALSE]
  if(!is.data.frame(values)||!all(c("recorded_at","value")%in%names(values)))values<-data.frame()
  if(nrow(values))values<-values[!is.na(fm_time(values$recorded_at))&
    is.finite(suppressWarnings(as.numeric(values$value))),,drop=FALSE]
  notes<-attr(history$valuation,"valuation_note") %||% character()
  if(!nrow(values))notes<-c(notes,paste0("Valuation history unavailable. Current value: ",player_card_money(player$value)))
  if(!nrow(points))notes<-c(notes,recent_round_points_empty_text(player$points))
  if(nrow(points)&&nrow(values)&&anyNA(player_trend_points_dates(points,matches,history$context %||% list())))
    notes<-c(notes,"Match dates unavailable: round axis is not aligned with dates.")
  if(!length(notes))return(NULL)
  fm_info(lapply(notes,shiny::tags$p),error=TRUE,class="player-history-status")
}
