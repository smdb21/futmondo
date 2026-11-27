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
  return("Futmondo rejected this offer. Check the league's buying rules before trying again.")
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

player_card_has_rival_owner <- function(player, current_team_id) {
  owner <- player_card_owner_id(player)
  current <- trimws(fm_scalar(current_team_id, ""))
  nzchar(owner) && nzchar(current) && owner != current
}

player_card_can_buy_clause <- function(player,current_team_id,clause_open) {
  isTRUE(clause_open) && player_card_has_rival_owner(player, current_team_id)
}

player_card_bid <- function(id, price) {
  id <- fm_scalar(id, "")
  price <- fm_number(price)
  if (!nzchar(id) || !is.finite(price) || price <= 0) return(NULL)
  list(id = id, price = price)
}

# The team-scoped market returns a single direct bid per player. Cancel uses
# only the bid ID, so it also needs to identify a single listing in this market.
player_card_market_bid <- function(rows, index) {
  if (length(index) != 1L) return(NULL)
  bid <- player_card_bid(rows$bid_id[index], rows$bid_price[index])
  if (!is.null(bid) && sum(rows$bid_id == bid$id, na.rm = TRUE) == 1L) bid
}

player_card_active_bid <- function(summary, capacity, player_id, now = Sys.time()) {
  deadline <- capacity$own_bid_valid_until %||% capacity$valid_until
  rows <- capacity$target_rows
  if (!acquisition_data_unverified(capacity) && is.numeric(deadline) &&
      length(deadline)==1L && is.finite(deadline) && deadline>as.numeric(now) &&
      is.data.frame(rows) && "id" %in% names(rows)) {
    index <- which(as.character(rows$id) == fm_scalar(player_id, ""))
    if (length(index)) return(player_card_market_bid(rows, index))
  }
  # Off-market owner offers may instead have immutable own-team evidence in
  # the summary. Its age and status must remain independently verifiable.
  if (is.list(summary) && !acquisition_data_unverified(summary) &&
      acquisition_source_deadline(summary,60,now)>as.numeric(now))
    return(player_card_bid(summary$my_bid_id, summary$my_bid_price))
  NULL
}

# A saved market observation can identify an offer for display between syncs.
# A submission receipt has an amount but no native bid ID. The manual check
# resolves that ID before showing a cancellation confirmation.
player_card_market_offer <- function(state, receipt, player_id, now = Sys.time()) {
  observed <- if (is.null(state)) NA_real_ else fm_manual_finance_time(state)
  rows <- state$data
  usable <- is.finite(observed) && observed <= as.numeric(now) && is.data.frame(rows) &&
    (!nrow(rows) || all(c("id", "bid_id", "bid_price") %in% names(rows)))
  if (!is.null(receipt) && (!usable || as.numeric(receipt$acknowledged_at) >= observed))
    return(list(bid = list(id = NULL, price = receipt$max_price), label = "Your submitted offer: "))
  if (!usable) return(NULL)
  target <- which(as.character(rows$id) == player_id)
  if (!length(target)) return(list(listed = FALSE))
  list(listed = TRUE, bid = player_card_market_bid(rows, target), label = "Your last recorded bid: ")
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
  } else if (nzchar(owner_name)) paste0(owner_name, " (rival)") else "Rival Owned"
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


# The selected table row can seed the same draft action that the card later
# renders from its saved summary. A known own bid waits for the update controls.
player_card_offer_button <- function(player, team, ns) {
  if (is.null(player) || !nzchar(fm_scalar(team, "")) || player_card_is_own_player(player, team) ||
      nzchar(fm_scalar(player$my_bid_id, ""))) return(NULL)
  if (isTRUE(player$market_inMarket) && is.finite(market_bid_minimum(player, FALSE))) {
    return(actionButton(ns("btn_bid_market"), tagList(icon("hand-holding-dollar"), " Make Market Offer"),
      class = "btn btn-buy-market", `data-offer-editor` = "true"))
  }
  if (!isTRUE(player$market_inMarket) && player_card_has_rival_owner(player, team)) {
    owner <- fm_scalar(player$userTeam, fm_scalar(player$teamname, "Owner"))
    return(actionButton(ns("btn_offer_owner"), tagList(icon("hand-holding-dollar"), paste0(" Offer to ", owner)),
      class = "btn btn-offer-money", `data-offer-editor` = "true"))
  }
  NULL
}

selected_player_UI <- function(id, player = NULL, team = NULL) {
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
    tags$section(id = ns("selected_player_box"), class = "player-card",
      div(class = "player-card-toolbar",
        h2("Player details"),
        div(class = "player-card-close-shortcut",
          tags$button(type = "button", class = "btn btn-default",
            `data-dismiss` = "modal", `aria-label` = "Close player card", title = "Close player card",
            icon("times")))
      ),
      uiOutput(ns("player_identity_summary")),
      div(id = ns("action_buttons"), class = "shiny-html-output player-card-actions",
        player_card_offer_button(player, team, ns)),
      tags$dl(class = "player-card-metrics",
        uiOutput(ns("player_points_description_box")),
        uiOutput(ns("player_last_points_description_box")),
        uiOutput(ns("player_value_description_box"), class = "player-card-valuation")
      ),
      uiOutput(ns("recent_round_points")),
      div(class = "player-card-section",
        div(class = "player-chart-heading",
          h3("Value and points history"),
          actionButton(ns("refresh_history"), NULL, icon = icon("rotate"),
            title = "Refresh history", `aria-label` = "Refresh history")
        ),
        uiOutput(ns("player_chart_info")),
        uiOutput(ns("player_chart_container"))
      ),
      div(class = "player-card-section", uiOutput(ns("player_external_details")))
    )
  )
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
        # Availability is shared per player. Incoming offers belong to the selected row.
        if (!is.null(previous)) {
          for (field in c("bid_price", "bid_user", "bid_id", "bid__id", "bidder_team_id", "bid_source"))
            previous[[field]] <- row[[field]]
        }
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

    active_bid_info_RV <- reactiveVal(NULL)
    modal_bid_info_RV <- reactiveVal(list())


    # The displayed editor and its confirmation belong to one generation.
    offer_modal_opened_RV <- reactiveVal(FALSE)
    market_offer_dialog_RV <- reactiveVal(NULL)
    market_offer_generation <- 0L
    submitting_offer <- NULL

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
      sp <- tryCatch(raw_selected_player(),error=function(e)NULL)
      if(!valid_login(auth) || is.null(sp) || !nzchar(fm_scalar(sp$id,""))) return(NULL)
      league_context_revision()
      ctx<-resource_context(auth,get_reactive_val(championship_id),get_reactive_val(user_team_id),sp$id)
      list(user=ctx$account_id,championship=ctx$championship_id,team=ctx$user_team_id,player=ctx$player_id,
        competition=ctx$competition_id,season=ctx$season,scoring=ctx$scoring_version)
    })
    action_context_generation <- 0L
    previous_player_action_context_RV <- reactiveVal(NULL)
    observeEvent(player_action_context_RV(), {
      current_context <- player_action_context_RV()
      previous_context <- isolate(previous_player_action_context_RV())
      if (!identical(current_context, previous_context)) action_context_generation <<- action_context_generation + 1L
      active_bid_info_RV(NULL);modal_context_RV(list());modal_bid_info_RV(list())
      market_offer_dialog_RV(NULL)
      submitting_offer <<- NULL
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
    # Shared preflight for clause buyouts and bid modifications.
    # It verifies roster capacity, verified
    # spendable funds, and (for modify) the existing own bid, then fails closed
    # when verification is unavailable/ambiguous.
    #
    # amount: numeric amount to spend (NULL when opening a bid modification)
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

    perform_acquisition <- function(handler,args,mode) {
      if(!exists('fm_clause_enabled',mode='function')||!fm_clause_enabled())return(do.call(get(handler,.GlobalEnv),args))
      action<-player_action_context_RV();sp<-selected_player()
      ctx<-resource_context(args$login,args$championship_id,args$team_id,args$player_id)
      current<-function()!session$isClosed()&&identical(isolate(player_action_context_RV()),action)
      result<-fm_clause_execute_async(handler,args,ctx,sp,current,session$token,action_mode=mode)
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
      div(class = "player-card-identity", `data-player-id`=as.character(sp$id),`data-paint-generation`=as.character(card_paint_generation()),
        div(class = "player-card-identity-portrait",
          img(src = photo, class = "player-card-identity-photo", alt = player_name,
            onerror = paste0("this.src='", SPACER_GIF, "';")),
          player_status_icon(raw_selected_player()[["status"]])),
        div(class = "player-card-identity-details",
          div(class = "player-card-identity-name", player_name),
          div(class = "player-card-identity-meta",
            team_logo,
            span(paste(role, "·", team)))
        )
      )
    })

    # Actions need ownership, current bids, and verified acquisition/listing
    # state. Stored points, valuation, profile, and scoring are independent.
    market_offer <- reactive({
      ctx <- card_context()
      if (is.null(ctx)) return(NULL)
      root <- session$rootScope()$userData
      state <- if (is.function(root$market_offer_state)) root$market_offer_state()
      dimensions <- c("account_id", "championship_id", "user_team_id", "competition_id", "season", "scoring_version")
      if (!is.null(state$context) && !identical(ctx[dimensions], state$context[dimensions])) state <- NULL
      receipt <- if (is.function(root$confirmed_market_offer)) root$confirmed_market_offer(ctx)
      player_card_market_offer(state, receipt, ctx$player_id)
    })
    observeEvent(
      list(selected_player(), market_offer(), card_states$summary(), card_states$capacity(), card_states$listings(), {
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
        offer <- market_offer()
        bid <- if (is.null(offer) || isFALSE(offer$listed)) {
          capacity <- if (is.null(offer)) card_states$capacity()$data
          player_card_active_bid(card_states$summary()$data, capacity, sp$id)
        } else offer$bid
        active_bid_info_RV(bid)
        action_buttons <- tagList();target_checked<-FALSE

        current_user_team <- get_reactive_val(user_team_id)
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

          if (is_listed_on_market) {
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
            action_buttons <- tagList(btn_update_sale, btn_cancel_sell)
          } else {
            btn_put_on_market <- actionButton(
              ns("btn_put_on_market"),
              label = tagList(icon("tags"), " Put on Market for Sale"),
              class = "btn btn-offer-money"
            )
            action_buttons <- btn_put_on_market
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
              class = "player-card-note",
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
            class = "player-card-note",
            tagList(icon("hand-holding-dollar"), paste0(" ", offer$label %||% "Your Active Bid: ", player_card_money(bid_info$price)))
          )
          btn_modify <- if (!is.null(bid_info$id)) actionButton(
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

          target_checked <- player_target_ready(sp)
          action_buttons <- tagList(action_buttons, player_card_offer_button(sp, current_user_team, ns))

          # Option 3: rival-owned release clause
          has_rival_owner <- player_card_has_rival_owner(sp, current_user_team)
          if (player_card_can_buy_clause(sp,current_user_team,is_clause_open)) {
            clause_label <- paste0(" Buy Clause: ", player_card_money(clause_price_val))
            action_buttons <- tagList(
              action_buttons,
              actionButton(
                ns("btn_pay_clause"),
                label = tagList(icon("bolt"), clause_label),
                class = "btn btn-buy-clause"
              )
            )
          } else if (has_rival_owner && clause_price_val > 0) {
            clause_label <- if (is_clause_transferred) {
              transfer_time <- fm_time(sp$clause_transferDate %||% NA_character_)
              if (length(transfer_time) == 1L && !is.na(transfer_time) && transfer_time > Sys.time()) {
                paste("Release clause purchased · Transfer scheduled for", fm_format_datetime(transfer_time))
              } else {
                "Release clause purchased"
              }
            } else if (is_clause_date_locked) {
              paste("Release Clause Locked until", clause_date_formatted)
            } else {
              "Release clause unavailable · transfer status unverified"
            }
            clause_note <- div(
              class = "player-card-note",
              tagList(icon("lock"), paste0(" ", clause_label, " (", player_card_money(clause_price_val), ")"))
            )
            action_buttons <- tagList(action_buttons, clause_note)
          }
          if(has_rival_owner&&isTRUE(allow_scheduled_clauses)&&exists('fm_clause_schedule_ui',mode='function')){
            ctx<-resource_context(get_reactive_val(login_token),get_reactive_val(championship_id),current_user_team,sp$id)
            player<-fm_clause_row_player(sp)
            action_buttons<-tagList(action_buttons,fm_clause_schedule_ui(ns,player,current_user_team,ctx))
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
        if(!is_own_player && has_active_bid)readiness<-resource_state(data=active_bid_info_RV())
        if(!is_own_player && !has_active_bid && summary_unverified && identical(card_states$summary()$status,"loading"))readiness<-resource_state(status="loading")
        if(!is_own_player && !has_active_bid && !identical(summary_state$status,"loading")&&summary_unverified)
          readiness<-if(is.null(summary_state$data))summary_state else resource_state(status="unverified",error=summary_state$error)
        {
          disable<-function(tag){
            if(inherits(tag,"shiny.tag")){
              if(identical(tag$name,"button")&&is.null(tag$attribs[["data-offer-editor"]]))tag$attribs$disabled<-"disabled"
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
            resource_failure_message(readiness$error),error=!is.null(readiness$error)),disable(action_buttons))
        }
        if (!stored_mode) {
          needs_listing <- is_own_player && !player_availability_valid(card_states$listings()$data, "listings")
          needs_summary <- !is_own_player && !has_active_bid && summary_unverified && worker_capacity_verification_available()
          if (needs_listing || needs_summary) {
            status <- if (needs_listing) card_states$listings()$status else summary_state$status
            recovery <- if (identical(status, "loading")) {
              p(class = "player-card-status", role = "status", "Checking availability…")
            } else {
              actionButton(ns("btn_verify_availability"), "Retry availability", class = "btn btn-default")
            }
            action_buttons <- tagList(action_buttons, recovery)
          }
        }
        # The own-listings snapshot can confirm a listing absent from the table row.
        display_row <- sp
        if (is_own_player) display_row$market_inMarket <- is_listed_on_market
        location <- player_card_location_label(display_row, current_user_team)
        if (is_own_player && is_listed_on_market && is.finite(current_asking_price))
          location <- paste0(location, " · Asking ", player_card_money(current_asking_price))
        action_buttons <- tagList(p(class = "player-card-status", location), action_buttons)
        output$action_buttons <- renderUI(action_buttons)
        if(!timed_availability&&!identical(readiness$status,"loading")){timed_availability<<-TRUE;resource_timing("availability_actions",isolate(card_started()))}
      }
    )

    offer_dialog_dependency <- htmltools::htmlDependency(
      "fm-offer-dialog", "2", src = c(file = "www"), script = "offer-dialog.js")
    market_bid_editor <- function(id, label, amount, base, base_label, player_value, min, step = "any") {
      editor <- div(class = "fm-market-bid-editor",
        `data-base` = as.character(base), `data-base-label` = base_label,
        `data-player-value` = if (is.finite(player_value)) as.character(player_value),
        numericInput(id, label, value = amount, min = min, step = step),
        div(class = "fm-offer-presets", role = "group",
          `aria-label` = paste("Add a percentage of the", base_label),
          lapply(c(1, 2, 4, 5, 10), function(pct) tags$button(
            type = "button", class = "btn btn-default", `data-fm-offer-premium` = pct,
            paste0("+", pct, "%")))),
        div(id = paste0(id, "_preview"), `aria-live` = "polite",
          strong(class = "fm-offer-total"), span(class = "fm-offer-percent"),
          tags$small(class = "fm-offer-warning")))
      htmltools::attachDependencies(editor, offer_dialog_dependency)
    }

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
        market_bid_editor(ns("new_bid_amount"), "New Bid Amount (EUR):",
          max(bid_info$price, pf$minimum_bid), pf$minimum_bid, "minimum market bid",
          fm_number(sp$value), min = pf$minimum_bid, step = 1),
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

    cancel_pending <- FALSE
    check_cancel_bid <- function(expected, callback) {
      req(!cancel_pending, !is.null(expected))
      context <- player_action_context_RV()
      login <- get_reactive_val(login_token)
      args <- list(login = login, championship_id = context$championship, user_team_id = context$team)
      ctx <- resource_context(login, context$championship, context$team)
      generation <- action_context_generation
      current <- function() !session$isClosed() && generation == action_context_generation &&
        identical(isolate(player_action_context_RV()), context)
      read <- function(handler, args, ctx, key) {
        invalidate_resources(ctx, resources = key, notify_availability = FALSE)
        async_adapter_fetch(handler, args, context = ctx, owner = session$token, is_current = current,
          priority = "interactive", intent = "manual_action")
      }
      market <- read("get_market_players", args, ctx, paste("market", context$championship, context$team, sep = "_"))
      cancel_pending <<- TRUE
      check <- promises::then(market, function(rows) {
        if (!current() || !is.data.frame(rows) || acquisition_data_unverified(rows)) return(NULL)
        if (any(as.character(rows$id) %in% context$player)) {
          return(player_card_active_bid(NULL, list(target_rows = rows,
            own_bid_valid_until = acquisition_source_deadline(rows, 20)), context$player))
        }
        args$player_id <- context$player
        ctx$player_id <- context$player
        summary <- read("get_player_summary", args, ctx,
          paste("player_summary", login[["userid"]], context$championship, context$team, context$player, sep = "_"))
        promises::then(summary, function(value) player_card_active_bid(value, NULL, context$player))
      })
      result <- promises::then(check, function(bid) {
        if (!current()) return(NULL)
        if (is.null(bid) || !identical(bid$price, expected$price) ||
            (!is.null(expected$id) && !identical(bid$id, expected$id))) {
          showNotification("Your offer has changed or could not be verified. Reopen the player to check it.", type = "warning")
          return(NULL)
        }
        callback(bid, context, login, current)
      })
      promises::finally(promises::catch(result, function(error) {
        if (current()) showNotification("Futmondo could not confirm the offer status. Check your active offers before acting again.", type = "warning")
      }), function() cancel_pending <<- FALSE)
      invisible(NULL)
    }

    observeEvent(input$btn_cancel_bid, {
      sp <- selected_player()
      req(sp)
      check_cancel_bid(active_bid_info_RV(), function(bid, context, login, current) {
        isolate({
          remember_action_context("cancel")
          remember_bid("cancel", bid)
        })
        showModal(modalDialog(
          title = tagList(icon("trash-can"), " Cancel Active Bid"),
          p(strong(sp$name)),
          p("Withdraw your offer of ", strong(player_card_money(bid$price)), " from the transfer market?"),
          footer = tagList(modalButton("Keep Bid"),
            actionButton(ns("submit_cancel_bid"), "Confirm Cancel Bid", class = "btn btn-cancel-bid")),
          easyClose = TRUE, size = "s"))
      })
    })

    observeEvent(input$submit_cancel_bid, {
      req(consume_action_context("cancel"))
      sp <- selected_player()
      check_cancel_bid(modal_bid_info_RV()[["cancel"]], function(bid, context, login, current) {
        verified_at <- Sys.time()
        dispatch <- function() current() && as.numeric(difftime(Sys.time(), verified_at, units = "secs")) < 15
        pending <- cancel_bid(login = login, championship_id = context$championship,
          team_id = context$team, bid_id = bid$id, owner = session$token, current = dispatch)
        promises::then(promises::promise_resolve(pending), function(success) {
          invalidate_manual_resources(login, context$championship, context$team, context$player)
          fm_worker_refresh_league(login, context$championship, context$team, context$player)
          if (!current()) return(NULL)
          removeModal()
          if (isTRUE(success)) {
            if (is.function(on_bid_updated)) isolate(on_bid_updated(action_type = "bid_cancelled",
              player_id = context$player, new_bid_price = NA_real_, is_cancel = TRUE))
            showNotification(paste("Your bid on", sp$name, "was cancelled."), type = "message")
          } else showNotification("Futmondo did not confirm the cancellation. Check your active offers before acting again.", type = "warning")
          NULL
        })
      })
    })

    # Render the complete editor from the selected row and available saved rules.
    # Futmondo validates the actual offer when it receives the explicit submission.
    open_offer_modal <- function(mode) {
      req(is.null(submitting_offer))
      sp <- selected_player()
      context <- player_action_context_RV()
      req(sp, !is.null(context), !player_card_is_own_player(sp, context$team), is.null(active_bid_info_RV()))
      market <- identical(mode, "bid")
      listing <- market_bid_minimum(sp, FALSE)
      req(if (market) isTRUE(sp$market_inMarket) && is.finite(listing)
          else player_card_has_rival_owner(sp, context$team) && !isTRUE(sp$market_inMarket))
      rule <- card_states$capacity()$data$rules$minimum_bid_at_value
      minimum <- if (market) market_bid_minimum(sp, rule) else NA_real_
      amount <- if (market) if (is.finite(minimum)) minimum else listing else fm_number(sp$value)
      if (!is.finite(amount) || amount <= 0) amount <- NA_real_
      owner <- fm_scalar(sp$userTeam, fm_scalar(sp$teamname, "Owner"))
      amount_id <- ns(if (market) "bid_amount" else "owner_offer_amount")
      submit_id <- ns(if (market) "submit_bid" else "submit_owner_offer")
      market_offer_generation <<- market_offer_generation + 1L
      generation <- market_offer_generation
      market_offer_dialog_RV(list(context = context, mode = mode, generation = generation, player = sp))
      offer_modal_opened_RV(TRUE)
      marker <- div(id = ns("market_offer_marker"), class = "fm-offer-dialog",
        `data-generation` = generation, `data-submit-input` = ns("confirm_player_offer"),
        `data-closed-input` = ns("market_offer_closed"), `data-amount-id` = amount_id,
        `data-submit-id` = submit_id,
        p(strong(sp$name)),
        if (market) p(if (is.finite(minimum)) "Saved minimum market bid: " else "Listed price: ",
          strong(player_card_money(amount)))
        else if (is.finite(amount)) p("Current Market Valuation: ", strong(player_card_money(amount))),
        if (market) market_bid_editor(amount_id, "Your offer amount (EUR):", amount, amount,
          if (is.finite(minimum)) "saved minimum market bid" else "listed price",
          fm_number(sp$value), min = 1)
        else tagList(numericInput(amount_id, "Your purchase offer amount (EUR):",
          value = amount, min = 1, step = "any"), uiOutput(ns("owner_offer_amount_preview"))))
      dialog <- modalDialog(
        title = tagList(icon("hand-holding-dollar"),
          if (market) " Place Market Offer" else paste0(" Offer Money to ", owner)),
        marker,
        footer = tagList(
          tags$div(class = "fm-offer-pending", role = "status", `aria-live` = "polite"),
          div(style = "display:flex;justify-content:center;gap:10px;flex-wrap:wrap;",
            modalButton("Cancel"),
            tags$button(id = submit_id, type = "button",
              class = if (market) "btn btn-buy-market" else "btn btn-offer-money",
              if (market) "Submit Market Offer" else "Submit Offer"))),
        easyClose = TRUE, fade = FALSE, size = "s")
      showModal(htmltools::attachDependencies(dialog, offer_dialog_dependency, append = TRUE))
      invisible(TRUE)
    }
    open_market_offer_modal <- function() open_offer_modal("bid")
    observeEvent(input$btn_bid_market, open_market_offer_modal())
    observeEvent(input$btn_offer_owner, open_offer_modal("offer"))
    observeEvent(input$market_offer_closed, {
      dialog <- market_offer_dialog_RV()
      if (!is.null(dialog) && identical(fm_scalar(input$market_offer_closed, ""), as.character(dialog$generation))) {
        market_offer_dialog_RV(NULL)
        offer_modal_opened_RV(FALSE)
      }
    })

    # ---- Shared received-offer confirmation modal ----
    received_decision_pending <- FALSE
    open_received_offer_modal <- function(action) {
      sp <- selected_player()
      req(sp, !received_decision_pending, player_card_is_own_player(sp, get_reactive_val(user_team_id)))
      id <- fm_scalar(sp$bid_id, "")
      if (!nzchar(id)) id <- fm_scalar(sp$bid__id, "")
      offer <- list(id = id, price = fm_number(sp$bid_price), team = fm_scalar(sp$bidder_team_id, ""),
        label = offer_bidder_label(sp$bid_user, sp$bid_source, sp$bidder_team_id))
      if (!nzchar(id) || !is.finite(offer$price) || offer$price <= 0) {
        showNotification("The selected offer is missing its ID or amount. Refresh My Team to check the current offer.", type = "warning")
        return(invisible(FALSE))
      }
      accept <- identical(action, "accept")
      title <- if (accept) "Accept Offer" else "Reject Offer"
      remember_action_context(action)
      remember_bid(action, offer)
      showModal(modalDialog(
        title = tagList(icon(if (accept) "circle-check" else "circle-xmark"), paste(title, "for", sp$name)),
        p(paste(if (accept) "Accept" else "Reject", "the offer of "), strong(player_card_money(offer$price)),
          " from ", strong(offer$label), "?"),
        if (accept) p("Accepting sells this player and credits your budget."),
        footer = tagList(modalButton("Cancel"), actionButton(ns(paste0("submit_", action, "_offer")),
          paste("Confirm", title), class = if (accept) "btn btn-offer-money" else "btn btn-cancel-bid")),
        easyClose = TRUE, size = "s"))
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
          open_received_offer_modal("accept")
        }
      }, ignoreNULL = TRUE)
    }

    # The browser sends the displayed generation and the current field value in
    # one confirmation, so a debounced input or a dismissed dialog cannot send.
    observeEvent(input$confirm_player_offer, {
      intent <- input$confirm_player_offer
      dialog <- market_offer_dialog_RV()
      req(is.list(intent), !is.null(dialog), is.null(submitting_offer),
        identical(fm_scalar(intent$generation, ""), as.character(dialog$generation)),
        identical(dialog$context, player_action_context_RV()))
      sp <- dialog$player
      login <- get_reactive_val(login_token)
      champ_id <- dialog$context$championship
      team_id <- dialog$context$team
      amount <- fm_number(intent$amount)
      if (!is.finite(amount) || amount <= 0 || amount > 1e16) {
        show_preflight_failure(list(message = "Please enter a valid offer amount."))
        session$sendCustomMessage("fm-offer-unlock", list(id = ns("market_offer_marker"), generation = dialog$generation))
        return()
      }
      submitting_offer <<- dialog$generation
      player_id <- sp$id
      args <- list(login = login, championship_id = champ_id, team_id = team_id,
        player_id = player_id, player_slug = fm_scalar(sp$slug, sp$name), price = amount, isClause = FALSE)
      ctx <- resource_context(login, champ_id, team_id, player_id)
      current <- function() !session$isClosed() && isolate({
        identical(dialog$context, player_action_context_RV()) &&
          identical(dialog$generation, market_offer_dialog_RV()$generation)
      })
      conflict <- fm_manual_reservation_conflict(fm_clause_jobs(ctx), player_id, amount, card_states$capacity()$data)
      if (!is.null(conflict)) {
        submitting_offer <<- NULL
        show_preflight_failure(list(message = conflict))
        session$sendCustomMessage("fm-offer-unlock", list(id = ns("market_offer_marker"), generation = dialog$generation))
        return()
      }
      begin_finance <- session$userData$begin_manual_finance_action
      confirmed <- if (is.function(begin_finance)) begin_finance(ctx, dialog$mode, sp$user_team_id, amount) else NULL
      result <- tryCatch(fm_send_player_offer(args, sp, ctx, confirmed),
        error = function(e) list(success = FALSE, code = "offer_not_sent", message = "The offer could not be queued."))
      fm_clause_complete(result, function(res) {
        submitting_offer <<- NULL
        market_offer_dialog_RV(NULL)
        offer_modal_opened_RV(FALSE)
        removeModal()
        if (!isTRUE(res$success)) {
          shiny::showNotification(tags$div(class = "fm-offer-error",
            offer_error_message(res)), type = "error", duration = NULL)
          if (isTRUE(res$refresh) && !isTRUE(res$refresh_queued) && is.function(on_bid_updated)) {
            tryCatch(on_bid_updated(action_type = "refresh"), error = function(e) NULL)
          }
          return()
        }
        kind <- if (identical(dialog$mode, "bid")) "Market" else "Direct"
        shiny::showNotification(paste0(kind, " offer of ", player_card_money(amount),
          " submitted successfully for ", sp$name, "!"), type = "message", duration = 5)
        invalidate_manual_resources(login, champ_id, team_id, player_id)
        if (is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(action_type = if (identical(dialog$mode, "bid")) "bid_placed" else "owner_offer_placed",
            player_id = player_id, new_bid_price = amount, is_cancel = FALSE), error = function(e) NULL)
        }
      }, current)
    })

    if(isTRUE(allow_scheduled_clauses)&&exists('fm_clause_card_observers',mode='function'))
      fm_clause_card_observers(input,session,selected_player,function()get_reactive_val(login_token),
        function()get_reactive_val(championship_id),function()get_reactive_val(user_team_id),
        remember_action_context,consume_action_context,player_action_context_RV)

    # The card and external clause actions share the same observed player and price.
    open_clause_buyout_modal <- function(sp) {
      req(sp)

      current_team <- get_reactive_val(user_team_id)
      if (!player_card_has_rival_owner(sp, current_team)) {
        shiny::showNotification(
          "Release clause purchases require a player owned by a rival team.",
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

      remember_action_context("clause")
      remember_bid("clause", list(id = sp$id, slug = fm_scalar(sp$slug, sp$name),
        name = sp$name, seller = sp$user_team_id, price = clause_price))
      clause_modal_opened_RV(TRUE)
      dialog <- modalDialog(
        title = tagList(icon("bolt"), " Confirm Release Clause Buyout"),
        tags$div(class = "fm-offer-dialog", `data-submit-input` = ns("submit_clause"),
          `data-submit-id` = ns("confirm_clause_button"), `data-operation` = "purchase",
          p(strong(sp$name))),
        p('This purchases the player for their official release clause. Delivery follows your league settings.'),
        p("Clause price: ", strong(player_card_money(clause_price))),
        p(style = "color: var(--fm-danger); font-size: 13px; font-weight: 600;", "Are you sure you want to trigger this clause buyout?"),
        footer = tagList(
          tags$div(class = "fm-offer-pending", role = "status", `aria-live` = "polite"),
          div(style = "display:flex;justify-content:center;gap:10px;flex-wrap:wrap;",
            modalButton("Cancel"),
            tags$button(id = ns("confirm_clause_button"), type = "button", class = "btn btn-buy-clause", "Confirm Clause Buyout"))),
        easyClose = TRUE, fade = FALSE, size = "s")
      showModal(htmltools::attachDependencies(dialog, offer_dialog_dependency, append = TRUE))
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
      purchase <- modal_bid_info_RV()$clause
      action <- player_action_context_RV()
      login <- get_reactive_val(login_token)
      req(purchase, action, login)
      args <- list(login = login, championship_id = action$championship, team_id = action$team,
        player_id = purchase$id, player_slug = purchase$slug, price = purchase$price)
      ctx <- resource_context(login, action$championship, action$team, purchase$id)
      conflict <- fm_manual_reservation_conflict(fm_clause_jobs(ctx), purchase$id, purchase$price, card_states$capacity()$data)
      if (!is.null(conflict)) {
        removeModal()
        show_preflight_failure(list(message = conflict))
        return()
      }
      begin_finance <- session$userData$begin_manual_finance_action
      confirmed <- if (is.function(begin_finance)) begin_finance(ctx, "clause", purchase$seller, purchase$price) else NULL
      result <- fm_send_manual_acquisition("buy_roster_clause", args, confirmed)
      fm_clause_complete(result, function(res) {
        uncertain <- identical(res$code, "purchase_response_uncertain")
        if (isTRUE(res$success)) {
          tryCatch(log_market_transaction(player_id = purchase$id, championship_id = action$championship,
            buyer_team_id = action$team, seller_team_id = purchase$seller, price = purchase$price,
            is_clause = TRUE), error = function(e) message("[Futmondo] Could not record confirmed clause purchase."))
        }
        refresh <- isTRUE(res$success) || uncertain
        queued <- FALSE
        if (refresh || identical(res$code, "api.market.max_number_players_in_roster")) {
          invalidate_manual_resources(login, action$championship, action$team, purchase$id)
          if (refresh) queued <- fm_worker_refresh_league(login, action$championship, action$team, purchase$id)
        }
        if (session$isClosed() || !identical(action, isolate(player_action_context_RV()))) return()
        removeModal()
        if (isTRUE(res$success)) {
          shiny::showNotification(paste0("Release clause buyout of ", player_card_money(purchase$price),
            " executed successfully for ", purchase$name, "!"), type = "message", duration = 5)
        } else {
          shiny::showNotification(res$message, type = if (uncertain) "warning" else "error", duration = 6)
        }
        if (refresh && !isTRUE(queued) && is.function(on_bid_updated)) {
          tryCatch(on_bid_updated(action_type = if (uncertain) "refresh" else "clause_paid"), error = function(e) NULL)
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
        sync_requested <- fm_worker_refresh_league(login,champ_id,team_id,sp$id)
        if(!sync_requested && is.function(on_bid_updated)) tryCatch(on_bid_updated(
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

    observeEvent(input$btn_accept_offer, open_received_offer_modal("accept"))
    observeEvent(input$btn_reject_offer, open_received_offer_modal("reject"))

    submit_received_offer <- function(action) {
      req(!received_decision_pending, consume_action_context(action))
      context <- player_action_context_RV()
      login <- get_reactive_val(login_token)
      offer <- modal_bid_info_RV()[[action]]
      player_name <- selected_player()$name
      req(offer, valid_login(login))
      current <- function() !session$isClosed() && identical(isolate(player_action_context_RV()), context)
      received_decision_pending <<- TRUE
      showModal(modalDialog(title = paste("Checking the offer for", player_name),
        "Waiting for Futmondo to verify the offer and confirm your decision.", footer = NULL, easyClose = FALSE))
      decide <- if (action == "accept") accept_bid else reject_bid
      pending <- decide(login = login, championship_id = context$championship, team_id = context$team,
        player_id = context$player, bid_id = offer$id, expected = offer, owner = session$token, current = current)
      promises::then(promises::promise_resolve(pending), function(result) {
        received_decision_pending <<- FALSE
        success <- isTRUE(result$success)
        if (success || result$code %in% c("offer_changed", "uncertain", "api.error.not_found")) {
          invalidate_manual_resources(login, context$championship, context$team, context$player)
          queued <- fm_worker_refresh_league(login, context$championship, context$team, context$player)
          if (!queued && current() && is.function(on_bid_updated)) {
            event <- if (!success) "refresh" else if (action == "accept") "offer_accepted" else "offer_rejected"
            tryCatch(on_bid_updated(action_type = event, player_id = context$player,
              new_bid_price = NA_real_, is_cancel = success), error = function(e) NULL)
          }
        }
        if (current()) {
          removeModal()
          message <- if (!success) result$message else if (action == "accept") paste(player_name, "sold successfully.")
            else paste("Offer rejected for", player_name)
          showNotification(message, type = if (success) "message" else "warning", duration = 8)
        }
        NULL
      })
    }
    observeEvent(input$submit_accept_offer, submit_received_offer("accept"))
    observeEvent(input$submit_reject_offer, submit_received_offer("reject"))

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
      ctx$season<-league_season_context(ctx$championship_id,summary$championship$season %||% summary$championship$seasonId,account_id=ctx$account_id)
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
    # The autonomous worker already publishes one calendar per league. Keep
    # this display interest league-scoped and independent of player Refresh.
    card_states$calendar<-card_read("card_fixture_calendar","get_finished_rounds",
      function(ctx)list(login=get_reactive_val(login_token),championship_id=ctx$championship_id),
      context=team_card_context,active=function() {
        ctx <- team_card_context()
        isTRUE(availability_mode()) && fm_context_known(ctx$competition_id) && fm_context_known(ctx$season)
      },refresh=function()0L)
    context_ready<-function()!identical(card_states$summary()$status,"loading")&&!identical(card_states$lineup()$status,"loading")
    card_states$points<-card_read("card_points","read_card_round_history",function(ctx)list(context=ctx),context=history_context,active=context_ready)
    card_states$valuation<-card_read("card_valuation","read_card_price_history",function(ctx)list(context=ctx),context=history_context,active=context_ready)
    # Offer editors use only the existing worker replay. A cold card and an
    # explicit offer opening never request a second team acquisition check.
    card_states$capacity <- reactive({
      ctx <- availability_team_context()
      if (is.null(ctx)) return(resource_state(status = "unverified"))
      saved <- availability_stored("get_team_acquisition_capacity", ctx, auth_args(ctx))
      saved %||% resource_state(status = "unverified")
    })
    worker_capacity_verification_available <- function() !is.null(availability_team_context())
    observeEvent(input$btn_verify_availability, {
      if (isTRUE(availability_mode())) return()
      if (player_card_is_own_player(selected_player(), get_reactive_val(user_team_id))) {
        if (!player_availability_valid(card_states$listings()$data, "listings")) attr(card_states$listings, "retry")()
      } else if (!player_target_ready()) {
        attr(card_states$summary, "ensure")()
      }
    })
    card_states$listings<-player_availability_source(session,"listings",availability_team_context,availability_login,
      active=function()!is.null(card_context())&&player_card_is_own_player(selected_player(),get_reactive_val(user_team_id)),refresh=global_refresh,
      stored=function(){ctx<-availability_team_context();availability_stored("get_my_market_players",ctx,if(!is.null(ctx))auth_args(ctx)else list())},
      current_context=availability_team_context,mode=availability_mode,opening=availability_opening)
    card_started<-reactiveVal(Sys.time());timed_summary<-FALSE;timed_history<-FALSE;timed_availability<-FALSE
    card_chart_ready<-reactiveVal(FALSE)
    card_paint_generation<-reactiveVal(0L)
    chart_context<-NULL
    chart_data<-reactiveVal(NULL)
    chart_mounted<-reactiveVal(FALSE)
    observeEvent(card_context(),{
      current<-card_context()
      if(identical(current,chart_context)) return()
      chart_context<<-current
      chart_data(NULL)
      chart_mounted(FALSE)
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
      if(!isTRUE(chart_mounted()))return(NULL)
      plotly::plotlyOutput(ns("player_trend_plot"),height="340px")
    })
    card_round_details <- reactive({
      history <- card_history()
      ctx <- history$context %||% list()
      evidence <- player_card_fixture_evidence(card_states$matches()$data, card_states$calendar()$data, ctx)
      rounds <- build_player_recent_rounds(history$points, evidence$matches, ctx, limit=Inf)
      list(rounds=rounds$rounds, unassociated=rounds$unassociated,
        dates=player_trend_points_dates(history$points, rounds$rounds, evidence$dates))
    })
    output$player_chart_info <- renderUI({
      sp<-selected_player();req(sp)
      if(!isTRUE(card_chart_ready()))return(fm_info("Loading chart…",pending=TRUE))
      player_trend_information(card_history(),sp)
    })
    # Publish only chart inputs: availability/status changes must not redraw the
    # widget. Keep the last curve while a refresh has no replacement data.
    observe({
      req(card_chart_ready())
      history<-card_history()
      dates<-card_round_details()$dates
      previous<-isolate(chart_data())
      has_data <- nrow(history$points) > 0L || nrow(history$valuation) > 0L
      # A cold card with empty history never loads the chart library just to
      # draw an empty plot. Keep a mounted chart while a refresh is pending.
      if (!has_data && (is.null(previous) || identical(history$status, "loading"))) return()
      chart_data(list(history=list(points=history$points,valuation=history$valuation,
        context=history$context,status="ready"),dates=dates))
      chart_mounted(TRUE)
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
        points$date <- data$dates
        points$calendar_date <- attr(data$dates, "round_calendar")
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

      points_dates <- points$date
      dated_points <- nrow(points)>0L && all(!is.na(points_dates))
      separate_round_axis <- nrow(values)>0L && nrow(points)>0L && !dated_points
      chart <- plotly::plot_ly()
      if (nrow(values)) chart <- chart %>% plotly::add_trace(data=values,type="scatter",mode="lines",x=~date,y=~value,
        name="Valuation",yaxis="y",line=list(color="#00ff88",width=2.5,dash="solid"),
        text=~display_date,hovertemplate="%{text}<br>Valuation: %{y:,.0f} EUR<extra></extra>")
      if (nrow(points)) {
        points$plot_x <- if(dated_points)fm_plot_datetime(points_dates) else points$round_number
        points$label <- paste0("Round ",points$round_number,"<br>Points: ",points$points,
          if(dated_points)paste0("<br>",ifelse(points$calendar_date,"Round calendar: ",""),fm_format_datetime(points_dates))else "",
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
      rounds <- card_round_details()
      rounds$rounds <- head(rounds$rounds, 50L)
      rounds$unassociated <- head(rounds$unassociated, 50L)
      notice <- player_round_history_notice(history, rounds, match_state)
      tags$details(class="player-recent-rounds",
        tags$summary(span(icon("futbol"), " Recent rounds"), icon("chevron-down")),
        div(class="player-round-boxes",lapply(rounds$rounds,function(round){
          div(class="player-match-card player-round-box",
            div(class="player-round-heading",span(paste("Round",round$round_number)),
              strong(if(is.finite(round$points))paste0(round$points," pts")else "Points unavailable")),
            if(nrow(round$fixtures))lapply(seq_len(nrow(round$fixtures)),function(i)render_match(round$fixtures[i,,drop=FALSE])))
        })),
        if(!length(rounds$rounds))fm_info(if(identical(history$status,"loading"))"Loading round points…"else recent_round_points_empty_text(sp$points),error=!identical(history$status,"loading"),pending=identical(history$status,"loading")),
        if(is.null(card_states$summary()$data)&&!identical(card_states$summary()$status,"loading"))fm_info(
          if(isTRUE(availability_mode())&&is.null(card_states$summary()$error))"Player history has not been saved yet."else
          resource_failure_message(card_states$summary()$error),error=!is.null(card_states$summary()$error)),
        if(length(notice))fm_info(paste(notice, collapse=" "),error=TRUE),
        if(nrow(rounds$unassociated))tagList(
          tags$h5("Other matches"),
          div(class="player-round-boxes",lapply(seq_len(nrow(rounds$unassociated)),function(i){
            match<-rounds$unassociated[i,,drop=FALSE]
            div(class="player-match-card player-round-box",if(is.finite(match$round))span(class="player-match-round",paste("Round",match$round)),render_match(match))
          }))),
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

    output$player_points_description_box <- renderUI({
      sp <- selected_player()
      req(sp)
      points <- fm_number(sp$points)
      tagList(tags$dt("Total points"), tags$dd(if (is.finite(points)) points else "Unavailable"))
    })

    output$player_last_points_description_box <- renderUI({
      sp <- selected_player()
      req(sp)
      total <- fm_number(sp$average.total)
      average <- fm_number(sp$average.averageLastFive)
      tagList(tags$dt("Last 5 matches"),
        tags$dd(if (is.finite(total)) total else "Unavailable",
          if (is.finite(average)) tags$small(paste("Avg", round(average, 1)))))
    })

    output$player_value_description_box <- renderUI({
      sp <- selected_player()
      req(sp)
      change <- fm_number(sp$change)
      percent <- fm_number(sp$change_by_value) * 100
      change_text <- if (is.finite(change)) add_sign(player_card_money(change)) else "Change unavailable"
      if (is.finite(percent)) change_text <- paste0(change_text, " (", round(percent, 2), "%)")
      direction <- if (is.finite(change) && change != 0) {
        if (change > 0) "player-value-up" else "player-value-down"
      }
      tagList(tags$dt("Market value"),
        tags$dd(player_card_money(sp$value), tags$small(class = direction, change_text)))
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

# Use an associated player fixture first. Calendar dates identify the round
# for chart alignment and carry separate hover provenance.
player_trend_points_dates <- function(points, rounds, calendar_dates=data.frame()) {
  dates <- fm_time(points$date %||% rep(NA_character_,nrow(points)))
  from_calendar <- rep(FALSE, length(dates))
  if (anyNA(dates)) for (round in rounds) {
    indices <- which(points$round_number==round$round_number & is.na(dates))
    if (!length(indices)) next
    fixture_dates <- fm_time(round$fixtures$occurred_at %||% character())
    fixture_dates <- fixture_dates[!is.na(fixture_dates)]
    if (length(fixture_dates)) dates[indices] <- max(fixture_dates)
  }
  if (nrow(calendar_dates) && anyNA(dates)) {
    for (i in which(is.na(dates))) {
      id <- fm_scalar(points$round_id[i], "")
      candidates <- which(if (fm_context_known(id)) calendar_dates$round_id == id else
        calendar_dates$round_number == points$round_number[i])
      if (length(candidates) != 1L || !is.finite(calendar_dates$date[candidates])) next
      dates[i] <- as.POSIXct(calendar_dates$date[candidates], origin="1970-01-01", tz="UTC")
      from_calendar[i] <- TRUE
    }
  }
  attr(dates, "round_calendar") <- from_calendar
  dates
}

player_round_history_notice <- function(history, rounds, match_state) {
  if (identical(match_state$status, "loading")) return(character())
  notice <- character()
  missing <- sum(vapply(rounds$rounds, function(round) !nrow(round$fixtures), logical(1)))
  unassociated <- nrow(rounds$unassociated)
  if (missing || unassociated) {
    notice <- c(notice,
      if (missing == 1L) "One round has no verified player fixture." else
        if (missing) sprintf("%d rounds have no verified player fixture.", missing),
      if (unassociated == 1L) "One match record has no verified points link." else
        if (unassociated) sprintf("%d match records have no verified points link.", unassociated))
  }
  if (!is.null(match_state$error)) notice <- c(notice, resource_failure_message(match_state$error))
  if (nrow(history$valuation) && any(is.na(rounds$dates) & is.finite(history$points$points)))
    notice <- c(notice,"The points chart uses a separate round axis because some dates are unavailable.")
  notice
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
player_trend_information <- function(history, player) {
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
  if(!length(notes))return(NULL)
  fm_info(lapply(notes,shiny::tags$p),error=TRUE,class="player-history-status")
}
