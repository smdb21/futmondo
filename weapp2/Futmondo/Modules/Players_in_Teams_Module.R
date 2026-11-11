library(reactable)
players_in_teams_UI <- function(id) {
  ns <- NS(id)
  tagList(tags$style(HTML(".fm-team-panels{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:20px;margin-bottom:20px}.fm-team-panels>div{min-width:0}.fm-team-panels .fm-incoming-panel{height:100%;margin-bottom:0}.fm-offer-source{display:inline-flex;align-items:center;gap:5px;border:1px solid var(--fm-border,#315540);border-radius:6px;padding:3px 7px;overflow-wrap:anywhere}.fm-offer-rival{color:var(--fm-warning,#f0c75e);border-color:currentColor;font-weight:600}.fm-team-offer{display:flex;flex-direction:column;gap:7px;padding:12px 0;border-top:1px solid var(--fm-border,#315540)}.fm-team-offer-heading{display:flex;justify-content:space-between;gap:10px;flex-wrap:wrap}.fm-team-offer-heading a{overflow-wrap:anywhere}@media(max-width:767px){.fm-team-panels{grid-template-columns:minmax(0,1fr);gap:12px}}.fm-incoming-panel{border:1px solid var(--fm-border,#315540);border-radius:8px;padding:16px;margin-bottom:20px}.fm-incoming-panel h3{margin-top:0}.fm-incoming-player{display:flex;gap:16px;justify-content:space-between;align-items:center;padding:12px 0;border-top:1px solid var(--fm-border,#315540)}.fm-incoming-identity{display:flex;gap:10px;align-items:center;flex-wrap:wrap;min-width:0}.fm-incoming-identity a{overflow-wrap:anywhere}.fm-incoming-arrival{display:flex;flex-direction:column;gap:4px;text-align:right;min-width:0}.fm-incoming-round{font-weight:600}.fm-incoming-schedule{color:var(--fm-muted)}.fm-incoming-error{color:var(--fm-warning,#f0c75e);overflow-wrap:anywhere}@media(max-width:600px){.fm-incoming-player{display:flex;flex-direction:column;align-items:stretch;gap:8px}.fm-incoming-arrival{text-align:left}.fm-incoming-panel{padding:12px}}")),
    tags$div(class="fm-team-panels",uiOutput(ns("incoming_players")),uiOutput(ns("outgoing_players")),
      tags$div(style="grid-column:1/-1",uiOutput(ns("received_offers")))),
    uiOutput(ns("team_value_box")),
    actionButton(ns("btn_put_all_on_market"), "Put All Players on Market", icon=icon("tags")),
    players_table_UI(ns("players_table_in_teams"), box_title="Players in Team",
      filter_by_team=FALSE, filter_by_value=FALSE, filter_by_change_value=FALSE,
      filter_by_is_favorite=FALSE, filter_by_is_from_futmondo=FALSE,
      show_position_breakdown=TRUE, hide_bid_column=FALSE))
}
players_in_teams_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger=NULL, round_catalog=NULL, league_settings_source=NULL) {
  moduleServer(id, function(input, output, session) {
    bind_factual_reads(c("get_players_from_team","get_my_market_players"),session,login_token,championship_id,user_team_id,
      is_active=is_module_active,refresh_trigger=refresh_trigger)

    if(is.function(refresh_trigger))observeEvent(refresh_trigger(), {
      auth<-login_token();req(valid_login(auth),championship_id(),user_team_id())
      invalidate_saved_roster_offers(resource_context(auth,championship_id(),user_team_id()))
      invalidate_saved_clause_transfers(resource_context(auth,championship_id(),user_team_id()))
    },ignoreInit=TRUE,priority=110)

    roster_state_reader <- factual_read_adapter(session,"get_players_from_team",login_token,championship_id,user_team_id,
      is_active=is_module_active,refresh_trigger=refresh_trigger,state_only=TRUE)
    roster_state <- reactive({
      roster_state_reader(login=login_token(),championship_id=championship_id(),user_team_id=user_team_id(),teams=NULL)
    })
    offers_state <- reactive(team_received_offers_state(roster_state()))
    output$received_offers <- renderUI({
      req(is_module_active());team_received_offers_panel(offers_state(),session$ns)
    })

    ns <- session$ns
    get_reactive_val <- function(x) if (is.function(x)) x() else x
    current_context <- reactive(list(user=fm_scalar(login_token()[["userid"]]), championship=fm_scalar(championship_id()), team=fm_scalar(user_team_id())))
    incoming <- incoming_clause_source(session,login_token,championship_id,user_team_id,is_module_active,refresh_trigger)
    incoming_selected <- reactiveVal(NULL)
    incoming_context <- reactiveVal(NULL)
    incoming_requested <- reactiveVal(NULL)
    output$incoming_players <- renderUI({
      req(is_module_active())
      rounds<-if(is.function(round_catalog))tryCatch(round_catalog(),error=function(e)data.frame())else data.frame()
      incoming_clause_panel(incoming(),next_round_context(rounds),ns)
    })
    output$outgoing_players <- renderUI({
      req(is_module_active())
      transfers<-incoming()
      rounds<-if(is.function(round_catalog))tryCatch(round_catalog(),error=function(e)data.frame())else data.frame()
      outgoing_clause_panel(list(status=transfers$outgoing_status %||% transfers$status,
        records=transfers$outgoing_records %||% outgoing_clause_empty(),error=transfers$outgoing_error %||% transfers$error),next_round_context(rounds),ns)
    })
    # Register click handlers once per row identity; remove handlers for old rows.
    incoming_handlers<-list()
    observe({
      req(is_module_active());rows<-incoming()$records
      for(handler in incoming_handlers)handler$destroy()
      incoming_handlers<<-lapply(as.character(rows$player_id),function(player_id) {
        observeEvent(input[[paste0("incoming_open_",player_id)]],{
          current<-incoming()$records
          record<-current[current$player_id%in%player_id,,drop=FALSE]
          req(nrow(record)==1L)
          incoming_requested(player_id)
          incoming_context(current_context())
          # Preserve the full cached source row for the existing player card.
          summary<-incoming_clause_cached_summary(login_token(),championship_id(),user_team_id(),record$player_id)
          card_row<-incoming_clause_card_snapshot(record,summary)
          incoming_selected(NULL)
          showModal(modalDialog(selected_player_UI(ns("incoming_card")),
            tags$script(HTML(paste0("$(document).off('hidden.bs.modal.fmIncoming','#shiny-modal').on('hidden.bs.modal.fmIncoming','#shiny-modal',function(){Shiny.setInputValue('",ns("incoming_closed"),"',Math.random(),{priority:'event'});});"))),
            footer=modalButton("Close"),easyClose=TRUE,size="l"))
          session$onFlushed(function(){
            if(identical(isolate(current_context()),isolate(incoming_context()))&&identical(isolate(incoming_requested()),player_id))
              incoming_selected(card_row)
          },once=TRUE)
        },ignoreInit=TRUE)
      })
    })
    outgoing_handlers<-list()
    observe({
      req(is_module_active());rows<-incoming()$outgoing_records %||% outgoing_clause_empty()
      for(handler in outgoing_handlers)handler$destroy()
      outgoing_handlers<<-lapply(as.character(rows$player_id),function(player_id) {
        observeEvent(input[[paste0("outgoing_open_",player_id)]],{
          current<-incoming()$outgoing_records
          record<-current[current$player_id%in%player_id,,drop=FALSE]
          req(nrow(record)==1L)
          key<-paste0("outgoing:",player_id)
          incoming_requested(key);incoming_context(current_context())
          summary<-incoming_clause_cached_summary(login_token(),championship_id(),user_team_id(),record$player_id)
          card_row<-outgoing_clause_card_snapshot(record,summary)
          incoming_selected(NULL)
          showModal(modalDialog(selected_player_UI(ns("incoming_card")),
            tags$script(HTML(paste0("$(document).off('hidden.bs.modal.fmIncoming','#shiny-modal').on('hidden.bs.modal.fmIncoming','#shiny-modal',function(){Shiny.setInputValue('",ns("incoming_closed"),"',Math.random(),{priority:'event'});});"))),
            footer=modalButton("Close"),easyClose=TRUE,size="l"))
          session$onFlushed(function(){
            if(identical(isolate(current_context()),isolate(incoming_context()))&&identical(isolate(incoming_requested()),key))
              incoming_selected(card_row)
          },once=TRUE)
        },ignoreInit=TRUE)
      })
    })
    offer_handlers <- list()
    observe({
      req(is_module_active());rows<-offers_state()$records
      for(handler in offer_handlers)handler$destroy()
      offer_handlers<<-lapply(seq_len(nrow(rows)),function(i) {
        row<-rows[i,,drop=FALSE];key<-team_received_offer_input(row)
        observeEvent(input[[key]],{
          current<-offers_state()$records
          record<-current[vapply(seq_len(nrow(current)),function(j)identical(team_received_offer_input(current[j,,drop=FALSE]),key),logical(1)),,drop=FALSE]
          req(nrow(record)==1L)
          roster<-roster_state()$data
          card_row<-roster[as.character(roster$id)%in%record$id,,drop=FALSE];req(nrow(card_row)==1L)
          for(field in c("bid_price","bid_user","bid_id","bidder_team_id","bid_source"))card_row[[field]]<-record[[field]]
          incoming_requested(key);incoming_context(current_context());incoming_selected(NULL)
          showModal(modalDialog(selected_player_UI(ns("incoming_card")),
            tags$script(HTML(paste0("$(document).off('hidden.bs.modal.fmIncoming','#shiny-modal').on('hidden.bs.modal.fmIncoming','#shiny-modal',function(){Shiny.setInputValue('",ns("incoming_closed"),"',Math.random(),{priority:'event'});});"))),
            footer=modalButton("Close"),easyClose=TRUE,size="l"))
          session$onFlushed(function(){
            if(identical(isolate(current_context()),isolate(incoming_context()))&&identical(isolate(incoming_requested()),key))
              incoming_selected(card_row)
          },once=TRUE)
        },ignoreInit=TRUE)
      })
    })
    observeEvent(input$incoming_closed,{incoming_selected(NULL);incoming_requested(NULL)})
    selected_player_Server("incoming_card",reactive({
      if(!isTRUE(is_module_active()))return(NULL)
      if(!identical(incoming_context(),current_context()))return(NULL)
      incoming_selected()
    }),login_token,championship_id,user_team_id,refresh_trigger=refresh_trigger,
      on_bid_updated=function(...)if(is.function(refresh_trigger))refresh_trigger(isolate(refresh_trigger())+1L))
    bulk_context <- reactiveVal(NULL)
    observeEvent(current_context(), { bulk_context(NULL) }, ignoreNULL=FALSE, priority=100)
    players_table_RV <- reactive({
      req(is_module_active() == TRUE)
      req(login_token())
      req(championship_id())
      req(user_team_id())
      if (!is.null(refresh_trigger)) refresh_trigger() # Cache invalidation dependency
      championship_id <- championship_id()
      user_team_id <- user_team_id()
      players_table <- get_players_from_team(
        login = login_token(),
        championship_id = championship_id,
        user_team_id = user_team_id,
        teams = NULL
      )
      players_table <- preserve_observation_time(players_table)
      players_table <- players_table %>%
        translate_player_positions()
      players_table <- players_table %>%
        calculate_player_changes()
      players_table <- players_table %>%
        unify_columns()

      # Ensure market_inMarket column is always present
      if (!"market_inMarket" %in% colnames(players_table)) {
        players_table$market_inMarket <- rep(FALSE, nrow(players_table))
      }
      # Reconcile only against the authenticated manager's own listings.
      my_mkt_df <- tryCatch({
        get_my_market_players(
          login = login_token(),
          championship_id = championship_id,
          user_team_id = user_team_id
        )
      }, error = function(e) {
        print(paste0("[Market check] Fetch warning: ", e$message))
        NULL
      })
      if (!is.null(my_mkt_df) && "id" %in% colnames(my_mkt_df) &&
          is.null(attr(my_mkt_df,"fetch_error")) && fm_scalar(attr(my_mkt_df,"fetch_status"),"ok")%in%c("ok","empty")) {
        mkt_ids <- as.character(my_mkt_df$id)
        players_table$market_inMarket <- as.character(players_table$id) %in% mkt_ids
      }

      players_table <- team_selling_prices(players_table,my_mkt_df)
      players_table <- preserve_observation_time(players_table)

      return(players_table)
    })

    output$team_value_box <- renderUI({
      req(is_module_active());roster <- players_table_RV()
      values <- suppressWarnings(as.numeric(roster$value))
      total <- if (length(values) && all(is.finite(values))) player_card_money(sum(values)) else "Unavailable"
      arrivals<-incoming();count<-nrow(arrivals$records)
      incoming_text<-if(identical(arrivals$status,"loading")) {
        if(count)paste0(count," incoming (checking)")else "checking incoming"
      }else if(identical(arrivals$status,"partial")) {
        if(count)paste0(count," incoming · verification incomplete")else "incoming signings need verification"
      }else paste0(count," incoming")
      shinydashboard::valueBox(total, paste0(nrow(roster)," players in your squad · ",incoming_text), icon=icon("users"), color="green", width=12)
    })
    # ---- Put All Players on Market Modal ----
    output$bulk_listing_preview <- renderUI({
      req(!is.null(bulk_context()), identical(bulk_context(), current_context()))
      mode <- if (identical(input$bulk_listing_price_mode, "closed_clause")) "closed_clause" else "value"
      roster <- players_table_RV()
      plan <- bulk_market_listing_plan(roster, mode,
        premium_pct = input$bulk_listing_clause_premium)
      bulk_market_listing_preview(plan, roster)
    })
    observeEvent(input$btn_put_all_on_market, {
      bulk_context(current_context())
      showModal(modalDialog(
        title = tagList(icon("tags"), " Put All Players on Market"),
        p("Choose how to price every player. Listings are sent one by one and only verified successes are refreshed."),
        radioButtons(ns("bulk_listing_price_mode"), "Listing price", choices = c(
          "Current market value" = "value",
          "Closed clause plus premium" = "closed_clause"
        ), selected = "value"),
        numericInput(ns("bulk_listing_clause_premium"), "Premium above selected price (%)",
          value = 0, min = 0, max = 1000, step = "any", width = "100%"),
        tags$div(style = "display:flex;flex-wrap:wrap;gap:6px;margin-bottom:16px;",
          `aria-label` = "Choose a premium",
          lapply(c(0.5, 1, 2, 4, 5, 10), function(pct) tags$button(
            type = "button", class = "btn btn-default", paste0("+", pct, "%"),
            `data-target-input` = ns("bulk_listing_clause_premium"), `data-premium` = pct,
            onclick = "var field=document.getElementById(this.dataset.targetInput);field.value=this.dataset.premium;field.dispatchEvent(new Event('change',{bubbles:true}));"))),
        p("Choose a button or type a percentage. Use 0% to list at the selected price. Prices are rounded up to whole euros."),
        uiOutput(ns("bulk_listing_preview"), `aria-live` = "polite"),
        p(style = "color: var(--fm-muted); font-size: 12px;", "The clause option skips players without a valid closed clause. Other users and Futmondo can bid on listed players."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("submit_put_all_on_market"), "Confirm Market Listing", class = "btn btn-offer-money")
        ),
        easyClose = TRUE,
        size = "m"
      ))
    })

    # ---- Submit Put All Players on Market ----
    observeEvent(input$submit_put_all_on_market, {
      expected <- isolate(bulk_context())
      if (is.null(expected) || !identical(expected, current_context())) {
        showNotification("Team context changed. Open the listing confirmation again.", type="warning")
        return()
      }
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(login, champ_id, team_id)

      mode <- if (identical(as.character(input$bulk_listing_price_mode), "closed_clause")) "closed_clause" else "value"
      roster <- tryCatch(players_table_RV(), error = function(e) NULL)
      plan <- bulk_market_listing_plan(roster, mode = mode,
        premium_pct = input$bulk_listing_clause_premium)
      if (!nrow(plan$entries)) {
        shiny::showNotification(plan$message, type = "warning", duration = 6)
        return()
      }

      bulk_context(NULL) # Consume the confirmation only after valid pricing; block duplicate submissions.
      outcomes <- lapply(seq_len(nrow(plan$entries)), function(i) {
        player <- plan$entries[i, , drop = FALSE]
        tryCatch({
          if (isTRUE(player$already_listed)) {
            update_player_market_listing(login, champ_id, team_id, player$player_id, player$price)
          } else {
            put_player_on_market(login, champ_id, team_id, player$player_id, player$price)
          }
        }, error = function(e) list(success = FALSE, code = "error", message = conditionMessage(e)))
      })
      succeeded <- vapply(outcomes, function(x) if (is.list(x)) isTRUE(x$success) else isTRUE(x), logical(1))
      success_count <- sum(succeeded)
      failure_count <- length(succeeded) - success_count
      removeModal()

      if (success_count > 0L) {
        invalidate_manual_resources(login,champ_id,team_id)
        if (!is.null(refresh_trigger) && is.function(refresh_trigger)) {
          tryCatch(refresh_trigger(refresh_trigger() + 1), error = function(e) NULL)
        }
      }
      detail <- if (plan$skipped > 0L) paste0(" ", plan$skipped, " player(s) skipped because no eligible price was available.") else ""
      if (failure_count == 0L) {
        shiny::showNotification(paste0(success_count, " player(s) listed successfully.", detail), type = "message", duration = 6)
      } else {
        shiny::showNotification(paste0(success_count, " player(s) listed; ", failure_count, " failed.", detail, " Refresh to review the failed listings."), type = "warning", duration = 8)
      }
    })


    selected <- players_table_Server("players_table_in_teams", players_table_RV, user_teams_RV,
      login_token, championship_id, user_team_id, hide_bid_column=FALSE, refresh_trigger=refresh_trigger,
      is_module_active=is_module_active,own_squad=TRUE,league_settings_source=league_settings_source)
    return(selected)
  })
}
# One numeric asking price for My Team; raw fields remain player-card metadata.
team_selling_prices <- function(roster,listings=NULL) {
  prices <- rep(NA_real_,nrow(roster))
  for(field in intersect(c("market_asking_price","effective_market_price","market_price","price"),names(roster))) {
    candidate<-suppressWarnings(as.numeric(as.character(roster[[field]])))
    use<-!is.finite(prices)&is.finite(candidate)&candidate>0;prices[use]<-candidate[use]
  }
  listed<-if("market_inMarket"%in%names(roster))as.logical(roster$market_inMarket)else rep(NA,nrow(roster))
  listings_verified<-is.data.frame(listings)&&is.null(attr(listings,"fetch_error"))&&
    fm_scalar(attr(listings,"fetch_status"),"ok")%in%c("ok","empty")
  if(isTRUE(listings_verified)&&!nrow(listings))listed[]<-FALSE
  if(isTRUE(listings_verified)&&"id"%in%names(listings)) {
    index<-match(as.character(roster$id),as.character(listings$id));listed<-!is.na(index)
    listing_prices<-rep(NA_real_,nrow(roster))
    for(field in intersect(c("price","market_price","effective_market_price"),names(listings))) {
      candidate<-suppressWarnings(as.numeric(as.character(listings[[field]][index])))
      use<-!is.finite(listing_prices)&is.finite(candidate)&candidate>0;listing_prices[use]<-candidate[use]
    }
    use<-is.finite(listing_prices);prices[use]<-listing_prices[use]
  }
  prices[is.na(listed)|!listed]<-NA_real_;roster$selling_by<-prices;roster
}

team_received_offer_input <- function(row) paste0("offer_open_",digest::digest(
  as.list(row[1,intersect(c("id","bid_id","bid_user","bidder_team_id"),names(row)),drop=FALSE]),algo="xxhash64"))

team_received_offers_state <- function(roster_state) {
  roster<-roster_state$data
  rows<-attr(roster,"received_offers") %||% received_offers_empty()
  # Resolve names from the same roster; an old-context player is never displayed.
  if(is.data.frame(roster)&&"id"%in%names(roster)) {
    rows<-rows[rows$id%in%as.character(roster$id),,drop=FALSE]
    index<-match(rows$id,as.character(roster$id))
    rows$player_name<-if("name"%in%names(roster))as.character(roster$name[index])else rows$id
    missing<-is.na(rows$player_name)|!nzchar(rows$player_name);rows$player_name[missing]<-rows$id[missing]
    rows$acquisition_cost<-vapply(index,function(i)current_player_acquisition_cost(roster[i,,drop=FALSE],NULL,NULL),numeric(1))
  }else {
    rows$player_name<-character(nrow(rows))
    rows$acquisition_cost<-rep(NA_real_,nrow(rows))
  }
  status<-roster_state$status %||% "unavailable"
  error<-roster_state$error %||% attr(roster,"received_offers_error")
  if(status%in%c("ok","empty"))status<-attr(roster,"received_offers_status") %||% "ok"
  if(!is.null(error)&&status%in%c("ok","empty"))status<-"partial"
  list(records=rows,status=status,error=error,observed_at=attr(roster,"received_offers_observed_at") %||% roster_state$observed_at)
}

team_received_offers_panel <- function(state,ns) {
  rows<-state$records;verified<-state$status%in%c("ok","empty")
  rival_count<-sum(rows$bid_source%in%"rival")
  shiny::tags$section(class="fm-incoming-panel fm-offers-panel",`aria-labelledby`=ns("offers_heading"),
    shiny::tags$h3(id=ns("offers_heading"),if(verified)paste0("Offers for your players (",nrow(rows),")")else "Offers for your players"),
    if(rival_count)shiny::tags$p(class="fm-offer-rival",shiny::icon("user"),paste0(" ",rival_count," rival offer",if(rival_count==1L)""else "s")),
    if(identical(state$status,"loading"))shiny::tags$p(role="status",shiny::icon("spinner",class="fa-spin")," Checking received offers…"),
    if(!verified&&!identical(state$status,"loading"))shiny::tags$p(role="alert",class="fm-incoming-error",
      if(identical(state$status,"stale"))"Showing previously observed offers; current offers could not be verified. "else "Received offers could not be fully verified. ",
      if(!is.null(state$error))resource_failure_message(state$error)else "Refresh to retry."),
    if(verified&&!nrow(rows))shiny::tags$p("No offers for your players."),
    lapply(seq_len(nrow(rows)),function(i) {
      row<-rows[i,,drop=FALSE];rival<-identical(row$bid_source,"rival");system<-identical(row$bid_source,"system")
      amount<-if(is.finite(row$bid_price))format_table_sale_price(row$bid_price,row$acquisition_cost)else "Amount unavailable"
      shiny::tags$div(class="fm-team-offer",
        shiny::tags$div(class="fm-team-offer-heading",if(nzchar(row$bid_id))shiny::actionLink(ns(team_received_offer_input(row)),row$player_name)else shiny::tags$span(row$player_name),shiny::tags$strong(amount)),
        shiny::tags$div(shiny::tags$span(class=paste("fm-offer-source",if(rival)"fm-offer-rival"else ""),
          shiny::icon(if(rival)"user"else if(system)"robot"else "circle-question"),
          if(rival)paste0("Rival · ",row$bid_user)else if(system)"Futmondo system"else "Unknown bidder")))
    }))
}

bulk_market_listing_plan <- function(roster, mode = c("value", "closed_clause"), clause_premium_pct = 5,
                                     now = Sys.time(), premium_pct = if (mode == "closed_clause") clause_premium_pct else 0) {
  mode <- match.arg(mode)
  empty <- data.frame(player_id = character(), price = numeric(), already_listed = logical(),
    base_price = numeric(), increase = numeric(), stringsAsFactors = FALSE)
  premium <- suppressWarnings(as.numeric(premium_pct))
  if (length(premium) != 1L || !is.finite(premium) || premium < 0 || premium > 1000)
    return(list(entries = empty, skipped = 0L, message = "Enter a premium between 0 and 1000%."))
  if (!is.data.frame(roster) || !nrow(roster) || !"id" %in% names(roster))
    return(list(entries = empty, skipped = 0L, message = "Your squad is unavailable. Refresh before listing players."))
  ids <- trimws(as.character(roster$id))
  listed <- if ("market_inMarket" %in% names(roster)) as.logical(roster$market_inMarket) else rep(FALSE, nrow(roster))
  listed[is.na(listed)] <- FALSE
  if (mode == "value") {
    base <- if ("value" %in% names(roster)) suppressWarnings(as.numeric(as.character(roster$value))) else rep(NA_real_, nrow(roster))
    message <- "No players have a valid market value to list."
  } else {
    clauses <- if ("clause_price" %in% names(roster)) suppressWarnings(as.numeric(as.character(roster$clause_price))) else rep(NA_real_, nrow(roster))
    transferred <- if ("clause_transferred" %in% names(roster)) as.logical(roster$clause_transferred) else rep(NA, nrow(roster))
    dates <- if ("clause_date" %in% names(roster)) as.character(roster$clause_date) else rep(NA_character_, nrow(roster))
    parsed <- suppressWarnings(as.POSIXct(gsub("Z$", "", gsub("T", " ", dates)), tz = "UTC"))
    closed <- is.finite(clauses) & clauses > 0 & !is.na(transferred) & !transferred & !is.na(parsed) & parsed > now
    base <- ifelse(closed, clauses, NA_real_)
    message <- "No players have a valid closed clause to use for this listing method."
  }
  # Source prices are whole euros. Avoid multiplying by 1.10, which can turn
  # an exact 110 EUR into 110.000000000000014 and incorrectly round to 111.
  extra <- base * premium / 100
  # Decimal custom percentages can also land a few floating-point units above
  # an integer (10,000 * 0.07 / 100). Snap only machine-precision differences.
  nearest <- round(extra)
  exact <- is.finite(extra) & abs(extra - nearest) <= 4 * .Machine$double.eps * abs(extra)
  extra[exact] <- nearest[exact]
  prices <- ceiling(base) + ceiling(extra)
  keep <- !is.na(ids) & nzchar(ids) & is.finite(base) & base > 0 & is.finite(prices) & prices > 0
  entries <- data.frame(player_id = ids[keep], price = prices[keep], already_listed = listed[keep],
    base_price = base[keep], increase = prices[keep] - base[keep], stringsAsFactors = FALSE)
  list(entries = entries, skipped = as.integer(sum(!keep)), message = message)
}

bulk_market_listing_preview <- function(plan, roster) {
  entries <- plan$entries
  if (!nrow(entries)) return(shiny::tags$p(role = "alert", plan$message))
  names <- if ("name" %in% names(roster)) as.character(roster$name[match(entries$player_id, as.character(roster$id))]) else entries$player_id
  missing <- is.na(names) | !nzchar(trimws(names)); names[missing] <- entries$player_id[missing]
  amounts <- function(base, increase, price) shiny::tags$div(
    style = "display:flex;flex-wrap:wrap;gap:6px 20px;overflow-wrap:anywhere;",
    shiny::tags$span(paste0("Base: ", player_card_money(base))),
    shiny::tags$strong(paste0("Extra: +", player_card_money(increase))),
    shiny::tags$span(paste0("Listing: ", player_card_money(price))))
  shiny::tagList(
    shiny::tags$p(shiny::tags$strong(paste0(nrow(entries), " player(s) · Total increase: +", player_card_money(sum(entries$increase))))),
    amounts(sum(entries$base_price), sum(entries$increase), sum(entries$price)),
    if (plan$skipped > 0L) shiny::tags$p(paste0(plan$skipped, " player(s) skipped: no eligible price available.")),
    shiny::tags$div(style = "max-height:280px;overflow-y:auto;margin:12px 0;",
      lapply(seq_len(nrow(entries)), function(i) shiny::tags$div(
        style = "padding:8px 0;border-top:1px solid var(--fm-border,#315540);overflow-wrap:anywhere;",
        shiny::tags$strong(names[i]),
        amounts(entries$base_price[i], entries$increase[i], entries$price[i])))))
}

get_ordinal_position <- function(position) {
  position <- case_when(
    position == 1 ~ paste0(position, "st"),
    position == 2 ~ paste0(position, "nd"),
    position == 3 ~ paste0(position, "rd"),
    TRUE ~ paste0(position, "th")
  )
  return(position)
}

# Page-scoped queue: at most two summary reads, with the same account-scoped
# raw cache used by cards/preflight. Navigation never starts whole-league work.
incoming_clause_cached_summary <- function(login,championship_id,user_team_id,player_id) {
  context<-resource_context(login,championship_id,user_team_id,player_id)
  key<-api_cache_key(paste0("player_summary_",login[["userid"]],"_",championship_id,"_",user_team_id,"_",player_id),context=context)
  item<-api_cache_env[[key]]
  now<-Sys.time();stamp<-fm_time(attr(item$data,"observed_at"))
  value<-if(!is.null(item$data)&&!acquisition_data_unverified(item$data)&&resource_snapshot_unexpired(item$data,now)&&
     length(stamp)==1L&&!is.na(stamp)&&stamp<=now&&
     as.numeric(difftime(now,stamp,units="secs"))<resource_policy("summary")$ttl)item$data else
       cached_clause_transfer_summary(context)
  if(!is.null(value))persist_clause_transfer_summary(value,context)
  value
}

incoming_clause_source_unverified <- function(state) {
  is.null(state$data)||state$status%in%c("stale","partial","unavailable")||
    identical(state$freshness,"stale")||acquisition_data_unverified(state$data)
}
incoming_clause_prerequisite_failure <- function(roster,history) {
  states<-list(roster=roster,transactions=history)
  labels<-c(roster="Roster",transactions="Transaction history")
  messages<-character();failures<-list()
  for(key in names(states)) {
    state<-states[[key]]
    if(!incoming_clause_source_unverified(state))next
    error<-state$error %||% attr(state$data,"fetch_error")
    if(!is.null(error)) {
      failure<-resource_failure(error,if(key=="roster")"roster"else"pressroom")
      failures[[key]]<-failure
      messages<-c(messages,paste0(labels[[key]],": ",resource_failure_message(failure)))
    } else {
      reason<-if(identical(state$freshness,"stale")||identical(state$status,"stale")||identical(attr(state$data,"fetch_status"),"stale"))"is stale"else
        if(identical(state$status,"partial")||identical(attr(state$data,"fetch_status"),"partial"))"is incomplete"else"could not be loaded"
      messages<-c(messages,paste0(labels[[key]]," ",reason,". Refresh to retry."))
    }
  }
  list(message=paste("Clause transfers could not be verified.",paste(messages,collapse=" ")),failures=failures)
}

incoming_clause_source <- function(session,login_token,championship_id,user_team_id,is_active,refresh_trigger=NULL) {
  empty_state<-function()list(status="loading",records=incoming_clause_empty(),
    outgoing_status="loading",outgoing_records=outgoing_clause_empty(),error=NULL)
  state<-shiny::reactiveVal(empty_state())
  roster_read<-factual_read_adapter(session,"get_players_from_team",login_token,championship_id,user_team_id,
    is_active=is_active,refresh_trigger=refresh_trigger,state_only=TRUE)
  history_read<-factual_read_adapter(session,"get_championship_pressroom",login_token,championship_id,user_team_id,
    is_active=is_active,refresh_trigger=refresh_trigger,state_only=TRUE)
  generation<-0L;closed<-FALSE;was_active<-FALSE;signature<-NULL;owner<-NULL
  jobs<-list();queue<-character();summaries<-list();snapshot<-NULL;job_sequence<-0L
  publish<-function(loading=FALSE) {
    if(is.null(snapshot))return(invisible(NULL))
    prior<-shiny::isolate(state())
    result<-reconcile_pending_clause_transfers(snapshot$roster,snapshot$history,summaries,snapshot$team,
      previous_records=prior$records,previous_outgoing_records=prior$outgoing_records)
    rows<-result$incoming_records;leaving<-result$outgoing_records
    state(list(status=if(loading)"loading"else if(any(nzchar(rows$timing_error)))"partial"else "ok",
      records=rows,outgoing_status=if(loading)"loading"else result$outgoing_status,
      outgoing_records=leaving,outgoing_error=if(!loading&&identical(result$outgoing_status,"partial")&&!any(nzchar(leaving$timing_error)))
        "Clause departures could not be verified: a player's current owner or pending clause details could not be retrieved. Use Refresh to retry."else NULL,error=NULL))
    invisible(NULL)
  }
  pump<-function() {
    if(closed||!was_active||is.null(snapshot))return(invisible(NULL))
    while(length(jobs)<2L&&length(queue)) {
      pid<-queue[1];queue<<-queue[-1];job_sequence<<-job_sequence+1L
      job_id<-as.character(job_sequence);job_generation<-generation;captured<-snapshot
      jobs[[job_id]]<<-TRUE
      # A fresh cache read can settle synchronously without another request.
      cached<-incoming_clause_cached_summary(captured$login,captured$league,captured$team,pid)
      if(!is.null(cached)) {summaries[[pid]]<<-cached;jobs[[job_id]]<<-NULL;next}
      local({
        id<-job_id;player_id<-pid;version<-job_generation;ctx<-captured
        current<-function()!closed&&was_active&&identical(generation,version)
        finish<-function(value=NULL,rechecked=FALSE) {
          if(current()&&!is.null(value)&&!resource_snapshot_unexpired(value)) {
            if(!rechecked) {
              invalidate_resources(resource_context(ctx$login,ctx$league,ctx$team,player_id),pattern="^player_summary_|^clause_transfer_summary_")
              acquire("get_player_summary",rechecked=TRUE)
              return(invisible(NULL))
            }
            value<-observed_data(value,status="stale")
          }
          jobs[[id]]<<-NULL
          if(current()) {
            if(!is.null(value))persist_clause_transfer_summary(value,resource_context(ctx$login,ctx$league,ctx$team,player_id,session_id=session$token))
            summaries[player_id]<<-list(value);publish(length(queue)>0L||length(jobs)>0L)
          }
          pump();invisible(NULL)
        }
        acquire<-function(handler,rechecked=FALSE) {
          request<-tryCatch(async_adapter_fetch(handler,list(login=ctx$login,
            championship_id=ctx$league,user_team_id=ctx$team,player_id=player_id),
            context=resource_context(ctx$login,ctx$league,ctx$team,player_id,session_id=session$token),
            is_current=current,priority="interactive"),error=function(e)promises::promise_reject(e))
          promises::then(request,function(value)finish(value,rechecked),function(error)finish())
        }
        acquire("get_clause_transfer_summary")
      })
    }
    if(!length(queue)&&!length(jobs))publish()
    invisible(NULL)
  }
  observer<-shiny::observe({
    active<-isTRUE(is_active());auth<-login_token()
    context<-if(valid_login(auth))list(account=fm_scalar(auth[["userid"]]),league=fm_scalar(championship_id()),team=fm_scalar(user_team_id()))else NULL
    if(!identical(owner,context)) {
      generation<<-generation+1L;owner<<-context;signature<<-NULL;snapshot<<-NULL;queue<<-character();summaries<<-list()
      state(empty_state())
    }
    if(!active||is.null(context)) {
      if(was_active){generation<<-generation+1L;queue<<-character();signature<<-NULL}
      was_active<<-FALSE;return()
    }
    was_active<<-TRUE
    roster<-roster_read(login=auth,championship_id=context$league,user_team_id=context$team,teams=NULL)
    history<-history_read(login=auth,championship_id=context$league)
    refresh<-if(is.function(refresh_trigger))refresh_trigger()else 0L
    if(incoming_clause_source_unverified(roster)||incoming_clause_source_unverified(history)) {
      generation<<-generation+1L;signature<<-NULL;queue<<-character();snapshot<<-NULL
      previous<-shiny::isolate(state())
      loading<-roster$status%in%c("loading","refreshing")||history$status%in%c("loading","refreshing")
      problem<-if(loading)NULL else incoming_clause_prerequisite_failure(roster,history)
      state(list(status=if(loading)"loading"else "partial",records=previous$records,
        outgoing_status=if(loading)"loading"else "partial",outgoing_records=previous$outgoing_records,
        error=if(loading)NULL else problem$message,source_errors=if(loading)list()else problem$failures))
      return()
    }
    previous<-shiny::isolate(state())
    candidates<-pending_clause_candidates(roster$data,history$data,context$team,previous$records,previous$outgoing_records)
    if(is.null(candidates)) {
      generation<<-generation+1L;signature<<-NULL;queue<<-character();snapshot<<-NULL
      previous<-shiny::isolate(state());state(list(status="partial",records=previous$records,
        outgoing_status="partial",outgoing_records=previous$outgoing_records,
        error="Clause transfers could not be verified: roster or transaction identities/timestamps are malformed. Use Refresh to retry."));return()
    }
    next_signature<-as.character(openssl::sha256(serialize(list(context,roster$data$id,candidates,
      attr(roster$data,"observed_at"),attr(history$data,"observed_at"),refresh),NULL)))
    if(identical(signature,next_signature))return()
    signature<<-next_signature;generation<<-generation+1L
    snapshot<<-list(login=auth,league=context$league,team=context$team,roster=roster$data,history=history$data)
    ids<-unique(as.character(candidates$player_id));summaries<<-list()
    for(pid in ids){cached<-incoming_clause_cached_summary(auth,context$league,context$team,pid);if(!is.null(cached))summaries[[pid]]<<-cached}
    queue<<-setdiff(ids,names(summaries));publish(length(queue)>0L||length(jobs)>0L);pump()
  })
  session$onSessionEnded(function(){closed<<-TRUE;generation<<-generation+1L;queue<<-character();snapshot<<-NULL;summaries<<-list();observer$destroy()})
  shiny::reactive(state())
}

incoming_clause_panel <- function(state,next_round,ns) {
  clause_transfer_panel(state,next_round,ns)
}

outgoing_clause_panel <- function(state,next_round,ns) {
  clause_transfer_panel(state,next_round,ns,outgoing=TRUE)
}

clause_transfer_panel <- function(state,next_round,ns,outgoing=FALSE) {
  rows<-state$records
  prefix<-if(outgoing)"outgoing"else"incoming"
  title<-if(outgoing)"Leaving players"else"Incoming players"
  timing<-if(isTRUE(next_round$available))paste0("Round ",next_round$round_number," starts ",incoming_clause_time(next_round$starts_at)," (Spain)")else "Next round schedule is loading or could not be verified."
  shiny::tags$section(class=paste("fm-incoming-panel",if(outgoing)"fm-outgoing-panel"),`aria-labelledby`=ns(paste0(prefix,"_heading")),
    shiny::tags$h3(id=ns(paste0(prefix,"_heading")),if(!nrow(rows)&&!identical(state$status,"ok"))title else paste0(title," (",nrow(rows),")")),
    shiny::tags$p(class="fm-incoming-schedule",timing),
    if(identical(state$status,"loading"))shiny::tags$p(role="status",shiny::icon("spinner",class="fa-spin"),
      if(outgoing)" Checking clause departures…"else" Checking incoming signings…"),
    if(!is.null(state$error))shiny::tags$p(role="alert",class="fm-incoming-error",state$error),
    if(!nrow(rows)&&identical(state$status,"ok"))shiny::tags$p(if(outgoing)"No players leaving through a release clause."else"No incoming clause signings."),
    lapply(seq_len(nrow(rows)),function(i) {
      row<-rows[i,,drop=FALSE];at<-if(outgoing)row$leaves_at else row$joins_at
      label<-incoming_clause_round_label(at,next_round)
      logo<-get_team_image_name(row$team,logo=row$logo)
      roles<-analytics_player_positions(data.frame(role=row$role),multiposition=TRUE)[[1]]
      role_names<-c(GK="Goalkeeper",DEF="Defender",MID="Midfielder",FWD="Forward")
      role<-paste(unname(role_names[roles]),collapse=", ")
      position<-get_reactable_columns_for_players(data.frame(role=role))$role$cell(role)
      shiny::tags$div(class=paste("fm-incoming-player",if(outgoing)"fm-outgoing-player"),
        shiny::tags$div(class="fm-incoming-identity",
          if(nzchar(logo))shiny::tags$img(src=paste0(TEAM_LOGO_URL,logo,".png"),alt="",title=row$team,width=24,height=24,loading="lazy",onerror="this.style.display='none';"),
          shiny::actionLink(ns(paste0(prefix,"_open_",row$player_id)),row$player_name),position),
        shiny::tags$div(class="fm-incoming-arrival",
          shiny::tags$strong(if(!is.na(at))paste0(if(outgoing)"Leaves: "else"Joins: ",incoming_clause_time(at)," (Spain)")else
            if(identical(state$status,"loading"))paste0("Confirming ",if(outgoing)"departure"else"arrival"," time…")else
              paste0(if(outgoing)"Departure"else"Arrival"," time could not be verified")),
          if(nzchar(label))shiny::tags$span(class="fm-incoming-round",label),
          if(isTRUE(row$overdue))shiny::tags$span(paste0("Scheduled time has passed; ",if(outgoing)"departure"else"arrival"," is awaiting source confirmation.")),
          if(nzchar(row$timing_error)&&!identical(state$status,"loading"))shiny::tags$span(class="fm-incoming-error",row$timing_error)))
    }))
}


incoming_clause_card_snapshot <- function(record,summary=NULL) {
  clause_transfer_card_snapshot(record,summary)
}

outgoing_clause_card_snapshot <- function(record,summary=NULL) {
  clause_transfer_card_snapshot(record,summary)
}

clause_transfer_card_snapshot <- function(record,summary=NULL) {
  fallback<-list(id=record$player_id,name=record$player_name,
    role=record$role,team=record$team,logo=record$logo)
  source_id<-if(is.list(summary$data))fm_scalar(summary$data$id %||% summary$data$`_id`,"")else ""
  # A response for another player cannot supply either profile or ownership.
  if(nzchar(source_id)&&!identical(source_id,as.character(record$player_id)))summary<-NULL
  data<-if(is.list(summary$data))summary$data else fallback
  data$id<-as.character(record$player_id)
  data$name<-fm_scalar(data$name,as.character(record$player_name))
  for(field in c("role","team","logo"))data[[field]]<-fm_scalar(data[[field]],as.character(record[[field]]))
  row<-translate_player_positions(parse_player_json(data))
  championship<-if(is.list(summary$championship))summary$championship else list()
  owner<-if(is.list(championship$owner))championship$owner else list()
  owner_id<-fm_scalar(owner$`_id` %||% owner$id,fm_scalar(record$seller_team_id,""))
  # Summary data is profile data: its championship block owns identity and
  # clause status. Pending transfers retain their observed current ownership.
  for(field in c("owner_team_id","user_team_id","userteamId","userTeamId",
                 "userTeam._id","userTeam.id","userteam._id","userteam.id"))row[[field]]<-owner_id
  row$userTeam<-fm_scalar(owner$name,"")
  row$clause_transferred<-TRUE
  clause<-championship$clause
  if(is.list(clause))for(field in names(clause))row[[paste0("clause_",field)]]<-clause[[field]]
  row
}
