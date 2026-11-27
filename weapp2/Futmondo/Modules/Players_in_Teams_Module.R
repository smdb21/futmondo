library(reactable)
players_in_teams_UI <- function(id) {
  ns <- NS(id)
  tagList(tags$style(HTML(".fm-team-panels{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:20px;margin-bottom:20px}.fm-team-panels>div{min-width:0}.fm-team-panels .fm-incoming-panel{height:100%;margin-bottom:0}.fm-offer-rival{color:var(--fm-warning,#f0c75e);font-weight:600}@media(max-width:767px){.fm-team-panels{grid-template-columns:minmax(0,1fr);gap:12px}}.fm-incoming-panel{border:1px solid var(--fm-border,#315540);border-radius:8px;padding:16px;margin-bottom:20px}.fm-incoming-panel h3{margin-top:0}.fm-incoming-player{display:flex;gap:16px;justify-content:space-between;align-items:center;padding:12px 0;border-top:1px solid var(--fm-border,#315540)}.fm-incoming-identity{display:flex;gap:10px;align-items:center;flex-wrap:wrap;min-width:0}.fm-incoming-identity a{overflow-wrap:anywhere}.fm-incoming-arrival{display:flex;flex-direction:column;gap:4px;text-align:right;min-width:0}.fm-incoming-schedule{color:var(--fm-muted)}.fm-incoming-error{color:var(--fm-warning,#f0c75e);overflow-wrap:anywhere}
    .fm-transfer-group + .fm-transfer-group { margin-top: 12px; }
    .fm-transfer-group > h4 {
      margin: 0 0 4px;
      padding: 5px 8px;
      border-inline-start: 4px solid var(--fm-muted);
      border-radius: 4px;
      background: var(--fm-bg);
      color: var(--fm-text);
      font-size: 14px;
      font-weight: 700;
      line-height: 1.35;
    }
    .fm-transfer-group[data-timing='before'] > h4 {
      border-inline-start-color: var(--fm-text);
      background: var(--fm-highlight);
    }
    .fm-transfer-group[data-timing='kickoff'] > h4 {
      border-inline-start-color: var(--fm-accent);
      background: color-mix(in srgb, var(--fm-surface), var(--fm-accent) 14%);
    }
    .fm-transfer-group[data-timing='after'] > h4 {
      border-inline-start-color: var(--fm-warning);
      background: color-mix(in srgb, var(--fm-surface), var(--fm-warning) 17%);
      color: var(--fm-warning);
    }
    .fm-transfer-group .fm-incoming-player { padding: 6px 0; }
    .fm-transfer-group > h4 + .fm-incoming-player { border-top: 0; }
    .fm-transfer-time { display: grid; grid-template-columns: minmax(0,max-content); gap: 0 4px; align-items: center; justify-content: end; }
    .fm-transfer-time > .fm-info { display: contents; }
    .fm-transfer-time > .fm-info > summary { grid-column: 2; grid-row: 1; }
    .fm-transfer-time > .fm-info > .fm-info-body { grid-column: 1 / -1; grid-row: 2; }
    @media (max-width: 600px) {
      .fm-incoming-player { flex-direction: column; align-items: stretch; gap: 8px; }
      .fm-transfer-group .fm-incoming-player { gap: 2px; }
      .fm-incoming-arrival { text-align: left; }
      .fm-transfer-time { justify-content: start; }
      .fm-incoming-panel { padding: 12px; }
    }")),
    uiOutput(ns("team_value_box")),
    actionButton(ns("btn_put_all_on_market"), "Put All Players on Market", icon=icon("tags")),
    tags$div(class="fm-team-panels",uiOutput(ns("incoming_players")),uiOutput(ns("outgoing_players")),
      uiOutput(ns("nonfit_players"), class="fm-squad-status-output"),
      tags$div(style="grid-column:1/-1",uiOutput(ns("received_offers")))),
    players_table_UI(ns("players_table_in_teams"), box_title="Players in Team",
      filter_by_team=FALSE, filter_by_value=FALSE, filter_by_change_value=FALSE,
      filter_by_is_favorite=FALSE, filter_by_is_from_futmondo=FALSE,
      show_position_breakdown=TRUE, hide_bid_column=FALSE))
}
team_value_summary <- function(team_state, roster_state) {
  roster <- roster_state$data
  roster_statuses <- c(roster_state$status, attr(roster, "worker_acquisition_status"), attr(roster, "fetch_status"))
  complete_roster <- all(roster_statuses %in% c("ok", "empty", "stale", "loading")) &&
    !any(attr(roster, "worker_completeness") %in% c("partial", "incomplete", "unavailable")) &&
    is.finite(effective_squad_count(roster, character(), character()))
  known_column <- function(column) {
    if (!complete_roster) return(NULL)
    values <- suppressWarnings(as.numeric(as.character(roster[[column]])))
    if (length(values) == nrow(roster) && all(is.finite(values))) values else NULL
  }
  status <- function(amount, state) {
    if (is.finite(amount)) {
      stale <- identical(state$freshness, "stale") || identical(attr(state$data, "fetch_status"), "stale") ||
        !is.null(state$error)
      if (stale) "stale" else "ready"
    } else if (identical(state$status, "loading")) "loading" else "unavailable"
  }

  info <- team_state$data
  value <- fm_number(info$teamValue, fm_number(info$team_value))
  value_source <- team_state
  if (!is.finite(value)) {
    values <- known_column("value")
    if (!is.null(values)) {
      value <- sum(values)
      value_source <- roster_state
    } else if (identical(roster_state$status, "loading")) {
      value_source <- roster_state
    }
  }
  changes <- known_column("change")
  trend <- if (is.null(changes)) NA_real_ else sum(changes)
  list(value = value, value_status = status(value, value_source),
    trend = trend, trend_status = status(trend, roster_state),
    up = if (is.null(changes)) NA_integer_ else sum(changes > 0),
    down = if (is.null(changes)) NA_integer_ else sum(changes < 0))
}

team_direction_count <- function(direction, count, label, count_label = NULL, missing_label = NULL) {
  tags$div(class = paste0("fm-team-trend-", direction), title = label,
    icon(paste0("arrow-", direction)),
    tags$span(class = "sr-only", paste0(label, " ")),
    if (is.na(count)) missing_label %||% tagList(
      tags$span(class = "sr-only", "Unavailable"), tags$span(`aria-hidden` = "true", "—")) else count,
    if (!is.null(count_label)) tags$span(`aria-hidden` = "true", count_label))
}

team_trend_counts <- function(summary, show_count_labels = FALSE) {
  tags$div(class = "fm-team-trend-counts",
    team_direction_count("up", summary$up, "Players with positive trend", if (show_count_labels) "rising"),
    team_direction_count("down", summary$down, "Players with negative trend", if (show_count_labels) "falling"))
}

team_trend_panel <- function(summary, label = "Total trend", counts = team_trend_counts(summary)) {
  amount <- if (identical(summary$trend_status, "loading")) "Loading…" else player_card_money(summary$trend)
  tags$div(class = "fm-team-value-trend",
    tags$div(title = "24-hour change in squad market value",
      if (identical(summary$trend_status, "stale")) "Last saved trend" else label,
      tags$strong(paste0(if (isTRUE(summary$trend > 0)) "+" else "", amount))),
    counts)
}

team_value_summary_panel <- function(summary, ns) {
  tags$section(class = "fm-team-value-summary", `aria-labelledby` = ns("team_value_label"),
    tags$div(class = "fm-team-value-total",
      tags$span(id = ns("team_value_label"), "Team value"),
      tags$strong(if (identical(summary$value_status, "loading")) "Loading…" else player_card_money(summary$value)),
      if (identical(summary$value_status, "stale")) tags$small("Last saved")),
    team_trend_panel(summary))
}

players_in_teams_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger=NULL, round_catalog=NULL, league_settings_source=NULL, clause_transfers_source=NULL, team_info_source=NULL) {
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
    offers_source_time <- resource_display_source(session,function(){
      req(is_module_active())
      state<-offers_state()
      if(isTRUE(state$observations_only))paste("Observed:",fm_format_datetime(state$observed_at))else ""
    })
    output$received_offers_source_time <- renderText(offers_source_time())
    shiny::outputOptions(output,"received_offers_source_time",suspendWhenHidden=FALSE)
    offers_panel_display <- resource_display_source(session,function(){
      req(is_module_active())
      state <- offers_state()
      state$observed_at <- NULL
      state
    })
    output$received_offers <- renderUI(team_received_offers_panel(offers_panel_display(), session$ns,
      source_note=textOutput(session$ns("received_offers_source_time"), inline=TRUE)))

    ns <- session$ns
    get_reactive_val <- function(x) if (is.function(x)) x() else x
    current_context <- reactive(list(user=fm_scalar(login_token()[["userid"]]), championship=fm_scalar(championship_id()), team=fm_scalar(user_team_id())))
    incoming <- if(is.function(clause_transfers_source))clause_transfers_source else
      incoming_clause_source(session,login_token,championship_id,user_team_id,is_module_active,refresh_trigger)
    departure_state <- resource_display_source(session,function(){
      req(is_module_active())
      rounds<-if(is.function(round_catalog))tryCatch(round_catalog(),error=function(e)data.frame())else data.frame()
      clause_departure_round_state(incoming(),next_round_context(rounds))
    })
    departing_ids <- reactive(departure_state()$excluded_ids)
    incoming_selected <- reactiveVal(NULL)
    incoming_context <- reactiveVal(NULL)
    incoming_requested <- reactiveVal(NULL)
    incoming_panel_display<-resource_display_source(session,function(){
      req(is_module_active())
      rounds<-if(is.function(round_catalog))tryCatch(round_catalog(),error=function(e)data.frame())else data.frame()
      incoming_clause_panel(incoming(),next_round_context(rounds),ns)
    })
    outgoing_panel_display<-resource_display_source(session,function(){
      req(is_module_active())
      transfers<-incoming()
      rounds<-if(is.function(round_catalog))tryCatch(round_catalog(),error=function(e)data.frame())else data.frame()
      outgoing_clause_panel(list(status=transfers$outgoing_status %||% transfers$status,
        records=transfers$outgoing_records %||% outgoing_clause_empty(),error=transfers$outgoing_error %||% transfers$error,observations_only=isTRUE(transfers$observations_only)),next_round_context(rounds),ns)
    })
    output$incoming_players<-renderUI(incoming_panel_display())
    output$outgoing_players<-renderUI(outgoing_panel_display())
    open_team_player_card <- function(card_row, key) {
      force(card_row)
      incoming_requested(key);incoming_context(current_context());incoming_selected(NULL)
      showModal(modalDialog(selected_player_UI(ns("incoming_card")),
        tags$script(HTML(paste0("$(document).off('hidden.bs.modal.fmIncoming','#shiny-modal').on('hidden.bs.modal.fmIncoming','#shiny-modal',function(){Shiny.setInputValue('",ns("incoming_closed"),"',Math.random(),{priority:'event'});});"))),
        footer=modalButton("Close"),easyClose=TRUE,size="l"))
      session$onFlushed(function(){
        if(identical(isolate(current_context()),isolate(incoming_context()))&&identical(isolate(incoming_requested()),key))
          incoming_selected(card_row)
      },once=TRUE)
    }

    nonfit_roster <- reactive({
      if (!isTRUE(is_module_active())) return(NULL)
      team_nonfit_roster(roster_state()$data)
    })
    nonfit_panel_display <- resource_display_source(session, function() {
      team_nonfit_panel(nonfit_roster(), ns)
    })
    output$nonfit_players <- renderUI(nonfit_panel_display())
    # The empty grid item is hidden until there is a status to render.
    outputOptions(output, "nonfit_players", suspendWhenHidden=FALSE)
    nonfit_ids <- resource_display_source(session, function() as.character(nonfit_roster()$id))
    nonfit_handlers <- list()
    observeEvent(nonfit_ids(), {
      for (handler in nonfit_handlers) handler$destroy()
      nonfit_handlers <<- lapply(nonfit_ids(), function(player_id) {
        key <- paste0("nonfit_open_", player_id)
        observeEvent(input[[key]], {
          current <- nonfit_roster()
          row <- current[current$id %in% player_id, , drop=FALSE]
          req(is.data.frame(row), nrow(row)==1L)
          open_team_player_card(translate_player_positions(row), key)
        }, ignoreInit=TRUE)
      })
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
          # Preserve the full cached source row for the existing player card.
          summary<-incoming_clause_cached_summary(login_token(),championship_id(),user_team_id(),record$player_id)
          open_team_player_card(incoming_clause_card_snapshot(record,summary), player_id)
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
          summary<-incoming_clause_cached_summary(login_token(),championship_id(),user_team_id(),record$player_id)
          open_team_player_card(outgoing_clause_card_snapshot(record,summary), key)
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
          open_team_player_card(card_row, key)
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
      players_table <- team_selling_prices(players_table,my_mkt_df)
      players_table <- preserve_observation_time(players_table)

      return(players_table)
    })

    effective_squad<-resource_display_source(session,function(){
      req(is_module_active());team_effective_squad_count(players_table_RV(),incoming())
    })
    team_value_display <- resource_display_source(session, function() {
      req(is_module_active())
      info <- if (is.function(team_info_source)) team_info_source() else NULL
      team_value_summary(info, roster_state())
    })
    output$team_value_box <- renderUI(team_value_summary_panel(team_value_display(), ns))
    # ---- Put All Players on Market Modal ----
    output$bulk_listing_preview <- renderUI({
      req(input$btn_put_all_on_market, !is.null(bulk_context()), identical(bulk_context(), current_context()))
      mode <- if (identical(input$bulk_listing_price_mode, "closed_clause")) "closed_clause" else "value"
      roster <- bulk_market_roster(players_table_RV)
      plan <- bulk_market_listing_plan(roster, mode, premium_pct = input$bulk_listing_clause_premium)
      updateActionButton(session, "submit_put_all_on_market", disabled = !nrow(plan$entries))
      bulk_market_listing_preview(plan, roster)
    })
    observeEvent(input$btn_put_all_on_market, {
      bulk_context(current_context())
      showModal(modalDialog(
        title = tagList(icon("tags"), " Put All Players on Market"),
        radioButtons(ns("bulk_listing_price_mode"), "Base asking price", choices = c(
          "Current market value" = "value",
          "Closed clause value" = "closed_clause"
        ), selected = "value"),
        numericInput(ns("bulk_listing_clause_premium"), "Optional premium (%)",
          value = 0, min = 0, max = 1000, step = "any", width = "100%"),
        tags$div(style = "display:flex;flex-wrap:wrap;gap:6px;margin-bottom:16px;",
          `aria-label` = "Add to the premium", `data-target-input` = ns("bulk_listing_clause_premium"),
          onclick = "const button = event.target.closest('button[data-premium]');
            if (!button) return;
            const field = document.getElementById(this.dataset.targetInput);
            if (field.validity.badInput) return;
            const [mantissa, exponent = 0] = field.value.toLowerCase().split('e');
            const decimals = (mantissa.split('.')[1] || '').length - Number(exponent);
            const sum = Number(field.value) + Number(button.dataset.premium);
            field.value = Number(sum.toFixed(Math.min(100, Math.max(1, decimals))));
            field.dispatchEvent(new Event('change', {bubbles: true}));",
          lapply(c(0.5, 1, 2, 4, 5, 10), function(pct) tags$button(
            type = "button", class = "btn btn-default", `data-premium` = pct, paste0("+", pct, "%")))),
        p("Each press adds to the premium. You can also type a percentage or enter 0 to use the base price. Asking prices round up to whole euros."),
        uiOutput(ns("bulk_listing_preview"), `aria-live` = "polite"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("submit_put_all_on_market"), "Confirm Market Listing", class = "btn btn-offer-money", disabled = TRUE)
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
      roster <- bulk_market_roster(players_table_RV)
      plan <- bulk_market_listing_plan(roster, mode = mode,
        premium_pct = input$bulk_listing_clause_premium)
      if (!nrow(plan$entries)) {
        shiny::showNotification(paste(c(plan$eligibility, plan$exclusions, plan$message), collapse = " "),
          type = if (plan$error) "warning" else "message", duration = 6)
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
      requested <- length(succeeded)
      removeModal()

      if (success_count > 0L) {
        invalidate_manual_resources(login,champ_id,team_id)
        if (!is.null(refresh_trigger) && is.function(refresh_trigger)) {
          tryCatch(refresh_trigger(refresh_trigger() + 1), error = function(e) NULL)
        }
      }
      result <- sprintf("Futmondo confirmed %d of %d requested %s.", success_count, requested,
        ngettext(requested, "listing", "listings"))
      if (any(!succeeded)) {
        players <- bulk_market_player_names(plan$entries$player_id[!succeeded], roster)
        result <- paste(result, sprintf("Unconfirmed for %s.", paste(players, collapse = ", ")),
          "Check My Team before trying again.")
      }
      shiny::showNotification(paste(c(result, plan$exclusions), collapse = " "),
        type = if (all(succeeded)) "message" else "warning", duration = if (all(succeeded)) 6 else 12)
    })


    selected <- players_table_Server("players_table_in_teams", players_table_RV, user_teams_RV,
      login_token, championship_id, user_team_id, hide_bid_column=FALSE, refresh_trigger=refresh_trigger,
      is_module_active=is_module_active,own_squad=TRUE,league_settings_source=league_settings_source,
      departing_player_ids=departing_ids,effective_squad_source=effective_squad)
    return(selected)
  })
}
# Display the same identity ledger as purchase preflights, using already shared
# observations. This projection never grants eligibility to place a transaction.
team_effective_squad_count <- function(roster, transfers) {
  pending<-is.null(transfers)||any(attr(roster,"fetch_status")%in%c("loading","refreshing"))||
    isTRUE(transfers$status%in%c("loading","refreshing"))||
    isTRUE(transfers$outgoing_status%in%c("loading","refreshing"))
  unavailable<-function(detail)list(count=NA_integer_,status=if(pending)"loading"else"unavailable",detail=detail)
  if(pending)return(unavailable("Pending arrivals and departures are still loading."))
  if(!is.data.frame(roster)||any(attr(roster,"fetch_status")%in%c("partial","unavailable"))||
     !is.null(attr(roster,"fetch_error"))||(!is.null(attr(roster,"worker_acquisition_status"))&&!isTRUE(attr(roster,"worker_acquisition_status")%in%c("ok","empty")))||
     any(attr(roster,"worker_completeness")%in%c("partial","unavailable","incomplete")))
    return(unavailable("Current squad observation is incomplete or unavailable."))
  if(!isTRUE(transfers$status%in%c("ok","observed"))||!isTRUE(transfers$outgoing_status%in%c("ok","observed"))||
     !is.null(transfers$error)||!is.null(transfers$outgoing_error))
    return(unavailable(transfers$error %||% transfers$outgoing_error %||% "Pending transfer verification is incomplete."))
  incoming<-transfers$records;outgoing<-transfers$outgoing_records
  if(!is.data.frame(incoming)||!is.data.frame(outgoing)||!"player_id"%in%names(incoming)||!"player_id"%in%names(outgoing))
    return(unavailable("Pending arrival or departure identities are unavailable."))
  count<-effective_squad_count(roster,incoming$player_id,outgoing$player_id)
  if(is.na(count))return(unavailable("Squad or pending transfer identities are incomplete or inconsistent."))
  bought<-length(setdiff(unique(as.character(incoming$player_id)),as.character(roster$id)))
  leaving<-length(intersect(unique(as.character(outgoing$player_id)),as.character(roster$id)))
  list(count=count,status=if(isTRUE(transfers$observations_only))"observed"else"ok",
    detail=paste0(nrow(roster)," current + ",bought," bought awaiting arrival − ",leaving,
      " leaving = ",count,". Based on observed transfers; purchases are rechecked before submitting."))
}

# One numeric asking price for My Team; raw fields remain player-card metadata.
team_selling_prices <- function(roster,listings=NULL) {
  roster <- roster_market_listings(roster, listings)
  if (!"market_inMarket" %in% names(roster)) roster$market_inMarket <- rep(NA, nrow(roster))
  prices <- rep(NA_real_,nrow(roster))
  for(field in intersect(c("market_asking_price","effective_market_price","market_price","price"),names(roster))) {
    candidate<-suppressWarnings(as.numeric(as.character(roster[[field]])))
    use<-!is.finite(prices)&is.finite(candidate)&candidate>0;prices[use]<-candidate[use]
  }
  listed <- as.logical(roster$market_inMarket)
  if (!any(listed %in% TRUE)) roster$expirationDate <- NULL
  else if (is.null(roster$expirationDate)) roster$expirationDate <- rep(NA_character_, nrow(roster))
  prices[is.na(listed)|!listed]<-NA_real_;roster$selling_by<-prices;roster
}

# Count distinct squad players, including an unlisted player with a direct offer.
team_market_counts <- function(state, squad_size) {
  counts <- c(listed = NA_integer_, offered = NA_integer_)
  roster <- state$data
  if (!is.finite(squad_size) || !is.data.frame(roster) ||
      (incoming_clause_source_unverified(state) && !incoming_clause_observed_source(state))) return(counts)
  if (squad_size == 0L) return(c(listed = 0L, offered = 0L))
  flags <- as.logical(roster$market_inMarket)
  if (length(flags) == nrow(roster) && !anyNA(flags)) counts["listed"] <- sum(flags)
  offers <- attr(roster, "received_offers", exact = TRUE)
  offer_stamp <- fm_time(attr(roster, "received_offers_observed_at"))
  if ((!length(offer_stamp) || isTRUE(offer_stamp <= Sys.time())) &&
      isTRUE(attr(roster, "received_offers_status") %in% c("ok", "empty")) &&
      is.null(attr(roster, "received_offers_error")) && roster_offer_rows_valid(offers))
    counts["offered"] <- length(intersect(offers$id, roster$id))
  counts
}

team_received_offer_input <- function(row) paste0("offer_open_",digest::digest(
  as.list(row[1,intersect(c("id","bid_id","bid_user","bidder_team_id"),names(row)),drop=FALSE]),algo="xxhash64"))

team_received_offers_state <- function(roster_state) {
  roster<-roster_state$data
  source_rows<-attr(roster,"received_offers",exact=TRUE)
  rows_valid<-roster_offer_rows_valid(source_rows)
  rows<-if(rows_valid)source_rows else received_offers_empty()
  # Resolve names from the same roster; an old-context player is never displayed.
  if(is.data.frame(roster)&&"id"%in%names(roster)) {
    rows<-rows[rows$id%in%as.character(roster$id),,drop=FALSE]
    index<-match(rows$id,as.character(roster$id))
    rows$player_name<-if("name"%in%names(roster))as.character(roster$name[index])else rows$id
    missing<-is.na(rows$player_name)|!nzchar(rows$player_name);rows$player_name[missing]<-rows$id[missing]
    rows$player_club <- if ("team" %in% names(roster)) as.character(roster$team[index]) else rep("", nrow(rows))
    rows$player_logo <- if ("logo" %in% names(roster)) as.character(roster$logo[index]) else rep("", nrow(rows))
    players <- unique(index)
    costs <- vapply(players, function(i) current_player_acquisition_cost(roster[i,,drop=FALSE], NULL, NULL), numeric(1))
    rows$acquisition_cost <- costs[match(index, players)]
  }else {
    rows$player_name<-character(nrow(rows))
    rows$acquisition_cost<-rep(NA_real_,nrow(rows))
  }
  status<-roster_state$status %||% "unavailable"
  error<-roster_state$error %||% attr(roster,"received_offers_error")
  if(status%in%c("ok","empty"))status<-attr(roster,"received_offers_status") %||% "ok"
  if((!is.null(error)||(!is.null(source_rows)&&!rows_valid))&&status%in%c("ok","empty"))status<-"partial"
  stamp<-fm_time(attr(roster,"received_offers_observed_at") %||% roster_state$observed_at)
  # Expiry changes current-action eligibility, not the successful acquisition
  # recorded in the durable worker snapshot. Require offer-specific parser proof;
  # a missing offers attribute on a nonempty roster is never a verified zero.
  offer_status<-attr(roster,"received_offers_status")
  complete_offers<-isTRUE(offer_status%in%c("ok","empty"))&&rows_valid
  empty_roster<-is.data.frame(roster)&&nrow(roster)==0L&&is.null(source_rows)&&
    identical(attr(roster,"worker_acquisition_status"),"empty")&&is.null(offer_status)
  observations_only<-incoming_clause_observed_source(roster_state)&&is.null(error)&&
    (complete_offers||empty_roster)&&length(stamp)==1L&&!is.na(stamp)&&stamp<=Sys.time()
  if(observations_only)status<-"observed" else if(!is.null(attr(roster,"worker_acquisition_status"))&&status%in%c("ok","empty"))status<-"partial"
  list(records=rows,status=status,error=error,observed_at=stamp,observations_only=observations_only)
}

team_received_offers_table <- function(rows, ns) {
  rows <- rows[order(rows$bid_source %in% "system"), , drop=FALSE]
  bidder <- ifelse(rows$bid_source == "system", "Liga Radar",
    ifelse(rows$bid_source == "rival", paste0("Rival · ", rows$bid_user), "Unknown bidder"))
  offers <- data.frame(player=lineup_player_label(rows$player_name), offer=rows$bid_price,
    profit=sale_profit_amount(rows$bid_price, rows$acquisition_cost), bidder=bidder)
  amounts <- format_table_currency(offers$offer)
  amounts[is.na(offers$offer)] <- "—"
  icons <- lapply(c(rival="user", system="robot", unknown="circle-question"), shiny::icon)
  reactable::reactable(offers, elementId=ns("received_offers_table"), class="fm-received-offers-table",
    compact=TRUE, striped=TRUE, highlight=TRUE, showSortable=TRUE,
    defaultColDef=reactable::colDef(minWidth=64, vAlign="center", sortNALast=TRUE, na="—"),
    columns=list(
      player=reactable::colDef(name="Player", minWidth=78, rowHeader=TRUE, cell=function(value, index) {
        row <- rows[index, , drop=FALSE]
        club <- fm_scalar(row$player_club, "")
        logo <- fm_scalar(row$player_logo, "")
        if (!nzchar(logo)) logo <- get_team_image_name(club)
        crest <- lineup_fixture_crest_url(logo)
        # Send the exact offer event even after Reactable sorts or paginates the cells.
        label <- if (nzchar(row$bid_id)) shiny::tags$button(type="button", class="fm-offer-player-name",
          title=row$player_name, `aria-label`=row$player_name,
          onclick=paste0("Shiny.setInputValue(", jsonlite::toJSON(ns(team_received_offer_input(row)), auto_unbox=TRUE),
            ",true,{priority:'event'});"), value) else shiny::tags$span(class="fm-offer-player-name", title=row$player_name, value)
        shiny::tags$span(class="fm-offer-player",
          if (nzchar(crest)) shiny::tags$img(class="fm-team-crest", src=crest, alt=club,
            title=club, width=20, height=20, loading="lazy", decoding="async", onerror="this.style.display='none';"),
          label)
      }),
      offer=reactable::colDef(name="Offer", minWidth=92, align="right", defaultSortOrder="desc",
        cell=function(value, index) shiny::tags$span(title=amounts[index], amounts[index])),
      profit=reactable::colDef(name="Profit", minWidth=72, align="right", defaultSortOrder="desc",
        cell=function(value) {
          label <- format_table_sale_profit(value)
          shiny::tags$span(title=label, label)
        }),
      bidder=reactable::colDef(name="From", minWidth=92, cell=function(value, index) {
        source <- rows$bid_source[index]
        shiny::tags$span(class=if (source == "rival") "fm-offer-bidder fm-offer-rival" else "fm-offer-bidder",
          title=value, icons[[source]], " ", value)
      })))
}

team_received_offers_panel <- function(state,ns,source_note=NULL) {
  rows<-state$records;verified<-state$status%in%c("ok","empty")
  observed<-identical(state$status,"observed")&&isTRUE(state$observations_only)
  rival_count<-sum(rows$bid_source%in%"rival")
  shiny::tags$section(class="fm-incoming-panel fm-offers-panel",`aria-labelledby`=ns("offers_heading"),
    shiny::tags$h3(id=ns("offers_heading"),if(verified)paste0("Offers for your players (",nrow(rows),")")else if(observed)paste0("Offers for your players (",nrow(rows)," observed)")else "Offers for your players"),
    if(rival_count)shiny::tags$p(class="fm-offer-rival",shiny::icon("user"),paste0(" ",rival_count," rival offer",if(rival_count==1L)""else "s")),
    if(identical(state$status,"loading"))fm_info("Checking received offers…",pending=TRUE),
    if(observed)fm_info(source_note %||% paste("Observed:",fm_format_datetime(state$observed_at))),
    if(!verified&&!observed&&!identical(state$status,"loading"))fm_info(error=TRUE,
      if(identical(state$status,"stale"))"Showing previously observed offers; current offers could not be verified. "else "Received offers could not be fully verified. ",
      if(!is.null(state$error))resource_failure_message(state$error)else "Refresh to retry."),
    if(verified&&!nrow(rows))shiny::tags$p("No offers for your players."),
    if(observed&&!nrow(rows))shiny::tags$p("No offers observed."),
    if (nrow(rows)) team_received_offers_table(rows, ns))
}

bulk_market_roster <- function(read_roster) {
  tryCatch(read_roster(), `fm-loading` = function(e) structure(data.frame(), fetch_status = "loading"),
    error = function(e) NULL)
}

bulk_market_listing_plan <- function(roster, mode = c("value", "closed_clause"), clause_premium_pct = 5,
                                     now = Sys.time(), premium_pct = if (mode == "closed_clause") clause_premium_pct else 0) {
  mode <- match.arg(mode)
  empty <- data.frame(player_id = character(), price = numeric(), already_listed = logical(),
    base_price = numeric(), increase = numeric(), stringsAsFactors = FALSE)
  invalid <- function(message) list(entries = empty, skipped = 0L, message = message, error = TRUE)
  premium <- suppressWarnings(as.numeric(premium_pct))
  if (length(premium) != 1L || !is.finite(premium) || premium < 0 || premium > 1000)
    return(invalid("Enter a premium between 0 and 1000%."))
  if (any(attr(roster, "fetch_status") %in% c("loading", "refreshing")))
    return(list(entries = empty, skipped = 0L, message = "Checking squad eligibility…", error = FALSE))
  if (!is.data.frame(roster) || !"id" %in% names(roster) || !is.null(attr(roster, "fetch_error")) ||
      any(c(attr(roster, "fetch_status"), attr(roster, "worker_acquisition_status"),
        attr(roster, "worker_completeness")) %in% c("unavailable", "partial", "incomplete", "loading", "refreshing")) ||
      (!nrow(roster) && !any(attr(roster, "fetch_status") %in% c("empty", "ok"))))
    return(invalid("Your full squad is unavailable. Check My Team before listing players."))
  if (!nrow(roster))
    return(list(entries = empty, skipped = 0L, message = "Your squad is empty. No listings to submit.", error = FALSE))
  ids <- trimws(as.character(roster$id))
  if (anyDuplicated(ids[!is.na(ids) & nzchar(ids)]))
    return(invalid("Your squad contains duplicate player IDs. Check My Team before listing players."))
  field <- function(name, default) if (name %in% names(roster)) roster[[name]] else rep(default, nrow(roster))
  listed <- as.logical(field("market_inMarket", FALSE))
  listed[is.na(listed)] <- FALSE
  open <- transferring <- rep(FALSE, nrow(roster))
  if (mode == "value") {
    base <- suppressWarnings(as.numeric(as.character(field("value", NA_real_))))
  } else {
    base <- suppressWarnings(as.numeric(as.character(field("clause_price", NA_real_))))
    transferred <- as.logical(field("clause_transferred", NA))
    dates <- field("clause_date", NA_character_)
    no_lock <- is.na(dates) | !nzchar(trimws(as.character(dates)))
    # Only a valid source instant may authorize a closed-clause listing.
    if (!is.numeric(dates) && !inherits(dates, "POSIXt")) {
      dates <- as.character(dates)
      date_format <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}([T ]([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]([.][0-9]+)?(Z|[+-]([01][0-9]|2[0-3]):?[0-5][0-9])?)?$"
      dates[!grepl(date_format, dates)] <- NA_character_
    }
    until <- fm_time(dates)
    has_price <- is.finite(base) & base > 0
    transferring <- transferred %in% TRUE
    open <- has_price & transferred %in% FALSE & (no_lock | (is.finite(until) & until <= now))
    closed <- has_price & transferred %in% FALSE & is.finite(until) & until > now
    base[!closed] <- NA_real_
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
  eligible <- nrow(entries)
  total <- nrow(roster)
  missing <- sum(!keep & !open & !transferring)
  exclusions <- c(
    if (any(open)) sprintf(ngettext(sum(open), "Left out %d player with an open clause.",
      "Left out %d players with open clauses."), sum(open)),
    if (any(transferring)) sprintf(ngettext(sum(transferring), "Left out %d player with a transfer in progress.",
      "Left out %d players with transfers in progress."), sum(transferring)),
    if (missing) sprintf("Required listing details are unavailable for %d %s.", missing, ngettext(missing, "player", "players")))
  eligibility <- sprintf("%s pricing applies to %d of %d %s.",
    if (mode == "value") "Market-value" else "Closed-clause", eligible, total, ngettext(total, "player", "players"))
  list(entries = entries, skipped = total - eligible, eligibility = eligibility, exclusions = exclusions,
    message = if (!eligible) "No listings to submit.", error = FALSE)
}

bulk_market_player_names <- function(ids, roster) {
  if (!"name" %in% names(roster)) return(ids)
  names <- as.character(roster$name[match(ids, trimws(as.character(roster$id)))])
  missing <- is.na(names) | !nzchar(trimws(names))
  names[missing] <- ids[missing]
  names
}

bulk_market_listing_preview <- function(plan, roster) {
  entries <- plan$entries
  if (plan$error) return(fm_info(plan$message, error = TRUE, inline = TRUE))
  selection <- shiny::tagList(
    if (!is.null(plan$eligibility)) shiny::tags$p(shiny::tags$strong(plan$eligibility)),
    if (length(plan$exclusions)) shiny::tags$p(paste(plan$exclusions, collapse = " ")))
  if (!nrow(entries)) return(shiny::tagList(selection, shiny::tags$p(plan$message)))
  names <- bulk_market_player_names(entries$player_id, roster)
  amounts <- function(base, increase, price) shiny::tags$div(
    style = "display:flex;flex-wrap:wrap;gap:6px 20px;overflow-wrap:anywhere;",
    shiny::tags$span(paste0("Base: ", player_card_money(base))),
    shiny::tags$strong(paste0("Extra: +", player_card_money(increase))),
    shiny::tags$span(paste0("Listing: ", player_card_money(price))))
  shiny::tagList(
    selection,
    shiny::tags$p(shiny::tags$strong(paste0("Total increase: +", player_card_money(sum(entries$increase))))),
    amounts(sum(entries$base_price), sum(entries$increase), sum(entries$price)),
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
incoming_clause_cached_summary <- function(login,championship_id,user_team_id,player_id,is_current=NULL,owner='',context=NULL,allow_observed=FALSE,on_pending=NULL) {
  context<-context %||% resource_context(login,championship_id,user_team_id,player_id)
  key<-api_cache_key(paste0("player_summary_",login[["userid"]],"_",championship_id,"_",user_team_id,"_",player_id),context=context)
  item<-api_cache_env[[key]]
  now<-Sys.time();stamp<-fm_time(attr(item$data,"observed_at"))
  value<-if(!is.null(item$data)&&!acquisition_data_unverified(item$data)&&resource_snapshot_unexpired(item$data,now)&&
     length(stamp)==1L&&!is.na(stamp)&&stamp<=now&&
     as.numeric(difftime(now,stamp,units="secs"))<resource_policy("summary")$ttl)item$data else
       cached_clause_transfer_summary(context)
  if(exists("fm_worker_configured",mode="function")&&fm_worker_configured()) {
    args<-list(championship_id=championship_id,user_team_id=user_team_id,player_id=player_id)
    read_context<-if(any(!vapply(context[c("competition_id","season","scoring_version")],fm_context_known,logical(1))))
      fm_worker_resolve_context("get_clause_transfer_summary",args,context) else context
    d<-fm_worker_descriptor("get_clause_transfer_summary",args,read_context)
    s<-fm_worker_account_state(context$account_id)
    if(is.function(is_current))fm_worker_demand(d,owner,is_current)
    try(fm_worker_resource_revision(s,d$resource_key)(),silent=TRUE)
    saved<-fm_worker_get(context$account_id,d$resource_key)$data
    if(is.null(saved)&&is.null(value)&&is.function(on_pending)&&!isTRUE(s$descriptors[[d$resource_key]]$read_complete))
      on_pending(player_id)
    stamp<-fm_time(attr(saved,"observed_at"));previous<-fm_time(attr(value,"observed_at"))
    historical<-isTRUE(allow_observed)&&isTRUE(attr(saved,"worker_acquisition_status")%in%c("ok","empty"))&&is.null(attr(saved,"fetch_error"))
    if(!is.null(saved)&&((resource_snapshot_unexpired(saved,now)&&!acquisition_data_unverified(saved))||historical)&&
       length(stamp)==1L&&!is.na(stamp)&&stamp<=now&&
       (is.null(value)||length(previous)!=1L||is.na(previous)||stamp>=previous))value<-saved
  }
  if(!is.null(value)&&!isTRUE(fm_worker_mode(context$account_id))&&is.null(attr(value,"worker_acquisition_status")))
    persist_clause_transfer_summary(value,context)
  value
}

incoming_clause_context_matches<-function(roster,history){
  if(!is.list(roster$context)||!is.list(history$context))return(TRUE)
  fields<-c("account_id","championship_id","competition_id","season","scoring_version")
  identical(fm_worker_json(roster$context[fields]),fm_worker_json(history$context[fields]))
}

incoming_clause_observed_source<-function(state){
  data<-state$data;stamp<-fm_time(attr(data,"observed_at"))
  is.data.frame(data)&&!isTRUE(state$status%in%c("partial","unavailable"))&&is.null(state$error)&&is.null(attr(data,"fetch_error"))&&
    isTRUE(attr(data,"worker_acquisition_status")%in%c("ok","empty"))&&
    !isTRUE(attr(data,"worker_completeness")%in%c("partial","unavailable"))&&
    isTRUE((attr(data,"fetch_status") %||% "ok")%in%c("ok","empty","stale"))&&
    length(stamp)==1L&&!is.na(stamp)&&stamp<=Sys.time()
}
incoming_clause_source_times<-function(times){
  if(!is.list(times))return("")
  labels<-c(roster="Roster observed",transactions="Transactions observed")
  paste(vapply(names(labels),function(key){
    stamp<-fm_time(times[[key]])
    if(length(stamp)!=1L||is.na(stamp))return("")
    paste0(labels[[key]],": ",fm_format_datetime(stamp),".")
  },character(1)),collapse=" ")
}

incoming_clause_source_unverified <- function(state) {
  is.null(state$data)||state$status%in%c("stale","partial","unavailable")||
    identical(state$freshness,"stale")||acquisition_data_unverified(state$data)||
    (!is.null(attr(state$data,"worker_acquisition_status"))&&!incoming_clause_observed_source(state))
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
      reason<-if(state$status%in%c("loading","refreshing"))"is refreshing"else
        if(identical(state$status,"partial")||identical(attr(state$data,"fetch_status"),"partial")||isTRUE(attr(state$data,"worker_completeness")%in%c("partial","unavailable")))"is incomplete"else
        if(identical(state$freshness,"stale")||identical(state$status,"stale")||identical(attr(state$data,"fetch_status"),"stale"))"is stale"else"could not be loaded"
      messages<-c(messages,paste0(labels[[key]]," ",reason,if(reason=="is refreshing")"."else ". Refresh to retry."))
    }
  }
  list(message=paste("Clause transfers could not be verified.",paste(messages,collapse=" ")),failures=failures)
}

incoming_clause_source <- function(session,login_token,championship_id,user_team_id,is_active,refresh_trigger=NULL,prerequisite_readers=NULL) {
  empty_state<-function()list(status="loading",records=incoming_clause_empty(),
    outgoing_status="loading",outgoing_records=outgoing_clause_empty(),error=NULL)
  state<-shiny::reactiveVal(empty_state())
  roster_read<-prerequisite_readers$roster %||% factual_read_adapter(session,"get_players_from_team",login_token,championship_id,user_team_id,
    is_active=is_active,refresh_trigger=refresh_trigger,state_only=TRUE)
  history_read<-prerequisite_readers$transactions %||% factual_read_adapter(session,"get_championship_pressroom",login_token,championship_id,user_team_id,
    is_active=is_active,refresh_trigger=refresh_trigger,state_only=TRUE)
  generation<-0L;closed<-FALSE;was_active<-FALSE;signature<-NULL;owner<-NULL
  jobs<-list();queue<-character();summaries<-list();snapshot<-NULL;job_sequence<-0L;prerequisite_problem<-NULL
  summary_context<-function(ctx,pid)resource_context(ctx$login,ctx$league,ctx$team,pid,
    season=ctx$scope$season %||% "unknown",scoring_version=ctx$scope$scoring_version %||% "unknown",
    competition_id=ctx$scope$competition_id %||% "unknown",session_id=session$token)
  publish<-function(loading=FALSE) {
    if(is.null(snapshot))return(invisible(NULL))
    prior<-shiny::isolate(state())
    result<-reconcile_pending_clause_transfers(snapshot$roster,snapshot$history,summaries,snapshot$team,
      previous_records=prior$records,previous_outgoing_records=prior$outgoing_records,observed_only=isTRUE(snapshot$observed_only))
    rows<-result$incoming_records;leaving<-result$outgoing_records
    if(!is.null(prerequisite_problem)) {
      state(list(status="partial",records=rows,outgoing_status="partial",outgoing_records=leaving,
        error=prerequisite_problem$message,source_errors=prerequisite_problem$failures,observations_only=isTRUE(snapshot$observed_only),source_times=snapshot$source_times))
      return(invisible(NULL))
    }
    if(isTRUE(snapshot$observed_only)) {
      incomplete_out<-identical(result$observed_outgoing_status,"partial")
      state(list(status=if(any(nzchar(rows$timing_error)))"partial"else"observed",records=rows,
        outgoing_status=if(incomplete_out||any(nzchar(leaving$timing_error)))"partial"else"observed",
        incoming_loading=any(rows$player_id %in% snapshot$pending_summary_ids),
        outgoing_loading=any(snapshot$pending_summary_ids %in% snapshot$roster$id),
        outgoing_records=leaving,observations_only=TRUE,source_times=snapshot$source_times,error=NULL,
        outgoing_error=if(incomplete_out&&!any(nzchar(leaving$timing_error)))
          "Saved departure details are incomplete: a player's owner or pending clause details could not be retrieved."else NULL))
      return(invisible(NULL))
    }
    state(list(status=if(loading)"loading"else if(any(nzchar(rows$timing_error)))"partial"else "ok",
      records=rows,outgoing_status=if(loading)"loading"else result$outgoing_status,
      outgoing_records=leaving,outgoing_error=if(!loading&&identical(result$outgoing_status,"partial")&&!any(nzchar(leaving$timing_error)))
        "Clause departures could not be verified: a player's current owner or pending clause details could not be retrieved. Use Refresh to retry."else NULL,error=NULL))
    invisible(NULL)
  }
  pump<-function() {
    if(closed||!was_active||is.null(snapshot)||!is.null(prerequisite_problem))return(invisible(NULL))
    while(length(jobs)<2L&&length(queue)) {
      pid<-queue[1];queue<<-queue[-1];job_sequence<<-job_sequence+1L
      job_id<-as.character(job_sequence);job_generation<-generation;captured<-snapshot
      jobs[[job_id]]<<-TRUE
      # A fresh cache read can settle synchronously without another request.
      cached<-incoming_clause_cached_summary(captured$login,captured$league,captured$team,pid,context=summary_context(captured,pid))
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
            if(!is.null(value))persist_clause_transfer_summary(value,summary_context(ctx,player_id))
            summaries[player_id]<<-list(value);publish(length(queue)>0L||length(jobs)>0L)
          }
          pump();invisible(NULL)
        }
        acquire<-function(handler,rechecked=FALSE) {
          awaiting<-TRUE
          live<-function()awaiting&&current()
          attr(live,"fm_worker_interest")<-paste0("clause:",id,":",handler)
          request<-tryCatch(async_adapter_fetch(handler,list(login=ctx$login,
            championship_id=ctx$league,user_team_id=ctx$team,player_id=player_id),
            context=summary_context(ctx,player_id),
            is_current=live,priority="interactive"),error=function(e)promises::promise_reject(e))
          promises::then(request,function(value){awaiting<<-FALSE;finish(value,rechecked)},
            function(error){awaiting<<-FALSE;finish()})
        }
        acquire("get_clause_transfer_summary")
      })
    }
    if(!length(queue)&&!length(jobs))publish()
    invisible(NULL)
  }
  observer<-shiny::observe({
    active<-isTRUE(is_active());auth<-login_token()
    context<-if(valid_login(auth)){
      scope<-fm_worker_resolve_context("get_players_from_team",list(championship_id=championship_id(),user_team_id=user_team_id()),resource_context(auth,championship_id(),user_team_id()))
      list(account=fm_scalar(auth[["userid"]]),league=fm_scalar(championship_id()),team=fm_scalar(user_team_id()),scope=scope[fm_worker_fields])
    }else NULL
    if(!identical(owner,context)) {
      generation<<-generation+1L;owner<<-context;signature<<-NULL;snapshot<<-NULL;queue<<-character();summaries<<-list();prerequisite_problem<<-NULL
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
    context_mismatch<-!incoming_clause_context_matches(roster,history)||!incoming_clause_context_matches(roster,list(context=context$scope))
    # A successful stored observation is historical evidence after its short
    # action TTL expires. Display it as observed; only real acquisition/scope
    # failures use the error path. This path never starts a source request.
    observed_scope<-is.list(roster$context)&&is.list(history$context)&&
      identical(roster$context$account_id,context$account)&&identical(roster$context$championship_id,context$league)&&
      identical(roster$context$user_team_id,context$team)
    if(observed_scope&&!context_mismatch&&incoming_clause_observed_source(roster)&&incoming_clause_observed_source(history)) {
      previous<-shiny::isolate(state())
      observed_candidates<-pending_clause_candidates(roster$data,history$data,context$team,previous$records,previous$outgoing_records)
      if(is.null(observed_candidates)) {
        generation<<-generation+1L;queue<<-character();jobs<<-list()
        state(list(status="partial",records=previous$records,outgoing_status="partial",outgoing_records=previous$outgoing_records,
          observations_only=TRUE,source_times=snapshot$source_times,
          error="Saved transfer identities or timestamps are malformed; current clause details could not be verified."))
        return()
      }
      if(!is.null(observed_candidates)) {
        if(!isTRUE(snapshot$observed_only)){generation<<-generation+1L;queue<<-character();jobs<<-list()}
        captured<-list(login=auth,league=context$league,team=context$team,scope=context$scope,
          roster=roster$data,history=history$data,observed_only=TRUE,
          source_times=list(roster=attr(roster$data,"observed_at"),transactions=attr(history$data,"observed_at")))
        display_live<-local({version<-generation;function()!closed&&was_active&&identical(generation,version)})
        attr(display_live,"fm_worker_interest")<-"clause-display"
        ids<-unique(as.character(observed_candidates$player_id));summaries<<-summaries[intersect(names(summaries),ids)]
        pending_ids<-character()
        pending<-function(pid)pending_ids<<-c(pending_ids,pid)
        for(pid in ids){
          cached<-incoming_clause_cached_summary(auth,context$league,context$team,pid,display_live,session$token,summary_context(captured,pid),allow_observed=TRUE,on_pending=pending)
          if(!is.null(cached))summaries[[pid]]<<-cached
        }
        captured$pending_summary_ids<-setdiff(pending_ids,names(summaries))
        prerequisite_problem<<-NULL;snapshot<<-captured;publish(FALSE);return()
      }
    }
    if(context_mismatch||incoming_clause_source_unverified(roster)||incoming_clause_source_unverified(history)) {
      # Keep a valid in-flight batch through transient source expiry. Its
      # observations remain historical and the displayed prerequisite warning stays.
      previous<-shiny::isolate(state())
      loading<-roster$status%in%c("loading","refreshing")||history$status%in%c("loading","refreshing")
      problem<-if(context_mismatch)list(message="Transfer sources use different account, league or scoring contexts; waiting for matching observations.",failures=list())else incoming_clause_prerequisite_failure(roster,history)
      prerequisite_problem<<-problem
      initial_loading<-loading&&is.null(snapshot)
      state(list(status=if(initial_loading)"loading"else "partial",records=previous$records,
        outgoing_status=if(initial_loading)"loading"else "partial",outgoing_records=previous$outgoing_records,
        incoming_loading=loading&&!context_mismatch&&!length(problem$failures),
        outgoing_loading=loading&&!context_mismatch&&!length(problem$failures),
        error=if(initial_loading)NULL else problem$message,source_errors=if(initial_loading)list()else problem$failures,
        observations_only=isTRUE(snapshot$observed_only),source_times=snapshot$source_times))
      return()
    }
    prerequisite_problem<<-NULL
    previous<-shiny::isolate(state())
    candidates<-pending_clause_candidates(roster$data,history$data,context$team,previous$records,previous$outgoing_records)
    if(is.null(candidates)) {
      generation<<-generation+1L;signature<<-NULL;queue<<-character();snapshot<<-NULL
      previous<-shiny::isolate(state());state(list(status="partial",records=previous$records,
        outgoing_status="partial",outgoing_records=previous$outgoing_records,
        error="Clause transfers could not be verified: roster or transaction identities/timestamps are malformed. Use Refresh to retry."));return()
    }
    # Freshness timestamps are not a change to transfer evidence. Hash only the
    # relevant source facts, in stable row order and without frame metadata.
    roster_facts<-as.data.frame(roster$data)[,intersect(c("id","clause_transferred","clause_date","owner_teamid","user_team_id"),names(roster$data)),drop=FALSE]
    roster_facts<-roster_facts[order(as.character(roster_facts$id)),,drop=FALSE];rownames(roster_facts)<-NULL
    candidate_facts<-as.data.frame(candidates)[,intersect(c("id","player_id","buyer_team_id","seller_team_id","created","is_clause","price"),names(candidates)),drop=FALSE]
    candidate_facts<-candidate_facts[order(as.character(candidate_facts$player_id)),,drop=FALSE];rownames(candidate_facts)<-NULL
    for(field in setdiff(names(attributes(roster_facts)),c("names","row.names","class")))attr(roster_facts,field)<-NULL
    for(field in setdiff(names(attributes(candidate_facts)),c("names","row.names","class")))attr(candidate_facts,field)<-NULL
    next_signature<-as.character(openssl::sha256(serialize(list(context,roster_facts,candidate_facts,refresh),NULL)))
    display_live<-local({version<-generation;function()!closed&&was_active&&identical(generation,version)})
    attr(display_live,"fm_worker_interest")<-"clause-display"
    if(identical(signature,next_signature)) {
      for(pid in unique(as.character(candidates$player_id))) {
        cached<-incoming_clause_cached_summary(auth,context$league,context$team,pid,display_live,session$token,summary_context(list(login=auth,league=context$league,team=context$team,scope=context$scope),pid))
        if(!is.null(cached))summaries[[pid]]<<-cached
      }
      snapshot<<-list(login=auth,league=context$league,team=context$team,roster=roster$data,history=history$data,scope=context$scope)
      publish(length(queue)>0L||length(jobs)>0L);pump();return()
    }
    signature<<-next_signature;generation<<-generation+1L
    snapshot<<-list(login=auth,league=context$league,team=context$team,roster=roster$data,history=history$data,scope=context$scope)
    display_live<-local({version<-generation;function()!closed&&was_active&&identical(generation,version)})
    attr(display_live,"fm_worker_interest")<-"clause-display"
    ids<-unique(as.character(candidates$player_id));summaries<<-list()
    for(pid in ids){cached<-incoming_clause_cached_summary(auth,context$league,context$team,pid,display_live,session$token,summary_context(list(login=auth,league=context$league,team=context$team,scope=context$scope),pid));if(!is.null(cached))summaries[[pid]]<<-cached}
    queue<<-setdiff(ids,names(summaries));publish(length(queue)>0L||length(jobs)>0L);pump()
  })
  session$onSessionEnded(function(){closed<<-TRUE;generation<<-generation+1L;queue<<-character();snapshot<<-NULL;summaries<<-list();observer$destroy()})
  shiny::reactive(state())
}

team_panel_player_identity <- function(row, input_id, name) {
  team <- fm_scalar(row[["team"]], "")
  logo <- get_team_image_name(team, logo=row[["logo"]])
  roles <- analytics_player_positions(row, multiposition=TRUE)[[1]]
  role_names <- c(GK="Goalkeeper", DEF="Defender", MID="Midfielder", FWD="Forward")
  role <- paste(unname(role_names[roles]), collapse=", ")
  position <- get_reactable_columns_for_players(data.frame(role=role))$role$cell(role)
  shiny::tags$div(class="fm-incoming-identity",
    if (nzchar(logo)) shiny::tags$img(src=paste0(TEAM_LOGO_URL,logo,".png"), alt="", title=team,
      width=24,height=24,loading="lazy",onerror="this.style.display='none';"),
    shiny::actionLink(input_id, name), position)
}

team_nonfit_roster <- function(roster) {
  if (!is.data.frame(roster) || !all(c("id","status") %in% names(roster))) return(NULL)
  ids <- as.character(roster$id)
  codes <- unique(roster$status)
  # The yellow-card asset identifies the display status without establishing unavailability.
  codes <- codes[!tolower(trimws(as.character(codes))) %in% "yellowcard"]
  known <- !vapply(lapply(codes, player_status_icon, show_unconfirmed=FALSE), is.null, logical(1))
  usable <- !is.na(ids) & nzchar(trimws(ids)) & !duplicated(ids) & !duplicated(ids, fromLast=TRUE)
  roster[usable & roster$status %in% codes[known], , drop=FALSE]
}

team_nonfit_panel <- function(rows, ns) {
  if (!is.data.frame(rows) || !nrow(rows)) return(NULL)
  shiny::tags$section(class="fm-incoming-panel fm-squad-status-panel", `aria-labelledby`=ns("nonfit_heading"),
    shiny::tags$h3(id=ns("nonfit_heading"), paste0("Players not fit (", nrow(rows), ")")),
    lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, , drop=FALSE]
      badge <- player_status_icon(row$status)
      shiny::tags$div(class="fm-incoming-player",
        team_panel_player_identity(row, ns(paste0("nonfit_open_", row$id)), fm_scalar(row$name, "Player")),
        shiny::tags$strong(class="fm-squad-status-label",
          htmltools::tagAppendAttributes(badge, `aria-hidden`="true"), htmltools::tagGetAttribute(badge, "title")))
    }))
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
  timing<-if(isTRUE(next_round$available))paste0("Round ",next_round$round_number," starts ",incoming_clause_time(next_round$starts_at))else "Next round schedule is loading or could not be verified."
  times <- if (outgoing) rows$leaves_at else rows$joins_at
  labels <- vapply(seq_along(times), function(i) incoming_clause_round_label(times[i], next_round), character(1))
  groups <- c(before="Before round starts", kickoff="At round start", after="After round starts", unverified="")
  render_row <- function(i) {
    row <- rows[i, , drop=FALSE]
    at <- times[i]
    observed_note <- if (isTRUE(state$observations_only) && "observed_at" %in% names(row) && !is.na(row$observed_at))
      paste("Observed:", fm_format_datetime(row$observed_at))
    overdue_note <- if (isTRUE(row$overdue)) paste0("Scheduled time has passed; ",
      if (outgoing) "departure" else "arrival", " is awaiting source confirmation.")
    timing_note <- if (nzchar(row$timing_error) && !identical(state$status, "loading")) row$timing_error
    detail <- if (!is.null(observed_note) || !is.null(overdue_note) || !is.null(timing_note))
      fm_info(observed_note, overdue_note, timing_note, error=!is.null(timing_note), pending=state$status %in% c("loading", "refreshing"))
    shiny::tags$div(class=paste("fm-incoming-player", if (outgoing) "fm-outgoing-player"),
      team_panel_player_identity(row, ns(paste0(prefix, "_open_", row$player_id)), row$player_name),
      shiny::tags$div(class="fm-incoming-arrival",
        if (is.na(at)) shiny::tagList(
          fm_info(if (identical(state$status, "loading")) paste0("Confirming ", if (outgoing) "departure" else "arrival", " time…") else
            paste0(if (outgoing) "Departure" else "Arrival", " time could not be verified"),
            error=!identical(state$status, "loading"), pending=identical(state$status, "loading")), detail
        ) else shiny::tags$div(class="fm-transfer-time",
          shiny::tags$strong(paste0(if (outgoing) "Leaves: " else "Joins: ", incoming_clause_time(at))), detail)))
  }
  shiny::tags$section(class=paste("fm-incoming-panel",if(outgoing)"fm-outgoing-panel"),`aria-labelledby`=ns(paste0(prefix,"_heading")),
    shiny::tags$h3(id=ns(paste0(prefix,"_heading")),if(!nrow(rows)&&!state$status%in%c("ok","observed"))title else paste0(title," (",nrow(rows),")")),
    if(isTRUE(next_round$available))shiny::tags$p(class="fm-incoming-schedule",timing)else fm_info(timing,error=!identical(state$status,"loading"),pending=state$status%in%c("loading","refreshing")),
    if(identical(state$status,"loading"))fm_info(if(outgoing)"Checking clause departures…"else"Checking incoming signings…",pending=TRUE),
    if(!is.null(state$error))fm_info(state$error,error=TRUE,pending=state$status%in%c("loading","refreshing")),
    if(!nrow(rows)&&state$status%in%c("ok","observed"))shiny::tags$p(
      if(outgoing)"No players leaving through a release clause."else"No incoming clause signings."),
    lapply(names(groups), function(group) {
      label <- groups[[group]]
      indices <- which(labels == label)
      if (!length(indices)) return(NULL)
      heading <- if (nzchar(label)) label else if (isTRUE(next_round$available)) "Timing unverified"
      shiny::tags$div(class="fm-transfer-group", `data-timing`=group, if (!is.null(heading)) shiny::tags$h4(heading),
        lapply(indices, render_row))
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

# Project source-confirmed schedules only; unknown is never an eligible zero.
# Shared by My Team and Lineup, with no retrieval, persistence or clock timer.
clause_departure_round_state <- function(transfers,next_round) {
  rows<-transfers$outgoing_records
  status<-fm_scalar(transfers$outgoing_status %||% transfers$status,"loading")
  starts<-fm_time(next_round$starts_at)
  schedule_known<-length(starts)==1L&&!is.na(starts)&&!identical(next_round$available,FALSE)
  valid_rows<-is.data.frame(rows)&&all(c("player_id","leaves_at")%in%names(rows))
  ids<-if(valid_rows)as.character(rows$player_id)else character()
  times<-if(valid_rows)fm_time(rows$leaves_at)else fm_time(character())
  valid_ids<-!is.na(ids)&nzchar(trimws(ids))
  excluded<-if(schedule_known)unique(ids[valid_ids&!is.na(times)&times<=starts])else character()
  unknown<-unique(ids[valid_ids&is.na(times)])
  complete<-schedule_known&&valid_rows&&all(valid_ids)&&!length(unknown)&&status%in%c("ok","empty","observed")
  error<-if(!schedule_known)"The next-round schedule is unavailable; departure timing cannot be checked."else
    if(!complete)fm_scalar(transfers$outgoing_error %||% transfers$error,
      if(status=="loading")"Checking departures before the next round…"else
        "Departure checks are incomplete; player availability for the next round cannot be confirmed.")else NULL
  list(excluded_ids=sort(excluded),unknown_ids=sort(unknown),complete=complete,status=status,error=error)
}
