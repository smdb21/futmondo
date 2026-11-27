

function(input, output, session) {
  session$userData$league_context_enabled<-TRUE
  session$userData$information_context_revision<-reactiveVal(0L)
  login_token_RV <- login_Server(id = "login")
  # Presentation state must never carry old account/league diagnostics forward.
  info_context<-NULL
  observeEvent(list(login_token_RV(),input$selected_league),{
    auth<-login_token_RV()
    key<-paste(if(valid_login(auth))auth[["userid"]]else"signed-out",input$selected_league %||% "",sep="|")
    if(!is.null(info_context)&&!identical(info_context,key)){
      session$userData$information_context_revision(isolate(session$userData$information_context_revision())+1L)
      session$sendCustomMessage("fm-information-reset",list())
    }
    info_context<<-key
  },ignoreNULL=FALSE,priority=100)
  fm_timezone_preferences(session, login_token_RV)
  fm_worker_session(session, login_token_RV, leagues=function()tryCatch(championships_RV(),error=function(e)list()),
    source_note=function()roster_source_note())
  fm_clause_session(session, login_token_RV)
  refresh_trigger <- reactiveVal(0)
  history_refresh_trigger <- round_history_refresh_trigger(session)
  startup_stage <- session$userData$startup_stage
  startup_started <- reactiveVal(Sys.time())
  startup_slow <- reactiveVal(FALSE)
  startup_generation <- 0L
  startup_closed <- FALSE
  startup_timer <- NULL
  session$onSessionEnded(function(){
    startup_closed <<- TRUE
    startup_generation <<- startup_generation+1L
    if(is.function(startup_timer))startup_timer()
  })
  startup_primary_ready <- resource_display_source(session,function(){
    if(!valid_login(login_token_RV()))return(FALSE)
    if(is.null(input$tabs))return(FALSE)
    if(!identical(input$tabs,"today"))return(TRUE)
    tryCatch(isTRUE(today$primary_settled()),error=function(e)FALSE)
  })
  observeEvent(list(login_token_RV(),refresh_trigger(),input$selected_league),{
    startup_generation <<- startup_generation+1L
    generation<-startup_generation;startup_started(Sys.time());startup_slow(FALSE)
    if(is.function(startup_timer))startup_timer()
    startup_timer <<- later::later(function(){
      if(!startup_closed&&identical(startup_generation,generation))startup_slow(TRUE)
    },5)
  },ignoreNULL=FALSE)
  championship_retry<-reactiveVal(0L)
  championship_state_read <- factual_read_adapter(session,"get_active_championships",login_token_RV,
    is_active=function()valid_login(login_token_RV()),refresh_trigger=function()paste(refresh_trigger(),championship_retry(),sep=":"),state_only=TRUE)
  championship_state <- reactive({req(valid_login(login_token_RV()));championship_state_read(login_token_RV())})
  bind_factual_reads("get_active_championships",session,login_token_RV,is_active=function()valid_login(login_token_RV()))
  observeEvent(input$refresh_all, {
    req(valid_login(login_token_RV()))
    request <- as.integer(input$refresh_all)
    feedback <- function(message) {
      session$sendCustomMessage("fm-league-refresh-status", list(request = request, message = message))
    }
    league <- tryCatch(championship_id_RV(), shiny.silent.error = function(e) NULL)
    if (!nzchar(fm_scalar(league, ""))) {
      feedback("Refresh needs a selected league. Choose a league and try again.")
      return()
    }
    if(isTRUE(fm_worker_mode(login_token_RV()[['userid']]))) {
      team <- tryCatch(user_team_id_RV(), shiny.silent.error = function(e) NULL)
      if (!nzchar(fm_scalar(team, ""))) {
        feedback("Your team is unavailable for this league. Try Refresh again when it loads.")
        return()
      }
      fm_worker_refresh_league(login_token_RV(),league,team,session=session,manual=TRUE,feedback=feedback)
      return()
    }
    invalidate_resources(resource_context(login_token_RV(),league),pattern="market|roster|teams|team_info|players|financial|capacity|round|notification|pressroom|history|summary|card",notify_availability=!isTRUE(fm_worker_mode(login_token_RV()[['userid']])))
    history_refresh_trigger(isolate(history_refresh_trigger()) + 1L)
    refresh_trigger(refresh_trigger() + 1)
    feedback("Refresh started. League balances and players are being reloaded.")
  })
  championships_RV <- resource_display_source(session,function(){
    req(valid_login(login_token_RV()))
    refresh_trigger()
    state<-championship_state()
    if(is.null(state$data))shiny::validate(shiny::need(FALSE,if(identical(state$status,"loading"))"Loading leagues…"else resource_failure_message(state$error)), errorClass=if(identical(state$status,"loading"))"fm-loading"else character())
    fm_filter_league_answer(state$data)$championships
  })
  league_preferences<-fm_league_preferences(session,login_token_RV,championships_RV)
  observe({
    ch <- championships_RV()
    metadata<-league_context_revision()
    ids <- vapply(ch, function(c) fm_scalar(c$id %||% c$`_id`, ""), character(1))
    labels <- vapply(ch, function(c) fm_scalar(c$name, "League"), character(1))
    repeated<-duplicated(labels)|duplicated(labels,fromLast=TRUE)
    for(i in which(repeated)){ctx<-league_context_metadata(ids[i],login_token_RV()[["userid"]]);suffix<-fm_scalar(ctx$payload$competition_name,"");if(!nzchar(suffix))suffix<-ids[i];labels[i]<-paste0(labels[i]," (",suffix,")")}
    selected <- league_preferences$selected()
    req(!is.null(selected))
    updateSelectInput(session, "selected_league", choices = setNames(ids, labels), selected = selected)
  })
  championship_RV <- resource_display_source(session,function(){
    ch <- championships_RV(); shiny::validate(shiny::need(length(ch)>0L,"No supported leagues are available for this account."))
    league_context_revision()
    ids <- vapply(ch, function(c) fm_scalar(c$id %||% c$`_id`, ""), character(1))
    selected <- league_preferences$selected()
    req(length(selected)==1L,selected %in% ids)
    context <- ch[[match(selected,ids)]]
    context$id <- context$id %||% context$`_id`
    context$userteam$id <- context$userteam$id %||% context$userteam$`_id`
    shiny::validate(shiny::need(nzchar(fm_scalar(context$userteam$id,"")),"This league has no playable team for this account."))
    unlist(context)
  })
  championship_id_RV <- resource_display_source(session,function(){ req(championship_RV()); unname(championship_RV()["id"]) })
  user_team_id_RV <- resource_display_source(session,function(){ req(championship_RV()); unname(championship_RV()["userteam.id"]) })
  user_team_name_RV <- resource_display_source(session,function(){ req(championship_RV()); unname(championship_RV()["userteam.name"]) })
  session$userData$league_context_source<-league_context_demand_source(session,login_token_RV,championship_id_RV,user_team_id_RV,refresh_trigger)
  observe({input$tabs;if(valid_login(login_token_RV()))session$userData$league_context_source()})
  bind_factual_reads(c("get_teams","get_finished_rounds"),session,
    login_token_RV,championship_id_RV,user_team_id_RV,is_active=function()valid_login(login_token_RV())&&isTRUE(startup_primary_ready()),refresh_trigger=refresh_trigger)
  # Today, My Team and the banner share the same account/league/team information.
  # The display never waits for roster, pressroom, lineup or acquisition checks.
  team_retry <- reactiveVal(0L); market_retry <- reactiveVal(0L)
  primary_read <- function(handler, retry) {
    factual_read_adapter(session,handler,login_token_RV,championship_id_RV,user_team_id_RV,
      is_active=function()valid_login(login_token_RV()),
      refresh_trigger=function()paste(refresh_trigger(),retry(),sep=":"),state_only=TRUE)
  }
  team_read <- primary_read("get_user_team_info", team_retry)
  market_read <- primary_read("get_market_players", market_retry)
  primary_team_state <- reactive({
    req(valid_login(login_token_RV()),championship_id_RV(),user_team_id_RV())
    team_read(login_token_RV(),championship_id_RV(),user_team_id_RV())
  })
  league_settings_source <- reactive({
    leagues<-championships_RV();id<-championship_id_RV()
    selected<-Filter(function(x)identical(as.character(x$id %||% x$`_id`),as.character(id)),leagues)
    state<-primary_team_state()
    list(league=if(length(selected))selected[[1]]else list(),info=state$data %||% list(),
      status=state$status,error=state$error,observed_at=state$observed_at,
      freshness=state$freshness,source=state$source,
      stale=identical(state$freshness,"stale")||identical(state$status,"stale")||!is.null(state$error))
  })
  primary_market_state <- reactive({
    req(valid_login(login_token_RV()),championship_id_RV(),user_team_id_RV())
    market_read(login_token_RV(),championship_id_RV(),user_team_id_RV())
  })
  session$userData$market_offer_state <- primary_market_state
  primary_sources <- list(team_state=primary_team_state,market_state=primary_market_state,
    display_cash=function()round_finance_RV()$cash,
    retry_team=function()team_retry(isolate(team_retry())+1L),
    retry_market=function()market_retry(isolate(market_retry())+1L))
  # The global sync note owns roster/history clocks on the three player pages.
  # Worker mode replays durable snapshots; this never runs clause-summary work.
  roster_note_active <- reactive({
    valid_login(login_token_RV())&&isTRUE(input$tabs%in%c("yourteam","today","players_in_championship"))&&
      isTRUE(fm_worker_mode(login_token_RV()[["userid"]]))
  })
  clause_transfer_revision <- reactiveVal(0L)
  # One owner for these shared prerequisites. The note's narrower visibility
  # must not deactivate reads still needed by My Team or Lineup.
  roster_sources_active <- reactive({
    valid_login(login_token_RV())&&
      isTRUE(input$tabs%in%c("login","yourteam","today","players_in_championship","lineup"))
  })
  home_visible <- reactive(valid_login(login_token_RV()) && identical(input$tabs, "login"))
  roster_source_readers <- lapply(c(roster="get_players_from_team",transactions="get_championship_pressroom"),function(handler)
    factual_read_adapter(session,handler,login_token_RV,championship_id_RV,user_team_id_RV,
      is_active=roster_sources_active,
      refresh_trigger=reactive(list(refresh_trigger(),clause_transfer_revision())),state_only=TRUE))
  roster_source_note <- resource_display_source(session,function(){
    if(!isTRUE(roster_note_active()))return("")
    auth<-login_token_RV();league<-championship_id_RV();team<-user_team_id_RV()
    states<-list(roster=roster_source_readers$roster(login=auth,championship_id=league,user_team_id=team,teams=NULL),
      transactions=roster_source_readers$transactions(login=auth,championship_id=league))
    times<-lapply(states,function(state){
      stamp<-fm_time(state$observed_at)
      if(is.null(state$data)||length(stamp)!=1L||is.na(stamp)||stamp>Sys.time())NULL else stamp
    })
    incoming_clause_source_times(times)
  })
  coach <- coach_on_login_source(session,login_token_RV,championship_id_RV,user_team_id_RV,primary_team_state,primary_sources$retry_team)
  # One visibility projection keeps the same box mounted between target pages.
  # No league, schedule or financial read can prevent the shell from rendering.
  account_finance_visible <- resource_display_source(session,function(){
    valid_login(login_token_RV()) && isTRUE(input$tabs %in% c("today","yourteam","players_in_championship"))
  })
  output$account_finance_summary <- renderUI({
    if (!isTRUE(account_finance_visible())) return(NULL)
    tags$section(class="fm-account-finance", `aria-label`="Balance summary",
      tags$div(class="fm-account-finance-heading", tags$h2("Balance summary"), uiOutput("round_countdown")),
      tags$div(id="round_finance_amounts",class="shiny-html-output fm-summary-output",round_finance_amounts_UI()),
      tags$div(id="account_squad",class="shiny-html-output fm-summary-output",account_squad_UI()),
      uiOutput("round_clause_pending"),
      uiOutput("round_finance_status",inline=TRUE))
  })
  shiny::outputOptions(output,"account_finance_summary",suspendWhenHidden=FALSE)
  round_finance_RV <- resource_display_source(session,function(){
    req(account_finance_visible())
    states <- list(team = primary_team_state(), market = primary_market_state())
    project <- session$userData$manual_finance_projection
    if (is.function(project) && isTRUE(account_finance_visible())) states <- project(states)
    finance <- round_finance_display(states$team, states$market)
    if(fm_clause_enabled())finance<-fm_clause_finance(finance,fm_clause_jobs(resource_context(login_token_RV(),championship_id_RV(),user_team_id_RV())),states$team$data)
    finance$confirmed_actions <- isTRUE(states$confirmed_actions)
    finance$balance_observed_at <- states$team$observed_at
    finance
  })
  round_finance_card_RV <- resource_display_source(session,function()round_finance_card_display(round_finance_RV()))
  output$round_finance_amounts <- renderUI({
    # Keep labels/unknown states visible even during context validation or a real
    # projection failure. The independent status output retains the actual error.
    card <- tryCatch(round_finance_card_RV(), shiny.silent.error=function(e)NULL,
      error=function(e)list())
    round_finance_amounts_UI(card)
  })
  shiny::outputOptions(output,"round_finance_amounts",suspendWhenHidden=FALSE)
  round_clause_pending_display <- resource_display_source(session,function(){
    if(!fm_clause_enabled())return(NULL)
    req(valid_login(login_token_RV()))
    fm_clause_pending_ui(fm_clause_jobs(resource_context(login_token_RV(),championship_id_RV(),user_team_id_RV())))
  })
  output$round_clause_pending <- renderUI(round_clause_pending_display())
  round_finance_status_display <- resource_display_source(session,function(){
    finance<-round_finance_RV()
    cash<-finance$cash;commitments<-finance$commitments;spendable<-finance$spendable_budget
    fm_info(
      if(!is.null(finance$message))tags$p(finance$message),
      if(finance$confirmed_actions)tags$p("Includes confirmed offers and purchases while saved figures catch up."),
      if(finance$cash_unverified)tags$p("Balance has not been verified by the latest check."),
      if(finance$cash_stale)tags$p("Balance check is out of date."),
      if(finance$offers_stale)tags$p("Active offers check is out of date."),
      tags$span(class="fm-info-neutral",textOutput("round_finance_checked",inline=TRUE)),
      error=identical(finance$class,"round-countdown-warning")||
        (!finance$cash_loading&&!finance$offers_loading&&
          (!is.finite(cash)||!is.finite(commitments)||!is.finite(spendable)))||
        (!is.null(finance$message)&&grepl("could not load",finance$message,fixed=TRUE)),
      pending=(finance$cash_loading||finance$offers_loading)&&!identical(finance$class,"round-countdown-warning"),
      class="round-countdown-solvency",source="Balance and active offers")
  })
  output$round_finance_status<-renderUI(round_finance_status_display())
  # Update source times independently of the countdown/financial card DOM.
  round_finance_checked<-resource_display_source(session,function(){
    if(!isTRUE(account_finance_visible()))return("")
    observations<-list(balance=round_finance_RV()$balance_observed_at,offers=primary_market_state()$observed_at)
    text<-vapply(names(observations),function(key){
      stamp<-fm_time(observations[[key]])
      if(length(stamp)!=1L||is.na(stamp)||stamp>Sys.time())return("")
      paste0(if(key=="balance")"Balance observed: "else"Offers observed: ",fm_format_datetime(stamp))
    },character(1))
    paste(text[nzchar(text)],collapse=" · ")
  })
  output$round_finance_checked<-renderText(round_finance_checked())
  shiny::outputOptions(output,"round_finance_checked",suspendWhenHidden=FALSE)
  round_catalog_read <- factual_read_adapter(session,'get_finished_rounds',login_token_RV,championship_id_RV,user_team_id_RV,
    is_active=function()valid_login(login_token_RV())&&isTRUE(startup_primary_ready()),refresh_trigger=refresh_trigger,state_only=TRUE)
  round_catalog_state_RV <- reactive({
    req(valid_login(login_token_RV()),startup_primary_ready(),championship_id_RV())
    round_catalog_read(login=login_token_RV(),championship_id=championship_id_RV())
  })
  round_catalog_RV <- reactive({
    req(valid_login(login_token_RV()), startup_primary_ready(), championship_id_RV())
    refresh_trigger()
    tryCatch(get_finished_rounds(login_token_RV(), championship_id_RV()), error=function(e) data.frame())
  })
  score_calendar_windows <- reactive({
    lineup_calendar_windows(round_catalog_RV())
  })
  session$userData$score_calendar_windows <- function(context){
    active <- resource_context(login_token_RV(),championship_id_RV(),user_team_id_RV())
    scope <- c("account_id","championship_id","competition_id","season")
    if(!identical(context[scope],active[scope]))return(data.frame())
    score_calendar_windows()
  }
  round_heading_state<-resource_display_source(session,function(){
    if(!isTRUE(account_finance_visible()))return(list(visible=FALSE))
    if(!valid_login(login_token_RV()))return(list(visible=TRUE,logged_in=FALSE))
    ready<-isTRUE(startup_primary_ready())
    rounds<-if(ready)round_catalog_RV()else data.frame()
    upcoming<-next_round_context(rounds)
    list(visible=TRUE,logged_in=TRUE,ready=ready,next_round=upcoming,
      current_round=if(!isTRUE(upcoming$available))current_round_context(rounds)else NULL)
  })
  output$round_countdown <- renderUI({
    heading<-round_heading_state()
    if(!isTRUE(heading$visible))return(NULL)
    if(!isTRUE(heading$logged_in)) {
      return(tags$div(class = "round-countdown-bar round-countdown-unavailable",
        icon("clock"), tags$span("Log in to see the next round countdown")))
    }
    primary_ready<-heading$ready
    next_round<-heading$next_round
    round_heading <- if (isTRUE(next_round$available)) {
      tags$div(class="round-countdown-main",icon("clock"),
        tags$strong(paste0("Round ",next_round$round_number)),tags$span(class="fm-browser-countdown",`data-deadline`=as.numeric(fm_time(next_round$starts_at)),`data-server-now`=as.numeric(Sys.time()),format_round_countdown(next_round$starts_at)))
    } else {
      current_round<-heading$current_round
      if (isTRUE(current_round$available)) {
        tags$div(class="round-countdown-main",icon("futbol"),
          tags$strong(paste0("Round ",current_round$round_number)),tags$span("In progress"))
      } else tags$div(class="round-countdown-main",icon("clock"),
        fm_info(if (primary_ready) "Round schedule unavailable" else "Loading round schedule…", error = primary_ready,pending=!primary_ready))
    }
    round_heading
  })

  user_teams_RV <- reactive({
    req(championship_id_RV()); refresh_trigger()
    get_teams(login_token_RV(), championship_id_RV())
  })
  is_admin_RV <- reactive(is_authorized_admin(login_token_RV()))
  page_preferences <- fm_page_preferences(session, login_token_RV, reactive(input$tabs), is_admin_RV)
  destination_pages <- reactive(fm_navigation_pages(is_admin_RV()))
  output$home_destinations <- renderUI({
    req(valid_login(login_token_RV()))
    pages <- destination_pages()
    tags$nav(class = "fm-home-grid", `aria-label` = "League pages",
      lapply(seq_len(nrow(pages)), function(i) {
        p <- pages[i, ]
        tags$button(type = "button", class = "fm-home-tile", `data-fm-page` = p$tab,
          tags$span(class = "fm-home-icon", icon(p$icon)),
          tags$span(class = "fm-home-tile-copy",
            tags$strong(p$label, if (p$tab == "notifications") uiOutput("home_notification_badge", inline = TRUE)),
            tags$small(p$detail)))
      }))
  })
  output$home_continue <- renderUI({
    req(valid_login(login_token_RV()))
    pages <- destination_pages()
    last <- page_preferences$last_page()
    if (is.null(last) || !last %in% pages$tab) return(NULL)
    tags$button(type = "button", class = "fm-home-continue", `data-fm-page` = last,
      paste("Continue to", pages$label[pages$tab == last]), icon("arrow-right"))
  })
  output$home_team <- renderText({
    req(home_visible())
    tryCatch(fm_scalar(user_team_name_RV(), ""), shiny.silent.error = function(e) "")
  })
  # Keep each metric independent while a saved league is being restored.
  # Settled empty accounts and leagues without a playable team have no metric.
  home_read <- function(read) {
    tryCatch(read(), shiny.silent.error = function(e) {
      leagues <- championship_state()
      waiting <- identical(leagues$status, "loading") || (!is.null(leagues$data) &&
        length(fm_filter_league_answer(leagues$data)$championships) > 0L &&
        is.null(league_preferences$selected()))
      list(status = if (waiting) "loading" else "unavailable")
    })
  }
  home_observation <- function(state, value = NULL, detail = NULL, show_stale_notice = TRUE) {
    incomplete <- isTRUE(state$status %in% c("partial", "unavailable")) ||
      any(attr(state$data, "fetch_status") %in% c("partial", "unavailable")) ||
      any(attr(state$data, "worker_completeness") %in% c("partial", "incomplete", "unavailable")) ||
      any(attr(state$data, "worker_acquisition_status") %in% c("partial", "failed", "unavailable"))
    if (is.null(value) || incomplete) return(tags$strong(
      if (identical(state$status, "loading")) "Loading…" else "Unavailable"))
    stale <- identical(state$freshness, "stale") || identical(state$status, "stale") ||
      identical(attr(state$data, "fetch_status"), "stale") || !is.null(state$error)
    tags$div(tags$strong(value), if (!is.null(detail)) tags$small(detail),
      if (show_stale_notice && stale) tags$small("May be out of date"))
  }
  # Calendar updates and navigation can replace a deadline well before kickoff.
  home_round_tick <- reactiveVal(NULL)
  advance_home_round <- function() home_round_tick(Sys.time())
  home_round_timer <- NULL
  cancel_home_round_timer <- function() {
    if (is.function(home_round_timer)) home_round_timer()
    home_round_timer <<- NULL
  }
  session$onSessionEnded(cancel_home_round_timer)
  output$home_round <- renderUI({
    cancel_home_round_timer()
    req(home_visible())
    home_round_tick()
    state <- home_read(round_catalog_state_RV)
    now <- Sys.time()
    rounds <- home_round_context(state$data, now)
    current <- rounds$current
    upcoming <- rounds$upcoming
    if (is.null(current) && is.null(upcoming)) return(home_observation(state))
    if (!is.null(rounds$changes_at)) {
      # Use the same session clock as invalidateLater, retaining its cancel handle.
      delay <- ceiling(as.numeric(difftime(rounds$changes_at, Sys.time(), units = "secs")) * 1000)
      home_round_timer <<- session$.scheduleTask(max(1, min(delay, .Machine$integer.max)), advance_home_round)
    }
    kickoff <- if (!is.null(upcoming)) {
      label <- if (is.null(current)) "Scheduled kickoff" else paste("Round", upcoming$round_number, "kickoff")
      tags$span(class = "fm-home-kickoff",
        tags$span(label),
        tags$span(fm_format_datetime(upcoming$starts_at)),
        tags$span("Time until kickoff"),
        tags$span(class = "fm-browser-countdown", `data-deadline` = as.numeric(upcoming$starts_at),
          `data-server-now` = as.numeric(now), format_round_countdown(upcoming$starts_at, now)))
    }
    headline <- if (is.null(current)) upcoming else current
    home_observation(state, paste("Round", headline$round_number),
      tagList(if (is.null(current)) "Upcoming" else current$status, kickoff),
      show_stale_notice = FALSE)
  })
  home_roster_state <- reactive({
    req(home_visible())
    home_read(function() roster_source_readers$roster(login = login_token_RV(),
      championship_id = championship_id_RV(), user_team_id = user_team_id_RV(), teams = NULL))
  })
  home_team_state <- reactive(home_read(primary_team_state))
  home_squad_summary <- reactive(team_value_summary(home_team_state(), home_roster_state()))
  output$home_balance <- renderUI({
    req(home_visible())
    states <- list(team = home_team_state(), market = home_read(primary_market_state))
    summary <- home_squad_summary()
    project <- session$userData$manual_finance_projection
    if (!is.null(states$team$data) && is.function(project)) states <- project(states)
    finance <- round_finance_display(states$team, states$market)
    amount <- finance$cash
    offers <- if (is.finite(finance$active_offer_count))
      paste(finance$active_offer_count, "·", player_card_money(finance$commitments))
    tagList(
      home_observation(states$team, if (is.finite(amount)) player_card_money(amount),
        if (isTRUE(states$confirmed_actions)) "Includes confirmed actions"),
      tags$div(class = "fm-home-detail fm-home-offers", tags$span("Active offers"),
        home_observation(states$market, offers)),
      tags$div(class = "fm-home-squad-value",
        if (identical(summary$value_status, "stale")) "Last saved squad value" else "Squad value",
        tags$strong(if (identical(summary$value_status, "loading")) "Loading…" else player_card_money(summary$value))),
      team_trend_panel(summary, label = "24h squad trend", counts = NULL))
  })
  home_transfer_counts <- resource_display_source(session, function() {
    req(home_visible())
    transfers <- clause_transfers_RV()
    count <- function(status, rows) {
      if (isTRUE(status %in% c("ok", "observed")) && !is.null(rows$player_id))
        return(length(unique(rows$player_id)))
      NA_integer_
    }
    list(incoming = count(transfers$status, transfers$records),
      leaving = count(transfers$outgoing_status, transfers$outgoing_records))
  })
  output$home_squad <- renderUI({
    req(home_visible())
    state <- home_roster_state()
    count <- observed_squad_count(state)
    transfers <- home_transfer_counts()
    market <- team_market_counts(state, count)
    market_label <- function(n, ready, missing) {
      if (is.finite(n)) paste(n, ready) else paste(missing, if (identical(state$status, "loading")) "loading…" else "unavailable")
    }
    tagList(
      home_observation(state, if (is.finite(count)) paste(count, if (count == 1L) "player" else "players")),
      tags$div(class = "fm-home-detail", tags$span("Market"),
        tags$span(paste(market_label(market["listed"], "listed", "Listings"),
          market_label(market["offered"], "with offers", "Offers"), sep = " · "))),
      tags$div(class = "fm-home-detail", tags$span("Players value"),
        team_trend_counts(home_squad_summary(), show_count_labels = TRUE)),
      tags$div(class = "fm-home-detail", tags$span("Transfers"),
        tags$div(class = "fm-team-trend-counts",
          team_direction_count("up", transfers$incoming, "Incoming players", "incoming"),
          team_direction_count("down", transfers$leaving, "Leaving players", "leaving"))))
  })
  notifications <- notifications_Server("notifications", reactive(input$tabs == "notifications"),
    login_token_RV, championship_id_RV, user_team_id_RV, user_teams_RV, refresh_trigger,
    on_market_event=function(...)refresh_trigger(isolate(refresh_trigger())+1L),startup_ready=startup_primary_ready)
  output$home_notification_badge <- renderUI({
    req(valid_login(login_token_RV()))
    n <- notifications$unread()
    if (is.finite(n) && n > 0)
      tags$span(class = "fm-home-unread-count", n, tags$span(class = "sr-only", " unread"))
  })
  output$notification_bell <- renderUI({
    if (!valid_login(login_token_RV())) return(NULL)
    n <- notifications$unread()
    label <- paste0("Notifications",if(is.finite(n))paste0(" (",n," unread)")else "")
    actionLink("open_notifications",label=if(is.finite(n)&&n>0)
      tags$span(class="fm-notification-unread-dot",`aria-hidden`="true")else NULL,
      icon=icon("bell"),class="fm-notification-link",title=label,`aria-label`=label)
  })
  outputOptions(output, "notification_bell", suspendWhenHidden = FALSE)
  observeEvent(input$open_welcome, updateTabItems(session, "tabs", "login"))
  observeEvent(input$open_notifications, {
    req(valid_login(login_token_RV()))
    updateTabItems(session, "tabs", "notifications")
  })
  refresh_clause_transfers <- function() {
    auth<-isolate(login_token_RV());req(valid_login(auth))
    invalidate_saved_clause_transfers(resource_context(auth,isolate(championship_id_RV()),isolate(user_team_id_RV())))
    clause_transfer_revision(isolate(clause_transfer_revision())+1L)
  }
  clause_transfers_RV <- incoming_clause_source(session,login_token_RV,championship_id_RV,user_team_id_RV,
    roster_sources_active,
    reactive(list(refresh_trigger(),clause_transfer_revision())),prerequisite_readers=roster_source_readers)
  fm_clause_reconcile_saved_offers(session,login_token_RV,championship_id_RV,user_team_id_RV,
    function()roster_source_readers$transactions(login=login_token_RV(),championship_id=championship_id_RV()),
    primary_team_state,primary_market_state)
  fm_manual_finance_session(session, login_token_RV, championship_id_RV, user_team_id_RV, primary_team_state,
    function() roster_source_readers$transactions(login = login_token_RV(), championship_id = championship_id_RV()))
  account_squad_RV <- resource_display_source(session,function(){
    req(account_finance_visible())
    auth<-login_token_RV();league<-championship_id_RV();team<-user_team_id_RV()
    roster<-roster_source_readers$roster(login=auth,championship_id=league,user_team_id=team,teams=NULL)
    jobs<-if(fm_clause_enabled())fm_clause_jobs(resource_context(auth,league,team))else list()
    account_squad_display(roster,clause_transfers_RV(),primary_team_state(),
      active_offers=tryCatch(round_finance_card_RV()$active_offer_count,error=function(e)NA_real_),
      reserved_slots=fm_clause_reservations(jobs)$count)
  })
  account_squad_card_RV <- resource_display_source(session,function(){
    state<-account_squad_RV()
    state[setdiff(names(state),c("roster_observed_at","team_observed_at"))]
  })
  output$account_squad_checked <- renderText({
    state<-tryCatch(account_squad_RV(),error=function(e)NULL)
    stamps<-c(roster="Roster observed",team="Squad limit observed")
    text<-vapply(names(stamps),function(key){
      at<-fm_time(state[[paste0(key,"_observed_at")]])
      if(length(at)!=1L||is.na(at)||at>Sys.time())return("")
      paste0(stamps[[key]],": ",fm_format_datetime(at))
    },character(1))
    paste(text[nzchar(text)],collapse=" · ")
  })
  shiny::outputOptions(output,"account_squad_checked",suspendWhenHidden=FALSE)
  output$account_squad <- renderUI({
    state<-tryCatch(account_squad_card_RV(),shiny.silent.error=function(e)NULL,
      error=function(e)list(status="unavailable",detail="Squad capacity could not be verified."))
    account_squad_UI(state)
  })
selected_player_RV <- players_in_teams_Server(id = "players_in_teams",
                                                 is_module_active = reactive({
                                                   input$tabs == "yourteam"
                                                 }),
                                                 login_token = login_token_RV,
                                                 championship_id = championship_id_RV,
                                                 user_team_id = user_team_id_RV,
                                                 user_teams_RV = user_teams_RV,
                                                 refresh_trigger = refresh_trigger,
                                                 round_catalog = round_catalog_RV,
                                                 league_settings_source = league_settings_source,
                                                 clause_transfers_source = clause_transfers_RV,
                                                 team_info_source = primary_team_state)

  lineup <- lineup_Server("lineup", reactive(identical(input$tabs,"lineup")),
    login_token_RV, championship_id_RV, user_team_id_RV, round_catalog_RV,
    refresh_trigger, history_refresh_trigger, clause_transfers_source=clause_transfers_RV,
    clause_transfers_refresh=refresh_clause_transfers,round_catalog_state=round_catalog_state_RV,coach_source=coach)

  today <- today_Server(id = "today",
               is_module_active = reactive({
                 input$tabs == "today"
               }),
               login_token = login_token_RV,
               championship_id = championship_id_RV,
               user_team_id = user_team_id_RV,
               user_teams_RV = user_teams_RV,
               refresh_trigger = refresh_trigger,
               primary_sources = primary_sources)

players_in_championship_Server(id = "players_in_championship", 
                                  is_module_active = reactive({
                                    input$tabs == "players_in_championship"  
                                  }),
                                  login_token = login_token_RV, 
                                  championship_id = championship_id_RV,
                                  user_team_id = user_team_id_RV,
                                  user_teams_RV = user_teams_RV,
                                  refresh_trigger = refresh_trigger,
                                  clause_transfers_source = clause_transfers_RV,
                                  team_state_source = primary_team_state)

  rivals_Server(id = "rivals",
                is_module_active = reactive({
                  input$tabs == "rivals"
                }),
                login_token = login_token_RV,
                championship_id = championship_id_RV,
                user_team_id = user_team_id_RV,
                user_teams_RV = user_teams_RV, refresh_trigger = refresh_trigger)

  plots_Server(id = "plots", is_module_active = reactive({ input$tabs == "plots" }),
               login_token = login_token_RV, championship_id = championship_id_RV,
               user_team_id = user_team_id_RV, user_teams_RV = user_teams_RV,
               refresh_trigger = refresh_trigger)

  classification_Server(id = "classification",
                        is_module_active = reactive({
                          input$tabs == "classification"
                        }),
                        login_token = login_token_RV,
                        championship_id = championship_id_RV,
                        user_team_id = user_team_id_RV,
                        user_teams_RV = user_teams_RV, refresh_trigger = refresh_trigger,
                        league_settings_source = league_settings_source, round_calendar = round_catalog_RV)

  round_mvps_Server(id = "round_mvps",
                    is_module_active = reactive({
                      input$tabs == "round_mvps"
                    }),
                    login_token = login_token_RV,
                    championship_id = championship_id_RV,
                    refresh_trigger = refresh_trigger)

  admin_Server(id = "admin",
               is_module_active = reactive({
                 req(input$tabs); input$tabs == "admin"
               }),
               login_token = login_token_RV,
               championship_id = championship_id_RV,
               user_team_id = user_team_id_RV,
               user_teams_RV = user_teams_RV, is_admin = is_admin_RV,refresh_trigger=refresh_trigger,
               league_settings_source = league_settings_source,
               retry_league_settings = function()team_retry(isolate(team_retry())+1L))
  observe({
    if(!valid_login(login_token_RV()))return()
    state<-championship_state()
    if(!identical(state$status,"loading")&&identical(isolate(startup_stage()),"loading_leagues"))resource_timing("leagues",isolate(startup_started()))
    if(identical(state$status,"loading")){startup_stage("loading_leagues");return()}
    if(is.null(state$data)){startup_stage("failed");return()}
    if(!length(fm_filter_league_answer(state$data)$championships)){startup_stage("empty");return()}
    if(!isTRUE(startup_primary_ready())){startup_stage("loading_today");return()}
    stage<-if(identical(input$tabs,"today"))today$primary_status()else "ready"
    if(!identical(isolate(startup_stage()),stage))resource_timing("today_primary",isolate(startup_started()))
    startup_stage(stage)
  })
  output$startup_status <- renderUI({
    if(!valid_login(login_token_RV()))return(NULL)
    stage<-startup_stage()
    if(stage %in% c("signed_out","ready","authenticating"))return(NULL)
    loading<-stage %in% c("loading_leagues","loading_today")
    message<-switch(stage,loading_leagues="Signed in. Loading your leagues…",loading_today="Loading today's market…",
      empty="Signed in. No supported leagues are available.",partial="Some sections could not load. Retry the affected data.",
      failed="The first page could not load. Refresh to retry.","")
    if(identical(stage,"failed")){state<-tryCatch(championship_state(),error=function(e)NULL);if(!is.null(state$error))message<-resource_failure_message(state$error)}
    if(loading&&isTRUE(startup_slow()))message<-paste(message,"Still loading data from LigaRadar…")
    if(loading)return(tags$div(class="fm-loading-status startup-feedback",
      fm_loading_visual("fm-startup-visual",compact=TRUE),
      tags$p(class="fm-loading-caption",role="status",`aria-live`="polite",message)))
    fm_info(tags$p(message), error = stage %in% c("partial", "failed"), class = "startup-feedback",
      if(!loading&&stage %in% c("partial","failed"))actionButton("retry_startup","Retry"))
  })
  observeEvent(input$retry_startup,{
    state<-championship_state()
    if(is.null(state$data))championship_retry(isolate(championship_retry())+1L)else today$retry_failed()
  })
  # observers ----
  ## observe user_team_id_RV()
observeEvent(login_token_RV(),
                {
                  if (!valid_login(login_token_RV())) {
                    fm_transport_cancel(session$token)
                    prior_user <- session$userData$futmondo_user_id
                    if(!is.null(prior_user))clear_api_cache(prior_user,session$token)
                    cancel_persistence_jobs(session$userData$futmondo_user_id,
                      session$token)
                    session$userData$futmondo_user_id <- NULL
                    updateTabItems(session, "tabs", "login")
                    return()
                  }
                  session$userData$futmondo_user_id <- login_token_RV()[["userid"]]
                },
                ignoreNULL = FALSE
   )
  
  # renders----
  ## render menu ----
  menu_identity <- resource_display_source(session, function() {
    auth <- login_token_RV()
    list(account = if (valid_login(auth)) auth[["userid"]] else NULL, admin = is_admin_RV())
  })
  output$menu <- shinydashboard::renderMenu({
    identity <- menu_identity()
    signed_in <- !is.null(identity$account)
    items <- list(shinydashboard::menuItem(if (signed_in) "Home" else "Welcome",
      tabName = "login", selected = TRUE, icon = icon("house")))
    if (signed_in) {
      pages <- fm_navigation_pages(identity$admin)
      items <- c(items, lapply(seq_len(nrow(pages)), function(i)
        shinydashboard::menuItem(pages$label[i], tabName = pages$tab[i], icon = icon(pages$icon[i]))))
    }
    do.call(shinydashboard::sidebarMenu, c(list(id = "tabs"), items))
  })
  outputOptions(output, "menu", suspendWhenHidden = FALSE)
}
