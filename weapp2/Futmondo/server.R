

function(input, output, session) {
  login_token_RV <- login_Server(id = "login")
  refresh_trigger <- reactiveVal(0)
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
  startup_primary_ready <- reactive({
    if(!valid_login(login_token_RV()))return(FALSE)
    if(is.null(input$tabs)||identical(input$tabs,"login"))return(FALSE)
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
    invalidate_resources(resource_context(login_token_RV(),isolate(championship_id_RV())),pattern="market|roster|teams|team_info|players|financial|round|notification|pressroom|history|summary|card")
    refresh_trigger(refresh_trigger() + 1)
  })
  championships_RV <- reactive({
    req(valid_login(login_token_RV()))
    refresh_trigger()
    state<-championship_state()
    if(is.null(state$data))shiny::validate(shiny::need(FALSE,if(identical(state$status,"loading"))"Loading leagues…"else resource_failure_message(state$error)))
    state$data$championships
  })
  observe({
    ch <- championships_RV()
    ids <- vapply(ch, function(c) fm_scalar(c$id %||% c$`_id`, ""), character(1))
    labels <- vapply(ch, function(c) fm_scalar(c$name, "League"), character(1))
    current <- isolate(input$selected_league)
    selected <- if (!is.null(current) && current %in% ids) current else if (length(ids)) ids[1] else character()
    updateSelectInput(session, "selected_league", choices = setNames(ids, labels), selected = selected)
  })
  championship_RV <- reactive({
    ch <- championships_RV(); req(length(ch))
    ids <- vapply(ch, function(c) fm_scalar(c$id %||% c$`_id`, ""), character(1))
    selected <- input$selected_league
    if (is.null(selected) || !selected %in% ids) selected <- ids[1]
    context <- ch[[match(selected,ids)]]
    context$id <- context$id %||% context$`_id`
    context$userteam$id <- context$userteam$id %||% context$userteam$`_id`
    unlist(context)
  })
  championship_id_RV <- reactive({ req(championship_RV()); unname(championship_RV()["id"]) })
  user_team_id_RV <- reactive({ req(championship_RV()); unname(championship_RV()["userteam.id"]) })
  user_team_name_RV <- reactive({ req(championship_RV()); unname(championship_RV()["userteam.name"]) })
  bind_factual_reads(c("get_teams","get_finished_rounds"),session,
    login_token_RV,championship_id_RV,user_team_id_RV,is_active=function()valid_login(login_token_RV())&&isTRUE(startup_primary_ready()),refresh_trigger=refresh_trigger)
  # Today and the banner consume the exact same account/league/team states.
  # The display never waits for roster, pressroom, lineup or acquisition checks.
  team_retry <- reactiveVal(0L); market_retry <- reactiveVal(0L)
  round_finance_version <- reactive({
    req(valid_login(login_token_RV()))
    refresh_trigger()
    invalidateLater(60000, session)
    paste(refresh_trigger(), as.numeric(Sys.time()), sep=":")
  })
  primary_read <- function(handler, retry) {
    factual_read_adapter(session,handler,login_token_RV,championship_id_RV,user_team_id_RV,
      is_active=function()valid_login(login_token_RV()),
      refresh_trigger=function()paste(round_finance_version(),retry(),sep=":"),state_only=TRUE)
  }
  team_read <- primary_read("get_user_team_info", team_retry)
  market_read <- primary_read("get_market_players", market_retry)
  primary_team_state <- reactive({
    req(valid_login(login_token_RV()),championship_id_RV(),user_team_id_RV())
    team_read(login_token_RV(),championship_id_RV(),user_team_id_RV())
  })
  primary_market_state <- reactive({
    req(valid_login(login_token_RV()),championship_id_RV(),user_team_id_RV())
    market_read(login_token_RV(),championship_id_RV(),user_team_id_RV())
  })
  primary_sources <- list(team_state=primary_team_state,market_state=primary_market_state,
    retry_team=function()team_retry(isolate(team_retry())+1L),
    retry_market=function()market_retry(isolate(market_retry())+1L))
  round_finance_RV <- reactive({
    round_finance_display(primary_team_state(), primary_market_state())
  })
  round_catalog_RV <- reactive({
    req(valid_login(login_token_RV()), startup_primary_ready(), championship_id_RV())
    refresh_trigger()
    invalidateLater(60000, session)
    tryCatch(get_finished_rounds(login_token_RV(), championship_id_RV()), error=function(e) data.frame())
  })
  output$round_countdown <- renderUI({
    invalidateLater(1000, session)
    if (!valid_login(login_token_RV())) {
      return(tags$div(class = "round-countdown-bar round-countdown-unavailable",
        icon("clock"), tags$span("Log in to see the next round countdown")))
    }
    primary_ready <- isTRUE(startup_primary_ready())
    rounds <- if (primary_ready) round_catalog_RV() else data.frame()
    next_round <- next_round_context(rounds)
    round_heading <- if (isTRUE(next_round$available)) {
      tags$div(class="round-countdown-main",icon("clock"),
        tags$strong(paste0("Round ",next_round$round_number)),tags$span(format_round_countdown(next_round$starts_at)))
    } else {
      current_round <- current_round_context(rounds)
      if (isTRUE(current_round$available)) {
        tags$div(class="round-countdown-main",icon("futbol"),
          tags$strong(paste0("Round ",current_round$round_number)),tags$span("In progress"))
      } else tags$div(class="round-countdown-main",icon("clock"),
        tags$span(if (primary_ready) "Round schedule unavailable" else "Loading round schedule…"))
    }
    finance <- round_finance_RV()
    cash <- finance$cash
    commitments <- finance$commitments
    active_offer_count <- finance$active_offer_count
    final_balance <- finance$projected_committed_balance
    spendable <- finance$spendable_budget
    money_text <- function(x, loading=FALSE) if (is.finite(x)) paste0(format(round(x), big.mark=".", decimal.mark=",", scientific=FALSE), " EUR") else if (loading) "Loading…" else "Unavailable"
    cash_text <- paste0(money_text(cash,finance$cash_loading),if (finance$cash_stale) " (cached)" else if (finance$cash_unverified) " (unverified)" else "")
    offers_text <- if (is.finite(commitments)) {
      suffix <- if (is.finite(active_offer_count)) paste0(" (", as.integer(active_offer_count), ")") else ""
      paste0(money_text(commitments), suffix)
    } else if (finance$offers_loading) "Loading…" else "Unavailable"
    solvency_class <- finance$class
    solvency_text <- finance$message
    tags$div(class = paste("round-countdown-bar", solvency_class),
      round_heading,
      tags$div(class = "round-countdown-finance",
        tags$span(class = "round-countdown-finance-item", paste0("Balance: ", cash_text)),
        tags$span(class = "round-countdown-finance-item", paste0("Active offers: ", offers_text)),
        tags$span(class = "round-countdown-finance-item", paste0("After offers: ", money_text(final_balance,finance$cash_loading || finance$offers_loading))),
        tags$span(class = "round-countdown-finance-item", paste0("Can spend: ", money_text(spendable,finance$cash_loading || finance$offers_loading))),
        if (!is.null(solvency_text)) tags$span(class = "round-countdown-solvency", solvency_text)))
  })
  user_teams_RV <- reactive({
    req(championship_id_RV()); refresh_trigger()
    get_teams(login_token_RV(), championship_id_RV())
  })
  is_admin_RV <- reactive(is_authorized_admin(login_token_RV()))
  notifications <- notifications_Server("notifications", reactive(input$tabs == "notifications"),
    login_token_RV, championship_id_RV, user_team_id_RV, user_teams_RV, refresh_trigger,
    on_market_event=function(...)refresh_trigger(isolate(refresh_trigger())+1L),startup_ready=startup_primary_ready)
  output$notification_bell <- renderUI({
    n <- notifications$unread()
    actionLink("open_notifications", label = paste0("Notifications", if (is.finite(n)) paste0(" (",n,")") else ""), icon = icon("bell"))
  })
  observeEvent(input$open_notifications, updateTabItems(session, "tabs", "notifications"))
selected_player_RV <- players_in_teams_Server(id = "players_in_teams",
                                                 is_module_active = reactive({
                                                   input$tabs == "yourteam"
                                                 }),
                                                 login_token = login_token_RV,
                                                 championship_id = championship_id_RV,
                                                 user_team_id = user_team_id_RV,
                                                 user_teams_RV = user_teams_RV,
                                                 refresh_trigger = refresh_trigger)

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
                                  refresh_trigger = refresh_trigger)

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
                        user_teams_RV = user_teams_RV, refresh_trigger = refresh_trigger)

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
               user_teams_RV = user_teams_RV, is_admin = is_admin_RV)
  observe({
    if(!valid_login(login_token_RV()))return()
    state<-championship_state()
    if(!identical(state$status,"loading")&&identical(isolate(startup_stage()),"loading_leagues"))resource_timing("leagues",isolate(startup_started()))
    if(identical(state$status,"loading")){startup_stage("loading_leagues");return()}
    if(is.null(state$data)){startup_stage("failed");return()}
    if(!length(state$data$championships)){startup_stage("empty");return()}
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
    message<-switch(stage,loading_leagues="Signed in. Loading your leagues…",loading_today="Loading Today…",
      empty="Signed in. No active leagues were returned.",partial="Some sections could not load. Retry the affected data.",
      failed="The first page could not load. Refresh to retry.","")
    if(identical(stage,"failed")){state<-tryCatch(championship_state(),error=function(e)NULL);if(!is.null(state$error))message<-resource_failure_message(state$error)}
    if(loading&&isTRUE(startup_slow()))message<-paste(message,"Still loading data from Futmondo…")
    div(class="startup-feedback",role="status",`aria-live`="polite",
      if(loading)icon("spinner",class="fa-spin"),span(message),
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
                  updateTabsetPanel(inputId = "tabs", selected = "today")
                },
                ignoreNULL = FALSE
   )
  
  # renders----
  ## render menu ----
  output$menu <- shinydashboard::renderMenu({
    # Check if logged-in user matches the admin env var (case-insensitive, trimmed)
    is_admin <- FALSE
    admin_env <- trimws(Sys.getenv("admin"))
    if (admin_env != "" && !is.null(login_token_RV()) && length(login_token_RV()) >= 3) {
      current_user <- trimws(as.character(login_token_RV()[["user_name"]]))
      is_admin <- tolower(current_user) == tolower(admin_env)
    }

    # Standard menu items
    menu_items <- list(
      shinydashboard::menuItem("Login", tabName = "login", icon = icon("right-to-bracket")),
      shinydashboard::menuItem("Notifications", tabName = "notifications", icon = icon("bell")),
      shinydashboard::menuItem("Today", tabName = "today", icon = icon("bolt")),
      shinydashboard::menuItem("Your team", tabName = "yourteam", icon = icon("users")),
      shinydashboard::menuItem("Players", tabName = "players_in_championship", icon = icon("table")),
      shinydashboard::menuItem("Rivals", tabName = "rivals", icon = icon("users-viewfinder")),
      shinydashboard::menuItem("Plots", tabName = "plots", icon = icon("chart-column")),
      shinydashboard::menuItem("Classification", tabName = "classification", icon = icon("trophy")),
      shinydashboard::menuItem("Round MVPs", tabName = "round_mvps", icon = icon("medal"))
    )

    # Append Admin menu item as the last item if user is admin
    if (is_admin) {
      menu_items <- c(menu_items, list(
        shinydashboard::menuItem("Admin", tabName = "admin", icon = icon("gears"))
      ))
    }

    do.call(shinydashboard::sidebarMenu, c(list(id = "tabs"), menu_items))
  })
}
