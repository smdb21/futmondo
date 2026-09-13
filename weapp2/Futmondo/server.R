

function(input, output, session) {
  login_token_RV <- login_Server(id = "login")
  refresh_trigger <- reactiveVal(0)
  observeEvent(input$refresh_all, {
    req(valid_login(login_token_RV()))
    clear_api_cache(login_token_RV()[["userid"]])
    refresh_trigger(refresh_trigger() + 1)
  })
  championships_RV <- reactive({
    req(valid_login(login_token_RV()))
    refresh_trigger()
    get_active_championships(login_token_RV())$championships
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
  output$round_countdown <- renderUI({
    invalidateLater(1000, session)
    if (!valid_login(login_token_RV())) {
      return(tags$div(class = "round-countdown-bar round-countdown-unavailable",
        icon("clock"), tags$span("Log in to see the next round countdown")))
    }
    rounds <- tryCatch(get_finished_rounds(login_token_RV(), championship_id_RV()),
      error = function(e) data.frame())
    next_round <- next_round_context(rounds)
    if (!isTRUE(next_round$available)) {
      current_round <- current_round_context(rounds)
      if (isTRUE(current_round$available)) {
        return(tags$div(class = "round-countdown-bar round-countdown-in-progress",
          tags$div(class = "round-countdown-main", icon("futbol"),
            tags$strong(paste0("Round ", current_round$round_number)),
            tags$span("In progress"))))
      }
      return(tags$div(class = "round-countdown-bar round-countdown-unavailable",
        icon("clock"), tags$span("Round schedule unavailable")))
    }
    finance <- tryCatch(get_financial_snapshot(login_token_RV(), championship_id_RV(), user_team_id_RV()),
      error = function(e) NULL)
    cash <- if (is.list(finance)) fm_number(finance$cash) else NA_real_
    commitments <- if (is.list(finance)) fm_number(finance$commitments) else NA_real_
    final_balance <- if (is.list(finance)) fm_number(finance$projected_committed_balance) else NA_real_
    spendable <- if (is.list(finance)) fm_number(finance$spendable_budget) else NA_real_
    money_text <- function(x) if (is.finite(x)) paste0(format(round(x), big.mark = ".", scientific = FALSE), " EUR") else "Unavailable"
    solvency_class <- if (is.finite(final_balance) && final_balance > 0) "round-countdown-solvent" else "round-countdown-warning"
    solvency_text <- if (!is.finite(final_balance)) {
      "Balance unavailable — verify it before the round"
    } else if (final_balance > 0) {
      paste0("Final balance positive: ", money_text(final_balance))
    } else {
      paste0("Restore at least ", format(round(abs(final_balance) + 1), big.mark = ".", scientific = FALSE),
        " EUR before kickoff to score points")
    }
    tags$div(class = paste("round-countdown-bar", solvency_class),
      tags$div(class = "round-countdown-main", icon("clock"),
        tags$strong(paste0("Round ", next_round$round_number)),
        tags$span(format_round_countdown(next_round$starts_at))),
      tags$div(class = "round-countdown-finance",
        tags$span(class = "round-countdown-finance-item", paste0("Balance: ", money_text(cash))),
        tags$span(class = "round-countdown-finance-item", paste0("Active offers: ", money_text(commitments))),
        tags$span(class = "round-countdown-finance-item", paste0("After offers: ", money_text(if (is.finite(cash) && is.finite(commitments)) cash - commitments else NA_real_))),
        tags$span(class = "round-countdown-finance-item", paste0("Can spend: ", money_text(spendable))),
        tags$span(class = "round-countdown-solvency", solvency_text)))
  })
  user_teams_RV <- reactive({
    req(championship_id_RV()); refresh_trigger()
    get_teams(login_token_RV(), championship_id_RV())
  })
  is_admin_RV <- reactive(is_authorized_admin(login_token_RV()))
  # Collect/persist in an isolated process. No on.exit HTTP writes or startup DB checks.
  observe({
    req(valid_login(login_token_RV()), championship_id_RV(), user_team_id_RV())
    refresh_trigger()
    defer_persistence("collect_account_observations", list(login_token_RV(), championship_id_RV(), user_team_id_RV()))
  })
  output$background_sync <- renderUI({
    req(valid_login(login_token_RV()))
    invalidateLater(5000,session)
    status <- background_sync_status(login_token_RV()[["userid"]])
    if (!is.null(status$error)) return(tags$p(class="text-warning",status$error))
    if (status$pending>0) tags$p(class="text-muted","Saving observations in the background…")
  })
  notifications <- notifications_Server("notifications", reactive(input$tabs == "notifications"),
    login_token_RV, championship_id_RV, user_team_id_RV, user_teams_RV, refresh_trigger,
    on_market_event=function(...)refresh_trigger(isolate(refresh_trigger())+1L))
  output$notification_bell <- renderUI({
    n <- notifications$unread()
    actionLink("open_notifications", label = paste0("Notifications", if (is.finite(n)) paste0(" (",n,")") else ""), icon = icon("bell"))
  })
  observeEvent(input$open_notifications, updateTabItems(session, "tabs", "notifications"))
  intelligence_Server("intelligence", reactive(input$tabs == "intelligence"), login_token_RV,
    championship_id_RV, user_team_id_RV, refresh_trigger)
  automation_Server("automation", reactive(input$tabs == "automation"), login_token_RV,
    championship_id_RV, user_team_id_RV, refresh_trigger)

selected_player_RV <- players_in_teams_Server(id = "players_in_teams",
                                                 is_module_active = reactive({
                                                   input$tabs == "yourteam"
                                                 }),
                                                 login_token = login_token_RV,
                                                 championship_id = championship_id_RV,
                                                 user_team_id = user_team_id_RV,
                                                 user_teams_RV = user_teams_RV,
                                                 refresh_trigger = refresh_trigger)

  today_Server(id = "today",
               is_module_active = reactive({
                 input$tabs == "today"
               }),
               login_token = login_token_RV,
               championship_id = championship_id_RV,
               user_team_id = user_team_id_RV,
               user_teams_RV = user_teams_RV,
               refresh_trigger = refresh_trigger)

  market_Server(id = "market",
                is_module_active = reactive({
                  input$tabs == "market"
                }),
                login_token = login_token_RV, 
                championship_id = championship_id_RV, 
                user_team_id = user_team_id_RV, 
                user_teams_RV = user_teams_RV,
                refresh_trigger = refresh_trigger)

players_in_championship_Server(id = "players_in_championship", 
                                  is_module_active = reactive({
                                    input$tabs == "players_in_championship"  
                                  }),
                                  login_token = login_token_RV, 
                                  championship_id = championship_id_RV, 
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

  classification_Server(id = "classification",
                        is_module_active = reactive({
                          input$tabs == "classification"
                        }),
                        login_token = login_token_RV,
                        championship_id = championship_id_RV,
                        user_team_id = user_team_id_RV,
                        user_teams_RV = user_teams_RV, refresh_trigger = refresh_trigger)

  admin_Server(id = "admin",
               is_module_active = reactive({
                 req(input$tabs); input$tabs == "admin"
               }),
               login_token = login_token_RV,
               championship_id = championship_id_RV,
               user_team_id = user_team_id_RV,
               user_teams_RV = user_teams_RV, is_admin = is_admin_RV)
  # observers ----
  ## observe user_team_id_RV()
observeEvent(login_token_RV(),
                {
                  if (!valid_login(login_token_RV())) {
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
      shinydashboard::menuItem("Predictions", tabName = "intelligence", icon = icon("chart-line")),
      shinydashboard::menuItem("Notifications", tabName = "notifications", icon = icon("bell")),
      shinydashboard::menuItem("Automation", tabName = "automation", icon = icon("robot")),
      shinydashboard::menuItem("Today", tabName = "today", icon = icon("bolt")),
      shinydashboard::menuItem("Your team", tabName = "yourteam", icon = icon("users")),
      shinydashboard::menuItem("Market", tabName = "market", icon = icon("money-bill-trend-up")),
      shinydashboard::menuItem("Players", tabName = "players_in_championship", icon = icon("table")),
      shinydashboard::menuItem("Rivals", tabName = "rivals", icon = icon("users-viewfinder")),
      shinydashboard::menuItem("Classification", tabName = "classification", icon = icon("trophy"))
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
