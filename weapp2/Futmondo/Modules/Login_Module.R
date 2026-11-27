# The public inventory is compared with official workflows in docs/feature_comparison.md.
login_feature_tiers <- function() {
  feature <- function(id, title, description) list(id = id, title = title, description = description)
  list(
    list(id = "essentials", title = "Essentials", icon = "compass",
      summary = "Find players, check your budget and see what changes before kickoff.",
      features = list(
        feature("scouting_filters", "Find players who fit your squad",
          "Filter by club, fantasy owner, positions, price movement and active clauses in one place."),
        feature("kickoff_balance", "Check your balance before kickoff",
          "See how much cash your open offers could use, what you would have left and how much your league lets you spend."),
        feature("clause_transfer_timing", "See who arrives before kickoff",
          "Check incoming signings and departures against the next round’s kickoff."),
        feature("squad_listings", "Track your listed players",
          "See listing deadlines in My Team and Market. Home counts your listed players and those with incoming offers."),
        feature("squad_status", "Check squad availability",
          "Review reported injuries, doubts, suspensions and other absences together in My Team."),
        feature("round_range_standings", "Compare a stretch of rounds",
          "Choose a round range to compare teams’ points and their points, position, Best XI and MVP bonuses."),
        feature("league_charts", "Follow your league over time",
          "Switch between points and ranks over selected rounds, compare squad values, and see which clubs supply your league’s players."),
        feature("page_refresh", "Refresh the page you’re viewing",
          "Request updated standings, listings or lineup data before the next scheduled check."),
        feature("local_timezone", "Read deadlines in your timezone",
          "See kickoffs and transfer history in local time or choose a timezone for your account.")
      )),
    list(id = "advanced", title = "Advanced", icon = "chart-line",
      summary = "Compare lineups, measure player value and understand your rivals’ finances.",
      features = list(
        feature("lineup_optimizer", "Compare your best XI and bench",
          "Use 1–20 completed rounds to compare allowed formations. Check bench cover and edit the XI on the pitch to see how its points estimate changes."),
        feature("price_efficiency", "Find points for your budget",
          "Compare recent points per million of market value or clause price across players."),
        feature("rival_cash", "Trace a rival’s estimated balance",
          "Follow opening cash, transfers and league rewards in a running ledger. See where the evidence is incomplete."),
        feature("live_substitutions", "Review live bench substitutions",
          "Tap to check your starters against announced La Liga, Premier League or Serie A lineups and see eligible bench replacements before kickoff. Confirm a swap from Lineup or Classification."),
        feature("scheduled_lineups", "Prepare for confirmed signings",
          "Include signings due before kickoff in a draft XI and bench. Where enabled, approve a lineup submission timed for an incoming signing."),
        feature("round_award_rankings", "Find repeat Best XI and MVP picks",
          "Compare Best XI appearances and MVP awards by player or club across confirmed rounds."),
        feature("recent_average", "Rank players on recent form",
          "Choose up to 20 recent rounds and sort players by average points across market and squad tables."),
        feature("squad_value_trend", "Track daily squad value changes",
          "See the net change for your squad or a rival’s and how many players rose or fell."),
        feature("bulk_listing_preview", "Price your squad for the market",
          "Set a markup on market value or a locked clause. Preview each eligible player’s asking price and the squad total before you confirm the listings."),
        feature("transfer_histogram", "See when your league is spending",
          "Compare money moved through league transfers by day, week or month."),
        feature("trading_profit", "Compare profit on completed deals",
          "Inspect matched purchases, sales and fees alongside league trading profit and estimated wealth gained."),
        feature("scheduled_clauses", "Plan a capped clause purchase",
          "Where enabled, approve a one-time attempt for a rival player’s published opening. See scheduled buyouts on Home, review the recorded outcome or cancel a pending attempt.")
      ))
  )
}

login_features_UI <- function(id) {
  ns <- NS(id)
  feature_row <- function(item) tags$li(`data-feature` = item$id,
    tags$h4(item$title), tags$p(item$description))
  tags$section(class = "fm-welcome-tiers", `aria-label` = "LigaRadar features",
    lapply(login_feature_tiers(), function(tier) {
      tags$section(class = paste("fm-welcome-tier", paste0("fm-welcome-tier-", tier$id)),
        `aria-labelledby` = ns(paste0("features_", tier$id)),
        div(class = "fm-welcome-tier-heading", icon(tier$icon),
          tags$h2(id = ns(paste0("features_", tier$id)), tier$title)),
        tags$p(class = "fm-welcome-tier-summary", tier$summary),
        tags$ul(lapply(head(tier$features, 3L), feature_row)),
        tags$details(class = "fm-welcome-more",
          tags$summary(paste("More", tier$title, "features"), icon("chevron-down")),
          tags$ul(lapply(tail(tier$features, -3L), feature_row)))
      )
    })
  )
}

login_account_UI <- function(id) {
  actionButton(NS(id, "logout_button"), "Log out", icon = icon("right-from-bracket"))
}

login_UI <- function(id) {
  ns <- NS(id)
  remember <- checkboxInput(ns("remember_login"), "Remember me on this device for 7 days", value = FALSE)
  if (!exists("remember_login_enabled", mode = "function") || !remember_login_enabled())
    remember <- htmltools::tagQuery(remember)$find("input")$addAttrs(disabled = "disabled")$allTags()
  tags$div(class = "fm-welcome", `data-fm-welcome` = ns(""),
    singleton(shiny::includeCSS("www/welcome.css")),
    singleton(shiny::includeScript("www/welcome.js")),
    singleton(tags$script(src = "remember-login.js")),
    conditionalPanel(condition = "output.signed_in !== 'TRUE'", ns = ns,
      tags$section(class = "fm-welcome-public", `aria-labelledby` = ns("public_title"),
        div(class = "fm-welcome-hero",
          div(class = "fm-welcome-art",
            fm_loading_visual(ns("welcome_visual")),
            tags$button(id = ns("toggle_motion"), type = "button", class = "fm-motion-toggle",
              `aria-label` = "Pause animation", title = "Pause animation",
              icon("pause"), icon("play"))),
          tags$h1(id = ns("public_title"), "Welcome to LigaRadar"),
          tags$p(class = "fm-welcome-intro",
            "Use your Futmondo league’s results, transfers and prices to choose your XI, compare players and understand rival budgets."),
          conditionalPanel(condition = "output.login_form_visible === 'TRUE'", ns = ns,
            tags$button(id = ns("open_login"), type = "button", class = "btn fm-welcome-cta",
              disabled = "disabled", `aria-haspopup` = "dialog", `aria-controls` = ns("login_dialog"),
              "Log in with Futmondo", icon("arrow-right"))),
          uiOutput(ns("database_status"), inline = FALSE, container = div, div(role = "status", `aria-live` = "polite", "Checking database connection…")),
          uiOutput(ns("access_notice"))
        ),
        login_features_UI(id)
      )
    ),
    tags$dialog(id = ns("login_dialog"), class = "fm-login-dialog", `aria-labelledby` = ns("dialog_title"),
      tags$header(
        tags$h2(id = ns("dialog_title"), "Log in to LigaRadar"),
        tags$button(id = ns("close_login"), type = "button", class = "fm-dialog-close",
          `aria-label` = "Close login", title = "Close login", icon("xmark"))),
      tags$p("Log in with your Futmondo account to authenticate directly with the official Futmondo server."),
      textInput(ns("user_name"), "User name", placeholder = "User name"),
      passwordInput(ns("password"), "Password"),
      remember, uiOutput(ns("remember_availability")),
      tags$p(class = "fm-login-sync-notice", "Signing in starts automatic data synchronization, including after logout. Access is encrypted and your password is never saved. Use Disconnect account to stop synchronization."),
      actionButton(ns("login_button"), "Log in", class = "fm-welcome-cta", disabled = TRUE),
      div(id = ns("login_progress"), class = "login-feedback", hidden = "hidden",
        role = "status", `aria-live` = "polite", icon("spinner", class = "fa-spin"), span("Connecting to LigaRadar…")),
      uiOutput(ns("login_feedback"))
    )
  )
}

login_Server <- function(id, user = NULL, password = NULL, database_probe = fm_database_probe) {
  moduleServer(id, function(input, output, session) {
    login_token_RV <- reactiveVal(NULL)
    login_pending_RV <- reactiveVal(FALSE)
    database_state <- reactiveVal("checking")
    database_busy <- FALSE
    database_owner <- paste0("database:", session$token)
    database_ready <- function() identical(isolate(database_state()), "ready")
    # Keep credentials hidden from the first browser paint until cookie probing ends.
    restore_checking <- reactiveVal(exists("remember_login_session", mode="function") &&
      exists("remember_login_enabled", mode="function") && remember_login_enabled())
    output$login_form_visible <- renderText((!restore_checking() || identical(database_state(), "error")) && !valid_login(login_token_RV()))
    output$signed_in <- renderText(valid_login(login_token_RV()))
    outputOptions(output, "login_form_visible", suspendWhenHidden=FALSE)
    outputOptions(output, "signed_in", suspendWhenHidden=FALSE)
    generation <- 0L
    closed <- FALSE
    if(is.null(session$userData$startup_stage))session$userData$startup_stage <- reactiveVal("signed_out")
    session$onSessionEnded(function(){closed<<-TRUE;generation<<-generation+1L;fm_transport_cancel(session$token);fm_transport_cancel(database_owner)})
    # Always visible, including while the saved-login probe hides credentials.
    # Do not move this blocking error into the collapsed information panel.
    output$database_status <- renderUI({
      state <- database_state()
      if (identical(state, "ready")) {
        if (restore_checking() && !valid_login(login_token_RV()))
          return(div(role = "status", `aria-live` = "polite", "Checking saved login…"))
        return(NULL)
      }
      if (identical(state, "checking")) return(div(role="status", `aria-live`="polite",
        icon("spinner", class="fa-spin"), " Checking database connection…"))
      div(class="alert alert-danger", role="alert",
        tags$strong("Database unavailable"),
        p("LigaRadar could not connect to its database. Login is disabled. Retry the connection; if it still fails, contact the app administrator."),
        actionButton(session$ns("database_retry"), "Retry connection"))
    })
    outputOptions(output, "database_status", suspendWhenHidden=FALSE)
    # Authentication feedback is intentionally independent from the token.
    # A failed first login changes NULL to NULL, which must still be visible.
    login_feedback_RV <- reactiveVal(NULL)
    output$login_feedback <- renderUI({
      feedback <- login_feedback_RV()
      if (is.null(feedback) || !identical(feedback$kind, "error")) return(NULL)
      fm_info(feedback$message, error = TRUE, inline = TRUE,
        class=paste("login-feedback", paste0("login-feedback-", feedback$kind)))
    })
    outputOptions(output, "login_feedback", suspendWhenHidden = FALSE)
    # Access can also be revoked while the credentials dialog is closed.
    access_notice <- reactiveVal(NULL)
    output$access_notice <- renderUI({
      message <- access_notice()
      if (!is.null(message)) fm_info(message, error = TRUE, inline = TRUE)
    })
    remember_retry <- reactiveVal(NULL)
    remember_feedback <- reactiveVal(NULL)
    remembered <- NULL
    output$remember_availability <- renderUI({
      available <- exists("remember_login_enabled",mode="function") && remember_login_enabled()
      if (!available) tags$p(class = "fm-login-note", "Remembered login is unavailable on this deployment.")
    })
    outputOptions(output, "remember_availability", suspendWhenHidden = FALSE)
    output$remember_feedback <- renderUI({
      feedback<-remember_feedback();if(is.null(feedback))return(NULL)
      fm_info(feedback$message,
        if(!is.null(remember_retry()))actionButton(session$ns("remember_retry"),"Retry saved access"),
        error=feedback$kind %in% c("error","warning"),
        class=paste("login-feedback",paste0("login-feedback-",feedback$kind)))
    })
    signed_out <- function(){
      restore_checking(FALSE)
      generation <<- generation+1L
      login_pending_RV(FALSE)
      fm_transport_cancel(session$token)
      session$userData$startup_stage("signed_out")
      session$sendCustomMessage(session$ns("login_state"),list(busy=FALSE))
      old <- isolate(login_token_RV())
      if (valid_login(old)) clear_api_cache(old[["userid"]])
      session$userData$futmondo_user_id <- NULL
      login_token_RV(NULL)
      login_feedback_RV(NULL)
      access_notice(NULL)
    }
    deny_access <- function(unavailable = FALSE) {
      signed_out()
      message <- if (unavailable) "App access could not be verified. Please try logging in again shortly." else
        "The app is currently restricted to administrators. Please try again when access is reopened."
      login_feedback_RV(list(kind = "error", message = message))
      access_notice(message)
    }
    authorize <- function(token, on_allowed) {
      if (closed || !database_ready()) return(promises::promise_resolve(FALSE))
      attempt <- generation
      promises::then(fm_app_access(token, owner=session$token), function(allowed) {
        if (closed || !database_ready() || !identical(attempt, generation)) return(FALSE)
        if (isTRUE(allowed)) { on_allowed(); return(TRUE) }
        deny_access(); FALSE
      }, function(error) {
        if (!closed && identical(attempt, generation)) deny_access(TRUE)
        FALSE
      })
    }
    # One bounded asynchronous read every 2 minutes for admitted non-admins.
    # The database remains authoritative across processes and redeployments.
    last_access_token <- NULL
    access_check_busy <- FALSE
    observe({
      token <- login_token_RV()
      if (!valid_login(token) || is_authorized_admin(token)) {
        last_access_token <<- NULL
        return()
      }
      invalidateLater(120000, session)
      if (!identical(token, last_access_token)) {
        last_access_token <<- token
        return() # Admission already checked the database.
      }
      if (access_check_busy) return()
      access_check_busy <<- TRUE
      promises::then(authorize(token, function() NULL), function(allowed) {
        access_check_busy <<- FALSE
        NULL
      }, function(error) {
        access_check_busy <<- FALSE
        NULL
      })
      invisible(NULL)
    }, priority=200)
    start_remembered <- function() {
      if(closed || !database_ready() || !is.null(remembered)) return(invisible(NULL))
      if(exists("remember_login_session",mode="function"))remembered<<-remember_login_session(session,
      on_restore=function(token){
        if(closed || !database_ready()) return(promises::promise_resolve(FALSE))
        generation<<-generation+1L
        authorize(token, function() {
          session$userData$futmondo_user_id<-token[["userid"]]
          session$userData$startup_stage("loading_leagues")
          login_feedback_RV(list(kind="success",message="Session restored. Loading your leagues…"))
          login_token_RV(token)
          session$sendCustomMessage(session$ns("login_admitted"), list())
        })
      },
      on_status=function(kind,message,retry=NULL){remember_feedback(list(kind=kind,message=message));remember_retry(retry)},
      on_begin=function(){restore_checking(TRUE);login_pending_RV(TRUE);remember_feedback(NULL);remember_retry(NULL)
        session$userData$startup_stage("authenticating")
        session$sendCustomMessage(session$ns("login_state"),list(busy=TRUE,message="Restoring your session…"))
        login_feedback_RV(list(kind="success",message="Restoring your session…"))},
      on_end=function(){restore_checking(FALSE);login_pending_RV(FALSE);if(!valid_login(isolate(login_token_RV())))session$userData$startup_stage("signed_out")
        session$sendCustomMessage(session$ns("login_state"),list(busy=FALSE))},
      on_expire=function(){signed_out();remember_feedback(list(kind="error",message="Your remembered session expired after 7 days. Log in again."))})
      invisible(NULL)
    }
    check_database <- function() {
      if(closed || database_busy || database_ready()) return(invisible(NULL))
      database_busy <<- TRUE
      database_state("checking")
      session$sendCustomMessage(session$ns("database_state"), list(ready=FALSE))
      request <- tryCatch(database_probe(owner=database_owner), error=promises::promise_reject)
      finish <- function(ready=FALSE) {
        if(closed) return(invisible(NULL))
        database_busy <<- FALSE
        database_state(if(isTRUE(ready)) "ready" else "error")
        session$sendCustomMessage(session$ns("database_state"), list(ready=isTRUE(ready)))
        if(isTRUE(ready)) shiny::withReactiveDomain(session, start_remembered())
        invisible(NULL)
      }
      promises::then(request, function(ready) finish(identical(ready, TRUE)), function(error) finish())
      invisible(NULL) # Release the Shiny flush while the HTTP request is pending.
    }
    observeEvent(TRUE, check_database(), once=TRUE)
    observeEvent(input$database_retry, check_database())
    observeEvent(input$remember_retry,{
      if(closed || !database_ready()) return()
      if(!is.null(remembered)){
        action <- isolate(remember_retry())
        if(identical(action,"restore"))restore_checking(TRUE)
        remembered$retry(action)
      }
    })
    observeEvent(input$remember_result,{if(!is.null(remembered))remembered$result(input$remember_result)})
    observeEvent(input$logout_button, {
      signed_out()
      remember_feedback(NULL);remember_retry(NULL)
      if(!is.null(remembered))remembered$logout()
      updateTextInput(session, "user_name", value = "")
      updateTextInput(session, "password", value = "")
    })
    login_root<-session$rootScope() %||% session
    login_root$userData$disconnect_login<-function(){
      signed_out()
      if(!is.null(remembered))remembered$logout()
      updateTextInput(session,"password",value="")
    }
    observeEvent(input$login_button, {
      if(closed || !database_ready()) {
        session$sendCustomMessage(session$ns("login_state"),list(busy=FALSE))
        return()
      }
      if(isTRUE(login_pending_RV()) || isTRUE(restore_checking()) || valid_login(isolate(login_token_RV()))) return()
      access_notice(NULL)
      user_name <- trimws(input$user_name %||% "")
      secret <- input$password %||% ""
      updateTextInput(session,"password",value="")
      if(!nzchar(user_name)||!nzchar(secret)) {
        login_feedback_RV(list(kind="error",message="Enter your username and password to log in."))
        session$sendCustomMessage(session$ns("login_state"),list(busy=FALSE))
        return()
      }
      if(!is.null(remembered))remembered$cancel()
      generation <<- generation+1L
      attempt <- generation
      login_pending_RV(TRUE)
      session$userData$startup_stage("authenticating")
      login_feedback_RV(NULL)
      started <- Sys.time()
      request <- tryCatch(fm_async_login(user_name,secret,owner=session$token),
        error=function(e)promises::promise_reject(e))
      secret <- NULL
      finish <- function(token=NULL) {
        if(closed||!database_ready()||!identical(attempt,generation))return(invisible(NULL))
        resource_timing("authentication",started)
        login_pending_RV(FALSE)
        session$sendCustomMessage(session$ns("login_state"),list(busy=FALSE))
        if(valid_login(token)) {
          login_pending_RV(TRUE)
          session$sendCustomMessage(session$ns("login_state"),list(busy=TRUE,message="Checking app access…"))
          authorize(token, function() {
            login_pending_RV(FALSE)
            session$sendCustomMessage(session$ns("login_state"),list(busy=FALSE))
            session$userData$futmondo_user_id <- token[["userid"]]
            session$userData$startup_stage("loading_leagues")
            login_feedback_RV(list(kind="success",message="Signed in. Loading your leagues…"))
            login_token_RV(token)
            session$sendCustomMessage(session$ns("login_admitted"), list())
            remember_feedback(NULL);remember_retry(NULL)
            if(!is.null(remembered))remembered$login(token,isTRUE(isolate(input$remember_login)))
          })
        } else {
          session$userData$futmondo_user_id <- NULL
          session$userData$startup_stage("failed")
          login_feedback_RV(list(kind="error",message="Could not authenticate with LigaRadar. Check your username and password, then try again."))
          login_token_RV(NULL)
        }
        invisible(NULL)
      }
      promises::then(request,function(token)finish(token),function(error)finish())
      # Completion updates state separately; release this observer's flush now.
      invisible(NULL)
    })


    return(login_token_RV)
  })
}
