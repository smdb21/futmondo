# Public, static inventory; comparison evidence lives in docs/feature_comparison.md.
# Keep higher-value analysis in Advanced without implying access restrictions.
login_feature_tiers <- function() {
  list(
    list(id = "essentials", title = "Essentials", summary = "Explore your league in more detail.",
      features = list(
        list(id = "scouting_filters", title = "Combined scouting filters",
          description = "Narrow player tables by owner, position, value, daily price movement and active clauses. Choose whether to include your own team in All Players."),
        list(id = "round_range_standings", title = "Standings for any round range",
          description = "Choose a stretch of rounds to compare team points and points, position, Best XI and MVP bonuses."),
        list(id = "league_charts", title = "League comparison charts",
          description = "Compare squad values and rank progression, and see fantasy-owned players grouped by real club.")
      )),
    list(id = "advanced", title = "Advanced", summary = "Go deeper into lineups, player value and rival finances.",
      features = list(
        list(id = "lineup_optimizer", title = "Best XI + bench optimization",
          description = "Build a lineup from your chosen 1–20 completed rounds, comparing allowed formations and bench coverage, with injury and next-round departure checks. Review and save it yourself."),
        list(id = "recent_average", title = "Player rankings by last-N average",
          description = "Sort players by their average over your chosen 1–20 recent rounds across market, squad and player tables."),
        list(id = "price_efficiency", title = "Points per million",
          description = "Rank recent points relative to market value or clause price to compare player value."),
        list(id = "rival_cash", title = "Reconstructed rival balances",
          description = "Inspect estimated rival cash and its running ledger of opening funds, transfers and configured rewards, with missing evidence identified."),
        list(id = "scheduled_clauses", title = "Scheduled clause purchases",
          description = "Authorize a capped purchase attempt at a rival player's published clause opening, with pending-offer reservations, cancellation and saved execution outcomes. Success is not guaranteed."),
        list(id = "trading_profit", title = "Trading profit and wealth comparisons",
          description = "Compare league trading profit and estimated wealth gained, and inspect matched player purchases, sales and related fees.")
      ))
  )
}

login_features_UI <- function(id) {
  ns <- NS(id)
  tags$section(class = "fm-login-features", `aria-labelledby` = ns("features_heading"),
    h3(id = ns("features_heading"), "More from your Futmondo league"),
    p(class = "fm-login-features-intro",
      "LigaRadar adds these workflows beyond the features described in the official Futmondo guides."),
    div(class = "fm-login-feature-tiers",
      lapply(login_feature_tiers(), function(tier) {
        tags$section(class = paste("fm-login-feature-tier", paste0("fm-login-feature-tier-", tier$id)),
          `aria-labelledby` = ns(paste0("features_", tier$id)),
          h4(id = ns(paste0("features_", tier$id)), tier$title),
          p(class = "fm-login-feature-summary", tier$summary),
          tags$ul(lapply(tier$features, function(feature) {
            tags$li(`data-feature` = feature$id,
              tags$details(class = "fm-login-feature",
                tags$summary(
                  tags$span(class = "fm-login-feature-toggle", `aria-hidden` = "true"),
                  tags$span(feature$title)
                ),
                p(feature$description)
              )
            )
          }))
        )
      })
    ),
    p(class = "fm-login-features-note",
      "Official app options can vary by league configuration. LigaRadar respects permitted formations and player eligibility; analysis depends on source data. Comparison: ",
      tags$a(href = "https://help.futmondo.com/article/91-reglas-del", "official rules",
        target = "_blank", rel = "noopener noreferrer"), ", ",
      tags$a(href = "https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/",
        "official app feature guide", target = "_blank", rel = "noopener noreferrer"), ".")
  )
}

login_UI <- function(id) {
  ns <- NS(id)
  div(
    style = "display: flex; flex-wrap: wrap; gap: 15px; align-items: flex-start;",
    conditionalPanel(
      condition = "output.login_form_visible === 'TRUE'", ns = ns,
      style = "flex: 1 1 300px; min-width: 0;",
    shinydashboardPlus::box(
      width = 12,
      id = ns("login_box"),
      title = h4("Welcome"),
      solidHeader = TRUE,
      textInput(inputId = ns("user_name"), label = "User name:", placeholder = "User name", value = ""),
      passwordInput(inputId = ns("password"), label = "Password:", placeholder = "password", value = ""),
      local({
        control<-checkboxInput(ns("remember_login"), "Remember me on this device for 7 days", value=FALSE)
        if(!exists("remember_login_enabled",mode="function")||!remember_login_enabled())
          control<-htmltools::tagQuery(control)$find("input")$addAttrs(disabled="disabled")$allTags()
        control
      }),
      uiOutput(ns("remember_availability")),
      singleton(tags$script(src="remember-login.js")),
      actionButton(inputId = ns("login_button"), label = "Login"),
      div(
        id = ns("login_progress"), class = "login-feedback", style = "display: none;",
        role = "status", `aria-live` = "polite",
        icon("spinner", class = "fa-spin"), span("Connecting to LigaRadar…")
      ),
      uiOutput(ns("login_feedback")),
      tags$script(id = ns("login_feedback_script"), HTML(paste0(
        "(function(){function bindLogin(){",
        "var login=document.getElementById('", ns("login_button"), "');",
        "if(!login||login.dataset.futmondoLoginBound)return;",
        "login.dataset.futmondoLoginBound='true';",
        "var progress=document.getElementById('", ns("login_progress"), "');",
        "var feedback=document.getElementById('", ns("login_feedback"), "');",
        "var logout=document.getElementById('", ns("logout_button"), "');",
        "var pending=false;var label=login.textContent;",
        "function setBusy(busy,message){pending=!!busy;login.disabled=pending;logout.disabled=pending;",
        "login.setAttribute('aria-busy',String(pending));",
        "login.textContent=pending?'Logging in…':label;",
        "progress.querySelector('span').textContent=message||'Connecting to LigaRadar…';progress.style.display=pending?'flex':'none';feedback.style.display=pending?'none':'';}",
        # Capture runs before Shiny's action-button handler, so feedback is immediate.
        "login.addEventListener('click',function(event){",
        "if(pending){event.preventDefault();event.stopImmediatePropagation();return;}",
        "setBusy(true);},true);",
        "['", ns("user_name"), "','", ns("password"), "'].forEach(function(id){",
        "document.getElementById(id).addEventListener('keydown',function(event){",
        "if(event.key==='Enter'&&!event.isComposing){event.preventDefault();",
        "if(!pending)login.click();}});});",
        "Shiny.addCustomMessageHandler('", ns("login_state"), "',function(message){setBusy(message.busy,message.message);});",
        "}if(document.readyState==='loading'){document.addEventListener('DOMContentLoaded',bindLogin,{once:true});}",
        "else{bindLogin();}})();"
      ))),
      fm_info(
        div(style = "display: flex; align-items: flex-start; gap: 6px;",
          shiny::tags$i(class = "fa-solid fa-server", style = "color: var(--fm-text); margin-top: 2px;"),
          span("Log in with your Futmondo account to authenticate directly with the official Futmondo server.")
        ),
        div(style = "display: flex; align-items: flex-start; gap: 6px;",
          shiny::tags$i(class = "fa-solid fa-shield-halved", style = "color: var(--fm-text); margin-top: 2px;"),
          span("Your password is never saved or stored anywhere.")
        )
      )
    )),
    div(style = "flex: 2 1 450px; min-width: 0;",
    shinydashboardPlus::box(
      id = ns("login_result_box"),
      width = 12,
      uiOutput(ns("text")),
      conditionalPanel(condition = "output.signed_in === 'TRUE'", ns = ns,
        actionButton(inputId = ns("logout_button"), label = "Logout")),
      login_features_UI(id)
    ))
  )
}

login_Server <- function(id, user = NULL, password = NULL) {
  moduleServer(id, function(input, output, session) {
    login_token_RV <- reactiveVal(NULL)
    login_pending_RV <- reactiveVal(FALSE)
    # Keep credentials hidden from the first browser paint until cookie probing ends.
    restore_checking <- reactiveVal(exists("remember_login_session", mode="function") &&
      exists("remember_login_enabled", mode="function") && remember_login_enabled())
    output$login_form_visible <- renderText(!restore_checking() && !valid_login(login_token_RV()))
    output$signed_in <- renderText(valid_login(login_token_RV()))
    outputOptions(output, "login_form_visible", suspendWhenHidden=FALSE)
    outputOptions(output, "signed_in", suspendWhenHidden=FALSE)
    generation <- 0L
    closed <- FALSE
    if(is.null(session$userData$startup_stage))session$userData$startup_stage <- reactiveVal("signed_out")
    session$onSessionEnded(function(){closed<<-TRUE;generation<<-generation+1L;fm_transport_cancel(session$token)})
    # Authentication feedback is intentionally independent from the token.
    # A failed first login changes NULL to NULL, which must still be visible.
    login_feedback_RV <- reactiveVal(NULL)
    output$login_feedback <- renderUI({
      feedback <- login_feedback_RV()
      if (is.null(feedback)) return(NULL)
      fm_info(feedback$message, error=identical(feedback$kind, "error"),
        class=paste("login-feedback", paste0("login-feedback-", feedback$kind)))
    })
    remember_retry <- reactiveVal(NULL)
    remember_feedback <- reactiveVal(NULL)
    remembered <- NULL
    output$remember_availability <- renderUI({
      available <- exists("remember_login_enabled",mode="function") && remember_login_enabled()
      if (!available) fm_info("Remembered login is not enabled on this deployment. You can log in normally.")
    })
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
    }
    deny_access <- function(unavailable = FALSE) {
      signed_out()
      login_feedback_RV(list(kind="error", message=if (unavailable)
        "App access could not be verified. Please try logging in again shortly." else
        "The app is currently restricted to administrators. Please try again when access is reopened."))
    }
    authorize <- function(token, on_allowed) {
      attempt <- generation
      promises::then(fm_app_access(token, owner=session$token), function(allowed) {
        if (closed || !identical(attempt, generation)) return(FALSE)
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
    if(exists("remember_login_session",mode="function"))remembered<-remember_login_session(session,
      on_restore=function(token){
        generation<<-generation+1L
        authorize(token, function() {
          session$userData$futmondo_user_id<-token[["userid"]]
          session$userData$startup_stage("loading_leagues")
          login_feedback_RV(list(kind="success",message="Session restored. Loading your leagues…"))
          login_token_RV(token)
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
    observeEvent(input$remember_retry,{
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
    observeEvent(input$login_button, {
      if(isTRUE(login_pending_RV()))return()
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
        if(closed||!identical(attempt,generation))return(invisible(NULL))
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


    # Authentication updates replace only this status; feature tiers stay mounted.
    output$text <- renderUI({
      div(
        h3("Welcome to LigaRadar"),
        p("Explore player form, compare your league and prepare your next lineup.")
      )
    })

    # observers ----
    ## observe login_token ----
    observeEvent(login_token_RV(),
      {
        login_token <- login_token_RV()
        # if login_token is null
        if (is.null(login_token) || length(login_token) != 3) {
          output$text <- renderUI({
            fm_info(login_feedback_RV()$message %||% "You are signed out.", error=TRUE,
              class="login-result login-result-error")
          })
          updateBox(
            id = "login_result_box",
            action = "update",
            options = list(
              title = h4("Authentication Status"),
              status = "danger",
              solidHeader = TRUE,
              width = 12,
              background = NULL,
              closable = FALSE
            )
          )
          return()
        }
        user_name <- login_token[["user_name"]]
        output$text <- renderUI({
          div(
            style = "padding: 10px; text-align: left;",
            div(
              style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;",
              shiny::tags$i(class = "fa-solid fa-circle-check", style = "font-size: 28px; color: var(--fm-text);"),
              div(
                h3(style = "font-weight: 700; color: var(--fm-text); margin: 0; font-size: 18px;", paste0("Welcome back, ", user_name, "!")),
                p(style = "color: var(--fm-text); margin: 2px 0 0 0; font-size: 13px;", "Signed in to LigaRadar")
              )
            ),
            fm_info("Signed in. Loading your leagues…",pending=TRUE)
          )
        })
        updateBox(
          id = "login_result_box",
          action = "update",
          options = list(
            title = h4("Authentication Status"),
            status = "success",
            solidHeader = TRUE,
            width = 12,
            background = NULL,
            closable = FALSE
          )
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    return(login_token_RV)
  })
}
