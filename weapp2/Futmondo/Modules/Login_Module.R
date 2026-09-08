login_UI <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      width = 4,
      id = ns("login_box"),
      title = h4("Login"),
      solidHeader = TRUE,
      textInput(inputId = ns("user_name"), label = "User name:", placeholder = "User name", value = ""),
      passwordInput(inputId = ns("password"), label = "Password:", placeholder = "password", value = ""),
      actionButton(inputId = ns("login_button"), label = "Login"),
      actionButton(inputId = ns("logout_button"), label = "Logout"),
      uiOutput(ns("login_feedback")),
      tags$script(HTML(paste0(
        "$(function(){var password=$('#", ns("password"), "');var login=$('#", ns("login_button"),
        "');password.off('keydown.futmondoLogin').on('keydown.futmondoLogin',function(event){",
        "if(event.key==='Enter'&&!event.isComposing){event.preventDefault();login.trigger('click');}});});"
      ))),
      div(style = "color: var(--fm-text); font-size: 11px; margin-top: 15px; display: flex; flex-direction: column; gap: 6px;",
        div(style = "display: flex; align-items: flex-start; gap: 6px;",
          shiny::tags$i(class = "fa-solid fa-server", style = "color: var(--fm-text); margin-top: 2px;"),
          span("Log in with your Futmondo account to authenticate directly with the official Futmondo server.")
        ),
        div(style = "display: flex; align-items: flex-start; gap: 6px;",
          shiny::tags$i(class = "fa-solid fa-shield-halved", style = "color: var(--fm-text); margin-top: 2px;"),
          span("Your password is never saved or stored anywhere.")
        )
      )
    ),
    shinydashboardPlus::box(
      id = ns("login_result_box"),
      width = 8,
      uiOutput(ns("text"))
    )
  )
}

login_Server <- function(id, user = NULL, password = NULL) {
  moduleServer(id, function(input, output, session) {
    login_token_RV <- reactiveVal(NULL)
    # Authentication feedback is intentionally independent from the token.
    # A failed first login changes NULL to NULL, which must still be visible.
    login_feedback_RV <- reactiveVal(NULL)
    output$login_feedback <- renderUI({
      feedback <- login_feedback_RV()
      if (is.null(feedback)) return(NULL)
      div(
        class = paste("login-feedback", paste0("login-feedback-", feedback$kind)),
        role = "alert",
        `aria-live` = "assertive",
        icon(if (identical(feedback$kind, "error")) "triangle-exclamation" else "circle-check"),
        span(feedback$message)
      )
    })
    observeEvent(input$logout_button, {
      old <- isolate(login_token_RV())
      if (valid_login(old)) clear_api_cache(old[["userid"]])
      session$userData$futmondo_user_id <- NULL
      login_token_RV(NULL)
      login_feedback_RV(NULL)
      updateTextInput(session, "user_name", value = "")
      updateTextInput(session, "password", value = "")
    })
    observeEvent(
      {
        input$login_button
      },
      {
        user_name <- input$user_name
        password <- input$password
        login_token <- NULL
        failure_message <- NULL
        tryCatch(
          {
            login_token <- login(user_name, password)
          },
          error = function(e) {
            # Do not expose transport or server details in the UI or logs.
            failure_message <<- "Could not authenticate with Futmondo. Check your username and password, then try again."
            NULL
          }
        )
        updateTextInput(session, "password", value = "")
        if (valid_login(login_token)) {
          session$userData$futmondo_user_id <- login_token[["userid"]]
          login_feedback_RV(list(kind = "success", message = "Connected to Futmondo."))
        } else {
          session$userData$futmondo_user_id <- NULL
          login_feedback_RV(list(kind = "error", message = failure_message %||% "Could not authenticate with Futmondo. Check your username and password, then try again."))
        }
        login_token_RV(login_token)
      }
    )


    # initial welcome card ----
    output$text <- renderUI({
      div(
        style = "padding: 10px;",
        h3(style = "font-weight: 700; color: var(--fm-text); margin-top: 0; font-size: 20px;", "Welcome to Futmondo Insights"),
        p(style = "color: var(--fm-text); font-size: 13px; margin-bottom: 20px;",
          "Advanced analytics and market intelligence platform for your Futmondo leagues."
        ),
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
          div(
            style = "background: var(--fm-surface); padding: 14px; border-radius: 8px; border: 1px solid #e2e8f0;",
            shiny::tags$i(class = "fa-solid fa-chart-line", style = "color: var(--fm-text); font-size: 20px;"),
            h5(style = "font-weight: 600; margin: 8px 0 4px 0; color: var(--fm-text);", "Valuation Trends"),
            p(style = "color: var(--fm-text); font-size: 12px; margin: 0;", "Track daily price movements and market value history.")
          ),
          div(
            style = "background: var(--fm-surface); padding: 14px; border-radius: 8px; border: 1px solid #e2e8f0;",
            shiny::tags$i(class = "fa-solid fa-coins", style = "color: var(--fm-text); font-size: 20px;"),
            h5(style = "font-weight: 600; margin: 8px 0 4px 0; color: var(--fm-text);", "League Finances"),
            p(style = "color: var(--fm-text); font-size: 12px; margin: 0;", "Monitor rival liquid cash balances and squad purchase costs.")
          ),
          div(
            style = "background: var(--fm-surface); padding: 14px; border-radius: 8px; border: 1px solid #e2e8f0;",
            shiny::tags$i(class = "fa-solid fa-hand-holding-dollar", style = "color: var(--fm-warning); font-size: 20px;"),
            h5(style = "font-weight: 600; margin: 8px 0 4px 0; color: var(--fm-text);", "Smart Bidding"),
            p(style = "color: var(--fm-text); font-size: 12px; margin: 0;", "Place, update, or cancel transfer market offers and clause buyouts.")
          ),
          div(
            style = "background: var(--fm-surface); padding: 14px; border-radius: 8px; border: 1px solid #e2e8f0;",
            shiny::tags$i(class = "fa-solid fa-user-ninja", style = "color: var(--fm-text); font-size: 20px;"),
            h5(style = "font-weight: 600; margin: 8px 0 4px 0; color: var(--fm-text);", "Rival Scouting"),
            p(style = "color: var(--fm-text); font-size: 12px; margin: 0;", "Scout rival rosters, position counts, and release clause timers.")
          )
        )
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
            div(
              class = "login-result login-result-error",
              h4(style = "margin-top: 0; font-weight: 700;", "Login Failed"),
              p(style = "margin-bottom: 0;", "Could not authenticate with Futmondo. Check your username and password, then try again.")
            )
          })
          updateBox(
            id = "login_result_box",
            action = "update",
            options = list(
              title = h4("Authentication Status"),
              status = "danger",
              solidHeader = TRUE,
              width = 8,
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
                p(style = "color: var(--fm-text); margin: 2px 0 0 0; font-size: 13px;", "Authenticated successfully with Futmondo")
              )
            ),
            div(
              style = "background-color: var(--fm-surface); border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px 16px; margin-top: 10px;",
              p(style = "margin: 0; font-size: 13px; color: var(--fm-text);",
                icon("shield-halved", style = "color: var(--fm-text); margin-right: 6px;"),
                strong("Status: "), "Connected & Active | Redirecting to your team..."
              )
            )
          )
        })
        updateBox(
          id = "login_result_box",
          action = "update",
          options = list(
            title = h4("Authentication Status"),
            status = "success",
            solidHeader = TRUE,
            width = 8,
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
