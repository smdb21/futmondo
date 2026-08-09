login_UI <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      width = 4,
      id = ns("login_box"),
      title = h4("Login"),
      solidHeader = TRUE,
      textInput(inputId = ns("user_name"), label = "User name:", placeholder = "User name", value = Sys.getenv("user_name")),
      passwordInput(inputId = ns("password"), label = "Password:", placeholder = "password", value = Sys.getenv("password")),
      actionButton(inputId = ns("login_button"), label = "Login"),
      div(style = "color: #64748b; font-size: 11px; margin-top: 15px; display: flex; flex-direction: column; gap: 6px;",
        div(style = "display: flex; align-items: flex-start; gap: 6px;",
          shiny::tags$i(class = "fa-solid fa-server", style = "color: #3b82f6; margin-top: 2px;"),
          span("Log in with your Futmondo account to authenticate directly with the official Futmondo server.")
        ),
        div(style = "display: flex; align-items: flex-start; gap: 6px;",
          shiny::tags$i(class = "fa-solid fa-shield-halved", style = "color: #10b981; margin-top: 2px;"),
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

login_Server <- function(id, user, password) {
  moduleServer(id, function(input, output, session) {
    login_token_RV <- reactiveVal(NA)
    observeEvent(
      {
        input$login_button
      },
      {
        user_name <- input$user_name
        password <- input$password
        login_token <- NULL
        tryCatch(
          {
            login_token <- login(user_name, password)
          },
          error = function(e) {
            print(paste0("Login failed: ", e$message))
            login_token_RV(e$message)
            return(NULL)
          }
        )
        login_token_RV(login_token)
      }
    )


    # initial welcome card ----
    output$text <- renderUI({
      div(
        style = "padding: 10px;",
        h3(style = "font-weight: 700; color: #0f172a; margin-top: 0; font-size: 20px;", "Welcome to Futmondo Insights"),
        p(style = "color: #64748b; font-size: 13px; margin-bottom: 20px;",
          "Advanced analytics and market intelligence platform for your Futmondo leagues."
        ),
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
          div(
            style = "background: #f8fafc; padding: 14px; border-radius: 8px; border: 1px solid #e2e8f0;",
            shiny::tags$i(class = "fa-solid fa-chart-line", style = "color: #3b82f6; font-size: 20px;"),
            h5(style = "font-weight: 600; margin: 8px 0 4px 0; color: #0f172a;", "Valuation Trends"),
            p(style = "color: #64748b; font-size: 12px; margin: 0;", "Track daily price movements and market value history.")
          ),
          div(
            style = "background: #f8fafc; padding: 14px; border-radius: 8px; border: 1px solid #e2e8f0;",
            shiny::tags$i(class = "fa-solid fa-coins", style = "color: #10b981; font-size: 20px;"),
            h5(style = "font-weight: 600; margin: 8px 0 4px 0; color: #0f172a;", "League Finances"),
            p(style = "color: #64748b; font-size: 12px; margin: 0;", "Monitor rival liquid cash balances and squad purchase costs.")
          ),
          div(
            style = "background: #f8fafc; padding: 14px; border-radius: 8px; border: 1px solid #e2e8f0;",
            shiny::tags$i(class = "fa-solid fa-hand-holding-dollar", style = "color: #f59e0b; font-size: 20px;"),
            h5(style = "font-weight: 600; margin: 8px 0 4px 0; color: #0f172a;", "Smart Bidding"),
            p(style = "color: #64748b; font-size: 12px; margin: 0;", "Place, update, or cancel transfer market offers and clause buyouts.")
          ),
          div(
            style = "background: #f8fafc; padding: 14px; border-radius: 8px; border: 1px solid #e2e8f0;",
            shiny::tags$i(class = "fa-solid fa-user-ninja", style = "color: #8b5cf6; font-size: 20px;"),
            h5(style = "font-weight: 600; margin: 8px 0 4px 0; color: #0f172a;", "Rival Scouting"),
            p(style = "color: #64748b; font-size: 12px; margin: 0;", "Scout rival rosters, position counts, and release clause timers.")
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
              style = "padding: 15px; background-color: #fef2f2; border: 1px solid #fecaca; border-radius: 8px; color: #991b1b;",
              h4(style = "margin-top: 0; font-weight: 700;", "Login Failed"),
              p(style = "margin-bottom: 0;", "Could not authenticate with Futmondo. Please check your username and password.")
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
              shiny::tags$i(class = "fa-solid fa-circle-check", style = "font-size: 28px; color: #10b981;"),
              div(
                h3(style = "font-weight: 700; color: #0f172a; margin: 0; font-size: 18px;", paste0("Welcome back, ", user_name, "!")),
                p(style = "color: #64748b; margin: 2px 0 0 0; font-size: 13px;", "Authenticated successfully with Futmondo")
              )
            ),
            div(
              style = "background-color: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px 16px; margin-top: 10px;",
              p(style = "margin: 0; font-size: 13px; color: #334155;",
                icon("shield-halved", style = "color: #3b82f6; margin-right: 6px;"),
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
