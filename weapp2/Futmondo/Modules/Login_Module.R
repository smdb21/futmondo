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
      actionButton(inputId = ns("login_button"), label = "Login")
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


    # observers ----
    ## observe login_token ----
    observeEvent(login_token_RV(),
      {
        login_token <- login_token_RV()
        # if login_token is null
        if (is.null(login_token) || length(login_token) != 3) {
          output$text <- renderUI({
            tagList(
              div(
                style = "color: red;",
                h4(paste("Login failed"))
              )
            )
          })
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
