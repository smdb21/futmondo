
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
    observeEvent({
      input$login_button
    }, {
      user_name <- input$user_name
      password <- input$password
      login_token <- NULL
      tryCatch({
        login_token <- login(user_name, password)
      }, error = function(e) {
        login_token_RV(e$message)
        return(NULL)
      })
      login_token_RV(login_token)
    })


    # observers ----
    ## observe login_token ----
    observeEvent(login_token_RV(), {
      login_token <- login_token_RV()
      # if login_token is null
      if (is.null(login_token) || length(login_token)!=3) {
        output$text <- renderUI({
          tagList(
            div(style = "color: red;",
            h4(paste("Login failed"))
            )
          )
        })
        return()
      }
      token <- login_token[['token']]
      user_name <- login_token[['user_name']]
      output$text <- renderUI({
        tagList(
          h4("Login successful"),
          h5(paste("User name: ", user_name)),
          h5(paste("Token: ", token))
        )
      })
      updateBox(id = "login_result_box",
                action = "update",
                options = list(
                  title = h4("Welcome", dashboardLabel(user_name, status = "primary")),
                  status = "warning",
                  solidHeader = TRUE,
                  width = 8,
                  background = NULL,
                  # height = "900px",
                  closable = FALSE
                )
      )
    }, ignoreNULL = FALSE,
    ignoreInit = TRUE)
    
    return(login_token_RV)
  }
  )
}
