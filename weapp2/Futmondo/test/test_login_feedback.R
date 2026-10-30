#!/usr/bin/env Rscript
# Fast server coverage; add --browser for real click/keyboard loading-state checks.
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(x, y) if (is.null(x)) y else x
valid_login <- function(x) is.character(x) && length(x) == 3L && all(c("token", "userid", "user_name") %in% names(x))
clear_api_cache <- function(...) invisible(NULL)
updateBox <- function(...) invisible(NULL)
resource_timing <- function(...) invisible(TRUE)
fm_transport_cancel <- function(...) invisible(TRUE)
source("Modules/Login_Module.R")

login_markup <- as.character(login_UI("login"))
stopifnot(grepl('id="login-login_progress"', login_markup, fixed = TRUE),
  grepl('role="status"', login_markup, fixed = TRUE),
  grepl("event.key==='Enter'", login_markup, fixed = TRUE))

attempts <- 0L
login_outcome <- "error"
fm_async_login <- function(...) {
  attempts <<- attempts + 1L
  promises::promise(function(resolve,reject) {
  switch(login_outcome,
    error = reject(simpleError("private transport detail")),
    invalid = resolve(NULL),
    success = resolve(c(token = "offline-token", userid = "offline-user", user_name = "Example")))
  })
}
messages <- list()
cleared_passwords <- list()
mock_session <- MockShinySession$new()
mock_session$sendCustomMessage <- function(type, message) {
  messages[[length(messages) + 1L]] <<- list(type = type, payload = message)
}
mock_session$sendInputMessage <- function(inputId, message) {
  if (identical(inputId, "password") || endsWith(inputId, "-password")) cleared_passwords[[length(cleared_passwords) + 1L]] <<- message
}
settle <- function(session){for(i in seq_len(10L)){later::run_now(0);session$flushReact()}}
testServer(login_Server, args = list(id = "login"), session = mock_session, {
  invisible(output$login_feedback)

  # Missing fields never reach the API, and always release the browser button.
  session$setInputs(user_name = "   ", password = "")
  session$setInputs(login_button = 1)
  settle(session)
  stopifnot(attempts == 0L, is.null(login_token_RV()),
    grepl("Enter your username and password", output$login_feedback$html, fixed = TRUE),
    length(messages) == 1L, identical(messages[[1L]]$payload, list(busy = FALSE)))

  # A failed first login remains visible even when NULL token does not change.
  session$setInputs(user_name = "person@example.invalid", password = "wrong")
  session$setInputs(login_button = 2)
  settle(session)
  html <- paste(as.character(output$login_feedback), collapse = "\n")
  stopifnot(is.null(login_token_RV()), attempts == 1L,
    grepl("Could not authenticate with Futmondo", html, fixed = TRUE),
    !grepl("private transport detail", html, fixed = TRUE),
    grepl('role="alert"', html, fixed = TRUE),
    length(messages) == 2L, identical(messages[[2L]]$payload, list(busy = FALSE)))

  # Invalid endpoint responses also release the browser button.
  login_outcome <<- "invalid"
  session$setInputs(password = "invalid")
  session$setInputs(login_button = 3)
  settle(session)
  stopifnot(attempts == 2L, is.null(login_token_RV()),
    identical(login_feedback_RV()$kind, "error"), length(messages) == 3L)

  login_outcome <<- "success"
  session$setInputs(password = "correct")
  session$setInputs(login_button = 4)
  settle(session)
  stopifnot(attempts == 3L, valid_login(login_token_RV()),
    identical(session$userData$futmondo_user_id, "offline-user"),
    identical(login_feedback_RV()$kind, "success"), length(messages) == 4L,
    all(vapply(messages, function(x) identical(x$type, session$ns("login_state")) && identical(x$payload, list(busy = FALSE)), logical(1))),
    length(cleared_passwords) == 4L,
    all(vapply(cleared_passwords, function(x) identical(x$value, ""), logical(1))))
})
cat("LOGIN FEEDBACK: 5 passed / 0 failed\n")

if ("--browser" %in% commandArgs(trailingOnly = TRUE)) {
  markup_path <- tempfile("futmondo-login-feedback-", fileext = ".html")
  writeLines(login_markup, markup_path)
  status <- system2("node", c("test/test_login_feedback.cjs", shQuote(markup_path)))
  unlink(markup_path)
  stopifnot(status == 0L)
}
