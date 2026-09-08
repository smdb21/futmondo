#!/usr/bin/env Rscript
# Login feedback must render even when a failed attempt keeps the token NULL.
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(x, y) if (is.null(x)) y else x
valid_login <- function(x) is.character(x) && length(x) == 3L && all(c("token", "userid", "user_name") %in% names(x))
clear_api_cache <- function(...) invisible(NULL)
source("Modules/Login_Module.R")

login_markup <- as.character(login_UI("login"))
stopifnot(grepl("keydown.futmondoLogin", login_markup, fixed = TRUE),
  grepl("event.key==='Enter'", login_markup, fixed = TRUE),
  grepl("login-login_button", login_markup, fixed = TRUE))

attempts <- 0L
login <- function(...) { attempts <<- attempts + 1L; stop("private transport detail") }
testServer(login_Server, args = list(id = "login"), {
  output$login_feedback
  session$setInputs(user_name = "person@example.invalid", password = "wrong")
  session$setInputs(login_button = 1)
  session$flushReact()
  html <- paste(as.character(output$login_feedback), collapse = "\n")
  stopifnot(is.null(login_token_RV()), attempts == 1L,
    grepl("Could not authenticate with Futmondo", html, fixed = TRUE),
    !grepl("private transport detail", html, fixed = TRUE),
    grepl('role="alert"', html, fixed = TRUE))
})
cat("LOGIN FEEDBACK: 2 passed / 0 failed\n")
