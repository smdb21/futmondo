#!/usr/bin/env Rscript
# Deterministic startup admission tests: no database, account or HTTP requests.
suppressPackageStartupMessages(library(shiny))
source("data_contracts.R")
`%||%` <- function(x, y) if (is.null(x)) y else x
valid_login <- function(x) is.character(x) && length(x) == 3L &&
  all(c("token", "userid", "user_name") %in% names(x))
clear_api_cache <- function(...) invisible(NULL)
updateBox <- function(...) invisible(NULL)
resource_timing <- function(...) invisible(TRUE)
cancelled <- character()
fm_transport_cancel <- function(owner) { cancelled <<- c(cancelled, owner); invisible(TRUE) }
admin_fixture <- TRUE
is_authorized_admin <- function(...) admin_fixture
auth <- c(token="synthetic-token", userid="synthetic-user", user_name="Fixture")
login_calls <- 0L
fm_async_login <- function(...) { login_calls <<- login_calls + 1L; promises::promise_resolve(auth) }
fm_app_access <- function(...) promises::promise_resolve(TRUE)
remember_enabled <- TRUE
remember_login_enabled <- function() remember_enabled
remember_starts <- 0L
remember_retries <- 0L
remember_login_session <- function(session, on_restore, on_status, on_begin, on_end, on_expire) {
  remember_starts <<- remember_starts + 1L
  session$userData$restore_callbacks <- list(begin=on_begin, end=on_end, restore=on_restore,
    status=on_status, expire=on_expire)
  list(login=function(...)NULL, cancel=function(...)NULL, logout=function(...)NULL,
    result=function(...)NULL, retry=function(...) { remember_retries <<- remember_retries + 1L })
}
source("Modules/Login_Module.R")
settle <- function(session) for (i in seq_len(12L)) { later::run_now(0); session$flushReact() }
checks <- 0L
check <- function(name, code) { force(code); checks <<- checks + 1L; cat("PASS", name, "\n") }
probe_calls <- 0L
pending <- list()
probe <- function(owner) {
  probe_calls <<- probe_calls + 1L
  promises::promise(function(resolve, reject) {
    pending[[length(pending) + 1L]] <<- list(resolve=resolve, reject=reject, owner=owner)
  })
}

check("pending and failed startup block all login paths, including administrators", {
  testServer(login_Server, args=list(id="login", database_probe=probe), {
    settle(session)
    stopifnot(database_state() == "checking", probe_calls == 1L, remember_starts == 0L,
      output$login_form_visible == "FALSE", grepl("Checking database", output$database_status$html))
    heartbeat_seen <- 0L
    observeEvent(input$fixture_heartbeat, { heartbeat_seen <<- input$fixture_heartbeat })
    session$setInputs(user_name="fixture", password="synthetic", login_button=1,
      remember_retry=1, database_retry=1, fixture_heartbeat=1)
    settle(session)
    stopifnot(heartbeat_seen == 1L, login_calls == 0L, remember_retries == 0L,
      probe_calls == 1L, is.null(login_token_RV()))
    pending[[1L]]$reject(simpleError("PRIVATE database URL / server key / response body"))
    settle(session)
    html <- output$database_status$html
    stopifnot(database_state() == "error", output$login_form_visible == "TRUE",
      grepl('role="alert"', html, fixed=TRUE), grepl("Database unavailable", html, fixed=TRUE),
      grepl("Retry connection", html, fixed=TRUE), !grepl("PRIVATE|<details|fm-info", html),
      !grepl("PRIVATE", paste(as.character(output$login_feedback), collapse="")))
    session$setInputs(login_button=2, remember_retry=2)
    settle(session)
    stopifnot(login_calls == 0L, remember_starts == 0L, is.null(login_token_RV()))

    session$setInputs(database_retry=2)
    settle(session)
    session$setInputs(database_retry=3, login_button=3)
    settle(session)
    stopifnot(probe_calls == 2L, database_state() == "checking", login_calls == 0L)
    pending[[2L]]$resolve(TRUE)
    settle(session)
    stopifnot(database_state() == "ready", remember_starts == 1L,
      login_calls == 0L, is.null(login_token_RV()))
    session$userData$restore_callbacks$end()
    settle(session)
    stopifnot(output$login_form_visible == "TRUE")
    session$setInputs(database_retry=4)
    settle(session)
    stopifnot(probe_calls == 2L, remember_starts == 1L)
    session$setInputs(password="synthetic", login_button=4)
    settle(session)
    stopifnot(login_calls == 1L, identical(login_token_RV(), auth))
  })
})

check("saved sessions restore only after a fresh successful startup probe", {
  before <- remember_starts
  before_login <- login_calls
  testServer(login_Server, args=list(id="login", database_probe=probe), {
    settle(session)
    stopifnot(database_state() == "checking", remember_starts == before,
      is.null(session$userData$restore_callbacks))
    pending[[length(pending)]]$resolve(TRUE)
    settle(session)
    stopifnot(remember_starts == before + 1L, probe_calls == 3L)
    callbacks <- session$userData$restore_callbacks
    callbacks$begin()
    callbacks$restore(auth)
    settle(session)
    callbacks$end()
    settle(session)
    stopifnot(identical(login_token_RV(), auth), login_calls == before_login,
      output$login_form_visible == "FALSE")
  })
})

check("only an explicit healthy probe result enables admission", {
  for (answer in list(NULL, FALSE, NA, "TRUE", 1, list(TRUE))) {
    invalid_probe <- local({ value <- answer; function(...) promises::promise_resolve(value) })
    testServer(login_Server, args=list(id="login", database_probe=invalid_probe), {
      settle(session)
      stopifnot(database_state() == "error", is.null(login_token_RV()))
    })
  }
  testServer(login_Server, args=list(id="login", database_probe=function(...)stop("PRIVATE")), {
    settle(session)
    stopifnot(database_state() == "error", !grepl("PRIVATE", output$database_status$html))
  })
})

check("closed sessions cancel the probe and ignore its late completion", {
  before <- remember_starts
  messages <- list()
  mock <- MockShinySession$new()
  mock$sendCustomMessage <- function(type, message) {
    messages[[length(messages) + 1L]] <<- list(type=type, message=message)
  }
  testServer(login_Server, args=list(id="login", database_probe=probe), session=mock, {
    settle(session)
    last <- pending[[length(pending)]]
    before_messages <- length(messages)
    session$close()
    last$resolve(TRUE)
    for (i in seq_len(12L)) later::run_now(0)
    stopifnot(remember_starts == before, length(messages) == before_messages,
      last$owner %in% cancelled)
  })
})

check("an unavailable database remains visible when remembered login is disabled", {
  remember_enabled <- FALSE
  testServer(login_Server, args=list(id="login", database_probe=probe), {
    settle(session)
    stopifnot(output$login_form_visible == "TRUE", database_state() == "checking")
    pending[[length(pending)]]$reject(simpleError("offline"))
    settle(session)
    stopifnot(output$login_form_visible == "TRUE", database_state() == "error",
      grepl("Login is disabled", output$database_status$html, fixed=TRUE))
  })
})
cat("DATABASE LOGIN GATE:", checks, "passed / 0 failed\n")
