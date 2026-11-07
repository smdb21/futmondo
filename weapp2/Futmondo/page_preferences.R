# Account-wide navigation preferences. See docs/page_preferences.md.
fm_saved_pages <- function(is_admin = FALSE) {
  c("notifications", "today", "yourteam", "lineup", "players_in_championship", "rivals",
    "plots", "classification", "round_mvps", if (isTRUE(is_admin)) "admin")
}

fm_page_store <- function(user_id, page = NULL, visited_at = NULL, owner = "") {
  if (!is.character(user_id) || length(user_id) != 1L || is.na(user_id) ||
      !grepl("^[A-Za-z0-9_-]{1,256}$", user_id, perl = TRUE))
    return(promises::promise_reject(simpleError("Invalid preference account")))
  if (!nzchar(get_sb_url()) || !nzchar(get_sb_key()))
    return(promises::promise_reject(simpleError("Page preference storage is not configured")))
  reading <- is.null(page)
  if (!reading && !page %in% fm_saved_pages(TRUE))
    return(promises::promise_reject(simpleError("Invalid preference page")))
  query <- if (reading) list(user_id = paste0("eq.", user_id), select = "page,visited_at", limit = "1") else
    list(on_conflict = "user_id")
  url <- httr::modify_url(paste0(sub("/$", "", get_sb_url()), "/rest/v1/user_page_preferences"), query = query)
  headers <- c(apikey = get_sb_key(), Authorization = paste("Bearer", get_sb_key()),
    `Content-Type` = "application/json", Prefer = "resolution=merge-duplicates,return=minimal")
  body <- if (reading) NULL else jsonlite::toJSON(list(user_id = user_id, page = page,
    visited_at = visited_at), auto_unbox = TRUE)
  promises::then(fm_async_http(url, if (reading) "GET" else "POST", body, headers,
    owner = owner, timeout = 8), function(response) {
      if (!reading) return(TRUE)
      rows <- jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE)
      if (!is.list(rows) || length(rows) > 1L) stop("Invalid page preference response")
      if (length(rows)) rows[[1L]] else NULL
    })
}

fm_page_preferences <- function(session, login_token, page, is_admin,
                                store = fm_page_store) {
  initial <- shiny::reactiveVal("login")
  issue <- shiny::reactiveVal(NULL)
  generation <- 0L
  closed <- FALSE
  state <- NULL
  current <- function(s) !closed && identical(s$generation, generation)
  report <- function(s, value) if (current(s)) issue(value)
  select_page <- function(s, value) {
    s$awaiting <- value
    initial(value)
    shinydashboard::updateTabItems(session, "tabs", value)
  }
  save <- function(s) {
    if (s$inflight || is.null(s$pending)) return(invisible(NULL))
    value <- s$pending
    s$pending <- NULL
    s$inflight <- TRUE
    failed <- function(error) {
      s$inflight <- FALSE
      if (is.null(s$pending)) s$pending <- value
      report(s, "save")
      NULL
    }
    promises::then(store(s$user_id, value$page, value$visited_at,
      owner = paste0(session$token, ":page-save")), function(ok) {
        s$inflight <- FALSE
        if (!isTRUE(ok)) return(failed(NULL))
        s$saved <- value$page
        report(s, NULL)
        save(s)
      }, failed)
    invisible(NULL)
  }
  restore <- function(s) {
    s$loading <- TRUE
    promises::then(store(s$user_id, owner = session$token), function(row) {
      if (!current(s)) return(NULL)
      s$loading <- FALSE
      report(s, NULL)
      if (s$navigated) return(NULL) # A late read must never move the user back.
      saved <- if (is.list(row)) row$page else NULL
      s$saved <- saved
      target <- if (is.character(saved) && length(saved) == 1L &&
        !is.na(saved) && saved %in% fm_saved_pages(shiny::isolate(is_admin()))) saved else "today"
      select_page(s, target)
      NULL
    }, function(error) {
      if (!current(s)) return(NULL)
      s$loading <- FALSE
      report(s, "load")
      if (!s$navigated) select_page(s, "today")
      NULL
    })
    invisible(NULL)
  }
  shiny::observeEvent(login_token(), {
    token <- login_token()
    user_id <- if (valid_login(token)) as.character(token[["userid"]]) else NULL
    if (!is.null(state) && identical(state$user_id, user_id)) return()
    generation <<- generation + 1L
    state <<- NULL
    initial("login")
    issue(NULL)
    if (is.null(user_id)) return()
    s <- new.env(parent = emptyenv())
    s$user_id <- user_id; s$generation <- generation
    s$initial_input <- shiny::isolate(page())
    s$saved <- NULL; s$pending <- NULL; s$inflight <- FALSE
    s$awaiting <- NULL; s$navigated <- FALSE; s$loading <- FALSE
    state <<- s
    restore(s)
  }, ignoreNULL = FALSE, priority = 100)
  shiny::observeEvent(page(), {
    s <- state
    if (is.null(s) || !valid_login(shiny::isolate(login_token()))) return()
    value <- page()
    if (!value %in% fm_saved_pages(shiny::isolate(is_admin()))) return()
    if (!is.null(s$awaiting)) {
      if (!identical(value, s$awaiting)) return()
      s$awaiting <- NULL
      return() # Restoring a page adds no database write.
    }
    if (s$loading && identical(value, s$initial_input)) return()
    s$navigated <- TRUE
    if (identical(value, s$saved) && is.null(s$pending) && !s$inflight) return()
    s$pending <- list(page = value,
      visited_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"))
    save(s)
  }, ignoreInit = TRUE)
  shiny::observeEvent(session$input$retry_page_preference, {
    s <- state
    if (is.null(s)) return()
    if (identical(issue(), "load")) restore(s) else save(s)
  })
  session$onSessionEnded(function() { closed <<- TRUE; generation <<- generation + 1L })
  session$output$page_preference_status <- shiny::renderUI({
    problem <- issue()
    if (is.null(problem)) return(NULL)
    shiny::div(class = "startup-feedback", role = "status",
      shiny::span(if (identical(problem, "load")) "Your saved page could not be loaded." else
        "Your last page has not been saved yet."),
      shiny::actionButton("retry_page_preference", "Retry"))
  })
  list(initial = initial)
}
