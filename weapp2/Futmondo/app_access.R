# Server-only application admission policy. See docs/app_access.md.
fm_access_store <- function(blocked = NULL, token = NULL, owner = "") {
  reading <- is.null(blocked)
  if (!reading && (!is_authorized_admin(token) || !is.logical(blocked) ||
      length(blocked) != 1L || is.na(blocked)))
    return(promises::promise_reject(simpleError("Unauthorized access-policy change")))
  if (!nzchar(get_sb_url()) || !nzchar(get_sb_key()))
    return(promises::promise_reject(simpleError("Access-policy storage is not configured")))
  query <- if (reading) list(id = "eq.main", select = "admin_only", limit = "1") else
    list(on_conflict = "id", select = "admin_only")
  url <- httr::modify_url(paste0(sub("/$", "", get_sb_url()), "/rest/v1/application_access"), query = query)
  headers <- c(apikey = get_sb_key(), Authorization = paste("Bearer", get_sb_key()),
    `Content-Type` = "application/json", Prefer = "resolution=merge-duplicates,return=representation")
  body <- if (reading) NULL else jsonlite::toJSON(list(id = "main", admin_only = blocked,
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")), auto_unbox = TRUE)
  promises::then(fm_async_http(url, if (reading) "GET" else "POST", body, headers,
    owner = owner, timeout = 8), function(response) {
      rows <- jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE)
      if (!is.list(rows) || length(rows) != 1L || !is.list(rows[[1L]]) ||
          !is.logical(rows[[1L]]$admin_only) || length(rows[[1L]]$admin_only) != 1L ||
          is.na(rows[[1L]]$admin_only)) stop("Invalid or missing access policy")
      value <- rows[[1L]]$admin_only
      if (!reading && !identical(value, blocked)) stop("Access-policy save was not confirmed")
      value
    })
}

fm_app_access <- function(token, owner = "", store = fm_access_store) {
  if (!valid_login(token)) return(promises::promise_resolve(FALSE))
  if (is_authorized_admin(token)) return(promises::promise_resolve(TRUE))
  promises::then(store(owner = owner), function(blocked) {
    # Only an explicit, persisted false opens access. Unknown values fail closed.
    identical(blocked, FALSE)
  })
}

fm_admin_access <- function(session, login_token, active, store = fm_access_store) {
  state <- shiny::reactiveVal(list(blocked = TRUE, loaded = FALSE, busy = FALSE,
    message = "Loading access setting…"))
  generation <- 0L
  closed <- FALSE
  authorized <- function() isTRUE(active()) && is_authorized_admin(login_token())
  load <- function() {
    if (!authorized()) return(invisible(NULL))
    generation <<- generation + 1L
    attempt <- generation
    s <- shiny::isolate(state()); s$busy <- TRUE; s$message <- "Loading access setting…"; state(s)
    promises::then(store(owner = session$token), function(blocked) {
      if (closed || attempt != generation || !shiny::isolate(authorized())) return(NULL)
      state(list(blocked = blocked, loaded = TRUE, busy = FALSE, message = NULL))
      NULL
    }, function(error) {
      if (closed || attempt != generation) return(NULL)
      s <- shiny::isolate(state()); s$busy <- FALSE; s$loaded <- FALSE
      s$message <- "Access setting could not be loaded. Non-admin access remains blocked until it can be verified. Retry loading."
      state(s); NULL
    })
    invisible(NULL)
  }
  shiny::observeEvent(list(active(), login_token()), {
    generation <<- generation + 1L
    state(list(blocked = TRUE, loaded = FALSE, busy = FALSE, message = NULL))
    if (authorized()) load()
  }, ignoreNULL = FALSE)
  shiny::observeEvent(session$input$retry_access, { if (!state()$busy) load() })
  shiny::observeEvent(session$input$admin_only, {
    if (!authorized()) return()
    s <- state(); value <- session$input$admin_only
    if (!s$loaded || s$busy || !is.logical(value) || length(value) != 1L ||
        is.na(value) || identical(value, s$blocked)) return()
    attempt <- generation
    s$busy <- TRUE; s$message <- "Saving access setting…"; state(s)
    promises::then(store(value, login_token(), owner = session$token), function(saved) {
      if (closed || attempt != generation || !shiny::isolate(authorized())) return(NULL)
      state(list(blocked = saved, loaded = TRUE, busy = FALSE, message = "Access setting saved."))
      NULL
    }, function(error) {
      if (closed || attempt != generation) return(NULL)
      # A timeout may have committed. Require a fresh read before another change.
      s <- shiny::isolate(state()); s$busy <- FALSE; s$loaded <- FALSE
      s$message <- "Access change could not be confirmed. Retry loading to verify the stored setting."
      state(s); NULL
    })
  })
  session$output$access_control <- shiny::renderUI({
    shiny::req(authorized())
    s <- state()
    control <- shiny::checkboxInput(session$ns("admin_only"), "Block non-admin access", value = s$blocked)
    query <- htmltools::tagQuery(control)$find("input")$addAttrs(role = "switch")
    if (!s$loaded || s$busy) query <- query$addAttrs(disabled = "disabled")
    shiny::tagList(shiny::div(class = "fm-access-switch", query$allTags()),
      shiny::p("Enabled: only administrators can use the app. Disabled: all users can log in. Existing non-admin sessions are checked every 15 seconds."),
      if (!is.null(s$message)) shiny::p(role = "status", s$message),
      shiny::actionButton(session$ns("retry_access"), "Reload access setting", disabled = s$busy),
      shiny::tags$style(".fm-access-switch input[type=checkbox]{appearance:none;-webkit-appearance:none;width:2.6em;height:1.5em;border-radius:1em;background:#777;position:relative;vertical-align:middle;margin-top:0;cursor:pointer}.fm-access-switch input:checked{background:#167a5c}.fm-access-switch input:before{content:'';position:absolute;width:1.1em;height:1.1em;left:.2em;top:.2em;background:white;border-radius:50%}.fm-access-switch input:checked:before{left:1.3em}.fm-access-switch input:focus-visible{outline:2px solid #167a5c;outline-offset:2px}.fm-access-switch input:disabled{opacity:.5;cursor:wait}.fm-access-switch label span{padding-left:1.1em}"))
  })
  session$onSessionEnded(function() { closed <<- TRUE; generation <<- generation + 1L })
  invisible(state)
}
