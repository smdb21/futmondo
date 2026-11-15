# Account-local presentation only. Never change the process TZ or stored instants.
fm_timezones <- local({ zones <- sort(unique(c("UTC", OlsonNames()))); function() zones })
fm_valid_timezone <- function(x) is.character(x) && length(x)==1L && !is.na(x) && x %in% fm_timezones()
fm_display_timezone <- function(session=shiny::getDefaultReactiveDomain()) {
  source <- if(!is.null(session)) session$userData$display_timezone else NULL
  if(is.function(source)) source() else "Europe/Madrid"
}
fm_format_datetime <- function(x, timezone=fm_display_timezone(), date_only=FALSE, missing="Date unavailable") {
  if(!fm_valid_timezone(timezone)) timezone <- "Europe/Madrid"
  stamp <- fm_time(x)
  value <- format(stamp,if(date_only)"%d/%m/%Y"else"%d/%m/%Y %H:%M %Z",tz=timezone)
  value[is.na(stamp)] <- missing
  unname(value)
}
# Plotly date axes use civil strings, so a browser in another zone cannot convert
# the account's selected wall clock a second time. Hover retains the zone label.
fm_plot_datetime <- function(x, timezone=fm_display_timezone()) {
  if(!fm_valid_timezone(timezone)) timezone <- "Europe/Madrid"
  stamp <- fm_time(x); value <- format(stamp,"%Y-%m-%dT%H:%M:%S",tz=timezone)
  value[is.na(stamp)] <- NA_character_; unname(value)
}
fm_local_midnight <- function(date, timezone=fm_display_timezone()) {
  if(!fm_valid_timezone(timezone)) timezone <- "Europe/Madrid"
  fm_time(as.POSIXct(paste(as.character(date),"00:00:00"),tz=timezone))
}

# Server-only personal preferences: even a LigaRadar administrator can only
# read/write the authenticated account's timezone. Legacy rows remain stored.
fm_timezone_store <- function(user_id=NULL, timezone=NULL, mode="get", token=NULL, owner="") {
  fail <- function(message) promises::promise_reject(simpleError(message))
  if(!mode %in% c("get","set")) return(fail("Invalid timezone operation"))
  if(!valid_login(token)) return(fail("Authentication required"))
  if(!is.character(user_id)||length(user_id)!=1L||is.na(user_id)||!grepl("^[A-Za-z0-9_-]{1,256}$",user_id,perl=TRUE))
    return(fail("Invalid preference account"))
  if(!identical(user_id,as.character(token[["userid"]]))) return(fail("Unauthorized preference account"))
  if(mode=="set" && !fm_valid_timezone(timezone)) return(fail("Invalid timezone"))
  if(!nzchar(get_sb_url())||!nzchar(get_sb_key())) return(fail("Timezone storage is not configured"))
  query <- list(user_id=paste0("eq.",user_id),select="user_id,display_name,timezone,source,updated_at",limit="1")
  body <- NULL; method <- "GET"; route <- "user_timezone_preferences"
  if(mode=="set") {
    method <- "POST"; route <- "rpc/set_user_timezone"; query <- list()
    body <- list(p_user=user_id,p_timezone=timezone,p_source="user")
  }
  url <- httr::modify_url(paste0(sub("/$","",get_sb_url()),"/rest/v1/",route),query=query)
  headers <- c(apikey=get_sb_key(),Authorization=paste("Bearer",get_sb_key()),`Content-Type`="application/json")
  wire <- if(is.null(body)) NULL else jsonlite::toJSON(body,auto_unbox=TRUE)
  request <- if(mode=="get") fm_async_read_http(url,method,wire,headers,owner=owner,timeout=8,priority="interactive") else
    fm_async_http(url,method,wire,headers,owner=owner,timeout=8)
  parsed <- promises::then(request,function(response) {
    tryCatch({
      raw <- rawToChar(response$content); rows <- jsonlite::fromJSON(raw,simplifyVector=FALSE)
      if(!grepl("^[[:space:]]*\\[",raw)||!is.list(rows)||!is.null(names(rows))||length(rows)>1L)
        stop("Invalid preference response")
      for(row in rows) {
        if(!is.list(row)||!identical(row$user_id,user_id)||!fm_valid_timezone(row$timezone)||
           !is.character(row$source)||length(row$source)!=1L||!row$source%in%c("browser","user","admin")) stop("Preference mismatch")
      }
      if(!length(rows)) { if(mode!="get") stop("Preference save not acknowledged"); return(NULL) }
      if(mode=="set" && (!identical(rows[[1]]$timezone,timezone)||!identical(rows[[1]]$source,"user"))) stop("Preference save mismatch")
      rows[[1]]
    },error=function(error) stop(resource_failure(resource="user_timezone_preferences",category="parser")))
  })
  promises::then(parsed,identity,function(error) promises::promise_reject(resource_log_failure(error,"user_timezone_preferences")))
}

fm_timezone_preferences <- function(session, login, store=fm_timezone_store) {
  zone <- shiny::reactiveVal("Europe/Madrid"); issue <- shiny::reactiveVal(NULL)
  ready <- shiny::reactiveVal(FALSE); busy <- shiny::reactiveVal(FALSE)
  generation <- 0L; account <- NULL; closed <- FALSE; started <- FALSE
  session$userData$display_timezone_ready <- ready
  session$userData$display_timezone <- zone
  session$userData$set_display_timezone <- function(row) { zone(row$timezone); issue(NULL); ready(TRUE) }
  current <- function(g) !closed && identical(g,generation)
  load <- function(g) {
    if(!current(g)||started||is.null(account)) return(invisible(NULL))
    started <<- TRUE; busy(TRUE)
    auth <- shiny::isolate(login())
    promises::then(store(account,mode="get",token=auth,owner=session$token),function(row) {
      if(!current(g)) return(NULL)
      # Only an explicit personal save is a choice. Old browser defaults and
      # administrator overrides do not define this user's preference.
      selected <- if(!is.null(row)&&identical(row$source,"user"))row$timezone else "Europe/Madrid"
      zone(selected); issue(NULL); ready(TRUE); busy(FALSE); NULL
    },function(error) {
      if(!current(g)) return(NULL)
      zone("Europe/Madrid"); busy(FALSE); ready(FALSE); issue("Your saved timezone could not be loaded. Temporarily displaying Spain time (Europe/Madrid); retry to restore your saved setting."); NULL
    })
    invisible(NULL)
  }
  shiny::observeEvent(login(), {
    auth <- login(); id <- if(valid_login(auth))as.character(auth[["userid"]])else NULL
    if(identical(account,id)) return()
    generation <<- generation+1L; account <<- id; started <<- FALSE
    zone("Europe/Madrid"); issue(NULL); ready(FALSE); busy(FALSE)
    if(is.null(id)) return()
    load(generation)
  },ignoreNULL=FALSE,priority=110)
  shiny::observeEvent(session$input$retry_timezone_preference,{
    if(!busy()) { started <<- FALSE; load(generation) }
  })
  shiny::observeEvent(session$input$save_timezone,{
    if(!ready()||busy()||!valid_login(login())) return()
    selected <- session$input$display_timezone
    if(!fm_valid_timezone(selected)) { issue("Choose a valid timezone."); return() }
    g <- generation; busy(TRUE)
    promises::then(store(account,selected,mode="set",token=login(),owner=session$token),function(row) {
      if(!current(g)) return(NULL)
      session$userData$set_display_timezone(row); busy(FALSE); NULL
    },function(error) {
      if(!current(g)) return(NULL)
      busy(FALSE); ready(FALSE); issue("Your timezone change was not confirmed. Retry to check the saved setting before changing it again."); NULL
    })
    invisible(NULL)
  })
  session$output$timezone_preference_status <- shiny::renderUI({
    if(is.null(issue())) return(NULL)
    fm_info(error=TRUE,shiny::span(issue()),shiny::actionButton("retry_timezone_preference","Retry timezone"))
  })
  session$output$timezone_settings <- shiny::renderUI({
    shiny::req(valid_login(login()))
    shiny::tags$details(shiny::tags$summary(paste("Display timezone:",zone())),
      shiny::p("Choose your own display timezone. Default: Spain (Europe/Madrid). Saved choices apply only to your account across sessions. Dates use DD/MM/YYYY and a 24-hour clock."),
      shiny::selectizeInput("display_timezone","Your timezone",choices=fm_timezones(),selected=zone(),width="100%"),
      shiny::actionButton("save_timezone","Save timezone",disabled=!ready()||busy()))
  })
  session$onSessionEnded(function(){closed <<- TRUE;generation <<- generation+1L})
  invisible(zone)
}
