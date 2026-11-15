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

# Server-only store: all identifiers come from the authenticated token or an
# authorized Admin selector. Initial registration atomically preserves any
# existing preference, including a concurrent administrator's override.
fm_timezone_store <- function(user_id=NULL, timezone=NULL, label="", mode="get", token=NULL,
                              owner="", after="", search="") {
  fail <- function(message) promises::promise_reject(simpleError(message))
  if(!mode %in% c("get","initialize","set","list")) return(fail("Invalid timezone operation"))
  if(!valid_login(token)) return(fail("Authentication required"))
  admin <- is_authorized_admin(token)
  if(mode=="list") { if(!admin) return(fail("Administrator required")) } else {
    if(!is.character(user_id)||length(user_id)!=1L||is.na(user_id)||!grepl("^[A-Za-z0-9_-]{1,256}$",user_id,perl=TRUE))
      return(fail("Invalid preference account"))
    if(!identical(user_id,as.character(token[["userid"]])) && !admin) return(fail("Unauthorized preference account"))
  }
  if(mode %in% c("initialize","set") && !fm_valid_timezone(timezone)) return(fail("Invalid timezone"))
  if(!nzchar(get_sb_url())||!nzchar(get_sb_key())) return(fail("Timezone storage is not configured"))
  query <- list(); body <- NULL; method <- "GET"; route <- "user_timezone_preferences"
  if(mode=="list") {
    method <- "POST"; route <- "rpc/list_user_timezones"
    body <- list(p_after=after,p_search=search)
  } else if(mode=="initialize") {
    method <- "POST"; route <- "rpc/initialize_user_timezone"
    body <- list(p_user=user_id,p_timezone=timezone,p_label=substr(fm_scalar(label,""),1,256))
  } else {
    query <- list(user_id=paste0("eq.",user_id),select="user_id,display_name,timezone,source,updated_at",limit="1")
    if(mode=="set") {
      method <- "POST"; route <- "rpc/set_user_timezone"; query <- list()
      body <- list(p_user=user_id,p_timezone=timezone,p_source=if(admin)"admin"else"user")
    }
  }
  url <- httr::modify_url(paste0(sub("/$","",get_sb_url()),"/rest/v1/",route),query=query)
  headers <- c(apikey=get_sb_key(),Authorization=paste("Bearer",get_sb_key()),`Content-Type`="application/json")
  wire <- if(is.null(body)) NULL else jsonlite::toJSON(body,auto_unbox=TRUE)
  request <- if(mode %in% c("get","list")) fm_async_read_http(url,method,wire,headers,owner=owner,timeout=8,priority="interactive") else
    fm_async_http(url,method,wire,headers,owner=owner,timeout=8)
  parsed <- promises::then(request,function(response) {
    tryCatch({
      raw <- rawToChar(response$content); rows <- jsonlite::fromJSON(raw,simplifyVector=FALSE)
      if(!grepl("^[[:space:]]*\\[",raw)||!is.list(rows)||!is.null(names(rows))||length(rows)>if(mode=="list")201L else 1L)
        stop("Invalid preference response")
      for(row in rows) {
        if(!is.list(row)||!is.character(row$user_id)||length(row$user_id)!=1L||is.na(row$user_id)||
           !grepl("^[A-Za-z0-9_-]{1,256}$",row$user_id,perl=TRUE)||
           (!is.null(row$timezone)&&!fm_valid_timezone(row$timezone))) stop("Invalid preference row")
        if(mode!="list" && (!identical(row$user_id,user_id)||!fm_valid_timezone(row$timezone))) stop("Preference mismatch")
      }
      if(mode=="list") return(rows)
      if(!length(rows)) { if(mode!="get") stop("Preference save not acknowledged"); return(NULL) }
      if(mode=="set" && !identical(rows[[1]]$timezone,timezone)) stop("Preference save mismatch")
      rows[[1]]
    },error=function(error) stop(resource_failure(resource="user_timezone_preferences",category="parser")))
  })
  promises::then(parsed,identity,function(error) promises::promise_reject(resource_log_failure(error,"user_timezone_preferences")))
}

fm_timezone_preferences <- function(session, login, store=fm_timezone_store) {
  zone <- shiny::reactiveVal("Europe/Madrid"); issue <- shiny::reactiveVal(NULL)
  ready <- shiny::reactiveVal(FALSE); busy <- shiny::reactiveVal(FALSE)
  generation <- 0L; account <- NULL; closed <- FALSE; started <- FALSE; fallback <- NULL
  session$userData$display_timezone_ready <- ready
  session$userData$display_timezone <- zone
  session$userData$set_display_timezone <- function(row) { zone(row$timezone); issue(NULL); ready(TRUE) }
  current <- function(g) !closed && identical(g,generation)
  load <- function(g) {
    if(!current(g)||started||is.null(account)) return(invisible(NULL))
    started <<- TRUE; busy(TRUE)
    detected <- shiny::isolate(session$input$worker_timezone_detected)
    candidate <- if(fm_valid_timezone(detected)) detected else "Europe/Madrid"
    auth <- shiny::isolate(login())
    promises::then(store(account,candidate,label=auth[["user_name"]],mode="initialize",token=auth,owner=session$token),function(row) {
      if(!current(g)) return(NULL)
      session$userData$set_display_timezone(row); busy(FALSE); NULL
    },function(error) {
      if(!current(g)) return(NULL)
      zone(candidate); busy(FALSE); ready(FALSE); issue("Your timezone could not be loaded or saved. Displaying a temporary timezone; retry to restore your saved setting."); NULL
    })
    invisible(NULL)
  }
  shiny::observeEvent(login(), {
    auth <- login(); id <- if(valid_login(auth))as.character(auth[["userid"]])else NULL
    if(identical(account,id)) return()
    generation <<- generation+1L; account <<- id; started <<- FALSE
    if(is.function(fallback)) fallback()
    zone("Europe/Madrid"); issue(NULL); ready(FALSE); busy(FALSE)
    if(is.null(id)) return()
    g <- generation
    if(!is.null(shiny::isolate(session$input$worker_timezone_detected))) load(g) else
      fallback <<- later::later(function() { if(current(g)) load(g) },1)
  },ignoreNULL=FALSE,priority=110)
  shiny::observeEvent(session$input$worker_timezone_detected,{ load(generation) })
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
  })
  session$output$timezone_preference_status <- shiny::renderUI({
    if(is.null(issue())) return(NULL)
    fm_info(error=TRUE,shiny::span(issue()),shiny::actionButton("retry_timezone_preference","Retry timezone"))
  })
  session$output$timezone_settings <- shiny::renderUI({
    shiny::req(valid_login(login()))
    shiny::tags$details(shiny::tags$summary(paste("Display timezone:",zone())),
      shiny::p("Dates use DD/MM/YYYY and a 24-hour clock. Saved for your account across sessions."),
      shiny::selectizeInput("display_timezone","Your timezone",choices=fm_timezones(),selected=zone(),width="100%"),
      shiny::actionButton("save_timezone","Save timezone",disabled=!ready()||busy()))
  })
  session$onSessionEnded(function(){closed <<- TRUE;generation <<- generation+1L;if(is.function(fallback))fallback()})
  invisible(zone)
}

fm_admin_timezones <- function(session, login, active, store=fm_timezone_store) {
  rows <- shiny::reactiveVal(list()); message <- shiny::reactiveVal(NULL); busy <- shiny::reactiveVal(FALSE)
  loaded <- shiny::reactiveVal(FALSE); cursor <- ""; generation <- 0L; closed <- FALSE
  authorized <- function() isTRUE(active()) && is_authorized_admin(login())
  load <- function(after="") {
    if(!shiny::isolate(authorized())||shiny::isolate(busy())) return()
    generation <<- generation+1L; g <- generation; busy(TRUE); loaded(FALSE)
    search <- shiny::isolate(session$input$timezone_search) %||% ""
    promises::then(store(mode="list",token=shiny::isolate(login()),owner=session$token,after=after,search=search),function(value) {
      if(closed||g!=generation||!shiny::isolate(authorized())) return(NULL)
      cursor <<- if(length(value)>200L)value[[200L]]$user_id else ""
      rows(head(value,200L)); loaded(TRUE);busy(FALSE);message(NULL);NULL
    },function(error){if(!closed&&g==generation){busy(FALSE);message("User timezones could not be loaded. Reload to retry.")};NULL})
    # Do not return this promise to an observer: Shiny would hold the entire
    # session flush until the user-directory request completes.
    invisible(NULL)
  }
  preference_ready <- session$rootScope()$userData$display_timezone_ready
  shiny::observeEvent(list(active(),login(),if(is.function(preference_ready))preference_ready()else NULL),{
    generation <<- generation+1L; busy(FALSE);loaded(FALSE);rows(list());message(NULL)
    if(authorized()) load()
  },ignoreNULL=FALSE)
  shiny::observeEvent(session$input$timezone_reload,load())
  shiny::observeEvent(session$input$timezone_next,if(nzchar(cursor))load(cursor))
  # Keep controls mounted while requests settle; status changes must never
  # reset a selection or overwrite an unsaved timezone choice.
  session$output$timezone_admin <- shiny::renderUI({
    shiny::req(authorized())
    shiny::tagList(shiny::p("Choose a user and save their timezone. Saved choices take precedence over browser detection on future logins."),
      shiny::fluidRow(shiny::column(6,shiny::textInput(session$ns("timezone_search"),"Find user",width="100%")),
        shiny::column(6,shiny::actionButton(session$ns("timezone_reload"),"Find / reload users"))),
      shiny::selectInput(session$ns("timezone_user"),"User",choices=character(),width="100%"),
      shiny::uiOutput(session$ns("timezone_editor")),shiny::uiOutput(session$ns("timezone_actions")),
      shiny::uiOutput(session$ns("timezone_status")))
  })
  shiny::observe({
    shiny::req(authorized()); users <- rows()
    choices <- setNames(vapply(users,function(x)x$user_id,character(1)),vapply(users,function(x){
      label <- fm_scalar(x$display_name,""); paste0(if(nzchar(label))paste0(label," · ")else"",x$user_id)
    },character(1)))
    selected <- shiny::isolate(session$input$timezone_user)
    if(length(selected)!=1L||!selected%in%choices)selected <- head(unname(choices),1L)
    shiny::updateSelectInput(session,"timezone_user",choices=choices,selected=selected)
  })
  session$output$timezone_status <- shiny::renderUI({
    shiny::req(authorized()); loading <- busy(); text <- message(); loaded()
    shiny::tagList(if(!is.null(text))fm_info(text,error=!startsWith(text,"Timezone saved:")),
      if(loading)shiny::p("Loading or saving timezone…"))
  })
  session$output$timezone_actions <- shiny::renderUI({
    shiny::req(authorized()); users <- rows(); selected <- session$input$timezone_user
    available <- isTRUE(loaded()) && length(selected)==1L && identical(selected,session$input$timezone_editing) && selected%in%vapply(users,function(x)x$user_id,character(1))
    shiny::tagList(if(available)shiny::actionButton(session$ns("timezone_save"),"Save user timezone",disabled=busy()),
      if(nzchar(cursor))shiny::actionButton(session$ns("timezone_next"),"Next 200 users",disabled=busy()))
  })
  session$output$timezone_editor <- shiny::renderUI({
    shiny::req(authorized(),loaded()); selected <- session$input$timezone_user
    row <- Filter(function(x)identical(x$user_id,selected),rows())
    if(length(row)!=1L) return(NULL)
    shiny::div(`data-user-id`=selected,
      shiny::tags$div(style="display:none",shiny::textInput(session$ns("timezone_editing"),NULL,value=selected)),
      shiny::p(if(fm_valid_timezone(row[[1]]$timezone))paste("Saved:",row[[1]]$timezone)else"No saved timezone yet; browser detection applies on first login."),
      shiny::selectizeInput(session$ns("timezone_choice"),"Timezone",choices=fm_timezones(),selected=row[[1]]$timezone %||% "Europe/Madrid",width="100%"))
  })
  shiny::observeEvent(session$input$timezone_save,{
    if(!authorized()||!loaded()||busy()) return()
    id <- session$input$timezone_user; selected <- session$input$timezone_choice
    if(!identical(id,session$input$timezone_editing)||!is.character(id)||length(id)!=1L||!id%in%vapply(rows(),function(x)x$user_id,character(1))||!fm_valid_timezone(selected)) return()
    generation <<- generation+1L; g <- generation; busy(TRUE)
    promises::then(store(id,selected,mode="set",token=login(),owner=session$token),function(row){
      if(closed||g!=generation||!shiny::isolate(authorized())) return(NULL)
      users <- shiny::isolate(rows()); index <- which(vapply(users,function(x)identical(x$user_id,id),logical(1)));users[[index]] <- row
      rows(users);busy(FALSE);message(paste("Timezone saved:",selected))
      root <- session$rootScope()
      if(identical(id,as.character(shiny::isolate(login())[["userid"]])) && is.function(root$userData$set_display_timezone))root$userData$set_display_timezone(row)
      NULL
    },function(error){if(!closed&&g==generation){busy(FALSE);loaded(FALSE);message("Timezone save was not confirmed. Reload users to check the stored setting before trying again.")};NULL})
  })
  session$onSessionEnded(function(){closed <<- TRUE;generation <<- generation+1L})
  invisible(rows)
}
