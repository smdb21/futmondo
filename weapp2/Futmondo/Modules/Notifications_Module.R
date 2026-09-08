fetch_account_insights_alerts <- function(login) {
  if(!valid_login(login)) return(NULL)
  active <- tryCatch(get_active_championships(login)$championships,error=function(e)NULL)
  if(is.null(active)) return(NULL)
  rows <- list()
  for(league in active) {
    id <- fm_scalar(league$id %||% league$`_id`,'')
    team <- fm_scalar(league$userteam$id %||% league$userteam$`_id`,'')
    if(!nzchar(id)||!nzchar(team)) next
    d <- fetch_user_smart_alerts(team,id,user_id=login[['userid']])
    if(is.null(d)) return(NULL)
    if(nrow(d)) {
      d <- d[!is.na(d$championship_id)&d$championship_id==id,,drop=FALSE]
      d$league_name <- rep(fm_scalar(league$name,id),nrow(d))
      rows[[length(rows)+1L]] <- d
    }
  }
  if(!length(rows)) return(data.frame(is_read=logical()))
  dplyr::bind_rows(rows)
}

notifications_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(shiny::column(12, shiny::h2("Notifications"),
      shiny::p("Futmondo events and Insights alerts, with their source and last refresh."))),
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput(ns("read_filter"), "Read status", c("Unread"="unread","All"="all"))),
      shiny::column(3, shiny::selectInput(ns("source_filter"), "Source", c("All","Futmondo","Insights"))),
      shiny::column(3, shiny::selectInput(ns("event_filter"), "Event", c("All"))),
      shiny::column(3, shiny::checkboxInput(ns("current_league"), "Current league only", TRUE),
        shiny::actionButton(ns("refresh"), "Refresh"))),
    shiny::uiOutput(ns("status")), shiny::uiOutput(ns("items")),
    shiny::uiOutput(ns("player_details")))
}

build_insight_alerts <- function(user_id, championship_id, user_team_id,
                                financial, squad = NULL, offers = NULL, now = Sys.time(), market = NULL) {
  alerts <- list()
  day <- format(now, "%Y-%m-%d", tz = "UTC")
  add <- function(type, title, message, severity = "info", player_id = NULL) {
    alerts[[length(alerts)+1L]] <<- data.frame(user_id = user_id, championship_id = championship_id,
      user_team_id = user_team_id, event_key = paste(type, player_id %||% "team", day, sep=":"),
      alert_type = type, title = title, message = message, severity = severity,
      player_id = player_id %||% NA_character_, stringsAsFactors = FALSE)
  }
  if (identical(financial$status, "ok") && is.finite(financial$cash) && financial$cash <= 0)
    add("solvency", "Restore solvency", "Your verified cash balance is zero or negative. Resolve this before the round starts.", "danger")
  if (!is.null(squad) && nrow(squad)) {
    rules <- financial$lineup_rules %||% list(); rules$exclude_unavailable <- FALSE
    xi <- tryCatch(optimize_starting_xi(squad, formation = "auto",rules=rules), error = function(e) NULL)
    if (!is.null(xi) && !isTRUE(xi$feasible)) add("lineup", "No legal XI available", "Your roster cannot fill a valid starting eleven.", "warning")
    status <- if ("status" %in% names(squad)) as.character(squad$status) else rep("",nrow(squad))
    for (i in which(status %in% c("injured","injured2","redcard","suspended")))
      add("availability", paste(squad$name[i], "availability warning"),
        paste("Reported status:", status[i]), "warning", as.character(squad$id[i]))
    if ("clause_date" %in% names(squad)) {
      dt <- suppressWarnings(fm_time(squad$clause_date))
      for (i in which(!is.na(dt) & dt > now & dt <= now + 86400))
        add("clause", paste(squad$name[i], "clause timing"), "The reported clause date is within 24 hours. Check protection before acting.", "info", as.character(squad$id[i]))
    }
  } else if (is.data.frame(squad) && !nrow(squad)) add("lineup", "Roster is empty", "No starting XI can be selected.", "warning")
  if (!is.null(offers) && nrow(offers)) add("offers", "Offers to review", paste(nrow(offers), "offer records are available in your roster."))
  if (is.data.frame(market) && nrow(market) && "expirationDate" %in% names(market)) {
    expiry <- fm_time(market$expirationDate)
    own <- if ("bid_id" %in% names(market)) !is.na(market$bid_id) & nzchar(as.character(market$bid_id)) else rep(FALSE,nrow(market))
    for (i in which(own & !is.na(expiry) & expiry>now & expiry<=now+3600))
      add("auction_deadline",paste(market$name[i],"auction closes soon"),"Your bid has a reported deadline within one hour.","warning",as.character(market$id[i]))
  }
  deadline <- fm_time(financial$rules$deadline)
  if (length(deadline)==1L && !is.na(deadline) && deadline>now && deadline<=now+86400)
    add("round_deadline","Review your team before the deadline","The verified round deadline is within 24 hours.","warning")
  dplyr::bind_rows(alerts)
}

notifications_Server <- function(id, is_module_active, login_token, championship_id,
                                  user_team_id, user_teams_RV, refresh_trigger = NULL, on_market_event=NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    tick <- shiny::reactiveVal(0L)
    count <- shiny::reactiveVal(NA_integer_)
    official <- shiny::reactiveVal(NULL)
    local <- shiny::reactiveVal(NULL)
    last_success <- shiny::reactiveVal(NULL)
    stale <- shiny::reactiveVal(FALSE)
    selected <- shiny::reactiveVal(NULL)
    detail_context <- shiny::reactiveVal(NULL)
    if (exists("selected_player_Server", mode="function")) {
      # Player links route through a dedicated module, preserving existing behavior.
      selected_player_Server("player", selected_player = selected, login_token = login_token,
        championship_id = shiny::reactive(detail_context()$championship_id),
        user_team_id = shiny::reactive(detail_context()$user_team_id),
        on_bid_updated=function(...) {
          clear_api_cache(login_token()[["userid"]])
          if(is.function(on_market_event)) on_market_event(...)
        })
    }
    shiny::observeEvent(championship_id(),{selected(NULL);detail_context(NULL)})
    output$player_details <- shiny::renderUI({ shiny::req(selected()); selected_player_UI(session$ns("player")) })
    refresh_counts <- function() {
      auth <- login_token()
      official_count <- get_notification_unread(auth)
      alerts <- fetch_account_insights_alerts(auth)
      local(alerts)
      count(if(is.finite(official_count) && is.data.frame(alerts)) official_count+sum(!alerts$is_read,na.rm=TRUE) else NA_integer_)
    }
    shiny::observe({
      shiny::req(valid_login(login_token()))
      shiny::invalidateLater(60000, session)
      refresh_counts()
    })
    shiny::observeEvent(login_token(), {
      official(NULL); local(NULL); selected(NULL); detail_context(NULL); count(NA_integer_); last_success(NULL)
    }, ignoreNULL = FALSE)
    refresh <- function() {
      auth <- login_token(); shiny::req(valid_login(auth))
      result <- get_notifications(auth)
      if (!is.null(result)) {
        official(result); last_success(attr(result,"observed_at") %||% Sys.time())
        stale(identical(attr(result,"fetch_status"),"stale"))
      } else stale(TRUE)
      refresh_counts()
    }
    shiny::observe({
      shiny::req(is_module_active(), valid_login(login_token()), championship_id())
      tick(); if (!is.null(refresh_trigger)) refresh_trigger()
      refresh()
    })
    shiny::observeEvent(input$refresh, { clear_api_cache(login_token()[["userid"]]); tick(tick()+1L) })
    combined <- shiny::reactive({
      d <- official()
      alerts <- local()
      if (!is.null(alerts) && nrow(alerts)) {
        a <- data.frame(id=paste0("insights:",alerts$id), created_at=as.character(alerts$created_at),
          updated_at=as.character(alerts$created_at), type=as.character(alerts$alert_type),
          action=as.character(alerts$alert_type), is_read=as.logical(alerts$is_read),
          player_id=if ("player_id" %in% names(alerts)) as.character(alerts$player_id) else "",
          player_name="", subject="", championship_id=as.character(alerts$championship_id),
          league_name=alerts$league_name, source="Insights", message=paste(alerts$title,alerts$message),
          stringsAsFactors=FALSE)
        d <- dplyr::bind_rows(d,a)
      }
      if (is.null(d)) return(normalize_notifications(list()))
      d[order(d$created_at,decreasing=TRUE),,drop=FALSE]
    })
    shiny::observe({
      d <- combined()
      shiny::updateSelectInput(session,"event_filter",choices=c("All",sort(unique(d$action))))
    })
    filtered <- shiny::reactive({
      d <- combined()
      if (isTRUE(input$current_league)) d <- d[d$championship_id==championship_id(),,drop=FALSE]
      if (identical(input$read_filter,"unread")) d <- d[!d$is_read,,drop=FALSE]
      if (!is.null(input$source_filter) && input$source_filter!="All") d<-d[d$source==input$source_filter,,drop=FALSE]
      if (!is.null(input$event_filter) && input$event_filter!="All") d<-d[d$action==input$event_filter,,drop=FALSE]
      d
    })
    output$status <- shiny::renderUI({
      if (!valid_login(login_token())) return(shiny::p("Log in to see notifications."))
      if (is.null(last_success())) return(shiny::p("Notifications unavailable. Refresh to retry."))
      shiny::p(if (stale()) "Showing the last successful refresh. " else "Updated ",
        format(last_success(),"%Y-%m-%d %H:%M UTC",tz="UTC"))
    })
    output$items <- shiny::renderUI({
      d <- filtered()
      if (!nrow(d)) return(shiny::p("No notifications match these filters."))
      shiny::tagList(lapply(seq_len(nrow(d)), function(i) {
        r <- d[i,,drop=FALSE]
        event_js <- function(key, value) paste0("Shiny.setInputValue(",
          jsonlite::toJSON(session$ns(key),auto_unbox=TRUE),",",
          jsonlite::toJSON(as.character(value),auto_unbox=TRUE),",{priority:'event'});")
        shiny::div(class=paste("fm-notification",if(!r$is_read) "fm-notification-unread"),
          shiny::div(class="fm-notification-meta",r$source," · ",r$league_name," · ",
            shiny::tags$time(datetime=r$created_at,r$created_at)),
          shiny::strong(r$message), shiny::p(r$player_name),
          if(nzchar(r$player_id) && !is.na(r$player_id)) shiny::tags$button(class="btn btn-default",type="button",
            onclick=event_js("open_player",r$id),"Player details"),
          if(!r$is_read) shiny::tags$button(class="btn btn-default",type="button",
            onclick=event_js("mark_read",r$id),"Mark read"))
      }))
    })
    shiny::observeEvent(input$mark_read, {
      d <- filtered(); r <- d[d$id==input$mark_read,,drop=FALSE]; shiny::req(nrow(r)==1)
      ok <- if(r$source=="Futmondo") mark_notification_read(login_token(),r$id) else
        supabase_patch("user_smart_alerts",list(is_read=TRUE),list(id=paste0("eq.",sub("^insights:","",r$id)),
          user_id=paste0("eq.",login_token()[["userid"]]),championship_id=paste0("eq.",r$championship_id)))
      if(isTRUE(ok)) { clear_api_cache(login_token()[["userid"]]); tick(tick()+1L) } else shiny::showNotification("Could not mark this notification read. Retry.",type="error")
    })
    shiny::observeEvent(input$open_player, {
      d <- filtered(); r <- d[d$id==input$open_player,,drop=FALSE]; shiny::req(nrow(r)==1)
      # Membership is checked against the authenticated user's leagues.
      active <- get_active_championships(login_token())$championships
      allowed <- vapply(active,function(x)fm_scalar(x$id,""),character(1))
      shiny::req(r$championship_id %in% allowed)
      league <- active[[match(r$championship_id,allowed)]]
      detail_context(list(championship_id=r$championship_id,user_team_id=fm_scalar(league$userteam$id)))
      catalog <- get_championship_players(login_token(),r$championship_id)
      row <- catalog[as.character(catalog$id)==r$player_id,,drop=FALSE]
      if(nrow(row)) selected(row[1,,drop=FALSE])
    })
    list(unread=count)
  })
}
