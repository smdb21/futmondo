# Only player-specific notifications may open the player details panel.
notification_has_player_details <- function(notification_row) {
  if (!is.data.frame(notification_row) || nrow(notification_row)!=1L ||
      !"player_id" %in% names(notification_row)) return(FALSE)
  player_id <- as.character(notification_row$player_id[1])
  if (is.na(player_id) || !nzchar(trimws(player_id))) return(FALSE)
  fields <- intersect(c("type","action","message"),names(notification_row))
  event <- tolower(paste(vapply(fields,function(x)as.character(notification_row[[x]][1]),character(1)),collapse=" "))
  is_round_close <- grepl("round[[:space:]_-]*(close|closed|closure)",event) ||
    grepl("(close|closed|closure)[[:space:]_-]*round",event)
  !is_round_close
}

notifications_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(shiny::column(12, shiny::h2("Notifications"),
      shiny::p("Official Futmondo events and their last refresh."))),
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput(ns("read_filter"), "Read status", c("Unread"="unread","All"="all"))),
      shiny::column(3, shiny::selectInput(ns("source_filter"), "Source", c("Futmondo"))),
      shiny::column(3, shiny::selectInput(ns("event_filter"), "Event", c("All"))),
      shiny::column(3, shiny::checkboxInput(ns("current_league"), "Current league only", TRUE),
        shiny::actionButton(ns("refresh"), "Refresh"))),
    shiny::uiOutput(ns("status")), shiny::uiOutput(ns("items")),
    shiny::uiOutput(ns("player_details")))
}

notifications_Server <- function(id, is_module_active, login_token, championship_id,
                                  user_team_id, user_teams_RV, refresh_trigger = NULL, on_market_event=NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    bind_factual_reads(c("get_notifications","get_championship_players","get_active_championships"),session,login_token,championship_id,user_team_id,is_active=is_module_active,refresh_trigger=function()paste(if(is.function(refresh_trigger))refresh_trigger()else 0L,tick(),sep=":"))

    bind_factual_reads("get_notification_unread",session,login_token,
      is_active=function()valid_login(login_token()),refresh_trigger=function()paste(if(is.function(refresh_trigger))refresh_trigger()else 0L,tick(),sep=":"))
    tick <- shiny::reactiveVal(0L)
    count <- shiny::reactive({
      if (!valid_login(login_token())) return(NA_integer_)
      tick(); if (is.function(refresh_trigger)) refresh_trigger()
      shiny::invalidateLater(60000, session)
      get_notification_unread(login_token())
    })
    official <- shiny::reactiveVal(NULL)
    last_success <- shiny::reactiveVal(NULL)
    stale <- shiny::reactiveVal(FALSE)
    selected <- shiny::reactiveVal(NULL)
    detail_context <- shiny::reactiveVal(NULL)
    pending_player <- shiny::reactiveVal(NULL)
    request_context <- shiny::reactive({
      auth<-login_token();if(!valid_login(auth))return(NULL)
      list(user=fm_scalar(auth[["userid"]]),league=fm_scalar(championship_id()),team=fm_scalar(user_team_id()))
    })
    if (exists("selected_player_Server", mode="function")) {
      # Player links route through a dedicated module, preserving existing behavior.
      selected_player_Server("player", selected_player = selected, login_token = login_token,
        championship_id = shiny::reactive(detail_context()$championship_id),
        user_team_id = shiny::reactive(detail_context()$user_team_id),
        refresh_trigger=refresh_trigger,
        on_bid_updated=function(...) {
          clear_api_cache(login_token()[["userid"]])
          if(is.function(on_market_event)) on_market_event(...)
        })
    }
    shiny::observeEvent(request_context(),{pending_player(NULL);selected(NULL);detail_context(NULL)},ignoreNULL=FALSE,priority=100)
    output$player_details <- shiny::renderUI({ if(!is.null(pending_player()))return(shiny::p("Loading player details…"));shiny::req(selected()); selected_player_UI(session$ns("player")) })
    shiny::observeEvent(login_token(), {
      official(NULL);pending_player(NULL); selected(NULL); detail_context(NULL); last_success(NULL)
    }, ignoreNULL = FALSE)
    refresh <- function() {
      auth <- login_token(); shiny::req(valid_login(auth))
      result <- tryCatch(get_notifications(auth),error=function(e)NULL)
      if (!is.null(result)) {
        official(result); last_success(attr(result,"observed_at") %||% Sys.time())
        stale(identical(attr(result,"fetch_status"),"stale"))
      } else stale(TRUE)
    }
    shiny::observe({
      shiny::req(is_module_active(), valid_login(login_token()), championship_id())
      tick(); if (!is.null(refresh_trigger)) refresh_trigger()
      refresh()
    })
    shiny::observeEvent(input$refresh, { invalidate_resources(resource_context(login_token()),pattern="notification"); tick(tick()+1L) })
    combined <- shiny::reactive({
      d <- official()
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
          if(notification_has_player_details(r)) shiny::tags$button(class="btn btn-default",type="button",
            onclick=event_js("open_player",r$id),"Player details"),
          if(!r$is_read) shiny::tags$button(class="btn btn-default",type="button",
            onclick=event_js("mark_read",r$id),"Mark read"))
      }))
    })
    shiny::observeEvent(input$mark_read, {
      d <- filtered(); r <- d[d$id==input$mark_read,,drop=FALSE]; shiny::req(nrow(r)==1)
      ok <- mark_notification_read(login_token(),r$id)
      if(isTRUE(ok)) { invalidate_resources(resource_context(login_token()),pattern="notification"); tick(tick()+1L) } else shiny::showNotification("Could not mark this notification read. Retry.",type="error")
    })
    shiny::observeEvent(input$open_player,{
      d<-filtered();row<-d[d$id==input$open_player,,drop=FALSE]
      shiny::req(nrow(row)==1L,notification_has_player_details(row),valid_login(login_token()))
      selected(NULL);detail_context(NULL)
      pending_player(list(player_id=as.character(row$player_id),championship_id=as.character(row$championship_id),login=login_token(),origin=request_context()))
    })
    shiny::observe({
      shiny::req(is_module_active());target<-pending_player();shiny::req(!is.null(target))
      if(!identical(target$origin,request_context())||!identical(target$login,login_token())){pending_player(NULL);return()}
      active<-get_active_championships(target$login)$championships
      ids<-vapply(active,function(x)fm_scalar(x$id %||% x$`_id`,""),character(1));index<-which(ids==target$championship_id)
      if(length(index)!=1L){pending_player(NULL);return()}
      league<-active[[index]];catalog<-get_championship_players(target$login,target$championship_id)
      shiny::req(identical(target,pending_player()),identical(target$origin,request_context()))
      rows<-if(is.data.frame(catalog)&&"id"%in%names(catalog))catalog[as.character(catalog$id)==target$player_id,,drop=FALSE]else data.frame()
      if(nrow(rows)){
        detail_context(list(championship_id=target$championship_id,user_team_id=fm_scalar(league$userteam$id %||% league$userteam$`_id`)))
        selected(rows[1,,drop=FALSE])
      }else shiny::showNotification("Player details are unavailable for this notification.",type="warning")
      pending_player(NULL)
    })
    list(unread=count)
  })
}
