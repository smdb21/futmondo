# Presentation only: reuse pressroom names, league teams and already loaded players.
today_transfer_rows <- function(rows, teams=NULL, players=NULL) {
  if(!is.data.frame(rows)||!nrow(rows))return(data.frame(Status="No completed transfers available."))
  if("created" %in% names(rows))rows<-rows[order(fm_time(rows$created),decreasing=TRUE,na.last=TRUE),,drop=FALSE]
  rows<-head(rows,20L)
  field<-function(name)if(name %in% names(rows))as.character(rows[[name]])else rep("",nrow(rows))
  clean<-function(x){x[is.na(x)]<-"";x}
  escape<-function(x)as.character(htmltools::htmlEscape(clean(x)))
  party<-function(side){
    ids<-clean(field(paste0(side,"_team_id")))
    labels<-clean(field(paste0(side,"_team_name")))
    if(is.data.frame(teams)&&nrow(teams)){
      id_col<-intersect(c("id","teamid","_id"),names(teams))[1]
      name_col<-intersect(c("teamname","name"),names(teams))[1]
      if(!is.na(id_col)&&!is.na(name_col)){
        mapped<-as.character(teams[[name_col]][match(ids,as.character(teams[[id_col]]))])
        use<-!is.na(mapped)&nzchar(mapped);labels[use]<-mapped[use]
      }
    }
    labels[!nzchar(labels)]<-ids[!nzchar(labels)]
    labels[!nzchar(ids)|tolower(ids)%in%c("system","futmondo","computer")]<-"Futmondo"
    escape(labels)
  }
  player<-escape(field("player_name"))
  club<-clean(field("player_team_name"));logo<-clean(field("player_team_slug"))
  if(is.data.frame(players)&&all(c("id","team")%in%names(players))){
    index<-match(field("player_id"),as.character(players$id))
    for(i in which(!is.na(index)&!nzchar(club)&!nzchar(logo))){
      club[i]<-clean(as.character(players$team[index[i]]))
      if("logo"%in%names(players))logo[i]<-clean(as.character(players$logo[index[i]]))
    }
  }
  for(i in seq_len(nrow(rows))){
    image_name<-get_team_image_name(club[i],logo=logo[i])
    if(nzchar(image_name))player[i]<-paste0(as.character(shiny::tags$img(
      src=paste0(TEAM_LOGO_URL,image_name,".png"),alt="",width=22,height=22,
      loading="lazy",decoding="async",style="object-fit:contain; margin-right:7px; vertical-align:middle;",
      onerror="this.style.display='none';")),player[i])
  }
  classified<-rival_classify_transfers(rows)
  method<-classified$is_clause
  system_side<-function(side)tolower(clean(field(paste0(side,"_team_id"))))%in%c("","system","futmondo","computer")
  transfer_type<-rep("Between rivals · method unconfirmed",nrow(rows))
  transfer_type[!is.na(method)&!method]<-"Buy/sell between rivals"
  transfer_type[system_side("seller")]<-"Bought from Futmondo"
  transfer_type[system_side("buyer")]<-"Sold to Futmondo"
  transfer_type[!is.na(method)&method]<-"Clause buyout"
  price<-suppressWarnings(as.numeric(field("price")))
  money<-rep("",length(price));valid<-is.finite(price)
  money[valid]<-paste0(formatC(price[valid],format="f",digits=0,big.mark=".",decimal.mark=",")," €")
  data.frame(Date=escape(format(fm_time(field("created")),"%d/%m/%Y %H:%M",tz="UTC")),Name=player,Price=escape(money),
    Buyer=party("buyer"),Seller=party("seller"),Type=escape(transfer_type),check.names=FALSE)
}

# Primary factual content resolves independently of optional history/finances.
today_UI <- function(id) {
  ns <- NS(id)
  tagList(h2("Today's market"),p("Available market players and offers, current league balances and completed transfers."),
    coach_preference_script(),checkboxInput(ns("automatically_hire_coach"),"Automatically hire coach",value=TRUE),
    textOutput(ns("coach_status"),container=tags$p),
    fluidRow(uiOutput(ns("kpi_cash_box")),uiOutput(ns("kpi_valuation_box"))),
    uiOutput(ns("market_status")),
    players_table_UI(ns("market_players"),box_title="Current market listings",filter_by_is_from_futmondo=TRUE,default_free_agents=TRUE,filter_by_players_with_bid=TRUE),
    h3("Recent transfers"),tableOutput(ns("recent_transfers")))
}
today_Server <- function(id,is_module_active,login_token,championship_id,user_team_id,user_teams_RV,refresh_trigger=NULL,primary_sources=NULL,coach_source=NULL) {
  moduleServer(id,function(input,output,session) {
    if (is.null(primary_sources)) {
      team_retry<-reactiveVal(0L);market_retry<-reactiveVal(0L)
      retry_team<-function()team_retry(isolate(team_retry())+1L)
      retry_market<-function()market_retry(isolate(market_retry())+1L)
      read_state <- function(handler)factual_read_adapter(session,handler,login_token,championship_id,user_team_id,
        is_active=is_module_active,refresh_trigger=function()paste(if(is.function(refresh_trigger))refresh_trigger()else 0L,if(handler=="get_user_team_info")team_retry()else market_retry(),sep=":"),state_only=TRUE)
      team_read <- read_state("get_user_team_info")
      market_read <- read_state("get_market_players")
      team_state <- reactive({req(is_module_active(),valid_login(login_token()),championship_id(),user_team_id());team_read(login_token(),championship_id(),user_team_id())})
      market_state <- reactive({req(is_module_active(),valid_login(login_token()),championship_id(),user_team_id());market_read(login_token(),championship_id(),user_team_id())})
    } else {
      team_state <- primary_sources$team_state
      market_state <- primary_sources$market_state
      retry_team <- primary_sources$retry_team
      retry_market <- primary_sources$retry_market
    }
    if(!is.null(coach_source)){
      preference_generation<-0L;preference_ready<-reactiveVal(FALSE);preference_warning<-reactiveVal("")
      observeEvent(coach_source$key(),{
        preference_generation<<-preference_generation+1L;preference_ready(FALSE)
        key<-coach_source$key();if(is.null(key))return()
        session$sendCustomMessage("coach_preference",list(key=key,input=session$ns("coach_preference_ready"),
          checkbox=session$ns("automatically_hire_coach"),nonce=preference_generation))
      },ignoreNULL=FALSE)
      observeEvent(input$coach_preference_ready,{
        message<-input$coach_preference_ready
        req(identical(as.integer(message$nonce),preference_generation),identical(message$key,coach_source$key()))
        if(!isTRUE(message$available)){
          preference_warning("Saved preference could not be read. Automatic hiring is disabled for this login.")
          updateCheckboxInput(session,"automatically_hire_coach",value=FALSE)
          coach_source$set_preference(message$key,FALSE);return()
        }
        preference_warning("")
        if(isTRUE(coach_source$set_preference(message$key,message$enabled)))preference_ready(TRUE)
      })
      observeEvent(input$automatically_hire_coach,{
        req(preference_ready());enabled<-isTRUE(input$automatically_hire_coach)
        coach_source$set_preference(coach_source$key(),enabled)
        session$sendCustomMessage("coach_preference_save",list(key=coach_source$key(),enabled=enabled,input=session$ns("coach_preference_saved")))
      },ignoreInit=TRUE)
      observeEvent(input$coach_preference_saved,{
        message<-input$coach_preference_saved;req(identical(message$key,coach_source$key()))
        preference_warning(if(isTRUE(message$ok))""else "Your choice applies to this login, but this browser could not remember it.")
      })
      output$coach_status<-renderText(paste(coach_source$status(),preference_warning()))
    }
    observeEvent(input$retry_team,retry_team())
    observeEvent(input$retry_market,retry_market())
    retry_failed<-function(){
      if(is.null(isolate(team_state())$data))retry_team()
      if(is.null(isolate(market_state())$data))retry_market()
    }
    # Once the first render settles, coach confirmation may refresh only team
    # information without suspending optional tables or re-opening startup UI.
    settled_context<-reactiveVal(NULL)
    primary_context<-reactive({
      auth<-login_token();if(!valid_login(auth))return(NULL)
      paste(auth[["userid"]],championship_id(),user_team_id(),sep="|")
    })
    primary_settled <- reactive({
      context<-primary_context();if(is.null(context))return(FALSE)
      states <- list(team_state(),market_state())
      all(vapply(states,function(s)!identical(s$status,"loading"),logical(1)))||identical(settled_context(),context)
    })
    observe({
      context<-primary_context();if(is.null(context)){settled_context(NULL);return()}
      states<-list(team_state(),market_state())
      if(all(vapply(states,function(s)!identical(s$status,"loading"),logical(1)))&&!identical(isolate(settled_context()),context))settled_context(context)
    })
    primary_status <- reactive({
      if(!primary_settled())return("loading")
      states <- list(team_state(),market_state())
      if(all(vapply(states,function(s)is.null(s$data),logical(1))))"failed" else
        if(any(vapply(states,function(s)is.null(s$data),logical(1))))"partial" else "ready"
    })
    # Explicitly demand both primary reads even before the table output is visible.
    observe({req(is_module_active());team_state();market_state()})
    bind_factual_reads("get_championship_pressroom",session,login_token,championship_id,user_team_id,
      is_active=function()isTRUE(is_module_active())&&isTRUE(primary_settled()),refresh_trigger=refresh_trigger)
    require_data <- function(state) {
      if(is.null(state$data))shiny::validate(shiny::need(FALSE,if(identical(state$status,"loading"))"Loading data…"else resource_failure_message(state$error)))
      state$data
    }
    market <- reactive({
      rows <- require_data(market_state())
      if(is.data.frame(rows)&&nrow(rows))rows<-unify_columns(calculate_player_changes(translate_player_positions(rows)))
      rows
    })
    primary_placeholder<-function(state,label){
      div(class="col-sm-6",div(class="small-box",h4(label),
        if(identical(state$status,"loading"))div(role="status",icon("spinner",class="fa-spin")," Loading data…")else
          tagList(p(resource_failure_message(state$error)),actionButton(session$ns("retry_team"),"Retry balance"))))
    }
    output$market_status<-renderUI({
      state<-market_state()
      if(!is.null(state$data)||identical(state$status,"loading"))return(NULL)
      div(role="status",span(resource_failure_message(state$error)),actionButton(session$ns("retry_market"),"Retry market"))
    })
    output$kpi_cash_box <- renderUI({
      state<-team_state();if(is.null(state$data))return(primary_placeholder(state,"Current balance"))
      info<-state$data
      shinydashboard::valueBox(player_card_money(fm_number(info$budget)),"Current balance",icon=icon("wallet"),color="green",width=6)
    })
    output$kpi_valuation_box <- renderUI({
      state<-team_state();if(is.null(state$data))return(NULL)
      info<-state$data
      shinydashboard::valueBox(player_card_money(fm_number(info$teamValue %||% info$team_value)),"Observed team value",icon=icon("users"),color="blue",width=6)
    })
    output$recent_transfers <- renderTable({
      req(is_module_active(),valid_login(login_token()))
      shiny::validate(shiny::need(primary_settled(),"Loading recent transfers…"))
      rows<-get_championship_pressroom(login_token(),championship_id())
      # Reuse classifications already established by the rival ledger, without
      # starting finance retrieval or reacting to its background progress ticks.
      shared<-session$rootScope()$userData$rival_finance_shared
      if(!is.null(shared)&&identical(shared$identity,primary_context())){
        known<-isolate(shared$value())$pressroom
        if(is.data.frame(rows)&&is.data.frame(known)&&all(c("id","is_clause")%in%names(known))&&"id"%in%names(rows)){
          method<-known$is_clause[match(rows$id,known$id)]
          if(!"is_clause"%in%names(rows))rows$is_clause<-NA
          use<-!is.na(method);rows$is_clause[use]<-method[use]
        }
      }
      today_transfer_rows(rows,user_teams_RV(),market_state()$data)
    },sanitize.text.function=identity)
    players_table_Server("market_players",market,user_teams_RV,login_token,championship_id,user_team_id,
      refresh_trigger=refresh_trigger,is_module_active=is_module_active,optional_ready=primary_settled,points_ratio_basis="value")
    list(primary_settled=primary_settled,primary_status=primary_status,retry_failed=retry_failed)
  })
}
