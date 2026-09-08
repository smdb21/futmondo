automation_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::h2("Background automation"),
    shiny::p("Policies start in observation mode. Live actions require 14 verified observation days, current league rules, and your declared limits."),
    shiny::uiOutput(ns("connection_status")),
    shiny::actionButton(ns("connect"),"Connect background session"),
    shiny::actionButton(ns("disconnect"),"Disconnect and pause"),
    shiny::p('Collect this league every 15 minutes without creating trading policies.'),
    shiny::actionButton(ns('observe_league'),'Enable league observations'),
    shiny::actionButton(ns('pause_observations'),'Pause league observations'),
    shiny::hr(),
    shiny::fluidRow(shiny::column(6,
      shiny::textInput(ns("policy_name"),"Policy name","My trading policy"),
      shiny::selectInput(ns("targets"),"Approved players",choices=character(),multiple=TRUE),
      shiny::selectInput(ns("action"),"Action",choices=c("Bid"="bid","Modify bid"="modify_bid",
        "Cancel bid"="cancel_bid","List for sale"="list","Delist"="delist",
        "Accept offer at or above floor"="accept_bid","Reject offer below floor"="reject_bid","Pay clause"="clause")),
      shiny::numericInput(ns("amount"),"Bid or listing amount (€)",value=0,min=0),
      shiny::checkboxInput(ns("model_driven"),"Use the profit model for bid amounts",FALSE)),
    shiny::column(6,
      shiny::numericInput(ns("per_action"),"Maximum acquisition amount (€)",0,min=0),
      shiny::numericInput(ns("total_limit"),"Cumulative acquisition ceiling (€)",0,min=0),
      shiny::numericInput(ns("sale_floor"),"Minimum sale price (€)",0,min=0),
      shiny::numericInput(ns("resale_ratio"),"Assumed executable resale / value",0.95,min=0,max=1,step=0.01),
      shiny::dateInput(ns("expiry"),"Policy expires (UTC)",value=Sys.Date()+21),
      shiny::actionButton(ns("save_policy"),"Create observation policy"))),
    shiny::hr(),shiny::selectInput(ns("policy_id"),"Existing policy",choices=character()),
    shiny::actionButton(ns("pause"),"Pause"),shiny::actionButton(ns("resume"),"Resume observation"),
    shiny::actionButton(ns("go_live"),"Enable eligible live policy"),
    shiny::uiOutput(ns("policy_status")),
    shiny::h3("Uncertain account actions"),
    shiny::p("An uncertain response pauses every policy on your account and reserves the spending. Reconciliation checks the current API state without repeating the action."),
    shiny::selectInput(ns("uncertain_job"),"Action to reconcile",choices=character()),
    shiny::actionButton(ns("reconcile"),"Check API confirmation"),
    shiny::uiOutput(ns("reconciliation_status")),
    shiny::h3("Execution history"),reactable::reactableOutput(ns("history")))
}

automation_Server <- function(id,is_module_active,login_token,championship_id,user_team_id,refresh_trigger=NULL) {
  shiny::moduleServer(id,function(input,output,session) {
    tick<-shiny::reactiveVal(0L)
    policies<-shiny::reactiveVal(list())
    account_jobs<-shiny::reactiveVal(list())
    session_rows<-shiny::reactiveVal(list())
    reconciliation_message<-shiny::reactiveVal("")
    shiny::observeEvent(list(login_token(),championship_id()),{
      policies(list());account_jobs(list());session_rows(list());reconciliation_message("")
      tick(shiny::isolate(tick())+1L)
    },ignoreNULL=FALSE,priority=100)
    shiny::observe({
      shiny::req(is_module_active(),valid_login(login_token()),championship_id(),user_team_id())
      tick();if(!is.null(refresh_trigger)) refresh_trigger()
      shiny::invalidateLater(60000,session)
      uid<-login_token()[["userid"]]
      rows<-read_automation_rows("automation_policies",list(user_id=paste0("eq.",uid),
        championship_id=paste0("eq.",championship_id()),order="updated_at.desc"))
      policies(rows)
      ids<-vapply(rows,function(x)fm_scalar(x$id,""),character(1))
      labels<-vapply(rows,function(x)fm_scalar(x$policy$name,x$id),character(1))
      selected<-shiny::isolate(input$policy_id)
      shiny::updateSelectInput(session,"policy_id",choices=stats::setNames(ids,labels),
        selected=if(length(selected)==1L && selected %in% ids)selected else head(ids,1))
      jobs<-read_automation_rows("automation_jobs",list(user_id=paste0("eq.",uid),order="created_at.desc",limit=1000))
      account_jobs(jobs)
      unresolved<-Filter(function(x)identical(x$status,"uncertain"),jobs)
      shiny::updateSelectInput(session,"uncertain_job",choices=stats::setNames(
        vapply(unresolved,function(x)x$id,character(1)),
        vapply(unresolved,function(x)paste(x$action_type,x$payload$player_id,x$championship_id),character(1))))
      session_rows(read_automation_rows("automation_sessions",list(user_id=paste0("eq.",uid))))
      players<-tryCatch(get_championship_players(login_token(),championship_id()),error=function(e)NULL)
      if(is.data.frame(players) && all(c("id","name") %in% names(players)))
        shiny::updateSelectInput(session,"targets",choices=stats::setNames(as.character(players$id),players$name),
          selected=shiny::isolate(input$targets))
    })
    output$connection_status<-shiny::renderUI({
      if(!valid_login(login_token())) return(shiny::p("Log in to configure automation."))
      configured<-tryCatch({automation_key();TRUE},error=function(e)FALSE)
      if(!configured) return(shiny::p("Background access is unavailable until the worker encryption key is configured."))
      rows<-session_rows()
      if(length(rows)==1L) return(shiny::p("Background session expires ",fm_scalar(rows[[1]]$expires_at)," UTC. Reconnect after expiry."))
      shiny::p("Connecting stores an encrypted API session for up to seven days. The worker pauses when it expires.")
    })
    shiny::observeEvent(input$connect,{
      auth<-login_token();shiny::req(valid_login(auth))
      expiry<-format(Sys.time()+7*86400,"%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      encrypted<-tryCatch(encrypt_automation_session(auth,expiry),error=function(e)NULL)
      if(is.null(encrypted)) {shiny::showNotification("Background encryption is unavailable.",type="error");return()}
      code<-supabase_post_direct("automation_sessions",list(user_id=auth[["userid"]],encrypted_session=encrypted,
        expires_at=expiry,updated_at=format(Sys.time(),"%Y-%m-%dT%H:%M:%OSZ",tz="UTC")),"user_id")
      shiny::showNotification(if(is.numeric(code)&&code %in% 200:299) "Background session connected." else "Could not connect. Apply the database migration and retry.")
      tick(tick()+1L)
    })
    change_observations <- function(enabled) {
      shiny::req(valid_login(login_token()),championship_id(),user_team_id())
      ok <- set_observation_subscription(login_token(),championship_id(),user_team_id(),enabled)
      shiny::showNotification(if(ok) if(enabled) 'League observations enabled.' else 'League observations paused.' else 'Could not save observation settings.',type=if(ok)'message' else 'error')
    }
    shiny::observeEvent(input$observe_league,change_observations(TRUE))
    shiny::observeEvent(input$pause_observations,change_observations(FALSE))
    shiny::observeEvent(input$disconnect,{
      shiny::req(valid_login(login_token()))
      uid<-login_token()[["userid"]]
      paused<-supabase_patch("automation_policies",list(enabled=FALSE,pause_reason="Background session disconnected"),list(user_id=paste0("eq.",uid)))
      supabase_patch('observation_subscriptions',list(enabled=FALSE),list(user_id=paste0('eq.',uid)))
      deleted<-supabase_delete("automation_sessions",paste0("user_id=eq.",uid))
      if(!isTRUE(paused) || !isTRUE(deleted)) shiny::showNotification("Background disconnect could not be fully confirmed. Refresh and retry.",type="error")
      tick(tick()+1L)
    })
    shiny::observeEvent(input$save_policy,{
      shiny::req(valid_login(login_token()),championship_id(),user_team_id(),length(input$targets)>0)
      expiry<-paste0(as.character(input$expiry),"T23:59:59Z")
      policy<-list(name=input$policy_name,actions=list(input$action),allowed_player_ids=as.list(input$targets),
        amount=input$amount,max_per_action=input$per_action,total_spending_limit=input$total_limit,
        minimum_sale_price=input$sale_floor,execution_ratio=input$resale_ratio,
        model_driven=isTRUE(input$model_driven),expires_at=expiry)
      valid<-validate_automation_policy(policy)
      if(!valid$ok) {shiny::showNotification(paste(valid$errors,collapse="; "),type="error");return()}
      id<-paste0("policy_",paste(format(openssl::rand_bytes(16)),collapse=""))
      code<-supabase_post_direct("automation_policies",list(id=id,user_id=login_token()[["userid"]],
        championship_id=championship_id(),user_team_id=user_team_id(),enabled=TRUE,mode="shadow",
        policy=policy,expires_at=expiry,model_validated=FALSE),"id")
      shiny::showNotification(if(is.numeric(code)&&code %in% 200:299) "Observation policy created." else "Policy could not be saved.")
      tick(tick()+1L)
    })
    own_policy<-function() {
      auth<-login_token()
      if(!valid_login(auth)) return(NULL)
      rows<-policies();i<-which(vapply(rows,function(x)identical(x$id,input$policy_id) &&
        identical(fm_scalar(x$user_id),fm_scalar(auth[["userid"]])) &&
        identical(fm_scalar(x$championship_id),fm_scalar(championship_id())) &&
        identical(fm_scalar(x$user_team_id),fm_scalar(user_team_id())),logical(1)))
      if(length(i)!=1L) return(NULL)
      rows[[i]]
    }
    update_policy<-function(payload) {
      row<-own_policy();shiny::req(!is.null(row),valid_login(login_token()))
      if(isTRUE(payload$enabled)) {
        pending<-read_automation_rows("automation_jobs",list(user_id=paste0("eq.",login_token()[["userid"]]),status="eq.uncertain",limit=1))
        if(length(pending) || identical(attr(pending,"status"),"unavailable")) {
          shiny::showNotification("Resolve uncertain account actions before resuming.",type="warning");return()
        }
        if(!validate_automation_policy(row$policy)$ok) {
          shiny::showNotification("Policy expired or limits are invalid. Create a new observation policy.",type="warning");return()
        }
      }
      payload$updated_at<-format(Sys.time(),"%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      ok<-supabase_patch("automation_policies",payload,list(id=paste0("eq.",row$id),
        user_id=paste0("eq.",login_token()[["userid"]]),championship_id=paste0("eq.",championship_id()),
        user_team_id=paste0("eq.",user_team_id())))
      if(!isTRUE(ok)) shiny::showNotification("Policy update failed.",type="error")
      tick(tick()+1L)
    }
    shiny::observeEvent(input$pause,update_policy(list(enabled=FALSE,pause_reason="Paused by you")))
    shiny::observeEvent(input$resume,update_policy(list(enabled=TRUE,mode="shadow",pause_reason="")))
    shiny::observeEvent(input$go_live,{
      row<-own_policy();shiny::req(!is.null(row))
      jobs<-read_automation_rows("automation_jobs",list(policy_id=paste0("eq.",row$id),user_id=paste0("eq.",login_token()[["userid"]]),status="eq.shadow"))
      if(automation_shadow_days(jobs,row)<14L || (isTRUE(row$policy$model_driven)&&!isTRUE(row$model_validated))) {
        shiny::showNotification("Live mode requires 14 verified observation dates over at least 14 days and validation for model-driven policies.",type="warning");return()
      }
      financial<-tryCatch(get_financial_snapshot(login_token(),championship_id(),user_team_id()),error=function(e)NULL)
      if(!isTRUE(financial$rules$solvency_verified) ||
         ("accept_bid" %in% unlist(row$policy$actions) && !isTRUE(financial$lineup_rules$verified))) {
        shiny::showNotification("Live actions remain unavailable until the league deadline, solvency and relevant lineup rules are verified.",type="warning");return()
      }
      update_policy(list(enabled=TRUE,mode="live",pause_reason=""))
    })
    output$policy_status<-shiny::renderUI({
      row<-own_policy();if(is.null(row)) return(shiny::p("No policy selected."))
      p<-row$policy
      days<-automation_shadow_days(account_jobs(),row)
      money<-function(x)paste0(format(fm_number(x,0),big.mark=",",scientific=FALSE,trim=TRUE)," €")
      shiny::tagList(shiny::p(if(isTRUE(row$enabled))"Enabled" else "Paused"," · ",row$mode,
          " · expires ",row$expires_at," · ",days," / 14 verified observation days"),
        shiny::p(fm_scalar(row$pause_reason,"")),shiny::p(fm_scalar(row$last_error,"")),
        shiny::p("Actions: ",paste(unlist(p$actions),collapse=", ")," · Approved players: ",paste(unlist(p$allowed_player_ids),collapse=", ")),
        shiny::p("Bid / listing amount: ",money(p$amount)," · Maximum acquisition: ",money(p$max_per_action)),
        shiny::p("Cumulative acquisition ceiling: ",money(p$total_spending_limit)," · Sale floor: ",money(p$minimum_sale_price)),
        shiny::p("Model amounts: ",if(isTRUE(p$model_driven))"enabled" else "disabled",
          " · Model validation: ",if(isTRUE(row$model_validated))"passed" else "pending"))
    })
    shiny::observeEvent(input$reconcile,{
      auth<-login_token();shiny::req(valid_login(auth),nzchar(fm_scalar(input$uncertain_job,"")))
      rows<-read_automation_rows("automation_jobs",list(id=paste0("eq.",input$uncertain_job),
        user_id=paste0("eq.",auth[["userid"]]),status="eq.uncertain",limit=1))
      if(length(rows)!=1L || !identical(fm_scalar(rows[[1]]$user_id),fm_scalar(auth[["userid"]]))) return()
      job<-rows[[1]]
      result<-reconcile_automation_job(job,auth)
      if(result$status=="succeeded") {
        ok<-finish_automation_job(job,result)
        reconciliation_message(if(isTRUE(ok))"API state confirmed. The reservation is released; policies remain paused until you resume them." else "Confirmation could not be saved. The reservation remains in place; retry.")
      } else reconciliation_message(result$reason)
      tick(tick()+1L)
    })
    output$reconciliation_status<-shiny::renderUI(shiny::p(reconciliation_message()))
    output$history<-reactable::renderReactable({
      shiny::req(is_module_active(),valid_login(login_token()))
      rows<-Filter(function(x)identical(fm_scalar(x$championship_id),fm_scalar(championship_id())),account_jobs())
      d<-if(length(rows))dplyr::bind_rows(lapply(rows,function(x)data.frame(
        Created=fm_scalar(x$created_at),Action=fm_scalar(x$action_type),Player=fm_scalar(x$payload$player_id),
        Amount=fm_number(x$payload$amount),Status=fm_scalar(x$status),
        Detail=fm_scalar(x$result$reason,x$result$code %||% ""),Finished=fm_scalar(x$finished_at),stringsAsFactors=FALSE))) else
        data.frame(Status=if(identical(attr(account_jobs(),"status"),"unavailable"))"History unavailable" else "No automation history yet.")
      reactable::reactable(d,searchable=TRUE,defaultPageSize=10)
    })
  })
}
