# Actual card and scheduling widgets, synthetic boundaries only.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_clause_enabled<-function()TRUE
fm_worker_configured<-function()FALSE
fm_clause_diagnostic<-function(login) identical(login[['userid']], 'fixture')
fm_async_http<-function(...) stop('The blocked diagnostic must never dispatch')
fm_worker_encrypt<-function(auth){x<-'synthetic-ciphertext';attr(x,'credential_id')<-'fixture-binding';x}
fixture<-new.env(parent=emptyenv());fixture$jobs<-list();fixture$pending<-NULL
capacity<-function()observed_data(list(status='ok',valid_until=as.numeric(Sys.time())+15,
 roster=list(status='ok',count=9,effective_count=9,cap=12),funds=list(spendable_budget=1000),outstanding=list(count=0),
 rules=list(minimum_bid_at_value=FALSE)))
async_adapter_fetch<-function(fn,args=list(),context=NULL,is_current=function()TRUE,...){
 if(fn=='get_scheduled_clause_check')return(promises::promise(function(resolve,reject)fixture$pending<-resolve))
 value<-switch(fn,get_team_acquisition_capacity=capacity(),get_player_summary=list(championship=list(),points=list(),prices=list()),
  get_lineup_from_team=list(pointSystem='stats'),get_player_full_profile=list(profile=list()),get_player_matches=list(matches=list()),
  read_card_round_history=data.frame(),read_card_price_history=data.frame(),stop(fn))
 promises::promise_resolve(value)
}
fm_clause_rpc<-function(name,payload,owner=''){
 message('Fixture RPC: ',name)
 if(name=='admit')fixture$jobs<-list(list(id=payload$p_id,account_id=payload$p_context$account_id,context=payload$p_context,
  kind='scheduled',mode='clause',player_id='p',player_name=payload$p_player$name,max_price=100,reserved_amount=100,reserved_slot=1,
  state='scheduled',opens_at=payload$p_player$opens_at,result_code='',acknowledged_at=NULL))
 if(name=='cancel'){fixture$jobs[[1]]$state<-'cancelled';fixture$jobs[[1]]$reserved_amount<-0;fixture$jobs[[1]]$reserved_slot<-0;fixture$jobs[[1]]$finished_at<-fm_clause_stamp()}
 if(name=='ack')fixture$jobs[[1]]$acknowledged_at<-fm_clause_stamp()
 if(!name%in%c('read','admit','cancel','ack','renew'))stop('Unexpected fixture RPC')
 promises::promise_resolve(if(name=='read')list(jobs=fixture$jobs)else if(name=='admit')fixture$jobs[[1]]else TRUE)
}
ui<-shiny::fluidPage(shiny::tags$head(shiny::tags$style(shiny::HTML(fm_theme_css())),shiny::includeCSS('www/custom_style.css')),
 shiny::actionButton('heartbeat','Heartbeat'),shiny::textOutput('beats'),shiny::actionButton('finish','Finish verification'),
 shiny::actionButton('finish_blocked','Finish blocked verification'),
 shiny::actionButton('logout','Log out'),shiny::actionButton('login','Log in'),shiny::uiOutput('pending'),shiny::div(style='display:none',shiny::textOutput('fixture_state')),shiny::uiOutput('clause_results'),selected_player_UI('card'))
server<-function(input,output,session){
 session$userData$league_context_enabled<-TRUE
 login<-shiny::reactiveVal(c(userid='fixture',token='synthetic'))
 ctx<-resource_context(c(userid='fixture',token='synthetic'),'l','t','p',competition_id='c',season='S',scoring_version=fm_scoring_version('stats'))
 remember_league_context(list(account_id='fixture',championship_id='l',competition_id='c',season='S',scoring_version=fm_scoring_version('stats'),
  observed_at=fm_clause_stamp(),expires_at=fm_clause_stamp(Sys.time()+300),complete=TRUE,payload=list()))
 s<-fm_clause_account('fixture');s$known<-TRUE;s$jobs<-list()
 selected<-shiny::reactive({if(!valid_login(login()))return(NULL)
  structure(data.frame(id='p',name='Fixture rival player',slug='fixture',role='defensa',team='Club',value=100,price=NA_real_,
   change=0,points=10,user_team_id='rival',market_inMarket=FALSE,clause_price=100,clause_transferred=FALSE,clause_date='2066-11-15T12:00:00Z'), availability_session_verified=TRUE)})
 fm_clause_session(session,login)
 selected_player_Server('card',selected,login_token=login,championship_id=shiny::reactive('l'),user_team_id=shiny::reactive('t'),
  capacity_fetcher=function(...)capacity(),allow_scheduled_clauses=TRUE)
 output$pending<-shiny::renderUI({shiny::req(valid_login(login()));fm_clause_pending_ui(fm_clause_jobs(ctx))})
 output$beats<-shiny::renderText(input$heartbeat %||% 0)
 output$fixture_state<-shiny::renderText({input$heartbeat;fm_clause_account('fixture')$revision();fm_worker_json(list(jobs=fixture$jobs,context=ctx))})
 shiny::outputOptions(output,'fixture_state',suspendWhenHidden=FALSE)
 shiny::observeEvent(input$finish,{
  shiny::req(is.function(fixture$pending))
  fixture$pending(list(player=list(id='p',owner_id='rival',owner_verified=TRUE,price=100,transferred=FALSE,opens_at='2066-11-15T12:00:00Z')))
  fixture$pending<-NULL
 })
 shiny::observeEvent(input$finish_blocked,{
  shiny::req(is.function(fixture$pending))
  facts <- capacity()
  facts$funds$spendable_budget <- 99
  facts$player <- list(id='p',slug='fixture',owner_id='rival',owner_verified=TRUE,price=100,
    transferred=FALSE,opens_at='2066-11-15T12:00:00Z')
  fixture$pending(facts)
  fixture$pending<-NULL
 })
 shiny::observeEvent(input$logout,login(NULL))
 shiny::observeEvent(input$login,login(c(userid='fixture',token='synthetic')))
}
shiny::runApp(shiny::shinyApp(ui,server),host='127.0.0.1',port=3914,launch.browser=FALSE)
