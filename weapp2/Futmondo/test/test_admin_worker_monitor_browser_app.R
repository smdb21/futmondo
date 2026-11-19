# Actual Admin module with delayed synthetic monitoring; all live I/O is forbidden.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source("global.R"))
Sys.setenv(admin="admin@example.invalid")
blocked_io<-function(...)stop("Unexpected live I/O in worker monitor browser fixture")
for(name in c("futmondo_post","fm_async_http","supabase_read","supabase_upsert","supabase_write","fm_worker_rpc"))assign(name,blocked_io)
fm_access_store<-function(...)promises::promise_resolve(TRUE)
shared_mvp_source<-function(...)reactiveVal(list(coverage=data.frame(),loading=FALSE,reason="Synthetic empty coverage"))
fm_transport_cancel<-function(...)invisible(NULL) # Let delayed responses arrive to test rejection after navigation.

monitor_snapshot<-function(){
  stamp<-format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
  lease<-format(Sys.time()+60,"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
  workers<-c("11111111-1111-4111-8111-111111111111","22222222-2222-4222-8222-222222222222")
  job<-function(id,worker,lane,route)list(job_id=id,worker_id=worker,lane=lane,
    account_ref="bbbbbbbbbbbb",handler="get_players_from_team",started_at=stamp,lease_until=lease,
    activity=list(phase="source_request",route=route,method="POST",started_at=stamp,reported_at=stamp,fresh=TRUE))
  list(checked_at=stamp,heartbeat_timeout_seconds=180,activity_sample_seconds=15,
    summary=list(live_workers=2,working_workers=2,active_jobs=4,retained_workers=3,expired_jobs=0),
    workers=lapply(workers,function(w)list(worker_id=w,status="leader",heartbeat=stamp,
      lease_until=lease,requests=12,jobs=8,active_jobs=2)),
    current=list(active_jobs=2,jobs=list(job("aaaaaaaaaaaa",workers[1],"0","/1/userteam/roster"),
      job("cccccccccccc",workers[2],"0","/1/userteam/roster"))),
    others=list(active_jobs=2,jobs=list(job("dddddddddddd",workers[1],"1","/1/userteam/roster"),
      job("eeeeeeeeeeee",workers[2],"1","/1/player/summary"))),
    limits=list(workers_truncated=FALSE,jobs_truncated=FALSE))
}
fm_admin_worker_store<-function(token,owner=""){
  session<-shiny::getDefaultReactiveDomain()$rootScope()
  fixture<-session$userData$monitor_fixture
  fixture$reads(isolate(fixture$reads())+1L)
  failure<-identical(isolate(session$input$response_mode),"failure")
  delay<-as.numeric(isolate(session$input$request_delay) %||% "0.05")
  promises::promise(function(resolve,reject)later::later(function(){
    fixture$completed(isolate(fixture$completed())+1L)
    if(failure)reject(simpleError("PRIVATE synthetic failure"))else resolve(monitor_snapshot())
  },delay))
}
ui<-fluidPage(tags$head(tags$style(HTML(fm_theme_css())),includeCSS("www/custom_style.css"),
  includeScript("www/worker-state.js")),
  div(class="fixture-controls",actionButton("toggle_admin","Open / close Admin"),
    actionButton("toggle_permission","Change admin permission"),actionButton("heartbeat","Heartbeat"),
    selectInput("response_mode","Response",c("success","failure"),selectize=FALSE),
    selectInput("request_delay","Synthetic delay",c("0.05","2"),selectize=FALSE)),
  textOutput("beats"),textOutput("reads"),textOutput("completed"),textOutput("permission"),uiOutput("page"))
server<-function(input,output,session){
  active<-reactiveVal(FALSE);admin<-reactiveVal(TRUE)
  session$userData$monitor_fixture<-list(reads=reactiveVal(0L),completed=reactiveVal(0L))
  login<-reactive(c(userid="fixture-admin",token="synthetic-only",
    user_name=if(admin())"admin@example.invalid"else"user@example.invalid"))
  observeEvent(input$toggle_admin,active(!active()))
  observeEvent(input$toggle_permission,admin(!admin()))
  output$page<-renderUI(if(active())admin_UI("admin")else p("Another page"))
  output$beats<-renderText(input$heartbeat %||% 0L)
  output$reads<-renderText(session$userData$monitor_fixture$reads())
  output$completed<-renderText(session$userData$monitor_fixture$completed())
  output$permission<-renderText(if(admin())"admin"else"regular")
  admin_Server("admin",active,login,function()"fixture-league",function()"own-team",
    function()data.frame(),is_admin=admin,
    league_settings_source=function()list(league=list(configuration=list(budget=300000000,
      playerMoveInDays=4,rankingMode="top",usersToRank=-1,ac=TRUE,mtoffset=-60)),info=list()))
}
shiny::runApp(shiny::shinyApp(ui,server),host="127.0.0.1",
  port=as.integer(Sys.getenv("FUTMONDO_MONITOR_TEST_PORT","3928")),launch.browser=FALSE)
