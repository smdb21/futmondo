# Actual Admin module with synthetic, deliberately slow database responses.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source("global.R"))
Sys.setenv(admin="admin@example.invalid")
fixture<-new.env();fixture$timezone_reads<-0L;fixture$writes<-0L
blocked_io<-function(...)stop("Unexpected live I/O in Admin fixture")
for(name in c("futmondo_post","fm_async_http","supabase_read","supabase_upsert","supabase_write"))assign(name,blocked_io)
fm_access_store<-function(...)promises::promise_resolve(TRUE)
fm_timezone_store<-function(...){
 fixture$timezone_reads<-fixture$timezone_reads+1L
 promises::promise(function(resolve,reject)later::later(function()resolve(list(
  list(user_id="fixture-user",display_name="Fixture user",timezone="Europe/Madrid"))),5))
}
shared_mvp_source<-function(...)reactiveVal(list(coverage=data.frame(),loading=FALSE,reason="Synthetic empty coverage"))
ui<-fluidPage(tags$head(tags$style(HTML(fm_theme_css())),includeCSS("www/custom_style.css")),
 actionButton("toggle_admin","Open / close Admin"),actionButton("heartbeat","Heartbeat"),
 textOutput("beats"),textOutput("reads"),uiOutput("page"))
server<-function(input,output,session){
 active<-reactiveVal(FALSE)
 observeEvent(input$toggle_admin,active(!active()))
 output$page<-renderUI(if(active())admin_UI("admin")else p("Another page"))
 output$beats<-renderText(input$heartbeat %||% 0L)
 output$reads<-renderText({input$heartbeat;fixture$timezone_reads})
 admin_Server("admin",active,function()c(userid="admin",token="synthetic",user_name="admin@example.invalid"),
  function()"fixture-league",function()"own-team",function()data.frame(),is_admin=TRUE,
  league_settings_source=function()list(league=list(configuration=list(budget=300000000,
   playerMoveInDays=4,rankingMode="top",usersToRank=-1,ac=TRUE,mtoffset=-60)),info=list()))
}
shiny::runApp(shiny::shinyApp(ui,server),host="127.0.0.1",port=as.integer(Sys.getenv("FUTMONDO_ADMIN_TEST_PORT","3917")),launch.browser=FALSE)
