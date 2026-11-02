# Synthetic HTTPS-proxied app for persistent HttpOnly cookie verification.
options(futmondo.offline=TRUE,futmondo.remember_fixture=TRUE)
suppressPackageStartupMessages(source("global.R"))
fixture_key<-sodium::random(32);fixture_records<-new.env(parent=emptyenv());fixture_fail_write<-FALSE
remember_login_enabled<-function()TRUE
remember_login_key<-function()fixture_key
remember_login_store<-function(method="GET",hash=NULL,payload=NULL,owner="",expires_before=NULL){
 if(!is.null(expires_before))return(promises::promise_resolve(TRUE))
 if(method=="POST"&&fixture_fail_write)return(promises::promise_reject(simpleError("Synthetic storage failure")))
 value<-switch(method,GET=fixture_records[[hash]],POST={fixture_records[[payload$browser_hash]]<-payload;TRUE},
  DELETE={if(!is.null(fixture_records[[hash]]))rm(list=hash,envir=fixture_records);TRUE})
 promises::promise_resolve(value)
}
remember_login_validate<-function(...)promises::promise_resolve(TRUE)
fm_async_login<-function(user_name,...){
 fixture_fail_write<<-identical(user_name,"remember-failure@example.invalid")
 promises::promise_resolve(c(token="synthetic-browser-token",userid="synthetic-browser-user",user_name="Fixture"))
}
# Real dashboard layout and Login -> Today navigation; synthetic data only.
fixture_ui<-source("ui.R",local=.GlobalEnv)$value
fixture_server<-function(input,output,session){
 token<-login_Server("login")
 output$menu<-shinydashboard::renderMenu(shinydashboard::sidebarMenu(id="tabs",
   shinydashboard::menuItem("Login",tabName="login",selected=TRUE),
   shinydashboard::menuItem("Today",tabName="today")))
 shiny::observeEvent(token(),{
   shiny::updateTabsetPanel(session,"tabs",selected=if(valid_login(token()))"today"else"login")
 },ignoreNULL=FALSE)
}
fixture_app_dir<-tempfile("futmondo-remember-fixture-");dir.create(fixture_app_dir);dir.create(file.path(fixture_app_dir,"www"))
file.copy("www/remember-login.js",file.path(fixture_app_dir,"www","remember-login.js"))
writeLines('shiny::shinyApp(ui=fixture_ui,server=fixture_server)',file.path(fixture_app_dir,"app.R"))
shiny::runApp(fixture_app_dir,host="127.0.0.1",port=as.integer(commandArgs(TRUE)[1]),launch.browser=FALSE)
