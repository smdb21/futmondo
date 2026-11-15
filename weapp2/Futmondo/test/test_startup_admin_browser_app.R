# Actual root/UI and worker renewal controller; synthetic data and RPCs only.
options(futmondo.offline=TRUE)
for(expr in parse('test/test_application_offline.R')){
 if(is.call(expr)&&identical(expr[[1]],as.name('app_check'))&&identical(expr[[2]],'full server login, navigation, refresh and logout'))break
 eval(expr,envir=.GlobalEnv)
}
Sys.setenv(admin='offline@example.invalid')
fm_timezone_store<-function(...)promises::promise_resolve(NULL)
# Exercise the production opt-in/renewal controller; all factual getters remain
# deterministic local fixtures, and every worker RPC is synthetic.
fm_worker_configured<-function()TRUE
fm_worker_mode<-function(...)FALSE
fm_worker_encrypt<-function(...){x<-'synthetic-encrypted-envelope';attr(x,'credential_id')<-'fixture';x}
fixture_pending<-shiny::reactiveVal(FALSE);fixture_renewals<-0L
fm_worker_rpc<-function(name,payload,owner=''){
 if(!name%in%c('read','renew'))stop('Unexpected worker RPC in root fixture')
 account<-list(enabled=TRUE,generation=1L,auth_status='active',timezone='Europe/Madrid',quiet_start=0,quiet_end=7,excluded_leagues=list())
 answer<-if(name=='read')list(account=account,health=list(heartbeat=rival_finance_stamp()),resources=list(),redirects=list(),jobs=list())else account
 if(name=='renew'){fixture_pending(TRUE);fixture_renewals<<-fixture_renewals+1L}
 promises::promise(function(resolve,reject)later::later(function(){if(name=='renew')fixture_pending(FALSE);resolve(answer)},if(name=='read')2 else 5))
}

ui<-htmltools::tagList(source('ui.R',local=.GlobalEnv)$value,shiny::div(style='display:none',shiny::textOutput('fixture_diag')),shiny::textOutput('fixture_beats'))
body(app_server)<-as.call(c(as.list(body(app_server)),quote(output$fixture_diag<-shiny::renderText({
 list_value<-tryCatch(league_settings_source(),error=function(e)NULL)
 jsonlite::toJSON(list(tab=input$tabs,admin=is_admin_RV(),startup=startup_stage(),configuration_fields=length(list_value$info$configuration),status=list_value$status,renewal_pending=fixture_pending(),renewals=fixture_renewals,io=offline_http_attempts),auto_unbox=TRUE)
}))))
body(app_server)<-as.call(c(as.list(body(app_server)),quote(output$fixture_beats<-shiny::renderText(input$fixture_heartbeat %||% 0L))))
body(app_server)<-as.call(c(as.list(body(app_server)),quote(shiny::outputOptions(output,'fixture_diag',suspendWhenHidden=FALSE))))
shiny::runApp(shiny::shinyApp(ui,app_server),host='127.0.0.1',port=3919,launch.browser=FALSE)
