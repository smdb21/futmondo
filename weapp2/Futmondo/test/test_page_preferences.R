#!/usr/bin/env Rscript
library(shiny)
library(promises)
source('page_preferences.R')
valid_login <- function(x) !is.null(x)
auth <- function(id) c(token='fixture',userid=id,user_name=paste0(id,'@example.invalid'))
settle <- function(session=NULL) {
  for(i in 1:15) {later::run_now(0);if(!is.null(session))session$flushReact()}
}
f <- new.env()
f$rows <- list(a=list(page='classification'),b=list(page='yourteam'),restricted=list(page='admin'),invalid=list(page='removed'))
f$calls <- list();f$reads <- list();f$writes <- list();f$delay_read <- FALSE;f$delay_write <- FALSE;f$fail_read <- FALSE;f$fail_write <- FALSE
fake_store <- function(user_id,page=NULL,visited_at=NULL,owner='') {
  f$calls[[length(f$calls)+1L]] <- list(user=user_id,page=page,stamp=visited_at)
  if(is.null(page)) {
    if(f$fail_read)return(promise_reject(simpleError('fixture read failed')))
    value <- f$rows[[user_id]]
    if(!f$delay_read)return(promise_resolve(value))
    return(promise(function(resolve,reject)f$reads[[length(f$reads)+1L]]<-list(resolve=resolve,value=value)))
  }
  if(f$fail_write)return(promise_reject(simpleError('fixture write failed')))
  if(f$delay_write)return(promise(function(resolve,reject)f$writes[[length(f$writes)+1L]]<-list(resolve=resolve,reject=reject,user=user_id,page=page)))
  f$rows[[user_id]]<-list(page=page)
  promise_resolve(TRUE)
}
server <- function(input,output,session) {
  token <- reactive(if(is.null(input$user)||input$user=='')NULL else auth(input$user))
  nav <- fm_page_preferences(session,token,reactive(input$tabs),reactive(isTRUE(input$admin)),fake_store)
}
write_count <- function()sum(vapply(f$calls,function(x)!is.null(x$page),logical(1)))
testServer(server,{
 session$setInputs(tabs='login',user='a');settle(session)
 stopifnot(nav$initial()=='classification',write_count()==0L)
 session$setInputs(tabs='classification');settle(session);stopifnot(write_count()==0L)
 session$setInputs(tabs='rivals');settle(session);stopifnot(f$rows$a$page=='rivals',write_count()==1L)
 session$setInputs(tabs='rivals');settle(session);stopifnot(write_count()==1L)
 session$setInputs(tabs='login');settle(session);stopifnot(f$rows$a$page=='rivals')
 session$setInputs(user='');settle(session)
 session$setInputs(user='a');settle(session);stopifnot(nav$initial()=='rivals')
 session$setInputs(tabs='rivals');settle(session)
 session$setInputs(user='b');settle(session);stopifnot(nav$initial()=='yourteam',f$rows$b$page=='yourteam')
 session$setInputs(tabs='yourteam');settle(session);stopifnot(write_count()==1L)
})
# New process/session-equivalent state re-reads the durable fake database.
testServer(server,{
 session$setInputs(tabs='login',user='a');settle(session);stopifnot(nav$initial()=='rivals')
 session$setInputs(user='restricted');settle(session);stopifnot(nav$initial()=='today')
 session$setInputs(tabs='today');settle(session)
 session$setInputs(tabs='admin');settle(session);stopifnot(f$rows$restricted$page=='admin')
 session$setInputs(user='');settle(session)
 session$setInputs(user='restricted',admin=TRUE);settle(session);stopifnot(nav$initial()=='admin')
 session$setInputs(user='invalid');settle(session);stopifnot(nav$initial()=='today')
 session$setInputs(user='new');settle(session);stopifnot(nav$initial()=='today')
})
cat('PASS account isolation, fresh-session restoration, no restore/duplicate writes, login exclusion and authorized pages\n')
f$delay_read<-TRUE
testServer(server,{
 session$setInputs(tabs='login',user='a');settle(session)
 stopifnot(nav$initial()=='login')
 session$setInputs(tabs='round_mvps');settle(session);stopifnot(f$rows$a$page=='round_mvps')
 f$reads[[1]]$resolve(f$reads[[1]]$value);settle(session)
 stopifnot(nav$initial()=='login',input$tabs=='round_mvps')
 session$setInputs(user='b');settle(session)
 session$setInputs(user='');settle(session)
 f$reads[[2]]$resolve(f$reads[[2]]$value);settle(session);stopifnot(nav$initial()=='login')
})
f$delay_read<-FALSE;f$fail_read<-TRUE
testServer(server,{
 session$setInputs(tabs='login',user='a');settle(session)
 stopifnot(nav$initial()=='today',grepl('could not be loaded',output$page_preference_status$html))
 f$fail_read<-FALSE
 session$setInputs(retry_page_preference=1);settle(session);stopifnot(nav$initial()=='round_mvps')
 session$setInputs(tabs='round_mvps');settle(session)
 f$fail_write<-TRUE
 session$setInputs(tabs='plots');settle(session)
 stopifnot(f$rows$a$page=='round_mvps',grepl('has not been saved',output$page_preference_status$html))
 f$fail_write<-FALSE
 session$setInputs(retry_page_preference=2);settle(session);stopifnot(f$rows$a$page=='plots')
})
cat('PASS delayed reads never override navigation/logout; read and write errors remain visible and retryable\n')
f$delay_write<-TRUE
before<-write_count()
testServer(server,{
 session$setInputs(tabs='login',user='a');settle(session)
 session$setInputs(tabs='plots');settle(session)
 session$setInputs(tabs='rivals');settle(session)
 session$setInputs(tabs='classification');settle(session)
 session$setInputs(tabs='notifications');settle(session)
 stopifnot(length(f$writes)==1L,write_count()==before+1L)
 f$writes[[1]]$resolve(TRUE);settle(session)
 stopifnot(length(f$writes)==2L,f$writes[[2]]$page=='notifications')
 # An unacknowledged save remains retryable.
 f$writes[[2]]$resolve(FALSE);settle(session)
 stopifnot(grepl('has not been saved',output$page_preference_status$html))
 session$setInputs(retry_page_preference=1);settle(session)
 stopifnot(length(f$writes)==3L,f$writes[[3]]$page=='notifications')
 f$writes[[3]]$resolve(TRUE);settle(session)
})
cat('PASS at most one in-flight write; rapid navigation coalesces to latest page; acknowledgements required\n')
# Exercise the real REST query/body construction with a synthetic HTTP boundary.
get_sb_url<-function()'https://fixture.invalid'
get_sb_key<-function()'server-fixture-key'
requests<-list()
fm_async_http<-function(url,method,body,headers,owner,timeout) {
 requests[[length(requests)+1L]]<<-list(url=url,method=method,body=body,timeout=timeout)
 promise_resolve(list(content=charToRaw(if(method=='GET') '[{"page":"rivals","visited_at":"2026-11-06T00:00:00Z"}]' else '')))
}
read<-NULL;written<-FALSE
then(fm_page_store('account-1'),function(x)read<<-x);settle()
then(fm_page_store('account-1','plots','2026-11-06T00:00:00Z'),function(x)written<<-x);settle()
stopifnot(read$page=='rivals',written,grepl('user_id=eq.account-1',requests[[1]]$url,fixed=TRUE),
 requests[[1]]$timeout==8,requests[[2]]$method=='POST')
body<-jsonlite::fromJSON(requests[[2]]$body)
stopifnot(identical(names(body),c('user_id','page','visited_at')),body$user_id=='account-1',body$page=='plots')
cat('PASS real REST account filter, upsert payload, bounded timeout and credential-free data\n')
