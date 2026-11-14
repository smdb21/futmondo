#!/usr/bin/env Rscript
# Synthetic responses only: no real accounts, credentials or database access.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('futmondo_functions.R'))
source('page_preferences.R')
get_sb_url<-function()'https://fixture.invalid'
get_sb_key<-function()'synthetic-server-key'
valid_login<-function(x)!is.null(x)
checks<-0L;check<-function(label,expr){force(expr);checks<<-checks+1L;cat('PASS',label,'\n')}
flush<-function(seconds=.03,session=NULL){until<-Sys.time()+seconds;while(Sys.time()<until){later::run_now(.005);if(!is.null(session))session$flushReact()}}
await<-function(p){done<-FALSE;result<-NULL;error<-NULL;promises::then(p,function(x){result<<-x;done<<-TRUE;NULL},function(e){error<<-e;done<<-TRUE;NULL});until<-Sys.time()+2;while(!done&&Sys.time()<until)later::run_now(.005);stopifnot(done);if(!is.null(error))stop(error);result}
settle<-function(p)tryCatch(await(p),error=identity)
response<-function(body)list(status_code=200L,content=charToRaw(body),times=list(total=0))
requests<-list();outcomes<-list();logs<-character()
fm_async_http<-function(url,method='GET',body=NULL,headers=character(),owner='',timeout=15,priority='normal'){
 requests[[length(requests)+1L]]<<-list(method=method,timeout=timeout,priority=priority,owner=owner)
 value<-outcomes[[min(length(requests),length(outcomes))]]
 if(inherits(value,'error'))promises::promise_reject(value)else promises::promise_resolve(value)
}
record_logs<-function(expr)withCallingHandlers(expr,message=function(note){logs<<-c(logs,conditionMessage(note));invokeRestart('muffleMessage')})
reset<-function(values){requests<<-list();outcomes<<-values;logs<<-character();stopifnot(!length(.fm_transport$read_states),!length(.fm_transport$reads))}
check('saved navigation recovers once from a transient response without blocking or showing an error',{
 reset(list(resource_failure(category='http',http_status=503L),response('[{"page":"yourteam","visited_at":"2026-11-14T01:19:00Z"}]')))
 shiny::testServer(function(input,output,session){
  nav<-fm_page_preferences(session,shiny::reactive(if(isTRUE(input$logged_in))c(userid='synthetic-account')else NULL),shiny::reactive(input$tabs),shiny::reactive(FALSE))
 },{
  session$setInputs(tabs='login',logged_in=TRUE);flush(.03,session)
  heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,.02);flush(.06,session)
  stopifnot(heartbeat,nav$initial()=='login',length(requests)==1L)
  flush(.3,session)
  stopifnot(nav$initial()=='yourteam',is.null(output$page_preference_status),length(requests)==2L,
   all(vapply(requests,function(x)x$timeout==8&&x$priority=='interactive',logical(1))))
 })
})
check('permanent failures stop and log only safe preference diagnostics',{
 reset(list(resource_failure(category='network')))
 failure<-record_logs(settle(fm_page_store('synthetic-account',owner='read-failure')))
 stopifnot(length(requests)==2L,inherits(failure,'error'),length(logs)==1L,
  grepl('resource=user_page_preferences category=network',logs,fixed=TRUE),
  !grepl('synthetic|fixture.invalid',logs))
 reset(list(resource_failure(category='http',http_status=401L)))
 failure<-record_logs(settle(fm_page_store('synthetic-account',owner='auth-failure')))
 stopifnot(length(requests)==1L,failure$http_status==401L,grepl('status=401',logs,fixed=TRUE))
})
check('invalid successful responses are diagnosed without exposing their contents or retrying acquisition',{
 for(body in c('{"private":"response-secret"}','[{"page":"today"},{"page":"plots"}]','response-secret','null','{}','[{}]','[{"page":null}]')){
  reset(list(response(body)))
  failure<-record_logs(settle(fm_page_store('synthetic-account',owner='parse-failure')))
  stopifnot(length(requests)==1L,failure$category=='parser',length(logs)==1L,
   grepl('resource=user_page_preferences category=parser',logs,fixed=TRUE),!grepl('response-secret',logs,fixed=TRUE))
 }
 reset(list(response('[]')));stopifnot(is.null(await(fm_page_store('new-account'))),length(requests)==1L)
})
check('preference writes remain single attempts and logout cancels pending read recovery',{
 reset(list(resource_failure(category='network')))
 failure<-record_logs(settle(fm_page_store('synthetic-account','plots','2026-11-14T01:19:00Z',owner='write-failure')))
 stopifnot(length(requests)==1L,requests[[1]]$method=='POST',inherits(failure,'error'))
 reset(list(resource_failure(category='network'),response('[{"page":"yourteam"}]')))
 pending<-fm_page_store('synthetic-account',owner='cancel-preference');flush(.03)
 fm_transport_cancel('cancel-preference')
 failure<-record_logs(settle(pending));flush(.3)
 stopifnot(inherits(failure,'fm_async_cancelled'),length(requests)==1L,!length(logs),
  !length(.fm_transport$reads),!length(.fm_transport$read_states))
})
cat(sprintf('PAGE PREFERENCE TRANSPORT: %d passed / 0 failed\n',checks))
