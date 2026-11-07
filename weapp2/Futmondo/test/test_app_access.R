#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
Sys.setenv(admin=' Admin@Example.Invalid ')
auth<-c(userid='fixture-user',token='synthetic',user_name='user@example.invalid')
admin<-c(userid='fixture-admin',token='synthetic',user_name='admin@example.invalid')
n<-0L
check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
await<-function(p){done<-FALSE;value<-NULL;err<-NULL;promises::then(p,function(x){value<<-x;done<<-TRUE;NULL},function(e){err<<-e;done<<-TRUE;NULL});deadline<-Sys.time()+2;while(!done&&Sys.time()<deadline)later::run_now(.01);stopifnot(done);if(!is.null(err))stop(err);value}
settle<-function(session,predicate=function()TRUE,timeout=2){deadline<-Sys.time()+timeout;repeat{session$flushReact();later::run_now(.01);session$flushReact();if(isTRUE(shiny::isolate(predicate())))break;if(Sys.time()>deadline)stop('fixture timeout')}}
check('only an explicit open policy admits a non-admin; admins bypass storage',{
 calls<-0L;store<-function(...){calls<<-calls+1L;promises::promise_resolve(TRUE)}
 stopifnot(!await(fm_app_access(auth,store=store)),await(fm_app_access(admin,store=store)),calls==1L)
 for(value in list(NULL,NA,TRUE,'false',0,list(FALSE)))stopifnot(!await(fm_app_access(auth,store=function(...)promises::promise_resolve(value))))
 stopifnot(await(fm_app_access(auth,store=function(...)promises::promise_resolve(FALSE))))
 stopifnot(inherits(tryCatch(await(fm_app_access(auth,store=function(...)promises::promise_reject(simpleError('unavailable')))),error=identity),'error'))
})
original_store<-fm_access_store
blocked<-TRUE;reads<-0L;writes<-0L;fail<-FALSE;resolve_policy<-NULL;delayed<-FALSE
fm_access_store<-function(blocked=NULL,token=NULL,owner=''){
 if(!is.null(blocked)){writes<<-writes+1L;return(promises::promise_resolve(blocked))}
 reads<<-reads+1L
 if(fail)return(promises::promise_reject(simpleError('storage unavailable')))
 if(delayed)return(promises::promise(function(resolve,reject)resolve_policy<<-resolve))
 promises::promise_resolve(blocked_policy)
}
blocked_policy<-TRUE
old_login<-fm_async_login
fm_async_login<-function(...)promises::promise_resolve(auth)
check('password admission blocks before publishing the token; reopen allows login',{
 shiny::testServer(login_Server,{
  session$setInputs(user_name='fixture',password='synthetic',login_button=1)
  settle(session,function()!login_pending_RV());stopifnot(is.null(login_token_RV()),grepl('restricted',login_feedback_RV()$message),session$userData$startup_stage()=='signed_out')
  blocked_policy<<-FALSE
  session$setInputs(password='synthetic',login_button=2)
  settle(session,function()valid_login(login_token_RV()));stopifnot(identical(login_token_RV(),auth))
  session$setInputs(logout_button=1);settle(session);stopifnot(is.null(login_token_RV()))
 })
})
check('failed policy reads deny access with truthful feedback',{
 fail<-TRUE
 shiny::testServer(login_Server,{
  session$setInputs(user_name='fixture',password='synthetic',login_button=1)
  settle(session,function()!login_pending_RV());stopifnot(is.null(login_token_RV()),grepl('could not be verified',login_feedback_RV()$message))
 });fail<-FALSE
})
check('late policy completions cannot restore an account after logout',{
 delayed<-TRUE
 shiny::testServer(login_Server,{
  session$setInputs(user_name='fixture',password='synthetic',login_button=1)
  settle(session,function()!is.null(resolve_policy));stopifnot(is.null(login_token_RV()),login_pending_RV())
  heartbeat<-FALSE;later::later(function(){heartbeat<<-TRUE},.001);deadline<-Sys.time()+.2;while(!heartbeat&&Sys.time()<deadline)later::run_now(.01);stopifnot(heartbeat)
  session$setInputs(logout_button=1);settle(session);resolve_policy(FALSE);settle(session);stopifnot(is.null(login_token_RV()))
 });delayed<-FALSE;resolve_policy<-NULL
})
check('admin login works while storage is unavailable',{
 fail<-TRUE;before<-reads;fm_async_login<-function(...)promises::promise_resolve(admin)
 shiny::testServer(login_Server,{
  session$setInputs(user_name='admin',password='synthetic',login_button=1)
  settle(session,function()valid_login(login_token_RV()));stopifnot(identical(login_token_RV(),admin),reads==before)
 });fail<-FALSE;fm_async_login<-function(...)promises::promise_resolve(auth)
})
check('remembered restoration uses the same admission gate',{
 old_remember<-remember_login_session
 remember_login_session<-function(session,on_restore,on_status,on_begin,on_end,on_expire){
  session$userData$restore<-function(token){on_begin();promises::then(on_restore(token),function(x){on_end();x})}
  list(retry=function(...)NULL,result=function(...)NULL,logout=function(...)NULL,cancel=function(...)NULL)
 }
 blocked_policy<-TRUE
 shiny::testServer(login_Server,{
  session$userData$restore(auth);settle(session,function()!login_pending_RV());stopifnot(is.null(login_token_RV()),grepl('restricted',login_feedback_RV()$message))
  blocked_policy<<-FALSE
  session$userData$restore(auth);settle(session,function()valid_login(login_token_RV()));stopifnot(identical(login_token_RV(),auth))
 })
 remember_login_session<-old_remember
})
check('active non-admin session is removed on the next policy check',{
 blocked_policy<-FALSE
 shiny::testServer(login_Server,{
  session$setInputs(user_name='fixture',password='synthetic',login_button=1)
  settle(session,function()valid_login(login_token_RV()))
  blocked_policy<<-TRUE
  before<-reads
  session$elapse(119999);settle(session)
  stopifnot(valid_login(login_token_RV()),reads==before)
  session$elapse(2)
  settle(session,function()is.null(login_token_RV()));stopifnot(grepl('restricted',login_feedback_RV()$message))
 })
})
check('admin control saves only explicit changes and requires reload after failure',{
 saved<-TRUE;save_fail<-FALSE;calls<-0L
 store<-function(blocked=NULL,token=NULL,owner=''){
  if(is.null(blocked))return(promises::promise_resolve(saved))
  stopifnot(is_authorized_admin(token));calls<<-calls+1L
  if(save_fail)return(promises::promise_reject(simpleError('timeout')))
  saved<<-blocked;promises::promise_resolve(saved)
 }
 shiny::testServer(function(input,output,session){token<-reactiveVal(admin);active<-reactiveVal(TRUE);policy<-fm_admin_access(session,token,active,store)},{
  settle(session,function()policy()$loaded);stopifnot(policy()$blocked,calls==0L)
  session$setInputs(admin_only=TRUE);settle(session);stopifnot(calls==0L)
  session$setInputs(admin_only=FALSE);settle(session,function()!policy()$busy);stopifnot(!saved,!policy()$blocked,calls==1L)
  save_fail<<-TRUE;session$setInputs(admin_only=TRUE);settle(session,function()!policy()$busy)
  stopifnot(!policy()$loaded,grepl('could not be confirmed',policy()$message),!saved)
  save_fail<<-FALSE;session$setInputs(retry_access=1);settle(session,function()policy()$loaded);stopifnot(!policy()$blocked)
  token(auth);settle(session);session$setInputs(admin_only=FALSE);settle(session);stopifnot(calls==2L)
 })
})
fm_access_store<-original_store;fm_async_login<-old_login
check('REST filters, strict response validation, write acknowledgement and authorization',{
 Sys.setenv(supabase_project_url='https://database.invalid',supabase_secret_key='synthetic-server-key')
 original_http<-fm_async_http;response<-'[{"admin_only":false}]';requests<-list()
 fm_async_http<-function(url,method='GET',body=NULL,headers=character(),...){requests[[length(requests)+1L]]<<-list(url=url,method=method,body=body);promises::promise_resolve(list(content=charToRaw(response)))}
 stopifnot(identical(await(fm_access_store()),FALSE),grepl('id=eq.main',requests[[1]]$url,fixed=TRUE))
 for(body in c('[]','null','{}','[{"admin_only":"false"}]','[{"admin_only":null}]','[{"admin_only":false},{"admin_only":true}]')){
  response<-body;stopifnot(inherits(tryCatch(await(fm_access_store()),error=identity),'error'))
 }
 before<-length(requests);stopifnot(inherits(tryCatch(await(fm_access_store(FALSE,auth)),error=identity),'error'),length(requests)==before)
 response<-'[{"admin_only":false}]';stopifnot(identical(await(fm_access_store(FALSE,admin)),FALSE),tail(requests,1)[[1]]$method=='POST')
 stopifnot(inherits(tryCatch(await(fm_access_store(TRUE,admin)),error=identity),'error'))
 fm_async_http<-original_http
})
cat('APP ACCESS:',n,'passed\n')
