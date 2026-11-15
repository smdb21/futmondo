# Actual worker session controller; no source, database or token storage I/O.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
fm_worker_encrypt<-function(...){x<-'synthetic';attr(x,'credential_id')<-'fixture';x}
auth<-c(userid='renewal-fixture',token='synthetic',user_name='fixture@example.invalid')
s<-fm_worker_account_state(auth[['userid']])
account<-list(enabled=TRUE,generation=1L,auth_status='active',timezone='Europe/Madrid',quiet_start=0,quiet_end=7,excluded_leagues=list())
s$known<-TRUE;s$account<-account
pending<-NULL;rpc<-character()
fm_worker_rpc<-function(name,payload,owner=''){
 rpc<<-c(rpc,name)
 if(name=='renew')return(promises::promise(function(resolve,reject)pending<<-resolve))
 if(name=='authorize'){account$enabled<<-isTRUE(payload$p_enabled);account$generation<<-2L;return(promises::promise_resolve(account))}
 stopifnot(name=='read');promises::promise_resolve(list(account=account,resources=list(),redirects=list(),jobs=list(),health=NULL))
}
settle<-function(session)for(i in 1:25){later::run_now(0);session$flushReact()}
shiny::testServer(function(input,output,session){
 token<-reactiveVal(auth);fm_worker_session(session,token)
 output$heartbeat<-renderText(input$pulse %||% 0L)
},{
 settle(session);stopifnot(is.function(pending),sum(rpc=='renew')==1L)
 session$setInputs(pulse=1);settle(session);stopifnot(output$heartbeat=='1')
 # An explicit disable can now be acknowledged while renewal is held. The
 # existing policy-generation guard must reject that renewal's older reply.
 session$setInputs(worker_enable=FALSE,worker_timezone='Europe/Madrid',worker_quiet_start=0,worker_quiet_end=7,worker_save=1)
 settle(session);stopifnot(!s$account$enabled,s$account$generation==2L,sum(rpc=='authorize')==1L)
 old<-account;old$enabled<-TRUE;old$generation<-1L;pending(old);settle(session)
 stopifnot(!s$account$enabled,s$account$generation==2L,sum(rpc=='renew')==1L,!length(.fm_transport$tasks))
 cat('PASS held renewal yields to input; disabling during renewal wins over a late older policy reply; no duplicate renewal or live I/O\n')
})
