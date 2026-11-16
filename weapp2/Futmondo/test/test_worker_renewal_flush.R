# Actual login controller, synthetic credentials and held database acknowledgements.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
fm_worker_can_connect<-function()TRUE
fm_worker_encrypt<-function(...){x<-'synthetic';attr(x,'credential_id')<-'fixture';x}
auth<-c(userid='renewal-fixture',token='synthetic',user_name='fixture@example.invalid')
policy<-function(id='renewal-fixture',generation=1L)list(account_id=id,enabled=TRUE,generation=generation,
 auth_status='active',timezone='Europe/Madrid',quiet_start=0,quiet_end=0,excluded_leagues=list())
seed<-function(id='renewal-fixture',generation=1L){
 s<-fm_worker_account_state(id);s$bootstrap_complete<-TRUE;fm_worker_set_policy(s,policy(id,generation));s
}
s<-seed();pending<-list();rpc<-list()
fm_worker_rpc<-function(name,payload,owner=''){
 rpc[[length(rpc)+1L]]<<-list(name=name,payload=payload)
 if(name=='connect')return(promises::promise(function(resolve,reject){
  pending[[length(pending)+1L]]<<-list(resolve=resolve,reject=reject)
 }))
 if(name=='disconnect'){
  a<-policy(payload$p_account,payload$p_generation+1L);a$enabled<-FALSE
  return(promises::promise_resolve(a))
 }
 stopifnot(name=='read')
 promises::promise_resolve(list(account=fm_worker_account_state(payload$p_account)$account,
  resources=list(),redirects=list(),jobs=list(),health=NULL))
}
settle<-function(session)for(i in 1:25){later::run_now(0);session$flushReact()}
count<-function(name)sum(vapply(rpc,function(x)x$name==name,logical(1)))
shiny::testServer(function(input,output,session){
 token<-reactiveVal(auth);fm_worker_session(session,token)
 session$userData$disconnect_login<-function()token(NULL)
 output$heartbeat<-renderText(input$pulse %||% 0L)
},{
 settle(session);stopifnot(length(pending)==1L,count('connect')==1L,count('read')>=1L)
 session$setInputs(pulse=1);settle(session);stopifnot(output$heartbeat=='1')
 # Revocation and signout win even when the earlier connect response arrives late.
 session$setInputs(worker_disconnect=1);settle(session)
 stopifnot(is.null(token()),count('disconnect')==1L,!s$account$enabled,s$account$generation==2L)
 pending[[1]]$resolve(policy());settle(session)
 stopifnot(is.null(token()),!s$account$enabled,count('connect')==1L)
 # A genuinely new login reconnects using the revoked generation, not generation 0.
 s<-seed(generation=2L);s$account$enabled<-FALSE;token(auth);settle(session)
 requests<-Filter(function(x)x$name=='connect',rpc)
 stopifnot(length(requests)==2L,requests[[2]]$payload$p_generation==2L)
 # Direct account switches also reset enrollment; an old callback cannot update it.
 next_auth<-auth;next_auth[['userid']]<-'second-fixture';next_auth[['token']]<-'second-token'
 next_state<-seed('second-fixture',4L);token(next_auth);settle(session)
 stopifnot(count('connect')==3L)
 pending[[2]]$resolve(policy(generation=2L));settle(session)
 stopifnot(identical(token(),next_auth),identical(next_state$account$generation,4L))
 pending[[3]]$resolve(policy('second-fixture',4L));settle(session)
 fm_worker_notify('second-fixture');settle(session);stopifnot(count('connect')==3L)
 # Logging out does not revoke background access or issue another connect request.
 token(NULL);settle(session);stopifnot(count('disconnect')==1L,count('connect')==3L)
 cat('PASS automatic single enrollment; held acknowledgement stays responsive; disconnect beats late replies; account switch isolation; logout retains sync\n')
})
# Crypto/configuration errors are caught; database browsing and input remain live.
fm_worker_encrypt<-function(...)stop('synthetic encryption failure')
s<-seed();before<-count('connect')
shiny::testServer(function(input,output,session){
 token<-reactiveVal(auth);fm_worker_session(session,token)
 output$heartbeat<-renderText(input$pulse %||% 0L)
},{
 settle(session);session$setInputs(pulse=2);settle(session)
 stopifnot(output$heartbeat=='2',count('connect')==before,!is.null(s$connect_error),!s$busy)
 cat('PASS synchronous enrollment failure leaves database reads and input responsive, with retryable status\n')
})
stopifnot(!length(.fm_transport$tasks))
