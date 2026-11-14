# The real factual adapter must publish a stored worker version once, without
# manufacturing an intervening stale/loading state or another acquisition.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
now<-base::Sys.time();Sys.time<-function()now
auth<-c(userid='replay-render-fixture',token='synthetic')
ctx<-resource_context(auth,'league','mine')
args<-list(championship_id='league',user_team_id='mine')
d<-fm_worker_descriptor('get_user_team_info',args,ctx)
s<-fm_worker_account_state(ctx$account_id);s$known<-TRUE;s$account<-list(enabled=TRUE,auth_status='active')
store<-function(budget,age=0,valid=60){
 value<-observed_data(list(budget=budget),observed_at=now-age)
 attr(value,'valid_until')<-as.numeric(now)+valid
 fm_worker_put(ctx$account_id,d$resource_key,list(data=value,manifest=list(version=paste0('v',budget))))
}
store(100)
calls<-list();fm_worker_rpc<-function(name,payload,owner=''){
 calls[[length(calls)+1L]]<<-list(name=name,payload=payload)
 promises::promise_resolve(TRUE)
}
settle<-function(session)for(i in 1:30){later::run_now(0);session$flushReact()}
stats<-new.env(parent=emptyenv());stats$publications<-list();stats$renders<-0L
shiny::testServer(function(input,output,session){
 refresh<-shiny::reactiveVal(0L);pulse<-shiny::reactiveVal(0L)
 read<-factual_read_adapter(session,'get_user_team_info',function()auth,function()'league',function()'mine',refresh_trigger=refresh,state_only=TRUE)
 state<-shiny::reactive({pulse();read(auth,'league','mine')})
 stats$publications<-list();stats$renders<-0L
 shiny::observe({current<-state();stats$publications[[length(stats$publications)+1L]]<-current})
 output$cash<-shiny::renderText({stats$renders<-stats$renders+1L;state()$data$budget})
},{
 settle(session);stopifnot(output$cash=='100',!length(calls))
 stats$publications<-list();stats$renders<-0L
 elapsed<-system.time(for(i in 1:5){
  now<<-now+1;store(100+i)
  rev<-fm_worker_resource_revision(s,d$resource_key);rev(shiny::isolate(rev())+1L)
  settle(session)
 })[['elapsed']]
 cat(sprintf('Measured five updates: %d publications, %d renders\n',length(stats$publications),stats$renders))
 stopifnot(output$cash=='105',length(stats$publications)==5L,stats$renders==5L,
   all(vapply(stats$publications,function(x)identical(x$status,'ok')&&identical(x$freshness,'fresh'),logical(1))),
   !length(calls),!length(.fm_transport$tasks),identical(attr(state()$data,'observed_at'),now))
 cat(sprintf('PASS five stored worker updates publish exactly five fresh states/renders, no transient loading or extra HTTP/queue work (%.3fs)\n',elapsed))
 stats$publications<-list();stats$renders<-0L
 for(i in 1:10){
  rev<-fm_worker_resource_revision(s,'unrelated');rev(shiny::isolate(rev())+1L)
  fm_worker_notify(ctx$account_id);settle(session)
 }
 stopifnot(!length(stats$publications),stats$renders==0L,!length(calls))
 cat('PASS ten unrelated worker publications cause zero primary rerenders or acquisitions\n')
 # Explicit user refresh still marks existing observations stale immediately.
 refresh(1L);session$flushReact()
 stopifnot(state()$status=='loading',state()$freshness=='stale')
 settle(session)
 stopifnot(length(calls)==1L,calls[[1L]]$name=='enqueue',calls[[1L]]$payload$p_priority==10L)
 cat('PASS manual refresh keeps its visible stale/loading state and priority acquisition\n')
 # An expired stored publication remains stale, with the original source time.
 now<<-now+2;store(106,age=120,valid=-60);stamp<-now-120
 s$jobs<-list(list(key=d$resource_key,running=TRUE,pending=TRUE))
 rev<-fm_worker_resource_revision(s,d$resource_key);rev(shiny::isolate(rev())+1L);settle(session)
 stopifnot(output$cash=='106',state()$status=='stale',state()$freshness=='stale',
   identical(attr(state()$data,'observed_at'),stamp),length(calls)==1L)
 cat('PASS expired durable data stays stale and preserves its original observation time without duplicate acquisition\n')
})
