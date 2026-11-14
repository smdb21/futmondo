# Unchanged worker polls must not invalidate rendered UI or other accounts.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
now<-base::Sys.time();Sys.time<-function()now
auth<-c(userid='status-fixture',token='synthetic')
other_auth<-c(userid='status-other',token='synthetic')
account<-auth[['userid']];other_account<-other_auth[['userid']]
s<-fm_worker_account_state(account);other<-fm_worker_account_state(other_account)
for(state in list(s,other)){
 state$known<-TRUE;state$account<-list(enabled=TRUE,auth_status='active',generation=1L)
 state$health<-list(heartbeat=rival_finance_stamp(now))
}
ctx<-resource_context(auth,'league','team')
d<-fm_worker_descriptor('get_user_team_info',list(championship_id='league',user_team_id='team'),ctx)
value<-observed_data(list(budget=100),observed_at=now);attr(value,'valid_until')<-as.numeric(now)+600
fm_worker_put(account,d$resource_key,list(data=value,manifest=list(version='v1',checked_at=rival_finance_stamp(now))))
fm_worker_demand(d,'status-test',function()TRUE)
s$jobs<-list(list(key=d$resource_key,error=NULL,pending=TRUE,running=TRUE))
rpc_calls<-0L;source_calls<-0L
fm_worker_rpc<-function(name,payload,owner=''){
 rpc_calls<<-rpc_calls+1L
 stopifnot(name=='read',identical(payload$p_account,account))
 promises::promise_resolve(list(account=s$account,health=list(heartbeat=rival_finance_stamp(now)),
  jobs=s$jobs,resources=list(),redirects=list()))
}
get_user_team_info<-function(...) {source_calls<<-source_calls+1L;stop('No source reads are allowed')}
flush<-function(session)for(i in 1:8){later::run_now(0);session$flushReact()}
shiny::testServer(function(input,output,session){
 login<-shiny::reactiveVal(auth);metrics<-new.env(parent=emptyenv());metrics$renders<-0L
 status<-fm_worker_status_source(session,login)
 output$status<-shiny::renderText({metrics$renders<-metrics$renders+1L;view<-status();paste(view$message,view$checked_at)})
},{
 flush(session);original<-output$status;before<-metrics$renders
 stopifnot(grepl('Preparing 1 requested resource',original,fixed=TRUE))
 started<-proc.time()[['elapsed']]
 for(i in 1:20){now<<-now+5;fm_worker_poll(account,session$token,TRUE);flush(session)}
 elapsed<-proc.time()[['elapsed']]-started
 stopifnot(metrics$renders==before,identical(output$status,original),rpc_calls==20L,source_calls==0L,!length(.fm_transport$tasks))
 cat(sprintf('PASS 20 successful worker polls/heartbeats: zero repeated output renders, zero source reads or enqueues (%.3fs)\n',elapsed))
 for(i in 1:20){other$health<-list(heartbeat=rival_finance_stamp(now+i));fm_worker_notify(other_account);flush(session)}
 stopifnot(metrics$renders==before,identical(output$status,original))
 cat('PASS other-account status changes cause zero output invalidations\n')
 s$jobs[[1]]$running<-FALSE;fm_worker_notify(account);flush(session)
 updated<-output$status;stopifnot(grepl('0 running',updated,fixed=TRUE),metrics$renders==before+1L)
 s$service_error<-'Background status could not be read from the database.';fm_worker_notify(account);flush(session)
 stopifnot(grepl('could not be read from the database',output$status,fixed=TRUE),metrics$renders==before+2L)
 s$service_error<-NULL;s$health$heartbeat<-rival_finance_stamp(now-61);fm_worker_notify(account);flush(session)
 stopifnot(grepl('not responding',output$status,fixed=TRUE),metrics$renders==before+3L)
 s$health$heartbeat<-rival_finance_stamp(now);fm_worker_notify(account);flush(session)
 stopifnot(!grepl('not responding',output$status,fixed=TRUE),metrics$renders==before+4L)
 cat('PASS queue, database failure, heartbeat expiry and recovery each publish their visible change once\n')
 login(other_auth);flush(session)
 stopifnot(!grepl('Preparing',output$status,fixed=TRUE))
 switched<-metrics$renders;fm_worker_notify(account);flush(session)
 stopifnot(metrics$renders==switched)
 login(NULL);flush(session);stopifnot(is.null(status()))
 cat('PASS account switch/logout drops old status and old-account subscriptions\n')
})
