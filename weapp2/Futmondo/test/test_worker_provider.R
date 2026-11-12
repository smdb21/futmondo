options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
fm_worker_rpc<-function(name,payload,owner=''){
 calls[[length(calls)+1L]]<<-list(name=name,payload=payload)
 promises::promise_resolve(TRUE)
}
calls<-list();auth<-c(userid='u',token='synthetic')
ctx<-resource_context(auth,'l');d<-fm_worker_descriptor('get_real_clubs',list(login=auth,championship_id='l'),ctx)
s<-fm_worker_account_state('u');s$known<-TRUE;s$account<-list(enabled=TRUE,auth_status='active')
value<-data.frame(teamId='club',team='Club',logo='logo');attr(value,'observed_at')<-Sys.time()-10;attr(value,'valid_until')<-as.numeric(Sys.time())+60
m<-list(version='v1',valid_until=rival_finance_stamp(Sys.time()+60),observed_at=rival_finance_stamp(attr(value,'observed_at')))
fm_worker_put('u',d$resource_key,list(data=value,manifest=m))
stopifnot(identical(get_real_clubs(auth,'l'),value),!length(calls),!length(.fm_transport$tasks))
cat('PASS opted-in synchronous browsing reuses a saved resource without HTTP\n')
settle<-function(p){out<-NULL;error<-NULL;promises::then(p,function(x)out<<-x,function(e)error<<-e);for(i in 1:20)later::run_now(.005);list(value=out,error=error)}
x<-settle(async_adapter_fetch('get_real_clubs',list(login=auth,championship_id='l'),ctx))
stopifnot(is.null(x$error),identical(x$value,value),!length(calls),!length(.fm_transport$tasks))
cat('PASS async adapter also reads the same snapshot without an API request\n')
attr(value,'valid_until')<-as.numeric(Sys.time())-1;m$valid_until<-rival_finance_stamp(Sys.time()-1)
fm_worker_put('u',d$resource_key,list(data=value,manifest=m))
a<-get_real_clubs(auth,'l');b<-get_real_clubs(auth,'l')
stopifnot(attr(a,'fetch_status')=='stale',attr(b,'fetch_status')=='stale',length(calls)==1L,calls[[1]]$name=='enqueue')
cat('PASS stale reads retain their data and coalesce a priority acquisition\n')
s$known<-FALSE;resolved<-FALSE;direct<-FALSE
p<-fm_worker_fetch('get_user_team_info',list(championship_id='l',user_team_id='t'),ctx,direct=function(){direct<<-TRUE;promises::promise_resolve(list(fixture=TRUE))})
stopifnot(!direct,length(s$waiters)==1L)
s$known<-TRUE;s$account<-list(enabled=FALSE);fm_worker_settle(s)
x<-settle(p);stopifnot(direct,isTRUE(x$value$fixture),!length(s$waiters))
cat('PASS unknown account policy waits; verified opt-out restores interactive acquisition\n')
s$account<-list(enabled=TRUE)
p<-fm_worker_fetch('get_player_summary',list(championship_id='l',user_team_id='t',player_id='p'),ctx,is_current=function()FALSE)
fm_worker_settle(s);x<-settle(p);stopifnot(inherits(x$error,'fm_async_cancelled'),!length(s$waiters))
cat('PASS abandoned generation cannot publish a late result\n')
# Source freshness and DB completeness survive the outer cache contract.
attr(value,'worker_completeness')<-'unknown'
stopifnot(resource_state(value)$completeness=='unknown')
cat('PASS source coverage is not promoted to complete by a successful DB read\n')
# A recovered job can retain the same payload version; its old error must clear.
attr(value,'valid_until')<-as.numeric(Sys.time())+60;m$valid_until<-rival_finance_stamp(Sys.time()+60)
fm_worker_put('u',d$resource_key,list(data=value,manifest=m))
s$errors[[d$resource_key]]<-'Background access is paused or this league is excluded.'
fm_worker_rpc<-function(name,payload,owner=''){
 if(name=='read')return(promises::promise_resolve(list(account=list(enabled=TRUE,auth_status='active'),health=NULL,resources=list(),jobs=list(list(key=d$resource_key,error=NULL,pending=FALSE,running=FALSE)))))
 promises::promise_resolve(TRUE)
}
fm_worker_poll('u',force=TRUE);for(i in 1:20)later::run_now(.005)
stopifnot(is.null(s$errors[[d$resource_key]]),!length(.fm_transport$tasks))
cat('PASS recovered job clears a stale paused/failure message without downloading its payload again\n')
cat('WORKER PROVIDER: 7 groups passed; source HTTP attempts=0\n')
