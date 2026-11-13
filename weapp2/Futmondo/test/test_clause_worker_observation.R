options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
clear_api_cache();auth<-c(userid='passive-clause',token='synthetic')
ctx<-resource_context(auth,'league','mine','player')
d<-fm_worker_descriptor('get_clause_transfer_summary',list(championship_id='league',user_team_id='mine',player_id='player'),ctx)
s<-fm_worker_account_state(ctx$account_id);s$known<-TRUE;s$account<-list(enabled=TRUE,auth_status='active')
active<-TRUE;live<-function()active;attr(live,'fm_worker_interest')<-'clause-display'
fm_worker_demand(d,'fixture',live)
value<-observed_data(list(data=list(id='player'),championship=list(owner=list(id='seller'),clause=list(transferred=TRUE,transferDate='2026-11-20T10:00:00Z'))),observed_at=Sys.time()-310)
attr(value,'valid_until')<-as.numeric(Sys.time())-10
fm_worker_put(ctx$account_id,d$resource_key,list(data=value,manifest=list(version='old')))
calls<-character()
fm_worker_rpc<-function(name,payload,owner=''){
 calls<<-c(calls,name)
 promises::promise_resolve(if(name=='read')list(account=s$account,health=list(),jobs=list(),resources=list())else TRUE)
}
fm_worker_poll(ctx$account_id,force=TRUE);for(i in 1:20)later::run_now(0)
stopifnot(identical(calls,'read'),d$resource_key%in%fm_worker_live_keys(s))
cat('PASS passive clause expiry reads worker status without enqueueing another acquisition\n')
stamp<-Sys.time()-5;attr(value,'observed_at')<-stamp;attr(value,'valid_until')<-as.numeric(stamp)+300
fm_worker_put(ctx$account_id,d$resource_key,list(data=value,manifest=list(version='new')))
persist_clause_transfer_summary<-function(...)TRUE
restored<-incoming_clause_cached_summary(auth,'league','mine','player',live,'fixture')
stopifnot(identical(restored,value),identical(calls,'read'),attr(restored,'observed_at')==stamp)
cat('PASS the GUI consumes independently published clause details without a source request or timestamp renewal\n')
active<-FALSE;stopifnot(!d$resource_key%in%fm_worker_live_keys(s))
cat('PASS leaving the page releases its passive interest without changing any background job schedule\n')
