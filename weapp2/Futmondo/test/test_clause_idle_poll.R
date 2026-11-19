source('clause_scheduler.R')
s<-new.env(parent=emptyenv());s$known<-TRUE;s$error<-NULL;s$jobs<-list()
stopifnot(fm_clause_poll_interval(s)==60)
for(state in c('scheduled','preparing','accepted','uncertain')){
 s$jobs<-list(list(state=state));stopifnot(fm_clause_poll_interval(s)==5)
}
for(state in c('succeeded','cancelled','expired','failed')){
 s$jobs<-list(list(state=state));stopifnot(fm_clause_poll_interval(s)==60)
}
s$jobs<-list();s$error<-'failed';stopifnot(fm_clause_poll_interval(s)==5)
s$error<-NULL;s$known<-FALSE;stopifnot(fm_clause_poll_interval(s)==5)
# Verify the actual throttle and explicit action bypass without live HTTP.
fm_clause_enabled<-function()TRUE
.fm_clause$accounts[['fixture']]<-s;s$busy<-FALSE;s$known<-TRUE;s$last<-as.numeric(Sys.time())-10
calls<-0
fm_clause_rpc<-function(...){calls<<-calls+1;promises::promise_resolve(list(jobs=list(),health=NULL))}
fm_clause_refresh('fixture');stopifnot(calls==0)
fm_clause_refresh('fixture',force=TRUE);stopifnot(calls==1)
cat('PASS idle 60-second throttle, active/error fast polling, explicit-action bypass\n')
