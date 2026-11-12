options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
for(expr in parse('worker/helper.R'))if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
  as.character(expr[[2]])%in%c('fm_worker_finance_timeout_code','fm_worker_headless_finances'))eval(expr,.GlobalEnv)
state<-list(seed_complete=TRUE,busy=2L,ownership_pending=1L,teams=data.frame(teamid=1:10),teams_status='ready',
 queue=list(list(kind='lineup',id='private-id'),list(kind='ownership',token='private-token'),list(kind='untrusted-secret')),
 reason='private error body')
expected<-'finance_timeout_s1_b2_q3_o1_t10_tsready_saved0_seed0_team0_lineup1_ranking0_notifications0_ownership1_ownership_catalog0'
stopifnot(identical(fm_worker_finance_timeout_code(state),expected))
stopifnot(!grepl('private|secret|untrusted',fm_worker_finance_timeout_code(state)))
cat('PASS timeout diagnostics expose only bounded counters and fixed labels, excluding IDs, credentials and private errors\n')
state$busy<-Inf;state$ownership_pending<--1;state$teams_status<-'private text'
stopifnot(grepl('_b0_q3_o0_t10_tsunknown_',fm_worker_finance_timeout_code(state),fixed=TRUE))
state$busy<-100000L
stopifnot(grepl('_b9999_',fm_worker_finance_timeout_code(state),fixed=TRUE))
cat('PASS malformed/oversized counters and unknown status text cannot escape the diagnostic contract\n')
local({
 env<-new.env(parent=.GlobalEnv)
 env$get_teams<-function(...)data.frame()
 env$get_mvp_saved_catalog<-function(...)list(saved=data.frame(),catalog=list(rounds=data.frame(),complete=TRUE))
 env$shared_rival_finance_source<-function(session,...){session$userData$rival_finance_shared<-state;function()list(loading=TRUE)}
 old_clock<-Sys.time;on.exit(assign('Sys.time',old_clock,.GlobalEnv))
 clock_calls<-0L
 assign('Sys.time',function(){clock_calls<<-clock_calls+1L;as.POSIXct(if(clock_calls==1L)0 else 601,origin='1970-01-01',tz='UTC')},.GlobalEnv)
 failure<-tryCatch(fm_worker_headless_finances(env,c(userid='fixture',token='synthetic'),
   list(championship_id='l',user_team_id='t')),fm_finance_timeout=function(e)e)
 stopifnot(inherits(failure,'fm_finance_timeout'),identical(failure$code,fm_worker_finance_timeout_code(state)))
})
cat('PASS the actual timeout branch attaches sanitized queue diagnostics without completing or inventing finance data\n')
