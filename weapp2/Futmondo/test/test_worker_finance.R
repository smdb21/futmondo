options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
for(expr in parse('worker/helper.R'))if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&identical(expr[[2]],as.name('fm_worker_headless_finances')))eval(expr,.GlobalEnv)
auth<-c(userid='u',token='synthetic');ctx<-resource_context(auth,'l','t')
env<-new.env(parent=.GlobalEnv)
for(n in ls(.GlobalEnv,all.names=TRUE)){
 f<-get(n,.GlobalEnv);if(is.function(f)&&!is.null(attr(f,'worker_original')))f<-attr(f,'worker_original')
 if(is.function(f)&&identical(environment(f),.GlobalEnv)){environment(f)<-env;assign(n,f,env)}
}
env$shared_rival_finance_source<-.fm_worker_finance_original;environment(env$shared_rival_finance_source)<-env
teams<-data.frame(teamid='t',teamname='Team',points=0,budget=90,teamValue=10)
env$get_teams<-function(...)teams
env$get_mvp_saved_catalog<-function(...)list(saved=data.frame(),catalog=list(rounds=data.frame(),complete=TRUE))
env$persist_rival_finance_evidence<-function(...)TRUE
env$persist_rival_calculation_snapshot<-function(cache,calculated,context,on_complete=NULL){if(is.function(on_complete))on_complete(TRUE);TRUE}
env$async_adapter_fetch<-function(fn,args,context,...){
 stamp<-Sys.time();info<-list(budget=90,teamValue=10,points=0);attr(info,'observed_at')<-stamp
 movements<-normalize_money_movements(list());attr(movements,'observed_at')<-stamp
 result<-switch(fn,
  read_rival_calculation_snapshot=NULL,
  get_rival_finance_saved=list(evidence=list(),complete=TRUE,defaults=NULL),
  get_rival_finance_seed=list(info=info,rules=list(initial_budget=100,money_per_point=0,ranking_pool=0),pressroom=data.frame(id='tx-1',created=rival_finance_stamp(Sys.time()-120),player_id='p',player_name='Player',buyer_team_id='t',buyer_team_name='Team',seller_team_id='',seller_team_name='',price=10,is_clause=FALSE,player_team_name='',player_team_slug='')),
  get_rival_team_finance_observation=list(team_id='t',info=info,movements=movements,rounds=structure(list(),valid_response=TRUE)),
  list(status='unsupported',rows=data.frame()))
 promises::promise_resolve(result)
}
updates<-list()
value<-fm_worker_headless_finances(env,auth,ctx,on_update=function(value)updates[[length(updates)+1L]]<<-value)
stopifnot(is.data.frame(value$team_finances),nrow(value$team_finances)==1L,!isTRUE(value$loading))
encoded<-fm_worker_encode(value)
cat('PASS actual headless finance controller completed; typed result bytes=',nchar(fm_worker_json(encoded)),'\n')

stopifnot(value$team_finances$estimated_cash[1]==90, sum(value$cash_ledgers$t$amount[value$cash_ledgers$t$included])==90)
cat('PASS headless ledger reconciles opening cash 100 minus a Market purchase of 10 to 90\n')

stopifnot(length(updates)>0L,any(vapply(updates,function(x)isTRUE(x$loading)&&is.data.frame(x$team_finances)&&'estimated_cash'%in%names(x$team_finances)&&any(is.finite(x$team_finances$estimated_cash)),logical(1))))
cat('PASS the existing accounting controller emits usable labeled calculations before the final history scan completes\n')
# Headless award reuse is independently checked without changing the calculator.
saved_awards<-data.frame(championship_id='l',round_id='r1',player_id=paste0('p',1:11),is_mvp=c(TRUE,rep(FALSE,10)),is_finished=TRUE,persistence='Saved',checked_at=rival_finance_stamp(Sys.time()-60))
recheck<-character();requests<-0L;writes<-0L
rounds<-data.frame(round_id='r1',round_number=1,is_finished=TRUE)
env$get_mvp_saved_catalog<-function(...)list(saved=saved_awards,catalog=list(rounds=rounds,complete=TRUE),settlement_rechecks=recheck)
env$get_official_dreamteam_result<-function(login,championship_id,reference){
 stopifnot(isTRUE(reference$is_finished));requests<<-requests+1L;list(status='ok',rows=saved_awards)
}
env$persist_official_award_snapshot<-function(...){writes<<-writes+1L;TRUE}
env$shared_rival_finance_source<-function(session,...){
 session$userData$rival_finance_shared<-list(seed_complete=TRUE,busy=0L,queue=list(),ownership_pending=0L)
 shiny::reactive(list(loading=FALSE))
}
invisible(fm_worker_headless_finances(env,auth,ctx));stopifnot(requests==0L,writes==0L)
recheck<-'r1';invisible(fm_worker_headless_finances(env,auth,ctx));stopifnot(requests==1L,writes==1L)
recheck<-character();saved_awards$is_finished<-FALSE
invisible(fm_worker_headless_finances(env,auth,ctx));stopifnot(requests==2L)
saved_awards$is_finished<-TRUE;saved_awards$checked_at<-rival_finance_stamp(Sys.time()-86401)
invisible(fm_worker_headless_finances(env,auth,ctx));stopifnot(requests==3L)
saved_awards$checked_at<-rival_finance_stamp(Sys.time()-60);saved_awards$is_finished<-NULL
invisible(fm_worker_headless_finances(env,auth,ctx));stopifnot(requests==4L)
saved_awards$is_finished<-TRUE;saved_awards$championship_id<-'other'
invisible(fm_worker_headless_finances(env,auth,ctx));stopifnot(requests==5L)
cat('PASS persisted final awards avoid refetch; settlement, daily, provisional, missing-finality and foreign-scope data still require acquisition\n')

# Exercise the actual helper progress envelope independently of source latency.
for(expr in parse('worker/helper.R'))if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&as.character(expr[[2]])%in%c('fm_worker_run','fm_worker_discover'))eval(expr,.GlobalEnv)
fm_worker_headless_finances<-function(env,auth,ctx,on_update=NULL){
 value<-list(team_finances=data.frame(teamid='t',estimated_cash=90,confidence='Reconciled'),
   cash_ledgers=list(t=data.frame(amount=c(100,-10),included=TRUE,running_balance=c(100,90))),
   loading=TRUE,finance_loading=TRUE,finance_pending=1L)
 on_update(value);on_update(value)
 value$loading<-FALSE;value$finance_loading<-FALSE;value$finance_pending<-0L
 on_update(value);value
}
messages<-list();fm_pipe_exchange<-function(message){messages[[length(messages)+1L]]<<-message;list(ok=TRUE)}
descriptor<-fm_worker_descriptor('fm_worker_finances',list(championship_id='l',user_team_id='t'),ctx)
finished<-fm_worker_run(descriptor)
stopifnot(length(messages)==2L,all(vapply(messages,function(x)x$type=='progress'&&x$result$status=='partial'&&x$result$completeness=='partial',logical(1))),
 isTRUE(messages[[2]]$result$ready),isTRUE(fm_worker_decode(messages[[1]]$result$value)$loading),!fm_worker_decode(finished$value)$loading)
stopifnot(sum(fm_worker_decode(messages[[1]]$result$value)$cash_ledgers$t$amount)==90)
cat('PASS actual helper sends throttled partial/ready envelopes with the same known-answer ledger; final result remains separate\n')
