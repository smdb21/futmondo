options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
for(expr in parse('worker/helper.R'))if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&identical(expr[[2]],as.name('fm_worker_headless_finances')))eval(expr,.GlobalEnv)
auth<-c(userid='optional-fixture',token='synthetic');ctx<-resource_context(auth,'l','t')
env<-new.env(parent=.GlobalEnv)
for(n in ls(.GlobalEnv,all.names=TRUE)){
 f<-get(n,.GlobalEnv);if(is.function(f)&&!is.null(attr(f,'worker_original')))f<-attr(f,'worker_original')
 if(is.function(f)&&identical(environment(f),.GlobalEnv)){environment(f)<-env;assign(n,f,env)}
}
env$shared_rival_finance_source<-.fm_worker_finance_original;environment(env$shared_rival_finance_source)<-env
teams<-data.frame(teamid=c('t','r'),teamname=c('Team','Rival'),points=0,teamValue=10)
info<-observed_data(list(budget=90,teamValue=10,points=0))
pressroom<-observed_data(data.frame(id='tx',created=rival_finance_stamp(Sys.time()-120),player_id='p',player_name='Player',
 buyer_team_id='t',buyer_team_name='Team',seller_team_id='r',seller_team_name='Rival',price=10,is_clause=FALSE,player_team_name='',player_team_slug=''))
env$get_teams<-function(...)teams
env$get_mvp_saved_catalog<-function(...)list(saved=data.frame(),catalog=list(rounds=data.frame(),complete=TRUE))
env$persist_rival_finance_evidence<-function(...)TRUE
env$persist_rival_calculation_snapshot<-function(cache,calculated,context,on_complete=NULL){if(is.function(on_complete))on_complete(TRUE);TRUE}
env$get_user_team_info<-function(login,championship_id,user_team_id){
 if(user_team_id=='t')return(info)
 stop(structure(list(message='Worker operation failed',call=NULL,code='source_rejected'),class=c('fm_worker_bridge_error','error','condition')))
}
env$get_user_team_moneymovements<-function(...)NULL
env$get_user_team_rounds<-function(...)NULL
rejected<-env$get_rival_team_finance_observation(auth,'l','r')
stopifnot(is.null(rejected$info),is.null(rejected$movements),!length(env$rival_team_evidence(rejected,ctx$account_id,'l','unknown')))
cat('PASS actual R optional getter retains unknown private fields and creates no fabricated team evidence\n')
env$async_adapter_fetch<-function(fn,args,context,...){
 result<-switch(fn,read_rival_calculation_snapshot=NULL,
  get_rival_finance_saved=list(evidence=list(),complete=TRUE,defaults=NULL),
  get_rival_finance_seed=list(info=info,rules=list(initial_budget=100,money_per_point=0,ranking_pool=0),pressroom=pressroom),
  get_rival_team_finance_observation=do.call(env$get_rival_team_finance_observation,c(list(login=auth),args[setdiff(names(args),'login')])),
  list(status='unsupported',rows=data.frame()))
 promises::promise_resolve(result)
}
value<-fm_worker_headless_finances(env,auth,ctx)
stopifnot(!isTRUE(value$loading),value$team_finances$estimated_cash[value$team_finances$teamid=='t']==90,
 value$team_finances$estimated_cash[value$team_finances$teamid=='r']==110,
 sum(value$cash_ledgers$r$amount[value$cash_ledgers$r$included])==110,
 is.na(value$team_finances$reconciliation_difference[value$team_finances$teamid=='r']))
cat('PASS actual accounting controller completes observed ledgers 90/110 while the unavailable rival balance remains unreconciled\n')
