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
value<-fm_worker_headless_finances(env,auth,ctx)
stopifnot(is.data.frame(value$team_finances),nrow(value$team_finances)==1L,!isTRUE(value$loading))
encoded<-fm_worker_encode(value)
cat('PASS actual headless finance controller completed; typed result bytes=',nchar(fm_worker_json(encoded)),'\n')

stopifnot(value$team_finances$estimated_cash[1]==90, sum(value$cash_ledgers$t$amount[value$cash_ledgers$t$included])==90)
cat('PASS headless ledger reconciles opening cash 100 minus a Market purchase of 10 to 90\n')
