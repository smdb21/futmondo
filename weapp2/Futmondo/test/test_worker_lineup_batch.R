options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
for(expr in parse('worker/helper.R'))if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&as.character(expr[[2]])%in%c('fm_worker_finance_timeout_code','fm_worker_headless_finances'))eval(expr,.GlobalEnv)
auth<-c(userid='fixture',token='synthetic');ctx<-resource_context(auth,'l','t1')
env<-new.env(parent=.GlobalEnv)
for(n in ls(.GlobalEnv,all.names=TRUE)){f<-get(n,.GlobalEnv);if(is.function(f)&&!is.null(attr(f,'worker_original')))f<-attr(f,'worker_original');if(is.function(f)&&identical(environment(f),.GlobalEnv)){environment(f)<-env;assign(n,f,env)}}
env$shared_rival_finance_source<-.fm_worker_finance_original;environment(env$shared_rival_finance_source)<-env
teams<-data.frame(teamid=paste0('t',1:10),teamname=paste('Team',1:10),points=0,budget=100,teamValue=0)
rounds<-lapply(1:12,function(i)list(id=paste0('r',i),number=i,points=i,status='closed',negative=FALSE))
awards<-do.call(rbind,lapply(1:12,function(i)data.frame(round_id=paste0('r',i),round_number=i,player_id=paste0('p',1:11),is_mvp=c(TRUE,rep(FALSE,10)),is_finished=TRUE)))
base<-lapply(teams$teamid,function(id)rival_finance_evidence('fixture','l','unknown','team_rounds',id,list(team_id=id,rounds=rounds)))
env$get_teams<-function(...)teams
env$get_mvp_saved_catalog<-function(...)list(saved=awards,catalog=list(rounds=data.frame(),complete=TRUE))
persisted<-list();calls<-list();mismatches<-0L;builds<-0L
build_original<-env$build_rival_cash_dataset
env$build_rival_cash_dataset<-function(...){builds<<-builds+1L;build_original(...)}
env$persist_rival_finance_evidence<-function(rows,context){persisted<<-c(persisted,rows);TRUE}
env$persist_rival_calculation_snapshot<-function(cache,calculated,context,on_complete=NULL){if(is.function(on_complete))on_complete(TRUE);TRUE}
env$async_adapter_fetch<-function(fn,args,context,...){
 calls[[length(calls)+1L]]<<-fn
 stamp<-Sys.time();info<-list(budget=100,teamValue=0,points=0);attr(info,'observed_at')<-stamp
 movements<-normalize_money_movements(list());attr(movements,'observed_at')<-stamp
 result<-switch(fn,
 read_rival_calculation_snapshot=NULL,
 get_rival_finance_saved=list(evidence=base,complete=TRUE,defaults=NULL),
 get_rival_finance_seed=list(info=info,rules=list(initial_budget=100,money_per_point=0,ranking_pool=0,dreamteam_reward=5,mvp_reward=10),pressroom=data.frame()),
 get_rival_team_finance_observation=list(team_id=args$user_team_id,info=info,movements=movements,rounds=rounds),
 get_rival_round_lineup_observation={
 expected<-rival_round_signature(rival_team_round_rows(Filter(function(r)r$id==args$round_id,rounds),args$user_team_id),awards)
 if(!identical(args$signature,expected))mismatches<<-mismatches+1L
 list(team_id=args$user_team_id,round_id=args$round_id,signature=args$signature,answer=list(players=lapply(paste0('p',1:11),function(id)list(id=id)),bench=list(players=list()),negative=FALSE,status='closed'))},
 list(status='unsupported',rows=data.frame()))
 attr(result,'observed_at')<-stamp
 promises::promise_resolve(result)
}
timing<-system.time(v<-fm_worker_headless_finances(env,auth,ctx))
stopifnot(sum(unlist(calls)=='get_rival_round_lineup_observation')==120L,length(rival_evidence_rows(persisted,'round_lineup'))==120L,mismatches==0L,!v$loading,builds<15L)
first_lineup<-min(which(unlist(calls)=='get_rival_round_lineup_observation'));last_team<-max(which(unlist(calls)=='get_rival_team_finance_observation'))
stopifnot(last_team<first_lineup)
cat('PASS all team reads/point repairs precede the 120-lineup batch and final accounting drains with bounded rebuilds:',builds,'\n')
cat('elapsed:',timing[['elapsed']],'lineup_reads:',sum(unlist(calls)=='get_rival_round_lineup_observation'),'lineups_saved:',length(rival_evidence_rows(persisted,'round_lineup')),'signature_mismatches:',mismatches,'loading:',v$loading,'\n')

base<-rival_merge_evidence(base,persisted);calls<-list();persisted<-list();builds<-0L
v2<-fm_worker_headless_finances(env,auth,ctx)
stopifnot(!v2$loading,!any(unlist(calls)=='get_rival_round_lineup_observation'),
 identical(v$team_finances$estimated_cash,v2$team_finances$estimated_cash))
cat('PASS a new controller reuses every persisted matching lineup and preserves the final balances without 120 new acquisitions\n')
