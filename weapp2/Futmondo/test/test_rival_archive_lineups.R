options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
stamp<-Sys.time()-3600
ctx<-resource_context(account_id='fixture',championship_id='l',user_team_id='t',competition_id='c',season='s',scoring_version='v')
round<-list(id='r1',number=1,points=10,status='closed',negative=FALSE)
rounds<-rival_team_round_rows(list(round),'t')
awards<-data.frame(round_id='r1',round_number=1,player_id='p1',points=10,is_mvp=TRUE,is_finished=TRUE)
answer<-list(status='closed',negative=FALSE,players=list(list(id='p1')),bench=list(players=list()))
observation<-list(answer=answer,team_id='t',round_id='r1',signature=rival_round_signature(rounds,awards))
attr(observation,'observed_at')<-stamp
archive<-rival_lineup_evidence(observation,'fixture','l','unknown')
original<-archive
resolve<-function(candidates=archive,context=ctx,current=rounds,official=awards,existing=list())
 rival_resolve_archived_lineups(candidates,context,current,official,existing)
resolved<-resolve()
stopifnot(length(resolved)==1L,resolved[[1]]$competition_id=='c',resolved[[1]]$season=='s',
 identical(resolved[[1]]$observed_at,archive[[1]]$observed_at),identical(archive,original),
 identical(resolved[[1]]$payload$players,archive[[1]]$payload$players),
 resolved[[1]]$payload$context_resolution$original_season=='unknown')
cat('PASS exact current team-round/award signature resolves one archived source observation without inventing timestamps or changing the archive\n')
for(field in c('competition_id','season')){
 unknown<-ctx;unknown[[field]]<-'unknown';stopifnot(!length(resolve(context=unknown)))
 bad<-archive;bad[[1]][[field]]<-'foreign';stopifnot(!length(resolve(bad)))
}
for(field in c('account_id','championship_id','source_id')){
 bad<-archive;bad[[1]][[field]]<-'foreign';stopifnot(!length(resolve(bad)))
}
stopifnot(!length(resolve(current=transform(rounds,round_id='other'))),
 !length(resolve(current=transform(rounds,status='running'))),
 !length(resolve(current=rbind(rounds,rounds))))
cat('PASS unknown current identity, foreign/contradictory scope and absent/open/ambiguous source rounds cannot resolve an archive\n')
stopifnot(!length(resolve(official=transform(awards,points=11))),!length(resolve(existing=resolved)))
for(change in list(function(x){x$payload$signature<-'old';x},function(x){x$payload$negative<-NA;x},
 function(x){x$payload$players<-list();x},function(x){x$observed_at<-NULL;x},
 function(x){x$observed_at<-rival_finance_stamp(Sys.time()+3600);x})){
 stopifnot(!length(resolve(list(change(archive[[1]])))))
}
contradiction<-archive[[1]];contradiction$payload$players<-list('different-player')
stopifnot(!length(resolve(c(archive,list(contradiction)))))
contradiction$observed_at<-rival_finance_stamp(stamp+1)
stopifnot(identical(resolve(c(archive,list(contradiction)))[[1]]$payload$players,list('different-player')))
contradiction$payload$signature<-'newer-revision'
stopifnot(!length(resolve(c(archive,list(contradiction)))))
cat('PASS changed revisions, invalid observations, tied contradictory records and existing known evidence are never silently overwritten\n')

remember_league_context(list(account_id='fixture',championship_id='l',competition_id='c',season='s',scoring_version='v',
 observed_at=rival_finance_stamp(Sys.time()),expires_at=rival_finance_stamp(Sys.time()+3600),complete=TRUE,payload=list()))
e<-function(type,id,p)rival_finance_evidence('fixture','l','s',type,id,p,stamp,competition_id='c')
base<-list(e('team_rounds','t',list(team_id='t',rounds=list(round))),
 e('movements','t:ranking',list(team_id='t',id='ranking',type='ranking',money=1,concept='1',date=rival_finance_stamp(stamp))))
reads<-0L
read_rival_finance_evidence<-function(...)list(evidence=base,complete=TRUE)
read_completed_history_baseline<-function(...)NULL
get_rival_saved_transfers<-function(...)data.frame()
history_read_pages<-function(table,query,max_rows,page_size,...){
 reads<<-reads+1L
 stopifnot(table=='rival_finance_evidence_latest',query$account_id=='eq.fixture',query$championship_id=='eq.l',
 query$evidence_type=='eq.eligibility',query$source_id=='like.lineup:*',max_rows==600L,page_size==50L)
 rows<-jsonlite::fromJSON(as.character(jsonlite::toJSON(archive,auto_unbox=TRUE)))
 attr(rows,'complete')<-TRUE;rows
}
saved<-get_rival_finance_saved('fixture','l','s')
stopifnot(reads==1L,length(saved$archive_lineups)==1L,saved$archive_lookup_status=='ok',
 !length(rival_evidence_rows(saved$evidence,'round_lineup')))
base<-c(base,resolved)
stopifnot(!length(get_rival_finance_saved('fixture','l','s')$archive_lineups),reads==1L)
base<-base[seq_len(2)]
cat('PASS bounded account/league archive lookup keeps candidates separate and is skipped when current lineups are already stored\n')

for(expr in parse('worker/helper.R'))if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
 as.character(expr[[2]])%in%c('fm_worker_headless_finances','fm_worker_finance_timeout_code'))eval(expr,.GlobalEnv)
env<-new.env(parent=.GlobalEnv)
for(n in ls(.GlobalEnv,all.names=TRUE)){
 f<-get(n,.GlobalEnv);if(is.function(f)&&!is.null(attr(f,'worker_original')))f<-attr(f,'worker_original')
 if(is.function(f)&&identical(environment(f),.GlobalEnv)){environment(f)<-env;assign(n,f,env)}
}
env$shared_rival_finance_source<-.fm_worker_finance_original;environment(env$shared_rival_finance_source)<-env
env$get_teams<-function(...)data.frame(teamid='t',teamname='Fixture',points=10)
env$get_mvp_saved_catalog<-function(...)list(saved=awards,catalog=list(rounds=data.frame(),complete=TRUE))
persisted<-list();lineup_calls<-0L
env$persist_rival_finance_evidence<-function(rows,context){persisted<<-c(persisted,rows);TRUE}
env$persist_rival_calculation_snapshot<-function(cache,calculated,context,on_complete=NULL){if(is.function(on_complete))on_complete(TRUE);TRUE}
env$async_adapter_fetch<-function(fn,args,context,...){
 if(fn=='get_rival_round_lineup_observation'){lineup_calls<<-lineup_calls+1L;stop('Archived fixture should not need lineup acquisition')}
 value<-switch(fn,read_rival_calculation_snapshot=NULL,get_rival_finance_saved=saved,
 get_rival_finance_seed=list(info=NULL,rules=list(initial_budget=100,money_per_point=0,ranking_pool=0,dreamteam_reward=5,mvp_reward=10),pressroom=data.frame()),
 get_rival_team_finance_observation=list(team_id='t',info=NULL,movements=NULL,rounds=NULL),
 list(status='unsupported',rows=data.frame()))
 promises::promise_resolve(value)
}
value<-fm_worker_headless_finances(env,c(userid='fixture',token='synthetic'),ctx)
scoped<-rival_evidence_rows(persisted,'round_lineup')
stopifnot(!value$loading,lineup_calls==0L,length(scoped)==1L,scoped[[1]]$competition_id=='c',scoped[[1]]$season=='s',
 identical(scoped[[1]]$observed_at,archive[[1]]$observed_at),value$team_finances$estimated_cash==116,
 sum(value$cash_ledgers$t$amount[value$cash_ledgers$t$included])==116)
cat('PASS actual controller persists the verified current record without a source lineup request; ledger is 100 opening + 1 recorded + 5 Best XI + 10 MVP = 116\n')
