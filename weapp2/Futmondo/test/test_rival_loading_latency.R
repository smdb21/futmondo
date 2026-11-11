options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
persist_rival_finance_evidence<-function(...)TRUE
persist_rival_calculation_snapshot<-function(...)TRUE
teams<-data.frame(teamid=c('a','b'),teamname=c('A','B'),points=c(1,2))
rules<-list(initial_budget=100,money_per_point=0,ranking_pool=0)
# Verify the real composite publishes configuration before either slow history read.
get_user_team_info<-function(...)list(configuration=list(budget=100))
progress_seen<-FALSE
publish_resource_progress<-function(value){stopifnot(value$rules$initial_budget==100);progress_seen<<-TRUE}
get_championship_pressroom<-function(...){stopifnot(progress_seen);data.frame()}
get_notifications<-function(...)NULL
invisible(get_rival_finance_seed(c(userid='u',token='fixture'),'l','a'))
rm(publish_resource_progress)

for(snapshot_first in c(TRUE,FALSE)){
 clear_api_cache();pending<-list();calls<-character();progress<-NULL;builds<-0L
 original_build<-build_rival_cash_dataset
 build_rival_cash_dataset<-function(...){builds<<-builds+1L;original_build(...)}
 async_adapter_fetch<-function(fn,args,context,on_progress=NULL,...){
   key<-paste(fn,args$user_team_id %||% '',sep=':');calls<<-c(calls,key)
   if(fn=='get_rival_finance_seed')progress<<-on_progress
   promises::promise(function(resolve,reject)pending[[key]]<<-list(resolve=resolve,reject=reject))
 }
 flush<-function(session){for(i in seq_len(12L)){later::run_now(0);session$flushReact()}}
 shiny::testServer(function(input,output,session){
   auth<-shiny::reactiveVal(c(userid='u',token='fixture'))
   feed<-shared_rival_finance_source(session,auth,function()'l',function()'a',function()teams,function()TRUE,function()list(rows=data.frame(),complete=FALSE))
 },{
   flush(session)
   snapshot<-list(scope=rival_snapshot_scope(resource_context(account_id='u',championship_id='l',season='unknown',user_team_id='a')),
     signature='fixture',team_finances=rival_finance_directory(teams))
   snapshot$team_finances$estimated_cash<-c(73,-5)
   saved<-function(){pending[['get_rival_finance_saved:']]$resolve(list(evidence=list(),complete=TRUE));flush(session)}
   preview<-function(){pending[['read_rival_calculation_snapshot:a']]$resolve(snapshot);flush(session)}
   if(snapshot_first){preview();saved()}else{saved();preview()}
   stopifnot(identical(feed()$team_finances$estimated_cash,c(73,-5)),feed()$calculation_snapshot_checking)
   before<-builds
   # Configuration can release team reads before the seed's histories complete.
   progress(list(info=list(configuration=list(budget=100)),rules=rules));flush(session)
   stopifnot('get_rival_team_finance_observation:a'%in%calls,
     !'get_rival_team_finance_observation:b'%in%calls,identical(feed()$team_finances$estimated_cash,c(73,-5)))
   pending[['get_rival_team_finance_observation:a']]$resolve(list(team_id='a'));flush(session)
   stopifnot('get_rival_team_finance_observation:b'%in%calls)
   pending[['get_rival_finance_seed:a']]$resolve(list(info=NULL,rules=rules,pressroom=data.frame()));flush(session)
   stopifnot(identical(feed()$team_finances$estimated_cash,c(73,-5)),builds==before)
   heartbeat<-FALSE;later::later(function(){heartbeat<<-TRUE},.005)
   started<-Sys.time()
   while(!heartbeat&&as.numeric(Sys.time()-started)<1)later::run_now(.001)
   elapsed<-as.numeric(Sys.time()-started)
   stopifnot(heartbeat,elapsed<.5,feed()$loading)
   pending[['get_rival_team_finance_observation:b']]$resolve(list(team_id='b'));flush(session)
   stopifnot(!feed()$loading,!feed()$calculation_snapshot_checking,
     identical(feed()$team_finances$estimated_cash,c(100,100)),builds==before+1L,
     sum(calls=='get_rival_team_finance_observation:a')==1L,sum(calls=='get_rival_team_finance_observation:b')==1L)
   # Old-context previews cannot leak into another account.
   auth(c(userid='other',token='fixture'));flush(session)
   stopifnot(!any(feed()$team_finances$estimated_cash%in%c(73,-5)))
   cat(sprintf('PASS snapshot_first=%s: snapshot retained, team reads overlap seed, one rebuild; held-I/O heartbeat %.1f ms\n',snapshot_first,elapsed*1000))
 })
 build_rival_cash_dataset<-original_build
}
