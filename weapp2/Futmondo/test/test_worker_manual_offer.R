options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
fm_worker_mode<-function(id)TRUE
record_acquired_resource<-function(...)invisible(TRUE)
fm_worker_rpc<-function(...)stop('GUI must not schedule acquisition')
auth<-c(userid='manual-worker-fixture',token='synthetic')
selected<-reactiveVal(data.frame(id='p',name='Fixture',role='defensa',team='Club',value=200,
 price=150,change=0,points=10,user_team_id='',market_inMarket=TRUE))
team<-reactiveVal('mine');jobs<-list();reads<-list();writes<-0L;sync<-0L
capacity<-function(valid=15)list(status='ok',valid_until=as.numeric(Sys.time())+valid,
 roster=list(status='ok',count=10,effective_count=10,cap=20),funds=list(spendable_budget=1000),
 outstanding=list(count=0),rules=list(minimum_bid_at_value=FALSE))
original_async<-async_adapter_fetch
async_adapter_fetch<-function(fn,args=list(),context,worker_bypass=FALSE,is_current=function()TRUE,...){
 reads[[length(reads)+1L]]<<-list(fn=fn,bypass=worker_bypass)
 if(fn=='get_team_acquisition_capacity'){
  if(worker_bypass)return(promises::promise(function(resolve,reject)jobs[[length(jobs)+1L]]<<-list(resolve=resolve,reject=reject,current=is_current)))
  return(promises::promise_resolve(observed_data(capacity(-30),status='stale')))
 }
 value<-switch(fn,get_player_summary=list(championship=list(),points=list(),prices=list()),
 get_lineup_from_team=list(pointSystem='fixture'),get_player_full_profile=list(profile=list()),
 get_player_matches=list(matches=list()),read_card_round_history=data.frame(),read_card_price_history=data.frame(),stop(fn))
 promises::promise_resolve(value)
}
buy_clause<-function(...){writes<<-writes+1L;TRUE}
settle<-function(session=NULL)for(i in 1:25){later::run_now(0);if(!is.null(session))session$flushReact()}
shiny::testServer(selected_player_Server,args=list(selected_player=selected,login_token=reactive(auth),
 championship_id=reactive('league'),user_team_id=team,capacity_fetcher=function(...){sync<<-sync+1L;capacity()}),{
 settle(session)
 stopifnot(!length(jobs),sum(vapply(reads,function(x)x$fn=='get_team_acquisition_capacity',logical(1)))==1L,
  all(!vapply(reads,`[[`,logical(1),'bypass')))
 cat('PASS passive card opening reads stale database capacity once without a live preflight\n')
 session$setInputs(btn_bid_market=1L);settle(session)
 stopifnot(length(jobs)==1L,market_offer_dialog_RV()$pending,market_offer_dialog_RV()$live_check,
  grepl('disabled',output$market_offer_submit$html,fixed=TRUE),sync==0L)
 session$setInputs(submit_bid=1L,bid_amount=150);settle(session);stopifnot(writes==0L,sync==0L)
 jobs[[1L]]$resolve(capacity());settle(session)
 stopifnot(!market_offer_dialog_RV()$pending,market_offer_dialog_RV()$preflight$ok,
  !grepl('disabled',output$market_offer_submit$html,fixed=TRUE),length(jobs)==1L)
 cat('PASS explicit offer opens one async live check; pending submission stays blocked and verified result enables confirmation\n')
 session$setInputs(btn_bid_market=2L);settle(session);stopifnot(length(jobs)==2L)
 team('another');settle(session);stopifnot(!jobs[[2L]]$current())
 jobs[[2L]]$resolve(capacity());settle(session)
 stopifnot(is.null(market_offer_dialog_RV()),is.null(modal_context_RV()$bid),writes==0L)
 cat('PASS a team change rejects the old live completion; no transaction or automatic retry occurs\n')
})
# The top-level live exception must propagate to every nested prerequisite.
async_adapter_fetch<-original_async
get_team_acquisition_capacity_async_original<-attr(get_team_acquisition_capacity_async,"worker_original") %||% get_team_acquisition_capacity_async
received<-NULL
get_team_acquisition_capacity_async<-function(...,worker_bypass=FALSE){received<<-worker_bypass;promises::promise_resolve(list(status='partial'))}
ctx<-resource_context(auth,'league','mine')
result<-NULL
promises::then(async_adapter_fetch('get_team_acquisition_capacity',list(login=auth,championship_id='league',user_team_id='mine'),ctx,worker_bypass=TRUE),function(x)result<<-x)
settle();stopifnot(isTRUE(received),result$status=='partial')
get_team_acquisition_capacity_async<-get_team_acquisition_capacity_async_original
nested<-list()
async_adapter_fetch<-function(fn,args=list(),context,worker_bypass=FALSE,...){
 nested[[length(nested)+1L]]<<-list(fn=fn,bypass=worker_bypass)
 promises::promise_reject(resource_failure(resource=fn,category='network'))
}
result<-NULL
promises::then(get_team_acquisition_capacity_async(auth,'league','mine',ctx,worker_bypass=TRUE),function(x)result<<-x)
settle();stopifnot(length(nested)==4L,all(vapply(nested,`[[`,logical(1),'bypass')),result$status=='unavailable')
cat('PASS explicit live bypass reaches all four prerequisite reads; failed prerequisites remain unavailable without repeating acquisition\n')
