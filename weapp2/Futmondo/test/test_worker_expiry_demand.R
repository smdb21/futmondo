options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
now<-base::Sys.time();Sys.time<-function()now
auth<-c(userid='expiry-fixture',token='synthetic');ctx<-resource_context(auth,'league','mine')
d<-fm_worker_descriptor('get_user_team_info',list(championship_id='league',user_team_id='mine'),ctx)
s<-fm_worker_account_state(ctx$account_id);s$known<-TRUE;s$account<-list(enabled=TRUE,auth_status='active')
value<-observed_data(list(budget=100),observed_at=now);attr(value,'valid_until')<-as.numeric(now)+60
fm_worker_put(ctx$account_id,d$resource_key,list(data=value,manifest=list(version='v1')))
calls<-list();invalidations<-0L;original_invalidate<-fm_worker_invalidate_pattern
fm_worker_invalidate_pattern<-function(...){invalidations<<-invalidations+1L;original_invalidate(...)}
fm_worker_rpc<-function(name,payload,owner=''){
 calls[[length(calls)+1L]]<<-list(name=name,payload=payload)
 stopifnot(name=='read')
 promises::promise_resolve(list(account=s$account,health=NULL,jobs=list(),resources=list()))
}
settle<-function(session)for(i in 1:30){later::run_now(0);session$flushReact()}
shiny::testServer(function(input,output,session){
 pulse<-shiny::reactiveVal(0L);refresh<-shiny::reactiveVal(0L)
 read<-factual_read_adapter(session,'get_user_team_info',function()auth,function()'league',function()'mine',refresh_trigger=refresh,state_only=TRUE)
 state<-shiny::reactive({pulse();read(auth,'league','mine')})
 output$cash<-shiny::renderText(state()$data$budget)
},{
 settle(session);stopifnot(output$cash=='100',!length(calls))
 s$jobs<-list(list(key=d$resource_key,running=TRUE,pending=TRUE))
 now<<-now+61;pulse(1L);settle(session)
 stopifnot(state()$freshness=='stale',invalidations==0L,!length(calls))
 cat('PASS automatic source expiry only marks stored data stale without queue changes\n')
 refresh(1L);settle(session)
 stopifnot(invalidations==1L,length(calls)==1L,calls[[1]]$name=='read')
 cat('PASS explicit Refresh rereads the database without scheduling acquisition\n')
 value$budget<-200;attr(value,'observed_at')<-now;attr(value,'valid_until')<-as.numeric(now)+60
 fm_worker_put(ctx$account_id,d$resource_key,list(data=value,manifest=list(version='v2')))
 rev<-fm_worker_resource_revision(s,d$resource_key);rev(shiny::isolate(rev())+1L);settle(session)
 stopifnot(output$cash=='200',state()$freshness=='fresh',length(calls)==1L,!length(.fm_transport$tasks))
 cat('PASS the next durable version restores fresh display without another invalidation or source HTTP\n')
})
stopifnot(fm_worker_interval('get_player_recent_points')==300L,
 fm_worker_priority('get_players_from_team')>fm_worker_priority('get_player_recent_points'),
 fm_worker_priority('get_championship_pressroom')>fm_worker_priority('fm_worker_finances'))
cat('PASS historical points use the summary cadence; clause prerequisites outrank history/finance demand\n')
