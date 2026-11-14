# Opted-in screens read durable snapshots; neither time nor navigation owns jobs.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
now<-base::Sys.time();Sys.time<-function()now
auth<-c(userid='readonly-fixture',token='synthetic')
ctx<-resource_context(auth,'league','mine')
args<-list(championship_id='league',user_team_id='mine')
d<-fm_worker_descriptor('get_user_team_info',args,ctx)
s<-fm_worker_account_state(ctx$account_id)
s$known<-TRUE;s$account<-list(enabled=TRUE,auth_status='active',generation=1L)
value<-observed_data(list(budget=101),observed_at=now-600)
attr(value,'valid_until')<-as.numeric(now)-540
wire<-charToRaw(fm_worker_json(fm_worker_encode(value)));compressed<-history_baseline_compress(wire)
manifest<-list(account_id=ctx$account_id,resource_key=d$resource_key,context=d$context,handler=d$handler,
 version='durable-v1',json_bytes=length(wire),parts=1L,sha256=paste0(openssl::sha256(compressed)),
 observed_at=rival_finance_stamp(attr(value,'observed_at')),checked_at=rival_finance_stamp(attr(value,'observed_at')),
 valid_until=rival_finance_stamp(as.POSIXct(attr(value,'valid_until'),origin='1970-01-01')),status='ok',completeness='complete')
resource<-list(key=d$resource_key,manifest=manifest,chunks=list(jsonlite::base64_enc(compressed)))
calls<-list()
fm_worker_rpc<-function(name,payload,owner=''){
 calls[[length(calls)+1L]]<<-list(name=name,payload=payload)
 stopifnot(name=='read')
 stopifnot(length(payload$p_descriptors)<=128L,
   identical(names(payload$p_descriptors),names(payload$p_versions)),
   !any(vapply(payload$p_descriptors,function(x)'login'%in%names(x$args),logical(1))))
 promises::promise_resolve(list(account=s$account,health=list(heartbeat=rival_finance_stamp()),
  jobs=list(list(key=d$resource_key,pending=TRUE,running=FALSE,error=NULL)),
  resources=if(d$resource_key%in%names(payload$p_versions)&&!identical(payload$p_versions[[d$resource_key]],manifest$version))list(resource)else list()))
}
settle<-function(session=NULL)for(i in 1:30){later::run_now(0);if(!is.null(session))session$flushReact()}
for(cold in 1:2){
 s<-fm_worker_account_state(ctx$account_id);s$known<-TRUE;s$account<-list(enabled=TRUE,auth_status='active',generation=1L)
 clear_api_cache();if(length(ls(.fm_worker$values)))rm(list=ls(.fm_worker$values),envir=.fm_worker$values)
 shiny::testServer(function(input,output,session){
  s$sessions<-c(s$sessions,session$token)
  session$onSessionEnded(function()fm_worker_detach(ctx$account_id,session$token))
  refresh<-shiny::reactiveVal(0L);pulse<-shiny::reactiveVal(0L)
  read<-factual_read_adapter(session,'get_user_team_info',function()auth,function()'league',function()'mine',refresh_trigger=refresh,state_only=TRUE)
  state<-shiny::reactive({pulse();read(auth,'league','mine')})
  output$cash<-shiny::renderText(state()$data$budget)
 },{
  settle(session);stopifnot(length(s$waiters)==1L)
  fm_worker_poll(ctx$account_id,session$token,TRUE);settle(session)
  stopifnot(output$cash=='101',state()$status=='stale',state()$freshness=='stale',!length(s$waiters),
   identical(as.numeric(attr(state()$data,'observed_at')),as.numeric(fm_time(manifest$observed_at))))
  deadline<-attr(fm_worker_get(ctx$account_id,d$resource_key)$data,'valid_until')
  before<-length(calls)
  elapsed<-system.time(for(i in 1:12){
   now<<-now+31;fm_worker_poll(ctx$account_id,session$token,TRUE);settle(session);pulse(i);settle(session)
  })[['elapsed']]
  stopifnot(length(calls)==before+12L,all(vapply(calls,function(x)x$name=='read',logical(1))),
   output$cash=='101',state()$status=='stale',!length(.fm_transport$tasks),
   identical(attr(fm_worker_get(ctx$account_id,d$resource_key)$data,'valid_until'),deadline))
  cat(sprintf('PASS cold session %d: durable stale result displayed; 12 polls across six minutes issue only database reads (%.3fs), source time/deadline unchanged\n',cold,elapsed))
  # Refresh and internal cache expiry never mutate worker scheduling or source age.
  before<-length(calls);refresh(1L);settle(session)
  stopifnot(length(calls)==before+1L,calls[[length(calls)]]$name=='read',output$cash=='101',
   identical(attr(fm_worker_get(ctx$account_id,d$resource_key)$data,'valid_until'),deadline))
  before<-length(calls)
  for(i in 1:20)invalidate_resources(ctx,pattern='team_info|summary')
  settle(session)
  stopifnot(length(calls)==before+1L,all(vapply(calls,function(x)x$name=='read',logical(1))))
  cat('PASS Refresh and 20 cache invalidations coalesce to database reads without enqueuing or changing durable freshness\n')
 })
}
# Stored expired clause evidence should be shown truthfully, never wait120 seconds
# for another collection simply because a GUI asks to display it.
s<-fm_worker_account_state(ctx$account_id);s$known<-TRUE;s$account<-list(enabled=TRUE,auth_status='active',generation=1L)
clause_ctx<-resource_context(auth,'league','mine','player')
clause_args<-list(championship_id='league',user_team_id='mine',player_id='player')
clause_d<-fm_worker_descriptor('get_clause_transfer_summary',clause_args,clause_ctx)
clause<-observed_data(list(data=list(id='player')),observed_at=now-600)
attr(clause,'valid_until')<-as.numeric(now)-300
fm_worker_put(ctx$account_id,clause_d$resource_key,list(data=clause,manifest=list(version='saved-clause')))
result<-NULL;before<-length(calls)
promises::then(fm_worker_fetch('get_clause_transfer_summary',clause_args,clause_ctx),function(x)result<<-x)
settle()
stopifnot(result$data$id=='player',attr(result,'fetch_status')=='stale',!length(s$waiters),length(calls)==before)
cat('PASS expired clause snapshot returns immediately as stale without queue writes or waiting for acquisition\n')

# A fresh displayed result becomes stale once at source expiry, even if no
# newly acquired version arrives. Repeated overdue polls never request jobs.
fm_worker_rpc<-function(name,payload,owner=''){
 calls[[length(calls)+1L]]<<-list(name=name,payload=payload);stopifnot(name=='read')
 promises::promise_resolve(list(account=s$account,health=NULL,jobs=list(),resources=list()))
}
fresh<-observed_data(list(budget=202),observed_at=now)
attr(fresh,'valid_until')<-as.numeric(now)+60
fm_worker_put(ctx$account_id,d$resource_key,list(data=fresh,manifest=list(version='fresh')))
shiny::testServer(function(input,output,session){
 read<-factual_read_adapter(session,'get_user_team_info',function()auth,function()'league',function()'mine',state_only=TRUE)
 state<-shiny::reactive(read(auth,'league','mine'))
 metrics<-new.env(parent=emptyenv());metrics$changes<-0L
 output$cash<-shiny::renderText({metrics$changes<-metrics$changes+1L;state()$data$budget})
},{
 settle(session);stopifnot(state()$freshness=='fresh',output$cash=='202')
 before<-metrics$changes;stamp<-attr(state()$data,'observed_at');now<<-now+61
 fm_worker_poll(ctx$account_id,session$token,TRUE);settle(session)
 stopifnot(state()$freshness=='stale',metrics$changes==before+1L,identical(attr(state()$data,'observed_at'),stamp))
 for(i in 1:5){now<<-now+31;fm_worker_poll(ctx$account_id,session$token,TRUE);settle(session)}
 stopifnot(metrics$changes==before+1L,all(vapply(calls,function(x)x$name=='read',logical(1))))
 cat('PASS expiry publishes one truthful stale transition; five further overdue polls cause zero renders or acquisition requests\n')
})

# Database-only getters are not source acquisitions just because their name
# begins with get_. Execute the actual history getter against a DB fixture.
get_sb_url<-function()'https://database.invalid'
get_sb_key<-function()'synthetic-key'
database_calls<-0L
fm_async_read_http<-function(url,method='GET',body=NULL,headers=character(),owner='',priority='normal',...){
 stopifnot(method=='GET',grepl('/rest/v1/user_team_history',url,fixed=TRUE))
 database_calls<<-database_calls+1L
 if(as.numeric(httr::parse_url(url)$query$offset)>0)return(promises::promise_resolve(list(status_code=200L,content=charToRaw('[]'))))
 promises::promise_resolve(list(status_code=200L,content=charToRaw('[{"points":-2,"budget":12,"position":1,"team_value":30,"recorded_at":"2026-01-01T00:00:00Z","user_teams":{"name":"Fixture","championship_id":"league"}}]')))
}
result<-NULL;failure<-NULL;before<-length(calls)
promises::then(async_adapter_fetch('get_league_standings_history',list(championship_id='league'),ctx),
 function(x)result<<-x,function(e)failure<<-e)
settle()
stopifnot(is.null(failure),is.data.frame(result),nrow(result)==1L,result$points==-2,
 database_calls==2L,length(calls)==before,!length(s$waiters))
cat('PASS database-only standings getter reads the database directly, preserving negative points without a worker dependency\n')
