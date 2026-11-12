options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
auth<-c(userid='live-fixture',token='synthetic')
ctx<-resource_context(auth,'l','t')
args<-list(championship_id='l',user_team_id='t')
d<-fm_worker_descriptor('get_user_team_info',args,ctx)
s<-fm_worker_account_state(auth[['userid']]);s$known<-TRUE;s$account<-list(enabled=TRUE,auth_status='active',generation=1L)
initial<-list(budget=100,withheld=0,teamValue=100,configuration=list(mnmp=.25))
attr(initial,'observed_at')<-Sys.time();attr(initial,'valid_until')<-as.numeric(Sys.time())+60
fm_worker_put(auth[['userid']],d$resource_key,list(data=initial,manifest=list(version='v1')))
wire<-charToRaw(fm_worker_json(fm_worker_encode(modifyList(initial,list(budget=200)))))
compressed<-history_baseline_compress(wire)
manifest<-list(account_id=auth[['userid']],resource_key=d$resource_key,context=d$context,handler=d$handler,
 version='v2',json_bytes=length(wire),parts=1L,sha256=paste0(openssl::sha256(compressed)),
 observed_at=rival_finance_stamp(),checked_at=rival_finance_stamp(),valid_until=rival_finance_stamp(Sys.time()+60),status='ok',completeness='unknown')
resource<-list(key=d$resource_key,manifest=manifest,chunks=list(jsonlite::base64_enc(compressed)))
rpc<-list()
fm_worker_rpc<-function(name,payload,owner=''){
 rpc[[length(rpc)+1L]]<<-list(name=name,payload=payload)
 stopifnot(name=='read')
 promises::promise_resolve(list(account=s$account,health=NULL,jobs=list(list(key=d$resource_key,error=NULL,pending=FALSE,running=FALSE)),
  resources=if(d$resource_key%in%names(payload$p_versions)&&!identical(payload$p_versions[[d$resource_key]],'v2'))list(resource)else list()))
}
flush<-function(session){for(i in 1:40){later::run_now(.001);session$flushReact()}}
shiny::testServer(function(input,output,session){
 active<-shiny::reactiveVal(TRUE)
 read<-factual_read_adapter(session,'get_user_team_info',function()auth,function()'l',function()'t',is_active=active,state_only=TRUE)
 state<-shiny::reactive(read(auth,'l','t'))
 output$cash<-shiny::renderText(state()$data$budget)
},{
 s$sessions<-c(session$token,'other-session');session$flushReact();flush(session)
 stopifnot(output$cash=='100',d$resource_key%in%names(s$descriptors))
 item<-s$descriptors[[d$resource_key]];item$at<-as.numeric(Sys.time())-300
 for(id in names(item$watchers))item$watchers[[id]]$at<-item$at
 s$descriptors[[d$resource_key]]<-item
 stopifnot(d$resource_key%in%fm_worker_live_keys(s))
 s$errors[['unwanted-old-context']]<-'old source failure'
 fm_worker_poll(auth[['userid']],session$token,TRUE);flush(session)
 stopifnot(output$cash=='200',length(rpc)==1L,rpc[[1]]$name=='read',!length(.fm_transport$tasks),is.null(s$errors[['unwanted-old-context']]))
 cat('PASS a visible source older than two minutes receives and renders a new durable version without refresh, enqueue or source HTTP\n')
 sibling<-function()FALSE;attr(sibling,'fm_worker_interest')<-'inactive-sibling'
 fm_worker_demand(d,session$token,sibling)
 stopifnot(d$resource_key%in%fm_worker_live_keys(s))
 active(FALSE);session$flushReact();stopifnot(!d$resource_key%in%fm_worker_live_keys(s))
 cat('PASS separate module interests share the resource; an inactive sibling cannot silence the active view, and inactive views stop polling\n')
 other<-function()TRUE;attr(other,'fm_worker_interest')<-'other-browser'
 fm_worker_demand(d,'other-session',other);fm_worker_detach(auth[['userid']],session$token)
 stopifnot(length(s$descriptors[[d$resource_key]]$watchers)==1L,d$resource_key%in%fm_worker_live_keys(s))
 fm_worker_detach(auth[['userid']],'other-session')
 stopifnot(is.null(s$descriptors[[d$resource_key]]),!exists(d$resource_key,s$resource_revisions,inherits=FALSE))
 cat('PASS closing one browser preserves another subscription; final detach releases callbacks and revision state\n')
})
s<-fm_worker_account_state('legacy-fixture')
old<-d;old$context$account_id<-'legacy-fixture'
fm_worker_demand(old,'legacy');s$descriptors[[old$resource_key]]$at<-as.numeric(Sys.time())-300
for(id in names(s$descriptors[[old$resource_key]]$watchers))s$descriptors[[old$resource_key]]$watchers[[id]]$at<-as.numeric(Sys.time())-300
stopifnot(!length(fm_worker_live_keys(s)))
for(i in 1:140)fm_worker_demand(old,paste0('owner-',i),function()FALSE)
stopifnot(length(s$descriptors[[old$resource_key]]$watchers)<=128L)
cat('PASS legacy interest still expires and subscriber storage remains bounded\n')
