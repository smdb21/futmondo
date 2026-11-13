options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
clear_api_cache();auth<-c(userid='handoff-fixture',token='synthetic');args<-list(championship_id='league',user_team_id='mine')
unknown<-resource_context(auth,'league','mine');known<-unknown;known$competition_id<-'competition';known$season<-'season';known$scoring_version<-'score'
old<-fm_worker_descriptor('get_players_from_team',args,unknown);canonical<-fm_worker_descriptor('get_players_from_team',args,known)
s<-fm_worker_account_state(auth[['userid']]);s$known<-TRUE;s$account<-list(enabled=TRUE,auth_status='active',generation=1L)
old_value<-observed_data(data.frame(id='old'),observed_at=Sys.time()-600);attr(old_value,'valid_until')<-as.numeric(Sys.time())-540
fm_worker_put(auth[['userid']],old$resource_key,list(data=old_value,manifest=list(version='old')))
value<-observed_data(data.frame(id='current'),observed_at=Sys.time()-5)
wire<-charToRaw(fm_worker_json(fm_worker_encode(value)));compressed<-history_baseline_compress(wire)
manifest<-list(account_id=auth[['userid']],resource_key=canonical$resource_key,context=canonical$context,handler=canonical$handler,
 version='canonical-v1',json_bytes=length(wire),parts=1L,sha256=paste0(openssl::sha256(compressed)),observed_at=rival_finance_stamp(attr(value,'observed_at')),
 checked_at=rival_finance_stamp(attr(value,'observed_at')),valid_until=rival_finance_stamp(Sys.time()+55),status='ok',completeness='complete')
resource<-list(key=canonical$resource_key,manifest=manifest,chunks=list(jsonlite::base64_enc(compressed)))
calls<-list();fm_worker_rpc<-function(name,payload,owner=''){
 calls[[length(calls)+1L]]<<-list(name=name,payload=payload)
 if(name=='enqueue')return(promises::promise_resolve(TRUE))
 stopifnot(name=='read')
 promises::promise_resolve(list(account=s$account,health=NULL,
 redirects=if(old$resource_key%in%names(payload$p_versions))list(list(requested_key=old$resource_key,descriptor=canonical))else list(),
 resources=if(!identical(payload$p_versions[[canonical$resource_key]],'canonical-v1'))list(resource)else list(),
 jobs=list(list(key=canonical$resource_key,error=NULL,pending=FALSE,running=FALSE))))
}
flush<-function(session)for(i in 1:35){later::run_now(.001);session$flushReact()}
shiny::testServer(function(input,output,session){
 active<-shiny::reactiveVal(TRUE);league<-shiny::reactiveVal('league')
 read<-factual_read_adapter(session,'get_players_from_team',function()auth,league,function()'mine',is_active=active,state_only=TRUE)
 current<-shiny::reactive(read(auth,league(),'mine'))
 output$id<-shiny::renderText(current()$data$id)
},{
 flush(session);stopifnot(old$resource_key%in%names(s$descriptors))
 # A separate outstanding waiter proves obsolete scopes are cancelled, not fulfilled with foreign data.
 rm(list=fm_worker_key(auth[['userid']],old$resource_key),envir=.fm_worker$values)
 cancelled<-NULL;waiting<-TRUE;p<-fm_worker_fetch('get_players_from_team',args,unknown,is_current=function()waiting)
 promises::then(p,function(value)stop('Old scope must not receive canonical rows'),function(e){waiting<<-FALSE;cancelled<<-e})
 fm_worker_poll(auth[['userid']],session$token,TRUE);flush(session)
 stopifnot(output$id=='current',current()$context$season=='season',inherits(cancelled,'fm_async_cancelled'),!length(s$waiters),
   !old$resource_key%in%names(s$descriptors),canonical$resource_key%in%names(s$descriptors),is.null(fm_worker_get(auth[['userid']],old$resource_key)),
   identical(fm_worker_get(auth[['userid']],canonical$resource_key)$manifest$context,canonical$context))
 cat('PASS unresolved factual resource rebinds automatically to canonical context; obsolete waiters cancel and no rows are cached under the old key\n')
 before<-sum(vapply(calls,function(x)x$name=='enqueue',logical(1)))
 fm_worker_poll(auth[['userid']],session$token,TRUE);flush(session)
 stopifnot(output$id=='current',sum(vapply(calls,function(x)x$name=='enqueue',logical(1)))==before,!length(.fm_transport$tasks))
 cat('PASS a second poll reuses the canonical version without another acquisition or source HTTP\n')
 league('another-league');flush(session)
 stopifnot(!canonical$resource_key%in%fm_worker_live_keys(s))
 cat('PASS changing the selected league deactivates the old canonical resource and its subscriptions\n')
 active(FALSE);flush(session)
})
# Never accept a forged target or a conflicting known season.
fm_worker_demand(old,'fixture',function()TRUE)
bad<-canonical;bad$context$account_id<-'other'
stopifnot(!fm_worker_accept_redirect(s,list(requested_key=old$resource_key,descriptor=bad),auth[['userid']]))
bad<-canonical;bad$resource_key<-paste(rep('0',64),collapse='')
stopifnot(!fm_worker_accept_redirect(s,list(requested_key=old$resource_key,descriptor=bad),auth[['userid']]))
conflict<-unknown;conflict$season<-'previous';d<-fm_worker_descriptor('get_players_from_team',args,conflict);fm_worker_demand(d,'fixture',function()TRUE)
stopifnot(!fm_worker_accept_redirect(s,list(requested_key=d$resource_key,descriptor=canonical),auth[['userid']]))
cat('PASS account, known-season and descriptor-integrity mismatches cannot redirect\n')

stopifnot(!fm_worker_accept_redirect(s,list(),auth[['userid']]))
a<-list(context=known);b<-list(context=known)
stopifnot(incoming_clause_context_matches(a,b))
for(field in c('account_id','championship_id','competition_id','season','scoring_version')){
 b$context<-known;b$context[[field]]<-'different'
 stopifnot(!incoming_clause_context_matches(a,b))
}
cat('PASS mismatched transfer prerequisites cannot reconcile across account, league, competition, season or scoring contexts\n')
