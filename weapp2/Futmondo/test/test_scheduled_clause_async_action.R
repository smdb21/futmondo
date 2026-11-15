options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='u',token='synthetic');ctx<-resource_context(auth,'l','t','p',competition_id='c',season='S',scoring_version='v')
fm_clause_enabled<-function()TRUE;fm_clause_refresh<-function(...)invisible(NULL)
calls<-list();posts<-list();check_pending<-NULL;post_pending<-NULL;current<-TRUE;callback_count<-0L
async_adapter_fetch<-function(...)promises::promise(function(resolve,reject)check_pending<<-list(resolve=resolve,reject=reject))
fm_clause_rpc<-function(name,payload,owner=''){
 calls[[length(calls)+1L]]<<-list(name=name,payload=payload)
 promises::promise_resolve(if(name=='admit')list(id=payload$p_id)else TRUE)
}
fm_async_http<-function(url,method,body,headers,owner='',...){
 posts[[length(posts)+1L]]<<-list(url=url,method=method,body=jsonlite::fromJSON(body,simplifyVector=FALSE),owner=owner)
 promises::promise(function(resolve,reject)post_pending<<-list(resolve=resolve,reject=reject))
}
facts<-function()list(context=fm_clause_scope(ctx),status='ok',player=list(id='p',slug='verified-slug',owner_id='rival',owner_verified=TRUE,
 price=90,transferred=FALSE,opens_at=fm_clause_stamp(Sys.time()-60)),
 roster=list(status='ok',count=9,effective_count=9,cap=12),outstanding=list(count=0),
 funds=list(spendable_budget=1000,api_bid_limit=1000),rules=list(minimum_bid_at_value=FALSE),offers=list(),valid_until=fm_clause_stamp(Sys.time()+14))
args<-list(login=auth,championship_id='l',team_id='t',player_id='p',player_slug='stale',price=100)
settle<-function()for(i in 1:30)later::run_now(0)
finish<-function(){post_pending$resolve(list(status_code=200L,content=charToRaw('{"answer":{"code":"api.general.ok"}}')));settle()}
p<-fm_clause_execute_async('buy_roster_clause',args,ctx,data.frame(id='p'),function()current)
fm_clause_complete(p,function(value){stopifnot(value$success);callback_count<<-callback_count+1L})
beats<-0L;later::later(function()beats<<-beats+1L,0);settle()
stopifnot(beats==1L,!length(posts),!length(calls))
check_pending$resolve(facts());settle()
stopifnot(length(posts)==1L,identical(vapply(calls,`[[`,character(1),'name'),c('admit','dispatch')),callback_count==0L,
 posts[[1]]$url==ROSTER_CLAUSE_URL,posts[[1]]$body$query$price==90,posts[[1]]$body$query$player_slug=='verified-slug',
 identical(posts[[1]]$body$answer,structure(list(),names=character())),posts[[1]]$owner=='')
later::later(function()beats<<-beats+1L,0);settle();stopifnot(beats==2L,callback_count==0L)
finish();stopifnot(callback_count==1L,tail(calls,1)[[1]]$payload$p_state=='accepted',length(posts)==1L)
cat('PASS held preflight and single-send HTTP both yield control; native payload and durable intent/settlement are preserved\n')
calls<-posts<-list();current<-TRUE
p<-fm_clause_execute_async('buy_roster_clause',args,ctx,data.frame(id='p'),function()current)
value<-NULL;promises::then(p,function(x)value<<-x);current<-FALSE;check_pending$resolve(facts());settle()
stopifnot(!length(posts),!length(calls),!value$success)
cat('PASS a context change before dispatch sends nothing\n')
current<-TRUE;calls<-posts<-list()
p<-fm_clause_execute_async('buy_roster_clause',args,ctx,data.frame(id='p'),function()current)
value<-NULL;promises::then(p,function(x)value<<-x);check_pending$resolve(facts());settle()
post_pending$reject(resource_failure(resource='source',category='network'));settle()
stopifnot(length(posts)==1L,!value$success,value$code=='purchase_response_uncertain',tail(calls,1)[[1]]$payload$p_state=='uncertain')
cat('PASS loss of the native response retains uncertainty and never repeats a write\n')
calls<-posts<-list()
ma<-list(login=auth,championship_id='l',team_id='t',player_id='p',bid_id='own-bid',new_price=70)
p<-fm_clause_execute_async('modify_bid',ma,ctx,data.frame(id='p',price=50),function()TRUE,action_mode='modify')
f<-facts();f$offers<-list(list(player_id='p',bid_id='own-bid',price=50));f$outstanding$count<-1L
check_pending$resolve(f);settle()
stopifnot(length(posts)==1L,posts[[1]]$url==MODIFY_BID_URL,posts[[1]]$body$query$bid=='own-bid',posts[[1]]$body$query$price==70)
finish();stopifnot(tail(calls,1)[[1]]$payload$p_state=='accepted')
cat('PASS verified existing bid identity, delta preflight and native modify-bid payload are reused asynchronously\n')
