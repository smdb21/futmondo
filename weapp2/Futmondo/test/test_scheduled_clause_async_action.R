options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='u',token='synthetic');ctx<-resource_context(auth,'l','t','p',competition_id='c',season='S',scoring_version='v')
fm_clause_enabled<-function()TRUE;fm_clause_refresh<-function(...)invisible(NULL)
calls<-list();posts<-list();check_pending<-NULL;post_pending<-NULL;current<-TRUE;callback_count<-0L
native_async<-async_adapter_fetch
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
calls<-posts<-list()
bid_args<-c(args,list(isClause=FALSE))
bid_row<-data.frame(id='p',value=90,price=90,market_inMarket=TRUE)
p<-fm_clause_execute_async('buy_clause',bid_args,ctx,bid_row,function()TRUE,action_mode='bid')
value<-NULL;promises::then(p,function(x)value<<-x)
first_check<-check_pending$resolve
f<-facts();f$valid_until<-fm_clause_stamp(Sys.time()-1)
first_check(f);settle()
stopifnot(!identical(first_check,check_pending$resolve),is.null(value),!length(calls),!length(posts))
check_pending$resolve(f);settle()
stopifnot(!value$success,value$code=='acquisition_preflight_blocked',
 grepl('expired',value$message,fixed=TRUE),!length(calls),!length(posts))
cat('PASS a second expired verification stops before any reservation or send\n')

p<-fm_clause_execute_async('buy_clause',bid_args,ctx,bid_row,function()TRUE,action_mode='bid')
value<-NULL;promises::then(p,function(x)value<<-x)
f<-facts();f$funds$spendable_budget<-99
check_pending$resolve(f);settle()
stopifnot(!value$success,grepl('spendable funds',value$message,fixed=TRUE),!length(calls),!length(posts))
cat('PASS confirmed insufficient funds return the verified reason and send nothing\n')

# Reproduce the user-facing error with the real replay, parsers and capacity
# calculation. Only transport responses, durable acknowledgement and time are fake.
local({
 clock <- Sys.time()
 assign('Sys.time',function() clock,envir=.GlobalEnv)
 clear_api_cache()
 active <- list(championships=list(list(id='league',name='Fixture',league='comp',
   pointSystem='classic',userteam=list(id='mine'))),leagues=list(list(id='comp',season='2026',sport='soccer')))
 publish_active_league_contexts(auth,observed_data(active))
 context <- resource_context(auth,'league','mine','target')
 get_sb_url <<- function() ''
 get_sb_key <<- function() ''
 record_acquired_resource <<- function(...) invisible(TRUE)
 get_locker_clause_payments <<- function(...) observed_data(data.frame())
 routes <- character()
 resume_history <- NULL
 target <- list(id='target',name='Target',slug='target',role='defensa',value=1000,
   price=1100,change=0,computer=TRUE,clause=list(price=2000,transferred=FALSE,date='2026-01-01T00:00:00Z'))
 fm_async_read_http <<- function(url,method='POST',body=NULL,...) {
   route <- basename(url)
   routes <<- c(routes,route)
   answer <- switch(route,activechampionships=active,
     information=list(budget=3000,withheld=0,teamValue=2000,maxBid=4000,
       configuration=list(maxPlayersInRoster=20,mnmp=.5,mbmv=TRUE)),
     roster=list(list(id='owned',role='defensa',value=2000,change=0,points=5)),
     players=list(target),pressroom=list(news=list()),
     summary=list(data=target,championship=list(clause=target$clause),market=list(p=target$price,bids=list())),
     stop('Unexpected fixture source route'))
   response <- list(status_code=200L,
     content=charToRaw(jsonlite::toJSON(list(answer=answer),auto_unbox=TRUE)),times=list(total=0))
   if(route=='pressroom')return(promises::promise(function(resolve,reject)
     resume_history <<- function() resolve(response)))
   promises::promise_resolve(response)
 }
 async_adapter_fetch <<- native_async
 calls <<- list()
 posts <<- list()
 value <- NULL
 bid <- list(login=auth,championship_id='league',team_id='mine',player_id='target',
   player_slug='target',price=1200,isClause=FALSE)
 p <- fm_clause_execute_async('buy_clause',bid,context,
   data.frame(id='target',value=1000,price=1100,market_inMarket=TRUE),function()TRUE,action_mode='bid')
 promises::then(p,function(x)value<<-x)
 settle()
 stopifnot(is.function(resume_history),!length(calls),!length(posts))
 clock <- clock+20
 resume_history();settle()
 stopifnot(identical(routes,c('activechampionships','information','roster','pressroom','players','summary','information')),
   length(posts)==1L,posts[[1]]$url==BID_URL,posts[[1]]$body$query$price==1200,
   identical(posts[[1]]$body$query$isClause,FALSE),
   identical(posts[[1]]$body$answer,structure(list(),names=character())),
   identical(vapply(calls,`[[`,character(1),'name'),c('admit','dispatch')),
   calls[[1]]$payload$p_mode=='bid',is.null(value))
 finish()
 stopifnot(isTRUE(value$success),isTRUE(value$durable),length(posts)==1L)
 cat('PASS slow history refreshes only the expired funds read and sends one normal free-agent offer after admission\n')
})
