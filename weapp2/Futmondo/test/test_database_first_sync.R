# Synthetic startup through the real session, resource, preference and codec paths.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
production_configured<-fm_worker_configured;production_can_connect<-fm_worker_can_connect
fm_worker_configured<-function()TRUE
fm_worker_can_connect<-function()TRUE
fm_worker_encrypt<-function(...){x<-'synthetic';attr(x,'credential_id')<-'fixture';x}
auth<-c(userid='dbfirst-fixture',token='synthetic',user_name='fixture@example.invalid')
account<-list(enabled=FALSE,generation=1L,auth_status='active',timezone='UTC',quiet_start=0,quiet_end=0,excluded_leagues=list())
ctx<-resource_context(auth)
leagues<-list(championships=list(list(id='league',name='Fixture',league='competition',pointSystem=1,userteam=list(id='mine',name='Mine'))),leagues=list(list(`_id`='competition',season='season')))
metadata<-normalize_active_league_contexts(leagues,ctx$account_id)[[1L]]
team_ctx<-resource_context(auth,'league','mine',competition_id=metadata$competition_id,season=metadata$season,scoring_version=metadata$scoring_version)
resources<-list();calls<-character();source_calls<-0L;hold_connection<-NULL;hold<-TRUE
snapshot<-function(handler,args,context,value,version){
 d<-fm_worker_descriptor(handler,args,context);value<-observed_data(value,observed_at=Sys.time()-600)
 wire<-charToRaw(fm_worker_json(fm_worker_encode(value)));compressed<-history_baseline_compress(wire)
 m<-list(account_id=ctx$account_id,resource_key=d$resource_key,context=d$context,handler=handler,version=version,
  json_bytes=length(wire),parts=1L,sha256=paste0(openssl::sha256(compressed)),observed_at=rival_finance_stamp(Sys.time()-600),
  checked_at=rival_finance_stamp(Sys.time()-600),valid_until=rival_finance_stamp(Sys.time()-540),status='ok',completeness='complete')
 resources[[d$resource_key]]<<-list(key=d$resource_key,manifest=m,chunks=list(jsonlite::base64_enc(compressed)));invisible(d$resource_key)
}
league_key<-snapshot('get_active_championships',list(),ctx,leagues,'leagues-v1')
args<-list(championship_id='league',user_team_id='mine')
team_key<-snapshot('get_user_team_info',args,team_ctx,list(budget=125),'team-v1')
snapshot('get_market_players',args,team_ctx,data.frame(id='player',value=10),'market-v1')
snapshot('get_players_from_team',args,team_ctx,data.frame(id='owned'),'roster-v1')
fm_worker_rpc<-function(name,payload,owner=''){
 calls<<-c(calls,name)
 if(name=='connect')return(promises::promise(function(resolve,reject){hold_connection<<-list(resolve=resolve,reject=reject)}))
 stopifnot(name%in%c('read','bootstrap'))
 keys<-if(name=='bootstrap')league_key else names(payload$p_versions)
 changed<-Filter(function(x)x$key%in%keys&&(name=='bootstrap'||!identical(payload$p_versions[[x$key]],x$manifest$version)),resources)
 promises::promise_resolve(list(account=account,health=list(heartbeat=rival_finance_stamp()),jobs=list(),redirects=list(),resources=unname(changed),preferred_league=list(championship_id='league')))
}
futmondo_post<-function(...){source_calls<<-source_calls+1L;stop('Unexpected browsing source request')}
settle<-function(session,n=20L)for(i in seq_len(n)){later::run_now(.005);session$flushReact()}
shiny::testServer(function(input,output,session){
 login<-reactiveVal(auth);fm_worker_session(session,login)
 read_leagues<-factual_read_adapter(session,'get_active_championships',login,state_only=TRUE)
 league_state<-reactive(read_leagues(login()))
 preferences<-fm_league_preferences(session,login,function()league_state()$data$championships)
 league<-reactive(preferences$selected())
 refresh<-reactiveVal(0L)
 read_team<-factual_read_adapter(session,'get_user_team_info',login,league,function()'mine',state_only=TRUE,refresh_trigger=refresh)
 team<-reactive({req(league());read_team(login(),league(),'mine')})
 metrics<-new.env(parent=emptyenv());metrics$paints<-0L
 display<-resource_display_source(session,function()team()$data$budget)
 output$cash<-renderText({metrics$paints<-metrics$paints+1L;display()})
 output$heartbeat<-renderText(input$pulse %||% 0L)
},{
 started<-proc.time()[['elapsed']];settle(session)
 stopifnot(output$cash=='125',is.function(hold_connection$resolve),sum(calls=='connect')==1L,
  sum(calls=='bootstrap')==1L,sum(calls=='read')==1L,source_calls==0L)
 cat(sprintf('PASS cold startup serves saved balance in two DB reads while connection acknowledgement is held (%.3fs)\n',proc.time()[['elapsed']]-started))
 session$setInputs(pulse=7);settle(session);stopifnot(output$heartbeat=='7')
 before<-metrics$paints
 refresh(1L);settle(session);fm_worker_poll(ctx$account_id,session$token,TRUE);settle(session)
 stopifnot(output$cash=='125',metrics$paints==before,source_calls==0L)
 account$enabled<<-TRUE;hold_connection$resolve(account);settle(session)
 snapshot('get_user_team_info',args,team_ctx,list(budget=150),'team-v2')
 fm_worker_poll(ctx$account_id,session$token,TRUE);settle(session)
 stopifnot(output$cash=='150',metrics$paints==before+1L,source_calls==0L)
 cat('PASS unchanged Refresh keeps values mounted; a changed balance paints once; delayed connection does not block input\n')
 login(NULL);settle(session)
 stopifnot(!'disconnect'%in%calls)
 cat('PASS ordinary logout detaches display interests without stopping unattended synchronization\n')
})
# Disabled/misconfigured collection never selects live display acquisition.
Sys.setenv(FUTMONDO_WORKER_ENABLED='false',FUTMONDO_WORKER_KEY='')
options(futmondo.offline=FALSE)
stopifnot(production_configured(),!production_can_connect(),isTRUE(fm_worker_mode('missing-worker')))
options(futmondo.offline=TRUE)
# Opening/reopening/refreshing availability only registers database interests.
s<-fm_worker_account_state(auth[['userid']]);s$known<-TRUE;s$account<-account;s$bootstrap_complete<-TRUE
shiny::testServer(function(input,output,session){
 context<-function(){x<-team_ctx;x$player_id<-'uncollected';x$session_id<-session$token;x}
 refresh<-reactiveVal(0L)
 state<-player_availability_source(session,'summary',context,function()auth,refresh=refresh,mode=function()TRUE,
  stored=function(){d<-fm_worker_descriptor('get_player_summary',c(args,list(player_id='uncollected')),context());fm_worker_resource_revision(s,d$resource_key)();NULL})
},{
 settle(session);stopifnot(state()$status=='unverified',source_calls==0L)
 refresh(1L);settle(session);stopifnot(source_calls==0L)
 cat('PASS missing card observations and card Refresh never fall back to Futmondo\n')
})

# A bad/failed DB response must settle startup promptly, without live fallback.
fm_worker_rpc<-function(...)promises::promise_resolve(list(unexpected=TRUE))
failure<-NULL;display_failure<-NULL
p<-fm_worker_preference('failed-read','fixture-owner')
promises::then(p,onRejected=function(e){failure<<-e;NULL})
p<-fm_worker_fetch('get_user_team_info',list(championship_id='league',user_team_id='mine'),
 resource_context(account_id='failed-read',championship_id='league',user_team_id='mine'),owner='fixture-owner')
promises::then(p,onRejected=function(e){display_failure<<-e;NULL})
for(i in 1:20)later::run_now(.005)
failed<-fm_worker_account_state('failed-read')
stopifnot(inherits(failure,'error'),inherits(display_failure,'error'),!failed$busy,
 !failed$bootstrap_complete,!length(failed$waiters),!length(failed$bootstrap_waiters),source_calls==0L)

# Closing a session cancels its held preference wait instead of leaking it.
held<-NULL;cancelled<-NULL
fm_worker_rpc<-function(...)promises::promise(function(resolve,reject)held<<-resolve)
p<-fm_worker_preference('closed-bootstrap','closing-owner')
promises::then(p,onRejected=function(e){cancelled<<-e;NULL})
for(i in 1:10)later::run_now(.005)
fm_worker_detach('closed-bootstrap','closing-owner')
for(i in 1:10)later::run_now(.005)
stopifnot(inherits(cancelled,'fm_async_cancelled'))
held(list(account=NULL,resources=list(),jobs=list()));for(i in 1:10)later::run_now(.005)
stopifnot(is.null(.fm_worker$accounts[['closed-bootstrap']]),source_calls==0L)
cat('PASS malformed database reads settle startup errors; closing a held bootstrap cancels its waiter and ignores late data\n')
