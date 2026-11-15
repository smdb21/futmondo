options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='u',token='synthetic')
ctx<-resource_context(auth,'l','t','p',competition_id='c',season='S',scoring_version='v')
now<-Sys.time()
summary<-observed_data(list(data=list(`_id`='p',slug='fixture',name='Fixture'),
 championship=list(owner=list(`_id`='rival'),clause=list(price=100,transferred=FALSE,date='2066-11-15T12:00:00Z',unlockDate='2066-11-16T12:05:00Z'))))
player<-fm_clause_player(summary)
stopifnot(player$id=='p',player$owner_id=='rival',player$price==100,fm_clause_eligible(player,'t'),
 fm_time(player$opens_at)==fm_time('2066-11-15T12:00:00Z'),!fm_clause_eligible(player,'rival'))
for(field in c('transferred','price','opens_at','owner_id')){bad<-player;bad[[field]]<-NA;stopifnot(!fm_clause_eligible(bad,'t'))}
cat('PASS authoritative identity, owner, clause date, explicit transfer state and unknown eligibility\n')
# Independent finance answer: max(150 withheld,120 official offers)+200 scheduled.
info<-observed_data(list(budget=1000,withheld=150,teamValue=1000,maxBid=1000,configuration=list(mnmp=.2)))
market<-observed_data(data.frame(id='offer-player',bid_id='official-offer',bid_price=120))
state<-function(value)list(status='ok',freshness='fresh',data=value)
base<-round_finance_display(state(info),state(market))
jobs<-list(list(kind='scheduled',state='scheduled',reserved_amount=200,reserved_slot=1),list(kind='scheduled',state='cancelled',reserved_amount=0,reserved_slot=0))
fin<-fm_clause_finance(base,jobs,info)
stopifnot(fin$projected_committed_balance==650,fin$spendable_budget==850,fin$scheduled_count==1,
 is.na(fm_clause_finance(base,NULL)$spendable_budget))
capacity<-list(status='ok',roster=list(status='ok',count=9,effective_count=9,cap=12,pending_inbound_ids=character(),pending_outbound_ids=character()),
 funds=list(spendable_budget=1050,api_bid_limit=1000,projected_committed_balance=850),
 outstanding=list(count=1,offers=1,total_amount=120),valid_until=as.numeric(now)+14,target_rows=market)
adjusted<-fm_clause_adjust_capacity(capacity,jobs)
stopifnot(adjusted$outstanding$count==2,adjusted$outstanding$total_amount==320,adjusted$funds$spendable_budget==850,
 fm_clause_adjust_capacity(capacity,NULL)$status=='partial')
cat('PASS scheduled offers reserve cash and capacity once; unavailable reservations remain unknown\n')
# Exercise the real headless-check projection with source-shaped inputs.
old<-mget(c('resource_context','get_resolved_league_context','get_player_summary','get_team_acquisition_capacity',
 'get_user_team_info','get_players_from_team','get_championship_pressroom','get_active_championships'),.GlobalEnv)
resource_context<-function(...)ctx
get_resolved_league_context<-function(...)ctx
get_active_championships<-function(...)list(championships=list(list(id="l",userteam=list(id="t"))))
get_player_summary<-function(...)summary
get_team_acquisition_capacity<-function(...)capacity
get_user_team_info<-function(...)info
get_players_from_team<-function(...)observed_data(data.frame(id=c('own1','own2')))
get_championship_pressroom<-function(...)observed_data(data.frame(player_id='p',buyer_team_id='t',seller_team_id='rival',price=100,created=fm_clause_stamp(now-10)))
facts<-get_scheduled_clause_check(auth,'l','t','p')
stopifnot(facts$player$id=='p',facts$transactions[[1]]$price==100,facts$transactions[[1]]$buyer_team_id=='t',
 identical(facts$owned_ids,I(c('own1','own2'))),fm_time(facts$financial_observed_at)<=Sys.time())
wire<-fm_worker_encode(facts);decoded<-fm_worker_decode(wire)
stopifnot(decoded$player$id=='p',identical(decoded$offers,list(list(player_id='offer-player',bid_id='official-offer',price=120))))
capacity$valid_until<-as.numeric(now)-1
stopifnot(inherits(try(get_scheduled_clause_check(auth,'l','t','p'),silent=TRUE),'try-error'))
invisible(list2env(old,.GlobalEnv))
cat('PASS check projection keeps source time, transfer evidence, offer identities and expiry through the R/Python codec\n')
# The diagnostic is scoped to a configured account and disabled by default.
before<-Sys.getenv('FUTMONDO_CLAUSE_DIAGNOSTIC_ACCOUNT',unset=NA_character_)
Sys.unsetenv('FUTMONDO_CLAUSE_DIAGNOSTIC_ACCOUNT');stopifnot(!fm_clause_diagnostic(auth))
Sys.setenv(FUTMONDO_CLAUSE_DIAGNOSTIC_ACCOUNT='u');stopifnot(fm_clause_diagnostic(auth),!fm_clause_diagnostic(c(userid='other',token='synthetic')))
if(is.na(before))Sys.unsetenv('FUTMONDO_CLAUSE_DIAGNOSTIC_ACCOUNT')else Sys.setenv(FUTMONDO_CLAUSE_DIAGNOSTIC_ACCOUNT=before)
cat('PASS temporary diagnostic is disabled by default and isolated to the configured account\n')
# Hold source acquisition to verify that confirmation yields control, does not
# enqueue before consent, and rejects a late completion from another context.
fm_clause_enabled<-function()TRUE
fm_worker_encrypt<-function(auth){x<-'synthetic-ciphertext';attr(x,'credential_id')<-'synthetic-binding';x}
calls<-list();checks<-list()
fm_clause_rpc<-function(name,payload,owner=''){
 calls[[length(calls)+1L]]<<-list(name=name,payload=payload)
 promises::promise_resolve(if(name=='admit')list(id=payload$p_id)else if(name=='read')list(jobs=list())else TRUE)
}
async_adapter_fetch<-function(fn,args=list(),context=NULL,is_current=function()TRUE,...){
 promises::promise(function(resolve,reject){checks[[length(checks)+1L]]<<-list(resolve=resolve,current=is_current)})
}
fixture<-function(id){moduleServer(id,function(input,output,session){
 selected<-reactiveVal(data.frame(id='p',slug='fixture',name='Fixture',user_team_id='rival',clause_price=100,clause_transferred=FALSE,clause_date='2066-11-15T12:00:00Z'))
 current<-reactiveVal(list(player='p'));remembered<-reactiveVal(NULL)
 remember<-function(action)remembered(list(action=action,context=current()))
 consume<-function(action){r<-remembered();if(is.null(r)||r$action!=action||!identical(r$context,current()))return(FALSE);remembered(NULL);TRUE}
 fm_clause_card_observers(input,session,selected,function()auth,function()'l',function()'t',remember,consume,current)
})}
settle<-function(session)for(i in 1:20){later::run_now(0);session$flushReact()}
shiny::testServer(fixture,{
 session$setInputs(schedule_clause=1L);settle(session)
 session$setInputs(confirm_scheduled_clause=1L,schedule_clause_consent=FALSE);settle(session)
 stopifnot(!length(checks),!length(calls))
 started<-proc.time()[['elapsed']];session$setInputs(confirm_scheduled_clause=2L,schedule_clause_consent=TRUE);settle(session)
 elapsed<-proc.time()[['elapsed']]-started
 stopifnot(length(checks)==1L,!any(vapply(calls,function(x)x$name=='admit',logical(1))),elapsed<.5)
 current(list(player='other'));checks[[1]]$resolve(facts);settle(session)
 stopifnot(!any(vapply(calls,function(x)x$name=='admit',logical(1))))
 cat(sprintf('MEASURE scheduling_pending_ms=%.1f source_checks=1 transactions=0\n',elapsed*1000))
 current(list(player='p'));session$setInputs(schedule_clause=2L);settle(session)
 session$setInputs(confirm_scheduled_clause=3L,schedule_clause_consent=TRUE);settle(session)
 checks[[2]]$resolve(facts);settle(session)
 admitted<-Filter(function(x)x$name=='admit',calls)
 stopifnot(length(admitted)==1L,admitted[[1]]$payload$p_price==100,admitted[[1]]$payload$p_kind=='scheduled',
 admitted[[1]]$payload$p_context$account_id=='u')
 session$setInputs(confirm_scheduled_clause=4L);settle(session)
 stopifnot(length(Filter(function(x)x$name=='admit',calls))==1L)
})
cat('PASS explicit consent, asynchronous verification, context cancellation and duplicate-click protection\n')

# Failed outcomes must survive logout/relogin and explicit acknowledgement.
.fm_clause$accounts<-new.env(parent=emptyenv())
model<-list(list(id=paste(rep('a',32),collapse=''),account_id='u',kind='scheduled',state='failed',result_code='execution_preflight_changed',
 context=fm_clause_scope(ctx),player_id='p',player_name='Fixture outcome',opens_at=fm_clause_stamp(now-30),finished_at=fm_clause_stamp(now-20),
 reserved_amount=0,reserved_slot=0,acknowledged_at=NULL))
fm_clause_rpc<-function(name,payload,owner=''){
 if(name=='ack')model[[1]]$acknowledged_at<<-fm_clause_stamp()
 promises::promise_resolve(if(name=='read')list(jobs=model)else TRUE)
}
# MockShinySession's getOutput drains real later timers to an empty loop. Stop
# the account's recurring poll explicitly after each simulated login.
stop_poll<-function(){state<-fm_clause_account('u');if(is.function(state$timer))state$timer();state$timer<-NULL}
shiny::testServer(function(input,output,session){login<-reactiveVal(auth);fm_clause_session(session,login)}, {
 settle(session);stop_poll();stopifnot(grepl('Fixture outcome',output$clause_results$html,fixed=TRUE))
 login(NULL);settle(session);stopifnot(!exists('u',envir=.fm_clause$accounts,inherits=FALSE))
 login(auth);settle(session);stop_poll();stopifnot(grepl('Fixture outcome',output$clause_results$html,fixed=TRUE))
 session$setInputs(clause_ack_id=paste(rep('a',32),collapse=''));settle(session)
 stopifnot(!is.null(fm_clause_account('u')$jobs[[1]]$acknowledged_at))
})
cat('PASS execution outcomes survive logout and next login until account-scoped acknowledgement\n')
