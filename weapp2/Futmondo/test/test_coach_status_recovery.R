options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='coach-fixture',token='synthetic')
checks<-0L;check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
info<-function(value=0,stamp=Sys.time())observed_data(list(budget=100,staff=list(coach=value),configuration=list(enableCoach='automatic')),observed_at=stamp)
settle<-function(session=NULL){for(i in 1:20){if(!is.null(session))session$flushReact();later::run_now(.001)}}
await<-function(p){done<-FALSE;result<-NULL;failure<-NULL;promises::then(p,function(x){result<<-x;done<<-TRUE},function(e){failure<<-e;done<<-TRUE});for(i in 1:100){later::run_now(.001);if(done)break};stopifnot(done);if(!is.null(failure))stop(failure);result}
ctx<-resource_context(auth,'league','team',session_id='fixture-session')
target<-list(account=auth[['userid']],championship='league',team='team')
real_read<-coach_read_team_info;real_hire<-fm_async_hire_coach
http<-list();saves<-list();saved_rows<-data.frame()
get_sb_url<-function()'https://coach-db.invalid';get_sb_key<-function()'fixture'
fm_async_read_http<-function(url,method,body=NULL,...){
 http[[length(http)+1L]]<<-list(url=url,body=body)
 response<-if(method=='GET')jsonlite::toJSON(saved_rows,auto_unbox=TRUE,null='null',digits=NA)else {
  p<-jsonlite::fromJSON(body,simplifyVector=FALSE)
  stopifnot(identical(p$query,list(championshipId='league',userteamId='team',type='market')))
  jsonlite::toJSON(list(answer=list(staff=list(coach=1),configuration=list(enableCoach='automatic'),budget=100)),auto_unbox=TRUE)
 }
 promises::promise_resolve(list(status_code=200L,content=charToRaw(response)))
}
# Capture writes without live DB mutation; failed enqueue remains callable again.
accept_save<-FALSE
defer_persistence<-function(fn,args,context,...){saves[[length(saves)+1L]]<<-list(fn=fn,args=args,context=context);accept_save}
record_acquired_resource<-function(...)NULL
fm_worker_configured<-function()TRUE
fm_worker_fetch<-function(...)stop('A coach safety read must bypass worker replay')
check('stale shared source yields a real fresh read, scoped persistence, and exact native query',{
 clear_api_cache();get_cached_data('team_info_league_team',info(0,Sys.time()-10),context=resource_context(auth,'league','team'))
 value<-await(real_read(auth,target,list(token='fixture-session'),function()TRUE))
 stopifnot(value$staff$coach==1,coach_info_status(resource_state(value))=='hired',length(http)==2L,length(saves)==1L,
  saves[[1]]$args[[1]]=='rival_finance_evidence',saves[[1]]$context$account_id==auth[['userid']])
 record<-saves[[1]]$args[[2]][[1]];payload<-record$payload
 stopifnot(payload$kind=='coach_status',payload$status=='complete',payload$coach==1,
  payload$scope$user_team_id=='team',!grepl('synthetic|token',jsonlite::toJSON(record)))
 # No success marker is installed for a rejected queue admission: retry identical revision.
 accept_save<-TRUE;stopifnot(isTRUE(persist_coach_snapshot(value,ctx)),length(saves)==2L,
  identical(saves[[1]]$args,saves[[2]]$args))
 saved_rows<-as.data.frame(record[setdiff(names(record),'payload')],stringsAsFactors=FALSE)
 saved_rows$payload<-I(list(payload))
})
check('acknowledged saved coach observation is reused with zero API reads and no duplicate save',{
 before<-length(http);saved_count<-length(saves)
 value<-await(real_read(auth,target,list(token='fixture-session'),function()TRUE))
 stopifnot(isTRUE(attr(value,'coach_from_saved')),coach_info_status(resource_state(value))=='hired',
  length(http)==before+1L,length(saves)==saved_count)
})
check('foreign scope, expired data and pre-mutation observations cannot authorize confirmation',{
 original<-saved_rows
 for(kind in c('foreign-team','foreign-account','expired','pre-mutation')){
  saved_rows<-original
  if(kind=='foreign-team')saved_rows$payload[[1]]$scope$user_team_id<-'other'
  if(kind=='foreign-account')saved_rows$account_id<-'other'
  if(kind=='expired'){
   stamp<-Sys.time()-60;saved_rows$observed_at<-rival_finance_stamp(stamp)
   saved_rows$payload[[1]]$source_observed_at<-rival_finance_stamp(stamp)
   saved_rows$payload[[1]]$valid_until<-rival_finance_stamp(stamp+20)
  }
  before<-length(http)
  value<-await(real_read(auth,target,list(token='fixture-session'),function()TRUE,
    not_before=if(kind=='pre-mutation')Sys.time()else NULL))
  stopifnot(length(http)==before+2L,!isTRUE(attr(value,'coach_from_saved')),value$staff$coach==1)
 }
 saved_rows<-data.frame()
})
# Controller fixtures hold asynchronous operations to verify latches and yielding.
reads<-list();hires<-list()
coach_read_team_info<-function(auth,target,session,is_current,not_before=NULL){
 promises::promise(function(resolve,reject)reads[[length(reads)+1L]]<<-list(resolve=resolve,reject=reject,current=is_current,after=not_before))
}
fm_async_hire_coach<-function(...)promises::promise(function(resolve,reject)hires[[length(hires)+1L]]<<-list(resolve=resolve,reject=reject))
reset<-function(){reads<<-list();hires<<-list();rm(list=ls(.coach_login_guards),envir=.coach_login_guards)}
controller<-function(input,output,session){
 login<-reactiveVal(auth);league<-reactiveVal('league');team<-reactiveVal('team')
 shared<-reactiveVal(resource_state(info(0,Sys.time()-30)))
 retries<-new.env();retries$n<-0L
 source<-coach_on_login_source(session,login,league,team,shared,function(){retries$n<-retries$n+1L;shared(resource_state(info(0,Sys.time()-30)))})
}
check('30-second durable snapshot waits asynchronously then recognizes an existing coach',{
 reset();shiny::testServer(controller,{
  settle(session);source$set_preference(source$key(),TRUE);settle(session)
  stopifnot(length(reads)==1L,!length(hires),source$status()=='Checking coach…')
  beat<-FALSE;later::later(function()beat<<-TRUE,0);settle(session);stopifnot(beat,length(reads)==1L)
  reads[[1]]$resolve(info(1));settle(session)
  stopifnot(source$status()=='Coach already hired',!length(hires))
 })
})
check('absent coach submits once and old worker confirmation falls back to a post-action read',{
 reset();shiny::testServer(controller,{
  settle(session);source$set_preference(source$key(),TRUE);settle(session)
  reads[[1]]$resolve(info(0));settle(session);stopifnot(length(hires)==1L)
  hires[[1]]$reject(simpleError('Synthetic uncertain response'));settle(session)
  stopifnot(retries$n==1L,length(reads)==2L,!is.null(reads[[2]]$after),source$status()=='Confirming coach…')
  reads[[2]]$resolve(info(1));settle(session)
  stopifnot(source$status()=='Coach hired',length(hires)==1L)
  shared(resource_state(info(0)));settle(session);stopifnot(length(hires)==1L,length(reads)==2L)
 })
})
check('failed reads remain truthful and later fresh shared data recovers without a permanent latch',{
 reset();shiny::testServer(controller,{
  settle(session);source$set_preference(source$key(),TRUE);settle(session)
  reads[[1]]$reject(resource_failure(category='network',resource='information'));settle(session)
  stopifnot(!length(hires),grepl('Connection unavailable',source$status()),length(reads)==1L)
  shared(resource_state(info(1)));settle(session)
  stopifnot(source$status()=='Coach already hired',!length(hires),length(reads)==1L)
 })
})
check('unknown fields never authorize hiring or count as a known unsupported mode',{
 reset();shiny::testServer(controller,{
  settle(session);source$set_preference(source$key(),TRUE);settle(session)
  reads[[1]]$resolve(observed_data(list(staff=list(coach=0))));settle(session)
  stopifnot(grepl('no fresh, verified coach status',source$status()),!length(hires),length(reads)==1L)
  shared(resource_state(info(1)));settle(session);stopifnot(source$status()=='Coach already hired')
 })
})
check('opt-out and context changes discard pending read authorization',{
 for(change in c('opt-out','logout','league','team')){
  reset();shiny::testServer(controller,{
   settle(session);source$set_preference(source$key(),TRUE);settle(session)
   if(change=='opt-out')source$set_preference(source$key(),FALSE)
   if(change=='logout')login(NULL)
   if(change=='league')league('other')
   if(change=='team')team('other')
   settle(session);reads[[1]]$resolve(info(0));settle(session);stopifnot(!length(hires))
  })
 }
})
check('failed confirmation never repeats a hire or a confirmation read',{
 reset();shiny::testServer(controller,{
  settle(session);source$set_preference(source$key(),TRUE);settle(session)
  reads[[1]]$resolve(info(0));settle(session);hires[[1]]$resolve(TRUE);settle(session)
  reads[[2]]$reject(resource_failure(category='network'));settle(session)
  stopifnot(grepl('could not be confirmed',source$status()),length(hires)==1L)
  shared(resource_state(info(0)));settle(session);stopifnot(length(reads)==2L,length(hires)==1L)
 })
})
cat(sprintf('COACH STATUS RECOVERY: %d passed / 0 failed\n',checks))
