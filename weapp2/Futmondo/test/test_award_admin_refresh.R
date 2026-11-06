#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
auth<-c(userid='award-admin-refresh-fixture',token='synthetic');cid<-'league'
check('an identified current round is not due even before its number is known',{
  reference<-list(round_id='current',round_number=NA_real_,is_finished=NA,current_round_id='current')
  stopifnot(award_round_pending(reference))
  reference$is_finished<-TRUE
  stopifnot(!award_round_pending(reference))
  rounds<-data.frame(round_id=c('past','current'),round_number=c(11,NA_real_),is_finished=c(TRUE,NA),status='future')
  coverage<-award_coverage(rounds,data.frame(),list(current='Old missing-selection failure'),current_round_id='current')
  stopifnot(coverage$required[1],!coverage$required[2],coverage$selection[2]=='Not due',coverage$issue[2]=='')
})
fixture<-new.env(parent=emptyenv());fixture$requests<-character();fixture$catalog_reads<-0L
fixture$inflight<-0L;fixture$maximum<-0L;fixture$reject_catalog<-FALSE
fixture$catalog<-list(rounds=data.frame(round_id=paste0('r',1:38),round_number=1:38,
  is_finished=c(rep(TRUE,11),rep(NA,27)),status=c(rep('done',11),rep('future',27))),
  current_round_number=12,current_round_id='r12',user_team_id='team',complete=FALSE,identity_complete=TRUE,
  reason='Some round numbers or completion states are not confirmed. Retrieve missing awards to check their schedules.')
xi<-list(players=lapply(1:11,function(i)list(id=paste0('p',i),name=paste('Player',i),points=i)),mvp='p1')
old_async<-async_adapter_fetch;old_persist<-persist_official_award_snapshot
async_adapter_fetch<-function(fn,args,context,...){
  if(fn=='get_mvp_saved_catalog')return(promises::promise_resolve(list(saved=data.frame(),saved_complete=TRUE,catalog=fixture$catalog)))
  if(fn=='get_award_round_catalog'){
    fixture$catalog_reads<-fixture$catalog_reads+1L
    if(fixture$reject_catalog)return(promises::promise_reject(simpleError('Synthetic source unavailable')))
    return(promises::promise(function(resolve,reject)later::later(function()resolve(fixture$catalog),.02)))
  }
  stopifnot(fn=='get_official_dreamteam_result')
  fixture$requests<-c(fixture$requests,args$reference$round_id)
  fixture$inflight<-fixture$inflight+1L;fixture$maximum<-max(fixture$maximum,fixture$inflight)
  stopifnot(identical(args$reference$current_round_id,fixture$catalog$current_round_id))
  promises::promise(function(resolve,reject)later::later(function(){
    fixture$inflight<-fixture$inflight-1L
    resolve(normalize_official_dreamteam(xi,args$championship_id,args$reference))
  },.005))
}
persist_official_award_snapshot<-function(result,login,championship_id,reference,context,on_complete){on_complete(TRUE);TRUE}
settle<-function(session,predicate){deadline<-Sys.time()+5;repeat{
  session$flushReact();later::run_now(.005);session$flushReact()
  if(isTRUE(shiny::isolate(predicate())))return(invisible(NULL))
  if(Sys.time()>deadline)stop('Admin round-refresh fixture timed out')
}}
check('missing retrieval rechecks round closure before requests and excludes future warnings',{
  shiny::testServer(function(input,output,session){source<-shared_mvp_source(session,function()auth,function()cid,function()TRUE)},{
    settle(session,function()!source()$loading)
    stopifnot(length(fixture$requests)==0L,fixture$catalog_reads==0L,
      sum(source()$coverage$required)==11L,sum(source()$coverage$selection=='Not due')==27L)
    # A new round can finish while Admin remains open. The click must observe it.
    fixture$catalog$rounds$is_finished[12]<-TRUE;fixture$catalog$rounds$status[12]<-'done'
    fixture$catalog$current_round_number<-13;fixture$catalog$current_round_id<-'r13'
    heartbeat<-FALSE;later::later(function(){heartbeat<<-TRUE},.001)
    stopifnot(attr(source,'retrieve')('missing'))
    settle(session,function()!source()$loading&&!source()$running)
    stopifnot(heartbeat,fixture$catalog_reads==1L,length(fixture$requests)==12L,
      setequal(fixture$requests,paste0('r',1:12)),fixture$maximum==2L,
      source()$fetched==12L,source()$saved==12L,source()$failed==0L,
      sum(source()$coverage$selection=='Not due')==26L,source()$complete,
      !nzchar(trimws(source()$reason)),all(source()$coverage$issue==''))
    # Repeated clicks reuse acknowledged awards, even though source status is rechecked.
    before<-length(fixture$requests)
    attr(source,'retrieve')('missing');settle(session,function()!source()$loading&&!source()$running)
    stopifnot(fixture$catalog_reads==2L,length(fixture$requests)==before,source()$failed==0L)
    # A real metadata outage remains visible and cannot launch uncertain requests.
    fixture$reject_catalog<-TRUE
    attr(source,'retrieve')('missing');settle(session,function()!source()$loading&&!source()$running)
    stopifnot(length(fixture$requests)==before,grepl('Round status could not be refreshed',source()$reason,fixed=TRUE),
      nrow(source()$rows)==132L)
  })
})
async_adapter_fetch<-old_async;persist_official_award_snapshot<-old_persist
cat(sprintf('ADMIN AWARD REFRESH: %d passed / 0 failed; at most 2 simultaneous reads; future rounds omitted\n',n))
