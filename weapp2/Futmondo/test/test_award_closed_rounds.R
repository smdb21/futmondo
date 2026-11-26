#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
auth<-c(userid='closed-round-fixture',token='synthetic');cid<-'league'
fixture<-new.env(parent=emptyenv());fixture$requests<-character()
fixture$catalog<-list(rounds=data.frame(round_id=paste0('r',1:38),round_number=1:38,
  is_finished=c(rep(TRUE,11),rep(NA,27)),status=c(rep('done',11),rep('future',27))),
  current_round_number=12,user_team_id='team',complete=FALSE,identity_complete=TRUE,
  reason='Some round numbers or completion states are not confirmed. Retrieve missing awards to check their schedules.')
xi<-list(players=lapply(1:11,function(i)list(id=paste0('p',i),name=paste('Player',i),points=i)),mvp='p1')
check('calendar evidence overrides a stale final XI until the final group is acquired',{
  compact<-data.frame(round_id='r23',round_number=23,is_finished=TRUE,status='done')
  matches<-lapply(1:10,function(i)list(`_id`=paste0('g',i),h=list(`_id`=paste0('h',i)),a=list(`_id`=paste0('a',i)),
    st=if(i<=2)'F'else'P',info=list(date=if(i<=2)'2026-12-02T20:30:00Z'else'2027-02-07T20:00:00Z')))
  records<-list(list(`_id`='r23',matches=matches))
  calendar<-function(records,stamp){
    metadata<-list(rows=compact,fixture_matches=league_fixture_matches(records),fixture_rounds=award_fixture_calendar(records))
    attr(metadata,'observed_at')<-fm_time(stamp)
    award_join_calendar(compact,metadata)$rounds
  }
  split<-calendar(records,'2026-12-03T10:00:00Z')
  saved<-normalize_official_dreamteam(xi,cid,list(round_id='r23',round_number=23,is_finished=TRUE))$rows
  saved$checked_at<-'2026-12-03T10:00:00Z';saved$finality<-'final';saved$persistence<-'Saved'
  current<-award_calendar_selections(saved,split)
  coverage<-award_coverage(split,saved,current_round_number=24)
  stopifnot(!coverage$required,coverage$finality=='Provisional / in progress',
    all(current$finality=='provisional'),all(!current$is_finished),all(saved$is_finished),
    round_award_player_stats(current,cid)$rounds==0,
    grepl('Provisional selection',as.character(round_award_card(round_mvp_rows(current,cid),round_best_xi_rows(current,cid,split)))))
  for(i in seq_along(matches))records[[1]]$matches[[i]]$st<-'F'
  split<-calendar(records,'2027-02-08T10:00:00Z')
  stopifnot(!split$partially_played,is.na(split$resume_at),
    split$final_fixture_at==as.numeric(fm_time('2027-02-07T20:00:00Z')))
  stale<-award_coverage(split,saved,current_round_number=24)
  stopifnot(stale$required,stale$finality=='Provisional / in progress')
  saved$checked_at<-'2027-02-08T10:00:00Z'
  stopifnot(award_coverage(split,saved,current_round_number=24)$finality=='Confirmed final',
    round_award_player_stats(award_calendar_selections(saved,split),cid)$rounds==1)
})
check('38-round coverage requires 11 closed rounds and labels the other 27 not due',{
  coverage<-award_coverage(fixture$catalog$rounds,data.frame(),
    failures=list(r12='Old request error',r38='Old request error'),current_round_number=12)
  stopifnot(sum(coverage$required)==11L,sum(coverage$selection=='Not due')==27L,
    all(coverage$finality[12:38]=='Not closed'),all(coverage$issue[12:38]==''))
  provisional<-normalize_official_dreamteam(xi,cid,list(round_id='r38',round_number=38,is_finished=FALSE))$rows
  cached<-award_coverage(fixture$catalog$rounds,provisional,failures=list(r38='Old future error'),current_round_number=12)
  stopifnot(!cached$required[38],cached$issue[38]=='')
  stopifnot(!award_round_pending(list(round_number=12,is_finished=TRUE),12),
    award_round_pending(list(round_number=11,is_finished=FALSE),12),
    !award_round_pending(list(round_number=11,is_finished=NA),12),
    !award_round_pending(list(round_number=38,is_finished=NA),NA_real_))
})
original_async<-async_adapter_fetch;original_persist<-persist_official_award_snapshot
async_adapter_fetch<-function(fn,args,context,...){
  if(fn=='get_mvp_saved_catalog')return(promises::promise_resolve(list(saved=data.frame(),saved_complete=TRUE,catalog=fixture$catalog)))
  if(fn=='get_award_round_catalog')return(promises::promise_resolve(fixture$catalog))
  stopifnot(fn=='get_official_dreamteam_result')
  fixture$requests<-c(fixture$requests,args$reference$round_id)
  promises::promise_resolve(normalize_official_dreamteam(xi,args$championship_id,args$reference))
}
persist_official_award_snapshot<-function(result,login,championship_id,reference,context,on_complete){on_complete(TRUE);TRUE}
settle<-function(session,predicate){deadline<-Sys.time()+4;repeat{session$flushReact();later::run_now(.005);session$flushReact();if(isTRUE(shiny::isolate(predicate())))return(invisible(NULL));if(Sys.time()>deadline)stop('Closed-round fixture timed out')}}
check('missing and refresh actions never request unclosed rounds or count them as failures',{
  shiny::testServer(function(input,output,session){source<-shared_mvp_source(session,function()auth,function()cid,function()TRUE)},{
    settle(session,function()!source()$loading)
    attr(source,'retrieve')('missing');settle(session,function()!source()$loading&&!source()$running)
    stopifnot(setequal(fixture$requests,paste0('r',1:11)),length(fixture$requests)==11L,
      source()$fetched==11L,source()$saved==11L,source()$failed==0L,source()$complete,
      sum(source()$coverage$selection=='Not due')==27L,!nzchar(trimws(source()$reason)))
    before<-length(fixture$requests)
    attr(source,'retrieve')('refresh');settle(session,function()!source()$loading&&!source()$running)
    attr(source,'retrieve')('selected','r38');settle(session,function()!source()$loading&&!source()$running)
    stopifnot(length(fixture$requests)==before,source()$failed==0L)
    fixture$catalog$rounds$is_finished[12]<-TRUE
    fixture$catalog$rounds$status[12]<-'done'
    attr(source,'retrieve')('refresh');settle(session,function()!source()$loading&&!source()$running)
    stopifnot(tail(fixture$requests,1)=='r12',source()$saved==1L,source()$failed==0L,
      sum(source()$coverage$required)==12L)
  })
})
async_adapter_fetch<-original_async;persist_official_award_snapshot<-original_persist
check('Admin excludes future rounds and a future-only awards page has no warning',{
  original_shared<-shared_mvp_source
  coverage<-award_coverage(fixture$catalog$rounds,data.frame(),current_round_number=12)
  snapshot<-list(rows=data.frame(),coverage=coverage,reason='',fetched=0L,saved=0L,failed=0L,pending=0L,unsaved=0L)
  shared_mvp_source<-function(...)reactiveVal(snapshot)
  shiny::testServer(admin_Server,args=list(is_module_active=function()TRUE,login_token=function()auth,
    championship_id=function()cid,user_team_id=function()'team',user_teams_RV=function()data.frame(),is_admin=function()TRUE),{
    encoded<-output$awards_coverage
    stopifnot(grepl('r11',encoded,fixed=TRUE),!grepl('r38',encoded,fixed=TRUE),!grepl('Not closed',encoded,fixed=TRUE))
  })
  snapshot$coverage<-coverage[!coverage$required,,drop=FALSE]
  shiny::testServer(round_mvps_Server,args=list(is_module_active=function()TRUE,login_token=function()auth,championship_id=function()cid),{
    html<-output$mvp_cards$html
    stopifnot(grepl('No completed round awards yet.',html,fixed=TRUE),!grepl('unavailable',html,fixed=TRUE))
  })
  shared_mvp_source<-original_shared
})
check('resolving an unknown current or future number avoids the dreamteam request',{
  original_api<-mvp_api_answer;dreamteam_reads<-0L
  mvp_api_answer<-function(login,url,query){
    if(grepl('/match/list$',url))return(list(status='ok',answer=list(round=list(`_id`=query$roundId,number=as.numeric(sub('r','',query$roundId))))))
    dreamteam_reads<<-dreamteam_reads+1L;stop('Unclosed award request forbidden')
  }
  # An already identified future round must not trigger even a schedule lookup.
  old_reads<-dreamteam_reads
  get_cached_data(paste0('official_dreamteam_',cid,'_r38'),mvp_result('rejected',reason='Old future error'),context=resource_context(auth,cid))
  result<-get_official_dreamteam_result(auth,cid,list(round_id='r38',round_number=38,is_finished=NA,current_round_number=12))
  stopifnot(result$status=='not_due',dreamteam_reads==old_reads)
  for(number in c(12,38)){
    clear_api_cache()
    result<-get_official_dreamteam_result(auth,cid,list(round_id=paste0('r',number),round_number=NA_real_,is_finished=NA,current_round_number=12))
    stopifnot(result$status=='not_due',result$reference$round_number==number,dreamteam_reads==0L)
  }
  # Being earlier than the active round allows retrieval; it does not invent finality.
  historical<-list(round_id='r11',round_number=11,is_finished=NA,current_round_number=12)
  rows<-normalize_official_dreamteam(xi,cid,historical)$rows
  stopifnot(all(is.na(rows$is_finished)),nrow(round_mvp_rows(rows,cid))==1L)
  mvp_api_answer<-original_api
})
cat(sprintf('CLOSED ROUND AWARDS: %d passed / 0 failed; 11 award requests, 27 skipped\n',n))
