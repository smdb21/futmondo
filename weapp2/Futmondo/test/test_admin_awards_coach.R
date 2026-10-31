#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
auth<-c(userid='fixture-u',token='synthetic-token')
xi<-function(mvp='p1')list(players=lapply(seq_len(11L),function(i)list(id=paste0('p',i),name=paste('Player',i),role=1,points=if(i==1)0 else i)),mvp=mvp,strategy='4-4-2',pointSystem='custom',custom=list(stats='ff'))
reference<-list(round_id='r1',round_number=1,is_finished=FALSE)
check('full XI keeps source finality and scoring; malformed selections fail closed',{
  result<-normalize_official_dreamteam(xi(),'c',reference)
  stopifnot(result$status=='ok',nrow(result$rows)==11L,sum(result$rows$is_mvp)==1L,all(result$rows$is_finished%in%FALSE),result$rows$points[1]==0,result$metadata$pointSystem=='custom')
  unknown<-reference;unknown$is_finished<-NULL;stopifnot(all(is.na(normalize_official_dreamteam(xi(),'c',unknown)$rows$is_finished)))
  short<-xi();short$players<-short$players[-1];stopifnot(normalize_official_dreamteam(short,'c',reference)$status!='ok')
  duplicate<-xi();duplicate$players[[2]]$id<-'p1';stopifnot(normalize_official_dreamteam(duplicate,'c',reference)$status!='ok')
})
check('saved award envelopes enforce account league season and scoring isolation',{
  result<-normalize_official_dreamteam(xi(),'c',reference)
  ctx<-resource_context(auth,'c',season='season-a');snapshot<-award_snapshot_evidence(result,auth,'c',reference,ctx)
  version<-snapshot$payload$scoring_version
  saved<-award_saved_rows(list(snapshot),'c','season-a',version,auth[['userid']]);stopifnot(nrow(saved)==11L)
  stopifnot(nrow(award_saved_rows(list(snapshot),'c','season-a',version))==0L)
  for(field in c('account_id','championship_id','season')){
    foreign<-snapshot;foreign[[field]]<-'foreign'
    stopifnot(nrow(award_saved_rows(list(foreign),'c','season-a',version,auth[['userid']]))==0L)
  }
  stopifnot(nrow(award_saved_rows(list(snapshot),'c','season-a','other-scoring',auth[['userid']]))==0L)
})
check('provisional and unknown awards never enter credited cash estimates',{
  rules<-list(dreamteam_reward=10,mvp_reward=20)
  eligible<-data.frame(round_id='r1',player_id='p1',team_id='t',eligible=TRUE,verified=TRUE)
  result<-normalize_official_dreamteam(xi(),'c',reference)
  stopifnot(nrow(rival_award_estimates(result$rows,'t',rules,eligible)$rows)==0L)
  result$rows$is_finished<-TRUE
  stopifnot(sum(rival_award_estimates(result$rows,'t',rules,eligible)$rows$amount)==30)
})
check('coach request matches captured envelope and private preference identities are scoped',{
  request<-build_hire_coach_payload(auth,'c','t')
  stopifnot(identical(request$query,list(championshipId='c',userteamId='t',roundId=NULL,type='coach')),
    coach_preference_key('u','c')!=coach_preference_key('v','c'),coach_preference_key('u','c')!=coach_preference_key('u','d'))
})
fresh_info<-function(coach=0,mode='automatic',status='ok'){
  now<-Sys.time();list(status=status,freshness='fresh',observed_at=now,fetched_at=now,data=list(staff=list(coach=coach),configuration=list(enableCoach=mode)))
}
settle<-function(session,predicate=function()TRUE,timeout=2){deadline<-Sys.time()+timeout;repeat{session$flushReact();later::run_now(.01);session$flushReact();if(isTRUE(shiny::isolate(predicate())))return(invisible(NULL));if(Sys.time()>deadline)stop('fixture timed out')}}
old_hire<-fm_async_hire_coach;hires<-0L;resolve_hire<-NULL
fm_async_hire_coach<-function(...){hires<<-hires+1L;promises::promise(function(resolve,reject){resolve_hire<<-resolve})}
check('coach waits for preference, respects opt-out, and never submits twice',{
  rm(list=ls(.coach_login_guards),envir=.coach_login_guards)
  shiny::testServer(function(input,output,session){a<-reactiveVal(auth);c<-reactiveVal('c');info<-reactiveVal(fresh_info());retries<-new.env();retries$n<-0L
    source<-coach_on_login_source(session,a,c,function()'t',info,function(){retries$n<-retries$n+1L;info(list(status='loading',freshness='fresh'))})},{
    settle(session);stopifnot(hires==0L,is.null(source$preference()))
    source$set_preference(source$key(),FALSE);settle(session);stopifnot(hires==0L,source$status()=='Disabled')
    source$set_preference(source$key(),TRUE);settle(session);stopifnot(hires==1L)
    info(fresh_info());settle(session);stopifnot(hires==1L)
    heartbeat<-FALSE;later::later(function(){heartbeat<<-TRUE},.001);later::run_now(.01);stopifnot(heartbeat)
    resolve_hire(TRUE);settle(session);stopifnot(retries$n==1L,source$status()=='Confirming coach…')
    info(fresh_info(1));settle(session);stopifnot(source$status()=='Coach hired',hires==1L)
    info(fresh_info(0));settle(session);stopifnot(hires==1L)
    a(NULL);settle(session);a(auth);settle(session);stopifnot(is.null(source$preference()))
    source$set_preference(source$key(),TRUE);settle(session);stopifnot(hires==1L) # cross-session cooldown
  })
})
check('already hired, unknown, unsupported and stale source never authorize a hire',{
  before<-hires
  for(value in list(fresh_info(1),fresh_info(NA),fresh_info(0,'manual'),within(fresh_info(),freshness<-'stale'))){
    shiny::testServer(function(input,output,session){info<-reactiveVal(value);source<-coach_on_login_source(session,function()auth,function()'isolated',function()'t',info,function()stop('no retry'))},{
      settle(session);source$set_preference(source$key(),TRUE);settle(session);stopifnot(hires==before)
    })
  }
})
check('logout discards late coach completion and failure never causes mutation retry',{
  rm(list=ls(.coach_login_guards),envir=.coach_login_guards)
  shiny::testServer(function(input,output,session){a<-reactiveVal(auth);info<-reactiveVal(fresh_info());retries<-new.env();retries$n<-0L
    source<-coach_on_login_source(session,a,function()'c',function()'t',info,function(){retries$n<-retries$n+1L})},{
    settle(session);source$set_preference(source$key(),TRUE);settle(session);stopifnot(hires==2L)
    a(NULL);settle(session);resolve_hire(TRUE);settle(session);stopifnot(retries$n==0L,source$status()=='Sign in to check coach')
  })
})
fm_async_hire_coach<-old_hire
old_async<-async_adapter_fetch;old_persist<-persist_official_award_snapshot
requests<-0L;running<-0L;maximum<-0L;saves<-list();read_count<-0L
async_adapter_fetch<-function(fn,args,context,...){
  if(fn=='get_mvp_saved_catalog'){read_count<<-read_count+1L;return(promises::promise_resolve(list(saved=data.frame(),saved_complete=TRUE,catalog=list(rounds=data.frame(round_id=c('r1','r2','r3'),round_number=1:3,is_finished=c(TRUE,FALSE,FALSE),status=c('done','playing','verified_future')),complete=TRUE,reason=''))))}
  stopifnot(fn=='get_official_dreamteam_result');requests<<-requests+1L;running<<-running+1L;maximum<<-max(maximum,running)
  promises::promise(function(resolve,reject)later::later(function(){running<<-running-1L;resolve(normalize_official_dreamteam(xi(),args$championship_id,args$reference))},.03))
}
persist_official_award_snapshot<-function(result,login,championship_id,reference,context,on_complete){saves[[reference$round_id]]<<-on_complete;TRUE}
check('Admin loads bulk coverage once, fetches only on click, limits concurrency and awaits durable save',{
  shiny::testServer(function(input,output,session){enabled<-reactiveVal(TRUE);cid<-reactiveVal('c');source<-shared_mvp_source(session,function()auth,cid,enabled)},{
    settle(session,function()!source()$loading);stopifnot(read_count==1L,requests==0L,nrow(source()$coverage)==3L)
    attr(source,'retrieve')('missing');settle(session,function()length(saves)==2L)
    stopifnot(requests==2L,maximum==2L,source()$saved==0L,source()$running,!any(source()$coverage$persistence%in%'Saved'))
    stopifnot(!attr(source,'retrieve')('missing'))
    saves[['r1']](TRUE);saves[['r2']](FALSE);settle(session,function()!source()$running)
    stopifnot(source()$saved==1L,source()$failed==1L,source()$unsaved==1L,!source()$complete,
      source()$coverage$selection[3]=='Not due',source()$coverage$persistence[2]=='Save failed',source()$coverage$finality[2]=='Provisional / in progress')
    before<-requests;attr(source,'retry_save')();settle(session);saves[['r2']](TRUE);settle(session,function()!source()$running)
    stopifnot(requests==before,source()$unsaved==0L,source()$complete,all(source()$coverage$persistence[1:2]=='Saved'))
    enabled(FALSE);settle(session);enabled(TRUE);settle(session);stopifnot(read_count==1L,requests==2L)
    cid('other');settle(session,function()!source()$loading);stopifnot(read_count==2L,nrow(source()$rows)==0L)
  })
})
async_adapter_fetch<-old_async;persist_official_award_snapshot<-old_persist
check('captured compact catalog preserves unknown finality and distinct labels',{
  active<-list(championships=list(list(id='c',league='l',userteam=list(id='t'))),rounds=list(list(`_id`='r12',number=12)))
  leagues<-list(list(`_id`='l',rounds=lapply(1:38,function(i)list(`_id`=paste0('r',i),status='future'))))
  catalog<-normalize_mvp_catalog(active,leagues,'c')
  stopifnot(nrow(catalog$rounds)==38L,sum(is.finite(catalog$rounds$round_number))==1L,
    all(is.na(catalog$rounds$is_finished)),catalog$current_round_number==12L,catalog$user_team_id=='t')
  enriched<-award_catalog_numbers(catalog$rounds,data.frame(round_id=c('r1','r1','r2','r2'),round_number=c(1,1,2,99)))
  stopifnot(enriched$round_number[1]==1L,is.na(enriched$round_number[2]),enriched$round_number[12]==12L)
  coverage<-award_coverage(catalog$rounds,data.frame())
  labels<-award_round_labels(coverage$round_number,coverage$round_id)
  stopifnot(all(coverage$required),all(coverage$selection=='Missing'),!anyDuplicated(labels),!any(grepl('Round NA',labels)))
})
old_api<-mvp_api_answer;old_async<-async_adapter_fetch;old_save<-persist_official_award_snapshot
check('explicit acquisition hydrates verified round IDs, saves historical awards and skips verified later rounds',{
  clear_api_cache();schedule_queries<-character();award_queries<-character();saved_numbers<-numeric()
  active<-list(championships=list(list(id='c',league='l',userteam=list(id='t'))),rounds=list(list(`_id`='r2',number=2)))
  catalog<-normalize_mvp_catalog(active,list(list(`_id`='l',rounds=lapply(1:3,function(i)list(`_id`=paste0('r',i),status='future')))),'c')
  mvp_api_answer<-function(login,url,query){
    if(grepl('/match/list$',url)){
      stopifnot(identical(query$userteamId,'t'));schedule_queries<<-c(schedule_queries,query$roundId)
      return(list(status='ok',answer=list(round=list(`_id`=query$roundId,number=as.numeric(sub('r','',query$roundId))))))
    }
    award_queries<<-c(award_queries,query$round);list(status='ok',answer=xi())
  }
  async_adapter_fetch<-function(fn,args,context,...){
    if(fn=='get_mvp_saved_catalog')return(promises::promise_resolve(list(saved=data.frame(),saved_complete=TRUE,catalog=catalog)))
    stopifnot(fn=='get_official_dreamteam_result');promises::promise_resolve(do.call(get_official_dreamteam_result,args))
  }
  persist_official_award_snapshot<-function(result,login,championship_id,reference,context,on_complete){
    saved_numbers<<-c(saved_numbers,reference$round_number);on_complete(TRUE);TRUE
  }
  shiny::testServer(function(input,output,session){source<-shared_mvp_source(session,function()auth,function()'c',function()TRUE)},{
    settle(session,function()!source()$loading);stopifnot(!length(schedule_queries),!length(award_queries))
    attr(source,'retrieve')('missing');settle(session,function()!source()$running)
    stopifnot(setequal(schedule_queries,c('r1','r3')),setequal(award_queries,c('r1','r2')),
      setequal(saved_numbers,1:2),source()$saved==2L,source()$failed==0L,
      source()$coverage$selection[3]=='Not due',all(source()$coverage$round_number==1:3),
      all(is.na(source()$rows$is_finished)),all(source()$coverage$persistence[1:2]=='Saved'))
  })
})
check('mismatched schedule identity never assigns its round number',{
  clear_api_cache()
  mvp_api_answer<-function(...)list(status='ok',answer=list(round=list(`_id`='foreign',number=99)))
  reference<-list(round_id='r1',round_number=NA_real_,is_finished=NA,user_team_id='t')
  result<-get_award_round_reference(auth,'c',reference)
  stopifnot(is.na(result$round_number),identical(result$round_id,'r1'),is.na(result$is_finished))
})
mvp_api_answer<-old_api;async_adapter_fetch<-old_async;persist_official_award_snapshot<-old_save
cat(sprintf('ADMIN AWARDS / COACH: %d passed / 0 failed\n',n))
