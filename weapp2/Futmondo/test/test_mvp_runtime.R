#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R');library(shiny)})
n <- 0L
check <- function(name,expr) {force(expr);n <<- n+1L;cat('PASS',name,'\n')}
active <- list(championships=list(list(id='c',league='l')),rounds=list(list(`_id`='r1',number=1)))
leagues <- list(list(`_id`='l',rounds=list(list(`_id`='r1',status='done'),list(`_id`='r2',number=2,status='future'))))
ref <- round_reference('r1',1)
answer <- list(mvp=list(`_id`='p'),players=list(list(`_id`='p',name='Player',points=0),list(id='q',name='Other')))
official <- normalize_official_dreamteam(answer,'c',ref)$rows
official <- official[official$is_mvp,,drop=FALSE]
check('catalog matches league, recognizes done, joins immutable round metadata', {
  x <- normalize_mvp_catalog(active,leagues,'c')
  stopifnot(x$complete,x$rounds$is_finished[1],!x$rounds$is_finished[2],x$rounds$round_number[1]==1)
  stopifnot(round_is_finished(list(status='done')))
  upcoming <- active;upcoming$rounds[[1]]$beginProcess <- '2026-10-01T00:00:00Z'
  pending <- leagues;pending[[1]]$rounds[[1]]$status <- NULL;pending[[1]]$rounds[[1]]$isFinished <- FALSE
  x <- normalize_mvp_catalog(upcoming,pending,'c')
  stopifnot(x$complete,!x$rounds$is_finished[1],x$rounds$begin_process[1]=='2026-10-01T00:00:00Z')
})
check('upcoming-only and unknown catalogs cannot claim historical absence', {
  stopifnot(!normalize_mvp_catalog(active,list(),'c')$complete)
  bad <- leagues;bad[[1]]$rounds[[1]]$status <- NULL
  stopifnot(!normalize_mvp_catalog(active,bad,'c')$complete)
  stopifnot(!normalize_mvp_catalog(active,leagues,'other')$complete)
  missing <- active;missing$championships[[1]]$league <- NULL
  stopifnot(!normalize_mvp_catalog(missing,list(list(rounds=list())),'c')$complete)
})
check('official parsing preserves identity zero and missing scores; rejects errors and malformed data', {
  x <- normalize_official_dreamteam(answer,'c',ref)
  stopifnot(x$status=='ok',x$rows$points[1]==0,is.na(x$rows$points[2]),sum(x$rows$is_mvp)==1)
  a <- answer;a$players <- data.frame(id=c('p','q'),name=c('Player','Other'),points=c(0,NA))
  stopifnot(normalize_official_dreamteam(a,'c',ref)$status=='ok')
  stopifnot(normalize_official_dreamteam(list(error=TRUE),'c',ref)$status=='rejected',
    normalize_official_dreamteam(list(),'c',ref)$status=='unsupported')
  missing <- answer;missing$mvp <- list()
  stopifnot(normalize_official_dreamteam(missing,'c',ref)$status=='unsupported')
})
eligibility <- data.frame(championship_id='c',round_id='r1',player_id='p',team_id='old-owner',eligible=TRUE,verified=TRUE)
rewards <- data.frame(championship_id='c',round_id='r1',amount=100,historical=TRUE)
payments <- data.frame(championship_id='c',round_id='r1',player_id='p',team_id='old-owner',amount=120,bonus_type='mvp')
check('historical eligibility attributes estimated award and current ownership is irrelevant', {
  x <- calculate_mvp_bonuses(official,'c',eligibility,rewards,coverage_complete=TRUE)
  stopifnot(x$complete,x$rows$team_id=='old-owner',x$rows$amount==100,x$rows$status=='estimated')
  no <- calculate_mvp_bonuses(official,'c',rewards=rewards,coverage_complete=TRUE)
  stopifnot(!no$complete,is.na(no$rows$amount),no$rows$status=='unresolved')
})
check('recorded payments override estimates and duplicate evidence is counted once', {
  x <- calculate_mvp_bonuses(official,'c',eligibility,rewards,rbind(payments,payments),TRUE)
  stopifnot(nrow(x$rows)==1,x$rows$amount==120,x$rows$status=='recorded')
  other <- payments;other$championship_id <- 'other'
  stopifnot(calculate_mvp_bonuses(official,'c',payments=other)$rows$status=='unresolved')
})
check('unknown coverage and changed rules remain explicit; verified disabled reward is zero', {
  current <- rewards;current$historical <- FALSE
  x <- calculate_mvp_bonuses(official,'c',eligibility,current)
  stopifnot(!x$complete,grepl('assumption',x$rows$source))
  zero <- rewards;zero$amount <- 0
  x <- calculate_mvp_bonuses(official,'c',rewards=zero,coverage_complete=TRUE)
  stopifnot(x$complete,x$rows$amount==0,x$rows$status=='disabled')
})
check('fund subtotal adds award once and leaves observed cash unchanged', {
  f <- data.frame(teamid=c('old-owner','new-owner'),budget=c(0,-5),initial_budget=1000,total_spent=300,total_sales=20,point_bonus=10)
  a <- calculate_mvp_bonuses(official,'c',eligibility,rewards,payments,TRUE)
  out <- add_mvp_finance_estimates(f,a)
  stopifnot(identical(out$budget,f$budget),identical(out$mvp_recorded,c(120,0)),identical(out$known_funds_subtotal,c(850,730)))
})
check('sync distinguishes queued confirmed and failed writes', {
  old_fetch <- get_official_dreamteam_result;old_post <- supabase_post
  get_official_dreamteam_result <- function(...) mvp_result('ok',official)
  supabase_post <- function(...) TRUE
  stopifnot(mvp_sync_round(NULL,'c','r1',1)$queued==1,mvp_sync_round(NULL,'c','r1',1)$saved==0)
  supabase_post <- function(...) 204L
  stopifnot(mvp_sync_round(NULL,'c','r1',1)$saved==1)
  supabase_post <- function(...) 400L
  stopifnot(mvp_sync_round(NULL,'c','r1',1)$failed==1)
  get_official_dreamteam_result <- old_fetch;supabase_post <- old_post
})
check('confirmed MVP invalidation clears only saved-MVP reads', {
  api_cache_env[['account:u:round_mvps_c']] <- list(data=official,time=Sys.time())
  api_cache_env[['account:u:other']] <- list(data=1,time=Sys.time())
  invalidate_mvp_saved_cache()
  stopifnot(!exists('account:u:round_mvps_c',api_cache_env,inherits=FALSE),exists('account:u:other',api_cache_env,inherits=FALSE))
})
check('unclassified bonuses never become MVP payment evidence', {
  old <- get_user_team_moneymovements
  get_user_team_moneymovements <- function(...) data.frame(category='bonus',money=100,concept='MVP Player')
  stopifnot(nrow(get_mvp_payment_evidence(NULL,'c','t'))==0L)
  get_user_team_moneymovements <- function(...) data.frame(bonus_type='mvp',round_id='r1',player_id='p',money=100)
  p <- get_mvp_payment_evidence(NULL,'c','t')
  stopifnot(p$team_id=='t',p$championship_id=='c',p$amount==100)
  get_user_team_moneymovements <- old
})
settle_mvp<-function(session,ready,timeout=3){deadline<-Sys.time()+timeout;repeat{session$flushReact();later::run_now(timeoutSecs=0.01);session$flushReact();if(isTRUE(shiny::isolate(ready())))return(invisible(TRUE));if(Sys.time()>=deadline)stop('Timed out settling official MVP source')}}
pump_mvp<-function(session){for(i in seq_len(5L)){session$flushReact();later::run_now(timeoutSecs=0);session$flushReact()}}
check('shared source recovers on refresh, survives save failure and isolates league switches',{
 old_catalog<-get_finished_round_catalog;old_fetch<-get_official_dreamteam_result;old_saved<-get_round_mvps;old_post<-supabase_post
 calls<-0L;catalog_calls<-0L;writes<-0L;failed<-TRUE
 get_round_mvps<-function(...)stop('database unavailable')
 get_finished_round_catalog<-function(login,championship_id){catalog_calls<<-catalog_calls+1L;if(failed)return(list(status='connection',rounds=empty_finished_rounds(),complete=FALSE,reason='Connection failed'));fixture<-active;fixture$championships[[1]]$id<-championship_id;normalize_mvp_catalog(fixture,leagues,championship_id)}
 get_official_dreamteam_result<-function(login,championship_id,reference){calls<<-calls+1L;normalize_official_dreamteam(answer,championship_id,reference)}
 supabase_post<-function(...){writes<<-writes+1L;FALSE}
 testServer(function(input,output,session){enabled<-reactiveVal(TRUE);cid<-reactiveVal('c');refresh<-reactiveVal(0);source<-shared_mvp_source(session,function()c(userid='u',token='t'),cid,enabled,refresh)}, {
  settle_mvp(session,function()identical(source()$status,'partial'));stopifnot(!source()$complete,grepl('Connection failed',source()$reason),calls==0L,catalog_calls==1L)
  failed<<-FALSE;refresh(1);settle_mvp(session,function()isTRUE(source()$complete));stopifnot(source()$status=='ok',nrow(source()$rows)==1L,source()$rows$points==0,calls==1L,writes==1L,catalog_calls==2L)
  enabled(FALSE);pump_mvp(session);stopifnot(calls==1L,catalog_calls==2L)
  cid('other');enabled(TRUE);session$flushReact();stopifnot(!any(source()$rows$championship_id%in%'c'));settle_mvp(session,function()isTRUE(source()$complete));stopifnot(nrow(source()$rows)==1L,all(source()$rows$championship_id=='other'),calls==2L,writes==2L,catalog_calls==3L)
 })
 get_finished_round_catalog<-old_catalog;get_official_dreamteam_result<-old_fetch;get_round_mvps<-old_saved;supabase_post<-old_post
})
check('inactive consumers start no reads; active consumers share saved rows without round refetch',{
 old_catalog<-get_finished_round_catalog;old_fetch<-get_official_dreamteam_result;old_saved<-get_round_mvps
 calls<-0L;saved_calls<-0L;round_calls<-0L
 get_round_mvps<-function(...){saved_calls<<-saved_calls+1L;official}
 get_finished_round_catalog<-function(...){calls<<-calls+1L;normalize_mvp_catalog(active,leagues,'c')}
 get_official_dreamteam_result<-function(...){round_calls<<-round_calls+1L;stop('saved round must not be refetched')}
 testServer(function(input,output,session){enabled<-reactiveVal(FALSE);first<-moduleServer('first',function(input,output,session)shared_mvp_source(session,function()c(userid='u',token='t'),function()'c',enabled));second<-moduleServer('second',function(input,output,session)shared_mvp_source(session,function()c(userid='u',token='t'),function()'c',function()FALSE))},{
  pump_mvp(session);stopifnot(calls==0L,saved_calls==0L,round_calls==0L)
  enabled(TRUE);settle_mvp(session,function()isTRUE(first()$complete));stopifnot(calls==1L,saved_calls==1L,round_calls==0L,nrow(first()$rows)==1L,first()$rows$points==0,identical(first(),second()))
  enabled(FALSE);pump_mvp(session);stopifnot(calls==1L,saved_calls==1L,round_calls==0L)
  enabled(TRUE);pump_mvp(session);stopifnot(calls==1L,saved_calls==1L,round_calls==0L,first()$complete,identical(first(),second()))
 })
 get_finished_round_catalog<-old_catalog;get_official_dreamteam_result<-old_fetch;get_round_mvps<-old_saved
})
cat(sprintf('MVP: %d passed / 0 failed\n',n))
