options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='fixture-user',token='synthetic-token')
catlog<-data.frame(round_id=c('r6','r11'),round_number=c(6,11),is_finished=TRUE,status='closed')
move<-function(id,round,type='ranking',date='2026-11-03T10:00:00Z')rival_finance_evidence('fixture-user','fixture-league','unknown','movements',id,
 list(team_id='reference',id=id,type=type,concept=round,money=100,date=date))
ev<-list(move('p6','6.2'),move('p11','11'))
rows<-data.frame(round_id=c('r6','r11'),checked_at=c('2026-11-03T11:00:00Z','2026-10-31T11:00:00Z'))
stopifnot(identical(award_settlement_rechecks(ev,rows,catlog),'r11'))
rows$checked_at[2]<-'2026-11-03T11:00:00Z'
stopifnot(length(award_settlement_rechecks(ev,rows,catlog))==0L,
 length(award_settlement_rechecks(list(move('points','11','points')),data.frame(),catlog))==0L)
mov<-rival_normalize_movements(rival_cash_latest_movements(ev,'reference'),catlog)
stopifnot(rival_reward_date('r6',rival_payment_anchors(mov))=='2026-11-03T10:00:00Z')
# Sanitized known-answer: revising the completed selection adds precisely one XI award.
xi<-function(player)list(players=lapply(1:11,function(i)list(id=if(i==1)player else paste0('p',i),name=paste('Player',i),points=i)),mvp='p2',pointSystem='custom',custom=list())
ref<-list(round_id='r11',round_number=11,is_finished=TRUE)
old<-normalize_official_dreamteam(xi('old-player'),'fixture-league',ref)
fresh<-normalize_official_dreamteam(xi('eligible-player'),'fixture-league',ref)
eligible<-data.frame(round_id='r11',player_id='eligible-player',team_id='rival',eligible=TRUE,verified=TRUE)
rules<-list(dreamteam_reward=500000,mvp_reward=1000000)
before<-sum(rival_award_estimates(old$rows,'rival',rules,eligible)$rows$amount)
after<-sum(rival_award_estimates(fresh$rows,'rival',rules,eligible)$rows$amount)
stopifnot(before==0,after==500000,33768672+after-before==34268672)
requests<-0L;saves<-0L;persisted<-FALSE
async_adapter_fetch<-function(fn,args,context,...){
 if(fn=='get_mvp_saved_catalog')return(promises::promise_resolve(list(saved=data.frame(),saved_complete=TRUE,
  settlement_rechecks=if(persisted)character()else'r11',catalog=list(rounds=catlog,complete=TRUE))))
 stopifnot(fn=='get_official_dreamteam_result',args$reference$round_id=='r11',args$reference$is_finished)
 requests<<-requests+1L
 promises::promise(function(resolve,reject)later::later(function()resolve(fresh),.03))
}
persist_official_award_snapshot<-function(result,login,championship_id,reference,context,on_complete){
 saves<<-saves+1L;persisted<<-TRUE;on_complete(TRUE);TRUE
}
settle<-function(session,source){deadline<-Sys.time()+3;repeat{
 session$flushReact();later::run_now(.01);session$flushReact()
 if(!shiny::isolate(source()$loading||source()$running))break
 if(Sys.time()>deadline)stop('timeout')
}}
for(i in 1:2)shiny::testServer(function(input,output,session){
 active<-reactiveVal(TRUE)
 src<-shared_mvp_source(session,function()auth,function()'fixture-league',active)
},{settle(session,src);stopifnot(requests==1L,saves==1L);active(FALSE);session$flushReact();active(TRUE);settle(session,src);stopifnot(requests==1L)})
cat('PASS settlement freshness, points-only exclusion, decimal dates, exact award delta, async acquisition and reuse across sessions\n')
