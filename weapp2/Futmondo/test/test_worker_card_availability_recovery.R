# Stored worker observations may outlive the short transaction deadline. The
# card must expose a real, enabled verification control before accepting input.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
now<-base::Sys.time();Sys.time<-function()now
auth<-c(userid='availability-recovery-fixture',token='synthetic')
account<-fm_worker_account_state(auth[['userid']])
account$known<-TRUE;account$account<-list(enabled=TRUE,auth_status='active')
record_acquired_resource<-function(...)invisible(TRUE)
jobs<-list();reads<-list();rpc<-character();writes<-0L
fm_worker_rpc<-function(name,...){
 rpc<<-c(rpc,name);stopifnot(name=='read')
 promises::promise_resolve(list(account=account$account,resources=list(),jobs=list(),health=NULL))
}
capacity<-function(valid=15,status='ok',budget=1000,age=0){
 observed_data(list(status=status,valid_until=as.numeric(now)+valid,
  roster=list(status='ok',count=10,effective_count=10,cap=20),
  funds=list(spendable_budget=budget),outstanding=list(count=0),
  rules=list(minimum_bid_at_value=FALSE)),observed_at=now-age)
}
put_capacity<-function(value,version,team='mine'){
 ctx<-resource_context(auth,'league',team)
 d<-fm_worker_descriptor('get_team_acquisition_capacity',
  list(login=auth,championship_id='league',user_team_id=team),ctx)
 attr(value,'valid_until')<-value$valid_until
 fm_worker_put(auth[['userid']],d$resource_key,list(data=value,manifest=list(version=version)))
 rev<-fm_worker_resource_revision(account,d$resource_key);rev(shiny::isolate(rev())+1L)
 invisible(d)
}
put_capacity(capacity(-30,age=45),'expired')
real_async<-async_adapter_fetch
async_adapter_fetch<-function(fn,args=list(),context,worker_bypass=FALSE,is_current=function()TRUE,...){
 reads[[length(reads)+1L]]<<-list(fn=fn,bypass=worker_bypass)
 if(fn=='get_team_acquisition_capacity'){
  if(worker_bypass)return(promises::promise(function(resolve,reject){
   jobs[[length(jobs)+1L]]<<-list(resolve=resolve,reject=reject,current=is_current,context=context)
  }))
  if(context$user_team_id=='another')return(promises::promise_resolve(NULL))
  if(identical(fm_worker_mode(context$account_id),FALSE))return(promises::promise_resolve(capacity(budget=800)))
  return(real_async(fn,args,context,worker_bypass=FALSE,is_current=is_current,...))
 }
 value<-switch(fn,get_player_summary=list(championship=list(),points=list(),prices=list()),
  get_lineup_from_team=list(pointSystem='fixture'),get_player_full_profile=list(profile=list()),
  get_player_matches=list(matches=list()),read_card_round_history=data.frame(),
  read_card_price_history=data.frame(),stop(fn))
 promises::promise_resolve(value)
}
buy_clause<-function(...){writes<<-writes+1L;stop('No transaction belongs to verification')}
player<-data.frame(id='p',name='Fixture',role='delantero',team='Club',value=200,
 price=150,change=0,points=10,user_team_id='rival',market_inMarket=TRUE,
 clause_price=400,clause_transferred=FALSE)
selected<-reactiveVal(player);team<-reactiveVal('mine')
settle<-function(session)for(i in 1:30){later::run_now(0);session$flushReact()}
button<-function(html,id){
 pattern<-paste0('<button\\b[^>]*\\bid="[^"]*',id,'"[^>]*>')
 match<-regmatches(html,regexpr(pattern,html,perl=TRUE))
 if(!length(match))return(NULL)
 match
}
enabled<-function(html,id){tag<-button(html,id);!is.null(tag)&&!grepl('\\bdisabled(?:=|\\s|>)',tag,perl=TRUE)}
blocked<-function(html,id){tag<-button(html,id);is.null(tag)||grepl('\\bdisabled(?:=|\\s|>)',tag,perl=TRUE)}
shiny::testServer(selected_player_Server,args=list(selected_player=selected,
 login_token=reactive(auth),championship_id=reactive('league'),user_team_id=team),{
 settle(session)
 stopifnot(enabled(output$action_buttons$html,'btn_verify_availability'),
  blocked(output$action_buttons$html,'btn_bid_market'),blocked(output$action_buttons$html,'btn_pay_clause'),
  !length(jobs),!length(rpc),sum(vapply(reads,function(x)x$fn=='get_team_acquisition_capacity',logical(1)))==1L)
 cat('PASS expired stored capacity exposes an enabled verification button while purchase controls remain disabled; passive opening acquires nothing\n')

 failed_saved<-capacity(-30,status='partial',age=45)
 failed_saved$diagnostics<-'reported budget unavailable'
 put_capacity(failed_saved,'expired-partial');settle(session)
 stopifnot(!grepl('Verify availability to check your current roster, funds and offers.',output$action_buttons$html,fixed=TRUE),
  blocked(output$action_buttons$html,'btn_bid_market'),enabled(output$action_buttons$html,'btn_verify_availability'))
 failed_saved<-capacity(-30,age=45)
 attr(failed_saved,'fetch_error')<-resource_failure(resource='get_team_acquisition_capacity',category='network')
 put_capacity(failed_saved,'expired-error');settle(session)
 stopifnot(!grepl('Verify availability to check your current roster, funds and offers.',output$action_buttons$html,fixed=TRUE),
  blocked(output$action_buttons$html,'btn_bid_market'),enabled(output$action_buttons$html,'btn_verify_availability'))
 failed_saved<-capacity(-30,age=45)
 attr(failed_saved,'worker_completeness')<-'partial'
 attr(failed_saved,'worker_acquisition_status')<-'partial'
 put_capacity(failed_saved,'expired-incomplete-envelope');settle(session)
 stopifnot(!grepl('Verify availability to check your current roster, funds and offers.',output$action_buttons$html,fixed=TRUE),
  blocked(output$action_buttons$html,'btn_bid_market'),enabled(output$action_buttons$html,'btn_verify_availability'))
 put_capacity(capacity(-30,age=45),'expired-recovered');settle(session)
 cat('PASS expired partial or failed stored observations retain failure presentation instead of being described as ordinary aging\n')

 session$setInputs(btn_verify_availability=1L);settle(session)
 stopifnot(length(jobs)==1L,blocked(output$action_buttons$html,'btn_verify_availability'),
  blocked(output$action_buttons$html,'btn_bid_market'),blocked(output$action_buttons$html,'btn_pay_clause'))
 session$setInputs(btn_verify_availability=2L);settle(session)
 heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,0);settle(session)
 stopifnot(length(jobs)==1L,heartbeat,writes==0L)
 jobs[[1L]]$resolve(capacity());settle(session)
 stopifnot(enabled(output$action_buttons$html,'btn_bid_market'),enabled(output$action_buttons$html,'btn_pay_clause'),
  !grepl('Availability checks expired',output$action_buttons$html,fixed=TRUE),writes==0L)
 cat('PASS one held live request coalesces duplicate verification input, leaves the event loop responsive and enables market/clause actions after success\n')

 put_capacity(capacity(-30,budget=99,age=45),'older-replay');settle(session)
 stopifnot(card_states$capacity()$data$funds$spendable_budget==1000,
  enabled(output$action_buttons$html,'btn_bid_market'),length(jobs)==1L,!length(rpc))
 now<<-now+1;put_capacity(capacity(budget=900),'newer-publication');settle(session)
 stopifnot(card_states$capacity()$data$funds$spendable_budget==900,
  enabled(output$action_buttons$html,'btn_bid_market'),length(jobs)==1L)
 cat('PASS older worker replay cannot replace a fresh live result; a newer stored observation can replace it without source acquisition\n')

 now<<-now+16;put_capacity(capacity(-1,age=16),'expired-again');settle(session)
 stopifnot(enabled(output$action_buttons$html,'btn_verify_availability'),blocked(output$action_buttons$html,'btn_bid_market'))
 session$setInputs(btn_verify_availability=3L);settle(session)
 jobs[[2L]]$reject(resource_failure(resource='get_team_acquisition_capacity',category='network'));settle(session)
 stopifnot(length(jobs)==2L,enabled(output$action_buttons$html,'btn_verify_availability'),
  blocked(output$action_buttons$html,'btn_bid_market'),blocked(output$action_buttons$html,'btn_pay_clause'),writes==0L)
 session$setInputs(btn_verify_availability=4L);settle(session)
 jobs[[3L]]$resolve(capacity(status='partial',budget=NA_real_));settle(session)
 stopifnot(length(jobs)==3L,enabled(output$action_buttons$html,'btn_verify_availability'),
  blocked(output$action_buttons$html,'btn_bid_market'),blocked(output$action_buttons$html,'btn_pay_clause'))
 session$setInputs(btn_verify_availability=5L);settle(session)
 jobs[[4L]]$resolve(capacity(-1));settle(session)
 stopifnot(length(jobs)==4L,enabled(output$action_buttons$html,'btn_verify_availability'),
  blocked(output$action_buttons$html,'btn_bid_market'),writes==0L)
 cat('PASS failed, partial and already-expired live checks keep actions blocked and offer explicit retry without automatic loops\n')

 session$setInputs(btn_verify_availability=6L);settle(session)
 undated<-capacity();undated$valid_until<-NULL
 jobs[[5L]]$resolve(undated);settle(session)
 stopifnot(length(jobs)==5L,enabled(output$action_buttons$html,'btn_verify_availability'),
  blocked(output$action_buttons$html,'btn_bid_market'))
 session$setInputs(btn_verify_availability=7L);settle(session)
 invalid_deadline<-capacity();invalid_deadline$valid_until<-NA_real_
 jobs[[6L]]$resolve(invalid_deadline);settle(session)
 stopifnot(length(jobs)==6L,enabled(output$action_buttons$html,'btn_verify_availability'),
  blocked(output$action_buttons$html,'btn_bid_market'),writes==0L)
 cat('PASS successful-looking live responses with missing or invalid deadlines cannot authorize an acquisition\n')

 session$setInputs(btn_verify_availability=8L);settle(session)
 stopifnot(length(jobs)==7L,jobs[[7L]]$current())
 team('another');settle(session)
 stopifnot(!jobs[[7L]]$current(),enabled(output$action_buttons$html,'btn_verify_availability'))
 jobs[[7L]]$resolve(capacity(budget=9999));settle(session)
 stopifnot(is.null(card_states$capacity()$data),blocked(output$action_buttons$html,'btn_bid_market'),writes==0L)
 session$setInputs(btn_verify_availability=9L);settle(session)
 stopifnot(length(jobs)==8L,jobs[[8L]]$context$user_team_id=='another')
 jobs[[8L]]$resolve(capacity());settle(session)
 stopifnot(enabled(output$action_buttons$html,'btn_bid_market'),enabled(output$action_buttons$html,'btn_pay_clause'))
 cat('PASS a team change discards the old completion; missing stored capacity still exposes verification and can recover independently\n')

 owner_player<-player;owner_player$id<-'owner-only';owner_player$price<-0;owner_player$market_inMarket<-FALSE
 selected(owner_player);settle(session)
 stopifnot(enabled(output$action_buttons$html,'btn_verify_availability'),blocked(output$action_buttons$html,'btn_offer_owner'))
 session$setInputs(btn_verify_availability=10L);settle(session)
 stopifnot(length(jobs)==9L)
 jobs[[9L]]$resolve(capacity());settle(session)
 stopifnot(enabled(output$action_buttons$html,'btn_offer_owner'),enabled(output$action_buttons$html,'btn_pay_clause'),writes==0L)
 selected(NULL);settle(session)
 stopifnot(!jobs[[9L]]$current(),length(jobs)==9L,
  all(vapply(reads,function(x)!x$bypass||x$fn=='get_team_acquisition_capacity',logical(1))),
  !length(rpc),writes==0L)
 cat('PASS changing players resets live verification; owner and clause actions share successful recovery, and closing the card releases its scope\n')

 # Retain the real worker provider's missing/loading state separately. Mock
 # output extraction waits on its 120s provider promise, so missing DOM behavior
 # above uses a settled absent result; the browser fixture covers held loading.
 team('missing');selected(player);settle(session)
 stopifnot(card_states$capacity()$status=='loading',is.null(card_states$capacity()$data),length(account$waiters)>0L)
 put_capacity(capacity(-30,age=45),'missing-arrived',team='missing');fm_worker_settle(account);settle(session)
 stopifnot(enabled(output$action_buttons$html,'btn_verify_availability'),length(jobs)==9L)
 session$setInputs(btn_verify_availability=11L);settle(session)
 stopifnot(length(jobs)==10L,jobs[[10L]]$current())
 fm_worker_set_policy(account,list(enabled=FALSE,auth_status='active'));settle(session)
 stopifnot(!jobs[[10L]]$current(),card_states$capacity()$data$funds$spendable_budget==800,
  enabled(output$action_buttons$html,'btn_bid_market'),is.null(button(output$action_buttons$html,'btn_verify_availability')))
 jobs[[10L]]$resolve(capacity(budget=9999));settle(session)
 stopifnot(card_states$capacity()$data$funds$spendable_budget==800,writes==0L)
 cat('PASS real missing provider remains loading until publication; disabling worker mode discards held verification and restores normal reads\n')
})
