# Browsing target facts must not activate expired financial worker observations.
# The draft can reuse a saved league rule without sourcing finances.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
now<-base::Sys.time();Sys.time<-function()now
auth<-c(userid='availability-recovery-fixture',token='synthetic')
account<-fm_worker_account_state(auth[['userid']])
account$known<-TRUE;account$account<-list(enabled=TRUE,auth_status='active')
record_acquired_resource<-function(...)invisible(TRUE)
reads<-list();rpc<-character();modal<-NULL
showModal<-function(ui,...)modal<<-as.character(ui)
fm_worker_rpc<-function(name,...){
 rpc<<-c(rpc,name);stopifnot(name=='read')
 promises::promise_resolve(list(account=account$account,resources=list(),jobs=list(),health=NULL))
}
capacity<-function(valid=15,status='ok',budget=1000,age=0){
 observed_data(list(status=status,valid_until=as.numeric(now)+valid,
  roster=list(status='ok',count=10,effective_count=10,cap=20),
  funds=list(spendable_budget=budget),outstanding=list(count=0),
  rules=list(minimum_bid_at_value=TRUE)),observed_at=now-age)
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
async_adapter_fetch<-function(fn,args=list(),context,worker_bypass=FALSE,is_current=function()TRUE,...){
 reads[[length(reads)+1L]]<<-list(fn=fn,bypass=worker_bypass)
 if(fn=='get_team_acquisition_capacity')stop('An offer draft must not acquire live financial facts')
 value<-switch(fn,get_player_summary=list(data=list(id=context$player_id),championship=list(owner=list(computer=TRUE),clause=list(price=400,transferred=FALSE,date='2026-11-14T00:00:00Z')),market=list(p=150,bids=list()),points=list(),prices=list()),
  get_lineup_from_team=list(pointSystem='fixture'),get_player_full_profile=list(profile=list()),
  get_player_matches=list(matches=list()),get_my_market_players=data.frame(),read_card_round_history=data.frame(),
  read_card_price_history=data.frame(),stop(fn))
 promises::promise_resolve(value)
}
player<-data.frame(id='p',name='Fixture',role='delantero',team='Club',value=200,
 price=150,change=0,points=10,user_team_id='rival',market_inMarket=TRUE,
 clause_price=400,clause_transferred=FALSE)
selected<-reactiveVal(player);team<-reactiveVal('mine')
settle<-function(session)for(i in 1:30){later::run_now(.001);session$flushReact()}
button<-function(html,id){
 pattern<-paste0('<button\\b[^>]*\\bid="[^"]*',id,'"[^>]*>')
 match<-regmatches(html,regexpr(pattern,html,perl=TRUE))
 if(!length(match))return(NULL)
 match
}
enabled<-function(html,id){tag<-button(html,id);!is.null(tag)&&!grepl('\\bdisabled(?:=|\\s|>)',tag,perl=TRUE)}
refresh<-reactiveVal(0L)
reset<-function(value=capacity(-30,age=45)){
 clear_api_cache();reads<<-list();rpc<<-character()
 rm(list=ls(.fm_worker$values),envir=.fm_worker$values)
 rm(list=auth[['userid']],envir=.fm_worker$accounts)
 account<<-fm_worker_account_state(auth[['userid']])
 account$known<-TRUE;account$account<-list(enabled=TRUE,auth_status='active')
 selected(player);team('mine');refresh(0L);put_capacity(value,paste0('fixture-',runif(1)))
}
args<-list(selected_player=selected,login_token=reactive(auth),championship_id=reactive('league'),
 user_team_id=team,refresh_trigger=refresh)
reset()
shiny::testServer(selected_player_Server,args=args,{
 settle(session)
 stopifnot(enabled(output$action_buttons$html,'btn_bid_market'),
  is.null(button(output$action_buttons$html,'btn_verify_availability')))
 before <- reads; before_rpc <- rpc
 session$setInputs(btn_bid_market=1L);settle(session)
 stopifnot(identical(before,reads),isTRUE(offer_modal_opened_RV()),
  grepl('Saved minimum market bid',modal,fixed=TRUE),grepl('value="200"',modal,fixed=TRUE),
  card_states$capacity()$data$valid_until<as.numeric(now),identical(rpc,before_rpc))
 cat('PASS a saved rule sets the draft amount without extending its expired capacity deadline or acquiring source data\n')
 team('another');settle(session)
 stopifnot(is.null(market_offer_dialog_RV()))
 before <- reads
 session$setInputs(btn_bid_market=2L);settle(session)
 stopifnot(identical(before,reads),grepl('Listed price',modal,fixed=TRUE),
  grepl('value="150"',modal,fixed=TRUE),is.null(card_states$capacity()$data))
 cat('PASS a new team uses its own listing and cannot reuse capacity from the previous team\n')
})
