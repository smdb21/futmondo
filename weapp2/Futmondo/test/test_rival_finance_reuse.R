options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
record_acquired_resource<-function(...)TRUE
persist_rival_finance_evidence<-function(...)TRUE
teams<-data.frame(teamid=c('a','b','c'),teamname=c('A','B','C'),points=1:3)
rules<-list(initial_budget=100,money_per_point=0,ranking_pool=0)
transfer<-function(id,buyer,price)data.frame(id=id,created='2026-11-03T10:00:00Z',player_id='p',player_name='Player',buyer_team_id=buyer,buyer_team_name=toupper(buyer),seller_team_id='',seller_team_name='Market',price=price,is_clause=FALSE)
p<-transfer('one','a',10);cache<-rival_calculation_cache()
args<-list(teams=teams,championship_id='c',rules=rules,account_id='u',pressroom=p)
build<-function()do.call(build_rival_cash_dataset,c(args,list(cache=cache)))
x<-build();y<-build();stopifnot(identical(x,y),cache$preparations==1L,cache$team_builds==3L,cache$hits==3L)
args$pressroom<-rbind(p,transfer('two','b',5));z<-build()
stopifnot(cache$preparations==1L,cache$team_builds==4L,identical(z,do.call(build_rival_cash_dataset,args)),z$team_finances$estimated_cash[2]==95)
args$pressroom$price[2]<-6;z<-build();stopifnot(cache$team_builds==5L,z$team_finances$estimated_cash[2]==94,identical(z,do.call(build_rival_cash_dataset,args)))
args$rules$initial_budget<-200;z<-build();stopifnot(cache$preparations==2L,cache$team_builds==8L,identical(z,do.call(build_rival_cash_dataset,args)))
args$account_id<-'other';z<-build();stopifnot(cache$preparations==3L,cache$team_builds==11L)
cat('PASS unchanged reuse, one affected team per new/revised transfer, rules invalidation and account isolation\n')

# Existing DB recovery: real source identities only, explicit opening boundary,
# empty parties are Market, and ordinary sales/purchases survive a failed live read.
stamp<-as.POSIXct('2026-08-01',tz='UTC')
e<-list(rival_finance_evidence('u','c','unknown','movements','opening',list(type='budget',money=100,date='2026-08-01T00:00:00Z'),stamp))
oldread<-history_read_pages
history_read_pages<-function(table,query,...){
 stopifnot(table=='market_transactions',query$championship_id=='eq.c',query$transaction_date=='gte.2026-08-01T00:00:00Z',query$source_event_id=='not.is.null')
 data.frame(source_event_id=c('700000000000000000000001','legacy'),player_id='p',buyer_team_id='a',seller_team_id=NA_character_,price=c(20,999),is_clause=FALSE,transaction_date='2026-08-02T00:00:00Z',player=I(list(list(name='Player'),list(name='Player'))),buyer=I(list(list(name='A'),list(name='A'))),seller=I(list(NULL,NULL)))
}
saved<-get_rival_saved_transfers('c',e);stopifnot(nrow(saved)==1L,saved$price==20,saved$seller_team_id=='',saved$seller_team_name=='Futmondo / Mercado')
stopifnot(nrow(get_rival_saved_transfers('c',list()))==0)
history_read_pages<-oldread
cat('PASS saved ordinary history uses existing source IDs and recorded reset boundary\n')

# A concurrent successful transfer read must repair a previously failed seed
# without any new API request or user selection.
clear_api_cache();calls<-0L
async_adapter_fetch<-function(fn,args,context,...){
 calls<<-calls+1L
 value<-switch(fn,get_rival_finance_saved=list(evidence=list(),pressroom=p),
  get_rival_finance_seed=list(rules=rules,pressroom=data.frame()),
  get_rival_team_finance_observation=list(team_id=args$user_team_id,info=NULL,movements=NULL,rounds=NULL),stop(fn))
 promises::promise_resolve(value)
}
flush<-function(session){for(i in seq_len(15)){later::run_now(.01);session$flushReact()}}
shiny::testServer(function(input,output,session){
 auth<-shiny::reactiveVal(c(userid='u',token='fixture'))
 feed<-shared_rival_finance_source(session,auth,function()'c',function()'a',function()teams,function()TRUE,function()list(rows=data.frame(),complete=FALSE))
},{
 flush(session);stopifnot(feed()$team_finances$estimated_cash[1]==90)
 cache_context<-resource_context(account_id='u',championship_id='c',user_team_id='a',season='unknown')
 cached<-api_cache_env[[api_cache_key(paste0('rival_calculation_',rival_calculation_version),context=cache_context)]]
 stopifnot(is.list(cached$data$teams),length(cached$data$teams)==3L)
 before<-calls;fresh<-rbind(p,transfer('three','a',20));attr(fresh,'fetch_status')<-'ok'
 context<-resource_context(c(userid='u',token='fixture'),'c')
 resource_store(api_cache_key('pressroom_c',context=context),fresh,context,'pressroom_c')
 flush(session);stopifnot(feed()$team_finances$estimated_cash[1]==70,calls==before)
 other<-resource_context(c(userid='other',token='fixture'),'c')
 resource_store(api_cache_key('pressroom_c',context=other),transfer('bad','a',99),other,'pressroom_c')
 flush(session);stopifnot(feed()$team_finances$estimated_cash[1]==70,calls==before)
})
cat('PASS concurrent live recovery updates balances with no new requests and no cross-account leakage\n')
