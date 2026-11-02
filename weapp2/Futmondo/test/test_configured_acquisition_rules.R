options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L;check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
# Every read is synthetic; attempts to bypass these read seams fail immediately.
for(method in c('GET','POST','PUT','PATCH','DELETE'))suppressMessages(trace(method,where=asNamespace('httr'),print=FALSE,tracer=quote(stop('Live HTTP forbidden'))))
auth<-c(userid='configured-fixture',token='synthetic')
info<-list(budget=100,withheld=50,teamValue=1000,maxBid=10000,configuration=list(maxPlayersInRoster=20,mnmp=.2,mbmv=FALSE))
reads<-0L
get_user_team_info<-function(...){reads<<-reads+1L;info}
get_players_from_team<-function(...)data.frame(id='owned',value=1000)
get_market_players<-function(...)data.frame(id='target',bid_id='own-bid',bid_price=50)
get_championship_pressroom<-function(...)data.frame(id=character(),player_id=character(),buyer_team_id=character(),seller_team_id=character(),created=character())
capacity<-function(){clear_api_cache();get_acquisition_capacity(auth,'fixture-league','mine','target')}
check('zero, twenty and fifty percent use configured borrowing across capacity and banner',{
 for(fraction in c(0,.2,.5)){
  info$configuration$mnmp<-fraction;cap<-capacity()
  expected<-50+1000*fraction
  stopifnot(cap$status=='ok',cap$funds$spendable_budget==expected,cap$funds$debt_limit==1000*fraction,
   cap$funds$debt_fraction==fraction,evaluate_acquisition_preflight(cap,'modify',expected+50,50)$ok,
   !evaluate_acquisition_preflight(cap,'modify',expected+51,50)$ok)
  display<-round_finance_display(resource_state(info),resource_state(get_market_players()))
  stopifnot(display$spendable_budget==expected)
 }
 before<-reads;get_acquisition_capacity(auth,'fixture-league','mine','another');stopifnot(reads==before)
})
check('missing malformed and negative borrowing settings cannot enable purchases',{
 for(fraction in list(NULL,NA_real_,'invalid',-1)){
  info$configuration$mnmp<-fraction;cap<-capacity()
  stopifnot(cap$status=='partial',is.na(cap$funds$spendable_budget),!evaluate_acquisition_preflight(cap,'bid',1)$ok,
   grepl('league borrowing allowance',acquisition_unavailable_message(cap),fixed=TRUE))
  display<-round_finance_display(resource_state(info),resource_state(get_market_players()))
  stopifnot(display$cash==100,display$projected_committed_balance==50,is.na(display$spendable_budget))
 }
 stopifnot(is.na(acquisition_headroom(100,1000)$spendable_budget))
})
check('reported API maximum still caps bidding and banner below configured headroom',{
 info$configuration$mnmp<-.5;info$maxBid<-120;cap<-capacity()
 stopifnot(cap$funds$spendable_budget==550,!evaluate_acquisition_preflight(cap,'bid',121)$ok,
  round_finance_display(resource_state(info),resource_state(get_market_players()))$spendable_budget==120)
 info$maxBid<-10000
})
check('market value floor follows mbmv while preserving explicit listing prices',{
 player<-list(value=200,price=150)
 for(flag in c(FALSE,TRUE)){
  info$configuration$mbmv<-flag;cap<-capacity();minimum<-market_bid_minimum(player,cap$rules$minimum_bid_at_value)
  stopifnot(minimum==if(flag)200 else 150,
   evaluate_acquisition_preflight(cap,'bid',minimum,minimum_bid=minimum)$ok,
   !evaluate_acquisition_preflight(cap,'bid',minimum-1,minimum_bid=minimum)$ok)
 }
 stopifnot(market_bid_minimum(list(value=100,price=150),FALSE)==150,
  market_bid_minimum(player,'false')==150,is.na(market_bid_minimum(player)),
  is.na(market_bid_minimum(list(value=200),FALSE)))
})
check('the supplied information example reconciles configured allowance with maxBid',{
 x<-acquisition_headroom(63047149,359096743,0,0,.5)
 stopifnot(x$spendable_budget==242595520.5)
})
check('real bid dialog and final preflight reuse the same current league minimum',{
 active_cap<-capacity();captured<-NULL
 async_adapter_fetch<-function(...)promises::promise(function(resolve,reject){})
 showModal<-function(ui,...)captured<<-as.character(ui)
 selected<-reactiveVal(data.frame(id='target',name='Fixture Player',role='defensa',team='Club',value=200,price=150,
   change=0,points=0,user_team_id='',market_inMarket=TRUE))
 testServer(selected_player_Server,args=list(selected_player=selected,login_token=reactive(auth),
   championship_id=reactive('fixture-league'),user_team_id=reactive('mine'),capacity_fetcher=function(...)active_cap),{
   for(flag in c(FALSE,TRUE)){
     active_cap$rules$minimum_bid_at_value<<-flag
     expected<-if(flag)200 else 150
     stopifnot(run_acquisition_preflight(selected_player(),'bid')$minimum_bid==expected,
       isTRUE(open_market_offer_modal()),grepl(paste0('min="',expected,'"'),captured,fixed=TRUE),
       !run_acquisition_preflight(selected_player(),'bid',expected-1)$ok)
   }
   active_cap$rules$minimum_bid_at_value<<-NA
   stopifnot(!run_acquisition_preflight(selected_player(),'bid',200)$ok,!isTRUE(open_market_offer_modal()))
 })
})
for(method in c('GET','POST','PUT','PATCH','DELETE'))suppressMessages(untrace(method,where=asNamespace('httr')))
cat('CONFIGURED ACQUISITION RULES:',checks,'passed / 0 failed\n')
