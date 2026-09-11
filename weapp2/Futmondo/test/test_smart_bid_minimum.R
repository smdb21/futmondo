#!/usr/bin/env Rscript
# Offline regression coverage for executable smart bids and player-card submissions.
suppressPackageStartupMessages({library(shiny);source('futmondo_functions.R');source('intelligence_engine.R')})
checks <- 0L
check <- function(name, expr) {force(expr); checks <<- checks + 1L;cat('[PASS]',name,'\n')}
capacity <- list(status='ok',roster=list(count=5L,cap=25L),outstanding=list(count=0L),
  funds=list(spendable_budget=30e6,api_bid_limit=30e6))
player <- data.frame(id='player',name='Player',value=10e6,average.average=10,
  average.averageLastFive=10,status='ok',stringsAsFactors=FALSE)
check('minimum covers valuation and the first valid listing price', {
  stopifnot(market_bid_minimum(player)==10e6,
    market_bid_minimum(list(value=10e6,price=8e6))==10e6,
    market_bid_minimum(list(value=10e6,price=12e6))==12e6,
    market_bid_minimum(list(value=10e6,effective_market_price=15e6,market_price=12e6))==15e6,
    market_bid_minimum(list(value=10e6,effective_market_price=NA,market_price=0,price='12000000'))==12e6,
    market_bid_minimum(list(value=10.2,price=10.1))==11,
    is.na(market_bid_minimum(list(value=Inf,price=NA))),is.na(market_bid_minimum(data.frame())))
})
check('poor form still produces an executable market-value bid', {
  p<-player;p$average.averageLastFive<-5
  x<-calculate_smart_bid(p,'champ',capacity=capacity)
  stopifnot(x$fair_value<p$value,x$recommended_bid==p$value,x$minimum_bid==p$value,
    x$min_winning_bid>=p$value,x$action=='bid',x$can_compete)
})
check('listing asking prices floor every recommendation', {
  for(field in c('effective_market_price','market_price','price')) {
    p<-player;p[[field]]<-12e6
    x<-calculate_smart_bid(p,'champ',capacity=capacity)
    stopifnot(x$recommended_bid==12e6,x$min_winning_bid==12e6,x$action=='bid')
  }
})
check('ineligible minimums yield no bid rather than a smaller invalid amount', {
  p<-player;p$status<-'injured'
  cases<-list(list(p=p,cap=capacity,highest=NULL),
    list(p=player,cap=modifyList(capacity,list(funds=list(spendable_budget=5e6))),highest=NULL),
    list(p=player,cap=modifyList(capacity,list(funds=list(api_bid_limit=5e6))),highest=NULL),
    list(p=player,cap=capacity,highest=20e6))
  for(case in cases) {
    x<-calculate_smart_bid(case$p,'champ',capacity=case$cap,market_high_bid=case$highest)
    stopifnot(x$action=='no_bid',x$recommended_bid==0,!x$can_compete,
      x$max_rational_bid<x$min_winning_bid,nzchar(x$message))
  }
  p<-player;p$value<-NA_real_
  x<-calculate_smart_bid(p,'champ',capacity=capacity)
  stopifnot(x$action=='no_bid',x$recommended_bid==0,x$reason=='minimum_unavailable')
})
check('preflight rejects under-minimum new and modified bids', {
  for(mode in c('bid','modify')) {
    x<-evaluate_acquisition_preflight(capacity,mode,amount=10e6-1,
      existing_bid_amount=10e6,minimum_bid=10e6)
    stopifnot(!x$ok,x$reason=='minimum_bid',grepl('10000000',x$message,fixed=TRUE))
    stopifnot(evaluate_acquisition_preflight(capacity,mode,amount=10e6,
      existing_bid_amount=10e6,minimum_bid=10e6)$ok)
  }
  stopifnot(!evaluate_acquisition_preflight(capacity,'bid',minimum_bid=NA_real_)$ok,
    evaluate_acquisition_preflight(capacity,'offer',amount=1,minimum_bid=10e6)$ok)
})
# Execute actual module handlers and their internal preflight with stubbed I/O.
tree<-parse('Modules/Selected_Player_Module.R')
locate<-function(x,target,operator) {
  if(is.call(x)&&identical(x[[1]],as.name(operator))&&
     identical(paste(deparse(x[[2]]),collapse=''),target)) return(x)
  if(is.call(x)||is.expression(x)||is.pairlist(x)) for(y in as.list(x)) {
    if(missing(y)) next
    found<-locate(y,target,operator);if(!is.null(found)) return(found)
  }
  NULL
}
env<-new.env(parent=globalenv())
env$selected_player<-function() player
env$login_token<-'offline';env$championship_id<-'champ';env$user_team_id<-'team'
env$get_reactive_val<-identity
env$capacity_fetcher<-function(...) capacity
env$active_bid_info_RV<-function() list(id='bid',price=10e6)
env$consume_action_context<-function(...) TRUE
env$remember_action_context<-function(...) NULL
env$offer_modal_opened_RV<-function(...) NULL
env$ns<-identity
env$format_currency<-function(x) paste(x,'EUR')
env$format_table_currency<-env$format_currency
env$clear_api_cache<-function(...) NULL
env$log_market_transaction<-function(...) NULL
env$removeModal<-function(...) NULL
env$on_bid_updated<-NULL
writes<-list();failures<-list();modal<-NULL
env$buy_clause<-function(...) {writes[[length(writes)+1L]]<<-list(...);TRUE}
env$modify_bid<-env$buy_clause
env$show_preflight_failure<-function(x) failures[[length(failures)+1L]]<<-x
env$showModal<-function(ui,...) modal<<-as.character(ui)
env$market_offer_decision<-function(sp,pf) list(open=isTRUE(pf$ok),reason=pf$reason)
eval(locate(tree,'run_acquisition_preflight','<-'),env)
eval(locate(tree,'open_market_offer_modal','<-'),env)
handler<-function(event) {
  f<-function() NULL
  body(f)<-locate(tree,event,'observeEvent')[[3]]
  environment(f)<-env
  f
}
session<-shiny::MockShinySession$new()
check('actual market and modify handlers block invalid offers before API writes', {
  shiny::withReactiveDomain(session,{
    for(event in c('input$submit_bid','input$submit_modify_bid')) {
      env$input<-list(bid_amount=10e6-1,new_bid_amount=10e6-1)
      handler(event)()
    }
    stopifnot(length(writes)==0L,length(failures)==2L,
      all(vapply(failures,function(x) x$reason=='minimum_bid',logical(1))))
    for(event in c('input$submit_bid','input$submit_modify_bid')) {
      env$input<-list(bid_amount=10e6,new_bid_amount=10e6)
      handler(event)()
    }
    stopifnot(length(writes)==2L,writes[[1]]$price==10e6,writes[[2]]$new_price==10e6)
  })
})
check('regular and modified offer modals expose the correct input minimum', {
  shiny::withReactiveDomain(session,{
    env$open_market_offer_modal()
    stopifnot(grepl('min="1e+07"',modal,fixed=TRUE)||grepl('min="10000000"',modal,fixed=TRUE))
    handler('input$btn_modify_bid')()
    stopifnot(grepl('min="1e+07"',modal,fixed=TRUE)||grepl('min="10000000"',modal,fixed=TRUE))
  })
})
check('smart-bid modal revalidates cached recommendation before opening', {
  shiny::withReactiveDomain(session,{
    modal<-NULL
    env$smart_bid_cache_RV<-function() list(can_compete=TRUE,funds_verified=TRUE,recommended_bid=5e6)
    handler('input$btn_use_smart_bid')()
    stopifnot(is.null(modal),tail(failures,1)[[1]]$reason=='minimum_bid')
    env$smart_bid_cache_RV<-function() list(can_compete=TRUE,funds_verified=TRUE,recommended_bid=10e6)
    handler('input$btn_use_smart_bid')()
    stopifnot(!is.null(modal),grepl('Minimum market bid',modal,fixed=TRUE))
  })
})
check('unaffordable smart-bid widget renders an explicit no-bid state', {
  env$get_acquisition_capacity<-function(...) modifyList(capacity,list(funds=list(spendable_budget=5e6)))
  env$get_championship_pressroom<-function(...) NULL
  env$smart_bid_cache_RV<-function(value) NULL
  widget<-locate(tree,'output$smart_bid_widget','<-')[[3]][[2]]
  f<-function() NULL;body(f)<-widget;environment(f)<-env
  html<-shiny::withReactiveDomain(session,as.character(f()))
  stopifnot(grepl('No bid',html,fixed=TRUE),grepl('disabled',html,fixed=TRUE),
    grepl('required minimum is',html,fixed=TRUE))
})
session$close()
check('zero ceilings explain the exact blocking constraint', {
  cash_zero <- capacity
  cash_zero$funds$spendable_budget <- 0
  x <- calculate_smart_bid(player, 'champ', capacity = cash_zero)
  stopifnot(x$reason == 'no_spendable_capacity', x$binding_constraint == 'spendable',
    grepl('spendable capacity is 0 EUR', x$message, fixed = TRUE))
  api_zero <- capacity
  api_zero$funds$api_bid_limit <- 0
  x <- calculate_smart_bid(player, 'champ', capacity = api_zero)
  stopifnot(x$reason == 'api_bid_limit_zero', x$binding_constraint == 'api_limit',
    grepl('maximum allowed bid of 0 EUR', x$message, fixed = TRUE))
  module <- paste(readLines('Modules/Selected_Player_Module.R', warn = FALSE), collapse = '\n')
  stopifnot(grepl('Verified Spendable:', module, fixed = TRUE),
    grepl('Futmondo Bid Limit:', module, fixed = TRUE))
})
cat('SMART BID MINIMUM:',checks,'passed / 0 failed\n')
