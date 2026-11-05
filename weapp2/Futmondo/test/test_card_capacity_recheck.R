options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fixture_time <- base::Sys.time(); Sys.time <- function() fixture_time
record_acquired_resource <- function(...) invisible(TRUE)
auth <- c(userid='capacity-expiry-audit',token='synthetic')
pending <- new.env(parent=emptyenv()); calls <- character()
async_adapter_fetch <- function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE,...) {
  calls <<- c(calls,fn)
  if(fn %in% c('get_player_summary','get_team_acquisition_capacity')) return(promises::promise(function(resolve,reject) pending[[fn]] <- list(resolve=resolve,reject=reject)))
  value <- switch(fn,get_lineup_from_team=list(pointSystem='fixture'),get_player_full_profile=list(profile=list()),get_player_matches=list(matches=list()),read_card_round_history=data.frame(),read_card_price_history=data.frame(),stop(fn))
  promises::promise_resolve(value)
}
player <- data.frame(id='p1',name='Berenguer',role='defensa',team='Club',value=1000,change=0,points=2,user_team_id='',price=1000,market_inMarket=TRUE)
selected <- shiny::reactiveVal(player)
settle <- function(session) for(i in seq_len(20L)){later::run_now(0);session$flushReact()}
clear_api_cache()
shiny::testServer(selected_player_Server,args=list(selected_player=selected,login_token=shiny::reactive(auth),championship_id=shiny::reactive('league'),user_team_id=shiny::reactive('mine')),{
  settle(session)
  pending$get_team_acquisition_capacity$resolve(list(status='ok',valid_until=as.numeric(Sys.time())+15,roster=list(status='ok',count=1,cap=20,effective_count=1),funds=list(spendable_budget=10000),outstanding=list(count=0),rules=list(minimum_bid_at_value=TRUE)))
  settle(session)
  stopifnot(grepl('Checking availability',output$action_buttons$html,fixed=TRUE))
  fixture_time <<- fixture_time+16
  pending$get_player_summary$resolve(list(championship=list(),points=list(),prices=list()))
  settle(session)
  stopifnot(sum(calls=='get_team_acquisition_capacity')==2L,
    grepl('Checking availability',output$action_buttons$html,fixed=TRUE))
  fresh<-function()list(status='ok',valid_until=as.numeric(Sys.time())+15,
    roster=list(status='ok',count=1,cap=20,effective_count=1),funds=list(spendable_budget=10000),
    outstanding=list(count=0),rules=list(minimum_bid_at_value=TRUE))
  pending$get_team_acquisition_capacity$resolve(fresh());settle(session)
  stopifnot(!grepl('Checking availability|Availability checks expired',output$action_buttons$html),
    !grepl('disabled',output$action_buttons$html,fixed=TRUE))
  cat('PASS late summary triggers one fresh team check and verified controls recover.\n')
  selected(NULL);settle(session);selected(player);settle(session)
  stopifnot(sum(calls=='get_team_acquisition_capacity')==2L)
  cat('PASS warm reopening reuses verified availability without another request.\n')
  fixture_time <<- fixture_time+16
  selected(NULL);settle(session);selected(player);settle(session)
  stopifnot(sum(calls=='get_team_acquisition_capacity')==3L,
    grepl('Checking availability',output$action_buttons$html,fixed=TRUE))
  pending$get_team_acquisition_capacity$resolve(fresh());settle(session)
  stopifnot(!grepl('Availability checks expired',output$action_buttons$html,fixed=TRUE))
  cat('PASS reopening after the source deadline refreshes inside the outer20s TTL.\n')
  # Even a source that repeatedly returns expired facts cannot create a retry loop.
  selected(NULL);settle(session);fixture_time <<- fixture_time+16;selected(player);settle(session)
  old<-fresh();old$valid_until<-as.numeric(Sys.time())-1
  pending$get_team_acquisition_capacity$resolve(old);settle(session)
  stopifnot(sum(calls=='get_team_acquisition_capacity')==5L)
  pending$get_team_acquisition_capacity$resolve(old);settle(session)
  stopifnot(sum(calls=='get_team_acquisition_capacity')==5L,
    grepl('Availability checks expired',output$action_buttons$html,fixed=TRUE),
    grepl('disabled',output$action_buttons$html,fixed=TRUE))
  cat('PASS unsuccessful recheck stops after one attempt with controls disabled.\n')

})
