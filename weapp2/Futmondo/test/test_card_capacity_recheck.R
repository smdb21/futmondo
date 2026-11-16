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
  stopifnot(sum(calls=='get_team_acquisition_capacity')==0L)
  fixture_time <<- fixture_time+32
  pending$get_player_summary$resolve(list(data=list(id='p1'),championship=list(owner=list(computer=TRUE)),market=list(p=1000,bids=list()),points=list(),prices=list()))
  settle(session)
  stopifnot(sum(calls=='get_team_acquisition_capacity')==0L,
    !grepl('Checking availability|Availability checks expired|Retry availability|disabled',output$action_buttons$html))
  cat('PASS a 32s target response never starts team acquisition; target completion alone readies the card.\n')
  selected(NULL);settle(session);fixture_time <<- fixture_time+90;selected(player);settle(session)
  stopifnot(sum(calls=='get_team_acquisition_capacity')==0L,sum(calls=='get_player_summary')==1L,
    !grepl('Retry availability|expired before',output$action_buttons$html))
  cat('PASS reopening after expiry reuses the successful session target check without acquisition or a Retry loop.\n')

})
