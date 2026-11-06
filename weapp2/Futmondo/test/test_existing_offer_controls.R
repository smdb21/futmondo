#!/usr/bin/env Rscript
# Regression for an existing market offer occupying the final roster slot.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
record_acquired_resource <- function(...) invisible(TRUE)
log_market_transaction <- function(...) invisible(TRUE)
get_sb_url <- function() ''
get_sb_key <- function() ''
auth <- c(userid='existing-offer-fixture',token='synthetic')
rows <- data.frame(id='pedrosa',bid_id='own-offer',bid_price=1000,numberOfBids=1L)
manual_rows <- rows
market_reads <- 0L; writes <- list(); capacity_status <- 'ok';summary_failed <- FALSE
capacity <- function() list(status=capacity_status,valid_until=as.numeric(Sys.time())+15,
  own_bid_valid_until=as.numeric(Sys.time())+20,target_rows=rows,
  roster=list(status='ok',count=18,effective_count=19,cap=20,
    pending_outbound_count=0,pending_inbound_count=1),
  funds=list(spendable_budget=10000),outstanding=list(count=1,total_amount=1000),
  rules=list(minimum_bid_at_value=TRUE))
get_market_players <- function(...) {market_reads <<- market_reads+1L;manual_rows}
modify_bid <- function(...) {writes[[length(writes)+1L]] <<- c(list(action='modify'),list(...));TRUE}
cancel_bid <- function(...) {writes[[length(writes)+1L]] <<- c(list(action='cancel'),list(...));TRUE}
async_adapter_fetch <- function(fn,args=list(),context,...) {
  if (fn=='get_player_summary' && summary_failed)
    return(promises::promise_reject(simpleError('Synthetic summary failure')))
  value <- switch(fn,get_team_acquisition_capacity=capacity(),
    get_player_summary=list(points=list(),prices=list(),championship=list()),
    get_lineup_from_team=list(pointSystem='fixture'),get_player_full_profile=list(profile=list()),
    get_player_matches=list(matches=list()),read_card_round_history=data.frame(),
    read_card_price_history=data.frame(),stop(paste('Unexpected resource',fn)))
  promises::promise_resolve(value)
}
settle <- function(session) for(i in seq_len(20L)){later::run_now(0);session$flushReact()}
selected <- shiny::reactiveVal(data.frame(id='pedrosa',name='Pedrosa',role='defender',team='Rayo',
  value=1000,change=0,points=54,user_team_id='',price=1000,market_inMarket=TRUE,
  bid_id='untrusted-rival',bid_price=9999))
team <- shiny::reactiveVal('mine');refresh <- shiny::reactiveVal(0L)
clear_api_cache()
shiny::testServer(selected_player_Server,args=list(selected_player=selected,
  login_token=shiny::reactive(auth),championship_id=shiny::reactive('league'),
  user_team_id=team,capacity_fetcher=function(...)capacity(),refresh_trigger=refresh),{
  settle(session)
  html <- output$action_buttons$html
  stopifnot(grepl('Update Bid',html,fixed=TRUE),grepl('Cancel Bid',html,fixed=TRUE),
    !grepl('Make Market Offer|Roster capacity reached|disabled',html),market_reads==0L,
    identical(active_bid_info_RV(),list(id='own-offer',price=1000)))
  cat('PASS full-roster card shows its own existing offer with zero additional display reads.\n')
  session$setInputs(btn_modify_bid=1);settle(session)
  stopifnot(modify_modal_opened_RV(),market_reads==1L)
  session$setInputs(new_bid_amount=1200,submit_modify_bid=1);settle(session)
  stopifnot(length(writes)==1L,writes[[1]]$action=='modify',
    writes[[1]]$bid_id=='own-offer',writes[[1]]$new_price==1200,market_reads==2L)
  session$setInputs(submit_modify_bid=2);settle(session)
  stopifnot(length(writes)==1L,market_reads==2L)
  cat('PASS update at capacity targets the confirmed own bid and cannot replay.\n')
  capacity_status <<- 'partial';summary_failed <<- TRUE;refresh(1L);settle(session)
  stopifnot(grepl('Cancel Bid',output$action_buttons$html,fixed=TRUE),
    !grepl('disabled',output$action_buttons$html,fixed=TRUE))
  session$setInputs(btn_cancel_bid=1);settle(session)
  session$setInputs(submit_cancel_bid=1);settle(session)
  stopifnot(length(writes)==2L,writes[[2]]$action=='cancel',writes[[2]]$bid_id=='own-offer')
  cat('PASS cancellation remains available with unrelated partial capacity evidence.\n')
  session$setInputs(btn_cancel_bid=2);settle(session)
  manual_rows <<- transform(rows,bid_id='replacement')
  session$setInputs(submit_cancel_bid=2);settle(session)
  stopifnot(length(writes)==2L)
  manual_rows <<- rows
  session$setInputs(btn_cancel_bid=3);settle(session)
  manual_rows <<- observed_data(rows,status='stale')
  session$setInputs(submit_cancel_bid=3);settle(session)
  stopifnot(length(writes)==2L)
  manual_rows <<- rows
  session$setInputs(btn_cancel_bid=4);settle(session)
  team('different-team');settle(session)
  session$setInputs(submit_cancel_bid=4);settle(session)
  stopifnot(length(writes)==2L)
  cat('PASS replaced offers, stale reads and switched contexts cannot be cancelled from an old confirmation.\n')
})
now <- Sys.time();cap <- capacity();summary <- list(my_bid_id='summary-own',my_bid_price=800)
cap$target_rows <- rbind(rows,transform(rows,bid_id='ambiguous'))
stopifnot(is.null(player_card_active_bid(summary,cap,'pedrosa')))
cap$target_rows <- transform(rows,bid_id=NA_character_,bid_price=NA_real_)
stopifnot(is.null(player_card_active_bid(summary,cap,'pedrosa')))
cap$target_rows <- rows;cap$own_bid_valid_until <- as.numeric(now)-1
stopifnot(is.null(player_card_active_bid(NULL,cap,'pedrosa',now)))
cap$own_bid_valid_until <- as.numeric(now)+20
attr(cap,'fetch_status') <- 'stale'
stopifnot(is.null(player_card_active_bid(NULL,cap,'pedrosa',now)))
stopifnot(is.null(player_card_active_bid(NULL,NULL,'pedrosa')))
cat('PASS duplicate, absent and expired own-bid evidence cannot promote rival or stale bid fields.\n')
