#!/usr/bin/env Rscript
# Held capacity promises exercise real shared-resource scheduling without I/O.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
record_acquired_resource <- function(...) invisible(TRUE)
log_market_transaction <- function(...) invisible(TRUE)
auth <- c(userid='offer-opening-fixture',token='synthetic')
selected <- reactiveVal(data.frame(id='de-jong',name='De Jong',role='centrocampista',team='Club',
  value=200,price=150,change=0,points=10,user_team_id='',market_inMarket=TRUE))
team <- reactiveVal('mine');refresh <- reactiveVal(0L);action <- reactiveVal(NULL)
capacity <- function(seconds=60) list(status='ok',valid_until=as.numeric(Sys.time())+seconds,
  roster=list(status='ok',count=10,effective_count=10,cap=20),
  funds=list(spendable_budget=1000),outstanding=list(count=0),rules=list(minimum_bid_at_value=FALSE))
manual_capacity <- capacity();jobs <- list();reads <- character();sync_reads <- 0L;writes <- 0L
modal_shows <- 0L;modal_html <- NULL
showModal <- function(ui,...) {modal_shows <<- modal_shows+1L;modal_html <<- as.character(ui)}
buy_clause <- function(...) {writes <<- writes+1L;TRUE}
async_adapter_fetch <- function(fn,args=list(),context,...) {
  reads <<- c(reads,fn)
  if(fn=='get_team_acquisition_capacity')return(promises::promise(function(resolve,reject)
    jobs[[length(jobs)+1L]] <<- list(resolve=resolve,reject=reject,context=context)))
  value <- switch(fn,get_player_summary=list(championship=list(),points=list(),prices=list()),
    get_lineup_from_team=list(pointSystem='fixture'),get_player_full_profile=list(profile=list()),
    get_player_matches=list(matches=list()),read_card_round_history=data.frame(),
    read_card_price_history=data.frame(),stop(paste('Unexpected resource',fn)))
  promises::promise_resolve(value)
}
settle <- function(session) for(i in seq_len(20L)){later::run_now(0);session$flushReact()}
checks <- 0L
check <- function(name,expr){force(expr);checks <<- checks+1L;cat('PASS',name,'\n')}
clear_api_cache()
testServer(selected_player_Server,args=list(selected_player=selected,login_token=reactive(auth),
  championship_id=reactive('league'),user_team_id=team,refresh_trigger=refresh,open_action=action,
  capacity_fetcher=function(...) {sync_reads <<- sync_reads+1L;manual_capacity}),{
  settle(session)
  started <- Sys.time();session$setInputs(btn_bid_market=1L);settle(session)
  elapsed <- as.numeric(difftime(Sys.time(),started,units='secs'))
  check('held availability opens a disabled shell without synchronous reads or duplicate acquisition',{
    stopifnot(offer_modal_opened_RV(),market_offer_dialog_RV()$pending,length(jobs)==1L,sync_reads==0L,
      grepl('market_offer_body',modal_html,fixed=TRUE),
      grepl('Checking availability',output$market_offer_body$html,fixed=TRUE),
      grepl('disabled',output$market_offer_submit$html,fixed=TRUE),elapsed<1)
    cat(sprintf('MEASURE pending_offer_open_ms=%.0f held_capacity_requests=%d\n',elapsed*1000,length(jobs)))
  })
  session$setInputs(bid_amount=200,submit_bid=1L);settle(session)
  check('a pending shell cannot authorize a forged submit',stopifnot(writes==0L,sync_reads==0L))
  jobs[[1]]$resolve(capacity());settle(session)
  check('completion updates the same modal with the configured listing minimum',{
    stopifnot(modal_shows==1L,!market_offer_dialog_RV()$pending,sync_reads==0L,
      grepl('min="150"',output$market_offer_body$html,fixed=TRUE),
      !grepl('disabled',output$market_offer_submit$html,fixed=TRUE))
  })
  session$setInputs(market_offer_closed=as.character(market_offer_dialog_RV()$generation));settle(session)
  before <- reads;action('market_bid');settle(session)
  check('external market action reuses fresh card evidence with zero source reads',{
    stopifnot(offer_modal_opened_RV(),!market_offer_dialog_RV()$pending,identical(before,reads),sync_reads==0L)
  })
  manual_capacity <<- capacity();manual_capacity$status <<- 'partial'
  session$setInputs(bid_amount=200,submit_bid=2L);settle(session)
  session$setInputs(submit_bid=3L);settle(session)
  check('submission rechecks current facts and retains the editor after rejection without replay',{
    stopifnot(sync_reads==1L,writes==0L,
      grepl('bid_amount',output$market_offer_body$html,fixed=TRUE),
      grepl('min="150"',output$market_offer_body$html,fixed=TRUE))
  })
  manual_capacity <<- capacity()
  session$setInputs(btn_bid_market=2L);settle(session)
  session$setInputs(bid_amount=149,submit_bid=4L);settle(session)
  check('submission still enforces the league listing minimum',stopifnot(sync_reads==2L,writes==0L))
  session$setInputs(btn_bid_market=3L);settle(session)
  session$setInputs(bid_amount=150,submit_bid=5L);settle(session)
  session$setInputs(submit_bid=6L);settle(session)
  check('only an explicit freshly verified submission writes once',stopifnot(sync_reads==3L,writes==1L))

  refresh(1L);settle(session);jobs[[length(jobs)]]$resolve(capacity(.1));settle(session)
  Sys.sleep(.15)
  before <- length(jobs);session$setInputs(btn_bid_market=4L);settle(session)
  check('an expired card opens immediately and refreshes its one shared resource',{
    stopifnot(market_offer_dialog_RV()$pending,length(jobs)==before+1L,sync_reads==3L,
      grepl('disabled',output$market_offer_submit$html,fixed=TRUE))
  })
  closed_generation <- market_offer_dialog_RV()$generation
  session$setInputs(market_offer_closed=as.character(closed_generation));settle(session)
  before <- modal_shows;jobs[[length(jobs)]]$resolve(capacity());settle(session)
  check('cancelled verification cannot reopen or authorize an offer',{
    stopifnot(is.null(market_offer_dialog_RV()),!offer_modal_opened_RV(),modal_shows==before,
      is.null(modal_context_RV()$bid))
  })
  session$setInputs(btn_bid_market=5L);settle(session)
  old_generation <- market_offer_dialog_RV()$generation
  session$setInputs(btn_bid_market=6L);settle(session)
  session$setInputs(market_offer_closed=as.character(old_generation));settle(session)
  check('a delayed old-modal close cannot dismiss a newer opening',{
    stopifnot(!is.null(market_offer_dialog_RV()),market_offer_dialog_RV()$generation!=old_generation)
  })
  refresh(2L);settle(session)
  session$setInputs(btn_bid_market=7L);settle(session)
  missing_deadline <- capacity();missing_deadline$valid_until <- NULL
  jobs[[length(jobs)]]$resolve(missing_deadline);settle(session)
  check('a success-shaped response without a freshness deadline stays disabled',{
    stopifnot(!market_offer_dialog_RV()$pending,!market_offer_dialog_RV()$preflight$ok,
      grepl('disabled',output$market_offer_submit$html,fixed=TRUE),is.null(modal_context_RV()$bid))
  })
  refresh(3L);settle(session)
  session$setInputs(btn_bid_market=8L);settle(session)
  partial <- capacity();partial$status <- 'partial';partial$diagnostics <- 'team info unavailable'
  jobs[[length(jobs)]]$resolve(partial);settle(session)
  check('partial acquisition settles to an actionable failure without enabling submission',{
    stopifnot(!market_offer_dialog_RV()$preflight$ok,grepl('team balance',output$market_offer_body$html,fixed=TRUE),
      grepl('disabled',output$market_offer_submit$html,fixed=TRUE),is.null(modal_context_RV()$bid))
  })
  refresh(4L);settle(session)
  session$setInputs(btn_bid_market=9L);settle(session)
  held <- length(jobs);before <- modal_shows
  changed <- selected();changed$id <- 'another';changed$name <- 'Another Player';selected(changed);settle(session)
  jobs[[held]]$resolve(capacity());settle(session)
  check('player changes discard pending dialog completion without reopening',{
    stopifnot(is.null(market_offer_dialog_RV()),modal_shows==before,is.null(modal_context_RV()$bid))
  })
  refresh(5L);settle(session)
  session$setInputs(btn_bid_market=10L);settle(session)
  held <- length(jobs);before <- modal_shows
  team('another-team');settle(session)
  jobs[[held]]$resolve(capacity());settle(session)
  session$setInputs(bid_amount=200,submit_bid=7L);settle(session)
  check('team changes reject old results and queued submission',{
    stopifnot(is.null(market_offer_dialog_RV()),modal_shows==before,sync_reads==3L,writes==1L)
  })
})
cat(sprintf('MARKET OFFER OPENING: %d passed / 0 failed\n',checks))
