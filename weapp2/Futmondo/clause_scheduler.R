# Explicit, durable purchase authorization; factual acquisition remains read-only.
# Contracts and rollout: docs/scheduled_clauses.md.
.fm_clause <- new.env(parent=emptyenv())
.fm_clause$accounts <- new.env(parent=emptyenv())
fm_clause_enabled <- function() identical(tolower(Sys.getenv('FUTMONDO_CLAUSE_SCHEDULING_ENABLED','false')),'true') && fm_worker_configured()
fm_clause_id <- function() paste0(as.character(openssl::rand_bytes(16)),collapse='')
fm_clause_encrypt <- function(auth) fm_worker_encrypt(c(token=fm_scalar(auth[['token']],''),userid=fm_scalar(auth[['userid']],''),user_name=if('user_name'%in%names(auth))fm_scalar(auth[['user_name']],'')else''))
fm_clause_stamp <- function(x=Sys.time()) format(fm_time(x),'%Y-%m-%dT%H:%M:%OS3Z',tz='UTC')
fm_clause_scope <- function(context) setNames(lapply(fm_worker_fields,function(f)fm_scalar(context[[f]],'')),fm_worker_fields)
fm_clause_safe_code <- function(code) if(is.character(code)&&length(code)==1L&&!is.na(code)&&grepl('^[a-zA-Z0-9_.:]{1,160}$',code))code else 'unclassified_response'
fm_clause_rpc <- function(name,payload,owner='') {
 headers<-c(apikey=get_sb_key(),Authorization=paste('Bearer',get_sb_key()),`Content-Type`='application/json')
 promises::then(fm_async_http(paste0(get_sb_url(),'/rest/v1/rpc/clause_',name),'POST',fm_worker_json(payload),headers,owner=owner),function(r)
  jsonlite::fromJSON(rawToChar(r$content),simplifyVector=FALSE))
}
fm_clause_rpc_sync <- function(name,payload) {
 r<-httr::POST(paste0(get_sb_url(),'/rest/v1/rpc/clause_',name),body=fm_worker_json(payload),
  httr::add_headers(apikey=get_sb_key(),Authorization=paste('Bearer',get_sb_key()),`Content-Type`='application/json'),httr::timeout(15))
 if(httr::status_code(r)<200L||httr::status_code(r)>=300L)stop('Scheduled purchase database check failed; no purchase was authorized.')
 jsonlite::fromJSON(httr::content(r,as='text',encoding='UTF-8'),simplifyVector=FALSE)
}
fm_clause_player <- function(summary) {
 ch<-summary$championship;raw<-summary$data;cl<-ch$clause
 list(id=fm_scalar(raw$`_id` %||% raw$id,''),slug=fm_scalar(raw$slug,''),name=fm_scalar(raw$name,'Player'),
  owner_id=fm_scalar(ch$owner$`_id` %||% ch$owner$id,''),
  owner_verified=nzchar(fm_scalar(ch$owner$`_id` %||% ch$owner$id,''))||isTRUE(ch$owner$computer),price=fm_number(cl$price),
  transferred=if(is.logical(cl$transferred)&&length(cl$transferred)==1L)cl$transferred else NA,
  opens_at=if(length(fm_time(cl$date))==1L&&!is.na(fm_time(cl$date)))fm_clause_stamp(cl$date)else NA_character_)
}
fm_clause_eligible <- function(player,team,now=Sys.time()) {
 is.list(player)&&nzchar(fm_scalar(player$id,''))&&nzchar(fm_scalar(player$owner_id,''))&&
 !identical(player$owner_id,as.character(team))&&isFALSE(player$transferred)&&
 is.finite(fm_number(player$price))&&player$price>0&&length(fm_time(player$opens_at))==1L&&
 !is.na(fm_time(player$opens_at))&&fm_time(player$opens_at)>now
}
fm_clause_row_player <- function(sp) list(id=fm_scalar(sp$id,''),slug=fm_scalar(sp$slug,''),name=fm_scalar(sp$name,'Player'),
 owner_id=player_card_owner_id(sp),price=fm_number(sp$clause_price),
 transferred=if(is.logical(sp$clause_transferred)&&length(sp$clause_transferred)==1L)sp$clause_transferred else NA,
 opens_at=if(length(fm_time(sp$clause_date))==1L&&!is.na(fm_time(sp$clause_date)))fm_clause_stamp(sp$clause_date)else NA_character_)
# A bounded, language-neutral subset of the existing acquisition engine. No new
# accounting formula, invented unknown values, or source freshness renewal.
get_scheduled_clause_check <- function(login,championship_id,user_team_id,player_id,require_clause=FALSE) {
 old<-options(futmondo.worker_live=TRUE);on.exit(options(old),add=TRUE)
 ctx<-resource_context(login,championship_id,user_team_id,player_id)
 resolved<-get_resolved_league_context(login,championship_id,user_team_id)
 active<-get_active_championships(login)
 membership<-Filter(function(ch)identical(fm_scalar(ch$id %||% ch$`_id`,''),as.character(championship_id)),active$championships)
 if(length(membership)!=1L||!identical(fm_scalar(membership[[1]]$userteam$id %||% membership[[1]]$userteam$`_id`,''),as.character(user_team_id)))stop('The buying team no longer matches verified membership.')
 for(f in c('account_id','championship_id','user_team_id','competition_id','season','scoring_version'))
  if(!identical(fm_scalar(resolved[[f]],''),fm_scalar(ctx[[f]],'')))stop('Current league context could not be verified.')
 capacity<-get_team_acquisition_capacity(login,championship_id,user_team_id)
 info<-get_user_team_info(login,championship_id,user_team_id)
 roster<-get_players_from_team(login,championship_id,user_team_id,facts_only=TRUE)
 history<-get_championship_pressroom(login,championship_id)
 if(acquisition_data_unverified(history)||!is.data.frame(history)||!all(c('player_id','buyer_team_id','seller_team_id','price','created')%in%names(history)))stop('Transfer history could not be verified.')
 if(is.finite(fm_number(capacity$valid_until))&&capacity$valid_until<=as.numeric(Sys.time())){
  capacity<-get_team_acquisition_capacity(login,championship_id,user_team_id)
  info<-get_user_team_info(login,championship_id,user_team_id)
  roster<-get_players_from_team(login,championship_id,user_team_id,facts_only=TRUE)
  history<-get_championship_pressroom(login,championship_id)
 }
 summary<-get_player_summary(login,championship_id,user_team_id,player_id)
 player<-fm_clause_player(summary)
 if(!identical(player$id,as.character(player_id)))stop('Current player identity could not be verified.')
 if(isTRUE(require_clause)&&(!isTRUE(player$owner_verified)||!is.logical(player$transferred)||is.na(player$transferred)||
  !is.finite(player$price)||player$price<=0||is.na(fm_time(player$opens_at))))stop('Current clause ownership, price or date is incomplete.')
 if(isTRUE(require_clause)&&!nzchar(player$slug)){
  catalog<-get_championship_players(login,championship_id)
  row<-if(is.data.frame(catalog)&&all(c('id','slug')%in%names(catalog)))catalog[catalog$id%in%player_id,,drop=FALSE]else NULL
  if(is.null(row)||nrow(row)!=1L||!nzchar(fm_scalar(row$slug,'')))stop('The current player URL identity could not be verified.')
  player$slug<-as.character(row$slug)
 }
 target_tx<-history[history$player_id%in%player_id,,drop=FALSE]
 if(nrow(target_tx)>1000L)stop('Player transfer history exceeds the bounded verification limit.')
 recent<-tail(history[order(fm_time(history$created)),,drop=FALSE],1000L)
 tx<-unique(rbind(target_tx,recent))
 transactions<-lapply(seq_len(nrow(tx)),function(i)list(player_id=as.character(tx$player_id[i]),
  buyer_team_id=fm_scalar(tx$buyer_team_id[i],''),seller_team_id=fm_scalar(tx$seller_team_id[i],''),
  price=fm_number(tx$price[i]),created=fm_clause_stamp(tx$created[i]),is_clause=if('is_clause'%in%names(tx))as.logical(tx$is_clause[i])else NA))
 market<-capacity$target_rows
 offers<-list()
 if(is.data.frame(market)&&all(c('id','bid_id','bid_price')%in%names(market)))for(i in which(!is.na(market$bid_id)&nzchar(as.character(market$bid_id))))
  offers[[length(offers)+1L]]<-list(player_id=as.character(market$id[i]),bid_id=as.character(market$bid_id[i]),price=fm_number(market$bid_price[i]))
 own_bid<-fm_scalar(summary$my_bid_id,'')
 if(nzchar(own_bid)&&!any(vapply(offers,function(o)identical(o$bid_id,own_bid),logical(1)))){
  if(any(vapply(offers,function(o)identical(o$player_id,as.character(player_id)),logical(1))))stop('The own offer changed during verification.')
  own_price<-fm_number(summary$my_bid_price)
  if(!is.finite(own_price)||own_price<=0)stop('The own offer amount could not be verified.')
  offers[[length(offers)+1L]]<-list(player_id=as.character(player_id),bid_id=own_bid,price=own_price)
  capacity$outstanding$count<-capacity$outstanding$count+1L;capacity$outstanding$offers<-capacity$outstanding$count
  capacity$outstanding$total_amount<-capacity$outstanding$total_amount+own_price
  borrowing<-acquisition_headroom(capacity$funds$reported_budget,capacity$funds$team_value,capacity$funds$withheld,
    capacity$outstanding$total_amount,capacity$funds$debt_fraction)
  for(field in names(borrowing))capacity$funds[[field]]<-borrowing[[field]]
 }
 valid<-min(fm_number(capacity$valid_until),as.numeric(attr(summary,'valid_until') %||% (as.numeric(fm_time(attr(summary,'observed_at')))+15)),na.rm=FALSE)
 if(!identical(capacity$status,'ok')||!identical(capacity$roster$status,'ok')||!is.finite(valid)||valid<=as.numeric(Sys.time())||
  acquisition_data_unverified(summary)||acquisition_data_unverified(info)||acquisition_data_unverified(roster))stop('Live player, funds and capacity checks are incomplete or expired.')
 list(context=fm_clause_scope(ctx),status=capacity$status,player=player,
  roster=capacity$roster,funds=capacity$funds,rules=capacity$rules,outstanding=capacity$outstanding,offers=unname(offers),transactions=unname(transactions),
  owned_ids=I(as.character(roster$id)),checked_at=fm_clause_stamp(),financial_observed_at=fm_clause_stamp(attr(info,'observed_at')),
  valid_until=fm_clause_stamp(as.POSIXct(valid,origin='1970-01-01',tz='UTC')))
}
fm_clause_account <- function(id) {
 s<-.fm_clause$accounts[[id]]
 if(is.null(s)){
  s<-new.env(parent=emptyenv());s$jobs<-list();s$known<-FALSE;s$error<-NULL;s$health<-NULL;s$busy<-FALSE
  s$context_updates<-list();s$manual_attempts<-list();s$reconcile_busy<-FALSE;s$manual_retry_due<-Inf
  s$revision<-shiny::withReactiveDomain(NULL,shiny::reactiveVal(0L));s$sessions<-character();s$timer<-NULL;s$last<-0
  .fm_clause$accounts[[id]]<-s
 }
 s
}
# Empty/settled purchase lists do not need a full DB download every five seconds.
# Active/uncertain reservations and errors retain fast polling; actions force a read.
fm_clause_poll_interval <- function(s) {
 if(!isTRUE(s$known)||!is.null(s$error))return(5)
 active<-any(vapply(s$jobs,function(j)!isTRUE(j$state%in%c('succeeded','cancelled','expired','failed')),logical(1)))
 if(active)5 else 60
}
fm_clause_refresh <- function(account,owner='',force=FALSE) {
 if(!fm_clause_enabled())return(invisible(NULL))
 s<-.fm_clause$accounts[[account]]
 if(is.null(s)){if(!nzchar(owner))return(invisible(NULL));s<-fm_clause_account(account)}
 if(s$busy||!force&&as.numeric(Sys.time())-s$last<fm_clause_poll_interval(s)-1)return(invisible(NULL))
 s$busy<-TRUE;s$last<-as.numeric(Sys.time())
 promises::then(fm_clause_rpc('read',list(p_account=account),owner),function(value){
  if(!is.list(value)||!is.list(value$jobs)||any(vapply(value$jobs,function(j)
   !is.list(j)||!identical(j$account_id,account)||!is.finite(fm_number(j$reserved_amount))||!is.finite(fm_number(j$reserved_slot)),logical(1)))){
   s$busy<-FALSE;s$error<-'Scheduled purchase response was incomplete. Reservations are unverified.'
   s$revision(shiny::isolate(s$revision())+1L);return(NULL)
  }
  signature<-fm_worker_json(value$jobs)
  changed<-!s$known||!identical(signature,fm_worker_json(s$jobs))||!is.null(s$error)
  if(changed)for(j in value$jobs){
   previous<-Filter(function(old)identical(old$id,j$id),s$jobs)
   if(j$state%in%c('accepted','succeeded','uncertain')&&!identical(j$result_code,'active_offer_confirmed')&&
     (!length(previous)||!identical(previous[[1]]$state,j$state)))
    invalidate_resources(structure(j$context,class='futmondo_context'),pattern='acq_capacity|team_info|market_players|financial_snapshot|roster')
  }
  s$jobs<-value$jobs %||% list();s$known<-TRUE;s$error<-NULL;s$health<-value$health;s$more_outcomes<-isTRUE(value$more_outcomes);s$busy<-FALSE
  if(changed)s$revision(shiny::isolate(s$revision())+1L)
  NULL
 },function(e){s$busy<-FALSE;s$error<-'Scheduled purchases could not be read. Reservations are unverified.'
  s$revision(shiny::isolate(s$revision())+1L);NULL})
 invisible(NULL)
}
fm_clause_jobs <- function(context) {
 if(!fm_clause_enabled())return(list())
 s<-fm_clause_account(context$account_id);try(s$revision(),silent=TRUE)
 if(!s$known||!is.null(s$error))return(NULL)
 Filter(function(j)all(vapply(c('account_id','championship_id','user_team_id','competition_id','season','scoring_version'),
  function(f)identical(fm_scalar(j$context[[f]],''),fm_scalar(context[[f]],'')),logical(1))),s$jobs)
}
# Saved transfer history can settle a completed bid or a paid clause.
fm_clause_completed_purchase_matches <- function(jobs,history,team,balance_at,history_at) {
 jobs<-head(jobs,64L)
 fields<-c('id','player_id','buyer_team_id','seller_team_id','price','created')
 if(!length(jobs)||!is.data.frame(history)||!all(fields%in%names(history)))return(list())
 players<-vapply(jobs,function(j)fm_scalar(j$player_id,''),character(1))
 keep<-history$buyer_team_id%in%team & history$player_id%in%players & !is.na(history$id) & nzchar(history$id)
 rows<-history[keep,fields,drop=FALSE]
 sellers<-as.character(rows$seller_team_id);sellers[is.na(sellers)]<-''
 is_clause<-(history$is_clause %||% rep(NA,nrow(history)))[keep]
 created<-fm_time(rows$created);price<-suppressWarnings(as.numeric(rows$price))
 matches<-list()
 for(j in jobs){
  dispatched<-fm_time(j$dispatched_at)
  if(length(dispatched)!=1L||is.na(dispatched)||balance_at<=dispatched)next
  seller<-fm_scalar(j$original_owner,'')
  clause<-identical(j$mode,'clause')
  acknowledged<-identical(j$state,'accepted')&&identical(j$result_code,API_CODE_OK)
  method<-if(clause) nzchar(seller) & (is_clause%in%TRUE | (acknowledged & is.na(is_clause))) else
   sellers=='' | is_clause%in%FALSE
  amount<-fm_number(if(clause)j$actual_price %||% j$max_price else j$max_price)
  idx<-which(method & rows$player_id==j$player_id & sellers==seller & price==amount &
   created>=dispatched-2 & created<=history_at & created<balance_at)
  if(length(idx)!=1L)next
  i<-idx[[1]]
  match<-list(job_id=j$id,player_id=j$player_id,buyer_team_id=team,
   seller_team_id=seller,price=price[i],created=fm_clause_stamp(created[i]))
  if(clause)match$is_clause<-is_clause[i]
  matches[[length(matches)+1L]]<-match
 }
 matches
}

fm_clause_active_offer_matches <- function(jobs,market,balance_at,market_at) {
 fields<-c('id','bid_id','bid_price')
 if(!is.data.frame(market)||!all(fields%in%names(market))||
    length(balance_at)!=1L||length(market_at)!=1L||anyNA(c(balance_at,market_at)))return(list())
 eligible<-!is.na(market$bid_id)&nzchar(market$bid_id)
 offers<-as.data.frame(market)[eligible,fields,drop=FALSE]
 if(!nrow(offers))return(list())
 prices<-suppressWarnings(as.numeric(offers$bid_price))
 matches<-list()
 for(j in head(jobs,64L)){
  dispatched<-fm_time(j$dispatched_at)
  if(length(dispatched)!=1L||is.na(dispatched)||min(balance_at,market_at)<=dispatched)next
  i<-which(offers$id==j$player_id)
  if(length(i)!=1L||sum(offers$bid_id==offers$bid_id[i])!=1L||!isTRUE(prices[i]==fm_number(j$max_price)))next
  matches[[length(matches)+1L]]<-list(job_id=j$id,player_id=j$player_id,bid_id=as.character(offers$bid_id[i]),price=prices[i])
 }
 matches
}

# A saved, successful response remains evidence at its source time after TTL expiry.
fm_manual_finance_time <- function(state) {
 if(is.null(state$data)||!is.null(state$error)||!is.null(attr(state$data,'fetch_error'))||
    state$status%in%c('partial','unavailable')||
    isTRUE(attr(state$data,'worker_acquisition_status')%in%c('partial','unavailable'))||
    isTRUE(attr(state$data,'worker_completeness')%in%c('partial','unavailable','incomplete')))return(NA_real_)
 fm_number(as.numeric(fm_time(state$observed_at)))
}

fm_manual_finance_session <- function(session,login,league,team,balance_state,history_state) {
 revision <- shiny::reactiveVal(0L)
 ledgers <- list()
 account <- ''
 generation <- 0L
 sequence <- 0L
 account_id <- function() {
  auth <- login()
  if(valid_login(auth))fm_scalar(auth[['userid']],'')else ''
 }
 shiny::observeEvent(account_id(), {
  account <<- account_id()
  generation <<- generation + 1L
  ledgers <<- list()
  revision(revision() + 1L)
 }, priority=100)
 scope_key <- function(ctx) {
  ctx$player_id <- ''
  api_cache_key('manual-finance',context=ctx)
 }
 cash_baseline <- function(state) {
  if(!is.list(state$data))return(state)
  original <- state$data
  fields <- c('budget','withheld','teamValue','team_value','maxBid','configuration')
  state$data <- original[intersect(names(original),fields)]
  state$data$configuration <- list(mnmp=original$configuration$mnmp)
  attributes(state$data) <- c(attributes(state$data),attributes(original)[setdiff(names(attributes(original)), 'names')])
  state
 }
 summary_read <- factual_read_adapter(session,'get_player_summary',login,league,team,state_only=TRUE)
 session$userData$begin_manual_finance_action <- function(ctx,mode,seller,amount) {
  key <- scope_key(ctx)
  player <- fm_scalar(ctx$player_id,'')
  sequence <<- sequence + 1L
  event <- list(id=as.character(sequence),kind='manual',state='accepted',result_code=API_CODE_OK,mode=mode,
    player_id=player,original_owner=fm_scalar(seller,''),max_price=amount,dispatched_at=Sys.time())
  # Capture the known cash before sending. A balance published during the POST
  # may already include the debit, even if the matching transfer is still pending.
  baseline <- if(mode=='clause')shiny::isolate(tryCatch(cash_baseline(balance_state()),error=function(e)NULL))else NULL
  epoch <- generation
  recorded <- FALSE
  function() {
   if(recorded||session$isClosed()||!identical(ctx$account_id,account)||epoch!=generation)return(invisible(NULL))
   recorded <<- TRUE
   entry <- ledgers[[key]]
   event$acknowledged_at <- Sys.time()
   if(mode!='clause') {
    previous <- Filter(function(x)x$mode!='clause'&&identical(x$player_id,player),entry$actions)
    if(any(vapply(previous,function(x)x$dispatched_at>event$dispatched_at,logical(1))))return(invisible(NULL))
    entry$actions[names(previous)] <- NULL
   }
   if(mode=='clause'&&!any(vapply(entry$actions,function(x)
    x$mode=='clause'&&x$dispatched_at<=event$dispatched_at,logical(1))))entry$baseline <- baseline
   entry$actions[[event$id]] <- event
   ledgers[[key]] <<- entry
   revision(shiny::isolate(revision())+1L)
  }
 }
 session$userData$confirmed_market_offer <- function(ctx) {
  revision()
  bids <- Filter(function(x) identical(x$mode, 'bid') && identical(x$player_id, ctx$player_id),
    ledgers[[scope_key(ctx)]]$actions)
  if(length(bids) == 1L) bids[[1L]]
 }
 session$userData$manual_finance_projection <- function(states, cash_only = FALSE) {
  revision()
  if(!length(ledgers))return(states)
  auth <- login()
  ctx <- resource_context(auth,league(),team())
  key <- scope_key(ctx)
  entry <- ledgers[[key]]
  if(!length(entry$actions))return(states)
  actions <- entry$actions
  balance_at <- fm_manual_finance_time(states$team)
  market_at <- fm_manual_finance_time(states$market)
  market <- states$market$data
  market_known <- is.data.frame(market)&&
   (!nrow(market)||all(c('id','bid_id','bid_price')%in%names(market)))
  market_offers <- if(market_known&&nrow(market))as.data.frame(market)[
   !is.na(market$bid_id)&nzchar(as.character(market$bid_id)),c('id','bid_id','bid_price'),drop=FALSE]else
   data.frame(id=character(),bid_id=character(),bid_price=numeric())
  clauses <- Filter(function(action)action$mode=='clause',actions)
  offers <- actions[setdiff(names(actions),names(clauses))]
  reflected <- if(market_known&&is.finite(market_at))fm_clause_active_offer_matches(offers,market_offers,
   market_at,market_at)else list()
  reflected_ids <- vapply(reflected,`[[`,character(1),'job_id')
  candidates <- Filter(function(action)isTRUE(balance_at>as.numeric(action$dispatched_at)),
   actions[setdiff(names(actions),reflected_ids)])
  completed <- character()
  if(!cash_only&&length(candidates)&&is.finite(balance_at)&&is.finite(fm_number(states$team$data$budget))) {
   history <- history_state()
   history_at <- fm_manual_finance_time(history)
   if(is.finite(history_at))completed <- vapply(
    fm_clause_completed_purchase_matches(candidates,history$data,ctx$user_team_id,balance_at,history_at),
    `[[`,character(1),'job_id')
  }
  # If balance arrives first, keep the known pre-purchase cash and apply the
  # confirmed debit once until transfer identity and balance chronology agree.
  clauses_left <- clauses[setdiff(names(clauses),completed)]
  overlaps <- any(vapply(clauses_left,function(action)
   !is.finite(balance_at)||balance_at>as.numeric(action$dispatched_at),logical(1)))
  settled <- if(overlaps)setdiff(completed,names(clauses))else completed
  if(!overlaps&&length(clauses_left)&&is.finite(fm_number(states$team$data$budget)))entry$baseline <- cash_baseline(states$team)
  if(overlaps) {
   if(is.finite(fm_number(entry$baseline$data$budget))) {
    original <- states$team
    states$team <- entry$baseline
    if(!is.null(original$error))states$team$error <- original$error
   }else states$team <- resource_state(status=states$team$status,error=states$team$error)
   balance_at <- fm_manual_finance_time(states$team)
  }
  debits <- sum(vapply(if(overlaps)clauses else clauses_left,`[[`,numeric(1),'max_price'))
  if(!is.null(states$team$data)&&debits>0) {
   states$team$data$budget <- fm_number(states$team$data$budget) - debits
  }
  # Home shows cash only. Keep the last confirmed debit against its baseline
  # until a full finance view verifies the corresponding transfer.
  if(cash_only) {
   states$confirmed_actions <- debits > 0
   return(states)
  }
  extra_holds <- 0
  displayed_balance_at <- fm_number(as.numeric(fm_time(states$team$observed_at)))
  adjusted_market <- FALSE
  pending_offer <- FALSE
  for(offer in offers[setdiff(names(offers),settled)]) {
   matching <- offer$id%in%reflected_ids
   own <- which(market_offers$id==offer$player_id)
   if(matching&&isTRUE(balance_at>as.numeric(offer$acknowledged_at))) {
    settled <- c(settled,offer$id)
    next
   }
   price <- offer$max_price
   bid_id <- paste0('manual:',offer$id)
   checked_at <- market_at
   absent <- market_known&&offer$mode=='bid'&&!length(own)
   if(!matching&&offer$mode=='offer') {
    summary <- summary_read(login=auth,championship_id=ctx$championship_id,user_team_id=ctx$user_team_id,player_id=offer$player_id)
    checked_at <- fm_manual_finance_time(summary)
    s <- summary$data
    if(isTRUE(checked_at>as.numeric(offer$acknowledged_at))&&
       identical(fm_scalar(s$data$`_id` %||% s$data$id,''),offer$player_id)) {
     saved_id <- fm_scalar(s$my_bid_id,'')
     saved_price <- fm_number(s$my_bid_price)
     if(nzchar(saved_id)&&is.finite(saved_price)&&saved_price>0) {
      bid_id <- saved_id
      price <- saved_price
     }
     absent <- is.list(s$bids)&&!nzchar(saved_id)
    }
   }
   if(absent&&isTRUE(checked_at>as.numeric(offer$acknowledged_at))&&isTRUE(balance_at>=checked_at)) {
    settled <- c(settled,offer$id)
    next
   }
   if(!matching&&market_known) {
    market_offers <- rbind(market_offers[setdiff(seq_len(nrow(market_offers)),own),,drop=FALSE],
     data.frame(id=offer$player_id,bid_id=bid_id,bid_price=price))
    adjusted_market <- TRUE
   }
   if(!matching&&startsWith(bid_id,'manual:'))pending_offer <- TRUE
   if(isTRUE(displayed_balance_at<=as.numeric(offer$dispatched_at)))extra_holds <- extra_holds + price
  }
  if(adjusted_market) {
   attributes(market_offers) <- c(attributes(market_offers),attributes(market)[
    setdiff(names(attributes(market)),c('names','row.names','class','.internal.selfref'))])
   states$market$data <- market_offers
  }
  if(!is.null(states$team$data)&&(debits>0||extra_holds>0)) {
   states$team$data$withheld <- fm_number(states$team$data$withheld) + extra_holds
   limit <- fm_number(states$team$data$maxBid)
   if(is.finite(limit))states$team$data$maxBid <- max(0,limit-debits-extra_holds)
  }
  entry$actions[settled] <- NULL
  if(!length(setdiff(names(clauses),settled)))entry$baseline <- NULL
  ledgers[[key]] <<- if(length(entry$actions))entry else NULL
  states$confirmed_actions <- debits>0 || extra_holds>0 || pending_offer
  states
 }
 invisible(NULL)
}

fm_clause_reconcile_saved_offers <- function(session,login,league,team,history_state,balance_state,market_state) {
 shiny::observe({
  auth<-login()
  if(!valid_login(auth)||!fm_clause_enabled()||!isTRUE(fm_worker_mode(auth[['userid']])))return()
  ctx<-resource_context(auth,league(),team())
  jobs<-Filter(function(j)identical(j$kind,'manual')&&isTRUE(j$mode%in%c('bid','offer','modify','clause'))&&
   j$state%in%c('accepted','uncertain','dispatched'),fm_clause_jobs(ctx))
  if(!length(jobs))return()
  state<-fm_clause_account(ctx$account_id)
  if(isTRUE(state$saved_offers_busy)||!is.null(state$saved_offers_retry))return()
  saved<-function(handler,state,args){
   if(!is.null(state$error)||isTRUE(state$status%in%c('partial','unavailable'))||
      !identical(attr(state$data,'worker_acquisition_status'),'ok')||
      isTRUE(attr(state$data,'worker_completeness')%in%c('partial','unavailable','incomplete')))return(NULL)
   d<-fm_worker_descriptor(handler,args,state$context)
   item<-fm_worker_get(ctx$account_id,d$resource_key)
   if(is.null(item)||!identical(fm_time(item$manifest$observed_at),fm_time(state$observed_at)))return(NULL)
   item$manifest
  }
  team_args<-list(championship_id=ctx$championship_id,user_team_id=ctx$user_team_id)
  balance<-balance_state()
  balance_manifest<-saved('get_user_team_info',balance,team_args)
  if(is.null(balance_manifest)||!is.finite(fm_number(balance$data$budget)))return()
  balance_at<-fm_time(balance_manifest$observed_at)
  balance_payload<-list(p_context=fm_clause_scope(ctx),p_balance_key=balance_manifest$resource_key,p_balance_version=balance_manifest$version)
  write_matches<-function(rpc,evidence_name,manifest,candidates,match){
   signature<-list(manifest$version,balance_manifest$version,lapply(candidates,function(j)
    j[c('id','state','result_code')]))
   if(identical(state$saved_offers_attempted[[evidence_name]],signature))return(FALSE)
   state$saved_offers_attempted[[evidence_name]]<-signature
   matches<-match()
   if(!length(matches))return(FALSE)
   state$saved_offers_busy<-TRUE
   evidence<-setNames(list(manifest$resource_key,manifest$version),paste0('p_',evidence_name,c('_key','_version')))
   payload<-c(balance_payload,evidence,list(p_matches=matches))
   failed<-function(error){
    state$saved_offers_busy<-FALSE
    state$saved_offers_attempted[[evidence_name]]<-NULL
    state$saved_offers_retry<-as.numeric(Sys.time())+30
    NULL
   }
   promises::then(fm_clause_rpc(rpc,payload),function(n){
    state$saved_offers_busy<-FALSE
    if(!is.numeric(n)||length(n)!=1L||!is.finite(n))return(failed(NULL))
    if(n>0)fm_clause_refresh(ctx$account_id,force=TRUE)
    else state$revision(shiny::isolate(state$revision())+1L)
    NULL
   },failed)
   TRUE
  }
  offers<-Filter(function(j)j$mode%in%c('bid','offer','modify'),jobs)
  if(length(offers)){
   market<-market_state()
   market_manifest<-saved('get_market_players',market,team_args)
   if(!is.null(market_manifest)&&write_matches('reconcile_active_offers','market',market_manifest,offers,function()
     fm_clause_active_offer_matches(offers,market$data,balance_at,fm_time(market_manifest$observed_at))))return()
  }
  purchases<-Filter(function(j)j$mode%in%c('bid','clause'),jobs)
  if(!length(purchases))return()
  history<-history_state()
  history_manifest<-saved('get_championship_pressroom',history,team_args['championship_id'])
  if(!is.null(history_manifest))write_matches('reconcile_completed_bids','history',history_manifest,purchases,function()
   fm_clause_completed_purchase_matches(purchases,history$data,ctx$user_team_id,balance_at,fm_time(history_manifest$observed_at)))
 },domain=session)
}

fm_clause_reservations <- function(jobs) {
 if(is.null(jobs))return(list(count=NA_real_,amount=NA_real_))
 active<-Filter(function(j)j$state%in%c('scheduled','preparing','dispatched','uncertain','accepted'),jobs)
 list(count=sum(vapply(active,function(j)fm_number(j$reserved_slot),numeric(1))),
  amount=sum(vapply(active,function(j)fm_number(j$reserved_amount),numeric(1))))
}
fm_clause_adjust_capacity <- function(capacity,jobs) {
 r<-fm_clause_reservations(jobs)
 if(any(!is.finite(c(r$count,r$amount)))){capacity$status<-'partial';capacity$diagnostics<-c(capacity$diagnostics,'scheduled purchase reservations unavailable');return(capacity)}
 capacity$outstanding$count<-fm_number(capacity$outstanding$count)+r$count
 capacity$outstanding$offers<-fm_number(capacity$outstanding$offers)+r$count
 capacity$outstanding$total_amount<-fm_number(capacity$outstanding$total_amount)+r$amount
 capacity$funds$spendable_budget<-max(0,fm_number(capacity$funds$spendable_budget)-r$amount)
 capacity$funds$projected_committed_balance<-fm_number(capacity$funds$projected_committed_balance)-r$amount
 capacity$scheduled_reservations<-r
 capacity
}
fm_clause_finance <- function(finance,jobs,team=NULL) {
 r<-fm_clause_reservations(jobs)
 if(!is.null(jobs)&&is.list(team)){
  observed<-fm_time(attr(team,'observed_at'))
  reflected_later<-Filter(function(j)j$kind=='scheduled'&&j$state=='succeeded'&&
   length(observed)==1L&&!is.na(observed)&&!is.null(j$dispatched_at)&&observed<=fm_time(j$dispatched_at),jobs)
  shadow<-sum(vapply(reflected_later,function(j)fm_number(j$actual_price),numeric(1)))
  r$amount<-r$amount+shadow
  if(shadow>0){finance$calculations_stale<-TRUE;finance$class<-'round-countdown-unavailable'
   finance$message<-'Waiting for the balance snapshot to reflect confirmed clause purchases.'}

 }
 scheduled<-fm_clause_reservations(if(is.null(jobs))NULL else Filter(function(j)j$kind=='scheduled',jobs))
 finance$scheduled_count<-scheduled$count;finance$scheduled_amount<-scheduled$amount
 finance$manual_pending_amount<-if(is.finite(r$amount)&&is.finite(scheduled$amount))max(0,r$amount-scheduled$amount)else NA_real_
 if(!is.finite(r$amount)){
  finance$spendable_budget<-finance$projected_committed_balance<-NA_real_
  finance$class<-'round-countdown-unavailable';finance$message<-'Scheduled purchase reservations could not be verified.'
 }else{
  if(is.list(team)){
   headroom<-acquisition_headroom(finance$cash,fm_number(team$teamValue %||% team$team_value),fm_number(team$withheld),
    finance$commitments,fm_number(team$configuration$mnmp))$spendable_budget
   finance$spendable_budget<-min(max(0,headroom-r$amount),fm_number(team$maxBid))
  }else finance$spendable_budget<-max(0,finance$spendable_budget-r$amount)
  finance$projected_committed_balance<-finance$projected_committed_balance-r$amount
  if(is.finite(finance$projected_committed_balance)&&finance$projected_committed_balance<=0){
   finance$class<-'round-countdown-warning';finance$message<-'Including held funds and pending actions, the projected balance may be nonpositive at kickoff.'
  }
 }
 finance
}
fm_clause_status_text <- function(job) {
 labels<-c(scheduled='Scheduled',preparing='Preparing purchase',dispatched='Purchase sent; checking outcome',uncertain='Checking purchase outcome',
  accepted='Purchase accepted; waiting for financial confirmation',succeeded='Purchase confirmed',failed='Purchase failed',cancelled='Cancelled',expired='Execution window expired')
 paste0(unname(labels[job$state] %||% job$state),if(nzchar(fm_scalar(job$result_code,'')))paste0(' (',fm_scalar(job$result_code,''),')')else'')
}
fm_clause_schedule_ui <- function(ns,player,team,context) {
 if(!fm_clause_enabled())return(NULL)
 jobs<-fm_clause_jobs(context)
 if(is.null(jobs))return(fm_info('Loading scheduled purchase reservations…',pending=TRUE))
 active<-Filter(function(j)identical(j$player_id,player$id)&&j$kind=='scheduled'&&j$state%in%c('scheduled','preparing','dispatched','uncertain','accepted'),jobs)
 if(length(active)){
  j<-active[[1]]
  return(shiny::tagList(shiny::p(fm_clause_status_text(j),': ',fm_format_datetime(j$opens_at)),
   if(j$state%in%c('scheduled','preparing'))shiny::actionButton(ns('cancel_scheduled_clause'),'Cancel scheduled purchase')))
 }
 if(fm_clause_eligible(player,team))shiny::actionButton(ns('schedule_clause'),'Schedule clause purchase',icon=shiny::icon('clock'),style='white-space:normal;max-width:100%;')
}
fm_clause_schedule_admit <- function(auth,ctx,expected,facts,id,owner='') {
 if(!fm_clause_eligible(facts$player,ctx$user_team_id)||!identical(facts$player$owner_id,expected$owner_id)||
  !identical(facts$player$id,expected$id)||facts$player$price>expected$price)stop('Player ownership, clause price or opening changed. Review the player before scheduling.')
 sealed<-fm_clause_encrypt(auth)
 fm_clause_rpc('admit',list(p_id=id,p_context=fm_clause_scope(ctx),p_kind='scheduled',p_mode='clause',p_player=expected,
  p_price=expected$price,p_facts=facts,p_token=sealed,p_credential=attr(sealed,'credential_id')),owner)
}
# Shared session reader: one coalesced poll per account, no source acquisition.
fm_clause_session <- function(session,login) {
 account<-NULL;renewed<-NULL;closed<-FALSE
 detach<-function(){if(!is.null(account)){
  s<-fm_clause_account(account);s$sessions<-setdiff(s$sessions,session$token)
  if(!length(s$sessions)){if(is.function(s$timer))s$timer();rm(list=account,envir=.fm_clause$accounts)}
 }}
 session$onSessionEnded(function(){closed<<-TRUE;detach()})
 shiny::observe({
  auth<-login();if(!valid_login(auth)||!fm_clause_enabled()){detach();account<<-NULL;return()}
  id<-as.character(auth[['userid']])
  if(!identical(account,id)){
   detach();account<<-id;s<-fm_clause_account(id);s$sessions<-union(s$sessions,session$token)
   if(length(s$sessions)==1L){
    tick<-function(){
     if(!length(s$sessions)||!identical(.fm_clause$accounts[[id]],s))return()
     fm_clause_refresh(id)
     if(isTRUE(s$renew_retry)||isTRUE(s$context_retry)){s$renew_retry<-FALSE;s$context_retry<-FALSE;s$revision(shiny::isolate(s$revision())+1L)}
     if(!s$reconcile_busy&&as.numeric(Sys.time())>=s$manual_retry_due){s$manual_retry_due<-Inf;s$revision(shiny::isolate(s$revision())+1L)}
     if(!is.null(s$saved_offers_retry)&&as.numeric(Sys.time())>=s$saved_offers_retry){
      s$saved_offers_retry<-NULL;s$revision(shiny::isolate(s$revision())+1L)
     }
     s$timer<-later::later(tick,5)
    };tick()
   }
  }
  s<-fm_clause_account(id);s$revision();league_context_revision()
  pending_contexts<-Filter(function(j)j$kind=='scheduled'&&j$state%in%c('scheduled','preparing'),s$jobs)
  for(j in pending_contexts){
   metadata<-league_context_metadata(j$context$championship_id,id)
   dimensions<-c('competition_id','season','scoring_version')
   if(is.null(metadata)||!isTRUE(metadata$complete)||!all(vapply(metadata[dimensions],fm_context_known,logical(1))))next
   if(all(vapply(dimensions,function(f)identical(j$context[[f]],metadata[[f]]),logical(1))))next
   source_scope<-metadata[c('account_id','championship_id',dimensions)]
   fingerprint<-fm_worker_json(list(context=source_scope,observed_at=metadata$observed_at))
   if(identical(s$context_updates[[j$context$championship_id]],fingerprint))next
   local({league<-j$context$championship_id;s$context_updates[[league]]<-fingerprint
    promises::then(fm_clause_rpc('retire_context',list(p_context=source_scope,p_observed=metadata$observed_at),session$token),
     function(x){if(!closed)fm_clause_refresh(id,force=TRUE);NULL},
     function(e){s$context_updates[[league]]<-NULL;s$context_retry<-TRUE;NULL})
   })
  }
  key<-paste0(openssl::sha256(charToRaw(auth[['token']])))
  if(s$known&&any(vapply(s$jobs,function(j)j$kind=='scheduled'&&j$state%in%c('scheduled','preparing','dispatched','uncertain','accepted'),logical(1)))&&!identical(renewed,key)){
   renewed<<-key;sealed<-fm_clause_encrypt(auth)
   promises::then(fm_clause_rpc('renew',list(p_account=id,p_token=sealed,p_credential=attr(sealed,'credential_id')),session$token),function(x)NULL,
    function(e){if(!closed){renewed<<-NULL;s$renew_retry<-TRUE};NULL})
  }
  if(s$known&&!s$reconcile_busy){
   manual<-Filter(function(j){a<-s$manual_attempts[[j$id]];j$kind=='manual'&&j$state%in%c('accepted','uncertain','dispatched')&&
    (is.null(a)||a$count<3L&&a$due<=as.numeric(Sys.time()))},s$jobs)
   if(length(manual)){
    j<-manual[[1]];prior<-s$manual_attempts[[j$id]];count<-if(is.null(prior))1L else prior$count+1L
    s$manual_attempts[[j$id]]<-list(count=count,due=as.numeric(Sys.time())+10*count)
    s$reconcile_busy<-TRUE;scope<-j$context
    current<-function()!closed&&valid_login(shiny::isolate(login()))&&identical(shiny::isolate(login()[['userid']]),id)
    check<-async_adapter_fetch('get_scheduled_clause_check',list(login=auth,championship_id=scope$championship_id,
     user_team_id=scope$user_team_id,player_id=j$player_id),context=structure(scope,class='futmondo_context'),
     is_current=current,owner=session$token,priority='interactive',worker_bypass=TRUE)
    check<-promises::then(check,function(facts){if(!current())return(NULL)
     fm_clause_rpc('reconcile_reservations',list(p_context=scope,p_facts=facts),session$token)})
    finish<-function(x){s$reconcile_busy<-FALSE
     if(count<3L)s$manual_retry_due<-as.numeric(Sys.time())+10*count
     if(!closed)fm_clause_refresh(id,force=TRUE);NULL}
    promises::then(check,finish,finish)
   }
  }
  invisible(NULL)
 })
 session$output$clause_results<-shiny::renderUI({
  auth<-login();if(!valid_login(auth)||!fm_clause_enabled())return(NULL)
  s<-fm_clause_account(auth[['userid']]);s$revision()
  jobs<-Filter(function(j)j$kind=='scheduled'&&is.null(j$acknowledged_at)&&j$state%in%c('accepted','succeeded','failed','cancelled','expired','uncertain'),s$jobs)
  if(!length(jobs))return(NULL)
  shiny::tags$section(class='clause-results',style='overflow-wrap:anywhere;max-width:100%;min-width:0;',`aria-label`='Scheduled purchase results',
   shiny::tags$strong('Scheduled purchase results'),
   if(isTRUE(s$more_outcomes))shiny::tags$p('More saved outcomes will appear after you acknowledge these.'),lapply(jobs,function(j)shiny::tags$p(
    j$player_name,' · ',j$context$championship_id,' · ',fm_clause_status_text(j),' · ',fm_format_datetime(j$finished_at %||% j$opens_at),
    shiny::actionButton(paste0('clause_ack_',j$id),'Acknowledge',onclick=sprintf("Shiny.setInputValue('clause_ack_id','%s',{priority:'event'})",j$id)))))
 })
 shiny::observeEvent(session$input$clause_ack_id,{
  auth<-login();shiny::req(valid_login(auth),fm_clause_enabled());id<-session$input$clause_ack_id
  if(!grepl('^[a-f0-9]{32}$',id))return()
  promises::then(fm_clause_rpc('ack',list(p_account=auth[['userid']],p_id=id),session$token),function(ok){fm_clause_refresh(auth[['userid']],force=TRUE);NULL})
 })
 shiny::observeEvent(session$input$clause_revoke,{
  auth<-login();shiny::req(valid_login(auth),fm_clause_enabled())
  promises::then(fm_clause_rpc('revoke',list(p_account=auth[['userid']]),session$token),function(x){fm_clause_refresh(auth[['userid']],force=TRUE);NULL})
 })
}
# All explicit acquisition writes share durable admission, including other views.
# No new auto-retry is introduced for manual actions.
fm_clause_install_guards <- function() {
 if(isTRUE(getOption('futmondo.worker_helper',FALSE)))return(invisible(NULL))
 for(name in c('buy_clause','buy_roster_clause','modify_bid'))local({
  original<-get(name,.GlobalEnv);handler<-name
  wrapper<-function(){
   args<-lapply(as.list(match.call())[-1L],eval,envir=parent.frame())
   if(!fm_clause_enabled())return(do.call(original,args))
   sent<-FALSE
   tryCatch({
   auth<-args$login;ctx<-resource_context(auth,args$championship_id,args$team_id,args$player_id)
   # Force fresh live preflight after acquiring intent, never trust a display cache.
   invalidate_resources(ctx,pattern='acq_capacity|team_info|market_players|financial_snapshot|player_summary')
   facts<-get_scheduled_clause_check(auth,args$championship_id,args$team_id,args$player_id,
    require_clause=handler=='buy_roster_clause'||handler=='buy_clause'&&isTRUE(args$isClause %||% TRUE))
   mode<-if(handler=='modify_bid')'modify'else if(handler=='buy_roster_clause'||isTRUE(args$isClause %||% TRUE))'clause'else'offer'
   if(mode=='clause'&&(identical(facts$player$owner_id,as.character(args$team_id))||
     !nzchar(fm_scalar(facts$player$owner_id,''))||!isFALSE(facts$player$transferred)||
     is.na(fm_time(facts$player$opens_at))||fm_time(facts$player$opens_at)>Sys.time()))
     stop('The current clause ownership or opening could not be verified.')
   price<-if(handler=='modify_bid')args$new_price else args$price
   if(mode=='clause'){
    if(facts$player$price>price)stop('The current clause exceeds the confirmed price.')
    args$price<-price<-facts$player$price;args$player_slug<-facts$player$slug
   }
   id<-fm_clause_id()
   job<-fm_clause_rpc_sync('admit',list(p_id=id,p_context=fm_clause_scope(ctx),p_kind='manual',p_mode=mode,
    p_player=facts$player,p_price=price,p_facts=facts))
   if(!is.list(job)||!identical(job$id,id))stop('Purchase reservation was not acknowledged.')
   sent<-FALSE
   on.exit({if(!sent)try(fm_clause_rpc_sync('settle',list(p_id=id,p_owner='',p_state='failed',p_code='dispatch_not_authorized')),silent=TRUE)
    fm_clause_refresh(auth[['userid']],force=TRUE)},add=TRUE)
   if(!isTRUE(fm_clause_rpc_sync('dispatch',list(p_id=id,p_owner='',p_facts=facts))))stop('The purchase no longer passes current offer or funds checks.')
   sent<-TRUE
   response<-tryCatch(do.call(original,args),error=function(e)list(success=FALSE,code='error'))
   response<-fm_clause_result(response)
   code<-fm_clause_safe_code(response$code %||% '')
   state<-if(isTRUE(response$success))'accepted'else if(grepl('^api\\.error\\.',code)||identical(code,'api.market.max_number_players_in_roster'))'failed'else'uncertain'
   tryCatch(fm_clause_rpc_sync('settle',list(p_id=id,p_owner='',p_state=state,p_code=code)),error=function(e)NULL)
   if(state=='uncertain')response$message<-'Purchase response is uncertain. Its reservation remains active; check the source outcome before resubmitting.'
   invalidate_resources(ctx,pattern='acq_capacity|team_info|market_players|financial_snapshot|player_summary')
   if(handler=='modify_bid')structure(isTRUE(response$success),api_code=response$code,failure_message=response$message)else response
   },error=function(error){result<-list(success=FALSE,code=if(sent)'purchase_response_uncertain'else'acquisition_verification_unavailable',
    message=if(sent)'The purchase response is uncertain. Its reservation remains active; check the source outcome before resubmitting.'else
     'The purchase was not sent. Current player facts, offer capacity, funds or database access could not be verified.');if(handler=='modify_bid')structure(FALSE,api_code=result$code,failure_message=result$message)else result})
  }
  formals(wrapper)<-formals(original);attr(wrapper,'clause_original')<-original;assign(handler,wrapper,.GlobalEnv)
 })
 invisible(NULL)
}

fm_clause_card_observers <- function(input,session,selected,auth,championship,team,remember,consume,current_context) {
 confirmation<-shiny::reactiveVal(NULL);busy<-shiny::reactiveVal(FALSE)
 shiny::observeEvent(input$schedule_clause,{
  shiny::req(fm_clause_enabled(),!busy());sp<-selected();player<-fm_clause_row_player(sp)
  shiny::req(fm_clause_eligible(player,team()))
  ctx<-resource_context(auth(),championship(),team(),player$id)
  confirmation(list(id=fm_clause_id(),context=ctx,player=player,action=current_context()))
  remember('schedule_clause')
  shiny::showModal(shiny::modalDialog(title='Schedule clause purchase',
   shiny::p(shiny::strong(player$name)),
   shiny::p('Maximum price: ',player_card_money(player$price)),
   shiny::p('Published opening: ',fm_format_datetime(player$opens_at),' (',fm_display_timezone(),')'),
   shiny::p('The service targets the opening second. Verified lock extensions move the schedule. A lower price is accepted. A higher price cancels the job.'),
   shiny::p(class='text-warning','This schedules a purchase attempt. Success is not guaranteed. The purchase may leave your balance negative.'),
   shiny::p('This reserves one offer slot and the maximum price in LigaRadar. It is not an official Futmondo offer until execution.'),
   shiny::checkboxInput(session$ns('schedule_clause_consent'),'Authorize this purchase after logout using an encrypted token, independently of Background sync.',FALSE),
   shiny::p('If Futmondo rejects the purchase, the attempt ends.'),
   footer=shiny::tagList(shiny::modalButton('Cancel'),shiny::actionButton(session$ns('confirm_scheduled_clause'),'Confirm schedule')),easyClose=FALSE))
 })
 shiny::observeEvent(input$confirm_scheduled_clause,{
  shiny::req(fm_clause_enabled(),!busy(),isTRUE(input$schedule_clause_consent),consume('schedule_clause'))
  request<-confirmation();shiny::req(!is.null(request),identical(request$action,current_context()))
  busy(TRUE);shiny::removeModal();login<-auth();ctx<-request$context
  current<-function()!session$isClosed()&&identical(shiny::isolate(current_context()),request$action)
  check<-async_adapter_fetch('get_scheduled_clause_check',list(login=login,championship_id=ctx$championship_id,
   user_team_id=ctx$user_team_id,player_id=ctx$player_id,require_clause=TRUE),context=ctx,is_current=current,
   owner=session$token,priority='interactive',worker_bypass=TRUE)
  check<-promises::then(check,function(facts){
   if(!current())stop('Scheduling context changed')
   fm_clause_schedule_admit(login,ctx,request$player,facts,request$id,session$token)
  })
  promises::then(check,function(job){
   busy(FALSE);confirmation(NULL)
   if(!is.list(job)||!identical(job$id,request$id)){
    shiny::showNotification('Scheduling acknowledgement is uncertain. Check the pending purchases before trying again.',type='warning',duration=NULL)
   }else shiny::showNotification('Clause purchase scheduled and saved. Execution is not guaranteed.',type='message',duration=8)
   fm_clause_refresh(login[['userid']],force=TRUE);NULL
  },function(error){
   busy(FALSE);confirmation(NULL);fm_clause_refresh(login[['userid']],force=TRUE)
   if(!session$isClosed())shiny::showNotification('Could not confirm scheduling. Check pending purchases; current source facts, offer capacity, funds or database access may have changed.',type='error',duration=12)
   NULL
  })
  invisible(NULL)
 })
 shiny::observeEvent(input$cancel_scheduled_clause,{
  shiny::req(fm_clause_enabled());login<-auth();sp<-selected()
  jobs<-fm_clause_jobs(resource_context(login,championship(),team(),sp$id))
  target<-Filter(function(j)j$kind=='scheduled'&&identical(j$player_id,as.character(sp$id))&&j$state%in%c('scheduled','preparing'),jobs)
  shiny::req(length(target)==1L)
  promises::then(fm_clause_rpc('cancel',list(p_account=login[['userid']],p_id=target[[1]]$id),session$token),function(ok){
   shiny::showNotification(if(isTRUE(ok))'Scheduled purchase cancelled.'else'Cancellation was too late; the purchase may already have been sent. Check its outcome.',type=if(isTRUE(ok))'message'else'warning')
   fm_clause_refresh(login[['userid']],force=TRUE);NULL
  },function(e){shiny::showNotification('Cancellation was not acknowledged. The purchase may still execute.',type='error',duration=NULL);NULL})
  invisible(NULL)
 })
 invisible(NULL)
}

fm_clause_pending_ui <- function(jobs) {
 scheduled<-if(is.null(jobs))NULL else Filter(function(j)j$kind=='scheduled',jobs)
 r<-fm_clause_reservations(scheduled)
 amount<-if(is.finite(r$amount))paste0(format(round(r$amount),big.mark='.',decimal.mark=',',scientific=FALSE),' EUR')else'Unavailable'
 pending<-Filter(function(j)j$state%in%c('scheduled','preparing','dispatched','uncertain','accepted'),scheduled %||% list())
 eur<-function(x)if(is.finite(fm_number(x)))paste0(format(round(fm_number(x)),big.mark='.',decimal.mark=',',scientific=FALSE),' EUR')else'Unavailable'
 # Holds for submitted manual actions, including acknowledged offers.
 manual<-if(is.null(jobs))list()else Filter(function(j)identical(j$kind,'manual')&&j$state%in%c('dispatched','uncertain','accepted'),jobs)
 shiny::tagList(
  if(is.null(jobs)||length(pending))shiny::tags$details(class='round-countdown-finance-item',shiny::tags$summary(paste0('Scheduled clause purchases: ',amount,' (',if(is.finite(r$count))r$count else'?',')')),
   if(is.null(jobs))shiny::tags$p('Scheduled purchase information is unavailable.'),
   lapply(pending,function(j)shiny::tags$p(shiny::tags$strong(j$player_name %||% 'Unknown player'),' · ',eur(j$reserved_amount),' · opens ',fm_format_datetime(j$opens_at),' · ',fm_clause_status_text(j))),
   if(length(pending))shiny::actionButton('clause_revoke','Cancel all scheduled purchases and revoke access',style='white-space:normal;max-width:100%;')),
  lapply(c('unconfirmed','accepted'),function(group){
   accepted<-identical(group,'accepted')
   entries<-Filter(function(j)identical(j$state,'accepted')==accepted,manual)
   if(!length(entries))return(NULL)
   label<-if(accepted)'Accepted offers awaiting balance update' else 'Offers awaiting confirmation'
   explanation<-if(accepted)'Futmondo accepted these actions. The amounts are held until the saved financial data accounts for them.'
    else 'LigaRadar is checking the result. The amounts are held until the outcome is verified.'
   shiny::tags$details(class='round-countdown-finance-item',
    shiny::tags$summary(paste0(label,': ',eur(sum(vapply(entries,function(j)fm_number(j$reserved_amount),numeric(1)))),' (',length(entries),')')),
    shiny::tags$p(explanation),
    lapply(entries,function(j)shiny::tags$p(shiny::tags$strong(j$player_name %||% 'Unknown player'),' · ',eur(j$reserved_amount),' · ',
     if(identical(j$mode,'clause'))'Clause purchase' else 'Offer')))
  }))
}

# Share the exact native builders with the ordinary synchronous API functions.
fm_clause_action_payload <- function(handler,args) {
 builder<-switch(handler,buy_clause=build_market_bid_payload,buy_roster_clause=build_roster_clause_payload,
  modify_bid=build_modify_bid_payload,NULL)
 url<-switch(handler,buy_clause=BID_URL,buy_roster_clause=ROSTER_CLAUSE_URL,modify_bid=MODIFY_BID_URL,NULL)
 if(is.null(builder)||is.null(url))stop('Unsupported acquisition action')
 list(url=url,body=as.character(jsonlite::toJSON(do.call(builder,args),auto_unbox=TRUE)))
}
fm_clause_result <- function(value) {
 if(is.list(value))return(value)
 list(success=isTRUE(value),code=attr(value,'api_code') %||% if(isTRUE(value))API_CODE_OK else'unclassified_response',
  message=attr(value,'failure_message') %||% '')
}
fm_clause_execute_async <- function(handler,args,context,player,is_current,owner='',action_mode=NULL) {
 invalidate_resources(context,pattern='acq_capacity|player_summary|market_players|team_info')
 id<-NULL;dispatched<-FALSE;mode<-if(handler=='modify_bid')'modify'else if(handler=='buy_roster_clause'||isTRUE(args$isClause %||% TRUE))'clause'else'offer'
 # A direct owner offer and a market bid share the native write endpoint.
 if(handler=='buy_clause'&&!isTRUE(args$isClause %||% TRUE)&&isTRUE(player$market_inMarket))mode<-'bid'
 if(!is.null(action_mode)){stopifnot(action_mode%in%c('bid','offer','modify','clause'));mode<-action_mode}
 stage<-'live_facts'
 read_facts<-function()async_adapter_fetch('get_scheduled_clause_check',list(login=args$login,championship_id=args$championship_id,
  user_team_id=args$team_id,player_id=args$player_id,require_clause=mode=='clause'),context=context,
  is_current=is_current,owner=owner,priority='interactive',worker_bypass=TRUE)
 # A long prerequisite read can outlast the first funds observation. Start
 # one fresh read before admission so expired inputs refresh and valid inputs
 # remain reusable. A dispatched or uncertain write never enters this path.
 check<-promises::then(read_facts(),function(facts){
  if(!isTRUE(is_current()))stop('Action context changed')
  deadline<-fm_time(facts$valid_until)
  if(length(deadline)==1L&&!is.na(deadline)&&deadline<=Sys.time()){
   stage<<-'live_facts_recheck'
   return(read_facts())
  }
  facts
 })
 check<-promises::then(check,function(facts){
  if(!isTRUE(is_current()))stop('Action context changed')
  amount<-if(handler=='modify_bid')args$new_price else args$price
  existing<-Filter(function(o)identical(o$player_id,as.character(args$player_id)),facts$offers)
  if(mode=='modify'&&(length(existing)!=1L||!identical(existing[[1]]$bid_id,as.character(args$bid_id))))stop('The existing offer changed')
  if(mode%in%c('bid','offer')&&length(existing))stop('An offer on this player already exists')
  if(mode=='clause'){
   p<-facts$player
   opens_at<-fm_time(p$opens_at)
   if(!nzchar(p$owner_id)||identical(p$owner_id,as.character(args$team_id))||!isFALSE(p$transferred)||
    !is.finite(p$price)||p$price>amount||length(opens_at)!=1L||is.na(opens_at)||opens_at>Sys.time())stop('Current clause eligibility changed')
   amount<-args$price<-p$price;args$player_slug<-p$slug
  }
  minimum<-if(mode%in%c('bid','modify'))market_bid_minimum(player,facts$rules$minimum_bid_at_value)else NULL
  numeric_capacity<-facts;numeric_capacity$valid_until<-as.numeric(fm_time(facts$valid_until))
  decision<-evaluate_acquisition_preflight(numeric_capacity,if(mode=='clause')'offer'else mode,amount,
   existing_bid_amount=if(length(existing))existing[[1]]$price else NULL,minimum_bid=minimum)
  if(!isTRUE(decision$ok))
   return(list(success=FALSE,code='acquisition_preflight_blocked',message=decision$message))
  id<<-fm_clause_id()
  request<-fm_clause_action_payload(handler,args)
  payload<-list(p_id=id,p_context=fm_clause_scope(context),p_kind='manual',p_mode=mode,p_player=facts$player,p_price=amount,p_facts=facts)
  stage<<-'admission'
  promises::then(fm_clause_rpc('admit',payload,owner),function(job){
   if(!is.list(job)||!identical(job$id,id)||!isTRUE(is_current()))stop('Action admission was not confirmed')
   stage<<-'dispatch'
   promises::then(fm_clause_rpc('dispatch',list(p_id=id,p_owner='',p_facts=facts),owner),function(ok){
    if(!isTRUE(ok))stop('Dispatch preflight changed')
    dispatched<<-TRUE
    if(!isTRUE(is_current())){
     # No HTTP request has been queued, so this failure is certain.
     dispatched<<-FALSE;stop('Action context changed before send')
    }
    # A queued mutation is independent of browser cancellation; it is never retried.
    stage<<-'native_write'
    promises::then(fm_async_http(request$url,'POST',request$body,c(`Content-Type`='application/json; charset=utf-8'),owner='',priority='interactive'),function(response){
     if(response$status_code<200L||response$status_code>=300L)stop('Purchase HTTP acknowledgement is uncertain')
     answer<-jsonlite::fromJSON(rawToChar(response$content),simplifyVector=FALSE)$answer
     code<-fm_clause_safe_code(if(is.list(answer))answer$code else NULL)
     result<-list(success=identical(code,API_CODE_OK),code=code,message=clause_buyout_error_message(code))
     state<-if(result$success)'accepted'else if(grepl('^api\\.error\\.',code)||code=='api.market.max_number_players_in_roster'||isTRUE(answer$error))'failed'else'uncertain'
     if(state=='uncertain')result$message<-'The purchase response is uncertain. Its reservation remains active; check the source outcome before resubmitting.'
     settle<-fm_clause_rpc('settle',list(p_id=id,p_owner='',p_state=state,p_code=code),owner='')
     promises::then(settle,function(ack){result$durable<-isTRUE(ack);result},function(e){result$durable<-FALSE;result})
    })
   })
  })
 })
 promises::then(check,function(result){
  if(identical(result$code,'acquisition_preflight_blocked'))return(result)
  invalidate_resources(context,pattern='acq_capacity|team_info|market_players|financial_snapshot|player_summary|roster')
  fm_clause_refresh(context$account_id,force=TRUE);result
 },function(error){
  if(isTRUE(is_current()))message(sprintf('[Futmondo acquisition] mode=%s stage=%s',mode,stage))
  state<-if(dispatched)'uncertain'else'failed'
  if(!is.null(id))promises::then(fm_clause_rpc('settle',list(p_id=id,p_owner='',p_state=state,
    p_code=if(dispatched)'purchase_response_uncertain'else'dispatch_not_sent'),owner=''),function(x)NULL,function(e)NULL)
  fm_clause_refresh(context$account_id,force=TRUE)
  list(success=FALSE,code=if(dispatched)'purchase_response_uncertain'else'acquisition_verification_unavailable',
   message=if(dispatched)'The purchase response is uncertain. Its reservation remains active; check the source outcome before resubmitting.'else
    'The purchase was not sent. Current player facts, offer capacity, funds or database access could not be verified.')
 })
}
# Called with the session's scoped jobs and a lazy lookup of its saved capacity.
fm_manual_reservation_conflict <- function(jobs, player_id, amount, capacity) {
  if (is.null(jobs)) return("Scheduled purchase information is unavailable in this session.")
  active <- Filter(function(job) identical(job$kind, "scheduled") &&
    job$state %in% c("scheduled", "preparing", "dispatched", "uncertain", "accepted"), jobs)
  if (!length(active)) return(NULL)
  if (any(vapply(active, function(job) identical(job$player_id, player_id), logical(1)))) {
    return("You already have a scheduled purchase for this player.")
  }
  held <- fm_clause_reservations(active)
  free_slots <- fm_number(capacity$roster$cap) - fm_number(capacity$roster$effective_count) -
    fm_number(capacity$outstanding$count)
  funds <- fm_number(capacity$funds$spendable_budget)
  if (!identical(capacity$status, "ok") || !all(is.finite(c(held$count, held$amount, free_slots, funds)))) {
    return("Saved team figures are unavailable to verify your scheduled purchase reservations.")
  }
  if (free_slots <= held$count) return("The remaining slots are reserved for scheduled purchases.")
  if (amount + held$amount > funds) return("This amount would use funds reserved for scheduled purchases.")
  NULL
}

# Each consumed confirmation queues a single native purchase or offer.
fm_send_manual_acquisition <- function(handler, args, on_confirmed = NULL) {
  clause <- identical(handler, "buy_roster_clause")
  request <- fm_clause_action_payload(handler, args)
  response <- fm_async_http(request$url, "POST", request$body,
    c(`Content-Type` = "application/json; charset=utf-8"), owner = "", priority = "interactive")
  result <- promises::then(response, function(value) {
    if (value$status_code < 200L || value$status_code >= 300L) stop("Missing purchase acknowledgement")
    answer <- jsonlite::fromJSON(rawToChar(value$content), simplifyVector = FALSE)$answer
    code <- fm_clause_safe_code(answer$code)
    if (identical(code, API_CODE_OK)) {
      if (is.function(on_confirmed)) tryCatch(on_confirmed(), error = function(e)
        message("[Futmondo] Could not update the session balance after a confirmed action."))
      return(list(success = TRUE, code = code))
    }
    if (startsWith(code, "api.error.") || startsWith(code, "api.market.") || isTRUE(answer$error)) {
      return(list(success = FALSE, code = code,
        message = if (clause) clause_buyout_error_message(code, answer$msg %||% answer$message) else answer$msg %||% answer$message %||% code, refresh = TRUE))
    }
    stop("Unrecognized purchase acknowledgement")
  })
  promises::catch(result, function(error) {
    if (inherits(error, "futmondo_unavailable") && identical(error$category, "read_budget")) {
      return(list(success = FALSE, code = "dispatch_not_sent", message = "The request queue is full. Your request was not sent."))
    }
    list(success = FALSE, code = "purchase_response_uncertain",
      refresh = TRUE, message = if (clause) "Futmondo did not confirm the purchase. Check your transfers before trying again." else "Futmondo did not confirm the offer. Check your active offers before trying again.")
  })
}
fm_send_player_offer <- function(args, player, context, on_confirmed = NULL) {
  outcome <- fm_send_manual_acquisition("buy_clause", args, on_confirmed)
  promises::then(outcome, function(result) {
    if (isTRUE(result$refresh)) {
      result$refresh_queued <- tryCatch({
        invalidate_manual_resources(args$login, context$championship_id, context$user_team_id, context$player_id)
        isTRUE(fm_worker_refresh_league(args$login, context$championship_id, context$user_team_id, context$player_id))
      }, error = function(e) FALSE)
    }
    if (isTRUE(result$success)) {
      tryCatch(log_market_transaction(args$player_id, args$championship_id, args$team_id, player$user_team_id, args$price),
        error = function(e) message("[Futmondo] Could not queue the confirmed offer record."))
    }
    result
  })
}

# Detach callbacks from Shiny's input promise chain so a held request does not
# suspend navigation or heartbeat inputs in that same browser session.
fm_clause_complete <- function(result,callback,is_current=function()TRUE) {
 if(inherits(result,'fm_clause_async_action')){is_current<-result$current;result<-result$result}
 if(promises::is.promise(result)){
  promises::then(result,function(value){if(isTRUE(is_current()))callback(value);NULL},function(error){
   if(isTRUE(is_current()))callback(list(success=FALSE,code='purchase_response_uncertain',message='The purchase outcome needs reconciliation.'));NULL})
 }else callback(result)
 invisible(NULL)
}
