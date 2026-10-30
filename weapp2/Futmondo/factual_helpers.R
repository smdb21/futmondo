# Factual accounting, numeric, identity and manual-action helpers.

safe_numeric <- function(x, default = NA_real_) {
  if (is.null(x)) return(rep(default, 1))
  vals <- suppressWarnings(as.numeric(as.character(x)))
  vals[is.na(vals) | is.nan(vals)] <- default
  vals
}

safe_clamp <- function(x, lo = 0, hi = 100) {
  pmin(pmax(x, lo), hi)
}

default_if_null_na <- function(x, default) {
  if (is.null(x)) return(default)
  if (length(x) == 1 && is.na(x)) return(default)
  x
}

analytics_numeric_column <- function(df, column, default = NA_real_) {
  if (!column %in% names(df)) return(rep(default, nrow(df)))
  x <- suppressWarnings(as.numeric(as.character(df[[column]])))
  x[!is.finite(x)] <- default
  x
}

player_value_growth_ratio <- function(players_df) {
  if (is.null(players_df) || !is.data.frame(players_df)) return(numeric(0))
  value <- analytics_numeric_column(players_df, "value")
  change <- analytics_numeric_column(players_df, "change")
  ratio <- ifelse(value > 0 & is.finite(change), change / value, NA_real_)
  ratio[!is.finite(ratio)] <- NA_real_
  ratio
}

market_bid_minimum <- function(player_row) {
  if (is.data.frame(player_row)) {
    if (nrow(player_row) == 0L) return(NA_real_)
    player_row <- as.list(player_row[1, , drop = FALSE])
  }
  if (!is.list(player_row)) return(NA_real_)
  positive_amount <- function(x) {
    if (length(x) != 1L) return(NA_real_)
    value <- suppressWarnings(as.numeric(as.character(x)))
    if (is.finite(value) && value > 0) value else NA_real_
  }
  value <- positive_amount(player_row$value)
  asking <- NA_real_
  for (field in c("effective_market_price", "market_price", "price")) {
    candidate <- positive_amount(player_row[[field]])
    if (is.finite(candidate)) {
      asking <- candidate
      break
    }
  }
  amounts <- c(value, asking)
  if (!any(is.finite(amounts))) return(NA_real_)
  ceiling(max(amounts, na.rm = TRUE))
}

analytics_time <- function(x) {
  x <- as.character(x)
  result <- suppressWarnings(as.POSIXct(x, format = '%Y-%m-%dT%H:%M:%OS', tz = 'UTC'))
  missing <- is.na(result)
  if (any(missing)) result[missing] <- suppressWarnings(as.POSIXct(x[missing], format = '%Y-%m-%d %H:%M:%OS', tz = 'UTC'))
  result
}

summarize_manager_transactions <- function(team_id, pressroom_df, user_teams_df = NULL) {
  result <- list(team_id = team_id, aggressiveness = NA_real_, avg_overpayment_pct = NA_real_,
    fav_position = 'Unknown', trading_frequency = 0, avg_holding_days = NA_real_, total_trades = 0,
    insights = 'Insufficient observed transactions.', premium_observations = 0L, method = 'observed_transactions')
  if (length(team_id) != 1L || is.na(team_id) || !nzchar(team_id)) return(result)
  if (!is.data.frame(pressroom_df) || !nrow(pressroom_df) ||
      !all(c('buyer_team_id', 'seller_team_id', 'player_id', 'price', 'created') %in% names(pressroom_df))) return(result)
  tx <- pressroom_df
  if ('id' %in% names(tx)) tx <- tx[!duplicated(tx$id), , drop = FALSE]
  tx <- tx[tx$buyer_team_id %in% team_id | tx$seller_team_id %in% team_id, , drop = FALSE]
  if (!nrow(tx)) return(result)
  buy <- tx$buyer_team_id %in% team_id
  result$total_trades <- nrow(tx)
  prices <- analytics_numeric_column(tx, 'price')
  reference <- rep(NA_real_, nrow(tx))
  for (column in c('market_value_at_time', 'reference_value', 'value_at_transaction')) {
    if (column %in% names(tx)) {
      candidate <- analytics_numeric_column(tx, column)
      use <- !is.finite(reference) & is.finite(candidate) & candidate > 0
      reference[use] <- candidate[use]
    }
  }
  usable <- buy & is.finite(prices) & is.finite(reference) & reference > 0
  if (any(usable)) {
    premium <- prices[usable] / reference[usable] - 1
    result$avg_overpayment_pct <- round(mean(premium) * 100, 2)
    result$premium_observations <- sum(usable)
    result$aggressiveness <- round(safe_clamp(50 + 100 * stats::median(premium)), 1)
  }
  if ('role' %in% names(tx) && any(buy)) {
    roles <- as.character(tx$role[buy]); roles <- roles[!is.na(roles) & nzchar(roles)]
    if (length(roles)) result$fav_position <- names(sort(table(roles), decreasing = TRUE))[1]
  }
  dates <- analytics_time(tx$created)
  valid <- !is.na(dates)
  if (any(valid)) {
    span <- max(1, as.numeric(difftime(max(dates[valid]), min(dates[valid]), units = 'days')) + 1)
    result$trading_frequency <- round(sum(valid) / span * 7, 2)
  }
  holdings <- numeric()
  for (pid in unique(tx$player_id)) {
    idx <- which(tx$player_id %in% pid & valid)
    idx <- idx[order(dates[idx], seq_along(idx))]
    acquired <- as.POSIXct(character(), tz = 'UTC')
    for (i in idx) {
      if (buy[i]) acquired <- c(acquired, dates[i])
      else if (length(acquired)) {
        holdings <- c(holdings, as.numeric(difftime(dates[i], acquired[1], units = 'days')))
        acquired <- acquired[-1]
      }
    }
  }
  if (length(holdings)) result$avg_holding_days <- round(mean(holdings[holdings >= 0]), 2)
  result$insights <- paste0(nrow(tx), ' observed transfers; ', result$trading_frequency,
    ' per observed week. ', if (result$premium_observations > 0)
      paste0('Average premium ', result$avg_overpayment_pct, '% from ', result$premium_observations, ' historical reference prices.')
      else 'Historical reference prices unavailable; overpayment cannot be estimated.')
  result
}

current_player_acquisition_cost <- function(player_row, pressroom_df, user_team_id) {
  p <- if (is.data.frame(player_row) && nrow(player_row)) as.list(player_row[1, , drop=FALSE]) else player_row
  if (is.list(p)) for (field in c("buyPrice", "buy_price", "acquisition_cost")) {
    cost <- suppressWarnings(as.numeric(as.character(p[[field]])))
    if (length(cost)==1L && is.finite(cost) && cost>0) return(cost)
  }
  required <- c("player_id","buyer_team_id","seller_team_id","price","created")
  if (!is.list(p) || is.null(p$id) || !is.data.frame(pressroom_df) || !nrow(pressroom_df) ||
      !all(required %in% names(pressroom_df)) || length(user_team_id)!=1L || is.na(user_team_id)) return(NA_real_)
  tx <- pressroom_df[as.character(pressroom_df$player_id)==as.character(p$id),,drop=FALSE]
  if (!nrow(tx)) return(NA_real_)
  if ("id" %in% names(tx)) tx <- tx[!duplicated(tx$id),,drop=FALSE]
  tx <- tx[order(analytics_time(tx$created),seq_len(nrow(tx))),,drop=FALSE]
  cost <- NA_real_
  for (i in seq_len(nrow(tx))) {
    price <- suppressWarnings(as.numeric(as.character(tx$price[i])))
    if (as.character(tx$buyer_team_id[i])==as.character(user_team_id) && is.finite(price) && price>0) cost <- price
    if (as.character(tx$seller_team_id[i])==as.character(user_team_id)) cost <- NA_real_
  }
  cost
}

clause_value_comparison <- function(clause_price, market_value) {
  clause <- suppressWarnings(as.numeric(as.character(clause_price)))
  market <- suppressWarnings(as.numeric(as.character(market_value)))
  if (length(clause)!=1L || length(market)!=1L || !is.finite(clause) || clause<=0 ||
      !is.finite(market) || market<=0) return(list(available=FALSE,percent=NA_real_,text="market-value comparison unavailable",favorable=NA))
  percent <- round(abs(clause / market - 1) * 100, 1)
  if (clause < market) return(list(available=TRUE,percent=percent,
    text=paste0(percent,"% below market value"),favorable=TRUE))
  if (clause > market) return(list(available=TRUE,percent=percent,
    text=paste0(percent,"% above market value"),favorable=FALSE))
  list(available=TRUE,percent=0,text="at market value",favorable=TRUE)
}

analytics_player_positions <- function(squad_df, multiposition = FALSE) {
  aliases <- c('1'='GK', gk='GK', goalkeeper='GK', portero='GK', '2'='DEF', df='DEF', def='DEF', defender='DEF', defensa='DEF',
    '3'='MID', mf='MID', md='MID', mid='MID', midfielder='MID', mediocampista='MID', centrocampista='MID', '4'='FWD', fw='FWD', fwd='FWD', forward='FWD', delantero='FWD')
  lapply(seq_len(nrow(squad_df)), function(i) {
    values <- character()
    columns <- if (isTRUE(multiposition)) c('primary_role', 'role', 'role2', 'position', 'eligible_positions') else c('primary_role', 'role', 'position')
    for (column in columns) {
      if (column %in% names(squad_df)) {
        candidate <- as.character(unlist(squad_df[[column]][i]))
        candidate <- candidate[!is.na(candidate) & nzchar(trimws(candidate))]
        values <- c(values, candidate)
        if (!isTRUE(multiposition) && length(candidate)) break
      }
    }
    pieces <- trimws(tolower(unlist(strsplit(values[!is.na(values)], '[,;/|]+'))))
    resolved <- unique(unname(aliases[pieces[pieces %in% names(aliases)]]))
    if (isTRUE(multiposition)) resolved else head(resolved, 1L)
  })
}

analytics_known_unavailable <- function(squad_df) {
  unavailable <- if ('status' %in% names(squad_df))
    tolower(trimws(as.character(squad_df$status))) %in% c('injured', 'injured2', 'redcard', 'suspended', 'unavailable') else rep(FALSE,nrow(squad_df))
  if ('available' %in% names(squad_df) && is.logical(squad_df$available)) unavailable <- unavailable | squad_df$available %in% FALSE
  unavailable
}

normalize_fixture_observation <- function(summary,player_id,championship_id,season,scoring_version) {
  stamp<-attr(summary,'observed_at');match<-summary$match
  date<-fm_time(match$info$date %||% NA_character_);round<-fm_number(match$r$number)
  id<-fm_scalar(match$`_id`,'')
  if(is.null(stamp)||is.na(date)||!is.finite(round)||!nzchar(id)||identical(attr(summary,'fetch_status'),'stale'))return(data.frame())
  data.frame(championship_id=championship_id,season=season,scoring_version=scoring_version,player_id=player_id,
    round=round,fixture_id=id,occurred_at=format(date,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),
    observed_at=format(stamp,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))
}

normalize_listing_opportunities <- function(market,championship_id) {
  if(!is.data.frame(market)||!all(c('id','expirationDate')%in%names(market)))return(data.frame())
  stamp<-attr(market,'observed_at')
  if(is.null(stamp)||identical(attr(market,'fetch_status'),'stale'))return(data.frame())
  expiry<-fm_time(market$expirationDate)
  keep<-!is.na(expiry)&!is.na(market$id)
  if(!any(keep))return(data.frame())
  data.frame(championship_id=championship_id,listing_id=paste(market$id[keep],market$expirationDate[keep],sep=':'),
    player_id=as.character(market$id[keep]),first_observed_at=format(stamp,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),
    observed_at=format(stamp,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),expires_at=as.character(market$expirationDate[keep]),
    outcome=ifelse(expiry[keep]>stamp,'open','unknown'),visibility_complete=FALSE)
}

record_verified_auction_outcome <- function(championship_id,listing_id,auction,bids,evidence) {
  # Called only by a verified settlement adapter; disappearing listings are not unsold evidence.
  if(!isTRUE(evidence$settlement_verified)||!nzchar(fm_scalar(evidence$source,'')) ||
     !fm_scalar(auction$outcome,'')%in%c('sold','unsold'))return(FALSE)
  if(!nzchar(fm_scalar(listing_id,'')) || !nzchar(fm_scalar(auction$auction_id,'')) ||
     !nzchar(fm_scalar(auction$player_id,'')) || is.na(fm_time(auction$observed_at %||% NA_character_)))return(FALSE)
  if(!is.null(auction$championship_id)&&!identical(auction$championship_id,championship_id))return(FALSE)
  if(is.data.frame(bids)&&nrow(bids)) {
    if(!all(c('championship_id','auction_id','player_id','bidder_id')%in%names(bids)))return(FALSE)
    if(anyNA(bids[c('championship_id','auction_id','player_id','bidder_id')]) ||
       any(bids$championship_id!=championship_id | bids$auction_id!=auction$auction_id | bids$player_id!=auction$player_id))return(FALSE)
  }
  complete<-isTRUE(evidence$bid_visibility_verified)&&length(evidence$eligible_manager_ids)>0L
  if(complete && any(!as.character(bids$bidder_id)%in%unlist(evidence$eligible_manager_ids)))return(FALSE)
  row<-list(championship_id=championship_id,listing_id=listing_id,player_id=auction$player_id,
    auction_id=auction$auction_id,first_observed_at=auction$observed_at,observed_at=auction$observed_at,
    outcome=auction$outcome,visibility_complete=complete,
    eligible_manager_ids=if(complete)evidence$eligible_manager_ids else list(),evidence=evidence)
  auction$championship_id<-championship_id;auction$listing_id<-listing_id
  auction$visibility_complete<-complete;auction$eligible_manager_ids<-row$eligible_manager_ids
  accepted<-function(table,payload) {
    status<-tryCatch(supabase_post(table,payload),error=function(e)NULL)
    isTRUE(status)||(is.numeric(status)&&length(status)==1L&&!is.na(status)&&status>=200&&status<300)
  }
  if(!accepted('auction_opportunities',row)||!accepted('auction_observations',auction))return(FALSE)
  if(is.data.frame(bids)&&nrow(bids)&&!accepted('bid_observations',bids))return(FALSE)
  TRUE
}

insights_rows <- function(data) {
  if(!is.data.frame(data)||!nrow(data)) return(list())
  lapply(seq_len(nrow(data)),function(i) lapply(data,function(column) {
    if(is.data.frame(column)) as.list(column[i,,drop=FALSE]) else if(is.list(column)) column[[i]] else column[i]
  }))
}

collection_player_batch <- function(ids, after=NULL, limit=30L) {
  ids <- sort(unique(as.character(ids[!is.na(ids) & nzchar(ids)])))
  if(!length(ids)) return(character())
  start <- match(after,ids); if(!length(start)||is.na(start)) start<-0L
  ids[((start+seq_len(min(length(ids),limit))-1L) %% length(ids))+1L]
}

normalize_sale_observations <- function(offers,roster,user_id,championship_id,user_team_id) {
  if(!is.data.frame(offers)||!nrow(offers)||!all(c('id','bid_id','bid_price')%in%names(offers))) return(data.frame())
  stamp <- attr(offers,'observed_at')
  if(is.null(stamp)||identical(attr(offers,'fetch_status'),'stale')) return(data.frame())
  value <- roster$value[match(offers$id,roster$id)]
  bidder <- if('bidder_team_id'%in%names(offers)) as.character(offers$bidder_team_id) else rep(NA_character_,nrow(offers))
  rows <- data.frame(user_id=user_id,championship_id=championship_id,user_team_id=user_team_id,
    offer_id=as.character(offers$bid_id),player_id=as.character(offers$id),amount=as.numeric(offers$bid_price),
    market_value=as.numeric(value),observed_at=format(stamp,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),
    counterparty=ifelse(is.na(bidder)|!nzchar(bidder),'Futmondo (System)',bidder),outcome='offered')
  rows[!is.na(rows$offer_id)&nzchar(rows$offer_id)&is.finite(rows$amount)&rows$amount>0,,drop=FALSE]
}

realized_trade_ledger <- function(transactions,user_team_id,season_start=NULL) {
  if(!is.data.frame(transactions)||!all(c('player_id','price','buyer_team_id','seller_team_id','created')%in%names(transactions)))return(data.frame())
  d<-transactions
  if('id'%in%names(d))d<-d[!duplicated(d$id),,drop=FALSE]
  d<-d[order(fm_time(d$created)),,drop=FALSE]
  costs<-list();rows<-list()
  for(i in seq_len(nrow(d))) {
    r<-d[i,,drop=FALSE];id<-as.character(r$player_id);price<-fm_number(r$price)
    if(!is.finite(price)||is.na(fm_time(r$created)))next
    buyer<-fm_scalar(r$buyer_team_id,'');seller<-fm_scalar(r$seller_team_id,'')
    if(buyer==user_team_id)costs[[id]]<-price
    if(seller==user_team_id) {
      cost<-costs[[id]];if(is.null(cost))cost<-NA_real_
      if(is.null(season_start)||fm_time(r$created)>=fm_time(season_start))rows[[length(rows)+1L]]<-data.frame(
        player_id=id,sold_at=as.character(r$created),proceeds=price,cost_basis=cost,
        realized_profit=price-cost,counterparty=if(nzchar(buyer))buyer else 'Futmondo (System)')
      costs[[id]]<-NULL
    }
  }
  dplyr::bind_rows(rows)
}

today_is_clause_open <- function(player_row, now = Sys.time()) {
  if (is.null(player_row)) return(FALSE)
  if (is.data.frame(player_row)) {
    if (nrow(player_row) == 0) return(FALSE)
    player_row <- player_row[1, , drop = FALSE]
  }

  cp <- if ("clause_price" %in% names(player_row)) suppressWarnings(as.numeric(player_row[["clause_price"]])) else NA_real_
  if (!is.finite(cp) || cp <= 0) return(FALSE)

  tr <- if ("clause_transferred" %in% names(player_row)) player_row[["clause_transferred"]] else NULL
  if (is.null(tr) || is.na(tr)) return(FALSE)
  if (!is.logical(tr)) tr <- suppressWarnings(as.logical(tr))
  if (is.na(tr) || isTRUE(tr)) return(FALSE)

  cd <- if ("clause_date" %in% names(player_row)) player_row[["clause_date"]] else NULL
  if (is.null(cd) || is.na(cd) || !nzchar(trimws(as.character(cd)))) return(FALSE)
  cd_chr <- trimws(as.character(cd))
  # Parse defensively: ISO-8601 with T/Z first, then a plain "YYYY-mm-dd
  # HH:MM:SS" fallback. Unparseable dates (error or NA) -> not open.
  dt <- tryCatch(
    suppressWarnings(as.POSIXct(cd_chr, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")),
    error = function(e) as.POSIXct(NA)
  )
  if (is.na(dt)) {
    dt <- tryCatch(
      suppressWarnings(as.POSIXct(gsub("Z", "", gsub("T", " ", cd_chr)), tz = "UTC")),
      error = function(e) as.POSIXct(NA)
    )
  }
  if (is.na(dt)) return(FALSE)
  dt <= now
}

# Session-local aliases capture immutable read arguments and share async state.
factual_read_adapter<-function(session,handler,login_token,championship_id=NULL,user_team_id=NULL,is_active=function()TRUE,refresh_trigger=NULL,state_only=FALSE){
 force(handler)
 if(is.null(session$userData$factual_read_registry))session$userData$factual_read_registry<-new.env(parent=emptyenv())
 registry<-session$userData$factual_read_registry
 value<-function(x)if(is.function(x))x()else x
 owner_context<-function(){auth<-value(login_token);list(account=if(valid_login(auth))fm_scalar(auth[["userid"]])else "",league=fm_scalar(value(championship_id)),team=fm_scalar(value(user_team_id)))}
 function(...){
  shiny::req(isTRUE(is_active()));args<-list(...);auth<-value(login_token);shiny::req(valid_login(auth))
  original<-get(handler,envir=.GlobalEnv,mode="function");formal_names<-names(formals(original));argument_names<-names(args)
  if(is.null(argument_names))argument_names<-rep("",length(args));position<-1L
  for(i in seq_along(args))if(!nzchar(argument_names[i])){
   while(position<=length(formal_names)&&formal_names[position]%in%argument_names)position<-position+1L
   if(position<=length(formal_names)&&formal_names[position]!="..."){argument_names[i]<-formal_names[position];position<-position+1L}
  }
  names(args)<-argument_names;requested_auth<-args$login %||% auth
  ctx<-resource_context(requested_auth,args$championship_id %||% value(championship_id),args$user_team_id %||% value(user_team_id),args$player_id,session_id=session$token)
  resource<-paste0(handler,":",as.character(openssl::sha256(serialize(args,NULL))))
  entry_key<-paste0(session$ns(""),resource)
  if(!exists(entry_key,registry,inherits=FALSE))shiny::isolate({
   keys<-ls(registry)
   if(length(keys)>=128L){times<-vapply(keys,function(k)attr(registry[[k]],"last_access") %||% 0,numeric(1));victim<-keys[which.min(times)];dispose<-attr(registry[[victim]],"dispose");if(is.function(dispose))dispose();rm(list=victim,envir=registry)}
   captured_args<-args;captured_context<-ctx;captured_owner<-owner_context();demand_refresh<-shiny::reactiveVal(0L)
   registry[[entry_key]]<-shared_resource(session,resource,context=function()captured_context,fetch=function(context)async_adapter_fetch(handler,captured_args,context=context,is_current=function()shiny::isolate(isTRUE(is_active())&&identical(owner_context(),captured_owner))),active=function()isTRUE(is_active())&&identical(owner_context(),captured_owner),refresh=function()paste(if(is.function(refresh_trigger))refresh_trigger()else 0L,demand_refresh(),sep=":"))
   attr(registry[[entry_key]],"demand_refresh")<-demand_refresh
  })
  entry<-registry[[entry_key]];attr(entry,"last_access")<-as.numeric(Sys.time());registry[[entry_key]]<-entry;state<-entry()
  stamp<-state$fetched_at;age<-if(length(stamp)==1L&&!is.na(stamp))as.numeric(difftime(Sys.time(),stamp,units="secs"))else 0
  if(!is.null(state$data)&&is.finite(age)&&age>resource_policy(handler)$ttl){bump<-attr(entry,"demand_refresh");if(is.function(bump))shiny::isolate(bump(bump()+1L));if(isTRUE(state_only))return(resource_state(status="loading"));shiny::validate(shiny::need(FALSE,"Refreshing data…"))}
  if(isTRUE(state_only))return(state)
  if(is.null(state$data))shiny::validate(shiny::need(FALSE,if(identical(state$status,"loading"))"Loading data…"else resource_failure_message(state$error)))
  result<-state$data;if(identical(state$freshness,"stale"))attr(result,"fetch_status")<-"stale";result
 }
}
bind_factual_reads<-function(handlers,session,login_token,championship_id=NULL,user_team_id=NULL,is_active=function()TRUE,refresh_trigger=NULL,env=parent.frame()){
 for(handler in handlers)assign(handler,factual_read_adapter(session,handler,login_token,championship_id,user_team_id,is_active,refresh_trigger),envir=env)
 invisible(TRUE)
}
invalidate_manual_resources<-function(login,championship_id,user_team_id,player_id=NULL){
 if(!valid_login(login))return(invisible(FALSE))
 invalidate_resources(resource_context(login,championship_id,user_team_id),pattern="market|roster|bid|capacity|financial|user_team_info|team_info|teams|pressroom")
 invalidate_resources(resource_context(login,championship_id),pattern="championshipplayers|championship_players|catalog|teams|pressroom")
 invalidate_resources(resource_context(login,championship_id,user_team_id,player_id),pattern="summary|player_card")
 invisible(TRUE)
}
