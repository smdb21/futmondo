# Collection, provenance and feedback contracts. See docs/insights_runtime.md.
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

read_league_preference <- function(user_id,championship_id,user_team_id) {
  rows <- tryCatch(supabase_read_all('league_preferences',list(user_id=paste0('eq.',user_id),
    championship_id=paste0('eq.',championship_id),user_team_id=paste0('eq.',user_team_id))),error=function(e)NULL)
  if(!is.data.frame(rows)||nrow(rows)!=1L) return(NULL)
  rows$auto_coach[1]
}

save_league_preference <- function(user_id,championship_id,user_team_id,enabled) {
  if(!is.logical(enabled)||length(enabled)!=1L||is.na(enabled)) return(FALSE)
  code <- supabase_post_direct('league_preferences',list(user_id=user_id,championship_id=championship_id,
    user_team_id=user_team_id,auto_coach=enabled,updated_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')),
    'user_id,championship_id,user_team_id')
  is.numeric(code)&&code %in% 200:299
}

set_observation_subscription <- function(login,championship_id,user_team_id,enabled) {
  if(!valid_login(login)) return(FALSE)
  allowed <- get_active_championships(login)$championships
  member <- vapply(allowed,function(x) identical(fm_scalar(x$id),championship_id) &&
    identical(fm_scalar(x$userteam$id),user_team_id),logical(1))
  if(!any(member)) return(FALSE)
  code <- supabase_post_direct('observation_subscriptions',list(user_id=login[['userid']],
    championship_id=championship_id,user_team_id=user_team_id,enabled=isTRUE(enabled),
    next_run_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')),'user_id,championship_id,user_team_id')
  is.numeric(code)&&code %in% 200:299
}

schedule_observations <- function(now=Sys.time()) {
  stamp <- format(now,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')
  rows <- read_automation_rows('observation_subscriptions',list(enabled='eq.true',next_run_at=paste0('lte.',stamp)))
  for(row in rows) {
    filters <- list(user_id=paste0('eq.',row$user_id),championship_id=paste0('eq.',row$championship_id),user_team_id=paste0('eq.',row$user_team_id))
    sessions <- read_automation_rows('automation_sessions',list(user_id=paste0('eq.',row$user_id)))
    auth <- if(length(sessions)==1L) tryCatch(decrypt_automation_session(sessions[[1]]$encrypted_session,now),error=function(e)NULL) else NULL
    error <- 'Background session expired; reconnect to resume observations.'
    if(valid_login(auth) && identical(fm_scalar(auth[['userid']]),row$user_id)) {
      options(futmondo.persistence_failures=0L)
      ok <- tryCatch(isTRUE(collect_account_observations(auth,row$championship_id,row$user_team_id)),error=function(e)FALSE)
      if(ok && getOption('futmondo.persistence_failures',0L)==0L) {
        evaluated <- tryCatch({
          reconcile_forecast_outcomes(row$user_id,row$championship_id,row$user_team_id,now)
          evaluate_saved_forecasts(row$user_id,row$championship_id,now)
          run_chronological_evaluations(auth,row$championship_id,row$user_team_id,now)
          TRUE
        },error=function(e)FALSE)
        error <- if(isTRUE(evaluated)&&getOption('futmondo.persistence_failures',0L)==0L) '' else 'Observation feedback incomplete; retry scheduled.'
      } else error <- 'Observation collection incomplete; retry scheduled.'
    }
    update <- list(last_error=error,next_run_at=format(now+900,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))
    if(!nzchar(error)) update$last_success_at <- stamp
    supabase_patch('observation_subscriptions',update,filters)
  }
  invisible(length(rows))
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

fit_sale_execution <- function(observations,as_of=Sys.time()) {
  fail <- list(status='insufficient_sale_evidence',mean=NA_real_,lower=NA_real_,upper=NA_real_,n=0L)
  if(!is.data.frame(observations)||!all(c('offer_id','amount','market_value','observed_at','counterparty')%in%names(observations))) return(fail)
  d <- observations[observations$counterparty=='Futmondo (System)' & !is.na(fm_time(observations$observed_at)) & fm_time(observations$observed_at)<=as_of,,drop=FALSE]
  d <- d[order(fm_time(d$observed_at)),,drop=FALSE]
  d <- d[!duplicated(d$offer_id),,drop=FALSE]
  ratio <- d$amount/d$market_value;ratio <- ratio[is.finite(ratio)&ratio>0]
  fail$n <- length(ratio)
  if(length(ratio)<5L) return(fail)
  list(status='observed_offer_distribution',mean=mean(ratio),lower=unname(quantile(ratio,.1)),
    upper=unname(quantile(ratio,.9)),n=length(ratio))
}

read_sale_observations <- function(user_id,championship_id) {
  tryCatch(supabase_read_all('sale_observations',list(user_id=paste0('eq.',user_id),
    championship_id=paste0('eq.',championship_id),order='observed_at.asc')),error=function(e)NULL)
}

forecast_observed_outcome <- function(record,prices,matches,auctions,now=Sys.time()) {
  cutoff <- fm_time(record$cutoff); horizon<-fm_number(record$horizon)
  if(is.na(cutoff)||!is.finite(horizon)) return(NULL)
  if(record$forecast_type=='resale_value' && is.data.frame(prices)&&nrow(prices)) {
    times<-fm_time(prices$observed_at);due<-cutoff+horizon*86400
    d<-prices[prices$player_id==record$player_id & !is.na(times)&times>=due&times<=due+86400&times<=now,,drop=FALSE]
    if(!nrow(d)) return(NULL)
    d<-d[order(fm_time(d$observed_at)),,drop=FALSE]
    return(list(value=d$value[1],observed_at=d$observed_at[1],kind='market_value'))
  }
  if(record$forecast_type=='fantasy_points' && is.data.frame(matches)&&all(c('round_start_at','points','is_final')%in%names(matches))) {
    times<-fm_time(matches$round_start_at)
    d<-matches[matches$player_id==record$player_id & !is.na(times)&times>cutoff&times<=now,,drop=FALSE]
    if(!nrow(d)) return(NULL)
    d<-d[order(fm_time(d$round_start_at)),,drop=FALSE]
    d<-head(d,horizon)
    if(nrow(d)!=horizon||!all(d$is_final %in% TRUE)||any(!is.finite(d$points)))return(NULL)
    return(list(points=sum(d$points),round_ids=as.character(d$round_id),kind='final_points'))
  }
  if(record$forecast_type=='rival_bid' && is.data.frame(auctions)&&nrow(auctions)) {
    id<-fm_scalar(record$prediction$auction_id,'')
    listing<-fm_scalar(record$prediction$listing_id,'')
    matched<-if(nzchar(id))auctions$auction_id==id else if(nzchar(listing)&&'listing_id'%in%names(auctions))auctions$listing_id==listing else rep(FALSE,nrow(auctions))
    times<-fm_time(auctions$settled_at)
    d<-auctions[which(matched & !is.na(times) & times>cutoff & times<=now),,drop=FALSE]
    if(nrow(d)!=1L)return(NULL)
    return(list(winning_bid=fm_number(d$winning_amount),winner_id=fm_scalar(d$winner_id),kind='settled_auction'))
  }
  NULL
}

reconcile_forecast_outcomes <- function(user_id,championship_id,user_team_id,now=Sys.time()) {
  filters<-list(user_id=paste0('eq.',user_id),championship_id=paste0('eq.',championship_id))
  records<-tryCatch(supabase_read_all('forecast_records',c(filters,list(evaluated_at='is.null',order='cutoff.asc'))),error=function(e)NULL)
  if(!is.data.frame(records)||!nrow(records)) return(invisible(0L))
  prices<-read_player_price_history(championship_id)
  auctions<-read_auction_observations(championship_id)
  count<-0L
  for(record in insights_rows(records)) {
    matches<-read_player_match_history(championship_id,cutoff=now,final_only=FALSE,
      season=record$season,scoring_version=record$scoring_version)
    outcome<-forecast_observed_outcome(record,prices,matches,auctions,now)
    if(!is.null(outcome)&&isTRUE(record_forecast_outcome(user_id,championship_id,record$id,outcome,now)))count<-count+1L
  }
  invisible(count)
}

summarize_forecast_evidence <- function(records) {
  rows <- list()
  for(record in insights_rows(records)) {
    p<-record$prediction;o<-record$outcome
    if(!is.list(p)||!is.list(o))next
    predicted <- fm_number(p$expected_points %||% p$expected_value)
    actual <- fm_number(o$points %||% o$value)
    if(!is.finite(predicted)||!is.finite(actual))next
    low<-fm_number(p$lower %||% p$lower_value);high<-fm_number(p$upper %||% p$upper_value)
    rows[[length(rows)+1L]] <- data.frame(model_version=record$model_version,
      player_id=fm_scalar(record$player_id),horizon=fm_number(record$horizon),
      forecast_type=record$forecast_type,cutoff=as.character(record$cutoff),
      absolute_error=abs(predicted-actual),baseline_error=abs(fm_number(p$baseline)-actual),
      covered=if(is.finite(low)&&is.finite(high))actual>=low&&actual<=high else NA)
  }
  d<-dplyr::bind_rows(rows)
  if(!nrow(d))return(d)
  d<-d[order(d$cutoff),,drop=FALSE]
  key<-paste(d$model_version,d$forecast_type,d$player_id,d$horizon,substr(d$cutoff,1,10),sep=':')
  d[!duplicated(key),,drop=FALSE]
}

evaluate_saved_forecasts <- function(user_id,championship_id,now=Sys.time()) {
  records<-tryCatch(supabase_read_all('forecast_records',list(user_id=paste0('eq.',user_id),
    championship_id=paste0('eq.',championship_id),evaluated_at='not.is.null',
    cutoff=paste0('lt.',format(now,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')))),error=function(e)NULL)
  evidence<-summarize_forecast_evidence(records)
  if(!nrow(evidence))return(invisible(FALSE))
  for(version in unique(evidence$model_version)) {
    d<-evidence[evidence$model_version==version,,drop=FALSE]
    # One player/type/horizon forecast per day is the unit for promotion reporting;
    # repeated UI refreshes must not manufacture independent evidence.
    days<-length(unique(substr(d$cutoff,1,10)))
    metrics<-list(observations=nrow(d),days=days)
    for(horizon in unique(d$horizon)) {
      h<-d[d$horizon==horizon,,drop=FALSE]
      values<-list(mae=mean(h$absolute_error),baseline_mae=if(all(is.finite(h$baseline_error)))mean(h$baseline_error) else NA_real_,
        coverage=if(all(!is.na(h$covered)))mean(h$covered) else NA_real_,observations=nrow(h))
      names(values)<-paste0('horizon_',horizon,'_',names(values));metrics<-c(metrics,values)
    }
    stamp<-format(now,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')
    id<-as.character(openssl::sha256(charToRaw(paste(user_id,championship_id,version,stamp,sep=':'))))
    supabase_post('model_evaluations',list(id=id,user_id=user_id,championship_id=championship_id,
      model_version=version,cutoff=stamp,status='advisory',metrics=metrics,
      evidence=list(forecast_ids=as.list(records$id),minimum_observation_days=14),approved=FALSE))
  }
  invisible(TRUE)
}

run_chronological_evaluations <- function(login,championship_id,user_team_id,now=Sys.time()) {
  if(!valid_login(login))return(FALSE)
  uid<-login[['userid']];day<-format(now,'%Y-%m-%d',tz='UTC')
  versions<-c('rival-bids-v1','points-shrinkage-v1','resale-log-return-v1')
  ids<-vapply(versions,function(v)as.character(openssl::sha256(charToRaw(paste(uid,championship_id,v,day,'chronological',sep=':')))),character(1))
  existing<-tryCatch(supabase_read_all('model_evaluations',list(user_id=paste0('eq.',uid),championship_id=paste0('eq.',championship_id),id=paste0('in.(',paste(ids,collapse=','),')'))),error=function(e)NULL)
  if(is.data.frame(existing)&&all(ids%in%existing$id))return(TRUE)
  players<-get_championship_players(login,championship_id)
  players<-preserve_observation_time(players)
  prices<-read_player_price_history(championship_id)
  fin<-get_financial_snapshot(login,championship_id,user_team_id)
  ctx<-list(championship_id=championship_id,season=fin$rules$season,scoring_version=fin$rules$scoring_version)
  read<-function(table,extra=list())tryCatch(supabase_read_all(table,c(list(championship_id=paste0('eq.',championship_id),
    observed_at=paste0('lte.',format(now,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))),extra)),error=function(e)data.frame())
  auctions<-read_model_auctions(championship_id,read('auction_observation_history',list(legacy_timing='eq.false')))
  bids<-read('bid_observation_history',list(legacy_timing='eq.false'))
  matches<-read('player_match_observations',list(season=paste0('eq.',ctx$season),scoring_version=paste0('eq.',ctx$scoring_version)))
  if(is.data.frame(matches)&&nrow(matches))matches$round_id<-paste(matches$season,matches$scoring_version,matches$round,sep=':')
  jobs<-list(function()rolling_bid_backtest(auctions,bids,as_of=now,user_id=user_team_id,context=ctx),
    function()rolling_point_backtest(players,matches,as_of=now,context=ctx),
    function()rolling_resale_backtest(players,prices,as_of=now,context=ctx))
  for(i in seq_along(jobs)) {
    if(is.data.frame(existing)&&ids[i]%in%existing$id)next
    result<-tryCatch(jobs[[i]](),error=function(e)list(status='insufficient_data',metrics=data.frame(),folds=data.frame()))
    metrics<-if(is.data.frame(result$metrics)&&nrow(result$metrics))as.list(stats::setNames(result$metrics$value,
      paste(result$metrics$metric,if('horizon'%in%names(result$metrics))result$metrics$horizon else '',sep=':'))) else list()
    supabase_post('model_evaluations',list(id=ids[i],user_id=uid,championship_id=championship_id,
      model_version=paste0(versions[i],'/chronological'),cutoff=format(now,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),
      status=result$status,metrics=metrics,evidence=list(folds=result$folds,source='expanding chronological validation'),approved=FALSE))
  }
  TRUE
}
