# League-scoped observed accounting. See docs/rival_cash_accounting.md.
rival_finance_stamp <- function(x=Sys.time()) format(fm_time(x),"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC")
rival_finance_records <- function(x) {
  if(is.data.frame(x)) return(lapply(seq_len(nrow(x)),function(i)as.list(x[i,,drop=FALSE])))
  if(is.list(x)&&length(x)&&!is.null(names(x))&&any(c("id","_id","money")%in%names(x)))return(list(x))
  if(is.list(x))x else list()
}
normalize_money_movements <- function(answer) {
  empty <- data.frame(id=character(),concept=character(),type=character(),category=character(),
    money=numeric(),date=character(),bonus_type=character(),round_id=character(),player_id=character(),
    source_updated_at=character(),source_event_id=character(),stringsAsFactors=FALSE)
  invalid <- function(){attr(empty,"complete")<-FALSE;attr(empty,"fetch_status")<-"unavailable";empty}
  if(!is.list(answer)||isTRUE(answer$error)||(!is.null(answer$code)&&!identical(answer$code,API_CODE_OK)))return(invalid())
  if("answer"%in%names(answer))answer<-answer$answer
  records<-rival_finance_records(answer)
  if(length(answer)&&!length(records))return(invalid())
  rows<-dplyr::bind_rows(c(list(empty),lapply(records,function(m){
    if(!is.list(m))return(NULL)
    data.frame(id=mvp_text(m$`_id` %||% m$id),concept=mvp_text(m$concept),type=mvp_text(m$type),
      category=mvp_text(m$category),money=fm_number(m$money),date=mvp_text(m$date),
      bonus_type=mvp_text(m$bonus_type),round_id=mvp_text(m$round_id),player_id=mvp_text(m$player_id),
      source_updated_at=mvp_text(m$updated %||% m$updated_at),source_event_id=mvp_text(m$source_event_id),stringsAsFactors=FALSE)
  })))
  # Futmondo omits IDs on native point/award settlements. Retain the absent
  # source ID separately and use a deterministic local key for storage/replays.
  # Amount is excluded so a revised settlement replaces its prior value.
  rows$source_movement_id<-rows$id
  rows$identity_source<-ifelse(nzchar(rows$id),"source","missing")
  native<-!nzchar(rows$id) & rows$type%in%c("points","dreamTeam","dreamteam","mvp") &
    grepl("^[0-9]+([.][0-9]+)?$",rows$concept) & !is.na(fm_time(rows$date))
  for(i in which(native)){
    key<-list(type=tolower(rows$type[i]),settlement=rows$concept[i],date=rows$date[i],player=rows$player_id[i])
    rows$id[i]<-paste0("settlement:",as.character(openssl::sha256(serialize(key,NULL,version=2))))
    rows$identity_source[i]<-"derived_settlement"
  }
  # Missing amounts remain unknown. Unidentified non-settlement transactions
  # still fail completeness; a local settlement key is never an API movement ID.
  attr(rows,"complete")<-nrow(rows)==length(records)&&all(nzchar(rows$id))&&all(is.finite(rows$money))
  attr(rows,"fetch_status")<-if(isTRUE(attr(rows,"complete")))if(nrow(rows))"ok"else"empty"else"partial"
  rows
}
rival_finance_evidence <- function(account_id,championship_id,season,type,id,payload,observed_at=Sys.time(),competition_id=NULL) {
  if(type%in%c("team_rounds","round_lineup","round_ranking")){
    payload$kind<-type;id<-paste0(switch(type,team_rounds="rounds:",round_lineup="lineup:",round_ranking="ranking:"),id);type<-"eligibility"
  }
  list(account_id=as.character(account_id),championship_id=as.character(championship_id),competition_id=fm_scalar(competition_id,league_competition_context(championship_id,account_id)),season=mvp_text(season,"unknown"),
    evidence_type=type,source_id=as.character(id),observed_at=rival_finance_stamp(observed_at),payload=payload)
}
rival_evidence_rows <- function(evidence,type) {
  if(!is.list(evidence))return(list())
  Filter(function(x)is.list(x)&&if(type%in%c("team_rounds","round_lineup","round_ranking"))
    identical(x$evidence_type,"eligibility")&&is.list(x$payload)&&identical(x$payload$kind,type)
    else identical(x$evidence_type,type),evidence)
}
rival_evidence_decode <- function(rows) {
  if(!is.data.frame(rows)||!nrow(rows))return(list())
  # jsonlite simplifies heterogeneous JSON objects to nested data frames. A
  # frame row's list columns must be indexed, not kept as one-item wrappers:
  # players/rounds already contain their complete nested records at [[i]].
  record<-function(frame,index)lapply(frame,function(column){
    if(is.data.frame(column))record(column,index)else if(is.list(column))column[[index]]else column[index]
  })
  lapply(seq_len(nrow(rows)),function(i){
    row<-as.list(rows[i,,drop=FALSE]);p<-rows$payload
    row$payload<-if(is.data.frame(p))record(p,i)else p[[i]]
    if(is.character(row$payload))row$payload<-tryCatch(jsonlite::fromJSON(row$payload,simplifyVector=FALSE),error=function(e)list())
    row
  })
}
read_rival_finance_evidence <- function(account_id,championship_id,season="unknown",max_rows=20000L,competition_id=NULL) {
  competition_id<-fm_scalar(competition_id,league_competition_context(championship_id,account_id))
  get_cached_data(paste0("rival_finance_evidence_",account_id,"_",championship_id,"_",season),{
    rows<-history_read_pages("rival_finance_evidence_latest",list(account_id=paste0("eq.",account_id),championship_id=paste0("eq.",championship_id),
      competition_id=paste0("eq.",competition_id),season=paste0("eq.",season),select="account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at,payload",
      source_id="not.like.rival-cache:*",order="evidence_type.asc,source_id.asc"),max_rows=max_rows,page_size=1000L)
    list(evidence=rival_evidence_decode(rows),complete=isTRUE(attr(rows,"complete")))
  },timeout_sec=300,context=resource_context(account_id=account_id,championship_id=championship_id,season=season,competition_id=competition_id))
}
# Restore only source-identified transfers within the recorded opening/reset.
# Legacy rows without an API identity must never be mixed into the cash ledger.
get_rival_saved_transfers <- function(championship_id,evidence) {
  openings<-Filter(function(x)identical(x$payload$type,"budget")&&is.finite(fm_number(x$payload$money)),rival_evidence_rows(evidence,"movements"))
  dates<-unique(vapply(openings,function(x)mvp_text(x$payload$date),character(1)))
  if(length(dates)!=1L||is.na(fm_time(dates)))return(data.frame())
  rows<-history_read_pages("market_transactions",list(championship_id=paste0("eq.",championship_id),
    transaction_date=paste0("gte.",dates),source_event_id="not.is.null",provenance="like.futmondo_pressroom*",
    select="source_event_id,player_id,buyer_team_id,seller_team_id,price,is_clause,transaction_date,player:players(name),buyer:user_teams!market_transactions_buyer_team_id_fkey(name),seller:user_teams!market_transactions_seller_team_id_fkey(name)",
    order="transaction_date.asc,source_event_id.asc"),max_rows=20000L,page_size=1000L)
  if(!is.data.frame(rows)||!nrow(rows))return(data.frame())
  label<-function(field){v<-rows[[field]];if(is.data.frame(v))as.character(v$name)else vapply(v,function(x)mvp_text(x$name),character(1))}
  out<-data.frame(id=as.character(rows$source_event_id),created=as.character(rows$transaction_date),
    player_id=as.character(rows$player_id),player_name=label("player"),buyer_team_id=as.character(rows$buyer_team_id),
    buyer_team_name=label("buyer"),seller_team_id=as.character(rows$seller_team_id),seller_team_name=label("seller"),
    price=as.numeric(rows$price),is_clause=as.logical(rows$is_clause),player_team_name="",player_team_slug="",stringsAsFactors=FALSE)
  for(field in c("buyer_team_id","seller_team_id","buyer_team_name","seller_team_name"))out[[field]][is.na(out[[field]])]<-""
  out$buyer_team_name[!nzchar(out$buyer_team_id)]<-"Futmondo / Mercado"
  out$seller_team_name[!nzchar(out$seller_team_id)]<-"Futmondo / Mercado"
  out<-out[grepl("^[0-9a-f]{24}$",out$id)&!duplicated(out$id),,drop=FALSE]
  attr(out,"fetch_status")<-"partial"
  out
}
get_rival_finance_saved <- function(account_id,championship_id,season="unknown") {
  result<-tryCatch(read_rival_finance_evidence(account_id,championship_id,season),error=function(e)list(evidence=list(),complete=FALSE))
  result$evidence<-Filter(function(x)!rival_snapshot_record(x),result$evidence)
  context<-resource_context(account_id=account_id,championship_id=championship_id,season=season)
  baseline<-tryCatch(read_completed_history_baseline(paste0("pressroom_",championship_id),context),error=function(e)NULL)
  saved<-if(is.data.frame(baseline)&&identical(attr(baseline,"fetch_status"),"ok"))baseline else
    tryCatch(get_rival_saved_transfers(championship_id,result$evidence),error=function(e)data.frame())
  result$pressroom<-merge_locker_transfers(saved,locker_saved_transfer_rows(result$evidence))
  if(nrow(result$pressroom))attr(result$pressroom,"fetch_status")<-"partial"
  result$archive_lineups<-list()
  # Unknown archives are candidates, never current accounting inputs. Only
  # missing pairs from known authoritative team-round histories need a lookup.
  if(fm_context_known(context$competition_id)&&fm_context_known(context$season)){
    rounds<-rival_saved_rounds(result$evidence,include_ranking=FALSE)
    known<-vapply(rival_evidence_rows(result$evidence,'round_lineup'),function(row)mvp_text(row$source_id),character(1))
    needed<-if(nrow(rounds))setdiff(paste0('lineup:',rounds$team_id,':',rounds$round_id),known)else character()
    if(length(needed)){
      archive<-tryCatch(history_read_pages('rival_finance_evidence_latest',list(
        account_id=paste0('eq.',account_id),championship_id=paste0('eq.',championship_id),
        evidence_type='eq.eligibility',source_id='like.lineup:*',
        or='(competition_id.eq.unknown,season.eq.unknown)',
        select='account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at,payload',
        order='source_id.asc'),max_rows=600L,page_size=50L),error=function(e)NULL)
      result$archive_lookup_status<-if(is.null(archive))'unavailable'else if(isTRUE(attr(archive,'complete')))'ok'else'partial'
      candidates<-rival_evidence_decode(archive)
      result$archive_lineups<-Filter(function(row)mvp_text(row$source_id)%in%needed,candidates)
    }
  }
  result
}
persist_rival_finance_evidence <- function(evidence,context) {
  if(!length(evidence))return(invisible(TRUE))
  evidence<-Filter(function(row){
    identical(mvp_text(row$account_id),mvp_text(context$account_id))&&
      identical(mvp_text(row$championship_id),mvp_text(context$championship_id))&&
      identical(fm_scalar(row$competition_id,"unknown"),fm_scalar(context$competition_id,"unknown"))&&
      identical(fm_scalar(row$season,"unknown"),fm_scalar(context$season,"unknown"))
  },evidence)
  if(!length(evidence))return(invisible(FALSE))
  # Small immutable revisions, passed through the existing bounded asynchronous queue.
  groups<-split(seq_along(evidence),ceiling(seq_along(evidence)/50L))
  index<-1L
  next_group<-function(){
    if(index>length(groups))return(invisible(TRUE))
    indices<-groups[[index]];index<<-index+1L
    # Validate only the current acknowledged batch, not thousands of records
    # synchronously on the serving process. R's expanded list size is not the
    # stored JSON size: an 80 KiB detailed lineup can occupy >700 KiB in R.
    batch<-Filter(function(row)nchar(jsonlite::toJSON(row,auto_unbox=TRUE,null='null',na='null',digits=NA),type='bytes')<=256*1024L,evidence[indices])
    if(!length(batch)){
      if(index>length(groups))return(invisible(FALSE))
      later::later(next_group,0);return(invisible(TRUE))
    }
    defer_persistence("supabase_post_direct",list("rival_finance_evidence",unname(batch),
      "account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at",TRUE),context=context,
      on_complete=function(ok){if(isTRUE(ok))later::later(next_group,0)})
  }
  isTRUE(next_group())
}
# Re-observing an unchanged transaction must not append another DB revision.
# Observation freshness for balances/coverage is retained independently.
rival_finance_evidence_delta <- function(old,new) {
  # JSONB reorders object keys and JSON restores whole numbers as integers.
  canonical<-function(value){
    if(is.null(value)||(is.atomic(value)&&length(value)==1L&&is.na(value)))return(NULL)
    if(is.list(value)){
      if(!is.null(names(value)))value<-value[sort(names(value))]
      return(lapply(value,canonical))
    }
    if(is.numeric(value))value<-as.numeric(value)
    value
  }
  known<-rival_evidence_rows(old,"movements")
  keys<-vapply(known,function(row)paste(row$account_id,row$championship_id,row$competition_id,row$season,row$source_id,sep="|"),character(1))
  Filter(function(row){
    if(!identical(row$evidence_type,"movements"))return(TRUE)
    key<-paste(row$account_id,row$championship_id,row$competition_id,row$season,row$source_id,sep="|")
    index<-match(key,keys)
    is.na(index)||!identical(canonical(row$payload),canonical(known[[index]]$payload))
  },new)
}
rival_merge_evidence <- function(old,new,max_rows=20000L) {
  rows<-c(old,new);if(!length(rows))return(rows)
  key<-vapply(rows,function(x)paste(x$account_id,x$championship_id,fm_scalar(x$competition_id,"unknown"),x$season,x$evidence_type,x$source_id,sep="|"),character(1))
  stamp<-vapply(rows,function(x)as.numeric(fm_time(x$observed_at)),numeric(1));stamp[!is.finite(stamp)]<--Inf
  order<-order(stamp);rows<-rows[order];key<-key[order]
  tail(rows[!duplicated(key,fromLast=TRUE)],max_rows)
}
# Legacy context/default arguments remain accepted for callers; only observed
# configuration establishes opening cash. Recorded opening movements override it.
rival_opening_cash <- function(championship_id,rules=list(),defaults=NULL) {
  value<-fm_number(rules$initial_budget)
  if(is.finite(value)&&value>=0)return(list(amount=value,source="league configuration"))
  list(amount=NA_real_,source="unknown")
}
rival_cash_latest_movements <- function(evidence,team_id) {
  records<-rival_evidence_rows(evidence,"movements")
  records<-Filter(function(x)identical(mvp_text(x$payload$team_id),as.character(team_id)),records)
  if(!length(records))return(data.frame())
  rows<-dplyr::bind_rows(lapply(records,function(x){
    p<-x$payload
    data.frame(id=mvp_text(p$id),team_id=mvp_text(p$team_id),type=mvp_text(p$type),category=mvp_text(p$category),concept=mvp_text(p$concept),
      money=fm_number(p$money),date=mvp_text(p$date),bonus_type=mvp_text(p$bonus_type),round_id=mvp_text(p$round_id),player_id=mvp_text(p$player_id),source_event_id=mvp_text(p$source_event_id),observed_at=mvp_text(x$observed_at),stringsAsFactors=FALSE)
  }))
  if(!all(c("id","money")%in%names(rows)))return(data.frame())
  rows<-rows[order(fm_time(rows$observed_at)),,drop=FALSE]
  rows[!duplicated(rows$id,fromLast=TRUE),,drop=FALSE]
}
# Keep the raw source contract alongside normalized accounting fields. A ranking
# concept is a round label (including split labels), not arbitrary free text.
rival_normalize_movements <- function(rows,catalog=data.frame()) {
  if(!is.data.frame(rows)||!nrow(rows))return(rows)
  rows$source_type<-rows$type;rows$source_category<-rows$category
  rows$settlement_label<-rep("",nrow(rows))
  for(field in c("bonus_type","round_id","player_id","source_event_id"))if(!field%in%names(rows))rows[[field]]<-rep("",nrow(rows))
  clause<-rows$type%in%c("clause","clause_sell")
  ordinary<-rows$type%in%c("buy","sell")
  rows$category[clause]<-"clause";rows$category[ordinary]<-"market"
  rows$type[rows$type%in%"clause"]<-"buy";rows$type[rows$type%in%"clause_sell"]<-"sell"
  # Native award types carry the same numeric settlement concept as points.
  native<-c(dreamTeam="dreamteam",dreamteam="dreamteam",mvp="mvp")
  award<-rows$type%in%names(native)
  rows$bonus_type[award]<-unname(native[rows$type[award]]);rows$type[award]<-"bonus"
  point<-rows$type%in%"points"
  rows$bonus_type[point]<-"points";rows$type[point]<-"bonus"
  ranking<-rows$type%in%"ranking"
  rows$bonus_type[ranking]<-"ranking";rows$type[ranking]<-"bonus"
  for(i in which(rows$bonus_type%in%c("points","ranking","dreamteam","mvp"))){
    label<-trimws(rows$concept[i])
    if(!grepl("^[0-9]+([.][0-9]+)?$",label))next
    rows$settlement_label[i]<-label
    if(!nzchar(rows$round_id[i])&&all(c("round_id","round_number")%in%names(catalog))){
      ids<-unique(catalog$round_id[is.finite(catalog$round_number)&catalog$round_number==floor(as.numeric(label))])
      if(length(ids)==1L)rows$round_id[i]<-ids
    }
  }
  rows
}

# Only explicit source types or a unique observed movement establish a clause.
# Counterparty membership is mandatory; names, amounts and times must match.
rival_classify_transfers <- function(pressroom,movements=data.frame()) {
  if(!is.data.frame(pressroom)||!nrow(pressroom))return(pressroom)
  tx<-pressroom;n<-nrow(tx);method<-rep(NA,n)
  if("is_clause"%in%names(tx))method<-as.logical(tx$is_clause)
  if("type"%in%names(tx)){
    method[tx$type%in%c("clause","clause_sell")]<-TRUE
    method[tx$type%in%c("buy","sell")]<-FALSE
  }
  system<-is.na(tx$buyer_team_id)|!nzchar(tx$buyer_team_id)|is.na(tx$seller_team_id)|!nzchar(tx$seller_team_id)
  method[system & is.na(method)]<-FALSE
  candidates<-vector("list",nrow(movements))
  times<-if("created"%in%names(tx))as.numeric(fm_time(tx$created))else rep(NA_real_,n)
  if(nrow(movements)&&all(c("team_id","type","category","money","date","concept")%in%names(movements))){
    for(j in seq_len(nrow(movements))){
      m<-movements[j,,drop=FALSE]
      if(!m$type%in%c("buy","sell")||!m$category%in%c("market","clause")||!is.finite(m$money)||
        (m$type=="buy"&&m$money>=0)||(m$type=="sell"&&m$money<=0))next
      participating<-if(m$type=="buy")tx$buyer_team_id%in%m$team_id else tx$seller_team_id%in%m$team_id
      exact<-if("id"%in%names(tx)&&nzchar(mvp_text(m$source_event_id)))which(participating & tx$id%in%m$source_event_id)else integer()
      if(!length(exact)){
        player<-if(nzchar(mvp_text(m$player_id))&&"player_id"%in%names(tx))tx$player_id%in%m$player_id else
          if("player_name"%in%names(tx)&&nzchar(m$concept))trimws(tx$player_name)%in%trimws(m$concept)else rep(FALSE,n)
        stamp<-as.numeric(fm_time(m$date))
        exact<-which(participating & player & is.finite(tx$price)&tx$price==abs(m$money)&is.finite(times)&is.finite(stamp)&abs(times-stamp)<=1)
      }
      if(length(exact)==1L)candidates[[j]]<-list(index=exact,clause=m$category=="clause")
    }
    for(i in seq_len(n)){
      matched<-Filter(function(x)!is.null(x)&&x$index==i,candidates)
      values<-unique(vapply(matched,function(x)x$clause,logical(1)))
      # Conflicting classifications cannot overwrite each other or known facts.
      if(length(values)==1L){if(is.na(method[i])||identical(method[i],values))method[i]<-values else method[i]<-NA}
      else if(length(values)>1L)method[i]<-NA
    }
  }
  tx$is_clause<-method;tx
}

# Attach display metadata without adding cash or merging ambiguous transfers.
rival_transfer_metadata <- function(ledger,pressroom,team_id,teams,movements=data.frame()) {
  fields<-c("transfer_player_name","transfer_club","transfer_logo","transfer_buyer","transfer_seller")
  for(field in fields)ledger[[field]]<-rep("",nrow(ledger))
  selected<-which(ledger$type%in%c("buy","sell","clause","clause_sell","transfer"))
  if(!length(selected))return(ledger)
  valid<-is.data.frame(pressroom)&&all(c("id","buyer_team_id","seller_team_id","player_name","price","created")%in%names(pressroom))
  party<-function(id,label){
    if(is.na(id)||!nzchar(id)||tolower(id)%in%c("system","futmondo","computer"))return("Futmondo")
    if(all(c("teamid","teamname")%in%names(teams))){known<-teams$teamname[match(id,teams$teamid)];if(length(known)&&!is.na(known)&&nzchar(known))return(as.character(known))}
    if(!is.na(label)&&nzchar(label))label else id
  }
  ledger$transfer_player_name[selected]<-sub(" [(]Purchased[)]$| [(]Sold[)]$","",ledger$component[selected])
  ledger$transfer_buyer[selected]<-ledger$transfer_seller[selected]<-"Unknown"
  buying<-ledger$type%in%c("buy","clause")
  ledger$transfer_buyer[intersect(selected,which(buying))]<-party(team_id,"")
  ledger$transfer_seller[intersect(selected,which(!buying))]<-party(team_id,"")
  if(!valid)return(ledger)
  # Most rival entries retain the source event ID: match the whole vector once.
  matches<-match(ledger$id,paste0("pressroom:",pressroom$id))
  stamp<-as.numeric(fm_time(pressroom$created))
  for(i in selected[is.na(matches[selected])]){
    m<-if(nrow(movements))movements[match(sub("^movement:","",ledger$id[i]),movements$id),,drop=FALSE]else data.frame()
    source_id<-if(nrow(m))mvp_text(m$source_event_id)else ""
    participating<-if(buying[i])pressroom$buyer_team_id%in%team_id else pressroom$seller_team_id%in%team_id
    index<-if(nzchar(source_id))which(participating & pressroom$id%in%source_id)else integer()
    if(!length(index)){
      player<-if(nzchar(mvp_text(ledger$player_id[i]))&&"player_id"%in%names(pressroom))pressroom$player_id%in%ledger$player_id[i]else trimws(pressroom$player_name)%in%trimws(ledger$transfer_player_name[i])
      index<-which(participating & player & is.finite(pressroom$price) & pressroom$price==abs(ledger$amount[i]) & is.finite(stamp) & abs(stamp-as.numeric(fm_time(ledger$date[i])))<=1)
    }
    if(length(index)==1L)matches[i]<-index
  }
  selected<-selected[!is.na(matches[selected])];index<-matches[selected]
  field<-function(key){x<-if(key%in%names(pressroom))as.character(pressroom[[key]][index])else rep("",length(index));x[is.na(x)]<-"";x}
  ledger$transfer_player_name[selected]<-field("player_name");ledger$transfer_club[selected]<-field("player_team_name");ledger$transfer_logo[selected]<-field("player_team_slug")
  ledger$transfer_identity_conflict<-rep(FALSE,nrow(ledger))
  observed_players<-field("player_id")
  ledger$transfer_identity_conflict[selected]<-nzchar(observed_players)&!is.na(ledger$player_id[selected])&
    nzchar(ledger$player_id[selected])&ledger$player_id[selected]!=observed_players
  # A unique observed event supplies identity for recorded movements that omit it.
  missing_player<-is.na(ledger$player_id[selected])|!nzchar(ledger$player_id[selected])
  ledger$player_id[selected[missing_player]]<-field("player_id")[missing_player]
  if("source_event_id"%in%names(ledger)){
    missing_event<-is.na(ledger$source_event_id[selected])|!nzchar(ledger$source_event_id[selected])
    ledger$source_event_id[selected[missing_event]]<-field("id")[missing_event]
  }
  for(side in c("buyer","seller")){
    ids<-field(paste0(side,"_team_id"));labels<-field(paste0(side,"_team_name"));keys<-paste(ids,labels,sep="|");unique_rows<-which(!duplicated(keys))
    names<-vapply(unique_rows,function(j)party(ids[j],labels[j]),character(1))
    ledger[[paste0("transfer_",side)]][selected]<-names[match(keys,keys[unique_rows])]
  }
  ledger
}

rival_payment_anchors <- function(movements) {
  empty<-data.frame(round_id=character(),date=character(),settlement_label=character(),stringsAsFactors=FALSE)
  if(!is.data.frame(movements)||!nrow(movements))return(empty)
  x<-movements[movements$bonus_type%in%"ranking"&nzchar(movements$round_id)&!is.na(fm_time(movements$date)),,drop=FALSE]
  if(!nrow(x))return(empty)
  # Use a single reference team's settlement timeline. Different teams' payment
  # timestamps may differ slightly; do not mistake those for new settlements.
  dplyr::bind_rows(lapply(split(seq_len(nrow(x)),x$round_id),function(indices){
    round<-x[indices,,drop=FALSE]
    settlements<-unique(round[,c("team_id","date","settlement_label"),drop=FALSE])
    repeated<-any(table(settlements$team_id)>1L)
    # Split/repeated evidence from ANY team invalidates a full-round date. A
    # reference team cannot hide another team's known later settlement.
    if(length(unique(round$settlement_label))>1L||repeated){
      return(data.frame(round_id=round$round_id[1],date="",settlement_label="unverified split settlement"))
    }
    reference<-sort(unique(round$team_id))[1]
    unique(round[round$team_id%in%reference,c("round_id","date","settlement_label"),drop=FALSE])
  }))
}
rival_reward_date <- function(round_id,anchors) {
  x<-anchors[anchors$round_id%in%round_id,,drop=FALSE]
  # One position settlement establishes the completed-round payment, including
  # a sole decimal label such as 6.2. Multiple distinct settlements stay ambiguous.
  if(nrow(x)==1L)x$date else ""
}


# Validate cumulative recorded payments independently before sharing their timing.
rival_point_reconciliation <- function(rounds,movements,rules,reference_team_id) {
  empty<-data.frame(round_id=character(),recorded=numeric(),expected=numeric(),difference=numeric(),settlement_labels=character(),date=character(),status=character())
  if(!nrow(rounds)&&!nrow(movements))return(empty)
  paid<-if(nrow(movements))movements[movements$team_id%in%reference_team_id & movements$bonus_type%in%"points",,drop=FALSE]else data.frame()
  ids<-unique(c(rounds$round_id,paid$round_id));ids<-ids[!is.na(ids)&nzchar(ids)]
  dplyr::bind_rows(c(list(empty),lapply(ids,function(id){
    r<-rounds[rounds$team_id%in%reference_team_id & rounds$round_id%in%id,,drop=FALSE]
    payments<-if(nrow(paid))paid[paid$round_id%in%id,,drop=FALSE]else data.frame()
    amount<-if(nrow(payments)&&all(is.finite(payments$money)))sum(payments$money)else NA_real_
    rate<-fm_number(rules$money_per_point)
    expected<-if(nrow(r)==1L&&round_is_finished(as.list(r))&&!is.na(r$negative)&&is.finite(r$points)&&is.finite(rate))if(isTRUE(r$negative))0 else r$points*rate else NA_real_
    dates<-if(nrow(payments))fm_time(payments$date)else as.POSIXct(character())
    valid_dates<-length(dates)>0L&&all(!is.na(dates))
    date<-if(valid_dates)payments$date[which.max(as.numeric(dates))]else ""
    status<-if(!is.finite(amount))"missing_payments"else if(!is.finite(expected))"unverified_history"else if(amount!=expected)"mismatch"else if(!valid_dates)"missing_date"else "validated"
    data.frame(round_id=id,recorded=amount,expected=expected,difference=amount-expected,
      settlement_labels=if(nrow(payments))paste(unique(payments$settlement_label),collapse=", ")else "",date=date,status=status)
  })))
}

# Same-type reference payments establish the league settlement calendar.
rival_reference_payments <- function(movements,reference_team_id) {
  empty<-data.frame(round_id=character(),bonus_type=character(),date=character(),settlement_labels=character())
  if(is.null(reference_team_id)||!nrow(movements))return(empty)
  rows<-movements[movements$team_id%in%reference_team_id & movements$bonus_type%in%c("points","ranking","dreamteam","mvp") & nzchar(movements$round_id),,drop=FALSE]
  if(!nrow(rows))return(empty)
  groups<-split(seq_len(nrow(rows)),paste(rows$round_id,rows$bonus_type,sep="|"))
  dplyr::bind_rows(c(list(empty),lapply(groups,function(indices){
    r<-rows[indices,,drop=FALSE];times<-fm_time(r$date)
    # A payment of another bonus type is never proof for this one.
    if(any(!is.finite(r$money))||anyNA(times))return(NULL)
    data.frame(round_id=r$round_id[1],bonus_type=r$bonus_type[1],date=r$date[which.max(as.numeric(times))],
      settlement_labels=paste(unique(r$settlement_label),collapse=", "))
  })))
}

# The official round-close notification uses directObject for a round: the
# shared decoder preserves its id/name as player_id/player_name. Persisted
# production evidence confirms type="round", action="close" and created_at.
# This dates reconstructed rewards; it neither creates income nor proves payment.
rival_round_publication_dates <- function(evidence,catalog=data.frame()) {
  empty<-data.frame(round_id=character(),date=character(),notification_id=character(),publication_count=integer(),stringsAsFactors=FALSE)
  rows<-dplyr::bind_rows(lapply(rival_evidence_rows(evidence,"notifications"),function(record){
    p<-record$payload
    if(!mvp_text(p$type)%in%c("round","rounds")||!identical(mvp_text(p$action),"close"))return(NULL)
    if(nzchar(mvp_text(p$championship_id))&&!identical(mvp_text(p$championship_id),mvp_text(record$championship_id)))return(NULL)
    id<-mvp_text(p$round_id,mvp_text(p$player_id));number<-fm_number(p$round_number,fm_number(p$player_name))
    if(all(c("round_id","round_number")%in%names(catalog))){
      if(nzchar(id)){
        known<-unique(catalog$round_number[catalog$round_id%in%id&is.finite(catalog$round_number)])
        if(is.finite(number)&&length(known)&&!number%in%known)return(NULL)
      }else if(is.finite(number)){
        ids<-unique(catalog$round_id[is.finite(catalog$round_number)&catalog$round_number==number])
        if(length(ids)==1L)id<-ids
      }
    }
    date<-mvp_text(p$created_at)
    # updated_at may change when a notification is read. Observation time is
    # retrieval time. Neither can substitute for the publication timestamp.
    if(!nzchar(id)||is.na(fm_time(date)))return(NULL)
    data.frame(round_id=id,date=date,notification_id=mvp_text(p$id,mvp_text(record$source_id)),stringsAsFactors=FALSE)
  }))
  if(!nrow(rows))return(empty)
  rows<-unique(rows)
  dplyr::bind_rows(lapply(split(seq_len(nrow(rows)),rows$round_id),function(indices){
    x<-rows[indices,,drop=FALSE]
    latest<-x[order(fm_time(x$date),x$notification_id,decreasing=TRUE)[1],,drop=FALSE]
    latest$publication_count<-nrow(x);latest
  }))
}

# Eligibility is accepted only as explicit historical evidence. No current roster lookup.
rival_award_estimates <- function(official,team_id,rules,eligibility=data.frame(),payments=data.frame(),round_rules=list(),bonus_types=c("dreamteam","mvp")) {
  empty<-data.frame(round_id=character(),player_id=character(),bonus_type=character(),amount=numeric(),reason=character(),stringsAsFactors=FALSE)
  if(!is.data.frame(official)||!all(c("round_id","player_id","is_mvp")%in%names(official)))return(list(rows=empty,missing="Official round awards unavailable."))
  unfinished<-!"is_finished"%in%names(official)||any(!official$is_finished%in%TRUE)
  if("is_finished"%in%names(official))official<-official[official$is_finished%in%TRUE,,drop=FALSE]else official<-official[FALSE,,drop=FALSE]
  events<-dplyr::bind_rows(lapply(seq_len(nrow(official)),function(i){
    row<-official[i,,drop=FALSE]
    data.frame(round_id=as.character(row$round_id),player_id=as.character(row$player_id),bonus_type=c("dreamteam",if(isTRUE(row$is_mvp))"mvp"),stringsAsFactors=FALSE)
  }))
  if(nrow(events))events<-events[events$bonus_type%in%bonus_types,,drop=FALSE]
  events<-unique(events);out<-list();missing<-if(unfinished)"Unfinished or unverified award selections are excluded from reconstructed cash."else character()
  for(i in seq_len(nrow(events))){
    e<-events[i,,drop=FALSE];rule<-round_rules[[e$round_id]] %||% rules
    amount<-fm_number(if(e$bonus_type=="mvp")rule$mvp_reward else rule$dreamteam_reward)
    if(is.finite(amount)&&amount==0)next
    unscoped<-nrow(payments)&&all(c("round_id","bonus_type")%in%names(payments))&&any(payments$bonus_type%in%e$bonus_type & !nzchar(payments$round_id))
    if(unscoped){missing<-c(missing,paste("Recorded",e$bonus_type,"income has no round identity; overlapping estimates of that reward type are excluded."));next}
    same<-if(nrow(payments)&&all(c("round_id","player_id","bonus_type")%in%names(payments)))payments$round_id%in%e$round_id & (!nzchar(payments$player_id)|payments$player_id%in%e$player_id) & payments$bonus_type%in%e$bonus_type else logical()
    # Native dreamTeam is one payment for XI members AND their MVP premium.
    # Its amount must be reconciled before attribution, never supplemented by
    # an estimated MVP merely because the source type is not literally mvp.
    combined<-nrow(payments)&&"source_type"%in%names(payments)&&any(
      payments$source_type%in%c("dreamTeam","dreamteam") &
      (payments$round_id%in%e$round_id | !nzchar(payments$round_id)))
    if(any(same)||combined)next
    evidence<-if(is.data.frame(eligibility)&&all(c("round_id","player_id","team_id","eligible","verified")%in%names(eligibility)))
      unique(eligibility[eligibility$round_id%in%e$round_id & eligibility$player_id%in%e$player_id & eligibility$verified%in%TRUE,c("team_id","eligible"),drop=FALSE])else data.frame()
    if(nrow(evidence)>1L){missing<-c(missing,paste("Conflicting historical ownership/eligibility for round",e$round_id,"player",e$player_id,"; reward excluded."));next}
    if(nrow(evidence)!=1L||!is.finite(amount)){missing<-c(missing,paste("Round",e$round_id,"player",e$player_id,": historical ownership (including roster-only recipients), eligibility or reward rule unavailable."));next}
    if(!isTRUE(evidence$eligible)||!identical(as.character(evidence$team_id),as.character(team_id)))next
    e$amount<-amount;e$reason<-if(isTRUE(rule$historical))"Historical rule; payment not observed."else"Assumes current rule; payment not observed."
    out[[length(out)+1L]]<-e
  }
  list(rows=dplyr::bind_rows(c(list(empty),out)),missing=unique(missing))
}
# Attribute a recorded native XI/MVP payment only when independent historical
# recipient calculations reproduce its total, including separately recorded MVP.
rival_award_payment_allocations <- function(official,team_id,rules,eligibility,movements) {
  empty<-data.frame(round_id=character(),recorded=numeric(),expected=numeric(),dreamteam=numeric(),mvp=numeric(),status=character())
  if(!nrow(movements)||!"source_type"%in%names(movements))return(empty)
  native<-movements$source_type%in%c("dreamTeam","dreamteam")
  if(!any(native))return(empty)
  selected<-if(is.data.frame(official)&&"round_id"%in%names(official))official[official$round_id%in%movements$round_id[native],,drop=FALSE]else data.frame()
  dplyr::bind_rows(c(list(empty),lapply(unique(movements$round_id[native]),function(id){
    paid<-movements[native & movements$round_id%in%id,,drop=FALSE]
    separate<-movements[!native & movements$round_id%in%id & movements$bonus_type%in%"mvp",,drop=FALSE]
    round_selection<-if(nrow(selected))selected[selected$round_id%in%id,,drop=FALSE]else selected
    expected<-rival_award_estimates(round_selection,team_id,rules,eligibility)
    rows<-expected$rows
    xi<-sum(rows$amount[rows$bonus_type%in%"dreamteam"])
    mvp<-sum(rows$amount[rows$bonus_type%in%"mvp"])
    recorded<-if(all(is.finite(paid$money)))sum(paid$money)else NA_real_
    explicit<-if(all(is.finite(separate$money)))sum(separate$money)else NA_real_
    valid<-nzchar(id)&&!length(expected$missing)&&nrow(rows)>0L&&is.finite(recorded)&&is.finite(explicit)&&
      recorded+explicit==xi+mvp&&mvp>=explicit
    data.frame(round_id=id,recorded=recorded,expected=if(nrow(rows))xi+mvp-explicit else NA_real_,
      dreamteam=if(valid)xi else NA_real_,mvp=if(valid)mvp-explicit else NA_real_,
      status=if(valid)"validated"else"unresolved")
  })))
}

# Source contract: public Futmondo Entities.Round (id, number, points, status,
# negative) and /1/userteam/roundlineup (players, bench.players, negative, status).
# Store one full round list per team; source revisions replace its earlier list.
rival_flag <- function(x) {
  if(is.logical(x)&&length(x)==1L&&!is.na(x))return(x)
  if(is.numeric(x)&&length(x)==1L&&x%in%c(0,1))return(x==1)
  NA
}
rival_team_round_rows <- function(answer,team_id) {
  empty<-data.frame(team_id=character(),round_id=character(),round_number=numeric(),points=numeric(),status=character(),negative=logical(),stringsAsFactors=FALSE)
  if(!is.list(answer)||isTRUE(answer$error)||!is.null(answer$code))return(empty)
  rows<-dplyr::bind_rows(c(list(empty),lapply(mvp_records(answer),function(row){
    if(!is.list(row)||!nzchar(mvp_text(row$id)))return(NULL)
    data.frame(team_id=as.character(team_id),round_id=mvp_text(row$id),round_number=fm_number(row$number),
      points=fm_number(row$points),status=mvp_text(row$status),negative=rival_flag(row$negative),stringsAsFactors=FALSE)
  })))
  rows[!duplicated(rows$round_id,fromLast=TRUE),,drop=FALSE]
}
# The official round-ranking endpoint returns the complete historical participant
# list. Its metadata status must confirm closure before it supplies cash inputs.
rival_round_ranking_rows <- function(payload) {
  empty<-data.frame(team_id=character(),round_id=character(),round_number=numeric(),points=numeric(),status=character(),negative=logical(),stringsAsFactors=FALSE)
  if(!is.list(payload)||!round_is_finished(list(status=payload$status))||!nzchar(mvp_text(payload$round_id)))return(empty)
  records<-mvp_records(payload$ranking)
  rows<-dplyr::bind_rows(c(list(empty),lapply(records,function(row){
    if(!is.list(row))return(NULL)
    data.frame(team_id=mvp_text(row$id),round_id=mvp_text(payload$round_id),round_number=fm_number(payload$round_number),
      points=fm_number(row$points),status=mvp_text(payload$status),negative=NA,stringsAsFactors=FALSE)
  })))
  if(!length(records)||nrow(rows)!=length(records)||any(!nzchar(rows$team_id))||anyDuplicated(rows$team_id)||any(!is.finite(rows$points)))return(empty)
  rows
}
rival_saved_rankings <- function(evidence,teams=NULL,source=NULL) {
  if(is.null(source))source<-rival_saved_rounds(evidence,include_ranking=FALSE)
  rows<-lapply(rival_evidence_rows(evidence,"round_ranking"),function(record){
    p<-record$payload;parsed<-rival_round_ranking_rows(p)
    ids<-if(is.null(teams))as.character(unlist(p$source_teams,use.names=FALSE))else as.character(teams)
    if(!nrow(parsed)||!length(ids)||!identical(mvp_text(p$signature),rival_ranking_signature(source,mvp_text(p$round_id),ids)))return(NULL)
    attr(record,"ranking_rows")<-parsed;record
  })
  Filter(Negate(is.null),rows)
}
rival_supplement_rounds <- function(rows,rankings) {
  for(record in rankings){
    ranking<-attr(record,"ranking_rows")
    key<-paste(rows$team_id,rows$round_id,sep="|")
    add<-!paste(ranking$team_id,ranking$round_id,sep="|")%in%key
    rows<-dplyr::bind_rows(rows,ranking[add,,drop=FALSE])
  }
  rows
}
rival_saved_rounds <- function(evidence,include_ranking=TRUE,teams=NULL) {
  empty<-rival_team_round_rows(list(),"")
  rows<-dplyr::bind_rows(c(list(empty),lapply(rival_evidence_rows(evidence,"team_rounds"),function(row){
    if(!is.list(row$payload))return(NULL)
    rival_team_round_rows(row$payload$rounds,mvp_text(row$payload$team_id))
  })))
  if(isTRUE(include_ranking))rows<-rival_supplement_rounds(rows,rival_saved_rankings(evidence,teams,rows))
  rows
}
rival_ranking_signature <- function(rows,round_id,teams) {
  selected<-rows[rows$round_id%in%round_id,intersect(c("team_id","round_id","round_number","points","status","negative"),names(rows)),drop=FALSE]
  selected<-selected[order(selected$team_id),,drop=FALSE];rownames(selected)<-NULL
  mvp_text(as.character(openssl::sha256(serialize(list(rows=selected,teams=sort(unique(as.character(teams)))),NULL))))
}
get_rival_round_ranking_observation <- function(login,championship_id,user_team_id,round_id,round_number,signature,source_teams) {
  response<-mvp_api_answer(login,"https://api.futmondo.com/1/ranking/round",list(
    championshipId=championship_id,roundNumber=round_number,userteamId=user_team_id))
  result<-list(round_id=round_id,round_number=round_number,signature=signature,source_teams=as.list(source_teams),status=response$status,
    answer=if(identical(response$status,"ok"))response$answer else NULL)
  attr(result,"observed_at")<-Sys.time();result
}
rival_ranking_evidence <- function(result,account_id,championship_id,season) {
  answer<-result$answer
  if(!is.list(answer)||isTRUE(answer$error)||!is.null(answer$code))return(list())
  payload<-list(round_id=result$round_id,round_number=result$round_number,status=mvp_text(answer$metadata$status),
    ranking=answer$ranking,signature=result$signature,source_teams=result$source_teams)
  if(!nrow(rival_round_ranking_rows(payload)))return(list())
  list(rival_finance_evidence(account_id,championship_id,season,"round_ranking",result$round_id,payload,
    attr(result,"observed_at") %||% Sys.time()))
}
rival_round_signature <- function(row,awards) {
  selected<-if(is.data.frame(awards)&&all(c("round_id","player_id")%in%names(awards)))awards[awards$round_id%in%row$round_id,,drop=FALSE]else data.frame()
  if(nrow(selected))selected<-selected[order(selected$player_id),intersect(c("player_id","points","is_mvp","is_finished"),names(selected)),drop=FALSE]
  mvp_text(as.character(openssl::sha256(serialize(list(round=as.list(row[,intersect(c("round_id","points","status","negative"),names(row)),drop=FALSE]),awards=unname(lapply(seq_len(nrow(selected)),function(i)as.list(selected[i,,drop=FALSE])))),NULL))))
}
rival_resolve_archived_lineups <- function(candidates,context,rounds,awards,existing=list()) {
  if(!fm_context_known(context$competition_id)||!fm_context_known(context$season)||
    !is.data.frame(rounds)||!nrow(rounds)||!is.data.frame(awards)||!nrow(awards))return(list())
  known<-vapply(rival_evidence_rows(existing,'round_lineup'),function(row)mvp_text(row$source_id),character(1))
  out<-list();records<-rival_evidence_rows(candidates,'round_lineup')
  stamps<-vapply(records,function(row)if(length(row$observed_at)==1L)as.numeric(fm_time(row$observed_at))else NA_real_,numeric(1))
  records<-records[order(stamps,decreasing=TRUE,na.last=TRUE)]
  stamp_groups<-split(records,vapply(records,function(row)paste(mvp_text(row$source_id),mvp_text(row$observed_at),sep='\034'),character(1)))
  for(record in records){
    p<-record$payload;source<-mvp_text(record$source_id)
    if(source%in%known||!identical(mvp_text(record$account_id),context$account_id)||
      !identical(mvp_text(record$championship_id),context$championship_id))next
    old_competition<-fm_scalar(record$competition_id,'unknown');old_season<-fm_scalar(record$season,'unknown')
    if(!old_competition%in%c('unknown',context$competition_id)||!old_season%in%c('unknown',context$season)||
      !('unknown'%in%c(old_competition,old_season)))next
    # A newer contradictory/revised observation must not expose an older
    # matching archive merely because the newer revision fails validation.
    known<-c(known,source)
    same_stamp<-stamp_groups[[paste(source,mvp_text(record$observed_at),sep='\034')]]
    if(length(unique(vapply(same_stamp,function(other)rival_calculation_hash(other$payload),character(1))))>1L)next
    if(!identical(source,paste0('lineup:',mvp_text(p$team_id),':',mvp_text(p$round_id))))next
    current<-rounds[rounds$team_id%in%mvp_text(p$team_id)&rounds$round_id%in%mvp_text(p$round_id),,drop=FALSE]
    if(nrow(current)!=1L||!round_is_finished(as.list(current))||!mvp_text(p$round_id)%in%awards$round_id||
      !identical(mvp_text(p$signature),rival_round_signature(current,awards)))next
    players<-unlist(p$players,use.names=FALSE)
    stamp<-fm_time(record$observed_at)
    if(!is.character(players)||!length(players)||anyNA(players)||any(!nzchar(players))||
      !is.logical(p$negative)||length(p$negative)!=1L||is.na(p$negative)||
      !round_is_finished(list(status=p$status))||length(stamp)!=1L||is.na(stamp)||stamp>Sys.time()+60)next
    # The authoritative current team-round identity and exact current revision
    # establish this record's scope. Preserve the original source observation.
    record$competition_id<-context$competition_id;record$season<-context$season
    record$payload$context_resolution<-list(method='current_team_round_and_award_signature',
      original_competition_id=old_competition,original_season=old_season)
    out<-c(out,list(record))
  }
  out
}
rival_lineup_evidence <- function(result,account_id,championship_id,season) {
  answer<-result$answer
  if(!is.list(answer)||isTRUE(answer$error)||!is.list(answer$players)||!length(answer$players))return(list())
  players<-unique(vapply(mvp_records(answer$players),function(x)if(is.list(x))mvp_text(x$id)else"",character(1)))
  bench<-unique(vapply(mvp_records(answer$bench$players),function(x)if(is.list(x))mvp_text(x$id)else"",character(1)))
  if(!any(nzchar(players)))return(list())
  payload<-list(team_id=result$team_id,round_id=result$round_id,status=mvp_text(answer$status),negative=rival_flag(answer$negative),
    players=as.list(players[nzchar(players)]),bench=as.list(bench[nzchar(bench)]),signature=result$signature,
    raw_lineup=answer)
  list(rival_finance_evidence(account_id,championship_id,season,"round_lineup",paste(result$team_id,result$round_id,sep=":"),payload,
    attr(result,"observed_at") %||% Sys.time()))
}
get_rival_round_lineup_observation <- function(login,championship_id,user_team_id,round_id,signature) {
  answer<-mvp_api_answer(login,"https://api.futmondo.com/1/userteam/roundlineup",
    list(championshipId=championship_id,userteamId=user_team_id,round=round_id))
  result<-list(team_id=user_team_id,round_id=round_id,signature=signature,status=answer$status,reason=answer$reason %||% "",answer=if(identical(answer$status,"ok"))answer$answer else NULL)
  attr(result,"observed_at")<-Sys.time();result
}
# FAQ142 + its linked official calculator; Math.round is floor(x + .5), not R's
# round-to-even. Tied teams share the prizes of all slots they occupy (FAQ134).
# Confirmed round-6 payment: truncate the shared prize to whole euros after
# averaging rounded slots (5,666,666.50 -> 5,666,666); do not round the balance.
rival_ranking_allocations <- function(points,pool,mode,users_to_rank=-1) {
  n<-length(points);unknown<-rep(NA_real_,n)
  if(!n||any(!is.finite(points))||!is.finite(pool)||pool<0||!mode%in%c("top","flop"))return(unknown)
  if(pool==0)return(rep(0,n))
  count<-if(identical(as.numeric(users_to_rank),-1))n else as.integer(users_to_rank)
  if(length(count)!=1L||is.na(count)||!count%in%c(3L,5L,n))return(unknown)
  weights<-if(count==n&&users_to_rank==-1)rev(seq_len(n))/sum(seq_len(n)) else if(count==3L)c(.5,.3,.2)else if(count==5L)c(.4,.3,.15,.1,.05)else return(unknown)
  slots<-c(floor(pool*weights+.5),rep(0,max(0,n-length(weights))))[seq_len(n)]
  order<-order(points,decreasing=mode=="top");ordered<-points[order];out<-numeric(n)
  for(score in unique(ordered)){indices<-which(ordered==score);out[order[indices]]<-floor(mean(slots[indices]))}
  out
}
# An explicitly empty, successfully saved history can identify a nonparticipant,
# but absence alone cannot establish the ranking denominator. Require the same
# remaining participants to reproduce payments in two different closed rounds.
rival_ranking_participants <- function(evidence,team_ids,rules,rounds=NULL) {
  ids<-unique(as.character(team_ids));ids<-ids[!is.na(ids)&nzchar(ids)]
  unchanged<-list(team_ids=ids,excluded=character(),verified_rounds=character())
  histories<-rival_evidence_rows(evidence,"team_rounds")
  empty<-vapply(ids,function(id){
    records<-Filter(function(row)identical(mvp_text(row$payload$team_id),id),histories)
    if(!length(records))return(FALSE)
    stamps<-vapply(records,function(row)as.numeric(fm_time(row$observed_at)),numeric(1));stamps[!is.finite(stamps)]<--Inf
    p<-records[[tail(order(stamps),1)]]$payload
    "rounds"%in%names(p)&&is.list(p$rounds)&&length(p$rounds)==0L
  },logical(1))
  if(!any(empty)||all(empty)||!is.finite(fm_number(rules$ranking_pool))||fm_number(rules$ranking_pool)<=0)return(unchanged)
  participants<-ids[!empty]
  if(is.null(rounds))rounds<-rival_saved_rounds(evidence,include_ranking=FALSE)
  if(!nrow(rounds)||any(rounds$team_id%in%ids[empty]))return(unchanged)
  catalog<-unique(rounds[,c("round_id","round_number"),drop=FALSE])
  movements<-dplyr::bind_rows(lapply(ids,function(id)rival_normalize_movements(rival_cash_latest_movements(evidence,id),catalog)))
  if(!nrow(movements))return(unchanged)
  paid<-movements[movements$bonus_type%in%"ranking",,drop=FALSE]
  if(!nrow(paid)||any(paid$team_id%in%ids[empty])||any(!nzchar(paid$round_id))||any(!is.finite(paid$money)))return(unchanged)
  verified<-character()
  for(round in unique(paid$round_id)){
    rows<-rounds[rounds$round_id%in%round&rounds$team_id%in%ids,,drop=FALSE]
    if(!rival_ranking_round_complete(rows,participants)||anyNA(rows$negative))next
    eligible<-!rows$negative
    expected<-rep(0,nrow(rows));expected[eligible]<-rival_ranking_allocations(rows$points[eligible],fm_number(rules$ranking_pool),
      mvp_text(rules$ranking_mode),fm_number(rules$users_to_rank %||% rules$configuration$usersToRank))
    if(any(!is.finite(expected)))return(unchanged)
    payments<-paid[paid$round_id%in%round,,drop=FALSE]
    # Latest revisions are already deduplicated by movement ID. Separate source
    # IDs, including split settlements, add once before comparison.
    totals<-tapply(payments$money,payments$team_id,sum)
    calculated<-expected[match(names(totals),rows$team_id)]
    if(anyNA(calculated)||any(abs(totals-calculated)>1e-7))return(unchanged)
    if(any(totals>0))verified<-c(verified,round)
  }
  if(length(verified)<2L)return(unchanged)
  list(team_ids=participants,excluded=ids[empty],verified_rounds=verified)
}
rival_ranking_round_complete <- function(rows,team_ids) {
  nrow(rows)==length(team_ids)&&setequal(rows$team_id,as.character(team_ids))&&all(is.finite(rows$points))&&
    all(vapply(seq_len(nrow(rows)),function(i)round_is_finished(as.list(rows[i,,drop=FALSE])),logical(1)))
}
# One preparation pass per publication, shared by all team calculations.
rival_historical_components <- function(evidence,teams,rules,official) {
  source<-rival_saved_rounds(evidence,include_ranking=FALSE)
  participants<-rival_ranking_participants(evidence,teams$teamid,rules,source)
  saved_rankings<-rival_saved_rankings(evidence,teams$teamid,source)
  rounds<-rival_supplement_rounds(source,saved_rankings);all_rounds<-rounds
  final<-if(nrow(rounds))vapply(seq_len(nrow(rounds)),function(i)round_is_finished(as.list(rounds[i,,drop=FALSE])),logical(1))else logical()
  rounds<-rounds[final,,drop=FALSE]
  lineups<-rival_evidence_rows(evidence,"round_lineup")
  keys<-paste(rounds$team_id,rounds$round_id,sep="|")
  lineup_groups<-split(lineups,vapply(lineups,function(record)paste(mvp_text(record$payload$team_id),mvp_text(record$payload$round_id),sep="|"),character(1)))
  signatures<-stats::setNames(rep("",nrow(rounds)),keys)
  needed<-which(keys%in%names(lineup_groups))
  signatures[needed]<-vapply(needed,function(i)rival_round_signature(rounds[i,,drop=FALSE],official),character(1))
  for(i in seq_len(nrow(rounds))){
    candidates<-Filter(function(record){
      p<-record$payload
      !nzchar(mvp_text(p$signature))||identical(mvp_text(p$signature),unname(signatures[i]))
    },lineup_groups[[keys[i]]])
    if(length(candidates)){
      negative<-rival_flag(tail(candidates,1)[[1]]$payload$negative)
      if(isTRUE(negative)||is.na(rounds$negative[i]))rounds$negative[i]<-negative
    }
  }
  income<-data.frame(team_id=character(),round_id=character(),bonus_type=character(),amount=numeric(),source=character(),stringsAsFactors=FALSE)
  standings<-data.frame(team_id=character(),round_id=character(),position=numeric(),tied=logical())
  missing<-list();add_missing<-function(id,text)missing[[id]]<<-unique(c(missing[[id]],text))
  for(id in as.character(teams$teamid)){
    rows<-rounds[rounds$team_id%in%id,,drop=FALSE]
    rate<-fm_number(rules$money_per_point)
    if(nrow(rows)&&is.finite(rate)){
      for(i in seq_len(nrow(rows))){
        row<-rows[i,,drop=FALSE]
        if(!is.finite(row$points)){add_missing(id,paste("Points missing for round",row$round_number));next}
        source<-"Published round points; assumes unchanged current reward rules."
        if(is.na(row$negative)){add_missing(id,"Starting cash eligibility is unknown for some rounds; rewards assume eligibility.");source<-paste(source,"Assumes eligible starting cash.")}
        income<-dplyr::bind_rows(income,data.frame(team_id=id,round_id=row$round_id,bonus_type="points",amount=if(isTRUE(row$negative))0 else row$points*rate,source=source))
      }
    }else if(!nrow(rows)&&(!is.finite(rate)||rate!=0)){
      add_missing(id,if(any(all_rounds$team_id%in%id))"No completed round points are published yet; open-round points are excluded."else "Per-round point history is unavailable; retrieve historical rounds to calculate point income.")
    }
    if(!is.finite(rate))add_missing(id,"Point reward rule unavailable.")
  }
  for(round in unique(rounds$round_id)){
    if(is.data.frame(official)&&all(c("round_id","is_finished")%in%names(official))&&
       any(official$round_id%in%round&official$is_finished%in%FALSE)){
      for(id in as.character(teams$teamid))add_missing(id,"Ranking awards are excluded while an official selection still marks the round unfinished.")
      next
    }
    rows<-rounds[rounds$round_id%in%round&rounds$team_id%in%teams$teamid,,drop=FALSE]
    saved_ranking<-Filter(function(record)identical(mvp_text(record$payload$round_id),round),saved_rankings)
    authoritative<-length(saved_ranking)>0L
    if(authoritative){
      ranked<-attr(tail(saved_ranking,1)[[1]],"ranking_rows")
      ranked$negative<-rows$negative[match(ranked$team_id,rows$team_id)]
      rows<-ranked
    }
    if((!authoritative&&!rival_ranking_round_complete(rows,participants$team_ids))||any(!is.finite(rows$points))){
      for(id in as.character(teams$teamid))add_missing(id,paste("Ranking history incomplete for round",rounds$round_number[match(round,rounds$round_id)]));next
    }
    # Actual score order, independent of top/flop prize distribution. Ties
    # share a position; incomplete participant sets never create a rank.
    standings<-dplyr::bind_rows(standings,data.frame(team_id=rows$team_id,round_id=round,
      position=rank(-rows$points,ties.method="min"),
      tied=duplicated(rows$points)|duplicated(rows$points,fromLast=TRUE)))
    # Negative-start teams do not take part in automatic premiums (FAQ142).
    eligible<-!rows$negative%in%TRUE
    value<-rep(0,nrow(rows));value[eligible]<-rival_ranking_allocations(rows$points[eligible],fm_number(rules$ranking_pool),
      mvp_text(rules$ranking_mode),fm_number(rules$users_to_rank %||% rules$configuration$usersToRank))
    for(i in seq_len(nrow(rows))){
      if(!is.finite(value[i])){add_missing(rows$team_id[i],"Ranking reward configuration is unavailable or unsupported.");next}
      if(!rows$team_id[i]%in%teams$teamid)next
      source<-if(authoritative)"Official historical round ranking; assumes unchanged reward rules."else"Published round standings; assumes unchanged ranking rules and historical league membership."
      if(!authoritative&&length(participants$excluded))source<-paste("Published round standings; nonparticipation supported by explicitly empty histories and matching recorded position payments in",length(participants$verified_rounds),"rounds; assumes unchanged reward rules.")
      if(anyNA(rows$negative))source<-paste(source,"Assumes eligibility where starting cash is unknown.")
      income<-dplyr::bind_rows(income,data.frame(team_id=rows$team_id[i],round_id=round,bonus_type="ranking",amount=value[i],source=source))
    }
  }
  if(!nrow(rounds)&&(!is.finite(fm_number(rules$ranking_pool))||fm_number(rules$ranking_pool)!=0))
    for(id in as.character(teams$teamid))add_missing(id,"Historical per-round standings are unavailable for ranking rewards.")
  eligibility<-data.frame(round_id=character(),player_id=character(),team_id=character(),eligible=logical(),verified=logical(),stringsAsFactors=FALSE)
  for(record in rival_evidence_rows(evidence,"round_lineup")){
    p<-record$payload;if(!is.list(p))next
    r<-rounds[rounds$team_id%in%mvp_text(p$team_id)&rounds$round_id%in%mvp_text(p$round_id),,drop=FALSE]
    if(!nrow(r))next
    if(mvp_text(p$status)%in%c("running","next")){add_missing(p$team_id,"Historical lineup is still marked open; its awards are excluded.");next}
    expected<-unname(signatures[paste(mvp_text(p$team_id),mvp_text(p$round_id),sep="|")])
    if(nzchar(mvp_text(p$signature))&&!identical(mvp_text(p$signature),mvp_text(expected))){
      add_missing(p$team_id,"A changed round is waiting for its historical lineup to refresh.");next
    }
    players<-unique(as.character(unlist(c(p$players,p$bench),use.names=FALSE)))
    if(!length(players))next
    eligibility<-dplyr::bind_rows(eligibility,data.frame(round_id=p$round_id,player_id=players,team_id=p$team_id,
      eligible=!isTRUE(p$negative)&&!isTRUE(r$negative[1]),verified=TRUE))
    if(is.na(rival_flag(p$negative))&&is.na(r$negative[1]))add_missing(p$team_id,"Award rewards assume eligible starting cash where the historical flag is absent.")
  }
  # Closed team histories permit candidate calculations for old award snapshots.
  # In reference mode, final-settlement evidence below gates their paid cash;
  # a team history can close between instalments of a postponed round.
  completed<-Filter(function(id){
    rows<-all_rounds[all_rounds$round_id%in%id,,drop=FALSE]
    nrow(rows)>0L&&setequal(rows$team_id,as.character(teams$teamid))&&
      all(vapply(seq_len(nrow(rows)),function(i)round_is_finished(as.list(rows[i,,drop=FALSE])),logical(1)))
  },unique(all_rounds$round_id))
  if(is.data.frame(official)&&nrow(official)&&"is_finished"%in%names(official))
    official$is_finished[is.na(official$is_finished)&official$round_id%in%completed]<-TRUE
  list(income=income,eligibility=unique(eligibility),official=official,missing=missing,rounds=rounds,standings=standings)
}

# The same entries drive component totals and transaction history. Reconstructed
# dates prefer applicable payment evidence, then round publication; recorded
# rows retain source dates.
rival_cash_entry <- function(id,component,amount,source,type="bonus",date="",category="",round_id="",player_id="",estimated=FALSE,included=TRUE,bonus_type="",calculation="",source_type="",source_category="",settlement_label="",payment_date_estimated=FALSE,payment_date_source="",source_event_id="") {
  data.frame(id=id,component=component,amount=amount,source=source,type=type,date=date,category=category,
    round_id=round_id,player_id=player_id,estimated=estimated,included=included,bonus_type=bonus_type,calculation=calculation,
    source_type=source_type,source_category=source_category,settlement_label=settlement_label,payment_date_estimated=payment_date_estimated,
    payment_date_source=payment_date_source,source_event_id=source_event_id,stringsAsFactors=FALSE)
}
rival_cash_ledger <- function(entries) {
  entries$timestamp<-fm_time(entries$date)
  entries$section<-ifelse(entries$type%in%"opening","opening",ifelse(is.na(entries$timestamp),"undated","dated"))
  # IDs break equal-time ties reproducibly, without claiming their true sequence.
  phase<-match(entries$section,c("opening","dated","undated"))
  entries<-entries[order(phase,entries$timestamp,entries$id,na.last=TRUE),,drop=FALSE]
  entries$ledger_order<-seq_len(nrow(entries))
  known<-entries$included & is.finite(entries$amount)
  opening<-which(entries$section%in%"opening")
  entries$running_balance<-if(length(opening)==1L&&is.finite(entries$amount[opening]))
    cumsum(ifelse(known,entries$amount,0))else rep(NA_real_,nrow(entries))
  entries$unknown_amounts<-cumsum(entries$included & !is.finite(entries$amount))
  rownames(entries)<-NULL
  entries
}

# Signed cash change from the same included ledger, excluding opening funding.
# Unknown opening cash does not hide known activity; unknown activity stays unknown.
rival_cash_net_gain <- function(ledger) {
  if(!is.data.frame(ledger)||!nrow(ledger)||!all(c("amount","type","included")%in%names(ledger)))return(NA_real_)
  if(anyNA(ledger$included))return(NA_real_)
  included<-ledger$included%in%TRUE
  opening<-ledger$type%in%"opening"
  if(anyNA(ledger$type[included])||any(!nzchar(ledger$type[included]))||!any(included & opening))return(NA_real_)
  amounts<-ledger$amount[included & !opening]
  if(any(!is.finite(amounts)))NA_real_ else sum(amounts)
}

# Realised transfer margins use completed ownership cycles from the included
# cash ledger. Equal-time rows are resolved only by ownership or a fully closed
# aggregate; source IDs never fabricate transaction order.
rival_realized_trades <- function(ledger) {
  empty<-data.frame(buy_id=character(),sell_id=character(),player_id=character(),
    buy_price=numeric(),sell_price=numeric(),fees=numeric(),profit=numeric(),stringsAsFactors=FALSE)
  unavailable<-function(note)list(profit=NA_real_,pairs=empty,complete=FALSE,notes=note)
  if(!is.data.frame(ledger)||!nrow(ledger)||!all(c("amount","type","included")%in%names(ledger)))
    return(unavailable("Transfer ledger is not available."))
  n<-nrow(ledger)
  text<-function(field){x<-if(field%in%names(ledger))as.character(ledger[[field]])else rep("",n);x[is.na(x)]<-"";trimws(x)}
  type<-text("type");ids<-text("id");player<-text("player_id");events<-text("source_event_id")
  selected<-ledger$included%in%TRUE
  if(anyNA(ledger$included)||any(selected & !nzchar(type)))return(unavailable("A ledger entry has unknown inclusion or transaction type."))
  buying<-type%in%c("buy","clause");selling<-type%in%c("sell","clause_sell")
  trades<-which(selected & (buying|selling))
  if("transfer_identity_conflict"%in%names(ledger)&&any(ledger$transfer_identity_conflict[trades]%in%TRUE))
    return(unavailable("Recorded and public transfer player identities conflict; the source event must be reconciled."))
  amount<-ledger$amount
  dates<-if("date"%in%names(ledger))as.numeric(fm_time(ledger$date))else
    if("timestamp"%in%names(ledger))as.numeric(ledger$timestamp)else rep(NA_real_,n)
  names<-text("transfer_player_name")
  fallback<-sub(" [(]Purchased[)]$| [(]Sold[)]$","",text("component"))
  names[!nzchar(names)]<-fallback[!nzchar(names)]
  names[tolower(names)%in%c("player","unknown","buy","sell","transfer","clause")]<-""
  # An exact observed name can bridge a missing ID only if it does not denote
  # multiple known players. IDs remain authoritative when labels change.
  names_with_ids<-split(player[trades[nzchar(player[trades]) & nzchar(names[trades])]],
    names[trades[nzchar(player[trades]) & nzchar(names[trades])]])
  names_with_ids<-lapply(names_with_ids,unique)
  key<-ifelse(nzchar(player),paste0("id:",player),"")
  for(index in trades[!nzchar(key[trades]) & nzchar(names[trades])]){
    candidates<-names_with_ids[[names[index]]]
    if(length(candidates)==1L){player[index]<-candidates;key[index]<-paste0("id:",candidates)}
  }
  notes<-character();closed<-list();held<-list()
  unknown<-trades[!nzchar(key[trades])]
  if(length(unknown)&&any(selling[trades]))notes<-c(notes,"A transfer player cannot be identified; its sale cost cannot be established.")
  groups<-split(trades[nzchar(key[trades])],key[trades[nzchar(key[trades])]])
  # Most histories contain one closed acquisition per player. Match those
  # vectors together; repeated ownership, source gaps and explicit fees retain
  # the general cycle resolver below.
  category<-text("category")
  related_fees<-selected & type%in%c("fee","commission","trade_fee","transfer_fee") &
    (type%in%c("trade_fee","transfer_fee")|category%in%c("trade","transfer","market","clause")|nzchar(events))
  if(!length(notes)&&!any(related_fees)&&length(groups)){
    sold_groups<-groups[vapply(groups,function(rows)any(selling[rows]),logical(1))]
    if(length(sold_groups)&&all(lengths(sold_groups)==2L)){
      first<-vapply(sold_groups,function(rows)rows[1L],integer(1))
      second<-vapply(sold_groups,function(rows)rows[2L],integer(1))
      one_each<-buying[first]!=buying[second] & selling[first]!=selling[second]
      buys<-ifelse(buying[first],first,second);sells<-ifelse(selling[first],first,second)
      known<-all(one_each)&&all(is.finite(amount[c(buys,sells)]))&&all(is.finite(dates[c(buys,sells)]))&&
        all(dates[buys]<=dates[sells])&&all(amount[buys]<=0)&&all(amount[sells]>=0)
      if(known){
        pairs<-data.frame(buy_id=ids[buys],sell_id=ids[sells],player_id=player[buys],
          buy_price=-amount[buys],sell_price=amount[sells],fees=rep(0,length(buys)),
          profit=amount[sells]+amount[buys],stringsAsFactors=FALSE,row.names=NULL)
        return(list(profit=sum(pairs$profit),pairs=pairs,complete=TRUE,notes=""))
      }
    }
  }
  close_cycle<-function(buys,sells,identity){
    if(any(!is.finite(amount[c(buys,sells)]))||any(amount[buys]>0)||any(amount[sells]<0)){
      notes<<-c(notes,paste("Purchase or sale amount is missing or has an invalid sign for",identity,"."));return(invisible(NULL))
    }
    closed[[length(closed)+1L]]<<-list(buys=buys,sells=sells,key=identity,
      from=min(dates[buys]),to=max(dates[sells]),fees=numeric())
    invisible(NULL)
  }
  for(identity in names(groups)){
    rows<-groups[[identity]]
    if(!any(selling[rows])){held[[identity]]<-rows[buying[rows]];next}
    # The usual single acquisition/sale needs no per-timestamp inventory scan.
    if(length(rows)==2L&&sum(buying[rows])==1L&&all(is.finite(dates[rows]))&&
      dates[rows[buying[rows]]]<=dates[rows[selling[rows]]]){
      close_cycle(rows[buying[rows]],rows[selling[rows]],identity);next
    }
    if(any(!is.finite(dates[rows]))){
      notes<-c(notes,paste("A transfer date is missing for",identity,"; completed cycles cannot be ordered."));next
    }
    rows<-rows[order(dates[rows])]
    batches<-split(rows,match(dates[rows],unique(dates[rows])))
    inventory<-0L;pending_buys<-pending_sells<-integer();bad<-FALSE
    for(batch in batches){
      buys<-batch[buying[batch]];sells<-batch[selling[batch]]
      next_inventory<-inventory+length(buys)-length(sells)
      if(!next_inventory%in%c(0L,1L)){
        notes<-c(notes,paste("Transfer history for",identity,"has a sale without a matching purchase or overlapping ownership."));bad<-TRUE;break
      }
      if(next_inventory==0L){
        close_cycle(c(pending_buys,buys),c(pending_sells,sells),identity)
        pending_buys<-pending_sells<-integer()
      }else if(inventory==1L&&length(buys)==1L&&length(sells)==1L){
        # With one player already held, selling it precedes its re-purchase.
        close_cycle(pending_buys,c(pending_sells,sells),identity)
        pending_buys<-buys;pending_sells<-integer()
      }else{
        pending_buys<-c(pending_buys,buys);pending_sells<-c(pending_sells,sells)
      }
      inventory<-next_inventory
    }
    if(!bad&&inventory==1L){
      held[[identity]]<-pending_buys
      if(length(pending_sells))notes<-c(notes,paste("Equal-time re-purchases for",identity,"leave an unidentified unsold cost; realised profit cannot be separated."))
    }
  }
  # A fee belongs to a completed trade only through an explicit source event or
  # an explicitly typed/categorised transfer fee with one historical owner cycle.
  fee_rows<-which(selected & type%in%c("fee","commission","trade_fee","transfer_fee"))
  if(length(fee_rows)){
    aliases<-lapply(closed,function(cycle){j<-c(cycle$buys,cycle$sells);unique(c(ids[j],sub("^(movement|pressroom):","",ids[j]),events[j]))})
    alias_keys<-unlist(lapply(aliases,function(x)x[nzchar(x)]),use.names=FALSE)
    alias_values<-rep(seq_along(closed),lengths(lapply(aliases,function(x)x[nzchar(x)])))
    lookup<-split(alias_values,alias_keys)
    category<-text("category")
    open_rows<-unlist(held,use.names=FALSE)
    open_aliases<-unique(c(ids[open_rows],sub("^(movement|pressroom):","",ids[open_rows]),events[open_rows]))
    for(index in fee_rows){
      linked<-integer()
      if(nzchar(events[index]))linked<-unique(lookup[[events[index]]])
      explicit<-type[index]%in%c("trade_fee","transfer_fee")||category[index]%in%c("trade","transfer","market","clause")
      if(!length(linked)&&explicit&&nzchar(player[index])&&is.finite(dates[index])){
        fee_key<-paste0("id:",player[index])
        linked<-which(vapply(closed,function(cycle)identical(cycle$key,fee_key)&&dates[index]>=cycle$from&&dates[index]<=cycle$to,logical(1)))
      }
      if(length(linked)==1L){
        if(!is.finite(amount[index]))notes<-c(notes,"A fee for a completed transfer has an unknown amount.")
        else closed[[linked]]$fees<-c(closed[[linked]]$fees,amount[index])
      }else if(length(linked)>1L)notes<-c(notes,"A transfer fee matches more than one completed cycle.")
      else if(explicit||nzchar(events[index])){
        held_fee<-nzchar(events[index])&&events[index]%in%open_aliases
        if(!held_fee&&explicit&&nzchar(player[index])&&is.finite(dates[index])){
          owned<-held[[paste0("id:",player[index])]]
          held_fee<-length(owned)>0L&&all(is.finite(dates[owned]))&&dates[index]>=min(dates[owned])
        }
        if(!held_fee)notes<-c(notes,"An explicit transfer fee cannot be assigned to a completed or unsold purchase; its source event or date is missing or inconsistent.")
      }
    }
  }
  pairs<-if(length(closed))dplyr::bind_rows(lapply(closed,function(cycle){
    buys<-cycle$buys;sells<-cycle$sells;cost<--sum(amount[buys]);proceeds<-sum(amount[sells]);fees<--sum(cycle$fees)
    list(buy_id=paste(ids[buys],collapse=","),sell_id=paste(ids[sells],collapse=","),
      player_id=sub("^(id|name):","",cycle$key),buy_price=cost,sell_price=proceeds,fees=fees,profit=proceeds-cost-fees)
  }))else empty
  notes<-unique(notes)
  list(profit=if(length(notes))NA_real_ else sum(pairs$profit),pairs=pairs,complete=!length(notes),notes=paste(notes,collapse=" "))
}

rival_estimate_summary <- function(missing,assumptions=character(),difference=NA_real_) {
  issues<-unique(missing[nzchar(missing)])
  standings<-grepl("^Ranking history incomplete for round ",issues)
  eligibility<-grepl("historical ownership|historical lineup|changed round",issues,ignore.case=TRUE)
  parts<-character()
  if(any(standings))parts<-c(parts,paste("Missing standings for",sum(standings),if(sum(standings)==1L)"round."else"rounds."))
  if(any(eligibility))parts<-c(parts,"Historical award eligibility is incomplete.")
  if(any(grepl("award coverage|Official round awards",issues,ignore.case=TRUE)))parts<-c(parts,"Round award history is incomplete.")
  if(is.finite(difference)&&abs(difference)>0.5)parts<-c(parts,"Reconstruction differs from observed cash.")
  if(!length(parts))parts<-if(length(issues))"Estimate uses available history; some financial evidence is incomplete."else if(length(assumptions))"Estimate assumes unchanged reward rules."else"No missing components in available financial evidence."
  paste(parts,collapse=" ")
}

# Public pure accounting interface; every numeric subtotal has an evidence trail.
# Versioned, context-bound calculation cache. Source records remain authoritative.
rival_calculation_version <- "2026-11-10-competition-v6"
rival_calculation_hash <- function(x) unname(unclass(as.character(openssl::sha256(serialize(x,NULL,version=2)))))
rival_calculation_evidence <- function(evidence) {
  rows<-Filter(function(x)!rival_snapshot_record(x)&&!x$evidence_type%in%c("team","coverage","rules"),evidence)
  rows<-lapply(rows,function(x){x$observed_at<-NULL;x$payload$observed_at<-NULL;x})
  if(length(rows))rows<-rows[order(vapply(rows,function(x)paste(x$evidence_type,x$source_id,sep=":"),character(1)))]
  rows
}
rival_calculation_cache <- function() {x<-new.env(parent=emptyenv());x$teams<-list();x}
source("rival_calculation_store.R")
rival_refresh_finance_row <- function(row,team,info=NULL,now=Sys.time(),missing=NULL,assumptions=NULL,source_stamp=NULL,estimated_movements=FALSE) {
  actual<-if(is.null(info))fm_number(row$observed_cash)else fm_number(info$budget)
  stamp<-if(is.null(info))mvp_text(row$observed_cash_at)else mvp_text(info$observed_at)
  age<-as.numeric(difftime(now,fm_time(stamp),units="secs"));fresh<-is.finite(actual)&&is.finite(age)&&age>=-60&&age<=300
  row$teamname<-mvp_text(team$teamname,mvp_text(row$teamname))
  if(nrow(team)){
    row$team_value<-fm_number(team$teamValue,fm_number(info$teamValue))
    row$points<-fm_number(team$points,fm_number(info$points))
    row$last_access<-mvp_text(team$lastAccess %||% team$last_access,NA_character_)
  }
  if(!is.null(info))row$squad_size<-fm_number(info$squad_size)
  row$observed_cash_at<-stamp
  row$observed_cash<-actual;row$observed_cash_fresh<-fresh;row$budget<-if(fresh)actual else NA_real_
  row$reconciliation_difference<-if(is.finite(actual))actual-fm_number(row$reconstructed_cash)else NA_real_
  if(!is.null(missing)&&!is.null(assumptions)){
    row$confidence<-if(fresh&&is.finite(row$reconciliation_difference)&&abs(row$reconciliation_difference)<=0.5)"Reconciled"else if(!length(missing)&&!length(assumptions))"Reconstructed"else"Estimated"
    reasons<-unique(c(missing,assumptions))
    if(is.finite(row$reconciliation_difference)&&abs(row$reconciliation_difference)>0.5)reasons<-c(reasons,paste0("Recorded cash differs from accounting by ",format(row$reconciliation_difference,big.mark=".",decimal.mark=",",scientific=FALSE)," EUR; no balancing adjustment added."))
    row$net_gain_estimated<-length(reasons)>0L||isTRUE(estimated_movements)
    row$estimate_notes<-paste(reasons,collapse=" ");row$estimate_summary<-rival_estimate_summary(missing,assumptions,row$reconciliation_difference)
  }else if(!fresh&&identical(row$confidence,"Reconciled"))row$confidence<-"Estimated"
  value<-fm_number(row$estimated_cash)
  row$cash_status<-if(!is.finite(value))"Unavailable"else if(value<0)if(row$confidence=="Reconciled")"Reconciled negative cash"else"Estimated negative cash"else"Positive or zero cash"
  if(nzchar(stamp))row$as_of<-stamp else if(!is.null(source_stamp))row$as_of<-source_stamp
  row
}

build_rival_cash_dataset <- function(teams,championship_id,evidence=list(),official=data.frame(),pressroom=data.frame(),rules=list(),opening_defaults=NULL,eligibility=data.frame(),coverage_complete=FALSE,now=Sys.time(),account_id=NULL,season="unknown",reference_team_id=NULL,cache=NULL) {
  empty<-data.frame(teamid=character(),teamname=character(),estimated_cash=numeric(),confidence=character(),as_of=character(),stringsAsFactors=FALSE)
  if(!is.data.frame(teams)||!nrow(teams))return(list(team_finances=empty,breakdowns=list(),cash_ledgers=list(),all_purchases=data.frame()))
  if(!nzchar(mvp_text(account_id)))stop("An explicit account is required for rival finance evidence.")
  evidence<-Filter(function(x)identical(mvp_text(x$championship_id),as.character(championship_id))&&identical(fm_scalar(x$competition_id,"unknown"),league_competition_context(championship_id,account_id))&&identical(mvp_text(x$season,"unknown"),as.character(season))&&(is.null(account_id)||identical(mvp_text(x$account_id),as.character(account_id))),Filter(function(x)!rival_snapshot_record(x),rival_merge_evidence(list(),evidence)))
  scope_frame<-function(rows){
    if(!is.data.frame(rows))return(data.frame())
    expected<-list(championship_id=championship_id,season=season,account_id=account_id)
    for(field in intersect(names(rows),names(expected)))if(!is.null(expected[[field]]))rows<-rows[rows[[field]]%in%expected[[field]],,drop=FALSE]
    rows
  }
  official<-scope_frame(official);eligibility<-scope_frame(eligibility)
  preparation_key<-rival_calculation_hash(list(version=rival_calculation_version,
    account=account_id,league=championship_id,season=season,reference=reference_team_id,
    teams=teams[,intersect(c("teamid","teamname"),names(teams)),drop=FALSE],
    evidence=rival_calculation_evidence(evidence),official=official,rules=rules,
    eligibility=eligibility,opening=opening_defaults))
  if(!is.null(cache)&&identical(cache$preparation_key,preparation_key)) {
    list2env(cache$prepared,envir=environment())
  } else {
  history<-rival_historical_components(evidence,teams,rules,official);official<-history$official
  stored_eligibility<-dplyr::bind_rows(lapply(rival_evidence_rows(evidence,"eligibility"),function(row){
    p<-row$payload
    if(!is.list(p)||!isTRUE(p$verified)||!all(c("round_id","player_id","team_id","eligible")%in%names(p)))return(NULL)
    data.frame(round_id=mvp_text(p$round_id),player_id=mvp_text(p$player_id),team_id=mvp_text(p$team_id),eligible=isTRUE(p$eligible),verified=TRUE)
  }))
  eligibility<-dplyr::bind_rows(eligibility,history$eligibility,stored_eligibility)
  if(all(c("round_id","player_id","team_id","eligible","verified")%in%names(eligibility)))
    eligibility<-unique(eligibility[,c("round_id","player_id","team_id","eligible","verified"),drop=FALSE])
  catalog<-unique(dplyr::bind_rows(rival_saved_rounds(evidence)[,c("round_id","round_number"),drop=FALSE],
    if(all(c("round_id","round_number")%in%names(official)))official[,c("round_id","round_number"),drop=FALSE]else data.frame()))
  all_movements<-dplyr::bind_rows(lapply(unique(vapply(rival_evidence_rows(evidence,"movements"),function(x)mvp_text(x$payload$team_id),character(1))),function(id)
    rival_normalize_movements(rival_cash_latest_movements(evidence,id),catalog)))
  ownership<-rival_ownership_eligibility(evidence,teams,official,history$rounds,catalog,all_movements)
  eligibility<-unique(dplyr::bind_rows(eligibility,ownership))
  for(id in unique(ownership$team_id[nzchar(ownership$team_id)]))history$missing[[id]]<-unique(c(history$missing[[id]],
    "Award ownership reconstructed from an unchanged player-summary owner throughout the official round-start to recorded final-settlement interval; historical reward-rule assumptions remain."))
  publications<-rival_round_publication_dates(evidence,catalog)
  anchors<-rival_payment_anchors(all_movements)
  reference_payments<-rival_reference_payments(all_movements,reference_team_id)
  # Official postponed-match policy: ranking/Best XI are settled only after all
  # matches. A dated final-category payment to ANY recipient proves settlement;
  # points, retrieved selections and team-round closed flags do not.
  final_settlements<-if(nrow(all_movements))unique(all_movements$round_id[
    all_movements$bonus_type%in%c("ranking","dreamteam","mvp") &
    is.finite(all_movements$money) & all_movements$money>0 &
    !is.na(fm_time(all_movements$date)) & nzchar(all_movements$round_id)])else character()
  raw_rounds<-rival_saved_rounds(evidence)
  point_rounds<-dplyr::bind_rows(history$rounds,raw_rounds[!paste(raw_rounds$team_id,raw_rounds$round_id)%in%paste(history$rounds$team_id,history$rounds$round_id),,drop=FALSE])
  point_checks<-rival_point_reconciliation(point_rounds,all_movements,rules,reference_team_id)
  opening<-rival_opening_cash(championship_id,rules,opening_defaults)
    if(!is.null(cache)){
      cache$preparation_key<-preparation_key
      cache$prepared<-mget(c("history","official","eligibility","catalog","all_movements","publications","anchors","reference_payments","final_settlements","raw_rounds","point_rounds","point_checks","opening"),inherits=FALSE)
      cache$preparations<-(cache$preparations %||% 0L)+1L
    }
  }
  # Preparation includes every movement; a player transfer does not change
  # other teams unless a shared reward/settlement prerequisite actually changed.
  dependency_key<-rival_calculation_hash(list(version=rival_calculation_version,account=account_id,league=championship_id,
    season=season,reference=reference_team_id,rules=rules,
    prepared=mget(c("history","official","eligibility","catalog","publications","anchors","reference_payments","final_settlements","raw_rounds","point_rounds","point_checks","opening"),inherits=FALSE)))
  if(!is.null(cache))cache$dependency_key<-dependency_key
  pressroom<-scope_frame(pressroom)
  if(is.data.frame(pressroom)&&nrow(pressroom)&&"id"%in%names(pressroom)){
    known<-!is.na(pressroom$id)&nzchar(as.character(pressroom$id))
    pressroom<-pressroom[!(known&duplicated(pressroom$id,fromLast=TRUE)),,drop=FALSE]
  }
  pressroom<-rival_classify_transfers(pressroom,all_movements)
  result<-list();breakdowns<-list();ledgers<-list()
  source_times<-vapply(evidence,function(x)mvp_text(x$observed_at),character(1));source_times<-source_times[!is.na(fm_time(source_times))]
  source_stamp<-if(length(source_times))source_times[which.max(fm_time(source_times))]else ""
  for(i in seq_len(nrow(teams))){
    team<-teams[i,,drop=FALSE];tid<-as.character(team$teamid %||% team$id);name<-as.character(team$teamname %||% team$name %||% tid)
    mov<-if(nrow(all_movements))all_movements[all_movements$team_id%in%tid,,drop=FALSE]else data.frame()
    own<-Filter(function(x)identical(mvp_text(x$source_id),tid),rival_evidence_rows(evidence,"team"));info<-if(length(own))tail(own,1)[[1]]$payload else list()
    coverage<-Filter(function(x)identical(mvp_text(x$source_id),tid),rival_evidence_rows(evidence,"coverage"));coverage<-if(length(coverage))tail(coverage,1)[[1]]$payload else list()
    actual<-fm_number(info$budget);stamp<-mvp_text(info$observed_at);fresh<-is.finite(actual)&&!is.na(fm_time(stamp))&&as.numeric(difftime(now,fm_time(stamp),units="secs"))>=-60&&as.numeric(difftime(now,fm_time(stamp),units="secs"))<=300
    team_transfers<-if(all(c("buyer_team_id","seller_team_id")%in%names(pressroom)))
      pressroom[pressroom$buyer_team_id%in%tid|pressroom$seller_team_id%in%tid,,drop=FALSE]else pressroom
    if("id"%in%names(team_transfers))team_transfers<-team_transfers[order(team_transfers$id),,drop=FALSE]
    rownames(team_transfers)<-NULL
    for(attribute in c("observed_at","fetched_at","full_history_at"))attr(team_transfers,attribute)<-NULL
    rownames(mov)<-NULL
    key_movements<-mov;key_movements$observed_at<-NULL
    key_coverage<-coverage;key_coverage$observed_at<-NULL
    team_key<-rival_calculation_hash(list(dependencies=dependency_key,team=tid,movements=key_movements,
      coverage=key_coverage,transfers=team_transfers,complete=coverage_complete))
    previous<-if(!is.null(cache))cache$teams[[tid]]else NULL
    if(!is.null(cache)&&!identical(previous$key,team_key)&&identical(cache$durable_teams[[tid]]$key,team_key))previous<-cache$durable_teams[[tid]]
    if(!is.null(previous)&&identical(previous$key,team_key)){
      cache$teams[[tid]]<-previous
      result[[i]]<-rival_refresh_finance_row(previous$result,team,info,now,previous$missing,previous$assumptions,source_stamp,previous$estimated_movements)
      breakdowns[[tid]]<-previous$breakdown;ledgers[[tid]]<-previous$ledger
      cache$hits<-(cache$hits %||% 0L)+1L
      next
    }
    details<-rival_cash_entry("opening","Opening cash",opening$amount,opening$source,type="opening")
    missing<-history$missing[[tid]] %||% character();assumptions<-character();recorded<-0;point_income<-ranking_income<-ideal_income<-mvp_income<-0
    failed_checks<-point_checks[point_checks$status!="validated" & is.finite(point_checks$recorded),,drop=FALSE]
    if(!is.null(reference_team_id)&&nrow(failed_checks))for(j in seq_len(nrow(failed_checks))){
      check<-failed_checks[j,,drop=FALSE]
      missing<-c(missing,paste("Reference point reconciliation",check$round_id,check$status,
        "recorded",check$recorded,"expected",check$expected,"difference",check$difference,"EUR."))
    }
    point_incomplete<-FALSE
    if(!is.null(reference_team_id)){
      relevant<-point_checks$round_id[is.finite(point_checks$recorded)|point_checks$round_id%in%history$rounds$round_id]
      paid_rounds<-if(nrow(mov))mov$round_id[mov$bonus_type%in%"points"]else character()
      for(id in setdiff(relevant,paid_rounds)){
        check<-point_checks[point_checks$round_id%in%id,,drop=FALSE]
        row<-history$rounds[history$rounds$round_id%in%id & history$rounds$team_id%in%tid,,drop=FALSE]
        if(nrow(check)!=1L||check$status!="validated"||nrow(row)!=1L||is.na(row$negative)||!is.finite(row$points)){
          point_incomplete<-TRUE
          missing<-c(missing,paste("Point reward for round",id,"requires refreshed history, eligibility or payment reconciliation; subtotal is incomplete."))
        }
      }
    }
    spent<-sales<-0
    # A source with unknown payment metadata can still establish the signed cash movement.
    complete_movements<-isTRUE(coverage$complete)&&isTRUE(coverage$history_complete)&&nrow(mov)>0L&&all(is.finite(mov$money))&&all(nzchar(mov$id))
    if(nrow(mov)){
      # An explicit opening allocation replaces the separate starting amount, never adds twice.
      budget<-which(mov$type%in%"budget");valid_budget<-budget[is.finite(mov$money[budget])]
      if(length(valid_budget)==1L){
        details$amount[1]<-mov$money[valid_budget];details$source[1]<-"Recorded opening allocation"
        details$id[1]<-paste0("movement:",mov$id[valid_budget]);details$date[1]<-mov$date[valid_budget]
      }
      if(length(valid_budget)>1L){complete_movements<-FALSE;missing<-c(missing,"Multiple budget allocations require season/reset evidence.")}
      excluded_budget<-if(length(valid_budget)==1L)setdiff(budget,valid_budget)else budget
      if(length(excluded_budget)){
        b<-mov[excluded_budget,,drop=FALSE]
        details<-dplyr::bind_rows(details,rival_cash_entry(paste0("movement:",b$id),"Unresolved budget allocation",b$money,
          "Excluded: opening/reset allocation cannot be resolved.",type="budget",date=b$date,included=FALSE))
        missing<-c(missing,"Unresolved budget allocation rows are shown but not added to cash.")
      }
      nonbudget<-mov[!mov$type%in%"budget",,drop=FALSE]
      if(nrow(nonbudget)){
        rows<-rival_cash_entry(paste0("movement:",nonbudget$id),ifelse(nzchar(nonbudget$concept),nonbudget$concept,nonbudget$type),nonbudget$money,
          "Recorded movement",type=nonbudget$type,date=nonbudget$date,category=nonbudget$category,round_id=nonbudget$round_id,player_id=nonbudget$player_id,
          bonus_type=nonbudget$bonus_type,source_type=nonbudget$source_type,source_category=nonbudget$source_category,settlement_label=nonbudget$settlement_label,source_event_id=nonbudget$source_event_id)
        ranked<-rows$bonus_type%in%"ranking"
        rows$component[ranked]<-"Position reward"
        details<-dplyr::bind_rows(details,rows);recorded<-sum(nonbudget$money[is.finite(nonbudget$money)])
        spent<--sum(nonbudget$money[nonbudget$type%in%"buy" & is.finite(nonbudget$money)]);sales<-sum(nonbudget$money[nonbudget$type%in%"sell" & is.finite(nonbudget$money)])
      }
    }
    # Prefer the account movement ledger as a whole: adding a second transfer feed
    # without common source identities would double count purchases and sales.
    if(!nrow(mov)){
      tx<-pressroom
      if(!is.data.frame(tx)||!all(c("buyer_team_id","seller_team_id","price")%in%names(tx)))missing<-c(missing,"Public transfer history is unavailable.")
      if(identical(attr(tx,"fetch_status"),"partial"))missing<-c(missing,"Public transfer history reached its page limit; earlier transfers may be missing.")
      if(is.data.frame(tx)&&all(c("buyer_team_id","seller_team_id","price")%in%names(tx))){
        if("id"%in%names(tx)){
          known_id<-!is.na(tx$id)&nzchar(as.character(tx$id))
          tx<-tx[!(known_id&duplicated(tx$id,fromLast=TRUE)),,drop=FALSE]
        }
        tx<-tx[tx$buyer_team_id%in%tid|tx$seller_team_id%in%tid,,drop=FALSE]
        if(nrow(tx)){
          money<-(as.numeric(tx$seller_team_id%in%tid)-as.numeric(tx$buyer_team_id%in%tid))*suppressWarnings(as.numeric(tx$price))
          if(any(!is.finite(money)))missing<-c(missing,"A public transfer amount is missing; the subtotal excludes it.")
          # Missing source IDs receive a content key for stable ordering only;
          # they are not presented as observed source identities or deduplicated.
          keys<-if("id"%in%names(tx))as.character(tx$id)else rep("",nrow(tx))
          absent<-is.na(keys)|!nzchar(keys)
          keys[absent]<-vapply(which(absent),function(j)as.character(openssl::sha256(serialize(as.list(tx[j,,drop=FALSE]),NULL))),character(1))
          dates<-if("created"%in%names(tx))as.character(tx$created)else rep("",nrow(tx))
          players<-as.character(tx$player_name %||% rep("Player",nrow(tx)));players[is.na(players)|!nzchar(players)]<-"Player"
          buying<-tx$buyer_team_id%in%tid;selling<-tx$seller_team_id%in%tid
          type<-ifelse(buying&selling,"transfer",ifelse(buying,"buy","sell"))
          concept<-paste0(players,ifelse(buying&selling," (Self transfer)",ifelse(buying," (Purchased)"," (Sold)")))
          details<-dplyr::bind_rows(details,rival_cash_entry(paste0("pressroom:",keys),concept,money,"Pressroom transfer",type=type,date=dates,category=ifelse(is.na(tx$is_clause),"unknown",ifelse(tx$is_clause,"clause","market")),player_id=as.character(tx$player_id %||% "")))
          spent<--sum(money[money<0],na.rm=TRUE);sales<-sum(money[money>0],na.rm=TRUE)
        }
      }
      missing<-c(missing,"Cash reconstructed from public transfers and round rewards; private/manual adjustments are not visible in the transfer feed.")
    }else if(!complete_movements)missing<-c(missing,"Movement history completeness is unverified; secondary transfer feed was not added twice.")
    # Any unclassified reward can already include these bonuses. Suppress estimated
    # rewards until it can be matched, rather than count an unidentified payment twice.
    unclassified<-nrow(mov)&&any((!is.finite(mov$money)|mov$money>0) & !mov$type%in%c("buy","sell","budget") & !mov$bonus_type%in%c("points","ranking","dreamteam","mvp"))
    award_allocations<-rival_award_payment_allocations(official,tid,rules,eligibility,mov)
    if(any(award_allocations$status!="validated"))missing<-c(missing,"Recorded combined Best XI/MVP payment does not yet reconcile with historical recipients/rules; its actual cash is retained, component attribution is unknown.")
    native_details<-which(!details$estimated & details$source_type%in%c("dreamTeam","dreamteam"))
    if(length(native_details)){
      details$component[native_details]<-"Best XI/MVP payment"
      details$source[native_details]<-paste(details$source[native_details],"Native dreamTeam payment covers Best XI and MVP together; no additional estimated MVP is added.")
    }
    estimate<-rival_award_estimates(official,tid,rules,eligibility,mov)
    missing<-c(missing,estimate$missing)
    if(unclassified&&nrow(estimate$rows)){missing<-c(missing,"Unclassified recorded income prevents adding potentially overlapping award estimates.");estimate$rows<-estimate$rows[FALSE,,drop=FALSE]}
    if(nrow(estimate$rows)){
      details<-dplyr::bind_rows(details,rival_cash_entry(paste("estimate",estimate$rows$bonus_type,estimate$rows$round_id,estimate$rows$player_id,sep=":"),
        ifelse(estimate$rows$bonus_type=="mvp","MVP reward","Best XI reward"),estimate$rows$amount,estimate$rows$reason,
        round_id=estimate$rows$round_id,player_id=estimate$rows$player_id,estimated=TRUE,bonus_type=estimate$rows$bonus_type))
      ideal_income<-sum(estimate$rows$amount[estimate$rows$bonus_type=="dreamteam"]);mvp_income<-sum(estimate$rows$amount[estimate$rows$bonus_type=="mvp"]);assumptions<-c(assumptions,estimate$rows$reason)
    }
    if(nrow(mov)){
      point_income<-sum(mov$money[mov$bonus_type%in%"points"],na.rm=TRUE);ranking_income<-sum(mov$money[mov$bonus_type%in%"ranking"],na.rm=TRUE)
      ideal_income<-ideal_income+sum(mov$money[mov$bonus_type%in%"dreamteam"],na.rm=TRUE);mvp_income<-mvp_income+sum(mov$money[mov$bonus_type%in%"mvp"],na.rm=TRUE)
    }
    if(nrow(mov)&&any(mov$bonus_type%in%c("points","ranking") & !nzchar(mov$round_id)))
      missing<-c(missing,"Recorded point/position income lacks round identity; overlapping estimates of those reward types are excluded.")
    round_income<-rival_round_income_estimates(evidence,tid,rules,mov)
    missing<-c(missing,round_income$missing)
    # Validated combined rows always use final team-round points, never an old
    # partial-credit estimate retained in legacy eligibility evidence.
    if(!is.null(reference_team_id)&&nrow(round_income$rows))
      round_income$rows<-round_income$rows[round_income$rows$component!="Point reward",,drop=FALSE]
    derived<-history$income[history$income$team_id%in%tid,,drop=FALSE]
    if(nrow(derived))for(j in seq_len(nrow(derived))){
      row<-derived[j,,drop=FALSE];component<-if(row$bonus_type=="points")"Point reward"else"Ranking reward"
      paid<-nrow(mov)&&any(mov$bonus_type%in%row$bonus_type & (mov$round_id%in%row$round_id | !nzchar(mov$round_id)))
      old<-nrow(round_income$rows)&&any(round_income$rows$component%in%component & round_income$rows$round_id%in%row$round_id)
      if(!paid&&!old)round_income$rows<-dplyr::bind_rows(round_income$rows,data.frame(component=component,amount=row$amount,source=row$source,round_id=row$round_id,player_id=""))
    }
    if(!is.null(reference_team_id)&&nrow(round_income$rows)){
      point_rows<-which(round_income$rows$component=="Point reward")
      allowed<-vapply(point_rows,function(j){
        id<-round_income$rows$round_id[j]
        check<-point_checks[point_checks$round_id%in%id,,drop=FALSE]
        own_round<-history$rounds[history$rounds$round_id%in%id & history$rounds$team_id%in%tid,,drop=FALSE]
        ok<-nrow(check)==1L&&check$status=="validated"&&nrow(own_round)==1L&&!is.na(own_round$negative)&&is.finite(own_round$points)
        if(!ok)missing<<-c(missing,paste("Point reward for round",id,"awaits payment reconciliation or historical eligibility; excluded from the known subtotal."))
        ok
      },logical(1))
      if(any(!allowed))round_income$rows<-round_income$rows[-point_rows[!allowed],,drop=FALSE]
    }
    if(unclassified&&nrow(round_income$rows)){
      missing<-c(missing,"Unclassified recorded income prevents adding potentially overlapping point/ranking estimates.")
    }else if(nrow(round_income$rows)){
      rows<-round_income$rows
      details<-dplyr::bind_rows(details,rival_cash_entry(paste("estimate",rows$component,rows$round_id,sep=":"),rows$component,rows$amount,rows$source,
        round_id=rows$round_id,player_id=rows$player_id,estimated=TRUE,bonus_type=ifelse(rows$component=="Point reward","points","ranking")))
      point_income<-point_income+sum(round_income$rows$amount[round_income$rows$component=="Point reward"])
      ranking_income<-ranking_income+sum(round_income$rows$amount[round_income$rows$component=="Ranking reward"])
      assumptions<-c(assumptions,round_income$rows$source)
    }
    # Reference position payments gate position cash; award ownership is independent.
    # Keep unconfirmed estimates auditable, but never include them in balances.
    pending_bonus<-character()
    if(!is.null(reference_team_id))for(j in which(details$estimated & details$bonus_type%in%"ranking" & (!is.finite(details$amount)|details$amount!=0))){
      payment<-reference_payments[reference_payments$round_id%in%details$round_id[j] & reference_payments$bonus_type%in%details$bonus_type[j],,drop=FALSE]
      if(nrow(payment)!=1L){
        details$included[j]<-FALSE;details$date[j]<-""
        details$payment_date_source[j]<-"reference_payment_pending"
        details$source[j]<-paste(details$source[j],"No reference-team payment of this bonus type is recorded for this round. Not counted as paid cash; a publication or another bonus type cannot establish payment.")
        pending_bonus<-unique(c(pending_bonus,details$bonus_type[j]))
        missing<-c(missing,paste("Reference payment not confirmed:",details$bonus_type[j],"round",details$round_id[j],"; excluded from paid cash."))
      }else{
        details$date[j]<-payment$date;details$payment_date_estimated[j]<-FALSE
        details$payment_date_source[j]<-"league_same_bonus_payment"
        details$source[j]<-paste(details$source[j],"Known league payment date from the reference team's same-type settlements",payment$settlement_labels,"; amount reconstructed independently. Combined at the latest recorded settlement; no future payment is assumed.")
      }
    }
    if(!is.null(reference_team_id))for(j in which(details$estimated & details$bonus_type%in%c("dreamteam","mvp") &
        !details$round_id%in%final_settlements & (!is.finite(details$amount)|details$amount!=0))){
      details$included[j]<-FALSE;details$date[j]<-""
      details$payment_date_source[j]<-"round_settlement_pending"
      details$source[j]<-paste(details$source[j],"Whole-round settlement is not yet evidenced. A retrieved selection or interim closed history cannot establish completion while matches may remain pending. Excluded from paid cash.")
      pending_bonus<-unique(c(pending_bonus,details$bonus_type[j]))
      missing<-c(missing,paste("Final settlement not confirmed for round",details$round_id[j],";",details$bonus_type[j],"excluded from paid cash."))
    }
    for(kind in c("ranking","mvp","dreamteam")){
      value<-sum(details$amount[details$included & details$bonus_type%in%kind],na.rm=TRUE)
      if(kind=="ranking")ranking_income<-value
      if(kind=="mvp")mvp_income<-value
      if(kind=="dreamteam")ideal_income<-value
    }
    if(nrow(award_allocations)){
      if(all(award_allocations$status=="validated")){
        attributed_mvp<-sum(award_allocations$mvp)
        ideal_income<-ideal_income-attributed_mvp;mvp_income<-mvp_income+attributed_mvp
      }else pending_bonus<-unique(c(pending_bonus,"dreamteam","mvp"))
    }
    # A configured zero reward is no cash movement and needs no payment date.
    # Preserve real zero-valued movements and all unknown amounts for auditing.
    zero_reward<-details$estimated & details$bonus_type%in%c("points","ranking","mvp","dreamteam") & is.finite(details$amount) & details$amount==0
    zero_bonus_rows<-details[zero_reward,c("round_id","bonus_type"),drop=FALSE]
    details<-details[!zero_reward,,drop=FALSE]
    if(!is.finite(details$amount[1]))missing<-c(missing,"Opening cash unavailable.")
    if(!isTRUE(coverage_complete))missing<-c(missing,"Round award coverage is incomplete.")
    # Group per-player award calculations into the two actual award components.
    award<-details$estimated & details$bonus_type%in%c("mvp","dreamteam")
    if(any(award)){
      groups<-split(which(award),paste(details$round_id[award],details$bonus_type[award],sep="|"))
      grouped<-lapply(groups,function(indices){
        row<-details[indices[1],,drop=FALSE];row$amount<-sum(details$amount[indices]);row$id<-paste("estimate",row$bonus_type,row$round_id,sep=":")
        labels<-if(all(c("player_id","player_name")%in%names(official)))official$player_name[match(details$player_id[indices],official$player_id)]else details$player_id[indices]
        labels[is.na(labels)|!nzchar(labels)]<-details$player_id[indices][is.na(labels)|!nzchar(labels)]
        row$calculation<-paste(paste0(labels,": ",format(details$amount[indices],scientific=FALSE)," EUR"),collapse="; ")
        row$player_id<-paste(details$player_id[indices],collapse=",");row
      })
      details<-dplyr::bind_rows(details[!award,,drop=FALSE],dplyr::bind_rows(grouped))
    }
    for(j in which(details$estimated & details$bonus_type%in%c("points","ranking","mvp","dreamteam"))){
      if(!details$included[j])next
      if(!is.null(reference_team_id)&&details$bonus_type[j]=="ranking")next
      check<-point_checks[point_checks$round_id%in%details$round_id[j],,drop=FALSE]
      if(!is.null(reference_team_id)&&details$bonus_type[j]=="points"&&nrow(check)==1L&&check$status=="validated"){
        details$date[j]<-check$date;details$payment_date_estimated[j]<-FALSE
        details$payment_date_source[j]<-"league_point_payment"
        details$component[j]<-"Combined round point payment"
        details$source[j]<-paste(details$source[j],"Reconstructed cumulative amount; known league payment date from reference-team settlements",check$settlement_labels,
          ". Earlier instalments are consolidated at the latest observed payment. A later payment replaces this cumulative entry after reconciliation; this date does not establish final round completion. Intermediate balances do not reproduce the instalment schedule.")
        next
      }
      if(!is.null(reference_team_id)&&details$bonus_type[j]%in%c("dreamteam","mvp")){
        payment<-reference_payments[reference_payments$round_id%in%details$round_id[j] & reference_payments$bonus_type%in%details$bonus_type[j],,drop=FALSE]
        details$source[j]<-paste(details$source[j],"Payment assumed from retrieved final award and historical eligibility, independently of whether the reference team received this award.")
        if(nrow(payment)==1L){
          details$date[j]<-payment$date;details$payment_date_estimated[j]<-FALSE
          details$payment_date_source[j]<-"league_same_bonus_payment"
          next
        }
      }
      publication<-publications[publications$round_id%in%details$round_id[j],,drop=FALSE]
      anchor<-if(details$bonus_type[j]%in%c("ranking","mvp","dreamteam"))rival_reward_date(details$round_id[j],anchors)else""
      if(nzchar(anchor)){
        details$date[j]<-anchor;details$payment_date_estimated[j]<-TRUE;details$payment_date_source[j]<-"league_position_payment"
        details$source[j]<-paste(details$source[j],"Date reconstructed at the unambiguous recorded league position-payment time; simultaneous position/MVP/Best XI rewards confirmed by league user.")
      }else if(nrow(publication)==1L){
        details$date[j]<-publication$date
        details$payment_date_estimated[j]<-TRUE
        details$payment_date_source[j]<-"round_points_publication"
        details$source[j]<-paste(details$source[j],"Date reconstructed from the round points-publication notification; actual payment time is not observed.",
          if(publication$publication_count>1L)"Repeated/split-round publications: the cumulative round reward is dated once at the latest publication."else"")
      }else{
        details$payment_date_source[j]<-"missing_round_publication"
        number<-unique(catalog$round_number[catalog$round_id%in%details$round_id[j]&is.finite(catalog$round_number)])
        label<-if(length(number)==1L)as.character(number)else details$round_id[j]
        issue<-paste("Round",label,"points-publication notification is missing; its reconstructed reward date cannot be established from saved evidence.")
        missing<-c(missing,issue);details$source[j]<-paste(details$source[j],issue)
      }
    }
    details$component[details$component%in%"Ranking reward"]<-"Position reward"
    ledger<-rival_cash_ledger(details)
    attr(ledger,"point_rewards_incomplete")<-point_incomplete
    attr(ledger,"pending_bonus_types")<-pending_bonus
    attr(ledger,"award_payment_allocations")<-award_allocations
    if(any(ledger$included&!is.finite(ledger$amount)))missing<-c(missing,"Missing movement amounts are excluded; displayed balances are known-amount estimates.")
    # Read the final cumulative amount, rather than independently summing a
    # second history. Filtering/sorting later never changes these balances.
    reconstructed<-tail(ledger$running_balance,1L)
    difference<-if(is.finite(actual)&&is.finite(reconstructed))actual-reconstructed else NA_real_
    confidence<-if(fresh&&is.finite(difference)&&abs(difference)<=0.5)"Reconciled"else if(!length(missing)&&!length(assumptions))"Reconstructed"else"Estimated"
    value<-reconstructed
    reasons<-unique(c(missing,assumptions));if(is.finite(difference)&&abs(difference)>0.5)reasons<-c(reasons,paste0("Recorded cash differs from accounting by ",format(difference,big.mark=".",decimal.mark=",",scientific=FALSE)," EUR; no balancing adjustment added."))
    if(!nzchar(stamp)){
      times<-vapply(evidence,function(x)mvp_text(x$observed_at),character(1));times<-times[!is.na(fm_time(times))]
      stamp<-if(length(times))times[which.max(fm_time(times))]else""
    }
    net_activity<-rival_cash_net_gain(ledger)
    result[[i]]<-data.frame(teamid=tid,teamname=name,estimated_cash=value,confidence=confidence,as_of=stamp,
      cash_status=if(!is.finite(value))"Unavailable"else if(value<0)if(confidence=="Reconciled")"Reconciled negative cash"else"Estimated negative cash"else"Positive or zero cash",
      initial_budget=details$amount[1],budget=if(fresh)actual else NA_real_,reconstructed_cash=reconstructed,reconciliation_difference=difference,
      observed_cash=actual,observed_cash_at=mvp_text(info$observed_at),observed_cash_fresh=fresh,
      total_inflow=sum(ledger$amount[ledger$included & is.finite(ledger$amount)&ledger$amount>0]),
      total_outflow=-sum(ledger$amount[ledger$included & is.finite(ledger$amount)&ledger$amount<0]),
      total_spent=spent,total_sales=sales,point_bonus=if(point_incomplete)NA_real_ else point_income,ranking_prize=if("ranking"%in%pending_bonus)NA_real_ else ranking_income,dreamteam_income=if("dreamteam"%in%pending_bonus)NA_real_ else ideal_income,mvp_income=if("mvp"%in%pending_bonus)NA_real_ else mvp_income,
      # Public directory values are authoritative; optional finance observations
      # may contain NA and must not erase them when the ledger is republished.
      team_value=fm_number(team$teamValue,fm_number(info$teamValue)),points=fm_number(team$points,fm_number(info$points)),
      last_access=mvp_text(team$lastAccess %||% team$last_access,NA_character_),
      squad_size=fm_number(info$squad_size),net_profit_loss=net_activity,cash_activity_complete=is.finite(net_activity),
      net_gain_estimated=length(reasons)>0L||any(ledger$included & ledger$estimated),estimate_summary=rival_estimate_summary(missing,assumptions,difference),estimate_notes=paste(reasons,collapse=" "),stringsAsFactors=FALSE)
    breakdowns[[tid]]<-ledger[,c("component","amount","source","round_id","player_id"),drop=FALSE]
    ledger$round_number<-history$rounds$round_number[match(ledger$round_id,history$rounds$round_id)]
    own_rounds<-history$rounds[history$rounds$team_id%in%tid,,drop=FALSE]
    own_standings<-history$standings[history$standings$team_id%in%tid,,drop=FALSE]
    ledger$round_points<-own_rounds$points[match(ledger$round_id,own_rounds$round_id)]
    ledger$round_position<-own_standings$position[match(ledger$round_id,own_standings$round_id)]
    ledger$round_position_tied<-own_standings$tied[match(ledger$round_id,own_standings$round_id)]
    ledger<-rival_transfer_metadata(ledger,pressroom,tid,teams,mov)
    trading<-rival_realized_trades(ledger)
    transfer_source<-nrow(mov)>0L||isTRUE(coverage$complete)||
      (is.data.frame(pressroom)&&all(c("buyer_team_id","seller_team_id","price")%in%names(pressroom))&&
        !mvp_text(attr(pressroom,"fetch_status"))%in%c("unavailable","error","loading"))
    if(!transfer_source){
      trading$profit<-NA_real_;trading$complete<-FALSE
      trading$notes<-paste(trading$notes,"Transfer history source is unavailable; a zero profit cannot be established.")
    }
    if(transfer_source&&!complete_movements)trading$notes<-paste(trading$notes,
      "Profit covers retrieved completed transfers; full-season history completeness is unverified.")
    if(!nrow(mov)&&identical(attr(pressroom,"fetch_status"),"partial"))trading$notes<-paste(trading$notes,
      "Public transfer history reached its page limit; earlier transfers may be missing.")
    result[[i]]$cash_activity_complete<-is.finite(net_activity)&&transfer_source
    # Count reconciled player movements once; rewards and fees are not trades.
    result[[i]]$player_transactions<-if(transfer_source)
      sum(ledger$included%in%TRUE & ledger$type%in%c("buy","sell","clause","clause_sell","transfer"))else NA_integer_
    result[[i]]$trading_profit<-trading$profit
    result[[i]]$trading_profit_complete<-trading$complete
    result[[i]]$trading_profit_notes<-trimws(trading$notes)
    attr(ledger,"realized_trades")<-trading
    # Preserve confirmed zero rewards and award coverage for round-filtered views.
    # Amounts always come from the included ledger, never a second cash calculation.
    bonus_rounds<-list()
    for(rid in unique(history$rounds$round_id[history$rounds$team_id%in%tid])){
      awards<-if("round_id"%in%names(official))official[official$round_id%in%rid,,drop=FALSE]else data.frame()
      for(kind in c("points","mvp","dreamteam","ranking")){
        entries<-ledger[ledger$included & ledger$round_id%in%rid & ledger$bonus_type%in%kind,,drop=FALSE]
        zero<-any(zero_bonus_rows$round_id%in%rid & zero_bonus_rows$bonus_type%in%kind)
        known<-nrow(entries)>0L||zero
        if(kind%in%c("mvp","dreamteam")){
          # A missing XI recipient cannot make a verified MVP unknown. Check
          # only this round/component while projecting the same cash ledger.
          award_check<-rival_award_estimates(awards,tid,rules,eligibility,mov,bonus_types=kind)
          rate<-fm_number(if(kind=="mvp")rules$mvp_reward else rules$dreamteam_reward)
          known<-is.finite(rate)&&rate==0 || (nrow(awards)>0L && (isTRUE(coverage_complete)||length(unique(awards$player_id))==11L) && !length(award_check$missing) && !unclassified)
        }
        allocation<-award_allocations[award_allocations$round_id%in%rid,,drop=FALSE]
        adjustment<-0
        if(kind%in%c("dreamteam","mvp")&&nrow(allocation)){
          known<-all(allocation$status=="validated")
          adjustment<-if(known)sum(allocation$mvp)*if(kind=="mvp")1 else -1 else 0
        }
        pending<-any(!ledger$included & ledger$round_id%in%rid & ledger$bonus_type%in%kind & ledger$payment_date_source%in%c("reference_payment_pending","round_settlement_pending"))
        if(pending)known<-FALSE
        unscoped<-any(ledger$included & ledger$bonus_type%in%kind & (is.na(ledger$round_id)|!nzchar(ledger$round_id)))
        bonus_rounds[[length(bonus_rounds)+1L]]<-data.frame(round_id=rid,
          round_number=history$rounds$round_number[match(rid,history$rounds$round_id)],bonus_type=kind,
          amount=if(known&&!unscoped&&all(is.finite(entries$amount)))sum(entries$amount)+adjustment else NA_real_)
      }
    }
    attr(ledger,"round_bonus_totals")<-dplyr::bind_rows(bonus_rounds)
    ledgers[[tid]]<-ledger
    if(!is.null(cache)){
      cache$teams[[tid]]<-list(key=team_key,result=result[[i]],breakdown=breakdowns[[tid]],ledger=ledger,missing=missing,assumptions=assumptions,estimated_movements=any(ledger$included & ledger$estimated))
      cache$team_builds<-(cache$team_builds %||% 0L)+1L
    }
  }
  if(!is.null(cache))cache$teams<-cache$teams[intersect(names(cache$teams),as.character(teams$teamid))]
  list(reference_payments=reference_payments,point_reconciliation=point_checks,
    pending_bonus_rounds=rival_pending_settlement_rounds(evidence,catalog,all_movements,now),
    team_finances=dplyr::bind_rows(result),breakdowns=breakdowns,cash_ledgers=ledgers,pressroom=pressroom,all_purchases=data.frame())
}

get_rival_finance_seed <- function(login,championship_id,user_team_id) {
  info<-tryCatch(get_user_team_info(login,championship_id,user_team_id),error=function(e)NULL)
  rules<-normalize_league_rules(info,championship_id=championship_id)
  # Release team reads after configuration, while transfer/notification history
  # continues through the same asynchronous adapter. No additional requests.
  if(exists("publish_resource_progress",mode="function"))
    publish_resource_progress(list(info=info,rules=rules))
  list(info=info,rules=rules,
    pressroom=tryCatch(get_championship_pressroom(login,championship_id),error=function(e)structure(data.frame(),fetch_status="unavailable")),
    notifications=tryCatch(get_notifications(login),error=function(e)NULL))
}
# Official client: fetchPreviousNotifications(oldest._id) -> list {to: id}.
# Read one page per async task so historical notifications cannot block the seed.
get_rival_round_notification_page <- function(login,championship_id,user_team_id,cursor) {
  response<-notification_request(login,"list",list(to=as.character(cursor)))
  if(!identical(response$status,"ok")||!is.list(response$data$items))
    return(list(status="unavailable",notifications=data.frame(),cursor=as.character(cursor)))
  items<-response$data$items
  if(length(items)>1000L)return(list(status="unavailable",notifications=data.frame(),cursor=as.character(cursor)))
  rows<-normalize_notifications(items)
  if(length(items)&&(!nrow(rows)||nrow(rows)!=length(items)||any(!nzchar(rows$id))))
    return(list(status="unavailable",notifications=data.frame(),cursor=as.character(cursor)))
  next_cursor<-if(nrow(rows))tail(rows$id,1L)else as.character(cursor)
  list(status=if(!nrow(rows))"empty"else if(identical(next_cursor,as.character(cursor)))"repeated"else"ok",
    notifications=rows,cursor=next_cursor,observed_at=rival_finance_stamp())
}
rival_notification_evidence <- function(rows,account_id,championship_id,season,observed_at) {
  if(!is.data.frame(rows)||!all(c("id","championship_id")%in%names(rows))||length(observed_at)!=1L||is.na(fm_time(observed_at)))return(list())
  rows<-rows[rows$championship_id%in%championship_id,,drop=FALSE]
  lapply(seq_len(nrow(rows)),function(i)rival_finance_evidence(account_id,championship_id,season,"notifications",rows$id[i],as.list(rows[i,,drop=FALSE]),observed_at))
}
get_rival_team_finance_observation <- function(login,championship_id,user_team_id) {
  list(team_id=as.character(user_team_id),info=tryCatch(get_user_team_info(login,championship_id,user_team_id),error=function(e)NULL),
    movements=tryCatch(get_user_team_moneymovements(login,championship_id,user_team_id),error=function(e)NULL),
    rounds=tryCatch(get_user_team_rounds(login,championship_id,user_team_id),error=function(e)NULL))
}
rival_team_evidence <- function(result,account_id,championship_id,season) {
  id<-result$team_id;info<-result$info;mov<-result$movements;rows<-list()
  if(is.list(info)&&!identical(attr(info,"fetch_status"),"stale")){
    stamp<-attr(info,"observed_at")
    if(!is.null(stamp)&&!is.na(fm_time(stamp))){
      payload<-list(budget=fm_number(info$budget),teamValue=fm_number(info$teamValue),points=fm_number(info$points),observed_at=rival_finance_stamp(stamp))
      rows<-c(rows,list(rival_finance_evidence(account_id,championship_id,season,"team",id,payload,stamp)))
      if(is.list(info$configuration))rows<-c(rows,list(rival_finance_evidence(account_id,championship_id,season,"rules",id,list(configuration=info$configuration,historical=FALSE),stamp)))
    }
  }
  stamp<-attr(mov,"observed_at")
  if(is.data.frame(mov)&&!identical(attr(mov,"fetch_status"),"stale")&&!is.null(stamp)&&!is.na(fm_time(stamp))){
    complete<-isTRUE(attr(mov,"complete"))
    if(nrow(mov)>5000L){mov<-head(mov,5000L);complete<-FALSE}
    rows<-c(rows,lapply(seq_len(nrow(mov)),function(i){
      if(!nzchar(mvp_text(mov$id[i])))return(NULL)
      p<-as.list(mov[i,,drop=FALSE]);p$team_id<-id
      rival_finance_evidence(account_id,championship_id,season,"movements",paste(id,p$id,sep=":"),p,stamp)
    }))
    rows<-c(rows,list(rival_finance_evidence(account_id,championship_id,season,"coverage",id,
      list(complete=complete,history_complete=FALSE,scope="returned movement list; historical reset/season coverage unverified",observed_at=rival_finance_stamp(stamp)),stamp)))
  }
  history<-result$rounds
  if(is.list(history)&&length(history)<=120L&&!isTRUE(history$error)&&is.null(history$code)&&!identical(attr(history,"fetch_status"),"stale")){
    parsed<-rival_team_round_rows(history,id)
    valid<-if(length(history))nrow(parsed)==length(history)else isTRUE(attr(history,"valid_response"))
    if(valid)rows<-c(rows,list(rival_finance_evidence(account_id,championship_id,season,"team_rounds",id,
      list(team_id=id,rounds=history),attr(history,"observed_at") %||% Sys.time())))
  }
  Filter(Negate(is.null),rows)
}
# A team directory does not depend on financial evidence being available.
rival_finance_directory <- function(teams) {
  if(!is.data.frame(teams)||!all(c("teamid","teamname")%in%names(teams)))return(data.frame())
  teams<-teams[!is.na(teams$teamid)&nzchar(as.character(teams$teamid)),,drop=FALSE]
  teams<-teams[!duplicated(teams$teamid),,drop=FALSE]
  if(!nrow(teams))return(data.frame())
  rows<-data.frame(teamid=as.character(teams$teamid),teamname=as.character(teams$teamname),
    confidence="Unavailable",as_of="",cash_status="Unavailable",estimate_notes="Financial details are not available yet.",stringsAsFactors=FALSE)
  for(field in c("estimated_cash","reconciliation_difference","team_value","points"))rows[[field]]<-NA_real_
  for(field in intersect(c("team_value","points"),names(teams)))rows[[field]]<-suppressWarnings(as.numeric(teams[[field]]))
  if("teamValue"%in%names(teams))rows$team_value<-suppressWarnings(as.numeric(teams$teamValue))
  rows$last_access<-as.character(teams$lastAccess %||% teams$last_access %||% rep(NA_character_,nrow(teams)))
  rows
}
# One shared root-session ledger, rendered in bounded batches rather than per write.
shared_rival_finance_source <- function(session,login,championship,user_team,teams,active,official,refresh=NULL,on_update=NULL) {
  root<-session$rootScope()
  if(is.null(root$userData$rival_finance_shared)){
    state<-new.env(parent=emptyenv());state$transfers<-shiny::reactiveVal(data.frame());state$value<-shiny::reactiveVal(list(team_finances=data.frame(),breakdowns=list(),loading=TRUE));state$key<-NULL
    state$calculation_cache<-rival_calculation_cache();state$snapshot<-NULL;state$snapshot_status<-"not_saved"
    state$generation<-0L;state$closed<-FALSE;state$queue<-list();state$busy<-0L;state$lineup_pending<-0L;state$archive_lineups<-list();state$evidence<-list();state$stored_movements<-list();state$seed<-list();state$defaults<-NULL;state$last_publish<-0
    state$notification_cursor<-"";state$notification_status<-"initial";state$scheduled_notification_cursors<-character();state$notification_pending<-FALSE
    state$scheduled_owners<-character();state$ownership_pending<-0L;state$ownership_catalog_scheduled<-FALSE;state$ownership_unsaved<-list();state$ownership_write_attempts<-character()
    state$publish_timer<-NULL;state$teams<-data.frame();state$teams_status<-"loading";state$teams_reason<-""
    state$awards<-list(rows=data.frame(),complete=FALSE);state$finance_ready<-FALSE;state$seed_ready<-FALSE;state$seed_complete<-FALSE;state$scheduled_teams<-character();state$scheduled_lineups<-character();state$scheduled_rankings<-character();state$pending_team_ids<-character()
    root$onSessionEnded(function(){state$closed<-TRUE;state$generation<-state$generation+1L;if(!is.null(state$publish_timer))state$publish_timer()})
    root$userData$rival_finance_shared<-state
  }
  state<-root$userData$rival_finance_shared
  snapshot_status<-function(calculated){
    calculated$calculation_snapshot_status<-state$snapshot_status
    calculated$calculation_snapshot_checking<-!is.null(state$snapshot)&&(!isTRUE(state$seed_complete)||length(state$pending_team_ids)>0L)
    calculated$reason<-paste(Filter(nzchar,c(state$reason %||% "",
      if(calculated$calculation_snapshot_checking)"Showing saved calculations while checking for updates."else "",
      if(identical(state$snapshot_status,"unavailable"))"Calculations could not be saved for next time; refresh retries saving."else "")),collapse=" ")
    calculated
  }
  publish<-function(force=FALSE){
    if(state$closed||is.null(state$key))return(invisible(NULL))
    elapsed<-as.numeric(Sys.time())-state$last_publish
    if(!force&&elapsed<2){
      if(is.null(state$publish_timer)){
        generation<-state$generation
        state$publish_timer<-later::later(function(){
          state$publish_timer<-NULL
          if(!state$closed&&generation==state$generation&&isTRUE(shiny::isolate(active())))publish(TRUE)
        },2-elapsed)
      }
      return(invisible(NULL))
    }
    if(!is.null(state$publish_timer)){state$publish_timer();state$publish_timer<-NULL}
    calculated<-list(team_finances=rival_finance_directory(state$teams),breakdowns=list(),all_purchases=data.frame())
    snapshot_preview<-!is.null(state$snapshot)&&(!isTRUE(state$seed_complete)||length(state$pending_team_ids)>0L)
    preview<-NULL
    if(snapshot_preview){
      rows<-state$snapshot$team_finances
      if(nrow(state$teams)){
        rows<-rows[rows$teamid%in%state$teams$teamid,,drop=FALSE]
        missing<-state$teams[!state$teams$teamid%in%rows$teamid,,drop=FALSE]
        rows<-dplyr::bind_rows(rows,rival_finance_directory(missing))
      }
      calculated$pending_bonus_rounds<-state$snapshot$pending_bonus_rounds %||% numeric()
      calculated$cash_ledgers<-state$snapshot$cash_ledgers %||% lapply(state$snapshot$cache$teams,`[[`,"ledger")
      calculated$cash_ledgers<-calculated$cash_ledgers[intersect(names(calculated$cash_ledgers),as.character(rows$teamid))]
      calculated$team_finances<-dplyr::bind_rows(lapply(seq_len(nrow(rows)),function(i){
        team<-state$teams[state$teams$teamid%in%rows$teamid[i],,drop=FALSE]
        rival_refresh_finance_row(rows[i,,drop=FALSE],team)
      }))
    }
    if(snapshot_preview)preview<-calculated
    inputs<-list(teams=state$teams,evidence=state$evidence,awards=state$awards,
      seed=state$seed,defaults=state$defaults,account=state$account,league=state$championship,season=state$season,
      reference=shiny::isolate(user_team()),fresh=vapply(rival_evidence_rows(state$evidence,"team"),function(x){
        if(!is.list(x$payload))return(FALSE)
        stamp<-fm_time(x$payload$observed_at);age<-as.numeric(difftime(Sys.time(),stamp,units="secs"));is.finite(age)&&age>=-60&&age<=300
      },logical(1)))
    # Owner responses arrive independently. Keep the last complete calculation
    # usable while their evidence accumulates, then rebuild once at batch drain.
    # Do not advance calculation_inputs for a retained value: the final response
    # must still invalidate it and calculate from every accumulated observation.
    retain_ownership_batch<-state$finance_ready&&state$ownership_pending>0L&&!is.null(state$calculated)
    # A historical lineup batch contributes one shared accounting preparation.
    # Keep the existing labeled result through the batch, then rebuild at drain.
    # Team reads must finish first so newly observed balances are not held back.
    retain_lineup_batch<-state$finance_ready&&state$lineup_pending>0L&&!length(state$pending_team_ids)&&!is.null(state$calculated)
    retain_history_batch<-retain_ownership_batch||retain_lineup_batch
    reuse<-retain_history_batch||(state$finance_ready&&!is.null(state$calculated)&&identical(inputs,state$calculation_inputs))
    if(reuse)calculated<-state$calculated
    if(state$finance_ready&&!snapshot_preview&&!reuse)calculated<-tryCatch(build_rival_cash_dataset(state$teams,state$championship,state$evidence,state$awards$rows,
      state$seed$pressroom %||% data.frame(),state$seed$rules %||% list(),state$defaults,
      coverage_complete=isTRUE(state$awards$complete),account_id=state$account,season=state$season,reference_team_id=shiny::isolate(user_team()),cache=state$calculation_cache),error=function(error){
        if(is.null(state$build_failure))state$build_failure<-resource_log_failure(
          resource_failure(error,resource="league_finances_calc",category="schema"))
        calculated$finance_error<-paste("Financial details could not be prepared.",resource_failure_message(state$build_failure))
        calculated
      })
    if(is.null(calculated$finance_error)){state$build_failure<-NULL;if(state$finance_ready&&!snapshot_preview&&!retain_history_batch){state$calculated<-calculated;state$calculation_inputs<-inputs}}
    # The snapshot is a labelled display preview, never fresh accounting input.
    # Keep it through history hydration and allow either request ordering.
    if(!is.null(preview)){
      calculated$team_finances<-preview$team_finances
      calculated$cash_ledgers<-preview$cash_ledgers
      calculated$pending_bonus_rounds<-preview$pending_bonus_rounds
    }
    calculated$finance_loading<-state$busy>0L||length(state$queue)>0L
    calculated$finance_pending<-state$busy+length(state$queue)
    calculated$loading<-calculated$finance_loading||identical(state$teams_status,"loading")
    calculated$teams_status<-state$teams_status;calculated$teams_reason<-state$teams_reason
    calculated$reason<-state$reason %||% ""
    calculated<-snapshot_status(calculated)
    state$last_publish<-as.numeric(Sys.time());state$transfers(calculated$pressroom %||% data.frame());state$value(calculated)
    if(is.function(on_update))on_update(calculated)
    cache_signature<-rival_snapshot_signature(state$calculation_cache)
    if(state$finance_ready&&!calculated$loading&&is.null(calculated$finance_error)&&!identical(cache_signature,state$shared_cache_signature)){
      # Reuse in this process immediately; durability is acknowledged separately.
      ctx<-resource_context(account_id=state$account,championship_id=state$championship,
        user_team_id=shiny::isolate(user_team()),season=state$season)
      resource<-paste0("rival_calculation_",rival_calculation_version)
      resource_store(api_cache_key(resource,context=ctx),as.list(state$calculation_cache),ctx,resource)
      state$shared_cache_signature<-cache_signature
    }
    if(state$finance_ready&&!calculated$loading&&is.null(calculated$finance_error)&&length(state$calculation_cache$teams)&&
       !identical(cache_signature,state$snapshot_signature)&&!identical(cache_signature,state$snapshot_attempted_signature)){
      ctx<-resource_context(account_id=state$account,championship_id=state$championship,user_team_id=shiny::isolate(user_team()),season=state$season,session_id=root$token)
      generation<-state$generation;state$snapshot_pending<-cache_signature;state$snapshot_attempted_signature<-cache_signature;state$snapshot_status<-"pending"
      persist_rival_calculation_snapshot(state$calculation_cache,calculated,ctx,function(ok){
        if(state$closed||!identical(generation,state$generation)||!identical(state$season,ctx$season)||
           !identical(state$snapshot_pending,cache_signature))return(invisible(NULL))
        state$snapshot_pending<-NULL
        if(isTRUE(ok)){state$snapshot_signature<-cache_signature;state$snapshot_status<-"saved"}else state$snapshot_status<-"unavailable"
        current<-snapshot_status(shiny::isolate(state$value()));state$value(current)
      })
      current<-snapshot_status(shiny::isolate(state$value()));state$value(current)
    }
  }
  hydrate_snapshot<-function(auth,cid,tid){
    generation<-state$generation
    ctx<-resource_context(auth,cid,tid,season=state$season,session_id=root$token)
    # One bounded latest-row request runs alongside saved evidence acquisition.
    # It can paint the table before paginated history or calculations finish.
    request<-tryCatch(async_adapter_fetch("read_rival_calculation_snapshot",list(account_id=state$account,
      championship_id=cid,season=state$season,user_team_id=tid),ctx,
      is_current=function()!state$closed&&identical(generation,state$generation)),error=function(e)NULL)
    if(is.null(request))return(invisible(NULL))
    promises::then(request,function(snapshot){
      if(state$closed||!identical(generation,state$generation)||!identical(state$season,ctx$season)||(isTRUE(state$seed_complete)&&!length(state$pending_team_ids))||is.null(snapshot))return(NULL)
      if(!identical(snapshot$scope,rival_snapshot_scope(ctx)))return(NULL)
      state$snapshot<-snapshot;state$snapshot_signature<-if(isTRUE(snapshot$ledger_complete))snapshot$signature else NULL;state$snapshot_status<-if(isTRUE(snapshot$ledger_complete))"saved"else"not_saved"
      if(is.list(snapshot$cache)){list2env(snapshot$cache,envir=state$calculation_cache);state$calculation_cache$durable_teams<-snapshot$cache$teams}
      publish(TRUE);NULL
    },function(error)NULL)
    invisible(NULL)
  }
  enqueue_teams<-function(){
    if(!state$seed_ready||!is.data.frame(state$teams))return(invisible(NULL))
    ids<-head(unique(as.character(state$teams$teamid %||% character())),100L)
    ids<-setdiff(ids[!is.na(ids)&nzchar(ids)],state$scheduled_teams)
    state$scheduled_teams<-c(state$scheduled_teams,ids)
    state$pending_team_ids<-unique(c(state$pending_team_ids,ids))
    state$queue<-c(state$queue,lapply(ids,function(id)list(kind="team",id=id)))
    invisible(NULL)
  }
  enqueue_point_repair<-function(){
    if(!state$seed_ready||length(state$pending_team_ids)||isTRUE(state$point_repair_done))return(invisible(NULL))
    state$point_repair_done<-TRUE
    rounds<-rival_saved_rounds(state$evidence)
    resolved<-rival_historical_components(state$evidence,state$teams,state$seed$rules %||% list(),state$awards$rows)$rounds
    rounds<-dplyr::bind_rows(resolved,rounds[!paste(rounds$team_id,rounds$round_id)%in%paste(resolved$team_id,resolved$round_id),,drop=FALSE])
    catalog<-unique(rounds[,c("round_id","round_number"),drop=FALSE])
    reference<-shiny::isolate(user_team())
    mov<-rival_normalize_movements(rival_cash_latest_movements(state$evidence,reference),catalog)
    checks<-rival_point_reconciliation(rounds,mov,state$seed$rules %||% list(),reference)
    bad<-checks$round_id[checks$status!="validated"]
    ids<-if(length(bad)||(!nrow(checks)&&is.finite(fm_number(state$seed$rules$money_per_point))&&fm_number(state$seed$rules$money_per_point)>0))reference else character()
    valid<-checks$round_id[checks$status=="validated"|is.finite(checks$recorded)]
    for(id in state$teams$teamid){
      rows<-rounds[rounds$team_id%in%id & rounds$round_id%in%valid,,drop=FALSE]
      if(length(setdiff(valid,rows$round_id))||anyNA(rows$negative)||any(!is.finite(rows$points))||any(!vapply(seq_len(nrow(rows)),function(i)round_is_finished(as.list(rows[i,,drop=FALSE])),logical(1))))ids<-c(ids,id)
    }
    ids<-unique(ids)
    auth<-shiny::isolate(login());cid<-shiny::isolate(championship())
    for(id in ids)invalidate_resources(resource_context(auth,cid,id),pattern=if(id==reference)"^(rounds_|moneymovements_|team_info_)"else"^(rounds_|moneymovements_)")
    state$pending_team_ids<-unique(c(state$pending_team_ids,ids))
    state$queue<-c(lapply(ids,function(id)list(kind="team",id=id)),state$queue)
    for(i in which(rounds$round_id%in%valid & is.na(rounds$negative))){
      row<-rounds[i,,drop=FALSE];signature<-rival_round_signature(row,state$awards$rows)
      key<-paste(row$team_id,row$round_id,signature,sep=":")
      if(key%in%state$scheduled_lineups||length(state$scheduled_lineups)>=600L)next
      state$scheduled_lineups<-c(state$scheduled_lineups,key)
      state$lineup_pending<-state$lineup_pending+1L
      state$queue<-c(state$queue,list(list(kind="lineup",id=row$team_id,round=row$round_id,signature=signature)))
    }
  }
  enqueue_rankings<-function(){
    # Complete the ordinary team reads first; otherwise an in-flight team looks
    # like a history gap and creates unnecessary historical ranking requests.
    if(!state$seed_ready||length(state$pending_team_ids)>0L)return(invisible(NULL))
    source<-rival_saved_rounds(state$evidence,include_ranking=FALSE)
    saved<-rival_saved_rankings(state$evidence,state$teams$teamid,source)
    participants<-rival_ranking_participants(state$evidence,state$teams$teamid,state$seed$rules %||% list(),source)
    closed<-if(nrow(source))vapply(seq_len(nrow(source)),function(i)round_is_finished(as.list(source[i,,drop=FALSE])),logical(1))else logical()
    for(id in unique(source$round_id[closed])){
      rows<-source[source$round_id%in%id,,drop=FALSE]
      complete<-rival_ranking_round_complete(rows,participants$team_ids)
      if(complete||any(vapply(saved,function(record)identical(mvp_text(record$payload$round_id),id),logical(1))))next
      number<-unique(rows$round_number[is.finite(rows$round_number)&rows$round_number>=1&rows$round_number==floor(rows$round_number)])
      if(length(number)!=1L)next
      signature<-rival_ranking_signature(source,id,state$teams$teamid);key<-paste(id,signature,sep=":")
      if(key%in%state$scheduled_rankings)next
      if(length(state$scheduled_rankings)>=120L){state$reason<-"Historical ranking batch limit reached. Use Refresh to continue.";break}
      state$scheduled_rankings<-c(state$scheduled_rankings,key)
      state$queue<-c(state$queue,list(list(kind="ranking",id=id,number=number,signature=signature,source_teams=as.character(state$teams$teamid))))
    }
    invisible(NULL)
  }
  enqueue_notifications<-function(){
    if(!state$seed_ready||state$notification_pending||length(state$pending_team_ids)>0L)return(invisible(NULL))
    rewards<-vapply(c("money_per_point","ranking_pool","dreamteam_reward","mvp_reward"),function(field)fm_number(state$seed$rules[[field]]),numeric(1),USE.NAMES=TRUE)
    if(!any(is.finite(rewards)&rewards>0))return(invisible(NULL))
    rounds<-rival_saved_rounds(state$evidence,teams=state$teams$teamid)
    closed<-if(nrow(rounds))vapply(seq_len(nrow(rounds)),function(i)round_is_finished(as.list(rounds[i,,drop=FALSE])),logical(1))else logical()
    catalog<-unique(dplyr::bind_rows(rounds[closed,c("round_id","round_number"),drop=FALSE],
      if(is.data.frame(state$awards$rows)&&all(c("round_id","round_number","is_finished")%in%names(state$awards$rows)))
        state$awards$rows[state$awards$rows$is_finished%in%TRUE,c("round_id","round_number"),drop=FALSE]else data.frame()))
    publications<-rival_round_publication_dates(state$evidence,catalog)
    missing<-setdiff(catalog$round_id,publications$round_id)
    # Points may be credited in phases, so another team's position payment can
    # date position/XI/MVP estimates only, never a per-point reward.
    if(!is.finite(rewards[["money_per_point"]])||rewards[["money_per_point"]]<=0){
      movements<-dplyr::bind_rows(lapply(unique(vapply(rival_evidence_rows(state$evidence,"movements"),function(record)mvp_text(record$payload$team_id),character(1))),function(id)
        rival_normalize_movements(rival_cash_latest_movements(state$evidence,id),catalog)))
      anchors<-rival_payment_anchors(movements)
      dated<-vapply(missing,function(id)nzchar(rival_reward_date(id,anchors)),logical(1))
      missing<-missing[!dated]
    }
    if(!length(missing)||!nzchar(state$notification_cursor)||!state$notification_status%in%c("initial","ok"))return(invisible(NULL))
    scans<-Filter(function(record)identical(mvp_text(record$source_id),"round-publication-scan"),rival_evidence_rows(state$evidence,"notifications"))
    if(length(scans)){
      scan<-tail(scans,1)[[1]]$payload
      if(!length(setdiff(missing,unlist(scan$missing_rounds,use.names=FALSE)))){
        if(mvp_text(scan$status)%in%c("empty","repeated"))return(invisible(NULL))
        if(!length(state$scheduled_notification_cursors)&&identical(mvp_text(scan$status),"limit")&&nzchar(mvp_text(scan$cursor)))state$notification_cursor<-mvp_text(scan$cursor)
      }
    }
    cursor<-state$notification_cursor
    if(cursor%in%state$scheduled_notification_cursors||length(state$scheduled_notification_cursors)>=20L)return(invisible(NULL))
    state$scheduled_notification_cursors<-c(state$scheduled_notification_cursors,cursor);state$notification_pending<-TRUE
    state$queue<-c(state$queue,list(list(kind="notifications",cursor=cursor,missing=missing)))
    invisible(NULL)
  }
  persist_ownership<-function(rows=list(),context=NULL){
    for(row in rows)state$ownership_unsaved[[row$source_id]]<-row
    if(is.null(context)||!length(state$ownership_unsaved))return(invisible(NULL))
    candidates<-Filter(function(row)!paste(row$source_id,row$observed_at)%in%state$ownership_write_attempts,state$ownership_unsaved)
    if(!length(candidates))return(invisible(NULL))
    groups<-split(seq_along(candidates),ceiling(seq_along(candidates)/50L))
    for(indices in groups)local({
      batch<-unname(candidates[indices]);generation<-state$generation
      keys<-vapply(batch,function(row)paste(row$source_id,row$observed_at),character(1))
      state$ownership_write_attempts<-c(state$ownership_write_attempts,keys)
      callback<-function(ok){
        if(state$closed||generation!=state$generation)return(invisible(NULL))
        if(isTRUE(ok)){
          for(row in batch)if(identical(state$ownership_unsaved[[row$source_id]],row))state$ownership_unsaved[[row$source_id]]<-NULL
          if(!length(state$ownership_unsaved)&&identical(state$reason,"Historical ownership was retrieved but could not be saved; Refresh retries saving."))state$reason<-""
        }else state$reason<-"Historical ownership was retrieved but could not be saved; Refresh retries saving."
        publish(FALSE);invisible(NULL)
      }
      queued<-defer_persistence("supabase_post_direct",list("rival_finance_evidence",batch,
        "account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at",TRUE),context=context,on_complete=callback)
      if(!isTRUE(queued))callback(FALSE)
    })
    invisible(NULL)
  }
  enqueue_ownership<-function(){
    # Finish ordinary source reads first so an in-flight lineup is not a gap.
    if(!state$seed_ready||length(state$pending_team_ids)||state$busy>0L||length(state$queue))return(invisible(NULL))
    awards<-state$awards$rows
    if(!is.data.frame(awards)||!nrow(awards)||!all(c("player_id","round_id","is_finished")%in%names(awards)))return(invisible(NULL))
    awards<-awards[awards$is_finished%in%TRUE,,drop=FALSE]
    if(!nrow(awards))return(invisible(NULL))
    history<-rival_historical_components(state$evidence,state$teams,state$seed$rules %||% list(),awards)
    explicit<-dplyr::bind_rows(lapply(rival_evidence_rows(state$evidence,"eligibility"),function(row){
      p<-row$payload
      if(!is.list(p)||!isTRUE(p$verified)||!all(c("round_id","player_id")%in%names(p)))return(NULL)
      data.frame(round_id=mvp_text(p$round_id),player_id=mvp_text(p$player_id))
    }))
    verified<-dplyr::bind_rows(history$eligibility[,c("round_id","player_id"),drop=FALSE],explicit)
    missing<-awards[!paste(awards$round_id,awards$player_id)%in%paste(verified$round_id,verified$player_id),,drop=FALSE]
    if(!nrow(missing))return(invisible(NULL))
    # Reuse returned historical transitions indefinitely for their observed interval;
    # a later round outside that interval needs a new observation.
    saved<-rival_ownership_evidence_rows(state$evidence,"player_ownership_history")
    catalog<-unique(history$rounds[,c("round_id","round_number"),drop=FALSE])
    movements<-dplyr::bind_rows(lapply(unique(vapply(rival_evidence_rows(state$evidence,"movements"),function(row)mvp_text(row$payload$team_id),character(1))),function(id)
      rival_normalize_movements(rival_cash_latest_movements(state$evidence,id),catalog)))
    intervals<-rival_ownership_round_intervals(state$evidence,catalog,movements)
    if(!state$ownership_catalog_scheduled&&!all(unique(missing$round_id)%in%intervals$round_id)){
      state$ownership_catalog_scheduled<-TRUE
      state$queue<-c(state$queue,list(list(kind="ownership_catalog")))
      return(invisible(NULL))
    }
    usable<-missing$round_id%in%intervals$round_id
    if(any(!usable))state$reason<-"Some historical ownership intervals lack an official round start or final-category settlement; affected bonuses remain unknown."
    missing<-missing[usable,,drop=FALSE]
    if(!nrow(missing))return(invisible(NULL))
    for(id in unique(as.character(missing$player_id))){
      required<-missing$round_id[missing$player_id%in%id]
      interval<-intervals[intervals$round_id%in%required,,drop=FALSE]
      records<-Filter(function(row)identical(mvp_text(row$payload$player_id),id),saved)
      reusable<-length(records)>0L&&length(required)>0L&&all(required%in%interval$round_id)&&any(vapply(records,function(row){
        rows<-tryCatch(dplyr::bind_rows(mvp_records(row$payload$rows)),error=function(e)NULL)
        # Unknown or ambiguous names remain unknown; their source need not be
        # reacquired repeatedly when its dated transitions already cover this interval.
        if(!is.data.frame(rows)||!all(c("changed_at","owner_name")%in%names(rows))||!nrow(rows))return(FALSE)
        dates<-fm_time(rows$changed_at);stamp<-fm_time(row$observed_at)
        !anyNA(dates)&&length(stamp)==1L&&!is.na(stamp)&&min(dates)<=min(fm_time(interval$begins_at))&&stamp>=max(fm_time(interval$settles_at))
      },logical(1)))
      if(reusable||id%in%state$scheduled_owners)next
      if(length(state$scheduled_owners)>=120L){state$reason<-"Historical ownership batch limit reached; Refresh continues acquisition.";break}
      state$scheduled_owners<-c(state$scheduled_owners,id)
      state$ownership_pending<-state$ownership_pending+1L
      state$queue<-c(state$queue,list(list(kind="ownership",id=id)))
    }
    invisible(NULL)
  }
  enqueue_lineups<-function(){
    if(!state$seed_ready||length(state$pending_team_ids))return(invisible(NULL))
    rounds<-rival_saved_rounds(state$evidence,teams=state$teams$teamid)
    awards<-state$awards$rows
    if(!nrow(rounds)||!is.data.frame(awards)||!nrow(awards)||!"round_id"%in%names(awards))return(invisible(NULL))
    ctx<-resource_context(account_id=state$account,championship_id=state$championship,season=state$season)
    # Ranking supplements alone cannot establish an archived record's season.
    resolved<-rival_resolve_archived_lineups(state$archive_lineups,ctx,
      rival_saved_rounds(state$evidence,include_ranking=FALSE),awards,state$evidence)
    if(length(resolved)){
      state$evidence<-rival_merge_evidence(state$evidence,resolved)
      persist_rival_finance_evidence(resolved,ctx)
      key<-api_cache_key(paste0('rival_finance_evidence_',state$account,'_',state$championship,'_',state$season),context=ctx)
      if(exists(key,envir=api_cache_env,inherits=FALSE))rm(list=key,envir=api_cache_env)
    }
    snapshots<-rival_evidence_rows(state$evidence,"round_lineup")
    for(i in seq_len(nrow(rounds))){
      row<-rounds[i,,drop=FALSE]
      if(!row$team_id%in%state$teams$teamid||!row$round_id%in%awards$round_id||isTRUE(row$negative)||!round_is_finished(as.list(row)))next
      signature<-rival_round_signature(row,awards);identity<-paste(row$team_id,row$round_id,sep=":")
      key<-paste(identity,signature,sep=":")
      saved<-Filter(function(record)identical(mvp_text(record$source_id),paste0("lineup:",identity))&&
        !mvp_text(record$payload$status)%in%c("running","next")&&identical(mvp_text(record$payload$signature),mvp_text(signature)),snapshots)
      if(length(saved)||key%in%state$scheduled_lineups)next
      if(length(state$scheduled_lineups)>=600L){state$reason<-"Historical lineup batch limit reached. Use Refresh to continue after the current batch.";break}
      state$scheduled_lineups<-c(state$scheduled_lineups,key)
      state$lineup_pending<-state$lineup_pending+1L
      state$queue<-c(state$queue,list(list(kind="lineup",id=row$team_id,round=row$round_id,signature=signature)))
    }
    invisible(NULL)
  }
  run_next<-function(){
    if(state$closed||!isTRUE(shiny::isolate(active())))return(invisible(NULL))
    auth<-shiny::isolate(login());cid<-shiny::isolate(championship());tid<-shiny::isolate(user_team());if(!valid_login(auth)||is.null(cid))return(invisible(NULL))
    generation<-state$generation;ctx<-resource_context(auth,cid,tid,season=state$season,session_id=root$token)
    while(state$busy<2L&&length(state$queue)){
      task<-state$queue[[1]];state$queue<-state$queue[-1];state$busy<-state$busy+1L
      local({task<-task
        fn<-switch(task$kind,saved="get_rival_finance_saved",seed="get_rival_finance_seed",team="get_rival_team_finance_observation",lineup="get_rival_round_lineup_observation",ranking="get_rival_round_ranking_observation",notifications="get_rival_round_notification_page",ownership="get_rival_player_ownership_observation",ownership_catalog="get_rival_ownership_round_catalog")
        args<-if(task$kind=="saved")list(account_id=auth[["userid"]],championship_id=cid,season=state$season)else list(login=auth,championship_id=cid,user_team_id=if(task$kind%in%c("team","lineup"))task$id else tid)
        if(task$kind=="ownership")args<-c(args,list(player_id=task$id))
        if(task$kind=="notifications")args<-c(args,list(cursor=task$cursor))
        if(task$kind=="lineup")args<-c(args,list(round_id=task$round,signature=task$signature))
        if(task$kind=="ranking")args<-c(args,list(round_id=task$id,round_number=task$number,signature=task$signature,source_teams=task$source_teams))
        progress<-if(task$kind=="seed")function(result){
          if(state$closed||generation!=state$generation||state$seed_ready)return(invisible(NULL))
          inferred<-result$rules$season %||% "unknown"
          if(!identical(inferred,state$season)&&inferred!="unknown")return(invisible(NULL))
          if(!is.null(result$info))state$seed$rules<-result$rules
          state$seed_ready<-TRUE
          enqueue_teams();publish(TRUE);run_next()
        }else NULL
        promises::then(async_adapter_fetch(fn,args,ctx,is_current=function()!state$closed&&identical(generation,state$generation),on_progress=progress),function(result){
          if(state$closed||generation!=state$generation)return(NULL)
          state$busy<-state$busy-1L
          if(task$kind=="lineup")state$lineup_pending<-max(0L,state$lineup_pending-1L)
          if(task$kind=="ownership")state$ownership_pending<-max(0L,state$ownership_pending-1L)
          if(task$kind=="team")state$pending_team_ids<-setdiff(state$pending_team_ids,task$id)
          if(task$kind=="notifications")state$notification_pending<-FALSE
          tryCatch({
          if(task$kind=="saved"){
            state$archive_lineups<-result$archive_lineups %||% list()
            state$stored_movements<-rival_evidence_rows(result$evidence,"movements")
            state$evidence<-rival_merge_evidence(state$evidence,result$evidence);state$defaults<-result$defaults;state$finance_ready<-TRUE
            saved_rules<-rival_evidence_rows(state$evidence,"rules")
            if(length(saved_rules)){
              latest<-saved_rules[[which.max(vapply(saved_rules,function(row)as.numeric(fm_time(row$observed_at)),numeric(1)))]]
              state$seed$rules<-normalize_league_rules(list(configuration=latest$payload$configuration),championship_id=cid)
            }
            if(!is.data.frame(state$seed$pressroom)||!nrow(state$seed$pressroom))state$seed$pressroom<-result$pressroom %||% data.frame()
            # Saved data is immediately usable; current sources refresh separately.
            state$queue<-c(list(list(kind="seed")),state$queue);publish(TRUE)
          }else if(task$kind=="seed"){
            if(is.null(result$info))result$rules<-state$seed$rules %||% result$rules
            live_transfer_status<-mvp_text(attr(result$pressroom,"fetch_status"))
            live_transfer_available<-is.data.frame(result$pressroom)&&all(c("id","price","buyer_team_id","seller_team_id")%in%names(result$pressroom))
            old_transfers<-state$seed$pressroom %||% data.frame()
            if((!live_transfer_available||!identical(live_transfer_status,"ok"))&&is.data.frame(old_transfers)&&nrow(old_transfers)){
              saved_locker<-if("locker_event_id"%in%names(old_transfers))old_transfers[!is.na(old_transfers$locker_event_id)&nzchar(old_transfers$locker_event_id),,drop=FALSE]else data.frame()
              if(nrow(saved_locker)){
                if("locker_event_id"%in%names(result$pressroom))saved_locker<-saved_locker[!saved_locker$locker_event_id%in%result$pressroom$locker_event_id,,drop=FALSE]
                result$pressroom<-merge_locker_transfers(result$pressroom,saved_locker)
              }
              other<-if("locker_event_id"%in%names(old_transfers))old_transfers[is.na(old_transfers$locker_event_id)|!nzchar(old_transfers$locker_event_id),,drop=FALSE]else old_transfers
              combined<-dplyr::bind_rows(other,result$pressroom)
              known<-!is.na(combined$id)&nzchar(as.character(combined$id))
              result$pressroom<-combined[!(known&duplicated(combined$id,fromLast=TRUE)),,drop=FALSE]
              attr(result$pressroom,"fetch_status")<-if(live_transfer_available&&identical(live_transfer_status,"ok"))"ok"else"partial"
            }
            if(!live_transfer_available||live_transfer_status%in%c("unavailable","stale"))state$reason<-"Live transfers could not be refreshed; using saved transfer history."
            state$seed<-result
            inferred<-result$rules$season %||% "unknown"
            if(!identical(inferred,state$season)&&identical(state$season,"unknown")&&inferred!="unknown"){
              state$season<-inferred;state$snapshot<-NULL;state$snapshot_signature<-NULL;state$snapshot_pending<-NULL;state$snapshot_status<-"not_saved";state$calculation_cache<-rival_calculation_cache();state$evidence<-list();state$stored_movements<-list();state$finance_ready<-FALSE;state$seed_ready<-FALSE;state$seed_complete<-FALSE;state$point_repair_done<-FALSE
              state$scheduled_teams<-character();state$scheduled_lineups<-character();state$lineup_pending<-0L;state$scheduled_rankings<-character();state$pending_team_ids<-character();state$queue<-list(list(kind="saved"));hydrate_snapshot(auth,cid,tid);run_next();return(NULL)
            }
            current<-rival_team_evidence(list(team_id=tid,info=result$info,movements=NULL),auth[["userid"]],cid,state$season)
            if(length(current)){state$evidence<-rival_merge_evidence(state$evidence,current);persist_rival_finance_evidence(current,ctx)}
            if(is.data.frame(result$notifications)&&nrow(result$notifications)){
              state$notification_cursor<-tail(result$notifications$id,1L)
              notification<-result$notifications[result$notifications$championship_id%in%cid,,drop=FALSE]
              record_round_publication_signal(root,auth[["userid"]],cid,notification)
              stamp<-attr(result$notifications,"observed_at")
              if(!is.null(stamp)&&!identical(attr(result$notifications,"fetch_status"),"stale")){
                observed<-rival_notification_evidence(notification,auth[["userid"]],cid,state$season,stamp)
                state$evidence<-rival_merge_evidence(state$evidence,observed)
                persist_rival_finance_evidence(observed,ctx)
              }
            }
            state$finance_ready<-TRUE;state$seed_ready<-TRUE;state$seed_complete<-TRUE;persist_ownership(context=ctx);enqueue_teams();enqueue_notifications();publish(TRUE)
          }else if(task$kind%in%c("ownership","ownership_catalog")){
            rows<-rival_ownership_observation_evidence(result,auth[["userid"]],cid,state$season,catalog=task$kind=="ownership_catalog")
            if(length(rows)){
              state$evidence<-rival_merge_evidence(state$evidence,rows);persist_ownership(rows,ctx)
            }else state$reason<-if(task$kind=="ownership_catalog")"Historical round start times could not be established; affected award ownership remains unknown."else
              "A player ownership history could not be retrieved; affected bonuses remain unknown."
          }else if(task$kind=="notifications"){
            state$notification_status<-mvp_text(result$status,"unavailable")
            state$notification_cursor<-mvp_text(result$cursor,task$cursor)
            if(state$notification_cursor%in%state$scheduled_notification_cursors&&state$notification_status=="ok")state$notification_status<-"repeated"
            observed<-rival_notification_evidence(result$notifications,auth[["userid"]],cid,state$season,result$observed_at)
            status<-if(state$notification_status=="ok"&&length(state$scheduled_notification_cursors)>=20L)"limit"else state$notification_status
            scan<-rival_finance_evidence(auth[["userid"]],cid,state$season,"notifications","round-publication-scan",
              list(kind="round_publication_scan",cursor=state$notification_cursor,status=status,missing_rounds=task$missing),Sys.time())
            observed<-c(observed,list(scan));state$evidence<-rival_merge_evidence(state$evidence,observed);persist_rival_finance_evidence(observed,ctx)
            if(state$notification_status=="unavailable")state$reason<-"Older round-publication notifications could not be retrieved; saved dates remain available."
            if(status=="limit")state$reason<-"Round-publication history page limit reached; Refresh continues from the saved cursor."
            enqueue_notifications()
          }else{
            if(task$kind=="team"&&identical(task$id,tid)&&is.list(result$info$configuration))
              state$seed$rules<-normalize_league_rules(result$info,championship_id=cid)
            # A corrected selection/round can finish before its older in-flight
            # lineup. Never let that late observation replace or persist over
            # the current signature merely because its completion time is newer.
            superseded_lineup<-FALSE
            if(task$kind=="lineup"){
              own<-Filter(function(record)identical(mvp_text(record$payload$team_id),task$id),
                rival_evidence_rows(state$evidence,"team_rounds"))
              current<-dplyr::bind_rows(lapply(own,function(record)rival_team_round_rows(
                Filter(function(row)is.list(row)&&identical(mvp_text(row$id),task$round),mvp_records(record$payload$rounds)),task$id)))
              if(!nrow(current)){
                current<-rival_saved_rounds(state$evidence,teams=state$teams$teamid)
                current<-current[current$team_id%in%task$id & current$round_id%in%task$round,,drop=FALSE]
              }
              superseded_lineup<-nrow(current)!=1L||
                !identical(task$signature,rival_round_signature(current,state$awards$rows))
            }
            rows<-if(superseded_lineup)list()else switch(task$kind,lineup=rival_lineup_evidence(result,auth[["userid"]],cid,state$season),
              ranking=rival_ranking_evidence(result,auth[["userid"]],cid,state$season),
              rival_team_evidence(result,auth[["userid"]],cid,state$season))
            if(length(rows)){delta<-rival_finance_evidence_delta(state$stored_movements,rows);state$evidence<-rival_merge_evidence(state$evidence,rows);persist_rival_finance_evidence(delta,ctx)}
            if(superseded_lineup)enqueue_lineups()
            if(task$kind=="lineup"&&!length(rows)&&!superseded_lineup)state$reason<-paste("Some historical lineups could not be retrieved; affected MVP/Best XI bonuses are not included. Use Refresh to retry.",mvp_text(result$reason))
            if(task$kind=="ranking"&&!length(rows))state$reason<-"A historical round ranking could not be confirmed; its position bonuses remain excluded."
            if(task$kind%in%c("team","ranking")){enqueue_point_repair();enqueue_rankings();enqueue_lineups();enqueue_notifications()}
          }
          },error=function(error){
            if(task$kind=="notifications"){state$notification_pending<-FALSE;state$notification_status<-"unavailable"}
            failure<-resource_log_failure(resource_failure(error,resource="league_finances_calc",category="schema"))
            state$reason<-paste("Some financial data could not be prepared.",resource_failure_message(failure))
            if(task$kind=="saved"&&!any(vapply(state$queue,function(task)identical(task$kind,"seed"),logical(1))))
              state$queue<-c(list(list(kind="seed")),state$queue)
            if(task$kind=="seed"){state$seed_ready<-TRUE;state$seed_complete<-TRUE;enqueue_teams()}
          })
          enqueue_ownership();publish(!length(state$queue)&&state$busy==0L);run_next();NULL
        },function(error){
          if(!state$closed&&generation==state$generation){state$busy<-state$busy-1L;if(task$kind=="lineup")state$lineup_pending<-max(0L,state$lineup_pending-1L);if(task$kind=="ownership")state$ownership_pending<-max(0L,state$ownership_pending-1L);if(task$kind=="notifications"){state$notification_pending<-FALSE;state$notification_status<-"unavailable"};if(task$kind=="team")state$pending_team_ids<-setdiff(state$pending_team_ids,task$id);state$reason<-"Some financial sources could not be refreshed; saved evidence remains available.";if(task$kind=="saved")state$queue<-list(list(kind="seed"))
            if(task$kind=="seed"){state$seed_ready<-TRUE;state$seed_complete<-TRUE;enqueue_teams()};enqueue_point_repair();enqueue_rankings();enqueue_ownership();publish(TRUE);run_next()};NULL
        })
      })
    }
    invisible(NULL)
  }
  shiny::observe({
    auth<-login()
    if(!valid_login(auth)){
      if(!is.null(state$key)){state$generation<-state$generation+1L;state$key<-NULL;state$evidence<-list();state$stored_movements<-list();state$queue<-list();state$busy<-0L;state$lineup_pending<-0L;state$ownership_pending<-0L
        if(!is.null(state$publish_timer)){state$publish_timer();state$publish_timer<-NULL}
        state$identity<-NULL;state$snapshot<-NULL;state$snapshot_status<-"not_saved";state$calculation_cache<-rival_calculation_cache();state$calculated<-NULL;state$calculation_inputs<-NULL;state$transfers(data.frame());state$teams<-data.frame();state$finance_ready<-FALSE;state$seed_ready<-FALSE;state$seed_complete<-FALSE
        state$value(list(team_finances=data.frame(),breakdowns=list(),loading=FALSE))};return()
    }
    if(!isTRUE(active()))return()
    cid<-championship();tid<-user_team();shiny::req(cid,tid)
    if(!league_context_ready(root,auth,cid)){
      state$generation<-state$generation+1L;state$key<-NULL;state$identity<-NULL;state$queue<-list();state$busy<-0L;state$lineup_pending<-0L
      state$value(list(team_finances=data.frame(),breakdowns=list(),loading=TRUE,reason="Resolving competition and season metadata."));return()
    }
    version<-if(is.function(refresh))refresh()else 0
    publication<-if(is.function(root$userData$round_publication_revision))root$userData$round_publication_revision()else 0
    league_context_revision()
    resolved<-resource_context(auth,cid,tid)
    identity<-paste(auth[["userid"]],cid,tid,resolved$competition_id,resolved$season,resolved$scoring_version,sep="|")
    key<-paste(identity,version,publication,sep="|")
    if(!identical(state$key,key)){
      same<-identical(state$identity,identity);state$identity<-identity
      state$key<-key;state$generation<-state$generation+1L;state$busy<-0L;state$lineup_pending<-0L;state$archive_lineups<-list();state$stored_movements<-list();state$season<-resolved$season;state$reason<-"";state$build_failure<-NULL
      if(!is.null(state$publish_timer)){state$publish_timer();state$publish_timer<-NULL}
      state$scheduled_owners<-character();state$ownership_pending<-0L;state$ownership_catalog_scheduled<-FALSE;state$ownership_write_attempts<-character();if(!same)state$ownership_unsaved<-list()
      state$account<-auth[["userid"]];state$championship<-cid;state$snapshot_attempted_signature<-NULL;state$snapshot_pending<-NULL;state$seed_ready<-FALSE;state$seed_complete<-FALSE;state$point_repair_done<-FALSE;state$notification_cursor<-"";state$notification_status<-"initial";state$scheduled_notification_cursors<-character();state$notification_pending<-FALSE;state$scheduled_teams<-character();state$scheduled_lineups<-character();state$scheduled_rankings<-character();state$pending_team_ids<-character()
      if(!same){state$calculation_cache<-rival_calculation_cache();state$snapshot<-NULL;state$snapshot_status<-"not_saved";state$snapshot_signature<-NULL;state$snapshot_pending<-NULL;state$shared_cache_signature<-NULL
        ctx<-resource_context(account_id=state$account,championship_id=cid,user_team_id=tid,season=state$season)
        saved_cache<-api_cache_env[[api_cache_key(paste0("rival_calculation_",rival_calculation_version),context=ctx)]]
        if(is.list(saved_cache$data)&&is.finite(saved_cache$expires_at)&&saved_cache$expires_at>as.numeric(Sys.time()))
          list2env(saved_cache$data,envir=state$calculation_cache)
        state$calculated<-NULL;state$calculation_inputs<-NULL;state$transfers(data.frame());state$evidence<-list();state$stored_movements<-list();state$seed<-list();state$defaults<-NULL;state$finance_ready<-FALSE
        state$teams<-data.frame();state$teams_status<-"loading";state$teams_reason<-"";state$awards<-list(rows=data.frame(),complete=FALSE)
        state$value(list(team_finances=data.frame(),breakdowns=list(),loading=TRUE))}
      else invalidate_resources(resource_context(account_id=auth[["userid"]],championship_id=cid,season=state$season),pattern="rival_finance_evidence")
      if(!same)hydrate_snapshot(auth,cid,tid)
      state$queue<-list(list(kind="saved"));run_next()
    }else run_next()
  },priority=100)
  # Resolve UI dependencies in observers, never in completion callbacks. A pending
  # factual read raises a Shiny validation condition and must not stall the queue.
  shiny::observe({
    shiny::req(isTRUE(active()),valid_login(login()))
    cid<-championship();tid<-user_team()
    ctx<-resource_context(login(),cid,tid)
    shiny::req(identical(state$identity,paste(ctx$account_id,cid,tid,ctx$competition_id,ctx$season,ctx$scoring_version,sep="|")),!is.null(state$key))
    directory<-tryCatch(teams(),error=function(error)error)
    failed<-inherits(directory,"error")
    pending<-failed&&inherits(directory,"shiny.silent.error")&&
      (!nzchar(conditionMessage(directory))||grepl("Loading|Refreshing",conditionMessage(directory)))
    valid<-is.data.frame(directory)&&all(c("teamid","teamname")%in%names(directory))
    if(valid){
      directory<-directory[!is.na(directory$teamid)&nzchar(as.character(directory$teamid)),,drop=FALSE]
      state$teams<-directory[!duplicated(directory$teamid),,drop=FALSE]
    }
    state$teams_status<-if(valid)if(nrow(state$teams))"ready"else"empty"else if(pending)"loading"else"unavailable"
    state$teams_reason<-if(state$teams_status%in%c("empty","unavailable"))"League teams unavailable. Use Refresh to retry."else""
    enqueue_teams();publish(TRUE);run_next()
  })
  shiny::observe({
    shiny::req(isTRUE(active()),valid_login(login()),!is.null(state$key))
    awards<-tryCatch(official(),error=function(error)NULL)
    if(is.list(awards))state$awards<-awards
    enqueue_rankings();enqueue_lineups();enqueue_notifications();enqueue_ownership();publish(isTRUE(state$awards$complete));run_next()
  })
  shiny::observe({
    transfer_publication_revision();state$value()
    shiny::req(isTRUE(active()),valid_login(login()),state$seed_ready)
    ctx<-resource_context(login(),championship())
    key<-api_cache_key(paste0("pressroom_",championship()),context=ctx)
    cached<-api_cache_env[[key]];current<-cached$data
    age<-if(!is.null(cached$time))as.numeric(difftime(Sys.time(),cached$time,units="secs"))else Inf
    if(is.finite(age)&&age>=0&&age<=resource_policy("pressroom")$ttl&&is.data.frame(current)&&identical(attr(current,"fetch_status"),"ok")&&
       !identical(current,state$seed$pressroom)){
      state$seed$pressroom<-current
      if(identical(state$reason,"Live transfers could not be refreshed; using saved transfer history."))state$reason<-""
      publish(TRUE)
    }
  })
  shiny::reactive(state$value())
}

# This consumes explicitly verified historical settlement evidence, not an invented
# wire contract. The documented team-round response does not establish these flags.
rival_round_income_estimates <- function(evidence,team_id,rules,movements=data.frame()) {
  empty<-data.frame(component=character(),amount=numeric(),source=character(),round_id=character(),player_id=character(),stringsAsFactors=FALSE)
  rounds<-rival_evidence_rows(evidence,"eligibility");out<-list()
  for(record in rounds){
    p<-record$payload
    if(!identical(mvp_text(p$team_id),as.character(team_id))||!isTRUE(p$verified))next
    round<-mvp_text(p$round_id);if(!nzchar(round))next
    for(type in c("points","ranking")){
      paid<-nrow(movements)&&all(c("bonus_type","round_id")%in%names(movements))&&any(movements$bonus_type%in%type & (movements$round_id%in%round | !nzchar(movements$round_id)))
      if(paid)next
      amount<-NA_real_;source<-""
      if(type=="points"&&isTRUE(p$points_credited)){
        rate<-fm_number(p$money_per_point);if(!is.finite(rate))rate<-fm_number(rules$money_per_point)
        if(is.finite(rate)&&is.finite(fm_number(p$credited_points))){amount<-rate*fm_number(p$credited_points);source<-if(isTRUE(p$historical_rules))"Historical credited points and rule; payment not observed."else"Historical credited points; assumes current point rate."}
      }
      if(type=="ranking"&&isTRUE(p$round_settled)&&isTRUE(p$ranking_rule_verified)&&is.finite(fm_number(p$ranking_reward))){amount<-fm_number(p$ranking_reward);source<-"Verified historical round-ranking allocation; payment not observed."}
      if(is.finite(amount))out[[length(out)+1L]]<-data.frame(component=if(type=="points")"Point reward"else"Ranking reward",amount=amount,source=source,round_id=round,player_id="",stringsAsFactors=FALSE)
    }
  }
  rows<-dplyr::bind_rows(c(list(empty),out));missing<-character()
  if(nrow(rows)){
    key<-paste(rows$component,rows$round_id,sep="|")
    groups<-split(seq_len(nrow(rows)),key)
    conflicting<-vapply(groups,function(i)length(unique(rows$amount[i]))!=1L,logical(1))
    if(any(conflicting))missing<-"Conflicting historical team-round settlement amounts were excluded."
    rows<-rows[unlist(lapply(groups[!conflicting],head,1L),use.names=FALSE),,drop=FALSE]
  }
  list(rows=rows,missing=missing)
}

# Observing a changed notification list asks for data again; it never credits cash
# or establishes final settlement. No undocumented round identifier is parsed.
record_round_publication_signal <- function(root,account_id,championship_id,notifications) {
  if(!is.data.frame(notifications)||!all(c("id","championship_id")%in%names(notifications)))return(invisible(FALSE))
  rows<-notifications[notifications$championship_id%in%championship_id,,drop=FALSE]
  fields<-intersect(c("id","updated_at","type","action"),names(rows))
  rows<-rows[order(rows$id),fields,drop=FALSE]
  key<-paste(account_id,championship_id,sep="|")
  fingerprint<-as.character(openssl::sha256(serialize(rows,NULL)))
  old<-root$userData$round_publication_fingerprints[[key]]
  root$userData$round_publication_fingerprints[[key]]<-fingerprint
  if(length(root$userData$round_publication_fingerprints)>10L)root$userData$round_publication_fingerprints<-tail(root$userData$round_publication_fingerprints,10L)
  if(is.null(root$userData$round_publication_revision))root$userData$round_publication_revision<-shiny::reactiveVal(0L)
  if(!is.null(old)&&!identical(old,fingerprint)){
    shiny::isolate(root$userData$round_publication_revision(root$userData$round_publication_revision()+1L))
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

# Explicit scalar decoding keeps JSON null scores as NA, including an all-null XI.
rival_award_snapshot_rows <- function(players) {
  dplyr::bind_rows(lapply(rival_finance_records(players),function(p){
    if(!is.list(p))return(NULL)
    data.frame(championship_id=mvp_text(p$championship_id),round_id=mvp_text(p$round_id),round_number=fm_number(p$round_number),
      player_id=mvp_text(p$player_id),player_name=mvp_text(p$player_name),player_role=mvp_text(p$player_role),player_position=mvp_text(p$player_position),
      points=fm_number(p$points),is_mvp=isTRUE(p$is_mvp),is_finished=isTRUE(p$is_finished),stringsAsFactors=FALSE)
  }))
}
