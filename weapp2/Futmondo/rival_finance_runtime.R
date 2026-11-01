# League-scoped observed accounting. See docs/rival_cash_accounting.md.
rival_cash_league_id <- "5b55f9d767214483120b87cf"
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
  # Missing amounts remain unknown; malformed identities never become fabricated IDs.
  attr(rows,"complete")<-nrow(rows)==length(records)&&all(nzchar(rows$id))&&all(is.finite(rows$money))
  attr(rows,"fetch_status")<-if(isTRUE(attr(rows,"complete")))if(nrow(rows))"ok"else"empty"else"partial"
  rows
}
rival_finance_evidence <- function(account_id,championship_id,season,type,id,payload,observed_at=Sys.time()) {
  if(type%in%c("team_rounds","round_lineup")){
    payload$kind<-type;id<-paste0(if(type=="team_rounds")"rounds:"else"lineup:",id);type<-"eligibility"
  }
  list(account_id=as.character(account_id),championship_id=as.character(championship_id),season=mvp_text(season,"unknown"),
    evidence_type=type,source_id=as.character(id),observed_at=rival_finance_stamp(observed_at),payload=payload)
}
rival_evidence_rows <- function(evidence,type) {
  if(!is.list(evidence))return(list())
  Filter(function(x)is.list(x)&&if(type%in%c("team_rounds","round_lineup"))
    identical(x$evidence_type,"eligibility")&&is.list(x$payload)&&identical(x$payload$kind,type)
    else identical(x$evidence_type,type),evidence)
}
rival_evidence_decode <- function(rows) {
  if(!is.data.frame(rows)||!nrow(rows))return(list())
  lapply(seq_len(nrow(rows)),function(i){
    row<-as.list(rows[i,,drop=FALSE]);p<-rows$payload
    row$payload<-if(is.data.frame(p))as.list(p[i,,drop=FALSE])else p[[i]]
    if(is.character(row$payload))row$payload<-tryCatch(jsonlite::fromJSON(row$payload,simplifyVector=FALSE),error=function(e)list())
    row
  })
}
read_rival_finance_evidence <- function(account_id,championship_id,season="unknown",max_rows=20000L) {
  get_cached_data(paste0("rival_finance_evidence_",account_id,"_",championship_id,"_",season),{
    rows<-history_read_pages("rival_finance_evidence_latest",list(account_id=paste0("eq.",account_id),championship_id=paste0("eq.",championship_id),
      season=paste0("eq.",season),select="account_id,championship_id,season,evidence_type,source_id,observed_at,payload",
      order="evidence_type.asc,source_id.asc"),max_rows=max_rows,page_size=1000L)
    list(evidence=rival_evidence_decode(rows),complete=isTRUE(attr(rows,"complete")))
  },timeout_sec=300,context=resource_context(account_id=account_id,championship_id=championship_id,season=season))
}
get_rival_finance_saved <- function(account_id,championship_id,season="unknown") {
  result<-tryCatch(read_rival_finance_evidence(account_id,championship_id,season),error=function(e)list(evidence=list(),complete=FALSE))
  result$defaults<-tryCatch(supabase_get("league_cash_defaults",list(championship_id=paste0("eq.",championship_id),select="championship_id,amount,source")),error=function(e)NULL)
  result
}
persist_rival_finance_evidence <- function(evidence,context) {
  if(!length(evidence))return(invisible(TRUE))
  evidence<-Filter(function(row){
    identical(mvp_text(row$account_id),mvp_text(context$account_id))&&
      identical(mvp_text(row$championship_id),mvp_text(context$championship_id))&&
      as.numeric(object.size(row))<=256*1024L
  },evidence)
  if(!length(evidence))return(invisible(FALSE))
  # Small immutable revisions, passed through the existing bounded asynchronous queue.
  groups<-split(seq_along(evidence),ceiling(seq_along(evidence)/50L))
  index<-1L
  next_group<-function(){
    if(index>length(groups))return(invisible(TRUE))
    indices<-groups[[index]];index<<-index+1L
    defer_persistence("supabase_post_direct",list("rival_finance_evidence",unname(evidence[indices]),
      "account_id,championship_id,season,evidence_type,source_id,observed_at",TRUE),context=context,
      on_complete=function(ok){if(isTRUE(ok))later::later(next_group,0)})
  }
  isTRUE(next_group())
}
rival_merge_evidence <- function(old,new,max_rows=20000L) {
  rows<-c(old,new);if(!length(rows))return(rows)
  key<-vapply(rows,function(x)paste(x$account_id,x$championship_id,x$season,x$evidence_type,x$source_id,sep="|"),character(1))
  stamp<-vapply(rows,function(x)as.numeric(fm_time(x$observed_at)),numeric(1));stamp[!is.finite(stamp)]<--Inf
  order<-order(stamp);rows<-rows[order];key<-key[order]
  tail(rows[!duplicated(key,fromLast=TRUE)],max_rows)
}
rival_opening_cash <- function(championship_id,rules=list(),defaults=NULL) {
  value<-fm_number(rules$initial_budget)
  if(is.finite(value)&&value>=0)return(list(amount=value,source="league configuration"))
  if(is.data.frame(defaults)&&all(c("championship_id","amount","source")%in%names(defaults))){
    row<-defaults[defaults$championship_id%in%championship_id & defaults$source%in%"user_confirmed",,drop=FALSE]
    if(nrow(row)==1L&&is.finite(fm_number(row$amount))&&fm_number(row$amount)>=0)return(list(amount=fm_number(row$amount),source="user confirmed"))
  }
  if(identical(as.character(championship_id),rival_cash_league_id))return(list(amount=300000000,source="user confirmed"))
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
  ranking<-rows$type%in%"ranking"
  rows$bonus_type[ranking]<-"ranking";rows$type[ranking]<-"bonus"
  for(i in which(ranking)){
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
    if(any(grepl("[.]",round$settlement_label))||repeated){
      return(data.frame(round_id=round$round_id[1],date="",settlement_label="unverified split settlement"))
    }
    reference<-sort(unique(round$team_id))[1]
    unique(round[round$team_id%in%reference,c("round_id","date","settlement_label"),drop=FALSE])
  }))
}
rival_reward_date <- function(round_id,anchors) {
  x<-anchors[anchors$round_id%in%round_id,,drop=FALSE]
  # Revised/split settlements require phase-specific selections. A latest full
  # selection cannot be assigned retroactively to the first or last payment.
  if(nrow(x)==1L&&!grepl("[.]",x$settlement_label))x$date else ""
}

# Eligibility is accepted only as explicit historical evidence. No current roster lookup.
rival_award_estimates <- function(official,team_id,rules,eligibility=data.frame(),payments=data.frame(),round_rules=list()) {
  empty<-data.frame(round_id=character(),player_id=character(),bonus_type=character(),amount=numeric(),reason=character(),stringsAsFactors=FALSE)
  if(!is.data.frame(official)||!all(c("round_id","player_id","is_mvp")%in%names(official)))return(list(rows=empty,missing="Official round awards unavailable."))
  unfinished<-!"is_finished"%in%names(official)||any(!official$is_finished%in%TRUE)
  if("is_finished"%in%names(official))official<-official[official$is_finished%in%TRUE,,drop=FALSE]else official<-official[FALSE,,drop=FALSE]
  events<-dplyr::bind_rows(lapply(seq_len(nrow(official)),function(i){
    row<-official[i,,drop=FALSE]
    data.frame(round_id=as.character(row$round_id),player_id=as.character(row$player_id),bonus_type=c("dreamteam",if(isTRUE(row$is_mvp))"mvp"),stringsAsFactors=FALSE)
  }))
  events<-unique(events);out<-list();missing<-if(unfinished)"Unfinished or unverified award selections are excluded from reconstructed cash."else character()
  for(i in seq_len(nrow(events))){
    e<-events[i,,drop=FALSE];rule<-round_rules[[e$round_id]] %||% rules
    amount<-fm_number(if(e$bonus_type=="mvp")rule$mvp_reward else rule$dreamteam_reward)
    if(is.finite(amount)&&amount==0)next
    unscoped<-nrow(payments)&&all(c("round_id","bonus_type")%in%names(payments))&&any(payments$bonus_type%in%e$bonus_type & !nzchar(payments$round_id))
    if(unscoped){missing<-c(missing,paste("Recorded",e$bonus_type,"income has no round identity; overlapping estimates of that reward type are excluded."));next}
    same<-if(nrow(payments)&&all(c("round_id","player_id","bonus_type")%in%names(payments)))payments$round_id%in%e$round_id & (!nzchar(payments$player_id)|payments$player_id%in%e$player_id) & payments$bonus_type%in%e$bonus_type else logical()
    if(any(same))next
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
rival_saved_rounds <- function(evidence) {
  empty<-rival_team_round_rows(list(),"")
  dplyr::bind_rows(c(list(empty),lapply(rival_evidence_rows(evidence,"team_rounds"),function(row){
    if(!is.list(row$payload))return(NULL)
    rival_team_round_rows(row$payload$rounds,mvp_text(row$payload$team_id))
  })))
}
rival_round_signature <- function(row,awards) {
  selected<-if(is.data.frame(awards)&&all(c("round_id","player_id")%in%names(awards)))awards[awards$round_id%in%row$round_id,,drop=FALSE]else data.frame()
  if(nrow(selected))selected<-selected[order(selected$player_id),intersect(c("player_id","points","is_mvp","is_finished"),names(selected)),drop=FALSE]
  mvp_text(as.character(openssl::sha256(serialize(list(round=as.list(row[,intersect(c("round_id","points","status","negative"),names(row)),drop=FALSE]),awards=unname(lapply(seq_len(nrow(selected)),function(i)as.list(selected[i,,drop=FALSE])))),NULL))))
}
rival_lineup_evidence <- function(result,account_id,championship_id,season) {
  answer<-result$answer
  if(!is.list(answer)||isTRUE(answer$error)||!is.list(answer$players)||!length(answer$players))return(list())
  players<-unique(vapply(mvp_records(answer$players),function(x)if(is.list(x))mvp_text(x$id)else"",character(1)))
  bench<-unique(vapply(mvp_records(answer$bench$players),function(x)if(is.list(x))mvp_text(x$id)else"",character(1)))
  if(!any(nzchar(players)))return(list())
  payload<-list(team_id=result$team_id,round_id=result$round_id,status=mvp_text(answer$status),negative=rival_flag(answer$negative),
    players=as.list(players[nzchar(players)]),bench=as.list(bench[nzchar(bench)]),signature=result$signature)
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
rival_ranking_allocations <- function(points,pool,mode,users_to_rank=-1) {
  n<-length(points);unknown<-rep(NA_real_,n)
  if(!n||any(!is.finite(points))||!is.finite(pool)||pool<0||!mode%in%c("top","flop"))return(unknown)
  if(pool==0)return(rep(0,n))
  count<-if(identical(as.numeric(users_to_rank),-1))n else as.integer(users_to_rank)
  if(length(count)!=1L||is.na(count)||!count%in%c(3L,5L,n))return(unknown)
  weights<-if(count==n&&users_to_rank==-1)rev(seq_len(n))/sum(seq_len(n)) else if(count==3L)c(.5,.3,.2)else if(count==5L)c(.4,.3,.15,.1,.05)else return(unknown)
  slots<-c(floor(pool*weights+.5),rep(0,max(0,n-length(weights))))[seq_len(n)]
  order<-order(points,decreasing=mode=="top");ordered<-points[order];out<-numeric(n)
  for(score in unique(ordered)){indices<-which(ordered==score);out[order[indices]]<-mean(slots[indices])}
  out
}
# One preparation pass per publication, shared by all team calculations.
rival_historical_components <- function(evidence,teams,rules,official) {
  rounds<-rival_saved_rounds(evidence);all_rounds<-rounds
  final<-if(nrow(rounds))vapply(seq_len(nrow(rounds)),function(i)round_is_finished(as.list(rounds[i,,drop=FALSE])),logical(1))else logical()
  rounds<-rounds[final,,drop=FALSE]
  source_rounds<-rounds
  lineups<-rival_evidence_rows(evidence,"round_lineup")
  for(i in seq_len(nrow(rounds))){
    row<-rounds[i,,drop=FALSE]
    candidates<-Filter(function(record){
      p<-record$payload
      is.list(p)&&identical(mvp_text(p$team_id),row$team_id)&&identical(mvp_text(p$round_id),row$round_id)&&
        (!nzchar(mvp_text(p$signature))||identical(mvp_text(p$signature),mvp_text(rival_round_signature(row,official))))
    },lineups)
    if(length(candidates)){
      negative<-rival_flag(tail(candidates,1)[[1]]$payload$negative)
      if(isTRUE(negative)||is.na(rounds$negative[i]))rounds$negative[i]<-negative
    }
  }
  income<-data.frame(team_id=character(),round_id=character(),bonus_type=character(),amount=numeric(),source=character(),stringsAsFactors=FALSE)
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
    if(!setequal(rows$team_id,as.character(teams$teamid))||any(!is.finite(rows$points))){
      for(id in as.character(teams$teamid))add_missing(id,paste("Ranking history incomplete for round",rounds$round_number[match(round,rounds$round_id)]));next
    }
    # Negative-start teams do not take part in automatic premiums (FAQ142).
    eligible<-!rows$negative%in%TRUE
    value<-rep(0,nrow(rows));value[eligible]<-rival_ranking_allocations(rows$points[eligible],fm_number(rules$ranking_pool),
      mvp_text(rules$ranking_mode),fm_number(rules$users_to_rank %||% rules$configuration$usersToRank))
    for(i in seq_len(nrow(rows))){
      if(!is.finite(value[i])){add_missing(rows$team_id[i],"Ranking reward configuration is unavailable or unsupported.");next}
      source<-"Published round standings; assumes unchanged ranking rules and historical league membership."
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
    source<-source_rounds[source_rounds$team_id%in%mvp_text(p$team_id)&source_rounds$round_id%in%mvp_text(p$round_id),,drop=FALSE]
    expected<-rival_round_signature(source[1,,drop=FALSE],official)
    if(nzchar(mvp_text(p$signature))&&!identical(mvp_text(p$signature),mvp_text(expected))){
      add_missing(p$team_id,"A changed round is waiting for its historical lineup to refresh.");next
    }
    players<-unique(as.character(unlist(c(p$players,p$bench),use.names=FALSE)))
    if(!length(players))next
    eligibility<-dplyr::bind_rows(eligibility,data.frame(round_id=p$round_id,player_id=players,team_id=p$team_id,
      eligible=!isTRUE(p$negative)&&!isTRUE(r$negative[1]),verified=TRUE))
    if(is.na(rival_flag(p$negative))&&is.na(r$negative[1]))add_missing(p$team_id,"Award rewards assume eligible starting cash where the historical flag is absent.")
  }
  # A completed team-round is actual publication/finality evidence even when an
  # old saved award snapshot predates schedule finality metadata.
  completed<-Filter(function(id){
    rows<-all_rounds[all_rounds$round_id%in%id,,drop=FALSE]
    nrow(rows)>0L&&setequal(rows$team_id,as.character(teams$teamid))&&
      all(vapply(seq_len(nrow(rows)),function(i)round_is_finished(as.list(rows[i,,drop=FALSE])),logical(1)))
  },unique(all_rounds$round_id))
  if(is.data.frame(official)&&nrow(official)&&"is_finished"%in%names(official))
    official$is_finished[is.na(official$is_finished)&official$round_id%in%completed]<-TRUE
  list(income=income,eligibility=unique(eligibility),official=official,missing=missing,rounds=rounds)
}

# The same entries drive component totals and transaction history. An undated
# estimate has no invented payment time and is applied only in reconciliation.
rival_cash_entry <- function(id,component,amount,source,type="bonus",date="",category="",round_id="",player_id="",estimated=FALSE,included=TRUE,bonus_type="",calculation="",source_type="",source_category="",settlement_label="") {
  data.frame(id=id,component=component,amount=amount,source=source,type=type,date=date,category=category,
    round_id=round_id,player_id=player_id,estimated=estimated,included=included,bonus_type=bonus_type,calculation=calculation,
    source_type=source_type,source_category=source_category,settlement_label=settlement_label,stringsAsFactors=FALSE)
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

# Public pure accounting interface; every numeric subtotal has an evidence trail.
build_rival_cash_dataset <- function(teams,championship_id,evidence=list(),official=data.frame(),pressroom=data.frame(),rules=list(),opening_defaults=NULL,eligibility=data.frame(),coverage_complete=FALSE,now=Sys.time(),account_id=NULL,season="unknown") {
  empty<-data.frame(teamid=character(),teamname=character(),estimated_cash=numeric(),confidence=character(),as_of=character(),stringsAsFactors=FALSE)
  if(!is.data.frame(teams)||!nrow(teams))return(list(team_finances=empty,breakdowns=list(),cash_ledgers=list(),all_purchases=data.frame()))
  if(!nzchar(mvp_text(account_id)))stop("An explicit account is required for rival finance evidence.")
  evidence<-Filter(function(x)identical(mvp_text(x$championship_id),as.character(championship_id))&&identical(mvp_text(x$season,"unknown"),as.character(season))&&(is.null(account_id)||identical(mvp_text(x$account_id),as.character(account_id))),rival_merge_evidence(list(),evidence))
  scope_frame<-function(rows){
    if(!is.data.frame(rows))return(data.frame())
    expected<-list(championship_id=championship_id,season=season,account_id=account_id)
    for(field in intersect(names(rows),names(expected)))if(!is.null(expected[[field]]))rows<-rows[rows[[field]]%in%expected[[field]],,drop=FALSE]
    rows
  }
  official<-scope_frame(official);eligibility<-scope_frame(eligibility)
  history<-rival_historical_components(evidence,teams,rules,official);official<-history$official
  stored_eligibility<-dplyr::bind_rows(lapply(rival_evidence_rows(evidence,"eligibility"),function(row){
    p<-row$payload
    if(!is.list(p)||!isTRUE(p$verified)||!all(c("round_id","player_id","team_id","eligible")%in%names(p)))return(NULL)
    data.frame(round_id=mvp_text(p$round_id),player_id=mvp_text(p$player_id),team_id=mvp_text(p$team_id),eligible=isTRUE(p$eligible),verified=TRUE)
  }))
  eligibility<-dplyr::bind_rows(eligibility,history$eligibility,stored_eligibility)
  if(all(c("round_id","player_id","team_id","eligible","verified")%in%names(eligibility)))
    eligibility<-unique(eligibility[,c("round_id","player_id","team_id","eligible","verified"),drop=FALSE])
  catalog<-unique(dplyr::bind_rows(history$rounds[,c("round_id","round_number"),drop=FALSE],
    if(all(c("round_id","round_number")%in%names(official)))official[,c("round_id","round_number"),drop=FALSE]else data.frame()))
  all_movements<-dplyr::bind_rows(lapply(unique(vapply(rival_evidence_rows(evidence,"movements"),function(x)mvp_text(x$payload$team_id),character(1))),function(id)
    rival_normalize_movements(rival_cash_latest_movements(evidence,id),catalog)))
  pressroom<-scope_frame(pressroom)
  if(is.data.frame(pressroom)&&nrow(pressroom)&&"id"%in%names(pressroom)){
    known<-!is.na(pressroom$id)&nzchar(as.character(pressroom$id))
    pressroom<-pressroom[!(known&duplicated(pressroom$id,fromLast=TRUE)),,drop=FALSE]
  }
  pressroom<-rival_classify_transfers(pressroom,all_movements)
  anchors<-rival_payment_anchors(all_movements)
  opening<-rival_opening_cash(championship_id,rules,opening_defaults);result<-list();breakdowns<-list();ledgers<-list()
  for(i in seq_len(nrow(teams))){
    team<-teams[i,,drop=FALSE];tid<-as.character(team$teamid %||% team$id);name<-as.character(team$teamname %||% team$name %||% tid)
    mov<-if(nrow(all_movements))all_movements[all_movements$team_id%in%tid,,drop=FALSE]else data.frame()
    own<-Filter(function(x)identical(mvp_text(x$source_id),tid),rival_evidence_rows(evidence,"team"));info<-if(length(own))tail(own,1)[[1]]$payload else list()
    coverage<-Filter(function(x)identical(mvp_text(x$source_id),tid),rival_evidence_rows(evidence,"coverage"));coverage<-if(length(coverage))tail(coverage,1)[[1]]$payload else list()
    actual<-fm_number(info$budget);stamp<-mvp_text(info$observed_at);fresh<-is.finite(actual)&&!is.na(fm_time(stamp))&&as.numeric(difftime(now,fm_time(stamp),units="secs"))>=-60&&as.numeric(difftime(now,fm_time(stamp),units="secs"))<=300
    details<-rival_cash_entry("opening","Opening cash",opening$amount,opening$source,type="opening")
    missing<-history$missing[[tid]] %||% character();assumptions<-character();recorded<-0;point_income<-ranking_income<-ideal_income<-mvp_income<-0
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
          bonus_type=nonbudget$bonus_type,source_type=nonbudget$source_type,source_category=nonbudget$source_category,settlement_label=nonbudget$settlement_label)
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
    derived<-history$income[history$income$team_id%in%tid,,drop=FALSE]
    if(nrow(derived))for(j in seq_len(nrow(derived))){
      row<-derived[j,,drop=FALSE];component<-if(row$bonus_type=="points")"Point reward"else"Ranking reward"
      paid<-nrow(mov)&&any(mov$bonus_type%in%row$bonus_type & (mov$round_id%in%row$round_id | !nzchar(mov$round_id)))
      old<-nrow(round_income$rows)&&any(round_income$rows$component%in%component & round_income$rows$round_id%in%row$round_id)
      if(!paid&&!old)round_income$rows<-dplyr::bind_rows(round_income$rows,data.frame(component=component,amount=row$amount,source=row$source,round_id=row$round_id,player_id=""))
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
    for(j in which(details$estimated & details$bonus_type%in%c("ranking","mvp","dreamteam"))){
      date<-rival_reward_date(details$round_id[j],anchors)
      if(nzchar(date)){
        details$date[j]<-date;details$source[j]<-paste(details$source[j],"Reconstructed at recorded league position-payment time; simultaneous rewards confirmed by league user.")
      }else details$source[j]<-paste(details$source[j],"Payment date unverified; split settlements require phase-specific evidence.")
    }
    details$component[details$component%in%"Ranking reward"]<-"Position reward"
    ledger<-rival_cash_ledger(details)
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
    result[[i]]<-data.frame(teamid=tid,teamname=name,estimated_cash=value,confidence=confidence,as_of=stamp,
      cash_status=if(!is.finite(value))"Unavailable"else if(value<0)if(confidence=="Reconciled")"Reconciled negative cash"else"Estimated negative cash"else"Positive or zero cash",
      initial_budget=details$amount[1],budget=if(fresh)actual else NA_real_,reconstructed_cash=reconstructed,reconciliation_difference=difference,
      observed_cash=actual,observed_cash_fresh=fresh,
      total_inflow=sum(ledger$amount[ledger$included & is.finite(ledger$amount)&ledger$amount>0]),
      total_outflow=-sum(ledger$amount[ledger$included & is.finite(ledger$amount)&ledger$amount<0]),
      total_spent=spent,total_sales=sales,point_bonus=point_income,ranking_prize=ranking_income,dreamteam_income=ideal_income,mvp_income=mvp_income,
      team_value=fm_number(info$teamValue %||% team$teamValue),points=fm_number(info$points %||% team$points),
      squad_size=fm_number(info$squad_size),net_profit_loss=NA_real_,estimate_notes=paste(reasons,collapse=" "),stringsAsFactors=FALSE)
    breakdowns[[tid]]<-ledger[,c("component","amount","source","round_id","player_id"),drop=FALSE]
    ledger$round_number<-history$rounds$round_number[match(ledger$round_id,history$rounds$round_id)]
    ledgers[[tid]]<-ledger
  }
  list(team_finances=dplyr::bind_rows(result),breakdowns=breakdowns,cash_ledgers=ledgers,pressroom=pressroom,all_purchases=data.frame())
}

get_rival_finance_seed <- function(login,championship_id,user_team_id) {
  info<-tryCatch(get_user_team_info(login,championship_id,user_team_id),error=function(e)NULL)
  list(info=info,rules=normalize_league_rules(info,championship_id=championship_id),
    pressroom=tryCatch(get_championship_pressroom(login,championship_id),error=function(e)data.frame()),
    notifications=tryCatch(get_notifications(login),error=function(e)NULL))
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
  rows
}
# One shared root-session ledger, rendered in bounded batches rather than per write.
shared_rival_finance_source <- function(session,login,championship,user_team,teams,active,official,refresh=NULL) {
  root<-session$rootScope()
  if(is.null(root$userData$rival_finance_shared)){
    state<-new.env(parent=emptyenv());state$value<-shiny::reactiveVal(list(team_finances=data.frame(),breakdowns=list(),loading=TRUE));state$key<-NULL
    state$generation<-0L;state$closed<-FALSE;state$queue<-list();state$busy<-0L;state$evidence<-list();state$seed<-list();state$defaults<-NULL;state$last_publish<-0
    state$publish_timer<-NULL;state$teams<-data.frame();state$teams_status<-"loading";state$teams_reason<-""
    state$awards<-list(rows=data.frame(),complete=FALSE);state$finance_ready<-FALSE;state$seed_ready<-FALSE;state$scheduled_teams<-character();state$scheduled_lineups<-character()
    root$onSessionEnded(function(){state$closed<-TRUE;state$generation<-state$generation+1L;if(!is.null(state$publish_timer))state$publish_timer()})
    root$userData$rival_finance_shared<-state
  }
  state<-root$userData$rival_finance_shared
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
    if(state$finance_ready)calculated<-tryCatch(build_rival_cash_dataset(state$teams,state$championship,state$evidence,state$awards$rows,
      state$seed$pressroom %||% data.frame(),state$seed$rules %||% list(),state$defaults,
      coverage_complete=isTRUE(state$awards$complete),account_id=state$account,season=state$season),error=function(error){
        if(is.null(state$build_failure))state$build_failure<-resource_log_failure(
          resource_failure(error,resource="league_finances_calc",category="schema"))
        calculated$finance_error<-paste("Financial details could not be prepared.",resource_failure_message(state$build_failure))
        calculated
      })
    if(is.null(calculated$finance_error))state$build_failure<-NULL
    calculated$finance_loading<-state$busy>0L||length(state$queue)>0L
    calculated$finance_pending<-state$busy+length(state$queue)
    calculated$loading<-calculated$finance_loading||identical(state$teams_status,"loading")
    calculated$teams_status<-state$teams_status;calculated$teams_reason<-state$teams_reason
    calculated$reason<-state$reason %||% ""
    state$last_publish<-as.numeric(Sys.time());state$value(calculated)
  }
  enqueue_teams<-function(){
    if(!state$seed_ready||!is.data.frame(state$teams))return(invisible(NULL))
    ids<-head(unique(as.character(state$teams$teamid %||% character())),100L)
    ids<-setdiff(ids[!is.na(ids)&nzchar(ids)],state$scheduled_teams)
    state$scheduled_teams<-c(state$scheduled_teams,ids)
    state$queue<-c(state$queue,lapply(ids,function(id)list(kind="team",id=id)))
    invisible(NULL)
  }
  enqueue_lineups<-function(){
    if(!state$seed_ready)return(invisible(NULL))
    rounds<-rival_saved_rounds(state$evidence)
    awards<-state$awards$rows
    if(!nrow(rounds)||!is.data.frame(awards)||!nrow(awards)||!"round_id"%in%names(awards))return(invisible(NULL))
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
        fn<-switch(task$kind,saved="get_rival_finance_saved",seed="get_rival_finance_seed",team="get_rival_team_finance_observation",lineup="get_rival_round_lineup_observation")
        args<-if(task$kind=="saved")list(account_id=auth[["userid"]],championship_id=cid,season=state$season)else list(login=auth,championship_id=cid,user_team_id=if(task$kind%in%c("team","lineup"))task$id else tid)
        if(task$kind=="lineup")args<-c(args,list(round_id=task$round,signature=task$signature))
        promises::then(async_adapter_fetch(fn,args,ctx,is_current=function()!state$closed&&identical(generation,state$generation)),function(result){
          if(state$closed||generation!=state$generation)return(NULL)
          state$busy<-state$busy-1L
          tryCatch({
          if(task$kind=="saved"){
            state$evidence<-rival_merge_evidence(state$evidence,result$evidence);state$defaults<-result$defaults;state$finance_ready<-TRUE
            saved_rules<-rival_evidence_rows(state$evidence,"rules")
            if(length(saved_rules)){
              latest<-saved_rules[[which.max(vapply(saved_rules,function(row)as.numeric(fm_time(row$observed_at)),numeric(1)))]]
              state$seed$rules<-normalize_league_rules(list(configuration=latest$payload$configuration),championship_id=cid)
            }
            state$seed$pressroom<-result$pressroom %||% data.frame()
            # Saved data is immediately usable; current sources refresh separately.
            state$queue<-c(list(list(kind="seed")),state$queue);publish(TRUE)
          }else if(task$kind=="seed"){
            if(is.null(result$info))result$rules<-state$seed$rules %||% result$rules
            old_transfers<-state$seed$pressroom %||% data.frame()
            if(is.data.frame(old_transfers)&&nrow(old_transfers)){
              combined<-dplyr::bind_rows(old_transfers,result$pressroom)
              known<-!is.na(combined$id)&nzchar(as.character(combined$id))
              result$pressroom<-combined[!(known&duplicated(combined$id,fromLast=TRUE)),,drop=FALSE]
            }
            state$seed<-result
            inferred<-result$rules$season %||% "unknown"
            if(!identical(inferred,state$season)&&identical(state$season,"unknown")&&inferred!="unknown"){
              state$season<-inferred;state$evidence<-list();state$finance_ready<-FALSE;state$seed_ready<-FALSE
              state$scheduled_teams<-character();state$scheduled_lineups<-character();state$queue<-list(list(kind="saved"));run_next();return(NULL)
            }
            current<-rival_team_evidence(list(team_id=tid,info=result$info,movements=NULL),auth[["userid"]],cid,state$season)
            if(length(current)){state$evidence<-rival_merge_evidence(state$evidence,current);persist_rival_finance_evidence(current,ctx)}
            if(is.data.frame(result$notifications)&&nrow(result$notifications)){
              notification<-result$notifications[result$notifications$championship_id%in%cid,,drop=FALSE]
              record_round_publication_signal(root,auth[["userid"]],cid,notification)
              stamp<-attr(result$notifications,"observed_at")
              if(!is.null(stamp)&&!identical(attr(result$notifications,"fetch_status"),"stale")){
                observed<-lapply(seq_len(nrow(notification)),function(i)rival_finance_evidence(auth[["userid"]],cid,state$season,"notifications",notification$id[i],as.list(notification[i,,drop=FALSE]),stamp))
                persist_rival_finance_evidence(observed,ctx)
              }
            }
            state$finance_ready<-TRUE;state$seed_ready<-TRUE;enqueue_teams();publish(TRUE)
          }else{
            rows<-if(task$kind=="lineup")rival_lineup_evidence(result,auth[["userid"]],cid,state$season)else rival_team_evidence(result,auth[["userid"]],cid,state$season)
            if(length(rows)){state$evidence<-rival_merge_evidence(state$evidence,rows);persist_rival_finance_evidence(rows,ctx)}
            if(task$kind=="lineup"&&!length(rows))state$reason<-paste("Some historical lineups could not be retrieved; affected MVP/Best XI bonuses are not included. Use Refresh to retry.",mvp_text(result$reason))
            if(task$kind=="team")enqueue_lineups()
          }
          },error=function(error){
            failure<-resource_log_failure(resource_failure(error,resource="league_finances_calc",category="schema"))
            state$reason<-paste("Some financial data could not be prepared.",resource_failure_message(failure))
            if(task$kind=="saved"&&!any(vapply(state$queue,function(task)identical(task$kind,"seed"),logical(1))))
              state$queue<-c(list(list(kind="seed")),state$queue)
            if(task$kind=="seed"){state$seed_ready<-TRUE;enqueue_teams()}
          })
          publish(!length(state$queue)&&state$busy==0L);run_next();NULL
        },function(error){
          if(!state$closed&&generation==state$generation){state$busy<-state$busy-1L;state$reason<-"Some financial sources could not be refreshed; saved evidence remains available.";if(task$kind=="saved")state$queue<-list(list(kind="seed"))
            if(task$kind=="seed"){state$seed_ready<-TRUE;enqueue_teams()};publish(TRUE);run_next()};NULL
        })
      })
    }
    invisible(NULL)
  }
  shiny::observe({
    auth<-login()
    if(!valid_login(auth)){
      if(!is.null(state$key)){state$generation<-state$generation+1L;state$key<-NULL;state$evidence<-list();state$queue<-list();state$busy<-0L
        if(!is.null(state$publish_timer)){state$publish_timer();state$publish_timer<-NULL}
        state$identity<-NULL;state$teams<-data.frame();state$finance_ready<-FALSE;state$seed_ready<-FALSE
        state$value(list(team_finances=data.frame(),breakdowns=list(),loading=FALSE))};return()
    }
    if(!isTRUE(active()))return()
    cid<-championship();tid<-user_team();shiny::req(cid,tid);version<-if(is.function(refresh))refresh()else 0
    publication<-if(is.function(root$userData$round_publication_revision))root$userData$round_publication_revision()else 0
    key<-paste(auth[["userid"]],cid,tid,version,publication,sep="|")
    if(!identical(state$key,key)){
      same<-identical(state$identity,paste(auth[["userid"]],cid,tid,sep="|"));state$identity<-paste(auth[["userid"]],cid,tid,sep="|")
      state$key<-key;state$generation<-state$generation+1L;state$busy<-0L;state$season<-league_season_context(cid);state$reason<-"";state$build_failure<-NULL
      if(!is.null(state$publish_timer)){state$publish_timer();state$publish_timer<-NULL}
      state$account<-auth[["userid"]];state$championship<-cid;state$seed_ready<-FALSE;state$scheduled_teams<-character();state$scheduled_lineups<-character()
      if(!same){state$evidence<-list();state$seed<-list();state$defaults<-NULL;state$finance_ready<-FALSE
        state$teams<-data.frame();state$teams_status<-"loading";state$teams_reason<-"";state$awards<-list(rows=data.frame(),complete=FALSE)
        state$value(list(team_finances=data.frame(),breakdowns=list(),loading=TRUE))}
      else invalidate_resources(resource_context(account_id=auth[["userid"]],championship_id=cid,season=state$season),pattern="rival_finance_evidence")
      state$queue<-list(list(kind="saved"));run_next()
    }else run_next()
  },priority=100)
  # Resolve UI dependencies in observers, never in completion callbacks. A pending
  # factual read raises a Shiny validation condition and must not stall the queue.
  shiny::observe({
    shiny::req(isTRUE(active()),valid_login(login()))
    cid<-championship();tid<-user_team()
    shiny::req(identical(state$identity,paste(login()[["userid"]],cid,tid,sep="|")),!is.null(state$key))
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
    enqueue_lineups();publish(isTRUE(state$awards$complete));run_next()
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
