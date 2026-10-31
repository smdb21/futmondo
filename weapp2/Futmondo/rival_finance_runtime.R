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
  list(account_id=as.character(account_id),championship_id=as.character(championship_id),season=mvp_text(season,"unknown"),
    evidence_type=type,source_id=as.character(id),observed_at=rival_finance_stamp(observed_at),payload=payload)
}
rival_evidence_rows <- function(evidence,type) {
  if(!is.list(evidence))return(list())
  Filter(function(x)is.list(x)&&identical(x$evidence_type,type),evidence)
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
      money=fm_number(p$money),date=mvp_text(p$date),bonus_type=mvp_text(p$bonus_type),round_id=mvp_text(p$round_id),player_id=mvp_text(p$player_id),observed_at=mvp_text(x$observed_at),stringsAsFactors=FALSE)
  }))
  if(!all(c("id","money")%in%names(rows)))return(data.frame())
  rows<-rows[order(fm_time(rows$observed_at)),,drop=FALSE]
  rows[!duplicated(rows$id,fromLast=TRUE),,drop=FALSE]
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
  events<-unique(events);out<-list();missing<-if(unfinished)"Unfinished or unverified award selections are excluded from credited cash."else character()
  for(i in seq_len(nrow(events))){
    e<-events[i,,drop=FALSE];rule<-round_rules[[e$round_id]] %||% rules
    amount<-fm_number(if(e$bonus_type=="mvp")rule$mvp_reward else rule$dreamteam_reward)
    if(is.finite(amount)&&amount==0)next
    same<-if(nrow(payments)&&all(c("round_id","player_id","bonus_type")%in%names(payments)))payments$round_id%in%e$round_id & payments$player_id%in%e$player_id & payments$bonus_type%in%e$bonus_type else logical()
    if(any(same))next
    evidence<-if(is.data.frame(eligibility)&&all(c("round_id","player_id","team_id","eligible","verified")%in%names(eligibility)))eligibility[eligibility$round_id%in%e$round_id & eligibility$player_id%in%e$player_id & eligibility$eligible%in%TRUE & eligibility$verified%in%TRUE,,drop=FALSE]else data.frame()
    if(nrow(evidence)!=1L||!is.finite(amount)){missing<-c(missing,"Historical award ownership/eligibility or reward rules unavailable.");next}
    if(!identical(as.character(evidence$team_id),as.character(team_id)))next
    e$amount<-amount;e$reason<-if(isTRUE(rule$historical))"Historical rule; payment not observed."else"Assumes current rule; payment not observed."
    out[[length(out)+1L]]<-e
  }
  list(rows=dplyr::bind_rows(c(list(empty),out)),missing=unique(missing))
}
# Public pure accounting interface; every numeric subtotal has an evidence trail.
build_rival_cash_dataset <- function(teams,championship_id,evidence=list(),official=data.frame(),pressroom=data.frame(),rules=list(),opening_defaults=NULL,eligibility=data.frame(),coverage_complete=FALSE,now=Sys.time(),account_id=NULL,season="unknown") {
  empty<-data.frame(teamid=character(),teamname=character(),estimated_cash=numeric(),confidence=character(),as_of=character(),stringsAsFactors=FALSE)
  if(!is.data.frame(teams)||!nrow(teams))return(list(team_finances=empty,breakdowns=list(),all_purchases=data.frame()))
  if(!nzchar(mvp_text(account_id)))stop("An explicit account is required for rival finance evidence.")
  evidence<-Filter(function(x)identical(mvp_text(x$championship_id),as.character(championship_id))&&identical(mvp_text(x$season,"unknown"),as.character(season))&&(is.null(account_id)||identical(mvp_text(x$account_id),as.character(account_id))),rival_merge_evidence(list(),evidence))
  scope_frame<-function(rows){
    if(!is.data.frame(rows))return(data.frame())
    expected<-list(championship_id=championship_id,season=season,account_id=account_id)
    for(field in intersect(names(rows),names(expected)))if(!is.null(expected[[field]]))rows<-rows[rows[[field]]%in%expected[[field]],,drop=FALSE]
    rows
  }
  official<-scope_frame(official);eligibility<-scope_frame(eligibility)
  if(!nrow(eligibility))eligibility<-dplyr::bind_rows(lapply(rival_evidence_rows(evidence,"eligibility"),function(row){
    p<-row$payload
    if(!isTRUE(p$verified)||!all(c("round_id","player_id","team_id","eligible")%in%names(p)))return(NULL)
    data.frame(round_id=mvp_text(p$round_id),player_id=mvp_text(p$player_id),team_id=mvp_text(p$team_id),eligible=isTRUE(p$eligible),verified=TRUE)
  }))
  opening<-rival_opening_cash(championship_id,rules,opening_defaults);result<-list();breakdowns<-list()
  for(i in seq_len(nrow(teams))){
    team<-teams[i,,drop=FALSE];tid<-as.character(team$teamid %||% team$id);name<-as.character(team$teamname %||% team$name %||% tid)
    mov<-rival_cash_latest_movements(evidence,tid)
    own<-Filter(function(x)identical(mvp_text(x$source_id),tid),rival_evidence_rows(evidence,"team"));info<-if(length(own))tail(own,1)[[1]]$payload else list()
    coverage<-Filter(function(x)identical(mvp_text(x$source_id),tid),rival_evidence_rows(evidence,"coverage"));coverage<-if(length(coverage))tail(coverage,1)[[1]]$payload else list()
    actual<-fm_number(info$budget);stamp<-mvp_text(info$observed_at);fresh<-is.finite(actual)&&!is.na(fm_time(stamp))&&as.numeric(difftime(now,fm_time(stamp),units="secs"))>=-60&&as.numeric(difftime(now,fm_time(stamp),units="secs"))<=300
    details<-data.frame(component="Opening cash",amount=opening$amount,source=opening$source,round_id="",player_id="",stringsAsFactors=FALSE)
    missing<-character();assumptions<-character();recorded<-0;point_income<-ranking_income<-ideal_income<-mvp_income<-0
    spent<-sales<-0
    # A source with unknown payment metadata can still establish the signed cash movement.
    complete_movements<-isTRUE(coverage$complete)&&isTRUE(coverage$history_complete)&&nrow(mov)>0L&&all(is.finite(mov$money))&&all(nzchar(mov$id))
    if(nrow(mov)){
      # An explicit opening allocation replaces the separate starting amount, never adds twice.
      budget<-which(mov$type%in%"budget");valid_budget<-budget[is.finite(mov$money[budget])]
      if(length(valid_budget)==1L){details$amount[1]<-mov$money[valid_budget];details$source[1]<-"Recorded opening allocation"}
      if(length(valid_budget)>1L){complete_movements<-FALSE;missing<-c(missing,"Multiple budget allocations require season/reset evidence.")}
      nonbudget<-mov[!mov$type%in%"budget",,drop=FALSE]
      if(nrow(nonbudget)){
        rows<-data.frame(component=ifelse(nzchar(nonbudget$concept),nonbudget$concept,nonbudget$type),amount=nonbudget$money,source="Recorded movement",round_id=nonbudget$round_id %||% "",player_id=nonbudget$player_id %||% "",stringsAsFactors=FALSE)
        details<-dplyr::bind_rows(details,rows);recorded<-sum(nonbudget$money[is.finite(nonbudget$money)])
        spent<--sum(nonbudget$money[nonbudget$type%in%"buy" & is.finite(nonbudget$money)]);sales<-sum(nonbudget$money[nonbudget$type%in%"sell" & is.finite(nonbudget$money)])
      }
    }
    # Prefer the account movement ledger as a whole: adding a second transfer feed
    # without common source identities would double count purchases and sales.
    if(!nrow(mov)){
      tx<-pressroom
      if(is.data.frame(tx)&&all(c("buyer_team_id","seller_team_id","price")%in%names(tx))){
        if("id"%in%names(tx))tx<-tx[!duplicated(tx$id),,drop=FALSE]
        tx<-tx[tx$buyer_team_id%in%tid|tx$seller_team_id%in%tid,,drop=FALSE]
        if(nrow(tx)){
          money<-ifelse(tx$seller_team_id%in%tid,1,-1)*suppressWarnings(as.numeric(tx$price))
          details<-dplyr::bind_rows(details,data.frame(component=ifelse(money>=0,"Player sale","Player purchase"),amount=money,source="Pressroom transfer",round_id="",player_id=as.character(tx$player_id %||% "")))
          spent<--sum(money[money<0],na.rm=TRUE);sales<-sum(money[money>0],na.rm=TRUE)
        }
      }
      missing<-c(missing,"Financial movements unavailable; transfers and unobserved income may be incomplete.")
    }else if(!complete_movements)missing<-c(missing,"Movement history completeness is unverified; secondary transfer feed was not added twice.")
    # Any unclassified reward can already include these bonuses. Suppress estimated
    # rewards until it can be matched, rather than count an unidentified payment twice.
    unclassified<-nrow(mov)&&any(!mov$type%in%c("buy","sell","budget") & !mov$bonus_type%in%c("points","ranking","dreamteam","mvp"))
    estimate<-rival_award_estimates(official,tid,rules,eligibility,mov)
    missing<-c(missing,estimate$missing)
    if(unclassified&&nrow(estimate$rows)){missing<-c(missing,"Unclassified recorded income prevents adding potentially overlapping award estimates.");estimate$rows<-estimate$rows[FALSE,,drop=FALSE]}
    if(nrow(estimate$rows)){
      details<-dplyr::bind_rows(details,data.frame(component=ifelse(estimate$rows$bonus_type=="mvp","MVP reward","Ideal XI reward"),amount=estimate$rows$amount,source=estimate$rows$reason,round_id=estimate$rows$round_id,player_id=estimate$rows$player_id))
      ideal_income<-sum(estimate$rows$amount[estimate$rows$bonus_type=="dreamteam"]);mvp_income<-sum(estimate$rows$amount[estimate$rows$bonus_type=="mvp"]);assumptions<-c(assumptions,estimate$rows$reason)
    }
    if(nrow(mov)){
      point_income<-sum(mov$money[mov$bonus_type%in%"points"],na.rm=TRUE);ranking_income<-sum(mov$money[mov$bonus_type%in%"ranking"],na.rm=TRUE)
      ideal_income<-ideal_income+sum(mov$money[mov$bonus_type%in%"dreamteam"],na.rm=TRUE);mvp_income<-mvp_income+sum(mov$money[mov$bonus_type%in%"mvp"],na.rm=TRUE)
    }
    round_income<-rival_round_income_estimates(evidence,tid,rules,mov)
    missing<-c(missing,round_income$missing)
    if(unclassified&&nrow(round_income$rows)){
      missing<-c(missing,"Unclassified recorded income prevents adding potentially overlapping point/ranking estimates.")
    }else if(nrow(round_income$rows)){
      details<-dplyr::bind_rows(details,round_income$rows)
      point_income<-point_income+sum(round_income$rows$amount[round_income$rows$component=="Point reward"])
      ranking_income<-ranking_income+sum(round_income$rows$amount[round_income$rows$component=="Ranking reward"])
      assumptions<-c(assumptions,round_income$rows$source)
    }
    if(!complete_movements){
      if(!is.finite(fm_number(rules$money_per_point))||fm_number(rules$money_per_point)!=0)missing<-c(missing,"Points totals do not establish credited point payments.")
      if(!is.finite(fm_number(rules$ranking_pool))||fm_number(rules$ranking_pool)!=0)missing<-c(missing,"Historical round-ranking payments or verified allocation rules unavailable.")
    }
    if(!is.finite(details$amount[1]))missing<-c(missing,"Opening cash unavailable.")
    if(!isTRUE(coverage_complete))missing<-c(missing,"Round award coverage is incomplete.")
    reconstructed<-if(is.finite(details$amount[1]))sum(details$amount,na.rm=TRUE)else NA_real_
    difference<-if(is.finite(actual)&&is.finite(reconstructed))actual-reconstructed else NA_real_
    confidence<-if(fresh)"Verified"else if(complete_movements&&!length(missing)&&!length(assumptions))"Reconstructed"else"Estimated"
    value<-if(fresh)actual else reconstructed
    reasons<-unique(c(missing,assumptions));if(is.finite(difference)&&abs(difference)>0.5)reasons<-c(reasons,paste0("Recorded cash differs from accounting by ",format(difference,big.mark=".",decimal.mark=",",scientific=FALSE)," EUR; no balancing adjustment added."))
    if(!nzchar(stamp)){
      times<-vapply(evidence,function(x)mvp_text(x$observed_at),character(1));times<-times[!is.na(fm_time(times))]
      stamp<-if(length(times))times[which.max(fm_time(times))]else""
    }
    result[[i]]<-data.frame(teamid=tid,teamname=name,estimated_cash=value,confidence=confidence,as_of=stamp,
      cash_status=if(!is.finite(value))"Unavailable"else if(value<0)if(fresh)"Confirmed debt"else"Estimated negative cash"else"Positive or zero cash",
      initial_budget=details$amount[1],budget=if(fresh)actual else NA_real_,reconstructed_cash=reconstructed,reconciliation_difference=difference,
      total_spent=spent,total_sales=sales,point_bonus=point_income,ranking_prize=ranking_income,dreamteam_income=ideal_income,mvp_income=mvp_income,
      team_value=fm_number(info$teamValue %||% team$teamValue),points=fm_number(info$points %||% team$points),
      squad_size=fm_number(info$squad_size),net_profit_loss=NA_real_,estimate_notes=paste(reasons,collapse=" "),stringsAsFactors=FALSE)
    breakdowns[[tid]]<-details
  }
  list(team_finances=dplyr::bind_rows(result),breakdowns=breakdowns,all_purchases=data.frame())
}

get_rival_finance_seed <- function(login,championship_id,user_team_id) {
  info<-tryCatch(get_user_team_info(login,championship_id,user_team_id),error=function(e)NULL)
  list(info=info,rules=normalize_league_rules(info,championship_id=championship_id),
    pressroom=tryCatch(get_championship_pressroom(login,championship_id),error=function(e)data.frame()),
    notifications=tryCatch(get_notifications(login),error=function(e)NULL))
}
get_rival_team_finance_observation <- function(login,championship_id,user_team_id) {
  list(team_id=as.character(user_team_id),info=tryCatch(get_user_team_info(login,championship_id,user_team_id),error=function(e)NULL),
    movements=tryCatch(get_user_team_moneymovements(login,championship_id,user_team_id),error=function(e)NULL))
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
    state$awards<-list(rows=data.frame(),complete=FALSE);state$finance_ready<-FALSE;state$seed_ready<-FALSE;state$scheduled_teams<-character()
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
    if(state$finance_ready)calculated<-build_rival_cash_dataset(state$teams,state$championship,state$evidence,state$awards$rows,
      state$seed$pressroom %||% data.frame(),state$seed$rules %||% list(),state$defaults,
      coverage_complete=isTRUE(state$awards$complete),account_id=state$account,season=state$season)
    calculated$finance_loading<-state$busy>0L||length(state$queue)>0L
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
  run_next<-function(){
    if(state$closed||!isTRUE(shiny::isolate(active())))return(invisible(NULL))
    auth<-shiny::isolate(login());cid<-shiny::isolate(championship());tid<-shiny::isolate(user_team());if(!valid_login(auth)||is.null(cid))return(invisible(NULL))
    generation<-state$generation;ctx<-resource_context(auth,cid,tid,season=state$season,session_id=root$token)
    while(state$busy<2L&&length(state$queue)){
      task<-state$queue[[1]];state$queue<-state$queue[-1];state$busy<-state$busy+1L
      local({task<-task
        fn<-switch(task$kind,saved="get_rival_finance_saved",seed="get_rival_finance_seed",team="get_rival_team_finance_observation")
        args<-if(task$kind=="saved")list(account_id=auth[["userid"]],championship_id=cid,season=state$season)else list(login=auth,championship_id=cid,user_team_id=if(task$kind=="team")task$id else tid)
        promises::then(async_adapter_fetch(fn,args,ctx,is_current=function()!state$closed&&identical(generation,state$generation)),function(result){
          if(state$closed||generation!=state$generation)return(NULL)
          state$busy<-state$busy-1L
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
              result$pressroom<-combined[!duplicated(combined$id,fromLast=TRUE),,drop=FALSE]
            }
            state$seed<-result
            inferred<-result$rules$season %||% "unknown"
            if(!identical(inferred,state$season)&&identical(state$season,"unknown")&&inferred!="unknown"){
              state$season<-inferred;state$evidence<-list();state$finance_ready<-FALSE;state$seed_ready<-FALSE
              state$scheduled_teams<-character();state$queue<-list(list(kind="saved"));run_next();return(NULL)
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
            rows<-rival_team_evidence(result,auth[["userid"]],cid,state$season)
            if(length(rows)){state$evidence<-rival_merge_evidence(state$evidence,rows);persist_rival_finance_evidence(rows,ctx)}
          }
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
      state$key<-key;state$generation<-state$generation+1L;state$busy<-0L;state$season<-league_season_context(cid);state$reason<-""
      if(!is.null(state$publish_timer)){state$publish_timer();state$publish_timer<-NULL}
      state$account<-auth[["userid"]];state$championship<-cid;state$seed_ready<-FALSE;state$scheduled_teams<-character()
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
    publish(isTRUE(state$awards$complete))
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
      paid<-nrow(movements)&&all(c("bonus_type","round_id")%in%names(movements))&&any(movements$bonus_type%in%type & movements$round_id%in%round)
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
