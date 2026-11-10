#' Identify rounds with explicit outstanding final settlement evidence
#' @param evidence Account/league/season-filtered saved financial evidence.
#' @param catalog Round IDs and their authoritative numbers.
#' @param movements Normalized recorded money movements from the same context.
#' @param now Current observation time; never used as an invented payment date.
#' @return Numeric round numbers pending final bonuses. Points remain unchanged.
#' @keywords internal
rival_pending_settlement_rounds <- function(evidence,catalog,movements,now=Sys.time()) {
  if(!is.data.frame(catalog)||!all(c("round_id","round_number")%in%names(catalog)))return(numeric())
  now<-fm_time(now)
  if(length(now)!=1L||is.na(now))return(numeric())
  paid<-data.frame(round_id=character(),at=numeric())
  if(is.data.frame(movements)&&all(c("round_id","bonus_type","money","date")%in%names(movements))){
    valid<-movements$bonus_type%in%c("ranking","mvp","dreamteam")&is.finite(movements$money)&movements$money>0
    at<-fm_time(movements$date)
    valid<-valid&!is.na(at)&at<=now
    paid<-data.frame(round_id=movements$round_id[valid],at=as.numeric(at[valid]))
  }
  pending<-character()
  for(record in rival_evidence_rows(evidence,"eligibility")){
    p<-record$payload;stamp<-fm_time(record$observed_at)
    if(!is.list(p)||!identical(p$kind,"round_settlement_status")||!identical(fm_number(p$version),1)||
       !identical(p$status,"pending")||!isTRUE(p$verified)||
       !identical(mvp_text(record$source_id),paste0("round-settlement:",mvp_text(p$round_id)))||
       length(stamp)!=1L||is.na(stamp)||stamp>now+60)next
    number<-unique(catalog$round_number[catalog$round_id%in%mvp_text(p$round_id)])
    if(length(number)!=1L||!is.finite(number)||number!=fm_number(p$round_number))next
    # A later actual final-category payment supersedes a pending observation.
    # Points, notifications and the passage of time do not establish settlement.
    if(!any(paid$round_id%in%p$round_id & paid$at>as.numeric(stamp)))pending<-c(pending,p$round_id)
  }
  for(record in rival_ownership_evidence_rows(evidence,"award_ownership_calendar")){
    stamp<-fm_time(record$observed_at)
    if(length(stamp)!=1L||is.na(stamp)||stamp>now+60)next
    for(row in mvp_records(record$payload$rows)){
      if(!is.list(row)||!is.character(unlist(row$fixture_dates,use.names=FALSE)))next
      dates<-fm_time(unlist(row$fixture_dates,use.names=FALSE))
      if(!length(dates)||anyNA(dates)||!any(dates>stamp))next
      last<-max(dates);rid<-mvp_text(row$round_id)
      if(last>now||!any(paid$round_id%in%rid & paid$at>=as.numeric(last)))pending<-c(pending,rid)
    }
  }
  values<-catalog$round_number[catalog$round_id%in%pending]
  sort(unique(values[is.finite(values)&values>=1&values==floor(values)]))
}
