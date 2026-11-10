# Historical ownership is a bounded source observation, not a complete season roster.
# Resolve only an unchanged owner across an evidenced round start/settlement interval.
rival_owner_history_rows <- function(summary) {
  empty<-data.frame(owner_name=character(),changed_at=character(),price=numeric(),stringsAsFactors=FALSE)
  if(!is.list(summary)||!isTRUE(summary$owners_received)||!is.list(summary$owners)||
     !length(summary$owners)||length(summary$owners)>500L)return(empty)
  records<-mvp_records(summary$owners)
  rows<-dplyr::bind_rows(lapply(records,function(row){
    if(!is.list(row)||!all(c("n","d","l")%in%names(row))||!identical(row$l,FALSE))return(NULL)
    name<-mvp_text(row$n);date<-mvp_text(row$d)
    if(!nzchar(name)||is.na(fm_time(date)))return(NULL)
    data.frame(owner_name=name,changed_at=date,price=fm_number(row$p),stringsAsFactors=FALSE)
  }))
  if(nrow(rows)!=length(records)||anyDuplicated(as.numeric(fm_time(rows$changed_at))))return(empty)
  rows<-rows[order(fm_time(rows$changed_at)),,drop=FALSE];rownames(rows)<-NULL
  rows
}
get_rival_player_ownership_observation <- function(login,championship_id,user_team_id,player_id) {
  summary<-get_player_summary(login,championship_id,user_team_id,player_id)
  rows<-rival_owner_history_rows(summary)
  stamp<-attr(summary,"observed_at") %||% Sys.time()
  stale<-identical(attr(summary,"fetch_status"),"stale")
  list(player_id=as.character(player_id),rows=rows,observed_at=rival_finance_stamp(stamp),
    status=if(nrow(rows)&&!stale)"ok"else"unavailable",
    coverage="returned_transitions_only")
}
get_rival_ownership_round_catalog <- function(login,championship_id,user_team_id) {
  result<-get_award_round_catalog(login,championship_id)
  rounds<-result$rounds
  empty<-data.frame(round_id=character(),begin_process=character(),stringsAsFactors=FALSE)
  if(!is.data.frame(rounds)||!all(c("round_id","begin_process")%in%names(rounds)))return(list(status="unavailable",rows=empty))
  rows<-rounds[,c("round_id","begin_process"),drop=FALSE]
  valid<-!is.na(rows$round_id)&nzchar(rows$round_id)&!is.na(fm_time(rows$begin_process))
  rows<-rows[valid,,drop=FALSE]
  if(nzchar(mvp_text(result$league_id))){
    metadata<-get_award_round_metadata(login,championship_id,result$league_id,current_round_id=result$current_round_id)
    # Legacy cached calendars did not retain fixtures; upgrade once through the
    # same single bulk resource, then persist/reuse its versioned dates.
    if(identical(metadata$status,"ok")&&is.null(metadata$fixture_rounds))
      metadata<-get_award_round_metadata(login,championship_id,result$league_id,refresh=TRUE,current_round_id=result$current_round_id)
    if(identical(metadata$status,"ok")&&award_fixture_calendar_valid(metadata$fixture_rounds)){
      fixtures<-metadata$fixture_rounds
      rows<-dplyr::bind_rows(fixtures,rows[!rows$round_id%in%fixtures$round_id,,drop=FALSE])
      attr(result,"observed_at")<-attr(metadata,"observed_at")
    }
  }
  if(anyDuplicated(rows$round_id)||!nrow(rows))return(list(status="unavailable",rows=empty))
  list(status="ok",rows=rows,observed_at=rival_finance_stamp(attr(result,"observed_at") %||% Sys.time()))
}
rival_ownership_observation_evidence <- function(result,account_id,championship_id,season,catalog=FALSE) {
  if(!is.list(result)||!identical(result$status,"ok")||!is.data.frame(result$rows)||!nrow(result$rows))return(list())
  payload<-list(kind=if(catalog)"award_ownership_calendar"else"player_ownership_history",version=1L,
    rows=lapply(seq_len(nrow(result$rows)),function(i)as.list(result$rows[i,,drop=FALSE])),
    coverage=if(catalog)"observed_round_start_times"else"returned_transitions_only")
  if(!catalog)payload$player_id<-mvp_text(result$player_id)
  if(!catalog&&!nzchar(payload$player_id))return(list())
  stamp<-fm_time(result$observed_at)
  if(length(stamp)!=1L||is.na(stamp)||stamp>Sys.time()+60)return(list())
  id<-if(catalog)"award-ownership-calendar"else paste0("player-owners:",payload$player_id)
  list(rival_finance_evidence(account_id,championship_id,season,"eligibility",id,payload,stamp))
}
rival_ownership_evidence_rows <- function(evidence,kind) {
  if(!kind%in%c("player_ownership_history","award_ownership_calendar"))return(list())
  Filter(function(row){
    p<-row$payload
    if(!is.list(p)||!identical(p$kind,kind)||!identical(fm_number(p$version),1)||!is.list(p$rows)||!length(p$rows))return(FALSE)
    expected<-c("kind","version","rows","coverage",if(kind=="player_ownership_history")"player_id")
    # Simplified heterogeneous JSON frames pad absent fields with NULL/NA.
    extra<-p[setdiff(names(p),expected)]
    if(!all(expected%in%names(p))||any(vapply(extra,function(value)!is.null(value)&&
      !(is.atomic(value)&&length(value)==1L&&is.na(value)),logical(1))))return(FALSE)
    count<-length(mvp_records(p$rows))
    if(!count)return(FALSE)
    if(kind=="player_ownership_history"){

        nzchar(mvp_text(p$player_id))&&identical(mvp_text(row$source_id),paste0("player-owners:",mvp_text(p$player_id)))&&
        identical(p$coverage,"returned_transitions_only")&&count<=500L
    }else identical(mvp_text(row$source_id),"award-ownership-calendar")&&
      identical(p$coverage,"observed_round_start_times")&&count<=120L
  },rival_evidence_rows(evidence,"eligibility"))
}
rival_ownership_round_intervals <- function(evidence,catalog,movements) {
  empty<-data.frame(round_id=character(),begins_at=character(),settles_at=character(),stringsAsFactors=FALSE)
  calendars<-rival_ownership_evidence_rows(evidence,"award_ownership_calendar")
  starts<-dplyr::bind_rows(lapply(calendars,function(row)tryCatch(dplyr::bind_rows(lapply(mvp_records(row$payload$rows),function(value)
    data.frame(round_id=mvp_text(value$round_id),begin_process=mvp_text(value$begin_process),stringsAsFactors=FALSE))),error=function(e)NULL)))
  if(!nrow(starts)||!all(c("round_id","begin_process")%in%names(starts))||!nrow(movements))return(empty)
  final<-movements[movements$bonus_type%in%c("ranking","mvp","dreamteam")&is.finite(movements$money)&movements$money>0&
    !is.na(fm_time(movements$date))&nzchar(movements$round_id),,drop=FALSE]
  if(!nrow(final))return(empty)
  dplyr::bind_rows(c(list(empty),lapply(intersect(unique(starts$round_id),unique(final$round_id)),function(id){
    begin<-unique(starts$begin_process[starts$round_id%in%id]);end<-final$date[final$round_id%in%id]
    if(length(begin)!=1L||is.na(fm_time(begin))||!length(end))return(NULL)
    end<-end[which.max(fm_time(end))]
    if(fm_time(begin)>=fm_time(end))return(NULL)
    data.frame(round_id=id,begins_at=begin,settles_at=end,stringsAsFactors=FALSE)
  })))
}
rival_resolve_owner_interval <- function(rows,begins_at,settles_at,observed_at,teams) {
  unknown<-list(known=FALSE,team_id="",unowned=FALSE)
  if(!is.data.frame(rows)||!nrow(rows)||!all(c("owner_name","changed_at")%in%names(rows))||
     !is.data.frame(teams)||!all(c("teamid","teamname")%in%names(teams)))return(unknown)
  begin<-fm_time(begins_at);end<-fm_time(settles_at);observed<-fm_time(observed_at);dates<-fm_time(rows$changed_at)
  if(length(begin)!=1L||length(end)!=1L||length(observed)!=1L||anyNA(c(begin,end,observed,dates))||
     begin>=end||observed<end||observed>Sys.time()+60||any(dates>observed)||anyDuplicated(as.numeric(dates)))return(unknown)
  rows<-rows[order(dates),,drop=FALSE];dates<-sort(dates)
  previous<-which(dates<begin)
  if(!length(previous)||any(dates>=begin&dates<=end))return(unknown)
  index<-tail(previous,1L);name<-mvp_text(rows$owner_name[index])
  # A trailing source interval is bounded by this observation, never by infinity.
  if(index==nrow(rows)&&observed<end)return(unknown)
  if(identical(tolower(name),"futmondo")){
    # A user naming a team after the System makes this name-only source ambiguous.
    if(any(tolower(teams$teamname)%in%"futmondo"))return(unknown)
    return(list(known=TRUE,team_id="",unowned=TRUE))
  }
  candidates<-unique(as.character(teams$teamid[!is.na(teams$teamname)&teams$teamname==name]))
  candidates<-candidates[!is.na(candidates)&nzchar(candidates)]
  if(length(candidates)!=1L)return(unknown)
  list(known=TRUE,team_id=candidates,unowned=FALSE)
}
rival_ownership_eligibility <- function(evidence,teams,official,rounds,catalog,movements) {
  empty<-data.frame(round_id=character(),player_id=character(),team_id=character(),eligible=logical(),verified=logical(),stringsAsFactors=FALSE)
  if(!is.data.frame(official)||!all(c("round_id","player_id")%in%names(official))||!nrow(official))return(empty)
  intervals<-rival_ownership_round_intervals(evidence,catalog,movements)
  if(!nrow(intervals))return(empty)
  histories<-rival_ownership_evidence_rows(evidence,"player_ownership_history")
  dplyr::bind_rows(c(list(empty),lapply(histories,function(record){
    p<-record$payload;rows<-tryCatch(dplyr::bind_rows(mvp_records(p$rows)),error=function(e)NULL)
    awards<-unique(official[official$player_id%in%mvp_text(p$player_id),c("round_id","player_id"),drop=FALSE])
    dplyr::bind_rows(lapply(seq_len(nrow(awards)),function(i){
      interval<-intervals[intervals$round_id%in%awards$round_id[i],,drop=FALSE]
      if(nrow(interval)!=1L)return(NULL)
      owner<-rival_resolve_owner_interval(rows,interval$begins_at,interval$settles_at,record$observed_at,teams)
      if(!isTRUE(owner$known))return(NULL)
      own_round<-rounds[rounds$round_id%in%awards$round_id[i]&rounds$team_id%in%owner$team_id,,drop=FALSE]
      if(!owner$unowned&&(nrow(own_round)!=1L||is.na(own_round$negative)))return(NULL)
      data.frame(round_id=awards$round_id[i],player_id=awards$player_id[i],team_id=owner$team_id,
        eligible=!owner$unowned&&!isTRUE(own_round$negative[1]),verified=TRUE,stringsAsFactors=FALSE)
    }))
  })))
}
