# Basketball is outside this application's football data scope. Unknown sports
# remain unknown; names and competition labels are never used to infer sport.
fm_basketball_sport <- function(sport) {
  tolower(trimws(fm_scalar(sport,""))) %in% c("basket","basketball","baloncesto")
}
fm_supported_leagues <- function(championships,leagues=list()) {
  Filter(function(champ){
    if(!is.list(champ))return(FALSE)
    if(fm_basketball_sport(champ$sport))return(FALSE)
    lid<-if(is.list(champ$league))fm_scalar(champ$league$id %||% champ$league$`_id`,"")else fm_scalar(champ$league,"")
    matched<-Filter(function(x)is.list(x)&&identical(fm_scalar(x$id %||% x$`_id`,""),lid),leagues)
    !any(vapply(matched,function(x)fm_basketball_sport(x$sport),logical(1)))
  },championships)
}
fm_filter_league_answer <- function(answer) {
  if(is.list(answer)&&is.list(answer$championships))answer$championships<-fm_supported_leagues(answer$championships,answer$leagues %||% list())
  answer
}

# Authoritative, account-scoped competition metadata and observed club membership.
# No authentication or private response bodies are retained in this store.
.fm_league_contexts <- new.env(parent=emptyenv())
.fm_league_contexts$items <- list()
.fm_league_contexts$publications <- list()
.fm_league_contexts$revision <- shiny::reactiveVal(0L)
league_context_revision <- function() .fm_league_contexts$revision()

fm_context_account <- function(account_id=NULL) {
  account<-fm_scalar(account_id,"")
  if(nzchar(account))return(account)
  request<-get0(".futmondo_request_context",inherits=TRUE)
  account<-fm_scalar(request$account_id,"")
  if(nzchar(account))return(account)
  domain<-shiny::getDefaultReactiveDomain()
  if(!is.null(domain))account<-fm_scalar(domain$rootScope()$userData$futmondo_user_id,"")
  account
}
fm_context_known <- function(value) length(value)==1L&&!is.na(value)&&nzchar(value)&&value!="unknown"
fm_league_context_key <- function(account,championship) paste(account,championship,sep="\034")
league_context_metadata <- function(championship_id,account_id=NULL,allow_stale=FALSE) {
  account<-fm_context_account(account_id);championship<-fm_scalar(championship_id,"")
  if(!nzchar(account)||!nzchar(championship))return(NULL)
  item<-.fm_league_contexts$items[[fm_league_context_key(account,championship)]]
  if(is.null(item)||(!allow_stale&&as.numeric(fm_time(item$expires_at))<=as.numeric(Sys.time())))return(NULL)
  scoring_deadline<-fm_time(item$payload$scoring_expires_at %||% item$expires_at)
  if(length(scoring_deadline)!=1L||is.na(scoring_deadline)||scoring_deadline<=Sys.time())item$scoring_version<-"unknown"
  item
}
league_competition_context <- function(championship_id,account_id=NULL) {
  fm_scalar(league_context_metadata(championship_id,account_id)$competition_id,"unknown")
}
remember_league_context <- function(row) {
  fields<-c("account_id","championship_id","competition_id","season","scoring_version","observed_at","expires_at","complete","payload")
  if(!is.list(row)||!all(fields%in%names(row))||
     !all(vapply(row[c("account_id","championship_id")],function(x)nzchar(fm_scalar(x,"")),logical(1))))return(FALSE)
  stamp<-fm_time(row$observed_at);expiry<-fm_time(row$expires_at)
  if(length(stamp)!=1L||is.na(stamp)||length(expiry)!=1L||is.na(expiry)||expiry<stamp||stamp>Sys.time()+60)return(FALSE)
  row<-row[fields]
  if(as.numeric(object.size(row))>1024^2)return(FALSE)
  key<-fm_league_context_key(row$account_id,row$championship_id)
  old<-.fm_league_contexts$items[[key]]
  if(!is.null(old)&&fm_time(old$observed_at)>stamp)return(FALSE)
  # A compact unchanged metadata response must not erase known scoring settings.
  if(!is.null(old)&&identical(row$competition_id,old$competition_id)&&identical(row$season,old$season)&&
     !fm_context_known(row$scoring_version)&&fm_context_known(old$scoring_version)&&fm_time(old$expires_at)>Sys.time()&&
     fm_time(old$payload$scoring_expires_at %||% old$expires_at)>Sys.time()){
    row$scoring_version<-old$scoring_version
    row$payload$point_system<-old$payload$point_system;row$payload$custom<-old$payload$custom
    row$payload$scoring_observed_at<-old$payload$scoring_observed_at %||% old$observed_at
    row$payload$scoring_expires_at<-old$payload$scoring_expires_at %||% old$expires_at
  }
  changed<-is.null(old)||(!is.null(old)&&fm_time(old$expires_at)<=Sys.time()&&expiry>Sys.time())||!identical(old[c("competition_id","season","scoring_version")],row[c("competition_id","season","scoring_version")])
  items<-.fm_league_contexts$items
  items[[key]]<-NULL;items[[key]]<-row
  while(length(items)>128L||sum(vapply(items,function(x)as.numeric(object.size(x)),numeric(1)))>8*1024^2)items<-items[-1L]
  .fm_league_contexts$items<-items
  if(changed)shiny::isolate(.fm_league_contexts$revision(.fm_league_contexts$revision()+1L))
  invisible(TRUE)
}
clear_league_contexts <- function(account_id) {
  account<-fm_scalar(account_id,"")
  .fm_league_contexts$items<-Filter(function(x)!identical(x$account_id,account),.fm_league_contexts$items)
  .fm_league_contexts$publications[[account]]<-NULL
  shiny::isolate(.fm_league_contexts$revision(.fm_league_contexts$revision()+1L));invisible(TRUE)
}
normalize_active_league_contexts <- function(answer,account_id,observed_at=Sys.time()) {
  records<-function(x)if(is.data.frame(x))lapply(seq_len(nrow(x)),function(i)as.list(x[i,,drop=FALSE]))else if(is.list(x))x else list()
  answer<-fm_filter_league_answer(answer)
  leagues<-records(answer$leagues)
  Filter(Negate(is.null),lapply(records(answer$championships),function(champ){
    if(!is.list(champ))return(NULL)
    cid<-fm_scalar(champ$id %||% champ$`_id`,"")
    lid<-if(is.list(champ$league))fm_scalar(champ$league$id %||% champ$league$`_id`,"")else fm_scalar(champ$league,"")
    if(!nzchar(cid))return(NULL)
    matched<-Filter(function(x)is.list(x)&&identical(fm_scalar(x$id %||% x$`_id`,""),lid),leagues)
    league<-if(length(matched)==1L)matched[[1]]else list()
    season<-fm_scalar(league$season,"unknown");competition<-if(length(matched)==1L&&nzchar(lid))lid else "unknown"
    scoring<-fm_scoring_version(champ$pointSystem %||% champ$ps,champ$custom)
    stamp<-fm_time(observed_at)
    list(account_id=as.character(account_id),championship_id=cid,competition_id=competition,season=season,
      scoring_version=scoring,observed_at=format(stamp,"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC"),
      expires_at=format(stamp+300,"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC"),
      complete=fm_context_known(competition)&&fm_context_known(season),
      payload=list(version=2L,championship_name=fm_scalar(champ$name,""),competition_name=fm_scalar(league$name,""),
        country=fm_scalar(champ$country %||% league$country,""),sport=fm_scalar(champ$sport,""),
        point_system=champ$pointSystem %||% champ$ps,custom=champ$custom,source="activechampionships",
        scoring_observed_at=if(fm_context_known(scoring))format(stamp,"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC")else NULL,
        scoring_expires_at=if(fm_context_known(scoring))format(stamp+300,"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC")else NULL))
  }))
}
publish_active_league_contexts <- function(login,answer) {
  if(!valid_login(login)||!is.list(answer)||identical(attr(answer,"fetch_status"),"stale"))return(invisible(FALSE))
  rows<-normalize_active_league_contexts(answer,login[["userid"]],attr(answer,"observed_at") %||% Sys.time())
  account<-as.character(login[["userid"]]);signature<-unclass(as.character(openssl::sha256(serialize(rows,NULL))))
  if(!identical(.fm_league_contexts$publications[[account]],signature)){
    for(row in rows)remember_league_context(row)
    .fm_league_contexts$publications[[account]]<-signature
    if(length(.fm_league_contexts$publications)>128L).fm_league_contexts$publications<-tail(.fm_league_contexts$publications,128L)
  }
  invisible(rows)
}
remember_league_scoring <- function(context,lineup,observed_at=Sys.time()) {
  row<-league_context_metadata(context$championship_id,context$account_id)
  if(is.null(row)||!is.list(lineup)||identical(attr(lineup,"fetch_status"),"stale"))return(NULL)
  version<-fm_scoring_version(lineup$pointSystem,lineup$custom)
  if(!fm_context_known(version))return(NULL)
  row$scoring_version<-version;row$payload$point_system<-lineup$pointSystem;row$payload$custom<-lineup$custom
  row$observed_at<-format(fm_time(observed_at),"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC")
  row$payload$scoring_observed_at<-row$observed_at
  row$payload$scoring_expires_at<-format(fm_time(observed_at)+300,"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC")
  # The scoring read cannot renew the active-league metadata's original expiry.
  if(isTRUE(remember_league_context(row)))row else NULL
}
persist_league_context <- function(row,context) {
  if(is.null(row)||!isTRUE(row$complete))return(invisible(FALSE))
  defer_persistence("supabase_post_direct",list("league_context_observations",list(row),
    "account_id,championship_id,competition_id,season,scoring_version,observed_at",TRUE),context)
}
get_persisted_league_context <- function(account_id,championship_id,competition_id,season) {
  # Called only after authenticated membership and competition/season are resolved.
  if(!all(vapply(list(competition_id,season),fm_context_known,logical(1))))return(NULL)
  query<-list(account_id=paste0("eq.",account_id),championship_id=paste0("eq.",championship_id),
    competition_id=paste0("eq.",competition_id),season=paste0("eq.",season),scoring_version="neq.unknown",complete="is.true",
    expires_at=paste0("gt.",format(Sys.time(),"%Y-%m-%dT%H:%M:%OSZ",tz="UTC")),
    select="account_id,championship_id,competition_id,season,scoring_version,observed_at,expires_at,complete,payload",order="observed_at.desc",limit=1L)
  query[["payload->>scoring_expires_at"]]<-paste0("gt.",format(Sys.time(),"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC"))
  rows<-supabase_get("league_context_observations",query)
  if(!is.data.frame(rows)||nrow(rows)!=1L)return(NULL)
  row<-lapply(rows,function(x)if(is.data.frame(x))as.list(x[1,,drop=FALSE])else if(is.list(x))x[[1]]else x[1])
  if(!all(vapply(c("account_id","championship_id","competition_id","season"),function(k)
    identical(fm_scalar(row[[k]],""),as.character(get(k))),logical(1)))||!isTRUE(row$complete)||
    !is.list(row$payload)||!identical(as.integer(row$payload$version),2L)||
    !fm_context_known(row$scoring_version)||
    !identical(fm_scoring_version(row$payload$point_system,row$payload$custom),row$scoring_version)||
    fm_time(row$payload$scoring_expires_at %||% row$expires_at)<=Sys.time()||
    fm_time(row$expires_at)<=Sys.time())return(NULL)
  observed<-fm_time(row$observed_at);expires<-fm_time(row$expires_at)
  scoring_observed<-fm_time(row$payload$scoring_observed_at %||% row$observed_at)
  scoring_expires<-fm_time(row$payload$scoring_expires_at %||% row$expires_at)
  if(anyNA(c(observed,expires,scoring_observed,scoring_expires))||observed>Sys.time()+60||
     scoring_observed>Sys.time()+60||expires<observed||expires>observed+300.01||
     scoring_expires<scoring_observed||scoring_expires>scoring_observed+300.01)return(NULL)
  current<-league_context_metadata(championship_id,account_id)
  if(!is.null(current)&&fm_context_known(current$scoring_version)&&!identical(current$scoring_version,row$scoring_version))return(NULL)
  if(!is.null(current)&&fm_time(current$observed_at)>observed){
    # Newer membership metadata may omit scoring. Supplement its component
    # evidence rather than dropping valid saved configuration as an older row.
    current$scoring_version<-row$scoring_version
    for(field in c("point_system","custom","scoring_observed_at","scoring_expires_at"))current$payload[[field]]<-row$payload[[field]]
    row<-current
  }
  if(!isTRUE(remember_league_context(row)))return(NULL)
  row
}
get_resolved_league_context <- function(login,championship_id,user_team_id=NULL) {
  active<-get_active_championships(login)
  ids<-vapply(active$championships,function(x)fm_scalar(x$id %||% x$`_id`,""),character(1))
  if(!as.character(championship_id)%in%ids)stop("League membership could not be verified")
  rows<-publish_active_league_contexts(login,active)
  # A bounded accelerator must not make a source-present league inaccessible.
  selected<-Filter(function(row)identical(row$championship_id,as.character(championship_id)),rows)
  if(length(selected)==1L)remember_league_context(selected[[1]])
  row<-league_context_metadata(championship_id,login[["userid"]])
  if(is.null(row)||!isTRUE(row$complete))stop("The source competition and season could not be resolved for this league")
  if(!fm_context_known(row$scoring_version)){
    saved<-tryCatch(get_persisted_league_context(login[["userid"]],championship_id,row$competition_id,row$season),error=function(e)NULL)
    if(!is.null(saved))row<-saved
  }
  resource_context(login,championship_id,user_team_id,season=row$season,scoring_version=row$scoring_version,competition_id=row$competition_id)
}
normalize_player_memberships <- function(players,context,source="futmondo_catalog") {
  empty<-data.frame(player_id=character(),competition_id=character(),season=character(),real_club_id=character(),observed_at=character(),effective_at=character(),source=character())
  if(!is.data.frame(players)||!nrow(players)||!all(c("id","teamId")%in%names(players))||
     !all(vapply(context[c("competition_id","season")],fm_context_known,logical(1)))||
     identical(attr(players,"fetch_status"),"stale"))return(empty)
  players<-preserve_observation_time(players)
  rows<-data.frame(player_id=as.character(players$id),competition_id=context$competition_id,season=context$season,
    real_club_id=as.character(players$teamId),observed_at=as.character(players$observed_at),
    effective_at=NA_character_,source=source,stringsAsFactors=FALSE)
  rows$real_club_id[is.na(rows$real_club_id)|!nzchar(trimws(rows$real_club_id))]<-NA_character_
  rows<-rows[!is.na(rows$player_id)&nzchar(rows$player_id)&!is.na(fm_time(rows$observed_at)),,drop=FALSE]
  unique(rows)
}

league_persistence_scope <- function(championship_id) {
  ctx<-getOption("futmondo.persistence_context")
  if(is.list(ctx)&&identical(fm_scalar(ctx$championship_id,""),fm_scalar(championship_id,"")))
    return(list(competition_id=fm_scalar(ctx$competition_id,"unknown"),season=fm_scalar(ctx$season,"unknown"),scoring_version=fm_scalar(ctx$scoring_version,"unknown")))
  row<-league_context_metadata(championship_id)
  list(competition_id=fm_scalar(row$competition_id,"unknown"),season=fm_scalar(row$season,"unknown"),scoring_version=fm_scalar(row$scoring_version,"unknown"))
}
complete_point_identity <- function(rows,context) {
  if(!is.data.frame(rows)||!nrow(rows))return(rows)
  if(!"competition_id"%in%names(rows))rows$competition_id<-rep(fm_scalar(context$competition_id,"unknown"),nrow(rows))
  if(!"source_round_number"%in%names(rows))rows$source_round_number<-rep(NA_real_,nrow(rows))
  for(k in c("round_id","match_id"))if(!k%in%names(rows))rows[[k]]<-rep(NA_character_,nrow(rows))
  if(!"source_round_key"%in%names(rows))rows$source_round_key<-ifelse(!is.na(rows$round_id)&nzchar(rows$round_id),paste0("id:",rows$round_id),paste0("number:",rows$round))
  rows
}

league_context_ready <- function(root,login,championship_id) {
  if(!isTRUE(root$userData$league_context_enabled))return(TRUE)
  source<-root$userData$league_context_source
  if(!is.function(source))return(FALSE)
  state<-source();ctx<-state$data
  is.list(ctx)&&state$status%in%c("ok","empty")&&!identical(state$freshness,"stale")&&
    identical(ctx$account_id,fm_scalar(login[["userid"]],""))&&identical(ctx$championship_id,as.character(championship_id))&&
    fm_context_known(ctx$competition_id)&&fm_context_known(ctx$season)&&
    !is.null(league_context_metadata(championship_id,ctx$account_id))
}

# A demand function deliberately checks freshness on every consumer request;
# a cached reactive expression would not notice expiry on same-page actions.
league_context_demand_source <- function(session,login,championship,user_team,refresh=NULL) {
  read<-factual_read_adapter(session,"get_resolved_league_context",login,championship,user_team,
    is_active=function()valid_login(login()),refresh_trigger=refresh,state_only=TRUE)
  function(){
    shiny::req(valid_login(login()),championship(),user_team())
    read(login(),championship(),user_team())
  }
}

summary_round_labels_ambiguous <- function(points) {
  if(is.data.frame(points))points<-split(points,seq_len(nrow(points)))
  if(!is.list(points)||!length(points)||!all(vapply(points,is.list,logical(1))))return(FALSE)
  numbers<-vapply(points,function(p)fm_scalar(p$round,""),character(1))
  ids<-vapply(points,function(p)fm_scalar(p$roundId,""),character(1))
  groups<-split(ids,numbers)
  any(vapply(groups,function(x)length(unique(x[nzchar(x)]))>1L,logical(1)))
}
resolve_summary_round_labels <- function(login,championship_id,summary) {
  if(!summary_round_labels_ambiguous(summary$points))return(summary)
  # The schedule lookup is demanded only for an actual identity collision.
  # Asynchronous replay still owns transport and normal database persistence.
  catalog<-tryCatch(get_finished_rounds(login,championship_id),error=function(e)NULL)
  if(!is.data.frame(catalog)||!all(c("round_id","round_number")%in%names(catalog))||
     identical(attr(catalog,"fetch_status"),"stale"))return(summary)
  points<-summary$points
  if(is.data.frame(points))points<-split(points,seq_len(nrow(points)))
  ids<-vapply(points,function(p)fm_scalar(p$roundId,""),character(1))
  known<-nzchar(ids)
  if(!all(known))return(summary)
  mapped<-vapply(ids,function(id){
    values<-unique(suppressWarnings(as.numeric(catalog$round_number[catalog$round_id%in%id])))
    values<-values[is.finite(values)&values>=1&values==floor(values)]
    if(length(values)==1L)values else NA_real_
  },numeric(1))
  unique_mapping<-unique(data.frame(id=ids,number=mapped))
  if(anyNA(mapped)||anyDuplicated(unique_mapping$number))return(summary)
  summary$points<-lapply(seq_along(points),function(i){
    point<-points[[i]];point$source_round_number<-point$source_round_number %||% point$round
    point$round<-mapped[i];point
  })
  summary$round_mapping_source<-"official_round_catalog"
  summary
}
