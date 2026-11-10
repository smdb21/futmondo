# Safe current-clause observations share the private, durable evidence store.
# Memory entries only accelerate the original sixty-second source deadline.
.clause_transfer_invalidated <- new.env(parent=emptyenv())
# Cooperative reads pin their ordinary Sys.time() for source consistency. Saved
# observations must instead expire against the clock when each replay runs.
clause_transfer_now <- base::Sys.time
clause_transfer_scope <- function(context) {
  scope<-roster_offer_scope(context)
  player<-fm_scalar(context$player_id,"")
  if(is.null(scope)||!nzchar(player)||nchar(player,type="bytes")>256L)return(NULL)
  scope$competition_id<-fm_scalar(context$competition_id,"unknown")
  c(scope,list(player_id=player))
}
clause_transfer_snapshot_id <- function(scope) paste0("rival-cache:clause-transfer-v2:",
  as.character(openssl::sha256(serialize(scope[c("user_team_id","competition_id","season","scoring_version","player_id")],NULL,version=2))))
clause_transfer_epoch_key <- function(context) as.character(openssl::sha256(serialize(
  context[c("account_id","championship_id","user_team_id","competition_id","season","scoring_version")],NULL,version=2)))
invalidate_saved_clause_transfers <- function(context) {
  if(is.null(roster_offer_scope(context)))return(invisible(FALSE))
  .clause_transfer_invalidated[[clause_transfer_epoch_key(context)]]<-Sys.time()
  keys<-ls(.clause_transfer_invalidated)
  if(length(keys)>256L){stamps<-vapply(mget(keys,.clause_transfer_invalidated),as.numeric,numeric(1));rm(list=keys[which.min(stamps)],envir=.clause_transfer_invalidated)}
  invalidate_resources(context,pattern="^clause_transfer_summary_")
  invisible(TRUE)
}
clause_transfer_clean_summary <- function(data,scope) {
  if(!is.list(data)||!is.list(data$data)||!is.list(data$championship)||acquisition_data_unverified(data))return(NULL)
  profile<-data$data;championship<-data$championship
  id<-fm_scalar(profile$id %||% profile$`_id`,"")
  owner<-championship$owner;clause<-championship$clause
  if(!identical(id,scope$player_id)||!is.list(owner)||!is.list(clause)||
     !is.logical(clause$transferred)||length(clause$transferred)!=1L||is.na(clause$transferred))return(NULL)
  owner_id<-fm_scalar(owner$`_id` %||% owner$id,"")
  if(!nzchar(owner_id))return(NULL)
  transfer_at<-fm_scalar(clause$transferDate,"")
  if(isTRUE(clause$transferred)&&(length(fm_time(transfer_at))!=1L||is.na(fm_time(transfer_at))))return(NULL)
  clean_profile<-lapply(c("name","role","team","logo"),function(field)fm_scalar(profile[[field]],""))
  names(clean_profile)<-c("name","role","team","logo");clean_profile<-c(list(id=id),clean_profile)
  list(data=clean_profile,championship=list(owner=list(id=owner_id,name=fm_scalar(owner$name,"")),
    clause=list(transferred=clause$transferred,transferDate=transfer_at)))
}
clause_transfer_cache_key <- function(context) api_cache_key(paste0("clause_transfer_summary_",clause_transfer_snapshot_id(clause_transfer_scope(context))),context=context)
cached_clause_transfer_summary <- function(context,now=clause_transfer_now()) {
  scope<-clause_transfer_scope(context);if(is.null(scope))return(NULL)
  item<-api_cache_env[[clause_transfer_cache_key(context)]]
  if(is.null(item$data)||!resource_snapshot_unexpired(item$data,now)||acquisition_data_unverified(item$data))return(NULL)
  stamp<-fm_time(attr(item$data,"observed_at"));invalidated<-.clause_transfer_invalidated[[clause_transfer_epoch_key(context)]]
  if(length(stamp)!=1L||is.na(stamp)||stamp>now||as.numeric(difftime(now,stamp,units="secs"))>=60||
     (!is.null(invalidated)&&stamp<invalidated))return(NULL)
  item$data
}
persist_clause_transfer_summary <- function(data,context,on_complete=NULL) {
  scope<-clause_transfer_scope(context)
  if(is.null(scope)||isTRUE(attr(data,"clause_from_saved")))return(invisible(FALSE))
  clean<-clause_transfer_clean_summary(data,scope);stamp<-fm_time(attr(data,"observed_at"))
  if(is.null(clean)||length(stamp)!=1L||is.na(stamp))return(invisible(FALSE))
  payload<-list(kind="clause_transfer_summary",version=2L,scope=scope,summary=clean,
    completeness="complete",source="futmondo",source_observed_at=rival_finance_stamp(stamp),valid_until=rival_finance_stamp(stamp+60))
  if(as.numeric(object.size(payload))>32*1024L)return(invisible(FALSE))
  record<-rival_finance_evidence(scope$account_id,scope$championship_id,scope$season,"eligibility",clause_transfer_snapshot_id(scope),payload,stamp,competition_id=scope$competition_id)
  # The shared queue deduplicates acknowledgements and retains failed writes as
  # retryable. A memory hit is never treated as an acknowledged database write.
  defer_persistence("supabase_post_direct",list("rival_finance_evidence",list(record),
    "account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at",TRUE),context=context,on_complete=on_complete)
}
read_saved_clause_transfer_summary <- function(context,now=clause_transfer_now()) {
  scope<-clause_transfer_scope(context);if(is.null(scope))return(NULL)
  query<-list(account_id=paste0("eq.",scope$account_id),championship_id=paste0("eq.",scope$championship_id),
    competition_id=paste0("eq.",scope$competition_id),season=paste0("eq.",scope$season),evidence_type="eq.eligibility",source_id=paste0("eq.",clause_transfer_snapshot_id(scope)),
    select="account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at,payload",limit=1L)
  # Cooperative pending-read conditions must propagate to async_adapter_fetch.
  saved<-tryCatch(supabase_get("rival_finance_evidence_latest",query),error=function(e)NULL)
  if(!is.data.frame(saved)||nrow(saved)!=1L)return(NULL)
  tryCatch({
    record<-rival_evidence_decode(saved)[[1]];p<-record$payload
    stamp<-fm_time(p$source_observed_at);until<-fm_time(p$valid_until)
    invalidated<-.clause_transfer_invalidated[[clause_transfer_epoch_key(context)]]
    if(!is.list(p)||as.numeric(object.size(p))>32*1024L||!identical(p$kind,"clause_transfer_summary")||
       !identical(as.integer(p$version),2L)||!identical(p$completeness,"complete")||!identical(p$source,"futmondo")||
       !identical(record$evidence_type,"eligibility")||!identical(record$source_id,clause_transfer_snapshot_id(scope))||
       !all(vapply(c("account_id","championship_id","competition_id","season"),function(field)identical(fm_scalar(record[[field]]),scope[[field]]),logical(1)))||
       !is.list(p$scope)||!setequal(names(p$scope),names(scope))||!all(vapply(names(scope),function(field)identical(p$scope[[field]],scope[[field]]),logical(1)))||
       length(stamp)!=1L||is.na(stamp)||stamp>now||length(until)!=1L||is.na(until)||now>=until||
       abs(as.numeric(difftime(until,stamp,units="secs"))-60)>0.01||
       !identical(as.numeric(fm_time(record$observed_at)),as.numeric(stamp))||(!is.null(invalidated)&&stamp<invalidated))return(NULL)
    clean<-clause_transfer_clean_summary(p$summary,scope);if(is.null(clean))return(NULL)
    result<-observed_data(clean,observed_at=stamp,status="ok")
    attr(result,"clause_from_saved")<-TRUE;attr(result,"valid_until")<-as.numeric(until)
    result
  },error=function(e)NULL)
}
get_clause_transfer_summary <- function(login,championship_id,user_team_id,player_id) {
  context<-resource_context(login,championship_id,user_team_id,player_id)
  now<-clause_transfer_now()
  saved<-cached_clause_transfer_summary(context,now)
  if(is.null(saved))saved<-read_saved_clause_transfer_summary(context,now)
  if(!is.null(saved)) {
    resource_store(clause_transfer_cache_key(context),saved,context,"clause_transfer_summary",ttl=60)
    return(saved)
  }
  get_player_summary(login,championship_id,user_team_id,player_id)
}
