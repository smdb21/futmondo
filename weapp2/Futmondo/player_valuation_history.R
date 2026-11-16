# Source price curves, kept separate from saved current-price observations.
# The server-only evidence store already supports bounded immutable snapshots;
# its reserved cache namespace excludes these payloads from accounting reads.
# Most curve dates are ISO UTC. Parse the vector in C, falling back only for
# exceptional legacy formats rather than dispatching one R parser per row.
player_valuation_time <- function(x) {
  if(inherits(x,"POSIXt")||is.numeric(x))return(fm_time(x))
  values<-as.character(x);normalized<-sub("Z$","+0000",values)
  normalized<-sub("([+-][0-9]{2}):([0-9]{2})$","\\1\\2",normalized)
  result<-as.POSIXct(normalized,format="%Y-%m-%dT%H:%M:%OS%z",tz="UTC")
  missing<-which(is.na(result)&!is.na(values)&nzchar(values))
  if(length(missing))result[missing]<-fm_time(values[missing])
  result
}
player_valuation_scope <- function(context) {
  fields<-c("account_id","championship_id","user_team_id","player_id","competition_id","season","scoring_version")
  if(!is.list(context)||!all(fields%in%names(context)))return(NULL)
  scope<-lapply(context[fields],function(x)fm_scalar(x,""))
  if(any(!vapply(scope,function(x)nzchar(x)&&x!="unknown"&&nchar(x,type="bytes")<=256L,logical(1))))return(NULL)
  scope
}
player_valuation_id <- function(scope) paste0("rival-cache:player-valuation:v1:",
  as.character(openssl::sha256(serialize(scope,NULL,version=2))))
player_valuation_profile <- function(profile,context) {
  if(!is.list(profile)||is.null(context))return(NULL)
  id<-fm_scalar(profile$player$id %||% profile$player$`_id`,"")
  competition<-fm_scalar(profile$league$`_id` %||% profile$league$id,"")
  season<-fm_scalar(profile$league$season,"")
  # A contradictory source identity can never be relabelled as this league.
  if(!identical(id,context$player_id)||
     (nzchar(competition)&&context$competition_id!="unknown"&&!identical(competition,context$competition_id))||
     (nzchar(season)&&context$season!="unknown"&&!identical(season,context$season)))return(NULL)
  prices<-profile$prices
  if(is.list(prices)&&!is.data.frame(prices))prices<-tryCatch(dplyr::bind_rows(prices),error=function(e)NULL)
  if(!is.data.frame(prices)||!nrow(prices)||nrow(prices)>5000L||
     !all(c("date","social","classic")%in%names(prices)))return(NULL)
  rows<-data.frame(date=format(player_valuation_time(prices$date),"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC"),
    social=suppressWarnings(as.numeric(prices$social)),classic=suppressWarnings(as.numeric(prices$classic)))
  valid<-!is.na(rows$date)&(is.finite(rows$social)|is.finite(rows$classic))
  complete<-all(valid)&&!anyDuplicated(rows$date)&&!identical(profile$complete,FALSE)
  rows<-rows[valid,,drop=FALSE];if(!nrow(rows))return(NULL)
  rows<-rows[order(rows$date),,drop=FALSE];rows<-rows[!duplicated(rows$date,fromLast=TRUE),,drop=FALSE]
  stamp<-fm_time(attr(profile,"observed_at"))
  if(length(stamp)!=1L||is.na(stamp)||stamp>Sys.time()+60)return(NULL)
  source_stamps<-if("source_observed_at"%in%names(prices))player_valuation_time(prices$source_observed_at)else rep(stamp,nrow(prices))
  if(anyNA(source_stamps)||any(source_stamps>stamp))return(NULL)
  # Reorder source clocks with their exact price dates, including duplicates.
  source_rows<-data.frame(date=format(player_valuation_time(prices$date),"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC"),
    source_observed_at=unname(format(source_stamps,"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC")))
  rows$source_observed_at<-source_rows$source_observed_at[match(rows$date,source_rows$date)]
  list(player=list(id=id),league=list(`_id`=competition,season=season),prices=rows,
    observed_at=unname(format(stamp,"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC")),complete=complete,
    freshness=if(identical(attr(profile,"fetch_status"),"stale"))"stale"else"fresh")
}
read_player_valuation_profile <- function(context) {
  scope<-player_valuation_scope(context);if(is.null(scope))return(NULL)
  query<-lapply(scope[c("account_id","championship_id","competition_id","season")],function(x)paste0("eq.",x))
  query$evidence_type<-"eq.eligibility";query$source_id<-paste0("eq.",player_valuation_id(scope))
  query$select<-"account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at,payload";query$limit<-1L
  rows<-tryCatch(supabase_get("rival_finance_evidence_latest",query),error=function(e)NULL)
  tryCatch({
    if(!is.data.frame(rows)||nrow(rows)!=1L)return(NULL)
    row<-rival_evidence_decode(rows)[[1]];payload<-row$payload
    if(!all(vapply(names(query)[1:4],function(f)identical(fm_scalar(row[[f]],""),scope[[f]]),logical(1)))||
       !identical(row$source_id,player_valuation_id(scope))||!identical(row$evidence_type,"eligibility")||
       !is.list(payload)||!identical(payload$kind,"player_valuation_profile")||
       !is.list(payload$scope)||!setequal(names(payload$scope),names(scope))||
       !all(vapply(names(scope),function(f)identical(payload$scope[[f]],scope[[f]]),logical(1)))||!isTRUE(payload$format==1L)||
       nchar(jsonlite::toJSON(payload,auto_unbox=TRUE,na="null",null="null"),type="bytes")>220L*1024L)return(NULL)
    profile<-payload$profile;attr(profile,"observed_at")<-fm_time(row$observed_at)
    checked<-player_valuation_profile(profile,context)
    if(is.null(checked)||!identical(checked$observed_at,profile$observed_at)||
       !identical(checked$complete,profile$complete))return(NULL)
    # This is historical evidence with its original source clock, not a fresh
    # API check and never an action preflight. The live profile still refreshes
    # current biographical facts through its existing independent resource.
    attr(profile,"fetch_status")<-"stale";profile
  },error=function(e)NULL)
}
persist_player_valuation_profile <- function(profile,context,previous=NULL) {
  scope<-player_valuation_scope(context)
  if(is.null(scope)||identical(attr(profile,"fetch_status"),"stale"))return(invisible(FALSE))
  clean<-player_valuation_profile(profile,context);if(is.null(clean))return(invisible(FALSE))
  older<-player_valuation_profile(previous,context)
  if(!is.null(older)){
    if(identical(clean,older))return(invisible(TRUE))
    merged<-dplyr::bind_rows(older$prices,clean$prices)
    merged<-merged[order(merged$date,merged$source_observed_at),,drop=FALSE]
    merged<-merged[!duplicated(merged$date,fromLast=TRUE),,drop=FALSE]
    if(nrow(merged)>5000L)return(invisible(FALSE))
    clean$prices<-merged;clean$complete<-clean$complete&&older$complete
  }
  payload<-list(kind="player_valuation_profile",format=1L,scope=scope,profile=clean)
  if(nchar(jsonlite::toJSON(payload,auto_unbox=TRUE,na="null",null="null"),type="bytes")>220L*1024L)return(invisible(FALSE))
  evidence<-rival_finance_evidence(scope$account_id,scope$championship_id,scope$season,"eligibility",
    player_valuation_id(scope),payload,clean$observed_at,competition_id=scope$competition_id)
  evidence$observed_at<-unname(evidence$observed_at)
  # A deterministic source time and ignore-on-conflict make retries idempotent.
  # The existing queue acknowledges only completed writes, and reports failures.
  if(!exists("defer_persistence",mode="function"))return(invisible(FALSE))
  defer_persistence("supabase_post_direct",list("rival_finance_evidence",list(evidence),
    "account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at",TRUE),context)
}
player_profile_valuation <- function(summary,profile,saved,context) {
  empty<-list(rows=data.frame(),note=character())
  if(is.null(profile)&&is.null(saved))return(empty)
  candidates<-lapply(list(saved,profile),function(p)player_valuation_profile(p,context))
  candidates<-Filter(Negate(is.null),candidates)
  if(!length(candidates)){empty$note<-"Longer valuation history could not be matched to this player, competition or season.";return(empty)}
  # Merge complete rows by their source date and acquisition clock. A partial
  # or shorter new response must not discard successfully saved older prices.
  parts<-lapply(candidates,function(p){x<-p$prices;x$observed_at<-x$source_observed_at;x})
  prices<-dplyr::bind_rows(parts);prices<-prices[order(prices$date,prices$observed_at),,drop=FALSE]
  prices<-prices[!duplicated(prices$date,fromLast=TRUE),,drop=FALSE]
  if(nrow(prices)>5000L){empty$note<-"Longer valuation history exceeds the supported source-history limit.";return(empty)}
  recent<-summary$prices
  if(is.list(recent)&&!is.data.frame(recent))recent<-tryCatch(dplyr::bind_rows(recent),error=function(e)NULL)
  if(!is.data.frame(recent)||!all(c("date","price")%in%names(recent))){
    empty$note<-"Longer valuation history is waiting for league price evidence.";return(empty)
  }
  # Match timestamps exactly, retaining fractional seconds; never fit an
  # arbitrary ratio or infer the league's mode from the latest player value.
  index<-match(as.numeric(player_valuation_time(recent$date)),as.numeric(player_valuation_time(prices$date)))
  actual<-suppressWarnings(as.numeric(recent$price));overlap<-which(!is.na(index)&is.finite(actual))
  if(!length(overlap)){empty$note<-"Longer valuation history has no shared dates with league prices to verify its valuation type.";return(empty)}
  divider<-fm_number(summary$championship$divider)
  divisors<-unique(c(1,if(is.finite(divider)&&divider>0)divider))
  matches<-list()
  for(mode in c("social","classic"))for(divisor in divisors){
    values<-prices[[mode]]/divisor
    if(all(is.finite(values[index[overlap]]))&&all(abs(values[index[overlap]]-actual[overlap])<.000001))
      matches[[length(matches)+1L]]<-list(mode=mode,divisor=divisor,values=values)
  }
  if(!length(matches)){empty$note<-"Longer valuation history does not match this league's reported prices.";return(empty)}
  # Equal recent prices cannot resolve differing earlier social/classic curves.
  if(length(matches)>1L&&!all(vapply(matches[-1L],function(m)isTRUE(all.equal(m$values,matches[[1L]]$values)),logical(1)))){
    empty$note<-"Longer valuation history has an ambiguous social/classic valuation type.";return(empty)
  }
  selected<-matches[[1L]];keep<-is.finite(selected$values)
  newest<-which.max(vapply(candidates,function(p)as.numeric(fm_time(p$observed_at)),numeric(1)))
  note<-if(!all(keep))"Some source history prices are unavailable for this league's valuation type."else
    if(!isTRUE(candidates[[newest]]$complete))"Futmondo returned partial valuation history; some dated source prices are unavailable."else character()
  list(rows=data.frame(recorded_at=prices$date[keep],value=selected$values[keep],source="futmondo_profile",
    observed_at=prices$observed_at[keep]),note=note)
}
