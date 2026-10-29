# Explicit bounded cache and session resource states.
resource_context <- function(login=NULL, championship_id=NULL, user_team_id=NULL, player_id=NULL,
  season="unknown", scoring_version="unknown", account_id=NULL, session_id=NULL) {
  account <- fm_scalar(account_id %||% if(valid_login(login)) login[["userid"]] else NULL, "")
  if(!nzchar(account)) stop("An explicit account is required for private resources.")
  structure(list(account_id=account,championship_id=fm_scalar(championship_id,""),
    user_team_id=fm_scalar(user_team_id,""),player_id=fm_scalar(player_id,""),
    season=fm_scalar(season,"unknown"),scoring_version=fm_scalar(scoring_version,"unknown"),
    session_id=fm_scalar(session_id,"")),class="futmondo_context")
}
resource_policy<-function(resource){
 ttl<-if(grepl("financial|capacity|market|bid|money|user_team_info|team_info",resource,ignore.case=TRUE))20 else
  if(grepl("summary|card|notifications",resource,ignore.case=TRUE))60 else
  if(grepl("real_clubs",resource,ignore.case=TRUE))900 else 300
 list(ttl=ttl,stale_for=if(ttl<=20)60 else 3600,max_entries=256L,max_bytes=32*1024^2)
}
# Only allowlisted diagnostic fields may cross a fetch boundary or reach logs/UI.
resource_diagnostic_name <- function(resource) {
  candidate <- sub("[?#].*$", "", fm_scalar(resource, ""))
  if (grepl("://", candidate, fixed=TRUE)) candidate <- tail(strsplit(candidate, "/", fixed=TRUE)[[1]], 1L)
  candidate <- sub(":.*$", "", candidate)
  allowed <- c("get_financial_snapshot", "get_acquisition_capacity", "get_players_from_team",
    "get_market_players", "get_real_clubs", "get_user_team_info", "get_championships",
    "get_championship_players", "get_teams", "get_lineup_from_team", "get_roster_bids",
    "get_my_market_players", "get_player_summary", "get_player_card_history", "get_player_full_profile",
    "get_player_matches", "get_championship_pressroom", "get_money_movements", "get_finished_rounds",
    "get_finished_round_catalog", "get_round_dreamteam", "get_active_championships",
    "read_player_points_history", "calculate_league_finances", "roster", "market", "real_clubs",
    "championships", "championshipplayers", "lineup", "teams", "team_info", "user_team_info",
    "roster_bids", "my_market_players", "acq_capacity", "financial_snapshot", "moneymovements",
    "pressroom", "player_summary", "player_profile", "player_matches", "rounds", "dreamteam",
    "league_finances_calc", "notifications_list", "notifications_unread", "information",
    "activechampionships", "championshipteams", "players", "summary", "fullprofile", "matches",
    "rosterbids", "myplayers", "getdtconfig", "unread", "list", "database")
  matches <- allowed[candidate == allowed | startsWith(candidate, paste0(allowed, "_"))]
  if (!length(matches)) "read" else matches[which.max(nchar(matches))]
}
resource_failure <- function(error=NULL, resource=NULL, category=NULL, http_status=NULL) {
  fields <- if (is.list(error)) error else list()
  text <- if (inherits(error, "condition")) conditionMessage(error) else ""
  cancelled <- inherits(error, "fm_async_cancelled") || identical(text, "Session request cancelled")
  categories <- c("http", "network", "parser", "schema", "read_budget", "unknown")
  category <- category %||% fields$category
  status <- suppressWarnings(as.integer(http_status %||% fields$http_status %||% fields$status_code))
  if (length(status)!=1L || is.na(status) || status<100L || status>599L) status <- NULL
  if (is.null(status) && grepl("^HTTP[ :]+[1-5][0-9]{2}([^0-9]|$)", text))
    status <- as.integer(sub("^HTTP[ :]+([1-5][0-9]{2}).*$", "\\1", text))
  if (length(category)!=1L || is.na(category) || !category %in% categories) {
    category <- if (!is.null(status)) "http" else
      if (inherits(error, "curl_error") || grepl("Connection failed|Network disabled|timed out|could not resolve|couldn't connect", text, ignore.case=TRUE)) "network" else
      if (grepl("request budget|request capacity|resource capacity", text, ignore.case=TRUE)) "read_budget" else
      if (grepl("lexical error|parse error|premature EOF|invalid JSON|trailing garbage", text, ignore.case=TRUE)) "parser" else
      if (grepl("arguments imply differing|names.*vector|subscript|not found|unavailable|invalid.*data|Mutation forbidden", text, ignore.case=TRUE)) "schema" else "unknown"
  }
  base <- resource_diagnostic_name(fields$resource)
  if (base == "read") base <- resource_diagnostic_name(resource)
  reference <- fm_scalar(fields$reference, "")
  if (!grepl("^FM-(HTTP|NET|PARSE|SCHEMA|LIMIT|UNKNOWN)-[0-9]{6}$", reference)) {
    sequence <- (.resource_metrics$failure_sequence %||% 0L) %% 999999L + 1L
    .resource_metrics$failure_sequence <- sequence
    code <- c(http="HTTP", network="NET", parser="PARSE", schema="SCHEMA", read_budget="LIMIT", unknown="UNKNOWN")[[category]]
    reference <- sprintf("FM-%s-%06d", code, sequence)
  }
  structure(list(message=if(cancelled) "Session request cancelled" else paste0("Data unavailable (", reference, "). Refresh to retry."),
    call=NULL, category=category, resource=base, http_status=status, reference=reference,
    logged=isTRUE(fields$logged)), class=c(if(cancelled) "fm_async_cancelled", "futmondo_unavailable", "error", "condition"))
}
resource_log_failure <- function(error, resource=NULL) {
  failure <- resource_failure(error, resource)
  if (inherits(failure, "fm_async_cancelled")) return(failure)
  if (!failure$logged) {
    message(sprintf("[Futmondo read] resource=%s category=%s%s ref=%s", failure$resource,
      failure$category, if(is.null(failure$http_status)) "" else paste0(" status=",failure$http_status), failure$reference))
    failure$logged <- TRUE
  }
  # Outer composite reads retain attribution even if an optional inner read is caught.
  signalCondition(structure(list(message="Read failure diagnostic", failure=failure),
    class=c("fm_fetch_diagnostic", "condition")))
  failure
}
resource_failure_message <- function(error) {
  if (is.null(error)) return("Data unavailable. Refresh to retry.")
  failure <- resource_failure(error)
  label <- c(http="Source request", network="Connection", parser="Source response", schema="Source data",
    read_budget="Read limit", unknown="Data")[[failure$category]]
  paste0(label, " unavailable (", failure$reference, "). Refresh to retry.")
}
api_cache_env <- new.env(parent=emptyenv())
.resource_metrics <- new.env(parent=emptyenv())
resource_metrics <- function(reset=FALSE) {
  defaults<-list(hits=0L,misses=0L,stale=0L,evictions=0L,coalesced=0L,requests=0L,bytes=0L)
  out<-lapply(names(defaults),function(k).resource_metrics[[k]] %||% defaults[[k]]);names(out)<-names(defaults)
  out$entries<-length(ls(api_cache_env));out$retained_bytes<-sum(vapply(as.list(api_cache_env),function(x)as.numeric(x$bytes %||% object.size(x)),numeric(1)))
  if(reset)for(k in names(defaults)).resource_metrics[[k]]<-defaults[[k]]
  out
}
resource_metric <- function(key,amount=1L).resource_metrics[[key]]<-(.resource_metrics[[key]] %||% 0)+amount
api_cache_key <- function(key,login=NULL,scope=NULL,context=NULL) {
  if(is.null(context))context<-resource_context(login=login,account_id=scope)
  dimensions<-context[setdiff(names(context),c("account_id","session_id"))]
  suffix<-if(all(vapply(dimensions,function(x)x%in%c("","unknown"),logical(1))))"" else paste0(":",as.character(openssl::sha256(serialize(dimensions,NULL))))
  paste0("account:",context$account_id,":",key,suffix)
}
resource_cache_prune <- function(now=Sys.time()) {
  for(k in ls(api_cache_env)) {
    item<-api_cache_env[[k]]
    if(!is.null(item$expires_at)&&as.numeric(now)>item$expires_at&&is.null(item$pending)) {
      rm(list=k,envir=api_cache_env);resource_metric("evictions")
    }
  }
  policy<-resource_policy("")
  repeat {
    keys<-ls(api_cache_env);values<-mget(keys,api_cache_env)
    size<-sum(vapply(values,function(x)as.numeric(x$bytes %||% object.size(x)),numeric(1)))
    if(length(keys)<=policy$max_entries&&size<=policy$max_bytes)break
    eligible<-which(vapply(values,function(x)is.null(x$pending),logical(1)))
    if(!length(eligible))break
    ages<-vapply(values[eligible],function(x)as.numeric(x$accessed_at %||% x$time %||% 0),numeric(1))
    rm(list=keys[eligible[which.min(ages)]],envir=api_cache_env);resource_metric("evictions")
  }
  invisible(TRUE)
}
resource_store <- function(key,data,context,resource,ttl=NULL) {
  now<-Sys.time();policy<-resource_policy(resource);ttl<-ttl %||% policy$ttl
  data<-observed_data(data,status=attr(data,"fetch_status"),observed_at=attr(data,"observed_at") %||% now)
  attr(data,"fetched_at")<-now
  item<-list(data=data,time=now,accessed_at=now,context=context,resource=resource,
    expires_at=as.numeric(now)+max(0,ttl)+policy$stale_for,bytes=as.numeric(object.size(data)))
  if(item$bytes<=policy$max_bytes)api_cache_env[[key]]<-item
  resource_cache_prune(now);data
}
get_cached_data <- function(key,expr,timeout_sec=NULL,context=NULL) {
  code<-substitute(expr);caller<-parent.frame()
  if(is.null(context))stop("get_cached_data requires explicit context")
  cache_key<-api_cache_key(key,context=context);now<-Sys.time();policy<-resource_policy(key)
  ttl<-min(timeout_sec %||% policy$ttl,policy$ttl);resource_cache_prune(now);cached<-api_cache_env[[cache_key]]
  if(!is.null(cached$data)&&as.numeric(difftime(now,cached$time,units="secs"))<ttl) {
    cached$accessed_at<-now;api_cache_env[[cache_key]]<-cached;resource_metric("hits");return(cached$data)
  }
  resource_metric("misses")
  tryCatch({
    diagnostic<-NULL
    value<-withCallingHandlers((function()eval(code,envir=caller))(),
      fm_fetch_diagnostic=function(note)diagnostic<<-note$failure)
    if(is.null(value))stop(resource_failure(resource=key,category="schema"))
    if(!is.null(diagnostic))attr(value,"fetch_error")<-diagnostic
    value<-resource_store(cache_key,value,context,key,ttl)
    if(exists("record_acquired_resource",mode="function"))tryCatch(record_acquired_resource(key,value,context),error=function(e)NULL)
    value
  },error=function(e) {
    failure<-resource_log_failure(e,key)
    if(!is.null(cached$data)&&as.numeric(difftime(now,cached$time,units="secs"))<=max(0,ttl)+policy$stale_for) {
      resource_metric("stale");fallback<-observed_data(cached$data,status="stale",observed_at=attr(cached$data,"observed_at") %||% cached$time)
      attr(fallback,"fetch_error")<-failure
      if(is.list(fallback)&&!is.data.frame(fallback)&&identical(fallback$status,"ok")) {
        fallback$status<-"partial";if("spendable_budget"%in%names(fallback))fallback$spendable_budget<-NA_real_
        if(is.list(fallback$funds))fallback$funds$spendable_budget<-NA_real_
      }
      return(fallback)
    }
    stop(failure)
  })
}
invalidate_resources <- function(context,resources=NULL,pattern=NULL) {
  keys<-ls(api_cache_env);drop<-vapply(keys,function(k) {
    x<-api_cache_env[[k]];ctx<-x$context
    if(is.null(ctx))return(!nzchar(context$session_id %||% "")&&startsWith(k,paste0("account:",context$account_id,":"))&&is.null(resources)&&is.null(pattern))
    scope<-all(vapply(c("account_id","championship_id","user_team_id","player_id","session_id"),function(n)
      !nzchar(context[[n]] %||% "")||identical(ctx[[n]],context[[n]]),logical(1)))
    scope&&(is.null(resources)||x$resource%in%resources)&&(is.null(pattern)||grepl(pattern,x$resource))
  },logical(1))
  if(any(drop))rm(list=keys[drop],envir=api_cache_env)
  invisible(sum(drop))
}
clear_api_cache <- function(user_id=NULL,session_id=NULL) {
  if(is.null(user_id)){keys<-ls(api_cache_env);if(length(keys))rm(list=keys,envir=api_cache_env)}else invalidate_resources(resource_context(account_id=user_id,session_id=session_id))
  invisible(TRUE)
}
resource_state <- function(data=NULL,status=NULL,error=NULL,source=NULL) {
  error<-error %||% attr(data,"fetch_error")
  if(!is.null(error))error<-resource_failure(error)
  status<-status %||% if(is.null(data))"unavailable" else attr(data,"fetch_status") %||% if(is.data.frame(data)&&!nrow(data))"empty" else "ok"
  list(data=data,status=status,source=source %||% attr(data,"source") %||% "futmondo",
    observed_at=attr(data,"observed_at"),fetched_at=attr(data,"fetched_at"),freshness=if(status=="stale")"stale" else "fresh",
    completeness=if(status%in%c("ok","empty"))"complete" else "partial",error=error)
}
get_cached_resource_async <- function(resource,context,fetch,refresh=FALSE) {
  key<-api_cache_key(resource,context=context)
  if(nzchar(fm_scalar(context$session_id,"")))key<-paste0(key,":session:",as.character(openssl::sha256(serialize(context$session_id,NULL))))
  resource_cache_prune();item<-api_cache_env[[key]];policy<-resource_policy(resource)
  if(!is.null(item$pending)){resource_metric("coalesced");return(item$pending)}
  if(!refresh&&!is.null(item$data)&&as.numeric(difftime(Sys.time(),item$time,units="secs"))<policy$ttl){resource_metric("hits");return(promises::promise_resolve(resource_state(item$data)))}
  if(refresh)invalidate_resources(context,resources=resource)
  if(is.null(item)&&length(ls(api_cache_env))>=policy$max_entries){
    keys<-ls(api_cache_env);entries<-mget(keys,api_cache_env)
    candidates<-which(vapply(entries,function(x)is.null(x$pending),logical(1)))
    if(!length(candidates))return(promises::promise_resolve(resource_state(status="unavailable",error=resource_log_failure(resource_failure(resource=resource,category="read_budget")))))
    ages<-vapply(entries[candidates],function(x)as.numeric(x$accessed_at %||% x$time),numeric(1))
    rm(list=keys[candidates[which.min(ages)]],envir=api_cache_env);resource_metric("evictions")
  }
  resource_metric("misses");generation<-new.env(parent=emptyenv())
  fail<-function(error){
    failure<-resource_log_failure(error,resource)
    current<-api_cache_env[[key]]
    if(identical(current$generation,generation)){
      if(!is.null(item$data)&&as.numeric(difftime(Sys.time(),item$time,units="secs"))<policy$ttl+policy$stale_for){
        api_cache_env[[key]]<-item
        return(resource_state(observed_data(item$data,status="stale"),error=failure))
      }
      rm(list=key,envir=api_cache_env)
    }
    resource_state(status="unavailable",error=failure)
  }
  fetched<-tryCatch(promises::promise_resolve(fetch()),error=function(e)promises::promise_reject(e))
  pending<-promises::then(fetched,function(data){
    current<-api_cache_env[[key]]
    if(!identical(current$generation,generation))return(resource_state(status="unavailable",error="Context changed"))
    if(is.null(data))return(fail(resource_failure(resource=resource,category="schema")))
    if(as.numeric(object.size(data))>policy$max_bytes)return(fail(resource_failure(resource=resource,category="read_budget")))
    value<-resource_store(key,data,context,resource);resource_state(value)
  })
  pending<-promises::catch(pending,fail)
  api_cache_env[[key]]<-list(pending=pending,generation=generation,context=context,resource=resource,time=Sys.time(),bytes=0)
  pending
}
shared_resource<-function(session,resource,context,fetch,active=function()TRUE,refresh=function()0){
 value<-shiny::reactiveVal(resource_state(status="loading"));last<-NULL;version<-NULL;closed<-FALSE;generation<-0L;was_active<-FALSE;cancel_end<-NULL
 dispose<-function(){closed<<-TRUE;generation<<-generation+1L;observer$destroy();if(is.function(cancel_end))cancel_end()}
 cancel_end<-session$onSessionEnded(function(){dispose();fm_transport_cancel(session$token)})
 observer<-shiny::observe({
  if(closed)return()
  if(!isTRUE(active())){if(was_active){generation<<-generation+1L;was_active<<-FALSE;value(resource_state(status="loading"))};return()}
  ctx<-context()
  if(is.null(ctx)){generation<<-generation+1L;was_active<<-FALSE;value(resource_state(status="loading"));return()}
  if(!nzchar(ctx$session_id))ctx$session_id<-session$token
  key<-api_cache_key(resource,context=ctx);next_version<-refresh();resumed<-!was_active;was_active<<-TRUE
  if(identical(last,key)&&identical(version,next_version)&&!resumed)return()
  force_refresh<-identical(last,key)&&!identical(version,next_version)
  last<<-key;version<<-next_version;generation<<-generation+1L;current_generation<-generation
  value(resource_state(status="loading"))
  promises::then(get_cached_resource_async(resource,ctx,function()fetch(ctx),force_refresh),function(result){if(!closed&&identical(generation,current_generation))value(result);NULL})
 })
 out<-shiny::reactive(value());attr(out,"dispose")<-dispose;out
}
