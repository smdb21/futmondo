# Cooperative libcurl transport. No socket waits or background processes.
.fm_transport<-new.env(parent=emptyenv())
.fm_transport$pool<-curl::new_pool(total_con=4L,host_con=4L)
.fm_transport$reads<-list();.fm_transport$tasks<-list();.fm_transport$scheduled<-FALSE
fm_transport_pump<-function(){
  curl::multi_run(timeout=0,pool=.fm_transport$pool)
  if(length(.fm_transport$tasks))later::later(fm_transport_pump,0.01)else .fm_transport$scheduled<-FALSE
  invisible(TRUE)
}
fm_transport_cancel<-function(owner){
  for(id in names(.fm_transport$tasks)){
    task<-.fm_transport$tasks[[id]]
    if(identical(task$owner,owner)){curl::multi_cancel(task$handle);.fm_transport$tasks[[id]]<-NULL;task$reject(resource_failure(simpleError("Session request cancelled")))}
  };invisible(TRUE)
}
fm_async_http<-function(url,method="GET",body=NULL,headers=character(),owner="",timeout=15){
  if(isTRUE(getOption("futmondo.offline",FALSE)))return(promises::promise_reject(resource_failure(resource=url,category="network")))
  if(length(.fm_transport$tasks)>=32L)return(promises::promise_reject(resource_failure(resource=url,category="read_budget")))
  promises::promise(function(resolve,reject){
    handle<-curl::new_handle();curl::handle_setopt(handle,url=url,customrequest=method,timeout_ms=as.integer(timeout*1000),connecttimeout_ms=5000L)
    if(!is.null(body))curl::handle_setopt(handle,postfields=as.character(body))
    if(length(headers))curl::handle_setheaders(handle,.list=as.list(headers))
    id<-as.character(openssl::sha256(serialize(list(url,Sys.time(),runif(1)),NULL)))
    .fm_transport$tasks[[id]]<-list(handle=handle,owner=owner,reject=reject)
    finish<-function(){.fm_transport$tasks[[id]]<-NULL}
    resource_metric("requests")
    curl::multi_add(handle,done=function(response){finish();resource_metric("bytes",length(response$content));if(response$status_code<200L||response$status_code>=300L)reject(resource_failure(resource=url,category="http",http_status=response$status_code))else resolve(response)},fail=function(message){finish();reject(resource_failure(resource=url,category="network"))},pool=.fm_transport$pool)
    if(!.fm_transport$scheduled){.fm_transport$scheduled<-TRUE;later::later(fm_transport_pump,0)}
  })
}
fm_httr_response<-function(response,url){
  structure(list(url=url,status_code=response$status_code,headers=httr::insensitive(list(`content-type`="application/json; charset=utf-8")),all_headers=list(),cookies=data.frame(),content=response$content,date=Sys.time(),times=c(total=response$times[["total"]] %||% 0),request=list(method="POST",url=url)),class="response")
}
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE){
  name<-if(is.character(fn))fn else deparse(substitute(fn))
  if(length(name)!=1L||!grepl("^(get_|read_|calculate_league_finances$)",name))return(promises::promise_reject(simpleError("Only factual reads may use asynchronous adapters")))
  adapter_env<-new.env(parent=.GlobalEnv);completed<-new.env(parent=emptyenv());requests<-0L;acquired<-list()
  for(nm in ls(.GlobalEnv,all.names=TRUE)){
    object<-get(nm,.GlobalEnv)
    if(is.function(object)&&identical(environment(object),.GlobalEnv)){environment(object)<-adapter_env;assign(nm,object,adapter_env)}
  }
  # Replaying a composite read may encounter the same optional failure again.
  # Keep at most one safe diagnostic per source/category, capped by the read budget.
  failures<-new.env(parent=emptyenv());log_failure<-resource_log_failure
  report_failure<-function(error,resource=name){
    failure<-resource_failure(error,resource)
    key<-paste(failure$resource,failure$category,failure$http_status,sep=":")
    previous<-failures[[key]]
    if(!is.null(previous))return(log_failure(previous))
    if(length(ls(failures))>=32L)failure$logged<-TRUE
    failure<-log_failure(failure)
    if(length(ls(failures))<32L)failures[[key]]<-failure
    failure
  }
  adapter_env$resource_log_failure<-report_failure
  started_at<-Sys.time();adapter_env$Sys.time<-function()started_at
  adapter_env$resource_store<-resource_store;adapter_env$resource_cache_prune<-resource_cache_prune
  need<-function(url,method,body=NULL,headers=character(),decode=FALSE){
    key<-as.character(openssl::sha256(serialize(list(url,method,body,headers),NULL)))
    if(exists(key,completed,inherits=FALSE)){
      response<-completed[[key]]
      if(!is.null(response$error))stop(response$error)
      if(decode)return(as.data.frame(jsonlite::fromJSON(rawToChar(response$content))))
      return(fm_httr_response(response,url))
    }
    stop(structure(list(message="Awaiting read response",key=key,url=url,method=method,body=body,headers=headers),class=c("fm_async_request","condition")))
  }
  adapter_env$futmondo_post<-function(url,...){
    route<-tail(strsplit(url,"/",fixed=TRUE)[[1]],1)
    allowed<-c("information","activechampionships","teams","roster","rounds","dreamteam","nightmareteam","lineup","championshipplayers","championshipteams","players","summary","fullprofile","matches","pressroom","moneymovements","rosterbids","myplayers","list","unread","getdtconfig")
    if(!route%in%allowed)stop("Mutation forbidden in asynchronous read")
    dots<-list(...);need(url,"POST",dots$body,c(`Content-Type`="application/json; charset=utf-8"))
  }
  adapter_env$supabase_get<-function(table_name,query_params=list()){
    if(!nzchar(get_sb_url())||!nzchar(get_sb_key()))return(NULL)
    url<-httr::modify_url(paste0(get_sb_url(),"/rest/v1/",table_name),query=query_params)
    need(url,"GET",headers=c(apikey=get_sb_key(),Authorization=paste("Bearer",get_sb_key()),Accept="application/json"),decode=TRUE)
  }
  adapter_env$supabase_post<-function(...)invisible(TRUE)
  adapter_env$supabase_post_direct<-function(...)invisible(TRUE)
  adapter_env$defer_persistence<-function(...)invisible(TRUE)
  adapter_env$record_acquired_resource<-function(resource,data,context){acquired[[resource]]<<-list(resource=resource,data=data,context=context);invisible(TRUE)}
  step<-function()tryCatch({
    if(!isTRUE(tryCatch(is_current(),error=function(e)FALSE)))return(promises::promise_reject(structure(list(message="Session request cancelled",call=NULL),class=c("fm_async_cancelled","error","condition"))))
    result<-tryCatch(list(done=TRUE,value=do.call(get(name,adapter_env),args)),fm_async_request=function(request)list(done=FALSE,request=request))
    if(result$done){for(record in acquired){record$context$session_id<-context$session_id;tryCatch(do.call(record_acquired_resource,record),error=function(e)NULL)};return(promises::promise_resolve(result$value))}
    requests<<-requests+1L
    if(requests>32L)return(promises::promise_reject(report_failure(resource_failure(resource=name,category="read_budget"))))
    request<-result$request
    promises::then(fm_async_read_http(request$url,request$method,request$body,request$headers,owner=owner),function(response){completed[[request$key]]<-response;step()},function(error){if(inherits(error,"fm_async_cancelled")||grepl("Session request cancelled",conditionMessage(error),fixed=TRUE))return(promises::promise_reject(error));completed[[request$key]]<-list(error=report_failure(error,request$url));step()})
  },error=function(e)promises::promise_reject(report_failure(e)))
  step()
}

fm_async_read_http<-function(url,method="GET",body=NULL,headers=character(),owner=""){
 key<-as.character(openssl::sha256(serialize(list(url,method,body,headers,owner),NULL)))
 pending<-.fm_transport$reads[[key]]
 if(!is.null(pending)){resource_metric("coalesced");return(pending)}
 pending<-promises::then(fm_async_http(url,method,body,headers,owner),function(response){.fm_transport$reads[[key]]<-NULL;response},function(error){.fm_transport$reads[[key]]<-NULL;stop(error)})
 .fm_transport$reads[[key]]<-pending;pending
}

# Authentication is intentionally a single, uncached attempt. Secrets never enter
# resource keys, metrics, diagnostics, or the factual-read replay adapter.
fm_async_login <- function(user_name,password,owner="") {
  user_name <- trimws(user_name)
  payload <- jsonlite::toJSON(list(header=list(token="null",userid=""),
    query=list(mail=user_name,pwd=password)),auto_unbox=TRUE)
  password <- NULL
  request <- fm_async_http(LOGIN_URL,"POST",payload,
    c(`Content-Type`="application/json; charset=utf-8"),owner=owner)
  payload <- NULL
  promises::then(request,function(response) {
    answer <- jsonlite::fromJSON(rawToChar(response$content),simplifyVector=FALSE)$answer$mobile
    token <- c(token=answer$token,userid=answer$userid,user_name=user_name)
    if(!valid_login(token))stop("Authentication failed")
    token
  })
}
