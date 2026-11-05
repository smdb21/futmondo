# Cooperative libcurl transport. No socket waits or background processes.
.fm_transport<-new.env(parent=emptyenv())
.fm_transport$pool<-curl::new_pool(total_con=4L,host_con=4L)
.fm_transport$reads<-list();.fm_transport$read_states<-list();.fm_transport$tasks<-list();.fm_transport$scheduled<-FALSE
.fm_transport$timer<-NULL;.fm_transport$interactive_run<-0L;.fm_transport$dispatch_read_key<-NULL
fm_transport_priority<-function(priority){
  if(!is.character(priority)||length(priority)!=1L||!priority%in%c("normal","interactive"))stop("Invalid request priority")
  priority
}
# One logical read reserves one place across queued/active HTTP and retry backoff.
# Direct writes/authentication count too, without counting a read's handle twice.
fm_transport_admitted<-function(){
  direct<-vapply(.fm_transport$tasks,function(task)is.null(task$read_key)||
    is.null(.fm_transport$read_states[[task$read_key]]),logical(1))
  length(.fm_transport$read_states)+sum(direct)
}
fm_transport_schedule<-function(delay=0){
  if(!.fm_transport$scheduled&&length(.fm_transport$tasks)){
    .fm_transport$scheduled<-TRUE;.fm_transport$timer<-later::later(fm_transport_pump,delay)
  }
  invisible(NULL)
}
fm_transport_finish<-function(id,response=NULL,error=NULL){
  task<-.fm_transport$tasks[[id]]
  if(is.null(task))return(invisible(NULL))
  .fm_transport$tasks[[id]]<-NULL
  if(is.null(error)){
    resource_metric("bytes",length(response$content))
    if(response$status_code<200L||response$status_code>=300L)
      error<-resource_failure(resource=task$url,category="http",http_status=response$status_code)
  }
  if(is.null(error))task$resolve(response)else task$reject(error)
  invisible(NULL)
}
fm_transport_start<-function(id){
  task<-.fm_transport$tasks[[id]]
  if(is.null(task)||isTRUE(task$active))return(invisible(NULL))
  tryCatch({
    # libcurl counts its internal connection queue against both timeouts. Add a
    # handle only when a slot is free, so network timers start at dispatch.
    handle<-curl::new_handle();curl::handle_setopt(handle,url=task$url,customrequest=task$method,
      timeout_ms=as.integer(task$timeout*1000),connecttimeout_ms=5000L)
    if(!is.null(task$body))curl::handle_setopt(handle,postfields=as.character(task$body))
    if(length(task$headers))curl::handle_setheaders(handle,.list=as.list(task$headers))
    task$handle<-handle;task$active<-TRUE
    curl::multi_add(handle,done=function(response)fm_transport_finish(id,response),
      fail=function(message)fm_transport_finish(id,error=resource_failure(resource=task$url,category="network")),
      pool=.fm_transport$pool)
    resource_metric("requests")
  },error=function(error){
    if(isTRUE(task$active)&&!is.null(task$handle))try(curl::multi_cancel(task$handle),silent=TRUE)
    fm_transport_finish(id,error=resource_failure(error,resource=task$url))
  })
  invisible(NULL)
}
fm_transport_pump<-function(){
  .fm_transport$scheduled<-FALSE;.fm_transport$timer<-NULL
  active<-sum(vapply(.fm_transport$tasks,function(task)isTRUE(task$active),logical(1)))
  while(active<4L){
    waiting<-names(Filter(function(task)!isTRUE(task$active),.fm_transport$tasks))
    if(!length(waiting))break
    interactive<-vapply(.fm_transport$tasks[waiting],function(task)identical(task$priority,"interactive"),logical(1))
    # FIFO within each class; a normal request follows at most four interactive
    # dispatches while both classes wait. Never interrupt an active request.
    high<-which(interactive);normal<-which(!interactive)
    pick<-if(length(high)&&(!length(normal)||.fm_transport$interactive_run<4L))high[1]else normal[1]
    .fm_transport$interactive_run<-if(interactive[pick])min(4L,.fm_transport$interactive_run+1L)else 0L
    fm_transport_start(waiting[pick])
    active<-sum(vapply(.fm_transport$tasks,function(task)isTRUE(task$active),logical(1)))
  }
  curl::multi_run(timeout=0,pool=.fm_transport$pool)
  fm_transport_schedule(0.01)
  invisible(TRUE)
}
fm_transport_cancel<-function(owner){
  for(key in names(.fm_transport$read_states)){
    state<-.fm_transport$read_states[[key]]
    if(identical(state$owner,owner)&&is.function(state$cancel))state$cancel()
  }
  for(id in names(.fm_transport$tasks)){
    task<-.fm_transport$tasks[[id]]
    if(identical(task$owner,owner)){
      if(isTRUE(task$active))curl::multi_cancel(task$handle)
      fm_transport_finish(id,error=resource_failure(simpleError("Session request cancelled")))
    }
  }
  if(!length(.fm_transport$tasks)){
    if(is.function(.fm_transport$timer)).fm_transport$timer()
    .fm_transport$timer<-NULL;.fm_transport$scheduled<-FALSE
  }else fm_transport_schedule()
  invisible(TRUE)
}
fm_async_http<-function(url,method="GET",body=NULL,headers=character(),owner="",timeout=15,priority="normal"){
  priority<-fm_transport_priority(priority)
  if(isTRUE(getOption("futmondo.offline",FALSE)))return(promises::promise_reject(resource_failure(resource=url,category="network")))
  read_key<-.fm_transport$dispatch_read_key
  reserved<-!is.null(read_key)&&!is.null(.fm_transport$read_states[[read_key]])
  if(!reserved&&fm_transport_admitted()>=32L)return(promises::promise_reject(resource_failure(resource=url,category="read_budget")))
  promises::promise(function(resolve,reject){
    id<-as.character(openssl::sha256(serialize(list(url,Sys.time(),runif(1)),NULL)))
    task<-new.env(parent=emptyenv());task$url<-url;task$method<-method;task$body<-body;task$headers<-headers
    task$owner<-owner;task$timeout<-timeout;task$priority<-priority;task$read_key<-if(reserved)read_key else NULL
    task$active<-FALSE;task$handle<-NULL;task$resolve<-resolve;task$reject<-reject
    .fm_transport$tasks[[id]]<-task
    fm_transport_schedule()
  })
}
fm_httr_response<-function(response,url){
  structure(list(url=url,status_code=response$status_code,headers=httr::insensitive(list(`content-type`="application/json; charset=utf-8")),all_headers=list(),cookies=data.frame(),content=response$content,date=Sys.time(),times=c(total=response$times[["total"]] %||% 0),request=list(method="POST",url=url)),class="response")
}
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE,priority=NULL){
  name<-if(is.character(fn))fn else deparse(substitute(fn))
  if(length(name)!=1L||!grepl("^(get_|read_|calculate_league_finances$)",name))return(promises::promise_reject(simpleError("Only factual reads may use asynchronous adapters")))
  if(is.null(priority))priority<-if(name%in%c("get_team_acquisition_capacity","get_acquisition_capacity",
    "get_player_summary","get_my_market_players"))"interactive"else "normal"
  priority<-fm_transport_priority(priority)
  if(identical(name,"get_team_acquisition_capacity")&&exists("get_team_acquisition_capacity_async",mode="function"))
    return(do.call(get_team_acquisition_capacity_async,c(args,list(context=context,is_current=is_current,priority=priority,owner=owner))))
  adapter_env<-new.env(parent=.GlobalEnv);completed<-new.env(parent=emptyenv());requests<-0L;page_requests<-0L;acquired<-list()
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
    allowed<-c("information","activechampionships","teams","roster","rounds","dreamteam","nightmareteam","roundlineup","lineup","championshipplayers","championshipteams","players","summary","fullprofile","matches","pressroom","news","moneymovements","rosterbids","myplayers","list","unread","getdtconfig")
    if(!route%in%allowed&&!identical(url,"https://api.futmondo.com/1/ranking/round"))stop("Mutation forbidden in asynchronous read")
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
    route<-result$request$url
    if(route%in%c("https://api.futmondo.com/1/locker/pressroom","https://api.futmondo.com/2/locker/news"))page_requests<<-page_requests+1L else requests<<-requests+1L
    if(requests>32L||page_requests>1024L)return(promises::promise_reject(report_failure(resource_failure(resource=name,category="read_budget"))))
    request<-result$request
    read_args<-list(request$url,request$method,request$body,request$headers,owner=owner)
    if(identical(priority,"interactive"))read_args$priority<-priority
    promises::then(do.call(fm_async_read_http,read_args),function(response){completed[[request$key]]<-response;step()},function(error){if(inherits(error,"fm_async_cancelled")||grepl("Session request cancelled",conditionMessage(error),fixed=TRUE))return(promises::promise_reject(error));completed[[request$key]]<-list(error=report_failure(error,request$url));step()})
  },error=function(e)promises::promise_reject(report_failure(e)))
  step()
}

# Retry only idempotent factual reads. Authentication and manual actions use
# fm_async_http directly and must never be replayed after an uncertain response.
fm_async_read_retry_allowed <- function(url,method) {
  if(identical(toupper(method),"GET"))return(TRUE)
  if(!identical(toupper(method),"POST"))return(FALSE)
  route<-tail(strsplit(sub("[?#].*$","",url),"/",fixed=TRUE)[[1]],1L)
  route%in%c("information","activechampionships","teams","roster","rounds",
    "dreamteam","nightmareteam","roundlineup","lineup","championshipplayers","championshipteams",
    "players","summary","fullprofile","matches","pressroom","news","moneymovements",
    "rosterbids","myplayers","list","unread","getdtconfig")||
    identical(url,"https://api.futmondo.com/1/ranking/round")
}
fm_async_read_retryable <- function(error) {
  failure<-resource_failure(error)
  !inherits(failure,"fm_async_cancelled")&&
    (identical(failure$category,"network")||
       (identical(failure$category,"http")&&isTRUE(failure$http_status%in%c(408L,429L,500L,502L,503L,504L))))
}
fm_async_read_http<-function(url,method="GET",body=NULL,headers=character(),owner="",priority="normal"){
  priority<-fm_transport_priority(priority)
  key<-as.character(openssl::sha256(serialize(list(url,method,body,headers,owner),NULL)))
  pending<-.fm_transport$reads[[key]]
  if(!is.null(pending)){
    if(identical(priority,"interactive")){
      .fm_transport$read_states[[key]]$priority<-priority
      for(task in .fm_transport$tasks)if(identical(task$read_key,key))task$priority<-priority
    }
    resource_metric("coalesced");return(pending)
  }
  if(fm_transport_admitted()>=32L)return(promises::promise_reject(resource_failure(resource=url,category="read_budget")))
  state<-new.env(parent=emptyenv());state$owner<-owner;state$settled<-FALSE;state$timer<-NULL;state$attempts<-0L
  state$priority<-priority
  .fm_transport$read_states[[key]]<-state
  pending<-promises::promise(function(resolve,reject){
    finish<-function(value=NULL,error=NULL){
      if(state$settled)return(invisible(NULL))
      state$settled<-TRUE
      if(is.function(state$timer))state$timer()
      state$timer<-NULL
      if(identical(.fm_transport$read_states[[key]],state)){
        .fm_transport$reads[[key]]<-NULL;.fm_transport$read_states[[key]]<-NULL
      }
      if(is.null(error))resolve(value)else reject(error)
      invisible(NULL)
    }
    state$cancel<-function()finish(error=resource_failure(simpleError("Session request cancelled")))
    attempt<-function(){
      state$timer<-NULL
      if(state$settled)return(invisible(NULL))
      state$attempts<-state$attempts+1L
      # The reservation is dynamic only during this synchronous admission call;
      # legacy transport test doubles retain their original positional interface.
      previous_key<-.fm_transport$dispatch_read_key;.fm_transport$dispatch_read_key<-key
      on.exit(.fm_transport$dispatch_read_key<-previous_key,add=TRUE)
      http_args<-list(url,method,body,headers,owner)
      if(identical(state$priority,"interactive"))http_args$priority<-state$priority
      request<-tryCatch(promises::promise_resolve(do.call(fm_async_http,http_args)),
        error=function(error)promises::promise_reject(error))
      .fm_transport$dispatch_read_key<-previous_key
      promises::then(request,function(response){finish(response);NULL},function(error){
        if(state$settled)return(NULL)
        if(state$attempts<2L&&fm_async_read_retry_allowed(url,method)&&fm_async_read_retryable(error)){
          failure<-resource_failure(error,resource=url)
          delay<-if(identical(failure$http_status,429L))1 else .25
          state$timer<-later::later(attempt,delay)
        }else finish(error=error)
        NULL
      })
      invisible(NULL)
    }
    attempt()
  })
  .fm_transport$reads[[key]]<-pending
  pending
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
