# Optional browser login persistence. Authentication material stays on the server.
remember_login_enabled <- function() {
  identical(tolower(Sys.getenv("FUTMONDO_REMEMBER_ENABLED")), "true") &&
    requireNamespace("sodium", quietly=TRUE) &&
    grepl("^[a-fA-F0-9]{64}$", Sys.getenv("FUTMONDO_REMEMBER_KEY")) &&
    exists("get_sb_url", mode="function") && exists("get_sb_key", mode="function") &&
    nzchar(get_sb_url()) && nzchar(get_sb_key()) && !isTRUE(getOption("futmondo.offline", FALSE))
}
remember_login_key <- function() sodium::hex2bin(Sys.getenv("FUTMONDO_REMEMBER_KEY"))
remember_login_random <- function() sodium::bin2hex(sodium::random(32))
remember_login_hash <- function(secret) {
  if (!is.character(secret) || length(secret)!=1L || is.na(secret) || !grepl("^[a-f0-9]{64}$",secret)) return(NULL)
  paste0(openssl::sha256(charToRaw(secret)))
}
remember_login_time <- function(x) {
  if(!is.character(x)||length(x)!=1L||is.na(x)||!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?(Z|[+-][0-9]{2}:[0-9]{2})$",x))return(as.POSIXct(NA))
  unname(fm_time(x))
}
remember_login_stamp <- function(x) format(x,"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC")
remember_login_encrypt <- function(token, created, expires, key=remember_login_key(),browser_hash) {
  stopifnot(valid_login(token), length(key)==32L,grepl("^[a-f0-9]{64}$",browser_hash))
  # Binding the timestamps inside the authenticated envelope prevents extending a
  # credential's life by editing only the database metadata.
  payload <- jsonlite::toJSON(list(version=1L,browser_hash=browser_hash,auth=as.list(token),created=as.numeric(created),
    expires=as.numeric(expires)),auto_unbox=TRUE)
  encrypted <- sodium::data_encrypt(charToRaw(payload),key)
  jsonlite::base64_enc(c(attr(encrypted,"nonce"),encrypted))
}
remember_login_decrypt <- function(row, now=Sys.time(), key=remember_login_key()) {
  if (!is.list(row) || !is.character(row$encrypted_payload) || length(row$encrypted_payload)!=1L ||
      nchar(row$encrypted_payload)>16384L) stop("Invalid saved access")
  created<-remember_login_time(row$created_at);expires<-remember_login_time(row$expires_at)
  if(length(created)!=1L||length(expires)!=1L||is.na(created)||is.na(expires)||
      created>now+60||expires<=now||expires<=created||as.numeric(difftime(expires,created,units="secs"))>604800.01||
      !is.null(row$revoked_at))stop("Invalid saved access")
  packed<-jsonlite::base64_dec(row$encrypted_payload)
  if(length(packed)<41L||length(packed)>8192L)stop("Invalid saved access")
  decoded<-rawToChar(sodium::data_decrypt(packed[-seq_len(24L)],key,packed[seq_len(24L)]))
  payload<-jsonlite::fromJSON(decoded,simplifyVector=FALSE)
  auth<-payload$auth
  if(!is.list(auth)||!setequal(names(auth),c("token","userid","user_name"))||
      !all(vapply(auth,function(x)is.character(x)&&length(x)==1L&&!is.na(x)&&nzchar(x),logical(1))))stop("Invalid saved access")
  token<-unlist(auth,use.names=TRUE)
  if(!valid_login(token)||!identical(payload$version,1L)||!identical(payload$browser_hash,row$browser_hash)||
      !isTRUE(abs(payload$created-as.numeric(created))<.01)||
      !isTRUE(abs(payload$expires-as.numeric(expires))<.01))stop("Invalid saved access")
  list(token=token,expires=expires)
}
remember_login_store <- function(method="GET",hash=NULL,payload=NULL,owner="",expires_before=NULL) {
  if(!exists("get_sb_url",mode="function")||!exists("get_sb_key",mode="function")||!nzchar(get_sb_url())||!nzchar(get_sb_key()))
    return(promises::promise_reject(simpleError("Saved access storage is not configured")))
  query<-if(!is.null(hash))list(browser_hash=paste0("eq.",hash))else list()
  if(!is.null(expires_before))query<-list(expires_at=paste0("lte.",expires_before))
  if(identical(method,"GET"))query<-c(query,list(select="browser_hash,encrypted_payload,created_at,expires_at,revoked_at",limit="1"))
  url<-httr::modify_url(paste0(sub("/$","",get_sb_url()),"/rest/v1/remembered_logins"),query=query)
  headers<-c(apikey=get_sb_key(),Authorization=paste("Bearer",get_sb_key()),`Content-Type`="application/json",Prefer="return=minimal")
  body<-if(is.null(payload))NULL else jsonlite::toJSON(payload,auto_unbox=TRUE,null="null")
  promises::then(fm_async_http(url,method,body,headers,owner=owner),function(response){
    if(!identical(method,"GET"))return(TRUE)
    data<-jsonlite::fromJSON(rawToChar(response$content),simplifyVector=FALSE)
    if(!is.list(data)||length(data)>1L)stop("Saved access unavailable")
    if(length(data))data[[1L]]else NULL
  })
}
remember_login_revoke <- function(hash,owner="") {
  if(is.null(hash))return(promises::promise_resolve(TRUE))
  remember_login_store("DELETE",hash,owner=owner)
}
 .remember_login_maintenance <- new.env(parent=emptyenv())
remember_login_cleanup <- function(owner="",now=Sys.time()) {
  previous<-.remember_login_maintenance$attempted
  if(!is.null(previous)&&as.numeric(difftime(now,previous,units="secs"))<86400)return(invisible(NULL))
  .remember_login_maintenance$attempted<-now
  # One indexed deletion per process/day at explicit issuance, no polling or job.
  promises::then(remember_login_store("DELETE",owner=owner,expires_before=remember_login_stamp(now)),
    function(...)TRUE,function(error){message("[Remember login] expired credential cleanup unavailable");FALSE})
  invisible(NULL)
}
remember_login_validate <- function(token,owner="") {
  payload<-jsonlite::toJSON(list(header=list(token=token[["token"]],userid=token[["userid"]]),
    query=list(excludeGeneral=FALSE,includeProphets=TRUE),answer=list()),auto_unbox=TRUE)
  promises::then(fm_async_http(ACTIVE_CHAMPIONSHIPS_URL,"POST",payload,
    c(`Content-Type`="application/json; charset=utf-8"),owner=owner),function(response){
    body<-jsonlite::fromJSON(rawToChar(response$content),simplifyVector=FALSE)
    answer<-body$answer
    # The captures do not establish an expired-token body code. Unknown API errors
    # are recoverable, never grounds for discarding a valid browser credential.
    if(!is.list(answer)||!is.list(answer$championships))stop("Saved access validation unavailable")
    answer$championships<-lapply(answer$championships,function(champ){
      champ$id<-champ$id %||% champ$`_id`
      if(is.list(champ$userteam))champ$userteam$id<-champ$userteam$id %||% champ$userteam$`_id`
      champ
    })
    context<-resource_context(login=token)
    resource_store(api_cache_key("active_championship_objects",context=context),answer,context,"active_championship_objects",60)
    TRUE
  })
}
remember_login_origin <- function(x) {
  if(!is.character(x)||length(x)!=1L||is.na(x)||!grepl("^https://[A-Za-z0-9.-]+(:[0-9]{1,5})?$",x))return(NULL)
  sub(":443$", "", tolower(x))
}
# Hosted Shiny may open an internal WebSocket without the browser Origin.
# Bind the cookie exchange to its real HTTP Origin plus Host or browser Fetch
# Metadata; never trust Shiny-input origins or arbitrary forwarded headers.
remember_login_same_origin <- function(req, websocket_origin=NULL) {
  origin<-remember_login_origin(req$HTTP_ORIGIN)
  if(is.null(origin))return(FALSE)
  if(!is.null(websocket_origin))return(identical(origin,websocket_origin))
  host<-req$HTTP_HOST
  if(!is.character(host)||length(host)!=1L||is.na(host))return(FALSE)
  host_origin<-remember_login_origin(paste0("https://",host))
  if(is.null(host_origin))return(FALSE)
  # Managed proxies may rewrite Host as well. Fetch Metadata is set by the
  # browser and cannot be supplied by page JavaScript. Require exact same-origin;
  # same-site, navigation (none), absent or malformed metadata cannot use this path.
  identical(origin,host_origin)||identical(req$HTTP_SEC_FETCH_SITE,"same-origin")
}
remember_login_path <- function(x) {
  if(!is.character(x)||length(x)!=1L||is.na(x)||!grepl("^/[A-Za-z0-9_./~%-]*$",x)||
      grepl("%|//|\\.\\.",x)||nchar(x)>1024L)return(NULL)
  if(!endsWith(x,"/"))x<-paste0(x,"/")
  x
}
remember_login_cookie_name <- function(path) paste0("__Secure-fm_remember_",substr(as.character(openssl::sha256(charToRaw(path))),1,12))
remember_login_cookie <- function(path,secret="",max_age=0L) paste0(remember_login_cookie_name(path),"=",secret,
  "; Path=",path,"; Max-Age=",as.integer(max_age),"; Secure; HttpOnly; SameSite=Lax")
remember_login_read_cookie <- function(header,path) {
  if(is.null(header)||length(header)!=1L||nchar(header)>32768L)return(NULL)
  items<-trimws(strsplit(header,";",fixed=TRUE)[[1]])
  prefix<-paste0(remember_login_cookie_name(path),"=")
  values<-substring(items[startsWith(items,prefix)],nchar(prefix)+1L)
  if(length(values)!=1L||is.null(remember_login_hash(values)))NULL else values
}
remember_login_response <- function(status="ok",cookie=NULL,http_status=200L,reason=NULL) {
  headers<-list(`Cache-Control`="no-store",Pragma="no-cache",`X-Content-Type-Options`="nosniff")
  if(!is.null(cookie))headers$`Set-Cookie`<-cookie
  body<-list(status=status)
  if(!is.null(reason))body$reason<-reason
  shiny:::httpResponse(http_status,"application/json; charset=utf-8",
    jsonlite::toJSON(body,auto_unbox=TRUE),headers)
}
# A session endpoint exchanges the HttpOnly cookie directly with the browser.
# Only operation status is returned: no login token or browser secret in JSON.
remember_login_session <- function(session,on_restore,on_status,on_begin,on_end,on_expire) {
  state<-new.env(parent=emptyenv());state$seq<-0L;state$closed<-FALSE;state$ready<-FALSE
  state$token<-NULL;state$hash<-NULL;state$pending_revoke<-character();state$expiry_cancel<-NULL
  state$csrf<-NULL;state$origin<-NULL;state$path<-NULL;state$url<-NULL;state$action<-NULL;state$desired<-"restore";state$used_seq<-NULL
  owner<-paste0("remember:",session$token)
  current<-function(seq)!state$closed&&identical(state$seq,seq)
  status<-function(kind,message,retry=NULL){if(!state$closed)on_status(kind,message,retry)}
  command<-function(action){
    state$seq<-state$seq+1L;state$action<-action
    if(state$ready)session$sendCustomMessage("futmondo_remember",list(url=state$url,csrf=state$csrf,
      ticket=state$seq,action=action,input=session$ns("remember_result")))
    invisible(TRUE)
  }
  expiry<-function(expires){
    if(is.function(state$expiry_cancel))state$expiry_cancel()
    state$expiry_cancel<-later::later(function(){if(!state$closed){state$token<-NULL;on_expire();command("revoke")}},
      max(0,as.numeric(difftime(expires,Sys.time(),units="secs"))))
  }
  revoke_pending<-function(){
    hashes<-unique(c(state$pending_revoke,state$hash))
    if(!length(hashes))return(promises::promise_resolve(TRUE))
    request<-promises::promise_resolve(TRUE)
    for(hash in hashes)local({h<-hash;request<<-promises::then(request,function(...){
      promises::then(remember_login_revoke(h,owner),function(...){state$pending_revoke<-setdiff(state$pending_revoke,h);TRUE})
    })})
    request
  }
  filter<-function(data,req){
    # Return only a fixed failure-stage label, never header values or credentials.
    rejection<-if(!identical(req$REQUEST_METHOD,"POST"))"method"
      else if(!remember_login_same_origin(req,state$origin))"origin"
      else if(!identical(req$HTTP_X_FUTMONDO_CSRF,state$csrf))"csrf"
      else if(!is.null(req$HTTP_SEC_FETCH_SITE)&&!req$HTTP_SEC_FETCH_SITE%in%c("same-origin","none"))"fetch_site" else NULL
    if(!is.null(rejection))return(remember_login_response("forbidden",http_status=403L,reason=rejection))
    size<-suppressWarnings(as.integer(req$CONTENT_LENGTH %||% "0"))
    if(length(size)!=1L||is.na(size)||size<1L||size>1024L)return(remember_login_response("invalid",http_status=400L))
    body<-tryCatch(jsonlite::fromJSON(rawToChar(req$rook.input$read(1025L)),simplifyVector=FALSE),error=function(e)NULL)
    if(!is.list(body)||!identical(body$action,state$action)||!isTRUE(body$ticket==state$seq)||state$closed)
      return(remember_login_response("cancelled",http_status=409L))
    if(identical(state$used_seq,state$seq))return(remember_login_response("cancelled",http_status=409L))
    state$used_seq<-state$seq
    seq<-state$seq;action<-state$action
    secret<-remember_login_read_cookie(req$HTTP_COOKIE,state$path)
    hash<-remember_login_hash(secret)
    clear<-function(code="signed_out")remember_login_response(code,remember_login_cookie(state$path))
    if(action=="confirm"){
      # A successful database write alone does not prove the browser kept the
      # HttpOnly cookie. Check it on one subsequent request without another DB read.
      if(!is.null(hash)&&identical(hash,state$hash)){
        status("success","Signed in. This device is remembered for 7 days.")
        return(remember_login_response("confirmed"))
      }
      status("error","Signed in for this session. Your browser did not retain saved access. Allow cookies for this app, then sign in again with Remember me checked.")
      return(remember_login_response("cookie_missing"))
    }
    if(!is.null(hash)){state$hash<-hash;state$pending_revoke<-unique(c(state$pending_revoke,hash))}
    clear_invalid<-function(code,message){
      on_end();status("error",message)
      promises::then(revoke_pending(),function(...)clear(code),function(error){
        status("error",paste0(message," Saved access could not be revoked; retry to revoke it."),"revoke")
        clear(code)
      })
    }
    safe<-function(request,success)promises::then(request,function(value){if(!current(seq))return(remember_login_response("cancelled",http_status=409L));success(value)},function(error){
      if(current(seq)){
        on_end()
        status("error",if(action=="revoke")"Signed out here. Saved access could not be revoked. Retry to revoke it."
          else if(action=="create")"Signed in for this session. Remembering this device failed; try again after logging in."
          else "Could not restore saved access right now. Retry or log in normally.",if(action%in%c("restore","revoke"))action else NULL)
      }
      if(action=="revoke")clear("revoke_failed")else remember_login_response("retry")
    })
    if(action=="revoke")return(safe(revoke_pending(),function(...){state$hash<-NULL;on_end();clear()}))
    if(!remember_login_enabled())return(remember_login_response("disabled"))
    if(action=="create"){
      if(!valid_login(state$token))return(remember_login_response("cancelled",http_status=409L))
      token<-state$token;new_secret<-remember_login_random();new_hash<-remember_login_hash(new_secret)
      created<-Sys.time();expires<-created+604800
      payload<-list(browser_hash=new_hash,encrypted_payload=remember_login_encrypt(token,created,expires,browser_hash=new_hash),
        created_at=remember_login_stamp(created),expires_at=remember_login_stamp(expires))
      remember_login_cleanup(owner)
      request<-promises::then(revoke_pending(),function(...){
        if(!current(seq))stop("Cancelled saved login")
        state$pending_revoke<-unique(c(state$pending_revoke,new_hash))
        remember_login_store("POST",payload=payload,owner=owner)
      })
      # A cancelled write may already have reached storage. Keep its hash for the
      # following logout/account-switch revocation; never issue its cookie late.
      return(safe(request,function(...){state$hash<-new_hash;expiry(expires)
        status("success","Signed in. Checking saved access on this device…")
        remember_login_response("remembered",remember_login_cookie(state$path,new_secret,max(0,floor(as.numeric(difftime(expires,Sys.time(),units="secs"))))))}))
    }
    if(action!="restore")return(remember_login_response("invalid",http_status=400L))
    if(is.null(hash)){on_end();return(remember_login_response("absent"))}
    on_begin()
    safe(remember_login_store(hash=hash,owner=owner),function(row){
      decoded<-tryCatch({if(!identical(row$browser_hash,hash))stop("Mismatched saved access");remember_login_decrypt(row)},error=function(e)NULL)
      if(is.null(decoded))return(clear_invalid("expired","Saved access has expired or is no longer valid. Log in again."))
      request<-remember_login_validate(decoded$token,owner)
      promises::then(request,function(...){
        if(!current(seq))return(remember_login_response("cancelled",http_status=409L))
        promises::then(promises::promise_resolve(on_restore(decoded$token)),function(allowed){
          if(!current(seq))return(remember_login_response("cancelled",http_status=409L))
          on_end()
          if(identical(allowed,FALSE)){state$token<-NULL;return(remember_login_response("blocked"))}
          state$token<-decoded$token;expiry(decoded$expires)
          remember_login_response("restored")
        })
      },function(error){
        if(!current(seq))return(remember_login_response("cancelled",http_status=409L))
        # 403 may be a hosting/firewall/storage policy problem; only an explicit
        # upstream401 establishes authentication rejection in the known contract.
        if(isTRUE(error$http_status==401L)){
          return(clear_invalid("rejected","Futmondo rejected saved access. Log in again."))
        }
        on_end();status("error","Could not validate saved access right now. Retry or log in normally.","restore")
        remember_login_response("retry")
      })
    })
  }
  # Initial HTTP routing and CSRF capability are created once, after client URL
  # metadata arrives. HTTPS/same-origin are checked on the cookie HTTP request;
  # an original browser WebSocket Origin, when available, remains binding.
  init<-shiny::observe({
    if(state$ready||!requireNamespace("sodium",quietly=TRUE))return()
    if(isTRUE(getOption("futmondo.offline",FALSE))&&!isTRUE(getOption("futmondo.remember_fixture",FALSE)))return()
    origin<-remember_login_origin(session$request$HTTP_ORIGIN)
    path<-remember_login_path(session$clientData$url_pathname)
    if(is.null(path))return()
    state$origin<-origin;state$path<-path;state$csrf<-remember_login_random()
    state$url<-session$registerDataObj("remember_login",NULL,function(data,req)shiny::withReactiveDomain(session,
      tryCatch(filter(data,req),error=function(error){on_end();status("error","Saved access is unavailable. Log in normally or retry.",if(state$action%in%c("restore","revoke"))state$action else NULL);remember_login_response("retry")})))
    state$ready<-TRUE
    session$onFlushed(function(){if(!state$closed&&(remember_login_enabled()||state$desired!="restore"))command(state$desired)},once=TRUE)
  })
  session$onSessionEnded(function(){state$closed<-TRUE;state$seq<-state$seq+1L;init$destroy()
    if(is.function(state$expiry_cancel))state$expiry_cancel();fm_transport_cancel(owner)})
  list(login=function(token,enabled){
    state$seq<-state$seq+1L;state$token<-token
    if(is.function(state$expiry_cancel))state$expiry_cancel()
    state$desired<-if(isTRUE(enabled))"create"else "revoke"
    if(state$ready)command(state$desired)
  },cancel=function(){state$seq<-state$seq+1L;fm_transport_cancel(owner)},logout=function(){state$desired<-"revoke";state$token<-NULL;if(is.function(state$expiry_cancel))state$expiry_cancel();command("revoke")},
  retry=function(action){if(action%in%c("restore","revoke"))command(action)},
  result=function(message){
    if(is.list(message)&&isTRUE(message$ticket==state$seq)&&identical(state$action,"create")&&identical(message$status,"remembered")){
      command("confirm");return(invisible(NULL))
    }
    if(is.list(message)&&isTRUE(message$ticket==state$seq)&&message$status%in%c("network","forbidden","invalid","disabled")){
      on_end();status("error","Saved access is unavailable. Log in normally or retry.",if(state$action%in%c("restore","revoke"))state$action else NULL)
    }
  },state=state)
}
