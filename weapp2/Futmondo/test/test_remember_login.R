#!/usr/bin/env Rscript
options(futmondo.offline=TRUE,futmondo.remember_fixture=TRUE)
suppressPackageStartupMessages(source("global.R"))
checks<-0L
check<-function(name,code){force(code);checks<<-checks+1L;cat("PASS",name,"\n")}
await<-function(p){done<-FALSE;value<-NULL;error<-NULL;promises::then(p,function(x){value<<-x;done<<-TRUE},function(e){error<<-e;done<<-TRUE});for(i in seq_len(300L)){later::run_now(.001);if(done)break};stopifnot(done);if(!is.null(error))stop(error);value}
failed<-function(code)inherits(tryCatch({force(code);NULL},error=function(e)e),"error")
auth<-c(token="synthetic-token",userid="synthetic-user",user_name="fixture@example.invalid")
key<-sodium::random(32);now<-as.POSIXct("2026-11-01 12:00:00",tz="UTC")+.123456
new_row<-function(){hash<-remember_login_hash(remember_login_random());list(browser_hash=hash,encrypted_payload=remember_login_encrypt(auth,now,now+604800,key,browser_hash=hash),created_at=remember_login_stamp(now),expires_at=remember_login_stamp(now+604800),revoked_at=NULL)}
row<-new_row()
check("authenticated JSON envelope roundtrips fractional timestamps and PostgREST UTC offsets",{
 row$created_at<-sub("Z$","+00:00",row$created_at);row$expires_at<-sub("Z$","+00:00",row$expires_at)
 stopifnot(identical(remember_login_decrypt(row,now+1,key)$token,auth),!grepl(auth[["token"]],row$encrypted_payload,fixed=TRUE),
   !identical(new_row()$encrypted_payload,new_row()$encrypted_payload))
})
check("expiry is absolute and bound to authenticated metadata",{
 stopifnot(failed(remember_login_decrypt(row,now+604800,key)))
 extended<-row;extended$expires_at<-remember_login_stamp(now+604801)
 stopifnot(failed(remember_login_decrypt(extended,now,key)))
 changed<-row;changed$created_at<-remember_login_stamp(now+10);changed$expires_at<-remember_login_stamp(now+604810)
 stopifnot(failed(remember_login_decrypt(changed,now+20,key)))
})
check("ciphertext tampering, revoked rows, key mismatch and oversize data fail closed",{
 bad<-row;packed<-jsonlite::base64_dec(bad$encrypted_payload);packed[30]<-as.raw(bitwXor(as.integer(packed[30]),1L));bad$encrypted_payload<-jsonlite::base64_enc(packed)
 stopifnot(failed(remember_login_decrypt(bad,now,key)),failed(remember_login_decrypt(row,now,sodium::random(32))))
 bad<-row;bad$browser_hash<-remember_login_hash(remember_login_random());stopifnot(failed(remember_login_decrypt(bad,now,key)))
 bad<-row;bad$revoked_at<-remember_login_stamp(now);stopifnot(failed(remember_login_decrypt(bad,now,key)))
 bad<-row;bad$encrypted_payload<-paste(rep("x",16385),collapse="");stopifnot(failed(remember_login_decrypt(bad,now,key)))
})
check("cookies are HTTPS HttpOnly path-scoped and reject malformed/duplicate secrets",{
 secret<-remember_login_random();path<-"/app/futmondo/";cookie<-remember_login_cookie(path,secret,604800)
 stopifnot(all(vapply(c("Secure","HttpOnly","SameSite=Lax","Path=/app/futmondo/","Max-Age=604800"),grepl,logical(1),x=cookie,fixed=TRUE)),!grepl("Domain=",cookie,fixed=TRUE),
   identical(remember_login_read_cookie(strsplit(cookie,";",fixed=TRUE)[[1]][1],path),secret),is.null(remember_login_read_cookie(paste(cookie,cookie,sep=";"),path)),
   is.null(remember_login_hash("bad")),is.null(remember_login_path("/bad/../")),is.null(remember_login_path("/bad%2f")),is.null(remember_login_origin("http://app.invalid")))
})
check("asynchronous validation reuses one official league response for startup",{
 original_http<-fm_async_http;http_calls<-0L;heartbeat<-FALSE
 fm_async_http<-function(...) {http_calls<<-http_calls+1L;promises::promise(function(resolve,reject){later::later(function(){resolve(list(content=charToRaw('{"answer":{"championships":[{"_id":"synthetic-league","userteam":{"_id":"synthetic-team"}}]}}')))},.04)})}
 clear_api_cache(auth[["userid"]]);later::later(function()heartbeat<<-TRUE,.005)
 started<-Sys.time();pending<-remember_login_validate(auth);dispatch<-as.numeric(difftime(Sys.time(),started,units="secs"))
 await(pending);leagues<-get_active_championships(auth)
 stopifnot(http_calls==1L,heartbeat,dispatch<.1,identical(leagues$championships[[1]]$id,"synthetic-league"),identical(leagues$championships[[1]]$userteam$id,"synthetic-team"))
 fm_async_http<-original_http
})
# In-memory persistence is the only storage boundary in the endpoint tests.
records<-new.env(parent=emptyenv());db_calls<-list();db_fail<-FALSE;validate_status<-"ok";validations<-0L
original_enabled<-remember_login_enabled;remember_login_enabled<-function()TRUE
remember_login_key<-function()key
remember_login_store<-function(method="GET",hash=NULL,payload=NULL,owner="",expires_before=NULL){
 db_calls[[length(db_calls)+1L]]<<-list(method=method,hash=hash)
 if(db_fail)return(promises::promise_reject(simpleError("sensitive database diagnostic")))
 if(!is.null(expires_before)){
   for(k in ls(records))if(remember_login_time(records[[k]]$expires_at)<=remember_login_time(expires_before))rm(list=k,envir=records)
   return(promises::promise_resolve(TRUE))
 }
 result<-switch(method,GET=records[[hash]],POST={records[[payload$browser_hash]]<-payload;TRUE},DELETE={if(!is.null(records[[hash]]))rm(list=hash,envir=records);TRUE})
 promises::promise_resolve(result)
}
remember_login_validate<-function(token,owner=""){
 validations<<-validations+1L
 if(validate_status=="ok")promises::promise_resolve(TRUE) else promises::promise_reject(
  if(validate_status=="unauthorized")structure(list(message="HTTP401",http_status=401L),class=c("error","condition")) else simpleError("private API diagnostic"))
}
fixture<-function(websocket_origin="https://app.invalid"){
 s<-new.env(parent=emptyenv());s$token<-remember_login_random();s$request<-list(HTTP_ORIGIN=websocket_origin)
 s$clientData<-shiny::reactiveValues(url_pathname="/futmondo/");s$messages<-list();s$ends<-list();s$filter<-NULL
 s$ns<-function(id)paste0("login-",id);s$sendCustomMessage<-function(type,message){s$messages[[length(s$messages)+1L]]<-list(type=type,message=message)}
 s$registerDataObj<-function(name,data,filterFunc){s$filter<-filterFunc;"session/fixture/dataobj/remember?w="}
 s$onFlushed<-function(fn,once=TRUE)fn();s$onSessionEnded<-function(fn){s$ends[[length(s$ends)+1L]]<-fn}
 f<-new.env(parent=emptyenv());f$statuses<-list();f$auth<-NULL;f$begin<-0L;f$end<-0L;f$expired<-FALSE;f$s<-s
 f$r<-remember_login_session(s,function(token)f$auth<-token,function(kind,message,retry=NULL)f$statuses[[length(f$statuses)+1L]]<-list(kind=kind,message=message,retry=retry),
  function()f$begin<-f$begin+1L,function()f$end<-f$end+1L,function()f$expired<-TRUE)
 f$init<-function()shiny:::flushReact()
 f$call<-function(cookie=NULL,method="POST",origin="https://app.invalid",csrf=f$r$state$csrf,ticket=f$r$state$seq,action=f$r$state$action,host="app.invalid",fetch_site="same-origin"){
  body<-charToRaw(jsonlite::toJSON(list(ticket=ticket,action=action),auto_unbox=TRUE))
  req<-list(REQUEST_METHOD=method,HTTP_ORIGIN=origin,HTTP_HOST=host,HTTP_X_FUTMONDO_CSRF=csrf,CONTENT_LENGTH=as.character(length(body)),
    HTTP_COOKIE=cookie,HTTP_SEC_FETCH_SITE=fetch_site,rook.input=list(read=function(...)body))
  result<-s$filter(NULL,req);if(promises::is.promise(result))await(result)else result
 }
 f$close<-function(){for(fn in s$ends)fn()};f
}
new_cookie<-function(f,response)strsplit(response$headers$`Set-Cookie`,";",fixed=TRUE)[[1]][1]
check("absence and malformed credentials do not query storage",{
 f<-fixture();f$init();before<-length(db_calls);res<-f$call("bad=bad");stopifnot(res$status==200L,length(db_calls)==before,f$begin==0L);f$close()
})
check("GET cross-origin and missing CSRF requests are rejected before storage",{
 f<-fixture();f$init();before<-length(db_calls)
 stopifnot(f$call(method="GET")$status==403L,f$call(origin="https://evil.invalid")$status==403L,f$call(csrf="wrong")$status==403L,length(db_calls)==before);f$close()
})
check("proxied sessions without a browser WebSocket Origin still save and restore",{
 f<-fixture(NULL);f$init();stopifnot(f$r$state$ready,is.null(f$r$state$origin))
 f$r$login(auth,TRUE);issued<-f$call();proxy_cookie<-new_cookie(f,issued)
 stopifnot(!any(vapply(f$statuses,function(x)grepl("remembered for 7 days",x$message,fixed=TRUE),logical(1))))
 f$r$result(list(ticket=f$r$state$seq,status="remembered"));before<-length(db_calls);f$call(proxy_cookie)
 stopifnot(identical(f$r$state$action,"confirm"),length(db_calls)==before,
   grepl("remembered for 7 days",tail(f$statuses,1)[[1]]$message,fixed=TRUE))
 g<-fixture(NULL);g$init();g$call(proxy_cookie);stopifnot(identical(g$auth,auth))
 g$r$logout();g$call(proxy_cookie);f$close();g$close();validations<-0L
})
check("proxy fallback rejects missing or mismatched HTTP origin and host before storage",{
 f<-fixture(NULL);f$init();before<-length(db_calls)
 stopifnot(f$call(origin=NULL)$status==403L,f$call(origin="https://evil.invalid",fetch_site="cross-site")$status==403L,
   f$call(host="internal.invalid",fetch_site=NULL)$status==403L,f$call(host=NULL)$status==403L,
   f$call(host="app.invalid,evil.invalid")$status==403L,f$call(origin="http://app.invalid")$status==403L,
   f$call(csrf="wrong")$status==403L,length(db_calls)==before)
 stopifnot(remember_login_same_origin(list(HTTP_ORIGIN="https://APP.invalid:443",HTTP_HOST="app.invalid")),
   !remember_login_same_origin(list(HTTP_ORIGIN="https://evil.invalid",HTTP_HOST="evil.invalid"),"https://app.invalid"))
 f$close()
})
check("rewritten proxy hosts require browser-controlled exact same-origin metadata",{
 f<-fixture(NULL);f$init();before<-length(db_calls)
 for(site in list(NULL,"none","same-site","cross-site","invalid"))
   stopifnot(f$call(host="127.0.0.1:3838",fetch_site=site)$status==403L)
 stopifnot(f$call(host="127.0.0.1:3838",csrf="wrong")$status==403L,length(db_calls)==before)
 f$r$login(auth,TRUE);issued<-f$call(host="127.0.0.1:3838");proxy_cookie<-new_cookie(f,issued)
 f$r$result(list(ticket=f$r$state$seq,status="remembered"));f$call(proxy_cookie,host="127.0.0.1:3838")
 stopifnot(grepl("remembered for 7 days",tail(f$statuses,1)[[1]]$message,fixed=TRUE))
 g<-fixture(NULL);g$init();g$call(proxy_cookie,host="127.0.0.1:3838");stopifnot(identical(g$auth,auth))
 g$r$logout();g$call(proxy_cookie,host="127.0.0.1:3838");f$close();g$close();validations<-0L
})
check("browser cookie rejection cannot produce a remembered confirmation",{
 f<-fixture(NULL);f$init();f$r$login(auth,TRUE);f$call()
 f$r$result(list(ticket=f$r$state$seq,status="remembered"));before<-length(db_calls);f$call()
 stopifnot(length(db_calls)==before,grepl("did not retain saved access",tail(f$statuses,1)[[1]]$message,fixed=TRUE),
   !any(vapply(f$statuses,function(x)grepl("remembered for 7 days",x$message,fixed=TRUE),logical(1))))
 f$r$logout();f$call();f$close()
})
check("unchecked login before endpoint initialization never creates a credential",{
 f<-fixture();f$r$login(auth,FALSE);f$init();before<-length(db_calls);res<-f$call()
 stopifnot(identical(f$r$state$action,"revoke"),length(db_calls)==before,!length(ls(records)),grepl("Max-Age=0",res$headers$`Set-Cookie`,fixed=TRUE));f$close()
})
check("opt-in issues opaque cookie once and preserves absolute expiry across fresh server sessions",{
 f<-fixture();f$init();f$r$login(auth,TRUE);issued<-f$call();cookie<-new_cookie(f,issued)
 stopifnot(!grepl(auth[["token"]],issued$content,fixed=TRUE),identical(issued$headers$`Cache-Control`,"no-store"),f$call()$status==409L)
 credential_hash<-remember_login_hash(remember_login_read_cookie(cookie,"/futmondo/"));saved<-records[[credential_hash]]
 g<-fixture();g$init();restored<-g$call(cookie)
 stopifnot(identical(g$auth,auth),g$begin==1L,is.null(restored$headers$`Set-Cookie`),identical(records[[credential_hash]]$expires_at,saved$expires_at),validations==1L)
 f$close();g$close()
})
check("temporary restore failure preserves cookie and offers retry",{
 f<-fixture();f$init();db_fail<-TRUE;res<-f$call(cookie);db_fail<-FALSE
 stopifnot(is.null(res$headers$`Set-Cookie`),is.null(f$auth),identical(tail(f$statuses,1)[[1]]$retry,"restore"),!grepl("sensitive",tail(f$statuses,1)[[1]]$message))
 f$r$retry("restore");res<-f$call(cookie);stopifnot(identical(f$auth,auth));f$close()
})
check("transient validation failure preserves credential, explicit401 revokes it",{
 f<-fixture();f$init();validate_status<-"temporary";res<-f$call(cookie)
 stopifnot(is.null(f$auth),is.null(res$headers$`Set-Cookie`))
 f$r$retry("restore");validate_status<-"unauthorized";res<-f$call(cookie);validate_status<-"ok"
 stopifnot(is.null(f$auth),grepl("Max-Age=0",res$headers$`Set-Cookie`,fixed=TRUE),is.null(records[[credential_hash]]));f$close()
})
check("logout clears cookie and only revokes this browser; storage failure stays visible and retryable",{
 f<-fixture();f$init();f$r$login(auth,TRUE);issued<-f$call();cookie<-new_cookie(f,issued);hash<-remember_login_hash(remember_login_read_cookie(cookie,"/futmondo/"))
 other<-fixture();other$init();other$r$login(auth,TRUE);other$call();other_hash<-other$r$state$hash
 f$r$logout();db_fail<-TRUE;res<-f$call(cookie);db_fail<-FALSE
 stopifnot(grepl("Max-Age=0",res$headers$`Set-Cookie`,fixed=TRUE),identical(tail(f$statuses,1)[[1]]$retry,"revoke"),!is.null(records[[other_hash]]))
 f$r$retry("revoke");f$call();stopifnot(is.null(records[[hash]]),!is.null(records[[other_hash]]));f$close();other$close()
})
check("logout during delayed creation cannot reissue cookie and revokes in-flight record",{
 original_store<-remember_login_store;resolve_post<-NULL
 remember_login_store<-function(method="GET",...){if(method=="POST")promises::promise(function(resolve,reject)resolve_post<<-function(){awaited<-original_store(method,...);promises::then(awaited,resolve,reject)})else original_store(method,...)}
 f<-fixture();f$init();f$r$login(auth,TRUE)
 body<-charToRaw(jsonlite::toJSON(list(ticket=f$r$state$seq,action="create"),auto_unbox=TRUE))
 pending<-f$s$filter(NULL,list(REQUEST_METHOD="POST",HTTP_ORIGIN="https://app.invalid",HTTP_X_FUTMONDO_CSRF=f$r$state$csrf,CONTENT_LENGTH=as.character(length(body)),rook.input=list(read=function(...)body)))
 for(i in 1:10)later::run_now(0)
 stopifnot(is.function(resolve_post));f$r$logout();resolve_post();res<-await(pending)
 stopifnot(res$status==409L,is.null(res$headers$`Set-Cookie`))
 remember_login_store<-original_store;pending_hashes<-f$r$state$pending_revoke;f$call();stopifnot(length(pending_hashes)>0L,all(!vapply(pending_hashes,exists,logical(1),envir=records,inherits=FALSE)));f$close()
})
check("account switching ignores an earlier delayed restore completion",{
 f<-fixture();f$init();f$r$login(auth,TRUE);issued<-f$call();cookie<-new_cookie(f,issued);f$close()
 original_validate<-remember_login_validate;resolve_validation<-NULL
 remember_login_validate<-function(...)promises::promise(function(resolve,reject)resolve_validation<<-resolve)
 g<-fixture();g$init()
 body<-charToRaw(jsonlite::toJSON(list(ticket=g$r$state$seq,action="restore"),auto_unbox=TRUE))
 pending<-g$s$filter(NULL,list(REQUEST_METHOD="POST",HTTP_ORIGIN="https://app.invalid",HTTP_X_FUTMONDO_CSRF=g$r$state$csrf,HTTP_COOKIE=cookie,CONTENT_LENGTH=as.character(length(body)),rook.input=list(read=function(...)body)))
 for(i in 1:10)later::run_now(0)
 stopifnot(is.function(resolve_validation));new_auth<-auth;new_auth[["userid"]]<-"another-synthetic-user"
 g$r$cancel();g$r$login(new_auth,TRUE);resolve_validation(TRUE);res<-await(pending)
 stopifnot(is.null(g$auth),identical(g$r$state$token,new_auth),res$status==409L,is.null(res$headers$`Set-Cookie`))
 remember_login_validate<-original_validate;g$close()
})
check("disabled restoration still permits logout cleanup of an existing cookie",{
 f<-fixture();f$init();f$r$login(auth,TRUE);issued<-f$call();cookie<-new_cookie(f,issued)
 hash<-f$r$state$hash;f$close();remember_login_enabled<-function()FALSE
 g<-fixture();g$init();g$r$logout();res<-g$call(cookie)
 stopifnot(grepl("Max-Age=0",res$headers$`Set-Cookie`,fixed=TRUE),is.null(records[[hash]]));g$close()
})
remember_login_enabled<-original_enabled
check("feature is gated and ordinary login remains available without configuration",{
 old<-Sys.getenv("FUTMONDO_REMEMBER_ENABLED");Sys.unsetenv("FUTMONDO_REMEMBER_ENABLED")
 stopifnot(!remember_login_enabled(),grepl('disabled="disabled"',as.character(login_UI("login")),fixed=TRUE))
 Sys.setenv(FUTMONDO_REMEMBER_ENABLED=old)
})
cat("REMEMBER LOGIN:",checks,"passed / 0 failed\n")
