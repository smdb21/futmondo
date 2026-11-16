.coach_login_guards <- new.env(parent=emptyenv())
coach_claim_submission <- function(target,now=Sys.time()) {
  keys<-ls(.coach_login_guards);for(key in keys)if(as.numeric(difftime(now,.coach_login_guards[[key]],units="secs"))>300)rm(list=key,envir=.coach_login_guards)
  key<-paste(target$account,target$championship,target$team,sep="|")
  if(exists(key,.coach_login_guards,inherits=FALSE)||length(ls(.coach_login_guards))>=128L)return(FALSE)
  .coach_login_guards[[key]]<-now;TRUE
}
# Login-only mutation path. It must never use the factual read replay adapter.
coach_preference_key <- function(account_id,championship_id) paste("futmondo.coach.v1",account_id,championship_id,sep=":")
coach_info_status <- function(state,now=Sys.time()) {
  if(!is.list(state)||!identical(state$freshness,"fresh")||!identical(state$status,"ok")||
    !is.null(state$error)||!resource_snapshot_unexpired(state$data,now))return("unknown")
  stamp<-state$observed_at %||% state$fetched_at %||% attr(state$data,"observed_at")
  age<-as.numeric(difftime(now,fm_time(stamp),units="secs"))
  if(length(age)!=1L||!is.finite(age)||age< -60||age>20)return("unknown")
  info<-state$data
  if(!is.list(info)||!is.list(info$configuration)||!is.list(info$staff))return("unknown")
  mode<-mvp_text(info$configuration$enableCoach)
  if(!nzchar(mode))return("unknown")
  if(!identical(mode,"automatic"))return("unsupported")
  value<-fm_number(info$staff$coach)
  if(!is.finite(value))"unknown"else if(value==1)"hired"else if(value==0)"absent"else"unknown"
}
build_hire_coach_payload <- function(login,championship_id,user_team_id) {
  list(header=list(token=login[["token"]],userid=login[["userid"]]),
    query=list(championshipId=championship_id,userteamId=user_team_id,roundId=NULL,type="coach"),answer=list())
}
fm_async_hire_coach <- function(login,championship_id,user_team_id,owner) {
  payload<-jsonlite::toJSON(build_hire_coach_payload(login,championship_id,user_team_id),auto_unbox=TRUE,null="null")
  promises::then(fm_async_http("https://api.futmondo.com/1/userteam/hirecoach","POST",payload,
    c(`Content-Type`="application/json; charset=utf-8"),owner=owner),function(response){
      body<-jsonlite::fromJSON(rawToChar(response$content),simplifyVector=FALSE)
      # Embedded staff.coach remains zero in the successful captured response.
      isTRUE(identical(body$answer$code,API_CODE_OK)||identical(body$code,API_CODE_OK))
    })
}
# Compact durable observations contain no authentication or unrelated team data.
coach_snapshot_scope <- function(context) {
  fields<-c("account_id","championship_id","user_team_id","competition_id","season","scoring_version")
  setNames(lapply(fields,function(field)fm_scalar(context[[field]],"unknown")),fields)
}
coach_snapshot_id <- function(scope) paste0("rival-cache:coach-status-v1:",
  as.character(openssl::sha256(serialize(scope,NULL,version=2))))
persist_coach_snapshot <- function(info,context) {
  if(isTRUE(attr(info,"coach_from_saved"))||coach_info_status(resource_state(info))=="unknown")return(invisible(FALSE))
  scope<-coach_snapshot_scope(context);stamp<-fm_time(attr(info,"observed_at"))
  payload<-list(kind="coach_status",version=1L,scope=scope,status="complete",source="futmondo",
    source_observed_at=rival_finance_stamp(stamp),valid_until=rival_finance_stamp(stamp+20),
    mode=mvp_text(info$configuration$enableCoach),coach=fm_number(info$staff$coach))
  record<-rival_finance_evidence(scope$account_id,scope$championship_id,scope$season,"eligibility",
    coach_snapshot_id(scope),payload,stamp,competition_id=scope$competition_id)
  defer_persistence("supabase_post_direct",list("rival_finance_evidence",list(record),
    "account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at",TRUE),context=context)
}
get_coach_team_info <- function(login,championship_id,user_team_id,not_before=NULL) {
  context<-resource_context(login,championship_id,user_team_id);scope<-coach_snapshot_scope(context)
  saved<-resource_optional_cache_read(function()supabase_get("rival_finance_evidence_latest",list(
    account_id=paste0("eq.",scope$account_id),championship_id=paste0("eq.",scope$championship_id),
    competition_id=paste0("eq.",scope$competition_id),season=paste0("eq.",scope$season),
    evidence_type="eq.eligibility",source_id=paste0("eq.",coach_snapshot_id(scope)),
    select="account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at,payload",limit=1L)))
  records<-tryCatch(rival_evidence_decode(saved),error=function(e)list())
  for(record in records){
    result<-tryCatch({
      p<-record$payload;stamp<-fm_time(p$source_observed_at);deadline<-fm_time(p$valid_until)
      if(!is.list(p)||!identical(p$kind,"coach_status")||!identical(as.integer(p$version),1L)||
        !identical(p$status,"complete")||!identical(p$source,"futmondo")||
        !identical(record$source_id,coach_snapshot_id(scope))||!identical(record$evidence_type,"eligibility")||
        !all(vapply(c("account_id","championship_id","competition_id","season"),function(f)identical(fm_scalar(record[[f]]),scope[[f]]),logical(1)))||
        !is.list(p$scope)||!setequal(names(scope),names(p$scope))||
        !all(vapply(names(scope),function(f)identical(p$scope[[f]],scope[[f]]),logical(1)))||
        length(stamp)!=1L||is.na(stamp)||stamp>Sys.time()||
        !identical(as.numeric(fm_time(record$observed_at)),as.numeric(stamp))||
        length(deadline)!=1L||is.na(deadline)||abs(as.numeric(deadline-stamp,units="secs")-20)>.01||
        Sys.time()>=deadline||(!is.null(not_before)&&stamp<not_before))NULL else {
          info<-observed_data(list(configuration=list(enableCoach=p$mode),staff=list(coach=p$coach)),observed_at=stamp)
          attr(info,"valid_until")<-as.numeric(deadline);attr(info,"coach_from_saved")<-TRUE
          if(coach_info_status(resource_state(info))=="unknown")NULL else info
        }
    },error=function(e)NULL)
    if(!is.null(result))return(result)
  }
  get_user_team_info(login,championship_id,user_team_id)
}
# Login safety checks bypass worker replay, retaining the normal factual parser,
# durable observation writes and a strict twenty-second action deadline.
coach_read_team_info <- function(auth,target,session,is_current,not_before=NULL) {
  context<-resource_context(auth,target$championship,target$team,session_id=session$token)
  cache_context<-context;cache_context$session_id<-""
  # A read preflight invalidates only its local cache. The post-hire path below
  # still records action invalidation after a submission may have changed funds.
  invalidate_resources(cache_context,resources=paste0("team_info_",target$championship,"_",target$team),notify_availability=FALSE)
  request<-async_adapter_fetch("get_coach_team_info",
    list(login=auth,championship_id=target$championship,user_team_id=target$team,not_before=not_before),
    context,owner=paste0("coach:",session$token),is_current=is_current,
    priority="interactive",worker_bypass=TRUE)
  promises::then(request,function(info){
    if(is_current())persist_coach_snapshot(info,context)
    info
  })
}
coach_on_login_source <- function(session,login,championship,user_team,team_state,retry_team) {
  status<-shiny::reactiveVal("Checking preference…");preference<-shiny::reactiveVal(NULL);key_value<-shiny::reactiveVal(NULL)
  state<-new.env(parent=emptyenv());state$generation<-0L;state$auth<-NULL;state$target<-NULL;state$submitted<-FALSE
  state$closed<-FALSE;state$confirming<-FALSE;state$done<-FALSE;state$confirmation_after<-NULL
  state$read_started<-FALSE;state$confirmation_read_started<-FALSE
  verified_info<-shiny::reactiveVal(NULL);read_error<-shiny::reactiveVal(NULL)
  session$onSessionEnded(function(){state$closed<-TRUE;state$generation<-state$generation+1L;fm_transport_cancel(paste0("coach:",session$token))})
  identity<-function(){auth<-login();if(!valid_login(auth))return(NULL);list(account=mvp_text(auth[["userid"]]),championship=mvp_text(championship()),team=mvp_text(user_team()))}
  current<-function(generation,target)!state$closed&&identical(state$generation,generation)&&identical(shiny::isolate(identity()),target)
  read_current<-function(confirming=FALSE){
    if(confirming)state$confirmation_read_started<-TRUE else state$read_started<-TRUE
    g<-state$generation;target<-state$target;auth<-state$auth
    still_current<-function()current(g,target)
    status(if(confirming)"Confirming coach…"else "Checking coach…")
    finish<-function(info=NULL,error=NULL){
      if(!still_current())return(NULL)
      result<-resource_state(info,error=error)
      if(confirming){
        state$confirming<-FALSE;state$done<-TRUE
        stamp<-fm_time(result$observed_at %||% result$fetched_at)
        confirmed<-coach_info_status(result)=="hired"&&length(stamp)==1L&&!is.na(stamp)&&stamp>=state$confirmation_after
        status(if(confirmed)"Coach hired"else "Hire could not be confirmed. Check in LigaRadar; no automatic retry.")
      }else{
        read_error(if(!is.null(error))resource_failure_message(error)else NULL)
        verified_info(result)
      }
      NULL
    }
    request<-tryCatch(coach_read_team_info(auth,target,session,still_current,
      not_before=if(confirming)state$confirmation_after else NULL),
      error=function(error)promises::promise_reject(error))
    promises::then(request,function(info)finish(info),function(error)finish(error=error))
    invisible(NULL)
  }
  set_preference<-function(key,enabled){
    if(!identical(mvp_text(key),shiny::isolate(key_value()))||!is.logical(enabled)||length(enabled)!=1L||is.na(enabled))return(FALSE)
    preference(enabled);if(!enabled&&!state$submitted)status("Disabled");TRUE
  }
  shiny::observe({
    auth<-login()
    if(!valid_login(auth)){
      state$generation<-state$generation+1L;state$auth<-NULL;state$target<-NULL;state$submitted<-state$confirming<-state$done<-FALSE
      state$read_started<-state$confirmation_read_started<-FALSE;verified_info(NULL);read_error(NULL)
      preference(NULL);key_value(NULL);status("Sign in to check coach");return()
    }
    target<-identity();if(any(!nzchar(unlist(target))))return()
    if(!identical(state$auth,auth)){
      state$generation<-state$generation+1L;state$auth<-auth;state$target<-target;state$submitted<-state$confirming<-state$done<-FALSE
      state$read_started<-state$confirmation_read_started<-FALSE;verified_info(NULL);read_error(NULL)
      preference(NULL);key_value(coach_preference_key(target$account,target$championship));status("Checking preference…")
    }
    if(!identical(state$target,target)){
      if(!isTRUE(state$done)){state$generation<-state$generation+1L;state$done<-TRUE}
      status("Coach check runs for the league selected at login");return()
    }
    enabled<-preference();if(is.null(enabled))return()
    if(!enabled&&!state$submitted){status("Disabled");return()}
    if(state$done)return()
    info<-team_state();check<-coach_info_status(info)
    if(state$confirming){
      stamp<-fm_time(info$observed_at %||% info$fetched_at)
      if(identical(info$status,"loading"))return()
      if(check=="unknown"||length(stamp)!=1L||is.na(stamp)||stamp<state$confirmation_after){
        if(!state$confirmation_read_started)read_current(confirming=TRUE)
        return()
      }
      state$confirming<-FALSE;state$done<-TRUE
      status(if(check=="hired")"Coach hired"else "Hire could not be confirmed. Check in LigaRadar; no automatic retry.")
      return()
    }
    if(state$submitted)return()
    if(identical(info$status,"loading")){status("Checking coach…");return()}
    if(check=="unknown"&&!is.null(verified_info())){
      info<-verified_info();check<-coach_info_status(info)
    }
    if(check=="hired"){state$done<-TRUE;status("Coach already hired");return()}
    if(check=="unsupported"){state$done<-TRUE;status("Coach mode is unsupported");return()}
    if(check!="absent"){
      if(!state$read_started){read_current();return()}
      if(is.null(verified_info())){status("Checking coach…");return()}
      issue<-read_error()
      status(if(!is.null(issue))paste("Coach check:",issue)else
        "Coach status unavailable: the team response has no fresh, verified coach status. No hire sent.")
      # A later successful shared read may recover before submission. Unknown
      # data never authorizes a hire and never permanently closes the login check.
      return()
    }
    # Latch before dispatch; duplicate observers can never submit a second request.
    if(!coach_claim_submission(target)){state$done<-TRUE;status("Coach action already checked recently. Verify in LigaRadar.");return()}
    state$submitted<-TRUE;status("Hiring coach…");g<-state$generation;captured<-target
    confirm<-function(){
      if(!current(g,captured))return(NULL)
      state$confirming<-TRUE;state$confirmation_after<-Sys.time();status("Confirming coach…")
      invalidate_resources(resource_context(auth,target$championship,target$team),pattern="team_info_|get_user_team_info:")
      retry_team();NULL
    }
    promises::then(fm_async_hire_coach(auth,target$championship,target$team,paste0("coach:",session$token)),
      function(ok)confirm(),function(error)confirm())
    invisible(NULL)
  })
  list(status=shiny::reactive(status()),preference=shiny::reactive(preference()),key=shiny::reactive(key_value()),set_preference=set_preference)
}
coach_preference_script <- function() shiny::tags$script(shiny::HTML("(function(){
  if(window.futmondoCoachPreference)return; window.futmondoCoachPreference=true;
  Shiny.addCustomMessageHandler('coach_preference',function(m){
    var enabled=true,available=true;
    try {var saved=localStorage.getItem(m.key);if(saved==='false')enabled=false;}catch(e){available=false;}
    var control=document.getElementById(m.checkbox);if(control){control.checked=enabled;$(control).trigger('change');}
    Shiny.setInputValue(m.input,{key:m.key,enabled:enabled,available:available,nonce:m.nonce},{priority:'event'});
  });
  Shiny.addCustomMessageHandler('coach_preference_save',function(m){var ok=true;try{localStorage.setItem(m.key,m.enabled?'true':'false');}catch(e){ok=false;} Shiny.setInputValue(m.input,{key:m.key,ok:ok,nonce:Date.now()},{priority:'event'});});
})();"))
