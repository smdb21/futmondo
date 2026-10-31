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
  if(!is.list(state)||!identical(state$freshness,"fresh")||!identical(state$status,"ok"))return("unknown")
  stamp<-state$observed_at %||% state$fetched_at %||% attr(state$data,"observed_at")
  age<-as.numeric(difftime(now,fm_time(stamp),units="secs"))
  if(length(age)!=1L||!is.finite(age)||age< -60||age>20)return("unknown")
  info<-state$data
  if(!identical(mvp_text(info$configuration$enableCoach),"automatic"))return("unsupported")
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
coach_on_login_source <- function(session,login,championship,user_team,team_state,retry_team) {
  status<-shiny::reactiveVal("Checking preference…");preference<-shiny::reactiveVal(NULL);key_value<-shiny::reactiveVal(NULL)
  state<-new.env(parent=emptyenv());state$generation<-0L;state$auth<-NULL;state$target<-NULL;state$submitted<-FALSE
  state$closed<-FALSE;state$confirming<-FALSE;state$done<-FALSE;state$confirmation_after<-NULL
  session$onSessionEnded(function(){state$closed<-TRUE;state$generation<-state$generation+1L;fm_transport_cancel(paste0("coach:",session$token))})
  identity<-function(){auth<-login();if(!valid_login(auth))return(NULL);list(account=mvp_text(auth[["userid"]]),championship=mvp_text(championship()),team=mvp_text(user_team()))}
  current<-function(generation,target)!state$closed&&identical(state$generation,generation)&&identical(shiny::isolate(identity()),target)
  set_preference<-function(key,enabled){
    if(!identical(mvp_text(key),shiny::isolate(key_value()))||!is.logical(enabled)||length(enabled)!=1L||is.na(enabled))return(FALSE)
    preference(enabled);if(!enabled&&!state$submitted)status("Disabled");TRUE
  }
  shiny::observe({
    auth<-login()
    if(!valid_login(auth)){
      state$generation<-state$generation+1L;state$auth<-NULL;state$target<-NULL;state$submitted<-state$confirming<-state$done<-FALSE
      preference(NULL);key_value(NULL);status("Sign in to check coach");return()
    }
    target<-identity();if(any(!nzchar(unlist(target))))return()
    if(!identical(state$auth,auth)){
      state$generation<-state$generation+1L;state$auth<-auth;state$target<-target;state$submitted<-state$confirming<-state$done<-FALSE
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
      if(info$status%in%c("unavailable","error","failed","stale")||identical(info$freshness,"stale")){state$confirming<-FALSE;state$done<-TRUE;status("Hire could not be confirmed. Check in Futmondo; no automatic retry.");return()}
      if(length(stamp)!=1L||is.na(stamp)||stamp<state$confirmation_after)return()
      state$confirming<-FALSE;state$done<-TRUE
      status(if(check=="hired")"Coach hired"else "Hire could not be confirmed. Check in Futmondo; no automatic retry.")
      return()
    }
    if(state$submitted)return()
    if(identical(info$status,"loading")){status("Checking coach…");return()}
    if(check=="hired"){state$done<-TRUE;status("Coach already hired");return()}
    if(check=="unsupported"){state$done<-TRUE;status("Coach mode is unsupported");return()}
    if(check!="absent"){state$done<-TRUE;status("Coach status unavailable");return()}
    # Latch before dispatch; duplicate observers can never submit a second request.
    if(!coach_claim_submission(target)){state$done<-TRUE;status("Coach action already checked recently. Verify in Futmondo.");return()}
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
