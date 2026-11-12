# Account-wide last league. Database storage survives sessions and redeployment.
fm_league_store <- function(user_id,championship_id=NULL,selected_at=NULL,owner="") {
  valid_id<-function(x)is.character(x)&&length(x)==1L&&!is.na(x)&&grepl('^[A-Za-z0-9_-]{1,256}$',x,perl=TRUE)
  if(!valid_id(user_id)||!is.null(championship_id)&&!valid_id(championship_id))
    return(promises::promise_reject(simpleError('Invalid league preference identity')))
  if(!nzchar(get_sb_url())||!nzchar(get_sb_key()))return(promises::promise_reject(simpleError('League preference storage is not configured')))
  reading<-is.null(championship_id)
  query<-if(reading)list(user_id=paste0('eq.',user_id),select='championship_id,selected_at',limit='1')else list(on_conflict='user_id')
  url<-httr::modify_url(paste0(get_sb_url(),'/rest/v1/user_league_preferences'),query=query)
  headers<-c(apikey=get_sb_key(),Authorization=paste('Bearer',get_sb_key()),`Content-Type`='application/json',Prefer='resolution=merge-duplicates,return=minimal')
  body<-if(reading)NULL else jsonlite::toJSON(list(user_id=user_id,championship_id=championship_id,selected_at=selected_at),auto_unbox=TRUE)
  promises::then(fm_async_http(url,if(reading)'GET'else'POST',body,headers,owner=owner,timeout=8),function(response){
    if(!reading)return(TRUE)
    rows<-jsonlite::fromJSON(rawToChar(response$content),simplifyVector=FALSE)
    if(!is.list(rows)||length(rows)>1L)stop('Invalid league preference response')
    if(length(rows))rows[[1L]]else NULL
  })
}
fm_league_preferences <- function(session,login_token,leagues,store=fm_league_store) {
  selected<-shiny::reactiveVal(NULL);issue<-shiny::reactiveVal(NULL);revision<-shiny::reactiveVal(0L)
  state<-NULL;generation<-0L;closed<-FALSE
  current<-function(s)!closed&&identical(s$generation,generation)
  notify<-function()revision(shiny::isolate(revision())+1L)
  report<-function(s,value)if(current(s))issue(value)
  ids<-function()vapply(fm_supported_leagues(leagues()),function(x)fm_scalar(x$id %||% x$`_id`,''),character(1))
  save<-function(s){
    if(s$inflight||is.null(s$pending))return(invisible(NULL))
    value<-s$pending;s$pending<-NULL;s$inflight<-TRUE
    failed<-function(error){s$inflight<-FALSE;if(is.null(s$pending))s$pending<-value;report(s,'save');NULL}
    promises::then(store(s$user_id,value$id,value$at,owner=paste0(session$token,':league-save')),function(ok){
      s$inflight<-FALSE;if(!isTRUE(ok))return(failed(NULL))
      s$saved<-value$id;report(s,NULL);save(s);NULL
    },failed)
    invisible(NULL)
  }
  restore<-function(s){
    s$loaded<-FALSE;s$restore_failed<-FALSE
    if(!s$navigated)s$initialized<-FALSE
    promises::then(store(s$user_id,owner=session$token),function(row){
      if(!current(s))return(NULL)
      if(!s$navigated)s$saved<-if(is.list(row))row$championship_id else NULL
      s$loaded<-TRUE;report(s,NULL);notify();NULL
    },function(error){if(current(s)){s$loaded<-TRUE;s$restore_failed<-TRUE;report(s,'load');notify()};NULL})
    invisible(NULL)
  }
  shiny::observeEvent(login_token(),{
    token<-login_token();account<-if(valid_login(token))as.character(token[['userid']])else NULL
    if(!is.null(state)&&identical(state$user_id,account))return()
    generation<<-generation+1L;state<<-NULL;selected(NULL);issue(NULL)
    if(is.null(account))return()
    s<-new.env(parent=emptyenv());s$user_id<-account;s$generation<-generation
    s$loaded<-FALSE;s$initialized<-FALSE;s$navigated<-FALSE;s$saved<-NULL;s$pending<-NULL;s$inflight<-FALSE;s$awaiting<-NULL
    s$initial_input<-shiny::isolate(session$input$selected_league);state<<-s;restore(s)
  },ignoreNULL=FALSE,priority=100)
  shiny::observe({
    revision();login_token();s<-state;if(is.null(s)||!s$loaded)return()
    available<-ids();value<-shiny::isolate(selected())
    target<-if(length(s$saved)==1L&&s$saved %in% available&&((!s$initialized&&!s$navigated)||length(value)!=1L||!value %in% available))s$saved else
      if(length(value)==1L&&value %in% available)value else if(length(available))available[1L]else character()
    initializing<-!s$initialized;changed<-!identical(value,target);s$initialized<-TRUE
    if(changed){s$awaiting<-if(length(target))target else NULL;selected(target)}
    if((changed||initializing)&&length(target)==1L&&!s$restore_failed&&(length(s$saved)!=1L||!s$saved %in% available)){
      s$pending<-list(id=target,at=format(Sys.time(),'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'));save(s)
    }
  })
  shiny::observeEvent(session$input$selected_league,{
    s<-state;if(is.null(s))return()
    value<-session$input$selected_league
    if(length(value)!=1L||!value %in% ids())return()
    if(!is.null(s$awaiting)){if(identical(value,s$awaiting))s$awaiting<-NULL;return()}
    if(!s$loaded&&identical(value,s$initial_input))return()
    s$navigated<-TRUE;selected(value)
    if(identical(value,s$saved)&&is.null(s$pending)&&!s$inflight)return()
    s$pending<-list(id=value,at=format(Sys.time(),'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'));save(s)
  },ignoreInit=TRUE)
  shiny::observeEvent(session$input$retry_league_preference,{
    if(is.null(state))return()
    if(identical(issue(),'load'))restore(state)else save(state)
  })
  session$onSessionEnded(function(){closed<<-TRUE;generation<<-generation+1L})
  session$output$league_preference_status<-shiny::renderUI({
    problem<-issue();if(is.null(problem))return(NULL)
    shiny::div(class='startup-feedback',role='status',shiny::span(if(identical(problem,'load'))'Your saved league could not be loaded.'else'Your last league has not been saved yet.'),
      shiny::actionButton('retry_league_preference','Retry'))
  })
  list(selected=selected)
}
