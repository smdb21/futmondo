# Server-only operational monitor. Factual data and acquisition remain separate.
# The database RPC returns bounded, credential-free telemetry; see docs/admin_worker_monitor.md.
fm_admin_worker_snapshot <- function(value) {
  fail <- function() stop("Invalid worker monitoring response", call.=FALSE)
  scalar <- function(x, pattern=NULL, optional=FALSE) {
    if(optional && is.null(x)) return(NULL)
    if(!is.character(x)||length(x)!=1L||is.na(x)||nchar(x)>160L||
       (!is.null(pattern)&&!grepl(pattern,x))) fail()
    x
  }
  number <- function(x) {
    if(!is.numeric(x)||length(x)!=1L||!is.finite(x)||x<0||x!=floor(x)) fail()
    x
  }
  flag <- function(x) {if(!is.logical(x)||length(x)!=1L||is.na(x))fail();x}
  stamp <- function(x,optional=FALSE) {
    if(optional&&is.null(x))return(NULL)
    scalar(x)
    t<-fm_time(x)
    if(length(t)!=1L||is.na(t))fail()
    x
  }
  rows <- function(x,limit) {
    if(!is.list(x)||!is.null(names(x))||length(x)>limit||
       !all(vapply(x,is.list,logical(1))))fail()
    x
  }
  identity <- function(x) scalar(x,"^[a-fA-F0-9-]{8,64}$")
  if(!is.list(value)||!is.list(value$summary)||!is.list(value$current)||
     !is.list(value$others)||!is.list(value$limits))fail()
  workers<-lapply(rows(value$workers,64L),function(w){
    status<-scalar(w$status)
    if(!status%in%c("leader","standby","recent"))fail()
    list(worker_id=identity(w$worker_id),status=status,heartbeat=stamp(w$heartbeat),
      lease_until=stamp(w$lease_until,TRUE),requests=number(w$requests),
      jobs=number(w$jobs),active_jobs=number(w$active_jobs))
  })
  job_rows<-function(group)list(active_jobs=number(group$active_jobs),
    jobs=lapply(rows(group$jobs,128L),function(j){
      a<-j$activity
      if(!is.null(a)){
        if(!is.list(a))fail()
        a<-list(phase=scalar(a$phase,"^(starting|processing|source_wait|source_request|database|publishing)$"),
          route=scalar(a$route,"^/?[a-z0-9_]+(/[a-z0-9_]+)*$",TRUE),
          method=scalar(a$method,"^(GET|POST|PATCH|DELETE)$",TRUE),
          started_at=stamp(a$started_at,TRUE),reported_at=stamp(a$reported_at,TRUE),
          fresh=flag(a$fresh))
      }
      list(job_id=scalar(j$job_id,"^[a-fA-F0-9]{6,64}$"),worker_id=identity(j$worker_id),
        lane=scalar(j$lane,"^(\\?|[a-zA-Z0-9_: -]{1,32})$"),account_ref=scalar(j$account_ref,"^[a-fA-F0-9]{8,64}$"),
        handler=scalar(j$handler,"^((get_|read_|fm_worker_)[a-z_]{1,80}|unknown)$"),
        started_at=stamp(j$started_at),lease_until=stamp(j$lease_until),activity=a)
    }))
  counts<-c("live_workers","working_workers","active_jobs","retained_workers","expired_jobs")
  summary<-setNames(lapply(counts,function(k)number(value$summary[[k]])),counts)
  current<-job_rows(value$current);others<-job_rows(value$others)
  if(summary$active_jobs!=current$active_jobs+others$active_jobs||
     current$active_jobs<length(current$jobs)||others$active_jobs<length(others$jobs)||
     summary$live_workers<length(workers)||summary$working_workers>summary$live_workers||
     length(current$jobs)+length(others$jobs)>128L)fail()
  list(checked_at=stamp(value$checked_at),heartbeat_timeout_seconds=number(value$heartbeat_timeout_seconds),
    activity_sample_seconds=number(value$activity_sample_seconds),summary=summary,workers=workers,
    current=current,others=others,limits=list(workers_truncated=flag(value$limits$workers_truncated),
      jobs_truncated=flag(value$limits$jobs_truncated)))
}

fm_admin_worker_store <- function(token,owner="") {
  if(!valid_login(token)||!is_authorized_admin(token))
    return(promises::promise_reject(simpleError("Unauthorized worker monitoring")))
  promises::then(fm_worker_rpc("admin_monitor",list(p_account=token[["userid"]]),owner=owner),
    fm_admin_worker_snapshot)
}

fm_admin_worker_monitor_UI <- function(state,ns) {
  s<-state$snapshot
  time_label<-function(value) {
    if(is.null(value))return("Not reported")
    format(fm_time(value),"%Y-%m-%d %H:%M:%S UTC",tz="UTC")
  }
  css<-shiny::singleton(shiny::tags$head(shiny::tags$style(shiny::HTML("
.fm-worker-monitor{min-width:0}.fm-worker-monitor p{overflow-wrap:anywhere}
.fm-worker-metrics{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:10px;margin:12px 0}
.fm-worker-metric,.fm-worker-card{border:1px solid var(--fm-border,#285238);border-radius:6px;padding:12px;min-width:0;background:var(--fm-surface,#0B120D)}
.fm-worker-metric strong{display:block;font-size:25px;font-variant-numeric:tabular-nums}.fm-worker-metric span{font-size:12px;overflow-wrap:anywhere}
.fm-worker-monitor h3{font-size:17px;margin:18px 0 8px}.fm-worker-card h4{font-size:14px;margin:0 0 8px;overflow-wrap:anywhere}
.fm-worker-cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%,280px),1fr));gap:10px}
.fm-worker-card dl{display:grid;grid-template-columns:minmax(85px,1fr) minmax(0,2fr);gap:5px 10px;margin:0;font-size:12px}
.fm-worker-card dd{margin:0;overflow-wrap:anywhere}.fm-worker-card dt{font-weight:normal;color:var(--fm-muted,#9BBFA7)}
.fm-worker-monitor code{white-space:normal;overflow-wrap:anywhere;color:inherit;background:transparent;padding:0}
.fm-worker-meta{font-size:12px;color:var(--fm-muted,#9BBFA7)}.fm-worker-monitor .alert{margin:10px 0}.fm-worker-monitor .btn{white-space:normal;max-width:100%}
.fm-worker-request-table{width:100%;table-layout:fixed;border-collapse:collapse;margin:12px 0}.fm-worker-request-table th,.fm-worker-request-table td{padding:8px;border-bottom:1px solid var(--fm-border,#285238);overflow-wrap:anywhere;vertical-align:top}.fm-worker-request-table th:first-child{width:60%;text-align:left}.fm-worker-request-table th:not(:first-child),.fm-worker-request-table td:not(:first-child){text-align:center;width:20%;font-variant-numeric:tabular-nums}.fm-worker-request-table td:first-child span{display:block;font-size:12px;color:var(--fm-muted,#9BBFA7)}
.fm-worker-count-active{color:var(--fm-text,#7CFF9B);font-weight:700}.fm-worker-count-zero,.fm-worker-count-stale{color:var(--fm-muted,#9BBFA7);font-weight:normal}.fm-worker-job-details>summary{cursor:pointer;margin:16px 0 8px}
@media(max-width:479px){.fm-worker-metrics{grid-template-columns:1fr}.fm-worker-metric strong{font-size:22px}.fm-worker-request-table th,.fm-worker-request-table td{padding:6px 4px}.fm-worker-request-table th{font-size:11px}.fm-worker-request-table th:first-child{width:52%}.fm-worker-request-table th:not(:first-child),.fm-worker-request-table td:not(:first-child){width:24%}}
"))))
  field<-function(label,value)shiny::tagList(shiny::tags$dt(label),shiny::tags$dd(value))
  metric<-function(label,value)shiny::div(class="fm-worker-metric",shiny::tags$strong(value),shiny::tags$span(label))
  phase_label<-function(activity) {
    if(is.null(activity)||!isTRUE(activity$fresh))return("Current activity not recently reported")
    labels<-c(starting="Starting job",processing="Processing data",source_wait="Waiting for source access",
      source_request="Requesting Futmondo data",database="Accessing saved data",publishing="Publishing saved result")
    if(activity$phase%in%names(labels))unname(labels[[activity$phase]])else "Processing job"
  }
  jobs_ui<-function(title,group,own=FALSE)shiny::tags$section(
    shiny::h3(paste0(title," · ",group$active_jobs," running")),
    if(!length(group$jobs))shiny::p(if(group$active_jobs==0)"No running jobs at this check."else "Running jobs are outside the display limit.")else
      shiny::div(class="fm-worker-cards",lapply(group$jobs,function(j){a<-j$activity
        shiny::tags$article(class="fm-worker-card",
          shiny::h4(gsub("_"," ",sub("^(get_|read_|fm_worker_)","",j$handler))),
          shiny::tags$dl(field("Worker / lane",paste0(substr(j$worker_id,1,8)," / ",j$lane)),
            if(!own)field("Account",j$account_ref),field("Activity",phase_label(a)),
            if(!is.null(a$route))field("Latest request",shiny::tags$code(paste(a$method %||% "",a$route))),
            field("Job started",time_label(j$started_at)),field("Activity reported",time_label(a$reported_at))),
          shiny::p(class="fm-worker-meta",paste0("Job ",j$job_id)))
      })))
  requests_ui<-function(snapshot) {
    project<-function(group)lapply(Filter(function(j){a<-j$activity
      !is.null(a)&&isTRUE(a$fresh)&&a$phase%in%c("source_request","database","publishing")&&
        !is.null(a$route)&&!is.null(a$method)
    },group$jobs),function(j){a<-j$activity
      list(kind=if(a$phase=="source_request")"Futmondo"else"Database",method=a$method,route=a$route)
    })
    mine<-project(snapshot$current);others<-project(snapshot$others)
    key<-function(x)paste(x$kind,x$method,x$route,sep="\n")
    mine_keys<-vapply(mine,key,character(1));other_keys<-vapply(others,key,character(1))
    all<-c(mine,others);keys<-c(mine_keys,other_keys)
    all<-all[!duplicated(keys)];keys<-unique(keys)
    order<-order(keys);all<-all[order];keys<-keys[order]
    count<-function(n)shiny::tags$span(class=if(!is.null(state$error))"fm-worker-count-stale"else if(n>0)"fm-worker-count-active"else "fm-worker-count-zero",n)
    shiny::tagList(
      shiny::h3("Latest reported requests"),
      if(!length(all))shiny::p("No active request was reported at this check.")else shiny::tags$table(class="fm-worker-request-table",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th(scope="col","Request type / endpoint"),
          shiny::tags$th(scope="col","My account"),shiny::tags$th(scope="col","Other accounts"))),
        shiny::tags$tbody(lapply(seq_along(all),function(i){request<-all[[i]]
          shiny::tags$tr(shiny::tags$td(shiny::tags$span(paste(request$kind,request$method,sep=" · ")),shiny::tags$code(request$route)),
            shiny::tags$td(count(sum(mine_keys==keys[i]))),shiny::tags$td(count(sum(other_keys==keys[i]))))
        }))),
      shiny::p(class="fm-worker-meta",paste0(
        max(0,snapshot$current$active_jobs-length(mine))," jobs for my account and ",
        max(0,snapshot$others$active_jobs-length(others))," for other accounts are processing, waiting, or have no current request report. Counts refer to reported activity, not extra worker processes.")))
  }
  shiny::div(class="fm-worker-monitor",css,
    shiny::p("Shared workers process jobs for all connected accounts. One worker can serve both groups below."),
    if(!is.null(state$error))shiny::div(class="alert alert-danger",role="alert",
      if(is.null(s))"Worker status could not be loaded. Retry to check current activity."else
        "Worker status could not be refreshed. Showing the previous check; activity may have changed."),
    if(isTRUE(state$busy))shiny::p(role="status",`aria-live`="polite",if(is.null(s))"Checking worker activity…"else "Updating worker activity…"),
    shiny::actionButton(ns("worker_monitor_refresh"),"Refresh worker status",disabled=isTRUE(state$busy)),
    if(!is.null(s))shiny::tagList(
      shiny::p(class="fm-worker-meta",paste0("Last checked: ",time_label(s$checked_at),". Refreshes every 15 seconds while this page is visible.")),
      shiny::div(class="fm-worker-metrics",metric("Live workers",s$summary$live_workers),
        metric("Workers processing jobs",s$summary$working_workers),metric("Running jobs",s$summary$active_jobs)),
      shiny::p(class="fm-worker-meta",paste0(s$summary$retained_workers," old worker records are excluded. ",
        s$summary$expired_jobs," expired job leases are excluded. A worker must report within ",s$heartbeat_timeout_seconds," seconds to count as live.")),
      shiny::p(class="fm-worker-meta",paste0("Activity sampled approximately every ",s$activity_sample_seconds,
        " seconds; reported requests may have completed since the sample.")),
      requests_ui(s),
      shiny::tags$details(class="fm-worker-job-details fm-worker-process-details",shiny::tags$summary("Worker details"),
        shiny::h3("Reporting workers"),
        if(!length(s$workers))shiny::p("No worker has reported recently.")else shiny::div(class="fm-worker-cards",lapply(s$workers,function(w)
          shiny::tags$article(class="fm-worker-card",shiny::h4(paste("Worker",substr(w$worker_id,1,8))),
            shiny::tags$dl(field("Status",switch(w$status,leader="Acquiring data",standby="Standby",recent="Recently reported")),
              field("Last heartbeat",time_label(w$heartbeat)),field("Running jobs",w$active_jobs),
              field("Completed jobs",w$jobs),field("Source requests",w$requests))))),
        shiny::p(class="fm-worker-meta","Counters restart with each worker process.")),
      shiny::tags$details(class="fm-worker-job-details",shiny::tags$summary("Running job details"),
        jobs_ui("Your account",s$current,TRUE),jobs_ui("Other accounts",s$others)),
      if(isTRUE(s$limits$workers_truncated)||isTRUE(s$limits$jobs_truncated))shiny::p(role="status",
        "Display limit reached. Summary counts include all current workers and jobs.")))
}

fm_admin_worker_monitor <- function(session,login_token,active,store=fm_admin_worker_store,visible=NULL) {
  root<-session$rootScope() %||% session
  if(is.null(visible))visible<-function()isTRUE(root$input$worker_visible)
  authorized<-function()valid_login(login_token())&&is_authorized_admin(login_token())
  available<-function()authorized()&&isTRUE(active())&&isTRUE(visible())
  state<-shiny::reactiveVal(list(snapshot=NULL,busy=FALSE,error=NULL))
  owner<-paste0("admin-worker:",session$token)
  generation<-0L;closed<-FALSE;deadline<-NULL
  cancel_deadline<-function(){if(is.function(deadline))deadline();deadline<<-NULL}
  reset<-function(){
    generation<<-generation+1L;cancel_deadline();fm_transport_cancel(owner)
    state(list(snapshot=NULL,busy=FALSE,error=NULL))
  }
  load<-function(){
    if(closed||!shiny::isolate(available())||isTRUE(shiny::isolate(state())$busy))return(invisible(NULL))
    generation<<-generation+1L;attempt<-generation;token<-shiny::isolate(login_token())
    s<-shiny::isolate(state());s$busy<-TRUE;state(s)
    current<-function()!closed&&attempt==generation&&shiny::isolate(available())&&identical(token,shiny::isolate(login_token()))
    finish<-function(value=NULL,error=NULL){
      if(!current())return(invisible(NULL))
      cancel_deadline();generation<<-generation+1L
      s<-shiny::isolate(state());s$busy<-FALSE;s$error<-if(is.null(error))NULL else TRUE
      if(is.null(error))s$snapshot<-value
      state(s);invisible(NULL)
    }
    deadline<<-later::later(function(){if(current()){finish(error=TRUE);fm_transport_cancel(owner)}},20)
    request<-tryCatch(store(token,owner=owner),error=promises::promise_reject)
    promises::then(request,function(value){
      if(!current())return(NULL)
      parsed<-tryCatch(fm_admin_worker_snapshot(value),error=function(e)NULL)
      if(is.null(parsed))finish(error=TRUE)else finish(parsed)
      NULL
    },function(error){finish(error=TRUE);NULL})
    invisible(NULL)
  }
  gate<-shiny::reactive(list(token=login_token(),active=isTRUE(active()),visible=isTRUE(visible())))
  shiny::observeEvent(gate(),{reset();if(available())load()},ignoreNULL=FALSE,priority=100)
  shiny::observe({
    gate()
    if(!available())return()
    shiny::invalidateLater(15000,session)
    load()
  })
  shiny::observeEvent(session$input$worker_monitor_refresh,load())
  session$output$worker_monitor<-shiny::renderUI({
    if(!available())return(NULL)
    fm_admin_worker_monitor_UI(state(),session$ns)
  })
  shiny::outputOptions(session$output,"worker_monitor",suspendWhenHidden=FALSE)
  session$onSessionEnded(function(){closed<<-TRUE;reset()})
  invisible(state)
}
