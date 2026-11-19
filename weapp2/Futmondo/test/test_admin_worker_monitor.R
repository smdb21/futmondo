#!/usr/bin/env Rscript
# Synthetic operational metadata only: no account, database or source requests.
# Optional arguments are snapshots exported by the SQL contract fixture.
suppressPackageStartupMessages(library(shiny))
source("data_contracts.R")
source("admin_worker_monitor.R")
Sys.setenv(admin="admin@example.invalid")
auth<-c(userid="fixture-admin",token="synthetic-only",user_name="admin@example.invalid")
regular<-c(userid="fixture-other",token="synthetic-only",user_name="user@example.invalid")
worker<-"11111111-1111-4111-8111-111111111111"
stamp<-"2026-11-19T03:00:00Z"
job<-function(id,route,phase="source_request")list(job_id=id,worker_id=worker,lane="0",
  account_ref="bbbbbbbbbbbb",handler="get_players_from_team",started_at=stamp,lease_until="2026-11-19T03:01:00Z",
  activity=list(phase=phase,route=route,method="POST",started_at=stamp,reported_at=stamp,fresh=TRUE))
snapshot<-list(checked_at=stamp,heartbeat_timeout_seconds=180,activity_sample_seconds=15,
  summary=list(live_workers=1,working_workers=1,active_jobs=2,retained_workers=3,expired_jobs=0),
  workers=list(list(worker_id=worker,status="leader",heartbeat=stamp,lease_until="2026-11-19T03:03:00Z",requests=12,jobs=8,active_jobs=2)),
  current=list(active_jobs=1,jobs=list(job("aaaaaaaaaaaa","/1/userteam/roster"))),
  others=list(active_jobs=1,jobs=list(job("cccccccccccc","rpc/worker_publish","publishing"))),
  limits=list(workers_truncated=FALSE,jobs_truncated=FALSE))
checks<-0L
check<-function(name,code){force(code);checks<<-checks+1L;cat("PASS",name,"\n")}
settle<-function(session)for(i in seq_len(12L)){later::run_now(0);session$flushReact()}
await<-function(p){
  done<-FALSE;value<-NULL;failure<-NULL
  promises::then(p,function(x){value<<-x;done<<-TRUE;NULL},function(e){failure<<-e;done<<-TRUE;NULL})
  until<-Sys.time()+2
  while(!done&&Sys.time()<until)later::run_now(.01)
  stopifnot(done);if(!is.null(failure))stop(failure);value
}
cancelled<-character()
fm_transport_cancel<-function(owner){cancelled<<-c(cancelled,owner);invisible(NULL)}
rpc_calls<-list()
fm_worker_rpc<-function(name,payload,owner=""){
  rpc_calls[[length(rpc_calls)+1L]]<<-list(name=name,payload=payload,owner=owner)
  promises::promise_resolve(snapshot)
}

check("store authorizes independently and derives the account from the authenticated token",{
  result<-await(fm_admin_worker_store(auth,"fixture-monitor"))
  stopifnot(result$summary$live_workers==1,length(rpc_calls)==1L,
    identical(rpc_calls[[1L]],list(name="admin_monitor",payload=list(p_account="fixture-admin"),owner="fixture-monitor")))
  for(token in list(NULL,regular,c(userid="fixture-admin",token="synthetic-only"))){
    denied<-tryCatch(await(fm_admin_worker_store(token)),error=identity)
    stopifnot(inherits(denied,"error"))
  }
  stopifnot(length(rpc_calls)==1L)
})
check("strict bounded projection drops unrequested fields and rejects malformed telemetry",{
  dirty<-snapshot;dirty$encrypted_token<-"PRIVATE";dirty$workers[[1L]]$headers<-"PRIVATE"
  dirty$current$jobs[[1L]]$body<-"PRIVATE";dirty$current$jobs[[1L]]$activity$password<-"PRIVATE"
  clean<-fm_admin_worker_snapshot(dirty)
  stopifnot(!grepl("PRIVATE",jsonlite::toJSON(clean,auto_unbox=TRUE)))
  bad<-list(NULL,list(),utils::modifyList(snapshot,list(summary=list(live_workers="1"))),
    utils::modifyList(snapshot,list(summary=list(active_jobs=3))),
    utils::modifyList(snapshot,list(checked_at="invalid")))
  oversized<-snapshot;oversized$workers<-rep(snapshot$workers,65L);bad<-c(bad,list(oversized))
  query<-snapshot;query$current$jobs[[1]]$activity$route<-"/1/userteam/roster?token=PRIVATE";bad<-c(bad,list(query))
  phase<-snapshot;phase$current$jobs[[1]]$activity$phase<-"private_phase";bad<-c(bad,list(phase))
  for(value in bad)stopifnot(inherits(tryCatch(fm_admin_worker_snapshot(value),error=identity),"error"))
})
check("request matrix separates accounts and highlights only positive fresh counts",{
  html<-as.character(fm_admin_worker_monitor_UI(list(snapshot=snapshot,busy=FALSE,error=NULL),NS("admin")))
  doc<-xml2::read_html(html);rows<-xml2::xml_find_all(doc,"//table[contains(@class,'fm-worker-request-table')]/tbody/tr")
  stopifnot(length(rows)==2L,grepl("My account",html),grepl("Other accounts",html),
    identical(xml2::xml_text(xml2::xml_find_all(rows[[1L]],"./td[position()>1]/span")),c("0","1")),
    identical(xml2::xml_text(xml2::xml_find_all(rows[[2L]],"./td[position()>1]/span")),c("1","0")),
    all(xml2::xml_text(xml2::xml_find_all(doc,"//span[@class='fm-worker-count-active']"))=="1"),
    all(xml2::xml_text(xml2::xml_find_all(doc,"//span[@class='fm-worker-count-zero']"))=="0"))
  waiting<-snapshot;waiting$others$jobs[[1L]]$activity$phase<-"source_wait"
  html<-as.character(fm_admin_worker_monitor_UI(list(snapshot=waiting,busy=FALSE,error=NULL),NS("admin")))
  doc<-xml2::read_html(html)
  stopifnot(length(xml2::xml_find_all(doc,"//table[contains(@class,'fm-worker-request-table')]/tbody/tr"))==1L,
    grepl("1 for other accounts are processing, waiting",html,fixed=TRUE))
  stale<-snapshot;stale$current$jobs[[1L]]$activity$fresh<-FALSE;stale$others$jobs[[1L]]$activity$fresh<-FALSE
  html<-as.character(fm_admin_worker_monitor_UI(list(snapshot=stale,busy=FALSE,error=NULL),NS("admin")))
  stopifnot(grepl("No active request was reported",html),!grepl('<table class="fm-worker-request-table"',html,fixed=TRUE))
})

pending<-list();calls<-0L
held_store<-function(token,owner=""){
  calls<<-calls+1L
  promises::promise(function(resolve,reject)pending[[length(pending)+1L]]<<-list(resolve=resolve,reject=reject,owner=owner))
}
active<-reactiveVal(TRUE);visible<-reactiveVal(TRUE);login<-reactiveVal(regular)
monitor_server<-function(input,output,session){
  monitor<-fm_admin_worker_monitor(session,login,active,store=held_store,visible=visible)
  heartbeat_seen<-reactiveVal(0L)
  observeEvent(input$heartbeat,heartbeat_seen(input$heartbeat))
}
check("non-admin inputs and hidden pages cannot start monitor reads",{
  testServer(monitor_server,{
    settle(session);session$setInputs(worker_monitor_refresh=1);settle(session)
    stopifnot(calls==0L,is.null(output$worker_monitor))
    login(auth);active(FALSE);settle(session);session$setInputs(worker_monitor_refresh=2);settle(session)
    stopifnot(calls==0L,is.null(output$worker_monitor))
    active(TRUE);visible(FALSE);settle(session);session$setInputs(worker_monitor_refresh=3);settle(session)
    stopifnot(calls==0L,is.null(output$worker_monitor))
  })
})
check("pending reads preserve heartbeat, serialize refreshes and poll only while visible",{
  active(TRUE);visible(TRUE);login(auth)
  testServer(monitor_server,{
    settle(session);stopifnot(calls==1L,monitor()$busy)
    session$setInputs(worker_monitor_refresh=1,heartbeat=7);settle(session)
    stopifnot(calls==1L,identical(heartbeat_seen(),7))
    session$elapse(15000);settle(session);stopifnot(calls==1L)
    pending[[1L]]$resolve(snapshot);settle(session)
    stopifnot(!monitor()$busy,monitor()$snapshot$summary$active_jobs==2L)
    session$elapse(15000);settle(session);stopifnot(calls==2L)
    visible(FALSE);settle(session)
    pending[[2L]]$resolve(snapshot);settle(session)
    stopifnot(is.null(monitor()$snapshot),is.null(output$worker_monitor),any(grepl("^admin-worker:",cancelled)))
    session$elapse(60000);settle(session);stopifnot(calls==2L)
  })
})
check("failures preserve a labelled stale check, never fabricate zero, and logout rejects late completion",{
  active(TRUE);visible(TRUE);login(auth)
  testServer(monitor_server,{
    settle(session);n<-length(pending);pending[[n]]$resolve(snapshot);settle(session)
    session$setInputs(worker_monitor_refresh=1);settle(session);n<-length(pending)
    pending[[n]]$reject(simpleError("PRIVATE server response / password / key"));settle(session)
    html<-output$worker_monitor$html
    stopifnot(!monitor()$busy,isTRUE(monitor()$error),monitor()$snapshot$summary$active_jobs==2,
      grepl("Showing the previous check",html),!grepl("PRIVATE",html))
    doc<-xml2::read_html(html)
    stopifnot(length(xml2::xml_find_all(doc,"//span[@class='fm-worker-count-active']"))==0L,
      length(xml2::xml_find_all(doc,"//span[@class='fm-worker-count-stale']"))==4L)
    session$setInputs(worker_monitor_refresh=2);settle(session);n<-length(pending)
    retry_html<-as.character(fm_admin_worker_monitor_UI(monitor(),NS("admin")))
    stopifnot(monitor()$busy,isTRUE(monitor()$error),grepl("Showing the previous check",retry_html),
      !grepl('<span class="fm-worker-count-active"',retry_html,fixed=TRUE))
    login(regular);settle(session);pending[[n]]$resolve(snapshot);settle(session)
    stopifnot(is.null(monitor()$snapshot),is.null(output$worker_monitor))
    before<-calls;session$setInputs(worker_monitor_refresh=3);session$elapse(30000);settle(session)
    stopifnot(calls==before)
  })
})
check("initial failure remains unavailable and closed sessions ignore pending responses",{
  login(auth);active(TRUE);visible(TRUE)
  testServer(monitor_server,{
    settle(session);n<-length(pending);pending[[n]]$reject(simpleError("PRIVATE"));settle(session)
    html<-output$worker_monitor$html
    stopifnot(is.null(monitor()$snapshot),grepl("Worker status could not be loaded",html),
      !grepl('class="fm-worker-metric"',html,fixed=TRUE),!grepl("PRIVATE",html))
    session$setInputs(worker_monitor_refresh=1);settle(session);n<-length(pending)
    before<-length(cancelled);late_parses<-0L;original_parser<-fm_admin_worker_snapshot
    fm_admin_worker_snapshot<<-function(value){late_parses<<-late_parses+1L;original_parser(value)}
    session$close();pending[[n]]$resolve(snapshot)
    for(i in seq_len(12L))later::run_now(0)
    fm_admin_worker_snapshot<<-original_parser
    stopifnot(late_parses==0L,length(cancelled)>before)
  })
})
check("overall deadline clears queued reads and rejects their late completion",local({
  namespace<-asNamespace("later");original_later<-get("later",envir=namespace)
  deadlines<-list();deadline_cancels<-0L
  replacement<-function(func,delay=0,...){
    if(identical(as.numeric(delay),20)){
      deadlines[[length(deadlines)+1L]]<<-func
      return(function(){deadline_cancels<<-deadline_cancels+1L;invisible(NULL)})
    }
    original_later(func,delay=delay,...)
  }
  unlockBinding("later",namespace);assign("later",replacement,envir=namespace);lockBinding("later",namespace)
  on.exit({unlockBinding("later",namespace);assign("later",original_later,envir=namespace);lockBinding("later",namespace)},add=TRUE)
  login(auth);active(TRUE);visible(TRUE)
  testServer(monitor_server,{
    settle(session);n<-length(pending);before<-length(cancelled)
    stopifnot(monitor()$busy,length(deadlines)==1L)
    deadlines[[1L]]();settle(session)
    stopifnot(!monitor()$busy,isTRUE(monitor()$error),is.null(monitor()$snapshot),
      length(cancelled)>before,deadline_cancels>=1L)
    pending[[n]]$resolve(snapshot);settle(session)
    stopifnot(is.null(monitor()$snapshot),isTRUE(monitor()$error))
    session$setInputs(worker_monitor_refresh=1);settle(session);n<-length(pending)
    stopifnot(monitor()$busy,length(deadlines)==2L)
    pending[[n]]$resolve(snapshot);settle(session)
    stopifnot(!monitor()$busy,is.null(monitor()$error),monitor()$snapshot$summary$active_jobs==2L)
  })
}))
for(path in commandArgs(trailingOnly=TRUE))check(paste("real SQL projection parses and renders",basename(path)),{
  projected<-fm_admin_worker_snapshot(jsonlite::fromJSON(path,simplifyVector=FALSE))
  stopifnot(projected$summary$live_workers==2L,projected$summary$working_workers==1L,
    projected$summary$active_jobs==2L,projected$current$active_jobs==1L,projected$others$active_jobs==1L,
    projected$summary$retained_workers==1L,projected$summary$expired_jobs==1L)
  html<-as.character(fm_admin_worker_monitor_UI(list(snapshot=projected,busy=FALSE,error=NULL),NS("admin")))
  doc<-xml2::read_html(html)
  rows<-xml2::xml_find_all(doc,"//table[contains(@class,'fm-worker-request-table')]/tbody/tr")
  if(is.null(projected$current$jobs[[1L]]$activity)){
    stopifnot(projected$current$jobs[[1L]]$lane=="?",projected$current$jobs[[1L]]$handler=="unknown",
      length(rows)==1L,identical(xml2::xml_text(xml2::xml_find_all(rows[[1L]],"./td[position()>1]/span")),c("0","1")),
      grepl("1 jobs for my account",html))
  }else{
    stopifnot(length(rows)==2L,
      identical(xml2::xml_text(xml2::xml_find_all(rows[[1L]],"./td[position()>1]/span")),c("0","1")),
      identical(xml2::xml_text(xml2::xml_find_all(rows[[2L]],"./td[position()>1]/span")),c("1","0")))
  }
})
cat("ADMIN WORKER MONITOR:",checks,"groups passed; no live I/O\n")
