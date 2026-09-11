#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source("futmondo_functions.R");source("supabase_connector.R");source("intelligence_engine.R");source("automation_runtime.R")})
passed<-0L
check<-function(name,expr){force(expr);passed<<-passed+1L;cat("PASS",name,"\n")}
now<-as.POSIXct("2026-09-05 12:00:00",tz="UTC")
key<-as.raw(seq_len(32));auth<-c(token="not-a-live-token",userid="u",user_name="test@example.org")
check("encrypted session roundtrip, tamper detection, expiry",{
  envelope<-encrypt_automation_session(auth,"2026-09-06T00:00:00Z",key)
  stopifnot(identical(unname(decrypt_automation_session(envelope,key,now)),unname(auth)))
  altered<-envelope;altered$ciphertext<-openssl::base64_encode(charToRaw("tampered"))
  stopifnot(inherits(try(decrypt_automation_session(altered,key,now),silent=TRUE),"try-error"),
    inherits(try(decrypt_automation_session(envelope,key,now+86400),silent=TRUE),"try-error"),
    !grepl("not-a-live-token",jsonlite::toJSON(envelope),fixed=TRUE))
})
p<-list(actions=list("bid","modify_bid","cancel_bid","accept_bid","reject_bid","list","delist","clause"),
  allowed_player_ids=list("p"),max_per_action=500,total_spending_limit=1000,minimum_sale_price=200,
  expires_at="2026-10-01T00:00:00Z")
policy<-list(id="policy",user_id="u",championship_id="c",user_team_id="t",enabled=TRUE,mode="shadow",
  policy=p,spent=0,reserved=0,shadow_started_at="2026-08-15T12:00:00Z")
job<-list(id="j",policy_id="policy",lease_token="lease",user_id="u",championship_id="c",user_team_id="t",
  action_type="bid",payload=list(player_id="p",amount=300,listing_expires_at="2026-09-05T13:00:00Z"))
fin<-list(status="ok",cash=1000,spendable_budget=1000,legal_bid_limit=600,observed_at=now,
  debt_limit=500,minimum_balance=-500,
  roster_count=15,roster_cap=20,commitments=list(count=0,completeness="complete"),
  rules=list(solvency_verified=TRUE,deadline="2026-09-06T12:00:00Z"),
  lineup_rules=list(verified=TRUE,formations="4-3-3",multiposition=FALSE,club_limit=Inf))
roster<-data.frame(id="owned")
market<-data.frame(id="p",expirationDate="2026-09-05T13:00:00Z",price=100,computer=TRUE,
  type="normal",isClause=FALSE,bid_id="",bid_price=NA_real_,stringsAsFactors=FALSE)
summary<-list(data=list(id="p"),championship=list())
services<-list(summary=function(...)summary,market=function(...)market,
  listings=function(...)data.frame(id=character(),price=numeric()),
  offers=function(...)data.frame(id="p",bid_id="offer",bid_price=300))
check("shadow policy permits approved identities and rejects foreign accounts and targets",{
  stopifnot(validate_automation_action(job,policy,fin,roster,now)$ok)
  for(field in c("user_id","championship_id","user_team_id")) {
    bad<-job;bad[[field]]<-"foreign"
    stopifnot(!validate_automation_action(bad,policy,fin,roster,now)$ok)
  }
  bad<-job;bad$payload$player_id<-"unapproved"
  stopifnot(!validate_automation_action(bad,policy,fin,roster,now)$ok)
})
check("collective spending, temporary debt, API limit, freshness and roster capacity",{
  po<-policy;po$reserved<-800
  stopifnot(!validate_automation_action(job,po,fin,roster,now)$ok)
  po$reserved<-Inf
  stopifnot(!validate_automation_action(job,po,fin,roster,now)$ok)
  for(field in c("spendable_budget","legal_bid_limit")) {
    f<-fin;f[[field]]<-if(field=="spendable_budget")299 else 299
    stopifnot(!validate_automation_action(job,policy,f,roster,now)$ok)
    f[[field]]<-NA_real_;stopifnot(!validate_automation_action(job,policy,f,roster,now)$ok)
  }
  f<-fin;f$cash<- -1
  stopifnot(validate_automation_action(job,policy,f,roster,now)$ok)
  f$debt_limit<-NA_real_;stopifnot(!validate_automation_action(job,policy,f,roster,now)$ok)
  stopifnot(!validate_automation_action(job,policy,fin,roster,now,target=list(ok=TRUE,
    verified_at="2026-09-05T11:59:00Z"))$ok)
  f<-fin;f$observed_at<-now-31
  stopifnot(!validate_automation_action(job,policy,f,roster,now)$ok)
  f<-fin;f$commitments$count<-5
  stopifnot(!validate_automation_action(job,policy,f,roster,now)$ok)
  f<-fin;f$commitments$completeness<-"partial"
  stopifnot(!validate_automation_action(job,policy,f,roster,now)$ok)
})
check("modifying a verified own bid charges only its increase",{
  j<-job;j$action_type<-"modify_bid";f<-fin;f$spendable_budget<-150
  stopifnot(!validate_automation_action(j,policy,f,roster,now)$ok)
  result<-validate_automation_action(j,policy,f,roster,now,target=list(ok=TRUE,previous_amount=200))
  stopifnot(result$ok,result$spending_charge==100)
})
check("live actions require deadline evidence, observation history and model validation",{
  po<-policy;po$mode<-"live";j<-job;j$payload$model_driven<-TRUE
  stopifnot(!validate_automation_action(j,po,fin,roster,now,13)$ok,
    !validate_automation_action(j,po,fin,roster,now,14)$ok)
  po$model_validated<-TRUE
  stopifnot(validate_automation_action(j,po,fin,roster,now,14)$ok)
  f<-fin;f$rules$solvency_verified<-FALSE
  stopifnot(!validate_automation_action(j,po,f,roster,now,14)$ok)
})
check("sales preserve a legal XI under verified league rules including injured eligible players",{
  r<-data.frame(id=c("p",paste0("own",1:11)),role=c("delantero","portero",rep("defensa",4),rep("centrocampista",3),rep("delantero",3)),
    status=c("", "injured", rep("",10)),points=1,average.matches=1,average.average=1,value=100)
  j<-job;j$action_type<-"accept_bid"
  stopifnot(validate_automation_action(j,policy,fin,r,now)$ok)
  stopifnot(!validate_automation_action(j,policy,fin,r[-2,],now)$ok)
  f<-fin;f$lineup_rules$verified<-FALSE
  stopifnot(!validate_automation_action(j,policy,f,r,now)$ok)
  j$action_type<-"list";j$payload$amount<-100
  stopifnot(!validate_automation_action(j,policy,fin,r,now)$ok)
})
check("normal-market target checks listing identity, expiry, price and existing own bids",{
  stopifnot(verify_automation_target(job,auth,services,now)$ok)
  for(field in c("id","expirationDate","computer","type","isClause")) {
    m<-market
    m[[field]]<-switch(field,id="other",expirationDate="2026-09-05T12:00:10Z",computer=FALSE,type="auction",isClause=TRUE)
    srv<-services;srv$market<-local({v<-m;function(...)v})
    stopifnot(!verify_automation_target(job,auth,srv,now)$ok)
  }
  j<-job;j$payload$listing_expires_at<-"2026-09-06T13:00:00Z"
  stopifnot(!verify_automation_target(j,auth,services,now)$ok)
  j<-job;j$payload$amount<-90
  stopifnot(!verify_automation_target(j,auth,services,now)$ok)
  srv<-services;srv$market<-function(...){m<-market;m$bid_id<-"own";m$bid_price<-250;m}
  stopifnot(!verify_automation_target(job,auth,srv,now)$ok)
  j<-job;j$action_type<-"modify_bid";j$payload$bid_id<-"own"
  stopifnot(verify_automation_target(j,auth,srv,now)$ok)
  j$payload$bid_id<-"rival";stopifnot(!verify_automation_target(j,auth,srv,now)$ok)
})
check("sale, reject and clause targets check fresh ownership, offer price and transfer locks",{
  srv<-services;srv$summary<-function(...)list(data=list(id="p"),championship=list(owner=list(`_id`="t")))
  j<-job;j$action_type<-"accept_bid";j$payload$bid_id<-"offer"
  stopifnot(verify_automation_target(j,auth,srv,now)$ok)
  j$payload$amount<-301
  stopifnot(!verify_automation_target(j,auth,srv,now)$ok)
  j$action_type<-"reject_bid"
  stopifnot(!verify_automation_target(j,auth,srv,now)$ok)
  j$action_type<-"list"
  stopifnot(verify_automation_target(j,auth,srv,now)$ok)
  j$action_type<-"delist"
  stopifnot(!verify_automation_target(j,auth,srv,now)$ok)
  j<-job;j$action_type<-"clause"
  srv$summary<-function(...)list(data=list(id="p"),championship=list(owner=list(`_id`="rival"),
    clause=list(price=300,date="2026-09-04T12:00:00Z",transferred=FALSE)))
  stopifnot(verify_automation_target(j,auth,srv,now)$ok)
  srv$summary<-function(...)list(data=list(id="p"),championship=list(owner=list(`_id`="rival"),
    clause=list(price=300,date="2026-09-04T12:00:00Z",transferred=TRUE)))
  stopifnot(!verify_automation_target(j,auth,srv,now)$ok)
})
check("logical success is recognised; transport errors and unknown responses remain uncertain",{
  saved<-list(buy_clause=buy_clause,buy_roster_clause=buy_roster_clause,modify_bid=modify_bid,cancel_bid=cancel_bid)
  modify_bid<-function(...)TRUE;cancel_bid<-function(...)TRUE
  for(a in c("modify_bid","cancel_bid")) {j<-job;j$action_type<-a;stopifnot(execute_automation_action(j,auth)$status=="succeeded")}
  cancel_bid<-function(...)FALSE;j$action_type<-"cancel_bid"
  stopifnot(execute_automation_action(j,auth)$status=="uncertain")
  buy_roster_clause<-function(...)list(success=FALSE,code="error",message="timeout")
  j$action_type<-"clause";stopifnot(execute_automation_action(j,auth)$status=="uncertain")
  calls<-0L;buy_clause<-function(...){calls<<-calls+1L;stop("timeout")}
  stopifnot(execute_automation_action(job,auth)$status=="uncertain",calls==1L)
  buy_clause<-function(...)list(success=FALSE,code="api.market.insufficientFunds")
  stopifnot(execute_automation_action(job,auth)$status=="failed")
  for(name in names(saved))assign(name,saved[[name]],envir=.GlobalEnv)
})
history<-lapply(14:1,function(i)list(id=paste0("old",i),policy_id="policy",user_id="u",status="shadow",
  finished_at=format(now-i*86400,"%Y-%m-%dT%H:%M:%OSZ",tz="UTC"),result=list(preflight_verified=TRUE)))
check("shadow gate counts distinct verified dates over at least fourteen elapsed days",{
  stopifnot(automation_shadow_days(history,policy,now)==14L,
    automation_shadow_days(rep(history[1],14),policy,now)==1L)
  bad<-history;bad[[1]]$result$preflight_verified<-FALSE
  stopifnot(automation_shadow_days(bad,policy,now)==13L)
  po<-policy;po$shadow_started_at<-"2026-09-01T12:00:00Z"
  stopifnot(automation_shadow_days(history,po,now)==0L)
  bad<-history;bad[[1]]$user_id<-"foreign"
  stopifnot(automation_shadow_days(bad,policy,now)==13L)
})
Sys.setenv(AUTOMATION_SESSION_KEY=openssl::base64_encode(key))
calls<-begins<-0L
run_services<-c(services,list(policies=function(...)list(policy),sessions=function(...)list(list(encrypted_session=envelope)),
  jobs=function(...)history,financial=function(...)fin,roster=function(...)roster,
  memberships=function(...)list(championships=list(list(id="c",userteam=list(id="t")))),
  clear_cache=function(...)NULL,now=function()now,
  begin=function(...){begins<<-begins+1L;TRUE},execute=function(...){calls<<-calls+1L;list(status="succeeded")}))
check("full shadow job performs preflight without crossing the execution fence",{
  result<-run_automation_job(job,now,run_services)
  stopifnot(result$status=="shadow",isTRUE(result$preflight_verified),calls==0L,begins==0L)
})
check("live jobs require a successful final lease fence, and reserve uncertain spending",{
  srv<-run_services;srv$policies<-function(...){po<-policy;po$mode<-"live";list(po)}
  srv$begin<-function(...)FALSE
  stopifnot(run_automation_job(job,now,srv)$status=="blocked",calls==0L)
  srv$begin<-function(...)TRUE
  stopifnot(run_automation_job(job,now,srv)$status=="succeeded",calls==1L)
  srv$jobs<-function(...)c(history,list(list(id="unknown",status="uncertain",action_type="bid",payload=list(amount=900))))
  stopifnot(run_automation_job(job,now,srv)$status=="blocked",calls==1L)
  srv$jobs<-function(...)structure(list(),status="unavailable")
  stopifnot(run_automation_job(job,now,srv)$status=="blocked",calls==1L)
})
check("session, membership and target failures never invoke account mutation",{
  srv<-run_services;srv$memberships<-function(...)list(championships=list())
  stopifnot(run_automation_job(job,now,srv)$status=="blocked",calls==1L)
  srv<-run_services;srv$market<-function(...)NULL
  stopifnot(run_automation_job(job,now,srv)$status=="blocked",calls==1L)
  stopifnot(run_automation_job(job,now+86400,run_services)$status=="blocked",calls==1L)
})
check("read-only reconciliation needs positive matching auction evidence",{
  srv<-run_services
  stopifnot(reconcile_automation_job(job,auth,srv)$status=="uncertain")
  srv$market<-function(...){m<-market;m$bid_id<-"own";m$bid_price<-300;m}
  stopifnot(reconcile_automation_job(job,auth,srv)$status=="succeeded")
  j<-job;j$payload$listing_expires_at<-"2026-09-06T12:00:00Z"
  stopifnot(reconcile_automation_job(j,auth,srv)$status=="uncertain")
  wrong<-auth;wrong[["userid"]]<-"foreign"
  stopifnot(reconcile_automation_job(job,wrong,srv)$status=="uncertain")
})
check("atomic SQL fences retain uncertainty and scheduler never merges duplicate jobs",{
  sql<-paste(readLines("scripts/migrations/20260905_reliable_insights.sql"),collapse="\n")
  stopifnot(grepl("FOR UPDATE SKIP LOCKED",sql,fixed=TRUE),grepl("status='uncertain'",sql,fixed=TRUE),
    grepl("PRIMARY KEY, job_id",sql,fixed=TRUE),grepl("REVOKE ALL ON FUNCTION claim_automation_job() FROM PUBLIC",sql,fixed=TRUE),
    grepl("expires_at='infinity'",sql,fixed=TRUE),grepl("IS DISTINCT FROM p_lease_token",sql,fixed=TRUE),
    !grepl("DELETE FROM automation_account_locks WHERE expires_at",sql,fixed=TRUE))
  stopifnot(grepl("resolution=ignore-duplicates",paste(deparse(body(insert_automation_job)),collapse=""),fixed=TRUE),
    !grepl("supabase_post_direct",paste(deparse(body(schedule_automation_policies)),collapse=""),fixed=TRUE))
  worker<-paste(readLines("scripts/automation_worker.R"),collapse="\n")
  stopifnot(grepl("invisible(automation_key())",worker,fixed=TRUE))
})
check("automation UI verifies current account ownership before pause and reconciliation",{
  source("Modules/Automation_Module.R")
  saved<-list(read_automation_rows=read_automation_rows,get_championship_players=get_championship_players,
    supabase_patch=supabase_patch,reconcile_automation_job=reconcile_automation_job,finish_automation_job=finish_automation_job)
  writes<-list();reconciled<-0L
  pending_job<-job;pending_job$status<-"uncertain"
  read_automation_rows<-function(table,filters) switch(table,
    automation_policies=list(policy),automation_jobs=if(identical(filters$status,"eq.uncertain") ||
      !is.null(filters$id))list(pending_job) else list(),list())
  get_championship_players<-function(...)data.frame(id="p",name="Player")
  supabase_patch<-function(table,payload,filters){writes[[length(writes)+1L]]<<-list(table=table,payload=payload,filters=filters);TRUE}
  reconcile_automation_job<-function(job,login){reconciled<<-reconciled+1L;list(status="succeeded",reason="confirmed")}
  finish_automation_job<-function(...)TRUE
  login_rv<-shiny::reactiveVal(auth)
  shiny::testServer(automation_Server,args=list(is_module_active=function()TRUE,login_token=login_rv,
    championship_id=function()"c",user_team_id=function()"t"),{
    session$flushReact()
    session$setInputs(policy_id="policy",pause=1)
    stopifnot(length(writes)==1L,writes[[1]]$filters$user_id=="eq.u",
      writes[[1]]$filters$championship_id=="eq.c",writes[[1]]$filters$user_team_id=="eq.t")
    session$setInputs(uncertain_job="j",reconcile=1)
    stopifnot(reconciled==1L)
    other<-auth;other[["userid"]]<-"other"
    login_rv(other);session$flushReact()
    session$setInputs(policy_id="policy",pause=2,uncertain_job="j",reconcile=2)
    stopifnot(length(writes)==1L,reconciled==1L)
  })
  for(name in names(saved))assign(name,saved[[name]],envir=.GlobalEnv)
})
cat(sprintf("AUTOMATION: %d passed / 0 failed\n",passed))
