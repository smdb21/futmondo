#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source("futmondo_functions.R"); source("factual_helpers.R")})
n<-0L; check<-function(name,expr){force(expr);n<<-n+1L;cat("PASS",name,"\n")}
auth<-c(token="offline-secret",userid="private-user");ctx<-resource_context(auth,"private-league","private-team")
await<-function(p){done<-FALSE;out<-NULL;failure<-NULL;promises::then(p,function(x){out<<-x;done<<-TRUE},function(e){failure<<-e;done<<-TRUE});end<-Sys.time()+3;while(!done&&Sys.time()<end)later::run_now(.01);stopifnot(done);if(!is.null(failure))stop(failure);out}
capture<-function(expr){logs<-character();value<-withCallingHandlers(force(expr),message=function(e){logs<<-c(logs,conditionMessage(e));invokeRestart("muffleMessage")});list(value=value,logs=logs)}
check("allowlisted diagnostics discard raw messages, private fields, calls, and resource suffixes",{
  raw<-structure(list(message='HTTP 503 https://private.invalid?token=SECRET body={password:SECRET}',
    call=quote(secret_function(SECRET)),resource="market_private-league_private-team:private-hash",
    private_body="SECRET",reference="SECRET"),class=c("error","condition"))
  got<-capture(resource_log_failure(raw));e<-got$value
  stopifnot(e$category=="http",e$http_status==503L,e$resource=="market",is.null(e$call),
    is.null(e$private_body),grepl("^FM-HTTP-[0-9]{6}$",e$reference),length(got$logs)==1L,
    !grepl("SECRET|private|password|token|https|secret_function",paste(c(got$logs,capture.output(str(e))),collapse=" ")))
  cases<-list(network=simpleError("Connection failed SECRET"),parser=simpleError("lexical error SECRET"),
    schema=simpleError("arguments imply differing number of rows SECRET"),
    read_budget=simpleError("Read request budget exceeded SECRET"),unknown=simpleError("SECRET"))
  for(category in names(cases))stopifnot(resource_failure(cases[[category]])$category==category)
  hostile<-resource_failure(structure(list(message="SECRET",category="SECRET",http_status=123456L,
    resource="SECRET",reference="SECRET"),class=c("error","condition")))
  stopifnot(hostile$category=="unknown",hostile$resource=="read",is.null(hostile$http_status))
})
check("nested cache failures preserve source category and log once even when caught by finance",{
  clear_api_cache();attempts<-0L
  load<-function()get_cached_data("financial_snapshot_private-league_private-team",{
    tryCatch(get_cached_data("market_private-league_private-team",{attempts<<-attempts+1L;stop(simpleError("HTTP 429 SECRET"))},context=ctx),error=function(e)NULL)
    list(status="partial",cash=NA_real_)
  },context=ctx)
  cold<-capture(load());failure<-attr(cold$value,"fetch_error")
  stopifnot(attempts==1L,length(cold$logs)==1L,failure$category=="http",failure$http_status==429L,
    failure$resource=="market",resource_state(cold$value)$error$reference==failure$reference)
  warm<-capture(load());stopifnot(attempts==1L,length(warm$logs)==0L,identical(warm$value,cold$value))
})
check("sync stale fallback retains evidence and the original safe failure",{
  clear_api_cache();key<-"financial_snapshot_private-league_private-team"
  initial<-get_cached_data(key,list(status="ok",funds=list(spendable_budget=400)),context=ctx)
  cache_key<-api_cache_key(key,context=ctx);api_cache_env[[cache_key]]$time<-Sys.time()-21
  stale<-capture(get_cached_data(key,stop(simpleError("HTTP 503 SECRET")),context=ctx))
  state<-resource_state(stale$value)
  stopifnot(length(stale$logs)==1L,stale$value$status=="partial",is.na(stale$value$funds$spendable_budget),
    state$status=="stale",state$error$category=="http",state$error$http_status==503L,
    identical(attr(initial,"observed_at"),state$observed_at))
  text<-capture(replicate(3L,resource_failure_message(state$error)))
  stopifnot(length(text$logs)==0L,all(grepl(state$error$reference,text$value,fixed=TRUE)),
    !any(grepl("SECRET|private",text$value)))
})
check("async wrappers preserve diagnostic identity, stale data, and quiet warm reads",{
  clear_api_cache();resource<-"get_market_players:private-hash";calls<-0L
  initial<-capture(await(get_cached_resource_async(resource,ctx,function(){calls<<-calls+1L;promises::promise_resolve(data.frame(value=0))})))
  stopifnot(length(initial$logs)==0L,initial$value$data$value==0)
  warm<-capture(await(get_cached_resource_async(resource,ctx,function()stop("Must remain warm"))))
  stopifnot(calls==1L,length(warm$logs)==0L)
  key<-api_cache_key(resource,context=ctx);api_cache_env[[key]]$time<-Sys.time()-21
  cause<-resource_failure(resource="market_private-league",category="network")
  stale<-capture(await(get_cached_resource_async(resource,ctx,function(){calls<<-calls+1L;promises::promise_reject(cause)})))
  stopifnot(calls==2L,length(stale$logs)==1L,stale$value$status=="stale",stale$value$data$value==0,
    stale$value$error$category=="network",stale$value$error$reference==cause$reference)
  clear_api_cache()
  failed<-capture(await(get_cached_resource_async(resource,ctx,function()promises::promise_reject(cause))))
  stopifnot(failed$value$status=="unavailable",failed$value$error$reference==cause$reference)
})
get_diagnostic_replay_fixture<-function(login,championship_id,user_team_id){
  context<-resource_context(login,championship_id,user_team_id)
  get_cached_data("financial_snapshot_private-league_private-team",{
    tryCatch(get_cached_data("market_private-league_private-team",{
      httr::content(futmondo_post("https://fixture.invalid/players",body="{}"))
    },context=context),error=function(e)NULL)
    futmondo_post("https://fixture.invalid/lineup",body="{}")
    futmondo_post("https://fixture.invalid/teams",body="{}")
    list(status="partial")
  },context=context)
}
check("adapter replay deduplicates a caught optional parser failure without losing attribution",{
  clear_api_cache();calls<-character();original_transport<-fm_async_read_http
  fm_async_read_http<-function(url,...){
    route<-tail(strsplit(url,"/",fixed=TRUE)[[1]],1);calls<<-c(calls,route)
    body<-if(route=="players")'{SECRET malformed JSON'else'{"answer":[]}'
    promises::promise_resolve(list(status_code=200L,content=charToRaw(body),times=list(total=0)))
  }
  got<-capture(await(async_adapter_fetch("get_diagnostic_replay_fixture",list(auth,"private-league","private-team"),ctx)))
  failure<-attr(got$value,"fetch_error")
  stopifnot(identical(calls,c("players","lineup","teams")),length(got$logs)==1L,
    failure$category=="parser",failure$resource=="market",grepl(failure$reference,got$logs,fixed=TRUE),
    !grepl("SECRET|private|fixture.invalid",got$logs))
  before<-calls;warm<-capture(await(async_adapter_fetch("get_diagnostic_replay_fixture",list(auth,"private-league","private-team"),ctx)))
  stopifnot(identical(calls,before),length(warm$logs)==0L)
  fm_async_read_http<-original_transport
})
check("the factual unavailable UI carries only the safe category and reference",{
  session<-shiny::MockShinySession$new();registry<-new.env(parent=emptyenv())
  session$userData$factual_read_registry<-registry
  args<-list(login=auth,championship_id="private-league",user_team_id="private-team")
  failure<-resource_failure(simpleError("HTTP 503 SECRET"),"market_private-league")
  resource<-paste0("get_market_players:",as.character(openssl::sha256(serialize(args,NULL))))
  registry[[paste0(session$ns(""),resource)]]<-shiny::reactive(resource_state(status="unavailable",error=failure))
  adapter<-factual_read_adapter(session,"get_market_players",function()auth,function()"private-league",function()"private-team")
  got<-capture(tryCatch(shiny::isolate(do.call(adapter,args)),error=identity))
  text<-conditionMessage(got$value)
  stopifnot(inherits(got$value,"shiny.silent.error"),grepl(failure$reference,text,fixed=TRUE),
    grepl("Source request unavailable",text,fixed=TRUE),!grepl("SECRET|private",text),length(got$logs)==0L)
  session$close()
})
cat(sprintf("READ DIAGNOSTICS: %d passed / 0 failed\n",n))
