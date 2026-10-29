#!/usr/bin/env Rscript
# Synthetic transport fixtures exercise real sync loaders and async replay.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source("futmondo_functions.R"))
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat("PASS",name,"\n")}
auth<-c(token="synthetic-token",userid="synthetic-user");league<-"synthetic-league";team<-"synthetic-team"
ctx<-resource_context(auth,league,team,session_id="contract-fixture")
await<-function(p){done<-FALSE;out<-NULL;failure<-NULL;promises::then(p,function(x){out<<-x;done<<-TRUE},function(e){failure<<-e;done<<-TRUE});end<-Sys.time()+3;while(!done&&Sys.time()<end)later::run_now(.01);stopifnot(done);if(!is.null(failure))stop(failure);out}
requests<-list();mode<-"contracts"
player<-list(id="player",name="Observed player",role="defensa",role2=NULL,value=1000,change=0,points=-1,teamId="club")
contract_response<-function(url,body){
  payload<-jsonlite::fromJSON(as.character(body),simplifyVector=FALSE)
  stopifnot(identical(payload$header$token,unname(auth[["token"]])),
    identical(payload$header$userid,unname(auth[["userid"]])))
  for(field in intersect(c("championshipId","userteamId","type","from"),names(payload$query)))
    stopifnot(is.character(payload$query[[field]]),length(payload$query[[field]])==1L)
  if("excludeGeneral"%in%names(payload$query))stopifnot(identical(payload$query$excludeGeneral,FALSE),identical(payload$query$includeProphets,TRUE))
  route<-tail(strsplit(url,"/",fixed=TRUE)[[1]],1L)
  requests[[length(requests)+1L]]<<-list(route=route,query=payload$query)
  answer<-switch(route,championshipteams=list(list(id="club",name="Club",logo="club.svg")),
    activechampionships=list(championships=list(list(id=league,name="Synthetic league"))),
    roster=list(player),players=list(player),championshipplayers=list(players=list(player)),
    lineup=list(players=list(),multiposition=FALSE),teams=list(list(id=team,name="Team",points=0)),
    rosterbids=list(),myplayers=list(),pressroom={
      cursor<-payload$query$from
      if(cursor=="")list(news=list(list(`_id`="sensitive-cursor-one",created="2026-10-01",price=100)))else
      if(cursor=="sensitive-cursor-one")list(news=list(list(`_id`="sensitive-cursor-two",created="2026-10-02",price=200)))else
        list(news=list())
    },stop("Unexpected synthetic route"))
  list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=answer),auto_unbox=TRUE)),times=list(total=0))
}
futmondo_post<-function(url,body,...)fm_httr_response(contract_response(url,body),url)
fm_async_read_http<-function(url,method="POST",body=NULL,...){
  if(mode=="failure")return(promises::promise_reject(resource_failure(resource=url,category="http",http_status=503L)))
  promises::promise_resolve(contract_response(url,body))
}
load_all_sync<-function(){
  get_real_clubs(auth,league);get_championships(auth);get_players_from_team(auth,league,team)
  get_market_players(auth,league,team);get_championship_players(auth,league)
  get_lineup_from_team(auth,league,team);get_teams(auth,league)
}
check("all seven corrected read loaders serialize string and boolean scalars synchronously",{
  clear_api_cache();suppressWarnings(load_all_sync())
  expected<-c("championshipteams","activechampionships","roster","players","championshipplayers","lineup","teams")
  stopifnot(all(expected%in%vapply(requests,`[[`,character(1),"route")),length(requests)==9L)
  before<-length(requests);suppressWarnings(load_all_sync());stopifnot(length(requests)==before)
})
check("real async roster and market use scalar requests with five cold and zero warm reads",{
  clear_api_cache();requests<-list();args<-list(login=auth,championship_id=league,user_team_id=team)
  roster<-await(async_adapter_fetch("get_players_from_team",args,ctx))
  market<-await(async_adapter_fetch("get_market_players",args,ctx))
  stopifnot(nrow(roster)==1L,nrow(market)==1L,roster$id=="player",market$id=="player",length(requests)==5L)
  before<-length(requests);again<-await(async_adapter_fetch("get_players_from_team",args,ctx));await(async_adapter_fetch("get_market_players",args,ctx))
  stopifnot(length(requests)==before,identical(again,roster))
  await(async_adapter_fetch("get_championships",list(login=auth),ctx))
  await(async_adapter_fetch("get_championship_players",list(login=auth,championship_id=league),ctx))
  await(async_adapter_fetch("get_lineup_from_team",args,ctx));await(async_adapter_fetch("get_teams",list(login=auth,championship_id=league),ctx))
  stopifnot(length(requests)==9L)
  cat("MEASURE async_roster_market_cold_reads=5 warm_reads=0\n")
})
check("pressroom replay sends each of three cursors once and emits no cursor IDs",{
  clear_api_cache();requests<-list();mode<-"pressroom"
  output<-capture.output(rows<-await(async_adapter_fetch("get_championship_pressroom",list(login=auth,championship_id=league),ctx)))
  cursors<-vapply(requests,function(request)request$query$from,character(1))
  stopifnot(nrow(rows)==2L,identical(cursors,c("","sensitive-cursor-one","sensitive-cursor-two")),
    !any(grepl("sensitive-cursor",output,fixed=TRUE)),any(grepl("Pagination complete",output,fixed=TRUE)))
  before<-length(requests);await(async_adapter_fetch("get_championship_pressroom",list(login=auth,championship_id=league),ctx))
  stopifnot(length(requests)==before)
  cat("MEASURE pressroom_pages=3 actual_reads=3 warm_reads=0\n")
})
check("pressroom preserves the safe HTTP cause through its local catch and async wrapper",{
  clear_api_cache();mode<-"failure";logs<-character()
  failure<-withCallingHandlers(tryCatch(await(async_adapter_fetch("get_championship_pressroom",list(login=auth,championship_id=league),ctx)),error=identity),
    message=function(e){logs<<-c(logs,conditionMessage(e));invokeRestart("muffleMessage")})
  stopifnot(inherits(failure,"futmondo_unavailable"),failure$category=="http",failure$http_status==503L,
    failure$resource=="pressroom",grepl("^FM-HTTP-[0-9]{6}$",failure$reference),length(logs)==1L,
    grepl(failure$reference,logs,fixed=TRUE),!grepl("synthetic|sensitive-cursor|token",logs))
})
cat(sprintf("READ REQUEST CONTRACTS: %d passed / 0 failed\n",n))
