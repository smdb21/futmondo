#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source("global.R"))
clock <- Sys.time()
stamp <- function(time)format(time,"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
player <- function(id,index,club,role)
  list(id=id,name=id,position=index,locked=FALSE,teamId=club,role=role,points=0)
raw <- list(strategy="4-4-2",status="running",players=list(player("Forward",0,"one","delantero")),
  bench=list(enabled=TRUE,change=FALSE,multiposition=TRUE,players=list(
    c(player("Reserve",2,"two","defensa"),list(role2="delantero")))))
round <- list(id="live",number=8)
matches <- list(round=list(`_id`="live",number=8),matches=list(
  list(id="fixture1",status="P",date=stamp(clock+1800),homeTeam=list(id="one"),awayTeam=list(id="other")),
  list(id="fixture2",status="P",date=stamp(clock+3600),homeTeam=list(id="two"),awayTeam=list(id="another"))))
rows <- live_sub_matches(matches,round)
clean <- jsonlite::fromJSON(jsonlite::toJSON(round_lineup_clean_answer(raw),auto_unbox=TRUE),simplifyVector=FALSE)
opts <- function(answer=clean,fixtures=rows,now=clock)live_sub_options(answer,fixtures,now)
pair <- live_sub_pair(opts(),1L,3L)
stopifnot(identical(pair$outPlayer,list(index=0L,id="Forward")),
  identical(pair$inPlayer,list(index=2L,id="Reserve")),pair$cutoff==as.numeric(fm_time(rows$kickoff[1])))
for(who in c("out","in")){
  a <- raw
  if(who=="out")a$players[[1]]$locked <- TRUE else a$bench$players[[1]]$locked <- TRUE
  stopifnot(!length(opts(a)$choices[[1]]))
  f <- rows;f$kickoff[if(who=="out")1 else 2] <- stamp(clock)
  stopifnot(!length(opts(fixtures=f)$choices[[1]]))
}
for(field in c("locked","teamId")){a <- raw;a$players[[1]][[field]] <- NULL;stopifnot(!length(opts(a)$choices[[1]]))}
for(status in c("F","running","","?")){f <- rows;f$status[1] <- status;stopifnot(!length(opts(fixtures=f)$choices[[1]]))}
for(flag in c("enabled","multiposition")){a <- raw;a$bench[[flag]] <- FALSE;stopifnot(!length(opts(a)$choices[[1]]))}
a <- raw;a$status <- "closed";stopifnot(!length(opts(a)$choices[[1]]))
stopifnot(!length(opts(fixtures=rbind(rows,rows[1,]))$choices[[1]]))
rounds <- data.frame(round_id=c("split","live","past","next"),round_number=c(6,8,7,9),
  status=c("closed","running","closed","next"),is_finished=c(FALSE,FALSE,TRUE,FALSE),partially_played=c(TRUE,FALSE,FALSE,FALSE))
stopifnot(identical(live_sub_rounds(rounds)$round_id,c("live","split")))
cat("PASS round identity, secondary role, both kickoff boundaries, lock flags, missing and conflicting fixture evidence\n")

auth <- c(userid="fixture-account",token="synthetic")
ctx <- resource_context(auth,"league","own",competition_id="competition",season="season",scoring_version="scoring")
calls <- character();writes <- 0L;response <- raw
active <- list(championships=list(list(id="league",userteam=list(id="own"),league="competition")))
lineup_api_read <- function(route,auth,context,owner,extra=list(),worker_live=FALSE){
  stopifnot(isTRUE(worker_live))
  calls <<- c(calls,route)
  promises::promise_resolve(switch(route,"/1/userteam/roundlineup"=response,
    "/1/match/list"=matches,"/2/user/activechampionships"=active))
}
settle <- function(p){
  done <- FALSE;value <- NULL
  promises::then(p,function(x){done <<- TRUE;value <<- x;NULL},function(e){done <<- TRUE;value <<- e;NULL})
  for(i in seq_len(100)){later::run_now(.01);if(done)break}
  stopifnot(done);value
}
check <- function()settle(live_sub_preflight(auth,ctx,round,pair,"fixture",function()TRUE))
stopifnot(isTRUE(check()),length(calls)==3L)
active$championships[[1]]$userteam$id <- "rival";stopifnot(inherits(check(),"error"))
active$championships[[1]]$userteam$id <- "own"
response$players[[1]]$id <- "new";stopifnot(inherits(check(),"error"))
response <- raw;matches$round$`_id` <- "next";stopifnot(inherits(check(),"error"));matches$round$`_id` <- "live"
response$status <- "closed";stopifnot(inherits(check(),"error"));response <- raw
# Exercise the actual transport dispatch boundary after the authorized window closes.
task <- new.env(parent=emptyenv())
task$active <- FALSE;task$priority <- "interactive";task$queued_at <- clock
task$before_dispatch <- function()FALSE
task$reject <- function(err)task$failure <- conditionMessage(err)
.fm_transport$tasks[["expired-live-sub"]] <- task
fm_transport_start("expired-live-sub")
stopifnot(identical(task$failure,"The confirmed action expired before dispatch."),
  !task$active,is.null(task$handle),is.null(.fm_transport$tasks[["expired-live-sub"]]))

fm_async_http <- function(url,method,body,headers,owner,priority,before_dispatch){
  q <- jsonlite::fromJSON(body,simplifyVector=FALSE)$query
  stopifnot(method=="POST",endsWith(url,"/2/userteam/makesubstitution"),
    identical(q,list(championshipId="league",userteamId="own",roundId="live",inPlayer=pair$inPlayer,outPlayer=pair$outPlayer)))
  if(!before_dispatch())return(promises::promise_reject(simpleError("expired")))
  writes <<- writes+1L
  promises::promise_resolve(list(content=charToRaw('{"answer":{"error":true}}')))
}
stopifnot(inherits(settle(live_sub_write(auth,ctx,round,pair,"fixture",function()FALSE)),"error"),writes==0,
  identical(settle(live_sub_write(auth,ctx,round,pair,"fixture",function()TRUE)),FALSE),writes==1)
swapped <- raw;swapped$players[[1]] <- raw$bench$players[[1]];swapped$players[[1]]$position <- 0
swapped$bench$players[[1]] <- raw$players[[1]];swapped$bench$players[[1]]$position <- 2
stopifnot(!live_sub_verified(raw,pair),live_sub_verified(swapped,pair))
cat("PASS live ownership, round and pair guards, exact one-pair POST, native rejection, readback of both slots\n")


if("--browser" %in% commandArgs(TRUE)){
  browser_db <- NULL;reads <- writes <- saves <- 0L;calls <- character();response <- raw
  # A second browser session restores the acknowledged synthetic database record.
  browser_snapshot <- function(answer){
    value <- list(round_id="live",answer=round_lineup_clean_answer(answer),valid_until=as.numeric(Sys.time())+300)
    attr(value,"observed_at") <- Sys.time();value
  }
  browser_db <- browser_snapshot(raw)
  first_readback <- TRUE
  preflight_read <- lineup_api_read
  lineup_api_read <- function(route,...){
    if(writes>0 && first_readback && route=="/1/userteam/roundlineup"){
      first_readback <<- FALSE;calls <<- c(calls,route)
      return(promises::promise_reject(simpleError("synthetic readback outage")))
    }
    preflight_read(route,...)
  }
  original_async <- async_adapter_fetch
  async_adapter_fetch <- function(fn,args,context,...){
    if(identical(fn,"read_saved_round_lineup")){reads <<- reads+1L;return(promises::promise_resolve(browser_db))}
    original_async(fn,args,context,...)
  }
  persist_round_lineup_snapshot <- function(data,context,on_complete=NULL){
    saves <<- saves+1L
    if(saves>1L)browser_db <<- data
    on_complete(saves>1L);TRUE
  }
  fm_worker_resolve_context <- function(handler,args,context){
    context[c("competition_id","season","scoring_version")] <- ctx[c("competition_id","season","scoring_version")]
    context
  }
  fixtures <- league_fixture_matches(list(list(`_id`="live",matches=lapply(matches$matches,function(m)
    list(`_id`=m$id,st=m$status,h=list(id=m$homeTeam$id,name=m$homeTeam$id),
      a=list(id=m$awayTeam$id,name=m$awayTeam$id),info=list(date=m$date))))))
  # The two pitches consume the same verified, provider-independent player IDs.
  club_xi_source <- function(...)reactive({
    sheet <- lapply(seq_len(nrow(fixtures$rows)),function(i){
      m <- as.list(fixtures$rows[i,,drop=FALSE])
      m$observed <- as.numeric(clock)
      m$unix <- as.numeric(fm_time(m$kickoff))
      m$status <- "confirmed"
      m$players <- list(Forward="benched",Reserve="starting")
      m
    })
    list(status="ready",matches=setNames(sheet,fixtures$rows$match_id))
  })
  all_rounds <- rounds[rounds$round_id!="split",]
  all_rounds$begin_process <- stamp(clock+c(-3600,-86400,86400))
  attr(all_rounds,"fixture_calendar") <- list(matches=fixtures,status="ok",scope=ctx,observed_at=clock,valid_until=as.numeric(clock)+300)
  bind_factual_reads <- function(...)invisible(NULL)
  shared_mvp_source <- function(...)reactive(list())
  shared_rival_finance_source <- function(...)reactive(list(loading=FALSE))
  get_classification_round_catalog <- function(...)list(rounds=all_rounds,current_round_id="live")
  get_user_team_rounds <- function(...)list(list(round=8,points=12))
  get_classification_round_ranking <- function(...)list(rows=data.frame(team_id="own",team_name="My team",round=8,points=12,players_left=2,score_status="provisional"))
  get_user_team_info <- function(...)list(configuration=list())
  get_round_dreamteam <- function(...)list()
  get_finished_rounds <- function(...)all_rounds
  fm_async_http <- function(url,method,body,headers,owner,priority,before_dispatch){
    stopifnot(endsWith(url,"/makesubstitution"))
    incoming <- jsonlite::fromJSON(body,simplifyVector=FALSE)$query$inPlayer$id
    promises::promise(function(resolve,reject)later::later(function(){
      if(!before_dispatch()){reject(simpleError("expired"));return()}
      writes <<- writes+1L;response <<- if(incoming=="Reserve")swapped else raw
      reject(simpleError("synthetic timeout after acceptance"))
    },.35))
  }
  ui <- fluidPage(tags$head(includeCSS("www/custom_style.css")),textOutput("counts"),
    tabsetPanel(id="screen",tabPanel("Lineup",lineup_UI("lineup")),
      tabPanel("Classification",classification_UI("classification"))))
  server <- function(input,output,session){
    login <- reactive(auth);league <- reactive("league");team <- reactive("own");calendar <- reactive(all_rounds)
    lineup_Server("lineup",reactive(identical(input$screen,"Lineup")),login,league,team,calendar,
      clause_transfers_source=reactive(list()))
    classification_Server("classification",reactive(identical(input$screen,"Classification")),login,league,team,
      reactive(data.frame(teamid="own",teamname="My team",points=12)),round_calendar=calendar)
    output$counts <- renderText({invalidateLater(100);paste(length(calls),writes,saves,reads)})
  }
  shiny::runApp(shinyApp(ui,server),host="127.0.0.1",port=as.integer(Sys.getenv("FUTMONDO_LIVE_TEST_PORT","8597")),launch.browser=FALSE)
}
