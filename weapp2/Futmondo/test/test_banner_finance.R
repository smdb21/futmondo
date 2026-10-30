#!/usr/bin/env Rscript
# Real server wiring with controlled, synthetic primary reads and no live I/O.
expressions <- parse("test/test_application_offline.R")
for (expr in expressions) {
  if (is.call(expr) && identical(expr[[1]],as.name("app_check")) &&
      identical(expr[[2]],"full server login, navigation, refresh and logout")) break
  eval(expr,envir=.GlobalEnv)
}
checks <- 0L
check <- function(name, code) { force(code); checks <<- checks+1L; cat("PASS",name,"\n") }
original_async <- async_adapter_fetch
calls <- character(); pending <- new.env(parent=emptyenv())
async_adapter_fetch <- function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE) {
  calls <<- c(calls,fn)
  if (fn %in% c("get_user_team_info","get_market_players")) {
    key <- paste(fn,context$championship_id,context$user_team_id,sep=":")
    return(promises::promise(function(resolve,reject)pending[[key]]<-list(resolve=resolve,reject=reject,current=is_current)))
  }
  original_async(fn,args,context,owner,is_current)
}
# The banner must never depend on this failing composite path.
get_financial_snapshot <- function(...) stop("Composite finance is deliberately unavailable")
app_rounds <- data.frame(round_number=11,begin_process=format(Sys.time()+86400,"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),is_finished=FALSE)
clear_api_cache()
shiny::testServer(app_server,{
  session$setInputs(tabs="login",`login-user_name`="offline@example.invalid",`login-password`="fixture",`login-login_button`=1)
  session$elapse(100);app_settle(session);session$setInputs(tabs="today");app_settle(session)
  team_key <- "get_user_team_info:champ:team1"; market_key <- "get_market_players:champ:team1"
  check("Today and banner dispatch one team read and one market read", {
    stopifnot(sum(calls=="get_user_team_info")==1L,sum(calls=="get_market_players")==1L,
      !any(calls=="get_financial_snapshot"))
  })
  pending[[team_key]]$resolve(list(budget=180546496,withheld=0,teamValue=350438934));app_settle(session)
  check("known balance renders in both places while offers and schedule are pending", {
    html <- output$round_countdown$html
    stopifnot(grepl("180.546.496 EUR",html,fixed=TRUE),grepl("Loading active offers",html,fixed=TRUE),
      !grepl("round-countdown-warning",html,fixed=TRUE),
      grepl("180.546.496 €",output[["today-kpi_cash_box"]]$html,fixed=TRUE))
  })
  pending[[market_key]]$reject(simpleError("Synthetic market timeout"));app_settle(session)
  check("failed offers retain known cash and produce a neutral retry message", {
    html <- output$round_countdown$html
    stopifnot(grepl("180.546.496 EUR",html,fixed=TRUE),grepl("could not load",html,fixed=TRUE),
      grepl("Active offers: Unavailable",html,fixed=TRUE),!grepl("round-countdown-warning",html,fixed=TRUE))
  })
  before <- calls
  for (i in seq_len(3L)) { session$elapse(1000);app_settle(session);invisible(output$round_countdown) }
  check("clock ticks do not start new team, market, or composite reads", {
    added <- if (length(calls)>length(before))tail(calls,length(calls)-length(before))else character()
    stopifnot(!any(added %in% c("get_user_team_info","get_market_players","get_financial_snapshot")))
  })
  before <- calls;today$retry_failed();app_settle(session)
  check("targeted Today retry preserves successful balance and retries only market", {
    added <- tail(calls,length(calls)-length(before))
    stopifnot(identical(added,"get_market_players"))
  })
  pending[[market_key]]$resolve(data.frame());app_settle(session)
  check("verified empty market produces zero reservation and a solvent banner", {
    html <- output$round_countdown$html
    stopifnot(grepl("round-countdown-solvent",html,fixed=TRUE),
      grepl("Active offers: 0 EUR (0)",html,fixed=TRUE),grepl("After offers: 180.546.496 EUR",html,fixed=TRUE))
  })
  before <- calls;session$setInputs(tabs="yourteam");app_settle(session);invisible(output$round_countdown)
  check("navigation reuses shared primary states", {
    added <- if (length(calls)>length(before))tail(calls,length(calls)-length(before))else character()
    stopifnot(!any(added %in% c("get_user_team_info","get_market_players","get_financial_snapshot")))
  })
  session$setInputs(tabs="today",refresh_all=1);app_settle(session)
  pending[[team_key]]$resolve(list(budget=-10,withheld=0,teamValue=1000))
  pending[[market_key]]$resolve(data.frame());app_settle(session)
  check("confirmed fresh debt is still visibly warned", {
    html <- output$round_countdown$html
    stopifnot(grepl("round-countdown-warning",html,fixed=TRUE),grepl("Restore at least 11 EUR",html,fixed=TRUE))
  })
  # Superseding a context must discard known old balance before new data arrives.
  .GlobalEnv$app_champ <- list(id="different-league",name="Other League",userteam=list(id="different-team",name="Other Team"))
  invalidate_resources(resource_context(app_auth),pattern="active_championship")
  session$setInputs(refresh_all=2);app_settle(session)
  check("context change clears old team cash while new context loads", {
    html <- output$round_countdown$html
    stopifnot(identical(championship_id_RV(),"different-league"),!grepl("Balance: -10 EUR",html,fixed=TRUE),
      grepl("Balance: Loading",html,fixed=TRUE))
  })
})
check("no HTTP attempts occurred",stopifnot(offline_http_attempts==0L))
cat(sprintf("BANNER FINANCE: %d passed / 0 failed\n",checks))
for(method in c("GET","POST","PUT","PATCH","DELETE"))suppressMessages(untrace(method,where=asNamespace("httr")))
