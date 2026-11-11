#!/usr/bin/env Rscript
# Real My Team reactive + shared resource adapter; no accounts, HTTP or DB.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
for(method in c('GET','POST','PUT','PATCH','DELETE')) {
  trace(method,where=asNamespace('httr'),print=FALSE,
    tracer=quote(stop('Unexpected HTTP in empty-squad regression')))
}
auth<-c(userid='empty-squad-fixture',token='synthetic')
roster<-data.frame(id=character(),name=character(),role=character(),value=numeric(),
  change=numeric(),points=numeric(),stringsAsFactors=FALSE)
roster<-observed_data(roster,status='empty')
reads<-character();publish_roster<-NULL
async_adapter_fetch<-function(fn,args=list(),context,...) {
  reads<<-c(reads,fn)
  if(fn=='get_players_from_team')return(promises::promise(function(resolve,reject)publish_roster<<-resolve))
  if(fn=='get_my_market_players')return(promises::promise_resolve(observed_data(data.frame(id=character(),price=numeric()),status='empty')))
  stop('Unexpected resource in empty-squad fixture')
}
incoming_clause_source<-function(...)shiny::reactive(list(status='ok',records=incoming_clause_empty(),error=NULL))
selected_player_Server<-function(...)NULL
players_table_Server<-function(...)shiny::reactive(NULL)
settle<-function(session)for(i in seq_len(12L)){later::run_now(0);session$flushReact()}
clear_api_cache()
shiny::testServer(players_in_teams_Server,args=list(is_module_active=function()TRUE,
  login_token=function()auth,championship_id=function()'league',user_team_id=function()'team',
  user_teams_RV=function()data.frame()),{
  settle(session)
  pending<-tryCatch(players_table_RV(),error=identity)
  stopifnot(inherits(pending,'shiny.silent.error'),is.null(roster_state()$data),
    identical(roster_state()$status,'loading'),sum(reads=='get_my_market_players')==0L)
  cat('PASS pending roster stays loading without invented empty data or listing request\n')
  publish_roster(roster);settle(session)
  # Force the exact reactive in the production traceback.
  empty<-players_table_RV()
  stopifnot(nrow(empty)==0L,identical(empty$market_inMarket,logical()),
    identical(empty$selling_by,numeric()),identical(attr(empty,'fetch_status'),'empty'),
    identical(attr(empty,'observed_at'),attr(roster,'observed_at')),
    grepl('0 players in your squad',output$team_value_box$html,fixed=TRUE),
    sum(reads=='get_players_from_team')==1L,sum(reads=='get_my_market_players')==1L)
  cat('PASS verified empty squad renders without adding a row or changing source metadata\n')
})
cat('EMPTY SQUAD MODULE: 2 checks passed; source HTTP and DB reads=0\n')
