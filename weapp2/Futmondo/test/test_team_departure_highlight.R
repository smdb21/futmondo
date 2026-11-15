#!/usr/bin/env Rscript
# Offline deadline and actual shared-table regression. No accounts or transactions.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(label,code){force(code);checks<<-checks+1L;cat('PASS',label,'\n')}
kickoff<-fm_time('2026-11-15T17:00:00Z')
transfers<-list(outgoing_status='ok',outgoing_records=data.frame(
 player_id=c('before','at','after'),leaves_at=kickoff+c(-1,0,1),stringsAsFactors=FALSE))
check('before and exact kickoff are excluded; later departures remain eligible',{
 result<-clause_departure_round_state(transfers,list(starts_at=kickoff))
 stopifnot(identical(result$excluded_ids,c('at','before')),isTRUE(result$complete),!length(result$unknown_ids))
})
check('missing dates and unknown schedule never establish eligibility',{
 unknown<-transfers;unknown$outgoing_records$leaves_at[3]<-as.POSIXct(NA,tz='UTC')
 result<-clause_departure_round_state(unknown,list(starts_at=kickoff))
 stopifnot(identical(result$excluded_ids,c('at','before')),identical(result$unknown_ids,'after'),!result$complete)
 for(target in list(NULL,list(),list(available=FALSE,starts_at=kickoff),list(starts_at=as.POSIXct(NA,tz='UTC')))){
  result<-clause_departure_round_state(transfers,target)
  stopifnot(!length(result$excluded_ids),!result$complete)
 }
 stopifnot(!clause_departure_round_state(list(outgoing_status='ok'),list(starts_at=kickoff))$complete)
})
check('retained known deadlines remain excluded during incomplete source verification',{
 partial<-transfers;partial$outgoing_status<-'partial';partial$outgoing_error<-'Departure source failed.'
 result<-clause_departure_round_state(partial,list(starts_at=kickoff))
 stopifnot(identical(result$excluded_ids,c('at','before')),!result$complete,nzchar(result$error))
 observed<-transfers;observed$outgoing_status<-'observed'
 stopifnot(clause_departure_round_state(observed,list(starts_at=kickoff))$complete)
 empty<-list(outgoing_status='ok',outgoing_records=outgoing_clause_empty())
 stopifnot(clause_departure_round_state(empty,list(starts_at=kickoff))$complete,
  !length(clause_departure_round_state(empty,list(starts_at=kickoff))$excluded_ids))
})
selected_player_Server<-function(...)NULL
recent_player_points_source<-function(...)list(history=function()data.frame(),status=function()NULL)
source_rows<-shiny::reactiveVal(data.frame(id=c('before','at','after'),name=c('Before','At','After'),
 role=c('Defender','Forward','Midfielder'),points=c(5,6,7),value=c(1e6,2e6,3e6)))
source_transfers<-shiny::reactiveVal(transfers)
round<-shiny::reactiveVal(list(starts_at=kickoff))
updates<-list();enrichments<-0L;original_average<-add_recent_points_average
add_recent_points_average<-function(...){enrichments<<-enrichments+1L;original_average(...)}
updateReactable<-function(outputId,data=NULL,...){if(!is.null(data))updates[[length(updates)+1L]]<<-list(data=data,args=list(...));invisible(NULL)}
getReactableState<-function(outputId,name,session){switch(name,page=1L,pageSize=20L,selected=NULL,NULL)}
settle<-function(session)for(i in 1:8){later::run_now(0);session$flushReact()}
shiny::testServer(players_table_Server,args=list(players_table_RV=source_rows,user_teams_RV=function()NULL,
 own_squad=TRUE,departing_player_ids=shiny::reactive(clause_departure_round_state(source_transfers(),round())$excluded_ids)),{
 settle(session);initial<-output$players_table;schema<-table_schema();before<-enrichments
 check('own-team widget carries highlighted source-row indexes without visible metadata columns',{
  widget<-jsonlite::fromJSON(initial,simplifyVector=FALSE)$x$tag$attribs
  stopifnot(identical(unlist(widget$meta$departingPlayerIndexes),c(0L,1L)),
   identical(widget$rowClass,as.character(players_table_departure_class())),
   any(vapply(widget$columns,function(column)identical(column$id,'.selection')&&identical(column$show,FALSE),logical(1))),
   !any(grepl('depart',names(table_display_data()))))
 })
 check('departure-only changes update metadata in place without roster enrichment',{
  changed<-transfers;changed$outgoing_records$leaves_at[2]<-kickoff+1
  source_transfers(changed);settle(session)
  stopifnot(identical(table_color_meta()$departingPlayerIndexes,list(0L)),enrichments==before,
   identical(table_schema(),schema),identical(output$players_table,initial),
   length(updates)==1L,identical(updates[[1]]$args$meta$departingPlayerIndexes,list(0L)))
 })
 check('source-clock-only updates do not redraw or enrich the table',{
  before_updates<-length(updates)
  elapsed<-system.time(for(i in 1:10){
   changed<-source_transfers();changed$source_times<-list(roster=Sys.time()+i)
   source_transfers(changed);settle(session)
  })[['elapsed']]
  stopifnot(length(updates)==before_updates,enrichments==before,identical(output$players_table,initial))
  cat(sprintf('  ten source-clock updates: %.3fs, zero widget updates or enrichment\n',elapsed))
 })
 session$setInputs(position_filter='Forward');settle(session);invisible(output$players_table)
 check('filtered row indexes never inherit another player departure highlight',{
  stopifnot(identical(table_display_ids(),'at'),!length(table_color_meta()$departingPlayerIndexes),enrichments==before)
  source_transfers(transfers);settle(session)
  stopifnot(identical(table_color_meta()$departingPlayerIndexes,list(0L)),enrichments==before)
 })
 check('empty new-context evidence clears previous highlights without a roster mutation',{
  source_transfers(list(outgoing_status='loading',outgoing_records=outgoing_clause_empty()));settle(session)
  stopifnot(!length(table_color_meta()$departingPlayerIndexes),identical(table_display_ids(),'at'))
 })
})
add_recent_points_average<-original_average
check('client sorting uses stable original row identity and safely handles empty metadata',{
 fixture<-tempfile(fileext='.json');script<-tempfile(fileext='.cjs')
 jsonlite::write_json(list(rowClass=as.character(players_table_departure_class())),fixture,auto_unbox=TRUE)
 writeLines(c("const assert=require('node:assert/strict'),fs=require('node:fs');",
  "const fn=eval('('+JSON.parse(fs.readFileSync(process.argv[2],'utf8')).rowClass+')');",
  "const state={meta:{departingPlayerIndexes:[0,2]}};",
  "assert.equal(fn({index:2,viewIndex:0},state),'fm-departing-before-round');",
  "assert.equal(fn({index:1,viewIndex:2},state)||'','');",
  "assert.equal(fn({index:0,viewIndex:1},state),'fm-departing-before-round');",
  "assert.equal(fn({index:0},{meta:{departingPlayerIndexes:[]}})||'','');",
  "assert.equal(fn({index:0},{})||'','');"),script)
 stopifnot(system2('node',c(shQuote(script),shQuote(fixture)))==0L);unlink(c(fixture,script))
})
cat(sprintf('TEAM DEPARTURE HIGHLIGHT: %d checks passed\n',checks))
