#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
check<-function(label,expr){force(expr);cat('PASS',label,'\n')}
selected_player_Server<-function(...)NULL
recent_player_points_source<-function(...)list(history=function()data.frame(),status=function()NULL)
settle<-function(session){session$elapse(350);for(i in 1:10){later::run_now(0);session$flushReact()}}
check('championship own-team checkbox exists, starts unchecked, and is absent from other tables',{
 html<-as.character(players_in_championship_UI('all'));id<-'all-championship_players_table-show_my_team_filter'
 input<-regmatches(html,regexpr(paste0('<input[^>]*id="',id,'"[^>]*>'),html))
 stopifnot(length(input)==1L,nzchar(input),!grepl('checked|disabled',input),grepl('Show players in my team',html,fixed=TRUE),
  !grepl('show_my_team_filter',as.character(players_table_UI('other')),fixed=TRUE))
})
rows<-data.frame(id=c('mine','mine-closed','rival','free','unknown'),name=c('Mine','Mine closed','Rival','Free','Unknown'),
 role=c('Defender','Forward','Defender','Forward','Goalkeeper'),role2='',value=1e6,
 owner_team_id=c('','','rival-id','',''),userteamId=c('mine-id','mine-id','','',''),
 userTeam=c('Same name','Same name','Same name','',''),computer=c(FALSE,FALSE,FALSE,TRUE,FALSE),
 clause_price=c(2e6,2e6,3e6,2e6,NA_real_),clause_transferred=c(FALSE,TRUE,FALSE,FALSE,NA),isClause=TRUE)
current<-shiny::reactiveVal('mine-id');data<-shiny::reactiveVal(rows);enrichments<-0L
original<-add_recent_points_average;add_recent_points_average<-function(...){enrichments<<-enrichments+1L;original(...)}
reset_messages<-list();updateCheckboxInput<-function(session,inputId,value,...){reset_messages[[inputId]]<<-value;invisible(NULL)}
shiny::testServer(players_table_Server,args=list(players_table_RV=data,user_teams_RV=function()NULL,
 user_team_id=current,filter_by_own_team=TRUE,optional_ready=function()FALSE),{
 settle(session);before<-enrichments
 check('own players are excluded by default using immutable IDs, including fallback aliases',{
  stopifnot(identical(players_table_filtered_RV()$id,c('rival','free','unknown')))
 })
 check('enabling includes own players and all other filters still apply',{
  session$setInputs(show_my_team_filter=TRUE);settle(session);stopifnot(nrow(players_table_filtered_RV())==5L)
  session$setInputs(active_clause_filter=TRUE);settle(session);stopifnot(identical(players_table_filtered_RV()$id,c('mine','rival','free')))
  session$setInputs(team_filter='team:mine-id');settle(session);stopifnot(identical(players_table_filtered_RV()$id,'mine'))
  session$setInputs(show_my_team_filter=FALSE);settle(session);stopifnot(nrow(players_table_filtered_RV())==0L)
  session$setInputs(team_filter='All',active_clause_filter=FALSE);settle(session)
  stopifnot(identical(players_table_filtered_RV()$id,c('rival','free','unknown')),enrichments==before)
 })
 check('league/team changes use the new own identity and Clear Filters resets off',{
  current('');settle(session);stopifnot(inherits(tryCatch(players_table_filtered_RV(),error=identity),'shiny.silent.error'))
  current('rival-id');settle(session);stopifnot(identical(players_table_filtered_RV()$id,c('mine','mine-closed','free','unknown')))
  session$setInputs(show_my_team_filter=TRUE,btn_clear_filters=1);settle(session);stopifnot(identical(reset_messages$show_my_team_filter,FALSE))
 })
})
add_recent_points_average<-original
check('shared effective-count ledger deduplicates transfers and never double counts arrivals/departures already applied',{
 roster<-data.frame(id=c('a','b','c'))
 stopifnot(effective_squad_count(roster,c('x','x','y'),'a')==4L,
  effective_squad_count(roster,c('a','x'),c('b','already-gone'))==3L,
  effective_squad_count(roster,character(),character())==3L,
  effective_squad_count(roster[0,,drop=FALSE],character(),character())==0L,
  is.na(effective_squad_count(roster,NULL,character())),is.na(effective_squad_count(roster,c(''),character())),
  is.na(effective_squad_count(roster,'x','x')),is.na(effective_squad_count(data.frame(id=c('a','a')),character(),character())))
})
roster<-data.frame(id=paste0('p',1:18),name='Player',role='Defender',value=100)
transfers<-list(status='observed',outgoing_status='observed',observations_only=TRUE,
 records=data.frame(player_id=c('p19','p20','p19')),outgoing_records=data.frame(player_id=c('p1','p1')))
check('observed effective count uses current + bought - leaving and remains unknown for partial data',{
 x<-team_effective_squad_count(roster,transfers);stopifnot(x$count==19L,x$status=='observed',grepl('18 current + 2 bought',x$detail,fixed=TRUE))
 for(kind in c('loading','partial','unavailable')){
  bad<-transfers;bad$status<-kind;stopifnot(is.na(team_effective_squad_count(roster,bad)$count))
 }
 bad<-transfers;bad$outgoing_records<-NULL;stopifnot(is.na(team_effective_squad_count(roster,bad)$count))
 for(kind in c('partial','unavailable')){bad_roster<-roster;attr(bad_roster,'fetch_status')<-kind
  stopifnot(is.na(team_effective_squad_count(bad_roster,transfers)$count))}
 bad_roster<-roster;attr(bad_roster,'worker_completeness')<-'unavailable';stopifnot(is.na(team_effective_squad_count(bad_roster,transfers)$count))
 for(kind in c('failed','partial')){bad_roster<-roster;attr(bad_roster,'worker_acquisition_status')<-kind;stopifnot(is.na(team_effective_squad_count(bad_roster,transfers)$count))}
 empty<-list(status='ok',outgoing_status='ok',records=incoming_clause_empty(),outgoing_records=outgoing_clause_empty())
 stopifnot(team_effective_squad_count(roster[0,,drop=FALSE],empty)$count==0L)
})
count<-shiny::reactiveVal(team_effective_squad_count(roster,transfers))
shiny::testServer(players_table_Server,args=list(players_table_RV=function()roster,user_teams_RV=function()NULL,
 optional_ready=function()FALSE,effective_squad_source=count),{
 settle(session)
 check('effective badge sits beside the actual count and position filters do not alter either squad total',{
  html<-output$position_breakdown_ui$html
  stopifnot(grepl('Total: 18 Players',html,fixed=TRUE),grepl('Effective: 19 Players',html,fixed=TRUE))
  session$setInputs(position_filter='Forward');settle(session)
  stopifnot(grepl('Total: 18 Players',output$position_breakdown_ui$html,fixed=TRUE),grepl('Effective: 19 Players',output$position_breakdown_ui$html,fixed=TRUE))
  count(list(count=NA_integer_,status='loading',detail='Checking transfers'));settle(session)
  stopifnot(grepl('Effective: Checking',output$position_breakdown_ui$html,fixed=TRUE),!grepl('Effective: 0',output$position_breakdown_ui$html,fixed=TRUE))
 })
})

shiny::testServer(players_table_Server,args=list(players_table_RV=function()roster[0,,drop=FALSE],user_teams_RV=function()NULL,
 optional_ready=function()FALSE,effective_squad_source=function()list(count=0L,status='ok',detail='Confirmed empty')),{
 settle(session)
 check('confirmed empty squad displays actual and effective zero',{
  stopifnot(grepl('Total: 0 Players',output$position_breakdown_ui$html,fixed=TRUE),
   grepl('Effective: 0 Players',output$position_breakdown_ui$html,fixed=TRUE))
 })
})
