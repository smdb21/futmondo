#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
selected_player_Server<-function(...)NULL
recent_player_points_source<-function(...)list(history=function()data.frame(),status=function()NULL)
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
settle<-function(session){for(i in seq_len(8)){later::run_now(0);session$flushReact()}}
# Native source rows need not contain derived isClause or an optional lock date.
source_rows<-shiny::reactiveVal(data.frame(id=c('free','owned'),name=c('Free','Owned'),role='defensa',value=1e6,
  clause_price=2e6,clause_transferred=FALSE,computer=c(TRUE,FALSE),userteamId=c('','rival')))
shiny::testServer(players_table_Server,args=list(players_table_RV=source_rows,user_teams_RV=shiny::reactive(NULL),optional_ready=function()FALSE),{
 check('schema observer survives both filters with absent clause fields',{
  session$setInputs(active_clause_filter=TRUE,is_from_futmondo_filter=TRUE);settle(session)
  stopifnot(identical(players_table_filtered_RV()$id,'free'),!is.null(output$players_table))
  rows<-source_rows();rows$isClause<-TRUE;source_rows(rows);settle(session)
  stopifnot(identical(players_table_filtered_RV()$id,'free'),!is.null(output$players_table))
 })
 check('optional filters accept absent fields and zero matches render then recover',{
  source_rows(data.frame(id='minimal',name='Minimal',value=100));settle(session)
  session$setInputs(active_clause_filter=TRUE,is_from_futmondo_filter=TRUE,is_favorite_filter=TRUE,
    players_you_bid_filter=TRUE,players_with_bid_filter=TRUE,position_filter='Defender',change_value_filter=0)
  settle(session);stopifnot(nrow(players_table_filtered_RV())==0L,!is.null(output$players_table))
  session$setInputs(active_clause_filter=FALSE,is_from_futmondo_filter=FALSE,is_favorite_filter=FALSE,
    players_you_bid_filter=FALSE,players_with_bid_filter=FALSE,position_filter='All',change_value_filter=NA)
  settle(session);stopifnot(identical(players_table_filtered_RV()$id,'minimal'),!is.null(output$players_table),
    !any(c('role','role2','clause_date','fav','bid_price','numberOfBids')%in%names(table_display_data())))
 })
 check('genuine empty source retains a renderable typed table',{
  source_rows(data.frame(id=character(),name=character(),value=numeric()));settle(session)
  stopifnot(is.data.frame(players_table_filtered_RV()),nrow(players_table_filtered_RV())==0L,!is.null(output$players_table))
 })

})
shiny::testServer(players_table_Server,args=list(players_table_RV=shiny::reactive(data.frame(id=c("low","high"),name=c("Low","High"),value=c(1e6,2e9))),user_teams_RV=shiny::reactive(NULL),optional_ready=function()FALSE),{
 check('blank maximum includes values above 1000M and a supplied cap still filters',{
  session$setInputs(max_value_filter=NA_real_);settle(session)
  stopifnot(nrow(players_table_filtered_RV())==2)
  session$setInputs(max_value_filter=1000);settle(session)
  stopifnot(identical(players_table_filtered_RV()$id,"low"))
  session$setInputs(max_value_filter=NA_real_);settle(session)
  stopifnot(nrow(players_table_filtered_RV())==2)
 })
})
check('clause eligibility uses price, transfer state and UTC lock times',{
 now<-as.POSIXct('2026-11-01 12:00:00',tz='UTC')
 rows<-data.frame(clause_price=c(10,10,10,10,10,0,10,10),clause_transferred=c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,NA,NA),
  isClause=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,NA,TRUE),clause_date=c(NA,'','2026-11-01T13:00:00+01:00','2026-11-01T13:00:00Z','','','','bad date'))
 stopifnot(identical(players_table_active_clause(rows,now),c(TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)))
})
check('owner aliases override flags; unknown owner with computer FALSE is not free',{
 rows<-data.frame(id=letters[1:6],owner_team_id=c('','known','','','',''),userteamId=c('rival','','','','',''),
   userTeam=c('','','Named owner','','',''),computer=c(TRUE,TRUE,TRUE,TRUE,FALSE,NA))
 stopifnot(identical(players_table_normalize_owner(rows)$is_free_agent,c(FALSE,FALSE,FALSE,TRUE,FALSE,TRUE)))
 unknown<-players_table_normalize_owner(data.frame(id=c('u','f'),computer=c(FALSE,TRUE)))
 stopifnot(identical(unknown$is_free_agent,c(FALSE,TRUE)))
})
check('Today and Players checkbox defaults apply only to their tables',{
 checked<-function(html,id){tag<-regmatches(html,regexpr(paste0('<input[^>]*id="',id,'"[^>]*>'),html));length(tag)==1L&&grepl('checked',tag,fixed=TRUE)}
 today<-htmltools::renderTags(today_UI('today'))$html;players<-htmltools::renderTags(players_in_championship_UI('players'))$html
 plain<-htmltools::renderTags(players_table_UI('plain'))$html
 stopifnot(checked(today,'today-market_players-is_from_futmondo_filter'),!checked(today,'today-market_players-active_clause_filter'),
   checked(players,'players-championship_players_table-active_clause_filter'),!checked(players,'players-championship_players_table-is_from_futmondo_filter'),
   !checked(plain,'plain-active_clause_filter'),!checked(plain,'plain-is_from_futmondo_filter'))
})
rows<-data.frame(id=paste0('p',seq_len(524)),name='Player',value=1e6,clause_price=2e6,clause_transferred=FALSE,computer=TRUE)
elapsed<-system.time({normalized<-players_table_filter_fields(players_table_normalize_owner(rows));mask<-players_table_active_clause(normalized)&normalized$is_free_agent})[['elapsed']]
stopifnot(all(mask));cat(sprintf('METRIC 524-row normalization/combined filter: %.3fs; %.1fKiB\n',elapsed,as.numeric(object.size(normalized))/1024))
cat(sprintf('PLAYER TABLE FILTERS: %d passed / 0 failed\n',n))
