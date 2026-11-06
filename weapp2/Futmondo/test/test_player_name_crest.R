#!/usr/bin/env Rscript
# Offline presentation regression; no external requests or account data.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
rows<-data.frame(id=paste0('p',1:4),name=c('Vivian','Kike Salas','Unknown club','<unsafe>'),
  role=c('Defender','Defender','Midfielder','Forward'),
  team=c('Athletic Club','Sevilla',NA,'Athletic Club'),
  logo=c('authoritative-club.png',NA,NA,NA),value=2e6,
  average.fitness='2,5,12,12,6',average.averageLastFive=7.4)
before<-rows
columns<-get_reactable_columns_for_players(rows)
html<-vapply(seq_len(nrow(rows)),function(i)as.character(columns$name$cell(rows$name[i],i)),character(1))
check('name remains sortable text; redundant club metadata is hidden',{
 stopifnot(identical(rows,before),identical(columns$name$name,'Name'),
  isFALSE(columns$team$show),isFALSE(columns$logo$show))
})
check('source crest takes precedence and retains the accessible club label',{
 stopifnot(grepl(paste0(TEAM_LOGO_URL,'authoritative-club.png'),html[1],fixed=TRUE),
  grepl('alt="Athletic Club"',html[1],fixed=TRUE),grepl('title="Athletic Club"',html[1],fixed=TRUE),
  grepl('Vivian',html[1],fixed=TRUE),grepl('loading="lazy"',html[1],fixed=TRUE),
  grepl('decoding="async"',html[1],fixed=TRUE),grepl('overflow-wrap: anywhere',html[1],fixed=TRUE))
})
check('club fallback, unavailable crest and escaped names remain usable',{
 stopifnot(grepl(paste0(TEAM_LOGO_URL,'sevilla.png'),html[2],fixed=TRUE),
  !grepl('<img',html[3],fixed=TRUE),grepl('Unknown club',html[3],fixed=TRUE),
  grepl('&lt;unsafe&gt;',html[4],fixed=TRUE),!grepl('<unsafe>',html[4],fixed=TRUE))
 plain<-get_reactable_columns_for_players(data.frame(name='Player'))
 stopifnot(grepl('Player',as.character(plain$name$cell('Player',1)),fixed=TRUE),
  identical(plain$name$cell(NA_character_,1),''))
})
selected_player_Server<-function(...)NULL
recent_player_points_source<-function(...)list(history=function()data.frame(),status=function()NULL)
settle<-function(session){for(i in seq_len(5)){later::run_now(0);session$flushReact()}}
shiny::testServer(players_table_Server,args=list(players_table_RV=shiny::reactive(rows),
 user_teams_RV=shiny::reactive(NULL),points_ratio_basis='value'),{
 session$setInputs(recent_games_n=2L);settle(session)
 display<-table_display_data()
 widget<-jsonlite::fromJSON(output$players_table,simplifyVector=FALSE)$x$tag$attribs
 definitions<-stats::setNames(widget$columns,vapply(widget$columns,`[[`,character(1),'id'))
 visible<-vapply(Filter(function(column)!identical(column$show,FALSE)&&column$id!='.selection',widget$columns),`[[`,character(1),'id')
 check('actual table starts Name, Position, averages and displays no Team column',{
  stopifnot(identical(names(display)[1:4],c('name','role','recent_points_avg','recent_points_ratio')),
   identical(head(visible,4),c('name','role','recent_points_avg','recent_points_ratio')),
   identical(definitions$role$name,'Position'),isFALSE(definitions$team$show),isFALSE(definitions$logo$show),
   identical(display$name,rows$name),identical(display$team,rows$team),identical(display$logo,rows$logo),
   all(display$recent_points_avg==9),all(display$recent_points_ratio==4.5))
 })
})
cat(sprintf('PLAYER NAME CREST: %d passed / 0 failed\n',checks))
