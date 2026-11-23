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
  average.fitness='2,5,12,12,6',average.averageLastFive=7.4,
  status=c('injured','ok',NA,'redcard'),fav=c(TRUE,FALSE,TRUE,FALSE),owner_team_id='mine',clause_suggestedClause=3e6)
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
check('separate crest keeps source/fallback labels and scrolling names without duplicate images',{
 separated<-get_reactable_columns_for_players(rows,separate_crest=TRUE)
 crests<-vapply(seq_len(nrow(rows)),function(i)paste(as.character(separated$team$cell(rows$team[i],i)),collapse=""),character(1))
 stopifnot(grepl('authoritative-club.png',crests[1],fixed=TRUE),
  grepl('alt="Athletic Club"',crests[1],fixed=TRUE),grepl('sevilla.png',crests[2],fixed=TRUE),
  identical(crests[3],''),!grepl('<img',as.character(separated$name$cell('Vivian',1)),fixed=TRUE),
  grepl('&lt;unsafe&gt;',as.character(separated$name$cell('<unsafe>',4)),fixed=TRUE),
  identical(separated$team$sticky,'left'),is.null(separated$role$sticky),is.null(separated$name$sticky),
  isFALSE(separated$team$resizable),isFALSE(separated$team$filterable))
})
check('only actionable or unrecognized reported status appears beside the name',{
  stopifnot(isFALSE(columns$status$show),
    grepl('aria-label="Injured"',html[1],fixed=TRUE),
    grepl('fa-plus',html[1],fixed=TRUE),
    grepl('title="Red card"',html[4],fixed=TRUE),
    grepl('fm-status-card',html[4],fixed=TRUE),
    !any(grepl('fm-player-status|Fit',html[c(2,3)])),
    is.null(player_status_icon(' FIT ')),is.null(player_status_icon('')),
    grepl('Doubtful',as.character(player_status_icon('doubt')),fixed=TRUE),
    grepl('Long-term injury',as.character(player_status_icon('injured2')),fixed=TRUE),
    grepl('Unavailable',as.character(player_status_icon('unavailable')),fixed=TRUE),
    grepl('Status unconfirmed',as.character(player_status_icon('unmapped')),fixed=TRUE))
})
check('card indicators distinguish red, two yellows and generic suspension',{
  sample <- data.frame(name = c("Second Player", "Third Player"), status = c("yellowcard", "suspended"))
  name_cell <- get_reactable_columns_for_players(sample)$name$cell
  yellow <- as.character(name_cell(sample$name[1], 1))
  suspended <- as.character(name_cell(sample$name[2], 2))
  stopifnot(grepl('aria-label="Two yellow cards"', yellow, fixed = TRUE),
    grepl('fm-status-card-pair', yellow, fixed = TRUE),
    grepl('aria-label="Suspended"', suspended, fixed = TRUE),
    !grepl('fm-status-card', suspended, fixed = TRUE))
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
 check('actual table starts pinned crest, scrolling Name, scrolling Position and pinned average',{
  stopifnot(identical(names(display)[1:5],c('team','name','role','recent_points_avg','recent_points_ratio')),
   identical(head(visible,5),c('team','name','role','recent_points_avg','recent_points_ratio')),
   identical(definitions$role$name,'Position'),identical(definitions$team$sticky,'left'),
   identical(definitions$team$width,36L),is.null(definitions$role$sticky),
   identical(definitions$recent_points_avg$sticky,'left'),identical(definitions$recent_points_avg$minWidth,84L),
   is.null(definitions$name$sticky),isFALSE(definitions$logo$show),
   identical(definitions$average.fitness$name,'Latest 5'),
   !any(c('clause_suggestedClause','fav','owner_team_id')%in%names(display)),
   !any(c('clause_suggestedClause','fav','owner_team_id')%in%visible),
   identical(display$average.fitness,rows$average.fitness),
   identical(display$status,rows$status),isFALSE(definitions$status$show),
   identical(display$name,rows$name),identical(display$team,rows$team),identical(display$logo,rows$logo),
   all(display$recent_points_avg==9),all(display$recent_points_ratio==4.5))
 })
 session$setInputs(is_favorite_filter=TRUE);settle(session)
 check('hidden favourite and owner data remain available to filtering and actions',{
  stopifnot(identical(players_table_filtered_RV()$id,c('p1','p3')),
   all(players_table_filtered_RV()$owner_team_id=='mine'),
   all(players_table_filtered_RV()$clause_suggestedClause==3e6),
   identical(table_display_data()$name,rows$name[c(1,3)]))
 })
})
cat(sprintf('PLAYER NAME CREST: %d passed / 0 failed\n',checks))
