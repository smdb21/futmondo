#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
source('test/fixtures/classification_cold_source.R')
clear_api_cache()
shiny::testServer(fixture_server,{
  messages <- list();root <- session$rootScope()
  root$sendInputMessage <- function(inputId,message)messages[[sub('^classification-','',inputId)]] <<- message
  html <- function(name)output[[paste0('classification-',name)]]$html
  table <- function()output[['classification-classification_table']]
  session$setInputs('classification-single_round_select'='all','classification-round_range_slider'=c(1,38))
  fixture_settle(session)
  stopifnot(grepl('Loading official round standings',html('classification_status'),fixed=TRUE),
    grepl('Loading league settings',html('configured_rewards_ui'),fixed=TRUE))
  heartbeat <- FALSE;later::later(function()heartbeat <<- TRUE,0);fixture_settle(session)
  stopifnot(heartbeat,fixture_reads(ROUNDS_URL)==0L)
  # Full native catalog parser, including unordered calendar numbers and a next-round marker.
  fixture_reply(ACTIVE_CHAMPIONSHIPS_URL,fixture$active);fixture_settle(session)
  fixture_reply('https://api.futmondo.com/2/league/list',fixture$leagues);fixture_settle(session)
  fixture_reply('https://api.futmondo.com/2/league/matches',fixture$calendar);fixture_settle(session)
  stopifnot(identical(as.numeric(messages$round_range_slider$value),c(12,12)),
    grepl('Round 12',messages$single_round_select$options,fixed=TRUE),
    !grepl('Round 13',messages$single_round_select$options,fixed=TRUE),
    fixture_reads('https://api.futmondo.com/1/ranking/round')==1L,
    tail(fixture$requests,1)[[1]]$query$roundNumber==12)
  session$setInputs('classification-round_range_slider'=c(12,12))
  fixture_reply('https://api.futmondo.com/1/ranking/round',fixture$ranking);fixture_settle(session)
  # Directory, every team history, settings and finance are all still pending.
  live_table <- table()
  stopifnot(grepl('Alpha',live_table,fixed=TRUE),grepl('26',live_table,fixed=TRUE),
    grepl('provisional',html('classification_status'),fixed=TRUE),fixture$writes==1L,
    fixture_reads(ROUNDS_URL)==0L,grepl('Loading league settings',html('configured_rewards_ui'),fixed=TRUE))
  # General cannot accidentally display the live-only 26 as cumulative points.
  session$setInputs('classification-general_classification'=1,'classification-round_range_slider'=c(1,12))
  fixture_settle(session)
  stopifnot(grepl('Loading official cumulative standings',html('classification_status'),fixed=TRUE),
    !grepl('Alpha',table(),fixed=TRUE))
  session$setInputs('classification-latest_round'=1,'classification-round_range_slider'=c(12,12))
  fixture_settle(session)
  # A real directory failure and an independent settings failure preserve live scores.
  fixture_reply(TEAMS_URL,error=resource_failure(resource=TEAMS_URL,category='schema'))
  fixture_reply('https://api.futmondo.com/1/userteam/information',error=resource_failure(category='schema'))
  fixture$finance('failed');fixture_settle(session)
  stopifnot(grepl('Alpha',table(),fixed=TRUE),grepl('26',table(),fixed=TRUE),
    grepl('unavailable',html('classification_status'),fixed=TRUE),
    grepl('unavailable',html('configured_rewards_ui'),fixed=TRUE),
    fixture_reads('https://api.futmondo.com/1/ranking/round')==1L)
})
cat('PASS real bound cold path: native catalog initializes12 before directory; live26 survives directory/settings/finance failure; General waits; one ranking read/write\n')

# A new consuming session reuses process catalog/ranking snapshots. Recovering the
# directory starts only its team histories; pending history cannot hide live rows.
shiny::testServer(fixture_server,{
  session$setInputs('classification-single_round_select'='all','classification-round_range_slider'=c(12,12))
  fixture_settle(session)
  fixture_reply(TEAMS_URL,list(teams=list(list(id='a',name='Alpha',points=100),list(id='b',name='Beta',points=90))))
  fixture_reply('https://api.futmondo.com/1/userteam/information',list(configuration=list(moneyPerPoint=70000,mvpPlayer=0)))
  fixture_settle(session)
  status <- output[['classification-classification_status']]$html
  stopifnot(grepl('26',output[['classification-classification_table']],fixed=TRUE),
    grepl('Historical round scores are loading',status,fixed=TRUE),
    grepl('70.000',output[['classification-configured_rewards_ui']]$html,fixed=TRUE),
    fixture_reads(ROUNDS_URL)==2L,fixture_reads('https://api.futmondo.com/1/ranking/round')==1L,fixture$writes==1L)
  session$setInputs('classification-general_classification'=1,'classification-round_range_slider'=c(1,12))
  fixture_settle(session)
  stopifnot(grepl('100',output[['classification-classification_table']],fixed=TRUE))
  session$setInputs('classification-round_range_slider'=c(11,11))
  fixture_reply(paste0(ROUNDS_URL,':a'),error=resource_failure(category='schema'))
  fixture_reply(paste0(ROUNDS_URL,':b'),list(list(round='r11',points=9)))
  fixture_settle(session)
  status <- output[['classification-classification_status']]$html
  stopifnot(grepl('unavailable',status,fixed=TRUE),grepl('missing',status,fixed=TRUE),
    !grepl('26',output[['classification-classification_table']],fixed=TRUE))
})
cat('PASS independent recovery: pending histories retain live26, General100 uses official cumulative source, historical failure stays unknown, warm ranking reuse\n')

# Legacy compact catalogs may lack the newer lineup marker and status column.
# Calendar hydration supplies playing12; a persisted JSON ranking is a data frame.
clear_api_cache()
legacy <- list(status='partial',identity_complete=TRUE,complete=FALSE,league_id='competition',
  rounds=data.frame(round_id=paste0('r',1:38),round_number=as.numeric(1:38),is_finished=c(rep(TRUE,11),rep(NA,27))))
invisible(resource_store(api_cache_key('mvp_catalog_league',context=resource_context(fixture$auth,'league')),
  legacy,resource_context(fixture$auth,'league'),'mvp_catalog_league'))
invisible(resource_store(api_cache_key('award_round_metadata_league',context=resource_context(fixture$auth,'league')),
  list(status='ok',rows=data.frame(round_id=paste0('r',1:38),round_number=as.numeric(1:38),
    is_finished=c(rep(TRUE,11),FALSE,rep(NA,26)),status=c(rep('done',11),'playing',rep('future',26)))),
  resource_context(fixture$auth,'league'),'award_round_metadata_league'))
# Read back through JSON's default simplifyVector behavior, as database reads do.
fixture$saved[[1]]$payload <- jsonlite::fromJSON(jsonlite::toJSON(fixture$saved[[1]]$payload,auto_unbox=TRUE,null='null',na='null'))
shiny::testServer(fixture_server,{
  messages<-list();root<-session$rootScope();root$sendInputMessage<-function(inputId,message)messages[[sub('^classification-','',inputId)]]<<-message
  session$setInputs('classification-single_round_select'='all','classification-round_range_slider'=c(12,12))
  fixture_settle(session)
  stopifnot(identical(as.numeric(messages$round_range_slider$value),c(12,12)),
    grepl('26',output[['classification-classification_table']],fixed=TRUE),
    fixture_reads('https://api.futmondo.com/1/ranking/round')==1L,fixture$writes==1L)
})
cat('PASS legacy catalog/calendar and persisted JSON ranking: recovered playing12 and26 without another ranking request/write\n')

# A directory-shaped legacy cache without cumulative points cannot turn live26
# into General, even though team identities are already present.
shiny::testServer(classification_Server,args=list(is_module_active=function()TRUE,
  login_token=function()fixture$auth,championship_id=function()'league',user_team_id=function()'a',
  user_teams_RV=function()data.frame(teamid='a',teamname='Alpha')) ,{
  session$setInputs(single_round_select='all',round_range_slider=c(1,12),general_classification=1)
  fixture_settle(session)
  stopifnot(grepl('unavailable',output$classification_status$html,fixed=TRUE),
    !grepl('26',output$classification_table,fixed=TRUE))
})
cat('PASS legacy directory without cumulative points never labels live-only scores General\n')

# A failed catalog is unavailable even while the independent directory/settings
# requests remain pending; their loading states must not conceal that failure.
clear_api_cache()
shiny::testServer(fixture_server,{
  session$setInputs('classification-single_round_select'='all','classification-round_range_slider'=c(1,38))
  fixture_settle(session)
  fixture_reply(ACTIVE_CHAMPIONSHIPS_URL,error=resource_failure(category='schema'))
  fixture_settle(session)
  stopifnot(grepl('unavailable',output[['classification-classification_status']]$html,fixed=TRUE),
    !grepl('Loading official round standings',output[['classification-classification_status']]$html,fixed=TRUE),
    grepl('Loading league settings',output[['classification-configured_rewards_ui']]$html,fixed=TRUE))
})
cat('PASS catalog failure remains explicit while directory/settings continue loading; cold heartbeat remains responsive\n')
