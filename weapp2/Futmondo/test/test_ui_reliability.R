#!/usr/bin/env Rscript
# Offline regression tests: no credentials, HTTP requests or database writes.
suppressPackageStartupMessages({library(shiny);library(dplyr);library(reactable)})
source('data_contracts.R')
source('Modules/Players_Table_Module.R')
source('Modules/Selected_Player_Module.R')
source('Modules/Players_in_Teams_Module.R')
suppressPackageStartupMessages(source('Modules/Classification_Module.R'))
suppressPackageStartupMessages(source('Modules/Round_MVPs_Module.R'))
suppressPackageStartupMessages(source('Modules/Rivals_Module.R'))
format_table_currency <- function(x) paste0(format(x, scientific=FALSE, trim=TRUE), ' EUR')
record <- function(name, expr) {force(expr); cat('[PASS]', name, '\n')}
record('desktop sidebar reserves room for full navigation labels', {
  ui_source <- paste(readLines('ui.R', warn=FALSE), collapse='\n')
  stopifnot(grepl('width = 180', ui_source, fixed=TRUE))
})
record('all player-card money displays use the shared formatter', {
  source_text <- paste(readLines('Modules/Selected_Player_Module.R', warn=FALSE), collapse='\n')
  stopifnot(!grepl('format_currency\\(', source_text),
            !grepl('format_table_currency\\(', source_text),
            length(gregexpr('player_card_money\\(', source_text)[[1]]) > 20L)
})
record('player card has an accessible top close control', {
  card_html <- htmltools::renderTags(selected_player_UI('fixture'))$html
  stopifnot(grepl('player-card-close-shortcut', card_html, fixed=TRUE),
            grepl('Close player card', card_html, fixed=TRUE),
            grepl('data-dismiss="modal"', card_html, fixed=TRUE))
})
record('player card hides the legacy userBox header', {
  css <- paste(readLines('www/custom_style.css', warn=FALSE), collapse='\n')
  stopifnot(grepl("[id$='selected_player_box'] .widget-user-header", css, fixed=TRUE),
            grepl("[id$='selected_player_box'] .widget-user-image", css, fixed=TRUE))
})
record('player card location distinguishes ownership from market listing', {
  stopifnot(player_card_location_label(data.frame(), 'mine') ==
      'Ownership: Free Agent · Market: Not listed on Market',
    player_card_location_label(data.frame(market_inMarket=TRUE), 'mine') ==
      'Ownership: Free Agent · Market: Listed on Market',
    player_card_location_label(data.frame(user_team_id='rival', userTeam='Rivals FC'), 'mine') ==
      'Ownership: Rival Owned: Rivals FC · Market: Not listed on Market',
    player_card_location_label(data.frame(user_team_id='rival',userTeam='Rivals FC',market_inMarket=TRUE),'mine') ==
      'Ownership: Rival Owned: Rivals FC · Market: Listed on Market',
    player_card_location_label(data.frame(user_team_id='mine',market_inMarket=FALSE,price=999),'mine') ==
      'Ownership: Your Squad · Market: Not listed on Market',
    !player_card_location_status(data.frame(computer=FALSE,market_inMarket=FALSE),'mine')$on_market)
})
record('own player never exposes clause purchase eligibility', {
  own <- data.frame(id='p1',user_team_id=' mine ',clause_price=1000000)
  rival <- data.frame(id='p2',user_team_id='rival',clause_price=1000000)
  stopifnot(player_card_is_own_player(own,'mine'),
    !player_card_can_buy_clause(own,'mine',TRUE),
    player_card_can_buy_clause(rival,'mine',TRUE),
    !player_card_can_buy_clause(rival,'mine',FALSE))
})
record('player card offer money uses stable Spanish euro formatting', {
  stopifnot(player_card_money(11234778) == '11.234.778 €',
            player_card_money(-5000.5) == '-5.000 €',
            player_card_money(NA_real_) == 'Unavailable')
})
record('signed and missing financial values', {
  stopifnot(ui_financial_amount(0)=='0 EUR', ui_financial_amount(-50)=='-50 EUR',
            ui_financial_amount(NA_real_)=='Unavailable',ui_financial_amount(NULL)=='Unavailable')
})
record('rival balance estimates require an explicit league budget', {
  teams <- data.frame(teamid='t1',teamname='Team')
  no_budget <- rivals_buying_power_values(data.frame(), teams)
  stopifnot(is.na(no_budget$value), nrow(rivals_buying_power_values(data.frame(),NULL))==0L)
  stopifnot(rivals_buying_power_values(data.frame(),teams,initial_budget=0)$value==0)
  stopifnot(rivals_buying_power_values(data.frame(),teams,initial_budget=-25)$value== -25)
})
record('restricted rival histories contain only observed transfers', {
  feed <- data.frame(id=c('a','a','b','c'),player_name=c('A','A','B','C'),
    buyer_team_id=c('t1','t1',NA,'t2'),seller_team_id=c(NA,NA,'t1','t1'),
    price=c(100,100,150,99),created=c('2026-08-01T10:00:00Z','2026-08-01T10:00:00Z','2026-08-02T10:00:00Z','invalid'))
  history <- rivals_observed_transfers(feed,'t1')
  stopifnot(nrow(history)==2L,sum(history$money)==50,all(is.na(history$running_balance)),
            setequal(history$type,c('buy','sell')),all(history$date%in%feed$created))
})
record('selected player remains a row and preserves table score', {
  original <- data.frame(id='p1',name='Player',value=2e6,fis_score=73.5,fis_tier='Buy')
  row <- selected_player_fis_row(original)
  stopifnot(nrow(row)==1L,row$id=='p1',row$value==2e6,row$fis_score==73.5,is.na(row$perf))
})
record('round filters change standings without replacing unavailable scores', {
  catalog <- data.frame(round_id=c('r1','r2'),round_number=1:2)
  a <- classification_round_rows(list(list(round='r1',points=10),list(round='r2',points=0)), 'a','Alpha',catalog)
  b <- classification_round_rows(list(list(number=1,points=2),list(number=2,points=20)), 'b','Beta',catalog)
  all <- rbind(a,b)
  first <- classification_window_table(all,c(1,2),'1')
  second <- classification_window_table(all,c(2,2),'all')
  stopifnot(first$team_id[1]=='a',second$team_id[1]=='b',sum(second$points)==20)
  a$points[2] <- NA_real_
  incomplete <- classification_window_table(a,c(1,2),'all')
  stopifnot(is.na(incomplete$points),nrow(classification_round_rows(list(),'a','Alpha'))==0)
  missing_round <- classification_window_table(all[-2, ], c(1, 2), 'all')
  stopifnot(is.na(missing_round$points[missing_round$team_id=='a']),
            missing_round$points[missing_round$team_id=='b']==22)
  missing_team <- classification_window_table(all[-2, ], c(2, 2), 'all')
  stopifnot(nrow(missing_team)==2L,is.na(missing_team$rank[missing_team$team_id=='a']))
})
record('round MVP records keep completed MVPs in newest-first order', {
  rows <- data.frame(championship_id=c('league','league','league','other'), round_number=c(2,1,2,9),
    player_id=c('p2','p1','not-mvp','other'), player_name=c('Latest','Earlier','Other','Wrong league'),
    player_role=c('MID','GK','DEF','FWD'), points=c(12,8,99,30),
    is_mvp=c(TRUE,'true',FALSE,TRUE), is_finished=c(TRUE,'1',TRUE,TRUE))
  out <- round_mvp_rows(rows, 'league')
  stopifnot(identical(out$player_id,c('p2','p1')), identical(out$round_number,c(2,1)),
            nrow(round_mvp_rows(rows[rows$is_finished %in% FALSE,,drop=FALSE], 'league')) == 0L)
})
record('round MVP UI has dedicated responsive card output', {
  html <- htmltools::renderTags(round_mvps_UI('mvp'))$html
  stopifnot(grepl('mvp-mvp_cards', html, fixed=TRUE), grepl('Round MVPs', html, fixed=TRUE))
  server_source <- paste(readLines('server.R', warn=FALSE), collapse='\n')
  stopifnot(grepl('tabName = "round_mvps"', server_source, fixed=TRUE),
            grepl('menuItem("Round MVPs"', server_source, fixed=TRUE))
})
record('dream team preserves actual MVP and missing scores', {
  d <- classification_dreamteam_rows(list(mvp='p1',players=list(list(id='p1',name='A',role=1,points=0),list(id='p2',name='B',role=2))))
  stopifnot(d$mvp[1],!d$mvp[2],d$points[1]==0,is.na(d$points[2]))
  alternate <- classification_dreamteam_rows(list(mvp=list(`_id`='p1'),players=list(list(`_id`='p1',name='A',points=0))))
  stopifnot(alternate$player_id=='p1',alternate$mvp,alternate$points==0)
})
record('submitted lineup comparison and explicit invalid state', {
  d <- squad_lineup_comparison(list(players=data.frame(id=c('p1','p2'))),list(feasible=TRUE,starting_xi=data.frame(id=c('p2','p3'))))
  stopifnot(d$status=='ok',identical(d$add,'p3'),identical(d$remove,'p1'))
  stopifnot(squad_lineup_comparison(NULL,list())$status=='unavailable')
  stopifnot(squad_lineup_comparison(list(players=data.frame(id='p1')),list(feasible=FALSE))$status=='invalid')
})
record('lineup display preserves assignment, captain, ordered reserves and missing forecasts', {
  players <- data.frame(id=c('p1','p2'),name=c('A','B'),role=c('MID','GK'),pos_group=c('DEF','GK'),
                        expected_points=c(0,NA_real_),forecast_lower=c(-1,NA_real_),forecast_upper=c(3,NA_real_),
                        value=c(1e6,2e6),private_raw='never render')
  xi <- squad_lineup_display_rows(players,captain_id='p1',starting=TRUE)
  stopifnot(xi$position[1]=='DEF',xi$selection[1]=='Captain',xi$expected_points[1]==0,
            is.na(xi$expected_points[2]),!('private_raw'%in%names(xi)))
  reserves <- squad_lineup_display_rows(players,bench_order='p2')
  stopifnot(reserves$selection[1]=='Reserve',reserves$selection[2]=='Bench 1')
})
# Stub unrelated rendering and child modules. The parent reactive is the API
# snapshot: after refresh it supplies the actual listing/bid state.
selected_player_Server <- function(...) NULL
selected_player_UI <- function(...) NULL
cfg_player_columns_to_hide <- character()
reorder_player_table_columns <- function(x) x
get_reactable_columns_for_players <- function(x) list()
record('all players clause filters respect maximum and verified funds', {
  players <- reactive(data.frame(
    id=c('low','equal','high'), name=c('Low','Equal','High'), role='MID', role2=NA_character_,
    value=1000000, clause_price=c(1000000,2000000,3000000), isClause=TRUE,
    clause_date='', fis_score=60, stringsAsFactors=FALSE
  ))
  testServer(players_table_Server, args=list(
    players_table_RV=players, user_teams_RV=reactive(NULL), available_funds_RV=reactive(2000000)
  ), {
    session$setInputs(max_clause_value_filter=2, clause_under_available_funds_filter=FALSE)
    stopifnot(identical(players_table_filtered_RV()$id, c('low','equal')))
    session$setInputs(max_clause_value_filter=1000, clause_under_available_funds_filter=TRUE)
    stopifnot(identical(players_table_filtered_RV()$id, c('low','equal')))
  })
})
record('listing and rejected offers refresh authoritative data', {
  trigger <- reactiveVal(0L)
  authoritative <- reactive({
    n <- trigger()
    data.frame(id='p1',name='Player',role='DF',role2=NA_character_,userTeam='My team',value=1e6,
               fis_score=60,bid_price=NA_real_,numberOfBids=if(n==0L) 0 else 2,market_inMarket=n>0L)
  })
  testServer(players_table_Server,args=list(players_table_RV=authoritative,user_teams_RV=reactive(NULL),refresh_trigger=trigger),{
    handle_bid_updated(player_id='p1',new_bid_price=1.5e6,action_type='player_listed')
    d <- players_table_filtered_RV()
    stopifnot(trigger()==1L,isTRUE(d$market_inMarket),is.na(d$bid_price),d$numberOfBids==2)
    handle_bid_updated(player_id='p1',is_cancel=TRUE,action_type='offer_rejected')
    d <- players_table_filtered_RV()
    stopifnot(trigger()==2L,isTRUE(d$market_inMarket),is.na(d$bid_price),d$numberOfBids==2)
  })
})
record('selected player identity survives authoritative table reordering', {
  trigger <- reactiveVal(0L)
  real_get_state <- getReactableState
  getReactableState <- function(outputId, name, session) session$input$fixture_selection
  authoritative <- reactive({
    rows <- data.frame(id=c('p1','p2'),name=c('Selected','Other'),role='DF',role2=NA_character_,
      userTeam='My team',value=c(1e6,2e6),fis_score=c(60,70),bid_price=NA_real_,numberOfBids=0,market_inMarket=FALSE)
    if (trigger()>0) rows <- rows[2:1,,drop=FALSE]
    rows
  })
  testServer(players_table_Server,args=list(players_table_RV=authoritative,user_teams_RV=reactive(NULL),refresh_trigger=trigger), {
    session$setInputs(fixture_selection=1L)
    session$flushReact()
    stopifnot(selected_player_RV()$id=='p1',player_selection_event_RV()==1L)
    session$setInputs(fixture_selection=NA_integer_)
    session$setInputs(fixture_selection=1L)
    session$flushReact()
    stopifnot(selected_player_RV()$id=='p1',player_selection_event_RV()==2L)
    handle_bid_updated(player_id='p1',action_type='bid_modified')
    session$flushReact()
    stopifnot(players_table_filtered_RV()$id[1]=='p2',selected_player_RV()$id=='p1')
  })
  getReactableState <- real_get_state
})
record('bid cancellation sends the selected ID and explicit action', {
  tree <- parse('Modules/Selected_Player_Module.R')
  find_event <- function(x) {
    if (is.call(x) && identical(x[[1]],as.name('observeEvent')) && identical(paste(deparse(x[[2]]),collapse=''),'input$submit_cancel_bid')) return(x[[3]])
    if (is.call(x)||is.expression(x)||is.pairlist(x)) for(y in as.list(x)) {if(missing(y)) next; r<-find_event(y);if(!is.null(r)) return(r)}
    NULL
  }
  body <- find_event(tree);stopifnot(!is.null(body))
  env <- new.env(parent=globalenv());events<-list()
  env$selected_player <- function() list(id='selected-player',name='Selected')
  env$login_token <- 'fake';env$championship_id<-'champ';env$user_team_id<-'team'
  env$get_reactive_val <- identity
  env$active_bid_info_RV <- function() list(id='own-bid',price=10)
  env$consume_action_context <- function(action) identical(action,'cancel')
  env$cancel_bid <- function(...) TRUE
  env$removeModal <- function(...) NULL
  env$clear_api_cache <- function(...) NULL
  env$on_bid_updated <- function(...) events[[length(events)+1L]] <<- list(...)
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session,eval(body,env))
  session$close()
  stopifnot(length(events)==1L,events[[1]]$player_id=='selected-player',events[[1]]$action_type=='bid_cancelled')
})
record('bulk listing handles unverified API outcomes without local roster mutation', {
  find_event <- function(x) {
    if (is.call(x) && identical(x[[1]], as.name('observeEvent')) &&
        identical(paste(deparse(x[[2]]), collapse=''), 'input$submit_put_all_on_market')) return(x[[3]])
    if (is.call(x) || is.expression(x) || is.pairlist(x)) for (y in as.list(x)) {
      if (missing(y)) next
      result <- find_event(y)
      if (!is.null(result)) return(result)
    }
    NULL
  }
  handler <- find_event(parse('Modules/Players_in_Teams_Module.R'))
  env <- new.env(parent=globalenv())
  env$login_token <- 'fixture'; env$championship_id <- 'champ'; env$user_team_id <- 'team'
  env$get_reactive_val <- identity
  env$input <- list(bulk_listing_price_mode='value', bulk_listing_clause_premium=5)
  env$players_table_RV <- function(...) data.frame(id='p1', value=1000000, market_inMarket=FALSE)
  calls <- 0L
  env$put_player_on_market <- function(...) { calls <<- calls + 1L; stop('unverified transport failure') }
  env$removeModal <- function(...) NULL
  env$refresh_trigger <- function(...) stop('must not refresh as successful mutation')
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, eval(handler, env))
  session$close()
  stopifnot(calls == 1L)
})
record('classification UI renders all active controls', {
  html <- as.character(classification_UI('class'))
  stopifnot(grepl('class-round_range_slider',html,fixed=TRUE),grepl('class-single_round_select',html,fixed=TRUE))
})
record('scenario save scopes data and restore retains stale IDs for validation', {
  locate <- function(x, target) {
    if (is.call(x) && identical(x[[1]],as.name('observeEvent')) && identical(paste(deparse(x[[2]]),collapse=''),target)) return(x[[3]])
    if (is.call(x)||is.expression(x)||is.pairlist(x)) for(y in as.list(x)) {if(missing(y)) next;r<-locate(y,target);if(!is.null(r)) return(r)}
    NULL
  }
  tree <- parse('Modules/Players_in_Teams_Module.R'); env <- new.env(parent=globalenv())
  env$input <- list(scenario_name='Round plan',sandbox_sells='old-player',sandbox_buys='new-target',opt_formation='auto',opt_mode='max_fis',scenario_saved='s1')
  env$login_token <- function() c(userid='fixture-user');env$championship_id<-function() 'fixture-league';env$user_team_id<-function() 'fixture-team'
  saved <- NULL;updates <- list();version<-0L
  env$scenario_version <- function(value) {if(!missing(value)) version<<-value;version}
  env$save_transfer_scenario <- function(...) {saved<<-list(...);TRUE}
  env$showNotification <- function(...) NULL
  eval(locate(tree,'input$scenario_save'),env)
  stopifnot(identical(saved[1:4],list('fixture-user','fixture-league','fixture-team','Round plan')),version==1L)
  payload <- saved[[5]];stopifnot(payload$sell_ids=='old-player',payload$buy_ids=='new-target')
  env$saved_scenarios_RV <- function() data.frame(id='s1',name='Round plan',scenario=I(list(payload)))
  env$players_table_RV <- function() data.frame(id='existing',name='Existing')
  env$market_players_RV <- function() data.frame(id='other-target',name='Other target')
  env$session <- shiny::MockShinySession$new()
  env$updateSelectizeInput <- function(session,inputId,choices,selected) updates[[inputId]] <<- list(choices=choices,selected=selected)
  env$updateSelectInput <- function(...) NULL
  eval(locate(tree,'input$scenario_load'),env)
  env$session$close()
  stopifnot('old-player'%in%updates$sandbox_sells$choices,updates$sandbox_sells$selected=='old-player',
            'new-target'%in%updates$sandbox_buys$choices,updates$sandbox_buys$selected=='new-target')
})
cat('UI reliability: 14 checks passed.\n')
