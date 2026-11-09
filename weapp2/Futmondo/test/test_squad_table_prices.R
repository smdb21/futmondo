#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(label,code){force(code);checks<<-checks+1L;cat('[PASS]',label,'\n')}
has<-function(text,expected)grepl(expected,as.character(text),fixed=TRUE)
check('sale-price display uses signed millions with two decimals and preserves unknown costs',{
  stopifnot(has(format_table_sale_price(12e6,9.5e6),'(+2.50 M€)'),
    has(format_table_sale_price(12e6,12.25e6),'(-0.25 M€)'),
    has(format_table_sale_price(12e6,12e6),'(0.00 M€)'),
    has(format_table_sale_price('12000000','9500000'),'(+2.50 M€)'),
    has(format_table_sale_price(12e6,NA_real_),'(P/L unavailable)'),
    identical(format_table_sale_price(NA_real_,1e6),''),
    identical(format_table_sale_price(0,1e6),''),
    !has(format_table_sale_price(9999999,1e7),'-0.00'))
})
roster<-data.frame(id=c('p','q','r'),name=c('Profit','Loss','Unknown'),
  user_team_id='mine',buyPrice=c('9500000','12250000',NA_character_),
  value=1,market_inMarket=TRUE,bid_price=12e6,bid_user='Rival',
  clause_suggestedClause=18e6,stringsAsFactors=FALSE)
listed<-team_selling_prices(roster,data.frame(id=c('p','q','r'),price=12e6))
check('listing and offer cells use roster buyPrice, never valuation or asking price',{
  columns<-get_reactable_columns_for_players(listed,show_sale_profit=TRUE)
  stopifnot(is.numeric(listed$selling_by),is.numeric(listed$bid_price),
    identical(listed$buyPrice,roster$buyPrice),
    has(columns$selling_by$cell(listed$selling_by[1],1),'(+2.50 M€)'),
    has(columns$bid_price$cell(listed$bid_price[2],2),'(-0.25 M€)'),
    has(columns$selling_by$cell(listed$selling_by[3],3),'(P/L unavailable)'),
    has(columns$bid_price$cell(listed$bid_price[3],3),'(P/L unavailable)'),
    has(columns$selling_by$cell(12e6,1),format_table_currency(12e6)),
    has(columns$selling_by$cell(12e6,1),'class="fm-sale-price"'),
    has(columns$bid_price$cell(12e6,1),'class="fm-sale-price"'),
    !has(get_reactable_columns_for_players(listed)$bid_price$cell(12e6,1),'fm-sale-price'),
    !has(get_reactable_columns_for_players(listed)$bid_price$cell(12e6,1),'M€'))
})
offers<-dplyr::bind_rows(normalize_received_offer(list(id='profit',price=12e6),'p'),
  normalize_received_offer(list(id='loss',price=12e6),'q'),
  normalize_received_offer(list(id='unknown',price=12e6),'r'),
  normalize_received_offer(list(id='old-context',price=12e6),'outside'))
attr(roster,'received_offers')<-offers
check('received-offer panel joins each current roster cost by player ID',{
  state<-team_received_offers_state(resource_state(roster))
  html<-team_received_offers_panel(state,identity)
  stopifnot(nrow(state$records)==3L,has(html,'(+2.50 M€)'),has(html,'(-0.25 M€)'),
    has(html,'(P/L unavailable)'),!any(state$records$id=='outside'),
    identical(state$records$acquisition_cost,c(9.5e6,12.25e6,NA_real_)))
})
check('renewal flag helper preserves explicit settings and primary precedence',{
  for(value in list(TRUE,'true','TRUE'))stopifnot(players_table_renewal_disabled(list(info=list(configuration=list(ddt=value)))))
  for(value in list(FALSE,'false',NA,1,'unknown',list(TRUE)))stopifnot(!players_table_renewal_disabled(list(info=list(configuration=list(ddt=value)))))
  stopifnot(!players_table_renewal_disabled(NULL),
    players_table_renewal_disabled(list(league=list(configuration=list(ddt=TRUE)))),
    !players_table_renewal_disabled(list(league=list(configuration=list(ddt=TRUE)),info=list(configuration=list(ddt=FALSE)))))
})

# Test the real table's reactive schema without account/API/database access.
selected_player_Server<-function(...)NULL
recent_player_points_source<-function(...)list(history=function()data.frame(),status=function()NULL)
settings<-shiny::reactiveVal(NULL)
listed_source<-shiny::reactiveVal(listed)
cfg_player_columns_to_hide<-unique(c(cfg_player_columns_to_hide,'buyPrice'))
settle<-function(session)for(i in seq_len(5L)){later::run_now(0);session$flushReact()}
shiny::testServer(players_table_Server,args=list(players_table_RV=listed_source,
  user_teams_RV=shiny::reactive(NULL),own_squad=TRUE,league_settings_source=settings),{
  session$setInputs(recent_games_n=5L);settle(session)
  check('own squad hides internal columns and retains action metadata',{
    stopifnot(!'owner_team_id'%in%names(table_display_data()),
      !'clause_suggestedClause'%in%names(table_display_data()),
      all(players_table_filtered_RV()$owner_team_id=='mine'),
      !'buyPrice'%in%names(table_display_data()),
      identical(table_display_data()$sale_acquisition_cost,c(9.5e6,12.25e6,NA_real_)),
      is.numeric(table_display_data()$selling_by),is.numeric(table_display_data()$bid_price))
  })
  settings(list(info=list(configuration=list(ddt=TRUE))));settle(session)
  check('suggested clause stays hidden regardless of renewal settings',{
    stopifnot(!'clause_suggestedClause'%in%names(table_display_data()),
      !has(output$players_table,'Suggested Clause'),has(output$players_table,'+2.50 M'))
    settings(list(info=list(configuration=list(ddt=FALSE))));settle(session)
    stopifnot(!'clause_suggestedClause'%in%names(table_display_data()),!has(output$players_table,'Suggested Clause'))
  })
  updated<-listed;updated$buyPrice[1]<-'11000000';listed_source(updated);settle(session)
  check('new acquisition costs update both formatted cells while numeric asking prices stay unchanged',{
    stopifnot(table_display_data()$sale_acquisition_cost[1]==11e6,
      table_display_data()$selling_by[1]==12e6,
      has(output$players_table,'+1.00 M'),!has(output$players_table,'+2.50 M'))
  })
})
cat('SQUAD TABLE PRICES:',checks,'passed / 0 failed\n')
