#!/usr/bin/env Rscript
# Offline request/row/UI checks for the My Team offers summary.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
record_acquired_resource <- function(...) invisible(TRUE)
get_sb_url <- function() ''
get_sb_key <- function() ''
read_saved_roster_offers <- function(...) NULL
checks<-0L
check<-function(label,code){force(code);checks<<-checks+1L;cat('[PASS]',label,'\n')}
auth<-c(userid='team-offers-fixture',token='synthetic')
bid<-function(id,price,userTeam=NULL)list(id=id,price=price,userTeam=userTeam)
system_bid<-bid('system',500)
rival_bid<-bid('rival',300,list(name='Rival <club>',slug='rival-club'))
second_rival<-bid('second-rival',400,list(id='rival-id',name='Another rival'))
check('source identity distinguishes named/ID rivals from system and malformed bidders',{
  rows<-dplyr::bind_rows(normalize_received_offer(system_bid,'one'),normalize_received_offer(rival_bid,'one'),
    normalize_received_offer(second_rival,'one'),normalize_received_offer(bid('broken',NULL,'unrecognized'),'one'),
    normalize_received_offer(bid('empty',0,list(name='')),'one'))
  stopifnot(identical(rows$bid_source,c('system','rival','rival','unknown','system')),
    rows$bidder_team_id[3]=='rival-id',is.na(rows$bid_price[4]),rows$bid_price[5]==0)
})
requests<-character()
player<-list(id='one',name='Player <one>',role='defensa',value=200,change=0,points=4,
  market=list(inMarket=TRUE,price=900,bids=list(system_bid,rival_bid)))
enrich_player_clubs<-function(players_df,...)players_df
futmondo_post<-function(url,body,...) {
  request<-jsonlite::fromJSON(as.character(body),simplifyVector=FALSE)
  route<-tail(strsplit(url,'/',fixed=TRUE)[[1]],1L);requests<<-c(requests,route)
  answer<-switch(route,roster=list(player),
    rosterbids=list(list(player_id='one',bids=list(system_bid,rival_bid,second_rival))),
    myplayers=list(list(id='one',name='Player <one>',price=1000)),stop('Unexpected fixture route'))
  fm_httr_response(list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=answer),auto_unbox=TRUE)),times=list(total=0)),url)
}
clear_api_cache()
roster<-get_players_from_team(auth,'league','mine')
check('one roster acquisition preserves every offer and legacy highest offer',{
  rows<-attr(roster,'received_offers')
  stopifnot(nrow(roster)==1L,nrow(rows)==3L,roster$bid_id=='system',roster$bid_price==500,
    sum(rows$bid_source=='rival')==2L,identical(sort(requests),sort(c('roster','rosterbids','myplayers'))))
  before<-length(requests);again<-get_players_from_team(auth,'league','mine')
  stopifnot(length(requests)==before,identical(attr(again,'received_offers'),rows))
  raw_stamp<-attr(api_cache_env[[api_cache_key('roster_payload_league_mine',context=resource_context(auth,'league','mine'))]]$data,'observed_at')
  stopifnot(as.numeric(attr(roster,'received_offers_observed_at'))==as.numeric(raw_stamp))
  cat('MEASURE cold_reads=3 additional_offer_panel_reads=0 warm_reads=0\n')
})
check('Selling by is one sortable numeric asking price, distinct from Offer',{
  rows<-team_selling_prices(roster,data.frame(id='one',price=1000))
  stopifnot(is.numeric(rows$selling_by),identical(rows$selling_by,1000),rows$bid_price==500)
  columns<-get_reactable_columns_for_players(rows)
  stopifnot(columns$selling_by$name=='Selling by',columns$bid_price$name=='Offer',
    !columns$market_inMarket$show,!columns$market_price$show,!columns$effective_market_price$show,
    !columns$market_asking_price$show,!columns$bid_source$show)
  stopifnot(is.na(team_selling_prices(rows,data.frame())$selling_by),
    team_selling_prices(rows,observed_data(data.frame(),status='stale'))$selling_by==1000,
    team_selling_prices(rows,observed_data(data.frame(id=character(),price=numeric()),status='stale'))$selling_by==1000,
    team_selling_prices(rows,NULL)$selling_by==1000)
  rows$market_inMarket<-FALSE
  stopifnot(is.na(team_selling_prices(rows)$selling_by),
    is.null(get_reactable_columns_for_players(roster)$selling_by))
})
state<-team_received_offers_state(resource_state(roster))
check('panel includes lower rival bids and explicit icons/text with escaped source labels',{
  html<-as.character(team_received_offers_panel(state,identity))
  stopifnot(grepl('Offers for your players (3)',html,fixed=TRUE),grepl('2 rival offers',html,fixed=TRUE),
    grepl('Futmondo system',html,fixed=TRUE),grepl('Rival · Rival &lt;club&gt;',html,fixed=TRUE),
    grepl('fa-user',html,fixed=TRUE),grepl('fa-robot',html,fixed=TRUE),
    grepl('Player &lt;one&gt;',html,fixed=TRUE),grepl('300',html,fixed=TRUE),grepl('400',html,fixed=TRUE))
})
check('empty loading unavailable stale and partial states do not claim verified zero offers',{
  empty<-received_offers_empty();empty$player_name<-character()
  for(status in c('loading','unavailable','stale','partial')) {
    html<-as.character(team_received_offers_panel(list(status=status,records=empty,error=NULL),identity))
    stopifnot(!grepl('No offers|players \\(0\\)',html))
  }
  fresh<-as.character(team_received_offers_panel(list(status='ok',records=empty,error=NULL),identity))
  stopifnot(grepl('No offers for your players.',fresh,fixed=TRUE))
  stale<-team_received_offers_state(resource_state(observed_data(roster,status='stale')))
  stopifnot(stale$status=='stale',nrow(stale$records)==3L)
  partial<-roster;attr(partial,'received_offers_status')<-'partial'
  stopifnot(team_received_offers_state(resource_state(partial))$status=='partial')
})
check('top panels use equal responsive columns and stable row links',{
  html<-as.character(players_in_teams_UI('team'))
  stopifnot(grepl('fm-team-panels',html,fixed=TRUE),grepl('repeat(2,minmax(0,1fr))',html,fixed=TRUE),
    grepl('@media(max-width:767px)',html,fixed=TRUE),grepl('team-incoming_players',html,fixed=TRUE),
    grepl('team-received_offers',html,fixed=TRUE),
    team_received_offer_input(state$records[1,,drop=FALSE])!=team_received_offer_input(state$records[2,,drop=FALSE]))
})
# Exercise the real shared-resource registry and click handler, with card/history
# modules stubbed so the fixture cannot acquire unrelated API/DB data.
reads<-character();active<-shiny::reactiveVal(TRUE);team<-shiny::reactiveVal('mine');refresh<-shiny::reactiveVal(0L)
async_adapter_fetch<-function(fn,args=list(),context,...) {
  reads<<-c(reads,fn)
  value<-switch(fn,get_players_from_team=roster,
    get_my_market_players=observed_data(data.frame(id='one',price=1000)),stop('Unexpected async fixture resource'))
  promises::promise_resolve(value)
}
incoming_clause_source<-function(...)shiny::reactive(list(status='ok',records=incoming_clause_empty(),error=NULL))
card_selection<-NULL
selected_player_Server<-function(id,selected_player,...){card_selection<<-selected_player;invisible(NULL)}
players_table_Server<-function(...)shiny::reactive(NULL)
settle<-function(session)for(i in seq_len(12L)){later::run_now(0);session$flushReact()}
clear_api_cache()
shiny::testServer(players_in_teams_Server,args=list(is_module_active=active,login_token=shiny::reactive(auth),
  championship_id=shiny::reactive('league'),user_team_id=team,
  user_teams_RV=shiny::reactive(data.frame()),refresh_trigger=refresh),{
  settle(session)
  check('panel and roster share one read and selecting a rival preserves that exact offer',{
    rendered<-output$received_offers$html;output$team_value_box
    stopifnot(sum(reads=='get_players_from_team')==1L,sum(reads=='get_my_market_players')==1L,
      grepl('2 rival offers',rendered,fixed=TRUE))
    row<-offers_state()$records[offers_state()$records$bid_id=='rival',,drop=FALSE]
    do.call(session$setInputs,setNames(list(1L),team_received_offer_input(row)));settle(session)
    selected<-shiny::isolate(card_selection())
    stopifnot(selected$id=='one',selected$bid_id=='rival',selected$bid_price==300,
      selected$user_team_id=='mine',sum(reads=='get_players_from_team')==1L)
  })
  check('changing context immediately clears the retained offer selection',{
    team('another-team');settle(session)
    stopifnot(is.null(shiny::isolate(card_selection())))
  })
})
cat(sprintf('TEAM RECEIVED OFFERS: %d checks passed.\n',checks))
