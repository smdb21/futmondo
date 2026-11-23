#!/usr/bin/env Rscript
# Actual catalog join -> table selection -> card, with delayed optional reads.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
clock<-as.POSIXct('2026-11-16 03:00:00',tz='UTC');Sys.time<-function()clock
get_sb_url<-function()'';get_sb_key<-function()''
fm_worker_configured<-function()FALSE
record_acquired_resource<-function(...)invisible(TRUE)
auth<-c(userid='selection-regression',token='synthetic')
rows<-function(age=120,id='dani-fixture'){
 catalog<-observed_data(data.frame(id=id,name='Dani fixture',role='centrocampista',role2=NA_character_,
   team='Club',value=1000,change=0,points=-2,user_team_id='',photo='',computer=TRUE),observed_at=clock-age)
 market<-observed_data(data.frame(id=id,price=1000),observed_at=clock)
 unify_columns(calculate_player_changes(translate_player_positions(championship_market_prices(catalog,resource_state(market)))))
}
source_rows<-shiny::reactiveVal(rows());league<-shiny::reactiveVal('league')
getReactableState<-function(outputId,name,session)switch(name,sorted=list(),page=1L,pageSize=10L,selected=session$input$fixture_selected,NULL)
updateReactable<-function(...)invisible(NULL)
reads<-character();summaries<-list();financial<-list();private_writes<-0L
futmondo_post<-function(...)stop('Unexpected synchronous source request')
supabase_get<-function(...)stop('Unexpected database request')
buy_clause<-function(...){private_writes<<-private_writes+1L;stop('No transaction is authorized')}
async_adapter_fetch<-function(fn,args=list(),context,is_current=function()TRUE,...){
 reads<<-c(reads,fn)
 if(fn=='get_player_summary')return(promises::promise(function(resolve,reject){
  summaries[[length(summaries)+1L]]<<-list(resolve=resolve,reject=reject,current=is_current,context=context)
 }))
 if(fn=='get_team_acquisition_capacity')return(promises::promise(function(resolve,reject){
  financial[[length(financial)+1L]]<<-list(resolve=resolve,reject=reject,current=is_current,context=context)
 }))
 if(fn%in%c('get_lineup_from_team','get_player_full_profile','get_player_matches','read_card_round_history','read_card_price_history'))
  return(promises::promise(function(resolve,reject)NULL))
 stop(paste('Unexpected fixture resource',fn))
}
settle<-function(session)for(i in 1:30){later::run_now(0);session$flushReact()}
blocked<-function(html,id){tag<-regmatches(html,regexpr(paste0('<button[^>]*id="[^"]*',id,'"[^>]*>'),html));!length(tag)||grepl('disabled',tag,fixed=TRUE)}
summary<-function(id,owner='rival',price=NULL)observed_data(list(data=list(id=id),
 championship=list(owner=list(`_id`=owner,name='Verified owner',computer=FALSE),
 clause=list(price=1500,transferred=FALSE,date='2026-11-15T00:00:00Z')),
 market=if(is.null(price))list(bids=list())else list(p=price,pr=FALSE,bids=list()),points=list(),prices=list()),observed_at=clock)
clear_api_cache()
shiny::testServer(players_table_Server,args=list(players_table_RV=source_rows,user_teams_RV=shiny::reactive(NULL),
 login_token=shiny::reactive(auth),championship_id=league,user_team_id=shiny::reactive('mine'),optional_ready=function()FALSE),{
 settle(session);started<-proc.time()[['elapsed']]
 session$setInputs(fixture_selected=1L);settle(session)
 html<-output[['selected_player-action_buttons']]$html
 stopifnot(!blocked(html,'btn_bid_market'),!grepl('Checking availability|Retry availability',html),
  !length(financial),length(summaries)==1L,
  player_card_recent_table_facts(selected_player_RV()),
  as.numeric(attr(selected_player_RV(),'availability_owner_valid_until'))==as.numeric(clock)+180,
  identical(attr(selected_player_RV(),'availability_market_valid_until'),as.numeric(clock)+20))
 cat(sprintf('PASS actual two-minute catalog -> joined market -> filtered table -> card: ready in %.0fms with zero financial reads while all history is held\n',1000*(proc.time()[['elapsed']]-started)))
 heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,0)
 clock<<-clock+32;settle(session)
 session$setInputs(card_closed=1L);settle(session);session$setInputs(fixture_selected=1L);settle(session)
 html<-output[['selected_player-action_buttons']]$html
 stopifnot(heartbeat,!blocked(html,'btn_bid_market'),!length(financial),length(summaries)==1L,
  !grepl('Checking availability|Retry availability',html))
 cat('PASS 32s held history and reopening after market expiry add zero availability/financial requests and preserve responsive controls\n')
 # An actually expired catalog asks only for this target's authoritative facts.
 session$setInputs(card_closed=2L);settle(session);source_rows(rows(301,'stale-target'));settle(session)
 session$setInputs(fixture_selected=1L);settle(session)
 stopifnot(length(summaries)==2L,!length(financial),!blocked(output[['selected_player-action_buttons']]$html,'btn_bid_market'))
 before<-length(reads);summaries[[2]]$resolve(summary('stale-target'));settle(session)
 html<-output[['selected_player-action_buttons']]$html
 stopifnot(!blocked(html,'btn_offer_owner'),!grepl('Checking availability|Retry availability',html),
  grepl('Verified owner',html,fixed=TRUE),!grepl('Ownership: Free Agent',html,fixed=TRUE),!length(financial))
 cat('PASS an expired catalog leaves the offer editor reachable while only target summary loads; current rival ownership and off-market status replace stale table facts\n')
 # A previous player's completion cannot change the current target.
 summaries[[1]]$resolve(summary('dani-fixture',price=9999));settle(session)
 stopifnot(!blocked(output[['selected_player-action_buttons']]$html,'btn_offer_owner'),!length(financial))
 # A new league must not inherit a previous session-target marker.
 session$setInputs(card_closed=3L);settle(session);league('other');settle(session)
 session$setInputs(fixture_selected=1L);settle(session)
 stopifnot(length(summaries)==3L,!length(financial),
  blocked(output[['selected_player-action_buttons']]$html,'btn_offer_owner'),
  !blocked(output[['selected_player-action_buttons']]$html,'btn_bid_market'))
 summaries[[3]]$reject(resource_failure(category='network'));settle(session)
 stopifnot(!blocked(output[['selected_player-action_buttons']]$html,'btn_verify_availability'),!length(financial),private_writes==0L)
 cat('PASS target session reuse is league-isolated and failed retrieval remains actionable without financial/history acquisition\n')
})
# A newer actual summary must update even a still-fresh table; an old summary
# must not overwrite a newer joined market observation.
source_rows(rows(120,'newer-target'));summaries<-list();financial<-list()
shiny::testServer(selected_player_Server,args=list(selected_player=shiny::reactive(source_rows()),
 login_token=shiny::reactive(auth),championship_id=shiny::reactive('league'),user_team_id=shiny::reactive('mine')),{
 settle(session);stopifnot(!blocked(output$action_buttons$html,'btn_bid_market'),!length(financial))
 clock<<-clock+1;summaries[[1]]$resolve(summary('newer-target'));settle(session)
 stopifnot(!blocked(output$action_buttons$html,'btn_offer_owner'),!length(financial),
  !grepl('Listed on Market',output$action_buttons$html,fixed=TRUE))
 cat('PASS a newer authoritative target summary replaces fresh-but-older table facts without financial acquisition\n')
})
# Explicit source deadlines, including stricter durable deadlines, win over TTLs.
for(which in c('owner','market')){
 value<-rows();attr(value,paste0('availability_',which,'_valid_until'))<-as.numeric(clock)-1
 stopifnot(!player_card_recent_table_facts(value))
}
# Authoritative captured summaries exercise the field mapping, without replaying requests.
entries<-jsonlite::fromJSON('app.futmondo.com.clause_exercise.har',simplifyVector=FALSE)$log$entries
fixtures<-Filter(function(e)endsWith(e$request$url,'/summary'),entries)
replayed<-0L
for(e in fixtures){
 if(is.null(e$response$content$text))next
 answer<-tryCatch(jsonlite::fromJSON(e$response$content$text,simplifyVector=FALSE)$answer,error=function(e)NULL)
 if(!is.list(answer)||is.null(answer$data))next
 raw<-rows(301,fm_scalar(answer$data$id %||% answer$data$`_id`,''))
 current<-player_card_summary_row(raw,observed_data(answer))
 if(is.null(current))next
 listed<-any(c('pr','p','vom','pom')%in%names(answer$market))
 stopifnot(identical(current$market_inMarket,listed),
  identical(current$points,raw$points),
  identical(current$user_team_id,if(isTRUE(answer$championship$owner$computer))''else fm_scalar(answer$championship$owner$`_id` %||% answer$championship$owner$id,'')))
 if(listed)stopifnot(current$effective_market_price==as.numeric(answer$market$p))
 replayed<-replayed+1L
}
stopifnot(replayed>0L)
cat(sprintf('PASS %d captured summaries preserve exact ownership/market mapping and existing signed points; explicit earlier source deadlines reject reuse\n',replayed))

market_rows<-player_availability_market_rows(observed_data(data.frame(id='market-target',user_team_id='',price=1000),observed_at=clock))
stopifnot(player_card_recent_table_facts(market_rows),
 identical(attr(market_rows,'availability_market_valid_until'),as.numeric(clock)+20),
 !player_card_recent_table_facts(market_rows,clock+21))
attr(market_rows,'valid_until')<-as.numeric(clock)+10
market_rows<-player_availability_market_rows(market_rows)
stopifnot(!player_card_recent_table_facts(market_rows,clock+11))
cat('PASS Market/Today endpoint rows retain their actual listing proof and original source deadline, including a stricter persisted deadline\n')

large<-observed_data(data.frame(id=paste0('p',1:527),user_team_id='',price=1000),observed_at=clock)
elapsed<-system.time(for(i in 1:100)player_availability_market_rows(large))[['elapsed']]
stopifnot(!'market_inMarket'%in%names(large))
cat(sprintf('MEASURE 100 x 527-row market projections %.3fs; original source unmodified; zero requests\n',elapsed))

column_row<-data.frame(id='column-clock',user_team_id='',market_inMarket=TRUE,observed_at=clock)
stopifnot(player_card_recent_table_facts(column_row),!player_card_recent_table_facts(column_row,clock+61))
cat('PASS a selected row carrying its original clock as a column retains the conservative native deadline\n')

# Minimized contract from Dani Lorenzo's persisted 04:19 UTC summary: owner
# is absent and the explicit System flag is on data, not championship.owner.
# Synthetic identity/price avoid retaining the private source body in tests.
computer_summary<-observed_data(list(data=list(id='computer-target',computer=TRUE),
 championship=list(pro=FALSE,clause=list()),market=list(pr=FALSE,p=123456,bids=list())))
computer_row<-data.frame(id='computer-target',user_team_id='old-owner',market_inMarket=TRUE,price=1000,points=-2)
projected<-player_card_summary_row(computer_row,computer_summary)
stopifnot(!is.null(projected),identical(projected$user_team_id,''),projected$market_inMarket,
 projected$effective_market_price==123456,projected$points==-2)
for(flag in list(NULL,FALSE,NA,'true')){
 invalid<-computer_summary;invalid$data$computer<-flag
 stopifnot(is.null(player_card_summary_row(computer_row,invalid)))
}
wrong<-computer_summary;wrong$data$id<-'other-target'
stopifnot(is.null(player_card_summary_row(computer_row,wrong)))
cat('PASS missing championship.owner accepts only explicit data.computer=TRUE with matching player ID; unknown ownership stays unverified\n')
