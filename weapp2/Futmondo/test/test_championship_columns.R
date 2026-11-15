#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(label,expr){force(expr);checks<<-checks+1L;cat('PASS',label,'\n')}
stamp<-Sys.time()
catalog<-data.frame(id=c('000123','pending','unlisted'),name=c('Listed','Pending','Unlisted'),
  role='defensa',value=c(100,200,300),change=0,points=c(-2,0,1),clause_price=1000,
  clause_transferred=c(FALSE,TRUE,FALSE),clause_date=NA_character_,
  clause_transferDate=c(NA,'2026-11-20T10:00:00Z',NA),stringsAsFactors=FALSE)
market<-observed_data(data.frame(id=c('outside','000123'),price=c(500,125)),observed_at=stamp)
state<-resource_state(market)
check('ID join preserves catalog rows, values and actual asking prices',{
 rows<-championship_market_prices(catalog,state)
 stopifnot(identical(rows$id,catalog$id),identical(rows$value,catalog$value),
  identical(rows$effective_market_price,c(125,NA_real_,NA_real_)),
  identical(rows$market_inMarket,c(TRUE,FALSE,FALSE)),identical(rows$clause_transferDate,catalog$clause_transferDate))
})
check('loading, failed, partial and stale results cannot claim a player is not listed',{
 for(s in list(resource_state(status='loading'),resource_state(status='unavailable'),
   resource_state(observed_data(data.frame(),status='stale')))) {
  rows<-championship_market_prices(catalog,s)
  stopifnot(all(is.na(rows$effective_market_price)),all(is.na(rows$market_inMarket)))
 }
 stale<-resource_state(market,status='stale');partial<-state;partial$completeness<-'partial'
 for(s in list(stale,partial)) {
  rows<-championship_market_prices(catalog,s)
  stopifnot(rows$effective_market_price[1]==125,is.na(rows$market_inMarket[3]))
 }
 partial_empty<-resource_state(observed_data(data.frame()));partial_empty$completeness<-'partial'
 stopifnot(all(is.na(championship_market_prices(catalog,partial_empty)$market_inMarket)))
 empty<-championship_market_prices(catalog,resource_state(observed_data(data.frame())))
 stopifnot(all(empty$market_inMarket==FALSE),all(is.na(empty$effective_market_price)))
 bad<-state;bad$data<-rbind(market,market[2,]);rows<-championship_market_prices(catalog,bad)
 stopifnot(all(is.na(rows$effective_market_price)),all(is.na(rows$market_inMarket)))
})
check('clause dates use the source instant and remain distinct from lock expiry',{
 rows<-championship_market_prices(catalog,state)
 columns<-get_reactable_columns_for_players(rows)
 date<-columns$clause_transferDate
 stopifnot(date$name=='Clause Transfer Date',date$cell(rows$clause_transferDate[2])==fm_format_datetime(fm_time('2026-11-20T10:00:00Z')),
  date$cell(NA_character_)=='Not reported',date$cell('broken')=='Not reported',
  identical(rows$id[players_table_active_clause(rows)],c('000123','unlisted')),
  columns$effective_market_price$cell(125)==format_table_currency(125))
})
# Replay local captured response bodies only; never issue their saved requests.
check('authoritative capture keeps every provided clause transfer date',{
 entries<-jsonlite::fromJSON('app.futmondo.com.market_new.har',simplifyVector=FALSE)$log$entries
 e<-Filter(function(e)endsWith(e$request$url,'/league/championshipplayers'),entries)[[1]]
 source<-jsonlite::fromJSON(e$response$content$text,simplifyVector=FALSE)$answer$players
 parsed<-as.data.frame(data.table::rbindlist(lapply(source,parse_player_json),fill=TRUE))
 expected<-vapply(source,function(p)fm_scalar(p$clause$transferDate,NA_character_),character(1))
 stopifnot(nrow(parsed)==520L,sum(!is.na(expected))==3L,identical(parsed$clause_transferDate,expected),
  all(parsed$clause_transferred[!is.na(expected)]))
})
check('durable typed market snapshot restores price evidence without source acquisition',{
 ctx<-resource_context(c(userid='columns-fixture',token='synthetic'),'league','mine')
 d<-fm_worker_descriptor('get_market_players',list(championship_id='league',user_team_id='mine'),ctx)
 wire<-charToRaw(fm_worker_json(fm_worker_encode(market)));compressed<-history_baseline_compress(wire)
 manifest<-list(account_id=ctx$account_id,resource_key=d$resource_key,handler=d$handler,context=d$context,
  version='columns-fixture',json_bytes=length(wire),parts=1L,sha256=paste0(openssl::sha256(compressed)),
  observed_at=rival_finance_stamp(stamp),checked_at=rival_finance_stamp(stamp),valid_until=rival_finance_stamp(stamp+60),status='ok',completeness='complete')
 restored<-fm_worker_restore(list(key=d$resource_key,manifest=manifest,chunks=list(jsonlite::base64_enc(compressed))),ctx$account_id)
 stopifnot(!is.null(restored))
 clear_api_cache();fm_worker_configured<-function()TRUE
 saved_state<-fm_worker_account_state(ctx$account_id);saved_state$known<-TRUE
 saved_state$account<-list(enabled=TRUE,auth_status='active')
 fm_worker_put(ctx$account_id,d$resource_key,restored)
 source_calls<-0L
 futmondo_post<-function(...){source_calls<<-source_calls+1L;stop('Source acquisition forbidden')}
 saved<-get_market_players(c(userid='columns-fixture',token='synthetic'),'league','mine')
 stopifnot(identical(saved$id,market$id),identical(saved$price,market$price),source_calls==0L,!length(.fm_transport$tasks))
 rows<-championship_market_prices(catalog,resource_state(saved))
 stopifnot(rows$effective_market_price[1]==125,as.numeric(attr(restored$data,'observed_at'))==as.numeric(fm_time(manifest$observed_at)))
})
# Real shared adapters, synthetic delayed market transport; no account/DB access.
fm_worker_configured<-function()FALSE
calls<-character();pending<-new.env(parent=emptyenv())
async_adapter_fetch<-function(fn,args,context=NULL,is_current=NULL,...) {
 calls<<-c(calls,paste(fn,args$championship_id,sep=':'))
 if(fn=='get_market_players')return(promises::promise(function(resolve,reject){pending$resolve<-resolve;pending$reject<-reject}))
 promises::promise_resolve(if(fn=='get_championship_players')observed_data(catalog)else list(status='ok',spendable_budget=100))
}
players_table_Server<-function(id,players_table_RV,...) {
 shiny::observe(tryCatch(players_table_RV(),error=function(e)NULL))
 players_table_RV
}
settle<-function(session)for(i in 1:10){later::run_now(0);session$flushReact()}
auth<-shiny::reactiveVal(c(userid='columns-fixture',token='synthetic'));league<-shiny::reactiveVal('league');refresh<-shiny::reactiveVal(0L)
shiny::testServer(players_in_championship_Server,args=list(is_module_active=shiny::reactive(TRUE),login_token=auth,
 championship_id=league,user_team_id=shiny::reactive('mine'),user_teams_RV=shiny::reactive(NULL),refresh_trigger=refresh),{
 settle(session)
 check('delayed optional market read leaves the full catalog responsive',{
  stopifnot(nrow(players_table_RV())==3L,all(is.na(players_table_RV()$effective_market_price)),
   grepl('Market prices loading',paste(unlist(output$market_prices_status),collapse=' '),fixed=TRUE),sum(startsWith(calls,'get_market_players:'))==1L)
  output$heartbeat<-shiny::renderText(input$ping)
  session$setInputs(ping='responsive');settle(session);stopifnot(output$heartbeat=='responsive')
 })
 pending$resolve(market);settle(session)
 check('completed read fills prices, warm redraws reuse it, and date/filter semantics survive',{
  stopifnot(players_table_RV()$effective_market_price[1]==125,
   players_table_RV()$clause_transferDate[2]=='2026-11-20T10:00:00Z')
  before<-calls
  for(i in 1:5){players_table_RV();session$setInputs(ping=as.character(i));settle(session)}
  stopifnot(identical(calls,before))
 })
 league('other-league');settle(session)
 check('league switch clears the previous market price while its own read is pending',{
  stopifnot(all(is.na(players_table_RV()$effective_market_price)),sum(startsWith(calls,'get_market_players:'))==2L)
 })
 pending$reject(simpleError('Synthetic read failure'));settle(session)
 check('market failure keeps catalog data and exposes an actionable safe error',{
  stopifnot(nrow(players_table_RV())==3L,all(is.na(players_table_RV()$effective_market_price)),
   grepl('Market prices:',paste(unlist(output$market_prices_status),collapse=' '),fixed=TRUE),grepl('Refresh to retry',paste(unlist(output$market_prices_status),collapse=' '),fixed=TRUE))
 })
 refresh(1L);settle(session)
 pending$resolve(market);settle(session)
 check('explicit refresh recovers the failed market read without per-player requests',{
  stopifnot(players_table_RV()$effective_market_price[1]==125,
   sum(startsWith(calls,'get_market_players:'))==3L,
   !grepl('Refresh to retry',paste(unlist(output$market_prices_status),collapse=' '),fixed=TRUE))
 })
})
large<-catalog[rep(seq_len(3L),length.out=527L),];large$id<-paste0('p',seq_len(527L))
large_market<-resource_state(observed_data(data.frame(id=large$id,price=seq_len(527L))))
elapsed<-system.time(for(i in 1:100)championship_market_prices(large,large_market))[['elapsed']]
cat(sprintf('MEASURE 100 x 527-row price joins %.3fs; retained projection %d bytes\n',elapsed,as.integer(object.size(championship_market_prices(large,large_market)))))
cat(sprintf('CHAMPIONSHIP COLUMNS: %d groups passed / 0 failed\n',checks))
