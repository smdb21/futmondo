#!/usr/bin/env Rscript
# Synthetic replica of the production nested-bid mapping failure. No private data.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
record_acquired_resource<-function(...)TRUE
checks<-0L
check<-function(label,expr){force(expr);checks<<-checks+1L;cat('PASS',label,'\n')}
player<-list(id='000123',name='Fixture player',role='defensa',teamId='club',value=1000,change=0,points=-2,
 average=list(average=0,matches=2,fitness=list(0,-2)),
 bids=list(list(id='offer-one',price=700,userteam=list(id='rival',name='Rival'))),
 price=1200,expirationDate='2026-11-20T20:00:00Z',clause=list(price=2500,transferred=FALSE))
# Python/JSONB sort keys; mappings cannot rely on source field order.
player<-fm_worker_sort(player)
check('nested bids occupy one column and cannot shift identities or amounts',{
 parsed<-parse_player_json(player)
 stopifnot(nrow(parsed)==1L,identical(parsed$id,'000123'),parsed$name=='Fixture player',
  parsed$price==1200,parsed$points== -2,parsed$value==1000,parsed$change==0,
  parsed$average.fitness=='0,-2',parsed$average.total== -2,parsed$clause_price==2500,
  identical(parsed$bids[[1]],player$bids),!anyNA(names(parsed)),!anyDuplicated(names(parsed)))
})
check('zero one and multiple nested bids retain exactly one player row',{
 for(bids in list(list(),player$bids,rep(player$bids,2))){
  p<-player;p$bids<-bids
  parsed<-parse_player_json(p)
  stopifnot(nrow(parsed)==1L,parsed$id==p$id,parsed$price==p$price)
  if(length(bids))stopifnot(identical(parsed$bids[[1]],bids))
 }
 p<-player;p$points<-NULL;p$extra<-list(observations=list(list(points=0),list(points=-2)))
 parsed<-parse_player_json(p)
 stopifnot(is.na(parsed$points),identical(parsed$extra[[1]],p$extra))
})
auth<-c(userid='nested-fixture',token='synthetic');ctx<-resource_context(auth,'league','mine')
calls<-0L
futmondo_post<-function(url,...){
 calls<<-calls+1L
 answer<-switch(url,
  'https://api.futmondo.com/1/userteam/roster'=list(player,modifyList(player,list(id='000456'))),
  'https://api.futmondo.com/1/league/championshipteams'=list(),
  'https://api.futmondo.com/1/market/rosterbids'=list(),
  'https://api.futmondo.com/1/market/myplayers'=list(player),stop('Unexpected fixture route'))
 fm_httr_response(list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=answer),auto_unbox=TRUE)),times=list(total=0)),url)
}
check('real roster loader joins listed prices by source identity and reuses warm results',{
 clear_api_cache();roster<-get_players_from_team(auth,'league','mine')
 stopifnot(nrow(roster)==2L,identical(roster$id,c('000123','000456')),
  identical(roster$market_inMarket,c(TRUE,FALSE)),roster$market_asking_price[1]==1200,
  is.na(roster$market_asking_price[2]),!acquisition_data_unverified(roster),calls==4L)
 for(i in 1:5)stopifnot(identical(get_players_from_team(auth,'league','mine'),roster))
 stopifnot(calls==4L)
})
check('typed durable snapshots preserve nested source evidence across cleared caches',{
 value<-get_my_market_players(auth,'league','mine');stamp<-attr(value,'observed_at')
 wire<-charToRaw(fm_worker_json(fm_worker_encode(value)));compressed<-history_baseline_compress(wire)
 d<-fm_worker_descriptor('get_my_market_players',list(championship_id='league',user_team_id='mine'),ctx)
 manifest<-list(account_id=ctx$account_id,resource_key=d$resource_key,handler=d$handler,context=d$context,
  version='fixture-v1',json_bytes=length(wire),parts=1L,sha256=paste0(openssl::sha256(compressed)),
  observed_at=rival_finance_stamp(stamp),checked_at=rival_finance_stamp(stamp),
  valid_until=rival_finance_stamp(Sys.time()+60),status='ok',completeness='unknown')
 restored<-fm_worker_restore(list(key=d$resource_key,manifest=manifest,chunks=list(jsonlite::base64_enc(compressed))),ctx$account_id)
 clear_api_cache();fm_worker_configured<-function()TRUE
 s<-fm_worker_account_state(ctx$account_id);s$known<-TRUE;s$account<-list(enabled=TRUE,auth_status='active')
 fm_worker_put(ctx$account_id,d$resource_key,restored)
 saved<-get_my_market_players(auth,'league','mine')
 stopifnot(saved$id=='000123',saved$price==1200,saved$points== -2,identical(saved$bids,value$bids),
  as.numeric(attr(saved,'observed_at'))==as.numeric(fm_time(manifest$observed_at)),calls==4L,!length(.fm_transport$tasks))
})
check('catalog-sized parsing preserves one row per player without expanding bids',{
 elapsed<-system.time({rows<-data.table::rbindlist(lapply(seq_len(527L),function(i)parse_player_json(player)),fill=TRUE)})[['elapsed']]
 stopifnot(nrow(rows)==527L,all(rows$id=='000123'),all(rows$price==1200),is.list(rows$bids))
 cat(sprintf('MEASURE nested_527_parser_elapsed=%.3fs retained_bytes=%d\n',elapsed,as.integer(object.size(rows))))
})
cat(sprintf('NESTED PLAYER FIELDS: %d groups passed; cold reads=4, warm/restart source reads=0\n',checks))
