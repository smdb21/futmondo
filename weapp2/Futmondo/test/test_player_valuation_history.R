#!/usr/bin/env Rscript
# Known-answer source shapes; all requests and persistence are synthetic.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
ctx<-resource_context(account_id='fixture',championship_id='league',user_team_id='team',player_id='player',
 competition_id='competition',season='2025-2026',scoring_version='scoring')
dates<-format(as.POSIXct('2026-07-06T02:25:25',tz='UTC')+0:70*86400,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC')
profile<-list(player=list(id='player'),league=list(`_id`='competition',season='2025-2026'),
 prices=data.frame(date=dates,social=1000000+0:70*10000,classic=900000+0:70*7000))
attr(profile,'observed_at')<-fm_time('2026-11-15T08:16:09Z')
summary<-list(championship=list(divider=1),prices=data.frame(date=tail(dates,7),price=tail(profile$prices$social,7)))
check('71 source prices extend the seven-day summary on their actual dates',{
 history<-assemble_player_card_history(summary,list(),data.frame(),data.frame(),ctx,profile)
 stopifnot(nrow(history$valuation)==71L,identical(as.numeric(history$valuation$value),as.numeric(profile$prices$social)),
  identical(as.numeric(fm_time(history$valuation$recorded_at)),as.numeric(fm_time(dates))),
  sum(history$valuation$source=='futmondo')==7L,!length(attr(history$valuation,'valuation_note')))
})
check('classic and an explicit divider are independently verified against all overlap dates',{
 s<-summary;s$prices$price<-tail(profile$prices$classic,7)
 stopifnot(identical(normalize_card_valuation(data.frame(),s,profile,ctx)$value,as.numeric(profile$prices$classic)))
 s$championship$divider<-10;s$prices$price<-s$prices$price/10
 stopifnot(identical(normalize_card_valuation(data.frame(),s,profile,ctx)$value,as.numeric(profile$prices$classic/10)))
 s$championship$divider<-NULL
 stopifnot(nrow(normalize_card_valuation(data.frame(),s,profile,ctx))==7L)
})
check('mismatches unknown dates and ambiguous modes never manufacture older prices',{
 for(field in c('player_id','competition_id','season')){
  other<-ctx;other[[field]]<-'other'
  stopifnot(nrow(normalize_card_valuation(data.frame(),summary,profile,other))==7L)
 }
 p<-profile;p$prices$classic[65:71]<-p$prices$social[65:71]
 stopifnot(nrow(normalize_card_valuation(data.frame(),summary,p,ctx))==7L)
 s<-summary;s$prices$price[1]<-s$prices$price[1]+1
 stopifnot(nrow(normalize_card_valuation(data.frame(),s,profile,ctx))==7L)
 s$prices$date<-format(fm_time(s$prices$date)+86400*100,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')
 stopifnot(nrow(normalize_card_valuation(data.frame(),s,profile,ctx))==7L)
})
check('database observations and exact fractional source dates retain their provenance',{
 stored<-data.frame(recorded_at=c('2026-07-01T12:00:00.123456Z',tail(dates,1)),value=c(850000,99),observed_at='2026-11-01T00:00:00Z')
 values<-normalize_card_valuation(stored,summary,profile,ctx)
 stopifnot(nrow(values)==72L,values$value[1]==850000,tail(values$value,1)==1700000,
  as.numeric(fm_time(values$recorded_at[1]))==as.numeric(fm_time(stored$recorded_at[1])))
})
queued<-list();callbacks<-list()
real_defer<-defer_persistence
defer_persistence<-function(fn,args,context,on_complete=NULL,...){queued[[length(queued)+1L]]<<-list(fn=fn,args=args,context=context);callbacks[[length(callbacks)+1L]]<<-on_complete;TRUE}
check('the already acquired curve persists only factual fields with original source clock',{
 stopifnot(persist_player_valuation_profile(profile,ctx),length(queued)==1L)
 row<-queued[[1]]$args[[2]][[1]]
 stopifnot(identical(row$payload$scope,player_valuation_scope(ctx)),
  row$payload$profile$complete,identical(row$observed_at,'2026-11-15T08:16:09.000000Z'),
  nrow(row$payload$profile$prices)==71L,!grepl('token|password|cookie|authorization',jsonlite::toJSON(row$payload)))
 # A failure/unacknowledged save is never installed as durable data.
 stopifnot(persist_player_valuation_profile(profile,ctx),length(queued)==2L,
  identical(queued[[1]]$args,queued[[2]]$args))
})
row<-queued[[1]]$args[[2]][[1]]
requests<-list()
supabase_get<-function(table,query){
 requests[[length(requests)+1L]]<<-list(table=table,query=query)
 if(table=='player_history')return(data.frame())
 stopifnot(table=='rival_finance_evidence_latest')
 jsonlite::fromJSON(as.character(jsonlite::toJSON(list(row),auto_unbox=TRUE,na='null',null='null',digits=NA)))
}
check('clearing all memory reuses database history before the live profile arrives',{
 clear_api_cache();saved<-read_card_price_history(ctx)
 history<-assemble_player_card_history(summary,list(),data.frame(),saved,ctx)
 stopifnot(nrow(history$valuation)==71L,
  identical(history$valuation$value,as.numeric(profile$prices$social)),
  requests[[2]]$query$limit==1L,requests[[2]]$query$season=='eq.2025-2026',
  requests[[2]]$query$competition_id=='eq.competition',
  identical(attr(attr(saved,'valuation_profile'),'fetch_status'),'stale'))
})
check('every saved context dimension is isolated and corrupt snapshots are rejected',{
 for(field in names(player_valuation_scope(ctx))){
  other<-ctx;other[[field]]<-'other'
  stopifnot(is.null(read_player_valuation_profile(other)))
 }
 original<-row;row$payload$scope<-row$payload$scope[rev(names(row$payload$scope))];stopifnot(!is.null(read_player_valuation_profile(ctx)));row<-original
 row$payload$scope$season<-'wrong';stopifnot(is.null(read_player_valuation_profile(ctx)));row<-original
 row$payload$profile$prices$date[1]<-'invalid';stopifnot(is.null(read_player_valuation_profile(ctx)));row<-original
})
check('partial stale source rows stay truthful and cannot write fresh snapshots',{
 p<-profile;attr(p,'fetch_status')<-'stale'
 before<-length(queued);stopifnot(!persist_player_valuation_profile(p,ctx),length(queued)==before,
  nrow(normalize_card_valuation(data.frame(),summary,p,ctx))==71L)
 p<-profile;p$prices$social[1]<-NA_real_
 values<-normalize_card_valuation(data.frame(),summary,p,ctx)
 stopifnot(nrow(values)==70L,length(attr(values,'valuation_note'))==1L)
 p<-profile;p$prices$date[1]<-'invalid';values<-normalize_card_valuation(data.frame(),summary,p,ctx)
 stopifnot(nrow(values)==70L,length(attr(values,'valuation_note'))==1L)
 p<-profile;p$prices$social[1:2]<-c(0,-1);values<-normalize_card_valuation(data.frame(),summary,p,ctx)
 stopifnot(nrow(values)==71L,identical(head(values$value,2),c(0,-1)))
})
# Independently replay the official response-only captures, when present.
check('captured official responses prove the 7-to-71-day field mapping',{
 path<-'app.futmondo.com.market_new.har'
 if(file.exists(path)){
  entries<-jsonlite::fromJSON(path,simplifyVector=FALSE)$log$entries
  response<-function(route){
   e<-Filter(function(e)endsWith(sub('\\?.*$','',e$request$url),route)&&is.character(e$response$content$text),entries)[[1]]
   jsonlite::fromJSON(e$response$content$text,simplifyVector=FALSE)$answer
  }
  p<-response('/player/fullprofile');s<-response('/player/summary');attr(p,'observed_at')<-attr(profile,'observed_at')
  known<-ctx;known$player_id<-p$player$id;known$competition_id<-p$league$`_id`;known$season<-p$league$season
  values<-normalize_card_valuation(data.frame(),s,p,known)
  stopifnot(length(s$prices)==7L,length(p$prices)==71L,nrow(values)==71L,
   all(values$value==vapply(p$prices,function(x)as.numeric(x$social),numeric(1))))
 }
})
check('history transforms remain bounded for a 5000-day curve',{
 p<-profile;p$prices<-data.frame(date=format(fm_time(dates[1])+0:4999*86400,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),social=1e6+0:4999,classic=2e6+0:4999)
 s<-summary;s$prices<-data.frame(date=tail(p$prices$date,7),price=tail(p$prices$social,7))
 started<-proc.time()[['elapsed']];values<-normalize_card_valuation(data.frame(),s,p,ctx);elapsed<-proc.time()[['elapsed']]-started
 stopifnot(nrow(values)==5000L);cat(sprintf('MEASURE 5000 prices: %.3fs / %d bytes\n',elapsed,as.numeric(object.size(values))))
})
check('shorter subsequent source responses retain saved older prices and clocks',{
 old<-read_player_valuation_profile(ctx);short<-profile;short$prices<-tail(profile$prices,7L)
 attr(short,'observed_at')<-attr(profile,'observed_at')+3600
 stopifnot(persist_player_valuation_profile(short,ctx,previous=old))
 latest<-tail(queued,1)[[1]]$args[[2]][[1]]$payload$profile
 stopifnot(nrow(latest$prices)==71L,
  latest$prices$source_observed_at[1]=='2026-11-15T08:16:09.000000Z',
  tail(latest$prices$source_observed_at,1)=='2026-11-15T09:16:09.000000Z')
})
# Exercise the actual selected-card reactive wiring with the long source held.
held<-list();calls<-character()
async_adapter_fetch<-function(fn,args=list(),context,is_current=function()TRUE,...){
 calls<<-c(calls,fn)
 if(fn=='get_player_full_profile')return(promises::promise(function(resolve,reject){held[[length(held)+1L]]<<-list(resolve=resolve,current=is_current)}))
 value<-switch(fn,get_player_summary={s<-summary;s$championship$season<-ctx$season;s$championship$competitionId<-ctx$competition_id;observed_data(s)},
  get_lineup_from_team=list(pointSystem='fixture'),get_player_matches=list(matches=list()),get_my_market_players=data.frame(),
  read_card_round_history=data.frame(),read_card_price_history=data.frame(),
  get_saved_player_availability=NULL,stop(fn))
 promises::promise_resolve(value)
}
record_acquired_resource<-function(...)TRUE
selected<-shiny::reactiveVal(data.frame(id='player',name='Dani Lorenzo fixture',role='centrocampista',team='Club',value=1700000,
 points=0,user_team_id='team',price=1700000,change=0,market_inMarket=FALSE))
settle<-function(session)for(i in 1:25){later::run_now(.001);session$flushReact()}
auth<-c(userid='fixture',token='synthetic')
clear_api_cache()
shiny::testServer(selected_player_Server,args=list(selected_player=selected,login_token=shiny::reactive(auth),
 championship_id=shiny::reactive('league'),user_team_id=shiny::reactive('team')),{
 settle(session)
 check('card renders recent prices while the full profile is still pending',{
  stopifnot(nrow(card_history()$valuation)==7L,length(held)==1L,card_states$profile()$status=='loading')
 })
 held[[1]]$resolve(profile);settle(session)
 check('arrival of the full profile redraws all 71 dates without another acquisition',{
  stopifnot(nrow(card_history()$valuation)==71L,sum(calls=='get_player_full_profile')==1L,
   all(card_history()$valuation$value==profile$prices$social))
 })
 check('Refresh history reloads the full-profile resource and retains source isolation',{
  before<-sum(calls=='get_player_full_profile');mutable_before<-sum(calls%in%c('get_team_acquisition_capacity','get_my_market_players'));session$setInputs(refresh_history=1);settle(session)
  stopifnot(sum(calls=='get_player_full_profile')==before+1L,length(held)==2L,
   sum(calls%in%c('get_team_acquisition_capacity','get_my_market_players'))==mutable_before)
  selected(NULL);settle(session);stopifnot(!held[[2]]$current())
  held[[2]]$resolve(profile);settle(session);stopifnot(is.null(card_context()))
 })
})
defer_persistence<-real_defer
cat(sprintf('PLAYER VALUATION HISTORY: %d groups passed\n',n))
