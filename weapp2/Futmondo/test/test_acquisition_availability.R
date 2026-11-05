#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('futmondo_functions.R'))
# This native capacity fixture has no locker payments; dedicated incoming/locker
# regressions cover that source and its pending-transfer reconciliation.
get_locker_clause_payments<-function(...)observed_data(data.frame())
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
auth<-c(userid='acquisition-fixture',token='synthetic-only');cid<-'league';team<-'mine';ctx<-resource_context(auth,cid,team,session_id='availability-fixture')
await<-function(p){done<-FALSE;result<-NULL;failure<-NULL;promises::then(p,function(x){result<<-x;done<<-TRUE},function(e){failure<<-e;done<<-TRUE});deadline<-Sys.time()+4;while(!done&&Sys.time()<deadline)later::run_now(.01);stopifnot(done);if(!is.null(failure))stop(failure);result}
requests<-character();missing_withheld<-FALSE;delay<-.005
response<-function(url,body){
 route<-tail(strsplit(url,'/',fixed=TRUE)[[1]],1);q<-jsonlite::fromJSON(body)$query
 requests<<-c(requests,route)
 answer<-switch(route,information={
   value<-list(budget=1000,withheld=0,teamValue=2000,maxBid=2000,configuration=list(maxPlayersInRoster=20,mnmp=.5,mbmv=TRUE))
   if(missing_withheld)value$withheld<-NULL;value
 },roster=list(list(id='owned',name='Owned',role='defensa',value=2000,change=0,points=5,teamId='club')),
 players=list(list(id='target',name='Target',role='defensa',value=1000,change=0,points=5,bid=list(id='own',price=100),numberOfBids=1)),
 pressroom=if(q$from=='')list(news=list(
   list(`_id`='new',created='2026-10-29T18:00:00Z',`_player`=list(`_id`='owned'),`_buyer`=list(`_id`=team),price=2000),
   list(`_id`='old',created='2026-10-29T12:00:00Z',`_player`=list(`_id`='owned'),`_seller`=list(`_id`=team),`_buyer`=list(`_id`='rival'),price=2000)
 ))else list(news=list()),championshipteams=list(list(id='club',name='Club',logo='club.svg')),rosterbids=list(),myplayers=list(),stop('Unexpected availability fixture route'))
 list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=answer),auto_unbox=TRUE)),times=list(total=0))
}
fm_async_read_http<-function(url,method='POST',body=NULL,...){
 promises::promise(function(resolve,reject)later::later(function()resolve(response(url,body)),delay))
}
args<-list(login=auth,championship_id=cid,user_team_id=team)
clear_api_cache()
check('cold real async capacity uses five factual reads with no roster presentation dependencies',{
 heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,.001);started<-Sys.time()
 capacity<-await(async_adapter_fetch('get_team_acquisition_capacity',args,ctx))
 stopifnot(heartbeat,capacity$status=='ok',capacity$roster$status=='ok',capacity$roster$effective_count==1L,
   capacity$outstanding$total_amount==100,capacity$funds$spendable_budget==1900,
   length(requests)==5L,identical(sort(requests),sort(c('information','roster','pressroom','pressroom','players'))))
 cat(sprintf('METRIC cold_capacity_reads=5 elapsed=%.3fs heartbeat=TRUE snapshot=%.1fKiB\n',as.numeric(difftime(Sys.time(),started,units='secs')),as.numeric(object.size(capacity))/1024))
})
check('different targets project their own bid from one cached team snapshot without more reads',{
 before<-length(requests)
 first<-await(async_adapter_fetch('get_acquisition_capacity',c(args,list(target_player_id='target')),ctx))
 second<-await(async_adapter_fetch('get_acquisition_capacity',c(args,list(target_player_id='other')),ctx))
 stopifnot(first$target$my_bid_id=='own',first$target$my_bid_amount==100,is.null(second$target$my_bid_id),
   is.na(second$target$my_bid_amount),length(requests)==before)
})
check('full roster presentation reuses raw payload and cannot receive the minimal facts frame',{
 before_roster<-sum(requests=='roster');full<-await(async_adapter_fetch('get_players_from_team',args,ctx))
 stopifnot(full$name=='Owned',full$role=='defensa',sum(requests=='roster')==before_roster,
   all(c('championshipteams','rosterbids','myplayers')%in%requests))
})
check('System counterparties and same-day source times create no phantom pending transfer',{
 roster<-data.frame(id='owned');tx<-data.frame(player_id=c('owned','owned','other'),buyer_team_id=c(team,'rival',NA),seller_team_id=c(NA,team,'rival'),
   created=c('2026-10-29T18:00:00.100Z','2026-10-29T12:00:00.900Z','invalid unrelated date'))
 result<-reconcile_pending_clause_transfers(roster,tx,list(),team)
 stopifnot(result$status=='ok',result$pending_outbound_count==0L,result$pending_inbound_count==0L,nrow(pending_clause_candidates(roster,tx,team))==0L)
 tx<-data.frame(player_id='incoming',buyer_team_id=team,seller_team_id=NA_character_,created='2026-10-29T12:00:00Z')
 stopifnot(reconcile_pending_clause_transfers(roster,tx,list(incoming=list(championship=list(owner=list(id='rival'),clause=list(transferred=TRUE,transferDate='2026-10-30T12:00:00Z')))),team,as.POSIXct('2026-10-29',tz='UTC'))$status=='ok')
})
check('partial prerequisites stay blocked with actionable diagnostics and no raw errors',{
 clear_api_cache();missing_withheld<-TRUE
 value<-await(async_adapter_fetch('get_team_acquisition_capacity',args,ctx));gate<-evaluate_acquisition_preflight(value,'bid',100)
 stopifnot(value$status=='partial',!gate$ok,grepl('Reserved funds for active offers',gate$message,fixed=TRUE),
   !grepl('status:|withheld|synthetic-only',gate$message))
 missing_withheld<-FALSE
 invalidate_resources(resource_context(auth,cid,team),pattern='capacity|team_info')
 value<-await(async_adapter_fetch('get_team_acquisition_capacity',args,ctx));stopifnot(value$status=='ok')
})
check('stale and partial source metadata never become verified roster facts or pass submission',{
 clear_api_cache()
 full<-data.frame(id='owned',value=2000);attr(full,'fetch_status')<-'partial'
 raw<-list(list(id='owned',value=2000));attr(raw,'fetch_status')<-'partial'
 resource_store(api_cache_key('roster_league_mine',context=resource_context(auth,cid,team)),full,resource_context(auth,cid,team),'roster_league_mine')
 resource_store(api_cache_key('roster_payload_league_mine',context=resource_context(auth,cid,team)),raw,resource_context(auth,cid,team),'roster_payload_league_mine')
 value<-await(async_adapter_fetch('get_team_acquisition_capacity',args,ctx))
 stopifnot(value$status=='partial',!evaluate_acquisition_preflight(value,'bid',100)$ok)
 fake<-value;fake$status<-'ok';attr(fake,'fetch_status')<-'partial';stopifnot(!evaluate_acquisition_preflight(fake,'bid',100)$ok)
 for(ids in list(c('owned',NA),c('owned',''),c('owned','owned')))stopifnot(inherits(try(roster_capacity_facts(data.frame(id=ids)),silent=TRUE),'try-error'))
})
check('derived roster caches and capacity deadlines preserve the raw observation age',{
 clear_api_cache();scope<-resource_context(auth,cid,team);before<-sum(requests=='roster');old_stamp<-Sys.time()-31
 raw<-list(list(id='expired',value=1));full<-data.frame(id='expired',name='Expired',value=1);facts<-data.frame(id='expired',value=1)
 for(item in list(list(name='roster_payload_league_mine',data=raw),list(name='roster_league_mine',data=full),list(name='roster_facts_league_mine',data=facts))){
   attr(item$data,'observed_at')<-old_stamp
   resource_store(api_cache_key(item$name,context=scope),item$data,scope,item$name)
 }
 value<-await(async_adapter_fetch('get_team_acquisition_capacity',args,ctx))
 stopifnot(value$status=='ok',sum(requests=='roster')==before+1L,
   !'expired'%in%get_players_from_team(auth,cid,team,facts_only=TRUE)$id)
 clear_api_cache();near_expiry<-data.frame(id='owned',value=2000);attr(near_expiry,'observed_at')<-Sys.time()-29
 resource_store(api_cache_key('roster_league_mine',context=scope),near_expiry,scope,'roster_league_mine')
 before<-sum(requests=='roster')
 value<-await(async_adapter_fetch('get_team_acquisition_capacity',args,ctx))
 stopifnot(value$valid_until-as.numeric(Sys.time())>10,sum(requests=='roster')==before+1L,
   !evaluate_acquisition_preflight(value,'bid',100,now=value$valid_until+1)$ok,
   grepl('expired',evaluate_acquisition_preflight(value,'bid',100,now=value$valid_until+1)$message))
 key<-api_cache_key('acq_capacity_acquisition-fixture_league_mine',context=scope)
 api_cache_env[[key]]$data$valid_until<-as.numeric(Sys.time())-1
 api_cache_env[[api_cache_key('roster_league_mine',context=scope)]]$data<-observed_data(near_expiry,observed_at=Sys.time()-31)
 before<-sum(requests=='roster');value<-await(async_adapter_fetch('get_team_acquisition_capacity',args,ctx))
 stopifnot(value$status=='ok',sum(requests=='roster')==before,value$valid_until>as.numeric(Sys.time()))
})
cat(sprintf('ACQUISITION AVAILABILITY: %d passed / 0 failed\n',n))
