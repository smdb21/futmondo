#!/usr/bin/env Rscript
# Durable replay through the actual encoder/decoder and private view query.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
clock<-as.POSIXct('2026-11-15 12:00:00.123456',tz='UTC');Sys.time<-function()clock
ctx<-resource_context(account_id='availability-store',championship_id='league',user_team_id='mine',
 player_id='p1',competition_id='competition',season='2026-27',scoring_version='fixture')
rows<-list();writes<-0L;reads<-0L;ack<-TRUE
save_row<-function(row){rows[[row$source_id]]<<-row;invisible(TRUE)}
defer_persistence<-function(fn,args,context=NULL,on_complete=NULL,...){
 stopifnot(fn=='supabase_post_direct',args[[1]]=='rival_finance_evidence',isTRUE(args[[4]]))
 writes<<-writes+1L;if(ack)for(row in args[[2]])save_row(row)
 if(is.function(on_complete))on_complete(ack)
 invisible(TRUE)
}
supabase_get<-function(table_name,query_params=list()){
 stopifnot(table_name=='rival_finance_evidence_latest')
 reads<<-reads+1L
 selected<-Filter(function(row)paste0('eq.',row$account_id)==query_params$account_id&&
  paste0('eq.',row$championship_id)==query_params$championship_id&&
  paste0('eq.',row$competition_id)==query_params$competition_id&&paste0('eq.',row$season)==query_params$season&&
  grepl(row$source_id,query_params$source_id,fixed=TRUE),rows)
 if(!length(selected))return(data.frame())
 result<-data.frame(account_id=vapply(selected,`[[`,character(1),'account_id'),
  championship_id=vapply(selected,`[[`,character(1),'championship_id'),competition_id=vapply(selected,`[[`,character(1),'competition_id'),
  season=vapply(selected,`[[`,character(1),'season'),evidence_type='eligibility',source_id=vapply(selected,`[[`,character(1),'source_id'),
  observed_at=vapply(selected,`[[`,character(1),'observed_at'))
 result$payload<-I(lapply(selected,function(row)jsonlite::fromJSON(jsonlite::toJSON(row$payload,auto_unbox=TRUE,null='null',na='null',digits=NA),simplifyVector=FALSE)))
 result
}
summary<-observed_data(list(data=list(id='p1'),championship=list(owner=list(id='rival')),points=list(list(round=1L,points=-2)),prices=list()),observed_at=clock)
stopifnot(player_availability_save(summary,ctx,'summary',clock))
clear_api_cache();saved<-get_saved_player_availability(ctx,'summary')
stopifnot(!is.null(saved),identical(saved$points,summary$points),
 abs(as.numeric(attr(saved,'observed_at'))-as.numeric(clock))<0.00001,isTRUE(attr(saved,'availability_from_saved')))
cat('PASS durable summary replay preserves signed points, source timestamps and exact scope after memory-cache clearing\n')
for(field in c('account_id','championship_id','user_team_id','competition_id','season','scoring_version','player_id')){
 wrong<-ctx;wrong[[field]]<-paste0(wrong[[field]],'-other')
 stopifnot(is.null(get_saved_player_availability(wrong,'summary')))
}
cat('PASS all account/league/team/competition/season/scoring/player mismatches reject replay\n')
clock<-clock+60
stopifnot(is.null(get_saved_player_availability(ctx,'summary')))
clock<-clock-60
id<-player_availability_id(player_availability_scope(ctx,'summary'));original<-rows[[id]]
rows[[id]]$payload$sha256<-paste(rep('0',64),collapse='')
stopifnot(is.null(get_saved_player_availability(ctx,'summary')))
rows[[id]]<-original
bad<-summary;bad$token<-'must-never-persist';before<-writes
stopifnot(!player_availability_save(bad,ctx,'summary',clock),writes==before)
anonymous<-summary;anonymous$data<-NULL
stopifnot(!player_availability_save(anonymous,ctx,'summary',clock),writes==before)
cat('PASS original expiry, payload integrity and secret-field rejection remain enforced\n')
clock<-clock+1
player_availability_invalidate(ctx,pattern='player_summary')
# Clear process-local epoch state too: the database tombstone must suffice.
rm(list=ls(.player_availability_epochs),envir=.player_availability_epochs)
stopifnot(is.null(get_saved_player_availability(ctx,'summary')))
clock<-clock+1;new_summary<-summary;attr(new_summary,'observed_at')<-clock
stopifnot(player_availability_save(new_summary,ctx,'summary',clock),!is.null(get_saved_player_availability(ctx,'summary')))
cat('PASS a persisted invalidation blocks old data across restart; a later verified observation restores reuse\n')
ack<-FALSE;completion<-NULL
stopifnot(player_availability_save(new_summary,ctx,'summary',clock,on_complete=function(ok)completion<<-ok),identical(completion,FALSE))
ack<-TRUE
stopifnot(player_availability_save(new_summary,ctx,'summary',clock,on_complete=function(ok)completion<<-ok),identical(completion,TRUE))
cat('PASS failed/unacknowledged persistence is not durable and can be retried\n')
# Known-answer team calculation and its inherited short deadline.
team<-calculate_team_acquisition_capacity(observed_data(list(budget=1000,withheld=0,teamValue=2000,maxBid=2000,
 configuration=list(maxPlayersInRoster=20,mnmp=.5,mbmv=TRUE)),observed_at=clock),
 observed_data(data.frame(id='owned',value=2000),observed_at=clock),
 observed_data(data.frame(id=character(),player_id=character(),buyer_team_id=character(),seller_team_id=character(),created=character()),observed_at=clock),
 observed_data(data.frame(id=character(),bid_id=character(),bid_price=numeric()),observed_at=clock),list(),'mine',now=clock)
team<-observed_data(team,observed_at=clock)
incomplete<-team;incomplete$roster$pending_inbound_ids<-NULL;before<-writes
stopifnot(!player_availability_save(incomplete,ctx,'team',clock),writes==before)
stopifnot(team$status=='ok',player_availability_save(team,ctx,'team',clock))
saved_team<-get_saved_player_availability(ctx,'team')
stopifnot(!is.null(saved_team),identical(saved_team$funds,team$funds),identical(saved_team$valid_until,team$valid_until))
clock<-clock+15;stopifnot(is.null(get_saved_player_availability(ctx,'team')))
cat('PASS durable team replay preserves known-answer funds and never renews its original 15-second deadline\n')
# Exercise the real cooperative DB adapter, not only the pure store decoder.
clock<-clock-14;clear_api_cache()
get_sb_url<-function()'https://database.invalid';get_sb_key<-function()'synthetic-fixture-key'
database_requests<-0L;live_requests<-0L
mock_query<-supabase_get
fm_async_read_http<-function(url,method='POST',body=NULL,...){
 stopifnot(method=='GET',grepl('/rest/v1/rival_finance_evidence_latest',url,fixed=TRUE))
 database_requests<<-database_requests+1L
 result<-mock_query('rival_finance_evidence_latest',httr::parse_url(url)$query)
 promises::promise_resolve(list(status_code=200L,content=charToRaw(jsonlite::toJSON(result,dataframe='rows',auto_unbox=TRUE,null='null',na='null',digits=NA)),times=list(total=0)))
}
native_async<-async_adapter_fetch
async_adapter_fetch<-function(fn,args=list(),context,...){
 if(fn=='get_saved_player_availability')return(native_async(fn,args,context,...))
 live_requests<<-live_requests+1L;stop('Valid durable replay must not acquire source data')
}
shiny::testServer(function(input,output,session){
 source<-player_availability_source(session,'team',shiny::reactive(ctx),function()c(userid=ctx$account_id,token='synthetic'))
 output$budget<-shiny::renderText(source()$data$funds$spendable_budget)
},{
 for(i in 1:25){later::run_now(0);session$flushReact()}
 stopifnot(nzchar(output$budget),database_requests==1L,live_requests==0L)
})
cat('PASS cold provider uses one cooperative database GET and zero source requests for a valid persisted team result\n')
# Deferred snapshot preparation must settle, produce the same validated payload
# and yield between finite JSON steps, rather than blocking one large render.
completion<-NULL;ticks<-proc.time()[['elapsed']]
beat<-function(){ticks<<-c(ticks,proc.time()[['elapsed']]);if(is.null(completion))later::later(beat,0.005)}
later::later(beat,0.005)
stopifnot(player_availability_save(new_summary,ctx,'summary',clock,on_complete=function(ok)completion<<-ok,deferred=TRUE),is.null(completion))
started<-proc.time()[['elapsed']]
while(is.null(completion)&&proc.time()[['elapsed']]-started<5)later::run_now(0.01)
stopifnot(identical(completion,TRUE),!is.null(get_saved_player_availability(ctx,'summary')))
cat(sprintf('PASS finite deferred encoding settles and restores a valid snapshot (heartbeat max gap %.1fms)\n',max(diff(ticks))*1000))
listed<-observed_data(data.frame(id='own-player',price=123.45),observed_at=clock)
stopifnot(player_availability_save(listed,ctx,'listings',clock))
restored_listings<-get_saved_player_availability(ctx,'listings')
stopifnot(!is.null(restored_listings),identical(restored_listings$price,listed$price))
clock<-clock+20;stopifnot(is.null(get_saved_player_availability(ctx,'listings')))
cat('PASS owned-listing snapshot preserves exact asking prices and the original 20-second market deadline\n')
empty_listings<-observed_data(data.frame(),observed_at=clock)
stopifnot(player_availability_save(empty_listings,ctx,'listings',clock))
restored_empty<-get_saved_player_availability(ctx,'listings')
stopifnot(is.data.frame(restored_empty),nrow(restored_empty)==0L,
 !player_availability_save(NULL,ctx,'listings',clock))
cat('PASS a verified empty listing response persists distinctly from unavailable data\n')
before_epoch<-shiny::isolate(player_availability_invalidated(ctx,'listings'))
clear_api_cache(ctx$account_id,session_id='departing-browser')
stopifnot(identical(before_epoch,shiny::isolate(player_availability_invalidated(ctx,'listings'))),
 is.data.frame(get_saved_player_availability(ctx,'listings')))
cat('PASS one browser session cleanup does not invalidate another session\'s valid account observations\n')
