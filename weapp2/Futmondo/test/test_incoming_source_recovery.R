options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
clear_api_cache();record_acquired_resource<-function(...)TRUE
checks<-0L
check<-function(label,code){force(code);checks<<-checks+1L;cat('PASS',label,'\n')}
raw<-data.frame(id='locker:payment',locker_event_id='payment',created='2026-10-01T10:00:00Z',player_name='Player',buyer_team_name='Buyer',seller_team_name='Seller',price=10,is_clause=TRUE)
locker_status<-'ok'
get_locker_clause_payments<-function(...)observed_data(raw,status=locker_status)
teams<-data.frame(teamid=c('mine','seller'),teamname=c('Buyer','Seller'))
catalog<-data.frame(id='p',name='Player',team='Original Club',logo='original-club')
catalog_reads<-0L;source_reads<-0L
get_teams<-function(...)teams
get_championship_players<-function(...){catalog_reads<<-catalog_reads+1L;catalog}
futmondo_post<-function(url,...){
 source_reads<<-source_reads+1L
 stopifnot(identical(url,PRESSROOM_URL))
 fm_httr_response(list(status_code=200L,content=charToRaw('{"answer":{"news":[]}}'),times=list(total=0)),url)
}
auth<-c(userid='fixture',token='synthetic');context<-resource_context(auth,'league')
first<-get_championship_pressroom(auth,'league')
check('complete source resolves a paid incoming player before delivery',{
 stopifnot(identical(attr(first,'fetch_status'),'ok'),nrow(first)==1L,first$player_id=='p',first$buyer_team_id=='mine',first$seller_team_id=='seller',catalog_reads==1L)
})
teams$teamname[1]<-'Renamed Buyer';catalog<-catalog[FALSE,,drop=FALSE]
invalidate_resources(context,pattern='pressroom')
second<-get_championship_pressroom(auth,'league')
check('same payment retains verified identities after directory rename or catalog removal',{
 stopifnot(identical(attr(second,'fetch_status'),'ok'),nrow(second)==1L,second$player_id=='p',second$buyer_team_id=='mine',second$seller_team_id=='seller',
  second$player_team_name=='Original Club',second$player_team_slug=='original-club',catalog_reads==1L,source_reads==2L)
})
check('recovered current history enables exact incoming reconciliation with fresh summary evidence',{
 summary<-observed_data(list(data=list(id='p',name='Player'),championship=list(owner=list(id='seller'),clause=list(transferred=TRUE,transferDate='2026-10-05T10:00:00Z'))))
 incoming<-reconcile_pending_clause_transfers(observed_data(data.frame(id='owned')),second,list(p=summary),'mine',now=fm_time('2026-10-02T10:00:00Z'))
 stopifnot(incoming$status=='ok',incoming$pending_inbound_count==1L,nrow(incoming$incoming_records)==1L,incoming$incoming_records$player_id=='p',
  incoming$incoming_records$joins_at==fm_time('2026-10-05T10:00:00Z'))
 stale<-second;attr(stale,'fetch_status')<-'stale'
 stopifnot(reconcile_pending_clause_transfers(data.frame(id='owned'),stale,list(p=summary),'mine')$status=='unavailable')
})
check('different, revised, partial or ambiguous prior facts cannot supply identities',{
 empty_teams<-teams[FALSE,,drop=FALSE]
 for(kind in c('event','amount','date','player','buyer','seller','partial','ambiguous')) {
  old<-second
  if(kind=='event')old$locker_event_id<-'other'
  if(kind=='amount')old$price<-11
  if(kind=='date')old$created<-'2026-10-01T11:00:00Z'
  if(kind=='player')old$player_name<-'Other Player'
  if(kind=='buyer')old$buyer_team_name<-'Other Buyer'
  if(kind=='seller')old$seller_team_name<-'Other Seller'
  if(kind=='partial')attr(old,'fetch_status')<-'partial'
  if(kind=='ambiguous'){old<-rbind(old,old);old$id[2]<-'other';attr(old,'fetch_status')<-'ok';attr(old,'full_history_at')<-Sys.time()}
  resolved<-locker_resolve_parties(raw,data.frame(),empty_teams,previous=old)
  stopifnot(!nzchar(resolved$player_id),!nzchar(resolved$buyer_team_id),!nzchar(resolved$seller_team_id))
 }
})
check('changed source that cannot be resolved stays unavailable and preserves complete checkpoint',{
 previous<-read_completed_history_baseline('pressroom_league',context)
 raw$price<-11;invalidate_resources(context,pattern='pressroom')
 result<-suppressMessages(tryCatch(get_championship_pressroom(auth,'league'),error=identity))
 stopifnot(inherits(result,'futmondo_unavailable'),identical(read_completed_history_baseline('pressroom_league',context),previous))
 raw$price<-10;locker_status<-'stale';invalidate_resources(context,pattern='pressroom')
 result<-suppressMessages(tryCatch(get_championship_pressroom(auth,'league'),error=identity))
 stopifnot(inherits(result,'futmondo_unavailable'),identical(read_completed_history_baseline('pressroom_league',context),previous))
})
check('incoming prerequisite failures identify the failed source and retain safe diagnostics',{
 good<-resource_state(observed_data(data.frame(id='owned')))
 failure<-resource_failure(resource='pressroom',category='http',http_status=503L)
 broken<-resource_state(status='unavailable',error=failure)
 problem<-incoming_clause_prerequisite_failure(good,broken)
 stopifnot(grepl('Transaction history:',problem$message,fixed=TRUE),!grepl('Roster',problem$message,fixed=TRUE),
  grepl(failure$reference,problem$message,fixed=TRUE),identical(problem$failures$transactions$reference,failure$reference),problem$failures$transactions$category=='http')
 stale<-resource_state(observed_data(data.frame(id='owned'),status='stale'))
 problem<-incoming_clause_prerequisite_failure(stale,good)
 stopifnot(grepl('Roster is stale',problem$message,fixed=TRUE),!grepl('Transaction history',problem$message,fixed=TRUE),incoming_clause_source_unverified(stale))
 partial<-resource_state(observed_data(data.frame(id='owned'),status='partial'))
 problem<-incoming_clause_prerequisite_failure(good,partial)
 stopifnot(grepl('Transaction history is incomplete',problem$message,fixed=TRUE),incoming_clause_source_unverified(partial),!incoming_clause_source_unverified(good))
})
cat(sprintf('INCOMING SOURCE RECOVERY: %d groups passed; unchanged payment refresh avoids one redundant catalog request\n',checks))
