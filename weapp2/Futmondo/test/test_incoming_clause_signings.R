options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(label,code){force(code);checks<<-checks+1L;cat('[PASS]',label,'\n')}
now<-fm_time('2026-09-17T12:00:00Z');roster<-data.frame(id=c('out','owned'))
tx<-data.frame(id=c('old','buy','duplicate','sell'),player_id=c('in','in','in','out'),
 player_name=c('Incoming','Incoming','Incoming','Leaving'),buyer_team_id=c('rival','mine','mine','rival'),
 seller_team_id=c(NA,'rival','rival','mine'),created=c('2026-09-10T12:00:00Z',rep('2026-09-16T12:00:00Z',3)))
summary<-function(owner='rival',date='2026-09-19T22:00:00.000Z',transferred=TRUE)observed_data(list(
 data=list(id='in',name='Incoming',role='centrocampista',team='Barcelona',logo='barcelona.png'),
 championship=list(owner=list(`_id`=owner),clause=list(transferred=transferred,transferDate=date))),observed_at=now)
summaries<-list(`in`=summary(),out=summary('mine'))
result<-reconcile_pending_clause_transfers(roster,tx,summaries,'mine',now)
check('incoming/outgoing reservations deduplicate and retain authoritative arrival',{
 stopifnot(result$status=='ok',result$pending_inbound_count==1L,result$pending_outbound_count==1L,
 nrow(result$incoming_records)==1L,result$incoming_records$player_id=='in',
 result$incoming_records$joins_at==fm_time('2026-09-19T22:00:00Z'),result$incoming_records$seller_team_id=='rival')
})
check('partial summary failure retains successful incoming rows and fails capacity closed',{
 t<-rbind(tx,data.frame(id='bad',player_id='bad',player_name='Failed',buyer_team_id='mine',seller_team_id='rival',created='2026-09-16T13:00:00Z'))
 partial<-reconcile_pending_clause_transfers(roster,t,summaries,'mine',now)
 stopifnot(partial$status=='unavailable',is.na(partial$pending_inbound_count),nrow(partial$incoming_records)==2L,
 !nzchar(partial$incoming_records$timing_error[partial$incoming_records$player_id=='in']),
 nzchar(partial$incoming_records$timing_error[partial$incoming_records$player_id=='bad']))
})
check('overdue arrival remains visible and cannot manufacture roster membership',{
 late<-reconcile_pending_clause_transfers(roster,tx,summaries,'mine',fm_time('2026-09-21T12:00:00Z'))
 stopifnot(late$status=='unavailable',nrow(late$incoming_records)==1L,late$incoming_records$overdue,!('in'%in%roster$id))
})
check('fresh confirmed arrival or cancellation resolves rows; stale roster preserves them',{
 arrived<-reconcile_pending_clause_transfers(rbind(roster,data.frame(id='in')),tx,summaries,'mine',now,previous_records=result$incoming_records)
 cancelled<-reconcile_pending_clause_transfers(roster,tx,list(`in`=summary(transferred=FALSE),out=summaries$out),'mine',now,previous_records=result$incoming_records)
 stale_roster<-roster;attr(stale_roster,'fetch_status')<-'stale'
 retained<-reconcile_pending_clause_transfers(stale_roster,tx,summaries,'mine',now,previous_records=result$incoming_records)
 stopifnot(!nrow(arrived$incoming_records),!nrow(cancelled$incoming_records),nrow(retained$incoming_records)==1L,retained$status=='unavailable')
})
check('malformed transferDate retains earlier schedule with a specific error',{
 invalid<-reconcile_pending_clause_transfers(roster,tx,list(`in`=summary(date='broken'),out=summaries$out),'mine',now,previous_records=result$incoming_records)
 stopifnot(nrow(invalid$incoming_records)==1L,grepl('transferDate',invalid$incoming_records$timing_error),invalid$incoming_records$joins_at==result$incoming_records$joins_at)
})
check('history omission preserves prior purchase until summary resolution; newer sale retires it',{
 omitted<-tx[!tx$player_id%in%'in',,drop=FALSE]
 retained<-reconcile_pending_clause_transfers(roster,omitted,summaries,'mine',now,previous_records=result$incoming_records)
 stopifnot(nrow(retained$incoming_records)==1L,retained$incoming_records$player_id=='in')
 sold<-rbind(omitted,data.frame(id='new-sale',player_id='in',player_name='Incoming',buyer_team_id='rival',seller_team_id='mine',created='2026-09-17T10:00:00Z'))
 stopifnot(!nrow(reconcile_pending_clause_transfers(roster,sold,summaries,'mine',now,previous_records=result$incoming_records)$incoming_records))
})
check('malformed championship shape produces a visible issue, not a crash',{
 malformed<-reconcile_pending_clause_transfers(roster,tx,list(`in`=list(championship='broken'),out=summaries$out),'mine',now)
 stopifnot(nrow(malformed$incoming_records)==1L,nzchar(malformed$incoming_records$timing_error))
})
check('Madrid midnight and daylight-saving boundaries',{
 stopifnot(incoming_clause_time(fm_time('2026-09-19T22:00:00Z'))=='20 Sep 2026, 00:00 CEST',
 incoming_clause_time(fm_time('2026-03-29T00:59:00Z'))=='29 Mar 2026, 01:59 CET',
 incoming_clause_time(fm_time('2026-03-29T01:00:00Z'))=='29 Mar 2026, 03:00 CEST',
 incoming_clause_time(fm_time('2026-10-25T00:30:00Z'))=='25 Oct 2026, 02:30 CEST',
 incoming_clause_time(fm_time('2026-10-25T01:30:00Z'))=='25 Oct 2026, 02:30 CET')
})
check('strict round-start comparison and postponed schedule',{
 rounds<-data.frame(round_number=12,begin_process='2026-09-19T22:00:00Z');next_round<-next_round_context(rounds,now)
 stopifnot(incoming_clause_round_label(next_round$starts_at-1,next_round)=='Before round starts',
 incoming_clause_round_label(next_round$starts_at,next_round)=='At round start',
 incoming_clause_round_label(next_round$starts_at+1,next_round)=='After round starts')
 rounds$begin_process<-'2026-09-20T22:00:00Z'
 stopifnot(incoming_clause_round_label(next_round$starts_at,next_round_context(rounds,now))=='Before round starts')
})
check('safe UI text, abbreviated position, authoritative arrival and separate rows',{
 state<-list(status='ok',records=result$incoming_records,error=NULL);state$records$player_name<-'<script>unsafe</script>'
 html<-as.character(incoming_clause_panel(state,list(available=TRUE,round_number=12,starts_at=fm_time('2026-09-20T22:00:00Z')),identity))
 stopifnot(grepl('Incoming players (1)',html,fixed=TRUE),grepl('&lt;script&gt;',html,fixed=TRUE),
 !grepl('<script>unsafe',html,fixed=TRUE),grepl('MID',html,fixed=TRUE),grepl('20 Sep 2026, 00:00 CEST',html,fixed=TRUE))
})
check('incoming card uses authoritative rival ownership and pending clause instead of profile remnants',{
 card_summary<-summary();card_summary$data$userteamId<-'mine';card_summary$data$owner_team_id<-'mine';card_summary$data$clause_transferred<-FALSE
 card<-incoming_clause_card_snapshot(result$incoming_records,card_summary)
 stopifnot(player_card_owner_id(card)=='rival',!player_card_is_own_player(card,'mine'),isTRUE(card$clause_transferred))
 fallback<-incoming_clause_card_snapshot(result$incoming_records)
 stopifnot(player_card_owner_id(fallback)=='rival',!player_card_is_own_player(fallback,'mine'),isTRUE(fallback$clause_transferred))
 unknown<-result$incoming_records;unknown$seller_team_id<-''
 card_summary$championship$owner<-list()
 for(field in c('owner_team_id','user_team_id','userteamId','userTeamId','userTeam._id','userTeam.id','userteam._id','userteam.id'))card_summary$data[[field]]<-'mine'
 unknown_card<-incoming_clause_card_snapshot(unknown,card_summary)
 stopifnot(player_card_owner_id(unknown_card)=='',!player_card_is_own_player(unknown_card,'mine'))
 empty_profile<-incoming_clause_card_snapshot(result$incoming_records,list(data=list()))
 stopifnot(empty_profile$id=='in',empty_profile$name=='Incoming')
 card_summary$data$id<-'different-player';card_summary$data$name<-'Wrong player'
 card_summary$championship$owner<-list(id='mine',name='Your team')
 card_summary$championship$clause<-list(transferred=FALSE)
 anchored<-incoming_clause_card_snapshot(result$incoming_records,card_summary)
 stopifnot(anchored$id=='in',anchored$name=='Incoming',player_card_owner_id(anchored)=='rival',
   !player_card_is_own_player(anchored,'mine'),isTRUE(anchored$clause_transferred))
})
# Manually completed promises exercise the queue without any network access.
original_adapter<-factual_read_adapter;original_fetch<-async_adapter_fetch
roster_state<-shiny::reactiveVal(resource_state(observed_data(data.frame(id='owned'))))
history_fixture<-tx[tx$buyer_team_id%in%'mine',,drop=FALSE]
for(pid in c('three','four'))history_fixture<-rbind(history_fixture,data.frame(id=pid,player_id=pid,player_name=pid,buyer_team_id='mine',seller_team_id='rival',created='2026-09-16T12:00:00Z'))
history_state<-shiny::reactiveVal(resource_state(observed_data(history_fixture)))
factual_read_adapter<-function(session,handler,...)if(handler=='get_players_from_team')function(...)roster_state()else function(...)history_state()
pending<-list();calls<-0L;maximum<-0L
async_adapter_fetch<-function(fn,args,context,is_current,...){calls<<-calls+1L;promises::promise(function(resolve,reject){pending[[length(pending)+1L]]<<-list(args=args,resolve=resolve,reject=reject,current=is_current);maximum<<-max(maximum,length(pending))})}
settle<-function(session){for(i in seq_len(12L)){later::run_now(0);session$flushReact()}}
complete<-function(fail=FALSE){job<-pending[[1]];pending<<-pending[-1]
 if(fail)job$reject(simpleError('synthetic failure'))else {
  value<-summary();value$data$id<-job$args$player_id
  context<-resource_context(job$args$login,job$args$championship_id,job$args$user_team_id,job$args$player_id)
  key<-api_cache_key(paste0('player_summary_',job$args$login[['userid']],'_',job$args$championship_id,'_',job$args$user_team_id,'_',job$args$player_id),context=context)
  if(job$current())resource_store(key,value,context,'player_summary')
  job$resolve(value)
 }
}
source_test<-function(input,output,session){
 active<-shiny::reactiveVal(TRUE);auth<-shiny::reactiveVal(c(token='synthetic',userid='user'));league<-shiny::reactiveVal('league');team<-shiny::reactiveVal('mine');refresh<-shiny::reactiveVal(0L)
 incoming<-incoming_clause_source(session,auth,league,team,active,refresh)
}
clear_api_cache()
shiny::testServer(source_test,{
 settle(session)
 check('queue starts at most two summary reads',{stopifnot(calls==2L,length(pending)==2L,maximum==2L)})
 complete();settle(session);complete(fail=TRUE);settle(session);complete();settle(session)
 check('partial failure preserves successful rows',{x<-incoming();stopifnot(nrow(x$records)==3L,x$status=='partial',sum(nzchar(x$records$timing_error))==1L,maximum==2L)})
 before<-calls;active(FALSE);settle(session);active(TRUE);settle(session)
 check('cached navigation retries only failed summary',{stopifnot(calls==before+1L,length(pending)==1L)})
 complete();settle(session);before<-calls;active(FALSE);settle(session);active(TRUE);settle(session)
 check('warm navigation makes no summary requests',{stopifnot(calls==before,incoming()$status=='ok')})
 fresh_records<-incoming()$records;roster_state(resource_state(observed_data(data.frame(id=c('owned','in')),status='stale')));settle(session)
 check('stale source cannot remove pending entries',{stopifnot(nrow(incoming()$records)==nrow(fresh_records),incoming()$status=='partial')})
 roster_state(resource_state(observed_data(data.frame(id='owned'))));clear_api_cache();refresh(1L);settle(session)
 check('manual invalidation retries with bounded concurrency',{stopifnot(length(pending)==2L,maximum==2L)})
 bad_history<-history_fixture;bad_history$created[1]<-'malformed';history_state(resource_state(observed_data(bad_history)));settle(session)
 before_bad_callbacks<-calls;complete();settle(session);complete();settle(session)
 check('malformed history cancels queued work and delayed callbacks cannot clear its error',{
  stopifnot(incoming()$status=='partial',grepl('malformed',incoming()$error),calls==before_bad_callbacks,length(pending)==0L,nrow(incoming()$records)==3L)
 })
 history_state(resource_state(observed_data(history_fixture)));settle(session)
 auth(c(token='other',userid='other'));settle(session)
 check('context switch discards previous confirmed arrivals',{stopifnot(all(nzchar(incoming()$records$timing_error)),maximum<=2L)})
 while(length(pending)){complete();settle(session)}
 check('old callbacks cannot publish into new account',{stopifnot(incoming()$status=='ok',maximum<=2L)})
})
factual_read_adapter<-original_adapter;async_adapter_fetch<-original_fetch
cat(checks,'incoming clause checks passed\n')
