options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(label,code){force(code);checks<<-checks+1L;cat('[PASS]',label,'\n')}
now<-fm_time('2026-11-10T12:00:00Z')
leaves_at<-fm_time('2026-11-14T23:00:00Z')
roster<-data.frame(id=c('out','ordinary','owned'))
tx<-data.frame(id=c('old-buy','clause-sale','duplicate','ordinary-sale','incoming'),
  player_id=c('out','out','out','ordinary','in'),
  player_name=c('Leaving','Leaving','Leaving','Ordinary','Incoming'),
  buyer_team_id=c('mine','rival','rival','rival','mine'),
  seller_team_id=c(NA,'mine','mine','mine','rival'),
  created=c('2026-11-01T12:00:00Z',rep('2026-11-09T12:00:00Z',4)),
  is_clause=c(FALSE,TRUE,TRUE,FALSE,TRUE),stringsAsFactors=FALSE)
summary<-function(pid='out',owner='mine',date='2026-11-14T23:00:00Z',transferred=TRUE)observed_data(list(
  data=list(id=pid,name=if(pid=='out')'Leaving'else pid,role='centrocampista',team='Barcelona',logo='barcelona.png'),
  championship=list(owner=list(id=owner),clause=list(transferred=transferred,transferDate=date))),observed_at=now)
summaries<-list(out=summary(),ordinary=summary('ordinary',transferred=FALSE),`in`=summary('in','rival'))
result<-reconcile_pending_clause_transfers(roster,tx[tx$player_id!='ordinary',,drop=FALSE],summaries,'mine',now)
check('paid departures deduplicate and retain the source schedule and buyer',{
  rows<-result$outgoing_records
  stopifnot(nrow(rows)==1L,rows$player_id=='out',rows$buyer_team_id=='rival',rows$seller_team_id=='mine',
    rows$leaves_at==leaves_at,rows$purchased_at==fm_time('2026-11-09T12:00:00Z'),
    !nzchar(rows$timing_error),!rows$overdue,result$pending_outbound_count==1L,
    nrow(result$incoming_records)==1L,result$incoming_records$player_id=='in')
})
check('ordinary sales never become leaving clause signings',{
  ordinary<-reconcile_pending_clause_transfers(roster,tx[tx$player_id=='ordinary',],list(ordinary=summaries$ordinary),'mine',now)
  missing<-reconcile_pending_clause_transfers(roster,tx[tx$player_id=='ordinary',],list(),'mine',now)
  stopifnot(!nrow(ordinary$outgoing_records),!nrow(missing$outgoing_records))
})
check('known paid clauses stay visible while their summary fails',{
  failed<-reconcile_pending_clause_transfers(roster,tx,list(`in`=summaries$`in`,ordinary=summaries$ordinary),'mine',now)
  stopifnot(nrow(failed$outgoing_records)==1L,nzchar(failed$outgoing_records$timing_error),
    is.na(failed$outgoing_records$leaves_at),failed$status=='unavailable',is.na(failed$pending_outbound_count))
  kept<-reconcile_pending_clause_transfers(roster,tx,list(),'mine',now,previous_outgoing_records=result$outgoing_records)
  stopifnot(kept$outgoing_records$leaves_at==leaves_at,nzchar(kept$outgoing_records$timing_error))
})
check('missing or malformed transferDate preserves the last verified departure',{
  for(date in c('',NA_character_,'broken')){
    current<-summaries;current$out<-summary(date=date)
    x<-reconcile_pending_clause_transfers(roster,tx,current,'mine',now,previous_outgoing_records=result$outgoing_records)
    stopifnot(nrow(x$outgoing_records)==1L,x$outgoing_records$leaves_at==leaves_at,
      grepl('transferDate',x$outgoing_records$timing_error,fixed=TRUE))
  }
})
check('malformed or mismatched summaries cannot certify a departing player',{
  for(value in list('broken',list(championship='broken'),summary('different-player'))){
    current<-summaries;current$out<-value
    x<-reconcile_pending_clause_transfers(roster,tx,current,'mine',now,previous_outgoing_records=result$outgoing_records)
    stopifnot(nrow(x$outgoing_records)==1L,x$outgoing_records$player_id=='out',
      x$outgoing_records$leaves_at==leaves_at,nzchar(x$outgoing_records$timing_error),x$outgoing_status=='partial')
  }
})
check('a passed date never removes a player still on the source roster',{
  x<-reconcile_pending_clause_transfers(roster,tx,summaries,'mine',leaves_at+1)
  stopifnot(nrow(x$outgoing_records)==1L,x$outgoing_records$overdue,x$status=='unavailable',
    is.na(x$pending_outbound_count),'out'%in%roster$id)
})
check('fresh departure, changed owner and explicit cancellation retire pending departures',{
  departed<-reconcile_pending_clause_transfers(roster[roster$id!='out',,drop=FALSE],tx,summaries,'mine',now,
    previous_outgoing_records=result$outgoing_records)
  transferred<-summaries;transferred$out<-summary(owner='rival')
  changed_owner<-reconcile_pending_clause_transfers(roster,tx,transferred,'mine',now,
    previous_outgoing_records=result$outgoing_records)
  cancelled<-summaries;cancelled$out<-summary(transferred=FALSE)
  cancelled<-reconcile_pending_clause_transfers(roster,tx,cancelled,'mine',now,
    previous_outgoing_records=result$outgoing_records)
  stopifnot(!nrow(departed$outgoing_records),!nrow(changed_owner$outgoing_records),!nrow(cancelled$outgoing_records))
})
check('stale prerequisites cannot erase a confirmed leaving player',{
  for(source in c('roster','history')){
    r<-roster;h<-tx
    if(source=='roster'){r<-r[r$id!='out',,drop=FALSE];attr(r,'fetch_status')<-'stale'}else {
      h<-h[h$player_id!='out',,drop=FALSE];attr(h,'fetch_status')<-'partial'
    }
    x<-reconcile_pending_clause_transfers(r,h,summaries,'mine',now,previous_outgoing_records=result$outgoing_records)
    stopifnot(nrow(x$outgoing_records)==1L,x$outgoing_records$leaves_at==leaves_at,x$status=='unavailable')
  }
})
check('history omissions retain departures until a newer confirmed reacquisition',{
  omitted<-tx[tx$player_id!='out',,drop=FALSE]
  kept<-reconcile_pending_clause_transfers(roster,omitted,summaries,'mine',now,previous_outgoing_records=result$outgoing_records)
  stopifnot(nrow(kept$outgoing_records)==1L,kept$outgoing_records$leaves_at==leaves_at)
  purchase<-data.frame(id='reacquired',player_id='out',player_name='Leaving',buyer_team_id='mine',
    seller_team_id='rival',created='2026-11-10T10:00:00Z',is_clause=FALSE)
  reacquired<-reconcile_pending_clause_transfers(roster,rbind(omitted,purchase),summaries,'mine',now,
    previous_outgoing_records=result$outgoing_records)
  stopifnot(!nrow(reacquired$outgoing_records))
})
check('missing seller or buyer IDs preserve System/Market semantics',{
  system<-tx;system$buyer_team_id[system$player_id=='out']<-NA_character_
  x<-reconcile_pending_clause_transfers(roster,system,summaries,'mine',now)
  stopifnot(nrow(x$outgoing_records)==1L,!is.na(x$outgoing_records$buyer_team_id),x$outgoing_records$buyer_team_id=='')
  system$seller_team_id[system$player_id=='out']<-NA_character_
  stopifnot(!nrow(reconcile_pending_clause_transfers(roster,system,summaries,'mine',now)$outgoing_records))
})
check('leaving panel matches incoming timing, roles, safe labels and separate identity',{
  state<-list(status='ok',records=result$outgoing_records,error=NULL)
  state$records$player_name<-'<script>Leaving & Co</script>'
  next_round<-list(available=TRUE,round_number=12,starts_at=leaves_at+1)
  html<-as.character(outgoing_clause_panel(state,next_round,identity))
  stopifnot(grepl('Leaving players (1)',html,fixed=TRUE),grepl('Leaves:',html,fixed=TRUE),
    grepl('15 Nov 2026, 00:00 CET',html,fixed=TRUE),grepl('Before round starts',html,fixed=TRUE),
    grepl('MID',html,fixed=TRUE),grepl('&lt;script&gt;',html,fixed=TRUE),
    !grepl('<script>Leaving',html,fixed=TRUE),grepl('outgoing_open_out',html,fixed=TRUE),
    grepl('fm-outgoing-panel',html,fixed=TRUE))
  next_round$starts_at<-leaves_at
  stopifnot(grepl('At round start',as.character(outgoing_clause_panel(state,next_round,identity)),fixed=TRUE))
  next_round$starts_at<-leaves_at-1
  stopifnot(grepl('After round starts',as.character(outgoing_clause_panel(state,next_round,identity)),fixed=TRUE))
  empty<-list(status='loading',records=outgoing_clause_empty(),error=NULL)
  stopifnot(!grepl('Leaving players (0)',as.character(outgoing_clause_panel(empty,next_round,identity)),fixed=TRUE))
  empty$status<-'ok'
  stopifnot(grepl('Leaving players (0)',as.character(outgoing_clause_panel(empty,next_round,identity)),fixed=TRUE))
})
check('leaving card uses the current owner and pending clause, including safe fallback',{
  card_summary<-summaries$out;card_summary$data$userteamId<-'rival';card_summary$data$clause_transferred<-FALSE
  card<-outgoing_clause_card_snapshot(result$outgoing_records,card_summary)
  fallback<-outgoing_clause_card_snapshot(result$outgoing_records)
  stopifnot(player_card_owner_id(card)=='mine',player_card_is_own_player(card,'mine'),isTRUE(card$clause_transferred),
    player_card_owner_id(fallback)=='mine',isTRUE(fallback$clause_transferred))
  card_summary$data$id<-'wrong';card_summary$data$name<-'Wrong player'
  anchored<-outgoing_clause_card_snapshot(result$outgoing_records,card_summary)
  stopifnot(anchored$id=='out',anchored$name=='Leaving',player_card_owner_id(anchored)=='mine',isTRUE(anchored$clause_transferred))
})

check('raw cache insertion cannot extend the original source age or absolute deadline',{
  clear_api_cache()
  auth<-c(userid='raw-clause-fixture',token='synthetic')
  context<-resource_context(auth,'league','mine','out')
  key<-api_cache_key('player_summary_raw-clause-fixture_league_mine_out',context=context)
  old<-summary();attr(old,'observed_at')<-Sys.time()-61
  resource_store(key,old,context,'player_summary')
  stopifnot(as.numeric(difftime(Sys.time(),api_cache_env[[key]]$time,units='secs'))<1,
    is.null(incoming_clause_cached_summary(auth,'league','mine','out')))
  expired<-summary();attr(expired,'observed_at')<-Sys.time()
  attr(expired,'valid_until')<-as.numeric(Sys.time())-1
  resource_store(key,expired,context,'player_summary')
  stopifnot(is.null(incoming_clause_cached_summary(auth,'league','mine','out')))
  clear_api_cache()
})

# Complete deferred summary reads manually to measure the joint queue and
# context guards without any account, HTTP, or database mutation.
original_adapter<-factual_read_adapter;original_fetch<-async_adapter_fetch
roster_state<-shiny::reactiveVal(resource_state(observed_data(data.frame(id=c('out','out2','owned')))))
history_fixture<-tx[tx$id%in%c('clause-sale','incoming'),,drop=FALSE]
extra<-history_fixture[history_fixture$player_id=='out',,drop=FALSE];extra$id<-'out2';extra$player_id<-'out2';extra$player_name<-'Leaving Two'
history_fixture<-rbind(history_fixture,extra)
history_state<-shiny::reactiveVal(resource_state(observed_data(history_fixture)))
factual_read_adapter<-function(session,handler,...)if(handler=='get_players_from_team')function(...)roster_state()else function(...)history_state()
pending<-list();calls<-0L;maximum<-0L;handlers<-character()
async_adapter_fetch<-function(fn,args,context,is_current,...){
  stopifnot(fn%in%c('get_clause_transfer_summary','get_player_summary'))
  calls<<-calls+1L;handlers<<-c(handlers,fn)
  promises::promise(function(resolve,reject){pending[[length(pending)+1L]]<<-list(fn=fn,args=args,resolve=resolve,reject=reject,current=is_current);maximum<<-max(maximum,length(pending))})
}
settle<-function(session){for(i in seq_len(12L)){later::run_now(0);session$flushReact()}}
complete<-function(fail=FALSE,expired=FALSE){
  force(fail);force(expired)
  job<-pending[[1L]];pending<<-pending[-1L]
  if(fail)job$reject(simpleError('synthetic outgoing summary failure'))else {
    pid<-job$args$player_id;value<-summary(pid,if(pid=='in')'rival'else job$args$user_team_id)
    attr(value,'observed_at')<-Sys.time()
    if(expired)attr(value,'valid_until')<-as.numeric(Sys.time())-1
    context<-resource_context(job$args$login,job$args$championship_id,job$args$user_team_id,pid)
    key<-api_cache_key(paste0('player_summary_',job$args$login[['userid']],'_',job$args$championship_id,'_',job$args$user_team_id,'_',pid),context=context)
    if(job$current())resource_store(key,value,context,'player_summary')
    job$resolve(value)
  }
}
source_test<-function(input,output,session){
  active<-shiny::reactiveVal(TRUE);auth<-shiny::reactiveVal(c(token='synthetic',userid='outgoing-fixture'))
  league<-shiny::reactiveVal('league');team<-shiny::reactiveVal('mine');refresh<-shiny::reactiveVal(0L)
  transfers<-incoming_clause_source(session,auth,league,team,active,refresh)
}
clear_api_cache()
shiny::testServer(source_test,{
  settle(session)
  check('incoming and leaving share one two-request queue during delayed acquisition',{
    stopifnot(calls==2L,length(pending)==2L,maximum==2L,
      transfers()$outgoing_status=='loading',nrow(transfers()$outgoing_records)==2L)
  })
  while(length(pending)){
    complete(fail=identical(pending[[1L]]$args$player_id,'out2'));settle(session)
  }
  check('one failed departing summary preserves successful departures and arrivals',{
    state<-transfers()
    stopifnot(nrow(state$outgoing_records)==2L,state$outgoing_status=='partial',state$status=='ok',
      nrow(state$records)==1L,sum(nzchar(state$outgoing_records$timing_error))==1L,maximum==2L)
  })
  before<-calls;active(FALSE);settle(session);active(TRUE);settle(session)
  check('navigation retries only the unsuccessful departure summary',{
    stopifnot(calls==before+1L,length(pending)==1L,pending[[1L]]$args$player_id=='out2')
  })
  complete();settle(session);before<-calls;active(FALSE);settle(session);active(TRUE);settle(session)
  check('warm navigation reuses successful incoming and departing summaries',{
    stopifnot(calls==before,transfers()$outgoing_status=='ok',transfers()$status=='ok',maximum==2L)
  })
  roster_state(resource_state(observed_data(data.frame(id='owned'),status='stale')));settle(session)
  check('stale prerequisite preserves both leaving rows with truthful status',{
    stopifnot(nrow(transfers()$outgoing_records)==2L,transfers()$outgoing_status=='partial',nzchar(transfers()$error))
  })
  roster_state(resource_state(observed_data(data.frame(id=c('out','out2','owned')))))
  clear_api_cache();refresh(1L);settle(session)
  auth(c(token='other',userid='another-account'));settle(session)
  check('account switch clears previous departing schedules before old callbacks finish',{
    stopifnot(maximum<=2L,all(nzchar(transfers()$outgoing_records$timing_error)),all(is.na(transfers()$outgoing_records$leaves_at)))
  })
  while(length(pending)){complete();settle(session)}
  check('old callbacks cannot exceed the joint cap or overwrite new-context departures',{
    stopifnot(maximum<=2L,transfers()$outgoing_status=='ok',transfers()$status=='ok')
  })
  uncertain<-history_fixture[history_fixture$player_id=='out',,drop=FALSE];uncertain$is_clause<-NA
  history_state(resource_state(observed_data(uncertain)));clear_api_cache();refresh(2L)
  auth(c(token='uncertain',userid='uncertain-account'));settle(session)
  while(length(pending)){complete(fail=TRUE);settle(session)}
  check('an unverifiable sale cannot become a verified zero leaving count',{
    state<-transfers()
    stopifnot(!nrow(state$outgoing_records),state$outgoing_status=='partial')
    html<-as.character(outgoing_clause_panel(list(status=state$outgoing_status,records=state$outgoing_records,
      error=state$outgoing_error),list(available=FALSE),identity))
    stopifnot(!grepl('Leaving players (0)',html,fixed=TRUE),!grepl('No players leaving',html,fixed=TRUE))
  })
})
# A durable result can cross its deadline while awaiting the response. Keep
# that logical request's slot until one direct factual refresh has settled.
roster_state(resource_state(observed_data(data.frame(id=c('out','out2','owned')))))
history_state(resource_state(observed_data(history_fixture)))
pending<-list();calls<-0L;maximum<-0L;handlers<-character();clear_api_cache()
shiny::testServer(source_test,{
  settle(session)
  expired_pid<-pending[[1L]]$args$player_id
  complete(expired=TRUE);settle(session)
  check('an expired saved callback triggers exactly one direct refresh within the same two slots',{
    rows<-transfers()$outgoing_records
    stopifnot(calls==3L,length(pending)==2L,maximum==2L,
      sum(handlers=='get_player_summary')==1L,
      any(vapply(pending,function(job)job$fn=='get_player_summary'&&job$args$player_id==expired_pid,logical(1))),
      transfers()$outgoing_status=='loading',is.na(rows$leaves_at[rows$player_id==expired_pid]),
      nzchar(rows$timing_error[rows$player_id==expired_pid]))
  })
  while(length(pending)){complete();settle(session)}
  check('fresh direct recheck completes the batch without publishing expired success',{
    rows<-transfers()$outgoing_records
    stopifnot(calls==4L,sum(handlers=='get_player_summary')==1L,maximum==2L,
      transfers()$outgoing_status=='ok',transfers()$status=='ok',
      all(!is.na(rows$leaves_at)),!any(nzchar(rows$timing_error)))
  })
  single<-history_fixture[history_fixture$player_id=='out',,drop=FALSE]
  history_state(resource_state(observed_data(single)));clear_api_cache();refresh(1L)
  auth(c(token='expired',userid='second-expiry-account'));settle(session)
  before<-calls;before_direct<-sum(handlers=='get_player_summary')
  stopifnot(length(pending)==1L,pending[[1L]]$fn=='get_clause_transfer_summary')
  complete(expired=TRUE);settle(session)
  stopifnot(length(pending)==1L,pending[[1L]]$fn=='get_player_summary')
  complete(expired=TRUE);settle(session)
  check('a second expired result remains partial and cannot start an infinite retry',{
    state<-transfers()
    stopifnot(calls==before+1L,sum(handlers=='get_player_summary')==before_direct+1L,
      !length(pending),maximum<=2L,state$outgoing_status=='partial',
      nrow(state$outgoing_records)==1L,is.na(state$outgoing_records$leaves_at),
      nzchar(state$outgoing_records$timing_error))
  })
})
factual_read_adapter<-original_adapter;async_adapter_fetch<-original_fetch
cat(sprintf('Outgoing clause signings: %d checks passed; maximum concurrent incoming/leaving reads: %d.\n',checks,maximum))
