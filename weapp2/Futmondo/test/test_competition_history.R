#!/usr/bin/env Rscript
# Synthetic fixtures only: no source accounts, HTTP or live persistence.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(label,expr){force(expr);checks<<-checks+1L;cat('PASS',label,'\n')}
settle<-function(session){for(i in seq_len(25L)){later::run_now(0);session$flushReact()}}
auth<-c(userid='competition-history-fixture',token='synthetic')
score<-fm_scoring_version('fixture',NULL)
ctx<-resource_context(auth,'fantasy','team','player',season='S1',scoring_version=score,competition_id='domestic')
row<-function(competition='domestic',season='S1',player='player',round=1,points=3,id='round-1')data.frame(
 player_id=player,championship_id='fantasy',competition_id=competition,season=season,scoring_version=score,
 round=round,round_id=id,source_round_key=paste0('id:',id),match_id=paste0('match-',id),points=points,
 score_status='final',observed_at='2026-10-01T00:00:00Z',occurred_at=NA_character_,round_start_at=NA_character_)
check('same source ID remains separate in simultaneous domestic and European contexts',{
 facts<-dplyr::bind_rows(row(),row('european',points=-2),row(season='S0',points=20))
 domestic<-reconcile_player_points(facts,data.frame(),ctx)
 european<-ctx;european$competition_id<-'european'
 stopifnot(identical(domestic$points,3),identical(reconcile_player_points(facts,data.frame(),european)$points,-2),
   identical(domestic$round_id,'round-1'),identical(domestic$match_id,'match-round-1'))
})
check('a changed source ID never inherits another player record or old-season scores',{
 changed<-ctx;changed$player_id<-'new-source-id'
 stopifnot(nrow(reconcile_player_points(row(),data.frame(),changed))==0L)
 next_season<-ctx;next_season$season<-'S2'
 stopifnot(nrow(reconcile_player_points(row(),data.frame(),next_season))==0L)
})
check('unknown legacy context cannot satisfy a resolved competition',{
 old<-row();old$competition_id<-'unknown';old$season<-'unknown'
 stopifnot(nrow(history_context_filter(old,ctx))==0L,
   identical(history_context_filter(row(),ctx)$player_id,'player'))
})
check('different official rounds sharing a number remain explicit ambiguity',{
 facts<-dplyr::bind_rows(row(id='group-round-1'),row(id='knockout-round-1',points=8))
 error<-tryCatch({reconcile_player_points(facts,data.frame(),ctx);NULL},error=identity)
 stopifnot(inherits(error,'error'),grepl('official round identities',conditionMessage(error),fixed=TRUE),
   nrow(facts)==2L,length(unique(facts$source_round_key))==2L)
})
check('transferred player fixtures cannot attach to the selected old competition',{
 points<-reconcile_player_points(row(),data.frame(),ctx);points$source<-'futmondo'
 fixtures<-data.frame(round=1,competition_id='european',season='S1',round_id='round-1',match_id='match-round-1')
 result<-build_player_recent_rounds(points,fixtures,ctx,current_player_responses=TRUE)
 stopifnot(nrow(result$rounds[[1]]$fixtures)==0L,nrow(result$unassociated)==1L)
 fixtures$competition_id<-'unknown';fixtures$round_id<-'';fixtures$match_id<-''
 stopifnot(nrow(build_player_recent_rounds(points,fixtures,ctx,current_player_responses=TRUE)$rounds[[1]]$fixtures)==0L)
})
check('card histories scope competition and season before pagination',{
 original<-read_history_incremental;captured<-list()
 read_history_incremental<-function(table,query,...){captured[[table]]<<-query;data.frame()}
 read_card_round_history(ctx);read_card_price_history(ctx)
 for(query in captured)stopifnot(query$competition_id=='eq.domestic',query$season=='eq.S1',query$championship_id=='eq.fantasy')
 stopifnot(grepl('source_round_key',captured$player_match_observations$select,fixed=TRUE),
   grepl('round_id',captured$player_match_observations$select,fixed=TRUE))
 read_history_incremental<-original
})
check('composite keyset pagination retains same-number source round boundaries',{
 original<-supabase_get;requests<-list()
 facts<-dplyr::bind_rows(row(id='a'),row(id='b'));facts<-facts[order(facts$source_round_key),]
 supabase_get<-function(table,query){requests[[length(requests)+1L]]<<-query
  if(length(requests)<=2L)facts[length(requests),,drop=FALSE]else facts[FALSE,,drop=FALSE]
 }
 result<-history_read_pages('fixture',list(order='round.asc,source_round_key.asc,observed_at.asc'),max_rows=10L,page_size=1L,cursor_fields=c('round','source_round_key','observed_at'))
 stopifnot(nrow(result)==2L,isTRUE(attr(result,'complete')),length(requests)==3L,
   identical(requests[[2]]$or,'(round.gt.1,and(round.eq.1,source_round_key.gt.id:a),and(round.eq.1,source_round_key.eq.id:a,observed_at.gt.2026-10-01T00:00:00Z))'))
 supabase_get<-original
})

metadata<-function(competition='domestic',season='S1',mode='fixture',custom=NULL){
 now<-Sys.time()
 remember_league_context(list(account_id=auth[['userid']],championship_id='fantasy',competition_id=competition,
  season=season,scoring_version=fm_scoring_version(mode,custom),observed_at=format(now,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),
  expires_at=format(now+300,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),complete=TRUE,
  payload=list(version=2L,point_system=mode,custom=custom,source='synthetic-fixture')))
}
clear_league_contexts(auth[['userid']]);metadata()
held_lineups<-list();held_matrices<-list();calls<-list();hold_lineup<-TRUE;hold_matrix<-FALSE
fixture_dataset<-function(context){
 points<-row(competition=context$competition_id,season=context$season,points=if(context$season=='S2')8 else 3)
 points$scoring_version<-context$scoring_version
 checks<-data.frame(player_id='player',championship_id='fantasy',competition_id=context$competition_id,
  season=context$season,scoring_version=context$scoring_version,checked_at='2026-10-01T00:00:00Z',result_status='scores',point_count=1L)
 list(points=points,checks=checks,complete=TRUE)
}
original_adapter<-async_adapter_fetch
held_contexts<-list();hold_context<-FALSE;resolver_context<-NULL
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE){
 calls[[length(calls)+1L]]<<-list(fn=fn,args=args,context=context)
 if(fn=='get_resolved_league_context'){
  if(hold_context)return(promises::promise(function(resolve,reject)held_contexts[[length(held_contexts)+1L]]<<-list(resolve=resolve,current=is_current)))
  return(promises::promise_resolve(resolver_context))
 }
 if(fn=='get_lineup_from_team'){
  if(hold_lineup)return(promises::promise(function(resolve,reject)held_lineups[[length(held_lineups)+1L]]<<-list(resolve=resolve,current=is_current,context=context)))
  return(promises::promise_resolve(list(pointSystem='fixture')))
 }
 if(fn=='read_league_round_dataset'){
  stopifnot(args$context$scoring_version==score,args$context$competition_id!='unknown',args$context$season!='unknown')
  if(hold_matrix)return(promises::promise(function(resolve,reject)held_matrices[[length(held_matrices)+1L]]<<-list(resolve=resolve,current=is_current,context=args$context)))
  return(promises::promise_resolve(fixture_dataset(args$context)))
 }
 if(fn=='get_championship_players')return(promises::promise_resolve(data.frame(id='player')))
 stop(paste('Unexpected source acquisition',fn))
}
server<-function(input,output,session){
 active<-reactiveVal(TRUE)
 source<-recent_player_points_source(session,reactive(auth),reactive('fantasy'),reactive('team'),reactive(data.frame(id='player')),active,catalog_scope=FALSE)
}
shiny::testServer(server,{
 settle(session)
 check('scoring bootstrap completes before the first scoped matrix query',{
  stopifnot(length(held_lineups)==1L,!any(vapply(calls,function(x)x$fn=='read_league_round_dataset',logical(1))))
  heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,0);settle(session);stopifnot(heartbeat)
  held_lineups[[1]]$resolve(list(pointSystem='fixture'));settle(session)
  matrix<-Filter(function(x)x$fn=='read_league_round_dataset',calls)
  stopifnot(length(matrix)==1L,matrix[[1]]$args$context$season=='S1',matrix[[1]]$args$context$competition_id=='domestic',
   source$status()$completed==1L,identical(source$history()$points,3))
 })
 check('warm hide and reopen reuses the same complete context without reads',{
  before<-length(calls);active(FALSE);settle(session);active(TRUE);settle(session)
  stopifnot(length(calls)==before,identical(source$history()$points,3))
 })
})

clear_league_contexts(auth[['userid']]);metadata();held_lineups<-held_matrices<-calls<-list()
hold_lineup<-FALSE;hold_matrix<-TRUE
shiny::testServer(server,{
 settle(session);stopifnot(length(held_matrices)==1L)
 old<-held_matrices[[1]]
 metadata(season='S2');settle(session)
 check('season rollover changes generation and rejects a late old matrix',{
  stopifnot(length(held_matrices)==2L,!old$current(),nrow(source$history())==0L)
  old$resolve(fixture_dataset(old$context));settle(session)
  stopifnot(nrow(source$history())==0L)
  fresh<-held_matrices[[2]];fresh$resolve(fixture_dataset(fresh$context));settle(session)
  stopifnot(identical(source$history()$points,8),all(source$history()$season=='S2'),source$status()$completed==1L)
 })
})

clear_league_contexts(auth[['userid']]);metadata();held_lineups<-held_matrices<-calls<-list()
hold_lineup<-TRUE;hold_matrix<-FALSE
shiny::testServer(server,{
 settle(session);old<-held_lineups[[1]]
 metadata(competition='european');settle(session)
 check('late lineup after competition switch cannot start an obsolete matrix query',{
  stopifnot(length(held_lineups)==2L,!old$current())
  old$resolve(list(pointSystem='fixture'));settle(session)
  stopifnot(!any(vapply(calls,function(x)x$fn=='read_league_round_dataset',logical(1))))
  held_lineups[[2]]$resolve(list(pointSystem='fixture'));settle(session)
  stopifnot(all(source$history()$competition_id=='european'),source$status()$completed==1L)
 })
})
# Actual server sessions opt into authenticated resolver bootstrap. Expired
# memory metadata cannot cause a database read of the unknown archive.
production_server<-function(input,output,session){
 session$userData$league_context_enabled<-TRUE
 active<-reactiveVal(TRUE)
 source<-recent_player_points_source(session,reactive(auth),reactive('fantasy'),reactive('team'),reactive(data.frame(id='player')),active,catalog_scope=FALSE)
}
clear_league_contexts(auth[['userid']]);metadata();held_lineups<-held_matrices<-held_contexts<-calls<-list()
old_key<-fm_league_context_key(auth[['userid']],'fantasy')
.fm_league_contexts$items[[old_key]]$expires_at<-format(Sys.time()-1,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC')
hold_context<-TRUE;hold_lineup<-hold_matrix<-FALSE
shiny::testServer(production_server,{
 settle(session)
 check('expired metadata resolves before history and reuses known scoring without lineup',{
  stopifnot(length(held_contexts)==1L,length(calls)==1L,calls[[1]]$fn=='get_resolved_league_context',nrow(source$history())==0L)
  metadata()
  fresh<-resource_context(auth,'fantasy','team',season='S1',scoring_version=score,competition_id='domestic')
  held_contexts[[1]]$resolve(fresh);settle(session)
  stopifnot(identical(source$history()$points,3),all(source$history()$season=='S1'),
    sum(vapply(calls,function(x)x$fn=='get_lineup_from_team',logical(1)))==0L,
    sum(vapply(calls,function(x)x$fn=='read_league_round_dataset',logical(1)))==1L,
    all(vapply(Filter(function(x)x$fn=='read_league_round_dataset',calls),function(x)x$args$context$competition_id=='domestic',logical(1))))
 })
 check('resolved complete context reopens without another resolver or source read',{
  before<-length(calls);active(FALSE);settle(session);active(TRUE);settle(session)
  stopifnot(length(calls)==before,source$status()$completed==1L)
 })
})
clear_league_contexts(auth[['userid']]);metadata(mode=NULL)
held_contexts<-held_lineups<-held_matrices<-calls<-list();hold_lineup<-TRUE
shiny::testServer(production_server,{
 settle(session)
 held_contexts[[1]]$resolve(resource_context(auth,'fantasy','team'));settle(session)
 check('unresolved scoring acquires exactly one lineup before the scoped matrix',{
  stopifnot(length(held_lineups)==1L,
    sum(vapply(calls,function(x)x$fn=='get_lineup_from_team',logical(1)))==1L,
    !any(vapply(calls,function(x)x$fn=='read_league_round_dataset',logical(1))))
  held_lineups[[1]]$resolve(list(pointSystem='fixture'));settle(session)
  stopifnot(identical(source$history()$points,3),
    sum(vapply(calls,function(x)x$fn=='get_lineup_from_team',logical(1)))==1L,
    sum(vapply(calls,function(x)x$fn=='read_league_round_dataset',logical(1)))==1L)
 })
})
hold_lineup<-FALSE
clear_league_contexts(auth[['userid']]);held_contexts<-calls<-list()
shiny::testServer(production_server,{
 settle(session)
 held_contexts[[1]]$resolve(resource_context(auth,'fantasy','team'));settle(session)
 check('unresolved source context exposes failure without querying legacy archives',{
  stopifnot(length(calls)==1L,calls[[1]]$fn=='get_resolved_league_context',
   'context'%in%names(source$status()$initial_errors),source$status()$pending==0L,nrow(source$history())==0L)
 })
})
async_adapter_fetch<-original_adapter
clear_league_contexts(auth[['userid']])
cat(sprintf('COMPETITION HISTORY: %d checks passed.\n',checks))
