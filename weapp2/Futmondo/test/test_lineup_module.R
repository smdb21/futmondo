#!/usr/bin/env Rscript
# Security-sensitive save lifecycle with synthetic promises and blocked I/O.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
roster<-data.frame(id=paste0('p',1:20),name=paste('Player',1:20),role=c(rep('portero',2),rep('defensa',6),rep('centrocampista',7),rep('delantero',5)),role2='',status='ok',buyPrice=1:20,value=21:40,photo='',club='Club')
rounds<-data.frame(round_id=paste0('r',1:7),round_number=1:7,begin_process=format(Sys.time()+c(-6:-1*86400,86400),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),is_finished=c(rep(TRUE,6),FALSE))
records<-lapply(seq_len(nrow(roster)),function(i)as.list(roster[i,,drop=FALSE]))
fixture<-function(){list(version=1L,roster=records,official=lineup_empty('4-4-2'),
 rules=list(allowed=lineup_formations(),multiposition=TRUE,bench_enabled=TRUE,captain_enabled=TRUE),
 source=list(wire=list(strategy='4-4-2',players=list()),locked=list(),status='',block_delayed=FALSE,target_id='r7',target_number=7),
 season='unknown',scoring_version='fixture',observed_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),expires_at=format(Sys.time()+30,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))}
history<-data.frame(player_id=rep(roster$id,each=6),round=rep(1:6,nrow(roster)),points=rep(1:20,each=6))
full_history<-history
history_data<-shiny::reactiveVal(history)
default_status<-list(pending=0L,remaining=0L,saving=0L,failed=0L)
history_status<-shiny::reactiveVal(default_status)
recent_player_points_source<-function(...)list(history=shiny::reactive(history_data()),status=shiny::reactive(history_status()))
lineup_snapshot_store<-function(context,target_round,payload=NULL,owner='')promises::promise_resolve(if(is.null(payload))NULL else TRUE)
state<-new.env();reset<-function(mode='success'){state$mode<-mode;state$reads<-0L;state$submits<-0L;state$optimizes<-0L;state$official<-fixture();state$pending<-NULL;history_data(history);history_status(default_status)}
optimize_impl<-lineup_optimize
lineup_optimize<-function(...){
 state$optimizes<-state$optimizes+1L
 if(state$mode=='optimize-error')stop('synthetic optimizer error')
 optimize_impl(...)
}
lineup_fetch_bundle<-function(auth,context,target,owner){
 state$reads<-state$reads+1L
 if(state$mode=='conflict'&&state$reads==2L){state$official$official$starters[11]<-'p1';state$official$source$wire$players<-list(list(`_id`='p1',position=10L,mp=FALSE,pr=1,b=FALSE,cpt=FALSE))}
 if(state$mode=='initial-delay'&&state$reads==1L)return(promises::promise(function(resolve,reject)state$pending<-resolve))
 if(state$mode=='delay'&&state$reads==2L)return(promises::promise(function(resolve,reject)state$pending<-resolve))
 if(state$mode=='readback-fail'&&state$reads>=3L)return(promises::promise_reject(simpleError('synthetic readback failure')))
 promises::promise_resolve(state$official)
}
lineup_submit<-function(auth,championship_id,user_team_id,current,modified,owner,changes=list()){
 state$submits<-state$submits+1L
 value<-lineup_empty(modified$strategy)
 for(player in modified$players){kind<-if(isTRUE(player$b))'bench'else'starters';value[[kind]][player$position+1L]<-player$`_id`;if(isTRUE(player$cpt))value$captain<-player$`_id`}
 state$official$official<-value;state$official$source$wire<-modified
 if(state$mode=='timeout')return(promises::promise_reject(simpleError('synthetic uncertain mutation timeout')))
 promises::promise_resolve(list(answer=list(code=API_CODE_OK)))
}
settle<-function(session){for(i in seq_len(30)){later::run_now(0);session$flushReact()}}
args<-list(is_module_active=function()TRUE,login_token=function()c(userid='fixture',token='synthetic'),championship_id=function()'league',user_team_id=function()'team',round_catalog=function()rounds)
reset()
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(formation='4-4-2');settle(session)
 session$setInputs(best=1);settle(session);optimized<-draft()
 # Four forwards have the largest observed averages in this fixture, so the
 # unrestricted optimum is 3-3-4 rather than the currently selected 4-4-2.
 stopifnot(optimized$strategy=='3-3-4',
  grepl('<option value="3-3-4" selected="selected">',output$formation_control$html,fixed=TRUE))
 settle(session);stopifnot(identical(draft(),optimized))
 session$setInputs(formation='3-3-4');settle(session)
 stopifnot(identical(draft(),optimized),is.null(best_request()),state$optimizes==1L,state$reads==1L,state$submits==0L,
  grepl('<option value="3-3-4" selected="selected">',output$formation_control$html,fixed=TRUE))
 cat('PASS Best XI changes formation, renders its selection, and retains the draft on reactive flush and input acknowledgement\n')
})
reset('conflict')
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session);wanted<-draft();session$setInputs(save=1);settle(session)
 stopifnot(state$submits==0L,!saving(),identical(draft(),wanted),grepl('another session',notice()))
 cat('PASS external official change prevents mutation and retains draft\n')
})
reset('timeout')
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session);session$setInputs(save=1);settle(session)
 stopifnot(state$submits==1L,state$reads==3L,!saving(),grepl('Saved to Futmondo and verified',notice()))
 cat('PASS uncertain mutation is read back and never automatically replayed\n')
})
reset('readback-fail')
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session);wanted<-draft();session$setInputs(save=1);settle(session)
 stopifnot(state$submits==1L,state$reads==3L,!saving(),identical(draft(),wanted),grepl('readback failed',notice()))
 cat('PASS failed readback settles saving and retains unsaved draft\n')
})
reset('delay')
who<-shiny::reactiveVal(c(userid='fixture',token='synthetic'))
args$login_token<-who
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session);session$setInputs(save=1);settle(session)
 stopifnot(saving(),is.function(state$pending));held<-state$pending
 who(c(userid='different',token='other-synthetic'));settle(session)
 state$official$official$starters[11]<-'p2';held(state$official);settle(session)
 stopifnot(state$submits==0L,!saving(),!identical(draft()$starters[11],'p2'))
 cat('PASS changed account invalidates pending preflight without mutation or stale publication\n')
})
reset('initial-delay')
who<-shiny::reactiveVal(c(userid='fixture',token='synthetic'));args$login_token<-who
shiny::testServer(lineup_Server,args=args,{
 settle(session);stopifnot(is.null(draft()),is.function(state$pending));old<-state$pending
 who(c(userid='different',token='other-synthetic'));settle(session)
 who(c(userid='fixture',token='synthetic'));settle(session)
 stopifnot(state$reads==3L,!is.null(draft()),!loading())
 saved<-draft();obsolete<-fixture();obsolete$official$starters[11]<-'p2';old(obsolete);settle(session)
 stopifnot(identical(draft(),saved),!loading())
 cat('PASS returning to a context restarts its pending source load and discards obsolete completion\n')
})
reset()
partial<-rounds;attr(partial,'catalog_complete')<-FALSE
attr(partial,'catalog_reason')<-'Numbered calendar read failed.'
catalog<-shiny::reactiveVal(partial);args$round_catalog<-catalog
shiny::testServer(lineup_Server,args=args,{
 settle(session);before<-draft();session$setInputs(best=1);settle(session)
 stopifnot(identical(draft(),before),all(is.na(scores()$average)),!length(wanted()),
  grepl('Numbered calendar read failed.',output$status$html,fixed=TRUE),!grepl('0 completed rounds',output$status$html),state$submits==0L)
 stopifnot(grepl('Best XI is waiting',notice()))
 catalog(rounds);settle(session)
 stopifnot(all(is.finite(scores()$average)),identical(wanted(),as.numeric(4:6)),
  all(nzchar(draft()$starters)),!grepl('not verified',output$status$html),state$submits==0L)
 cat('PASS a queued Best XI reports incomplete calendar and automatically recovers on verified publication\n')
})
# Unknown finality elsewhere does not block known completed scores or add warnings.
reset()
partial<-rounds;partial$is_finished[1]<-NA
attr(partial,'catalog_complete')<-FALSE;attr(partial,'catalog_reason')<-''
args$round_catalog<-function()partial
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session)
 stopifnot(identical(wanted(),as.numeric(4:6)),all(is.finite(scores()$average)),
  all(nzchar(draft()$starters)),!nzchar(catalog_issue()),
  !grepl('not verified|waiting for the completed-round schedule',output$status$html),state$submits==0L)
 cat('PASS unknown finality does not block available completed rounds or display a warning\n')
})
# The two goalkeepers deliberately reverse ranking between the five- and
# two-round windows; no optimizer implementation is used to derive the answer.
args$round_catalog<-function()rounds
reset()
window_history<-history
window_history$points[window_history$player_id=='p1']<-c(0,100,100,100,0,0)
window_history$points[window_history$player_id=='p2']<-c(0,0,0,0,50,50)
history_data(window_history)
shiny::testServer(lineup_Server,args=args,{
 settle(session);before<-draft();session$setInputs(recent_n=2);settle(session)
 stopifnot(identical(draft(),before),state$optimizes==0L,!grepl('fm-lineup-best-warning',output$status$html))
 busy_flushed<-FALSE
 session$onFlushed(function(){shiny::isolate({
  if(!is.null(best_request())&&grepl('Calculating Best XI',notice()))busy_flushed<<-TRUE
 })},once=FALSE)
 session$setInputs(best=1,recent_n=5)
 settle(session)
 stopifnot(draft()$starters[11]=='p1',scores()$average[scores()$id=='p1']==60,
  grepl('rounds 2, 3, 4, 5, 6',best_note(),fixed=TRUE),is.null(best_request()),busy_flushed)
 five_round_draft<-draft()
 session$setInputs(recent_n=2);settle(session)
 stopifnot(identical(draft(),five_round_draft),state$optimizes==1L,scores()$average[scores()$id=='p2']==50,
  identical(wanted(),as.numeric(5:6)),grepl('fm-lineup-best-warning',output$status$html),
  grepl('last 5 completed rounds',output$status$html,fixed=TRUE),grepl('current settings (last 2',output$status$html,fixed=TRUE))
 session$setInputs(recent_n=5);settle(session)
 stopifnot(identical(draft(),five_round_draft),state$optimizes==1L,!grepl('fm-lineup-best-warning',output$status$html))
 session$setInputs(recent_n=2);settle(session)
 session$setInputs(best=2);settle(session)
 stopifnot(draft()$starters[11]=='p2',state$optimizes==2L,grepl('rounds 5, 6',best_note(),fixed=TRUE),
  !grepl('fm-lineup-best-warning',output$status$html),grepl('aria-busy="false"',output$status$html,fixed=TRUE))
 two_round_draft<-draft()
 session$setInputs(include_unavailable=TRUE);settle(session)
 stopifnot(identical(draft(),two_round_draft),state$optimizes==2L,grepl('fm-lineup-best-warning',output$status$html))
 session$setInputs(include_unavailable=FALSE);settle(session)
 stopifnot(!grepl('fm-lineup-best-warning',output$status$html))
 # Later history publication also updates only averages, without an optimizer run.
 revised<-window_history;revised$points[revised$player_id=='p1']<-1000;history_data(revised);settle(session)
 stopifnot(identical(draft(),two_round_draft),state$optimizes==2L)
 session$setInputs(clear=1);settle(session);session$setInputs(recent_n=5);settle(session)
 stopifnot(all(!nzchar(draft()$starters)),is.null(best_request()),!grepl('fm-lineup-best-warning',output$status$html),state$reads==1L,state$submits==0L)
 cat('PASS N changes update only averages and warn; one explicit click flushes progress then changes the known-answer goalkeeper once\n')
})
reset()
partial_history<-history[history$player_id!='p20',]
history_data(partial_history);history_status(list(pending=1L,remaining=0L,saving=0L,failed=0L))
shiny::testServer(lineup_Server,args=args,{
 settle(session);before<-draft();session$setInputs(best=1);settle(session)
 stopifnot(identical(draft(),before),grepl('waiting for complete scores for 1 eligible player',notice()),!nzchar(best_note()))
 # Even though a complete XI could be formed, it must not silently exclude p20.
 history_data(full_history);history_status(default_status);settle(session)
 stopifnot('p20'%in%draft()$starters,all(nzchar(draft()$starters)),nzchar(best_note()),state$reads==1L)
 cat('PASS one Best XI click waits for all eligible histories and applies the full result when acquisition finishes\n')
})
reset()
history_data(partial_history);history_status(list(pending=1L,remaining=0L,saving=0L,failed=0L))
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session);session$setInputs(reset=1);settle(session)
 restored<-draft();history_data(full_history);history_status(default_status);settle(session)
 stopifnot(identical(draft(),restored),identical(draft(),source()$official),is.null(best_request()),
  grepl('Changes undone',notice()))
 cat('PASS Undo changes cancels a queued calculation before late histories can overwrite the restored lineup\n')
})
reset()
history_data(partial_history);history_status(list(pending=1L,remaining=0L,saving=0L,failed=0L))
shiny::testServer(lineup_Server,args=args,{
 settle(session);before<-draft();session$setInputs(best=1);settle(session)
 stopifnot(!is.null(best_request()),grepl('aria-busy="true"',output$status$html,fixed=TRUE))
 session$setInputs(recent_n=2);settle(session)
 stopifnot(is.null(best_request()),grepl('cancelled because the settings changed',notice()))
 history_data(full_history);history_status(default_status);settle(session)
 stopifnot(identical(draft(),before),state$optimizes==0L,grepl('aria-busy="false"',output$status$html,fixed=TRUE))
 session$onFlushed(function(){shiny::isolate(edit(lineup_clear(draft())))},once=TRUE)
 session$setInputs(best=2);settle(session)
 stopifnot(all(!nzchar(draft()$starters)),state$optimizes==0L,is.null(best_request()))
 cat('PASS changed settings and manual edits cancel pending/deferred work before late history or optimizer publication\n')
})
reset()
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session);session$setInputs(save=1);settle(session)
 saved<-draft();stopifnot(state$submits==1L,all(nzchar(saved$starters)),is.null(best_request()))
 session$setInputs(recent_n=2);settle(session)
 stopifnot(identical(draft(),saved),grepl('fm-lineup-best-warning',output$status$html),state$optimizes==1L)
 session$setInputs(clear=1);settle(session);session$setInputs(reset=1);settle(session)
 stopifnot(identical(draft(),saved),!dirty(),grepl('successfully saved to Futmondo',notice()),
  grepl('Undo changes',as.character(lineup_UI('fixture')),fixed=TRUE))
 cat('PASS Undo changes restores the latest successfully saved and verified lineup\n')
})
# Local picker edits must not trigger source reads or write an incomplete XI.
reset()
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session)
 original<-draft();value<-original;value$captain<-value$starters[1];draft(value)
 session$setInputs(slot_click=slot_event(FALSE,1L));settle(session);pick<-selected()
 session$setInputs(player_choice=list(id='',picker=pick$sequence));settle(session)
 stopifnot(draft()$starters[1]=='',draft()$captain=='',is.null(selected()),is.null(best_request()),
  identical(draft()$starters[-1],original$starters[-1]),identical(draft()$bench,original$bench),
  grepl('Select all eleven starters',output$save_control$html),state$reads==1L,state$submits==0L)
 session$setInputs(slot_click=slot_event(FALSE,1L));settle(session);pick<-selected()
 session$setInputs(player_choice=list(id=original$starters[1],picker=pick$sequence));settle(session)
 stopifnot(identical(draft()$starters,original$starters),is.null(selected()))
 session$setInputs(slot_click=slot_event(TRUE,1L));settle(session);pick<-selected()
 session$setInputs(player_choice=list(id='',picker=pick$sequence));settle(session)
 stopifnot(draft()$bench[1]=='',identical(draft()$bench[-1],original$bench[-1]),state$reads==1L,state$submits==0L)
 cat('PASS one-click starter/bench edits and removal leave other slots intact, clear removed captain, and make no source requests\n')
})
reset()
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session);original<-draft()
 session$setInputs(slot_click=slot_event(FALSE,1L));settle(session);old<-selected()
 session$setInputs(slot_click=slot_event(FALSE,2L));settle(session);pick<-selected()
 session$setInputs(player_choice=list(id='',picker=old$sequence));settle(session)
 stopifnot(identical(draft(),original),identical(selected(),pick))
 session$setInputs(player_choice=list(id='p1',picker=pick$sequence));settle(session)
 stopifnot(identical(draft(),original),!is.null(selected()),grepl('cannot play',output$picker_error$html))
 session$setInputs(player_choice=list(id=original$starters[1],picker=pick$sequence));settle(session)
 stopifnot(identical(draft()$starters[1:2],rev(original$starters[1:2])),is.null(selected()),
  !anyDuplicated(c(draft()$starters,draft()$bench)),state$reads==1L,state$submits==0L)
 cat('PASS obsolete picker clicks are ignored; invalid roles retain the draft; eligible selections swap immediately\n')
})
reset()
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session)
 optimized<-draft();event<-slot_event(TRUE,1L)
 session$setInputs(slot_click=event);settle(session)
 html<-xml2::read_html(output$picker_players$html)
 membership<-function(id)xml2::xml_attr(xml2::xml_find_first(html,paste0('//button[@data-player-id="',id,'"]')),'data-lineup-membership')
 stopifnot(membership(optimized$starters[1])=='starter',membership(optimized$bench[1])=='bench',
  grepl('including unsaved changes',output$picker_players$html,fixed=TRUE),
  grepl('Current slot',output$picker_players$html,fixed=TRUE),all(!nzchar(source()$official$starters)))
 markers<-xml2::xml_find_all(html,'//button//span[@class="fm-lineup-choice-membership"]')
 stopifnot(length(markers)==15L,
  all(nzchar(xml2::xml_attr(markers,'aria-label'))),all(trimws(xml2::xml_text(markers))==''),
  length(xml2::xml_find_all(markers,'.//i[contains(@class,"fa-crosshairs")]'))==1L)
 # A new local assignment must immediately change membership without a source read.
 replacement<-setdiff(roster()$id,c(optimized$starters,optimized$bench))[1]
 session$setInputs(player_choice=list(id=replacement,picker=selected()$sequence));settle(session)
 session$setInputs(slot_click=event);settle(session)
 html<-xml2::read_html(output$picker_players$html)
 stopifnot(membership(replacement)=='bench',membership(optimized$bench[1])=='none',state$reads==1L,state$submits==0L)
 previous<-selected()
 session$setInputs(slot_click=utils::modifyList(event,list(generation=event$generation-1L)));settle(session)
 stopifnot(identical(selected(),previous))
 session$setInputs(slot_click=utils::modifyList(event,list(strategy='obsolete')));settle(session)
 stopifnot(identical(selected(),previous))
 session$setInputs(slot_click=utils::modifyList(event,list(index=99L)));settle(session)
 stopifnot(identical(selected(),previous))
 session$setInputs(reset=1);settle(session);session$setInputs(slot_click=event);settle(session)
 # Undo restores the empty official fixture; a matching refreshed event opens it.
 session$setInputs(slot_click=utils::modifyList(event,list(strategy=draft()$strategy)));settle(session)
 html<-xml2::read_html(output$picker_players$html)
 stopifnot(all(xml2::xml_attr(xml2::xml_find_all(html,'//button[@data-lineup-membership]'),'data-lineup-membership')=='none'),
  state$reads==1L,state$submits==0L)
 cat('PASS picker membership follows unsaved XI/bench edits and undo; obsolete and invalid slot events are ignored without source reads\n')
})
for(mode in c('optimize-error','no-xi')){
 reset(mode)
 if(mode=='no-xi')state$official$rules$allowed<-character()
 shiny::testServer(lineup_Server,args=args,{
  settle(session);before<-draft();session$setInputs(best=1);settle(session)
  stopifnot(identical(draft(),before),is.null(best_request()),state$optimizes==1L,
   grepl(if(mode=='no-xi')'No eligible XI'else'calculation failed',notice()),
   grepl('aria-busy="false"',output$status$html,fixed=TRUE),state$reads==1L,state$submits==0L)
  cat('PASS',mode,'retains the lineup and clears calculation feedback\n')
 })
}
cat('LINEUP MODULE: 18 groups passed / 0 failed\n')
