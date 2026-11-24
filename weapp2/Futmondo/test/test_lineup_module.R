#!/usr/bin/env Rscript
# Security-sensitive save lifecycle with synthetic promises and blocked I/O.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
roster<-data.frame(id=paste0('p',1:20),name=paste('Player',1:20),role=c(rep('portero',2),rep('defensa',6),rep('centrocampista',7),rep('delantero',5)),role2='',status='ok',buyPrice=1:20,value=21:40,photo='',club='Club')
rounds<-data.frame(round_id=paste0('r',1:7),round_number=1:7,begin_process=format(Sys.time()+c(-6:-1*86400,86400),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),is_finished=c(rep(TRUE,6),FALSE))
records<-lapply(seq_len(nrow(roster)),function(i)as.list(roster[i,,drop=FALSE]))
fixture<-function(){list(version=2L,roster=records,official=lineup_empty('4-4-2'),
 rules=list(allowed=lineup_formations(),multiposition=TRUE,bench_enabled=TRUE,captain_enabled=TRUE),
 source=list(wire=list(strategy='4-4-2',players=list()),locked=list(),status='',block_delayed=FALSE,target_id='r7',target_number=7),
 competition_id='unknown',season='unknown',scoring_version='fixture',observed_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),expires_at=format(Sys.time()+30,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))}
history<-data.frame(player_id=rep(roster$id,each=6),round=rep(1:6,nrow(roster)),points=rep(1:20,each=6))
full_history<-history
history_data<-shiny::reactiveVal(history)
default_status<-list(pending=0L,remaining=0L,saving=0L,failed=0L)
history_status<-shiny::reactiveVal(default_status)
recent_player_points_source<-function(...)list(history=shiny::reactive(history_data()),status=shiny::reactive(history_status()))
lineup_snapshot_store<-function(context,target_round,payload=NULL,owner=''){
 if(!is.null(payload)&&identical(state$mode,'snapshot-fail'))return(promises::promise_reject(simpleError('Lineup snapshot context mismatch')))
 promises::promise_resolve(if(is.null(payload))NULL else TRUE)
}
state<-new.env();reset<-function(mode='success'){state$mode<-mode;state$reads<-0L;state$submits<-0L;state$optimizes<-0L;state$official<-fixture();state$pending<-NULL;history_data(history);history_status(default_status)}
optimize_impl<-lineup_optimize
lineup_optimize<-function(...){
 state$optimizes<-state$optimizes+1L
 if(state$mode=='optimize-error')stop('synthetic optimizer error')
 optimize_impl(...)
}
lineup_fetch_bundle<-function(auth,context,target,owner,is_current=function()TRUE){
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
args<-list(is_module_active=function()TRUE,login_token=function()c(userid='fixture',token='synthetic'),championship_id=function()'league',user_team_id=function()'team',round_catalog=function()rounds,clause_transfers_source=function()list(outgoing_status='ok',outgoing_records=outgoing_clause_empty()))
reset()
shiny::testServer(lineup_Server,args=args,{
 settle(session)
 stopifnot(grepl('−5',output$expected_points,fixed=TRUE),grepl('-55',output$expected_points,fixed=TRUE))
 value<-draft();value$starters[11]<-'p1';value$bench[1]<-'p20';edit(value)
 points<-full_history;points$points[points$player_id=='p1']<- -3
 history_data(points);settle(session)
 stopifnot(grepl('-53',output$expected_points,fixed=TRUE))
 history_data(points[points$player_id!='p1',]);settle(session)
 stopifnot(grepl('missing average for 1 starter',output$expected_points,fixed=TRUE),state$reads==1L,state$submits==0L)
 cat('PASS empty slots, signed averages, missing starter history and bench exclusion in manual XI total\n')
})
reset()
state$official$roster[[20]]$status<-'injured'
state$official$roster[[19]]$status<-'doubt'
state$official$roster[[18]]$status<-'doubtful'
history_data(full_history[!full_history$player_id%in%c('p20','p19','p18'),])
shiny::testServer(lineup_Server,args=args,{
 settle(session);ui<-xml2::read_html(as.character(lineup_UI('fixture')))
 checkbox<-xml2::xml_find_first(ui,'.//input[@id="fixture-include_doubtful"]')
 stopifnot(is.na(xml2::xml_attr(checkbox,'checked')),
  grepl('Include doubtful players',xml2::xml_text(xml2::xml_parent(checkbox)),fixed=TRUE))
 session$setInputs(best=1,require_bench_coverage=TRUE);settle(session);before<-draft()
 stopifnot(state$optimizes==1L,is.null(best_request()),
  !any(c('p20','p19','p18')%in%c(before$starters,before$bench)))
 session$setInputs(include_doubtful=TRUE,best=2);settle(session)
 stopifnot(identical(draft(),before),state$optimizes==1L,!is.null(best_request()),
  grepl('complete scores for 2 eligible players',notice(),fixed=TRUE))
 session$setInputs(include_doubtful=FALSE);settle(session)
 stopifnot(is.null(best_request()),grepl('settings changed',notice(),fixed=TRUE))
 history_data(full_history[full_history$player_id!='p20',]);settle(session)
 stopifnot(identical(draft(),before),state$optimizes==1L)
 session$setInputs(include_doubtful=TRUE,best=3);settle(session)
 stopifnot(state$optimizes==2L,is.null(best_request()),all(c('p19','p18')%in%draft()$starters),
  !'p20'%in%c(draft()$starters,draft()$bench))
 included<-draft();session$setInputs(include_doubtful=FALSE);settle(session)
 stopifnot(identical(draft(),included),grepl('doubtful-player setting',output$status$html,fixed=TRUE))
 session$setInputs(best=4);settle(session)
 stopifnot(state$optimizes==3L,!any(c('p20','p19','p18')%in%c(draft()$starters,draft()$bench)),
  state$reads==1L,state$submits==0L)
 cat('PASS doubtful-only checkbox defaults off, matches history eligibility, cancels pending work and never admits injured players\n')
})
reset()
shiny::testServer(lineup_Server,args=args,{
 settle(session);ui<-as.character(lineup_UI('fixture'))
 checkbox<-xml2::xml_find_first(xml2::read_html(ui),'.//input[@id="fixture-require_bench_coverage"]')
 stopifnot(!is.na(xml2::xml_attr(checkbox,'id')),is.na(xml2::xml_attr(checkbox,'checked')),
  grepl('Goalkeeper cover is not required',ui,fixed=TRUE))
 session$setInputs(best=1);settle(session);before<-draft()
 session$setInputs(require_bench_coverage=TRUE);settle(session)
 stopifnot(identical(draft(),before),state$optimizes==1L,
  grepl('bench coverage setting',output$status$html,fixed=TRUE),state$reads==1L,state$submits==0L)
 session$setInputs(best=2);settle(session)
 stopifnot(isTRUE(best_basis()$coverage),length(lineup_bench_coverage(roster(),draft()$bench))==3L,
  !grepl('fm-lineup-best-warning',output$status$html),state$optimizes==2L)
 session$setInputs(require_bench_coverage=FALSE);settle(session)
 stopifnot(grepl('bench coverage setting',output$status$html,fixed=TRUE),state$optimizes==2L)
 session$setInputs(require_bench_coverage=TRUE);settle(session)
 stopifnot(!grepl('fm-lineup-best-warning',output$status$html),state$reads==1L,state$submits==0L)
 cat('PASS bench coverage defaults off with explanatory tooltip; changing it warns without recalculation or I/O\n')
})
reset()
scarce<-c('p1',paste0('p',3:6),paste0('p',9:13),paste0('p',16:19))
state$official$roster<-Filter(function(row)row$id%in%scarce,state$official$roster)
state$official$rules$allowed<-'4-4-2'
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session);before<-draft()
 session$setInputs(require_bench_coverage=TRUE,best=2);settle(session)
 stopifnot(identical(draft(),before),is.null(best_request()),notice_error(),
  grepl('No eligible XI can leave separate defender, midfielder and forward',notice(),fixed=TRUE),
  grepl('aria-busy="false"',output$status$html,fixed=TRUE),state$submits==0L)
 no_bench<-source();no_bench$rules$bench_enabled<-FALSE;source(no_bench)
 session$setInputs(best=3);settle(session)
 stopifnot(identical(draft(),before),is.null(best_request()),state$optimizes==2L,
  grepl('substitutes are disabled',notice(),fixed=TRUE),state$reads==1L,state$submits==0L)
 cat('PASS infeasible coverage and disabled benches retain the draft with actionable status and no mutation\n')
})
reset()
history_data(history[history$player_id!='p20',])
shiny::testServer(lineup_Server,args=args,{
 settle(session);before<-draft();session$setInputs(best=1);settle(session)
 stopifnot(!is.null(best_request()),state$optimizes==0L)
 session$setInputs(require_bench_coverage=TRUE);settle(session)
 stopifnot(is.null(best_request()),grepl('settings changed',notice()))
 history_data(full_history);settle(session)
 stopifnot(identical(draft(),before),state$optimizes==0L,state$reads==1L,state$submits==0L)
 cat('PASS bench coverage changes cancel pending work before late history can replace the draft\n')
})
reset()
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(formation='4-4-2');settle(session)
 stopifnot(!grepl('players have complete scores',output$status$html),
  !grepl('fm-lineup-refresh-help',as.character(lineup_UI('fixture'))))
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
reset()
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session)
 # Selected substitutes have lower averages than starters: membership must
 # outrank points. Verify the live draft again after a starter/bench swap.
 assert_picker_order<-function(){
  nodes<-xml2::xml_find_all(xml2::read_html(output$picker_players$html),'.//button[contains(@class,"fm-lineup-choice")]')
  ids<-xml2::xml_attr(nodes,'data-player-id');rows<-roster();avg<-scores()$average[match(ids,scores()$id)]
  group<-ifelse(ids%in%draft()$bench,1L,ifelse(ids%in%draft()$starters,3L,2L))
  stopifnot(identical(order(group,-avg,rows$name[match(ids,rows$id)],ids,na.last=TRUE),seq_along(ids)))
  invisible(ids)
 }
 session$setInputs(slot_click=slot_event(FALSE,1L));settle(session);ids<-assert_picker_order()
 stopifnot(ids[1]%in%draft()$bench,tail(ids,1)%in%draft()$starters)
 pick<-selected();incoming<-ids[1];outgoing<-draft()$starters[1]
 session$setInputs(player_choice=list(id=incoming,picker=pick$sequence));settle(session)
 stopifnot(incoming%in%draft()$starters,outgoing%in%draft()$bench)
 session$setInputs(slot_click=slot_event(FALSE,1L));settle(session);ids<-assert_picker_order()
 stopifnot(match(outgoing,ids)<match(incoming,ids))
 session$setInputs(slot_click=slot_event(TRUE,1L));settle(session);ids<-assert_picker_order()
 stopifnot(any(!ids%in%c(draft()$bench,draft()$starters)),state$reads==1L,state$submits==0L)
 cat('PASS starter and substitute pickers show bench, unused squad, then starters, updating after unsaved swaps without I/O\n')
})
reset()
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session);before<-draft();data<-source()
 ids<-c(before$starters[1:2],before$bench[1:3])
 codes<-c('injured','doubt','injured2','redcard','unavailable')
 labels<-c('Injured','Doubtful','Long-term injury','Red card','Unavailable')
 for(i in seq_along(ids))data$roster[[match(ids[i],vapply(data$roster,`[[`,character(1),'id'))]]$status<-codes[i]
 source(data);settle(session)
 pitch<-xml2::read_html(output$pitch$html)
 for(i in seq_along(ids)){
  icon<-xml2::xml_find_first(pitch,paste0('.//button[@data-player-id="',ids[i],'"]//div[@class="fm-lineup-portrait"]//span[@data-status="',codes[i],'"]'))
  stopifnot(xml2::xml_attr(icon,'title')==labels[i],xml2::xml_attr(icon,'aria-label')==labels[i])
 }
 session$setInputs(slot_click=slot_event(TRUE,1L));settle(session)
 picker<-xml2::read_html(output$picker_players$html)
 for(i in seq_along(ids)){
  icon<-xml2::xml_find_first(picker,paste0('.//button[@data-player-id="',ids[i],'"]//span[@data-status="',codes[i],'"]'))
  stopifnot(xml2::xml_attr(icon,'title')==labels[i],xml2::xml_attr(icon,'role')=='img')
 }
 stopifnot(is.null(lineup_status_icon('ok')),is.null(lineup_status_icon(NA_character_)),is.null(lineup_status_icon('')),
  grepl('Status unconfirmed',as.character(lineup_status_icon('new-source-code')),fixed=TRUE),
  grepl('Doubtful',as.character(lineup_status_icon(' DOUBT ')),fixed=TRUE),
  !analytics_known_unavailable(data.frame(status='doubt')),
  identical(draft(),before),state$optimizes==1L,state$reads==1L,state$submits==0L)
 cat('PASS observed injury, doubt, long-term injury and suspension icons follow pitch/bench portraits and picker rows without I/O or eligibility changes\n')
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
 stopifnot(state$submits==1L,state$reads==3L,!saving(),grepl('Saved to LigaRadar and verified',notice()))
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
  grepl('Optimization inputs changed:',output$status$html,fixed=TRUE),
  grepl('last N rounds',output$status$html,fixed=TRUE),grepl('Optimize to update this lineup.',output$status$html,fixed=TRUE),
  !grepl('doubtful-player setting|completed-round window|player departures',output$status$html))
 session$setInputs(recent_n=5);settle(session)
 stopifnot(identical(draft(),five_round_draft),state$optimizes==1L,!grepl('fm-lineup-best-warning',output$status$html))
 session$setInputs(recent_n=2);settle(session)
 session$setInputs(best=2);settle(session)
 stopifnot(draft()$starters[11]=='p2',state$optimizes==2L,grepl('rounds 5, 6',best_note(),fixed=TRUE),
  !grepl('fm-lineup-best-warning',output$status$html),grepl('aria-busy="false"',output$status$html,fixed=TRUE))
 two_round_draft<-draft()
 session$setInputs(include_doubtful=TRUE);settle(session)
 stopifnot(identical(draft(),two_round_draft),state$optimizes==2L,grepl('fm-lineup-best-warning',output$status$html),
  grepl('Optimization inputs changed: doubtful-player setting.',output$status$html,fixed=TRUE))
 session$setInputs(include_doubtful=FALSE);settle(session)
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
 stopifnot(identical(draft(),before),grepl('waiting for complete scores for 1 eligible player',notice()),!nzchar(best_note()),
  # Pending source disclosures are intentionally omitted; the visible action
  # notice above still explains why this request is waiting.
  !grepl('players have complete scores',output$status$html,fixed=TRUE))
 # Even though a complete XI could be formed, it must not silently exclude p20.
 history_data(full_history);history_status(default_status);settle(session)
 stopifnot('p20'%in%draft()$starters,all(nzchar(draft()$starters)),nzchar(best_note()),state$reads==1L,
  !grepl('players have complete scores',output$status$html))
 cat('PASS one Best XI click waits for all eligible histories and applies the full result when acquisition finishes\n')
})
reset()
shiny::testServer(lineup_Server,args=args,{
 settle(session)
 for(failure in list(list(failed=1L),list(save_error='History could not be saved.'))){
  history_status(utils::modifyList(default_status,failure));settle(session)
  stopifnot(all(is.finite(scores()$average)),grepl('fm-info-error',output$status$html),
   grepl(if(is.null(failure$save_error))'Some history reads failed'else failure$save_error,output$status$html,fixed=TRUE),
   !grepl('players have complete scores',output$status$html))
 }
 history_status(default_status);settle(session)
 stopifnot(!grepl('fm-info-error|players have complete scores',output$status$html),state$reads==1L,state$submits==0L)
 cat('PASS complete scores hide coverage while history read/save failures remain reported until recovery without source requests\n')
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
 stopifnot(identical(draft(),saved),!dirty(),grepl('successfully saved to LigaRadar',notice()),
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

# A successful worker schedule is historical evidence after its action TTL.
# Expiry cannot invent a changed completed window or clear useful score history.
reset()
base_calendar<-rounds;base_calendar$is_finished[6]<-FALSE
worker_calendar<-function(rows=base_calendar,expired=FALSE){
 at<-Sys.time()-if(expired)120 else 0
 value<-observed_data(rows,observed_at=at,status=if(expired)'stale'else'ok')
 attr(value,'worker_acquisition_status')<-'ok';attr(value,'worker_completeness')<-'complete'
 attr(value,'valid_until')<-as.numeric(at)+60
 value
}
# The optional shared state supplies scope and real acquisition failure proof.
proof<-worker_calendar(expired=TRUE)
proof_context<-resource_context(c(userid='fixture',token='synthetic'),'league','team',
 competition_id='competition',season='2026-27',scoring_version='fixture')
proof_state<-list(data=proof,status='stale',freshness='stale',context=proof_context,error=NULL)
stopifnot(!nzchar(lineup_catalog_problem(proof,proof_state,proof_context)))
for(field in c('account_id','championship_id','user_team_id','competition_id','season','scoring_version')){
 wrong<-proof_state;wrong$context[[field]]<-paste0('other-',field)
 stopifnot(nzchar(lineup_catalog_problem(proof,wrong,proof_context)))
}
for(kind in c('state_error','state_partial','missing_time','future_time','incomplete_catalog','missing_completeness','nonworker')){
 altered<-proof;state_copy<-proof_state
 if(kind=='state_error')state_copy$error<-resource_failure(category='network')
 if(kind=='state_partial')state_copy$status<-'partial'
 if(kind=='missing_time')attr(altered,'observed_at')<-NULL
 if(kind=='future_time')attr(altered,'observed_at')<-Sys.time()+3600
 if(kind=='incomplete_catalog'){attr(altered,'catalog_complete')<-FALSE;attr(altered,'catalog_reason')<-'Numbered calendar read failed.'}
 if(kind=='missing_completeness')attr(altered,'worker_completeness')<-NULL
 if(kind=='nonworker')attr(altered,'worker_acquisition_status')<-NULL
 stopifnot(nzchar(lineup_catalog_problem(altered,state_copy,proof_context)))
}
known_finality<-proof;attr(known_finality,'catalog_complete')<-FALSE;attr(known_finality,'catalog_reason')<-''
stopifnot(!nzchar(lineup_catalog_problem(known_finality,proof_state,proof_context)))
cat('PASS observed calendar reuse rejects wrong account/league/team/competition/season/scoring scope, failed state and incomplete/malformed evidence\n')
calendar<-shiny::reactiveVal(worker_calendar());args$round_catalog<-calendar
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session)
 baseline_draft<-draft();averages<-scores();initial_note<-best_note()
 stopifnot(identical(wanted(),as.numeric(3:5)),state$optimizes==1L,!nzchar(catalog_issue()))
 expired<-worker_calendar(expired=TRUE);calendar(expired);settle(session)
 stopifnot(identical(wanted(),as.numeric(3:5)),identical(scores(),averages),identical(draft(),baseline_draft),
  identical(best_note(),initial_note),!nzchar(catalog_issue()),!grepl('fm-lineup-best-warning',output$status$html),
  state$optimizes==1L,state$reads==1L,state$submits==0L,
  attr(calendar(),'valid_until')<as.numeric(Sys.time()),identical(attr(calendar(),'observed_at'),attr(expired,'observed_at')))
 cat('PASS expired successful worker calendar preserves known scores/window and creates no false optimization warning, source read or renewed timestamp\n')
 for(kind in c('error','partial','incomplete')){
  failed<-expired
  if(kind=='error')attr(failed,'fetch_error')<-resource_failure(category='network')
  if(kind=='partial')attr(failed,'worker_acquisition_status')<-'partial'
  if(kind=='incomplete')attr(failed,'worker_completeness')<-'partial'
  calendar(failed);settle(session);before_count<-state$optimizes
  stopifnot(nzchar(catalog_issue()),!length(wanted()),all(is.na(scores()$average)),
   !grepl('fm-lineup-best-warning',output$status$html),identical(draft(),baseline_draft))
  session$setInputs(best=before_count+1L);settle(session)
  stopifnot(state$optimizes==before_count,!is.null(best_request()),grepl('waiting for the completed-round schedule',notice(),fixed=TRUE))
  calendar(expired);settle(session)
  stopifnot(state$optimizes==before_count+1L,is.null(best_request()),identical(draft(),baseline_draft),
   !grepl('fm-lineup-best-warning',output$status$html),state$reads==1L)
 }
 cat('PASS actual worker acquisition errors/partial evidence block calculation without inventing settings changes; verified publication resumes one requested calculation\n')
 before_count<-state$optimizes;calendar(worker_calendar(rounds));settle(session)
 stopifnot(identical(wanted(),as.numeric(4:6)),identical(draft(),baseline_draft),state$optimizes==before_count,
  grepl('Optimization inputs changed: completed-round window.',output$status$html,fixed=TRUE),
  grepl('Optimize to update this lineup.',output$status$html,fixed=TRUE),state$reads==1L,state$submits==0L)
 cat('PASS a genuinely advanced verified completed-round window emits one compact actionable warning without overwriting the lineup\n')
})
args$round_catalog<-function()rounds

# Shared, already-persisted outgoing schedules drive both XI and bench eligibility.
transfer_state<-function(ids=character(),offset=numeric(),status='ok')list(outgoing_status=status,
 outgoing_records=data.frame(player_id=ids,leaves_at=if(length(offset))fm_time(rounds$begin_process[7])+offset else as.POSIXct(character(),tz='UTC')))
transfers<-shiny::reactiveVal(transfer_state())
args$clause_transfers_source<-transfers
reset()
transfers(transfer_state(c('p20','p19','p18'),c(-1,0,1)))
history_data(full_history[!full_history$player_id%in%c('p20','p19'),])
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1,include_doubtful=TRUE);settle(session)
 stopifnot(state$optimizes==1L,is.null(best_request()),!any(c(draft()$starters,draft()$bench)%in%c('p20','p19')),
  'p18'%in%draft()$starters,state$reads==1L,state$submits==0L,
  !grepl('fm-lineup-departures',output$status$html,fixed=TRUE),
  !grepl('player(s) leave by the next round start',output$status$html,fixed=TRUE))
 session$setInputs(slot_click=slot_event(FALSE,1L));settle(session)
 html<-xml2::read_html(output$picker_players$html)
 disabled<-function(id)xml2::xml_attr(xml2::xml_find_first(html,paste0('//button[@data-player-id="',id,'"]')),'disabled')
 stopifnot(disabled('p20')=='disabled',disabled('p19')=='disabled',is.na(disabled('p18')),
  grepl('leaves by round start',output$picker_players$html,fixed=TRUE))
 before<-draft();session$setInputs(player_choice=list(id='p20',picker=selected()$sequence));settle(session)
 stopifnot(identical(draft(),before),grepl('leaves by the next round start',picker_notice(),fixed=TRUE),state$reads==1L)
 # The server rejects a forged/obsolete save, including a departure on the bench.
 bad<-draft();bad$bench[1]<-'p19';draft(bad);session$setInputs(save=1);settle(session)
 stopifnot(state$submits==0L,state$reads==1L,grepl('replace them before saving',notice(),fixed=TRUE))
 cat('PASS before/at kickoff departures are excluded despite missing histories and enabling doubtful players; later departures remain eligible; picker and save reject departing players\n')
})
reset()
transfers(transfer_state(status='loading'))
shiny::testServer(lineup_Server,args=args,{
 settle(session);before<-draft();session$setInputs(best=1);settle(session)
 stopifnot(identical(draft(),before),state$optimizes==0L,!is.null(best_request()),grepl('waiting for verified clause',notice()))
 transfers(transfer_state('p20',NA_real_,status='partial'));settle(session)
 stopifnot(state$optimizes==0L,!is.null(best_request()),!departures()$complete)
 transfers(transfer_state('p20',-1));settle(session)
 stopifnot(state$optimizes==1L,is.null(best_request()),!'p20'%in%c(draft()$starters,draft()$bench),state$reads==1L)
 before<-draft();transfers(transfer_state(status='partial'));settle(session)
 stopifnot(identical(draft(),before),state$optimizes==1L,!grepl('fm-lineup-best-warning',output$status$html),
  grepl('Departure eligibility is not verified',output$status$html,fixed=TRUE))
 transfers(transfer_state(c('p20','p19'),c(-1,-1)));settle(session)
 stopifnot(identical(draft(),before),state$optimizes==1L,grepl('fm-lineup-best-warning',output$status$html),
  grepl('Optimization inputs changed: player departures.',output$status$html,fixed=TRUE))
 cat('PASS pending/unknown departure times retain the draft; one verified publication completes one calculation, and later departures warn without overwriting selections\n')
})
reset('delay')
transfers(transfer_state())
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session);session$setInputs(save=1);settle(session)
 stopifnot(saving(),is.function(state$pending));held<-state$pending
 departing<-draft()$starters[1];transfers(transfer_state(departing,-1))
 stopifnot(length(departure_issues(draft(),current_departures()))>0L)
 held(state$official);settle(session)
 stopifnot(!saving(),state$submits==0L,state$reads==2L,grepl('replace them before saving',notice(),fixed=TRUE))
 cat('PASS a departure discovered during save preflight prevents submission without mutation retry\n')
})
reset()
transfers(transfer_state())
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session);transfers(transfer_state(status='partial'));settle(session)
 session$setInputs(save=1);settle(session)
 stopifnot(state$submits==0L,state$reads==1L,grepl('Departure eligibility is not verified',notice(),fixed=TRUE))
 cat('PASS incomplete transfer evidence blocks saving without extra source requests\n')
})

reset()
transfers(transfer_state(status='partial'));transfer_refreshes<-0L
args$clause_transfers_refresh<-function(){transfer_refreshes<<-transfer_refreshes+1L}
shiny::testServer(lineup_Server,args=args,{
 settle(session);before<-draft();session$setInputs(best=1);settle(session)
 stopifnot(!is.null(best_request()),state$optimizes==0L)
 session$setInputs(reload=1);settle(session)
 stopifnot(transfer_refreshes==1L,state$reads==2L,is.null(best_request()),identical(draft(),before))
 session$setInputs(best=2);settle(session);transfers(transfer_state('p20',-1));settle(session)
 stopifnot(state$optimizes==1L,!('p20'%in%c(draft()$starters,draft()$bench)),state$submits==0L)
 # Timestamp-only transfer publications must not invalidate the projected state.
 updates<-new.env();updates$n<-0L
 observer<-shiny::observe({departures();updates$n<-updates$n+1L})
 settle(session);initial<-updates$n
 for(i in 1:10){fresh<-transfer_state('p20',-1);fresh$source_times<-list(summary=Sys.time()+i);transfers(fresh);session$flushReact()}
 stopifnot(updates$n==initial,state$reads==2L,state$optimizes==1L,state$submits==0L)
 observer$destroy()
 cat('PASS explicit lineup refresh retries departure acquisition once and preserves the draft; ten timestamp-only publications cause no eligibility redraws or source reads\n')
})
reset('snapshot-fail');transfers(transfer_state())
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session);session$setInputs(save=1);settle(session)
 stopifnot(state$submits==1L,!saving(),!dirty(),grepl('Saved to LigaRadar and verified',notice(),fixed=TRUE),
  grepl('App copy not saved: context mismatch (FM-',cache_issue(),fixed=TRUE),
  grepl('App copy not saved:',output$status$html,fixed=TRUE),!grepl('fm-lineup-best-warning',output$status$html,fixed=TRUE))
 cat('PASS a verified Futmondo save remains distinct from a failed app snapshot with a safe diagnostic and no false recalculation warning\n')
})
cat('LINEUP MODULE: all checks passed\n')
