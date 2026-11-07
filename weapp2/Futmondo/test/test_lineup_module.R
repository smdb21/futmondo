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
state<-new.env();reset<-function(mode='success'){state$mode<-mode;state$reads<-0L;state$submits<-0L;state$official<-fixture();state$pending<-NULL;history_data(history);history_status(default_status)}
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
  grepl('not verified',output$status$html),!grepl('0 completed rounds',output$status$html),state$submits==0L)
 stopifnot(grepl('Best XI is waiting',notice()))
 catalog(rounds);settle(session)
 stopifnot(all(is.finite(scores()$average)),identical(wanted(),as.numeric(2:6)),
  all(nzchar(draft()$starters)),!grepl('not verified',output$status$html),state$submits==0L)
 cat('PASS a queued Best XI reports incomplete calendar and automatically recovers on verified publication\n')
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
 settle(session);session$setInputs(best=1,recent_n=5);settle(session)
 stopifnot(draft()$starters[11]=='p1',scores()$average[scores()$id=='p1']==60,
  grepl('rounds 2, 3, 4, 5, 6',best_note(),fixed=TRUE))
 session$setInputs(recent_n=2);settle(session)
 stopifnot(draft()$starters[11]=='p2',scores()$average[scores()$id=='p2']==50,
  identical(wanted(),as.numeric(5:6)),grepl('rounds 5, 6',best_note(),fixed=TRUE))
 # Returning to a previously calculated window clears a wider-window wait.
 history_data(window_history[window_history$round>=5,]);session$setInputs(recent_n=5);settle(session)
 stopifnot(grepl('waiting for complete scores',notice()),!nzchar(best_note()))
 session$setInputs(recent_n=2);settle(session)
 stopifnot(grepl('rounds 5, 6',best_note(),fixed=TRUE),!grepl('waiting',notice()))
 # Manual edits stop automatic regeneration when the scoring window changes.
 session$setInputs(clear=1);settle(session);session$setInputs(recent_n=5);settle(session)
 stopifnot(all(!nzchar(draft()$starters)),is.null(best_request()),state$reads==1L,state$submits==0L)
 cat('PASS changing five to two rounds updates the known-answer goalkeeper; manual edits cancel regeneration without source requests\n')
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
shiny::testServer(lineup_Server,args=args,{
 settle(session);session$setInputs(best=1);settle(session);session$setInputs(save=1);settle(session)
 saved<-draft();stopifnot(state$submits==1L,all(nzchar(saved$starters)),is.null(best_request()))
 session$setInputs(clear=1);settle(session);session$setInputs(reset=1);settle(session)
 stopifnot(identical(draft(),saved),!dirty(),grepl('successfully saved to Futmondo',notice()),
  grepl('Undo changes',as.character(lineup_UI('fixture')),fixed=TRUE))
 cat('PASS Undo changes restores the latest successfully saved and verified lineup\n')
})
cat('LINEUP MODULE: 10 groups passed / 0 failed\n')
