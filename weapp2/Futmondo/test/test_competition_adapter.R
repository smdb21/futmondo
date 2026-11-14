#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()FALSE
auth<-c(userid='adapter-context-account',token='synthetic')
seed<-function(season,competition='competition-A'){
 answer<-list(championships=list(list(id='game',league=competition,ps='stats')),leagues=list(list(`_id`=competition,season=season)))
 publish_active_league_contexts(auth,answer)
}
get_context_fixture<-function(login,championship_id,user_team_id)NULL
calls<-list();held<-list()
async_adapter_fetch<-function(fn,args,context,...){
 calls[[length(calls)+1L]]<<-context
 if(fn=='get_context_fixture')return(promises::promise(function(resolve,reject){held[[length(held)+1L]]<<-resolve}))
 stop('Unexpected synthetic adapter')
}
settle<-function(session)for(i in seq_len(25L)){later::run_now(0);session$flushReact()}
server<-function(input,output,session){
 read<-factual_read_adapter(session,'get_context_fixture',shiny::reactive(auth),shiny::reactive('game'),shiny::reactive('team'),state_only=TRUE)
 state<-shiny::reactive(read(auth,'game','team'))
 output$scope<-shiny::renderText(state()$data$season %||% 'loading')
}
seed('season-one');clear_api_cache()
shiny::testServer(server,{
 settle(session);stopifnot(length(calls)==1L,calls[[1]]$season=='season-one',output$scope=='loading')
 seed('season-two');settle(session)
 stopifnot(length(calls)==2L,calls[[2]]$season=='season-two',output$scope=='loading')
 held[[2]](list(season='season-two'));settle(session);stopifnot(output$scope=='season-two')
 held[[1]](list(season='season-one'));settle(session);stopifnot(output$scope=='season-two')
 before<-length(calls);settle(session);stopifnot(length(calls)==before)
})
cat('PASS shared adapter starts a new exact context and rejects a late prior-season response; warm reuse adds zero reads.\n')
# An explicitly requested historical competition must not use today's cache key.
seed('season-two');clear_api_cache();reads<-character()
history_read_pages<-function(table,query,...){
 competition<-sub('^eq\\.','',query$competition_id);reads<<-c(reads,competition)
 d<-data.frame(account_id=auth[['userid']],championship_id='game',competition_id=competition,season='season-two',evidence_type='team',source_id='team',observed_at='2026-11-01T00:00:00Z')
 d$payload<-I(list(list(budget=if(competition=='competition-A')10 else 20)));attr(d,'complete')<-TRUE;d
}
a<-read_rival_finance_evidence(auth[['userid']],'game','season-two',competition_id='competition-B')
b<-read_rival_finance_evidence(auth[['userid']],'game','season-two',competition_id='competition-A')
warm<-read_rival_finance_evidence(auth[['userid']],'game','season-two',competition_id='competition-A')
stopifnot(a$evidence[[1]]$competition_id=='competition-B',b$evidence[[1]]$competition_id=='competition-A',
 warm$evidence[[1]]$payload$budget==10,identical(reads,c('competition-B','competition-A')))
cat('PASS explicit historical competition evidence queries have isolated cache identities and warm reuse.\n')

# Expiry on the same page must cause a demand read, without a polling timer.
seed('season-two');clear_api_cache();context_reads<-0L
async_adapter_fetch<-function(fn,args,context,...){
 stopifnot(fn=='get_resolved_league_context');context_reads<<-context_reads+1L
 seed('season-two');promises::promise_resolve(resource_context(auth,'game','team'))
}
expiry_server<-function(input,output,session){
 session$userData$league_context_enabled<-TRUE
 session$userData$league_context_source<-league_context_demand_source(session,shiny::reactive(auth),shiny::reactive('game'),shiny::reactive('team'))
 output$ready<-shiny::renderText({input$n;as.character(league_context_ready(session,auth,'game'))})
}
shiny::testServer(expiry_server,{
 session$setInputs(n=1L);settle(session);stopifnot(output$ready=='TRUE',context_reads==1L)
 row<-league_context_metadata('game',auth[['userid']]);row$observed_at<-format(Sys.time()-400,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC');row$expires_at<-format(Sys.time()-100,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC')
 .fm_league_contexts$items[[fm_league_context_key(auth[['userid']],'game')]]<-row
 session$setInputs(n=2L);settle(session)
 stopifnot(output$ready=='TRUE',context_reads>=2L)
 before<-context_reads;session$setInputs(n=3L);settle(session);stopifnot(context_reads==before)
})
cat('PASS same-page demand renews expired context once and subsequent demand reuses the result without polling.\n')
