#!/usr/bin/env Rscript
# Exact completed windows must not reuse an ongoing-round window or acquire the
# whole championship for a lineup-only client. Everything is synthetic/offline.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='lineup-history',token='synthetic')
scoring<-paste0(normalize_league_rules(list(),list(pointSystem='fixture'),'league')$scoring_version)
players<-data.frame(id=c('a','b'))
observations<-function(id,rounds){data.frame(player_id=id,championship_id='league',season='unknown',scoring_version=scoring,
 round=rounds,points=as.numeric(rounds),score_status='final',observed_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),occurred_at=NA_character_,round_start_at=NA_character_)}
points<-dplyr::bind_rows(observations('a',2:6),observations('b',2:6),observations('catalog-only',1:8))
checks<-data.frame();calls<-character();pending<-list()
read_league_round_dataset<-function(...)list(points=points,checks=checks,complete=TRUE)
get_lineup_from_team<-function(...)list(pointSystem='fixture')
get_championship_players<-function(...)data.frame(id=c('a','b','catalog-only'))
persist_recent_points_snapshot<-function(payload,context,on_complete){on_complete(TRUE);TRUE}
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE){
 calls<<-c(calls,paste(fn,args$player_id %||% '',sep=':'))
 if(fn=='get_player_recent_points')return(promises::promise(function(resolve,reject)pending[[args$player_id]]<<-list(resolve=resolve,context=context)))
 promises::promise_resolve(do.call(get(fn,.GlobalEnv),args))
}
settle<-function(session){for(i in seq_len(25)){later::run_now(0);session$flushReact()}}
resolve_all<-function(rounds){for(id in names(pending)){p<-pending[[id]];p$resolve(list(context=p$context,checked_at=Sys.time(),observations=observations(id,rounds)))};pending<<-list()}
server<-function(input,output,session){
 window<-reactiveVal(as.numeric(2:6));table_active<-reactiveVal(FALSE)
 lineup<-recent_player_points_source(session,reactive(auth),reactive('league'),reactive('team'),reactive(players),
  required_rounds=function()6L,catalog_scope=FALSE,required_window=window)
 table<-recent_player_points_source(session,reactive(auth),reactive('league'),reactive('team'),reactive(data.frame(id='catalog-only')),
  is_active=table_active)
}
shiny::testServer(server,{
 settle(session)
 stopifnot(!any(grepl('get_championship_players|get_player_recent_points',calls)),sum(grepl('read_league_round_dataset',calls))==1L)
 cat('PASS lineup-only exact saved window reuses history without acquiring championship catalog\n')
 window(as.numeric(3:7));settle(session)
 stopifnot(setequal(names(pending),c('a','b')),!any(grepl('catalog-only',calls)))
 resolve_all(3:7);settle(session)
 stopifnot(!length(pending),all(c(3,7)%in%lineup$history()$round))
 cat('PASS missing completed window acquires only affected squad histories\n')
 before<-length(calls);window(as.numeric(4:8));settle(session)
 stopifnot(length(calls)==before+2L,setequal(names(pending),c('a','b')))
 resolve_all(4:8);settle(session)
 before<-calls;settle(session);stopifnot(identical(calls,before))
 cat('PASS changed same-N completed window reacquires once instead of being blocked by prior attempt\n')
 table_active(TRUE);settle(session)
 stopifnot(sum(grepl('get_championship_players',calls))==1L)
 table_active(FALSE);settle(session);table_active(TRUE);settle(session)
 stopifnot(sum(grepl('get_championship_players',calls))==1L)
 cat('PASS navigating to an existing table initializes catalog once after a lineup-only session\n')
})
cat('LINEUP HISTORY: 4 groups passed / 0 failed\n')
