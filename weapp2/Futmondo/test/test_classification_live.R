#!/usr/bin/env Rscript
# Source-derived request/row contracts, synthetic transport/storage, no accounts.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-c(userid='classification-test',token='synthetic');cid<-'league'
ctx<-resource_context(auth,cid,'a',season='unknown',session_id='live-test')
answer<-list(metadata=list(status='playing'),ranking=lapply(seq_along(c(26,18,15,9,8,6,2,0,0)),function(i)
  list(id=letters[i],name=paste('Team',i),points=c(26,18,15,9,8,6,2,0,0)[i],live=c(9,7,5,3,1,0,2,4,6)[i],bench=2,starting=11)))
requests<-0L;writes<-list();saved<-list(evidence=list(),complete=TRUE)
read_rival_finance_evidence<-function(...)saved
persist_rival_finance_evidence<-function(evidence,context){writes<<-c(writes,evidence);TRUE}
fm_async_read_http<-function(url,method='GET',body=NULL,...){
  stopifnot(identical(url,'https://api.futmondo.com/1/ranking/round'),method=='POST')
  p<-jsonlite::fromJSON(body);stopifnot(p$query$roundNumber%in%c(12L,13L),identical(p$query$championshipId,cid),identical(p$query$userteamId,'a'))
  requests<<-requests+1L
  response<-if(p$query$roundNumber==13L)list(metadata=list(status='next'),ranking=list())else answer
  promises::promise_resolve(list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=response),auto_unbox=TRUE,null='null',na='null')),times=c(total=0)))
}
await<-function(p){done<-FALSE;out<-err<-NULL;promises::then(p,function(x){out<<-x;done<<-TRUE},function(e){err<<-e;done<<-TRUE});deadline<-Sys.time()+3;while(!done&&Sys.time()<deadline)later::run_now(.001);stopifnot(done);if(!is.null(err))stop(err);out}
args<-list(login=auth,championship_id=cid,user_team_id='a',round_id='r12',round_number=12)
clear_api_cache();live<-await(async_adapter_fetch('get_classification_round_ranking',args,ctx))
stopifnot(identical(live$rows$points,c(26,18,15,9,8,6,2,0,0)),all(live$rows$score_status=='provisional'),identical(live$rows$players_left,c(9,7,5,3,1,0,2,4,6)),requests==1L,length(writes)==1L)
e<-writes[[1]]
stopifnot(e$evidence_type=='eligibility',e$source_id=='classification:r12',e$payload$kind=='classification_round_ranking',
  length(rival_evidence_rows(list(e),'round_ranking'))==0L,length(rival_evidence_rows(list(e),'team_rounds'))==0L,
  nrow(rival_saved_rounds(list(e)))==0L,nrow(rival_round_ranking_rows(list(round_id='r12',round_number=12,status='playing',ranking=answer$ranking)))==0L)
invisible(await(async_adapter_fetch('get_classification_round_ranking',args,ctx)));stopifnot(requests==1L,length(writes)==1L)
# Fresh persisted source survives a cleared process cache; reopening doesn't save it again.
saved$evidence<-list(e);clear_api_cache();again<-await(async_adapter_fetch('get_classification_round_ranking',args,ctx))
stopifnot(identical(again$rows,live$rows),requests==1L,length(writes)==1L,
  identical(as.numeric(attr(again,'observed_at')),as.numeric(fm_time(e$observed_at))))
# Remaining values survive database-shaped JSON and a cleared process cache.
saved$evidence[[1]]$payload <- jsonlite::fromJSON(jsonlite::toJSON(e$payload,auto_unbox=TRUE,null='null',na='null'))
clear_api_cache();json_reopen<-await(async_adapter_fetch('get_classification_round_ranking',args,ctx))
stopifnot(identical(json_reopen$rows$players_left,live$rows$players_left),requests==1L,length(writes)==1L)
invalid <- answer
bad <- list(NULL,-1,2.5,Inf,TRUE,list(3),'unknown')
for(i in seq_along(bad))invalid$ranking[[i]]$live <- bad[[i]]
stopifnot(all(is.na(classification_ranking_rows(invalid,'r12',12)$players_left[seq_along(bad)])))
missing<-answer;missing$ranking[[1]]$points<-NULL;missing$ranking[[2]]$points<--2
rows<-classification_ranking_rows(missing,'r12',12)
stopifnot(is.na(rows$points[1]),rows$points[2]==-2,rows$points[8]==0)
future_args<-args;future_args$round_id<-'r13';future_args$round_number<-13
future<-await(async_adapter_fetch('get_classification_round_ranking',future_args,ctx))
stopifnot(isTRUE(future$not_started),nrow(future$rows)==0L,length(writes)==1L,requests==2L)
original_catalog<-get_award_round_catalog;original_metadata<-get_award_round_metadata
get_award_round_catalog<-function(...)list(league_id='competition',current_round_id='r13',current_round_number=13,
  rounds=data.frame(round_id=paste0('r',11:13),round_number=11:13,is_finished=c(TRUE,FALSE,FALSE),status=c('done','next','future')))
get_award_round_metadata<-function(...)list(status='ok',rows=data.frame(round_id=c('r13','r12','r11'),round_number=c(13,12,11),status=c('next','playing','closed')))
hydrated<-get_classification_round_catalog(auth,cid)
stopifnot(hydrated$current_round_number==13,hydrated$rounds$status[hydrated$rounds$round_number==12]=='playing')
get_award_round_catalog<-original_catalog;get_award_round_metadata<-original_metadata
ledger<-data.frame();attr(ledger,'round_bonus_totals')<-data.frame(round_number=12,bonus_type=c('points','mvp','dreamteam','ranking'),amount=1)
bonus<-classification_bonus_table(data.frame(team_id='a'),live$rows,list(cash_ledgers=list(a=ledger)),c(12,12))
stopifnot(all(is.na(bonus[,c('points_bonus','mvp_bonus','best_xi_bonus','position_bonus')])))
cat('PASS live request/parser: 26/18/15 known answers, missing/negative/zero, one cold and zero warm/reopen HTTP reads, display-only persistence\n')
# Team identity, rather than current rank/order, controls the remaining count.
projection <- data.frame(team_id=c('b','absent','a'))
observed <- data.frame(team_id=c('a','b'),round=12,score_status='provisional',players_left=c(3,0))
calendar <- data.frame(round_number=12,is_finished=FALSE,status='playing')
projected <- classification_remaining_table(projection,observed,'12',calendar)
stopifnot(identical(projected$players_left,c(0,NA_real_,3)),
  !'players_left'%in%names(classification_remaining_table(projection,observed,'all',calendar)))
closed <- calendar;closed$is_finished <- TRUE
stopifnot(!'players_left'%in%names(classification_remaining_table(projection,observed,'12',closed)))
final <- observed;final$score_status <- 'final'
stopifnot(!'players_left'%in%names(classification_remaining_table(projection,final,'12',calendar)))
old <- observed;old$players_left <- NULL
stopifnot(all(is.na(classification_remaining_table(projection,old,'12',calendar)$players_left)))
cat('PASS remaining-player projection: ID mapping, zero, absent teams/legacy snapshots unknown, hidden for completed rounds and partial ranges\n')

# Delayed catalog/live publications exercise real preset observers and totals.
bind_factual_reads<-function(...)invisible(NULL)
shared_mvp_source<-function(...)reactive(list())
shared_rival_finance_source<-function(...)reactive(list())
fail_live<-reactiveVal(FALSE)
catalog<-reactiveVal(list(rounds=data.frame()))
ranking<-reactiveVal(NULL)
history<-reactiveVal(list())
get_classification_round_catalog<-function(...)catalog()
get_classification_round_ranking<-function(login,championship_id,user_team_id,round_id,round_number){
  if(round_number==13)return(list(rows=data.frame(),not_started=TRUE))
  stopifnot(round_id=='r12',round_number==12)
  if(fail_live())stop('Synthetic source unavailable')
  ranking()
}
get_user_team_rounds<-function(...)history()
get_user_team_info<-function(...)list(configuration=list())
get_round_dreamteam<-function(...)list()
teams<-data.frame(teamid=letters[1:9],teamname=paste('Team',1:9),points=100:108)
shiny::testServer(classification_Server,args=list(is_module_active=reactive(TRUE),login_token=reactive(auth),
  championship_id=reactive(cid),user_team_id=reactive('a'),user_teams_RV=reactive(teams)),{
  messages<-list();root<-session$rootScope();root$sendInputMessage<-function(inputId,message)messages[[sub('^.*-','',inputId)]]<<-message
  session$setInputs(classification_view='rounds',round_range_slider=c(1,38))
  stopifnot(is.null(picked_round_RV()))
  catalog(list(current_round_id='r13',current_round_number=13,rounds=data.frame(round_id=paste0('r',11:13),round_number=11:13,is_finished=c(TRUE,FALSE,FALSE),status=c('closed','next','future'))))
  session$flushReact()
  stopifnot(max(round_numbers_RV())==11,is.null(picked_round_RV()))
  session$setInputs(round_range_slider=c(11,11))
  ranking(live);session$flushReact()
  stopifnot(selected_round_RV()==12,max(round_numbers_RV())==12)
  session$setInputs(round_range_slider=c(12,12))
  stopifnot(identical(standings_RV()$points,c(26,18,15,9,8,6,2,0,0)),all(is.na(standings_RV()$points_bonus)),
    grepl('provisional',output$classification_status$html,fixed=TRUE),is.null(picked_round_RV()),
    identical(standings_RV()$players_left,c(9,7,5,3,1,0,2,4,6)),
    grepl('Players left',output$classification_table,fixed=TRUE))
  # Empty history is not permission to call current-only points General.
  session$setInputs(classification_view='partial',round_range_slider=c(11,12))
  stopifnot(!'players_left'%in%names(standings_RV()),!grepl('Players left',output$classification_table,fixed=TRUE),identical(standings_RV()$points,as.numeric(100:108)),identical(as.numeric(standings_RV()$rank),as.numeric(9:1)))
  session$setInputs(round_range_slider=c(11,11));stopifnot(all(is.na(standings_RV()$points)),partial_view_RV())
  # A background publication cannot override a deliberate manual selection.
  history(list(list(round=11,points=7)));session$flushReact()
  stopifnot(partial_view_RV(),standings_RV()$points[standings_RV()$team_id=='a']==7)
  # A same-round history snapshot is replaced, never added to the live value.
  history(list(list(round=11,points=7),list(round=12,points=999)))
  session$setInputs(classification_view='rounds');session$flushReact()
  stopifnot(standings_RV()$points[standings_RV()$team_id=='a']==26,
    sum(round_rows_RV()$team_id=='a'&round_rows_RV()$round==12)==1L)
  history(list(list(round=11,points=7)))
  fail_live(TRUE)
  catalog(list(current_round_id='r12',current_round_number=12,rounds=data.frame(round_id=paste0('r',11:13),
    round_number=11:13,is_finished=c(TRUE,FALSE,FALSE),status=c('closed','playing','next'))))
  session$flushReact()
  stopifnot(max(round_numbers_RV())==12,all(is.na(standings_RV()$points)),all(is.na(standings_RV()$players_left)),
    grepl('unavailable',output$classification_status$html,fixed=TRUE))
})
cat('PASS known-live failure retains12/unavailable; active12/future13, delayed Latest convergence, General cumulative100, missing whole-round coverage, manual selection, no live double count\n')
