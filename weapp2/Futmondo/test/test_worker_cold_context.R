options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
source('history_repository.R')
for(expr in parse('worker/helper.R'))if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&as.character(expr[[2]])%in%c('fm_worker_run','fm_worker_discover'))eval(expr,.GlobalEnv)
.fm_league_contexts$items<-list();.fm_league_contexts$publications<-list()
queries<-list();getter_calls<-0L;bad_reply<-FALSE
get_mvp_saved_catalog<-function(login,championship_id){getter_calls<<-getter_calls+1L;list(season='s',saved=data.frame(),catalog=list(rounds=data.frame(),complete=TRUE))}
fm_pipe_exchange<-function(message){
 if(message$type=='resource')return(list(value=fm_worker_encode(list(season='unknown')),observed_at=rival_finance_stamp(Sys.time())))
 stopifnot(message$type=='database');queries[[length(queries)+1L]]<<-message$query
 if(bad_reply)return(list(missing_rows=list()))
 list(rows=list())
}
fm_worker_headless_finances<-function(env,auth,ctx,on_update=NULL){
 current<-env$resource_context(auth,ctx$championship_id,ctx$user_team_id)
 season<-env$league_season_context(ctx$championship_id)
 evidence<-env$read_rival_finance_evidence(ctx$account_id,ctx$championship_id,season)
 catalog<-env$get_mvp_saved_catalog(auth,ctx$championship_id)
 stopifnot(identical(catalog$season,'s'))
 list(current=current,season=season,metadata=env$league_context_metadata(ctx$championship_id,ctx$account_id),
   archive=env$resource_context(auth,ctx$championship_id,season='unknown',scoring_version='unknown',competition_id='unknown'),
   foreign_account=env$resource_context(account_id='other',championship_id=ctx$championship_id),
   foreign_league=env$resource_context(auth,'other-league'))
}
ctx<-resource_context(account_id='u',championship_id='l',user_team_id='t',competition_id='c',season='s',scoring_version='v')
d<-fm_worker_descriptor('fm_worker_finances',list(championship_id='l',user_team_id='t'),ctx)
value<-fm_worker_decode(fm_worker_run(d)$value)
stopifnot(value$current$season=='s',value$current$competition_id=='c',value$current$scoring_version=='v',value$season=='s',
 length(queries)==1L,queries[[1]]$season=='eq.s',queries[[1]]$competition_id=='eq.c')
cat('PASS a cold helper reads the verified competition/season archive before any source discovery or duplicated acquisition\n')
stopifnot(value$archive$season=='unknown',value$archive$competition_id=='unknown',value$archive$scoring_version=='unknown',
 value$foreign_account$season=='unknown',value$foreign_league$season=='unknown')
cat('PASS explicit unknown archives and foreign account/league requests do not inherit the active scope\n')
stopifnot(!isTRUE(value$metadata$complete),is.null(value$metadata$observed_at),is.null(value$metadata$expires_at))
cat('PASS descriptor identity does not manufacture source metadata timestamps or completeness\n')
stopifnot(getter_calls==1L)
cat('PASS a wrongly scoped persisted award catalog is reacquired rather than returned as current-season evidence\n')
bad_reply<-TRUE
stopifnot(inherits(try(fm_worker_run(d),silent=TRUE),'try-error'))
cat('PASS malformed database replies remain failures; only a genuine empty row array becomes an empty dataframe\n')
