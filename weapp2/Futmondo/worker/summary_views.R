# Materialize display projections from one successful source summary. Called in
# the headless helper environment, whose persistence writes require DB ACKs.
fm_worker_summary_views <- function(summary,context) {
 stamp<-fm_time(attr(summary,'observed_at'))
 if(!is.list(summary)||acquisition_data_unverified(summary)||!is.null(attr(summary,'fetch_error'))||
    length(stamp)!=1L||is.na(stamp)||stamp>base::Sys.time())return(list())
 ctx<-context;ctx$session_id<-''
 profile_id<-fm_scalar(summary$data$id %||% summary$data$`_id`,'')
 if(nzchar(profile_id)&&!identical(profile_id,ctx$player_id))stop('Summary player identity mismatch')
 source_season<-fm_scalar(summary$championship$season %||% summary$championship$seasonId,'unknown')
 if(fm_context_known(source_season)&&fm_context_known(ctx$season)&&!identical(source_season,ctx$season))
  stop('Summary season identity mismatch')
 if(!all(vapply(ctx[c('account_id','championship_id','user_team_id','player_id')],function(x)nzchar(fm_scalar(x,'')),logical(1))))return(list())
 args<-list(championship_id=ctx$championship_id,user_team_id=ctx$user_team_id,player_id=ctx$player_id)
 out<-list()
 add<-function(handler,value,parameters,status='ok',complete='complete'){
  attr(value,'observed_at')<-stamp;attr(value,'valid_until')<-as.numeric(stamp)+300
  out[[length(out)+1L]]<<-list(descriptor=fm_worker_descriptor(handler,parameters,ctx),value=fm_worker_encode(value),interval_seconds=300L,
   observed_at=rival_finance_stamp(stamp),checked_at=rival_finance_stamp(stamp),valid_until=rival_finance_stamp(stamp+300),status=status,completeness=complete)
 }
 # Only authoritative season/scoring identities can acknowledge reusable
 # numbered history. Unknown scopes wait for the context acquisition job.
 if(all(vapply(ctx[c('competition_id','season','scoring_version')],fm_context_known,logical(1)))&&recent_points_summary_valid(summary)){
  points<-recent_points_from_summary(summary,ctx)
  acknowledged<-FALSE
  accepted<-persist_recent_points_snapshot(points,ctx,function(ok)acknowledged<<-isTRUE(ok))
  if(!isTRUE(accepted)||!acknowledged)stop('Derived history persistence was not acknowledged')
  attr(points,'worker_history_persisted')<-TRUE
  add('get_player_recent_points',points,c(args,list(scoring_version=ctx$scoring_version)),
    status=if(nrow(points$observations))'ok'else'empty')
 }
 scope<-clause_transfer_scope(ctx)
 clean<-if(is.null(scope))NULL else clause_transfer_clean_summary(summary,scope)
 if(!is.null(clean)){
  clean<-observed_data(clean,observed_at=stamp,status='ok')
  attr(clean,'valid_until')<-as.numeric(stamp)+300
  acknowledged<-FALSE
  accepted<-persist_clause_transfer_summary(clean,ctx,function(ok)acknowledged<<-isTRUE(ok))
  if(!isTRUE(accepted)||!acknowledged)stop('Derived clause persistence was not acknowledged')
  add('get_clause_transfer_summary',clean,args)
 }
 out
}
