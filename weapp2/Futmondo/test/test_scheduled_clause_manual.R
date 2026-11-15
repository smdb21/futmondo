options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_clause_enabled<-function()TRUE
fm_clause_refresh<-function(...)invisible(NULL)
auth<-c(userid='u',token='synthetic')
ctx<-resource_context(auth,'l','t','p',competition_id='c',season='S',scoring_version='v')
get_scheduled_clause_check<-function(...)list(context=fm_clause_scope(ctx),player=list(id='p',slug='verified-slug',name='Fixture',owner_id='rival',
 owner_verified=TRUE,price=90,transferred=FALSE,opens_at=fm_clause_stamp(Sys.time()-60)))
writes<-0L;calls<-list();failure<-NULL;native<-list(success=TRUE,code='api.general.ok')
buy_roster_clause<-function(login,championship_id,team_id,player_id,player_slug,price){
 writes<<-writes+1L;stopifnot(player_slug=='verified-slug',price==90);native}
buy_clause<-function(login,championship_id,team_id,player_id,player_slug,price,isClause=TRUE){writes<<-writes+1L;native}
modify_bid<-function(login,championship_id,team_id,player_id,bid_id,new_price){writes<<-writes+1L;structure(isTRUE(native$success),api_code=native$code)}
fm_clause_rpc_sync<-function(name,payload){
 calls[[length(calls)+1L]]<<-list(name=name,payload=payload)
 if(identical(failure,name))stop('synthetic-private-error-not-for-output')
 if(name=='admit')list(id=payload$p_id)else if(name=='dispatch')!identical(failure,'dispatch_false')else TRUE
}
fm_clause_install_guards()
res<-buy_roster_clause(auth,'l','t','p','stale-slug',100)
stopifnot(res$success,writes==1L,identical(vapply(calls,`[[`,character(1),'name'),c('admit','dispatch','settle')),
 calls[[1]]$payload$p_mode=='clause',calls[[1]]$payload$p_price==90,calls[[3]]$payload$p_state=='accepted')
cat('PASS manual clause writes use verified slug/current lower price and durable admission before the only send\n')
failure<-'admit';writes<-0L
res<-buy_roster_clause(auth,'l','t','p','stale-slug',100)
stopifnot(!res$success,writes==0L,!grepl('private',res$message,fixed=TRUE),res$code=='acquisition_verification_unavailable')
failure<-'dispatch_false';calls<-list()
res<-buy_roster_clause(auth,'l','t','p','stale-slug',100)
stopifnot(!res$success,writes==0L,tail(calls,1)[[1]]$payload$p_state=='failed')
cat('PASS admission/dispatch failure is contained and never sends or exposes a private error\n')
failure<-NULL;native<-list(success=FALSE,code='error');calls<-list()
res<-buy_roster_clause(auth,'l','t','p','stale-slug',100)
stopifnot(!res$success,writes==1L,tail(calls,1)[[1]]$payload$p_state=='uncertain',grepl('uncertain',res$message,fixed=TRUE))
cat('PASS a lost manual purchase response retains its reservation and is never retried\n')
native<-list(success=TRUE,code='api.general.ok');calls<-list()
res<-buy_clause(auth,'l','t','p','fixture',50,isClause=FALSE)
stopifnot(res$success,calls[[1]]$payload$p_mode=='offer')
calls<-list();res<-modify_bid(auth,'l','t','p','existing',70)
stopifnot(isTRUE(res),calls[[1]]$payload$p_mode=='modify',calls[[1]]$payload$p_price==70)
cat('PASS owner offers and existing bid modifications share the same durable commitment boundary\n')
