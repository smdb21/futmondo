options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
args<-commandArgs(trailingOnly=TRUE)
fixture<-function(){
 teams<-data.frame(teamid=c('a','b','c'),teamname=c('A','B','C'),teamValue=c(10,20,30),points=1:3,lastAccess='2026-11-01T00:00:00Z')
 rules<-list(initial_budget=100,money_per_point=0,ranking_pool=0)
 tx<-function(id,buyer,price)data.frame(id=id,created='2026-11-03T10:00:00Z',player_id=paste0('p',id),player_name='Player',buyer_team_id=buyer,buyer_team_name=toupper(buyer),seller_team_id='',seller_team_name='Market',price=price,is_clause=FALSE)
 evidence<-list(rival_finance_evidence('u','l','s','movements','a:one',list(id='one',team_id='a',type='buy',category='market',money=-10,date='2026-11-03T10:00:00Z',player_id='pone',source_event_id='one'),as.POSIXct('2026-11-03',tz='UTC')))
 list(teams=teams,championship_id='l',evidence=evidence,pressroom=tx('one','a',10),rules=rules,account_id='u',season='s',reference_team_id='a')
}
ctx<-list(account_id='u',championship_id='l',season='s',user_team_id='a')
row_frame<-function(payload){
 row<-rival_finance_evidence('u','l','s','eligibility',rival_snapshot_id(rival_snapshot_scope(ctx)),payload)
 jsonlite::fromJSON(as.character(jsonlite::toJSON(list(row),auto_unbox=TRUE,na='null',null='null',digits=NA)))
}
if(length(args)&&args[1]=='--child'){
 payload<-jsonlite::fromJSON(args[2],simplifyVector=FALSE)
 supabase_get<-function(...)row_frame(payload)
 saved<-read_rival_calculation_snapshot('u','l','s','a');stopifnot(!is.null(saved),is.list(saved$cache))
 cache<-rival_calculation_cache();invisible(list2env(saved$cache,envir=cache))
 # No history preparation, trade matching or team reconstruction may occur.
 rival_historical_components<-function(...)stop('unchanged snapshot rebuilt history')
 rival_realized_trades<-function(...)stop('unchanged snapshot rematched trades')
 result<-do.call(build_rival_cash_dataset,c(fixture(),list(cache=cache)))
 stopifnot(cache$hits==3L,is.null(cache$team_builds),is.null(cache$preparations),identical(result$team_finances,saved$team_finances))
 cat('PASS fresh process restores exact typed cache and reuses every unchanged ledger\n');quit(status=0L)
}
f<-fixture();cache<-rival_calculation_cache();cold<-do.call(build_rival_cash_dataset,c(f,list(cache=cache)))
payload<-rival_snapshot_payload(cache,cold,ctx);stopifnot(!is.null(payload))
path<-tempfile(fileext='.json')
writeLines(as.character(jsonlite::toJSON(payload,auto_unbox=TRUE,na='null',null='null',digits=NA)),path)
status<-system2(file.path(R.home('bin'),'Rscript'),c('test/test_rival_persistent_cache.R','--child',shQuote(path)))
unlink(path);stopifnot(status==0L)

supabase_get<-function(table,query){stopifnot(table=='rival_finance_evidence_latest',query$limit==1L,query$source_id==paste0('eq.',rival_snapshot_id(rival_snapshot_scope(ctx))));row_frame(payload)}
saved<-read_rival_calculation_snapshot('u','l','s','a');stopifnot(!is.null(saved))
restored<-rival_calculation_cache();invisible(list2env(saved$cache,envir=restored))
stopifnot(identical(restored$teams,cache$teams),identical(restored$prepared,cache$prepared))
# Normal private transfers invalidate only that team, including corrections/deletions.
f$evidence[[1]]$observed_at<-rival_finance_stamp()
movement<-f$evidence[[1]];movement$source_id<-'b:two';movement$payload$id<-'two';movement$payload$team_id<-'b';movement$payload$money<--5
f$evidence<-c(f$evidence,list(movement));new<-do.call(build_rival_cash_dataset,c(f,list(cache=restored)))
stopifnot(restored$team_builds==1L,restored$hits==2L,identical(new,do.call(build_rival_cash_dataset,f)))
f$evidence[[2]]$payload$money<--6;revised<-do.call(build_rival_cash_dataset,c(f,list(cache=restored)))
stopifnot(restored$team_builds==2L,identical(revised,do.call(build_rival_cash_dataset,f)))
f$evidence<-f$evidence[1];deleted<-do.call(build_rival_cash_dataset,c(f,list(cache=restored)))
stopifnot(restored$team_builds==3L,identical(deleted,do.call(build_rival_cash_dataset,f)))
f$rules$initial_budget<-200;invisible(do.call(build_rival_cash_dataset,c(f,list(cache=restored))))
stopifnot(restored$team_builds==6L)
cat('PASS new/revised/deleted private transfers rebuild one team; changed rules invalidate all\n')

# Live directory and observed cash changes refresh presentation without trade work.
f<-fixture();cache<-rival_calculation_cache();before<-do.call(build_rival_cash_dataset,c(f,list(cache=cache)))
info<-rival_finance_evidence('u','l','s','team','a',list(budget=90,observed_at=rival_finance_stamp()))
f$evidence<-c(f$evidence,list(info));f$teams$teamValue<-100:102;f$teams$points<-10:12
fresh<-do.call(build_rival_cash_dataset,c(f,list(cache=cache)))
stopifnot(cache$team_builds==3L,fresh$team_finances$observed_cash_fresh[1],fresh$team_finances$team_value[1]==100)
f$evidence[[2]]$payload$budget<-NA_real_;f$evidence[[2]]$payload$observed_at<-rival_finance_stamp()
missing<-do.call(build_rival_cash_dataset,c(f,list(cache=cache)))
stopifnot(cache$team_builds==3L,is.na(missing$team_finances$observed_cash[1]),!missing$team_finances$observed_cash_fresh[1])
old<-fresh$team_finances[1,,drop=FALSE];old$as_of<-'2020-01-01T00:00:00Z';old$observed_cash_at<-old$as_of
old<-rival_refresh_finance_row(old,f$teams[1,,drop=FALSE]);stopifnot(!old$observed_cash_fresh,is.na(old$budget),old$as_of=='2020-01-01T00:00:00Z')
uncoupled<-fresh$team_finances[1,,drop=FALSE];uncoupled$observed_cash_at<-''
uncoupled<-rival_refresh_finance_row(uncoupled,f$teams[1,,drop=FALSE]);stopifnot(!uncoupled$observed_cash_fresh,is.na(uncoupled$budget))
f$teams$teamValue<-NA_real_;f$teams$points<-NA_real_;f$evidence[[2]]$payload$squad_size<-12
metadata_unknown<-do.call(build_rival_cash_dataset,c(f,list(cache=cache)))
stopifnot(cache$team_builds==3L,identical(metadata_unknown,do.call(build_rival_cash_dataset,f)),
 is.na(metadata_unknown$team_finances$team_value[1]),is.na(metadata_unknown$team_finances$points[1]),metadata_unknown$team_finances$squad_size[1]==12)
exact<-rival_refresh_finance_row(fresh$team_finances[1,,drop=FALSE],fixture()$teams[1,,drop=FALSE],
 list(budget=90,observed_at=rival_finance_stamp()),missing=character(),assumptions=character())
different<-rival_refresh_finance_row(exact,fixture()$teams[1,,drop=FALSE],
 list(budget=80,observed_at=rival_finance_stamp()),missing=character(),assumptions=character())
stopifnot(!exact$net_gain_estimated,different$net_gain_estimated)
cat('PASS changing current metadata/cash never rematches; stale or unknown observations remain unknown\n')

# Acknowledgement follows successful storage; a failed enqueue/write is retryable.
callbacks<-list();writes<-0L
defer_persistence<-function(fn,args,context,on_complete=NULL,...){writes<<-writes+1L;callbacks[[writes]]<<-on_complete;TRUE}
ack<-NULL
stopifnot(persist_rival_calculation_snapshot(cache,missing,ctx,function(ok)ack<<-ok),is.null(ack),writes==1L)
callbacks[[1]](FALSE);stopifnot(identical(ack,FALSE))
invisible(persist_rival_calculation_snapshot(cache,missing,ctx,function(ok)ack<<-ok));callbacks[[2]](TRUE)
stopifnot(isTRUE(ack),writes==2L)
cat('PASS enqueue is pending until callback, failed writes remain retryable\n')

# Deliberately malformed scope/version/shape/size and self-evidence all miss safely.
original_payload<-payload
for(field in c('account_id','championship_id','season','user_team_id')){
 wrong<-ctx;wrong[[field]]<-'other'
 supabase_get<-function(...)row_frame(payload)
 stopifnot(is.null(do.call(read_rival_calculation_snapshot,wrong)))
}
mutate_wire<-function(fn){
 raw<-history_baseline_decompress(openssl::base64_decode(original_payload$data),original_payload$json_bytes)
 wire<-jsonlite::fromJSON(rawToChar(raw),simplifyVector=FALSE);wire<-fn(wire)
 bytes<-charToRaw(as.character(jsonlite::toJSON(wire,auto_unbox=TRUE,na='null',null='null',digits=NA)));gzip<-history_baseline_compress(bytes)
 p<-original_payload;p$data<-openssl::base64_encode(gzip);p$json_bytes<-length(bytes);p$sha256<-history_baseline_hash(gzip);p
}
for(change in list(function(w){w$scope$version<-'old';w},function(w){w$cache$values[[3]]$values$account_id<-list(type='null');w},function(w){w$summary$values$teamid$values<-list('a','a','c');w})){
 payload<-mutate_wire(change);supabase_get<-function(...)row_frame(payload)
 stopifnot(is.null(read_rival_calculation_snapshot('u','l','s','a')))
}
payload<-original_payload;payload$json_bytes<-.rival_snapshot_limits$json+1L
stopifnot(is.null(read_rival_calculation_snapshot('u','l','s','a')))
payload<-original_payload;payload$data<-'corrupt';stopifnot(is.null(read_rival_calculation_snapshot('u','l','s','a')))
payload<-original_payload
self<-rival_finance_evidence('u','l','s','eligibility',rival_snapshot_id(rival_snapshot_scope(ctx)),payload)
f<-fixture();f$evidence<-c(f$evidence,list(self))
stopifnot(identical(do.call(build_rival_cash_dataset,f),cold))
cat('PASS scope/version/shape/size rejection and snapshots excluded from their own accounting inputs\n')

# The first saved table must not wait for evidence/history or pretend it is fresh.
clear_api_cache();saved$team_finances$as_of<-'2020-01-01T00:00:00Z';saved$team_finances$observed_cash_fresh<-TRUE
pending<-list();requests<-character();snapshot_callbacks<-list()
league_season_context<-function(...)'s'
persist_rival_finance_evidence<-function(...)TRUE
defer_persistence<-function(fn,args,context,on_complete=NULL,...){snapshot_callbacks[[length(snapshot_callbacks)+1L]]<<-on_complete;TRUE}
async_adapter_fetch<-function(fn,args,context,...){
 requests<<-c(requests,fn)
 if(fn=='read_rival_calculation_snapshot')return(promises::promise_resolve(saved))
 if(fn=='get_rival_finance_seed')return(promises::promise_resolve(list(info=NULL,rules=fixture()$rules,pressroom=fixture()$pressroom)))
 if(fn=='get_rival_team_finance_observation')return(promises::promise_resolve(list(team_id=args$user_team_id,info=NULL,movements=NULL,rounds=NULL)))
 promises::promise(function(resolve,reject)pending[[fn]]<<-list(resolve=resolve,reject=reject))
}
flush<-function(session){for(i in seq_len(15L)){later::run_now(0);session$flushReact()}}
shiny::testServer(function(input,output,session){
 directory<-shiny::reactiveVal(fixture()$teams)
 source<-shared_rival_finance_source(session,function()c(userid='u',token='t'),function()'l',function()'a',directory,function()TRUE,function()list(rows=data.frame(),complete=FALSE))
},{
 flush(session)
 stopifnot(source()$calculation_snapshot_checking,identical(source()$calculation_snapshot_status,'saved'),
   identical(source()$team_finances$estimated_cash,c(90,100,100)),!any(source()$team_finances$observed_cash_fresh),
   identical(requests,c('read_rival_calculation_snapshot','get_rival_finance_saved')),length(snapshot_callbacks)==0L)
 # Failed evidence acquisition retains the labelled saved preview.
 pending[['get_rival_finance_saved']]$reject(simpleError('fixture saved evidence failure'));flush(session)
 stopifnot(!source()$loading,length(snapshot_callbacks)==1L,source()$calculation_snapshot_status=='pending')
 snapshot_callbacks[[1]](FALSE);flush(session)
 stopifnot(source()$calculation_snapshot_status=='unavailable')
 # A real dependency change retries; only the successful callback marks Saved.
 changed<-directory();changed$teamid[3]<-'d';changed$teamname[3]<-'D';directory(changed);flush(session)
 stopifnot(length(snapshot_callbacks)==2L,source()$calculation_snapshot_status=='pending')
 changed$teamid[3]<-'e';changed$teamname[3]<-'E';directory(changed);flush(session)
 stopifnot(length(snapshot_callbacks)==3L,source()$calculation_snapshot_status=='pending')
 snapshot_callbacks[[2]](TRUE);flush(session)
 stopifnot(source()$calculation_snapshot_status=='pending')
 snapshot_callbacks[[3]](TRUE);flush(session)
 snapshot_callbacks[[1]](FALSE);flush(session)
 stopifnot(source()$calculation_snapshot_status=='saved')
 before<-length(requests);writes_before<-length(snapshot_callbacks)
 changed$points<-changed$points+100;changed$teamValue<-changed$teamValue+10;directory(changed);flush(session)
 stopifnot(length(requests)==before,length(snapshot_callbacks)==writes_before,
   identical(source()$team_finances$points,as.numeric(changed$points)))
})
cat('PASS saved table paints before history; runtime write acknowledgement/failure/recovery and metadata-only reuse\n')
