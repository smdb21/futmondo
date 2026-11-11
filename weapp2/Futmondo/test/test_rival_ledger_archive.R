options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
args<-commandArgs(trailingOnly=TRUE)
ctx<-resource_context(account_id='archive-user',championship_id='archive-league',season='s',user_team_id='a')
db<-list();queued<-list()
jsonb<-function(value){if(is.list(value)){if(!is.null(names(value)))value<-value[sort(names(value))];value<-lapply(value,jsonb)};value}
frame<-function(rows)jsonlite::fromJSON(as.character(jsonlite::toJSON(jsonb(unname(rows)),auto_unbox=TRUE,na='null',null='null',digits=NA)))
supabase_get<-function(table,query){
 stopifnot(table=='rival_finance_evidence_latest',query$account_id=='eq.archive-user',query$season=='eq.s')
 ids<-if(startsWith(query$source_id,'eq.'))substring(query$source_id,4L)else strsplit(sub('^in.\\((.*)\\)$','\\1',query$source_id),',',fixed=TRUE)[[1]]
 selected<-db[intersect(ids,names(db))]
 if(!length(selected))return(data.frame())
 frame(selected)
}
if(length(args)&&args[1]=='--child'){
 db<-jsonlite::fromJSON(args[2],simplifyVector=FALSE)
 x<-read_rival_calculation_snapshot('archive-user','archive-league','s','a')
 stopifnot(!is.null(x),is.null(x$cache),nrow(x$cash_ledgers$a)==5001L,
   sum(x$cash_ledgers$a$amount[x$cash_ledgers$a$included])==x$team_finances$estimated_cash[1],
   x$team_finances$estimated_cash[1]==-4900,!anyDuplicated(x$cash_ledgers$a$id))
 cat('PASS separate R process restores balance with every associated transaction\n');quit(status=0)
}
teams<-data.frame(teamid='a',teamname='A')
cache<-rival_calculation_cache()
x<-build_rival_cash_dataset(teams,'archive-league',rules=list(initial_budget=100,money_per_point=0,ranking_pool=0),account_id='archive-user',season='s',cache=cache)
ledger<-x$cash_ledgers$a
rows<-ledger[rep(1L,5000L),,drop=FALSE]
rows$id<-paste0('transaction:',seq_len(5000L));rows$amount<--1;rows$included<-TRUE
rows$component<-vapply(seq_len(5000L),function(i)paste(rep(history_baseline_hash(charToRaw(as.character(i))),2),collapse=''),character(1))
x$cash_ledgers$a<-rbind(ledger,rows);rownames(x$cash_ledgers$a)<-NULL
x$team_finances$estimated_cash<--4900
cache$prepared$padding<-raw(.rival_snapshot_limits$json+1L)
encode_seconds<-system.time(payload<-rival_snapshot_payload(cache,x,ctx))[["elapsed"]]
stopifnot(!is.null(payload$ledger_manifest),length(attr(payload,'ledger_chunks'))>1L)
defer_persistence<-function(fn,args,context,on_complete,...){queued[[length(queued)+1L]]<<-list(args=args,callback=on_complete);TRUE}
ack<-NULL
stopifnot(persist_rival_calculation_snapshot(cache,x,ctx,function(ok)ack<<-ok),length(queued)==1L,is.null(ack))
# A failed first chunk cannot publish or acknowledge a new balance.
queued[[1]]$callback(FALSE);stopifnot(identical(ack,FALSE),!length(db),length(queued)==1L)
queued<-list();ack<-NULL
invisible(persist_rival_calculation_snapshot(cache,x,ctx,function(ok)ack<<-ok))
i<-1L
repeat{
 job<-queued[[i]];row<-job$args[[2]][[1]]
 if(!is.null(row$payload$ledger_manifest))stopifnot(i==length(payload$ledger_manifest$parts)+1L,is.null(ack))
 else stopifnot(is.null(read_rival_calculation_snapshot('archive-user','archive-league','s','a')))
 db[[row$source_id]]<-row;job$callback(TRUE)
 if(!is.null(ack))break
 i<-i+1L
}
stopifnot(isTRUE(ack),length(db)==length(payload$ledger_manifest$parts)+1L)
decode_seconds<-system.time(y<-read_rival_calculation_snapshot('archive-user','archive-league','s','a'))[["elapsed"]]
stopifnot(!is.null(y),identical(y$cash_ledgers,x$cash_ledgers),identical(y$team_finances,x$team_finances))
path<-tempfile(fileext='.json');writeLines(as.character(jsonlite::toJSON(db,auto_unbox=TRUE,na='null',null='null',digits=NA)),path)
status<-system2(file.path(R.home('bin'),'Rscript'),c('test/test_rival_ledger_archive.R','--child',shQuote(path)));unlink(path);stopifnot(status==0L)
cat(sprintf('MEASURE archive encode=%.3fs restore=%.3fs encoded=%.1f KiB\n',encode_seconds,decode_seconds,sum(vapply(attr(payload,'ledger_chunks'),function(row)nchar(row$payload$data,type='bytes'),numeric(1)))/1024))
cat(sprintf('PASS %.0f transaction rows, %d immutable chunks, acknowledged manifest last; no ledger truncation\n',nrow(y$cash_ledgers$a),length(payload$ledger_manifest$parts)))
# Missing, corrupt and foreign chunks never turn into a summary-only success.
key<-payload$ledger_manifest$parts[1];original<-db[[key]]
db[[key]]<-NULL;stopifnot(is.null(read_rival_calculation_snapshot('archive-user','archive-league','s','a')))
db[[key]]<-original;db[[key]]$payload$data<-paste0('bad',db[[key]]$payload$data)
stopifnot(is.null(read_rival_calculation_snapshot('archive-user','archive-league','s','a')))
db[[key]]<-original;db[[key]]$payload$scope$account_id<-'other'
stopifnot(is.null(read_rival_calculation_snapshot('archive-user','archive-league','s','a')))
cat('PASS missing/corrupt/foreign archive rejected; source acquisition remains required\n')
# Unchanged current-list movements are not written again; corrections/new IDs are.
e<-function(id,amount,at=Sys.time())rival_finance_evidence('u','l','s','movements',id,list(id=id,money=amount,date='2026-11-01T00:00:00Z'),at)
old<-list(e('one',10));same<-e('one',10,Sys.time()+1)
old[[1]]$payload<-old[[1]]$payload[rev(names(old[[1]]$payload))];old[[1]]$payload$money<-10L
stopifnot(!length(rival_finance_evidence_delta(old,list(same))),length(rival_finance_evidence_delta(old,list(e('one',11),e('two',0))))==2L)
cat('PASS only new or corrected movement payloads append DB evidence revisions\n')
# Only DB-confirmed movements suppress writes. In-session observations whose
# write failed must retry on refresh, even though the live ledger already uses them.
clear_api_cache();attempts<-0L
observation<-function(amount){
 rows<-normalize_money_movements(list(list(id='income',type='bonus',money=amount,date='2026-11-01T00:00:00Z')))
 attr(rows,'observed_at')<-Sys.time()
 list(team_id='a',movements=rows)
}
saved_rows<-rival_team_evidence(observation(10),'u','l','unknown')
live<-observation(11)
persist_rival_finance_evidence<-function(rows,context){attempts<<-attempts+length(rival_evidence_rows(rows,'movements'));FALSE}
async_adapter_fetch<-function(fn,args,context,...){
 value<-switch(fn,read_rival_calculation_snapshot=NULL,
  get_rival_finance_saved=list(evidence=saved_rows,complete=TRUE),
  get_rival_finance_seed=list(rules=list(initial_budget=100,money_per_point=0,ranking_pool=0),pressroom=data.frame()),
  get_rival_team_finance_observation=live)
 promises::promise_resolve(value)
}
shiny::testServer(function(input,output,session){
 revision<-shiny::reactiveVal(0L)
 feed<-shared_rival_finance_source(session,function()c(userid='u',token='fixture'),function()'l',function()'a',
  function()data.frame(teamid='a',teamname='A'),function()TRUE,function()list(rows=data.frame(),complete=FALSE),revision)
},{
 flush<-function(){for(i in seq_len(30L)){later::run_now(0);session$flushReact()}}
 flush();stopifnot(attempts==1L)
 revision(1L);flush();stopifnot(attempts==2L)
 saved_rows<<-rival_team_evidence(live,'u','l','unknown')
 revision(2L);flush();stopifnot(attempts==2L)
})
cat('PASS failed movement persistence retries after refresh; only confirmed DB state suppresses repeat writes\n')
# A valid durable transfer baseline also avoids a second full transaction-table
# scan while the seed later checks only the source head/overlap.
read_rival_finance_evidence<-function(...)list(evidence=list(),complete=TRUE)
read_completed_history_baseline<-function(resource,context){
 stopifnot(resource=='pressroom_l',context$account_id=='u',context$season=='s')
 rows<-data.frame(id='saved',buyer_team_id='a',seller_team_id='',price=1)
 attr(rows,'fetch_status')<-'ok';rows
}
get_rival_saved_transfers<-function(...)stop('redundant historical transaction scan')
restored<-get_rival_finance_saved('u','l','s')
stopifnot(nrow(restored$pressroom)==1L,restored$pressroom$id=='saved')
cat('PASS saved finances reuse the durable transfer baseline before scanning historical transaction rows\n')
