options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
clear_api_cache()
record_acquired_resource<-function(...)TRUE
requests<-new.env();requests$n<-0L
fm_async_read_http<-function(url,method,body,headers,owner){
 stopifnot(identical(url,'https://api.futmondo.com/2/locker/news'))
 requests$n<-requests$n+1L;q<-jsonlite::fromJSON(body)$query
 page<-if(!nzchar(q$from))1L else as.integer(sub('event','',q$from))+1L
 news<-if(page<=35L)list(list(`_id`=paste0('event',page),styp='info',txt='No financial event'))else list()
 response<-list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=list(news=news)),auto_unbox=TRUE)),times=list(total=0))
 promises::promise(function(resolve,reject)later::later(function()resolve(response),.001))
}
await<-function(p){done<-FALSE;out<-NULL;error<-NULL;promises::then(p,function(x){out<<-x;done<<-TRUE},function(e){error<<-e;done<<-TRUE});deadline<-Sys.time()+20;while(!done&&Sys.time()<deadline)later::run_now(.01);stopifnot(done);if(!is.null(error))stop(error);out}
auth<-c(userid='u',token='fixture');args<-list(login=auth,championship_id='c');ctx<-resource_context(auth,'c',session_id='fixture')
beat<-FALSE;later::later(function()beat<<-TRUE,.005)
x<-await(async_adapter_fetch('get_locker_clause_payments',args,ctx));stopifnot(nrow(x)==0,requests$n==36L,beat)
await(async_adapter_fetch('get_locker_clause_payments',args,ctx));stopifnot(requests$n==36L)
invalidate_resources(resource_context(auth,'c'),pattern='pressroom')
await(async_adapter_fetch('get_locker_clause_payments',args,ctx));stopifnot(requests$n==72L)
args$login[['userid']]<-'other';ctx<-resource_context(args$login,'c',session_id='fixture')
await(async_adapter_fetch('get_locker_clause_payments',args,ctx));stopifnot(requests$n==108L)
cat('PASS 36-page cooperative locker acquisition, heartbeat, zero-request warm reuse, targeted transfer refresh and account isolation\n')
# Timestamped source news can reuse a complete baseline, including an empty
# clause subset: the checkpoint belongs to all source news, not payment rows.
clear_api_cache();clear_completed_history_baselines()
requests$n<-0L;mode<-'ok';start<-as.POSIXct('2026-08-01',tz='UTC')
clause<-function(amount)paste0('El equipo <strong>Buyer</strong> ha pagado <strong>',amount,'</strong> propiedad de <strong>Seller</strong> como clausula de <strong>Player</strong>')
news<-lapply(seq_len(300),function(i)list(`_id`=paste0('news',i),created=format(start+i*3600,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),styp=if(i%%10==5)'clause'else'info',txt=if(i%%10==5)clause(100)else'Information'))
fm_async_read_http<-function(url,method,body,headers,owner){
 requests$n<-requests$n+1L;q<-jsonlite::fromJSON(body)$query
 last<-if(nzchar(q$from))as.integer(sub('news','',q$from))-1L else length(news)
 rows<-if(last>0L)rev(news[seq_len(last)])else list()
 rows<-if(mode=='repeat')head(rev(news),10L)else head(rows,75L)
 promises::promise_resolve(list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=list(news=rows)),auto_unbox=TRUE)),times=list(total=0)))
}
auth<-c(userid='u',token='fixture');args<-list(login=auth,championship_id='c');ctx<-resource_context(auth,'c',session_id='fixture')
resource<-'pressroom_locker_payments_c';key<-api_cache_key(resource,context=resource_context(auth,'c'))
read<-function()await(async_adapter_fetch('get_locker_clause_payments',args,ctx))
expire<-function(){api_cache_env[[key]]$time<-Sys.time()-301}
x<-read();stopifnot(nrow(x)==30L,requests$n==5L,as.numeric(attr(x,'source_latest_at'))==as.numeric(fm_time(news[[300]]$created)))
news[[295]]$txt<-clause(999);news[[298]]$styp<-'clause';news[[298]]$txt<-clause(7)
expire();before<-requests$n;x<-read()
stopifnot(requests$n==before+1L,nrow(x)==31L,x$price[x$id=='locker:news295']==999,x$price[x$id=='locker:news298']==7)
invalidate_resources(resource_context(auth,'c'),pattern='pressroom');before<-requests$n;x<-read();stopifnot(requests$n==before+1L)
news[[5]]$txt<-clause(91);attr(.transaction_history_baselines[[key]]$data,'full_history_at')<-Sys.time()-8*86400
expire();before<-requests$n;x<-read();stopifnot(requests$n==before+1L,x$price[x$id=='locker:news5']!=91)
before<-requests$n;x<-await(async_adapter_fetch('get_locker_clause_payments',c(args,list(full_audit=TRUE)),ctx))
stopifnot(requests$n==before+5L,x$price[x$id=='locker:news5']==91)
previous<-read_completed_history_baseline(resource,resource_context(auth,'c'))
mode<-'repeat';expire();x<-suppressMessages(read())
stopifnot(identical(attr(x,'fetch_status'),'stale'),identical(read_completed_history_baseline(resource,resource_context(auth,'c')),previous))
cat('PASS one-day locker overlap, all-news watermark, corrected/late payments, refresh baseline reuse, explicit full audit, failed checkpoint retention; full_reads=5 overlap_reads=1\n')
