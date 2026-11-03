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
