options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
for(expr in parse('worker/helper.R'))if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
 as.character(expr[[2]])%in%c('fm_worker_failure_code','fm_pipe_exchange','fm_worker_run','fm_worker_discover'))eval(expr,.GlobalEnv)
for(text in c('private response body','Unsupported worker class: private text'))
 stopifnot(fm_worker_failure_code(simpleError(text))=='resource_parse')
stopifnot(fm_worker_failure_code(simpleError('Unsupported worker class'))=='resource_parse_codec_class')
fm_pipe_send<-function(value)invisible(NULL)
pipe_in<-textConnection('{"error":"database_400"}')
error<-tryCatch(fm_pipe_exchange(list(type='database')),error=function(e)e);close(pipe_in)
failure<-resource_failure(error,'get_players_from_team')
stopifnot(failure$category=='http',failure$resource=='database',failure$http_status==400L,
 fm_worker_failure_code(failure)=='resource_parse_http_database_400')
cat('PASS only fixed codes/category/resource/status cross the bridge; private exception text is excluded\n')

outer<-get_cached_data('classification_ranking_fixture',{
 cache<-resource_optional_cache_read(function(){
  stop(resource_log_failure(resource_failure(resource='database',category='http',http_status=400)))
 })
 stopifnot(is.null(cache));structure(list(rows=data.frame(points=-2)),observed_at=Sys.time())
},context=resource_context(account_id='fixture',championship_id='l'))
stopifnot(is.null(attr(outer,'fetch_error')),outer$rows$points==-2)
control<-structure(list(message='Awaiting read'),class=c('fm_async_request','condition'))
propagated<-tryCatch(resource_optional_cache_read(function()stop(control)),fm_async_request=function(e)e)
stopifnot(inherits(propagated,'fm_async_request'))
cat('PASS an optional cache miss does not taint validated fresh data; async awaiting control still propagates\n')

get_real_clubs<-function(login,championship_id)structure(data.frame(teamId='club',team='Fixture'),encode_marker=TRUE)
fm_pipe_exchange<-function(...)stop('Top-level source should not request a child snapshot')
original_encode<-fm_worker_encode;encoded_frames<-0L
fm_worker_encode<-function(x,depth=0L){
 if(is.data.frame(x)&&isTRUE(attr(x,'encode_marker')))encoded_frames<<-encoded_frames+1L
 original_encode(x,depth)
}
ctx<-resource_context(account_id='fixture',championship_id='l')
descriptor<-fm_worker_descriptor('get_real_clubs',list(championship_id='l'),ctx)
result<-fm_worker_run(descriptor)
stopifnot(encoded_frames==1L,length(result$resources)==0L,fm_worker_decode(result$value)$teamId=='club')
cat('PASS the top-level source result is encoded once while final typed data is preserved\n')
