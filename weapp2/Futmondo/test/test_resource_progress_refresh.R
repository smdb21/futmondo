#!/usr/bin/env Rscript
# A held initial GET keeps the actual replay read pending across a refresh.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('futmondo_functions.R'))
settle<-function(session)for(i in 1:20){later::run_now(0);session$flushReact()}
get_sb_url<-function()'https://fixture.invalid'
get_sb_key<-function()'synthetic'
record_acquired_resource<-function(...)invisible(TRUE)
requests<-character();held<-list();publishers<-list();reads<-0L
response<-function(label)list(status_code=200L,content=charToRaw(jsonlite::toJSON(
 list(list(label=label,observed_at='2026-01-01T00:00:00Z')),auto_unbox=TRUE)),times=list(total=0))
fm_async_read_http<-function(url,method='GET',...){
 stopifnot(method=='GET');index<-length(requests)+1L
 requests<<-c(requests,tail(strsplit(url,'/',fixed=TRUE)[[1]],1L))
 promises::promise(function(resolve,reject)held[[index]]<<-list(resolve=resolve,reject=reject))
}
get_refresh_progress_fixture<-function(){
 saved<-supabase_get('saved')
 publish_resource_progress(observed_data(saved,observed_at=fm_time(saved$observed_at[1])))
 latest<-supabase_get('latest')
 observed_data(latest,observed_at=fm_time(latest$observed_at[1]))
}
clear_api_cache()
shiny::testServer(function(input,output,session){
 refresh<-shiny::reactiveVal(0L)
 context<-function()resource_context(account_id='refresh-fixture',championship_id='league',user_team_id='team',session_id=session$token)
 state<-shared_resource(session,'refresh-progress-fixture',context=context,refresh=refresh,progressive=TRUE,
  fetch=function(ctx,publish){
   reads<<-reads+1L;publishers[[reads]]<<-publish
   async_adapter_fetch('get_refresh_progress_fixture',context=ctx,on_progress=publish)
  })
 output$label<-shiny::renderText(state()$data$label %||% 'loading')
},{
 settle(session);stopifnot(identical(requests,'saved'),is.null(state()$data))
 refresh(1L);settle(session)
 stopifnot(reads==1L,identical(requests,'saved'),is.null(state()$data))
 held[[1]]$resolve(response('saved'));settle(session)
 stopifnot(identical(requests,c('saved','latest')),state()$status=='loading',state()$freshness=='stale',
  output$label=='saved',identical(as.numeric(state()$observed_at),as.numeric(fm_time('2026-01-01T00:00:00Z'))))
 cat('PASS same-context refresh reuses the delayed GET and receives its saved preview before the source settles.\n')
 held[[2]]$resolve(response('fresh'));settle(session)
 stopifnot(state()$status=='ok',output$label=='fresh',reads==1L)
 publishers[[1]](data.frame(label='late'));settle(session)
 stopifnot(state()$status=='ok',output$label=='fresh')
 refresh(2L);settle(session)
 stopifnot(reads==2L,identical(requests,c('saved','latest','saved')))
 publishers[[1]](data.frame(label='obsolete'));settle(session)
 stopifnot(state()$status=='loading',output$label=='fresh')
 held[[3]]$resolve(response('saved again'));settle(session)
 stopifnot(output$label=='saved again',identical(requests,c('saved','latest','saved','latest')))
 held[[4]]$resolve(response('fresh again'));settle(session)
 stopifnot(state()$status=='ok',output$label=='fresh again')
 cat('PASS completed publishers retire and cannot replace data from a later read of the same context.\n')
})
cat('RESOURCE PROGRESS REFRESH: 2 passed / 0 failed\n')
