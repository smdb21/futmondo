#!/usr/bin/env Rscript
# Real replay adapter and shared-resource boundary; synthetic HTTP and storage.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
settle<-function(session)for(i in 1:20){later::run_now(0);session$flushReact()}
auth<-c(userid='lineup-latency-fixture',token='synthetic')
ctx<-resource_context(auth,'league','rival',season=league_season_context('league'))
answer<-function(points)list(strategy='3-5-2',status='running',pointSystem='custom',
  players=list(list(id='p1',name='Fixture Player',position=0,role='delantero',rated=TRUE,points=0,customPoints=points)),
  bench=list(players=list()))
fixture<-new.env(parent=emptyenv())
setup<-function(age=600,wrong_scope=FALSE){
  clear_api_cache()
  stamp<-Sys.time()-age
  scope<-round_lineup_scope(ctx);if(wrong_scope)scope$user_team_id<-'another-team'
  payload<-list(kind='classification_round_lineup',version=2L,scope=scope,round_id='r1',answer=answer(8),
    source='futmondo',source_observed_at=rival_finance_stamp(stamp),valid_until=rival_finance_stamp(stamp+300),completeness='complete')
  fixture$saved<-list(rival_finance_evidence(ctx$account_id,ctx$championship_id,ctx$season,'eligibility',
    round_lineup_snapshot_id(ctx,'r1'),payload,stamp))
  fixture$observed_at<-fm_time(payload$source_observed_at)
  fixture$valid_until<-as.numeric(fm_time(payload$valid_until))
  fixture$methods<-character();fixture$writes<-list();fixture$pending<-NULL
}
get_sb_url<-function()'https://fixture.invalid'
get_sb_key<-function()'synthetic-db-key'
defer_persistence<-function(fn,args,context,...){
  stopifnot(fn=='supabase_post_direct',args[[1]]=='rival_finance_evidence')
  fixture$writes<-c(fixture$writes,args[[2]]);TRUE
}
fm_async_read_http<-function(url,method='GET',body=NULL,headers=character(),...){
  fixture$methods<-c(fixture$methods,method)
  response<-function(value)list(status_code=200L,content=charToRaw(as.character(jsonlite::toJSON(value,
    auto_unbox=TRUE,null='null',na='null',digits=NA))),times=list(total=0))
  if(method=='GET'){
    stopifnot(grepl('/rival_finance_evidence_latest?',url,fixed=TRUE))
    return(promises::promise_resolve(response(fixture$saved)))
  }
  stopifnot(url=='https://api.futmondo.com/1/userteam/roundlineup')
  request<-jsonlite::fromJSON(body,simplifyVector=FALSE)
  stopifnot(identical(request$query,list(championshipId='league',userteamId='rival',round='r1')))
  promises::promise(function(resolve,reject){fixture$pending<-list(
    resolve=function(points=9)resolve(response(list(code=API_CODE_OK,answer=answer(points)))),reject=reject)})
}
app<-function(input,output,session){
  read<-factual_read_adapter(session,'get_round_lineup_snapshot',function()auth,function()'league',function()'own',state_only=TRUE)
  snapshot<-reactive(read(auth,'league','rival','r1'))
  output$lineup<-renderUI({
    state<-snapshot()
    if(is.null(state$data))return(p('Loading lineup'))
    tagList(if(state$freshness=='stale')p('Showing saved lineup while checking the source'),round_lineup_pitch_ui(state$data$answer))
  })
  output$heartbeat<-renderText(input$heartbeat)
}
setup(age=0)
shiny::testServer(app,{
  settle(session)
  check('fresh persisted lineup needs one database read and no source request or rewrite',{
    stopifnot(snapshot()$data$answer$pointSystem=='custom',identical(fixture$methods,'GET'),
      length(fixture$writes)==0L,grepl('8*',output$lineup$html,fixed=TRUE))
    settle(session);stopifnot(identical(fixture$methods,'GET'))
  })
})
setup()
shiny::testServer(app,{
  settle(session)
  check('expired saved lineup paints while the single source request remains held',{
    state<-snapshot()
    stopifnot(identical(fixture$methods,c('GET','POST')),length(fixture$writes)==0L,
      state$status=='loading',state$freshness=='stale',state$data$valid_until==fixture$valid_until,
      identical(as.numeric(state$observed_at),as.numeric(fixture$observed_at)),
      grepl('Showing saved lineup',output$lineup$html,fixed=TRUE),grepl('8*',output$lineup$html,fixed=TRUE))
    session$setInputs(heartbeat='responsive');settle(session)
    stopifnot(output$heartbeat=='responsive',identical(fixture$methods,c('GET','POST')))
  })
  fixture$pending$resolve();settle(session)
  check('fresh correction replaces the preview and persists once without another database read',{
    state<-snapshot()
    stopifnot(state$status=='ok',state$freshness=='fresh',state$data$valid_until>as.numeric(Sys.time()),
      grepl('9*',output$lineup$html,fixed=TRUE),!grepl('Showing saved lineup',output$lineup$html,fixed=TRUE),
      identical(fixture$methods,c('GET','POST')),length(fixture$writes)==1L,
      fixture$writes[[1]]$payload$answer$players[[1]]$customPoints==9)
  })
})
setup()
shiny::testServer(app,{
  settle(session);fixture$pending$reject(resource_failure(category='network'));settle(session)
  check('failed refresh retains truthful saved scores and original deadline with no rewrite',{
    state<-snapshot()
    stopifnot(state$freshness=='stale',!is.null(state$error),state$data$valid_until==fixture$valid_until,
      grepl('8*',output$lineup$html,fixed=TRUE),length(fixture$writes)==0L,
      identical(fixture$methods,c('GET','POST')))
  })
})
setup(wrong_scope=TRUE)
shiny::testServer(app,{
  settle(session)
  check('wrong-team saved payload is never published while the source is pending',{
    stopifnot(is.null(snapshot()$data),!grepl('8*',output$lineup$html,fixed=TRUE),
      identical(fixture$methods,c('GET','POST')))
  })
  fixture$pending$resolve();settle(session)
  stopifnot(grepl('9*',output$lineup$html,fixed=TRUE),length(fixture$writes)==1L)
})
clear_api_cache()
shiny::testServer(function(input,output,session){
  context<-reactiveVal(resource_context(account_id='old',championship_id='old-league',user_team_id='old-team'))
  callbacks<-new.env(parent=emptyenv())
  state<-shared_resource(session,'progress-fixture',context=context,progressive=TRUE,fetch=function(ctx,publish){
    promises::promise(function(resolve,reject)callbacks[[ctx$account_id]]<-list(publish=publish,resolve=resolve))
  })
  output$status<-renderText(state()$status)
},{
  settle(session)
  callbacks$old$publish(list(label='old'));settle(session);stopifnot(state()$data$label=='old')
  context(resource_context(account_id='new',championship_id='new-league',user_team_id='new-team'));settle(session)
  callbacks$old$publish(list(label='obsolete'));callbacks$old$resolve(list(label='obsolete'));settle(session)
  check('late progress and completion cannot cross a changed account/league/team context',{
    stopifnot(is.null(state()$data))
    callbacks$new$publish(list(label='current'));settle(session);stopifnot(state()$data$label=='current')
    callbacks$new$resolve(list(label='fresh'));settle(session);stopifnot(state()$data$label=='fresh',state()$status=='ok')
    callbacks$old$publish(list(label='obsolete'));settle(session);stopifnot(state()$data$label=='fresh')
  })
})
cat(sprintf('ROUND LINEUP PROGRESSIVE: %d passed / 0 failed\n',checks))
