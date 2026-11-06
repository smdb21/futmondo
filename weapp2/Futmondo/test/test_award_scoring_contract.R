#!/usr/bin/env Rscript
# Real read replay and controller; only external HTTP, catalog storage and save
# acknowledgement are synthetic. No live Futmondo or database access is allowed.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
auth<-c(userid='award-scoring-fixture',token='synthetic-token');cid<-'award-league'
custom<-list(ps=NULL,media=lapply(seq_len(4),function(i)list(media=paste0('media-',i),pct=c(45,25,20,10)[i])),stats='ff')
reordered<-list(stats='ff',media=lapply(custom$media,function(x)x[c('pct','media')]),ps=NULL)
version<-award_scoring_version('custom',custom)
reference<-list(round_id='r1',round_number=1,is_finished=TRUE)
xi<-function(scoring=reordered,mode='custom')list(players=lapply(seq_len(11),function(i)
  list(id=paste0('p',i),name=paste('Player',i),role=1,points=i)),mvp='p1',strategy='4-4-2',pointSystem=mode,custom=scoring)
roundtrip<-function(rows)rival_evidence_decode(as.data.frame(jsonlite::fromJSON(
  jsonlite::toJSON(rows,auto_unbox=TRUE,na='null',null='null',digits=NA))))
snapshot<-award_snapshot_evidence(normalize_official_dreamteam(xi(),cid,reference),auth,cid,reference,resource_context(auth,cid))
check('canonical scoring ignores recursive object order and preserves arrays, nulls and value types',{
  stopifnot(identical(version,award_scoring_version('custom',reordered)))
  nested<-list(b=list(y=2,x=1),a=TRUE);nested_reordered<-list(a=TRUE,b=list(x=1,y=2))
  stopifnot(identical(award_scoring_version('custom',nested),award_scoring_version('custom',nested_reordered)))
  changed<-custom;changed$media<-rev(changed$media)
  absent<-custom;absent$ps<-NULL
  typed<-custom;typed$media[[1]]$pct<-'45'
  weighted<-custom;weighted$media[[1]]$pct<-45.00001
  for(value in list(changed,absent,typed,weighted))stopifnot(!identical(version,award_scoring_version('custom',value)))
  stopifnot(!identical(version,award_scoring_version('stats',custom)),award_scoring_version(NULL,custom)=='unknown',
    award_scoring_version('custom',NULL)=='unknown')
})
check('legacy fingerprints are recomputed from stored metadata after the actual JSON read roundtrip',{
  legacy<-snapshot
  legacy$payload$scoring_version<-mvp_text(normalize_league_rules(list(),list(pointSystem='custom',custom=reordered))$scoring_version)
  stopifnot(!identical(legacy$payload$scoring_version,version))
  decoded<-roundtrip(list(legacy));stopifnot(is.data.frame(decoded[[1]]$payload$custom$media))
  saved<-award_saved_rows(decoded,cid,'unknown',version,auth[['userid']])
  stopifnot(nrow(saved)==11L,all(saved$scoring_version==version),all(saved$persistence=='Saved'))
  for(field in c('account_id','championship_id','season')){
    foreign<-legacy;foreign[[field]]<-'foreign'
    stopifnot(nrow(award_saved_rows(roundtrip(list(foreign)),cid,'unknown',version,auth[['userid']]))==0L)
  }
  for(kind in c('different','unknown')){
    wrong<-snapshot;wrong$payload$scoring_version<-version
    if(kind=='different')wrong$payload$custom$media[[1]]$pct<-99 else wrong$payload['pointSystem']<-list(NULL)
    stopifnot(nrow(award_saved_rows(roundtrip(list(wrong)),cid,'unknown',version,auth[['userid']]))==0L)
  }
  legacy$payload[c('pointSystem','custom')]<-NULL;legacy$payload$scoring_version<-version
  stopifnot(nrow(award_saved_rows(list(legacy),cid,'unknown',version,auth[['userid']]))==11L)
})

fixture<-new.env(parent=emptyenv());fixture$mode<-'api';fixture$persisted<-list();fixture$pending<-list();fixture$requests<-list()
catalog<-list(rounds=data.frame(round_id='r1',round_number=1,is_finished=TRUE,status='done'),
  complete=TRUE,reason='',user_team_id='own-team',current_round_number=1)
original_catalog<-get_mvp_saved_catalog;original_save<-persist_official_award_snapshot
original_http<-fm_async_http;original_post<-futmondo_post;original_url<-get_sb_url;original_key<-get_sb_key
get_mvp_saved_catalog<-function(login,championship_id){
  stored<-if(length(fixture$persisted))roundtrip(fixture$persisted)else list()
  list(saved=award_saved_rows(stored,championship_id,'unknown',version,login[['userid']]),
    expected_scoring=version,saved_complete=TRUE,catalog=catalog)
}
persist_official_award_snapshot<-function(result,login,championship_id,reference,context,on_complete){
  row<-award_snapshot_evidence(result,login,championship_id,reference,context)
  stopifnot(!is.null(row));fixture$pending[[reference$round_id]]<-list(row=row,complete=on_complete);TRUE
}
futmondo_post<-function(...)stop('Unexpected synchronous external request')
get_sb_url<-function()'';get_sb_key<-function()''
fm_async_http<-function(url,method='GET',body=NULL,headers=character(),owner='',...){
  stopifnot(identical(method,'POST'))
  sent<-jsonlite::fromJSON(as.character(body),simplifyVector=FALSE)
  stopifnot(identical(sent$header$userid,unname(auth[['userid']])),identical(sent$header$token,unname(auth[['token']])))
  fixture$requests[[length(fixture$requests)+1L]]<-list(url=url,query=sent$query)
  if(identical(url,DREAMTEAM_URL)){
    stopifnot(identical(sent$query,list(championshipId=cid,type='dreamteam',round='r1')))
    if(fixture$mode=='http')return(promises::promise_reject(resource_failure(resource=url,category='http',http_status=403L)))
    if(fixture$mode=='network')return(promises::promise_reject(resource_failure(resource=url,category='network')))
    if(fixture$mode=='parser')return(promises::promise_resolve(list(status_code=200L,content=charToRaw('{invalid-json'),times=list(total=0))))
    answer<-switch(fixture$mode,api=list(error=TRUE,code='api.error.invalid'),malformed=list(players=list(),mvp='p1'),
      missing=xi(mode=NULL),missing_custom=xi(scoring=NULL),weight={x<-xi();x$custom$media[[1]]$pct<-99;x},mode=xi(mode='stats'),ok=xi(),stop('Unexpected fixture mode'))
  }else if(identical(url,ACTIVE_CHAMPIONSHIPS_URL)){
    answer<-list(championships=list(list(id=cid,league='league-source',ps='custom',custom=custom,userteam=list(id='own-team'))),
      rounds=list(list(`_id`='r1',number=1)))
  }else if(identical(url,'https://api.futmondo.com/2/league/list')){
    answer<-list(list(`_id`='league-source',rounds=list(list(`_id`='r1',number=1,status='done'))))
  }else stop('Unexpected external HTTP route')
  promises::promise_resolve(list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=answer),
    auto_unbox=TRUE,null='null',digits=NA)),times=list(total=0)))
}
options(futmondo.offline=FALSE)
settle<-function(session,predicate,timeout=3){
  deadline<-Sys.time()+timeout
  repeat{session$flushReact();later::run_now(.005);session$flushReact()
    if(isTRUE(shiny::isolate(predicate())))return(invisible(NULL))
    if(Sys.time()>deadline)stop('Award scoring fixture timed out')}
}
reset_fixture<-function(mode){clear_api_cache();fixture$mode<-mode;fixture$persisted<-list();fixture$pending<-list();fixture$requests<-list()}
dreamteam_reads<-function()sum(vapply(fixture$requests,function(x)identical(x$url,DREAMTEAM_URL),logical(1)))
check('API, HTTP, network, parser and malformed responses retain the real failure reason with known scoring',{
  for(kind in c('api','http','network','parser','malformed')){
    reset_fixture(kind)
    shiny::testServer(function(input,output,session){source<-shared_mvp_source(session,function()auth,function()cid,function()TRUE)},{
      settle(session,function()!source()$loading);stopifnot(length(fixture$requests)==0L)
      stopifnot(attr(source,'retrieve')('missing'));settle(session,function()!source()$running)
      issue<-source()$coverage$issue
      expected<-if(kind=='api')'Futmondo rejected the request (api.error.invalid).'else if(kind=='http')
        'Futmondo access was denied. Log in again.'else if(kind=='parser')'Futmondo returned a malformed data response.'else if(kind=='malformed')
        'No identifiable official MVP was returned; publication status is unknown.'else
        'Futmondo data could not be loaded because the connection failed. Refresh to retry.'
      stopifnot(identical(issue,expected),source()$failed==1L,source()$fetched==0L,source()$saved==0L,
        !length(fixture$pending),dreamteam_reads()==if(kind=='network')2L else 1L)
    })
  }
})
check('valid XI with missing scoring is distinct from genuine weight or mode contradictions',{
  for(kind in c('missing','missing_custom','weight','mode')){
    reset_fixture(kind)
    shiny::testServer(function(input,output,session){source<-shared_mvp_source(session,function()auth,function()cid,function()TRUE)},{
      settle(session,function()!source()$loading);attr(source,'retrieve')('missing');settle(session,function()!source()$running)
      expected<-if(kind%in%c('missing','missing_custom'))'Selection scoring metadata is missing or unrecognized; compatibility with the league configuration could not be verified.'else
        'Selection scoring differs from the league configuration; it was not used.'
      stopifnot(source()$coverage$issue==expected,source()$failed==1L,source()$fetched==0L,
        nrow(source()$rows)==0L,!length(fixture$pending),dreamteam_reads()==1L)
    })
  }
})
check('explicit selected retry evicts only scoped source caches and waits for acknowledged canonical XI storage',{
  reset_fixture('api')
  foreign<-resource_context(account_id='other-account',championship_id=cid)
  other_league<-resource_context(auth,'other-league')
  unrelated<-paste0('official_dreamteam_',cid,'_r2')
  get_cached_data(paste0('official_dreamteam_',cid,'_r1'),list(marker='foreign'),context=foreign)
  get_cached_data(paste0('official_dreamteam_other-league_r1'),list(marker='other'),context=other_league)
  get_cached_data(unrelated,list(marker='another-round'),context=resource_context(auth,cid))
  get_cached_data(paste0('mvp_catalog_',cid),catalog,context=resource_context(auth,cid))
  shiny::testServer(function(input,output,session){source<-shared_mvp_source(session,function()auth,function()cid,function()TRUE)},{
    settle(session,function()!source()$loading);attr(source,'retrieve')('missing');settle(session,function()!source()$running)
    stopifnot(source()$failed==1L,dreamteam_reads()==1L)
    fixture$mode<-'ok';stopifnot(attr(source,'retrieve')('selected','r1'))
    settle(session,function()length(fixture$pending)==1L)
    state<-session$rootScope()$userData$mvp_shared
    stopifnot(state$fetched==1L,state$failed==0L,source()$saved==0L,source()$running,
      all(state$rows$persistence=='Saving'),dreamteam_reads()==2L,
      sum(vapply(fixture$requests,function(x)identical(x$url,ACTIVE_CHAMPIONSHIPS_URL),logical(1)))==1L,
      sum(vapply(fixture$requests,function(x)identical(x$url,'https://api.futmondo.com/2/league/list'),logical(1)))==1L)
    stopifnot(exists(api_cache_key(paste0('official_dreamteam_',cid,'_r1'),context=foreign),api_cache_env,inherits=FALSE),
      exists(api_cache_key('official_dreamteam_other-league_r1',context=other_league),api_cache_env,inherits=FALSE),
      exists(api_cache_key(unrelated,context=resource_context(auth,cid)),api_cache_env,inherits=FALSE))
    pending<-fixture$pending[['r1']];fixture$persisted<-list(pending$row);pending$complete(TRUE)
    settle(session,function()!source()$running)
    stopifnot(source()$saved==1L,source()$complete,source()$coverage$persistence=='Saved',
      all(source()$rows$scoring_version==version),!nzchar(source()$coverage$issue))
  })
  before<-length(fixture$requests)
  shiny::testServer(function(input,output,session){source<-shared_mvp_source(session,function()auth,function()cid,function()TRUE)},{
    settle(session,function()!source()$loading)
    stopifnot(source()$complete,nrow(source()$rows)==11L,source()$coverage$persistence=='Saved')
    attr(source,'retrieve')('missing');settle(session,function()!source()$running)
    stopifnot(length(fixture$requests)==before)
  })
  cat('MEASURE rejected_read=1 selected_retry_dreamteam_reads=1 catalog_reads=2 saved_reuse_reads=0\n')
})
get_mvp_saved_catalog<-original_catalog;persist_official_award_snapshot<-original_save
fm_async_http<-original_http;futmondo_post<-original_post;get_sb_url<-original_url;get_sb_key<-original_key
options(futmondo.offline=TRUE)
cat(sprintf('AWARD SCORING CONTRACT: %d passed / 0 failed\n',n))
