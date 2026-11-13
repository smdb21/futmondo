#!/usr/bin/env Rscript
# Private headless bridge. Real credentials and HTTP connections belong to Python.
options(futmondo.worker_helper=TRUE)
pipe_in<-file('stdin',open='r');pipe_out<-stdout()
sink(stderr())
suppressPackageStartupMessages(source('global.R'))
source('history_repository.R')
if(file.exists('worker_contract.R'))source('worker_contract.R')
# Unexpected direct HTTP is a programming error, never an alternate acquisition path.
for(verb in c('GET','POST','PUT','PATCH','DELETE','HEAD'))
 assignInNamespace(verb,function(...)stop('Direct helper HTTP is disabled'),ns='httr')
for(name in c('curl_fetch_memory','curl_fetch_disk','curl_fetch_stream','multi_add'))
 assignInNamespace(name,function(...)stop('Direct helper HTTP is disabled'),ns='curl')
fm_pipe_send<-function(value){writeLines(as.character(jsonlite::toJSON(value,auto_unbox=TRUE,null='null',na='null',digits=NA)),pipe_out);flush(pipe_out)}
fm_pipe_exchange<-function(value){
 fm_pipe_send(value);line<-readLines(pipe_in,n=1L,warn=FALSE)
 if(!length(line))stop('Bridge closed')
 answer<-jsonlite::fromJSON(line,simplifyVector=FALSE)
 if(!is.null(answer$error)){
  code<-fm_scalar(answer$error,'')
  status<-if(grepl('^(database|source)_[1-5][0-9]{2}$',code))as.integer(sub('^.*_','',code))else NULL
  category<-if(!is.null(status))'http'else if(code%in%c('database_connection','source_transient'))'network'else'unknown'
  resource<-if(startsWith(code,'database_'))'database'else resource_diagnostic_name(value$url %||% '')
  stop(structure(list(message='Worker operation failed',call=NULL,code=code,category=category,http_status=status,resource=resource),class=c('fm_worker_bridge_error','error','condition')))
 }
 answer
}
fm_worker_discover<-function(handler,value,context,auth){
 out<-list();add<-function(fn,args=list(),ctx=context,interval=fm_worker_interval(fn)){
  d<-fm_worker_descriptor(fn,args,ctx);d$interval_seconds<-interval;out[[length(out)+1L]]<<-d
 }
 if(handler=='get_active_championships'&&is.list(value$championships)){
  add('get_notifications',ctx=resource_context(auth));add('get_notification_unread',ctx=resource_context(auth))
  for(ch in value$championships){
   cid<-fm_scalar(ch$id %||% ch$`_id`,'');tid<-fm_scalar(ch$userteam$id %||% ch$userteam$`_id`,'')
   if(!nzchar(cid)||!nzchar(tid))next
   ctx<-resource_context(auth,cid,tid);args<-list(championship_id=cid,user_team_id=tid)
   for(fn in c('get_teams','get_finished_rounds','get_championship_players','get_real_clubs','get_lineup_from_team','get_classification_round_catalog','get_award_round_catalog','get_mvp_saved_catalog'))
    add(fn,args[intersect(names(args),names(formals(get(fn))))],ctx)
   for(fn in c('get_market_players','get_players_from_team','get_user_team_info','get_financial_snapshot','get_my_market_players','get_roster_bids','get_user_team_rounds','fm_worker_finances'))add(fn,args,ctx)
   add('get_championship_pressroom',list(championship_id=cid),ctx)
   for(route in c('/1/userteam/lineup','/1/userteam/roster','/1/userteam/information','/5/strategy/availables')){
    query<-list(championshipId=cid,userteamId=tid);if(route=='/1/userteam/information')query$type<-'market'
    add('get_worker_http_read',list(route=route,query=query),ctx,60L)
   }
  }
 }
 if(handler%in%c('get_award_round_catalog','get_classification_round_catalog')&&is.data.frame(value$rounds)){
  for(i in seq_len(nrow(value$rounds))){
   row<-value$rounds[i,,drop=FALSE]
   if(!nzchar(fm_scalar(row$round_id,''))||!is.finite(fm_number(row$round_number)))next
   if(!isTRUE(row$is_finished)&&!identical(fm_scalar(row$round_id,''),fm_scalar(value$current_round_id,'')))next
   args<-list(championship_id=context$championship_id,user_team_id=fm_scalar(value$user_team_id,context$user_team_id),round_id=as.character(row$round_id),round_number=as.numeric(row$round_number))
   add('get_classification_round_ranking',args,context,if(isTRUE(row$is_finished))86400L else 60L)
  }
 }
 if(handler=='get_championship_players'&&is.data.frame(value)&&'id'%in%names(value)){
  for(id in unique(as.character(value$id))){
   if(is.na(id)||!nzchar(id))next
   ctx<-context;ctx$player_id<-id
   args<-list(championship_id=ctx$championship_id,user_team_id=ctx$user_team_id,player_id=id)
   add('get_player_summary',args,ctx,3600L)
   add('get_player_card_history',args,ctx,3600L)
   add('get_player_full_profile',list(player_id=id),ctx,21600L)
   add('get_player_matches',list(player_id=id),ctx,21600L)
  }
 }
 if(handler%in%c('get_players_from_team','get_market_players')&&is.data.frame(value)&&'id'%in%names(value)){
  for(id in unique(as.character(value$id))){
   if(is.na(id)||!nzchar(id))next
   ctx<-context;ctx$player_id<-id
   add('get_player_summary',list(championship_id=ctx$championship_id,user_team_id=ctx$user_team_id,player_id=id),ctx,300L)
  }
 }
 out
}
fm_worker_database_rows<-function(table,rows){
 if(!is.list(rows)||!is.null(names(rows)))stop('Invalid database bridge response')
 if(!length(rows))return(data.frame())
 # The finance view has text identity/time columns and one JSON object column.
 # Preserve nested payloads as list columns instead of serializing the entire
 # history a second time and expanding heterogeneous JSON into nested frames.
 if(identical(table,'rival_finance_evidence_latest')){
  fields<-names(rows[[1]])
  allowed<-c('account_id','championship_id','competition_id','season','evidence_type','source_id','observed_at','payload')
  valid<-length(fields)>0L&&all(fields%in%allowed)&&all(vapply(rows,function(row)
    is.list(row)&&setequal(names(row),fields)&&all(vapply(fields,function(field){
      value<-row[[field]]
      if(field=='payload')is.list(value)else is.null(value)||is.character(value)&&length(value)==1L
    },logical(1))),logical(1)))
  if(valid){
   columns<-setNames(lapply(fields,function(field){
    if(field=='payload')lapply(rows,`[[`,field)else vapply(rows,function(row)row[[field]] %||% NA_character_,character(1))
   }),fields)
   return(structure(columns,class='data.frame',row.names=.set_row_names(length(rows))))
  }
 }
 result<-jsonlite::fromJSON(as.character(jsonlite::toJSON(rows,auto_unbox=TRUE,null='null',na='null',digits=NA)))
 if(!is.data.frame(result))stop('Invalid database bridge rows')
 result
}
fm_worker_failure_code<-function(error){
 if(inherits(error,'fm_finance_timeout'))return(error$code %||% 'finance_timeout')
 if(inherits(error,'futmondo_unavailable')){
  category<-fm_scalar(error$category,'unknown')
  if(!category%in%c('http','network','parser','schema','read_budget','unknown'))category<-'unknown'
  code<-paste('resource_parse',category,resource_diagnostic_name(error$resource),sep='_')
  status<-error$http_status
  if(is.numeric(status)&&length(status)==1L&&!is.na(status)&&status>=100&&status<=599&&status==floor(status))code<-paste(code,status,sep='_')
  return(code)
 }
 text<-fm_scalar(conditionMessage(error),'')
 fixed<-c('Unsupported worker class'='resource_parse_codec_class',
   'Unsupported worker value'='resource_parse_codec_value',
   'Unsupported worker primitive'='resource_parse_codec_value',
   'Credential in worker value'='resource_parse_codec_secret',
   'Incomplete source acquisition'='resource_parse_incomplete',
   'Invalid database bridge response'='resource_parse_database_shape',
   'Invalid database bridge rows'='resource_parse_database_shape')
 if(text%in%names(fixed))unname(fixed[[text]])else'resource_parse'
}
fm_worker_finance_timeout_code<-function(state){
 count<-function(x){if(length(x)!=1L||!is.numeric(x)||!is.finite(x)||x<0)return(0L);as.integer(min(9999,floor(x)))}
 queue<-state$queue %||% list()
 kinds<-vapply(queue,function(task)if(is.list(task))fm_scalar(task$kind,'')else'',character(1))
 status<-state$teams_status %||% 'unknown'
 if(!status%in%c('ready','empty','loading','unavailable'))status<-'unknown'
 parts<-c(paste0('s',as.integer(isTRUE(state$seed_complete))),paste0('b',count(state$busy)),
   paste0('q',count(length(queue))),paste0('o',count(state$ownership_pending)),
   paste0('t',count(if(is.data.frame(state$teams))nrow(state$teams)else 0)),paste0('ts',status))
 for(kind in c('saved','seed','team','lineup','ranking','notifications','ownership','ownership_catalog'))
   parts<-c(parts,paste0(kind,count(sum(kinds==kind))))
 paste(c('finance_timeout',parts),collapse='_')
}
fm_worker_headless_finances<-function(env,auth,ctx,on_update=NULL){
 s<-shiny::MockShinySession$new();on.exit(s$close(),add=TRUE)
 teams<-env$get_teams(auth,ctx$championship_id)
 catalog<-env$get_mvp_saved_catalog(auth,ctx$championship_id)
 rounds<-catalog$catalog$rounds;awards<-catalog$saved
 complete<-isTRUE(catalog$catalog$complete)
 if(is.data.frame(rounds)&&nrow(rounds))for(i in which(rounds$is_finished%in%TRUE)){
  ref<-round_reference(rounds$round_id[i],rounds$round_number[i]);ref$is_finished<-TRUE
  saved<-if(is.data.frame(awards)&&'round_id'%in%names(awards))awards[awards$round_id%in%ref$round_id,,drop=FALSE]else data.frame()
  checked<-if('checked_at'%in%names(saved))fm_time(saved$checked_at)else as.POSIXct(character())
  age<-if(length(checked))as.numeric(difftime(Sys.time(),min(checked),units='secs'))else Inf
  reusable<-award_selection_valid(saved)&&'championship_id'%in%names(saved)&&all(saved$championship_id%in%ctx$championship_id)&&
    (!fm_context_known(ctx$season)||'season'%in%names(saved)&&all(saved$season%in%ctx$season))&&
    (!fm_context_known(ctx$scoring_version)||'scoring_version'%in%names(saved)&&all(saved$scoring_version%in%ctx$scoring_version))&&
    'is_finished'%in%names(saved)&&all(saved$is_finished%in%TRUE)&&
    'persistence'%in%names(saved)&&all(saved$persistence%in%'Saved')&&is.finite(age)&&age>=0&&age<86400&&
    !ref$round_id%in%(catalog$settlement_rechecks %||% character())
  if(isTRUE(reusable))next
  result<-env$get_official_dreamteam_result(auth,ctx$championship_id,ref)
  if(identical(result$status,'ok')){
   if(!isTRUE(env$persist_official_award_snapshot(result,auth,ctx$championship_id,ref,ctx)))stop('Award persistence failed')
   awards<-dplyr::bind_rows(awards[!awards$round_id%in%ref$round_id,,drop=FALSE],result$rows)
  }else complete<-FALSE
 }
 feed<-NULL
 shiny::withReactiveDomain(s,{
  feed<-env$shared_rival_finance_source(s,function()auth,function()ctx$championship_id,function()ctx$user_team_id,
    function()teams,function()TRUE,function()list(rows=awards,complete=complete),on_update=on_update)
 })
 started<-Sys.time()
 repeat{
  later::run_now(0.02);s$flushReact()
  state<-s$userData$rival_finance_shared
  if(!is.null(state)&&isTRUE(state$seed_complete)&&state$busy==0L&&!length(state$queue)&&state$ownership_pending==0L){
   result<-shiny::isolate(feed());if(!isTRUE(result$loading))break
  }
  if(as.numeric(difftime(Sys.time(),started,units='secs'))>600)stop(structure(list(message='Finance preparation timeout',call=NULL,
    code=fm_worker_finance_timeout_code(state)),class=c('fm_finance_timeout','error','condition')))
 }
 if(!is.null(result$finance_error))stop('Finance preparation failed')
 result
}
fm_worker_run<-function(descriptor){
 ctx<-descriptor$context
 if(!identical(descriptor$version,1L)&&!identical(descriptor$version,1))stop('Unsupported worker protocol')
 auth<-c(userid=ctx$account_id,token='worker-placeholder')
 env<-new.env(parent=.GlobalEnv)
 for(nm in ls(.GlobalEnv,all.names=TRUE)){
  f<-get(nm,.GlobalEnv)
  if(is.function(f)&&identical(environment(f),.GlobalEnv)){environment(f)<-env;assign(nm,f,env)}
 }
 env$.futmondo_request_context<-ctx
 # Bind default identity to this verified job without inventing observed
 # metadata, borrowing another league, or changing explicit archive arguments.
 original_context<-env$resource_context
 context_wrapper<-function(){
  args<-lapply(as.list(match.call())[-1L],eval,envir=parent.frame())
  value<-do.call(original_context,args)
  if(identical(value$account_id,ctx$account_id)&&identical(value$championship_id,ctx$championship_id)&&nzchar(value$championship_id)){
   if(!'season'%in%names(args)&&!fm_context_known(value$season))value$season<-ctx$season
   if(!'scoring_version'%in%names(args)&&!fm_context_known(value$scoring_version))value$scoring_version<-ctx$scoring_version
   if(is.null(args$competition_id)&&!fm_context_known(value$competition_id))value$competition_id<-ctx$competition_id
  }
  value
 }
 formals(context_wrapper)<-formals(original_context);env$resource_context<-context_wrapper
 same_scope<-function(championship_id,account_id=NULL)identical(env$fm_context_account(account_id),ctx$account_id)&&
   nzchar(fm_scalar(ctx$championship_id,''))&&identical(fm_scalar(championship_id,''),ctx$championship_id)
 original_competition<-env$league_competition_context
 env$league_competition_context<-function(championship_id,account_id=NULL){
  value<-original_competition(championship_id,account_id)
  if(!fm_context_known(value)&&same_scope(championship_id,account_id))ctx$competition_id else value
 }
 original_season<-env$league_season_context
 season_wrapper<-function(){
  args<-lapply(as.list(match.call())[-1L],eval,envir=parent.frame())
  value<-do.call(original_season,args)
  if(same_scope(args$championship_id,args$account_id)){
   if(fm_context_known(value)&&fm_context_known(ctx$season)&&!identical(value,ctx$season))stop('Conflicting authoritative season metadata')
   if(!fm_context_known(value))value<-ctx$season
  }
  value
 }
 formals(season_wrapper)<-formals(original_season);env$league_season_context<-season_wrapper

 produced<-list();produced_bytes<-0;source_clock<-NULL
 ack_scopes<-character()
 history_ack_restore<-function(child,context){
  if(child$resource_key%in%ack_scopes)return(invisible(NULL))
  ack_scopes<<-c(ack_scopes,child$resource_key)
  saved<-tryCatch(fm_pipe_exchange(list(type='history_ack',descriptor=child)),error=function(e)NULL)
  for(field in c('history_write_ack','locker_write_ack')){
   node<-saved[[field]]
   if(is.null(node))next
   hashes<-tryCatch(fm_worker_decode(node),error=function(e)NULL)
   if(!is.character(hashes)||is.null(names(hashes))||length(hashes)>20000L||anyNA(hashes)||
      anyNA(names(hashes))||any(!nzchar(names(hashes)))||anyDuplicated(names(hashes))||
      any(!grepl('^[a-f0-9]{64}$',hashes)))next
   resource<-if(field=='history_write_ack')'history_write_ack_pressroom'else'history_write_ack_locker'
   key<-api_cache_key(resource,context=context)
   env$resource_store(key,list(hashes=hashes),context,resource)
   if(!is.null(api_cache_env[[key]]))api_cache_env[[key]]$expires_at<-as.numeric(Sys.time())+7*86400
  }
  invisible(NULL)
 }
 history_ack_attach<-function(value,context){
  for(field in c('history_write_ack','locker_write_ack')){
   resource<-if(field=='history_write_ack')'history_write_ack_pressroom'else'history_write_ack_locker'
   hashes<-api_cache_env[[api_cache_key(resource,context=context)]]$data$hashes
   if(is.character(hashes)&&length(hashes)<=20000L)attr(value,field)<-hashes
  }
  value
 }
 env$Sys.time<-function()source_clock %||% Sys.time()
 env$get_sb_url<-function()'https://worker-database.invalid'
 env$get_sb_key<-function()'worker-placeholder'
 env$futmondo_post<-function(url,...){
  dots<-list(...);body<-dots$body
  if(is.character(body))body<-jsonlite::fromJSON(body,simplifyVector=FALSE)
  if(!is.list(body))stop('Invalid source payload')
  body$header<-NULL
  result<-fm_pipe_exchange(list(type='http',url=url,body=body))
  source_time<-fm_time(result$observed_at)
  if(length(source_time)!=1L||is.na(source_time))stop('Missing source observation time')
  source_clock<<-if(is.null(source_clock))source_time else min(source_clock,source_time)
  content<-charToRaw(as.character(jsonlite::toJSON(result$body,auto_unbox=TRUE,null='null',na='null',digits=NA)))
  fm_httr_response(list(status_code=as.integer(result$status),content=content,times=c(total=0)),url)
 }
 env$get_worker_http_read<-function(route,query=list()){
  httr::content(env$futmondo_post(paste0('https://api.futmondo.com',route),body=list(header=list(),query=query,answer=list())))
 }
 env$supabase_get<-function(table_name,query_params=list()){
  result<-fm_pipe_exchange(list(type='database',table=table_name,query=query_params))
  if(!'rows'%in%names(result)||!is.list(result$rows)||!is.null(names(result$rows)))stop('Invalid database bridge response')
  fm_worker_database_rows(table_name,result$rows)
 }
 env$supabase_post_direct<-function(table_name,payload,conflict=NULL,ignore_duplicates=FALSE){
  if(is.data.frame(payload)&&!nrow(payload)||is.list(payload)&&!length(payload))return(TRUE)
  if(is.null(conflict))conflict<-supabase_conflict_key(table_name)
  fm_pipe_exchange(list(type='persist',table=table_name,payload=payload,conflict=conflict,ignore=ignore_duplicates))$ok
 }
 env$supabase_post<-env$supabase_post_direct
 env$defer_persistence<-function(fn,args=list(),context=NULL,on_complete=NULL,retained_bytes=0){
  if(!fn%in%c('supabase_post_direct','supabase_fact_batch_direct','supabase_round_history_batch_direct','supabase_pressroom_history_batch_direct'))stop('Unsupported persistence')
  ops<-if(fn=='supabase_post_direct')list(args)else args[[1]]
  if(!all(vapply(ops,persistence_operation_valid,logical(1))))stop('Invalid persistence operations')
  for(op in ops)if(!isTRUE(do.call(env$supabase_post_direct,op)))stop('Persistence was not acknowledged')
  if(is.function(on_complete))on_complete(TRUE)
  TRUE
 }
 # Controller promises run in this private process. No live Shiny session is shared.
 env$async_adapter_fetch<-function(fn,args=list(),context=NULL,...){
  tryCatch(promises::promise_resolve(do.call(get(fn,env),args)),error=function(e)promises::promise_reject(e))
 }
 # Preserve useful nested acquisitions as shared snapshots; do not refetch them
 # merely because a different screen asks for the same normalized resource.
 # Finance task envelopes are controller-private. Their successful source reads
 # have HTTP checkpoints and their normalized evidence is persisted by the
 # controller. Encoding another typed copy of a detailed raw lineup is expensive
 # and has no display consumer; it also duplicates the durable evidence.
 finance_tasks<-c('get_rival_finance_seed','get_rival_team_finance_observation',
   'get_rival_round_lineup_observation','get_rival_round_ranking_observation',
   'get_rival_round_notification_page','get_rival_player_ownership_observation','get_rival_ownership_round_catalog')
 for(nm in ls(env)){
  fn<-env[[nm]]
  if(!is.function(fn)||!startsWith(nm,'get_')||nm%in%finance_tasks||endsWith(nm,'_async')||!'login'%in%names(formals(fn)))next
  local({name<-nm;original<-fn
   wrapper<-function(){
    params<-lapply(as.list(match.call())[-1L],eval,envir=parent.frame())
    childctx<-env$resource_context(params$login,params$championship_id,params$user_team_id %||% params$team_id,params$player_id)
    child<-fm_worker_descriptor(name,params,childctx)
    if(child$resource_key!=descriptor$resource_key){
     existing<-produced[[child$resource_key]]
     if(!is.null(existing))return(fm_worker_decode(existing$value))
     saved<-fm_pipe_exchange(list(type='resource',descriptor=child))
     if(!is.null(saved$value)){
      restored<-fm_worker_decode(saved$value)
      valid_scope<-name!='get_mvp_saved_catalog'||!fm_context_known(childctx$season)||
        identical(fm_scalar(restored$season,'unknown'),childctx$season)
      if(valid_scope){
       stamp<-fm_time(saved$observed_at)
       if(length(stamp)==1L&&!is.na(stamp))source_clock<<-if(is.null(source_clock))stamp else min(source_clock,stamp)
       return(restored)
      }
     }
    }
    if(name=='get_championship_pressroom')history_ack_restore(child,childctx)
    value<-do.call(original,params)
    # The top-level result is encoded below once. It is excluded from child
    # publication, so constructing another full typed copy here is wasted work.
    childctx<-env$resource_context(params$login,params$championship_id,params$user_team_id %||% params$team_id,params$player_id)
    child<-fm_worker_descriptor(name,params,childctx)
    if(identical(child$resource_key,descriptor$resource_key))return(value)
    tryCatch({
     wire<-fm_worker_encode(value)
     bytes<-as.numeric(object.size(wire))
     if(!is.null(value)&&produced_bytes+bytes<=8*1024^2){
      produced[[child$resource_key]]<<-list(descriptor=child,value=wire,
       observed_at=rival_finance_stamp(attr(value,'observed_at') %||% Sys.time()),checked_at=rival_finance_stamp(),
       status=if(is.list(value)&&!is.data.frame(value)&&!is.null(value$status)&&value$status%in%c('partial','unavailable','unsupported'))'partial'else if(acquisition_data_unverified(value)||!is.null(attr(value,'fetch_error')))'partial'else'ok',
       completeness=if(isTRUE(attr(value,'complete')))'complete'else'unknown')
      produced_bytes<<-produced_bytes+bytes
     }
    },error=function(e)NULL)
    value
   }
   formals(wrapper)<-formals(original);assign(name,wrapper,env)
  })
 }
 handler<-descriptor$handler
 if(!grepl('^(get_|read_|fm_worker_finances$)',handler))stop('Unsupported resource')
 clear_api_cache()
 args<-if(!is.null(descriptor$encoded_args))fm_worker_decode(descriptor$encoded_args)else descriptor$args
 if(!length(args))args<-list()
 last_progress<- -Inf;progress_signature<-NULL
 progress<-function(value){
  rows<-value$team_finances
  if(!is.null(value$finance_error)||!is.data.frame(rows)||!'estimated_cash'%in%names(rows)||!any(is.finite(rows$estimated_cash)))return(invisible(NULL))
  now<-proc.time()[['elapsed']]
  if(isTRUE(value$loading)&&now-last_progress<15)return(invisible(NULL))
  signature<-paste0(openssl::sha256(serialize(value[c('team_finances','cash_ledgers','breakdowns','finance_loading','finance_pending')],NULL)))
  if(identical(signature,progress_signature))return(invisible(NULL))
  preview<-value;preview$loading<-TRUE;preview$finance_loading<-TRUE
  preview$reason<-trimws(paste(value$reason %||% '',if(isTRUE(value$loading))'Preparing remaining financial evidence.'else'Saving prepared finance resources.'))
  result<-list(value=fm_worker_encode(preview),observed_at=rival_finance_stamp(source_clock %||% Sys.time()),
    checked_at=rival_finance_stamp(),status='partial',completeness='partial',ready=!isTRUE(value$loading))
  if(!isTRUE(fm_pipe_exchange(list(type='progress',result=result))$ok))stop('Finance progress was not acknowledged')
  last_progress<<-now;progress_signature<<-signature
  invisible(NULL)
 }
 if(handler=='fm_worker_finances')value<-fm_worker_headless_finances(env,auth,ctx,on_update=progress)else{
  if(!exists(handler,env,mode='function'))stop('Unknown resource')
  f<-get(handler,env)
  if('login'%in%names(formals(f)))args$login<-auth
  if('context'%in%names(formals(f))&&is.null(args$context))args$context<-ctx
  value<-do.call(f,args)
 }
 if(is.null(value))stop('Source did not return a resource')
 # Drain callbacks such as acknowledged history chunks before certifying a snapshot.
 for(i in seq_len(20L))later::run_now(0)
 if(handler=='get_championship_pressroom'){
  history_context<-env$resource_context(auth,ctx$championship_id)
  value<-history_ack_attach(value,history_context)
 }
 for(key in names(produced))if(produced[[key]]$descriptor$handler=='get_championship_pressroom'){
  child_context<-env$resource_context(auth,produced[[key]]$descriptor$context$championship_id)
  decoded<-history_ack_attach(fm_worker_decode(produced[[key]]$value),child_context)
  wire<-fm_worker_encode(decoded);bytes<-as.numeric(object.size(wire))
  prior_bytes<-as.numeric(object.size(produced[[key]]$value))
  if(produced_bytes-prior_bytes+bytes<=8*1024^2){
   produced[[key]]$value<-wire;produced_bytes<-produced_bytes-prior_bytes+bytes
  }
 }
 failure<-attr(value,'fetch_error')
 if(inherits(failure,'condition'))stop(failure)
 if(identical(attr(value,'fetch_status'),'unavailable')||!is.null(failure))stop('Incomplete source acquisition')
 observed<-attr(value,'observed_at') %||% Sys.time()
 status<-if(is.list(value)&&!is.data.frame(value))value$status %||% 'ok'else attr(value,'fetch_status') %||% 'ok'
 if(!status%in%c('ok','empty','partial','unsupported'))status<-'partial'
 completeness<-if(isTRUE(attr(value,'complete')))'complete'else if(is.list(value)&&identical(value$completeness,'complete'))'complete'else'unknown'
 if(status=='partial')completeness<-'partial'
 list(type='result',value=fm_worker_encode(value),observed_at=rival_finance_stamp(observed),checked_at=rival_finance_stamp(),
  status=status,completeness=completeness,resources=unname(produced[setdiff(names(produced),descriptor$resource_key)]),discoveries=fm_worker_discover(handler,value,ctx,auth))
}
fm_pipe_send(list(type='ready',version=1L))
repeat{
 line<-readLines(pipe_in,n=1L,warn=FALSE)
 if(!length(line))break
 tryCatch({
  command<-jsonlite::fromJSON(line,simplifyVector=FALSE)
  if(command$type!='run')stop('Invalid command')
  fm_pipe_send(fm_worker_run(command$descriptor))
 },error=function(e)fm_pipe_send(list(type='failure',code=fm_worker_failure_code(e))))
}
