# Database-backed browsing for explicitly opted-in accounts. All transport is async.
.fm_worker <- new.env(parent=emptyenv())
.fm_worker$accounts<-new.env(parent=emptyenv());.fm_worker$values<-new.env(parent=emptyenv())
fm_worker_configured<-function() !isTRUE(getOption('futmondo.offline',FALSE))&&
 !isTRUE(getOption('futmondo.worker_helper',FALSE))&&identical(tolower(Sys.getenv('FUTMONDO_WORKER_ENABLED','true')),'true')&&
 grepl('^[a-fA-F0-9]{64}$',Sys.getenv('FUTMONDO_WORKER_KEY'))&&requireNamespace('sodium',quietly=TRUE)&&nzchar(get_sb_url())&&nzchar(get_sb_key())
fm_worker_resource_revision<-function(state,key){
 if(is.null(state$resource_revisions[[key]]))state$resource_revisions[[key]]<-shiny::reactiveVal(0L)
 state$resource_revisions[[key]]
}
fm_worker_notify<-function(account){
 state<-.fm_worker$accounts[[account]]
 if(!is.null(state))state$status_revision(shiny::isolate(state$status_revision())+1L)
 invisible(NULL)
}
fm_worker_account_state<-function(id){
 state<-.fm_worker$accounts[[id]]
 if(is.null(state)){
  state<-new.env(parent=emptyenv());state$account<-NULL;state$known<-FALSE;state$busy<-FALSE;state$last_poll<-0
  state$status_revision<-shiny::reactiveVal(0L);state$redirects<-list();state$redirect_revision<-shiny::reactiveVal(0L);state$descriptors<-list();state$waiters<-list();state$queued<-list();state$health<-NULL;state$jobs<-list();state$errors<-list();state$sessions<-character();state$policy_revision<-shiny::reactiveVal(0L);state$settings_revision<-shiny::reactiveVal(0L);state$resource_revisions<-new.env(parent=emptyenv())
  .fm_worker$accounts[[id]]<-state
 }
 state
}
fm_worker_set_policy<-function(s,account){
 if(s$known&&!is.null(account$generation)&&!is.null(s$account$generation)&&account$generation<s$account$generation)return(invisible(FALSE))
 fields<-c('enabled','timezone','quiet_start','quiet_end','excluded_leagues')
 project<-function(a)setNames(lapply(fields,function(f)a[[f]]),fields)
 changed_settings<-!s$known||!identical(fm_worker_json(project(s$account)),fm_worker_json(project(account)))
 changed_policy<-!s$known||!identical(s$account,account)
 s$known<-TRUE;s$account<-account
 if(changed_policy)s$policy_revision(shiny::isolate(s$policy_revision())+1L)
 if(changed_settings)s$settings_revision(shiny::isolate(s$settings_revision())+1L)
 invisible(changed_policy)
}
fm_worker_mode<-function(id){
 if(!fm_worker_configured()||isTRUE(getOption('futmondo.worker_live',FALSE)))return(FALSE)
 state<-fm_worker_account_state(id)
 try(state$policy_revision(),silent=TRUE)
 if(!state$known)return(NA)
 isTRUE(state$account$enabled)
}
fm_worker_rpc<-function(name,payload,owner=''){
 url<-paste0(get_sb_url(),'/rest/v1/rpc/worker_',name)
 headers<-c(apikey=get_sb_key(),Authorization=paste('Bearer',get_sb_key()),`Content-Type`='application/json')
 promises::then(fm_async_http(url,'POST',fm_worker_json(payload),headers,owner=owner),function(response){
  jsonlite::fromJSON(rawToChar(response$content),simplifyVector=FALSE)
 })
}
fm_worker_encrypt<-function(auth){
 # A dedicated secretbox envelope; unrelated to remembered browser login.
 credential_id<-sodium::bin2hex(sodium::random(16))
 payload<-list(version=1L,account_id=auth[['userid']],generation=credential_id,auth=as.list(auth))
 encrypted<-sodium::data_encrypt(charToRaw(fm_worker_json(payload)),sodium::hex2bin(Sys.getenv('FUTMONDO_WORKER_KEY')))
 result<-gsub('[[:space:]]','',jsonlite::base64_enc(c(attr(encrypted,'nonce'),encrypted)))
 attr(result,'credential_id')<-credential_id;result
}
fm_worker_key<-function(account,key)paste(account,key,sep=':')
fm_worker_put<-function(account,key,item){
 cache<-.fm_worker$values;id<-fm_worker_key(account,key)
 item$accessed<-as.numeric(Sys.time());item$bytes<-as.numeric(object.size(item));cache[[id]]<-item
 repeat{
  keys<-ls(cache);sizes<-vapply(keys,function(k)cache[[k]]$bytes,numeric(1))
  if(length(keys)<=256L&&sum(sizes)<=32*1024^2)break
  times<-vapply(keys,function(k)cache[[k]]$accessed,numeric(1));rm(list=keys[which.min(times)],envir=cache)
 }
 invisible(NULL)
}
fm_worker_get<-function(account,key){
 id<-fm_worker_key(account,key);item<-.fm_worker$values[[id]]
 if(!is.null(item)){item$accessed<-as.numeric(Sys.time());.fm_worker$values[[id]]<-item}
 item
}
fm_worker_restore<-function(resource,account){
 m<-resource$manifest
 if(!identical(m$account_id,account)||!identical(m$resource_key,resource$key)||
    !is.numeric(m$json_bytes)||m$json_bytes<1||m$json_bytes>16*1024^2||
    !is.list(resource$chunks)||length(resource$chunks)!=m$parts)stop('Invalid worker snapshot')
 compressed<-jsonlite::base64_dec(paste0(unlist(resource$chunks,use.names=FALSE),collapse=''))
 if(!identical(paste0(openssl::sha256(compressed)),m$sha256))stop('Corrupt worker snapshot')
 con<-gzcon(rawConnection(compressed));on.exit(close(con),add=TRUE)
 raw<-readBin(con,'raw',n=as.integer(m$json_bytes)+1L)
 if(length(raw)!=m$json_bytes)stop('Worker snapshot length differs')
 value<-fm_worker_decode(jsonlite::fromJSON(rawToChar(raw),simplifyVector=TRUE,simplifyDataFrame=FALSE,simplifyMatrix=FALSE))
 if(identical(m$handler,'get_active_championships'))value<-fm_filter_league_answer(value)
 attr(value,'observed_at')<-fm_time(m$observed_at);attr(value,'fetched_at')<-fm_time(m$checked_at)
 attr(value,'worker_completeness')<-m$completeness
 attr(value,'worker_acquisition_status')<-m$status
 existing<-attr(value,'valid_until') %||% if(is.list(value))value$valid_until else NULL
 deadline<-as.numeric(fm_time(m$valid_until))
 if(is.numeric(existing)&&length(existing)==1L&&is.finite(existing))deadline<-min(deadline,existing)
 attr(value,'valid_until')<-deadline
 attr(value,'fetch_status')<-if(as.numeric(fm_time(m$valid_until))<=as.numeric(Sys.time()))'stale'else m$status
 list(data=value,manifest=m)
}
fm_worker_enqueue<-function(descriptor,owner='',force=FALSE){
 state<-fm_worker_account_state(descriptor$context$account_id);key<-descriptor$resource_key
 if(!force&&any(vapply(state$jobs,function(job)identical(job$key,key)&&isTRUE(job$running),logical(1))))return(invisible(NULL))
 if(!is.null(state$queued[[key]])&&as.numeric(Sys.time())-state$queued[[key]]<30&&!force)return(invisible(NULL))
 state$queued[[key]]<-as.numeric(Sys.time())
 p<-fm_worker_rpc('enqueue',list(p_account=descriptor$context$account_id,p_descriptor=descriptor,
  p_interval=fm_worker_interval(descriptor$handler),p_priority=if(force)10L else fm_worker_priority(descriptor$handler)),owner)
 promises::then(p,function(ok){if(!isTRUE(ok)){state$queued[[key]]<-NULL;state$errors[[key]]<-'Background access is paused or this league is excluded.'};fm_worker_notify(descriptor$context$account_id);NULL},
  function(error){state$queued[[key]]<-NULL;state$errors[[key]]<-'Could not queue background refresh; database access failed.';fm_worker_notify(descriptor$context$account_id);NULL})
 invisible(NULL)
}
fm_worker_demand<-function(descriptor,owner='',is_current=NULL){
 s<-fm_worker_account_state(descriptor$context$account_id);key<-descriptor$resource_key
 watchers<-s$descriptors[[key]]$watchers %||% list();now<-as.numeric(Sys.time())
 interest<-if(is.function(is_current))attr(is_current,'fm_worker_interest') %||% paste0('live:',owner)else paste0('legacy:',owner)
 watchers[[interest]]<-list(owner=owner,current=is_current,at=now)
 if(length(watchers)>128L){old<-order(vapply(watchers,`[[`,numeric(1),'at'));watchers[old[seq_len(length(watchers)-128L)]]<-NULL}
 s$descriptors[[key]]<-list(descriptor=descriptor,at=now,owner=owner,watchers=watchers)
 if(length(s$descriptors)>128L){times<-vapply(s$descriptors,function(x)x$at,numeric(1));victims<-names(sort(times))[seq_len(length(times)-128L)];s$descriptors[victims]<-NULL;s$queued[victims]<-NULL;s$errors[victims]<-NULL
  for(v in victims)if(exists(v,s$resource_revisions,inherits=FALSE))rm(list=v,envir=s$resource_revisions)
 }
 invisible(NULL)
}
fm_worker_live_keys<-function(state,now=as.numeric(Sys.time())){
 keys<-character()
 for(key in names(state$descriptors)){
  d<-state$descriptors[[key]];watchers<-d$watchers
  if(!length(watchers)){if(now-d$at<=120)keys<-c(keys,key);next}
  live<-vapply(watchers,function(w)if(is.function(w$current))
    isTRUE(tryCatch(shiny::isolate(w$current()),error=function(e)FALSE))else now-w$at<=120,logical(1))
  if(any(live)){
   chosen<-watchers[[which(live)[1L]]];d$owner<-chosen$owner
   if(any(vapply(watchers[live],function(w)is.function(w$current),logical(1))))d$at<-now
   state$descriptors[[key]]<-d;keys<-c(keys,key)
  }
 }
 keys
}
# Only a server-verified peer can fill unknown dimensions. Never relabel data.
fm_worker_accept_redirect<-function(state,redirect,account){
 if(!is.list(redirect)||!is.character(redirect$requested_key)||length(redirect$requested_key)!=1L||
    is.na(redirect$requested_key)||!grepl('^[a-f0-9]{64}$',redirect$requested_key))return(FALSE)
 old_key<-redirect$requested_key;target<-redirect$descriptor
 if(!is.list(target)||!is.list(target$context)||!is.list(target$args))return(FALSE)
 previous<-state$descriptors[[old_key]]$descriptor
 if(is.null(previous)||!previous$handler%in%c('get_players_from_team','get_championship_pressroom','get_clause_transfer_summary')||
    !is.list(target)||!identical(previous$handler,target$handler)||
    !identical(as.integer(previous$version),as.integer(target$version))||
    !identical(fm_worker_json(previous$args),fm_worker_json(target$args))||
    !identical(fm_scalar(target$context$account_id),account))return(FALSE)
 dims<-c('competition_id','season','scoring_version');other<-setdiff(fm_worker_fields,dims)
 if(!identical(fm_worker_json(previous$context[other]),fm_worker_json(target$context[other]))||
    !all(vapply(target$context[dims],fm_context_known,logical(1)))||
    any(vapply(dims,function(f)fm_context_known(previous$context[[f]])&&!identical(previous$context[[f]],target$context[[f]]),logical(1))))return(FALSE)
 rebuilt<-tryCatch(fm_worker_descriptor(target$handler,target$args,target$context),error=function(e)NULL)
 if(is.null(rebuilt)||!identical(rebuilt$resource_key,target$resource_key))return(FALSE)
 changed<-!identical(state$redirects[[old_key]]$descriptor$resource_key,target$resource_key)
 state$redirects[[old_key]]<-list(descriptor=rebuilt,at=as.numeric(Sys.time()))
 if(length(state$redirects)>128L)state$redirects<-tail(state$redirects,128L)
 # Register the target before decoding its chunks; old watchers are retired.
 item<-state$descriptors[[old_key]];target_item<-state$descriptors[[target$resource_key]]
 item$descriptor<-rebuilt;item$watchers<-c(target_item$watchers %||% list(),item$watchers)
 item$watchers<-tail(item$watchers,128L);state$descriptors[[target$resource_key]]<-item
 state$descriptors[[old_key]]<-NULL;state$queued[[old_key]]<-NULL;state$errors[[old_key]]<-NULL
 for(id in names(state$waiters))if(identical(state$waiters[[id]]$key,old_key)){
  waiter<-state$waiters[[id]];state$waiters[[id]]<-NULL;if(is.function(waiter$cancel))waiter$cancel()
  waiter$reject(structure(list(message='Resource context resolved; retry with its canonical context',call=NULL),class=c('fm_async_cancelled','error','condition')))
 }
 if(changed)state$redirect_revision(shiny::isolate(state$redirect_revision())+1L)
 TRUE
}
fm_worker_resolve_context<-function(handler,args,context){
 if(!fm_worker_configured()||!handler%in%c('get_players_from_team','get_championship_pressroom','get_clause_transfer_summary'))return(context)
 state<-fm_worker_account_state(context$account_id);try(state$redirect_revision(),silent=TRUE)
 descriptor<-fm_worker_descriptor(handler,args,context);hint<-state$redirects[[descriptor$resource_key]]
 if(!is.null(hint)&&as.numeric(Sys.time())-hint$at<300){
  dims<-c('competition_id','season','scoring_version')
  context[dims]<-hint$descriptor$context[dims]
 }
 context
}
fm_worker_fetch<-function(handler,args,context,owner=context$session_id %||% '',is_current=function()TRUE,direct=NULL){
 mode<-fm_worker_mode(context$account_id)
 if(identical(mode,FALSE)){
  if(is.function(direct))return(direct())
  return(promises::promise_reject(simpleError('Background access is not enabled')))
 }
 context<-fm_worker_resolve_context(handler,args,context)
 d<-fm_worker_descriptor(handler,args,context);fm_worker_demand(d,owner,is_current)
 s<-fm_worker_account_state(context$account_id);try(fm_worker_resource_revision(s,d$resource_key)(),silent=TRUE);saved<-fm_worker_get(context$account_id,d$resource_key)
 if(!is.null(saved)){
  if(!resource_snapshot_unexpired(saved$data)){
   attr(saved$data,'fetch_status')<-'stale';if(isTRUE(mode))fm_worker_enqueue(d,owner)
  }
  if(handler!='get_clause_transfer_summary'||resource_snapshot_unexpired(saved$data))
    return(promises::promise_resolve(saved$data))
 }
 if(isTRUE(mode))fm_worker_enqueue(d,owner)
 promises::promise(function(resolve,reject){
  if(length(s$waiters)>=128L){reject(simpleError('Background read capacity reached; existing requests are still pending.'));return(invisible(NULL))}
  id<-paste0('w',paste0(openssl::sha256(serialize(list(Sys.time(),runif(1)),NULL))))
  waiter<-list(key=d$resource_key,owner=owner,current=is_current,resolve=resolve,reject=reject,descriptor=d,direct=direct)
  waiter$cancel<-later::later(function(){
   if(!is.null(s$waiters[[id]])){s$waiters[[id]]<-NULL;reject(simpleError('Background synchronization is still pending; inspect sync status.'))}
  },120)
  s$waiters[[id]]<-waiter
 })
}
fm_worker_settle<-function(s){
 for(id in names(s$waiters)){
  w<-s$waiters[[id]];saved<-fm_worker_get(w$descriptor$context$account_id,w$key)
  cancelled<-!isTRUE(tryCatch(w$current(),error=function(e)FALSE))
  disabled<-s$known&&!isTRUE(s$account$enabled)
  ready<-!is.null(saved)&&(w$descriptor$handler!='get_clause_transfer_summary'||resource_snapshot_unexpired(saved$data))
  if(cancelled||disabled||ready){
   s$waiters[[id]]<-NULL;if(is.function(w$cancel))w$cancel()
   if(cancelled)w$reject(structure(list(message='Session request cancelled',call=NULL),class=c('fm_async_cancelled','error','condition')))
   else if(disabled&&is.function(w$direct))promises::then(w$direct(),w$resolve,w$reject)
   else if(disabled)w$reject(simpleError('Background access disabled'))else w$resolve(saved$data)
  }
 }
}
fm_worker_poll<-function(account,owner='',force=FALSE){
 s<-fm_worker_account_state(account)
 if(s$busy||!force&&as.numeric(Sys.time())-s$last_poll<4)return(invisible(NULL))
 s$busy<-TRUE;s$last_poll<-as.numeric(Sys.time())
 keys<-fm_worker_live_keys(s)
 versions<-setNames(lapply(keys,function(k)fm_worker_get(account,k)$manifest$version %||% ''),keys)
 if(!length(versions))versions<-structure(list(),names=character())
 p<-fm_worker_rpc('read',list(p_account=account,p_versions=versions),owner)
 promises::then(p,function(result){
  s$busy<-FALSE;fm_worker_set_policy(s,result$account);s$health<-result$health;s$jobs<-result$jobs;s$service_error<-NULL
  s$errors<-s$errors[intersect(names(s$errors),keys)]
  for(redirect in result$redirects)fm_worker_accept_redirect(s,redirect,account)
  keys<-fm_worker_live_keys(s)
  for(resource in result$resources){
   d<-s$descriptors[[resource$key]]$descriptor
   if(is.null(d)||!identical(fm_worker_json(resource$manifest$context),fm_worker_json(d$context)))next
   item<-tryCatch(fm_worker_restore(resource,account),error=function(e)NULL)
   if(!is.null(item)){
    if(identical(d$handler,'get_active_championships')){
     rows<-tryCatch(normalize_active_league_contexts(item$data,account,attr(item$data,'observed_at')),error=function(e)list())
     for(row in rows)remember_league_context(row)
    }
    if(identical(d$handler,'get_lineup_from_team'))try(remember_league_scoring(d$context,item$data,attr(item$data,'observed_at')),silent=TRUE)
    # A genuinely newer source publication confirms an existing context hint;
    # rereading an old version never renews its identity lease or source time.
    for(alias in names(s$redirects))if(identical(s$redirects[[alias]]$descriptor$resource_key,resource$key)){
     checked<-as.numeric(fm_time(item$manifest$checked_at))
     if(length(checked)==1L&&is.finite(checked)&&checked<=as.numeric(Sys.time()))s$redirects[[alias]]$at<-max(s$redirects[[alias]]$at,checked)
    }
    fm_worker_put(account,resource$key,item);rev<-fm_worker_resource_revision(s,resource$key);rev(shiny::isolate(rev())+1L);s$queued[[resource$key]]<-NULL;s$errors[[resource$key]]<-NULL}
   else s$errors[[resource$key]]<-'Stored snapshot failed integrity checks; acquisition is required.'
  }
  for(job in result$jobs)s$errors[[job$key]]<-if(!is.null(job$error))paste('Background acquisition failed:',job$error)else NULL
  if(isTRUE(s$account$enabled))for(k in keys){
   saved<-fm_worker_get(account,k)
   # Clause subscriptions observe autonomous worker publications. Their expiry
   # must not turn every GUI poll into another priority acquisition.
   if(s$descriptors[[k]]$descriptor$handler!='get_clause_transfer_summary'&&
      (is.null(saved)||!resource_snapshot_unexpired(saved$data)))fm_worker_enqueue(s$descriptors[[k]]$descriptor,owner)
  }
  fm_worker_settle(s);fm_worker_notify(account);NULL
 },function(error){s$busy<-FALSE;s$service_error<-'Background status could not be read from the database.';fm_worker_notify(account);NULL})
 invisible(NULL)
}
fm_worker_peek<-function(handler,args,context,direct,is_current=NULL){
 mode<-fm_worker_mode(context$account_id)
 if(identical(mode,FALSE))return(direct())
 context<-fm_worker_resolve_context(handler,args,context)
 d<-fm_worker_descriptor(handler,args,context);fm_worker_demand(d,context$session_id %||% '',is_current)
 s<-fm_worker_account_state(context$account_id);try(fm_worker_resource_revision(s,d$resource_key)(),silent=TRUE)
 item<-fm_worker_get(context$account_id,d$resource_key)
 if(isTRUE(mode)&&(is.null(item)||!resource_snapshot_unexpired(item$data)))fm_worker_enqueue(d,context$session_id %||% '')
 if(is.null(item))stop(structure(list(message='Loading data from background sync…',call=NULL),class=c('shiny.silent.error','validation','error','condition')))
 if(!resource_snapshot_unexpired(item$data))attr(item$data,'fetch_status')<-'stale'
 item$data
}
fm_worker_wrap_reads<-function(){
 if(isTRUE(getOption('futmondo.worker_helper',FALSE)))return(invisible(NULL))
 for(n in ls(.GlobalEnv)){
  f<-get(n,.GlobalEnv)
  if(!is.function(f)||!startsWith(n,'get_')||!'login'%in%names(formals(f))||!is.null(attr(f,'worker_original')))next
  local({handler<-n;original<-f
   wrapper<-function(){
    matched<-as.list(match.call())[-1L];args<-lapply(matched,eval,envir=parent.frame())
    auth<-args$login
    # Ordinary functions remain completely unchanged when worker support is off.
    if(!valid_login(auth)||!fm_worker_configured()||identical(fm_worker_mode(auth[['userid']]),FALSE))return(do.call(original,args))
    ctx<-resource_context(auth,args$championship_id,args$user_team_id %||% args$team_id,args$player_id)
    fm_worker_peek(handler,args,ctx,function()do.call(original,args))
   }
   formals(wrapper)<-formals(original);attr(wrapper,'worker_original')<-original
   assign(handler,wrapper,.GlobalEnv)
  })
 }
 # Explicit preflights retain direct live reads throughout their synchronous scope.
 for(n in c('get_acquisition_capacity','direct_sale_preflight','buy_clause','buy_roster_clause','modify_bid','cancel_bid','direct_sell_player','put_player_on_market','update_player_market_listing','cancel_player_sell','put_all_on_market','accept_bid','reject_bid','mark_notification_read')){
  original<-get(n,.GlobalEnv);if(!is.null(attr(original,'worker_original')))original<-attr(original,'worker_original')
  local({f<-original;handler<-n;wrapper<-function(){
   args<-lapply(as.list(match.call())[-1L],eval,envir=parent.frame())
   old<-options(futmondo.worker_live=TRUE);on.exit(options(old),add=TRUE);do.call(f,args)
  };formals(wrapper)<-formals(f);attr(wrapper,'worker_original')<-f;assign(handler,wrapper,.GlobalEnv)})
 }
 invisible(NULL)
}
fm_worker_finance_source<-function(session,login,championship,user_team,teams,active,official,refresh=NULL){
 direct<-.fm_worker_finance_original(session,login,championship,user_team,teams,
  function()isTRUE(active())&&identical(fm_worker_mode(fm_scalar(login()[['userid']],'')),FALSE),official,refresh)
 shiny::reactive({
  auth<-login();shiny::req(valid_login(auth));mode<-fm_worker_mode(auth[['userid']])
  if(identical(mode,FALSE))return(direct())
  ctx<-resource_context(auth,championship(),user_team(),session_id=session$token)
  if(!fm_context_known(ctx$competition_id)||!fm_context_known(ctx$season))return(list(
    team_finances=rival_finance_directory(tryCatch(teams(),error=function(e)data.frame())),breakdowns=list(),loading=TRUE,
    finance_loading=TRUE,reason='Resolving league context before preparing finances.'))
  value<-tryCatch(fm_worker_peek('fm_worker_finances',
   list(championship_id=ctx$championship_id,user_team_id=ctx$user_team_id),ctx,function()NULL,
   is_current=function()shiny::isolate(isTRUE(active())&&valid_login(login())&&
     identical(login()[['userid']],ctx$account_id)&&identical(as.character(championship()),ctx$championship_id)&&
     identical(as.character(user_team()),ctx$user_team_id))),error=function(e)NULL)
  value %||% list(team_finances=rival_finance_directory(tryCatch(teams(),error=function(e)data.frame())),breakdowns=list(),loading=TRUE,
   finance_loading=TRUE,reason='Preparing financial data in background sync.')
 })
}
fm_worker_detach<-function(account,owner){
 s<-.fm_worker$accounts[[account]];if(is.null(s))return(invisible(NULL))
 s$sessions<-setdiff(s$sessions,owner)
 for(key in names(s$descriptors)){
  d<-s$descriptors[[key]]
  if(length(d$watchers)){
   keep<-!vapply(d$watchers,function(w)identical(w$owner,owner),logical(1));d$watchers<-d$watchers[keep]
   if(!length(d$watchers)){
    s$descriptors[[key]]<-NULL;s$queued[[key]]<-NULL;s$errors[[key]]<-NULL
    if(exists(key,s$resource_revisions,inherits=FALSE))rm(list=key,envir=s$resource_revisions)
   }else s$descriptors[[key]]<-d
  }
 }
 for(id in names(s$waiters))if(identical(s$waiters[[id]]$owner,owner)){
  w<-s$waiters[[id]];s$waiters[[id]]<-NULL;if(is.function(w$cancel))w$cancel();w$reject(simpleError('Session request cancelled'))
 }
 if(!length(s$sessions)&&!length(s$waiters))rm(list=account,envir=.fm_worker$accounts)
 invisible(NULL)
}
# Poll heartbeats and unrelated accounts are not visible page changes. Publish
# a compact display projection only when its text or source timestamp changes.
fm_worker_status_source<-function(session,login){
 value<-shiny::reactiveVal(NULL)
 observer<-shiny::observe({
  auth<-login();if(!valid_login(auth)){value(NULL);return()}
  account<-auth[['userid']];s<-fm_worker_account_state(account);s$status_revision()
  if(!isTRUE(s$account$enabled)){value(NULL);return()}
  health_age<-as.numeric(difftime(Sys.time(),fm_time(s$health$heartbeat),units='secs'))
  message<-if(!identical(s$account$auth_status,'active'))'Background access needs renewal. Log in again.'else
   if(length(health_age)!=1L||!is.finite(health_age)||health_age>60)'Background worker is not responding; saved data remains available.'else
    if(!is.null(s$service_error))s$service_error else
     if(length(s$errors))paste(unique(unlist(s$errors)),collapse=' ')else 'Background sync is enabled. Screens use stored observations; stale data keeps its original source time.'
  live_keys<-fm_worker_live_keys(s)
  jobs<-s$jobs[vapply(s$jobs,function(j)j$key%in%live_keys,logical(1))]
  pending<-sum(vapply(jobs,function(j)isTRUE(j$pending),logical(1)));running<-sum(vapply(jobs,function(j)isTRUE(j$running),logical(1)))
  if(pending>0&&identical(s$account$auth_status,'active'))message<-paste(message,sprintf('Preparing %d requested resource(s), %d running.',pending,running))
  checks<-vapply(live_keys,function(k){item<-fm_worker_get(account,k);if(is.null(item))NA_real_ else as.numeric(fm_time(item$manifest$checked_at))},numeric(1))
  checks<-checks[is.finite(checks)]
  value(list(message=message,checked_at=if(length(checks))format(as.POSIXct(min(checks),origin='1970-01-01',tz='UTC'),'%Y-%m-%d %H:%M:%S UTC')else NULL))
 },domain=session)
 session$onSessionEnded(function()observer$destroy())
 value
}
fm_worker_session<-function(session,login,leagues=function()list()){
 if(!fm_worker_configured())return(invisible(NULL))
 closed<-FALSE;last_account<-NULL;renewed<-NULL
 session$onSessionEnded(function(){closed<<-TRUE;if(!is.null(last_account))fm_worker_detach(last_account,session$token)})
 shiny::observe({
  auth<-login();if(!valid_login(auth)){if(!is.null(last_account)){fm_worker_detach(last_account,session$token);last_account<<-NULL};return()}
  account<-auth[['userid']];if(!is.null(last_account)&&!identical(last_account,account))fm_worker_detach(last_account,session$token);last_account<<-account;s<-fm_worker_account_state(account)
  s$sessions<-unique(c(s$sessions,session$token))
  if(!identical(session$input$worker_visible,FALSE)){
   shiny::invalidateLater(5000,session);fm_worker_poll(account,session$token)
  }
 },priority=1000)
 output<-session$output
 status<-fm_worker_status_source(session,login)
 output$worker_status<-shiny::renderUI({
  view<-status();if(is.null(view))return(NULL)
  shiny::tags$div(class='alert alert-info',role='status',view$message,
   if(!is.null(view$checked_at))shiny::tags$small(style='display:block',paste('Oldest saved check among recently displayed data:',view$checked_at)))
 })
 output$worker_settings<-shiny::renderUI({
  auth<-login();if(!valid_login(auth))return(NULL);s<-fm_worker_account_state(auth[['userid']]);s$settings_revision()
  if(!s$known)return(shiny::tags$p('Loading background sync settings…'))
  a<-s$account
  shiny::tags$details(id='worker_settings_details',open=if(isTRUE(shiny::isolate(session$input$worker_settings_open)))'open'else NULL,shiny::tags$summary('Background sync'),
   shiny::tags$p('Allow this server to retrieve factual Futmondo data while you are signed out. No password is saved. This continues until you disable it or Futmondo rejects access.'),
   shiny::checkboxInput('worker_enable','Enable background sync',isTRUE(a$enabled)),
   shiny::textInput('worker_timezone','Timezone',a$timezone %||% session$input$worker_timezone_detected %||% 'UTC'),
   shiny::fluidRow(shiny::column(6,shiny::numericInput('worker_quiet_start','Quiet hours start (0–23)',a$quiet_start %||% 0,0,23)),
    shiny::column(6,shiny::numericInput('worker_quiet_end','Quiet hours end (0–23)',a$quiet_end %||% 7,0,23))),
   {items<-tryCatch(leagues(),error=function(e)list());choices<-setNames(vapply(items,function(x)fm_scalar(x$id %||% x$`_id`,''),character(1)),vapply(items,function(x)fm_scalar(x$name,'League'),character(1)));
    if(length(choices))shiny::checkboxGroupInput('worker_leagues','Leagues to sync',choices=choices,selected=setdiff(unname(choices),unlist(a$excluded_leagues)))
    else shiny::tags$p('All active leagues are included. League choices appear when your league list is ready.')},
   shiny::actionButton('worker_save','Save background sync settings'),shiny::uiOutput('worker_setting_feedback'))
 })
 feedback<-shiny::reactiveVal(NULL);output$worker_setting_feedback<-shiny::renderUI(shiny::tags$p(feedback()))
 authorize<-function(auth,enabled,settings=NULL,renew_only=FALSE){
  generation<-auth[['userid']]
  sealed<-if(enabled)fm_worker_encrypt(auth)else NULL
  payload<-if(renew_only)list(p_account=generation,p_token=sealed,p_credential=attr(sealed,'credential_id'))else c(list(p_account=generation,p_enabled=enabled,p_token=sealed,p_credential=attr(sealed,'credential_id')),settings)
  promises::then(fm_worker_rpc(if(renew_only)'renew'else'authorize',payload,session$token),function(a){
   if(closed||!identical(shiny::isolate(login()[['userid']]),generation))return(NULL)
   if(is.null(a)){fm_worker_poll(generation,session$token,TRUE);return(NULL)}
   if(enabled)renewed<<-paste0(openssl::sha256(charToRaw(auth[['token']])))
   s<-fm_worker_account_state(generation);fm_worker_set_policy(s,a);s$queued<-list();s$errors<-list();fm_worker_settle(s)
   if(enabled){d<-fm_worker_descriptor('get_active_championships',list(),resource_context(account_id=generation));fm_worker_enqueue(d,session$token,TRUE)}
   if(!renew_only)feedback('Background sync settings saved.');fm_worker_notify(generation);fm_worker_poll(generation,session$token,TRUE);NULL
  },function(error){feedback('Could not save background sync settings. Database acknowledgement is required.');NULL})
 }
 shiny::observeEvent(session$input$worker_save,{
  auth<-login();shiny::req(valid_login(auth));s<-fm_worker_account_state(auth[['userid']]);shiny::req(s$known)
  tz<-session$input$worker_timezone;start<-session$input$worker_quiet_start;end<-session$input$worker_quiet_end
  valid_hour<-function(x)is.numeric(x)&&length(x)==1L&&is.finite(x)&&x==floor(x)&&x>=0&&x<=23
  if(!is.character(tz)||length(tz)!=1L||!tz%in%OlsonNames()||!valid_hour(start)||!valid_hour(end)){feedback('Choose a valid timezone and whole hours from 0 to 23.');return()}
  items<-tryCatch(leagues(),error=function(e)list());ids<-vapply(items,function(x)fm_scalar(x$id %||% x$`_id`,''),character(1))
  excluded<-if(length(ids))unique(c(setdiff(ids,session$input$worker_leagues),setdiff(unlist(s$account$excluded_leagues),ids)))else unlist(s$account$excluded_leagues)
  if(is.null(excluded))excluded<-character()
  authorize(auth,isTRUE(session$input$worker_enable),list(p_timezone=tz,p_start=start,p_end=end,p_excluded=I(excluded)))
 })
 shiny::observe({
  auth<-login();if(!valid_login(auth))return();s<-fm_worker_account_state(auth[['userid']]);s$policy_revision()
  key<-paste0(openssl::sha256(charToRaw(auth[['token']])))
  if(s$known&&isTRUE(s$account$enabled)&&!identical(renewed,key)){
   renewed<<-key;a<-s$account
   authorize(auth,TRUE,renew_only=TRUE)
  }
 },priority=100)
 invisible(NULL)
}
fm_worker_invalidate<-function(account,league,team=NULL,player=NULL){
 if(!fm_worker_configured())return(invisible(NULL))
 s<-fm_worker_account_state(account)
 if(!isTRUE(s$account$enabled))return(invisible(NULL))
 for(k in names(s$descriptors)){
  d<-s$descriptors[[k]]$descriptor
  if(identical(d$context$championship_id,league)&&
     (grepl('market|roster|players_from_team|info|financial|capacity|pressroom|finances|summary',d$handler))){
   item<-fm_worker_get(account,k)
   if(!is.null(item)){item$manifest$valid_until<-rival_finance_stamp(Sys.time()-1);attr(item$data,'fetch_status')<-'stale';fm_worker_put(account,k,item);rev<-fm_worker_resource_revision(s,k);rev(shiny::isolate(rev())+1L)}
   fm_worker_enqueue(d,s$descriptors[[k]]$owner,TRUE)
  }
 }
 fm_worker_notify(account);invisible(NULL)
}

fm_worker_invalidate_pattern<-function(context,resources=NULL,pattern=NULL){
 if(!fm_worker_configured())return(invisible(NULL))
 s<-fm_worker_account_state(context$account_id);if(!isTRUE(s$account$enabled))return(invisible(NULL))
 aliases<-c(get_players_from_team='roster',get_championship_players='championshipplayers',get_championship_pressroom='pressroom',get_lineup_from_team='lineup',get_user_team_info='team_info')
 for(k in names(s$descriptors)){
  d<-s$descriptors[[k]]$descriptor
  if(nzchar(context$championship_id %||% '')&&!identical(context$championship_id,d$context$championship_id))next
  alias<-if(d$handler%in%names(aliases))aliases[[d$handler]]else sub('^get_','',d$handler)
  candidates<-c(d$handler,paste(alias,d$context$championship_id,d$context$user_team_id,d$context$player_id,sep='_'))
  matches<-if(!is.null(pattern))any(grepl(pattern,candidates))else any(vapply(resources,function(r)any(startsWith(candidates,r)|startsWith(r,candidates)),logical(1)))
  if(matches){
   item<-fm_worker_get(context$account_id,k)
   if(!is.null(item)){attr(item$data,'valid_until')<-as.numeric(Sys.time())-1;attr(item$data,'fetch_status')<-'stale';fm_worker_put(context$account_id,k,item)}
   fm_worker_enqueue(d,s$descriptors[[k]]$owner,TRUE)
  }
 }
 invisible(NULL)
}
