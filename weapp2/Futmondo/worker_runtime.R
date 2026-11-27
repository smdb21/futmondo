# Browsing always reads durable observations. Collection is a separate service.
.fm_worker <- new.env(parent=emptyenv())
.fm_worker$accounts<-new.env(parent=emptyenv());.fm_worker$values<-new.env(parent=emptyenv())
fm_worker_configured<-function() !isTRUE(getOption('futmondo.offline',FALSE))&&
 !isTRUE(getOption('futmondo.worker_helper',FALSE))
fm_worker_can_connect<-function() fm_worker_configured()&&identical(tolower(Sys.getenv('FUTMONDO_WORKER_ENABLED','true')),'true')&&
 grepl('^[a-fA-F0-9]{64}$',Sys.getenv('FUTMONDO_WORKER_KEY'))&&requireNamespace('sodium',quietly=TRUE)&&nzchar(get_sb_url())&&nzchar(get_sb_key())
fm_worker_resource_revision<-function(state,key){
 if(is.null(state$resource_revisions[[key]]))state$resource_revisions[[key]]<-shiny::withReactiveDomain(NULL,shiny::reactiveVal(0L))
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
  state$bootstrap_complete<-FALSE;state$bootstrap_waiters<-list();state$requested_keys<-character();state$read_scheduled<-FALSE
  state$status_revision<-shiny::withReactiveDomain(NULL,shiny::reactiveVal(0L));state$redirects<-list();state$redirect_revision<-shiny::withReactiveDomain(NULL,shiny::reactiveVal(0L));state$descriptors<-list();state$waiters<-list();state$queued<-list();state$health<-NULL;state$jobs<-list();state$errors<-list();state$sessions<-character();state$policy_revision<-shiny::withReactiveDomain(NULL,shiny::reactiveVal(0L));state$settings_revision<-shiny::withReactiveDomain(NULL,shiny::reactiveVal(0L));state$resource_revisions<-new.env(parent=emptyenv())
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
 TRUE
}
fm_worker_rpc<-function(name,payload,owner=''){
 if(!nzchar(get_sb_url())||!nzchar(get_sb_key()))return(promises::promise_reject(simpleError('Database access is not configured.')))
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
 capacity_contract<-identical(m$handler,'get_team_acquisition_capacity')&&identical(m$completeness,'unknown')&&
  identical(m$status,'ok')&&acquisition_capacity_observation_complete(value)
 attr(value,'observed_at')<-fm_time(m$observed_at);attr(value,'fetched_at')<-fm_time(m$checked_at)
 attr(value,'worker_completeness')<-m$completeness
 # Older round snapshots recorded 'unknown' because the collector read only
 # attr(complete), while this getter used attr(catalog_complete). Reconstruct
 # that one metadata mapping from the saved parser proof, never from row count
 # or a successful HTTP response alone. Other handlers/failures stay unchanged.
 if(identical(m$handler,'get_finished_rounds')&&identical(m$completeness,'unknown')&&
    isTRUE(m$status%in%c('ok','empty'))&&isTRUE(attr(value,'catalog_complete'))&&
    identical(attr(value,'catalog_status'),'ok')&&is.data.frame(value)&&nrow(value)>0L&&nrow(value)<=120L&&
    all(c('round_id','round_number','is_finished')%in%names(value))&&
    is.character(value$round_id)&&!anyNA(value$round_id)&&all(nzchar(value$round_id))&&!anyDuplicated(value$round_id)&&
    is.numeric(value$round_number)&&is.logical(value$is_finished)){
  numbers<-value$round_number;known<-is.finite(numbers);observed<-attr(value,'observed_at')
  if(all(is.na(numbers)|known)&&all(numbers[known]>=1&numbers[known]==floor(numbers[known]))&&
     !anyDuplicated(numbers[known])&&all(known[value$is_finished%in%TRUE])&&
     length(observed)==1L&&!is.na(observed)&&observed<=Sys.time()){
   attr(value,'worker_completeness')<-'complete'
   attr(value,'worker_completeness_source')<-'catalog_complete'
  }
 }
 # Historical capacity envelopes also omitted completeness although the
 # parser stored an explicit verified roster/funds/commitments contract. Recover
 # only that successful contract; its original action deadline stays unchanged.
 if(capacity_contract){
  observed<-attr(value,'observed_at')
  if(length(observed)==1L&&!is.na(observed)&&observed<=Sys.time()){
   attr(value,'complete')<-TRUE
   attr(value,'worker_completeness')<-'complete'
   attr(value,'worker_completeness_source')<-'capacity_contract'
  }
 }
 attr(value,'worker_acquisition_status')<-m$status
 existing<-attr(value,'valid_until') %||% if(is.list(value))value$valid_until else NULL
 deadline<-as.numeric(fm_time(m$valid_until))
 if(is.numeric(existing)&&length(existing)==1L&&is.finite(existing))deadline<-min(deadline,existing)
 attr(value,'valid_until')<-deadline
 attr(value,'fetch_status')<-if(as.numeric(fm_time(m$valid_until))<=as.numeric(Sys.time()))'stale'else m$status
 list(data=value,manifest=m)
}
fm_worker_demand<-function(descriptor,owner='',is_current=NULL){
 s<-fm_worker_account_state(descriptor$context$account_id);key<-descriptor$resource_key
 watchers<-s$descriptors[[key]]$watchers %||% list();now<-as.numeric(Sys.time())
 interest<-if(is.function(is_current))attr(is_current,'fm_worker_interest') %||% paste0('live:',owner)else paste0('legacy:',owner)
 watchers[[interest]]<-list(owner=owner,current=is_current,at=now)
 if(length(watchers)>128L){old<-order(vapply(watchers,`[[`,numeric(1),'at'));watchers[old[seq_len(length(watchers)-128L)]]<-NULL}
 entry<-s$descriptors[[key]]
 s$descriptors[[key]]<-list(descriptor=descriptor,at=now,owner=owner,watchers=watchers,read_complete=entry$read_complete)
 if(length(s$descriptors)>128L){times<-vapply(s$descriptors,function(x)x$at,numeric(1));victims<-names(sort(times))[seq_len(length(times)-128L)];s$descriptors[victims]<-NULL;s$queued[victims]<-NULL;s$errors[victims]<-NULL
  for(v in victims)if(exists(v,s$resource_revisions,inherits=FALSE))rm(list=v,envir=s$resource_revisions)
 }
 if(!key%in%s$requested_keys&&is.null(fm_worker_get(descriptor$context$account_id,key)))fm_worker_schedule_read(descriptor$context$account_id,owner)
 invisible(NULL)
}
# Batch new interests on the next event-loop turn instead of waiting five seconds.
fm_worker_schedule_read<-function(account,owner=''){
 s<-fm_worker_account_state(account)
 if(isTRUE(s$read_scheduled))return(invisible(NULL))
 s$read_scheduled<-TRUE
 later::later(function(){
  if(!identical(.fm_worker$accounts[[account]],s))return()
  s$read_scheduled<-FALSE
  if(!s$busy)fm_worker_poll(account,owner,TRUE)
 },0.001)
 invisible(NULL)
}
fm_worker_preference<-function(account,owner=''){
 s<-fm_worker_account_state(account)
 if(isTRUE(s$bootstrap_complete))return(promises::promise_resolve(s$preferred_league))
 if(length(s$bootstrap_waiters)>=128L)return(promises::promise_reject(resource_failure(category='read_budget')))
 fm_worker_schedule_read(account,owner)
 promises::promise(function(resolve,reject){s$bootstrap_waiters[[length(s$bootstrap_waiters)+1L]]<-list(resolve=resolve,reject=reject,owner=owner)})
}
fm_worker_prefetch<-function(account,leagues,owner=''){
 s<-fm_worker_account_state(account);rows<-leagues$championships
 if(!length(rows))return(invisible(NULL))
 ids<-vapply(rows,function(x)fm_scalar(x$id %||% x$`_id`,''),character(1))
 selected<-s$preferred_league$championship_id
 index<-if(length(selected)==1L&&selected%in%ids)match(selected,ids)else 1L
 row<-rows[[index]];team<-fm_scalar(row$userteam$id %||% row$userteam$`_id`,'')
 if(!nzchar(ids[index])||!nzchar(team))return(invisible(NULL))
 ctx<-resource_context(account_id=account,championship_id=ids[index],user_team_id=team)
 args<-list(championship_id=ids[index],user_team_id=team)
 for(handler in c('get_user_team_info','get_market_players','get_players_from_team'))
  fm_worker_demand(fm_worker_descriptor(handler,args,ctx),owner)
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
 if(is.null(previous)||!grepl('^(get_[a-z0-9_]+|fm_worker_finances)$',previous$handler)||
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
 if(!fm_worker_configured()||!grepl('^(get_[a-z0-9_]+|fm_worker_finances)$',handler))return(context)
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
   attr(saved$data,'fetch_status')<-'stale'
  }
  return(promises::promise_resolve(saved$data))
 }
 promises::promise(function(resolve,reject){
  if(length(s$waiters)>=128L){reject(simpleError('Background read capacity reached; existing requests are still pending.'));return(invisible(NULL))}
  id<-paste0('w',paste0(openssl::sha256(serialize(list(Sys.time(),runif(1)),NULL))))
  waiter<-list(key=d$resource_key,owner=owner,current=is_current,resolve=resolve,reject=reject,descriptor=d,direct=direct,
   allow_missing=identical(handler,'get_championship_transfer_amounts'))
  waiter$cancel<-later::later(function(){
   if(!is.null(s$waiters[[id]])){s$waiters[[id]]<-NULL;reject(simpleError('Background synchronization is still pending; inspect sync status.'))}
  },120)
  s$waiters[[id]]<-waiter
 })
}
fm_worker_settle<-function(s,read_keys=character()){
 for(id in names(s$waiters)){
  w<-s$waiters[[id]];saved<-fm_worker_get(w$descriptor$context$account_id,w$key)
  cancelled<-!isTRUE(tryCatch(w$current(),error=function(e)FALSE))
  ready<-!is.null(saved)
  missing<-isTRUE(w$allow_missing)&&w$key%in%read_keys
  if(cancelled||ready||missing){
   s$waiters[[id]]<-NULL;if(is.function(w$cancel))w$cancel()
   if(cancelled)w$reject(structure(list(message='Session request cancelled',call=NULL),class=c('fm_async_cancelled','error','condition')))
   else if(!ready)w$resolve(list())
   else {
    if(!resource_snapshot_unexpired(saved$data))attr(saved$data,'fetch_status')<-'stale'
    w$resolve(saved$data)
   }
  }
 }
}
fm_worker_poll<-function(account,owner='',force=FALSE){
 s<-fm_worker_account_state(account)
 if(s$busy||!force&&as.numeric(Sys.time())-s$last_poll<14)return(invisible(NULL))
 s$busy<-TRUE;s$last_poll<-as.numeric(Sys.time())
 bootstrap<-!isTRUE(s$bootstrap_complete)
 if(bootstrap){
  descriptor<-fm_worker_descriptor('get_active_championships',list(),resource_context(account_id=account))
  fm_worker_demand(descriptor,owner)
 }
 keys<-fm_worker_live_keys(s)
 read_keys<-keys
 s$requested_keys<-union(intersect(s$requested_keys,names(s$descriptors)),keys)
 versions<-setNames(lapply(keys,function(k)fm_worker_get(account,k)$manifest$version %||% ''),keys)
 if(!length(versions))versions<-structure(list(),names=character())
 descriptors<-setNames(lapply(keys,function(k)s$descriptors[[k]]$descriptor),keys)
 if(!length(descriptors))descriptors<-structure(list(),names=character())
 # Derived transfer summaries have no independent acquisition job. A first
 # completed database lookup distinguishes an absent projection from a pending read.
 finish_clause_reads<-function(){
  if(bootstrap)return()
  for(key in read_keys){
   entry<-s$descriptors[[key]]
   if(!identical(entry$descriptor$handler,'get_clause_transfer_summary')||isTRUE(entry$read_complete))next
   s$descriptors[[key]]$read_complete<-TRUE
   if(is.null(fm_worker_get(account,key))){
    revision<-fm_worker_resource_revision(s,key);revision(shiny::isolate(revision())+1L)
   }
  }
 }
 p<-tryCatch(fm_worker_rpc(if(bootstrap)'bootstrap'else'read',if(bootstrap)list(p_account=account)else list(p_account=account,p_versions=versions,p_descriptors=descriptors),owner),error=promises::promise_reject)
 p<-promises::then(p,function(result){
  if(!is.list(result)||!all(c('account','resources','jobs')%in%names(result)))stop('Invalid database read response')
  result
 })
 promises::then(p,function(result){
  if(!identical(.fm_worker$accounts[[account]],s))return(NULL)
  s$busy<-FALSE;fm_worker_set_policy(s,result$account);s$health<-result$health;s$jobs<-result$jobs;s$service_error<-NULL
  if(bootstrap){s$bootstrap_complete<-TRUE;s$preferred_league<-result$preferred_league
   for(w in s$bootstrap_waiters)w$resolve(s$preferred_league);s$bootstrap_waiters<-list()
   # The bootstrap RPC read only the account descriptor, not page interests.
   s$requested_keys<-descriptor$resource_key
  }
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
     if(bootstrap)fm_worker_prefetch(account,item$data,owner)
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
  # Expiry changes presentation once, not acquisition. Preserve source times and
  # notify only this resource when its local freshness crosses the deadline.
  for(k in keys){
   saved<-fm_worker_get(account,k)
   if(!is.null(saved)&&!resource_snapshot_unexpired(saved$data)&&!identical(attr(saved$data,'fetch_status'),'stale')){
    attr(saved$data,'fetch_status')<-'stale';fm_worker_put(account,k,saved)
    revision<-fm_worker_resource_revision(s,k);revision(shiny::isolate(revision())+1L)
   }
  }
  # Polling is read-only: stale/missing observations never alter worker jobs.
  finish_clause_reads()
  fm_worker_settle(s,if(!bootstrap)read_keys else character());fm_worker_notify(account)
  if(length(setdiff(fm_worker_live_keys(s),s$requested_keys)))fm_worker_schedule_read(account,owner)
  NULL
 },function(error){
  if(!identical(.fm_worker$accounts[[account]],s))return(NULL)
  s$busy<-FALSE;s$service_error<-'Saved data could not be read from the database.'
  finish_clause_reads()
  for(w in s$bootstrap_waiters)w$reject(error);s$bootstrap_waiters<-list()
  for(id in names(s$waiters)){
   w<-s$waiters[[id]];s$waiters[[id]]<-NULL;if(is.function(w$cancel))w$cancel();w$reject(error)
  }
  fm_worker_notify(account);NULL
 })
 invisible(NULL)
}
fm_worker_peek<-function(handler,args,context,direct,is_current=NULL){
 mode<-fm_worker_mode(context$account_id)
 if(identical(mode,FALSE))return(direct())
 context<-fm_worker_resolve_context(handler,args,context)
 d<-fm_worker_descriptor(handler,args,context);fm_worker_demand(d,context$session_id %||% '',is_current)
 s<-fm_worker_account_state(context$account_id);try(fm_worker_resource_revision(s,d$resource_key)(),silent=TRUE)
 item<-fm_worker_get(context$account_id,d$resource_key)
 if(is.null(item))stop(structure(list(message='Loading data from background sync…',call=NULL),class=c('shiny.silent.error','fm-loading','validation','error','condition')))
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
 for(i in rev(seq_along(s$bootstrap_waiters)))if(identical(s$bootstrap_waiters[[i]]$owner,owner)){
  w<-s$bootstrap_waiters[[i]];s$bootstrap_waiters[i]<-NULL
  w$reject(structure(list(message='Session request cancelled',call=NULL),class=c('fm_async_cancelled','error','condition')))
 }
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
  health_age<-as.numeric(difftime(Sys.time(),fm_time(s$health$heartbeat),units='secs'))
  failed<-!isTRUE(s$account$enabled)||!identical(s$account$auth_status,'active')||length(health_age)!=1L||!is.finite(health_age)||health_age>180||!is.null(s$service_error)||!is.null(s$connect_error)||length(s$errors)>0L
  message<-if(!is.null(s$service_error))s$service_error else if(!is.null(s$connect_error))s$connect_error else
   if(!s$known)'Loading saved data…'else if(!isTRUE(s$account$enabled))'Automatic sync is not connected; saved data remains available.'else
   if(!identical(s$account$auth_status,'active'))'Background access needs renewal. Log in again.'else
   if(length(health_age)!=1L||!is.finite(health_age)||health_age>180)'Background worker is not responding; saved data remains available.'else
    if(!is.null(s$service_error))s$service_error else
     if(length(s$errors))paste(unique(unlist(s$errors)),collapse=' ')else 'Background sync is enabled.'
  live_keys<-fm_worker_live_keys(s)
  jobs<-s$jobs[vapply(s$jobs,function(j)j$key%in%live_keys,logical(1))]
  running<-sum(vapply(jobs,function(j)isTRUE(j$running),logical(1)))
  # Routine collection is not a page-loading state or a changing queue banner.
  checks<-vapply(live_keys,function(k){item<-fm_worker_get(account,k);if(is.null(item))NA_real_ else as.numeric(fm_time(item$manifest$checked_at))},numeric(1))
  checks<-checks[is.finite(checks)]
  value(list(message=message,error=failed,checked_at=if(length(checks))as.POSIXct(min(checks),origin='1970-01-01',tz='UTC')else NULL))
 },domain=session)
 session$onSessionEnded(function()observer$destroy())
 value
}
fm_worker_session<-function(session,login,leagues=function()list(),source_note=function()""){
 if(!fm_worker_configured())return(invisible(NULL))
 closed<-FALSE;last_account<-NULL;login_identity<-NULL;renewed<-NULL;connecting<-FALSE;connect_after<-0;disconnecting<-FALSE;login_generation<-NULL
 session$onSessionEnded(function(){closed<<-TRUE;if(!is.null(last_account))fm_worker_detach(last_account,session$token)})
 shiny::observe({
  auth<-login();if(!valid_login(auth)){if(!is.null(last_account)){fm_worker_detach(last_account,session$token);last_account<<-NULL};login_identity<<-NULL;renewed<<-NULL;connecting<<-FALSE;disconnecting<<-FALSE;connect_after<<-0;login_generation<<-NULL;return()}
  identity<-paste(auth[['userid']],paste0(openssl::sha256(charToRaw(auth[['token']]))))
  if(!identical(login_identity,identity)){login_identity<<-identity;renewed<<-NULL;connecting<<-FALSE;disconnecting<<-FALSE;connect_after<<-0;login_generation<<-NULL}
  account<-auth[['userid']];if(!is.null(last_account)&&!identical(last_account,account))fm_worker_detach(last_account,session$token);last_account<<-account;s<-fm_worker_account_state(account)
  s$sessions<-unique(c(s$sessions,session$token))
  s$policy_revision()
  if(!identical(renewed,paste0(openssl::sha256(charToRaw(auth[['token']]))))&&s$known&&!connecting&&!disconnecting&&as.numeric(Sys.time())>=connect_after)connect(auth)
  if(!identical(session$input$worker_visible,FALSE)){
   shiny::invalidateLater(15000,session);fm_worker_poll(account,session$token)
  }
 },priority=1000)
 output<-session$output
 status<-fm_worker_status_source(session,login)
 status_visible<-resource_display_source(session,function()!is.null(status()))
 status_message<-resource_display_source(session,function()status()$message %||% '')
 status_error<-resource_display_source(session,function()isTRUE(status()$error))
 status_checked<-resource_display_source(session,function(){
  if(identical(session$input$tabs,'login'))return('')
  checked<-status()$checked_at
  if(is.null(checked))''else paste('Oldest displayed data check:',fm_format_datetime(checked))
 })
 status_source_note<-resource_display_source(session,function(){
  if(!isTRUE(status_visible()))return('')
  source_note()
 })
 output$worker_status<-shiny::renderUI({
  if(!isTRUE(status_visible()))return(NULL)
  fm_info(error=status_error(),source="Background sync",
   shiny::textOutput('worker_status_message',inline=TRUE),
   shiny::tags$small(class='fm-info-neutral',style='display:block',shiny::textOutput('worker_status_checked_at',inline=TRUE)),
   shiny::tags$small(class='fm-info-neutral',style='display:block',shiny::textOutput('worker_roster_source_times',inline=TRUE)))
 })
 output$worker_status_message<-shiny::renderText(status_message())
 shiny::outputOptions(output,"worker_status_message",suspendWhenHidden=FALSE)
 output$worker_status_checked_at<-shiny::renderText(status_checked())
 shiny::outputOptions(output,"worker_status_checked_at",suspendWhenHidden=FALSE)
 output$worker_roster_source_times<-shiny::renderText(status_source_note())
 shiny::outputOptions(output,"worker_roster_source_times",suspendWhenHidden=FALSE)
 output$worker_settings<-shiny::renderUI({
  auth<-login();if(!valid_login(auth))return(NULL);s<-fm_worker_account_state(auth[['userid']]);s$settings_revision()
  if(!s$known)return(fm_info('Loading background sync settings…',pending=TRUE))
  a<-s$account
  shiny::tags$details(id='worker_settings_details',open=if(isTRUE(shiny::isolate(session$input$worker_settings_open)))'open'else NULL,shiny::tags$summary('Background sync'),
   shiny::tags$p('Signing in connects automatic synchronization, including while you are signed out. Access is encrypted; your password is never saved.'),
   {items<-tryCatch(leagues(),error=function(e)list());choices<-setNames(vapply(items,function(x)fm_scalar(x$id %||% x$`_id`,''),character(1)),vapply(items,function(x)fm_scalar(x$name,'League'),character(1)));
    if(length(choices))shiny::checkboxGroupInput('worker_leagues','Leagues to sync',choices=choices,selected=setdiff(unname(choices),unlist(a$excluded_leagues)))
    else shiny::tags$p('All active leagues are included. League choices appear when your league list is ready.')},
   shiny::actionButton('worker_save','Save leagues'),
   shiny::actionButton('worker_disconnect','Disconnect account and stop sync'),shiny::uiOutput('worker_setting_feedback'))
 })
 feedback<-shiny::reactiveVal(NULL);output$worker_setting_feedback<-shiny::renderUI({
  message<-feedback();if(is.null(message))return(NULL)
  fm_info(message,error=!identical(message,'Background sync settings saved.'))
 })
 connect<-function(auth){
  if(connecting||disconnecting)return(invisible(NULL))
  account<-auth[['userid']];state<-fm_worker_account_state(account)
  if(!fm_worker_can_connect()){
   state$connect_error<-'Automatic sync is not configured on the server; saved data remains available.'
   connect_after<<-as.numeric(Sys.time())+30;fm_worker_notify(account);return(invisible(NULL))
  }
  connecting<<-TRUE;key<-paste0(openssl::sha256(charToRaw(auth[['token']])))
  if(is.null(login_generation))login_generation<<-state$account$generation %||% 0
  current<-function()!closed&&!disconnecting&&identical(.fm_worker$accounts[[account]],state)&&
   identical(shiny::isolate(login()),auth)
  request<-tryCatch({
   sealed<-fm_worker_encrypt(auth)
   payload<-list(p_account=account,p_token=sealed,p_credential=attr(sealed,'credential_id'),
    p_generation=login_generation,p_timezone=fm_display_timezone())
   fm_worker_rpc('connect',payload,session$token)
  },error=promises::promise_reject)
  promises::then(request,function(a){
   if(!current())return(NULL)
   connecting<<-FALSE
   if(is.null(a)||!is.null(state$account$generation)&&a$generation<state$account$generation){
    renewed<<-key;state$connect_error<-'Synchronization access was revoked. Sign in again to reconnect.'
    fm_worker_notify(account);fm_worker_poll(account,session$token,TRUE);return(NULL)
   }
   renewed<<-key;state$connect_error<-NULL;fm_worker_set_policy(state,a);fm_worker_notify(account);NULL
  },function(error){
   if(!current())return(NULL)
   connecting<<-FALSE;connect_after<<-as.numeric(Sys.time())+30
   state$connect_error<-'Automatic sync could not be connected. The server will retry; saved data remains available.'
   fm_worker_notify(account);NULL
  })
  invisible(NULL)
 }
 shiny::observeEvent(session$input$worker_save,{
  auth<-login();shiny::req(valid_login(auth));s<-fm_worker_account_state(auth[['userid']]);shiny::req(s$known)
  items<-tryCatch(leagues(),error=function(e)list());ids<-vapply(items,function(x)fm_scalar(x$id %||% x$`_id`,''),character(1))
  excluded<-if(length(ids))unique(c(setdiff(ids,session$input$worker_leagues),setdiff(unlist(s$account$excluded_leagues),ids)))else unlist(s$account$excluded_leagues)
  promises::then(fm_worker_rpc('preferences',list(p_account=auth[['userid']],p_generation=s$account$generation,
   p_excluded=I(excluded %||% character())),session$token),function(a){
   if(closed||!identical(shiny::isolate(login()),auth)||!identical(.fm_worker$accounts[[auth[['userid']]]],s))return(NULL)
   if(is.null(a)){feedback('Settings changed elsewhere. Reopen settings and retry.');return(NULL)}
   fm_worker_set_policy(s,a);feedback('Background sync settings saved.');NULL
  },function(error){if(!closed&&identical(shiny::isolate(login()),auth))feedback('Could not save synchronization settings.');NULL})
  invisible(NULL)
 })
 shiny::observeEvent(session$input$worker_disconnect,{
  auth<-login();shiny::req(valid_login(auth));s<-fm_worker_account_state(auth[['userid']]);shiny::req(s$known)
  if(disconnecting)return();disconnecting<<-TRUE
  promises::then(fm_worker_rpc('disconnect',list(p_account=auth[['userid']],p_generation=s$account$generation),session$token),function(a){
   if(closed||!identical(shiny::isolate(login()),auth)||!identical(.fm_worker$accounts[[auth[['userid']]]],s))return(NULL)
   if(is.null(a)){disconnecting<<-FALSE;feedback('Account access changed. Retry disconnecting.');return(NULL)}
   fm_worker_set_policy(s,a)
   logout<-session$userData$disconnect_login
   if(is.function(logout))logout()
   NULL
  },function(error){if(!closed&&identical(shiny::isolate(login()),auth)){disconnecting<<-FALSE;feedback('Could not revoke synchronization access. Retry disconnecting.')};NULL})
  invisible(NULL)
 })

 invisible(NULL)
}
# Explicit league refreshes advance existing factual work. Publication flows
# through the same versioned database read as routine worker updates.
fm_worker_refresh_league <- function(login, league, team, player=NULL, session=shiny::getDefaultReactiveDomain(), manual=FALSE, feedback=NULL) {
 if(!valid_login(login)||!isTRUE(fm_worker_mode(login[['userid']])))return(invisible(FALSE))
 account<-login[['userid']]
 payload<-list(p_account=account,p_league=league,p_team=team,
  p_players=I(if(is.null(player))character()else as.character(player)))
 report<-function(message,type){
  if(is.null(session)||session$isClosed())return()
  if(is.function(feedback))feedback(message) else shiny::showNotification(message,type=type,id="league-refresh",session=session)
 }
 pending<-tryCatch(fm_worker_rpc('league_refresh',payload),error=promises::promise_reject)
 promises::then(pending,function(count){
  if(is.numeric(count)&&length(count)==1L&&isTRUE(count>0)){
   if(manual)report('Refresh queued. Balances and players will update as new data arrives.','message')
  }else report('League sync could not be queued. Check Background sync or try Refresh again.','warning')
  NULL
 },function(error){
  report('The league update could not be queued. Try Refresh again.','warning')
  NULL
 })
 invisible(TRUE)
}

# Routine invalidation still only rereads durable observations.
fm_worker_invalidate<-function(account,league,team=NULL,player=NULL){
 if(!fm_worker_configured())return(invisible(NULL))
 s<-fm_worker_account_state(account)
 fm_worker_poll(account,force=TRUE)
 invisible(NULL)
}
fm_worker_invalidate_pattern<-function(context,resources=NULL,pattern=NULL){
 if(!fm_worker_configured())return(invisible(NULL))
 s<-fm_worker_account_state(context$account_id)
 fm_worker_poll(context$account_id,context$session_id %||% '',force=TRUE)
 invisible(NULL)
}
