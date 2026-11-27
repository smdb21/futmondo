# Interactive availability reuse. See docs/player_availability.md.
# Only factual payloads enter the existing private evidence store; credentials
# remain in the interactive session and are never part of a snapshot.
.player_availability_epochs<-new.env(parent=emptyenv())
player_availability_scope <- function(context,kind) {
  fields<-c('account_id','championship_id','user_team_id','competition_id','season','scoring_version')
  if(!kind%in%c('team','summary','listings')||!is.list(context))return(NULL)
  scope<-lapply(context[fields],function(x)fm_scalar(x,''));names(scope)<-fields
  if(any(!vapply(scope,function(x)nzchar(x)&&nchar(x,type='bytes')<=256L,logical(1))))return(NULL)
  scope$player_id<-if(kind=='summary')fm_scalar(context$player_id,'')else ''
  if(kind=='summary'&&(!nzchar(scope$player_id)||nchar(scope$player_id,type='bytes')>256L))return(NULL)
  c(list(version=1L,kind=kind),scope)
}
player_availability_id <- function(scope) paste0('rival-cache:availability-v1:',
  as.character(openssl::sha256(serialize(scope,NULL,version=2))))
player_availability_epoch <- function(context) {
  scope<-player_availability_scope(context,'team');if(is.null(scope))return(NULL)
  key<-player_availability_id(scope)
  value<-.player_availability_epochs[[key]]
  if(is.null(value)){
    value<-list(scope=scope,revision=shiny::withReactiveDomain(NULL,shiny::reactiveVal(0L)),team=NULL,summary=NULL,listings=NULL,players=list(),at=Sys.time())
    .player_availability_epochs[[key]]<-value
    keys<-ls(.player_availability_epochs)
    if(length(keys)>256L){stamps<-vapply(mget(keys,.player_availability_epochs),function(x)as.numeric(x$at),numeric(1));rm(list=keys[which.min(stamps)],envir=.player_availability_epochs)}
  }
  value
}
player_availability_invalidated <- function(context,kind) {
  epoch<-player_availability_epoch(context);if(is.null(epoch))return(NULL)
  epoch$revision()
  if(kind=='team')epoch$team else if(kind=='listings')epoch$listings else {
    stamps<-c(epoch$summary,epoch$players[[fm_scalar(context$player_id,'')]])
    if(length(stamps))max(stamps)else NULL
  }
}
# Preserve source deadlines through table projections. Explicit persisted/worker
# deadlines win; legacy native observations use their existing resource policy.
player_availability_deadline <- function(data,resource) {
  stamp<-fm_time(attr(data,'observed_at') %||% if(is.data.frame(data)&&nrow(data)==1L&&'observed_at'%in%names(data))data$observed_at[1]else NULL)
  if(length(stamp)!=1L||is.na(stamp))return(NA_real_)
  deadline<-attr(data,'valid_until') %||% (as.numeric(stamp)+resource_policy(resource)$ttl)
  if(!is.numeric(deadline)||length(deadline)!=1L||!is.finite(deadline))NA_real_ else deadline
}
# The market endpoint's rows are published listings. Preserve that endpoint's
# own source deadline when handing them to either Market or Today's table.
player_availability_market_rows <- function(data) {
  if(!is.data.frame(data))return(data)
  data$market_inMarket<-rep(TRUE,nrow(data))
  if (nrow(data) && is.null(data$expirationDate)) data$expirationDate <- rep(NA_character_, nrow(data))
  attr(data,'availability_owner_valid_until')<-player_availability_deadline(data,'market')
  attr(data,'availability_market_valid_until')<-attr(data,'availability_owner_valid_until')
  attr(data,'availability_market_observed_at')<-attr(data,'observed_at')
  attr(data,'availability_complete')<-!acquisition_data_unverified(data)
  data
}
# A successful target display check survives source expiry in this browser.
# Facts are already persisted by their catalog/market/summary acquisition path;
# this bounded session marker is not persisted as purchase authorization.
player_availability_remember_target <- function(session,identity,row=NULL) {
  root<-session$rootScope()
  if(is.null(root$userData$player_availability_targets))root$userData$player_availability_targets<-new.env(parent=emptyenv())
  entries<-root$userData$player_availability_targets
  key<-as.character(openssl::sha256(serialize(identity,NULL,version=2)))
  if(is.null(row)){
    value<-entries[[key]]
    if(is.null(value))return(NULL)
    value$used_at<-Sys.time();entries[[key]]<-value
    return(value$row)
  }
  bytes<-as.numeric(object.size(row))
  if(bytes>128L*1024L||any(fm_worker_secret_name(names(row))))return(row)
  attr(row,'availability_session_verified')<-TRUE
  entries[[key]]<-list(row=row,bytes=bytes,used_at=Sys.time())
  keys<-ls(entries)
  while(length(keys)>64L||sum(vapply(mget(keys,entries),function(x)x$bytes,numeric(1)))>4L*1024L^2L){
    victim<-keys[which.min(vapply(mget(keys,entries),function(x)as.numeric(x$used_at),numeric(1)))]
    rm(list=victim,envir=entries);keys<-ls(entries)
  }
  row
}
# A successful database observation remains a display fact after its action
# deadline. This does not renew timestamps or make it valid for a transaction.
# Legacy unknown envelope completeness still needs the caller's field contract.
player_availability_observed <- function(data,kind,now=Sys.time()) {
  if(!kind%in%c('summary','listings')||!is.list(data)||
     !isTRUE(attr(data,'worker_acquisition_status')%in%if(kind=='summary')'ok'else c('ok','empty'))||
     !is.null(attr(data,'fetch_error'))||identical(attr(data,'complete'),FALSE)||
     any(attr(data,'worker_completeness')%in%c('partial','incomplete','unavailable'))||
     !isTRUE(attr(data,'fetch_status')%in%c('ok','empty','stale')))return(FALSE)
  stamp<-fm_time(attr(data,'observed_at'));deadline<-attr(data,'valid_until')
  if(length(stamp)!=1L||is.na(stamp)||stamp>now||
     !is.numeric(deadline)||length(deadline)!=1L||!is.finite(deadline))return(FALSE)
  if(kind=='listings')return(is.data.frame(data)&&
    (!nrow(data)||('id'%in%names(data)&&!anyNA(data$id)&&all(nzchar(as.character(data$id)))&&!anyDuplicated(data$id))))
  !is.data.frame(data)
}

# Pure action validation keeps expired, incomplete and failed evidence out of reuse.
player_availability_valid <- function(data,kind,now=Sys.time()) {
  if(!is.list(data)||acquisition_data_unverified(data))return(FALSE)
  stamp<-fm_time(attr(data,'observed_at'))
  if(length(stamp)!=1L||is.na(stamp)||stamp>now)return(FALSE)
  if(kind=='team'){
    deadline<-data$valid_until
    identical(data$status,'ok')&&is.numeric(deadline)&&length(deadline)==1L&&is.finite(deadline)&&
      as.numeric(now)<deadline&&resource_snapshot_unexpired(data,now)&&
      !identical(evaluate_acquisition_preflight(data,'offer',now=now)$reason,'unavailable')
  }else{
    lifetime<-if(kind=='summary')60 else 20
    if(kind=='listings'&&(!is.data.frame(data)||(nrow(data)&&(!'id'%in%names(data)||anyNA(data$id)||any(!nzchar(as.character(data$id)))||anyDuplicated(data$id)))))return(FALSE)
    deadline<-attr(data,'valid_until') %||% (as.numeric(stamp)+lifetime)
    is.numeric(deadline)&&length(deadline)==1L&&is.finite(deadline)&&
      as.numeric(now)<min(deadline,as.numeric(stamp)+lifetime)&&resource_snapshot_unexpired(data,now)
  }
}
# JSON's default significant-digit formatting loses sub-millisecond precision
# at epoch-sized doubles. Encode doubles/time as exact round-trip decimal text.
player_availability_encode <- function(value) {
  pack<-function(node){
    if(node$type%in%c('double','time'))node$values<-I(vapply(node$values,function(x)if(is.na(x))NA_character_ else sprintf('%.17g',x),character(1)))
    if(node$type%in%c('list','frame'))node$values<-lapply(node$values,pack)
    if(length(node$attributes))node$attributes<-lapply(node$attributes,pack)
    node
  }
  pack(rival_snapshot_encode(value))
}
player_availability_decode <- function(node) {
  unpack<-function(x,depth=0L){
    if(depth>18L||!is.list(x)||!is.character(x$type)||length(x$type)!=1L)stop('Invalid availability snapshot depth')
    if(x$type%in%c('double','time'))x$values<-lapply(x$values,function(value){
      if(is.null(value))return(NULL)
      if(!is.character(value)||length(value)!=1L||is.na(value)||!is.finite(suppressWarnings(as.numeric(value))))stop('Invalid availability numeric value')
      as.numeric(value)
    })
    if(x$type%in%c('list','frame'))x$values<-lapply(x$values,unpack,depth=depth+1L)
    if(length(x$attributes))x$attributes<-lapply(x$attributes,unpack,depth=depth+1L)
    x
  }
  rival_snapshot_decode(unpack(node))
}
# Emit vetted primitive wire data in short finite steps. Large list-of-records
# snapshots otherwise spend ~150ms inside toJSON's recursive R dispatch.
player_availability_json_async <- function(wire) {
  promises::promise(function(resolve,reject){
    small<-function(value){
      pending<-list(value);remaining<-128L
      while(length(pending)){
        x<-pending[[length(pending)]];pending[[length(pending)]]<-NULL
        remaining<-remaining-length(x);if(remaining<0L)return(FALSE)
        if(is.list(x)&&length(x))pending<-c(pending,x)
      }
      TRUE
    }
    stack<-list(list(value=wire,literal=FALSE));chunks<-list();index<-0L
    step<-function(){tryCatch({
      started<-proc.time()[['elapsed']]
      while(length(stack)){
        task<-stack[[length(stack)]];stack[[length(stack)]]<<-NULL
        if(isTRUE(task$literal))text<-task$value else if(is.list(task$value)&&small(task$value))
          text<-as.character(jsonlite::toJSON(task$value,auto_unbox=TRUE,na='null',null='null',digits=NA))else if(is.list(task$value)){
          value<-task$value;labels<-names(value);object<-!is.null(labels)
          text<-if(object)'{'else'['
          stack[[length(stack)+1L]]<<-list(value=if(object)'}'else']',literal=TRUE)
          if(length(value))for(i in rev(seq_along(value))){
            stack[[length(stack)+1L]]<<-list(value=value[[i]],literal=FALSE)
            if(object)stack[[length(stack)+1L]]<<-list(value=paste0(as.character(jsonlite::toJSON(labels[i],auto_unbox=TRUE)),':'),literal=TRUE)
            if(i>1L)stack[[length(stack)+1L]]<<-list(value=',',literal=TRUE)
          }
        }else text<-as.character(jsonlite::toJSON(task$value,auto_unbox=TRUE,na='null',null='null',digits=NA))
        index<<-index+1L;chunks[[index]]<<-text
        if(proc.time()[['elapsed']]-started>=0.004)break
      }
      if(length(stack))later::later(step,0.001)else resolve(charToRaw(paste(unlist(chunks,use.names=FALSE),collapse='')))
    },error=reject)}
    later::later(step,0.001)
  })
}
player_availability_save <- function(data,context,kind,started_at,on_complete=NULL,deferred=FALSE) {
  scope<-player_availability_scope(context,kind)
  if(is.null(scope)||!player_availability_valid(data,kind))return(invisible(FALSE))
  returned_id<-if(kind=='summary')fm_scalar(data$data$id %||% data$data$`_id`,'')else ''
  if(kind=='summary'&&(!nzchar(returned_id)||!identical(returned_id,scope$player_id)))return(invisible(FALSE))
  if(kind=='team'&&!acquisition_capacity_observation_complete(data))return(invisible(FALSE))
  # Source metadata is explicit because the existing bounded primitive serializer
  # deliberately omits arbitrary R attributes.
  metadata<-attributes(data)[intersect(names(attributes(data)),c('observed_at','fetched_at','valid_until','source','fetch_status','complete','worker_completeness'))]
  body<-data
  if(is.data.frame(body))attributes(body)<-attributes(body)[intersect(names(attributes(body)),
    c('names','row.names','class','round_bonus_totals','point_rewards_incomplete','pending_bonus_types','award_payment_allocations','realized_trades'))]
  wire<-tryCatch(list(scope=scope,data=player_availability_encode(body),metadata=lapply(metadata,player_availability_encode)),error=function(e)NULL)
  if(is.null(wire))return(invisible(FALSE))
  enqueue<-function(raw){
  if(length(raw)>8L*1024L^2L){if(is.function(on_complete))on_complete(FALSE);return(invisible(FALSE))}
  bytes<-history_baseline_compress(raw)
  encoded<-openssl::base64_encode(bytes)
  if(nchar(encoded,type='bytes')>220L*1024L){if(is.function(on_complete))on_complete(FALSE);return(invisible(FALSE))}
  until<-if(kind=='team')data$valid_until else min(attr(data,'valid_until') %||% Inf,as.numeric(fm_time(attr(data,'observed_at')))+if(kind=='summary')60 else 20)
  payload<-list(kind='player_availability',version=1L,scope=scope,encoding='json-gzip-base64-v1',
    data=encoded,json_bytes=length(raw),sha256=history_baseline_hash(bytes),completeness='complete',
    source='futmondo',source_observed_at=rival_finance_stamp(attr(data,'observed_at')),
    valid_until=sprintf('%.17g',until),started_at=sprintf('%.17g',as.numeric(started_at)))
  row<-rival_finance_evidence(scope$account_id,scope$championship_id,scope$season,'eligibility',
    player_availability_id(scope),payload,attr(data,'observed_at'),competition_id=scope$competition_id)
  accepted<-defer_persistence('supabase_post_direct',list('rival_finance_evidence',list(row),
    'account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at',TRUE),context,on_complete)
  if(!isTRUE(accepted)&&is.function(on_complete))on_complete(FALSE)
  invisible(accepted)
  }
  if(!isTRUE(deferred))return(enqueue(charToRaw(as.character(jsonlite::toJSON(wire,auto_unbox=TRUE,na='null',null='null',digits=NA)))))
  promises::then(player_availability_json_async(wire),enqueue,function(error){if(is.function(on_complete))on_complete(FALSE);NULL})
  invisible(TRUE)
}

get_saved_player_availability <- function(context,kind) {
  scope<-player_availability_scope(context,kind);if(is.null(scope))return(NULL)
  epoch_scope<-player_availability_scope(context,'team')
  epoch_specs<-if(kind!='summary')setNames(list(scope),paste0(player_availability_id(scope),':epoch'))else
    setNames(list(epoch_scope,scope),c(paste0(player_availability_id(epoch_scope),':summary-epoch'),paste0(player_availability_id(scope),':epoch')))
  query<-list(account_id=paste0('eq.',scope$account_id),championship_id=paste0('eq.',scope$championship_id),
    competition_id=paste0('eq.',scope$competition_id),season=paste0('eq.',scope$season),evidence_type='eq.eligibility',
    source_id=paste0('in.(',paste(c(player_availability_id(scope),names(epoch_specs)),collapse=','),')'),
    select='account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at,payload',limit=3L)
  rows<-supabase_get('rival_finance_evidence_latest',query)
  tryCatch({
    if(!is.data.frame(rows)||!nrow(rows))return(NULL)
    records<-rival_evidence_decode(rows);saved<-NULL;invalidated<-NULL
    for(row in records){
      if(!all(vapply(c('account_id','championship_id','competition_id','season'),function(f)identical(fm_scalar(row[[f]]),scope[[f]]),logical(1)))||
         !identical(row$evidence_type,'eligibility'))return(NULL)
      if(row$source_id%in%names(epoch_specs)){
        p<-row$payload
        if(!is.list(p)||!identical(p$kind,'player_availability_epoch')||!identical(p$scope,epoch_specs[[row$source_id]])||
           !identical(p$applies_to,kind)||!is.character(p$invalidated_at)||length(p$invalidated_at)!=1L||
           !is.finite(suppressWarnings(as.numeric(p$invalidated_at))))return(NULL)
        invalidated<-max(c(invalidated,as.numeric(p$invalidated_at)))
      }else if(identical(row$source_id,player_availability_id(scope)))saved<-row
    }
    if(is.null(saved))return(NULL)
    p<-saved$payload
    if(!is.list(p)||!identical(p$kind,'player_availability')||!identical(as.integer(p$version),1L)||!identical(p$scope,scope)||
       !identical(p$encoding,'json-gzip-base64-v1')||!identical(p$completeness,'complete')||!identical(p$source,'futmondo')||
       !is.character(p$data)||length(p$data)!=1L||nchar(p$data,type='bytes')>220L*1024L||
       !is.numeric(p$json_bytes)||length(p$json_bytes)!=1L||!is.finite(p$json_bytes)||p$json_bytes<1||p$json_bytes>8L*1024L^2L||
       !is.character(p$started_at)||length(p$started_at)!=1L||!is.finite(suppressWarnings(as.numeric(p$started_at)))||as.numeric(p$started_at)>as.numeric(Sys.time())||
       (!is.null(invalidated)&&as.numeric(p$started_at)<invalidated))return(NULL)
    local<-shiny::isolate(player_availability_invalidated(context,kind))
    if(!is.null(local)&&as.numeric(p$started_at)<as.numeric(local))return(NULL)
    bytes<-openssl::base64_decode(p$data)
    if(!identical(history_baseline_hash(bytes),p$sha256))return(NULL)
    raw<-history_baseline_decompress(bytes,p$json_bytes)
    if(length(raw)!=p$json_bytes)return(NULL)
    wire<-jsonlite::fromJSON(rawToChar(raw),simplifyVector=FALSE)
    if(!is.list(wire)||!identical(wire$scope,scope))return(NULL)
    value<-player_availability_decode(wire$data)
    allowed<-c('observed_at','fetched_at','valid_until','source','fetch_status','complete','worker_completeness')
    if(length(setdiff(names(wire$metadata),allowed)))return(NULL)
    for(field in names(wire$metadata))attr(value,field)<-player_availability_decode(wire$metadata[[field]])
    returned_id<-if(kind=='summary')fm_scalar(value$data$id %||% value$data$`_id`,'')else ''
    if(kind=='summary'&&(!nzchar(returned_id)||!identical(returned_id,scope$player_id)))return(NULL)
    if(kind=='team'&&!acquisition_capacity_observation_complete(value))return(NULL)
    if(!player_availability_valid(value,kind)||
       abs(as.numeric(fm_time(p$source_observed_at))-as.numeric(fm_time(attr(value,'observed_at'))))>0.00001)return(NULL)
    deadline<-if(kind=='team')value$valid_until else min(attr(value,'valid_until') %||% Inf,as.numeric(fm_time(attr(value,'observed_at')))+if(kind=='summary')60 else 20)
    if(!identical(as.numeric(p$valid_until),as.numeric(deadline)))return(NULL)
    if(kind!='team')attr(value,'valid_until')<-deadline
    attr(value,'availability_from_saved')<-TRUE
    value
  },error=function(e)NULL)
}
# Invalidation epochs use the same private store, so a process restart cannot
# restore a snapshot invalidated by an acknowledged manual action.
player_availability_invalidate <- function(context,resources=NULL,pattern=NULL) {
  if(isTRUE(getOption('futmondo.worker_helper',FALSE)))return(invisible(FALSE))
  if(nzchar(fm_scalar(context$session_id,''))&&is.null(resources)&&is.null(pattern))return(invisible(FALSE))
  scope<-player_availability_scope(context,'team')
  if(is.null(scope)){
    # Root refresh/logout may specify an account/league without a team. Expand
    # only already known scopes, never enumerate or acquire application data.
    for(key in ls(.player_availability_epochs)){
      known<-.player_availability_epochs[[key]]$scope
      if(all(vapply(c('account_id','championship_id','user_team_id'),function(f)
        !nzchar(fm_scalar(context[[f]],''))||identical(context[[f]],known[[f]]),logical(1)))){
        expanded<-known;expanded$player_id<-context$player_id %||% '';expanded$session_id<-context$session_id %||% ''
        player_availability_invalidate(expanded,resources,pattern)
      }
    }
    return(invisible(TRUE))
  }
  team_labels<-c('acq_capacity','card_capacity',paste0('team_info_',context$championship_id,'_',context$user_team_id),
    paste0('roster_',context$championship_id,'_',context$user_team_id),paste0('market_',context$championship_id,'_',context$user_team_id),paste0('pressroom_',context$championship_id))
  summary_label<-paste0('player_summary_',context$account_id,'_',context$championship_id,'_',context$user_team_id,'_',context$player_id)
  matches<-function(x)(is.null(resources)||x%in%resources)&&(is.null(pattern)||grepl(pattern,x))
  team<-any(vapply(team_labels,matches,logical(1)))
  summary<-matches(summary_label)
  listings<-any(vapply(c('card_listings',paste0('my_market_players_',context$championship_id,'_',context$user_team_id)),matches,logical(1)))
  if(!team&&!summary&&!listings)return(invisible(FALSE))
  epoch<-player_availability_epoch(context);stamp<-Sys.time()
  if(team)epoch$team<-stamp
  if(listings)epoch$listings<-stamp
  if(summary){if(nzchar(fm_scalar(context$player_id,'')))epoch$players[[context$player_id]]<-stamp else epoch$summary<-stamp}
  if(length(epoch$players)>64L){epoch$summary<-stamp;epoch$players<-list()}
  epoch$at<-stamp;.player_availability_epochs[[player_availability_id(scope)]]<-epoch
  shiny::isolate(epoch$revision(epoch$revision()+1L))
  write_epoch<-function(epoch_scope,id,kind){
    payload<-list(kind='player_availability_epoch',scope=epoch_scope,applies_to=kind,
      invalidated_at=sprintf('%.17g',as.numeric(stamp)))
    row<-rival_finance_evidence(scope$account_id,scope$championship_id,scope$season,'eligibility',id,payload,stamp,competition_id=scope$competition_id)
    defer_persistence('supabase_post_direct',list('rival_finance_evidence',list(row),
      'account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at',TRUE),context)
  }
  if(team)write_epoch(scope,paste0(player_availability_id(scope),':epoch'),'team')
  if(listings){listing_scope<-player_availability_scope(context,'listings');write_epoch(listing_scope,paste0(player_availability_id(listing_scope),':epoch'),'listings')}
  if(summary){
    target<-if(nzchar(fm_scalar(context$player_id,'')))player_availability_scope(context,'summary')else scope
    id<-paste0(player_availability_id(target),if(nzchar(target$player_id))':epoch'else':summary-epoch')
    write_epoch(target,id,'summary')
  }
  invisible(TRUE)
}
player_availability_publish <- function(entry,entries,key,data,error=NULL) {
  bytes<-if(is.null(data))0 else as.numeric(object.size(data))
  if(is.null(error)&&bytes>8L*1024L^2L)error<-resource_failure(category='read_budget')
  retained<-function()sum(vapply(mget(ls(entries),entries),function(x)x$bytes %||% 0,numeric(1)))-entry$bytes
  if(is.null(error)&&retained()+bytes>16L*1024L^2L){
    candidates<-setdiff(ls(entries),key)
    candidates<-candidates[vapply(mget(candidates,entries),function(x)!isTRUE(x$pending)&&!isTRUE(x$saving)&&!length(x$consumers),logical(1))]
    if(length(candidates))candidates<-candidates[order(vapply(mget(candidates,entries),function(x)as.numeric(x$at),numeric(1)))]
    for(candidate in candidates){
      victim<-entries[[candidate]];victim$value(resource_state(status='loading'));victim$bytes<-0
      rm(list=candidate,envir=entries)
      if(retained()+bytes<=16L*1024L^2L)break
    }
    if(retained()+bytes>16L*1024L^2L)error<-resource_failure(category='read_budget')
  }
  entry$bytes<-if(is.null(error))bytes else 0
  entry$value(resource_state(if(is.null(error))data else NULL,error=error))
  invisible(entry$value())
}
# Root-session entries share pending and complete team reads across card modules.
# A consumer's selection never owns the team request's cancellation predicate.
player_availability_source <- function(session,kind,context,login,refresh=function()0,active=function()TRUE,
  stored=function()NULL,current_context=context,mode=function()FALSE,opening=function()NULL,intent=function()'display') {
  root<-session$rootScope()
  if(is.null(root$userData$player_availability))root$userData$player_availability<-new.env(parent=emptyenv())
  entries<-root$userData$player_availability;handle<-shiny::reactiveVal(NULL)
  consumer<-paste0(session$ns(''),kind)
  attach<-function(entry){
    previous<-handle()
    if(!is.null(previous)&&!identical(previous,entry))previous$consumers<-setdiff(previous$consumers,consumer)
    if(!is.null(entry))entry$consumers<-union(entry$consumers,consumer)
    handle(entry)
  }
  closed<-FALSE;last_demand<-NULL
  session$onSessionEnded(function(){closed<<-TRUE})
  acquire<-function(force=FALSE,allow_start=TRUE,manual=FALSE){shiny::isolate({
    ctx<-context();if(is.null(ctx)||!isTRUE(active()))return(invisible(NULL))
    scope<-player_availability_scope(ctx,kind);if(is.null(scope))return(invisible(NULL))
    epoch<-player_availability_invalidated(ctx,kind)
    identity<-list(scope=scope,refresh=refresh(),mode=mode(),epoch=epoch)
    key<-as.character(openssl::sha256(serialize(identity,NULL,version=2)))
    entry<-entries[[key]];fresh_entry<-is.null(entry)
    if(is.null(entry)){
      keys<-ls(entries)
      if(length(keys)>=64L){eligible<-keys[vapply(mget(keys,entries),function(x)!isTRUE(x$pending)&&!isTRUE(x$saving)&&!length(x$consumers),logical(1))]
        if(!length(eligible)){
          blocked<-new.env(parent=emptyenv());blocked$consumers<-character();blocked$pending<-FALSE;blocked$identity<-identity
          blocked$value<-shiny::reactiveVal(resource_state(error=resource_failure(category='read_budget')))
          attach(blocked);return(invisible(NULL))
        }
        ages<-vapply(mget(eligible,entries),function(x)as.numeric(x$at),numeric(1));rm(list=eligible[which.min(ages)],envir=entries)}
      entry<-new.env(parent=emptyenv());entry$value<-shiny::reactiveVal(resource_state(status='loading'));entry$pending<-FALSE
      entry$identity<-identity;entry$attempted<-FALSE;entry$durable<-FALSE;entry$saving<-FALSE;entry$generation<-0L;entry$bytes<-0;entry$consumers<-character();entry$at<-Sys.time();entries[[key]]<-entry
    }
    attach(entry);entry$at<-Sys.time()
    if(fm_worker_configured()&&isTRUE(fm_worker_mode(ctx$account_id))&&!manual&&!identical(intent(),'manual_action')){
      handler<-switch(kind,team='get_team_acquisition_capacity',summary='get_player_summary',listings='get_my_market_players')
      args<-list(championship_id=ctx$championship_id,user_team_id=ctx$user_team_id)
      if(kind=='summary')args$player_id<-ctx$player_id
      descriptor<-fm_worker_descriptor(handler,args,ctx)
      fm_worker_demand(descriptor,root$token,function()shiny::isolate(!closed&&isTRUE(active())&&identical(context(),ctx)))
      saved<-fm_worker_get(ctx$account_id,descriptor$resource_key)
      old<-entry$value()
      if(!is.null(saved$data)){
        newer<-is.null(old$data)||as.numeric(fm_time(attr(saved$data,'observed_at')))>=as.numeric(fm_time(old$observed_at))
        if(isTRUE(newer)){
          attr(saved$data,'availability_from_saved')<-TRUE
          player_availability_publish(entry,entries,key,saved$data)
          entry$verified<-player_availability_valid(saved$data,kind);entry$durable<-TRUE
        }
      }else if(is.null(old$data)&&!isTRUE(entry$pending))entry$value(resource_state(status='unverified'))
      return(invisible(NULL))
    }
    schedule_save<-function(data,save_context,started_at){
      if(isTRUE(entry$saving))return(invisible(TRUE))
      entry$saving<-TRUE;g<-entry$generation
      complete<-function(ok){if(identical(entry$generation,g)){entry$saving<-FALSE;entry$durable<-isTRUE(ok)}}
      # This is bounded synchronous encoding followed by cooperative JSON work,
      # not a claim that a timer turns CPU work into a background process.
      root$onFlushed(function(){later::later(function(){
        accepted<-tryCatch(player_availability_save(data,save_context,kind,started_at,complete,deferred=TRUE),error=function(error)FALSE)
        if(!isTRUE(accepted))complete(FALSE)
      },0)},once=TRUE)
      invisible(TRUE)
    }
    # A first card opened after explicit refresh cannot restore pre-refresh DB
    # observations. Other cards still join/reuse this revision's shared entry.
    revisions<-suppressWarnings(as.numeric(strsplit(paste(refresh(),collapse=':'),':',fixed=TRUE)[[1]]))
    if(fresh_entry&&any(is.finite(revisions)&revisions>0))force<-TRUE
    if(isTRUE(entry$pending))return(invisible(NULL))
    after_epoch<-function(data){
      stamp<-fm_time(attr(data,'observed_at'))
      is.null(epoch)||(length(stamp)==1L&&!is.na(stamp)&&as.numeric(stamp)>=as.numeric(epoch))
    }
    # Retain successful card observations for this session/scope. This marker
    # supports display/editor entry only: original deadlines and actual-time
    # transaction preflights remain authoritative for every purchase.
    if(!force&&kind%in%c('team','summary')&&isTRUE(entry$verified)){
      if(!entry$durable&&!isTRUE(entry$saving)&&player_availability_valid(entry$value()$data,kind))
        schedule_save(entry$value()$data,ctx,entry$started_at %||% Sys.time())
      return(invisible(NULL))
    }
    old<-entry$value();saved<-if(force)NULL else stored()
    if(!is.null(saved$data)&&kind!='team'){
      stamp<-fm_time(attr(saved$data,'observed_at'))
      if(length(stamp)==1L&&!is.na(stamp))attr(saved$data,'valid_until')<-min(attr(saved$data,'valid_until') %||% Inf,as.numeric(stamp)+if(kind=='summary')60 else 20)
    }
    if(!is.null(saved$data)&&!after_epoch(saved$data))saved<-NULL
    saved_stamp<-as.numeric(saved$observed_at %||% NA_real_)
    old_stamp<-as.numeric(old$observed_at %||% NA_real_)
    if(!is.null(saved$data)&&player_availability_valid(saved$data,kind)&&length(saved_stamp)==1L&&is.finite(saved_stamp)&&
       (length(old_stamp)!=1L||!is.finite(old_stamp)||saved_stamp>old_stamp)){
      player_availability_publish(entry,entries,key,saved$data,saved$error);old<-entry$value()
      if(!is.null(old$error)&&identical(old$error$category,"read_budget"))return(invisible(NULL))
    }
    if(!force&&player_availability_valid(old$data,kind)){
      entry$verified<-TRUE
      resource_timing('availability_reuse',Sys.time())
      if(isTRUE(attr(old$data,'availability_from_saved')))entry$durable<-TRUE
      if(!entry$durable)schedule_save(old$data,ctx,entry$started_at %||% Sys.time())
      return(invisible(NULL))
    }
    if(!is.null(old$data)&&!resource_snapshot_unexpired(old$data)&&!identical(old$status,'stale')){
      old<-resource_state(observed_data(old$data,status='stale'),status='stale',error=old$error);entry$value(old)
    }
    # An error settles once; redraws never become an automatic retry loop.
    if(!force&&entry$attempted&&(!allow_start||!is.null(old$error)))return(invisible(NULL))
    entry$generation<-entry$generation+1L;generation<-entry$generation;entry$saving<-FALSE;entry$verified<-FALSE
    entry$pending<-TRUE;entry$attempted<-TRUE;entry$started_at<-Sys.time();entry$completed_at<-NULL;entry$value(resource_state(status='loading'))
    captured<-ctx;auth<-login();captured$session_id<-root$token
    current<-function()shiny::isolate({
      if(root$isClosed())return(FALSE)
      now<-tryCatch(current_context(),error=function(e)NULL);if(is.null(now))return(FALSE)
      fields<-c('account_id','championship_id','user_team_id','competition_id','season','scoring_version')
      !root$isClosed()&&identical(entry$generation,generation)&&
        all(vapply(fields,function(f)identical(now[[f]],ctx[[f]]),logical(1)))&&
        identical(mode(),identity$mode)&&identical(refresh(),identity$refresh)&&
        identical(player_availability_invalidated(ctx,kind),epoch)
    })
    finish<-function(data=NULL,error=NULL){shiny::isolate({
      if(!current()){entry$pending<-FALSE;return(NULL)}
      entry$pending<-FALSE;entry$completed_at<-Sys.time()
      if(is.null(error)&&is.null(data))error<-resource_failure(resource=paste0('availability_',kind),category='schema')
      if(is.null(error)&&kind=='team'&&(!is.numeric(data$valid_until)||length(data$valid_until)!=1L||!is.finite(data$valid_until)))error<-resource_failure(resource='get_team_acquisition_capacity',category='schema')
      if(!is.null(data))data<-observed_data(data,observed_at=attr(data,'observed_at') %||% Sys.time())
      if(!is.null(data)&&kind!='team'){
        attr(data,'valid_until')<-min(attr(data,'valid_until') %||% Inf,as.numeric(fm_time(attr(data,'observed_at')))+if(kind=='summary')60 else 20)
        if(!resource_snapshot_unexpired(data))attr(data,'fetch_status')<-'stale'
      }
      returned_id<-if(kind=='summary')fm_scalar(data$data$id %||% data$data$`_id`,'')else ''
      if(is.null(error)&&kind=='summary'&&nzchar(returned_id)&&!identical(returned_id,ctx$player_id))error<-resource_failure(resource='get_player_summary',category='schema')
      entry$verified<-is.null(error)&&player_availability_valid(data,kind)
      published<-player_availability_publish(entry,entries,key,data,error);error<-published$error
      if(!is.null(error))entry$verified<-FALSE
      resource_timing(paste0('availability_',kind),entry$started_at)
      if(is.null(error)&&player_availability_valid(data,kind)){
        entry$durable<-isTRUE(attr(data,'availability_from_saved'))
        if(!entry$durable)schedule_save(data,captured,entry$started_at)
      }
      NULL
    })}
    live<-function(){
      if(!current())return(promises::promise_reject(structure(list(message='Session request cancelled',call=NULL),class=c('fm_async_cancelled','error','condition'))))
      handler<-switch(kind,team='get_team_acquisition_capacity',summary='get_player_summary',listings='get_my_market_players')
      args<-list(login=auth,championship_id=ctx$championship_id,user_team_id=ctx$user_team_id)
      if(kind=='summary')args$player_id<-ctx$player_id
      tryCatch(async_adapter_fetch(handler,args,context=captured,is_current=current,priority='interactive',owner=root$token,intent='manual_action'),error=function(error)promises::promise_reject(error))
    }
    if(!is.null(saved)&&player_availability_valid(saved$data,kind)){
      finish(saved$data);return(invisible(NULL))
    }
    # A native successful calculation/summary avoids a database round trip.
    native_resource<-if(kind=='team')paste0('acq_capacity_',ctx$account_id,'_',ctx$championship_id,'_',ctx$user_team_id)else if(kind=='listings')paste0('my_market_players_',ctx$championship_id,'_',ctx$user_team_id)else
      paste0('player_summary_',ctx$account_id,'_',ctx$championship_id,'_',ctx$user_team_id,'_',ctx$player_id)
    native<-api_cache_env[[api_cache_key(native_resource,context=ctx)]]
    if(!force&&!is.null(native$data)&&after_epoch(native$data)&&player_availability_valid(native$data,kind)){
      finish(native$data);return(invisible(NULL))
    }
    db_started<-Sys.time()
    request<-if(force||!nzchar(get_sb_url())||!nzchar(get_sb_key()))live()else promises::then(tryCatch(async_adapter_fetch('get_saved_player_availability',
      list(context=captured,kind=kind),context=captured,is_current=current,priority='interactive',owner=root$token),error=function(error)promises::promise_reject(error)),function(data){
        resource_timing('availability_database',db_started)
        if(player_availability_valid(data,kind))data else live()
      },function(error){if(inherits(error,'fm_async_cancelled'))stop(error);live()})
    promises::then(request,function(data)finish(data),function(error)finish(error=error))
    invisible(NULL)
  })}
  # Derive scalar demand projections so metadata-only row redraws cannot
  # restart a failed/partial read. Worker publications may update the display,
  # but only opening, context, explicit refresh or retry starts acquisition.
  active_state<-shiny::reactive(isTRUE(active()))
  opening_state<-shiny::reactive(opening())
  saved_state<-shiny::reactive(stored())
  demand<-shiny::observeEvent(list(context(),refresh(),active_state(),mode(),opening_state(),saved_state(),{
    ctx<-context();if(!is.null(ctx))player_availability_invalidated(ctx,kind)
  }),{
    if(closed)return()
    if(!active_state()||is.null(context())){shiny::isolate(attach(NULL));last_demand<<-NULL;return()}
    ctx<-context()
    identity<-list(scope=player_availability_scope(ctx,kind),refresh=refresh(),mode=mode(),
      opening=opening_state(),epoch=player_availability_invalidated(ctx,kind))
    allow_start<-!identical(identity,last_demand)
    force<-!is.null(last_demand)&&!identical(identity$refresh,last_demand$refresh)
    last_demand<<-identity
    acquire(force=force,allow_start=allow_start)
  },ignoreNULL=FALSE,priority=100,domain=session)
  session$onSessionEnded(function()demand$destroy())
  selected_entry<-shiny::reactive({
    ctx<-tryCatch(context(),error=function(e)NULL);if(is.null(ctx)||!active_state())return(NULL)
    scope<-player_availability_scope(ctx,kind);if(is.null(scope))return(NULL)
    identity<-list(scope=scope,refresh=refresh(),mode=mode(),epoch=player_availability_invalidated(ctx,kind))
    entry<-handle();if(is.null(entry)||!identical(entry$identity,identity))NULL else entry
  })
  value<-shiny::reactive({
    failure<-NULL
    ctx<-tryCatch(context(),error=function(e){failure<<-resource_failure(e);NULL})
    if(!is.null(failure))return(resource_state(error=failure))
    if(!is.null(ctx)&&is.null(player_availability_scope(ctx,kind)))return(resource_state(error=resource_failure(category='schema')))
    entry<-selected_entry()
    if(is.null(entry))return(resource_state(status='loading'))
    state<-entry$value();state$session_verified<-isTRUE(entry$verified)
    state
  })
  attr(value,'retry')<-function()acquire(TRUE,manual=TRUE)
  # Editor demand first attaches an existing entry, then rechecks actual source
  # freshness. A dormant accessor's loading shell must not force a duplicate.
  attr(value,'ensure')<-function()shiny::isolate({
    acquire(FALSE,manual=TRUE)
    entry<-selected_entry()
    if(!is.null(entry)&&!isTRUE(entry$pending)&&!player_availability_valid(entry$value()$data,kind))acquire(TRUE,manual=TRUE)
    invisible(NULL)
  })
  attr(value,'verification')<-shiny::reactive({entry<-selected_entry();if(is.null(entry))return(NULL)
    list(pending=entry$pending,started_at=entry$started_at,completed_at=entry$completed_at,generation=entry$generation,state=entry$value())})
  value
}
