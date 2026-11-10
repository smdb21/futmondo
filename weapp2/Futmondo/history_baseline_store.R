# Durable complete transfer baselines. Only normalized factual data is serialized;
# authentication and session state never enter the snapshot or its database key.
.history_baseline_ack <- new.env(parent=emptyenv())
.history_baseline_limits <- list(rows=20000L, json_bytes=16L*1024L^2L,
  payload_bytes=900L*1024L, memory_bytes=32L*1024L^2L, acknowledgements=256L)

history_baseline_scope <- function(resource,context) {
  if(exists("transaction_history_context",mode="function"))context<-transaction_history_context(context)
  context$competition_id <- fm_scalar(context$competition_id,"unknown")
  fields <- c("account_id","championship_id","competition_id","season","scoring_version")
  if(!is.list(context)||!all(fields%in%names(context)))return(NULL)
  values <- lapply(context[fields],function(x)fm_scalar(x,""))
  if(any(!vapply(values,function(x)nzchar(x)&&nchar(x,type="bytes")<=256L,logical(1))))return(NULL)
  resource <- fm_scalar(resource,"")
  if(!resource%in%c(paste0("pressroom_",values$championship_id),
      paste0("pressroom_locker_payments_",values$championship_id)))return(NULL)
  c(values,list(resource=resource))
}
history_baseline_hash <- function(bytes) unname(unclass(as.character(openssl::sha256(bytes))))
history_baseline_key <- function(scope) history_baseline_hash(serialize(scope,NULL,version=2))
history_baseline_stamp <- function(value,optional=FALSE) {
  if(is.null(value)&&optional)return(NULL)
  stamp <- fm_time(value)
  if(length(stamp)!=1L||is.na(stamp)||stamp>Sys.time()+60)return(NA_character_)
  micros <- round(as.numeric(stamp)*1e6)
  seconds <- floor(micros/1e6)
  paste0(format(as.POSIXct(seconds,origin="1970-01-01",tz="UTC"),"%Y-%m-%dT%H:%M:%S",tz="UTC"),
    sprintf(".%06dZ",as.integer(micros-seconds*1e6)))
}
# Native bids are a JSON list. Reject accidental authentication fields and
# executable/custom objects recursively instead of serializing arbitrary state.
history_baseline_safe_value <- function(value,depth=0L) {
  if(depth>12L||is.environment(value)||is.function(value)||is.language(value))return(FALSE)
  if(!is.null(names(value))&&any(grepl("token|password|passwd|authorization|cookie|secret|session|login|api.?key|(^|[_.-])(pwd|header)([_.-]|$)",
      names(value),ignore.case=TRUE)))return(FALSE)
  if(!is.null(attr(value,"class"))&&!all(class(value)%in%c("data.frame","AsIs","list","character","logical","numeric","integer")))return(FALSE)
  if(is.list(value))return(all(vapply(value,history_baseline_safe_value,logical(1),depth=depth+1L)))
  is.null(value)||is.character(value)||is.logical(value)||is.numeric(value)
}
history_baseline_clean_frame <- function(data,resource,context) {
  if(!is.data.frame(data)||nrow(data)>.history_baseline_limits$rows||
     as.numeric(object.size(data))>.history_baseline_limits$memory_bytes)return(NULL)
  columns <- c("id","created","player_id","player_name","player_team_name","player_team_slug",
    "buyer_team_id","buyer_team_name","seller_team_id","seller_team_name","price","bids",
    "bids_available","is_clause","locker_event_id","delivery_at")
  if(anyDuplicated(names(data))||any(!names(data)%in%columns))return(NULL)
  locker <- identical(resource,paste0("pressroom_locker_payments_",context$championship_id))
  required <- if(locker)c("id","created","locker_event_id","player_name","buyer_team_name","seller_team_name","price")else
    c("id","created","player_id","buyer_team_id","seller_team_id","price")
  if(nrow(data)&&!all(required%in%names(data)))return(NULL)
  # Reconstruct the frame to drop unrelated custom attributes before inspecting
  # nested source bid rows. Source metadata is stored separately below.
  clean <- as.list(data);attributes(clean) <- list(names=names(data),row.names=.set_row_names(nrow(data)),class="data.frame")
  if(!history_baseline_safe_value(clean))return(NULL)
  for(field in setdiff(names(clean),c("price","bids","bids_available","is_clause")))
    if(!is.character(clean[[field]]))return(NULL)
  if("price"%in%names(clean)&&!is.numeric(clean$price))return(NULL)
  for(field in intersect(c("bids_available","is_clause"),names(clean)))if(!is.logical(clean[[field]]))return(NULL)
  if("bids"%in%names(clean)&&!is.list(clean$bids))return(NULL)
  if(nrow(clean)&&(anyNA(clean$id)||any(!nzchar(clean$id))||anyDuplicated(clean$id)||
      anyNA(fm_time(clean$created))||any(!is.finite(clean$price))))return(NULL)
  clean
}
# Ordinary JSON stays fast for large histories and never reconstructs arbitrary
# R objects. Column descriptors preserve empty frames, NA scalars and list bids.
history_baseline_json <- function(snapshot) {
  semantic <- attr(snapshot,"semantic_json")
  if(!is.null(semantic)&&all(c("observed_at","fingerprint")%in%names(snapshot))){
    suffix <- as.character(jsonlite::toJSON(snapshot[c("observed_at","fingerprint")],auto_unbox=TRUE))
    return(paste0(substr(semantic,1L,nchar(semantic)-1L),",",substring(suffix,2L)))
  }
  frame <- snapshot$frame
  snapshot$frame <- list(columns=I(names(frame)),types=I(vapply(frame,typeof,character(1))),
    as_is=I(vapply(frame,inherits,logical(1),what="AsIs")),count=nrow(frame),
    values=lapply(frame,function(column)I(unclass(column))))
  as.character(jsonlite::toJSON(snapshot,auto_unbox=TRUE,na="null",null="null",digits=NA))
}
history_baseline_restore_frame <- function(wire) {
  if(!is.list(wire)||!identical(names(wire),c("columns","types","as_is","count","values")))return(NULL)
  columns <- unlist(wire$columns,use.names=FALSE);types <- unlist(wire$types,use.names=FALSE)
  as_is <- unlist(wire$as_is,use.names=FALSE);count<-wire$count;values <- wire$values
  if(is.null(columns))columns<-character()
  if(is.null(types))types<-character()
  if(is.null(as_is))as_is<-logical()
  if(!is.character(columns)||anyNA(columns)||anyDuplicated(columns)||!is.character(types)||
     !is.logical(as_is)||anyNA(as_is)||length(columns)!=length(types)||length(columns)!=length(as_is)||
     any(!types%in%c("character","integer","double","logical","list"))||!is.list(values)||
     length(values)!=length(columns)||!is.numeric(count)||length(count)!=1L||is.na(count)||
     count<0||count!=floor(count)||count>.history_baseline_limits$rows)return(NULL)
  if(length(columns)&&!identical(names(values),columns))return(NULL)
  result <- setNames(vector("list",length(columns)),columns)
  for(j in seq_along(columns)){
    entries <- values[[j]]
    if(!is.list(entries)||length(entries)!=count)return(NULL)
    if(types[j]=="list")column<-entries else {
      valid <- vapply(entries,function(value)is.null(value)||(length(value)==1L&&
        switch(types[j],character=is.character(value),integer=is.numeric(value)&&is.finite(value)&&value==floor(value),
          double=is.numeric(value),logical=is.logical(value))),logical(1))
      if(!all(valid))return(NULL)
      default <- switch(types[j],character=NA_character_,integer=NA_integer_,double=NA_real_,logical=NA)
      column <- vapply(entries,function(value)if(is.null(value))default else
        switch(types[j],character=as.character(value),integer=as.integer(value),double=as.numeric(value),logical=as.logical(value)),default)
    }
    if(as_is[j])column<-I(column)
    result[[j]] <- column
  }
  attributes(result)<-list(names=columns,row.names=.set_row_names(as.integer(count)),class="data.frame")
  result
}
history_baseline_snapshot <- function(resource,data,context) {
  scope <- history_baseline_scope(resource,context)
  if(is.null(scope)||!identical(attr(data,"fetch_status"),"ok"))return(NULL)
  frame <- history_baseline_clean_frame(data,resource,context)
  if(is.null(frame))return(NULL)
  full <- history_baseline_stamp(attr(data,"full_history_at"))
  latest <- history_baseline_stamp(attr(data,"source_latest_at"),optional=TRUE)
  observed <- history_baseline_stamp(attr(data,"observed_at"))
  if(anyNA(c(full,latest,observed)))return(NULL)
  # A nonempty pressroom must retain its raw source high watermark. Locker
  # payments may be empty while non-clause source news supplies this timestamp.
  if(nrow(frame)&&is.null(latest))return(NULL)
  semantic <- list(format=2L,scope=scope,frame=frame,full_history_at=full,source_latest_at=latest)
  json <- history_baseline_json(semantic)
  fingerprint <- history_baseline_hash(charToRaw(json))
  snapshot <- list(format=2L,scope=scope,frame=frame,full_history_at=full,source_latest_at=latest,
    observed_at=observed,fingerprint=fingerprint)
  attr(snapshot,"semantic_json") <- json
  snapshot
}
# Base R's in-memory zlib decompressor cannot enforce an output bound. Emit
# standard gzip once on changed saves, then read through a capped connection.
history_baseline_compress <- function(bytes) {
  path <- tempfile("futmondo-history-baseline-")
  on.exit(unlink(path),add=TRUE)
  if(!file.create(path)||!Sys.chmod(path,"0600"))stop("History snapshot compression unavailable")
  connection <- gzfile(path,"wb",compression=6L)
  tryCatch(writeBin(bytes,connection),finally=close(connection))
  readBin(path,"raw",n=.history_baseline_limits$payload_bytes+1L)
}
history_baseline_decompress <- function(bytes,size) {
  connection <- gzcon(rawConnection(bytes),allowNonCompressed=FALSE)
  on.exit(close(connection),add=TRUE)
  withCallingHandlers(readBin(connection,"raw",n=as.integer(size)+1L),warning=function(w)stop("Invalid history snapshot compression"))
}
history_baseline_acknowledge <- function(key,fingerprint) {
  .history_baseline_ack[[key]] <- list(fingerprint=fingerprint,time=Sys.time())
  keys <- ls(.history_baseline_ack)
  if(length(keys)>.history_baseline_limits$acknowledgements){
    stamps <- vapply(mget(keys,.history_baseline_ack),function(x)as.numeric(x$time),numeric(1))
    rm(list=keys[order(stamps)[seq_len(length(keys)-.history_baseline_limits$acknowledgements)]],envir=.history_baseline_ack)
  }
  invisible(TRUE)
}

read_persisted_history_baseline <- function(resource,context) {
  scope <- history_baseline_scope(resource,context)
  if(is.null(scope))return(NULL)
  # This ordinary supabase_get call is intercepted by async_adapter_fetch.
  # An absent schema/row is an optimization miss, never proof of empty history.
  query <- lapply(scope,function(value)paste0("eq.",value))
  query$select <- "account_id,championship_id,competition_id,season,scoring_version,resource,encoding,payload,payload_sha256,json_bytes,snapshot_hash,observed_at"
  query$limit <- 1L
  rows <- tryCatch(supabase_get("complete_history_baselines",query),error=function(e)NULL)
  if(!is.data.frame(rows)||nrow(rows)!=1L||!all(c(names(scope),"encoding","payload","payload_sha256","json_bytes","snapshot_hash","observed_at")%in%names(rows)))return(NULL)
  tryCatch({
    if(!all(vapply(names(scope),function(field)identical(as.character(rows[[field]][1]),scope[[field]]),logical(1)))||
       !identical(as.character(rows$encoding[1]),"json-gzip-base64-v1"))return(NULL)
    encoded <- as.character(rows$payload[1]);size <- suppressWarnings(as.numeric(rows$json_bytes[1]))
    if(is.na(encoded)||nchar(encoded,type="bytes")>.history_baseline_limits$payload_bytes||
       length(size)!=1L||!is.finite(size)||size<1||size>.history_baseline_limits$json_bytes)return(NULL)
    compressed <- openssl::base64_decode(encoded)
    if(!identical(history_baseline_hash(compressed),as.character(rows$payload_sha256[1])))return(NULL)
    raw <- history_baseline_decompress(compressed,size)
    if(length(raw)!=size||length(raw)>.history_baseline_limits$json_bytes)return(NULL)
    snapshot <- jsonlite::fromJSON(rawToChar(raw),simplifyVector=FALSE)
    if(!is.list(snapshot)||!identical(names(snapshot),c("format","scope","frame","full_history_at","source_latest_at","observed_at","fingerprint"))||
       !identical(as.integer(snapshot$format),2L)||
       !identical(snapshot$scope,scope))return(NULL)
    data <- history_baseline_restore_frame(snapshot$frame)
    if(!is.data.frame(data))return(NULL)
    attr(data,"fetch_status") <- "ok"
    attr(data,"full_history_at") <- fm_time(snapshot$full_history_at)
    attr(data,"source_latest_at") <- if(is.null(snapshot$source_latest_at))NULL else fm_time(snapshot$source_latest_at)
    attr(data,"observed_at") <- fm_time(snapshot$observed_at)
    valid <- history_baseline_snapshot(resource,data,context)
    if(is.null(valid)||!identical(valid$fingerprint,snapshot$fingerprint)||
       !identical(valid$fingerprint,as.character(rows$snapshot_hash[1]))||
       !identical(valid$observed_at,history_baseline_stamp(rows$observed_at[1])))return(NULL)
    history_baseline_acknowledge(history_baseline_key(scope),valid$fingerprint)
    attr(data,"source") <- "futmondo"
    data
  },error=function(e)NULL)
}

history_baseline_ack_callback <- function(key,fingerprint) {
  force(key);force(fingerprint)
  function(ok){if(isTRUE(ok))history_baseline_acknowledge(key,fingerprint);invisible(NULL)}
}

persist_completed_history_baseline <- function(resource,data,context) {
  snapshot <- history_baseline_snapshot(resource,data,context)
  if(is.null(snapshot))return(invisible(FALSE))
  key <- history_baseline_key(snapshot$scope)
  if(identical(.history_baseline_ack[[key]]$fingerprint,snapshot$fingerprint))return(invisible(TRUE))
  json <- charToRaw(history_baseline_json(snapshot))
  if(length(json)>.history_baseline_limits$json_bytes)return(invisible(FALSE))
  compressed <- history_baseline_compress(json)
  encoded <- openssl::base64_encode(compressed)
  if(nchar(encoded,type="bytes")>.history_baseline_limits$payload_bytes)return(invisible(FALSE))
  payload <- c(snapshot$scope,list(encoding="json-gzip-base64-v1",payload=encoded,
    payload_sha256=history_baseline_hash(compressed),json_bytes=length(json),
    snapshot_hash=snapshot$fingerprint,observed_at=snapshot$observed_at))
  # One atomic upsert contains both the complete data and its completion marker.
  # Only successful acknowledgement suppresses a later identical save attempt.
  args <- list("complete_history_baselines",payload,
    "account_id,championship_id,competition_id,season,scoring_version,resource",FALSE)
  if(as.numeric(object.size(args))>1024L^2L)return(invisible(FALSE))
  defer_persistence("supabase_post_direct",args,context,
    on_complete=history_baseline_ack_callback(key,snapshot$fingerprint))
}
