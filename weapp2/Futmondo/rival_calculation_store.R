# Derived results are an optimization, never financial evidence. The existing
# server-only evidence table supplies revision ordering and the bounded queue.
.rival_snapshot_limits <- list(json=8L*1024L^2L,payload=220L*1024L,teams=100L,rows=20000L)
rival_snapshot_scope <- function(context) {
  if(!is.list(context))return(NULL)
  metadata<-if(exists('league_context_metadata',mode='function'))league_context_metadata(context$championship_id,context$account_id)else NULL
  context$competition_id<-fm_scalar(context$competition_id,fm_scalar(metadata$competition_id,'unknown'))
  context$scoring_version<-fm_scalar(context$scoring_version,fm_scalar(metadata$scoring_version,'unknown'))
  fields<-c('account_id','championship_id','competition_id','season','scoring_version','user_team_id')
  if(!is.list(context)||!all(fields%in%names(context)))return(NULL)
  values<-lapply(context[fields],function(x)fm_scalar(x,''))
  if(any(!vapply(values,function(x)nzchar(x)&&nchar(x,type='bytes')<=256L,logical(1))))return(NULL)
  c(values,list(version=paste0(rival_calculation_version,':context-v2')))
}
rival_snapshot_id <- function(scope) paste0('rival-cache:',scope$version,':',rival_calculation_hash(scope[c('user_team_id','competition_id','season','scoring_version')]))
rival_snapshot_record <- function(row) is.list(row)&&startsWith(mvp_text(row$source_id),'rival-cache:')
# Only primitive vectors, lists and data frames have a wire representation. No
# arbitrary R classes, expressions, environments or serialized objects are read.
rival_snapshot_encode <- function(x,depth=0L) {
  if(is.null(x))return(list(type='null'))
  if(depth>18L||is.environment(x)||is.function(x)||is.language(x)||
     (!is.null(names(x))&&any(grepl('token|password|authorization|cookie|secret|session|login|api.?key',names(x),ignore.case=TRUE))))stop('Unsupported calculation snapshot value')
  if(inherits(x,'POSIXct')||inherits(x,'Date'))return(list(type='time',classes=I(class(x)),timezone=if(is.null(attr(x,'tzone')))NULL else I(attr(x,'tzone')),values=I(unclass(x))))
  if(!is.null(class(x))&&!all(class(x)%in%c('data.frame','tbl_df','tbl','AsIs','list','character','logical','numeric','integer')))stop('Unsupported snapshot class')
  attrs<-c('round_bonus_totals','point_rewards_incomplete','pending_bonus_types','award_payment_allocations','realized_trades')
  if(is.data.frame(x))return(list(type='frame',classes=I(class(x)),attribute_order=I(names(attributes(x))),row_names=rival_snapshot_encode(.row_names_info(x,0L),depth+1L),values=lapply(x,rival_snapshot_encode,depth=depth+1L),count=nrow(x),
    attributes=lapply(attributes(x)[intersect(attrs,names(attributes(x)))],rival_snapshot_encode,depth=depth+1L)))
  if(is.null(x))return(list(type='null'))
  if(is.list(x))return(list(type='list',names=if(is.null(names(x)))NULL else I(names(x)),values=unname(lapply(x,rival_snapshot_encode,depth=depth+1L)),as_is=inherits(x,'AsIs')))
  if(!typeof(x)%in%c('character','integer','double','logical'))stop('Unsupported calculation snapshot vector')
  list(type=typeof(x),names=if(is.null(names(x)))NULL else I(names(x)),values=I(unclass(x)),as_is=inherits(x,'AsIs'))
}
rival_snapshot_decode <- function(node,depth=0L) {
  if(depth>18L||!is.list(node)||!is.character(node$type)||length(node$type)!=1L)stop('Invalid calculation snapshot node')
  type<-node$type
  if(type=='time'){
    classes<-unlist(node$classes,use.names=FALSE);timezone<-unlist(node$timezone,use.names=FALSE)
    if(!identical(names(node),c('type','classes','timezone','values'))||
       !(identical(classes,'Date')||identical(classes,c('POSIXct','POSIXt')))||
       (!is.null(timezone)&&(!is.character(timezone)||length(timezone)>3L||anyNA(timezone)))||!is.list(node$values))stop('Invalid snapshot time')
    value<-vapply(node$values,function(x){if(is.null(x))return(NA_real_);if(!is.numeric(x)||length(x)!=1L||!is.finite(x))stop('Invalid snapshot time value');as.numeric(x)},numeric(1))
    class(value)<-classes;if(!is.null(timezone))attr(value,'tzone')<-timezone
    return(value)
  }
  if(type=='null'){if(!identical(names(node),'type'))stop('Invalid null');return(NULL)}
  if(type=='frame'){
    classes<-unlist(node$classes,use.names=FALSE)
    if(!(identical(classes,'data.frame')||identical(classes,c('tbl_df','tbl','data.frame')))||
       !identical(names(node),c('type','classes','attribute_order','row_names','values','count','attributes'))||!is.list(node$values)||
       !is.numeric(node$count)||length(node$count)!=1L||!is.finite(node$count)||node$count<0||node$count!=floor(node$count)||
       node$count>.rival_snapshot_limits$rows||!is.list(node$attributes))stop('Invalid snapshot frame')
    columns<-names(node$values)
    if(length(node$values)&&(is.null(columns)||anyNA(columns)||anyDuplicated(columns)))stop('Invalid snapshot columns')
    out<-lapply(node$values,rival_snapshot_decode,depth=depth+1L)
    if(any(lengths(out)!=node$count))stop('Invalid snapshot column lengths')
    row_names<-rival_snapshot_decode(node$row_names,depth+1L)
    if(!is.integer(row_names)&&!is.character(row_names))stop('Invalid snapshot row names')
    attributes(out)<-list(names=columns %||% character(),row.names=row_names,class=classes)
    allowed<-c('round_bonus_totals','point_rewards_incomplete','pending_bonus_types','award_payment_allocations','realized_trades')
    if(length(node$attributes)&&(!all(names(node$attributes)%in%allowed)||anyDuplicated(names(node$attributes))))stop('Invalid snapshot attributes')
    for(name in names(node$attributes))attr(out,name)<-rival_snapshot_decode(node$attributes[[name]],depth+1L)
    order<-unlist(node$attribute_order,use.names=FALSE)
    if(!is.character(order)||anyNA(order)||anyDuplicated(order)||!setequal(order,names(attributes(out))))stop('Invalid snapshot attribute order')
    attributes(out)<-attributes(out)[order]
    if(nrow(out)!=node$count)stop('Invalid snapshot row count')
    return(out)
  }
  if(!type%in%c('list','character','integer','double','logical')||!identical(names(node),c('type','names','values','as_is'))||
     !is.list(node$values)||!is.logical(node$as_is)||length(node$as_is)!=1L||is.na(node$as_is))stop('Invalid snapshot vector')
  if(type=='list')out<-lapply(node$values,rival_snapshot_decode,depth=depth+1L)else {
    default<-switch(type,character=NA_character_,integer=NA_integer_,double=NA_real_,logical=NA)
    out<-vapply(node$values,function(value){
      if(is.null(value))return(default)
      valid<-length(value)==1L&&switch(type,character=is.character(value),logical=is.logical(value),
        integer=is.numeric(value)&&is.finite(value)&&value==floor(value)&&abs(value)<=.Machine$integer.max,double=is.numeric(value)&&is.finite(value))
      if(!valid)stop('Invalid snapshot scalar')
      switch(type,character=as.character(value),logical=as.logical(value),integer=as.integer(value),double=as.numeric(value))
    },default)
  }
  if(!is.null(node$names)){
    labels<-unlist(node$names,use.names=FALSE)
    if(!is.character(labels)||length(labels)!=length(out)||anyNA(labels))stop('Invalid snapshot names')
    names(out)<-labels
  }
  if(node$as_is)out<-I(out)
  out
}
rival_snapshot_signature <- function(cache) rival_calculation_hash(list(preparation=cache$dependency_key,
  teams=lapply(cache$teams[sort(names(cache$teams))],function(x)x$key)))
#' Validate the bounded ledger projection retained by compact snapshots
#' @param totals Named list of per-team round_bonus_totals data frames.
#' @param team_ids Team identities permitted by the enclosing snapshot.
#' @return TRUE only for scoped, unique, numeric round/component observations.
#' @keywords internal
rival_snapshot_bonus_valid <- function(totals,team_ids) {
  if(!is.list(totals)||is.data.frame(totals)||length(totals)>.rival_snapshot_limits$teams)return(FALSE)
  if(!length(totals))return(TRUE)
  ids<-names(totals)
  if(is.null(ids)||anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids)||!all(ids%in%team_ids))return(FALSE)
  all(vapply(totals,function(rows){
    is.data.frame(rows)&&nrow(rows)<=480L&&
      all(c("round_number","bonus_type","amount")%in%names(rows))&&
      is.numeric(rows$round_number)&&all(is.finite(rows$round_number))&&
      all(rows$round_number>=1&rows$round_number==floor(rows$round_number))&&
      is.character(rows$bonus_type)&&all(rows$bonus_type%in%c("points","ranking","mvp","dreamteam"))&&
      is.numeric(rows$amount)&&all(is.na(rows$amount)|is.finite(rows$amount))&&
      (!"settlement_pending"%in%names(rows)||(is.logical(rows$settlement_pending)&&!anyNA(rows$settlement_pending)))&&
      !anyDuplicated(paste(rows$round_number,rows$bonus_type,sep="|"))
  },logical(1)))
}
rival_snapshot_payload <- function(cache,calculated,context) {
  scope<-rival_snapshot_scope(context)
  if(is.null(scope)||!is.data.frame(calculated$team_finances)||!nrow(calculated$team_finances)||
     nrow(calculated$team_finances)>.rival_snapshot_limits$teams)return(NULL)
  saved_cache<-as.list(cache)[intersect(c('preparation_key','dependency_key','prepared','teams'),names(as.list(cache)))]
  if(as.numeric(object.size(saved_cache))>.rival_snapshot_limits$json)saved_cache<-NULL
  summary<-calculated$team_finances
  # Retain the small numeric ledger projection even when the detailed cache
  # exceeds the snapshot budget. Never recalculate rewards from summary income.
  ledgers<-calculated$cash_ledgers %||% list()
  totals<-lapply(ledgers[intersect(names(ledgers),summary$teamid)],function(ledger)attr(ledger,"round_bonus_totals"))
  totals<-Filter(function(rows)is.data.frame(rows)&&nrow(rows)>0L,totals)
  if(length(calculated$pending_bonus_rounds))totals<-lapply(totals,function(rows){
    rows$settlement_pending<-rows$round_number%in%calculated$pending_bonus_rounds
    rows
  })
  if(!rival_snapshot_bonus_valid(totals,summary$teamid))return(NULL)
  if(length(totals))attr(summary,"round_bonus_totals")<-totals
  make<-function(value){
    wire<-list(format=2L,scope=scope,signature=rival_snapshot_signature(cache),summary=rival_snapshot_encode(summary),cache=rival_snapshot_encode(value))
    json<-charToRaw(as.character(jsonlite::toJSON(wire,auto_unbox=TRUE,na='null',null='null',digits=NA)))
    if(length(json)>.rival_snapshot_limits$json)return(NULL)
    compressed<-history_baseline_compress(json)
    encoded<-openssl::base64_encode(compressed)
    if(nchar(encoded,type='bytes')>.rival_snapshot_limits$payload)return(NULL)
    list(kind='rival_calculation_cache',encoding='json-gzip-base64-v1',json_bytes=length(json),sha256=history_baseline_hash(compressed),data=encoded)
  }
  tryCatch(make(saved_cache),error=function(e)NULL) %||% tryCatch(make(NULL),error=function(e)NULL)
}
read_rival_calculation_snapshot <- function(account_id,championship_id,season,user_team_id,competition_id=NULL,scoring_version=NULL) {
  scope<-rival_snapshot_scope(list(account_id=account_id,championship_id=championship_id,competition_id=competition_id,season=season,scoring_version=scoring_version,user_team_id=user_team_id))
  if(is.null(scope))return(NULL)
  query<-list(account_id=paste0('eq.',account_id),championship_id=paste0('eq.',championship_id),competition_id=paste0('eq.',scope$competition_id),season=paste0('eq.',season),
    evidence_type='eq.eligibility',source_id=paste0('eq.',rival_snapshot_id(scope)),
    select='account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at,payload',limit=1L)
  rows<-tryCatch(supabase_get('rival_finance_evidence_latest',query),error=function(e)NULL)
  tryCatch({
    if(!is.data.frame(rows)||nrow(rows)!=1L)return(NULL)
    row<-rival_evidence_decode(rows)[[1]]
    if(!all(vapply(c('account_id','championship_id','competition_id','season'),function(field)identical(mvp_text(row[[field]]),scope[[field]]),logical(1)))||
       !identical(mvp_text(row$source_id),rival_snapshot_id(scope))||!identical(mvp_text(row$evidence_type),'eligibility')||
       is.na(history_baseline_stamp(row$observed_at)))return(NULL)
    payload<-row$payload
    if(!is.list(payload)||!identical(payload$kind,'rival_calculation_cache')||!identical(payload$encoding,'json-gzip-base64-v1')||
       !is.character(payload$data)||length(payload$data)!=1L||nchar(payload$data,type='bytes')>.rival_snapshot_limits$payload||
       !is.numeric(payload$json_bytes)||length(payload$json_bytes)!=1L||!is.finite(payload$json_bytes)||payload$json_bytes<1||
       payload$json_bytes!=floor(payload$json_bytes)||payload$json_bytes>.rival_snapshot_limits$json)return(NULL)
    bytes<-openssl::base64_decode(payload$data)
    if(!identical(history_baseline_hash(bytes),payload$sha256))return(NULL)
    raw<-history_baseline_decompress(bytes,payload$json_bytes)
    if(length(raw)!=payload$json_bytes)return(NULL)
    wire<-jsonlite::fromJSON(rawToChar(raw),simplifyVector=FALSE)
    if(!is.list(wire)||!identical(names(wire),c('format','scope','signature','summary','cache'))||
       !identical(wire$format,2L)||!identical(wire$scope,scope)||!is.character(wire$signature)||length(wire$signature)!=1L||
       !grepl('^[0-9a-f]{64}$',wire$signature))return(NULL)
    summary<-rival_snapshot_decode(wire$summary);cache<-rival_snapshot_decode(wire$cache)
    totals<-attr(summary,"round_bonus_totals") %||% list()
    attr(summary,"round_bonus_totals")<-NULL
    if(!rival_snapshot_bonus_valid(totals,summary$teamid))return(NULL)
    if(!is.data.frame(summary)||!nrow(summary)||nrow(summary)>.rival_snapshot_limits$teams||
       !all(c('teamid','teamname','estimated_cash','observed_cash','observed_cash_at','observed_cash_fresh','as_of')%in%names(summary))||
       !is.character(summary$teamid)||anyNA(summary$teamid)||any(!nzchar(summary$teamid))||anyDuplicated(summary$teamid))return(NULL)
    if(!is.null(cache)){
      if(!is.list(cache)||!all(c('preparation_key','dependency_key','prepared','teams')%in%names(cache))||
         !identical(names(cache),c('preparation_key','dependency_key','prepared','teams'))||
         !is.character(cache$preparation_key)||length(cache$preparation_key)!=1L||!grepl('^[0-9a-f]{64}$',cache$preparation_key)||
         !is.character(cache$dependency_key)||length(cache$dependency_key)!=1L||!grepl('^[0-9a-f]{64}$',cache$dependency_key)||
         !is.list(cache$prepared)||!identical(names(cache$prepared),c('history','official','eligibility','catalog','all_movements','publications','anchors','reference_payments','final_settlements','raw_rounds','point_rounds','point_checks','opening'))||
         !is.list(cache$teams)||length(cache$teams)>100L||
         is.null(names(cache$teams))||anyDuplicated(names(cache$teams))||!setequal(names(cache$teams),summary$teamid)||!identical(rival_snapshot_signature(cache),wire$signature))return(NULL)
      if(!all(vapply(names(cache$teams),function(id){x<-cache$teams[[id]]
        is.list(x)&&identical(names(x),c('key','result','breakdown','ledger','missing','assumptions','estimated_movements'))&&is.character(x$key)&&length(x$key)==1L&&
        grepl('^[0-9a-f]{64}$',x$key)&&is.data.frame(x$result)&&nrow(x$result)==1L&&identical(x$result$teamid,id)&&
        identical(names(x$result),names(summary))&&is.data.frame(x$breakdown)&&is.data.frame(x$ledger)&&
        is.character(x$missing)&&is.character(x$assumptions)&&is.logical(x$estimated_movements)&&length(x$estimated_movements)==1L&&!is.na(x$estimated_movements)
      },logical(1))))return(NULL)
    }
    ledgers<-if(is.list(cache))lapply(cache$teams,function(team)team$ledger)else list()
    for(id in setdiff(names(totals),names(ledgers))){
      ledger<-data.frame()
      attr(ledger,"round_bonus_totals")<-totals[[id]]
      ledgers[[id]]<-ledger
    }
    pending<-sort(unique(unlist(lapply(totals,function(rows){
      if("settlement_pending"%in%names(rows))rows$round_number[rows$settlement_pending]else numeric()
    }),use.names=FALSE)))
    list(scope=scope,signature=wire$signature,team_finances=summary,cash_ledgers=ledgers,
      pending_bonus_rounds=pending,cache=cache,saved_at=row$observed_at)
  },error=function(e)NULL)
}
persist_rival_calculation_snapshot <- function(cache,calculated,context,on_complete=NULL) {
  payload<-rival_snapshot_payload(cache,calculated,context);scope<-rival_snapshot_scope(context)
  if(is.null(payload)||is.null(scope)){if(is.function(on_complete))on_complete(FALSE);return(FALSE)}
  row<-rival_finance_evidence(scope$account_id,scope$championship_id,scope$season,'eligibility',rival_snapshot_id(scope),payload,competition_id=scope$competition_id)
  if(as.numeric(object.size(row))>256L*1024L){if(is.function(on_complete))on_complete(FALSE);return(FALSE)}
  defer_persistence('supabase_post_direct',list('rival_finance_evidence',list(row),
    'account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at',TRUE),context,on_complete)
}
