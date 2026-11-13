# Safe, language-neutral worker values. No R serialization or executable classes.
fm_worker_version <- 1L
fm_worker_fields <- c('account_id','championship_id','user_team_id','player_id','competition_id','season','scoring_version')
fm_worker_secret_name <- function(x) grepl('(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)',x,ignore.case=TRUE)
fm_worker_encode <- function(x,depth=0L) {
  if(depth>24L||is.environment(x)||is.function(x)||is.language(x))stop('Unsupported worker value')
  if(is.null(x))return(list(kind='null'))
  # Transport data.table results as factual frames, without process-local pointers.
  if(inherits(x,'data.table')){
    if(!identical(class(x),c('data.table','data.frame')))stop('Unsupported worker class')
    x<-as.data.frame(x)
  }
  attrs<-attributes(x);classes<-class(x)
  if(!is.null(attrs$class)&&!all(classes%in%c('data.frame','tbl_df','tbl','AsIs','POSIXct','POSIXt','Date','futmondo_context')))stop('Unsupported worker class')
  if(!is.null(names(x))&&any(fm_worker_secret_name(names(x))))stop('Credential in worker value')
  metadata<-attrs[setdiff(names(attrs),c('names','class','row.names'))]
  # Diagnostics carry conditions/classes, not factual data; expose only safe codes.
  metadata$fetch_error<-NULL
  metadata<-lapply(metadata,fm_worker_encode,depth=depth+1L)
  if(is.data.frame(x))return(list(kind='frame',columns=lapply(x,fm_worker_encode,depth=depth+1L),column_names=I(names(x)),count=nrow(x),meta=metadata))
  if(is.list(x))return(list(kind='list',names=if(is.null(names(x)))NULL else I(names(x)),values=unname(lapply(x,fm_worker_encode,depth=depth+1L)),context=inherits(x,'futmondo_context'),meta=metadata))
  if(!typeof(x)%in%c('character','logical','integer','double'))stop('Unsupported worker primitive')
  list(kind=typeof(x),values=I(unclass(x)),names=if(is.null(names(x)))NULL else I(names(x)),
    time=if(inherits(x,'POSIXct'))'datetime'else if(inherits(x,'Date'))'date'else NULL,meta=metadata)
}
fm_worker_decode <- function(x,depth=0L) {
  if(depth>24L||!is.list(x)||!is.character(x$kind)||length(x$kind)!=1L)stop('Invalid worker value')
  if(x$kind=='null')return(NULL)
  if(x$kind=='frame'){
    if(!is.list(x$columns)||is.null(names(x$columns))&&length(x$columns)||anyDuplicated(names(x$columns))||
       !is.numeric(x$count)||length(x$count)!=1L||!is.finite(x$count)||x$count<0||x$count>50000||x$count!=floor(x$count))stop('Invalid worker frame')
    order<-unlist(x$column_names %||% names(x$columns),use.names=FALSE) %||% character()
    if(!is.character(order)||anyNA(order)||anyDuplicated(order)||!setequal(order,names(x$columns)))stop('Invalid worker column order')
    result<-lapply(x$columns[order],fm_worker_decode,depth=depth+1L)
    if(any(lengths(result)!=x$count))stop('Worker frame lengths differ')
    attributes(result)<-list(names=order,row.names=.set_row_names(as.integer(x$count)),class='data.frame')
  }else if(x$kind=='list'){
    if(!is.list(x$values))stop('Invalid worker list')
    result<-lapply(x$values,fm_worker_decode,depth=depth+1L)
    # JSON [] decodes to list(); unlist() must retain a typed empty name vector.
    if(!is.null(x$names)){n<-unlist(x$names,use.names=FALSE) %||% character();if(!is.character(n)||length(n)!=length(result)||anyNA(n)||any(fm_worker_secret_name(n)))stop('Invalid worker names');names(result)<-n}
    if(isTRUE(x$context)){if(!all(fm_worker_fields%in%names(result)))stop('Invalid worker context');class(result)<-'futmondo_context'}
  }else{
    if(!x$kind%in%c('character','logical','integer','double')||!(is.list(x$values)||is.atomic(x$values)))stop('Invalid worker primitive')
    default<-switch(x$kind,character=NA_character_,logical=NA,integer=NA_integer_,double=NA_real_)
    if(is.atomic(x$values)){
      v<-x$values
      valid<-all(is.na(v))||switch(x$kind,character=is.character(v),logical=is.logical(v),integer=is.numeric(v)&&all(is.na(v)|(is.finite(v)&v==floor(v))),double=is.numeric(v)&&all(is.na(v)|is.finite(v)))
      if(!valid)stop('Invalid worker vector')
      result<-switch(x$kind,character=as.character(v),logical=as.logical(v),integer=as.integer(v),double=as.numeric(v))
    }else result<-vapply(x$values,function(v){
      if(is.null(v))return(default)
      if(length(v)!=1L||!switch(x$kind,character=is.character(v),logical=is.logical(v),integer=is.numeric(v)&&is.finite(v)&&v==floor(v),double=is.numeric(v)&&is.finite(v)))stop('Invalid worker scalar')
      switch(x$kind,character=v,logical=v,integer=as.integer(v),double=as.numeric(v))
    },default)
    if(!is.null(x$names)){n<-unlist(x$names,use.names=FALSE) %||% character();if(length(n)!=length(result)||anyNA(n)||any(fm_worker_secret_name(n)))stop('Invalid worker names');names(result)<-n}
    if(!is.null(x$time)){if(!is.double(result))stop('Invalid worker time');class(result)<-switch(x$time,datetime=c('POSIXct','POSIXt'),date='Date',stop('Invalid worker time class'))}
  }
  if(length(x$meta)){
    if(is.null(names(x$meta))||anyDuplicated(names(x$meta))||any(names(x$meta)%in%c('class','names','row.names'))||any(fm_worker_secret_name(names(x$meta))))stop('Invalid worker metadata')
    for(n in names(x$meta))attr(result,n)<-fm_worker_decode(x$meta[[n]],depth+1L)
  }
  result
}
fm_worker_sort <- function(x){if(is.data.frame(x))return(x);if(is.list(x)){if(!is.null(names(x)))x<-x[sort(names(x))];x<-lapply(x,fm_worker_sort)};x}
fm_worker_json <- function(x) as.character(jsonlite::toJSON(fm_worker_sort(x),auto_unbox=TRUE,null='null',na='null',digits=NA))
fm_worker_descriptor <- function(handler,args,context) {
  # Optional display inputs never define the acquired source resource.
  args<-args[setdiff(names(args),c('login','teams','saved','refresh','full_audit','now'))]
  if(any(fm_worker_secret_name(names(args))))stop('Credential in worker arguments')
  scope<-setNames(lapply(fm_worker_fields,function(n)fm_scalar(context[[n]],if(n%in%c('season','scoring_version','competition_id'))'unknown'else'')),fm_worker_fields)
  if(!nzchar(scope$account_id))stop('Missing worker account')
  if(!is.null(args$account_id)&&!identical(as.character(args$account_id),scope$account_id))stop('Foreign worker account argument')
  if(!is.null(args$player_id))scope$player_id<-as.character(args$player_id)
  if(!is.null(args$target_player_id))scope$player_id<-as.character(args$target_player_id)
  for(n in intersect(fm_worker_fields,names(args)))scope[[n]]<-fm_scalar(args[[n]],scope[[n]])
  # Player-only wire endpoints have no league source dependency, but remain account isolated.
  if(handler%in%c('get_player_full_profile','get_player_matches'))scope[c('championship_id','user_team_id','competition_id','season','scoring_version')]<-list('','','unknown','unknown','unknown')
  scope$user_team_id<-fm_scalar(args$user_team_id %||% args$team_id,'')
  parameters<-fm_worker_sort(args)
  descriptor<-list(version=fm_worker_version,handler=handler,args=parameters,context=scope)
  descriptor$encoded_args<-fm_worker_encode(parameters)
  descriptor$resource_key<-paste0(openssl::sha256(charToRaw(fm_worker_json(descriptor))))
  descriptor
}
fm_worker_interval <- function(handler){
  if(grepl('capacity',handler))15L else if(grepl('full_profile|matches',handler))21600L else if(grepl('summary|card_history|recent_points',handler))300L else
    if(grepl('catalog|championship_players|finished_rounds|real_clubs|active_championship',handler))300L else 60L
}

# Read urgency is separate from source validity and explicit Refresh.
fm_worker_priority <- function(handler) {
 if(handler%in%c('get_players_from_team','get_championship_pressroom','get_market_players',
   'get_my_market_players','get_user_team_info','get_roster_bids','get_lineup_from_team',
   'get_notifications','get_notification_unread','get_active_championships'))10L else
 if(handler%in%c('get_resolved_league_context','get_finished_rounds'))9L else
 if(handler%in%c('get_clause_transfer_summary','fm_worker_finances','get_acquisition_capacity','get_team_acquisition_capacity','get_financial_snapshot'))8L else 6L
}
