# Bounded saving of observations already fetched by active views.
.persistence_state<-new.env(parent=emptyenv())
.persistence_state$queue<-list();.persistence_state$active<-NULL;.persistence_state$progress<-list()
.persistence_state$scheduled<-FALSE;.persistence_state$errors<-list();.persistence_state$acknowledged<-list()
.persistence_max_jobs_per_owner<-25L
persistence_job_scope<-function(name,args){
 identity<-list(fn=name)
 if(identical(name,"supabase_fact_batch_direct")&&length(args)==1L){identity$operations<-vapply(args[[1]],function(op)persistence_job_scope("supabase_post_direct",op),character(1));return(as.character(openssl::sha256(serialize(identity,NULL))))}
 if(identical(name,"supabase_post_direct")&&length(args)>=2L){
  identity$table<-args[[1]];payload<-args[[2]];conflict<-if(length(args)>=3L)args[[3]]else supabase_conflict_key(args[[1]])
  fields<-if(length(conflict)==1L&&!is.na(conflict))setdiff(strsplit(conflict,",",fixed=TRUE)[[1]],"observed_at")else "id"
  rows<-if(is.data.frame(payload))lapply(seq_len(nrow(payload)),function(i)as.list(payload[i,,drop=FALSE]))else if(is.list(payload)&&!is.null(names(payload)))list(payload)else payload
  if(length(fields)&&length(rows)&&all(vapply(rows,function(row)is.list(row)&&all(fields%in%names(row)),logical(1))))identity$rows<-sort(vapply(rows,function(row)as.character(openssl::sha256(serialize(row[fields],NULL))),character(1)))else identity$payload<-payload
 }else identity$args<-args
 as.character(openssl::sha256(serialize(identity,NULL)))
}

persistence_failure_message<-function(issues=list()){
  categories<-vapply(issues,function(x)fm_scalar(x$category,"write"),character(1))
  if("configuration"%in%categories)return("History saving is unavailable because the database is not configured. Live Futmondo data is still available.")
  if("authorization"%in%categories)return("History could not be saved because database access was denied.")
  if("schema"%in%categories)return("History could not be saved because the database needs an update.")
  "Some history could not be saved. Live data remains available; Refresh to retry."
}
persistence_record_result<-function(job,ok,issues=list()){
 errors<-.persistence_state$errors[[job$owner]] %||% list();key<-job$error_key %||% job$key
 errors[[key]]<-if(isTRUE(ok))NULL else persistence_failure_message(issues)
 if(length(errors)>64L)errors<-tail(errors,64L)
 .persistence_state$errors[[job$owner]]<-if(length(errors))errors else NULL
 if(length(.persistence_state$errors)>128L).persistence_state$errors<-tail(.persistence_state$errors,128L)
 if(isTRUE(ok)){.persistence_state$acknowledged[[job$key]]<-Sys.time();if(length(.persistence_state$acknowledged)>256L).persistence_state$acknowledged<-.persistence_state$acknowledged[-1]};invisible(ok)
}

defer_persistence<-function(fn,args=list(),context=getOption("futmondo.persistence_context")){
  if(isTRUE(getOption("futmondo.offline",FALSE)))return(invisible(TRUE))
  name<-if(is.character(fn))fn else deparse(substitute(fn))
  if(!name%in%c("supabase_post_direct","supabase_fact_batch_direct"))return(FALSE)
  if(identical(name,"supabase_fact_batch_direct")&&(length(args)!=1L||!supabase_fact_batch_valid(args[[1]])))return(FALSE)
  domain<-if(requireNamespace("shiny",quietly=TRUE))shiny::getDefaultReactiveDomain()else NULL
  owner<-fm_scalar(context$account_id %||% if(!is.null(domain))domain$userData$futmondo_user_id else NULL,"server")
  session_id<-fm_scalar(context$session_id %||% if(!is.null(domain))domain$token else NULL,owner)
  key<-as.character(openssl::sha256(serialize(list(owner,name,args),NULL)))
  existing<-c(.persistence_state$queue,if(!is.null(.persistence_state$active))list(.persistence_state$active)else list())
  stamp<-.persistence_state$acknowledged[[key]]
  if((!is.null(stamp)&&as.numeric(difftime(Sys.time(),stamp,units="secs"))<3600)||any(vapply(existing,function(x)identical(x$key,key),logical(1))))return(invisible(TRUE))
  bytes<-as.numeric(object.size(args));retained<-sum(vapply(existing,function(x)as.numeric(x$bytes %||% object.size(x$args)),numeric(1)))
  job<-list(fn=name,args=args,owner=owner,session_id=session_id,key=key,bytes=bytes,error_key=persistence_job_scope(name,args))
  if(bytes>1024^2||retained+bytes>16*1024^2){persistence_record_result(job,FALSE);return(FALSE)}
  if(length(existing)>=200L||sum(vapply(existing,function(x)identical(x$owner,owner),logical(1)))>=.persistence_max_jobs_per_owner){persistence_record_result(job,FALSE);return(FALSE)}
  .persistence_state$queue[[length(.persistence_state$queue)+1L]]<-job
  if(!.persistence_state$scheduled){.persistence_state$scheduled<-TRUE;later::later(persistence_tick,0)}
  invisible(TRUE)
}
cancel_persistence_jobs<-function(owner,session_id=NULL){
  matches<-function(job)identical(job$owner,owner)&&(is.null(session_id)||identical(job$session_id,session_id))
  before<-length(.persistence_state$queue);.persistence_state$queue<-Filter(function(job)!matches(job),.persistence_state$queue)
  active<-.persistence_state$active;dropped<-before-length(.persistence_state$queue)
  if(!is.null(active)&&matches(active)){.persistence_state$active<-NULL;fm_transport_cancel(paste0("persistence:",active$session_id));dropped<-dropped+1L;later::later(persistence_tick,0)}
  invisible(dropped)
}
background_sync_status<-function(user_id){
  jobs<-c(.persistence_state$queue,if(!is.null(.persistence_state$active))list(.persistence_state$active)else list())
  errors<-unlist(.persistence_state$errors[[fm_scalar(user_id,"")]],use.names=FALSE)
  list(pending=sum(vapply(jobs,function(x)identical(x$owner,user_id),logical(1))),error=if(length(errors))paste(unique(errors),collapse=" ")else NULL,progress=NULL)
}
persistence_tick<-function(){
  if(!is.null(.persistence_state$active))return(invisible(TRUE))
  if(!length(.persistence_state$queue)){.persistence_state$scheduled<-FALSE;return(invisible(TRUE))}
  job<-.persistence_state$queue[[1]];.persistence_state$queue<-.persistence_state$queue[-1];.persistence_state$active<-job
  operations<-if(identical(job$fn,"supabase_fact_batch_direct"))job$args[[1]]else list(job$args)
  index<-job$operation_index %||% 1L;op<-operations[[index]]
  finish<-function(ok,issues=list()){
    if(!identical(.persistence_state$active$key,job$key))return(NULL)
    .persistence_state$active<-NULL
    if(isTRUE(ok)&&index<length(operations)){job$operation_index<-index+1L;.persistence_state$queue<-c(list(job),.persistence_state$queue)}else persistence_record_result(job,ok,issues)
    later::later(persistence_tick,0);NULL
  }
  table<-op[[1]];payload<-op[[2]];conflict<-if(length(op)>=3L)op[[3]]else supabase_conflict_key(table)
  if(!nzchar(get_sb_url())||!nzchar(get_sb_key())){finish(FALSE,list(list(category="configuration")));return(invisible(FALSE))}
  url<-httr::modify_url(paste0(get_sb_url(),"/rest/v1/",table),query=if(!is.null(conflict))list(on_conflict=conflict)else NULL)
  headers<-c(apikey=get_sb_key(),Authorization=paste("Bearer",get_sb_key()),`Content-Type`="application/json",Prefer=if(length(op)>=4L&&isTRUE(op[[4]]))"resolution=ignore-duplicates" else "resolution=merge-duplicates")
  promises::then(fm_async_http(url,"POST",jsonlite::toJSON(payload,auto_unbox=TRUE,na="null"),headers,owner=paste0("persistence:",job$session_id)),function(response){if(identical(table,"round_dream_team"))invalidate_mvp_saved_cache();finish(TRUE)},function(error){code<-conditionMessage(error);category<-if(grepl("HTTP (401|403)",code))"authorization"else if(grepl("HTTP (400|404)",code))"schema"else "write";finish(FALSE,list(list(category=category)))})
  invisible(TRUE)
}
record_acquired_resource<-function(resource,data,context){
  if(is.null(data)||identical(attr(data,"fetch_status"),"stale"))return(invisible(FALSE))
  old<-getOption("futmondo.persistence_context");options(futmondo.persistence_context=context)
  on.exit(options(futmondo.persistence_context=old),add=TRUE)
  league<-context$championship_id
  if(identical(resource,"active_championship_objects")&&is.list(data)){
   champs<-dplyr::bind_rows(lapply(data$championships,function(champ){
    id<-fm_scalar(champ$id %||% champ$`_id`,"");name<-fm_scalar(champ$name,"")
    if(!nzchar(id)||!nzchar(name))return(NULL)
    data.frame(id=id,name=name,mode=fm_scalar(champ$mode),sport=fm_scalar(champ$sport))
   }))
   teams<-dplyr::bind_rows(lapply(data$championships,function(champ){
    team<-champ$userteam;id<-fm_scalar(team$id %||% team$`_id`,"");name<-fm_scalar(team$name,"");cid<-fm_scalar(champ$id %||% champ$`_id`,"")
    if(!nzchar(id)||!nzchar(name)||!nzchar(cid))return(NULL)
    data.frame(id=id,championship_id=cid,name=name)
   }))
   if(nrow(champs))supabase_post("championships",champs[!duplicated(champs$id),,drop=FALSE],conflict="id")
   if(nrow(teams))supabase_post("user_teams",teams[!duplicated(teams$id),,drop=FALSE],conflict="id")
   return(invisible(TRUE))
  }
  if(!nzchar(league))return(invisible(TRUE))
  if(grepl("^championshipplayers_",resource)&&is.data.frame(data)){
    sync_real_clubs_to_supabase(data);sync_players_to_supabase(data);log_player_history(data,league);log_player_daily_snapshots(data,league)
  }else if(grepl("^teams_",resource)&&is.data.frame(data)){
    sync_user_teams_to_supabase(data,league);log_user_team_history(data,championship_id=league)
  }else if(grepl("^team_info_",resource)&&is.list(data)){
    stamp<-attr(data,"observed_at");cash<-fm_number(data$budget);value<-fm_number(data$teamValue %||% data$team_value);points<-fm_number(data$points)
    if(nzchar(context$user_team_id)&&length(stamp)==1L&&!is.na(fm_time(stamp))&&any(is.finite(c(cash,value,points))))supabase_post("user_team_history",list(user_team_id=context$user_team_id,championship_id=league,observed_at=format(fm_time(stamp),"%Y-%m-%dT%H:%M:%OSZ",tz="UTC"),budget=cash,team_value=value,points=points))
  }else if(grepl("pressroom",resource)&&is.data.frame(data)){
    persist_auction_history(data,league)
  }else if(grepl("^player_card_history$",resource)&&is.list(data)&&nrow(data$points)){
    points<-data$points;ctx<-data$context
    points<-points[points$source=="futmondo"&!is.na(fm_time(points$observed_at)),,drop=FALSE]
    previous<-data$previous_points
    if(nrow(points)&&is.data.frame(previous)&&nrow(previous)){
     index<-match(points$round_number,previous$round_number);new_date<-fm_time(points$date);old_date<-fm_time(previous$date[index])
     changed<-is.na(index)|points$points!=previous$points[index]|points$score_status!=previous$score_status[index]|(!is.na(new_date)&(is.na(old_date)|new_date!=old_date))
     points<-points[changed%in%TRUE,,drop=FALSE]
    }
    if(nrow(points))supabase_post("player_match_observations",data.frame(player_id=ctx$player_id,championship_id=ctx$championship_id,season=ctx$season,scoring_version=ctx$scoring_version,round=points$round_number,points=points$points,score_status=points$score_status,occurred_at=points$date,observed_at=points$observed_at))
  };invisible(TRUE)
}
