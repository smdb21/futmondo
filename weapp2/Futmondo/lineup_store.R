# Server-only bounded database snapshots. Transport and persistence never block
# the Shiny event loop; cached source age is not reset by database hydration.
lineup_snapshot_store <- function(context,target_round,payload=NULL,owner='') {
  if(!nzchar(get_sb_url())||!nzchar(get_sb_key()))return(promises::promise_resolve(NULL))
  scope<-list(account_id=context$account_id,championship_id=context$championship_id,user_team_id=context$user_team_id,target_round=target_round)
  requested<-scope
  for(field in c('season','scoring_version'))if(nzchar(context[[field]])&&!identical(context[[field]],'unknown'))requested[[field]]<-context[[field]]
  query<-if(is.null(payload))c(lapply(requested,function(x)paste0('eq.',x)),list(select='*',order='observed_at.desc',limit='1'))else
    list(on_conflict='account_id,championship_id,user_team_id,target_round,season,scoring_version')
  headers<-c(apikey=get_sb_key(),Authorization=paste('Bearer',get_sb_key()),`Content-Type`='application/json',Prefer='resolution=merge-duplicates,return=minimal')
  url<-httr::modify_url(paste0(sub('/$','',get_sb_url()),'/rest/v1/team_lineup_snapshots'),query=query)
  if(!is.null(payload)){
    row<-c(scope,list(season=payload$season,scoring_version=payload$scoring_version,payload=payload,
      observed_at=payload$observed_at,expires_at=payload$expires_at,complete=TRUE))
    body<-jsonlite::toJSON(row,auto_unbox=TRUE,null='null',na='null')
    if(nchar(body,type='bytes')>524288)return(promises::promise_reject(simpleError('Lineup snapshot exceeds storage limit')))
    return(promises::then(fm_async_http(url,'POST',body,headers,owner=owner,timeout=8),function(response)TRUE))
  }
  promises::then(fm_async_read_http(url,'GET',headers=headers,owner=owner),function(response){
    if(length(response$content)>600000L)stop('Invalid saved lineup')
    rows<-jsonlite::fromJSON(rawToChar(response$content),simplifyVector=FALSE)
    if(!length(rows))return(NULL)
    if(!is.list(rows)||length(rows)!=1L)stop('Invalid saved lineup')
    row<-rows[[1]]
    if(!all(vapply(names(requested),function(key)identical(fm_scalar(row[[key]]),requested[[key]]),logical(1)))||!isTRUE(row$complete))stop('Saved lineup context mismatch')
    data<-lineup_validate_snapshot(row$payload)
    if(!identical(row$season,data$season)||!identical(row$scoring_version,data$scoring_version)||
       !isTRUE(as.numeric(fm_time(row$observed_at))==as.numeric(fm_time(data$observed_at)))||!isTRUE(as.numeric(fm_time(row$expires_at))==as.numeric(fm_time(data$expires_at))))stop('Saved lineup metadata mismatch')
    data
  })
}
lineup_validate_snapshot <- function(data) {
  if(!is.list(data)||!identical(as.integer(data$version),1L)||!is.list(data$roster)||!is.list(data$official)||
     !is.list(data$rules)||!is.list(data$source)||!nzchar(fm_scalar(data$season))||!nzchar(fm_scalar(data$scoring_version)))stop('Saved lineup is incomplete')
  stamp<-fm_time(data$observed_at);until<-fm_time(data$expires_at)
  if(length(stamp)!=1L||is.na(stamp)||length(until)!=1L||is.na(until)||until<stamp||stamp>Sys.time()+60)stop('Saved lineup timestamps invalid')
  official<-data$official
  official$strategy<-fm_scalar(official$strategy,'')
  official$starters<-as.character(unlist(official$starters,use.names=FALSE))
  official$bench<-as.character(unlist(official$bench,use.names=FALSE))
  official$captain<-fm_scalar(official$captain,'')
  if(!official$strategy%in%lineup_formations()||length(official$starters)!=11L||length(official$bench)!=4L||
     anyNA(c(official$starters,official$bench))||anyDuplicated(c(official$starters,official$bench)[nzchar(c(official$starters,official$bench))]))stop('Saved lineup assignments invalid')
  data$official<-official
  data$rules$allowed<-intersect(lineup_formations(),as.character(unlist(data$rules$allowed,use.names=FALSE)))
  for(flag in c('multiposition','bench_enabled','captain_enabled'))if(!is.logical(data$rules[[flag]])||length(data$rules[[flag]])!=1L||is.na(data$rules[[flag]]))stop('Saved lineup rules invalid')
  if(!is.list(data$source$wire)||!identical(data$source$wire$strategy,official$strategy)||!is.list(data$source$wire$players))stop('Saved lineup source contract invalid')
  if(length(data$roster)>2000L)stop('Saved roster exceeds limits')
  ids<-vapply(data$roster,function(row){
    if(!is.list(row)||!nzchar(fm_scalar(row$id,''))||!nzchar(fm_scalar(row$name,'')))stop('Saved roster player invalid')
    fm_scalar(row$id,'')
  },character(1))
  if(anyDuplicated(ids))stop('Saved roster has duplicate identities')
  data<-data[intersect(names(data),c('version','roster','official','rules','source','season','scoring_version','observed_at','expires_at'))]
  data
}
lineup_snapshot_fresh <- function(data,now=Sys.time()) {
  !is.null(data)&&isTRUE(fm_time(data$expires_at)>now)&&isTRUE(fm_time(data$observed_at)<=now+60)
}
