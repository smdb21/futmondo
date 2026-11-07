# Explicit factual reads and one-shot user-authorized batch lineup submission.
lineup_api_payload <- function(auth,championship_id,user_team_id,extra=list()) list(
  header=list(token=auth[['token']],userid=auth[['userid']]),
  query=c(list(championshipId=championship_id,userteamId=user_team_id),extra),answer=list())
lineup_api_read <- function(route,auth,context,owner,extra=list()) {
  body<-jsonlite::toJSON(lineup_api_payload(auth,context$championship_id,context$user_team_id,extra),auto_unbox=TRUE,null='null')
  promises::then(fm_async_read_http(paste0('https://api.futmondo.com',route),'POST',body,
    c(`Content-Type`='application/json; charset=utf-8'),owner=owner,priority='interactive'),function(response){
      if(length(response$content)>2*1024^2)stop('Lineup response is too large')
      parsed<-jsonlite::fromJSON(rawToChar(response$content),simplifyVector=FALSE)
      if(!is.list(parsed)||is.null(parsed$answer)||isTRUE(parsed$answer$error)||
         (!is.null(parsed$code)&&!identical(parsed$code,API_CODE_OK))||
         (!is.null(parsed$answer$code)&&!identical(parsed$answer$code,API_CODE_OK)))stop('Lineup source rejected this request')
      parsed$answer
    })
}
lineup_roster_records <- function(raw) {
  if(!is.list(raw)||length(raw)>2000L)stop('Squad is invalid')
  records<-lapply(raw,function(player){
    id<-fm_scalar(player$id %||% player$`_id`,'');if(!nzchar(id))stop('Squad player identity missing')
    team<-player$team
    list(id=id,name=fm_scalar(player$name,id),role=fm_scalar(player$role,''),role2=fm_scalar(player$role2,''),
      status=fm_scalar(player$status,''),photo=fm_scalar(player$photo,''),buyPrice=fm_number(player$buyPrice),value=fm_number(player$value),
      club=if(is.list(team))fm_scalar(team$name,'')else fm_scalar(team,''))
  })
  ids<-vapply(records,`[[`,character(1),'id');if(anyDuplicated(ids))stop('Duplicate squad player identity')
  records
}
lineup_roster_frame <- function(data) {
  rows<-data$roster %||% list()
  if(!length(rows))return(data.frame(id=character(),name=character(),role=character(),role2=character(),status=character(),photo=character(),club=character()))
  result<-dplyr::bind_rows(rows);attr(result,'observed_at')<-fm_time(data$observed_at)
  result
}
lineup_parse_official <- function(raw) {
  strategy<-fm_scalar(raw$strategy,'');draft<-lineup_empty(strategy)
  if(!strategy%in%lineup_formations())stop('Official formation is unsupported')
  wire<-list();captain<-character();locked<-list()
  parse<-function(players,bench){
    slots<-rep('',if(bench)4L else 11L)
    if(!is.list(players)||length(players)>length(slots))stop('Official lineup positions invalid')
    for(player in players){
      id<-fm_scalar(player$id %||% player$`_id`,'');position<-fm_number(player$position)
      if(!nzchar(id)||!is.finite(position)||position!=floor(position)||position<0||position>=length(slots)||nzchar(slots[position+1L]))stop('Official lineup position missing')
      slots[position+1L]<-id
      wire[[length(wire)+1L]]<<-list(`_id`=id,position=as.integer(position),mp=isTRUE(player$multiposition),
        pr=if(is.finite(fm_number(player$buyPrice)))fm_number(player$buyPrice)else fm_number(player$value),b=bench,cpt=isTRUE(player$cpt))
      if(isTRUE(player$locked))locked[[length(locked)+1L]]<<-list(id=id,position=as.integer(position),bench=bench)
      if(!bench&&isTRUE(player$cpt))captain<<-c(captain,id)
    }
    slots
  }
  draft$starters<-parse(raw$players %||% list(),FALSE)
  draft$bench<-parse(raw$bench$players %||% list(),TRUE)
  if(length(captain)>1L)stop('Official captain is ambiguous')
  draft$captain<-if(length(captain))captain else ''
  ids<-c(draft$starters,draft$bench);ids<-ids[nzchar(ids)];if(anyDuplicated(ids))stop('Official lineup contains duplicate players')
  list(draft=draft,wire=list(strategy=strategy,players=wire),locked=locked,
    enabled=isTRUE(raw$bench$enabled),multiposition=isTRUE(raw$multiposition),
    status=fm_scalar(raw$status,''),pointSystem=raw$pointSystem,custom=raw$custom)
}
lineup_fetch_bundle <- function(auth,context,target,owner) {
  calls<-list(lineup=lineup_api_read('/1/userteam/lineup',auth,context,owner),
    roster=lineup_api_read('/1/userteam/roster',auth,context,owner),
    info=lineup_api_read('/1/userteam/information',auth,context,owner,list(type='market')),
    strategies=lineup_api_read('/5/strategy/availables',auth,context,owner))
  promises::then(do.call(promises::promise_all,calls),function(answers){
    lineup_normalize_bundle(answers,context,target)
  })
}
lineup_normalize_bundle <- function(answers,context,target) {
  official<-lineup_parse_official(answers$lineup);cfg<-answers$info$configuration %||% list()
  available<-lineup_available_formations(answers$strategies,official$draft$strategy,cfg)
  scoring<-list(point_system=official$pointSystem,custom=official$custom)
  stamp<-Sys.time();season<-league_season_context(context$championship_id,answers$info$season %||% answers$info$seasonId)
  list(version=1L,roster=lineup_roster_records(answers$roster),official=official$draft,
    rules=list(allowed=available,multiposition=official$multiposition,
      bench_enabled=official$enabled,captain_enabled=isTRUE(cfg$cpt)),
    source=list(wire=official$wire,locked=official$locked,status=official$status,block_delayed=isTRUE(cfg$blc),
      target_id=target$id %||% '',target_number=target$number %||% NA_real_,
      target_starts=if(is.null(target))''else format(target$starts_at,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')),
    season=season,scoring_version=as.character(openssl::sha256(charToRaw(jsonlite::toJSON(scoring,auto_unbox=TRUE)))),
    observed_at=format(stamp,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),
    expires_at=format(stamp+30,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))
}
lineup_available_formations <- function(answer,current,configuration=list()) {
  if(!is.list(answer)||!is.list(answer$strategies))stop('Formation availability is missing')
  bought<-vapply(answer$bought %||% list(),function(x)fm_scalar(x$st,''),character(1))
  allowed<-vapply(answer$strategies,function(strategy){
    st<-fm_scalar(strategy$st,'');pro<-strategy$pro
    if(!is.logical(pro)||length(pro)!=1L||is.na(pro))stop('Formation entitlement is unknown')
    if(!pro||st%in%bought||identical(st,current))st else ''
  },character(1))
  intersect(lineup_formations(),allowed[nzchar(allowed)])
}
lineup_modified_wire <- function(draft,roster,rules) {
  roles<-lineup_slot_roles(draft$strategy);pos<-lineup_positions(roster,rules$multiposition)
  primary<-lineup_positions(roster,FALSE)
  native<-c(GK='portero',DEF='defensa',MID='centrocampista',FWD='delantero')
  players<-list()
  for(bench in c(FALSE,TRUE)){
    ids<-if(bench)draft$bench else draft$starters
    for(i in which(nzchar(ids))){
      row<-match(ids[i],roster$id);role<-if(bench)primary[[row]][1]else roles[i]
      players[[length(players)+1L]]<-list(`_id`=ids[i],position=as.integer(i-1L),mp=FALSE,
        pr=if('buyPrice'%in%names(roster)&&is.finite(fm_number(roster$buyPrice[row])))fm_number(roster$buyPrice[row])else if('value'%in%names(roster))fm_number(roster$value[row])else NA_real_,b=bench,cpt=!bench&&rules$captain_enabled&&identical(ids[i],draft$captain))
    }
  }
  list(strategy=draft$strategy,players=players)
}
lineup_build_batch <- function(auth,championship_id,user_team_id,current,modified,changes=list()) {
  lineup_api_payload(auth,championship_id,user_team_id,list(current=current,modified=modified,changes=changes))
}
lineup_submit <- function(auth,championship_id,user_team_id,current,modified,owner,changes=list()) {
  payload<-jsonlite::toJSON(lineup_build_batch(auth,championship_id,user_team_id,current,modified,changes),auto_unbox=TRUE,null='null')
  # Deliberately one attempt. Even a rejected/uncertain response is reconciled
  # with a factual read by the module; it is never replayed automatically.
  promises::then(fm_async_http('https://api.futmondo.com/5/userteam/multichanges','POST',payload,
    c(`Content-Type`='application/json; charset=utf-8'),owner=owner,priority='interactive'),function(response){
      result<-jsonlite::fromJSON(rawToChar(response$content),simplifyVector=FALSE)
      if(isTRUE(result$answer$error)||(!is.null(result$code)&&!identical(result$code,API_CODE_OK))||
         (!is.null(result$answer$code)&&!identical(result$answer$code,API_CODE_OK)))stop('Futmondo rejected the lineup change')
      result
    })
}
lineup_locked_changes <- function(snapshot,draft) {
  locked<-snapshot$source$locked %||% list();issues<-character()
  for(entry in locked){
    ids<-if(isTRUE(entry$bench))draft$bench else draft$starters
    if(!identical(ids[as.integer(entry$position)+1L],entry$id)||
       !identical(snapshot$official$strategy,draft$strategy))issues<-c(issues,entry$id)
  }
  unique(issues)
}
lineup_changes <- function(current,modified) {
  # Mirror official player removal/placement events. Remove moved players before
  # placing anyone, so swaps never transiently duplicate a selected player.
  key<-function(player)paste(isTRUE(player$b),as.integer(player$position),sep=':')
  before<-setNames(current$players,vapply(current$players,key,character(1)))
  after<-setNames(modified$players,vapply(modified$players,key,character(1)))
  same_formation<-identical(current$strategy,modified$strategy)
  events<-list()
  event<-function(from,to,position,bench,captain=FALSE){
    value<-list(type='player',cpt=captain,position=position,isBench=bench,multiposition=FALSE)
    if(!is.null(from))value$from<-from
    if(!is.null(to))value$to<-to
    value
  }
  for(k in names(before)){
    old<-before[[k]];new<-after[[k]]
    if(same_formation&&!is.null(new)&&identical(old$`_id`,new$`_id`))next
    events[[length(events)+1L]]<-event(old$`_id`,NULL,old$position,isTRUE(old$b))
  }
  if(!same_formation)events[[length(events)+1L]]<-list(type='strategy',from=current$strategy,to=modified$strategy,cpt=FALSE,isBench=FALSE,multiposition=FALSE)
  for(k in names(after)){
    new<-after[[k]];old<-before[[k]]
    if(same_formation&&!is.null(old)&&identical(old$`_id`,new$`_id`))next
    events[[length(events)+1L]]<-event(NULL,new$`_id`,new$position,isTRUE(new$b))
  }
  old_captain<-vapply(Filter(function(player)isTRUE(player$cpt),current$players),function(player)player$`_id`,character(1))
  new_captain<-vapply(Filter(function(player)isTRUE(player$cpt),modified$players),function(player)player$`_id`,character(1))
  if(length(new_captain)&&(!identical(old_captain,new_captain)||!same_formation||length(events)))
    events[[length(events)+1L]]<-event(NULL,new_captain[[1]],NULL,FALSE,TRUE)
  events
}
