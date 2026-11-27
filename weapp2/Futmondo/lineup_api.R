# Explicit factual reads and one-shot user-authorized batch lineup submission.
lineup_api_payload <- function(auth,championship_id,user_team_id,extra=list()) list(
  header=list(token=auth[['token']],userid=auth[['userid']]),
  query=c(list(championshipId=championship_id,userteamId=user_team_id),extra),answer=list())
# Read envelopes retain the original worker clocks; decoding answer must not
# turn a saved observation into a newly acquired source result.
lineup_api_answer <- function(envelope, worker=FALSE, observed_at=Sys.time()) {
  if(!is.list(envelope)||!is.list(envelope$answer)||isTRUE(envelope$answer$error)||
     (!is.null(envelope$code)&&!identical(envelope$code,API_CODE_OK))||
     (!is.null(envelope$answer$code)&&!identical(envelope$answer$code,API_CODE_OK)))stop('Lineup source rejected this request')
  answer<-envelope$answer
  if(worker){
    status<-attr(envelope,'worker_acquisition_status')
    if(!isTRUE(status%in%c('ok','empty'))||!is.null(attr(envelope,'fetch_error'))||
       isTRUE(attr(envelope,'worker_completeness')%in%c('partial','unavailable')))stop('Lineup source observation is incomplete')
    for(field in c('observed_at','fetched_at','valid_until','fetch_status','worker_acquisition_status','worker_completeness'))
      attr(answer,field)<-attr(envelope,field)
    attr(answer,'worker_replay')<-TRUE
  }else{
    answer<-observed_data(answer,observed_at=observed_at)
    attr(answer,'valid_until')<-as.numeric(fm_time(observed_at))+30
  }
  answer
}

# The lineup endpoint can omit pointSystem while retaining the full custom
# settings. Resolve that omission only against independently observed, exact
# league identity and matching custom evidence; never guess a scoring system.
lineup_resolve_scoring <- function(lineup,context) {
  point_system<-lineup$pointSystem
  omitted<-is.null(point_system)||length(point_system)==0L||
    (is.character(point_system)&&length(point_system)==1L&&!is.na(point_system)&&!nzchar(trimws(point_system)))
  if(!omitted)return(lineup)
  row<-league_context_metadata(context$championship_id,context$account_id,allow_stale=TRUE)
  fields<-c('account_id','championship_id','competition_id','season','scoring_version')
  if(is.null(row)||!isTRUE(row$complete)||!is.list(row$payload)||
     !all(vapply(fields,function(field)fm_context_known(context[[field]])&&
       identical(fm_scalar(row[[field]]),fm_scalar(context[[field]])),logical(1))))stop('Lineup scoring context is unavailable')
  reference<-fm_scalar(row$payload$point_system,'')
  if(!identical(reference,'custom')||!is.list(row$payload$custom)||!is.list(lineup$custom))stop('Lineup scoring configuration is unavailable')
  expected<-fm_scoring_version(reference,row$payload$custom)
  actual<-fm_scoring_version(reference,lineup$custom)
  if(!fm_context_known(expected)||!identical(expected,context$scoring_version)||!identical(actual,expected))stop('Lineup source scoring context changed')
  lineup$pointSystem<-reference
  lineup
}

# A complete bundle is no newer or longer-lived than its oldest/first-expiring
# component. Legacy direct parser callers have no envelope and keep the live TTL.
lineup_bundle_observation <- function(answers,now=Sys.time()) {
  stamps<-deadlines<-numeric();states<-character();replayed<-FALSE
  for(answer in answers){
    worker<-isTRUE(attr(answer,'worker_replay'));replayed<-replayed||worker
    raw_stamp<-attr(answer,'observed_at');raw_deadline<-attr(answer,'valid_until')
    if(worker&&(is.null(raw_stamp)||is.null(raw_deadline)))stop('Lineup source timestamps are unavailable')
    stamp<-fm_time(raw_stamp %||% now);until<-fm_time(raw_deadline %||% (as.numeric(stamp)+30))
    if(length(stamp)!=1L||is.na(stamp)||length(until)!=1L||is.na(until)||until<stamp||stamp>now+60)stop('Lineup source timestamps are invalid')
    status<-attr(answer,'fetch_status') %||% 'ok'
    if(!isTRUE(status%in%c('ok','empty','stale'))||!is.null(attr(answer,'fetch_error')))stop('Lineup source observation is incomplete')
    stamps<-c(stamps,as.numeric(stamp));deadlines<-c(deadlines,as.numeric(until));states<-c(states,status)
  }
  list(observed_at=format(fm_time(min(stamps)),'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),
    expires_at=format(fm_time(min(deadlines)),'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),
    fetch_status=if(any(states=='stale')||min(deadlines)<=as.numeric(now))'stale'else'ok',worker_replay=replayed)
}
lineup_api_read <- function(route,auth,context,owner,extra=list(),worker_live=FALSE) {
  if(!worker_live&&exists("fm_worker_configured",mode="function")&&fm_worker_configured()&&!identical(fm_worker_mode(auth[['userid']]),FALSE)){
    args<-list(route=route,query=c(list(championshipId=context$championship_id,userteamId=context$user_team_id),extra))
    return(promises::then(fm_worker_fetch("get_worker_http_read",args,context,owner),function(value)lineup_api_answer(value,worker=TRUE)))
  }
  payload<-lineup_api_payload(auth,context$championship_id,context$user_team_id,extra)
  if(route=='/2/user/activechampionships')payload$query<-list(excludeGeneral=FALSE,includeProphets=TRUE)
  body<-jsonlite::toJSON(payload,auto_unbox=TRUE,null='null')
  promises::then(fm_async_read_http(paste0('https://api.futmondo.com',route),'POST',body,
    c(`Content-Type`='application/json; charset=utf-8'),owner=owner,priority='interactive'),function(response){
      if(length(response$content)>2*1024^2)stop('Lineup response is too large')
      parsed<-jsonlite::fromJSON(rawToChar(response$content),simplifyVector=FALSE)
      lineup_api_answer(parsed)
    })
}
lineup_roster_records <- function(raw) {
  if(!is.list(raw)||length(raw)>2000L)stop('Squad is invalid')
  records<-lapply(raw,function(player){
    id<-fm_scalar(player$id %||% player$`_id`,'');if(!nzchar(id))stop('Squad player identity missing')
    team<-player$team
    list(id=id,name=fm_scalar(player$name,id),role=fm_scalar(player$role,''),role2=fm_scalar(player$role2,''),
      status=fm_scalar(player$status,''),available=if(is.logical(player$available)&&length(player$available)==1L)player$available else NA,
      points=fm_number(player$points),photo=fm_scalar(player$photo,''),buyPrice=fm_number(player$buyPrice),value=fm_number(player$value),
      club=if(is.list(team))fm_scalar(team$name,'')else fm_scalar(team,''),
      logo=fm_scalar(if(is.list(team))team$logo %||% player$logo else player$logo,''))
  })
  ids<-vapply(records,`[[`,character(1),'id');if(anyDuplicated(ids))stop('Duplicate squad player identity')
  records
}
lineup_roster_frame <- function(data) {
  rows<-data$roster %||% list()
  if(!length(rows))return(data.frame(id=character(),name=character(),role=character(),role2=character(),status=character(),photo=character(),club=character(),logo=character(),points=numeric()))
  result<-dplyr::bind_rows(rows)
  if(!'logo'%in%names(result))result$logo<-''
  if(!'points'%in%names(result))result$points<-rep(NA_real_,nrow(result))
  attr(result,'observed_at')<-fm_time(data$observed_at)
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
lineup_target_matches <- function(actual, round_id, kickoff, number=NULL) {
  !is.null(actual) && identical(actual$id, round_id) &&
    (is.null(number) || isTRUE(actual$number == number)) &&
    isTRUE(abs(as.numeric(actual$starts_at) - as.numeric(fm_time(kickoff))) < 0.001)
}
# The scoped match list returns one round and its real matches, including the
# dates of games already played. Check it only at explicit submission time.
lineup_round_started <- function(answer, round_id, now=Sys.time()) {
  if(!identical(mvp_text(answer$round$`_id`),round_id))return(NA)
  matches <- mvp_records(answer$matches)
  if(!length(matches)||length(matches)>50L||!all(vapply(matches,is.list,logical(1))))return(NA)
  dates <- fm_time(vapply(matches,function(m)mvp_text(m$date),character(1)))
  finished <- vapply(matches,function(m)identical(mvp_text(m$status),'F'),logical(1))
  if(any(finished)||any(dates<=now,na.rm=TRUE))return(TRUE)
  if(anyNA(dates))return(NA)
  FALSE
}
lineup_source_closed <- function(status) identical(tolower(mvp_text(status)), 'closed')

# These public source IDs identify the announced 2026–27 LaLiga round even
# while the active league metadata retains its previous season label.
lineup_laliga_j23 <- function(competition_id,target) {
  if(!identical(target$id,'6a4af7f2ae633549bd0f0471') || !isTRUE(target$number==23))return(FALSE)
  if(!fm_context_known(competition_id))return(NA)
  identical(competition_id,'504e4f584d8bec9a67000079')
}
lineup_j23_window_issue <- function(competition_id,target,answer,now=Sys.time(),opening_only=FALSE) {
  scoped <- lineup_laliga_j23(competition_id,target)
  if(is.na(scoped))return('Futmondo has not confirmed the competition for this round 23 lineup. Your draft is retained.')
  if(!scoped)return(character())
  # Real Sociedad–Real Madrid and Barcelona–Atlético are the two advanced
  # fixtures. The first of the other eight determines the reopening date.
  advanced <- c('6a4af7f2ae633549bd0f047a','6a4af7f2ae633549bd0f0474')
  matches <- mvp_records(answer$matches)
  ids <- vapply(matches,function(m)mvp_text(m$id %||% m$`_id`),character(1))
  dates <- fm_time(vapply(matches,function(m)mvp_text(m$date),character(1)))
  unconfirmed <- 'Futmondo has not confirmed the round 23 lineup window. Your draft is retained.'
  if(!identical(mvp_text(answer$round$`_id`),target$id) || any(!nzchar(ids)) ||
     anyDuplicated(ids) || !all(advanced %in% ids))return(unconfirmed)
  early <- dates[ids %in% advanced]
  if(!isTRUE(target$reopened) && lineup_target_matches(target,target$id,min(early)))return(character())
  if(opening_only || !isTRUE(target$reopened))return(unconfirmed)
  remaining <- dates[!ids %in% advanced]
  if(length(remaining)!=8L || anyNA(remaining) || !lineup_target_matches(target,target$id,min(remaining)))return(unconfirmed)
  day <- format(min(remaining),'%Y-%m-%d',tz='Europe/Madrid')
  opens <- as.POSIXct(paste(day,'10:00:00'),tz='Europe/Madrid')
  if(as.numeric(now)<as.numeric(opens))
    return(paste('Round 23 reopens at 10:00 Madrid time on',paste0(day,'.'),'Your draft is retained.'))
  character()
}
lineup_bundle_routes <- c(lineup='/1/userteam/lineup', roster='/1/userteam/roster',
  info='/1/userteam/information', strategies='/5/strategy/availables')
lineup_route_extra <- function(route) if(route=='/1/userteam/information')list(type='market')else list()

lineup_fetch_bundle <- function(auth,context,target,owner,is_current=function()TRUE,worker_live=FALSE,readback=FALSE) {
  calls <- lapply(lineup_bundle_routes,function(route)
    lineup_api_read(route,auth,context,owner,lineup_route_extra(route),worker_live=worker_live))
  verify_target <- worker_live && !is.null(target)
  if(verify_target) {
    calls$active <- lineup_api_read('/2/user/activechampionships',auth,context,owner,worker_live=TRUE)
    if(!readback) calls$matches <- lineup_api_read('/1/match/list',auth,context,owner,list(roundId=target$id),worker_live=TRUE)
  }
  promises::then(do.call(promises::promise_all,calls),function(answers){
    if(!isTRUE(is_current()))stop(structure(list(message='Session request cancelled',call=NULL),class=c('fm_async_cancelled','error','condition')))
    if(verify_target){
      actual <- lineup_active_target(answers$active,context$championship_id,context$user_team_id,
        competition_id=if(fm_context_known(context$competition_id))context$competition_id)
      if(!lineup_target_matches(actual,target$id,target$starts_at,target$number))
        stop(structure(list(message='Futmondo has not confirmed this lineup round and deadline. Your draft is retained.',call=NULL),
          class=c('lineup_target_changed','error','condition')))
    }
    if(verify_target && !readback){
      if(lineup_source_closed(answers$lineup$status))
        stop(structure(list(message='Futmondo has closed lineup changes. Your draft is retained.',call=NULL),
          class=c('lineup_target_changed','error','condition')))
      started <- lineup_round_started(answers$matches,target$id)
      if(is.na(started))stop(structure(list(message='Futmondo has not confirmed which games in this round have started. Your draft is retained.',call=NULL),
        class=c('lineup_target_changed','error','condition')))
      target$reopened <- started || isTRUE(target$reopened)
    }
    data<-lineup_normalize_bundle(answers,context,target)
    if(verify_target && !readback){
      issue <- lineup_j23_window_issue(data$competition_id,target,answers$matches)
      if(length(issue))stop(structure(list(message=issue,call=NULL),
        class=c('lineup_target_changed','error','condition')))
    }
    # This direct async bundle does not cross record_acquired_resource. Publish
    # verified scoring only after every component succeeds in its own generation.
    if(!isTRUE(data$source$worker_replay)&&exists('remember_league_scoring',mode='function')){
      row<-remember_league_scoring(context,answers$lineup,attr(answers$lineup,'observed_at') %||% fm_time(data$observed_at))
      if(!is.null(row))persist_league_context(row,context)
    }
    data
  })
}
lineup_normalize_bundle <- function(answers,context,target) {
  official<-lineup_parse_official(lineup_resolve_scoring(answers$lineup,context));cfg<-answers$info$configuration %||% list()
  available<-lineup_available_formations(answers$strategies,official$draft$strategy,cfg)
  source_league<-if(is.list(answers$info$league))answers$info$league else list()
  competition<-fm_scalar(source_league$id %||% source_league$`_id`,fm_scalar(context$competition_id,'unknown'))
  season<-fm_scalar(answers$info$season %||% answers$info$seasonId %||% source_league$season,fm_scalar(context$season,'unknown'))
  for(field in c('competition_id','season')){
    expected<-fm_scalar(context[[field]],'unknown');actual<-if(field=='competition_id')competition else season
    if(expected!='unknown'&&actual!='unknown'&&!identical(expected,actual))stop('Lineup source context changed')
  }
  observation<-lineup_bundle_observation(answers)
  list(version=2L,roster=lineup_roster_records(answers$roster),official=official$draft,
    rules=list(allowed=available,multiposition=official$multiposition,
      bench_enabled=official$enabled,captain_enabled=isTRUE(cfg$cpt)),
    source=list(wire=official$wire,locked=official$locked,status=official$status,block_delayed=isTRUE(cfg$blc),
      delayed_edits_allowed=if(identical(answers$info$championshipMode,'classic'))TRUE else
        if(identical(answers$info$championshipMode,'social')&&is.logical(cfg$blc)&&length(cfg$blc)==1L)!cfg$blc else NA,
      match_scoring=list(mode=official$pointSystem,custom=official$custom),
      fetch_status=observation$fetch_status,worker_replay=observation$worker_replay,
      target_id=target$id %||% '',target_number=target$number %||% NA_real_,reopened=isTRUE(target$reopened),
      target_starts=if(is.null(target))''else format(target$starts_at,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')),
    competition_id=competition,season=season,scoring_version=fm_scoring_version(official$pointSystem,official$custom),
    observed_at=observation$observed_at,expires_at=observation$expires_at)
}
lineup_available_formations <- function(answer,current,configuration=list()) {
  if(!is.list(answer)||!is.list(answer$strategies))stop('Formation availability is missing')
  bought<-vapply(answer$bought %||% list(),function(x)fm_scalar(x$st,''),character(1))
  # League admin setting (official client: configuration.allowExtraStrategies)
  # unlocks paid formations for every team in the league.
  extra<-isTRUE(configuration$allowExtraStrategies)
  allowed<-vapply(answer$strategies,function(strategy){
    st<-fm_scalar(strategy$st,'');pro<-strategy$pro
    # The source omits pro for standard formations; its client checks pro ===
    # true for paid entries. Reject malformed values rather than guessing.
    if(!is.null(pro)&&(!is.logical(pro)||length(pro)!=1L||is.na(pro)))stop('Formation entitlement is unknown')
    if(!isTRUE(pro)||extra||st%in%bought||identical(st,current))st else ''
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
         (!is.null(result$answer$code)&&!identical(result$answer$code,API_CODE_OK)))stop('LigaRadar rejected the lineup change')
      result
    })
}
# Matchday reopening preserves the formation and permits losing the original captain.
lineup_reopened_issues <- function(snapshot,draft,target) {
  if(!isTRUE(target$reopened)&&!isTRUE(snapshot$source$reopened))return(character())
  if(!isTRUE(snapshot$source$delayed_edits_allowed))return(
    if(identical(snapshot$source$delayed_edits_allowed,FALSE))'This league keeps the XI saved at the first kickoff.'
    else 'Waiting for confirmation that this league allows postponed-match edits.')
  issues <- character()
  if(!identical(draft$strategy,snapshot$official$strategy))
    issues <- c(issues,'Keep the formation saved at the start of this round.')
  if(nzchar(draft$captain)&&!identical(draft$captain,snapshot$official$captain))
    issues <- c(issues,'A replacement captain cannot be assigned during a reopened round.')
  if(!any(nzchar(snapshot$official$starters))&&any(nzchar(draft$starters)))
    issues <- c(issues,'Futmondo cannot refill an empty XI during this reopening.')
  issues
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

# Explicit manual-action preflight/readback, compatible with existing offline seams.
lineup_fetch_live <- function(auth,context,target,owner,is_current=function()TRUE,readback=FALSE){
 args<-list(auth=auth,context=context,target=target,owner=owner,is_current=is_current)
 if('worker_live'%in%names(formals(lineup_fetch_bundle)))args$worker_live<-TRUE
 else if(exists('fm_worker_configured',mode='function')&&fm_worker_configured()&&!identical(fm_worker_mode(auth[['userid']]),FALSE))stop('Live lineup verification is unavailable')
 if('readback'%in%names(formals(lineup_fetch_bundle)))args$readback<-readback
 do.call(lineup_fetch_bundle,args)
}
