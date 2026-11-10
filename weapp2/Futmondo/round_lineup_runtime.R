# Historical lineups retain official 0-based pitch and bench positions. The API
# already applies substitutions: changed marks incoming field/outgoing bench.
round_lineup_read_state <- function(read) {
  tryCatch(list(data=read(),loading=FALSE,error=NULL),error=function(error){
    pending<-inherits(error,"shiny.silent.error")&&(!nzchar(conditionMessage(error))||grepl("Loading|Refreshing",conditionMessage(error)))
    list(data=NULL,loading=pending,error=if(pending)NULL else error)
  })
}
classification_single_round <- function(window) {
  if(length(window)==2L && all(is.finite(window)) && window[1]==window[2] && window[1]>=1 && window[1]==floor(window[1]))
    as.character(window[1]) else "all"
}
round_lineup_records <- function(items) {
  if(is.data.frame(items))return(lapply(seq_len(nrow(items)),function(i)lapply(items,function(column)
    if(is.data.frame(column))round_lineup_records(column)[[i]]else if(is.list(column))column[[i]]else column[i])))
  if(is.list(items))items else list()
}
round_lineup_slots <- function(items,size) {
  records<-round_lineup_records(items)
  if(length(records)>size)stop("Historical lineup has too many positions")
  slots<-rep(list(NULL),size)
  for(player in records){
    if(is.null(player))next
    if(!is.list(player))stop("Historical lineup position unavailable")
    position<-fm_number(player$position)
    if(!is.finite(position)||position!=floor(position)||position<0||position>=size||!is.null(slots[[position+1L]]))
      stop("Historical lineup position unavailable")
    # Only the public display fields cross the persistence boundary.
    clean<-player[intersect(c("id","name","photo","teamId","logo","position","role","points","changed","rated","zeroPointsReason","customPoints"),names(player))]
    if(is.list(player$detailedPoints))clean$detailedPoints<-list(po=lapply(
      round_lineup_records(player$detailedPoints$po),function(score)
        if(is.list(score))score[intersect(c("mode","r","p"),names(score))]else list()))
    if(is.list(player$team))clean$team<-player$team[intersect(c("id","name","logo"),names(player$team))]
    else if(nzchar(mvp_text(player$team)))clean$team<-mvp_text(player$team)
    slots[position+1L]<-list(clean)
  }
  slots
}
normalize_round_lineup <- function(answer) {
  if(!is.list(answer)||isTRUE(answer$error)||!is.null(answer$code)||!"players"%in%names(answer)||!is.list(answer$players))
    stop("Historical lineup unavailable")
  strategy<-mvp_text(answer$strategy)
  if(!grepl("^[0-9]+-[0-9]+-[0-9]+$",strategy))stop("Historical formation unavailable")
  parts<-suppressWarnings(as.integer(strsplit(strategy,"-",fixed=TRUE)[[1]]))
  if(length(parts)!=3L||anyNA(parts)||any(parts<0)||sum(parts)!=10L)stop("Historical formation unavailable")
  field<-round_lineup_slots(answer$players,11L)
  bench<-round_lineup_slots(answer$bench$players %||% list(),4L)
  list(strategy=strategy,rows=c(rev(parts),1L),players=field,bench=bench,
    status=mvp_text(answer$status),pointSystem=mvp_text(answer$pointSystem,"press"),negative=rival_flag(answer$negative))
}
round_lineup_clean_answer <- function(answer) {
  normalized<-normalize_round_lineup(answer)
  list(strategy=normalized$strategy,players=Filter(Negate(is.null),normalized$players),
    bench=list(players=Filter(Negate(is.null),normalized$bench)),status=normalized$status,
    pointSystem=normalized$pointSystem,negative=normalized$negative)
}
round_lineup_scope <- function(context) {
  context$competition_id<-fm_scalar(context$competition_id,"unknown")
  context[c("account_id","championship_id","user_team_id","competition_id","season","scoring_version")]
}
# v1 lost score inputs; v2 also discarded the official flat club/crest fields.
round_lineup_snapshot_id <- function(context,round_id) paste0("rival-cache:round-lineup-v4:",
  as.character(openssl::sha256(serialize(list(team=context$user_team_id,round=round_id,competition=fm_scalar(context$competition_id,"unknown"),season=context$season,scoring=context$scoring_version),NULL,version=2))))
persist_round_lineup_snapshot <- function(data,context) {
  context$competition_id<-fm_scalar(context$competition_id,"unknown")
  if(!is.list(data)||isTRUE(data$from_saved)||!is.null(attr(data,"fetch_error")))return(invisible(FALSE))
  answer<-tryCatch(round_lineup_clean_answer(data$answer),error=function(e)NULL)
  stamp<-fm_time(attr(data,"observed_at"))
  if(is.null(answer)||length(stamp)!=1L||is.na(stamp)||!nzchar(mvp_text(data$round_id)))return(invisible(FALSE))
  payload<-list(kind="classification_round_lineup",version=4L,scope=round_lineup_scope(context),round_id=data$round_id,
    answer=answer,source="futmondo",source_observed_at=rival_finance_stamp(stamp),
    valid_until=rival_finance_stamp(stamp+300),completeness="complete")
  record<-rival_finance_evidence(context$account_id,context$championship_id,context$season,"eligibility",
    round_lineup_snapshot_id(context,data$round_id),payload,stamp,competition_id=fm_scalar(context$competition_id,"unknown"))
  persist_rival_finance_evidence(list(record),context)
}
read_saved_round_lineup <- function(context,round_id,now=Sys.time()) {
  context$competition_id<-fm_scalar(context$competition_id,"unknown")
  query<-list(account_id=paste0("eq.",context$account_id),championship_id=paste0("eq.",context$championship_id),
    competition_id=paste0("eq.",fm_scalar(context$competition_id,"unknown")),season=paste0("eq.",context$season),evidence_type="eq.eligibility",source_id=paste0("eq.",round_lineup_snapshot_id(context,round_id)),
    select="account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at,payload",limit=1L)
  saved<-tryCatch(supabase_get("rival_finance_evidence_latest",query),error=function(e)NULL)
  if(!is.data.frame(saved)||nrow(saved)!=1L)return(NULL)
  tryCatch({
    record<-rival_evidence_decode(saved)[[1]];p<-record$payload;scope<-round_lineup_scope(context)
    stamp<-fm_time(p$source_observed_at);until<-fm_time(p$valid_until)
    if(!is.list(p)||as.numeric(object.size(p))>256*1024L||!identical(record$evidence_type,"eligibility")||!identical(p$kind,"classification_round_lineup")||!identical(as.integer(p$version),4L)||
       !identical(p$completeness,"complete")||!identical(p$source,"futmondo")||!identical(mvp_text(p$round_id),as.character(round_id))||
       !is.list(p$scope)||!setequal(names(p$scope),names(scope))||
       !all(vapply(names(scope),function(k)identical(mvp_text(p$scope[[k]]),mvp_text(scope[[k]])),logical(1)))||
       !all(vapply(c("account_id","championship_id","competition_id","season"),function(k)identical(mvp_text(record[[k]]),mvp_text(context[[k]])),logical(1)))||
       !identical(mvp_text(record$source_id),round_lineup_snapshot_id(context,round_id))||
       length(stamp)!=1L||is.na(stamp)||stamp>now||length(until)!=1L||is.na(until)||
       abs(as.numeric(difftime(until,stamp,units="secs"))-300)>0.01||
       !identical(as.numeric(fm_time(record$observed_at)),as.numeric(stamp)))return(NULL)
    result<-list(round_id=round_id,answer=round_lineup_clean_answer(p$answer),from_saved=TRUE,valid_until=as.numeric(until))
    attr(result,"observed_at")<-stamp;attr(result,"source")<-"saved Futmondo historical lineup";result
  },error=function(e)NULL)
}
get_round_lineup_snapshot <- function(login,championship_id,user_team_id,round_id) {
  if(!nzchar(mvp_text(round_id))||!nzchar(mvp_text(user_team_id)))stop("Historical lineup identity unavailable")
  context<-resource_context(login,championship_id,user_team_id,season=league_season_context(championship_id))
  get_cached_data(paste0("round_lineup_snapshot_v4_",championship_id,"_",user_team_id,"_",round_id),{
    saved<-read_saved_round_lineup(context,round_id)
    if(!is.null(saved)&&as.numeric(Sys.time())<saved$valid_until)return(saved)
    # A validated expired snapshot can paint while the source refresh runs.
    # The adapter marks this preview stale; only the completed read is cached.
    if(!is.null(saved)&&exists("publish_resource_progress",mode="function"))publish_resource_progress(saved)
    result<-tryCatch({
      payload<-list(header=list(token=login[["token"]],userid=login[["userid"]]),
        query=list(championshipId=championship_id,userteamId=user_team_id,round=round_id),answer=list())
      response<-futmondo_post("https://api.futmondo.com/1/userteam/roundlineup",body=jsonlite::toJSON(payload,auto_unbox=TRUE),
        httr::add_headers(.headers=c("Content-Type"="application/json; charset=utf-8")))
      body<-httr::content(response)
      if(httr::status_code(response)!=200L||!is.list(body)||isTRUE(body$error)||
         (!is.null(body$code)&&!identical(body$code,API_CODE_OK)))stop("Historical lineup unavailable")
      answer<-round_lineup_clean_answer(body$answer)
      out<-list(round_id=round_id,answer=answer,valid_until=as.numeric(Sys.time())+300)
      attr(out,"observed_at")<-Sys.time();out
    },error=function(e){
      if(is.null(saved))stop(e)
      attr(saved,"fetch_status")<-"stale";attr(saved,"fetch_error")<-resource_failure(e,resource="get_round_lineup_snapshot");saved
    })
    result
  },timeout_sec=300,context=context)
}
round_lineup_player_score <- function(player,point_system="press",status="") {
  points<-NA_real_;provisional<-FALSE
  if(is.list(player)){
    if(nzchar(mvp_text(player$zeroPointsReason)))points<-0
    else if(identical(point_system,"custom")){
      # Custom leagues report their score separately, even for rated players.
      points<-fm_number(player$customPoints)
      provisional<-is.finite(points)&&identical(player$rated,FALSE)
    }
    else if(isTRUE(player$rated))points<-fm_number(player$points)
    else if(is.list(player$detailedPoints)){
      # Official RoundLineUp._getPlayerPoints selects the mode AND chosen role.
      scores<-Filter(function(score)is.list(score)&&nzchar(mvp_text(player$role))&&
        identical(mvp_text(score$mode),point_system)&&identical(mvp_text(score$r),mvp_text(player$role)),
        round_lineup_records(player$detailedPoints$po))
      if(length(scores)==1L)points<-fm_number(scores[[1]]$p)
      provisional<-is.finite(points)
    }else if(is.null(player$rated))points<-fm_number(player$points)
    # An explicit unrated flag without a matching score is not a numeric zero.
  }
  list(points=points,provisional=is.finite(points)&&(provisional||status%in%c("running","playing")))
}
round_lineup_player_ui <- function(player,bench=FALSE,index,point_system="press",status="",pitch_role="") {
  occupied<-is.list(player)&&nzchar(mvp_text(player$id))
  changed<-occupied&&isTRUE(player$changed)
  # Pitch slots identify the assigned role, including secondary-position starts.
  # Bench role is the historical main position; never use role2 or bench order.
  position<-if(bench&&occupied)analytics_player_positions(data.frame(role=mvp_text(player$role)))[[1]]else pitch_role
  position<-if(length(position)&&nzchar(position[1]))position[1]else ""
  label<-if(occupied)mvp_text(player$name,paste("Player",mvp_text(player$id)))else "Empty position"
  score<-round_lineup_player_score(if(occupied)player else NULL,point_system,status)
  points<-score$points
  photo<-if(occupied)mvp_text(player$photo)else ""
  # The official player model uses flat team/teamId/logo fields. Keep support
  # for nested public display records without assuming every team is a list.
  team<-if(occupied)player$team else NULL
  club<-if(is.list(team))mvp_text(team$name)else mvp_text(team)
  logo<-if(occupied)mvp_text(player$logo)else ""
  if(!nzchar(logo)&&is.list(team))logo<-mvp_text(team$logo)
  asset_url<-function(value,base){
    if(!nzchar(value))return("")
    if(grepl("^https://",value,ignore.case=TRUE))return(value)
    if(grepl("^[A-Za-z0-9_.-]+$",value))paste0(base,value)else ""
  }
  photo<-asset_url(photo,"https://static01.mondocore.com/futmondo/img/faces/64/")
  logo<-asset_url(logo,TEAM_LOGO_URL)
  # Photos come from the official response; do not synthesize current-roster data.
  safe_image<-function(url)grepl("^https://",url,ignore.case=TRUE)
  shiny::div(class=paste("fm-lineup-player",if(!occupied)"fm-lineup-empty",if(changed)if(bench)"fm-lineup-out"else"fm-lineup-in"),
    `data-slot`=index,`data-player-id`=if(occupied)mvp_text(player$id)else "",title=label,
    if(bench)shiny::span(class="fm-lineup-order",paste("Bench",index)),
    shiny::div(class="fm-lineup-points",title=if(score$provisional)"Provisional points"else"Round points",
      if(is.finite(points))paste0(format(points,trim=TRUE),if(score$provisional)"*"else"")else "—"),
    shiny::div(class="fm-lineup-portrait",
      if(safe_image(photo))shiny::tags$img(src=photo,alt="",loading="lazy",onerror="this.style.display='none'")
      else shiny::span(class="fm-lineup-silhouette",if(occupied)"●"else "+",`aria-hidden`="true"),
      if(changed)shiny::span(class="fm-lineup-change",title=if(bench)"Substituted out"else"Substituted in",
        `aria-label`=if(bench)"Substituted out"else"Substituted in",if(bench)"↓"else"↑")),
    shiny::div(class="fm-lineup-name",`data-position`=if(occupied)position else "",
      title=if(occupied)paste0(if(bench)"Main position for substitutions: "else"Lineup position: ",
        if(nzchar(position))switch(position,GK="Goalkeeper",DEF="Defender",MID="Midfielder",FWD="Forward")else"unavailable"),
      if(safe_image(logo))shiny::tags$img(class="fm-lineup-crest",src=logo,alt=club,title=club,
        width=14,height=14,loading="lazy",decoding="async",onerror="this.style.display='none'"),
      shiny::span(class="fm-lineup-label",if(occupied)lineup_player_label(label)else label)))
}
round_lineup_pitch_ui <- function(answer) {
  lineup<-normalize_round_lineup(answer)
  ends<-cumsum(lineup$rows);starts<-c(1L,head(ends,-1L)+1L)
  shiny::div(class="fm-round-lineup",
    shiny::div(class="fm-lineup-heading",shiny::strong(paste("Formation",lineup$strategy)),shiny::span("Official round lineup")),
    shiny::div(class="fm-lineup-layout",
      shiny::div(class="fm-lineup-pitch",role="group",`aria-label`=paste("Pitch, formation",lineup$strategy),
      shiny::div(class="fm-lineup-markings",`aria-hidden`="true",shiny::div(class="fm-lineup-circle"),shiny::div(class="fm-lineup-goal-top"),shiny::div(class="fm-lineup-goal-bottom")),
      lapply(which(lineup$rows>0),function(row)shiny::div(class="fm-lineup-row",style=paste0("--lineup-count:",lineup$rows[row]),
        lapply(seq.int(starts[row],ends[row]),function(i)round_lineup_player_ui(lineup$players[[i]],index=i,
          point_system=lineup$pointSystem,status=lineup$status,pitch_role=c("FWD","MID","DEF","GK")[row]))))),
    shiny::div(class="fm-lineup-bench",shiny::h4("Substitutes"),shiny::div(class="fm-lineup-bench-row",
      lapply(seq_len(4L),function(i)round_lineup_player_ui(lineup$bench[[i]],bench=TRUE,index=i,
        point_system=lineup$pointSystem,status=lineup$status))))),
    shiny::p(class="fm-lineup-legend","* Provisional points · ↑ Substituted in · ↓ Substituted out · — Points unavailable. Bench order is 1–4. Substitutes can only cover their main position; secondary positions do not count."))
}
