# The editor targets the next kickoff. Match browsing also keeps the latest
# started round reachable, including the interval between matchdays.
lineup_match_rounds <- function(rounds, now=Sys.time()) {
  next_round <- lineup_target(rounds,now)
  if(!is.data.frame(rounds)||!all(c('round_id','round_number','begin_process')%in%names(rounds)))
    return(list(next_round=next_round))
  starts <- fm_time(rounds$begin_process)
  started <- which(!is.na(starts)&starts<=now&!is.na(rounds$round_id)&nzchar(rounds$round_id)&is.finite(rounds$round_number))
  latest <- NULL
  if(length(started)){
    i <- started[which.max(starts[started])]
    latest <- list(id=rounds$round_id[i],number=rounds$round_number[i],starts_at=starts[i])
  }
  list(next_round=next_round,latest=latest)
}

lineup_match_parse <- function(match, fixture, observed_at) {
  if(!is.list(match)||!all(vapply(match[c('r','h','a','info')],is.list,logical(1)))||
     !is.list(match$h$id)||!is.list(match$a$id)||!identical(mvp_text(match$`_id`),fixture$match_id)||
     !identical(mvp_text(match$r$`_id`),fixture$round_id)||
     !identical(mvp_text(match$h$id$`_id`),fixture$home_id)||
     !identical(mvp_text(match$a$id$`_id`),fixture$away_id))return(NULL)
  status <- mvp_text(match$st)
  minute <- fm_number(match$info$min)
  score <- c(home=fm_number(match$h$score),away=fm_number(match$a$score))
  players <- function(side) {
    if(!is.list(side$ps)||length(side$ps)>60L)return(NULL)
    rows <- lapply(side$ps,function(entry){
      if(!is.list(entry)||!is.list(entry$p)||!is.list(entry$s))return(NULL)
      p <- entry$p
      s <- entry$s
      id <- mvp_text(p$`_id`)
      name <- mvp_text(p$name)
      participation <- mvp_text(s$st)
      if(!nzchar(id)||!nzchar(name)||!nzchar(participation))return(NULL)
      if(!participation%in%c('st','bc'))return(list(id=id,participation=participation))
      stats <- mvp_records(s$po)
      if(length(stats)>100L)stats <- list()
      stats <- Filter(is.list,stats)
      points <- data.frame(
        mode=vapply(stats,function(x)mvp_text(x$mode),character(1)),
        role=vapply(stats,function(x)mvp_text(x$r),character(1)),
        value=vapply(stats,function(x)fm_number(x$p),numeric(1)))
      points <- points[nzchar(points$mode)&is.finite(points$value),,drop=FALSE]
      list(id=id,name=name,participation=participation,role=mvp_text(s$role),
        position=fm_number(s$pos),points=points)
    })
    ids <- vapply(rows,function(x)mvp_text(x$id),character(1))
    if(any(!nzchar(ids))||anyDuplicated(ids))return(NULL)
    Filter(function(p)p$participation%in%c('st','bc'),rows)
  }
  home <- players(match$h)
  away <- players(match$a)
  has_points <- function(rows) {
    starter_count <- sum(vapply(rows,function(p)p$participation=='st',logical(1)))
    starter_count==11L && all(vapply(rows,function(p)nrow(p$points)>0L,logical(1)))
  }
  list(status=status,minute=minute,score=score,kickoff=fixture$kickoff,
    home_id=fixture$home_id,away_id=fixture$away_id,round_id=fixture$round_id,
    home=home,away=away,observed_at=observed_at,source_updated_at=mvp_text(match$uAt),
    complete=nzchar(status)&&all(is.finite(score))&&has_points(home)&&has_points(away))
}

lineup_match_same_fixture <- function(detail, fixture) {
  is.list(detail)&&identical(detail$round_id,fixture$round_id)&&
    identical(detail$home_id,fixture$home_id)&&identical(detail$away_id,fixture$away_id)&&
    identical(detail$kickoff,fixture$kickoff)
}

# Use the saved fixture calendar and last acknowledged snapshot through the
# private worker bridge. A browser or standalone caller has no source path.
get_lineup_match_details <- function(login, championship_id) {
  saved <- get0('fm_worker_saved_resource',mode='function',inherits=TRUE)
  result <- list(status='unavailable',matches=list())
  if(is.null(saved))return(result)
  now <- Sys.time()
  ctx <- resource_context(login,championship_id)
  args <- list(championship_id=championship_id)
  rounds <- saved('get_finished_rounds',args,ctx)
  calendar <- attr(rounds,'fixture_calendar')
  if(!identical(calendar$status,'ok')||!league_fixture_matches_valid(calendar$matches)||
     !all(vapply(c('account_id','championship_id','competition_id','season'),
       function(field)identical(calendar$scope[[field]],ctx[[field]]),logical(1))))stop('Incomplete source acquisition')

  refs <- lineup_match_rounds(rounds,now)
  ids <- vapply(Filter(Negate(is.null),refs),function(x)x$id,character(1))
  fixtures <- calendar$matches$rows
  fixtures <- fixtures[fixtures$round_id%in%ids,,drop=FALSE]
  fixtures <- fixtures[!duplicated(fixtures$match_id)&!duplicated(fixtures$match_id,fromLast=TRUE),,drop=FALSE]
  prior <- saved('get_lineup_match_details',args,ctx)
  old <- prior$matches
  result$status <- 'ok'
  result$attempts <- prior$attempts[intersect(names(prior$attempts),fixtures$match_id)] %||% list()
  due <- rep(Inf,nrow(fixtures))
  kickoff <- as.numeric(fm_time(fixtures$kickoff))
  for(i in seq_len(nrow(fixtures))){
    row <- fixtures[i,,drop=FALSE]
    id <- row$match_id
    previous <- old[[id]]
    if(!lineup_match_same_fixture(previous,row))previous <- NULL
    if(!is.null(previous))result$matches[[id]] <- previous
    if(is.na(kickoff[i])||kickoff[i]>as.numeric(now)||!nzchar(row$home_id)||!nzchar(row$away_id))next
    # Revisit finished results for provider corrections, then reuse indefinitely
    # while the round remains visible. Retry incomplete results on later sweeps.
    if(isTRUE(previous$complete)&&identical(previous$status,'F')){
      observed <- as.numeric(fm_time(previous$observed_at))
      if(observed>=kickoff[i]+48*3600&&!isTRUE(previous$refresh_incomplete))next
      due[i] <- observed+6*3600
    }else{
      due[i] <- if(is.null(previous))kickoff[i] else as.numeric(fm_time(previous$observed_at))+60
    }
    last_attempt <- fm_number(result$attempts[[id]])
    if(is.finite(last_attempt))due[i] <- max(due[i],last_attempt+60)
  }
  # Give recent/live games priority over initial backfill. Oldest due first
  # within each group gives every simultaneous fixture a turn.
  eligible <- which(due<=as.numeric(now))
  recent <- kickoff[eligible]>=as.numeric(now)-3*3600
  for(i in head(eligible[order(!recent,due[eligible])],6L)){
    row <- fixtures[i,,drop=FALSE]
    id <- row$match_id
    response <- mvp_api_answer(login,'https://api.futmondo.com/2/match/fetchmatch',list(matchId=id))
    stamp <- attr(response,'observed_at') %||% now
    detail <- if(identical(response$status,'ok'))lineup_match_parse(response$answer$match,row,stamp)
    previous <- result$matches[[id]]
    if(!is.null(detail)&&(!isTRUE(previous$complete)||isTRUE(detail$complete))){
      result$matches[[id]] <- detail
      result$attempts[[id]] <- NULL
    }else{
      result$attempts[[id]] <- as.numeric(now)
      if(!is.null(previous)){
        previous$refresh_incomplete <- TRUE
        result$matches[[id]] <- previous
      }
    }
  }
  attr(result,'complete') <- all(vapply(fixtures$match_id[!is.na(kickoff)&kickoff<=as.numeric(now)],
    function(id)isTRUE(result$matches[[id]]$complete)&&!isTRUE(result$matches[[id]]$refresh_incomplete),logical(1)))
  result
}

lineup_match_points <- function(player, mode, custom=NULL) {
  points <- player$points
  if(!is.data.frame(points)||!nrow(points))return(NA_real_)
  lookup <- function(key, exact_role=TRUE) {
    rows <- points[points$mode==key,,drop=FALSE]
    if(exact_role)rows <- rows[rows$role==player$role,,drop=FALSE]
    if(!nrow(rows))return(NA_real_)
    if(exact_role&&length(unique(rows$value))>1L)return(NA_real_)
    rows$value[1L]
  }
  if(!identical(mode,'custom'))return(lookup(mode))
  stats_mode <- mvp_text(custom$stats)
  media <- mvp_records(custom$media)
  weights <- vapply(media,function(x)fm_number(x$pct),numeric(1))
  if(any(!is.finite(weights)|weights<0|weights>100))return(NA_real_)
  active <- which(weights>0)
  values <- vapply(media[active],function(x)lookup(mvp_text(x$media),FALSE),numeric(1))
  if(nzchar(stats_mode)){
    stats <- lookup(stats_mode)
    if(is.na(stats))stats <- lookup(stats_mode,FALSE)
    values <- c(values*weights[active]/100,stats)
  }else values <- values*weights[active]/100
  if(!length(values)||anyNA(values))NA_real_ else sum(values)
}

lineup_match_status <- function(detail, fixture, now=Sys.time()) {
  status <- mvp_text(detail$status,mvp_text(fixture$status))
  if(!nzchar(status))status <- mvp_text(fixture$status)
  minute <- fm_number(detail$minute)
  if(status=='F')return('Finished')
  if(is.finite(minute)&&minute>0)return(paste0('Reported match minute · ',minute,'′'))
  if(isTRUE(fm_time(fixture$kickoff)>now))return('Scheduled')
  'Awaiting match status'
}
