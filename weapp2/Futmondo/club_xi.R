# StrikerAPI reads run in the external team-sheet worker. Browsing reads its saved result.
club_xi_supported <- function(competition,coverage) {
  if(!nzchar(coverage) || coverage=="not_configured")return("not_configured")
  if(!mvp_text(competition) %in% strsplit(coverage,",",fixed=TRUE)[[1]])return("unsupported")
  "ready"
}

club_xi_display_coverage <- function() {
  if(nzchar(Sys.getenv("STRIKER_API_KEY")))"504e4f584d8bec9a67000079" else "not_configured"
}

get_club_lineups <- function(login,championship_id) {
  ctx <- resource_context(login,championship_id)
  status <- club_xi_supported(ctx$competition_id,Sys.getenv("FUTMONDO_XI_COVERAGE"))
  if(status!="ready")return(list(status="ok",club_xi=list(status=status)))
  saved <- get0("fm_worker_saved_resource",mode="function",inherits=TRUE)
  fetch <- get0("fm_worker_club_xi",mode="function",inherits=TRUE)
  if(is.null(saved)||is.null(fetch))return(list(status="unavailable"))
  args <- list(championship_id=championship_id)
  calendar <- attr(saved("get_finished_rounds",args,ctx),"fixture_calendar")
  previous <- saved("get_club_lineups",args,ctx)
  now <- Sys.time()
  fields <- c("account_id","championship_id","competition_id","season")
  if(!identical(calendar$status,"ok")||!league_fixture_matches_valid(calendar$matches)||
     !all(vapply(fields,function(k)identical(calendar$scope[[k]],ctx[[k]]),logical(1)))){
    feed <- previous$club_xi %||% list()
    feed$status <- "calendar_pending"
    return(list(status="ok",club_xi=feed,valid_until=as.numeric(now)+300))
  }
  feed <- club_xi_collect(calendar$matches$rows,previous$club_xi,ctx,now,saved,fetch)
  list(status="ok",club_xi=feed,valid_until=feed$next_check)
}

club_xi_collect <- function(fixtures, prior, context, now, saved, fetch) {
  fixtures <- fixtures[!duplicated(fixtures$match_id)&!duplicated(fixtures$match_id,fromLast=TRUE),,drop=FALSE]
  unix <- as.numeric(fm_time(fixtures$kickoff))
  clock <- as.numeric(now)
  result <- list(status="ready",clubs=prior$clubs %||% list(),profiles=prior$profiles %||% list(),
    matches=list())
  # Preserve the observed sheet through the matchday, including delayed rounds.
  for(id in intersect(names(prior$matches),fixtures$match_id)){
    m <- prior$matches[[id]]
    f <- fixtures[match(id,fixtures$match_id),,drop=FALSE]
    if(lineup_match_same_fixture(m,f) && isTRUE(fm_time(f$kickoff)>now-7*86400))result$matches[[id]] <- m
  }
  window <- which(is.finite(unix) & unix<=clock+90*60 & unix>=clock-2*3600)
  last <- vapply(fixtures$match_id[window],function(id)fm_number(result$matches[[id]]$checked,0),numeric(1))
  due_at <- vapply(seq_along(window),function(k){
    i <- window[k]
    m <- result$matches[[fixtures$match_id[i]]]
    if(identical(m$status,"confirmed") && !length(m$unresolved)){
      return(Inf)
    }
    retry <- if(isTRUE(m$status %in% c("access_denied","credits_exhausted")))3600 else 180
    last[k]+retry
  },numeric(1))
  due <- window[due_at<=clock]
  upcoming <- c(clock+21600,unix[which(unix>clock+5400)]-5400,due_at[due_at>clock])
  result$next_check <- max(clock+60,min(upcoming))
  if(!length(due))return(result)
  result$next_check <- min(result$next_check,clock+if(length(due)>2L)60 else 180)
  due <- head(due[order(last[match(due,window)],unix[due])],2L)
  roster <- saved("get_championship_players",list(championship_id=context$championship_id),context)
  for(i in due){
    f <- fixtures[i,,drop=FALSE];id <- f$match_id
    m <- as.list(f[c("match_id","round_id","home_id","away_id","home_name","away_name","kickoff")])
    m$unix <- unix[i]
    teams <- c(f$home_id,f$away_id)
    players <- if(is.data.frame(roster)&&all(c("id","name","teamId")%in%names(roster)))
      roster[roster$teamId %in% teams,intersect(c("id","name","slug","teamId","number","dob"),names(roster)),drop=FALSE]
    old <- result$matches[[id]]
    response <- list(status="catalog_pending")
    if(NROW(players) && all(teams %in% players$teamId)){
      unresolved <- intersect(unlist(old$unresolved,use.names=FALSE),as.character(players$id))
      ages <- vapply(unresolved,function(p)fm_number(result$profiles[[p]]$checked,0),numeric(1))
      # Reuse player-only observations already collected by factual sync.
      # Prioritize missing corroboration, with a cap shared by both clubs.
      retry <- vapply(unresolved,function(p){
        bio <- result$profiles[[p]]
        if(nzchar(mvp_text(bio$full_name))||nzchar(mvp_text(bio$dob)))86400 else 300
      },numeric(1))
      missing <- unresolved[order(ages)]
      for(p in head(intersect(missing,unresolved[clock-ages>=retry]),3L)){
        raw <- saved("get_player_full_profile",list(player_id=p),context)
        info <- raw$profile
        if(!identical(mvp_text(raw$player$id %||% raw$player$`_id`),p))info <- NULL
        result$profiles[[p]] <- list(full_name=mvp_text(info$name),dob=mvp_text(info$dob),
          number=fm_number(info$number),checked=clock)
      }
      records <- lapply(seq_len(nrow(players)),function(k){
        p <- as.list(players[k,,drop=FALSE]);bio <- result$profiles[[as.character(p$id)]]
        if(!is.null(bio)){
          p$full_name <- bio$full_name
          if(nzchar(mvp_text(bio$dob)))p$dob <- substr(bio$dob,1,10)
          if(is.finite(bio$number))p$number <- bio$number
        }
        p
      })
      clubs <- lapply(result$clubs[intersect(names(result$clubs),teams)],function(x)
        jsonlite::fromJSON(x,simplifyVector=FALSE))
      request <- m
      request$fixture_id <- old$fixture_id
      if(!is.null(old$evidence))request$evidence <- jsonlite::fromJSON(old$evidence,simplifyVector=FALSE)
      response <- fetch(request,records,clubs)
    }
    m$checked <- clock
    m$status <- mvp_text(response$status,"unavailable")
    m$observed <- response$observed
    m$fixture_id <- if(m$status!="fixture_unmatched")response$fixture_id %||% old$fixture_id
    if(!is.null(response$evidence))m$evidence <- as.character(jsonlite::toJSON(response$evidence,
      auto_unbox=TRUE,null="null",digits=NA))
    else if(!m$status %in% c("pending","incomplete","fixture_unmatched"))m$evidence <- old$evidence
    m$unresolved <- response$unresolved %||% old$unresolved
    if(identical(m$status,"confirmed")){
      m$players <- response$players
      for(cid in intersect(teams,names(response$clubs)))result$clubs[[cid]] <-
        as.character(jsonlite::toJSON(response$clubs[[cid]],auto_unbox=TRUE,null="null",na="null",digits=NA))
    } else if(!m$status %in% c("pending","incomplete","fixture_unmatched") && !is.null(old$players)){
      # Keep the old observation and its original clock on transport failures.
      m$players <- old$players;m$observed <- old$observed;m$fixture_id <- old$fixture_id
    }
    result$matches[[id]] <- m
  }
  if(is.data.frame(roster) && nrow(roster)){
    result$profiles <- result$profiles[intersect(names(result$profiles),as.character(roster$id))]
    result$clubs <- result$clubs[intersect(names(result$clubs),unique(c(fixtures$home_id,fixtures$away_id)))]
  }
  result
}

club_xi_label <- function(feed, player, fixtures, now=Sys.time()) {
  if(!identical(feed$status,"ready")||is.null(player))return(NULL)
  club <- mvp_text(player$teamId)
  row <- which(nzchar(club) & (fixtures$home_id %in% club | fixtures$away_id %in% club))
  if(length(row)!=1L)return(NULL)
  f <- fixtures[row,,drop=FALSE]
  m <- feed$matches[[f$match_id]]
  delta <- as.numeric(difftime(fm_time(f$kickoff),now,units="mins"))
  if(!lineup_match_same_fixture(m,f)){
    if(!is.finite(delta)||delta>90||delta< -120)return(NULL)
    return(list(text="Awaiting club XI",warning=FALSE,title="Waiting for the first synced team sheet."))
  }
  verdict <- m$players[[mvp_text(player$id)]]
  observed <- fm_number(m$observed)
  if(identical(m$status,"confirmed") && is.finite(observed) && observed<=as.numeric(now)){
    label <- switch(mvp_text(verdict),starting="Confirmed starter",benched="On club bench",
      absent="Not in club’s starting XI","Player match unconfirmed")
    return(list(text=label,warning=isTRUE(verdict %in% c("absent","benched")),
      title=paste("StrikerAPI · Club announcement observed",fm_format_datetime(observed))))
  }
  text <- switch(m$status,pending="Club XI unconfirmed",catalog_pending="Waiting for club roster",
    fixture_unmatched="Fixture match unconfirmed",incomplete="Team sheet incomplete",
    confirmed="Club XI awaiting update",rate_limited="Lineup provider rate limited",
    credits_exhausted="Lineup provider credits exhausted",access_denied="Lineup provider access denied","Club XI unavailable")
  list(text=text,warning=FALSE,title=if(is.finite(observed))paste(text,"· Last observed",fm_format_datetime(observed))else text)
}


club_xi_source <- function(session,active,login,championship,scope) {
  read <- factual_read_adapter(session,"get_club_lineups",login,championship,is_active=active,state_only=TRUE)
  coverage <- club_xi_display_coverage()
  shiny::reactive({
    s <- scope()
    shiny::req(s)
    status <- club_xi_supported(s$ctx$competition_id,coverage)
    if(status!="ready")return(list(status=status))
    if(!fm_worker_configured()||!isTRUE(fm_worker_mode(s$ctx$account_id)))return(list(status="offline"))
    snapshot <- read(login=login(),championship_id=championship())
    snapshot$data$club_xi %||% list(status="loading")
  })
}
