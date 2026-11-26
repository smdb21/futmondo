# Official round discovery, cooperative MVP loading and conservative bonus attribution.
# Contracts and examples: docs/round_mvps_module.md and docs/league_finances.md.
mvp_text <- function(x, default="") {
  if(!is.atomic(x) || length(x)!=1L || is.na(x)) return(default)
  trimws(as.character(x))
}
mvp_result <- function(status, rows = data.frame(), reason = "", code = "", complete = FALSE) {
  list(status=status, rows=rows, reason=reason, code=code, complete=complete)
}

# Input is the shared, scoped award snapshot with one current revision per round.
round_award_player_stats <- function(rows, championship_id) {
  players <- data.frame(player_id = character(), player_name = character(), best_xi = integer(), mvp = integer())
  players$award_positions <- character()
  players$award_team <- players$award_logo <- character()
  appearances <- data.frame(round = character(), award_team_id = character(), award_team = character(),
    award_logo = character(), is_mvp = logical())
  out <- list(players = players, appearances = appearances, rounds = 0L, unconfirmed_rounds = 0L, incomplete_rounds = 0L,
    unsaved_rounds = 0L, scope_issue = "")
  if (!is.data.frame(rows) || !nrow(rows)) return(out)
  if ("championship_id" %in% names(rows))
    rows <- rows[rows$championship_id %in% championship_id, , drop = FALSE]
  if (!nrow(rows)) return(out)

  if (!all(c("player_id", "player_name", "is_mvp", "is_finished") %in% names(rows)) ||
      !any(c("round_id", "round_number") %in% names(rows))) {
    out$scope_issue <- "The saved selections lack the fields needed to verify round awards."
    return(out)
  }
  round_key <- if ("round_id" %in% names(rows)) trimws(as.character(rows$round_id)) else {
    number <- suppressWarnings(as.numeric(as.character(rows$round_number)))
    ifelse(is.finite(number), as.character(number), NA_character_)
  }
  if (anyNA(round_key) || any(!nzchar(round_key))) out$scope_issue <- "Some award rows have no round identity."
  round_key[!nzchar(round_key)] <- NA_character_
  groups <- split(seq_len(nrow(rows)), round_key)
  player_id <- trimws(as.character(rows$player_id))
  flag <- function(x) tolower(trimws(as.character(x)))
  mvp <- flag(rows$is_mvp)
  is_mvp <- mvp %in% c("true", "t", "1")
  mvp_known <- is_mvp | mvp %in% c("false", "f", "0")
  finished <- flag(rows$is_finished) %in% c("true", "t", "1")
  saved <- if ("persistence" %in% names(rows)) rows$persistence %in% "Saved" else rep(FALSE, nrow(rows))
  states <- vapply(groups, function(i) {
    ids <- player_id[i]
    if (length(i) != 11L || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids) ||
        !all(mvp_known[i]) || sum(is_mvp[i]) != 1L) return("incomplete")
    if (!all(finished[i])) return("unconfirmed")
    if (!all(saved[i])) return("unsaved")
    "counted"
  }, character(1))
  out$incomplete_rounds <- sum(states == "incomplete")
  out$unconfirmed_rounds <- sum(states == "unconfirmed")
  out$unsaved_rounds <- sum(states == "unsaved")
  selected <- unlist(groups[states == "counted"], use.names = FALSE)
  if (!length(selected)) return(out)
  for (field in intersect(c("season", "scoring_version"), names(rows))) {
    if (length(unique(rows[[field]][selected])) > 1L) {
      out$scope_issue <- "The saved selections span different seasons or scoring settings. A single ranking is unavailable."
      return(out)
    }
  }

  # Prefer the latest numbered selection with a recorded name when labels change.
  if ("round_number" %in% names(rows)) {
    number <- suppressWarnings(as.numeric(as.character(rows$round_number[selected])))
    selected <- selected[order(number, decreasing = TRUE, na.last = TRUE)]
  }
  ids <- player_id[selected]
  names_seen <- trimws(as.character(rows$player_name[selected]))
  named <- !is.na(names_seen) & nzchar(names_seen)
  players <- unique(ids)
  names_latest <- names_seen[named][match(players, ids[named])]
  names_latest[is.na(names_latest)] <- players[is.na(names_latest)]
  index <- match(ids, players)
  # The official player_position is a pitch slot (0–10). Named roles identify
  # which football positions the player represented in the counted selections.
  roles <- as.character(rows$player_role[selected])
  observed_roles <- unique(roles)
  normalized <- analytics_player_positions(data.frame(role = observed_roles), multiposition = TRUE)
  positions <- rep(NA_character_, length(players))
  for (position in c("GK", "DEF", "MID", "FWD")) {
    raw_roles <- observed_roles[vapply(normalized, function(x) position %in% x, logical(1))]
    winners <- unique(index[roles %in% raw_roles])
    previous <- positions[winners]
    positions[winners] <- ifelse(is.na(previous), position, paste(previous, position, sep = " / "))
  }
  out$rounds <- sum(states == "counted")
  out$players <- data.frame(player_id = players, player_name = names_latest,
    best_xi = tabulate(index, length(players)), mvp = tabulate(index[is_mvp[selected]], length(players)))
  # Keep the club label and asset from one awarded appearance, even after a transfer.
  club_fields <- c("award_team", "award_logo")
  for (field in setdiff(club_fields, names(rows))) rows[[field]] <- ""
  clubs <- rows[selected, club_fields, drop = FALSE]
  clubs[is.na(clubs)] <- ""
  observed <- which(nzchar(clubs$award_team) | nzchar(clubs$award_logo))
  latest <- clubs[observed[match(players, ids[observed])], , drop = FALSE]
  latest[is.na(latest)] <- ""
  out$players[club_fields] <- latest
  out$players$award_positions <- positions
  out$appearances <- data.frame(round = round_key[selected],
    award_team_id = if ("award_team_id" %in% names(rows)) rows$award_team_id[selected] else "",
    award_team = clubs$award_team, award_logo = clubs$award_logo, is_mvp = is_mvp[selected])
  out
}

# Club membership comes from the awarded appearance. The catalog can identify a
# recorded club ID or crest, including after the player has moved.
round_award_club_stats <- function(appearances, catalog = NULL) {
  clubs <- data.frame(club = character(), crest = character(), best_xi = integer(),
    mvp = integer(), represented_rounds = integer())
  result <- list(clubs = clubs, missing_xi = 0L, missing_mvp = 0L, needs_catalog = FALSE)
  if (!nrow(appearances)) return(result)

  field <- function(rows, column) {
    if (!column %in% names(rows)) return(rep("", NROW(rows)))
    value <- trimws(as.character(rows[[column]]))
    value[is.na(value)] <- ""
    value
  }
  facts <- data.frame(id = field(appearances, "award_team_id"), name = field(appearances, "award_team"),
    logo = field(appearances, "award_logo"))
  if (is.data.frame(catalog) && nrow(catalog)) {
    facts <- rbind(facts, unique(data.frame(id = field(catalog, "teamId"), name = field(catalog, "team"),
      logo = field(catalog, "logo"))))
  }
  logos <- unique(facts$logo)
  crest <- vapply(logos, lineup_fixture_crest_url, character(1), USE.NAMES = FALSE)[match(facts$logo, logos)]
  # Official selections can carry the crest filename stem in the team field.
  slug <- nzchar(crest) & facts$name == sub("\\.[[:alnum:]]+$", "", basename(crest)) &
    facts$name == tolower(facts$name)
  named <- nzchar(facts$name)
  has_id <- nzchar(facts$id)
  name_key <- tolower(facts$name)

  # Blank means no association, NA means contradictory associations.
  lookup <- function(from, to, query) {
    keep <- nzchar(from) & nzchar(to)
    pairs <- unique(data.frame(from = from[keep], to = to[keep]))
    pairs$to[duplicated(pairs$from) | duplicated(pairs$from, fromLast = TRUE)] <- NA_character_
    at <- match(query, pairs$from)
    values <- pairs$to[at]
    values[is.na(at)] <- ""
    values
  }

  key <- ifelse(has_id, paste0("id:", facts$id), "")
  named_only <- !has_id & named
  matched_id <- lookup(name_key, key, name_key[named_only])
  key[named_only] <- ifelse(is.na(matched_id), "", ifelse(nzchar(matched_id), matched_id,
    paste0("name:", name_key[named_only])))

  # Only unnamed or filename-labeled appearances can borrow another label by
  # crest. A shared asset cannot combine two distinct explicit club names.
  anchor <- has_id | (named & !slug)
  slug_only <- !has_id & slug
  linked <- lookup(crest[anchor], key[anchor], crest[slug_only])
  key[slug_only] <- ifelse(is.na(linked), "", ifelse(nzchar(linked), linked, key[slug_only]))
  crest_only <- !has_id & !named
  linked <- lookup(crest, key, crest[crest_only])
  key[crest_only] <- ifelse(is.na(linked), "", linked)

  appearance <- seq_len(nrow(appearances))
  group <- key[appearance]
  # Prefer a recorded human-readable name. Award observations precede the
  # catalog if both supply one. Without a match the recorded slug is retained.
  labels <- which(named & nzchar(key))
  labels <- labels[order(slug[labels])]
  label_at <- labels[match(group, key[labels])]
  known <- nzchar(group) & !is.na(label_at)
  result$missing_xi <- sum(!known)
  result$missing_mvp <- sum(!known & appearances$is_mvp)
  result$needs_catalog <- is.null(catalog) && any((!known | slug[label_at] %in% TRUE) &
    (has_id[appearance] | nzchar(crest[appearance])))
  if (!any(known)) return(result)

  distinct <- unique(group[known])
  label <- facts$name[labels[match(distinct, key[labels])]]
  duplicate_label <- duplicated(tolower(label)) | duplicated(tolower(label), fromLast = TRUE)
  label[duplicate_label] <- paste0(label[duplicate_label], " (", sub("^id:", "", distinct[duplicate_label]), ")")
  images <- which(nzchar(crest) & nzchar(key))
  logo_at <- match(distinct, key[images])
  index <- match(group[known], distinct)
  represented <- !duplicated(data.frame(index = index, round = appearances$round[known]))
  result$clubs <- data.frame(club = label, crest = ifelse(is.na(logo_at), "", crest[images[logo_at]]),
    best_xi = tabulate(index, length(distinct)), mvp = tabulate(index[appearances$is_mvp[known]], length(distinct)),
    represented_rounds = tabulate(index[represented], length(distinct)))
  result
}

mvp_records <- function(x) {
  if (is.data.frame(x)) return(lapply(seq_len(nrow(x)), function(i) as.list(x[i,,drop=FALSE])))
  if (is.list(x)) x else list()
}

mvp_api_answer <- function(login, url, query) {
  if (isTRUE(getOption("futmondo.offline", FALSE))) return(mvp_result("unavailable", reason="Offline data source is not configured."))
  tryCatch({
    response <- futmondo_post(url, body=jsonlite::toJSON(list(
      header=list(token=login[["token"]],userid=login[["userid"]]), query=query, answer=list()), auto_unbox=TRUE),
      httr::add_headers("Content-Type"="application/json; charset=utf-8"))
    status <- httr::status_code(response)
    if (status %in% c(401L,403L)) return(mvp_result("authentication", reason="LigaRadar access was denied. Log in again."))
    if (status < 200L || status >= 300L) return(mvp_result("error", reason=paste("LigaRadar request failed (HTTP",status,").")))
    body <- httr::content(response)
    answer <- body$answer
    if (!is.list(answer)) return(mvp_result("unsupported", reason="The returned LigaRadar format is not supported yet."))
    if (isTRUE(answer$error) || (!is.null(answer$code) && !identical(answer$code,API_CODE_OK))) {
      code <- fm_scalar(answer$code, "")
      if (!grepl("^[a-zA-Z0-9_.-]{0,80}$", code)) code <- ""
      return(mvp_result("rejected", reason=paste0("LigaRadar rejected the request",if(nzchar(code)) paste0(" (",code,")") else "","."), code=code))
    }
    result <- list(status="ok", answer=answer)
    attr(result,"observed_at") <- attr(response,"observed_at")
    result
  }, error=function(e) {
    # Async HTTP/parser failures arrive as conditions before an httr response.
    failure<-resource_failure(e,resource=url)
    if(identical(failure$category,"http")&&!is.null(failure$http_status)){
      if(failure$http_status%in%c(401L,403L))return(mvp_result("authentication",reason="LigaRadar access was denied. Log in again."))
      return(mvp_result("error",reason=paste("LigaRadar request failed (HTTP",failure$http_status,").")))
    }
    if(identical(failure$category,"parser"))return(mvp_result("unsupported",reason="LigaRadar returned a malformed data response."))
    mvp_result("connection",reason="LigaRadar data could not be loaded because the connection failed. Refresh to retry.")
  })
}

# The current lineup write has no round selector. Only the active championship
# response identifies the round Futmondo has opened for this team.
lineup_active_target <- function(active, championship_id, user_team_id, now=Sys.time(), competition_id=NULL) {
  teams <- Filter(function(x) is.list(x) && is.list(x$userteam) &&
    identical(mvp_text(x$id %||% x$`_id`),mvp_text(championship_id)) &&
    identical(mvp_text(x$userteam$id %||% x$userteam$`_id`),mvp_text(user_team_id)),
    mvp_records(active$championships))
  if(!nzchar(mvp_text(championship_id)) || !nzchar(mvp_text(user_team_id)) || length(teams)!=1L)return(NULL)
  league <- teams[[1]]$league
  league_id <- if(is.list(league))mvp_text(league$id %||% league$`_id`) else mvp_text(league)
  if(!nzchar(league_id) || (!is.null(competition_id) && !identical(league_id,competition_id)))return(NULL)
  active_rounds <- Filter(function(x) is.list(x) && identical(mvp_text(x$championshipId),league_id),
    mvp_records(active$rounds))
  if(length(active_rounds)!=1L)return(NULL)
  r <- active_rounds[[1]]
  id <- mvp_text(r$`_id` %||% r$id)
  number <- fm_number(r$number)
  starts <- fm_time(mvp_text(r$beginProcess))
  if(!nzchar(id) || !is.finite(number) || number<1 || number!=floor(number) ||
     !isTRUE(starts>now))return(NULL)
  list(id=id,number=number,starts_at=starts)
}

# A split is observable when a whole other round fits between two kickoffs
# of this round. Keep the original fixture and round IDs throughout.
league_fixture_date_groups <- function(times) {
  empty <- data.frame(round_id=character(),begin=as.POSIXct(character(),tz='UTC'),
    end=as.POSIXct(character(),tz='UTC'),index=integer(),groups=integer())
  if(!length(times))return(empty)
  first <- vapply(times,min,numeric(1))
  last <- vapply(times,max,numeric(1))
  dplyr::bind_rows(lapply(names(times),function(id){
    dates <- sort(unique(times[[id]]))
    cuts <- vapply(seq_len(length(dates)-1L),function(i)
      any(first>dates[i] & last<dates[i+1L]),logical(1))
    start <- c(1L,which(cuts)+1L)
    stop <- c(which(cuts),length(dates))
    data.frame(round_id=id,begin=fm_time(dates[start]),end=fm_time(dates[stop]),
      index=seq_along(start),groups=length(start))
  }))
}

lineup_fixture_windows <- function(rounds,matches) {
  if(!league_fixture_matches_valid(matches))return(league_fixture_date_groups(list()))
  complete <- intersect(rounds$round_id,matches$rounds$round_id[matches$rounds$complete])
  rows <- matches$rows[matches$rows$round_id %in% complete,,drop=FALSE]
  league_fixture_date_groups(split(as.numeric(fm_time(rows$kickoff)),rows$round_id))
}

lineup_calendar_windows <- function(rounds,include_partial=FALSE) {
  windows <- attr(rounds,'fixture_windows',exact=TRUE)
  if(is.null(windows))windows <- lineup_fixture_windows(rounds,attr(rounds,'fixture_calendar')$matches)
  windows$partial <- rep(FALSE,nrow(windows))
  if(include_partial){
    calendar <- attr(rounds,'fixture_calendar')
    matches <- calendar$matches
    partial <- matches$rounds$round_id[matches$rounds$complete %in% FALSE]
    if(length(partial) && identical(calendar$status,'ok') && league_fixture_matches_valid(matches)){
      # A dated match can be shown even when other matches lack kickoff
      # times. Its date certifies neither a round deadline nor completion.
      rows <- matches$rows[matches$rows$round_id %in% partial & nzchar(matches$rows$kickoff),,drop=FALSE]
      if(nrow(rows)){
        dates <- split(as.numeric(fm_time(rows$kickoff)),rows$round_id)
        windows <- rbind(windows,data.frame(round_id=names(dates),begin=fm_time(vapply(dates,min,numeric(1))),
          end=fm_time(vapply(dates,max,numeric(1))),index=1L,groups=1L,partial=TRUE))
      }
    }
  }
  # Partial calendars may still have an independently supplied round start.
  missing <- !rounds$round_id %in% windows$round_id
  if(any(missing)){
    starts <- fm_time(rounds$begin_process[missing])
    windows <- rbind(windows,data.frame(round_id=rounds$round_id[missing],begin=starts,end=starts,index=1L,groups=1L,partial=FALSE))
  }
  windows$number <- rounds$round_number[match(windows$round_id,rounds$round_id)]
  valid <- !is.na(windows$begin) & is.finite(windows$number) & !is.na(windows$round_id) & nzchar(windows$round_id)
  windows <- windows[valid,,drop=FALSE]
  windows[order(windows$begin,windows$round_id),,drop=FALSE]
}

# A mid-season break can separate two instalments even when no whole matchday
# fits between them. Use the saved, fixture-confirmed gap and its actual dates.
home_round_windows <- function(rounds, matches) {
  windows <- lineup_calendar_windows(rounds, include_partial=TRUE)
  gap_ids <- if ("resume_at" %in% names(rounds)) rounds$round_id[is.finite(rounds$resume_at)] else
    names(award_calendar_gaps(matches))
  for (id in gap_ids) {
    games <- matches$rows[matches$rows$round_id %in% id,,drop=FALSE]
    dates <- fm_time(games$kickoff)
    final <- games$status %in% "F"
    if (!any(final) || all(final) || anyNA(dates)) next
    last <- max(dates[final])
    resume <- min(dates[!final])
    i <- which(windows$round_id==id & windows$begin<=last & windows$end>=resume)
    if (length(i)!=1L || last>=resume) next
    rest <- windows[i,,drop=FALSE]
    rest$begin <- resume
    windows$end[i] <- last
    windows <- rbind(windows, rest)
  }
  windows
}

home_round_phase <- function(round, window, matches, now) {
  if (isTRUE(round$is_finished) && !isTRUE(round$partially_played)) return("Closed")
  games <- matches$rows[matches$rows$round_id %in% round$round_id,,drop=FALSE]
  dates <- fm_time(games$kickoff)
  final <- games$status %in% "F"
  coverage <- matches$rounds
  complete <- round$round_id %in% coverage$round_id[coverage$complete %in% TRUE]
  if (complete && length(final) && all(final)) return("Awaiting round closure")
  in_group <- !is.na(dates) & dates>=window$begin & dates<=window$end
  if (any(final[in_group])) {
    if (all(final[in_group]) && any(!final & (is.na(dates) | dates>window$end)))
      return("Awaiting remaining fixtures")
    if (any(!final & in_group & dates>now)) return("In progress")
  }
  if (isTRUE(round$running)) return("In progress")
  if (isTRUE(round$partially_played)) return("Awaiting remaining fixtures")
  "Awaiting round status"
}

# Home follows fixture groups chronologically. Closed/partial describes the
# fantasy round; an editable lineup marker alone does not establish play.
home_round_context <- function(rounds, now=Sys.time()) {
  result <- list(current=NULL, upcoming=NULL, changes_at=NULL)
  now <- fm_time(now)
  if (!is.data.frame(rounds) || !nrow(rounds) || length(now)!=1L || is.na(now) ||
      !all(c("round_id","round_number","begin_process") %in% names(rounds)) ||
      anyDuplicated(rounds$round_id)) return(result)
  status <- tolower(rounds$status %||% rep("",nrow(rounds)))
  finished <- rounds$is_finished %||% rep(NA,nrow(rounds))
  partial <- (rounds$partially_played %||% rep(FALSE,nrow(rounds))) %in% TRUE
  finished <- !partial & (finished %in% TRUE |
    (is.na(finished) & status %in% c("done","finished","closed","final","completed","complete","ended")))
  calendar <- attr(rounds,"fixture_calendar",exact=TRUE)
  windows <- home_round_windows(rounds, calendar$matches)
  windows <- windows[order(windows$begin,windows$round_id),,drop=FALSE]
  row <- match(windows$round_id, rounds$round_id)
  rounds$is_finished <- finished
  rounds$running <- status %in% c("running","active","playing","in_progress")

  future <- which(windows$begin>now & !finished[row])
  if (length(future)) {
    next_group <- future[1L]
    result$upcoming <- list(round_number=windows$number[next_group], starts_at=windows$begin[next_group])
    result$changes_at <- result$upcoming$starts_at
  }
  undated <- !rounds$round_id %in% windows$round_id & is.finite(rounds$round_number)
  active <- which(undated & rounds$running & !finished & !partial)
  started <- unname(which(windows$begin<=now))
  undated_current <- if (length(active)==1L) list(round_number=rounds$round_number[active], status="In progress")
  if (!length(started)) {
    closed <- which(undated & finished)
    if (length(closed)>1L) closed <- closed[rounds$round_id[closed] %in% calendar$scope$current_round_id]
    if (!is.null(undated_current)) result$current <- undated_current else if (length(closed)==1L)
      result$current <- list(round_number=rounds$round_number[closed], status="Closed")
    return(result)
  }
  latest_start <- max(windows$begin[started])
  # A shorter overlapping round can finish while a regular matchday continues.
  # Retain only groups whose fixture windows actually overlap the latest start.
  candidates <- started[windows$begin[started]==latest_start | windows$end[started]>now]
  phases <- vapply(candidates, function(i) home_round_phase(rounds[row[i],,drop=FALSE],
    windows[i,,drop=FALSE],calendar$matches,now), character(1))
  latest <- windows$begin[candidates]==latest_start
  eligible <- which(phases=="In progress" & (!windows$partial[candidates] | latest))
  if (!length(eligible)) eligible <- which(latest)
  # At the same kickoff prefer an otherwise equal source-identified round.
  marker <- calendar$scope$current_round_id
  selected <- tail(eligible[order(windows$begin[candidates[eligible]],
    windows$round_id[candidates[eligible]] %in% marker)],1L)
  result$current <- list(round_number=windows$number[candidates[selected]], status=phases[selected])
  # Without a kickoff an old 'running' marker cannot move its round ahead
  # of a later numbered matchday already observed on the calendar.
  if (!is.null(undated_current) && phases[selected]!="In progress" &&
      undated_current$round_number>max(windows$number[started])) result$current <- undated_current
  # Kickoffs within an overlapping or partly played group can change the
  # projection too. The caller wakes once at the earliest relevant boundary.
  games <- calendar$matches$rows
  dates <- fm_time(games$kickoff[games$round_id %in% windows$round_id[candidates] & !games$status %in% "F"])
  deadlines <- c(dates[!is.na(dates) & dates>now],windows$end[candidates],result$changes_at)
  future <- deadlines[deadlines>now]
  if (length(future)) result$changes_at <- min(future)
  result
}

normalize_mvp_catalog <- function(active, leagues, championship_id) {
  unknown <- function(reason) list(status="unsupported",rounds=empty_finished_rounds(),complete=FALSE,reason=reason)
  matches <- Filter(function(x) is.list(x) && identical(mvp_text(x$id %||% x$`_id`),as.character(championship_id)), mvp_records(active$championships))
  if (length(matches)!=1L) return(unknown("The selected championship could not be matched to a league."))
  champ <- matches[[1]]; league <- champ$league
  lid <- if(is.list(league)) mvp_text(league$id %||% league$`_id`) else mvp_text(league)
  selected <- Filter(function(x) is.list(x) && nzchar(lid) && identical(mvp_text(x$id %||% x$`_id`),lid),mvp_records(leagues))
  if(length(selected)!=1L || is.null(selected[[1]]$rounds)) return(unknown("We cannot determine completed rounds from the available LigaRadar league catalog."))
  rounds <- mvp_records(selected[[1]]$rounds)
  if(!length(rounds)) return(unknown("LigaRadar returned an empty round catalog; historical coverage is unknown."))
  metadata <- c(mvp_records(active$rounds),mvp_records(champ$rounds))
  metadata <- Filter(function(round)is.list(round)&&
    (!nzchar(mvp_text(round$championshipId))||mvp_text(round$championshipId)%in%c(lid,as.character(championship_id))),metadata)
  rows <- dplyr::bind_rows(lapply(rounds,function(r) {
    if(!is.list(r)) r <- list()
    id <- mvp_text(r$`_id` %||% r$id)
    matched <- Filter(function(x) is.list(x) && nzchar(id) && identical(mvp_text(x$`_id` %||% x$id),id),metadata)
    number <- fm_number(r$number)
    if(!is.finite(number) && length(matched)==1L) number <- fm_number(matched[[1]]$number)
    status <- tolower(mvp_text(r$status))
    detail <- if(length(matched)==1L)matched[[1]]else list()
    flags <- c(r,detail)[intersect(c("isFinished","finished","isClosed","closed","isCompleted","completed","finalized"),names(c(r,detail)))]
    explicit_false <- any(vapply(flags,function(x) identical(x,FALSE) || identical(x,0L) || identical(x,0) || tolower(mvp_text(x)) %in% c("false","f","0"),logical(1)))
    # Compact league/list future/next statuses can be stale even for the
    # current round. They establish neither finality nor publication eligibility.
    finished <- if(round_is_finished(r) || round_is_finished(detail) || identical(status,"done")) TRUE else if(explicit_false || status %in% c("active","playing","in_progress","running")) FALSE else NA
    begin <- mvp_text(r$beginProcess)
    if(!nzchar(begin) && length(matched)==1L) begin <- mvp_text(matched[[1]]$beginProcess)
    data.frame(round_id=id,round_number=number,begin_process=begin,is_finished=finished,status=status)
  }))
  valid <- nzchar(rows$round_id) & !duplicated(rows$round_id)
  complete <- all(valid) && !anyNA(rows$is_finished)
  rows <- rows[valid,,drop=FALSE]
  team_id <- mvp_text(champ$userteam$id %||% champ$userteam$`_id`)
  # Retain the source's editable deadline even when it has already passed.
  # Consumers compare it to their own clock when restoring a saved catalog.
  attr(rows,'lineup_active') <- list(target=lineup_active_target(active,championship_id,team_id,
    now=as.POSIXct(-Inf,origin='1970-01-01',tz='UTC'),competition_id=lid))
  active_details<-Filter(function(round){
    is.list(round)&&mvp_text(round$`_id` %||% round$id)%in%rows$round_id&&
      (!nzchar(mvp_text(round$championshipId))||mvp_text(round$championshipId)%in%c(lid,as.character(championship_id)))
  },mvp_records(active$rounds))
  anchors<-unique(vapply(active_details,function(round)fm_number(round$number),numeric(1)))
  anchors<-anchors[is.finite(anchors)&anchors>=1&anchors==floor(anchors)]
  list(status=if(complete) "ok" else "partial",rounds=rows,complete=complete,identity_complete=all(valid),
    league_id=lid,user_team_id=team_id,
    current_round_id=if(length(active_details)==1L)mvp_text(active_details[[1]]$`_id` %||% active_details[[1]]$id)else "",
    current_round_number=if(length(anchors)==1L)anchors else NA_real_,
    reason=if(!all(valid)) "The round catalog contains missing or duplicate round identities." else if(any(!is.finite(rows$round_number))) "Some round numbers are unavailable." else "")
}

get_finished_round_catalog <- function(login, championship_id) {
  tryCatch(get_cached_data(paste0("mvp_catalog_",championship_id), {
    active <- mvp_api_answer(login,ACTIVE_CHAMPIONSHIPS_URL,list(excludeGeneral=FALSE,includeProphets=TRUE))
    if(active$status!="ok") return(c(active[c("status","reason","code","complete")],list(rounds=empty_finished_rounds())))
    leagues <- mvp_api_answer(login,"https://api.futmondo.com/2/league/list", "")
    if(leagues$status!="ok") return(c(leagues[c("status","reason","code","complete")],list(rounds=empty_finished_rounds())))
    normalize_mvp_catalog(active$answer,leagues$answer,championship_id)
  }, context=resource_context(login=login,championship_id=championship_id)), error=function(e) list(status="connection",rounds=empty_finished_rounds(),complete=FALSE,
    reason="Round discovery failed. Refresh to retry."))
}

# The official client uses this bulk source for its numbered match calendar.
# Persist compact source metadata and normalized fixtures, never authentication.
award_calendar_rows_valid <- function(rows) {
  is.data.frame(rows)&&nrow(rows)>0L&&nrow(rows)<=120L&&
    identical(names(rows),c("round_id","round_number","is_finished","status"))&&
    is.character(rows$round_id)&&!anyNA(rows$round_id)&&all(nzchar(rows$round_id))&&!anyDuplicated(rows$round_id)&&
    is.numeric(rows$round_number)&&all(is.finite(rows$round_number)&rows$round_number>=1&rows$round_number==floor(rows$round_number))&&
    !anyDuplicated(rows$round_number)&&is.logical(rows$is_finished)&&is.character(rows$status)&&!anyNA(rows$status)
}
# Persisted JSONB object keys need not follow R's data-frame column order.
award_calendar_rows_decode <- function(value) {
  columns<-c("round_id","round_number","is_finished","status")
  rows<-tryCatch(dplyr::bind_rows(mvp_records(value)),error=function(e)NULL)
  if(!is.data.frame(rows)||!setequal(names(rows),columns))return(NULL)
  rows[,columns,drop=FALSE]
}
# The official match-calendar consumer groups rounds[].matches[].info.date.
# Retain every valid fixture timestamp; a start needs all returned fixtures dated.
award_fixture_calendar <- function(records) {
  empty<-data.frame(round_id=character(),begin_process=character(),stringsAsFactors=FALSE)
  empty$fixture_dates<-I(vector("list",0L));empty$all_fixtures_dated<-logical()
  dplyr::bind_rows(c(list(empty),lapply(mvp_records(records),function(round){
    if(!is.list(round)||!nzchar(mvp_text(round$`_id` %||% round$id)))return(NULL)
    fixtures<-mvp_records(round$matches)
    if(!length(fixtures)||length(fixtures)>50L)return(NULL)
    dates<-vapply(fixtures,function(match)if(is.list(match)&&is.list(match$info))mvp_text(match$info$date)else "",character(1))
    valid<-!is.na(fm_time(dates));complete<-all(valid)
    if(!any(valid))return(NULL)
    values<-unique(dates[valid]);values<-values[order(fm_time(values))]
    row<-data.frame(round_id=mvp_text(round$`_id` %||% round$id),begin_process=if(complete)values[1]else "",stringsAsFactors=FALSE)
    row$fixture_dates<-I(list(values));row$all_fixtures_dated<-complete
    row
  })))
}
award_fixture_calendar_decode <- function(value) {
  records<-mvp_records(value)
  if(!length(records))return(award_fixture_calendar(list()))
  rows<-dplyr::bind_rows(lapply(records,function(record){
    if(!is.list(record))return(NULL)
    row<-data.frame(round_id=mvp_text(record$round_id),begin_process=mvp_text(record$begin_process),stringsAsFactors=FALSE)
    row$fixture_dates<-I(list(unlist(record$fixture_dates,use.names=FALSE)))
    row$all_fixtures_dated<-identical(record$all_fixtures_dated,TRUE)
    row
  }))
  rows
}
award_fixture_calendar_valid <- function(rows) {
  is.data.frame(rows)&&nrow(rows)<=120L&&identical(names(rows),c("round_id","begin_process","fixture_dates","all_fixtures_dated"))&&
    is.character(rows$round_id)&&!anyNA(rows$round_id)&&all(nzchar(rows$round_id))&&!anyDuplicated(rows$round_id)&&
    is.character(rows$begin_process)&&!anyNA(rows$begin_process)&&is.list(rows$fixture_dates)&&
    is.logical(rows$all_fixtures_dated)&&!anyNA(rows$all_fixtures_dated)&&all(vapply(seq_len(nrow(rows)),function(i){
      dates<-unlist(rows$fixture_dates[[i]],use.names=FALSE);begin<-rows$begin_process[i]
      is.character(dates)&&length(dates)>0L&&length(dates)<=50L&&!anyNA(fm_time(dates))&&
        if(rows$all_fixtures_dated[i])nzchar(begin)&&!is.na(fm_time(begin))&&fm_time(begin)==min(fm_time(dates))else !nzchar(begin)
    },logical(1)))
}
# Public official client: rounds[].matches[] has _id/id, h/a club objects,
# info.date and st. Whitelist display facts instead of retaining raw responses.
league_fixture_empty <- function(version=2L) {
  rows<-data.frame(round_id=character(),match_id=character(),home_id=character(),home_name=character(),home_logo=character(),
    away_id=character(),away_name=character(),away_logo=character(),kickoff=character(),status=character(),stringsAsFactors=FALSE)
  if(identical(version,1L)||identical(version,1))rows[c('home_logo','away_logo')]<-NULL
  rows
}
league_fixture_matches_current <- function(value) is.list(value)&&(identical(value$version,2L)||identical(value$version,2))
league_fixture_matches <- function(records) {
  rounds<-list();rows<-list()
  for(round in mvp_records(records)){
    if(!is.list(round))next
    id<-mvp_text(round$`_id` %||% round$id);if(!nzchar(id))next
    matches<-mvp_records(round$matches);count<-if(is.list(round$matches))length(matches)else NA_integer_
    parsed<-league_fixture_empty()
    if(!is.na(count)&&count>0L&&count<=50L)parsed<-dplyr::bind_rows(c(list(parsed),lapply(matches,function(match){
      if(!is.list(match))return(NULL)
      club<-function(side){
        value<-match[[side]];if(!is.list(value))value<-list()
        name<-mvp_text(value$name);if(!nzchar(name))name<-mvp_text(value$shortname)
        list(id=mvp_text(value$`_id` %||% value$id),name=name,logo=mvp_text(value$logo))
      }
      home<-club('h');away<-club('a')
      date<-if(is.list(match$info))mvp_text(match$info$date)else ''
      if(is.na(fm_time(date)))date<-''
      data.frame(round_id=id,match_id=mvp_text(match$`_id` %||% match$id),
        home_id=home$id,home_name=home$name,home_logo=home$logo,away_id=away$id,away_name=away$name,away_logo=away$logo,
        kickoff=date,status=mvp_text(match$st),stringsAsFactors=FALSE)
    })))
    # Conflicting identities do not become repeated games or a complete schedule.
    duplicate<-duplicated(parsed$match_id)|duplicated(parsed$match_id,fromLast=TRUE)
    parsed<-parsed[nzchar(parsed$match_id)&!duplicate,,drop=FALSE]
    complete<-!is.na(count)&&count>0L&&count<=50L&&nrow(parsed)==count&&
      all(nzchar(parsed$home_id)&nzchar(parsed$away_id)&parsed$home_id!=parsed$away_id&
        nzchar(parsed$home_name)&nzchar(parsed$away_name)&nzchar(parsed$kickoff))
    rounds[[length(rounds)+1L]]<-data.frame(round_id=id,fixture_count=if(!is.na(count)&&count<=50L)count else NA_integer_,complete=complete)
    rows[[length(rows)+1L]]<-parsed
  }
  list(version=2L,rounds=dplyr::bind_rows(c(list(data.frame(round_id=character(),fixture_count=integer(),complete=logical())),rounds)),
    rows=dplyr::bind_rows(c(list(league_fixture_empty()),rows)))
}
league_fixture_matches_valid <- function(value,round_ids=NULL) {
  if(!is.list(value)||!is.numeric(value$version)||length(value$version)!=1L||is.na(value$version)||!value$version%in%c(1,2))return(FALSE)
  rounds<-value$rounds;rows<-value$rows
  if(!is.data.frame(rounds)||nrow(rounds)<1L||nrow(rounds)>120L||!identical(names(rounds),c('round_id','fixture_count','complete'))||
     !is.character(rounds$round_id)||anyNA(rounds$round_id)||any(!nzchar(rounds$round_id))||anyDuplicated(rounds$round_id)||
     (!is.null(round_ids)&&!setequal(rounds$round_id,round_ids))||
     !is.numeric(rounds$fixture_count)||any(!is.na(rounds$fixture_count)&(!is.finite(rounds$fixture_count)|rounds$fixture_count<0|rounds$fixture_count>50|rounds$fixture_count!=floor(rounds$fixture_count)))||
     !is.logical(rounds$complete)||anyNA(rounds$complete)||
     !is.data.frame(rows)||nrow(rows)>6000L||!identical(names(rows),names(league_fixture_empty(value$version)))||
     !all(vapply(rows,function(x)is.character(x)&&!anyNA(x),logical(1)))||
     any(!rows$round_id%in%rounds$round_id)||any(!nzchar(rows$match_id))||anyDuplicated(paste(rows$round_id,rows$match_id))||
     any(nzchar(rows$kickoff)&is.na(fm_time(rows$kickoff))))return(FALSE)
  all(vapply(seq_len(nrow(rounds)),function(i){
    games<-rows[rows$round_id==rounds$round_id[i],,drop=FALSE];count<-rounds$fixture_count[i]
    if(nrow(games)>50L||(!is.na(count)&&nrow(games)>count))return(FALSE)
    !rounds$complete[i]||(!is.na(count)&&count>0L&&nrow(games)==count&&
      all(nzchar(games$home_id)&nzchar(games$away_id)&games$home_id!=games$away_id&
        nzchar(games$home_name)&nzchar(games$away_name)&nzchar(games$kickoff)))
  },logical(1)))
}
league_fixture_matches_decode <- function(value) {
  if(!is.list(value))return(NULL)
  empty<-league_fixture_matches(list())
  empty$rows<-league_fixture_empty(value$version)
  list(version=value$version,
    rounds=dplyr::bind_rows(c(list(empty$rounds),mvp_records(value$rounds))),
    rows=dplyr::bind_rows(c(list(empty$rows),mvp_records(value$rows))))
}
league_fixture_observation <- function(metadata) list(matches=metadata$fixture_matches,scope=metadata$scope,
  observed_at=attr(metadata,'observed_at'),valid_until=metadata$valid_until,
  status=metadata$status,reason=metadata$reason %||% '')

# A complete fixture set can establish a long idle period in a partly played
# round. Keep ordinary matchdays and the daily corrections around each phase
# on their usual cadence.
award_calendar_gaps <- function(matches) {
  games <- matches$rows
  coverage <- matches$rounds
  if(!is.data.frame(games) || !is.data.frame(coverage) || !nrow(games))return(list())
  final <- games$status %in% "F"
  ids <- intersect(intersect(games$round_id[final], games$round_id[!final]),
    coverage$round_id[coverage$complete %in% TRUE])
  gaps <- list()
  for(id in ids){
    group <- games$round_id==id
    if(anyNA(games$status[group]) || any(!nzchar(games$status[group])))next
    previous <- max(as.numeric(fm_time(games$kickoff[group & final])))
    next_game <- min(as.numeric(fm_time(games$kickoff[group & !final])))
    if(is.finite(previous) && is.finite(next_game) && next_game-previous>=7*86400)
      gaps[[id]] <- list(slow_from=previous+86400,resume_at=next_game-86400)
  }
  gaps
}

# The numbered calendar can close the first instalment of a split round.
# Keep the provider's status and use the games in that same observation to
# distinguish a published phase from completion of the whole round.
award_calendar_round_states <- function(metadata) {
  rows <- metadata$rows
  if(!is.data.frame(rows) || !all(c("round_id","round_number","status") %in% names(rows)))return(NULL)
  if(!"is_finished" %in% names(rows))rows$is_finished <- rep(NA,nrow(rows))
  rows$partially_played <- rep(FALSE,nrow(rows))
  unfinished <- tolower(rows$status) %in% c("future","next","running","active","playing","in_progress")
  rows$is_finished[is.na(rows$is_finished) & unfinished] <- FALSE
  games <- metadata$fixture_matches$rows
  all_final <- character()
  if(is.data.frame(games) && nrow(games)){
    final <- games$status %in% "F"
    remaining <- !is.na(games$status) & nzchar(games$status) & !final
    split <- intersect(games$round_id[final], games$round_id[remaining])
    rows$partially_played <- rows$round_id %in% split
    rows$is_finished[rows$partially_played] <- FALSE
    coverage <- metadata$fixture_matches$rounds
    all_final <- setdiff(coverage$round_id[coverage$complete %in% TRUE],games$round_id[!final])
  }
  # Older durable calendars can carry the complete list of fixture dates
  # without individual match statuses. A phase closure spanning a future
  # kickoff is not final as of that calendar's original observation.
  schedule <- metadata$fixture_rounds
  observed <- fm_time(attr(metadata,"observed_at"))
  if(is.data.frame(schedule) && length(observed)==1L && !is.na(observed)){
    candidates <- which(schedule$round_id %in% setdiff(rows$round_id[rows$is_finished %in% TRUE],all_final) &
      lengths(schedule$fixture_dates)>1L)
    spanning <- vapply(schedule$fixture_dates[candidates],function(stamps){
      dates <- fm_time(stamps)
      any(dates<=observed,na.rm=TRUE) && any(dates>observed,na.rm=TRUE)
    },logical(1))
    partial <- rows$round_id %in% schedule$round_id[candidates[spanning]]
    rows$partially_played[partial] <- TRUE
    rows$is_finished[partial] <- FALSE
  }
  rows
}

award_join_calendar <- function(rounds,metadata) {
  states <- award_calendar_round_states(metadata)
  reason <- if(identical(metadata$complete,FALSE))"The numbered round calendar contains incomplete or conflicting round identities." else ""
  if(!is.data.frame(states))return(list(rounds=rounds,reason=reason))
  if(!"partially_played" %in% names(rounds))rounds$partially_played <- rep(FALSE,nrow(rounds))
  duplicates <- duplicated(states$round_id) | duplicated(states$round_id,fromLast=TRUE)
  if(any(duplicates))reason <- "The numbered round calendar contains conflicting round identities."
  detail <- states[!duplicates,,drop=FALSE]
  index <- match(rounds$round_id,detail$round_id)
  number <- detail$round_number[index]
  conflict <- is.finite(number) & is.finite(rounds$round_number) & number!=rounds$round_number
  if(any(conflict))reason <- "Round numbers conflict with the league catalog."
  joined <- !is.na(index) & !conflict
  numbered <- joined & is.finite(number)
  rounds$round_number[numbered] <- number[numbered]
  state <- detail$is_finished[index]
  verified <- joined & !is.na(state)
  rounds$is_finished[verified] <- state[verified]
  rounds$partially_played[joined] <- detail$partially_played[index[joined]]
  status <- detail$status[index]
  labelled <- joined & !is.na(status) & nzchar(status)
  if(!"status" %in% names(rounds))rounds$status <- rep("",nrow(rounds))
  rounds$status[labelled] <- status[labelled]
  gaps <- award_calendar_gaps(metadata$fixture_matches)
  rounds$slow_from <- rep(NA_real_,nrow(rounds))
  rounds$resume_at <- rep(NA_real_,nrow(rounds))
  for(id in names(gaps)){
    index <- joined & rounds$round_id==id
    rounds$slow_from[index] <- gaps[[id]]$slow_from
    rounds$resume_at[index] <- gaps[[id]]$resume_at
  }
  schedule <- metadata$fixture_rounds
  observed <- fm_time(attr(metadata,"observed_at"))
  if(is.data.frame(schedule) && all(c("round_id","all_fixtures_dated","begin_process") %in% names(schedule))){
    dates <- schedule[schedule$all_fixtures_dated %in% TRUE,,drop=FALSE]
    duplicate <- duplicated(dates$round_id) | duplicated(dates$round_id,fromLast=TRUE)
    dates <- dates[!duplicate,,drop=FALSE]
    first <- dates$begin_process[match(rounds$round_id,dates$round_id)]
    rounds$first_kickoff <- ifelse(joined & !is.na(first),first,"")
    if(!"begin_process" %in% names(rounds))rounds$begin_process <- rep("",nrow(rounds))
    missing <- is.na(rounds$begin_process) | !nzchar(rounds$begin_process)
    rounds$begin_process[missing] <- rounds$first_kickoff[missing]
    # A pre-kickoff selection cannot be the final XI. The last dated game
    # remains available after every game is F and the cadence gap has closed.
    if(is.list(dates$fixture_dates) && length(observed)==1L && !is.na(observed)){
      last <- vapply(dates$fixture_dates,function(stamps)max(as.numeric(fm_time(stamps))),numeric(1))
      cutoff <- last[match(rounds$round_id,dates$round_id)]
      closed <- joined & rounds$is_finished%in%TRUE & is.finite(cutoff) & cutoff<=as.numeric(observed)
      rounds$final_fixture_at <- ifelse(closed,cutoff,NA_real_)
    }
  }
  games <- metadata$fixture_matches$rows
  if(is.data.frame(games) && nrow(games) && length(observed)==1L && !is.na(observed)){
    kickoffs <- as.numeric(fm_time(games$kickoff))
    played <- which(kickoffs<=as.numeric(observed))
    last <- tapply(kickoffs[played],games$round_id[played],max)
    rounds$latest_fixture_at <- as.numeric(last[match(rounds$round_id,names(last))])
    rounds$latest_fixture_at[!joined] <- NA_real_
  }
  list(rounds=rounds,reason=reason)
}

get_award_round_metadata <- function(login,championship_id,league_id,refresh=FALSE,current_round_id="",include_matches=FALSE) {
  context<-resource_context(login,championship_id,season=league_stable_season(championship_id,login[["userid"]]))
  scope<-c(context[c("account_id","championship_id","competition_id","season")],list(league_id=as.character(league_id),current_round_id=mvp_text(current_round_id)))
  same_scope<-function(value,check_marker=TRUE)is.list(value)&&setequal(names(value),names(scope))&&
    identical(value[setdiff(names(scope),"current_round_id")],scope[setdiff(names(scope),"current_round_id")])&&
    (!check_marker||!nzchar(scope$current_round_id)||identical(value$current_round_id,scope$current_round_id))
  resource<-paste0("award_round_metadata_",championship_id)
  source_id<-paste0("calendar:v1:",as.character(openssl::sha256(as.character(league_id))))
  ttl<-resource_policy(resource)$ttl
  read_saved<-function(){
    if(nzchar(get_sb_url())&&nzchar(get_sb_key())){
      query<-list(account_id=paste0("eq.",context$account_id),championship_id=paste0("eq.",championship_id),
        competition_id=paste0("eq.",context$competition_id),season=paste0("eq.",context$season),evidence_type="eq.eligibility",source_id=paste0("eq.",source_id),
        select="account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at,payload",limit=1L)
      saved<-tryCatch(rival_evidence_decode(supabase_get("rival_finance_evidence_latest",query)),error=function(e)list())
      if(length(saved)==1L){
        row<-saved[[1]];payload<-row$payload
        if(!is.list(payload))payload<-list()
        restored<-award_calendar_rows_decode(payload$rows)
        stamp<-fm_time(row$observed_at);deadline<-fm_number(payload$valid_until);now<-as.numeric(Sys.time())
        if(is.list(payload)&&identical(payload$kind,"league_round_calendar")&&fm_number(payload$version)%in%c(1,2)&&
           same_scope(payload$scope,FALSE)&&identical(payload$completeness,"complete")&&
           all(vapply(c("account_id","championship_id","competition_id","season"),function(field)identical(mvp_text(row[[field]]),context[[field]]),logical(1)))&&
           identical(mvp_text(row$source_id),source_id)&&identical(row$evidence_type,"eligibility")&&
           length(stamp)==1L&&!is.na(stamp)&&as.numeric(stamp)<=now+5&&is.finite(deadline)&&
           deadline>as.numeric(stamp)&&deadline<=as.numeric(stamp)+ttl+0.001&&award_calendar_rows_valid(restored)){
          result<-list(status="ok",rows=restored,scope=payload$scope,complete=TRUE,valid_until=deadline,from_saved=TRUE)
          valid_fixtures<-TRUE
          if(identical(fm_number(payload$version),2)){
            fixtures<-tryCatch(award_fixture_calendar_decode(payload$fixture_rounds),error=function(e)NULL)
            valid_fixtures<-award_fixture_calendar_valid(fixtures)
            if(valid_fixtures)result$fixture_rounds<-fixtures
          }
          if(!is.null(payload$fixture_matches)||isTRUE(include_matches)){
            games<-tryCatch(league_fixture_matches_decode(payload$fixture_matches),error=function(e)NULL)
            valid_fixtures<-valid_fixtures&&league_fixture_matches_valid(games,restored$round_id)&&
              (!isTRUE(include_matches)||league_fixture_matches_current(games))
            if(valid_fixtures)result$fixture_matches<-games
          }
          if(valid_fixtures){
            attr(result,"observed_at")<-stamp
            return(result)
          }
        }
      }
    }
    NULL
  }
  process<-api_cache_env[[api_cache_key(resource,context=context)]]$data
  if(isTRUE(refresh))invalidate_resources(context,resources=resource)
  result<-get_cached_data(resource,{
    saved<-if(!isTRUE(refresh))read_saved()
    if(!is.null(saved)&&same_scope(saved$scope)&&as.numeric(Sys.time())<saved$valid_until)return(saved)
    failed<-function(response){
      # A source failure cannot undo an observed partial phase. Keep only the
      # last positive partial IDs and their original clock, never its old final flags.
      if(isTRUE(refresh))saved<-read_saved()
      if(is.list(process)&&same_scope(process$scope,FALSE)&&(isTRUE(process$complete)||NROW(process$previous_partial)>0L)&&
         (is.null(saved)||isTRUE(attr(process,"observed_at")>attr(saved,"observed_at"))))saved<-process
      if(!is.null(saved)){
        partial<-saved$previous_partial
        if(is.null(partial)){
          states<-award_calendar_round_states(saved)
          partial<-states[states$partially_played%in%TRUE,c("round_id","round_number"),drop=FALSE]
        }
        if(NROW(partial)){
          response$previous_partial<-partial
          response$scope<-saved$scope
          attr(response,"observed_at")<-attr(saved,"observed_at")
          response$valid_until<-min(saved$valid_until,as.numeric(Sys.time()))
        }
      }
      response
    }
    response<-mvp_api_answer(login,"https://api.futmondo.com/2/league/matches",list(leagueId=league_id))
    if(!identical(response$status,"ok"))return(failed(response))
    records<-mvp_records(response$answer$rounds)
    if(!length(records)||length(records)>120L)return(failed(mvp_result("unsupported",reason="The numbered round calendar could not be read.")))
    rows<-dplyr::bind_rows(lapply(records,function(round){
      if(!is.list(round))return(NULL)
      number<-fm_number(round$number)
      if(!is.finite(number)||number<1||number!=floor(number))number<-NA_real_
      flags<-round[intersect(c("isFinished","finished","isClosed","closed","isCompleted","completed","finalized"),names(round))]
      unfinished<-any(vapply(flags,function(x)identical(x,FALSE)||identical(x,0L)||identical(x,0)||tolower(mvp_text(x))%in%c("false","f","0"),logical(1)))
      status<-tolower(mvp_text(round$status))
      data.frame(round_id=mvp_text(round$`_id` %||% round$id),round_number=number,
        is_finished=if(round_is_finished(round))TRUE else if(unfinished||status%in%c("active","playing","in_progress","running"))FALSE else NA,
        status=status,stringsAsFactors=FALSE)
    }))
    stamp<-Sys.time();deadline<-as.numeric(stamp)+ttl
    result<-list(status="ok",rows=rows,fixture_rounds=award_fixture_calendar(records),fixture_matches=league_fixture_matches(records),
      scope=scope,complete=length(records)==nrow(rows)&&award_calendar_rows_valid(rows),valid_until=deadline)
    attr(result,"observed_at")<-stamp
    result
  },context=context)
  # Older persisted/process observations kept running as unknown. The saved
  # authoritative status resolves it without refetching or renewing its expiry.
  if(identical(result$status,"ok")&&award_calendar_rows_valid(result$rows)){
    running<-is.na(result$rows$is_finished)&result$rows$status=="running"
    result$rows$is_finished[running]<-FALSE
  }
  # A changed source league/active round cannot reuse the old process entry.
  if(identical(result$status,"ok")&&(!same_scope(result$scope)||
     (isTRUE(include_matches)&&!league_fixture_matches_current(result$fixture_matches)))&&!isTRUE(refresh))
    return(get_award_round_metadata(login,championship_id,league_id,refresh=TRUE,current_round_id=current_round_id,include_matches=include_matches))
  result
}

# Called by record_acquired_resource after cooperative acquisition completes;
# replay itself cannot schedule database writes.
persist_award_calendar_snapshot <- function(data,context) {
  scope<-data$scope;stamp<-attr(data,"observed_at")
  if(!identical(data$status,"ok")||!isTRUE(data$complete)||isTRUE(data$from_saved)||!is.list(scope)||
     !all(c("account_id","championship_id","competition_id","season","league_id","current_round_id")%in%names(scope))||
     !all(vapply(c("account_id","championship_id","competition_id","season"),function(field)identical(scope[[field]],context[[field]]),logical(1)))||
     !nzchar(mvp_text(scope$league_id))||!award_calendar_rows_valid(data$rows)||
     length(stamp)!=1L||is.na(fm_time(stamp))||!is.finite(fm_number(data$valid_until)))return(invisible(FALSE))
  payload<-list(kind="league_round_calendar",version=1L,scope=scope,rows=data$rows,
    completeness="complete",valid_until=data$valid_until)
  gaps<-award_calendar_gaps(data$fixture_matches)
  if(length(gaps))payload$split_gaps<-gaps
  if(award_fixture_calendar_valid(data$fixture_rounds)){
    payload$version<-2L;payload$fixture_rounds<-lapply(seq_len(nrow(data$fixture_rounds)),function(i){
      row<-as.list(data$fixture_rounds[i,,drop=FALSE]);row$fixture_dates<-as.list(unlist(row$fixture_dates,use.names=FALSE));row
    })
  }
  if(league_fixture_matches_valid(data$fixture_matches))payload$fixture_matches<-data$fixture_matches
  source_id<-paste0("calendar:v1:",as.character(openssl::sha256(scope$league_id)))
  evidence<-rival_finance_evidence(context$account_id,context$championship_id,context$season,"eligibility",source_id,payload,stamp)
  persist_rival_finance_evidence(list(evidence),context)
}

# Metadata acquisition is safe during active-page loading; it fetches no awards.
# It runs inside the cooperative read adapter (one bulk HTTP request when cold).
get_award_round_catalog <- function(login,championship_id,saved=data.frame(),refresh=FALSE) {
  catalog<-get_finished_round_catalog(login,championship_id)
  rounds<-award_catalog_numbers(catalog$rounds,saved)
  if(!is.data.frame(rounds)||!nrow(rounds))return(catalog)
  required_before<-!vapply(seq_len(nrow(rounds)),function(i)award_round_pending(rounds[i,,drop=FALSE],
    catalog$current_round_number,catalog$current_round_id),logical(1))
  unresolved<-any(!is.finite(rounds$round_number[required_before]))||anyNA(rounds$is_finished[required_before])
  # A later round can close an instalment while earlier rounds are still due.
  # Verify that unusual closure against its own fixtures before requesting XI.
  current<-fm_number(catalog$current_round_number)
  advanced_closed<-which(is.finite(current) & rounds$is_finished%in%TRUE & rounds$round_number>=current)
  metadata_reason<-""
  if(nzchar(mvp_text(catalog$league_id))){
    # Only re-establish this closure from the matching numbered calendar.
    rounds$is_finished[advanced_closed]<-NA
    # A previously opened round can still be partial after the source marker
    # moves past it. Reuse the same scoped, durable bulk calendar for every ID.
    metadata<-get_award_round_metadata(login,championship_id,catalog$league_id,refresh=refresh,current_round_id=catalog$current_round_id)
    attr(catalog,'fixture_calendar')<-league_fixture_observation(metadata)
    attr(catalog,"observed_at")<-attr(metadata,"observed_at")
    attr(catalog,"valid_until")<-metadata$valid_until
    if(identical(metadata$status,"ok")&&is.data.frame(metadata$rows)){
      hydrated<-award_join_calendar(rounds,metadata)
      rounds<-hydrated$rounds;metadata_reason<-hydrated$reason
    }else{
      prior<-metadata$previous_partial
      if(is.data.frame(prior)&&nrow(prior)){
        link<-match(rounds$round_id,prior$round_id)
        phase<-!is.na(link)&(!is.finite(rounds$round_number)|!is.finite(prior$round_number[link])|rounds$round_number==prior$round_number[link])
        rounds$partially_played<-phase
        rounds$is_finished[phase]<-FALSE
      }
      if(unresolved||length(advanced_closed)||NROW(prior))
        metadata_reason<-mvp_text(metadata$reason,"The numbered round calendar could not be loaded.")
    }
  }
  catalog$rounds<-rounds
  current<-which(rounds$round_id%in%mvp_text(catalog$current_round_id))
  if(length(current)==1L&&is.finite(rounds$round_number[current]))catalog$current_round_number<-rounds$round_number[current]
  required<-!vapply(seq_len(nrow(rounds)),function(i)award_round_pending(rounds[i,,drop=FALSE],
    catalog$current_round_number,catalog$current_round_id),logical(1))
  # Future completion flags are irrelevant to historical award coverage.
  resolved<-all(is.finite(rounds$round_number[required]))&&all(rounds$is_finished[required]%in%TRUE)
  catalog$complete<-isTRUE(catalog$identity_complete)&&resolved&&!nzchar(metadata_reason)
  catalog$status<-if(catalog$complete)"ok"else "partial"
  catalog$reason<-if(nzchar(metadata_reason))metadata_reason else if(resolved)""else catalog$reason
  catalog
}

normalize_official_dreamteam <- function(answer, championship_id, reference) {
  if(!is.list(answer)) return(mvp_result("unsupported",reason="The returned MVP format is not supported yet."))
  if(isTRUE(answer$error)) return(mvp_result("rejected",reason="LigaRadar rejected the MVP request."))
  # Empty/missing data alone does not establish that an award is unpublished.
  if(is.null(answer$players) || is.null(answer$mvp)) return(mvp_result("unsupported",reason="LigaRadar did not return a recognizable official MVP selection; publication status is unknown."))
  mvp <- answer$mvp
  mid <- if(is.list(mvp)) mvp_text(mvp$id %||% mvp$`_id`) else mvp_text(mvp)
  players <- mvp_records(answer$players)
  if(!nzchar(mid) || !length(players)) return(mvp_result("unsupported",reason="No identifiable official MVP was returned; publication status is unknown."))
  rows <- dplyr::bind_rows(lapply(players,function(p) {
    if(!is.list(p)) return(NULL)
    data.frame(championship_id=as.character(championship_id),round_id=reference$round_id,
      round_number=reference$round_number,player_id=mvp_text(p$id %||% p$`_id`),
      player_name=mvp_text(p$name),player_role=mvp_text(p$role),player_position=mvp_text(p$position),points=fm_number(p$points),
      award_team_id=mvp_text(p[["teamId"]]),award_team=mvp_text(p[["team"]]),award_logo=mvp_text(p[["logo"]]),
      is_mvp=identical(mvp_text(p$id %||% p$`_id`),mid),
      is_finished=if(is.logical(reference$is_finished)&&length(reference$is_finished)==1L)reference$is_finished else NA)
  }))
  if(nrow(rows)!=11L || any(!nzchar(rows$player_id)) || anyDuplicated(rows$player_id) || sum(rows$is_mvp)!=1L || !nzchar(rows$player_name[rows$is_mvp]))
    return(mvp_result("unsupported",reason="The official MVP identity could not be resolved uniquely."))
  result <- mvp_result("ok",rows,complete=TRUE)
  result$metadata <- list(strategy=mvp_text(answer$strategy), pointSystem=answer$pointSystem,
    custom=answer$custom, finality=if(isTRUE(reference$is_finished))"final" else
      if(identical(reference$is_finished,FALSE))"provisional" else "unknown")
  result
}

# This read runs only inside explicit Admin acquisition, never on a display tick.
# Captured match/list returns the requested round identity and number separately
# from match results. Neither a score nor a compact status establishes payment.
get_award_round_reference <- function(login,championship_id,reference) {
  if(is.finite(fm_number(reference$round_number)))return(reference)
  detail<-tryCatch(get_cached_data(paste0("award_schedule_",championship_id,"_",reference$round_id),{
    response<-mvp_api_answer(login,"https://api.futmondo.com/1/match/list",list(
      championshipId=championship_id,userteamId=reference$user_team_id %||% NULL,roundId=reference$round_id))
    if(!identical(response$status,"ok"))NULL else response$answer$round
  },context=resource_context(login,championship_id)),error=function(e)NULL)
  id<-if(is.list(detail))mvp_text(detail$`_id` %||% detail$id)else ""
  number<-if(is.list(detail))fm_number(detail$number)else NA_real_
  if(!identical(id,reference$round_id)||!is.finite(number)||number<1||number!=floor(number))return(reference)
  reference$round_number<-number
  if(round_is_finished(detail))reference$is_finished<-TRUE
  reference
}
get_official_dreamteam_result <- function(login, championship_id, reference) {
  # Known unclosed rounds need neither an award request nor a schedule lookup.
  if(award_round_pending(reference)){
    result<-mvp_result("not_due");result$reference<-reference;return(result)
  }
  tryCatch(get_cached_data(paste0("official_dreamteam_",championship_id,"_",reference$round_id), {
    reference<-get_award_round_reference(login,championship_id,reference)
    if(award_round_pending(reference)){
      result<-mvp_result("not_due",reason="The round has not closed yet.")
    }else{
      response <- mvp_api_answer(login,DREAMTEAM_URL,list(championshipId=championship_id,type="dreamteam",round=reference$round_id))
      if(response$status!="ok")result<-response
      else result<-normalize_official_dreamteam(response$answer,championship_id,reference)
    }
    result$reference<-reference
    result
  }, context=resource_context(login=login,championship_id=championship_id)),error=function(e) mvp_result("connection",reason="MVP retrieval failed. Refresh to retry."))
}

# The explicit, shared Admin acquisition controller lives in award_runtime.R.
source("award_runtime.R")

calculate_mvp_bonuses <- function(official, championship_id, eligibility=data.frame(), rewards=data.frame(), payments=data.frame(), coverage_complete=FALSE) {
  rows <- if(is.data.frame(official)) official else data.frame()
  if(!all(c("championship_id","round_id","player_id","is_mvp") %in% names(rows))) rows <- data.frame()
  if(nrow(rows)) rows <- rows[rows$championship_id==championship_id & rows$is_mvp %in% TRUE & if("is_finished"%in%names(rows))rows$is_finished%in%TRUE else FALSE,,drop=FALSE]
  scope <- function(df) if(is.data.frame(df) && "championship_id" %in% names(df)) df[!is.na(df$championship_id) & df$championship_id==championship_id,,drop=FALSE] else data.frame()
  eligibility <- scope(eligibility); rewards <- scope(rewards); payments <- scope(payments)
  events <- if(nrow(rows)) unique(rows[c("championship_id","round_id","player_id")]) else data.frame()
  valid_payment <- all(c("round_id","player_id","team_id","amount","bonus_type") %in% names(payments))
  if(valid_payment) {
    keep <- payments$bonus_type %in% "mvp" & is.finite(payments$amount) & !is.na(payments$team_id) & nzchar(payments$team_id) & !is.na(payments$round_id) & nzchar(payments$round_id) & !is.na(payments$player_id) & nzchar(payments$player_id)
    payments <- payments[keep,,drop=FALSE]
    events <- unique(dplyr::bind_rows(events,payments[c("championship_id","round_id","player_id")]))
  }
  out <- dplyr::bind_rows(lapply(seq_len(nrow(events)),function(i) {
    e <- events[i,,drop=FALSE]; team <- ""; amount <- NA_real_; status <- "unresolved"; source <- "none"
    reason <- "Historical MVP recipient and eligibility are not verified."
    pay <- if(valid_payment) unique(payments[payments$round_id==e$round_id & payments$player_id==e$player_id,c("team_id","amount"),drop=FALSE]) else data.frame()
    if(nrow(pay)==1L) {team <- pay$team_id; amount <- pay$amount; status <- "recorded"; source <- "payment"; reason <- ""}
    else if(nrow(pay)>1L) reason <- "Conflicting MVP payment evidence."
    else {
      rule <- if(all(c("round_id","amount","historical") %in% names(rewards))) rewards[rewards$round_id==e$round_id,,drop=FALSE] else data.frame()
      evidence <- if(all(c("round_id","player_id","team_id","eligible","verified") %in% names(eligibility))) eligibility[eligibility$round_id==e$round_id & eligibility$player_id==e$player_id & eligibility$eligible %in% TRUE & eligibility$verified %in% TRUE,,drop=FALSE] else data.frame()
      if(nrow(rule)==1L && is.finite(rule$amount) && rule$amount==0 && isTRUE(rule$historical)) {
        amount <- 0; status <- "disabled"; source <- "historical configuration"; reason <- "MVP reward disabled for this round."
      } else if(nrow(evidence)==1L && nzchar(evidence$team_id)) {
        team <- evidence$team_id
        if(nrow(rule)==1L && is.finite(rule$amount)) {
          amount <- rule$amount; status <- "estimated"; source <- if(isTRUE(rule$historical)) "historical configuration" else "current configuration assumption"
          reason <- if(isTRUE(rule$historical)) "Payment not observed." else "Assumes today's reward applied to this round; payment not observed."
        } else reason <- "Applicable MVP reward is unknown."
      }
    }
    data.frame(e,team_id=team,amount=amount,status=status,source=source,reason=reason)
  }))
  list(rows=out,complete=isTRUE(coverage_complete) && (!nrow(out) || !any(out$status=="unresolved")),
    reason=if(!isTRUE(coverage_complete)) "Official round coverage is incomplete." else "")
}

add_mvp_finance_estimates <- function(finances, attribution) {
  if(!is.data.frame(finances) || !nrow(finances)) return(finances)
  rows <- attribution$rows
  finances$mvp_recorded <- finances$mvp_estimated <- 0
  unresolved <- if(nrow(rows)) sum(rows$status=="unresolved") else 0L
  finances$mvp_unresolved <- unresolved
  reasons <- if(nrow(rows)) unique(rows$reason[nzchar(rows$reason)]) else character()
  finances$mvp_status <- paste(if(isTRUE(attribution$complete)) "MVP coverage complete; other income may be missing." else paste("Incomplete:",unresolved,"unresolved MVP awards.",attribution$reason),paste(reasons,collapse=" "))
  for(i in seq_len(nrow(finances))) {
    if(nrow(rows)) {
      own <- rows[rows$team_id==finances$teamid[i],,drop=FALSE]
      finances$mvp_recorded[i] <- sum(own$amount[own$status=="recorded"],na.rm=TRUE)
      finances$mvp_estimated[i] <- sum(own$amount[own$status=="estimated"],na.rm=TRUE)
    }
  }
  # This is a subtotal, never a replacement for authoritative API cash.
  finances$known_funds_subtotal <- with(finances,initial_budget-total_spent+total_sales+point_bonus+mvp_recorded+mvp_estimated)
  finances
}

get_mvp_payment_evidence <- function(login, championship_id, team_ids) {
  dplyr::bind_rows(lapply(unique(as.character(team_ids)),function(team) {
    movements <- tryCatch(get_user_team_moneymovements(login,championship_id,team),error=function(e)NULL)
    fields <- c("bonus_type","round_id","player_id","money")
    if(!is.data.frame(movements) || !all(fields %in% names(movements))) return(NULL)
    rows <- movements[movements$bonus_type %in% "mvp",,drop=FALSE]
    if(!nrow(rows)) return(NULL)
    data.frame(championship_id=championship_id,round_id=rows$round_id,player_id=rows$player_id,
      team_id=team,amount=suppressWarnings(as.numeric(rows$money)),bonus_type="mvp")
  }))
}

mvp_sync_round <- function(login, championship_id, round_id, round_number) {
  result <- get_official_dreamteam_result(login,championship_id,round_reference(round_id,round_number))
  counts <- list(status=result$status,fetched=nrow(result$rows),queued=0L,saved=0L,skipped=0L,failed=0L,reason=result$reason)
  if(result$status!="ok") {counts$failed <- 1L; return(counts)}
  if(!is.finite(round_number)) {counts$status <- "partial"; counts$skipped <- counts$fetched; counts$reason <- "Round number unavailable; results cannot be saved."; return(counts)}
  # These fields belong to the full-XI evidence payload, outside the legacy table schema.
  legacy <- result$rows[, setdiff(names(result$rows), c("award_team_id", "award_team", "award_logo", "player_position")), drop = FALSE]
  value <- tryCatch(supabase_post("round_dream_team",legacy),error=function(e)NULL)
  if(isTRUE(value)) {counts$status <- "queued"; counts$queued <- counts$fetched}
  else if(is.numeric(value) && length(value)==1L && !is.na(value) && value>=200 && value<300) {counts$status <- "ok"; counts$saved <- counts$fetched}
  else {counts$status <- "error"; counts$failed <- counts$fetched; counts$reason <- "Official results loaded, but database saving failed."}
  counts
}

mvp_sync_all <- function(login, championship_id, verbose=TRUE) {
  catalog <- get_finished_round_catalog(login,championship_id)
  rounds <- catalog$rounds; done <- rounds[rounds$is_finished %in% TRUE,,drop=FALSE]
  results <- lapply(seq_len(nrow(done)),function(i) mvp_sync_round(login,championship_id,done$round_id[i],done$round_number[i]))
  totals <- lapply(c("fetched","queued","saved","skipped","failed"),function(key) sum(vapply(results,function(x) as.numeric(x[[key]]),numeric(1))))
  names(totals) <- c("fetched","queued","saved","skipped","failed")
  status <- if(!isTRUE(catalog$complete) || totals$failed>0 || totals$skipped>0) "partial" else if(totals$queued>0) "queued" else "ok"
  c(list(status=status,total_rounds=nrow(done),total_players=totals$saved,per_round=results,
    reason=paste(unique(c(catalog$reason,vapply(results,function(x)x$reason,character(1)))),collapse=" ")),totals)
}

mvp_sync_message <- function(result) {
  paste0("Dream teams: ",result$fetched," fetched, ",result$queued," queued, ",result$saved,
    " confirmed saved, ",result$skipped," skipped, ",result$failed," failed. ",result$reason)
}

sync_round_dreamteam_to_supabase <- function(login, championship_id, round_id, round_number)
  mvp_sync_round(login,championship_id,round_id,round_number)
sync_all_championship_dreamteams <- function(login, championship_id, verbose=TRUE)
  mvp_sync_all(login,championship_id,verbose)

invalidate_mvp_saved_cache <- function() {
  keys <- ls(api_cache_env)
  keys <- keys[grepl("(^|:)round_mvps_|(^|:)db_round_dream_team_",keys)]
  if(length(keys)) rm(list=keys,envir=api_cache_env)
  invisible(TRUE)
}
