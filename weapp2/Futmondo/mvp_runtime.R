# Official round discovery, cooperative MVP loading and conservative bonus attribution.
# Contracts and examples: docs/round_mvps_module.md and docs/league_finances.md.
mvp_text <- function(x, default="") {
  if(!is.atomic(x) || length(x)!=1L || is.na(x)) return(default)
  trimws(as.character(x))
}
mvp_result <- function(status, rows = data.frame(), reason = "", code = "", complete = FALSE) {
  list(status=status, rows=rows, reason=reason, code=code, complete=complete)
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
    if (status %in% c(401L,403L)) return(mvp_result("authentication", reason="Futmondo access was denied. Log in again."))
    if (status < 200L || status >= 300L) return(mvp_result("error", reason=paste("Futmondo request failed (HTTP",status,").")))
    body <- httr::content(response)
    answer <- body$answer
    if (!is.list(answer)) return(mvp_result("unsupported", reason="The returned Futmondo format is not supported yet."))
    if (isTRUE(answer$error) || (!is.null(answer$code) && !identical(answer$code,API_CODE_OK))) {
      code <- fm_scalar(answer$code, "")
      if (!grepl("^[a-zA-Z0-9_.-]{0,80}$", code)) code <- ""
      return(mvp_result("rejected", reason=paste0("Futmondo rejected the request",if(nzchar(code)) paste0(" (",code,")") else "","."), code=code))
    }
    list(status="ok", answer=answer)
  }, error=function(e) {
    # Async HTTP/parser failures arrive as conditions before an httr response.
    failure<-resource_failure(e,resource=url)
    if(identical(failure$category,"http")&&!is.null(failure$http_status)){
      if(failure$http_status%in%c(401L,403L))return(mvp_result("authentication",reason="Futmondo access was denied. Log in again."))
      return(mvp_result("error",reason=paste("Futmondo request failed (HTTP",failure$http_status,").")))
    }
    if(identical(failure$category,"parser"))return(mvp_result("unsupported",reason="Futmondo returned a malformed data response."))
    mvp_result("connection",reason="Futmondo data could not be loaded because the connection failed. Refresh to retry.")
  })
}

normalize_mvp_catalog <- function(active, leagues, championship_id) {
  unknown <- function(reason) list(status="unsupported",rounds=empty_finished_rounds(),complete=FALSE,reason=reason)
  matches <- Filter(function(x) is.list(x) && identical(mvp_text(x$id %||% x$`_id`),as.character(championship_id)), mvp_records(active$championships))
  if (length(matches)!=1L) return(unknown("The selected championship could not be matched to a league."))
  champ <- matches[[1]]; league <- champ$league
  lid <- if(is.list(league)) mvp_text(league$id %||% league$`_id`) else mvp_text(league)
  selected <- Filter(function(x) is.list(x) && nzchar(lid) && identical(mvp_text(x$id %||% x$`_id`),lid),mvp_records(leagues))
  if(length(selected)!=1L || is.null(selected[[1]]$rounds)) return(unknown("We cannot determine completed rounds from the available Futmondo league catalog."))
  rounds <- mvp_records(selected[[1]]$rounds)
  if(!length(rounds)) return(unknown("Futmondo returned an empty round catalog; historical coverage is unknown."))
  metadata <- c(mvp_records(active$rounds),mvp_records(champ$rounds))
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
    finished <- if(round_is_finished(r) || round_is_finished(detail) || identical(status,"done")) TRUE else if(explicit_false || status %in% c("active","playing","in_progress")) FALSE else NA
    begin <- mvp_text(r$beginProcess)
    if(!nzchar(begin) && length(matched)==1L) begin <- mvp_text(matched[[1]]$beginProcess)
    data.frame(round_id=id,round_number=number,begin_process=begin,is_finished=finished,status=status)
  }))
  valid <- nzchar(rows$round_id) & !duplicated(rows$round_id)
  complete <- all(valid) && !anyNA(rows$is_finished)
  rows <- rows[valid,,drop=FALSE]
  active_details<-Filter(function(round){
    is.list(round)&&mvp_text(round$`_id` %||% round$id)%in%rows$round_id&&
      (!nzchar(mvp_text(round$championshipId))||identical(mvp_text(round$championshipId),as.character(championship_id)))
  },mvp_records(active$rounds))
  anchors<-unique(vapply(active_details,function(round)fm_number(round$number),numeric(1)))
  anchors<-anchors[is.finite(anchors)&anchors>=1&anchors==floor(anchors)]
  list(status=if(complete) "ok" else "partial",rounds=rows,complete=complete,identity_complete=all(valid),
    user_team_id=mvp_text(champ$userteam$id %||% champ$userteam$`_id`),
    current_round_number=if(length(anchors)==1L)anchors else NA_real_,
    reason=if(complete) "" else "Some round numbers or completion states are not confirmed. Retrieve missing awards to check their schedules.")
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

normalize_official_dreamteam <- function(answer, championship_id, reference) {
  if(!is.list(answer)) return(mvp_result("unsupported",reason="The returned MVP format is not supported yet."))
  if(isTRUE(answer$error)) return(mvp_result("rejected",reason="Futmondo rejected the MVP request."))
  # Empty/missing data alone does not establish that an award is unpublished.
  if(is.null(answer$players) || is.null(answer$mvp)) return(mvp_result("unsupported",reason="Futmondo did not return a recognizable official MVP selection; publication status is unknown."))
  mvp <- answer$mvp
  mid <- if(is.list(mvp)) mvp_text(mvp$id %||% mvp$`_id`) else mvp_text(mvp)
  players <- mvp_records(answer$players)
  if(!nzchar(mid) || !length(players)) return(mvp_result("unsupported",reason="No identifiable official MVP was returned; publication status is unknown."))
  rows <- dplyr::bind_rows(lapply(players,function(p) {
    if(!is.list(p)) return(NULL)
    data.frame(championship_id=as.character(championship_id),round_id=reference$round_id,
      round_number=reference$round_number,player_id=mvp_text(p$id %||% p$`_id`),
      player_name=mvp_text(p$name),player_role=mvp_text(p$role),player_position=mvp_text(p$position),points=fm_number(p$points),
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
  value <- tryCatch(supabase_post("round_dream_team",result$rows),error=function(e)NULL)
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
