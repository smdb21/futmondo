# Identify outstanding final round rewards from scoped, dated source evidence.
rival_pending_settlement_rounds <- function(evidence,catalog,movements,now=Sys.time(),
  calendar=rival_settlement_calendar(evidence,now),with_clock=FALSE,zero_final_rewards=FALSE) {
  now <- as.numeric(fm_time(now))
  edges <- numeric()
  finish <- function(numbers){
    if(!with_clock)return(numbers)
    edges <- edges[is.finite(edges)]
    before <- edges[edges<=now]
    after <- edges[edges>now]
    list(rounds=numbers,interval=c(if(length(before))max(before)else NA_real_,
      if(length(after))min(after)else NA_real_))
  }
  if(length(now)!=1L||!is.finite(now)||!is.data.frame(catalog)||
     !all(c("round_id","round_number") %in% names(catalog)))return(finish(numeric()))
  payments <- data.frame(round_id=character(),at=numeric(),final=logical())
  if(is.data.frame(movements)&&all(c("round_id","bonus_type","money","date") %in% names(movements))){
    movements <- movements[movements$bonus_type %in% c("points","ranking","mvp","dreamteam"),,drop=FALSE]
    at <- as.numeric(fm_time(movements$date))
    edges <- c(edges,at)
    final <- movements$bonus_type %in% c("ranking","mvp","dreamteam") & movements$money>0
    keep <- is.finite(movements$money) & (final | movements$bonus_type %in% "points")
    keep <- keep & is.finite(at) & at<=now
    payments <- data.frame(round_id=movements$round_id[keep],at=at[keep],final=final[keep])
  }
  pending <- character()
  evidence_rows <- rival_evidence_rows(evidence,"eligibility")
  for(record in evidence_rows){
    p <- record$payload
    if(!is.list(p)||!mvp_text(p$kind) %in% c("round_settlement_status","award_ownership_calendar","league_round_calendar"))next
    stamp <- as.numeric(fm_time(record$observed_at))
    if(length(stamp)!=1L||!is.finite(stamp))next
    edges <- c(edges,stamp-60,stamp)
    if(stamp>now+60 || !identical(p$kind,"round_settlement_status") || !identical(fm_number(p$version),1) ||
       !identical(p$status,"pending")||!isTRUE(p$verified)||
       !identical(mvp_text(record$source_id),paste0("round-settlement:",mvp_text(p$round_id))))next
    number <- unique(catalog$round_number[catalog$round_id %in% mvp_text(p$round_id)])
    if(length(number)==1L && isTRUE(number==fm_number(p$round_number)) &&
       !any(payments$final & payments$round_id %in% p$round_id & payments$at>stamp))pending <- c(pending,p$round_id)
  }
  # Prefer the newest dated source per round, regardless of input order.
  # The older ownership calendar lacks a completeness flag.
  calendars <- rival_ownership_evidence_rows(evidence,"award_ownership_calendar")
  dated <- lapply(calendars,function(record){
    stamp <- as.numeric(fm_time(record$observed_at))
    list(at=if(length(stamp)==1L)stamp else NA_real_,rows=mvp_records(record$payload$rows))
  })
  if(!is.null(calendar))dated <- c(dated,list(list(at=as.numeric(calendar$observed_at),
    rows=mvp_records(calendar$fixtures))))
  stamps <- vapply(dated,function(x)x$at,numeric(1))
  seen <- character()
  for(observation in dated[order(stamps,seq_along(stamps),decreasing=TRUE,na.last=NA)]){
    if(observation$at>now+60)next
    rows <- observation$rows
    times <- lapply(rows,function(row){
      dates <- unlist(row$fixture_dates,use.names=FALSE)
      if(!is.character(dates)||!length(dates)||length(dates)>50L)return(numeric())
      as.numeric(fm_time(dates))
    })
    names(times) <- vapply(rows,function(row)mvp_text(row$round_id),character(1))
    complete <- vapply(rows,function(row)!identical(row$all_fixtures_dated,FALSE),logical(1))
    valid <- lengths(times)>0L & vapply(times,function(x)all(is.finite(x)),logical(1))
    groups <- league_fixture_date_groups(times[complete & valid])
    split <- groups$round_id[groups$groups>1L]
    for(i in which(valid & !names(times) %in% seen & names(times) %in% catalog$round_id)){
      rid <- names(times)[i]
      dates <- times[[i]]
      first <- min(dates)
      last <- max(dates)
      seen <- c(seen,rid)
      edges <- c(edges,first,last)
      if(now<first)next
      paid <- payments[payments$round_id %in% rid,,drop=FALSE]
      # An actual phase payment before the final fixture, or a complete
      # intervening matchday, establishes a split even after fixture refresh.
      early_payment <- any(paid$at>=first & paid$at<last)
      split_round <- rid %in% split || early_payment || !complete[i]
      if(now<last || (split_round && !any(paid$final & paid$at>=last)))pending <- c(pending,rid)
    }
  }
  if(isTRUE(zero_final_rewards)&&length(pending)&&!is.null(calendar$matches)&&as.numeric(calendar$observed_at)<=now){
    matches <- tryCatch(league_fixture_matches_decode(calendar$matches),error=function(e)NULL)
    if(league_fixture_matches_valid(matches,calendar$rounds$round_id)){
      games <- matches$rows
      finished <- games$status %in% "F" & fm_time(games$kickoff)<=calendar$observed_at
      all_final <- setdiff(matches$rounds$round_id[matches$rounds$complete],games$round_id[!finished %in% TRUE])
      # A zero final reward does not produce a cash receipt. Require matching
      # complete schedule and fixture identities before releasing the round.
      for(id in intersect(pending,all_final)){
        i <- match(id,calendar$fixtures$round_id)
        if(is.na(i)||!isTRUE(calendar$fixtures$all_fixtures_dated[i]))next
        schedule <- as.numeric(fm_time(unlist(calendar$fixtures$fixture_dates[[i]],use.names=FALSE)))
        kickoffs <- as.numeric(fm_time(games$kickoff[games$round_id==id]))
        if(setequal(schedule,kickoffs))pending <- setdiff(pending,id)
      }
    }
  }
  numbers <- catalog$round_number[catalog$round_id %in% pending]
  finish(sort(unique(numbers[is.finite(numbers)&numbers>=1&numbers==floor(numbers)])))
}
# Restore the worker's numbered fixture calendar from its original, scoped
# observation. Financial projections never treat a kickoff as a payment.
rival_settlement_calendar <- function(evidence,now) {
  fields <- c('account_id','championship_id','competition_id','season')
  candidates <- Filter(function(record)is.list(record$payload)&&identical(record$payload$kind,'league_round_calendar'),
    rival_evidence_rows(evidence,'eligibility'))
  stamps <- vapply(candidates,function(record){
    stamp <- as.numeric(fm_time(record$observed_at))
    if(length(stamp)==1L)stamp else NA_real_
  },numeric(1))
  for(record in candidates[order(stamps,decreasing=TRUE,na.last=NA)]){
    p <- record$payload
    if(!is.list(p)||!identical(p$kind,'league_round_calendar')||!identical(fm_number(p$version),2)||
       !identical(p$completeness,'complete')||!is.list(p$scope))next
    if(!all(vapply(fields,function(key)fm_context_known(record[[key]]) &&
       identical(mvp_text(record[[key]]),mvp_text(p$scope[[key]])),logical(1))))next
    league <- mvp_text(p$scope$league_id)
    stamp <- fm_time(record$observed_at)
    if(!identical(league,mvp_text(record$competition_id))||length(stamp)!=1L||is.na(stamp)||stamp>now+60||
       !identical(record$source_id,paste0('calendar:v1:',as.character(openssl::sha256(league)))))next
    rounds <- award_calendar_rows_decode(p$rows)
    fixtures <- tryCatch(award_fixture_calendar_decode(p$fixture_rounds),error=function(e)NULL)
    if(award_calendar_rows_valid(rounds) && award_fixture_calendar_valid(fixtures) &&
       all(fixtures$round_id %in% rounds$round_id))return(list(rounds=rounds,fixtures=fixtures,observed_at=stamp,matches=p$fixture_matches))
  }
  NULL
}
