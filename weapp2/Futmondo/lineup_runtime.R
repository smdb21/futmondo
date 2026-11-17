# Pure next-round lineup rules and exact historical-score selection.
lineup_player_label <- function(name) {
  name<-trimws(gsub("[[:space:]]+"," ",as.character(name)))
  sub("^([^[:space:]])[^[:space:]]* (.+)$","\\1. \\2",name)
}
lineup_formations <- function() c('4-4-2','4-3-3','4-5-1','3-4-3','3-5-2','5-4-1','5-3-2','3-6-1','4-2-4','3-3-4','4-6-0','5-2-3')
lineup_slot_roles <- function(strategy) {
  if(!strategy%in%lineup_formations())stop('Unsupported formation')
  counts<-as.integer(strsplit(strategy,'-',fixed=TRUE)[[1]])
  c(rep('FWD',counts[3]),rep('MID',counts[2]),rep('DEF',counts[1]),'GK')
}
lineup_empty <- function(strategy='4-4-2') list(strategy=strategy,starters=rep('',11L),bench=rep('',4L),captain='')
lineup_clear <- function(draft) lineup_empty(draft$strategy)
lineup_identity <- function(draft) jsonlite::toJSON(list(strategy=draft$strategy,starters=unname(draft$starters),bench=unname(draft$bench),captain=draft$captain),auto_unbox=TRUE)
lineup_positions <- function(roster,multiposition=TRUE) {
  # Source role/role2 are positions; the API's position field is a pitch index.
  fields<-intersect(c('role','role2'),names(roster));analytics_player_positions(roster[,fields,drop=FALSE],multiposition)
}
lineup_target <- function(rounds,now=Sys.time()) {
  if(!is.data.frame(rounds)||!all(c('round_id','round_number','begin_process')%in%names(rounds)))return(NULL)
  times<-fm_time(rounds$begin_process);pick<-which(!is.na(times)&times>now)
  if(!length(pick))return(NULL)
  i<-pick[which.min(times[pick])]
  id<-fm_scalar(rounds$round_id[i],'');number<-fm_number(rounds$round_number[i])
  if(!nzchar(id)||!is.finite(number)||number<1||number!=floor(number))return(NULL)
  list(id=id,number=number,starts_at=times[i])
}
# Images use only observed logo values, never inferred club-name filenames.
lineup_fixture_crest_url <- function(logo) {
  logo<-mvp_text(logo)
  if(grepl('^https://[^[:space:]]+$',logo,ignore.case=TRUE))return(logo)
  if(!grepl('^[A-Za-z0-9][A-Za-z0-9_.-]*$',logo))return('')
  paste0(TEAM_LOGO_URL,logo,if(grepl('\\.(png|svg|webp)$',logo,ignore.case=TRUE))''else'.png')
}
# No acquisition here: the shared round resource includes the durable calendar.
lineup_fixture_view <- function(rounds,target,scope,now=Sys.time()) {
  result<-list(rows=league_fixture_empty(),message='',complete=FALSE,stale=FALSE,observed_at=NULL)
  unavailable<-function(message){result$message<-message;result}
  if(is.null(target))return(unavailable('The next round has not been confirmed.'))
  calendar<-attr(rounds,'fixture_calendar')
  if(!is.list(calendar))return(unavailable('Next-round fixtures are awaiting the synced match calendar.'))
  if(!identical(calendar$status,'ok'))return(unavailable('The match calendar could not be synced. Background sync will retry.'))
  fields<-c('account_id','championship_id','competition_id','season')
  if(!is.list(calendar$scope)||!all(vapply(fields,function(field)
    identical(mvp_text(calendar$scope[[field]]),mvp_text(scope[[field]])),logical(1))))
    return(unavailable('Waiting for the match calendar for this league and season.'))
  stamp<-fm_time(calendar$observed_at);deadline<-fm_number(calendar$valid_until)
  if(length(stamp)!=1L||is.na(stamp)||stamp>now||!is.finite(deadline)||!league_fixture_matches_valid(calendar$matches))
    return(unavailable('The saved match calendar is incomplete. Background sync will retry.'))
  coverage<-calendar$matches$rounds;coverage<-coverage[coverage$round_id==target$id,,drop=FALSE]
  rows<-calendar$matches$rows;rows<-rows[rows$round_id==target$id,,drop=FALSE]
  result$rows<-rows[order(fm_time(rows$kickoff),rows$match_id,na.last=TRUE),,drop=FALSE]
  result$observed_at<-stamp;result$stale<-deadline<=as.numeric(now)
  result$complete<-nrow(coverage)==1L&&isTRUE(coverage$complete)
  if(!nrow(rows))result$message<-'Fixtures for this round have not been confirmed in the synced calendar.'
  else if(!result$complete)result$message<-'Some match details are still unconfirmed; the schedule may be incomplete.'
  if(nzchar(mvp_text(calendar$refresh_error))){
    result$stale<-TRUE;result$message<-trimws(paste(result$message,calendar$refresh_error))
  }
  result
}
# Completed-round facts remain usable when a successful worker observation ages.
# Actual read failures, incomplete catalogs and foreign scopes remain unverified.
lineup_catalog_problem <- function(catalog,state=NULL,expected_context=NULL,now=Sys.time()) {
  if(!is.data.frame(catalog)||!nrow(catalog))return('Round schedule is unavailable or still loading.')
  if(!is.null(state)) {
    if(!is.null(state$error)||isTRUE(state$status%in%c('partial','unavailable')))
      return('Round schedule could not be refreshed.')
    if(!is.null(expected_context)) {
      fields<-c('account_id','championship_id','user_team_id','competition_id','season','scoring_version')
      if(!is.list(state$context)||!all(vapply(fields,function(field)
        identical(fm_scalar(state$context[[field]],''),fm_scalar(expected_context[[field]],'')),logical(1))))
        return('Round schedule belongs to a different context; waiting for matching data.')
    }
  }
  if(!is.null(attr(catalog,'fetch_error')))return('Round schedule could not be refreshed.')
  stamp<-fm_time(attr(catalog,'observed_at'))
  worker_status<-attr(catalog,'worker_acquisition_status')
  observed<-isTRUE(worker_status%in%c('ok','empty'))&&identical(attr(catalog,'worker_completeness'),'complete')&&
    isTRUE((attr(catalog,'fetch_status') %||% 'ok')%in%c('ok','empty','stale'))&&
    length(stamp)==1L&&!is.na(stamp)&&stamp<=now
  if(!is.null(worker_status)&&!observed)return('Round schedule observations are incomplete or invalid.')
  if(acquisition_data_unverified(catalog)&&!observed)return('Round schedule is unavailable from LigaRadar.')
  if(identical(attr(catalog,'catalog_complete'),FALSE))return(fm_scalar(attr(catalog,'catalog_reason'),''))
  ''
}
lineup_average_scores <- function(roster,history,rounds,n=5L) {
  n<-max(1L,min(20L,as.integer(n)));ids<-as.character(roster$id)
  result<-data.frame(id=ids,average=rep(NA_real_,length(ids)),coverage=integer(length(ids)),required=integer(length(ids)),status=rep('missing',length(ids)),stringsAsFactors=FALSE)
  wanted<-if(is.data.frame(rounds)&&all(c('round_number','is_finished')%in%names(rounds)))
    tail(sort(unique(as.numeric(rounds$round_number[rounds$is_finished%in%TRUE]))),n)else numeric()
  wanted<-wanted[is.finite(wanted)];result$required<-rep(length(wanted),length(ids));attr(result,'rounds')<-wanted
  if(!length(wanted)||!is.data.frame(history)||!all(c('player_id','round','points')%in%names(history)))return(result)
  history<-history[as.numeric(history$round)%in%wanted,,drop=FALSE]
  # Shared controller supplies reconciled context-scoped revisions. Keep its
  # final duplicate only, retaining an explicit missing-score correction.
  history<-history[!duplicated(paste(history$player_id,history$round),fromLast=TRUE),,drop=FALSE]
  by_player<-split(history,as.character(history$player_id))
  for(i in seq_along(ids)){
    rows<-by_player[[ids[i]]];if(is.null(rows))next
    values<-suppressWarnings(as.numeric(rows$points[match(wanted,as.numeric(rows$round))]))
    result$coverage[i]<-sum(is.finite(values))
    if(all(is.finite(values))){result$average[i]<-mean(values);result$status[i]<-'complete'}else if(result$coverage[i])result$status[i]<-'partial'
  }
  attr(result,'rounds')<-wanted;result
}
# One player per pass, vectorized over a bounded positional count lattice.
# Predecessors reconstruct exact distinct-player assignments (no greedy picks).
lineup_optimize <- function(roster,scores,allowed,current=NULL,include_unavailable=FALSE,bench_enabled=TRUE,multiposition=TRUE,excluded_ids=character()) {
  allowed<-intersect(lineup_formations(),as.character(allowed));if(!length(allowed))return(NULL)
  roster<-roster[order(as.character(roster$id)),,drop=FALSE]
  values<-as.numeric(scores$average[match(as.character(roster$id),as.character(scores$id))])
  keep<-is.finite(values)&!duplicated(as.character(roster$id))&!as.character(roster$id)%in%excluded_ids
  if(!include_unavailable)keep<-keep&!analytics_known_unavailable(roster)
  roster<-roster[keep,,drop=FALSE];values<-values[keep];positions<-lineup_positions(roster,multiposition)
  counts<-t(vapply(allowed,function(x)c(1L,as.integer(strsplit(x,'-',fixed=TRUE)[[1]])),integer(4)))
  maxima<-apply(counts,2,max);dims<-maxima+1L;stride<-c(1L,cumprod(dims)[1:3]);size<-prod(dims)
  lattice<-as.matrix(expand.grid(lapply(maxima,function(x)0:x)));valid<-rowSums(lattice)<=11L
  # Equal observed totals favor dual MID/FWD players in midfield; no bonus
  # points are invented. Track this in every state before retention ties.
  midfield_eligible<-vapply(positions,function(p)all(c('MID','FWD')%in%p),logical(1))
  best<-rep(-Inf,size);best[1]<-0;midfield<-integer(size);retained<-integer(size)
  prev<-matrix(0L,nrow=nrow(roster),ncol=size);chosen<-matrix(0L,nrow=nrow(roster),ncol=size)
  current_ids<-if(is.list(current))current$starters else character();role_names<-c('GK','DEF','MID','FWD')
  for(i in seq_len(nrow(roster))){
    old<-best;old_midfield<-midfield;old_kept<-retained;next_best<-best;next_midfield<-midfield;next_kept<-retained;prev[i,]<-seq_len(size)
    for(role in which(role_names%in%positions[[i]])){
      from<-which(is.finite(old)&valid&lattice[,role]<maxima[role]&rowSums(lattice)<11L);if(!length(from))next
      to<-from+stride[role];candidate<-old[from]+values[i];kept<-old_kept[from]+as.integer(roster$id[i]%in%current_ids)
      mids<-old_midfield[from]+as.integer(role_names[role]=='MID'&&midfield_eligible[i])
      better<-candidate>next_best[to]+1e-9|(abs(candidate-next_best[to])<=1e-9&
        (mids>next_midfield[to]|(mids==next_midfield[to]&kept>next_kept[to])))
      hit<-which(better);if(!length(hit))next
      dest<-to[hit];next_best[dest]<-candidate[hit];next_midfield[dest]<-mids[hit];next_kept[dest]<-kept[hit];prev[i,dest]<-from[hit];chosen[i,dest]<-role
    }
    best<-next_best;midfield<-next_midfield;retained<-next_kept
  }
  ends<-1L+as.vector(counts%*%stride);viable<-which(is.finite(best[ends]));if(!length(viable))return(NULL)
  rank<-order(-best[ends[viable]],-midfield[ends[viable]],allowed[viable]!=(current$strategy %||% ''),-retained[ends[viable]],match(allowed[viable],lineup_formations()))
  pick<-viable[rank[1]];state<-ends[pick];assignment<-setNames(vector('list',4L),role_names)
  for(i in rev(seq_len(nrow(roster)))){
    role<-chosen[i,state];if(role)assignment[[role]]<-c(as.character(roster$id[i]),assignment[[role]])
    state<-prev[i,state]
  }
  draft<-lineup_empty(allowed[pick]);draft$starters<-unname(unlist(assignment[c('FWD','MID','DEF','GK')],use.names=FALSE))
  bench<-lineup_bench(roster,scores,draft$starters,if(bench_enabled)4L else 0L,multiposition)
  draft$bench[seq_along(bench$ids)]<-bench$ids
  captain<-current$captain %||% ''
  if(!captain%in%draft$starters){starter_scores<-scores$average[match(draft$starters,scores$id)];captain<-draft$starters[order(-starter_scores,draft$starters)[1]]}
  draft$captain<-captain
  list(draft=draft,total=best[ends[pick]],bench_total=bench$total,coverage=bench$coverage,
    incomplete=any(!is.finite(scores$average)),rounds=attr(scores,'rounds'))
}
lineup_bench <- function(roster,scores,exclude=character(),size=4L,multiposition=TRUE) {
  positions<-lineup_positions(roster,multiposition);values<-as.numeric(scores$average[match(roster$id,scores$id)])
  eligible<-!roster$id%in%exclude&is.finite(values)&vapply(positions,function(p)any(p%in%c('DEF','MID','FWD')),logical(1))
  roster<-roster[eligible,,drop=FALSE];values<-values[eligible];positions<-positions[eligible]
  order_ids<-order(as.character(roster$id));roster<-roster[order_ids,,drop=FALSE];values<-values[order_ids];positions<-positions[order_ids]
  size<-min(as.integer(size),nrow(roster));if(size<=0)return(list(ids=character(),coverage=character(),total=0))
  # Mask records roles assigned to distinct substitutes; the fourth may add
  # points without adding a role. Coverage has priority over total score.
  best<-matrix(-Inf,size+1L,8L);best[1,1]<-0;paths<-vector('list',(size+1L)*8L)
  index<-function(k,mask)k+1L+(size+1L)*mask
  paths[[1]]<-integer()
  for(i in seq_len(nrow(roster))){
    old<-best;old_paths<-paths
    for(k in 0:(size-1L))for(mask in 0:7){
      if(!is.finite(old[k+1,mask+1]))next
      bits<-which(c('DEF','MID','FWD')%in%positions[[i]])-1L
      masks<-unique(c(mask,vapply(bits,function(bit)bitwOr(mask,bitwShiftL(1L,bit)),integer(1))))
      for(new_mask in masks){
        score<-old[k+1,mask+1]+values[i]
        if(score>best[k+2,new_mask+1]+1e-9){best[k+2,new_mask+1]<-score;paths[[index(k+1,new_mask)]]<-c(old_paths[[index(k,mask)]],i)}
      }
    }
  }
  masks<-which(is.finite(best[size+1,]))-1L;coverage<-vapply(masks,function(mask)sum(as.logical(intToBits(mask)[1:3])),integer(1))
  pick<-masks[order(-coverage,-best[size+1,masks+1],masks)[1]];rows<-paths[[index(size,pick)]]
  rows<-rows[order(-values[rows],as.character(roster$id[rows]))]
  list(ids=as.character(roster$id[rows]),coverage=c('DEF','MID','FWD')[as.logical(intToBits(pick)[1:3])],total=sum(values[rows]))
}
lineup_reform <- function(draft,strategy,roster,multiposition) {
  if(identical(strategy,draft$strategy))return(draft)
  roles<-lineup_slot_roles(strategy);pos<-lineup_positions(roster,multiposition)
  ids<-draft$starters[nzchar(draft$starters)];new<-lineup_empty(strategy);new$bench<-draft$bench
  # Maximum bipartite matching retains the largest valid set on formation change.
  slots<-rep('',11L)
  place<-function(id,visited){
    options<-which(roles%in%pos[[match(id,roster$id)]]&!visited)
    for(slot in options){visited[slot]<-TRUE;old<-slots[slot]
      if(!nzchar(old)||place(old,visited)){slots[slot]<<-id;return(TRUE)}}
    FALSE
  }
  for(id in ids[ids%in%roster$id])place(id,rep(FALSE,11L))
  new$starters<-slots;new$captain<-if(draft$captain%in%slots)draft$captain else '';new
}
lineup_assign <- function(draft,roster,rules,bench,index,id) {
  if(!is.character(id)||length(id)!=1L||(!id%in%roster$id&&nzchar(id)))stop('Player is no longer in your squad')
  roles<-lineup_slot_roles(draft$strategy);positions<-lineup_positions(roster,rules$multiposition)
  valid<-function(player,is_bench,slot)!nzchar(player)||is_bench||roles[slot]%in%positions[[match(player,roster$id)]]
  target<-if(bench)'bench'else'starters';old<-draft[[target]][index]
  if(!valid(id,bench,index))stop('Player cannot play in this position')
  for(kind in c('starters','bench')){
    from<-which(draft[[kind]]==id&nzchar(id))
    if(length(from)){slot<-from[1];if(kind==target&&slot==index)return(draft)
      if(!valid(old,kind=='bench',slot))stop('These players cannot swap positions; clear the original position first')
      draft[[kind]][slot]<-old}
  }
  draft[[target]][index]<-id
  if(!draft$captain%in%draft$starters)draft$captain<-''
  draft
}
lineup_validate <- function(draft,roster,rules) {
  issues<-character()
  if(!is.list(draft)||!draft$strategy%in%rules$allowed)return('Formation is not currently available')
  if(length(draft$starters)!=11L||any(!nzchar(draft$starters)))issues<-c(issues,'Select all eleven starters')
  ids<-c(draft$starters,draft$bench);ids<-ids[nzchar(ids)]
  if(anyDuplicated(ids))issues<-c(issues,'A player occupies more than one place')
  if(any(!ids%in%roster$id))issues<-c(issues,'A selected player is no longer in your squad')
  positions<-lineup_positions(roster,rules$multiposition);roles<-lineup_slot_roles(draft$strategy)
  if(length(draft$starters)==11L)for(i in seq_len(11L))if(nzchar(draft$starters[i])&&draft$starters[i]%in%roster$id&&
    !roles[i]%in%positions[[match(draft$starters[i],roster$id)]])issues<-c(issues,'A player is assigned to an ineligible position')
  if(!rules$bench_enabled&&any(nzchar(draft$bench)))issues<-c(issues,'Substitutes are disabled in this league')
  if(rules$captain_enabled&&!draft$captain%in%draft$starters)issues<-c(issues,'Select a captain from the starting XI')
  unique(issues)
}
lineup_bench_coverage <- function(roster,ids,multiposition=TRUE) {
  # Reuse the exact distinct-substitute coverage solver with equal weights.
  rows<-roster[roster$id%in%ids[nzchar(ids)],,drop=FALSE]
  if(!nrow(rows))return(character())
  lineup_bench(rows,data.frame(id=rows$id,average=0),size=nrow(rows),multiposition=multiposition)$coverage
}
