# Unknown round numbers retain a distinct official ID until schedule lookup.
award_round_labels <- function(numbers,ids) {
  vapply(seq_along(ids),function(i){
    number<-fm_number(numbers[i]);id<-mvp_text(ids[i])
    if(is.finite(number))paste("Round",number)else paste0("Round (",id,")")
  },character(1))
}
# Reuse scoped persisted request metadata without inventing missing ordinals.
award_catalog_numbers <- function(rounds,saved) {
  if(!is.data.frame(rounds)||!nrow(rounds)||!is.data.frame(saved)||!nrow(saved)||
     !all(c("round_id","round_number")%in%names(saved)))return(rounds)
  for(i in which(!is.finite(rounds$round_number))){
    numbers<-unique(saved$round_number[saved$round_id%in%rounds$round_id[i]])
    numbers<-numbers[is.finite(numbers)&numbers>=1&numbers==floor(numbers)]
    if(length(numbers)==1L)rounds$round_number[i]<-numbers
  }
  rounds
}
# Full-round snapshots are immutable; only acknowledged revisions count as saved.
award_selection_valid <- function(rows) {
  is.data.frame(rows) && nrow(rows)==11L && all(c("round_id","player_id","is_mvp")%in%names(rows)) &&
    length(unique(rows$round_id))==1L && all(nzchar(rows$player_id)) && !anyDuplicated(rows$player_id) && sum(rows$is_mvp%in%TRUE)==1L
}
# Object key order is not scoring. Keep array order and values, including NULL,
# and restore record arrays simplified to data frames by persisted JSON reads.
award_canonical_scoring <- function(value) {
  if(is.data.frame(value)){
    record<-function(frame,i)lapply(frame,function(column){
      if(is.data.frame(column))record(column,i)else if(is.list(column))column[[i]]else column[i]
    })
    value<-lapply(seq_len(nrow(value)),function(i)record(value,i))
  }
  if(!is.list(value))return(value)
  value<-lapply(value,award_canonical_scoring);keys<-names(value)
  if(!is.null(keys)&&!anyNA(keys)&&all(nzchar(keys))&&!anyDuplicated(keys))value<-value[order(keys,method="radix")]
  value
}
award_scoring_version <- function(point_system=NULL,custom=NULL) {
  if(!is.atomic(point_system)||length(point_system)!=1L||is.na(point_system)||!nzchar(as.character(point_system)))return("unknown")
  if(identical(mvp_text(point_system),"custom")&&is.null(custom))return("unknown")
  scoring<-award_canonical_scoring(list(point_system=point_system,custom=custom))
  mvp_text(as.character(openssl::sha256(charToRaw(jsonlite::toJSON(scoring,auto_unbox=TRUE,null="null",digits=NA)))))
}
award_expected_scoring <- function(login,championship_id) {
  active<-tryCatch(get_active_championships(login),error=function(e)NULL)
  championships<-Filter(function(x)is.list(x)&&identical(mvp_text(x$id %||% x$`_id`),as.character(championship_id)),mvp_records(active$championships))
  if(length(championships)!=1L)return("unknown")
  selected<-championships[[1]]
  award_scoring_version(selected$pointSystem %||% selected$ps,selected$custom)
}
award_saved_rows <- function(snapshots,championship_id,season,expected_scoring="unknown",account_id=NULL) {
  if(!nzchar(mvp_text(account_id)))return(data.frame())
  snapshots<-Filter(function(snapshot){
    identical(mvp_text(snapshot$account_id),as.character(account_id))&&
      identical(mvp_text(snapshot$championship_id),as.character(championship_id))&&
      identical(mvp_text(snapshot$season,"unknown"),as.character(season))
  },snapshots)
  dplyr::bind_rows(lapply(snapshots,function(snapshot){
    payload<-snapshot$payload
    version<-mvp_text(payload$scoring_version,"unknown")
    # Raw persisted scoring is authoritative, including unknown or contradictory
    # metadata. Only metadata-free legacy rows retain their stored-version fallback.
    if(any(c("pointSystem","custom")%in%names(payload)))
      version<-award_scoring_version(payload$pointSystem,payload$custom)
    if(expected_scoring!="unknown"&&!identical(version,expected_scoring))return(NULL)
    if(!identical(mvp_text(payload$round_id),mvp_text(snapshot$source_id)))return(NULL)
    rows<-tryCatch(rival_award_snapshot_rows(payload$players),error=function(e)data.frame())
    if(!award_selection_valid(rows)||!all(rows$championship_id%in%championship_id)||!all(rows$round_id%in%snapshot$source_id))return(NULL)
    # Legacy hardcoded TRUE is not finality evidence. New snapshots carry metadata.
    finality<-mvp_text(payload$finality,"unknown")
    rows$is_finished<-if(finality=="final")TRUE else if(finality=="provisional")FALSE else NA
    rows$finality<-finality;rows$checked_at<-snapshot$observed_at;rows$persistence<-"Saved"
    rows$season<-season;rows$scoring_version<-version
    rows
  }))
}
# A selection captured before settlement can still be provisional even when the
# catalog called the round closed. Recheck once after a dated final-category payment.
award_settlement_rechecks <- function(evidence,rows,catalog) {
  if(!is.data.frame(catalog)||!all(c("round_id","round_number")%in%names(catalog)))return(character())
  movements<-rival_evidence_rows(evidence,"movements")
  teams<-unique(vapply(movements,function(x)mvp_text(x$payload$team_id),character(1)))
  payments<-dplyr::bind_rows(lapply(teams,function(id)
    rival_normalize_movements(rival_cash_latest_movements(evidence,id),catalog)))
  if(!nrow(payments))return(character())
  payments<-payments[payments$bonus_type%in%c("ranking","dreamteam","mvp") &
    is.finite(payments$money)&payments$money>0 & !is.na(fm_time(payments$date)),,drop=FALSE]
  Filter(function(id){
    checked<-if(is.data.frame(rows)&&all(c("round_id","checked_at")%in%names(rows)))
      fm_time(rows$checked_at[rows$round_id%in%id])else as.POSIXct(character())
    !length(checked)||anyNA(checked)||min(checked)<max(fm_time(payments$date[payments$round_id%in%id]))
  },intersect(unique(payments$round_id),catalog$round_id))
}
get_mvp_saved_catalog <- function(login,championship_id) {
  season<-league_season_context(championship_id)
  saved<-tryCatch(read_rival_finance_evidence(login[["userid"]],championship_id,season),error=function(e)list(evidence=list(),complete=FALSE))
  expected<-award_expected_scoring(login,championship_id)
  rows<-award_saved_rows(rival_evidence_rows(saved$evidence,"awards"),championship_id,season,expected,login[["userid"]])
  catalog<-get_award_round_catalog(login,championship_id,rows)
  list(saved=rows,expected_scoring=expected,saved_complete=isTRUE(saved$complete),catalog=catalog,season=season,
    settlement_rechecks=award_settlement_rechecks(saved$evidence,rows,award_catalog_numbers(catalog$rounds,rows)))
}
award_snapshot_evidence <- function(result,login,championship_id,reference,context) {
  if(!identical(result$status,"ok")||!award_selection_valid(result$rows))return(NULL)
  stamp<-attr(result,"observed_at") %||% Sys.time()
  if(identical(attr(result,"fetch_status"),"stale")||is.na(fm_time(stamp)))return(NULL)
  metadata<-result$metadata %||% list()
  scoring<-award_scoring_version(metadata$pointSystem,metadata$custom)
  payload<-list(round_id=reference$round_id,round_number=reference$round_number,
    players=lapply(seq_len(nrow(result$rows)),function(i)as.list(result$rows[i,,drop=FALSE])),
    strategy=metadata$strategy,pointSystem=metadata$pointSystem,custom=metadata$custom,
    scoring_version=scoring,finality=mvp_text(metadata$finality,"unknown"),publication="official_selection_observed")
  rival_finance_evidence(login[["userid"]],championship_id,context$season,"awards",reference$round_id,payload,stamp)
}
persist_official_award_snapshot <- function(result,login,championship_id,reference,context,on_complete=NULL) {
  row<-award_snapshot_evidence(result,login,championship_id,reference,context)
  if(is.null(row)){if(is.function(on_complete))on_complete(FALSE);return(FALSE)}
  defer_persistence("supabase_post_direct",list("rival_finance_evidence",list(row),
    "account_id,championship_id,season,evidence_type,source_id,observed_at",TRUE),context,on_complete)
}
# Eligibility for retrieval is separate from finality/credited rewards. A known
# current or later round is not due unless the source explicitly confirms closure.
award_round_pending <- function(reference,current_round_number=reference$current_round_number,current_round_id=reference$current_round_id) {
  if(isTRUE(reference$is_finished))return(FALSE)
  if(identical(reference$is_finished,FALSE))return(TRUE)
  current_id<-mvp_text(current_round_id)
  if(nzchar(current_id)&&identical(mvp_text(reference$round_id),current_id))return(TRUE)
  number<-fm_number(reference$round_number);current<-fm_number(current_round_number)
  is.finite(number)&&is.finite(current)&&number>=current
}
award_coverage <- function(rounds,rows,failures=list(),current_round_number=NA_real_,current_round_id="") {
  if(!is.data.frame(rounds)||!nrow(rounds))return(data.frame())
  dplyr::bind_rows(lapply(seq_len(nrow(rounds)),function(i){
    round<-rounds[i,,drop=FALSE];group<-if(is.data.frame(rows)&&"round_id"%in%names(rows))rows[rows$round_id%in%round$round_id,,drop=FALSE]else data.frame()
    required<-!mvp_text(round$status)%in%c("verified_future")&&!award_round_pending(round,current_round_number,current_round_id)
    valid<-award_selection_valid(group)
    finality<-if(!valid){if(required)"Missing"else "Not closed"}else if(all(group$is_finished%in%TRUE))"Confirmed final"else if(any(group$is_finished%in%FALSE))"Provisional / in progress"else "Finality unknown"
    data.frame(round_id=round$round_id,round_number=round$round_number,selection=if(valid)"11 players"else if(required)"Missing"else "Not due",required=required,
      finality=finality,mvp=if(valid)mvp_text(group$player_name[group$is_mvp%in%TRUE])else "",
      last_fetched=if(valid)mvp_text(group$checked_at[1])else "",persistence=if(valid)mvp_text(group$persistence[1],"Live only")else "Not saved",
      issue=if(required)mvp_text(failures[[round$round_id]])else "",stringsAsFactors=FALSE)
  }))
}
shared_mvp_source <- function(session,login,championship,active,refresh=NULL) {
  root<-session$rootScope()
  if(is.null(root$userData$round_publication_revision))root$userData$round_publication_revision<-shiny::reactiveVal(0L)
  if(is.null(root$userData$mvp_shared)){
    state<-new.env(parent=emptyenv());state$value<-shiny::reactiveVal(mvp_result("loading",reason="Loading saved round awards."))
    state$key<-NULL;state$generation<-0L;state$closed<-FALSE;state$loading<-FALSE;state$running<-FALSE
    state$rows<-data.frame();state$rounds<-empty_finished_rounds();state$queue<-list();state$inflight<-0L;state$writes<-0L
    state$catalog_complete<-state$catalog_identity_complete<-state$saved_complete<-FALSE;state$catalog_reason<-"";state$needs_recheck<-FALSE;state$expected_scoring<-"unknown"
    state$failures<-list();state$unsaved<-list();state$fetched<-0L;state$saved<-0L;state$failed<-0L;state$consumers<-list()
    root$onSessionEnded(function(){state$closed<-TRUE;state$generation<-state$generation+1L})
    root$userData$mvp_shared<-state
  }
  state<-root$userData$mvp_shared;state$consumers[[session$ns("mvp_consumer")]]<-active
  enabled<-function()shiny::isolate(any(vapply(state$consumers,function(f)isTRUE(f()),logical(1))))
  identity<-function(){auth<-login();if(!valid_login(auth))return("");paste(auth[["userid"]],championship(),league_season_context(championship()),sep="|")}
  current<-function(generation,key) !state$closed&&identical(generation,state$generation)&&identical(key,state$key)&&identical(shiny::isolate(identity()),key)
  publish<-function(){
    coverage<-award_coverage(state$rounds,state$rows,state$failures,state$current_round_number,state$current_round_id)
    required<-if(nrow(coverage))coverage[coverage$required%in%TRUE,,drop=FALSE]else coverage
    # Missing completion flags on future rounds are not missing historical data.
    relevant<-if(nrow(required))match(required$round_id,state$rounds$round_id)else integer()
    scoped_catalog_complete<-isTRUE(state$catalog_identity_complete)&&(!nrow(required)||
      all(is.finite(required$round_number)&(state$rounds$is_finished[relevant]%in%TRUE|required$finality=="Confirmed final")))
    complete<-nrow(required)>0L&&(isTRUE(state$catalog_complete)||scoped_catalog_complete)&&isTRUE(state$saved_complete)&&all(required$persistence=="Saved")&&all(required$selection=="11 players")
    catalog_reason<-state$catalog_reason
    if(scoped_catalog_complete&&identical(catalog_reason,"Some round numbers or completion states are not confirmed. Retrieve missing awards to check their schedules."))catalog_reason<-""
    reason<-paste(unique(c(catalog_reason,if(!state$saved_complete)"Saved coverage could not be fully read.",
      if(nrow(coverage)&&any(coverage$selection=="Missing"))"Some round awards are missing. Retrieve them in Admin.",
      if(nrow(required)&&any(required$finality!="Confirmed final"))"Unfinished or unverified selections do not establish credited rewards.",
      if(length(state$unsaved))"Some fetched selections could not be saved. Retry saving in Admin.")),collapse=" ")
    state$value(list(status=if(state$loading||state$running)"loading"else if(complete)"ok"else"partial",rows=state$rows,
      complete=complete,reason=reason,coverage=coverage,loading=state$loading,running=state$running,
      fetched=state$fetched,saved=state$saved,failed=state$failed,pending=length(state$queue)+state$inflight+state$writes,
      unsaved=length(state$unsaved),needs_recheck=isTRUE(state$needs_recheck)))
  }
  # No per-round publication: consumers update on completion or a two-second cap.
  state$last_publish<-state$last_publish %||% as.POSIXct(0,origin="1970-01-01",tz="UTC")
  progress<-function(force=FALSE){if(force||as.numeric(difftime(Sys.time(),state$last_publish,units="secs"))>=2){state$last_publish<-Sys.time();publish()}}
  replace_rows<-function(rows){
    if(is.data.frame(state$rows)&&"round_id"%in%names(state$rows))
      state$rows<-state$rows[!state$rows$round_id%in%rows$round_id,,drop=FALSE]
    else state$rows<-data.frame()
    state$rows<-dplyr::bind_rows(state$rows,rows)
  }
  save_result<-function(result,reference,auth,cid,ctx,generation,key){
    id<-reference$round_id;state$writes<-state$writes+1L
    callback<-function(ok){
      if(!current(generation,key))return(invisible(NULL))
      state$writes<-max(0L,state$writes-1L)
      if(isTRUE(ok)){
        state$saved<-state$saved+1L;state$unsaved[[id]]<-NULL;state$failures[[id]]<-NULL
        state$rows$persistence[state$rows$round_id%in%id]<-"Saved"
        invalidate_resources(ctx,pattern="rival_finance_evidence_")
      }else{
        state$failed<-state$failed+1L;state$unsaved[[id]]<-list(result=result,reference=reference)
        state$rows$persistence[state$rows$round_id%in%id]<-"Save failed";state$failures[[id]]<-"Database save failed; live selection retained."
      }
      pump();invisible(NULL)
    }
    called<-FALSE;once<-function(ok){if(called)return(invisible(NULL));called<<-TRUE;callback(ok)}
    accepted<-tryCatch(persist_official_award_snapshot(result,auth,cid,reference,ctx,once),error=function(e)FALSE)
    if(!isTRUE(accepted))once(FALSE)
  }
  pump<-function(){
    if(state$closed)return(invisible(NULL))
    if(!enabled()||(!is.null(state$acquisition_active)&&!isTRUE(shiny::isolate(state$acquisition_active())))){state$queue<-list()}
    while(length(state$queue)&&state$inflight<2L&&state$writes<2L&&enabled()){
      task<-state$queue[[1]];state$queue<-state$queue[-1];auth<-shiny::isolate(login());cid<-shiny::isolate(championship())
      generation<-state$generation;key<-state$key;ctx<-resource_context(auth,cid,season=state$season,session_id=root$token)
      if(identical(task$kind,"save")){save_result(task$result,task$reference,auth,cid,ctx,generation,key);next}
      state$inflight<-state$inflight+1L
      # Explicit acquisition bypasses both wrapper and source caches for this round.
      invalidate_resources(resource_context(auth,cid),resources=paste0("official_dreamteam_",cid,"_",task$reference$round_id))
      local({ref<-task$reference;g<-generation;k<-key;a<-auth;c<-cid;x<-ctx
        finish<-function(result){
          if(!current(g,k))return(NULL)
          state$inflight<-max(0L,state$inflight-1L)
          resolved<-result$reference %||% ref
          if(identical(mvp_text(resolved$round_id),mvp_text(ref$round_id))){
            index<-which(state$rounds$round_id%in%ref$round_id)
            if(is.finite(fm_number(resolved$round_number)))state$rounds$round_number[index]<-resolved$round_number
            if(isTRUE(resolved$is_finished))state$rounds$is_finished[index]<-TRUE
            if(identical(result$status,"not_due"))state$rounds$status[index]<-"verified_future"
          }else resolved<-ref
          if(identical(result$status,"not_due")){
            state$failures[[ref$round_id]]<-NULL
          }else if(!identical(result$status,"ok")||!award_selection_valid(result$rows)){
            state$failed<-state$failed+1L;state$failures[[ref$round_id]]<-mvp_text(result$reason,"Round selection unavailable or malformed.")
          }else{
            version<-award_scoring_version(result$metadata$pointSystem,result$metadata$custom)
            compatible<-identical(state$expected_scoring,"unknown")||identical(version,state$expected_scoring)
            if(!compatible){
              state$failed<-state$failed+1L;state$failures[[ref$round_id]]<-if(identical(version,"unknown"))
                "Selection scoring metadata is missing or unrecognized; compatibility with the league configuration could not be verified."else
                "Selection scoring differs from the league configuration; it was not used."
            }else{
              state$fetched<-state$fetched+1L;rows<-result$rows;rows$checked_at<-rival_finance_stamp(attr(result,"observed_at") %||% Sys.time())
              rows$finality<-mvp_text(result$metadata$finality,"unknown");rows$persistence<-"Saving";rows$season<-state$season
              rows$scoring_version<-version;replace_rows(rows)
              save_result(result,resolved,a,c,x,g,k)
            }
          }
          pump();NULL
        }
        promises::then(async_adapter_fetch("get_official_dreamteam_result",list(login=a,championship_id=c,reference=ref),x,
          is_current=function()current(g,k)),finish,function(e)finish(mvp_result("unavailable",reason="Round request failed. Retry this round.")))
      })
    }
    state$running<-length(state$queue)+state$inflight+state$writes>0L
    progress(!state$running);invisible(NULL)
  }
  request<-function(mode="missing",round_id=NULL){
    if(!isTRUE(shiny::isolate(active()))||state$loading||state$running||!nzchar(shiny::isolate(identity()))||!identical(state$key,shiny::isolate(identity())))return(FALSE)
    coverage<-award_coverage(state$rounds,state$rows,state$failures,state$current_round_number,state$current_round_id);if(!nrow(coverage))return(FALSE)
    state$acquisition_active<-active
    launch<-function(){
      if(!isTRUE(shiny::isolate(active()))){publish();return(FALSE)}
      eligible<-award_coverage(state$rounds,state$rows,state$failures,state$current_round_number,state$current_round_id)
      # Choose after a catalog refresh so a newly closed round is included.
      ids<-switch(mode,missing=eligible$round_id[eligible$selection=="Missing"&eligible$required%in%TRUE],
        refresh=eligible$round_id[eligible$required%in%TRUE&(eligible$finality!="Confirmed final"|isTRUE(state$needs_recheck))],
        selected=intersect(as.character(round_id),eligible$round_id),character())
      ids<-intersect(ids,eligible$round_id[eligible$required%in%TRUE])
      state$queue<-lapply(head(unique(ids),120L),function(id){row<-state$rounds[state$rounds$round_id%in%id,,drop=FALSE];
        list(kind="round",reference=list(round_id=id,round_number=row$round_number[1],is_finished=row$is_finished[1],
          user_team_id=state$user_team_id,current_round_number=state$current_round_number,current_round_id=state$current_round_id))})
      state$fetched<-state$saved<-state$failed<-0L;state$needs_recheck<-FALSE;state$running<-length(state$queue)>0L;publish();pump();TRUE
    }
    if(mode%in%c("missing","refresh","selected")){
      # Recheck metadata for every explicit batch, including rounds that closed
      # while Admin stayed open. The numbered catalog/schedule caches are reused.
      auth<-shiny::isolate(login());cid<-shiny::isolate(championship());g<-state$generation;k<-state$key
      ctx<-resource_context(auth,cid,season=state$season,session_id=root$token)
      invalidate_resources(resource_context(auth,cid),resources=paste0(c("mvp_catalog_","award_round_metadata_"),cid));state$loading<-TRUE;publish()
      promises::then(async_adapter_fetch("get_award_round_catalog",list(login=auth,championship_id=cid,saved=state$rows),ctx,is_current=function()current(g,k)),function(catalog){
        if(!current(g,k))return(NULL)
        state$loading<-FALSE
        if(is.data.frame(catalog$rounds)&&nrow(catalog$rounds)){
          state$rounds<-award_catalog_numbers(catalog$rounds,state$rows);state$catalog_complete<-isTRUE(catalog$complete);state$catalog_identity_complete<-isTRUE(catalog$identity_complete %||% catalog$complete);state$catalog_reason<-catalog$reason %||% ""
          state$user_team_id<-catalog$user_team_id;state$current_round_number<-catalog$current_round_number;state$current_round_id<-catalog$current_round_id;launch()
        }else{state$catalog_reason<-"Round status could not be refreshed. Retry before refreshing selections.";publish()};NULL
      },function(e){if(current(g,k)){state$loading<-FALSE;state$catalog_reason<-"Round status could not be refreshed. Retry before refreshing selections.";publish()};NULL})
      return(TRUE)
    }
    launch()
  }

  retry_save<-function(){
    if(!isTRUE(shiny::isolate(active()))||state$loading||state$running||!identical(state$key,shiny::isolate(identity())))return(FALSE)
    state$acquisition_active<-active
    state$queue<-lapply(state$unsaved,function(task)c(list(kind="save"),task));state$fetched<-state$saved<-state$failed<-0L
    state$running<-length(state$queue)>0L;publish();pump();TRUE
  }
  shiny::observe({
    if(!isTRUE(active())&&!is.null(state$acquisition_active)&&!isTRUE(shiny::isolate(state$acquisition_active()))&&length(state$queue)){state$queue<-list();state$running<-state$inflight+state$writes>0L;publish()}
    key<-identity()
    if(!nzchar(key)||(!is.null(state$key)&&!identical(key,state$key))){
      state$generation<-state$generation+1L;state$key<-NULL;state$rows<-data.frame();state$rounds<-empty_finished_rounds()
      state$queue<-list();state$inflight<-state$writes<-0L;state$loading<-state$running<-FALSE;state$unsaved<-state$failures<-list()
      state$catalog_complete<-state$catalog_identity_complete<-state$saved_complete<-FALSE;state$catalog_reason<-"";state$needs_recheck<-FALSE;publish()
    }
    if(!nzchar(key)||!isTRUE(active()))return()
    publication<-root$userData$round_publication_revision();version<-if(is.function(refresh))refresh()else 0
    if(!identical(state$key,key)||(!state$running&&!identical(state$refresh,version))){
      state$key<-key;state$refresh<-version;state$generation<-state$generation+1L;g<-state$generation
      state$loading<-TRUE;state$season<-league_season_context(championship());state$catalog_complete<-state$catalog_identity_complete<-state$saved_complete<-FALSE;state$catalog_reason<-"";publish()
      auth<-login();cid<-championship();ctx<-resource_context(auth,cid,season=state$season,session_id=root$token)
      promises::then(async_adapter_fetch("get_mvp_saved_catalog",list(login=auth,championship_id=cid),ctx,is_current=function()current(g,key)),function(result){
        if(!current(g,key))return(NULL)
        state$loading<-FALSE;state$expected_scoring<-mvp_text(result$expected_scoring,"unknown");state$rows<-result$saved %||% data.frame();state$rounds<-award_catalog_numbers(result$catalog$rounds %||% empty_finished_rounds(),state$rows)
        state$catalog_complete<-isTRUE(result$catalog$complete);state$catalog_identity_complete<-isTRUE(result$catalog$identity_complete %||% result$catalog$complete);state$saved_complete<-isTRUE(result$saved_complete);state$catalog_reason<-result$catalog$reason %||% ""
        state$user_team_id<-result$catalog$user_team_id;state$current_round_number<-result$catalog$current_round_number;state$current_round_id<-result$catalog$current_round_id
        # Active-session recovery only; the normal bounded async acquisition and
        # acknowledged persistence path handles these paid-round revisions.
        ids<-intersect(result$settlement_rechecks %||% character(),state$rounds$round_id)
        if(length(ids)&&enabled()){
          state$acquisition_active<-active
          state$queue<-lapply(head(unique(ids),120L),function(id){
            row<-state$rounds[state$rounds$round_id%in%id,,drop=FALSE]
            list(kind="round",reference=list(round_id=id,round_number=row$round_number[1],is_finished=TRUE,
              user_team_id=state$user_team_id,current_round_number=state$current_round_number,current_round_id=state$current_round_id))
          })
          state$running<-TRUE
        }
        publish();pump();NULL
      },function(e){if(current(g,key)){state$loading<-FALSE;state$catalog_reason<-"Saved awards or round catalog could not load. Refresh to retry.";publish()};NULL})
    }
    if(!is.null(state$publication)&&!identical(state$publication,publication)){state$needs_recheck<-TRUE;publish()}
    state$publication<-publication
    # Active Admin navigation resumes no stopped acquisition without a new click.
    invisible(NULL)
  })
  out<-shiny::reactive(state$value());attr(out,"retrieve")<-request;attr(out,"retry_save")<-retry_save;out
}
