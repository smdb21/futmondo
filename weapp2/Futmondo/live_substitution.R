# Manual changes to the effective, round-specific pitch.
live_sub_rounds <- function(rounds) {
  if(!is.data.frame(rounds) || !all(c("round_id","round_number","is_finished") %in% names(rounds)))
    return(data.frame())
  playing <- tolower(rounds$status %||% rep("",nrow(rounds))) %in% c("running","active","playing","in_progress")
  partial <- (rounds$partially_played %||% rep(FALSE,nrow(rounds))) %in% TRUE
  rows <- rounds[(playing | partial) & !rounds$is_finished %in% TRUE &
    !is.na(rounds$round_id) & nzchar(rounds$round_id) & is.finite(rounds$round_number),,drop=FALSE]
  rows[order(rows$round_number,decreasing=TRUE),,drop=FALSE]
}

live_sub_issue <- function(message) {
  structure(list(message=message,call=NULL),class=c("live_sub_issue","error","condition"))
}

live_sub_matches <- function(answer, round) {
  if(!identical(mvp_text(answer$round$`_id`),round$id) ||
     !isTRUE(fm_number(answer$round$number)==round$number))stop(live_sub_issue("The official fixture round changed."))
  matches <- mvp_records(answer$matches)
  if(!length(matches)||length(matches)>50L)stop(live_sub_issue("The round fixtures are unavailable."))
  rows <- dplyr::bind_rows(lapply(matches,function(m)data.frame(match_id=mvp_text(m$id),
    home_id=mvp_text(m$homeTeam$id),away_id=mvp_text(m$awayTeam$id),kickoff=mvp_text(m$date),
    status=if(isTRUE(fm_number(m$minutesPlayed)>0))"started" else mvp_text(m$status))))
  if(any(!nzchar(rows$match_id))||anyDuplicated(rows$match_id)||any(!nzchar(rows$home_id)|
    !nzchar(rows$away_id)|rows$home_id==rows$away_id))stop(live_sub_issue("The round fixture identities are incomplete."))
  rows
}

live_sub_options <- function(answer, fixtures, now=Sys.time()) {
  lineup <- normalize_round_lineup(answer)
  issue <- if(!lineup$status %in% c("running","playing","in_progress","active")) "Round is not open for substitutions"
    else if(identical(answer$bench$enabled,FALSE)) "Bench changes disabled"
    else if(!isTRUE(answer$bench$enabled)) "Bench permissions unavailable"
    else if(!lineup$strategy %in% lineup_formations()) "Formation unavailable"
  players <- c(lineup$players,lineup$bench)
  ids <- vapply(players,function(p)mvp_text(p$id),character(1))
  if(anyDuplicated(ids[nzchar(ids)]))issue <- "Conflicting player identities"
  slots <- lapply(players,function(p){
    date <- NA_real_
    reason <- if(is.null(p)||!nzchar(mvp_text(p$id))) "Empty slot" else issue
    if(is.null(reason)){
      club <- mvp_text(p$teamId)
      row <- which(nzchar(club) & (fixtures$home_id %in% club | fixtures$away_id %in% club))
      if(isTRUE(p$locked))reason <- "Locked by Futmondo"
      else if(!identical(p$locked,FALSE))reason <- "Player lock unavailable"
      else if(length(row)!=1L)reason <- "Fixture unavailable"
      else {
        date <- as.numeric(fm_time(fixtures$kickoff[row]))
        status <- tolower(mvp_text(fixtures$status[row]))
        if(!is.finite(date))reason <- "Kickoff unavailable"
        else if(date<=as.numeric(now)||status %in% c("f","started","live","running","finished"))reason <- "Match has started"
        else if(!status %in% c("p","scheduled","pending"))reason <- "Fixture status unconfirmed"
      }
    }
    list(issue=reason,kickoff=date)
  })
  roles <- if(is.null(issue))lineup_slot_roles(lineup$strategy) else rep("",11L)
  bench_roles <- lapply(lineup$bench,function(p)analytics_player_positions(data.frame(
    role=mvp_text(p$role),role2=mvp_text(p$role2)),multiposition=isTRUE(answer$bench$multiposition))[[1]])
  choices <- lapply(seq_len(11L),function(i){
    if(!is.null(slots[[i]]$issue))return(integer())
    which(vapply(seq_len(4L),function(j)is.null(slots[[11L+j]]$issue)&&roles[i] %in% bench_roles[[j]],logical(1)))
  })
  list(lineup=lineup,slots=slots,choices=choices)
}

live_sub_pair <- function(options,starter,sub) {
  if(!starter %in% seq_len(11L)||!sub %in% options$choices[[starter]])return(NULL)
  field <- options$lineup$players[[starter]]
  bench <- options$lineup$bench[[sub]]
  list(outPlayer=list(index=as.integer(field$position),id=field$id),
       inPlayer=list(index=as.integer(bench$position),id=bench$id),
       cutoff=min(options$slots[[starter]]$kickoff,options$slots[[11L+sub]]$kickoff))
}
live_sub_verified <- function(answer,pair) {
  pitch <- normalize_round_lineup(answer)
  ids <- vapply(c(pitch$players,pitch$bench),function(p)mvp_text(p$id),character(1))
  !anyDuplicated(ids[nzchar(ids)]) &&
    identical(pitch$players[[pair$outPlayer$index+1L]]$id,pair$inPlayer$id) &&
    identical(pitch$bench[[pair$inPlayer$index+1L]]$id,pair$outPlayer$id)
}

live_sub_read <- function(auth,ctx,round,owner) {
  lineup_api_read("/1/userteam/roundlineup",auth,ctx,owner,list(round=round$id),worker_live=TRUE)
}
live_sub_write <- function(auth,ctx,round,pair,owner,guard) {
  payload <- lineup_api_payload(auth,ctx$championship_id,ctx$user_team_id,
    c(list(roundId=round$id),pair[c("inPlayer","outPlayer")]))
  promises::then(fm_async_http("https://api.futmondo.com/2/userteam/makesubstitution","POST",
    jsonlite::toJSON(payload,auto_unbox=TRUE,null="null"),c(`Content-Type`="application/json; charset=utf-8"),
    owner=owner,priority="interactive",before_dispatch=guard),function(response){
      result <- jsonlite::fromJSON(rawToChar(response$content),simplifyVector=FALSE)
      !isTRUE(result$error) && !isTRUE(result$answer$error) &&
        all(c(result$code,result$answer$code) %in% API_CODE_OK)
    })
}

live_sub_preflight <- function(auth,ctx,round,pair,owner,current,on_conflict=NULL) {
  reads <- promises::promise_all(lineup=live_sub_read(auth,ctx,round,owner),
    matches=lineup_api_read("/1/match/list",auth,ctx,owner,list(roundId=round$id),worker_live=TRUE),
    active=lineup_api_read("/2/user/activechampionships",auth,ctx,owner,worker_live=TRUE))
  promises::then(reads,function(r){
    own <- Filter(function(champ)is.list(champ) && is.list(champ$userteam) &&
      identical(mvp_text(champ$id %||% champ$`_id`),ctx$championship_id) &&
      identical(mvp_text(champ$userteam$id %||% champ$userteam$`_id`),ctx$user_team_id),
      mvp_records(r$active$championships))
    if(length(own)!=1L)stop(live_sub_issue("Futmondo could not verify your team in this championship."))
    league <- own[[1]]$league
    id <- if(is.list(league))mvp_text(league$id %||% league$`_id`) else mvp_text(league)
    if(!nzchar(id)||!identical(id,ctx$competition_id))stop(live_sub_issue("The competition could not be verified."))
    options <- live_sub_options(r$lineup,live_sub_matches(r$matches,round))
    fresh <- live_sub_pair(options,pair$outPlayer$index+1L,pair$inPlayer$index+1L)
    if(!current())stop(live_sub_issue("The round or deadline changed before submission."))
    if(!identical(fresh,pair)){
      if(is.function(on_conflict))on_conflict(r$lineup)
      stop(live_sub_issue("The lineup, eligibility or kickoff changed. Review the pair again."))
    }
    TRUE
  })
}


live_sub_button <- function(ns,event,label,...) {
  shiny::tags$button(type="button",class="btn btn-default",
    `data-event`=as.character(jsonlite::toJSON(event,auto_unbox=TRUE)),
    `data-input`=ns("action"),
    onclick="Shiny.setInputValue(this.dataset.input, JSON.parse(this.dataset.event), {priority:'event'})",
    label,...)
}

# Independent round state lets both views share one confirmation and readback.
live_sub_round_state <- function(root,key) {
  records <- root$userData$live_subs
  if(is.null(records[[key]])){
    if(length(records)>=8L){
      settled <- which(vapply(records,function(r)is.null(shiny::isolate(r$pending)) &&
        is.null(shiny::isolate(r$save_failed)),logical(1)))
      # Retain outstanding readbacks and unacknowledged database observations.
      if(length(settled))records <- records[-head(settled,length(records)-7L)]
    }
    records[[key]] <- shiny::reactiveValues()
    root$userData$live_subs <- records
  }
  records[[key]]
}

live_sub_server <- function(id,active,login,championship,team,round_ref,calendar) {
  shiny::moduleServer(id,function(input,output,session){
    root <- session$rootScope()
    selected <- shiny::reactiveVal(NULL)
    saved <- shiny::reactiveVal(NULL)
    scope <- shiny::reactive({
      if(!isTRUE(active())||!valid_login(login()))return(NULL)
      round <- round_ref()
      if(is.null(round)||!nzchar(mvp_text(team()))||!nzchar(mvp_text(championship())))return(NULL)
      league_context_revision()
      args <- list(championship_id=championship(),user_team_id=team(),round_id=round$id)
      ctx <- fm_worker_resolve_context("get_round_lineup_snapshot",args,
        resource_context(login(),championship(),team(),session_id=session$token))
      list(ctx=ctx,round=round,key=round_lineup_snapshot_key(ctx,round$id))
    })
    worker_read <- factual_read_adapter(session,"get_round_lineup_snapshot",login,championship,team,
      is_active=active,state_only=TRUE)
    club_sheet <- club_xi_source(session,active,login,championship,scope)
    # The manual readback can be newer than the worker publication after a restart.
    observed_scope <- NULL
    shiny::observeEvent(scope(),{
      s <- scope()
      if(identical(observed_scope,s))return()
      observed_scope <<- s
      selected(NULL);saved(NULL)
      if(is.null(s))return()
      state <- live_sub_round_state(root,s$key)
      latest <- state$latest$data
      if(isTRUE(latest$valid_until>as.numeric(Sys.time())))return()
      keep <- function()shiny::isolate(identical(scope(),s))
      req <- async_adapter_fetch("read_saved_round_lineup",list(context=s$ctx,round_id=s$round$id),
        s$ctx,is_current=keep)
      promises::then(req,function(data){
        if(keep()){
          saved(list(key=s$key,data=data))
          prior <- shiny::isolate(state$latest$data)
          if(!is.null(data) && (is.null(prior)||isTRUE(data$valid_until>prior$valid_until)))
            state$latest <- list(key=s$key,data=data)
        }
        NULL
      },
        function(e){if(keep())saved(list(key=s$key,error=TRUE));NULL})
      invisible(NULL)
    },ignoreNULL=FALSE)
    snapshot <- shiny::reactive({
      s <- scope();shiny::req(s)
      state <- live_sub_round_state(root,s$key)
      db <- saved();recent <- state$latest
      items <- list(if(identical(db$key,s$key))db$data,if(identical(recent$key,s$key))recent$data)
      if(fm_worker_configured() && isTRUE(fm_worker_mode(s$ctx$account_id))){
        w <- worker_read(login=login(),championship_id=championship(),user_team_id=team(),round_id=s$round$id)
        items <- c(items,list(w$data))
      }
      items <- Filter(function(x)!is.null(x$answer)&&identical(x$round_id,s$round$id),items)
      if(!length(items))return(NULL)
      stamp <- vapply(items,function(x)fm_number(x$valid_until)-300,numeric(1))
      items[[which.max(replace(stamp,!is.finite(stamp),-Inf))]]
    })
    fixtures <- shiny::reactive({
      s <- scope();shiny::req(s)
      lineup_fixture_view(calendar(),s$round,s$ctx)$rows
    })
    options <- shiny::reactive({
      data <- snapshot();shiny::req(data)
      now <- as.numeric(Sys.time())
      result <- live_sub_options(data$answer,fixtures(),fm_time(now))
      deadlines <- vapply(result$slots,function(x)x$kickoff,numeric(1))
      due <- deadlines[is.finite(deadlines)&deadlines>now]
      if(length(due))shiny::invalidateLater(min(86400000,ceiling((min(due)-now)*1000)),session)
      result
    })
    note <- function(key,message){
      state <- live_sub_round_state(root,key)
      state$feedback <- list(key=key,message=message)
    }
    persist <- function(s,data){
      state <- live_sub_round_state(root,s$key)
      failure <- list(key=s$key,data=data)
      ack <- function(ok){
        if(!isTRUE(ok))state$save_failed <- failure
        else if(identical(shiny::isolate(state$save_failed),failure))state$save_failed <- NULL
      }
      if(!isTRUE(tryCatch(persist_round_lineup_snapshot(data,s$ctx,ack),error=function(e)FALSE)))ack(FALSE)
    }
    publish <- function(s,answer){
      state <- live_sub_round_state(root,s$key)
      stamp <- attr(answer,"observed_at") %||% Sys.time()
      data <- list(round_id=s$round$id,answer=round_lineup_clean_answer(answer),valid_until=as.numeric(stamp)+300)
      attr(data,"observed_at") <- stamp
      state$latest <- list(key=s$key,data=data)
      persist(s,data)
    }
    readback <- function(s,auth,pair,rejected=FALSE){
      state <- live_sub_round_state(root,s$key)
      owner <- paste0("live-sub:",session$token)
      fail <- function(e){
        state$pending <- list(key=s$key,pair=pair,uncertain=TRUE)
        note(s$key,"The result is uncertain. Check the official lineup again.")
        NULL
      }
      promises::catch(promises::then(live_sub_read(auth,s$ctx,s$round,owner),function(answer){
        match <- live_sub_verified(answer,pair)
        publish(s,answer)
        state$pending <- NULL
        note(s$key,if(match)"Substitution verified." else if(rejected)"Futmondo rejected the substitution."
          else "The official lineup does not show the requested substitution. Review the pitch before confirming another change.")
        NULL
      }),fail)
      invisible(NULL)
    }
    output$pitch <- shiny::renderUI({
      s <- scope();shiny::req(s)
      state <- live_sub_round_state(root,s$key)
      data <- snapshot()
      if(is.null(data))return(fm_info(if(is.null(saved()))"Loading the saved round lineup…" else
        "The synced round lineup is unavailable.",pending=is.null(saved())))
      o <- options();sel <- selected()
      if(!identical(sel$key,s$key))sel <- NULL
      pending <- state$pending
      event <- function(kind,...)list(key=s$key,kind=kind,...)
      button <- function(kind,label,...)live_sub_button(session$ns,event(kind,...),label)
      feed <- club_sheet()
      round_fixtures <- fixtures()
      eligible_fixtures <- club_xi_checkable(feed,o$lineup$players,round_fixtures,holds=state$xi_holds)
      if(identical(feed$status,"ready")){
        starts <- as.numeric(fm_time(round_fixtures$kickoff))-5400
        remaining <- c(starts,eligible_fixtures$available_at)-as.numeric(Sys.time())
        later_windows <- remaining[is.finite(remaining)&remaining>0]
        if(length(later_windows))shiny::invalidateLater(min(86400000,ceiling(min(later_windows)*1000)),session)
      }
      controls <- function(tile,i){
        player <- o$lineup$players[[i]]
        if(is.null(player))return(tile)
        badge <- club_xi_label(feed,player,round_fixtures)
        if(!is.null(badge))tile <- htmltools::tagAppendChildren(tile,
          shiny::tags$small(class=if(badge$warning)"fm-club-xi-warning" else "fm-live-sub-reason",
            title=badge$title,badge$text))
        issue <- o$slots[[i]]$issue
        if(is.null(issue)&&!length(o$choices[[i]]))issue <- "No eligible substitute"
        htmltools::tagAppendChildren(tile,if(is.null(issue)&&is.null(pending))
          live_sub_button(session$ns,event("starter",player=player$id),shiny::icon("right-left"),
            title=paste("Substitute",player$name),`aria-label`=paste("Substitute",player$name)) else shiny::tags$small(class="fm-live-sub-reason",issue))
      }
      i <- match(sel$out %||% "",vapply(o$lineup$players,function(p)mvp_text(p$id),character(1)))
      chooser <- if(!is.null(sel)&&!is.na(i)&&is.null(pending)){
        j <- match(sel$in_id %||% "",vapply(o$lineup$bench,function(p)mvp_text(p$id),character(1)))
        pair <- if(!is.na(j))live_sub_pair(o,i,j)
        shiny::div(class="fm-live-sub-picker",
          shiny::strong(paste("Replace",o$lineup$players[[i]]$name)),
          if(is.null(sel$pair))lapply(o$choices[[i]],function(k){
            p <- o$lineup$bench[[k]];button("bench",mvp_text(p$name),out=sel$out,player=p$id)
          }) else if(!is.null(pair)&&identical(sel$pair,pair))shiny::tagList(
            shiny::p(paste0("Round ",s$round$number," · ",o$lineup$players[[i]]$name," out, ",
              o$lineup$bench[[j]]$name," in. Locks ",fm_format_datetime(pair$cutoff),".")),
            button("confirm","Confirm substitution",nonce=sel$nonce))
          else shiny::p("Eligibility changed. Choose the pair again."),
          button("cancel","Cancel"))
      }
      shiny::tagList(if(isTRUE(as.numeric(Sys.time())>=data$valid_until))fm_info("Showing the last synced round lineup."),
        shiny::div(class="fm-lineup-fixtures-note",switch(feed$status,
          not_configured="Club XI checks require a StrikerAPI connection.",
          unsupported="StrikerAPI coverage is not configured for this competition.",
          calendar_pending="Waiting for the synced fixture calendar.",
          offline="Club XI checks require factual sync.",loading="Waiting for synced club team sheets.",
          "Club team sheets · Check a fixture within 90 minutes of kickoff.")),
        if(nrow(eligible_fixtures))shiny::div(class="fm-club-xi-checks",lapply(seq_len(nrow(eligible_fixtures)),function(i){
          f <- eligible_fixtures[i,,drop=FALSE]
          label <- paste0("Check club XI · ",f$home_name," vs ",f$away_name)
          if(f$available_at>as.numeric(Sys.time())){
            reason <- if(f$available_at>=as.numeric(fm_time(f$kickoff)))"No more checks before kickoff" else
              paste("Available",fm_format_datetime(f$available_at))
            return(shiny::tags$button(type="button",class="btn btn-default",disabled="disabled",paste(label,"·",reason)))
          }
          button("club_xi",label,fixture=f$match_id)
        })),
        if(identical(state$xi_note$key,s$key) &&
           !isTRUE(feed$matches[[state$xi_note$fixture]]$checked>=state$xi_note$at))
          shiny::div(role="status",state$xi_note$message),
        round_lineup_pitch_ui(data$answer,controls),chooser,
        if(identical(state$feedback$key,s$key))shiny::div(role="status",state$feedback$message),
        if(identical(pending$key,s$key)&&isTRUE(pending$uncertain))button("verify","Check official lineup"),
        if(identical(state$save_failed$key,s$key))button("persist","Retry saving the observed lineup"))
    })
    shiny::observeEvent(input$action,{
      e <- input$action;s <- scope()
      if(is.null(s)||!is.list(e)||!identical(e$key,s$key))return()
      state <- live_sub_round_state(root,s$key)
      if(identical(e$kind,"club_xi")){
        feed <- club_sheet()
        rows <- club_xi_checkable(feed,options()$lineup$players,fixtures(),holds=state$xi_holds)
        f <- rows[rows$match_id %in% e$fixture,,drop=FALSE]
        if(nrow(f)!=1L || f$available_at>as.numeric(Sys.time()) || isTRUE(state$xi_submitting))return()
        state$xi_submitting <- TRUE
        at <- as.numeric(Sys.time())
        update <- function(message)state$xi_note <- list(key=s$key,fixture=f$match_id,at=at,message=message)
        update("Requesting this club XI…")
        request <- tryCatch(club_xi_request(s,f,session$token),error=promises::promise_reject)
        promises::then(request,function(status){
          state$xi_submitting <- FALSE
          status <- mvp_text(status)
          until <- switch(status,limit_reached=Inf,requested=at+180,busy=at+180,0)
          if(startsWith(status,"retry_at:")){
            until <- fm_number(sub("^retry_at:","",status),0)
            status <- "retry_later"
          }
          holds <- shiny::isolate(state$xi_holds) %||% list()
          holds[[f$match_id]] <- list(kickoff=f$kickoff,until=until)
          state$xi_holds <- holds
          update(switch(status,limit_reached="Both checks for this fixture have been used.",
            retry_later=paste("The next check is available",fm_format_datetime(until)),
            requested="Club XI check requested.",
            busy="A check was requested recently. Allow a moment for the saved result.",
            outside_window="This fixture is outside the pre-kickoff window.","Club XI check unavailable."))
          NULL
        },function(error){
          state$xi_submitting <- FALSE
          update("Could not verify the request. Check the saved result before requesting again.")
          NULL
        })
        return()
      }
      if(identical(e$kind,"persist")&&identical(state$save_failed$key,s$key)){
        persist(s,state$save_failed$data);return()
      }
      pending <- state$pending
      if(!is.null(pending)){
        if(identical(e$kind,"verify")&&identical(pending$key,s$key)&&isTRUE(pending$uncertain)){
          state$pending <- list(key=s$key,pair=pending$pair)
          readback(s,login(),pending$pair)
        }
        return()
      }
      if(identical(e$kind,"cancel")){selected(NULL);return()}
      o <- options()
      ids <- vapply(o$lineup$players,function(p)mvp_text(p$id),character(1))
      if(identical(e$kind,"starter")){
        i <- match(e$player,ids)
        if(length(i)==1L&&!is.na(i)&&length(o$choices[[i]]))selected(list(key=s$key,out=e$player))
        return()
      }
      sel <- selected()
      if(!identical(sel$key,s$key))return()
      i <- match(sel$out,ids)
      if(is.na(i))return()
      bench_ids <- vapply(o$lineup$bench,function(p)mvp_text(p$id),character(1))
      if(identical(e$kind,"bench")&&identical(sel$out,e$out)){
        j <- match(e$player,bench_ids)
        if(length(j)!=1L||is.na(j))return()
        pair <- live_sub_pair(o,i,j)
        if(!is.null(pair))selected(list(key=s$key,out=sel$out,in_id=e$player,pair=pair,nonce=fm_clause_id()))
        return()
      }
      if(!identical(e$kind,"confirm")||is.null(sel$nonce)||!identical(e$nonce,sel$nonce))return()
      pair <- sel$pair
      selected(NULL)
      if(as.numeric(Sys.time())>=pair$cutoff){note(s$key,"A selected player has reached kickoff.");return()}
      auth <- login();owner <- paste0("live-sub:",session$token)
      state$pending <- list(key=s$key,pair=pair)
      note(s$key,"Checking both players with Futmondo…")
      current <- function()isTRUE(tryCatch(shiny::isolate(!session$isClosed()&&identical(scope(),s)&&
        identical(login(),auth)&&identical(state$pending$key,s$key)&&as.numeric(Sys.time())<pair$cutoff),error=function(e)FALSE))
      fails <- function(e){
        state$pending <- NULL
        note(s$key,if(inherits(e,"live_sub_issue"))conditionMessage(e) else
          paste("The live substitution check failed.",resource_failure_message(e)))
        NULL
      }
      promises::then(live_sub_preflight(auth,s$ctx,s$round,pair,owner,current,function(answer)publish(s,answer)),function(ok){
        if(!current())return(fails(live_sub_issue("The round or deadline changed before submission.")))
        sent <- FALSE
        guard <- function(){sent <<- current();sent}
        promises::then(live_sub_write(auth,s$ctx,s$round,pair,owner,guard),
          function(ok)readback(s,auth,pair,rejected=identical(ok,FALSE)),
          function(e){if(sent)readback(s,auth,pair) else fails(live_sub_issue("The substitution could not be dispatched."))})
        NULL
      },fails)
      invisible(NULL)
    })
  })
}

round_lineup_snapshot_key <- function(ctx,round_id)
  paste(ctx$account_id,ctx$championship_id,round_lineup_snapshot_id(ctx,round_id),sep="/")
