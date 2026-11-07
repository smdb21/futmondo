lineup_UI <- function(id) {
  ns<-shiny::NS(id)
  shiny::tagList(shiny::tags$style(shiny::HTML(".fm-lineup-controls{display:flex;flex-wrap:wrap;gap:12px;align-items:flex-end;margin-bottom:16px}.fm-lineup-controls .form-group{margin:0}.fm-lineup-controls .shiny-input-container{width:180px}.fm-lineup-editor{max-width:900px;margin:auto}.fm-lineup-editor .fm-lineup-player{background:transparent;border:0;color:var(--fm-text,#eef6f0);cursor:pointer;padding:6px;min-width:0}.fm-lineup-editor .fm-lineup-player:focus{outline:2px solid var(--fm-warning,#f0c75e)}.fm-lineup-editor .fm-lineup-portrait{margin:auto}.fm-lineup-editor .fm-lineup-name{white-space:normal}.fm-lineup-editor .fm-lineup-portrait img{max-width:48px;max-height:48px}.fm-lineup-note{padding:10px 0;color:var(--fm-muted,#acbdb0)}.fm-lineup-warning{color:var(--fm-warning,#f0c75e)}.fm-lineup-actions{display:flex;gap:8px;flex-wrap:wrap;margin:14px 0}.fm-lineup-coverage{display:flex;gap:12px;flex-wrap:wrap}.fm-lineup-editor .fm-lineup-row{display:grid;grid-template-columns:repeat(var(--lineup-count),minmax(0,1fr))}@media(max-width:600px){.fm-lineup-controls .shiny-input-container{width:140px}.fm-lineup-editor .fm-lineup-name{font-size:10px}.fm-lineup-editor .fm-lineup-player{padding:2px}.fm-lineup-editor .fm-lineup-portrait img{max-width:34px;max-height:34px}}")),
    shiny::div(class='fm-lineup-editor',
      shiny::h2('Lineup'),shiny::uiOutput(ns('heading')),
      shiny::div(class='fm-lineup-controls',
        shiny::uiOutput(ns('formation_control')),
        shiny::numericInput(ns('recent_n'),'Last completed rounds',value=5,min=1,max=20,step=1),
        shiny::checkboxInput(ns('include_unavailable'),'Include injured/suspended players',FALSE)),
      shiny::div(class='fm-lineup-actions',
        shiny::actionButton(ns('best'),'Best XI + bench',icon=shiny::icon('wand-magic-sparkles')),
        shiny::actionButton(ns('clear'),'Clear lineup',icon=shiny::icon('eraser')),
        shiny::actionButton(ns('reset'),'Undo changes',icon=shiny::icon('rotate-left'),
          title='Restore the latest lineup loaded from or successfully saved to Futmondo'),
        shiny::actionButton(ns('reload'),'Refresh lineup',icon=shiny::icon('rotate'))),
      shiny::uiOutput(ns('status')),shiny::uiOutput(ns('pitch')),shiny::uiOutput(ns('captain_control')),
      shiny::uiOutput(ns('save_control'))))
}
lineup_Server <- function(id,is_module_active,login_token,championship_id,user_team_id,round_catalog,
  refresh_trigger=NULL,history_refresh_trigger=NULL) {
  shiny::moduleServer(id,function(input,output,session){
    source<-shiny::reactiveVal(NULL);draft<-shiny::reactiveVal(NULL);loading<-shiny::reactiveVal(FALSE)
    saving<-shiny::reactiveVal(FALSE);notice<-shiny::reactiveVal('');cache_issue<-shiny::reactiveVal('')
    selected<-shiny::reactiveVal(NULL);best_note<-shiny::reactiveVal('');reload<-shiny::reactiveVal(0L)
    best_request<-shiny::reactiveVal(NULL);best_sequence<-0L;best_applied<-NULL
    owner<-paste0('lineup:',session$token);generation<-0L;closed<-FALSE;loaded_key<-NULL;requests<-list();request_sequence<-0L;retained<-list();retained_account<-NULL
    identity<-shiny::reactive({
      auth<-login_token();if(!valid_login(auth))return(NULL)
      list(account=fm_scalar(auth[['userid']]),championship=fm_scalar(championship_id()),team=fm_scalar(user_team_id()))
    })
    target<-shiny::reactive(lineup_target(round_catalog()))
    context<-function(){
      who<-shiny::isolate(identity());season<-league_season_context(who$championship);scoring<-'unknown'
      shared<-session$rootScope()$userData$shared_round_points
      entry<-if(!is.null(shared))tryCatch(shiny::isolate(shared$entry()),error=function(e)NULL)else NULL
      if(!is.null(entry)&&identical(entry$context$account_id,who$account)&&identical(entry$context$championship_id,who$championship)&&identical(entry$context$user_team_id,who$team)){
        if(!identical(entry$season,'unknown'))season<-entry$season
        scoring<-entry$scoring
      }
      saved<-shiny::isolate(source())
      if(!is.null(saved)){if(identical(season,'unknown')&&!identical(saved$season,'unknown'))season<-saved$season;if(identical(scoring,'unknown'))scoring<-saved$scoring_version}
      resource_context(shiny::isolate(login_token()),who$championship,who$team,season=season,scoring_version=scoring,session_id=session$token)
    }
    key_for<-function(who,next_round)paste(who$account,who$championship,who$team,next_round$id %||% '',sep='\034')
    current<-function(g,key)!closed&&identical(g,generation)&&identical(shiny::isolate(key_for(identity(),target())),key)
    persist<-function(data,ctx,round_id,g,key){
      promises::then(lineup_snapshot_store(ctx,round_id,data,owner),function(ok){
        if(current(g,key))cache_issue(if(isTRUE(ok))''else 'Database cache is not configured; this lineup is available in the current session.');NULL
      },function(error){if(current(g,key))cache_issue('Lineup loaded, but its database snapshot could not be saved.');NULL})
      invisible(NULL)
    }
    accept<-function(data,replace=FALSE){
      source(data)
      if(is.null(shiny::isolate(draft()))||replace){draft(data$official);shiny::updateSelectInput(session,'formation',selected=data$official$strategy)}
    }
    fetch<-function(force=FALSE){
      who<-shiny::isolate(identity());if(is.null(who)||!isTRUE(shiny::isolate(is_module_active())))return(invisible(NULL))
      next_round<-shiny::isolate(target());key<-key_for(who,next_round);ctx<-context();g<-generation;auth<-shiny::isolate(login_token())
      if(identical(requests[[key]]$generation,g))return(invisible(NULL))
      request_sequence<<-request_sequence+1L;token<-list(generation=g,sequence=request_sequence)
      loading(TRUE);requests[[key]]<<-token;notice('Loading lineup…')
      finish<-function(data,error=NULL,save=TRUE){
        if(identical(requests[[key]],token))requests[[key]]<<-NULL
        if(!current(g,key))return(NULL)
        loading(FALSE)
        if(!is.null(error)){notice(paste('Could not load current lineup.',resource_failure_message(error)));return(NULL)}
        accept(data);notice('');if(save)persist(data,ctx,next_round$id %||% '',g,key);NULL
      }
      live<-function(){
        if(!current(g,key)){if(identical(requests[[key]],token))requests[[key]]<<-NULL;return(invisible(NULL))}
        promise<-tryCatch(lineup_fetch_bundle(auth,ctx,next_round,owner),error=function(e)promises::promise_reject(e))
        promises::then(promise,function(data)finish(data),function(error)finish(NULL,error));invisible(NULL)
      }
      if(force){live();return(invisible(NULL))}
      promises::then(lineup_snapshot_store(ctx,next_round$id %||% '',owner=owner),function(saved){
        if(!current(g,key)){if(identical(requests[[key]],token))requests[[key]]<<-NULL;return(NULL)}
        if(!is.null(saved))accept(saved)
        if(lineup_snapshot_fresh(saved)){finish(saved,save=FALSE);return(NULL)}
        live();NULL
      },function(error){if(current(g,key))cache_issue('Saved lineup could not be read; loading from Futmondo.');live();NULL})
      invisible(NULL)
    }
    session$onSessionEnded(function(){closed<<-TRUE;generation<<-generation+1L;fm_transport_cancel(owner)})
    shiny::observe({
      who<-identity()
      if(is.null(who)){
        generation<<-generation+1L;loaded_key<<-NULL;retained<<-list();source(NULL);draft(NULL);selected(NULL);best_request(NULL);saving(FALSE);return()
      }
      if(!identical(retained_account,who$account)){retained<<-list();loaded_key<<-NULL;source(NULL);draft(NULL);retained_account<<-who$account}
      shiny::req(is_module_active());next_round<-target();key<-key_for(who,next_round)
      if(!identical(key,loaded_key)){
        if(!is.null(loaded_key))retained[[loaded_key]]<<-list(source=shiny::isolate(source()),draft=shiny::isolate(draft()))
        # Keep only this account's two most recently visited league-round drafts.
        retained<<-tail(Filter(function(x)!is.null(x$source),retained),2L)
        generation<<-generation+1L;loaded_key<<-key;saving(FALSE);loading(FALSE);selected(NULL);best_request(NULL);notice('');best_note('');cache_issue('')
        prior<-retained[[key]];source(prior$source);draft(prior$draft)
        if(!is.null(prior$draft))shiny::updateSelectInput(session,'formation',selected=prior$draft$strategy)
        fetch(FALSE)
      }
    },priority=100)
    shiny::observeEvent(list(reload(),if(is.function(refresh_trigger))refresh_trigger()else 0L),{
      if(isTRUE(is_module_active())&&!is.null(identity())&&!isTRUE(saving()))fetch(TRUE)
    },ignoreInit=TRUE)
    shiny::observeEvent(input$reload,{best_request(NULL);reload(shiny::isolate(reload())+1L)})
    roster<-shiny::reactive(lineup_roster_frame(source()))
    n_rounds<-shiny::reactive({n<-suppressWarnings(as.integer(input$recent_n));if(length(n)!=1L||is.na(n))5L else max(1L,min(20L,n))})
    catalog_issue<-shiny::reactive({
      catalog<-round_catalog()
      if(!is.data.frame(catalog)||!nrow(catalog))return('Round schedule is unavailable or still loading.')
      if(acquisition_data_unverified(catalog))return('Round schedule is unavailable from Futmondo.')
      if(identical(attr(catalog,'catalog_complete'),FALSE))return(fm_scalar(attr(catalog,'catalog_reason'),''))
      ''
    })
    scoring_catalog<-shiny::reactive({catalog<-round_catalog();if(nzchar(catalog_issue()))data.frame()else catalog})
    wanted<-shiny::reactive({catalog<-scoring_catalog();if(!is.data.frame(catalog)||!all(c('round_number','is_finished')%in%names(catalog)))return(numeric());tail(sort(unique(as.numeric(catalog$round_number[catalog$is_finished%in%TRUE]))),n_rounds())})
    history<-recent_player_points_source(session,login_token,championship_id,user_team_id,roster,
      is_active=function()isTRUE(is_module_active())&&!is.null(source())&&length(wanted())>0L,
      refresh_trigger=if(is.function(history_refresh_trigger))history_refresh_trigger else round_history_refresh_trigger(session),
      required_rounds=function()max(6L,n_rounds()),catalog_scope=FALSE,required_window=wanted)
    scores<-shiny::reactive(lineup_average_scores(roster(),history$history(),scoring_catalog(),n_rounds()))
    dirty<-shiny::reactive(!is.null(draft())&&!is.null(source())&&!identical(lineup_identity(draft()),lineup_identity(source()$official)))
    edit<-function(value){best_request(NULL);draft(value);best_note('')}
    shiny::observeEvent(input$clear,{shiny::req(draft(),!saving());edit(lineup_clear(draft()));notice('Lineup cleared. Select players or use Best XI + bench to start over.')})
    shiny::observeEvent(input$reset,{shiny::req(source(),!saving());edit(source()$official);shiny::updateSelectInput(session,'formation',selected=source()$official$strategy);notice('Changes undone. Restored the latest lineup loaded from or successfully saved to Futmondo.')})
    shiny::observeEvent(input$formation,{
      shiny::req(source(),draft(),!saving());strategy<-input$formation
      if(!strategy%in%unlist(source()$rules$allowed)){shiny::updateSelectInput(session,'formation',selected=draft()$strategy);notice('This formation is locked or unavailable.');return()}
      if(!identical(strategy,draft()$strategy))edit(lineup_reform(draft(),strategy,roster(),source()$rules$multiposition))
    },ignoreInit=TRUE)
    shiny::observeEvent(input$best,{
      if(saving()){notice('Wait for the lineup save to finish before calculating Best XI.');return()}
      shiny::req(identity(),is_module_active())
      best_sequence<<-best_sequence+1L
      best_request(list(generation=generation,key=loaded_key,sequence=best_sequence));best_note('')
      notice('Preparing Best XI + bench…')
    })
    # Retain an explicit request while its exact window is being acquired. The
    # same request follows N/availability changes until a manual edit or save.
    shiny::observe({
      request<-best_request();if(is.null(request))return()
      if(!current(request$generation,request$key)||saving()||!isTRUE(is_module_active()))return()
      data<-source()
      if(is.null(data)){best_applied<<-NULL;best_note('');notice('Best XI is waiting for the current squad and lineup to load.');return()}
      issue<-catalog_issue()
      if(nzchar(issue)){best_applied<<-NULL;best_note('');notice(paste('Best XI is waiting for the completed-round schedule.',issue));return()}
      score<-scores();n<-n_rounds();include<-isTRUE(input$include_unavailable);rows<-roster()
      if(!length(attr(score,'rounds'))){best_applied<<-NULL;best_note('');notice('No completed rounds are available for this calculation.');return()}
      eligible<-if(include)rep(TRUE,nrow(rows))else !analytics_known_unavailable(rows)
      missing<-sum(eligible&!is.finite(score$average[match(rows$id,score$id)]))
      if(missing){
        best_applied<<-NULL;best_note('');status<-history$status()
        active<-(status$pending %||% 0L)+(status$remaining %||% 0L)>0L
        notice(paste0('Best XI is waiting for complete scores for ',missing,' eligible player',if(missing!=1L)'s'else'',
          ' over rounds ',paste(attr(score,'rounds'),collapse=', '),'. ',
          if(active)'Loading missing history…'else if((status$failed %||% 0L)>0L)
            'Some history reads failed; refresh the data to retry.'else 'Missing history has not been confirmed yet.'))
        return()
      }
      signature<-list(request=request,rounds=n,include=include,roster=rows,scores=score,rules=data$rules)
      if(identical(signature,best_applied))return()
      result<-lineup_optimize(rows,score,unlist(data$rules$allowed),shiny::isolate(draft()),include,
        data$rules$bench_enabled,data$rules$multiposition)
      best_applied<<-signature
      if(is.null(result)){notice('No eligible XI can be formed from the current squad and allowed formations. Your draft is retained.');return()}
      value<-result$draft;if(!data$rules$captain_enabled)value$captain<-''
      draft(value);shiny::updateSelectInput(session,'formation',selected=value$strategy)
      best_note(paste0('Best XI + bench',
        ' · starters ',format(round(result$total,2),trim=TRUE),' average points · rounds ',paste(result$rounds,collapse=', '),
        ' · bench coverage ',if(length(result$coverage))paste(result$coverage,collapse=' / ')else'none'))
      notice('Review your selections, then Save to Futmondo.')
    })
    open_picker<-function(bench,index){
      shiny::req(source(),draft(),!saving());selected(list(bench=bench,index=index))
      rows<-roster();positions<-lineup_positions(rows,source()$rules$multiposition)
      eligible<-if(bench)rep(TRUE,nrow(rows))else vapply(positions,function(p)lineup_slot_roles(draft()$strategy)[index]%in%p,logical(1))
      rows<-rows[eligible,,drop=FALSE];averages<-scores()$average[match(rows$id,scores()$id)];positions<-positions[eligible]
      order_rows<-order(-averages,rows$name,rows$id,na.last=TRUE);rows<-rows[order_rows,,drop=FALSE];averages<-averages[order_rows];positions<-positions[order_rows]
      labels<-paste0(rows$name,' · ',vapply(positions,paste,character(1),collapse='/'),' · ',ifelse(is.finite(averages),format(round(averages,2),trim=TRUE),'scores loading'),
        ifelse(analytics_known_unavailable(rows),' · injured/suspended',''))
      old<-if(bench)draft()$bench[index]else draft()$starters[index]
      dialog<-shiny::modalDialog(title=if(bench)paste('Substitute',index)else paste('Select',lineup_slot_roles(draft()$strategy)[index]),
        shiny::selectizeInput(session$ns('player_choice'),'Player',choices=c('Empty position'='',setNames(as.character(rows$id),labels)),selected=old),
        footer=shiny::tagList(shiny::modalButton('Cancel'),shiny::actionButton(session$ns('assign'),'Select player')),easyClose=TRUE,fade=FALSE)
      # Only this frequent picker opens without Bootstrap's 300ms fade.
      shiny::showModal(dialog)
    }
    for(i in seq_len(11L))local({slot<-i;shiny::observeEvent(input[[paste0('starter_',slot)]],open_picker(FALSE,slot),ignoreInit=TRUE)})
    for(i in seq_len(4L))local({slot<-i;shiny::observeEvent(input[[paste0('bench_',slot)]],open_picker(TRUE,slot),ignoreInit=TRUE)})
    shiny::observeEvent(input$assign,{
      shiny::req(selected(),draft(),source(),!saving())
      tryCatch({pick<-selected();edit(lineup_assign(draft(),roster(),source()$rules,pick$bench,pick$index,input$player_choice %||% ''));shiny::removeModal();selected(NULL)},
        error=function(error)notice(conditionMessage(error)))
    })
    shiny::observeEvent(input$captain,{shiny::req(draft(),source(),!saving());captain<-input$captain
      if(captain%in%draft()$starters&&!identical(captain,draft()$captain)){value<-draft();value$captain<-captain;edit(value)}
    },ignoreInit=TRUE)
    output$formation_control<-shiny::renderUI({
      allowed<-if(is.null(source()))character()else unlist(source()$rules$allowed)
      strategy<-if(is.null(draft()))'4-4-2'else draft()$strategy
      shiny::div(class='form-group shiny-input-container',shiny::tags$label(`for`=session$ns('formation'),'Formation'),
        shiny::tags$select(id=session$ns('formation'),class='form-control',lapply(lineup_formations(),function(value)
          shiny::tags$option(value=value,selected=if(identical(value,strategy))'selected',
            disabled=if(!value%in%allowed)'disabled',paste0(value,if(!value%in%allowed)' (locked)')))))
    })
    output$heading<-shiny::renderUI({
      shiny::req(is_module_active());next_round<-target()
      shiny::div(class='fm-lineup-note',if(is.null(next_round))'Next-round schedule unavailable; saving is disabled.'else
        paste0('Next scheduled round ',next_round$number,' · saves the current editable Futmondo lineup'),
        if(dirty())shiny::strong(' · Unsaved changes'))
    })
    output$status<-shiny::renderUI({
      shiny::req(is_module_active());status<-history$status();score<-scores()
      shiny::tagList(shiny::div(role='status',`aria-live`='polite',notice()),
        if(nzchar(best_note()))shiny::div(class='fm-lineup-note',best_note()),
        if(nzchar(cache_issue()))shiny::div(class='fm-lineup-warning',cache_issue()),
        if(nzchar(catalog_issue()))shiny::div(class='fm-lineup-warning',catalog_issue())else
          if(nrow(score))shiny::div(class='fm-lineup-note',paste(sum(is.finite(score$average)),'of',nrow(score),'players have complete scores for',length(wanted()),'completed rounds.'),
          if((status$pending %||% 0L)+(status$remaining %||% 0L)>0L)' Loading missing history…',
          if((status$failed %||% 0L)>0L)' Some history reads failed; use Refresh to retry.',
          if((status$saving %||% 0L)>0L)' Saving acquired history…',if(!is.null(status$save_error))status$save_error))
    })
    tile<-function(id,role,index,bench=FALSE){
      rows<-roster();row<-rows[match(id,rows$id),,drop=FALSE];occupied<-nzchar(id)&&id%in%rows$id
      average<-scores()$average[match(id,scores()$id)]
      logo<-if(occupied)fm_scalar(row$logo,'')else''
      image<-if(occupied)get_team_image_name(row$club,logo=logo)else''
      crest_url<-if(grepl('^https://',logo,ignore.case=TRUE))logo else
        if(nzchar(image)&&grepl('^[A-Za-z0-9_.-]+$',image))paste0(TEAM_LOGO_URL,image,'.png')else''
      position<-if(!occupied)''else if(bench)fm_scalar(lineup_positions(row,FALSE)[[1]],'')else role
      shiny::actionButton(session$ns(paste0(if(bench)'bench_'else'starter_',index)),label=shiny::tagList(
        if(bench)shiny::span(class='fm-lineup-order',paste('Bench',index)),
        shiny::div(class='fm-lineup-points',if(length(average)&&is.finite(average))format(round(average,2),trim=TRUE)else'—'),
        shiny::div(class='fm-lineup-portrait',if(occupied&&nzchar(row$photo)&&grepl('^[A-Za-z0-9_.-]+$',row$photo))
          shiny::tags$img(src=paste0(PHOTO_URL,'/',row$photo),alt='',loading='lazy',onerror="this.style.display='none'")else shiny::span(class='fm-lineup-silhouette',if(occupied)'●'else'+'),
          if(nzchar(crest_url))shiny::tags$img(class='fm-lineup-crest',src=crest_url,alt=row$club,title=row$club,
            width=22,height=22,loading='lazy',decoding='async',onerror="this.style.display='none'")),
        shiny::div(class='fm-lineup-name',`data-position`=position,if(occupied)lineup_player_label(row$name)else'Empty position'),
        if(occupied&&analytics_known_unavailable(row))shiny::span(class='fm-lineup-warning','⚠')),
        class=paste('fm-lineup-player',if(!occupied)'fm-lineup-empty'),disabled=saving(),
        title=if(occupied)paste('Change',row$name)else paste('Select',role))
    }
    output$pitch<-shiny::renderUI({
      shiny::req(is_module_active(),draft(),source());value<-draft();roles<-lineup_slot_roles(value$strategy)
      shiny::div(class='fm-round-lineup',shiny::div(class='fm-lineup-heading',shiny::strong(paste('Formation',value$strategy))),
        shiny::div(class='fm-lineup-pitch',role='group',`aria-label`=paste('Pitch, formation',value$strategy),
          shiny::div(class='fm-lineup-markings',`aria-hidden`='true',shiny::div(class='fm-lineup-circle'),shiny::div(class='fm-lineup-goal-top'),shiny::div(class='fm-lineup-goal-bottom')),
          lapply(unique(roles),function(role){indices<-which(roles==role);shiny::div(class='fm-lineup-row',style=paste0('--lineup-count:',length(indices)),lapply(indices,function(i)tile(value$starters[i],role,i)))})),
        if(source()$rules$bench_enabled)shiny::div(class='fm-lineup-bench',shiny::h4('Substitutes · best to worst'),
          shiny::div(class='fm-lineup-bench-row',lapply(seq_len(4L),function(i)tile(value$bench[i],'Substitute',i,TRUE))),
          shiny::div(class='fm-lineup-coverage',lapply(c('DEF','MID','FWD'),function(role){
            covered<-lineup_bench_coverage(roster(),value$bench,source()$rules$multiposition)
            shiny::span(class=if(!role%in%covered)'fm-lineup-warning',paste(role,if(role%in%covered)'✓'else'—'))}))))
    })
    output$captain_control<-shiny::renderUI({
      shiny::req(is_module_active(),draft(),source());if(!source()$rules$captain_enabled)return(NULL)
      ids<-draft()$starters[nzchar(draft()$starters)];rows<-roster()
      shiny::selectInput(session$ns('captain'),'Captain',choices=c('Select captain'='',setNames(ids,rows$name[match(ids,rows$id)])),selected=draft()$captain)
    })
    output$save_control<-shiny::renderUI({
      shiny::req(is_module_active());issues<-if(is.null(draft())||is.null(source()))'Lineup is loading'else lineup_validate(draft(),roster(),source()$rules)
      if(is.null(target()))issues<-c(issues,'Next round is not established')
      shiny::tagList(shiny::actionButton(session$ns('save'),if(saving())'Saving…'else'Save to Futmondo',icon=shiny::icon('floppy-disk'),
        disabled=saving()||loading()||length(issues)>0L||!dirty()),if(length(issues))shiny::div(class='fm-lineup-note',paste(unique(issues),collapse=' · ')))
    })
    shiny::observeEvent(input$save,{
      shiny::req(source(),draft(),target(),!saving(),!loading(),dirty());value<-draft();baseline<-source();ctx<-context();next_round<-target();key<-loaded_key;g<-generation;auth<-login_token()
      issues<-lineup_validate(value,roster(),baseline$rules);if(length(issues)){notice(paste(issues,collapse=' · '));return()}
      best_request(NULL);saving(TRUE);notice('Checking the official lineup and current squad…')
      fail<-function(text){if(current(g,key)){saving(FALSE);notice(text)};NULL}
      verify<-function(){
        if(!current(g,key))return(NULL)
        notice('Verifying saved lineup…')
        promises::then(lineup_fetch_bundle(auth,ctx,next_round,owner),function(result)tryCatch({
          if(!current(g,key))return(NULL)
          saving(FALSE);source(result);persist(result,ctx,next_round$id,g,key)
          if(identical(lineup_identity(result$official),lineup_identity(value))){draft(result$official);notice('Saved to Futmondo and verified.');best_note('')}
          else notice('Futmondo did not confirm the requested lineup. Your draft is retained; no automatic retry was sent.')
          NULL
        },error=function(error)fail('Saved lineup could not be validated. Your draft is retained.')),function(error)fail('The save response is uncertain and readback failed. Your draft is retained. Check Futmondo before saving again.'))
        NULL
      }
      promises::then(lineup_fetch_bundle(auth,ctx,next_round,owner),function(fresh)tryCatch({
        if(!current(g,key))return(NULL)
        if(!identical(lineup_identity(fresh$official),lineup_identity(baseline$official))){source(fresh);return(fail('The official lineup changed in another session. Your draft is retained. Undo changes or review it before saving again.'))}
        if(is.null(shiny::isolate(target()))||!identical(shiny::isolate(target())$id,next_round$id)||next_round$starts_at<=Sys.time())return(fail('The round context changed before saving. Refresh the lineup.'))
        if(!identical(fresh$season,baseline$season)||!identical(fresh$scoring_version,baseline$scoring_version))return(fail('The league season or scoring rules changed. Refresh before saving.'))
        rows<-lineup_roster_frame(fresh);issues<-lineup_validate(value,rows,fresh$rules)
        if(length(issues))return(fail(paste(issues,collapse=' · ')))
        locked<-lineup_locked_changes(fresh,value)
        if(length(locked))return(fail(paste('Futmondo has locked selected positions:',paste(locked,collapse=', '))))
        wire<-lineup_modified_wire(value,rows,fresh$rules);changes<-lineup_changes(fresh$source$wire,wire)
        notice('Saving lineup to Futmondo…')
        promises::then(lineup_submit(auth,ctx$championship_id,ctx$user_team_id,fresh$source$wire,wire,owner,changes),function(result)verify(),function(error)verify())
        NULL
      },error=function(error)fail(paste('Lineup validation failed.',conditionMessage(error)))),function(error)fail(paste('Current lineup could not be verified.',resource_failure_message(error))))
      invisible(NULL)
    })
    list(draft=shiny::reactive(draft()),official=shiny::reactive(if(is.null(source()))NULL else source()$official),
      source=shiny::reactive(source()),saving=shiny::reactive(saving()),scores=scores)
  })
}
