lineup_status_icon <- function(status) {
  code<-tolower(trimws(fm_scalar(status,'')))
  if(code%in%c('','ok'))return(NULL)
  info<-switch(code,
    doubt=list(label='Doubtful',icon='triangle-exclamation',tone='warning'),
    doubtful=list(label='Doubtful',icon='triangle-exclamation',tone='warning'),
    injured=list(label='Injured',icon='circle-minus',tone='danger'),
    injured2=list(label='Long-term injury',icon='hospital',tone='danger'),
    redcard=list(label='Suspended',icon='square',tone='danger'),
    suspended=list(label='Suspended',icon='square',tone='danger'),
    unavailable=list(label='Unavailable',icon='ban',tone='muted'),
    list(label='Status unconfirmed',icon='circle-question',tone='muted'))
  shiny::span(class=paste('fm-lineup-status',paste0('fm-lineup-status-',info$tone)),
    `data-status`=code,title=info$label,role='img',`aria-label`=info$label,shiny::icon(info$icon))
}
lineup_UI <- function(id) {
  ns<-shiny::NS(id)
  shiny::tagList(shiny::tags$style(shiny::HTML(".fm-lineup-controls{display:flex;flex-wrap:wrap;gap:12px;align-items:flex-end;margin-bottom:16px}.fm-lineup-controls .form-group{margin:0}.fm-lineup-optimize-controls{display:flex;gap:12px;align-items:flex-end;flex:0 0 100%;min-width:0}.fm-lineup-optimize-controls .shiny-input-container{flex:1;min-width:0;max-width:180px}.fm-lineup-optimize-controls .btn{flex-shrink:0}.fm-lineup-formation .shiny-input-container{width:180px;max-width:100%;margin-bottom:8px}.fm-lineup-availability{flex:0 0 100%;min-width:0}.fm-lineup-controls .shiny-input-container{width:180px}.fm-lineup-editor{max-width:900px;margin:auto}.fm-lineup-editor .fm-lineup-player{background:transparent;border:0;color:var(--fm-text,#eef6f0);cursor:pointer;padding:6px;min-width:0}.fm-lineup-editor .fm-lineup-player:focus{outline:2px solid var(--fm-warning,#f0c75e)}.fm-lineup-editor .fm-lineup-portrait{margin:auto}.fm-lineup-editor .fm-lineup-name{white-space:normal}.fm-lineup-editor .fm-lineup-portrait img{max-width:48px;max-height:48px}.fm-lineup-note{padding:10px 0;color:var(--fm-muted,#acbdb0)}.fm-lineup-warning{color:var(--fm-warning,#f0c75e)}.fm-lineup-actions{display:flex;gap:8px;flex-wrap:wrap;margin:14px 0}.fm-lineup-coverage{display:flex;gap:12px;flex-wrap:wrap}.fm-lineup-editor .fm-lineup-row{display:grid;grid-template-columns:repeat(var(--lineup-count),minmax(0,1fr))}@media(max-width:600px){.fm-lineup-controls .shiny-input-container{width:140px}.fm-lineup-editor .fm-lineup-name{font-size:10px}.fm-lineup-editor .fm-lineup-player{padding:2px}.fm-lineup-editor .fm-lineup-portrait img{max-width:34px;max-height:34px}}")),
    shiny::tags$style(shiny::HTML("
      .fm-lineup-picker-list{max-height:60vh;overflow-y:auto}
      .fm-lineup-choice,.fm-lineup-picker-heading{display:grid;grid-template-columns:26px minmax(0,1fr) 58px 78px;gap:8px;align-items:center;padding:10px 8px}
      .fm-lineup-picker-heading{font-size:12px;color:var(--fm-muted,#647067)}
      .fm-lineup-picker-heading span:nth-last-child(-n+2){text-align:right}
      .fm-lineup-choice{width:100%;border:0;border-bottom:1px solid var(--fm-border,#d8e2da);border-radius:0;background:transparent;color:var(--fm-text,#24382a);text-align:left;cursor:pointer}
      .fm-lineup-choice:hover,.fm-lineup-choice[aria-pressed=true]{background:var(--fm-surface-hover,rgba(128,160,140,.15))}
      .fm-lineup-choice:focus-visible{outline:2px solid var(--fm-warning,#bd8d1e);outline-offset:-2px}
      .fm-lineup-choice-name{min-width:0;overflow-wrap:anywhere}
      .fm-lineup-choice-name small{display:block;color:var(--fm-muted,#647067)}
      .fm-lineup-choice-label{display:flex;align-items:center;gap:6px;flex-wrap:wrap}
      .fm-lineup-choice-membership{display:inline-flex;flex-shrink:0}
      .fm-lineup-status{display:inline-flex;flex:0 0 auto;align-items:center;justify-content:center;font-size:14px;line-height:1}
      .fm-lineup-status-warning{color:var(--fm-warning,#b38012)}
      .fm-lineup-status-danger{color:var(--fm-danger,#c63c4a)}
      .fm-lineup-status-muted{color:var(--fm-muted,#647067)}
      .fm-lineup-editor .fm-lineup-portrait>.fm-lineup-status{position:absolute;right:0;bottom:2px;width:18px;height:18px;border-radius:50%;background:var(--fm-surface,#fff)}
      .fm-lineup-choice-total,.fm-lineup-choice-average{text-align:right;font-variant-numeric:tabular-nums}
      .fm-lineup-choice-crest{width:24px;height:24px;object-fit:contain}
      .fm-lineup-fixtures{margin-top:24px;padding-top:16px;border-top:1px solid var(--fm-border,#d8e2da)}
      .fm-lineup-fixtures h3{margin-top:0;font-size:20px}
      .fm-lineup-fixtures-note{font-size:12px;color:var(--fm-muted,#647067);overflow-wrap:anywhere}
      .fm-lineup-fixtures-list{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:10px;list-style:none;padding:0}
      .fm-lineup-fixture{min-width:0;padding:12px;border:1px solid var(--fm-border,#d8e2da);border-radius:8px}
      .fm-lineup-fixture-time{display:block;font-size:12px;color:var(--fm-muted,#647067);margin-bottom:6px}
      .fm-lineup-fixture-teams{display:grid;grid-template-columns:minmax(0,1fr) auto minmax(0,1fr);gap:10px;align-items:center}
      .fm-lineup-fixture-team{display:flex;align-items:center;gap:6px;min-width:0;font-weight:600}
      .fm-lineup-fixture-team-name{min-width:0;overflow-wrap:anywhere}
      .fm-lineup-fixture-crest{width:24px;height:24px;flex:0 0 24px;object-fit:contain}
      .fm-lineup-fixture-away{justify-content:flex-end;text-align:right}
      .fm-lineup-fixture-away>.fm-lineup-fixture-crest{order:1}
      .fm-lineup-fixture-vs{font-size:12px;color:var(--fm-muted,#647067)}
      @media(max-width:600px){.fm-lineup-fixtures-list{grid-template-columns:minmax(0,1fr)}}
    ")),
    shiny::div(class='fm-lineup-editor',
      shiny::h2('Lineup'),shiny::uiOutput(ns('heading')),
      shiny::div(class='fm-lineup-controls',
        shiny::div(class='fm-lineup-optimize-controls',
          shiny::actionButton(ns('best'),'Optimize',icon=shiny::icon('wand-magic-sparkles')),
          shiny::numericInput(ns('recent_n'),'Last N rounds',value=3,min=1,max=20,step=1)),
        shiny::div(class='fm-lineup-availability',
          shiny::checkboxInput(ns('include_doubtful'),shiny::span(
            title='Allow doubtful players when optimizing. Injured, suspended and unavailable players are always excluded from the starting XI and bench.',
            'Include doubtful players'),FALSE,width='100%'),
          shiny::checkboxInput(ns('require_bench_coverage'),shiny::span(
            title='When enabled, Optimize requires separate substitutes covering defender, midfielder and forward. Goalkeeper cover is not required. The starting XI or formation may change to leave this cover on the bench. If no eligible lineup can provide it, your draft is kept.',
            'Require full outfield bench coverage'),FALSE,width='100%'))),
      shiny::div(class='fm-lineup-actions',
        shiny::actionButton(ns('clear'),'Clear lineup',icon=shiny::icon('eraser')),
        shiny::actionButton(ns('reset'),'Undo changes',icon=shiny::icon('rotate-left'),
          title='Restore the latest lineup loaded from or successfully saved to LigaRadar'),
        shiny::actionButton(ns('reload'),label=NULL,icon=shiny::icon('rotate'),`aria-label`='Refresh lineup',
          title='Reload the official lineup, squad and rules from LigaRadar; keep your unsaved changes')),
      shiny::uiOutput(ns('status')),
      shiny::div(class='fm-lineup-actions',shiny::uiOutput(ns('save_control'))),
      shiny::div(class='fm-lineup-formation',shiny::uiOutput(ns('formation_control'))),
      shiny::uiOutput(ns('pitch')),shiny::uiOutput(ns('captain_control')),
      shiny::uiOutput(ns('fixtures'))))
}
lineup_Server <- function(id,is_module_active,login_token,championship_id,user_team_id,round_catalog,
  refresh_trigger=NULL,history_refresh_trigger=NULL,clause_transfers_source=NULL,clause_transfers_refresh=NULL,round_catalog_state=NULL) {
  shiny::moduleServer(id,function(input,output,session){
    source<-shiny::reactiveVal(NULL);draft<-shiny::reactiveVal(NULL);loading<-shiny::reactiveVal(FALSE)
    saving<-shiny::reactiveVal(FALSE);notice_value<-shiny::reactiveVal('');notice_error<-shiny::reactiveVal(FALSE);cache_issue<-shiny::reactiveVal('')
    notice<-function(value,error=FALSE){
      if(missing(value))return(notice_value())
      notice_error(isTRUE(error));notice_value(value)
    }
    selected<-shiny::reactiveVal(NULL);picker_notice<-shiny::reactiveVal('');picker_sequence<-0L;best_note<-shiny::reactiveVal('');reload<-shiny::reactiveVal(0L)
    best_request<-shiny::reactiveVal(NULL);best_sequence<-0L;best_scheduled<-NULL;best_basis<-shiny::reactiveVal(NULL)
    owner<-paste0('lineup:',session$token);generation<-0L;closed<-FALSE;loaded_key<-NULL;requests<-list();request_sequence<-0L;retained<-list();retained_account<-NULL
    identity<-shiny::reactive({
      auth<-login_token();if(!valid_login(auth))return(NULL)
      if(exists('league_context_revision',mode='function'))league_context_revision()
      ctx<-resource_context(auth,championship_id(),user_team_id())
      list(account=ctx$account_id,championship=ctx$championship_id,team=ctx$user_team_id,
        competition=ctx$competition_id,season=ctx$season,scoring=ctx$scoring_version)
    })
    target<-shiny::reactive(lineup_target(round_catalog()))
    fixture_clubs_read<-factual_read_adapter(session,'get_real_clubs',login_token,championship_id,user_team_id,
      is_active=is_module_active,refresh_trigger=refresh_trigger,state_only=TRUE)
    local_transfer_refresh<-shiny::reactive(list(shared=if(is.function(refresh_trigger))refresh_trigger()else 0L,local=reload()))
    transfers<-if(is.function(clause_transfers_source))clause_transfers_source else
      incoming_clause_source(session,login_token,championship_id,user_team_id,is_module_active,local_transfer_refresh)
    current_departures<-function()clause_departure_round_state(transfers(),target())
    departures<-resource_display_source(session,current_departures)
    departure_issues<-function(value,state=departures()){
      issues<-character()
      if(!isTRUE(state$complete))issues<-c(issues,paste('Departure eligibility is not verified.',
        state$error %||% 'Waiting for the current clause departure details.'))
      if(any(c(value$starters,value$bench)%in%state$excluded_ids))issues<-c(issues,
        'A selected player leaves by the next round start; replace them before saving')
      issues
    }
    context<-function(){
      who<-shiny::isolate(identity());season<-who$season;scoring<-who$scoring
      shared<-session$rootScope()$userData$shared_round_points
      entry<-if(!is.null(shared))tryCatch(shiny::isolate(shared$entry()),error=function(e)NULL)else NULL
      if(!is.null(entry)&&identical(entry$context$account_id,who$account)&&identical(entry$context$championship_id,who$championship)&&identical(entry$context$user_team_id,who$team)&&
         identical(fm_scalar(entry$context$competition_id,'unknown'),who$competition)&&identical(entry$season,who$season)){
        if(!identical(entry$season,'unknown'))season<-entry$season
        # Historical rows may still use an earlier scoring configuration. They
        # can fill unknown scope, but cannot replace independently known current
        # identity used by the lineup source and durable snapshot key.
        if(!fm_context_known(scoring))scoring<-entry$scoring
      }
      resource_context(shiny::isolate(login_token()),who$championship,who$team,season=season,scoring_version=scoring,session_id=session$token,competition_id=who$competition)
    }
    key_for<-function(who,next_round)paste(who$account,who$championship,who$team,who$competition,who$season,who$scoring,next_round$id %||% '',sep='\034')
    current<-function(g,key)!closed&&identical(g,generation)&&identical(shiny::isolate(key_for(identity(),target())),key)
    persist<-function(data,ctx,round_id,g,key){
      promises::then(lineup_snapshot_store(ctx,round_id,data,owner),function(ok){
        if(current(g,key))cache_issue(if(isTRUE(ok))''else 'Database cache is not configured; this lineup is available in the current session.');NULL
      },function(error){if(current(g,key))cache_issue(lineup_snapshot_failure(error));NULL})
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
        if(!is.null(error)){notice(paste('Could not load current lineup.',resource_failure_message(error)),error=TRUE);return(NULL)}
        accept(data);notice('');if(save)persist(data,ctx,next_round$id %||% '',g,key);NULL
      }
      live<-function(){
        if(!current(g,key)){if(identical(requests[[key]],token))requests[[key]]<<-NULL;return(invisible(NULL))}
        promise<-tryCatch(lineup_fetch_bundle(auth,ctx,next_round,owner,is_current=function()current(g,key)),error=function(e)promises::promise_reject(e))
        promises::then(promise,function(data)finish(data),function(error)finish(NULL,error));invisible(NULL)
      }
      if(force){live();return(invisible(NULL))}
      promises::then(lineup_snapshot_store(ctx,next_round$id %||% '',owner=owner),function(saved){
        if(!current(g,key)){if(identical(requests[[key]],token))requests[[key]]<<-NULL;return(NULL)}
        if(!is.null(saved))accept(saved)
        if(lineup_snapshot_fresh(saved)){finish(saved,save=FALSE);return(NULL)}
        live();NULL
      },function(error){if(current(g,key))cache_issue('Saved lineup could not be read; loading from LigaRadar.');live();NULL})
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
        if(!is.null(loaded_key))retained[[loaded_key]]<<-list(source=shiny::isolate(source()),draft=shiny::isolate(draft()),best_basis=shiny::isolate(best_basis()))
        # Keep only this account's two most recently visited league-round drafts.
        retained<<-tail(Filter(function(x)!is.null(x$source),retained),2L)
        generation<<-generation+1L;loaded_key<<-key;saving(FALSE);loading(FALSE);selected(NULL);best_request(NULL);notice('');best_note('');cache_issue('')
        prior<-retained[[key]];source(prior$source);draft(prior$draft);best_basis(prior$best_basis)
        if(!is.null(prior$draft))shiny::updateSelectInput(session,'formation',selected=prior$draft$strategy)
        fetch(FALSE)
      }
    },priority=100)
    shiny::observeEvent(list(reload(),if(is.function(refresh_trigger))refresh_trigger()else 0L),{
      if(isTRUE(is_module_active())&&!is.null(identity())&&!isTRUE(saving()))fetch(TRUE)
    },ignoreInit=TRUE)
    shiny::observeEvent(input$reload,{
      shiny::req(identity());best_request(NULL)
      if(is.function(clause_transfers_refresh))clause_transfers_refresh()else
        if(!is.function(clause_transfers_source))invalidate_saved_clause_transfers(context())
      reload(shiny::isolate(reload())+1L)
    })
    roster<-shiny::reactive(lineup_roster_frame(source()))
    n_rounds<-shiny::reactive({n<-suppressWarnings(as.integer(input$recent_n));if(length(n)!=1L||is.na(n))3L else max(1L,min(20L,n))})
    catalog_issue<-shiny::reactive({
      state<-if(is.function(round_catalog_state))round_catalog_state()else NULL
      expected<-if(!is.null(state))fm_worker_resolve_context('get_finished_rounds',
        list(championship_id=championship_id()),resource_context(login_token(),championship_id(),user_team_id()))else NULL
      lineup_catalog_problem(round_catalog(),state,expected)
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
    shiny::observeEvent(input$clear,{shiny::req(draft(),!saving());edit(lineup_clear(draft()));notice('Lineup cleared. Select players or use Optimize to start over.')})
    shiny::observeEvent(input$reset,{shiny::req(source(),!saving());edit(source()$official);shiny::updateSelectInput(session,'formation',selected=source()$official$strategy);notice('Changes undone. Restored the latest lineup loaded from or successfully saved to LigaRadar.')})
    shiny::observeEvent(input$formation,{
      shiny::req(source(),draft(),!saving());strategy<-input$formation
      if(!strategy%in%unlist(source()$rules$allowed)){shiny::updateSelectInput(session,'formation',selected=draft()$strategy);notice('This formation is locked or unavailable.',error=TRUE);return()}
      if(!identical(strategy,draft()$strategy))edit(lineup_reform(draft(),strategy,roster(),source()$rules$multiposition))
    },ignoreInit=TRUE)
    shiny::observeEvent(input$best,{
      if(saving()){notice('Wait for the lineup save to finish before calculating Best XI.');return()}
      shiny::req(identity(),is_module_active())
      best_sequence<<-best_sequence+1L
      best_request(list(generation=generation,key=loaded_key,sequence=best_sequence,n=n_rounds(),include=isTRUE(input$include_doubtful),coverage=isTRUE(input$require_bench_coverage)));best_note('')
      notice('Preparing Best XI + bench…')
    })
    shiny::observe({
      busy<-!is.null(best_request())
      shiny::updateActionButton(session,'best',label=if(busy)'Calculating…'else'Optimize',
        icon=shiny::icon(if(busy)'spinner'else'wand-magic-sparkles',class=if(busy)'fa-spin'))
    })
    # One click waits for complete history, then calculates once. Changing the
    # settings cancels a pending request and never replaces the displayed XI.
    shiny::observe({
      request<-best_request();if(is.null(request))return()
      if(!current(request$generation,request$key)||saving()||!isTRUE(is_module_active()))return()
      n<-n_rounds();include<-isTRUE(input$include_doubtful);coverage<-isTRUE(input$require_bench_coverage)
      if(!identical(request$n,n)||!identical(request$include,include)||!identical(request$coverage,coverage)){
        best_request(NULL);notice('Best XI calculation cancelled because the settings changed. Press Optimize to calculate again.');return()
      }
      data<-source()
      if(is.null(data)){notice('Best XI is waiting for the current squad and lineup to load.');return()}
      if(coverage&&!isTRUE(data$rules$bench_enabled)){
        best_request(NULL);notice('Full outfield bench coverage is unavailable because substitutes are disabled in this league. Uncheck the coverage option to optimize. Your draft is retained.',error=TRUE);return()
      }
      issue<-catalog_issue()
      if(nzchar(issue)){notice(paste('Best XI is waiting for the completed-round schedule.',issue),error=TRUE);return()}
      departure<-departures()
      if(!isTRUE(departure$complete)){
        notice(paste('Best XI is waiting for verified clause departure details.',departure$error %||% ''),error=!is.null(departure$error));return()
      }
      score<-scores();rows<-roster()
      if(!length(attr(score,'rounds'))){best_request(NULL);notice('No completed rounds are available for this calculation.');return()}
      eligible<-!rows$id%in%departure$excluded_ids&lineup_status_eligible(rows,include)
      missing<-sum(eligible&!is.finite(score$average[match(rows$id,score$id)]))
      if(missing){
        status<-history$status()
        active<-(status$pending %||% 0L)+(status$remaining %||% 0L)>0L
        notice(paste0('Best XI is waiting for complete scores for ',missing,' eligible player',if(missing!=1L)'s'else'',
          ' over rounds ',paste(attr(score,'rounds'),collapse=', '),'. ',
          if(active)'Loading missing history…'else if((status$failed %||% 0L)>0L)
            'Some history reads failed; refresh the data to retry.'else 'Missing history has not been confirmed yet.'),error=(status$failed %||% 0L)>0L)
        return()
      }
      if(identical(best_scheduled,request))return()
      best_scheduled<<-request;notice('Calculating Best XI + bench…')
      # Flush the busy state before doing the synchronous optimizer work.
      session$onFlushed(function(){later::later(function(){shiny::isolate({
        if(identical(best_scheduled,request))best_scheduled<<-NULL
        if(!current(request$generation,request$key)||!identical(best_request(),request)||saving()||
          !isTRUE(is_module_active())||!identical(n_rounds(),n)||!identical(isTRUE(input$include_doubtful),include)||
          !identical(isTRUE(input$require_bench_coverage),coverage))return()
        latest_departure<-current_departures()
        if(!isTRUE(latest_departure$complete)||!identical(latest_departure$excluded_ids,departure$excluded_ids)){
          best_request(NULL);notice('Departure eligibility changed before calculation. Press Optimize to calculate again.');return()
        }
        best_request(NULL)
        tryCatch({
          result<-lineup_optimize(rows,score,unlist(data$rules$allowed),draft(),include,
            data$rules$bench_enabled,data$rules$multiposition,excluded_ids=departure$excluded_ids,require_bench_coverage=coverage)
          if(is.null(result)){notice(if(coverage)
            'No eligible XI can leave separate defender, midfielder and forward cover on the bench with the allowed formations. Review player availability or uncheck the coverage option. Your draft is retained.'else
            'No eligible XI can be formed from the current squad and allowed formations. Your draft is retained.',error=TRUE);return()}
          value<-result$draft;if(!data$rules$captain_enabled)value$captain<-''
          draft(value);shiny::updateSelectInput(session,'formation',selected=value$strategy)
          best_basis(list(n=n,include=include,coverage=coverage,rounds=result$rounds,excluded_ids=departure$excluded_ids,lineup=lineup_identity(value)))
          best_note(paste0('Best XI + bench',
            ' · starters ',format(round(result$total,2),trim=TRUE),' average points · rounds ',paste(result$rounds,collapse=', '),
            ' · bench coverage ',if(length(result$coverage))paste(result$coverage,collapse=' / ')else'none'))
          notice('Review your selections, then Save to LigaRadar.')
        },error=function(error)notice(paste('Best XI calculation failed. Your draft is retained.',conditionMessage(error)),error=TRUE))
      })},delay=0)},once=TRUE)
    })
    crest_url<-function(row){
      logo<-fm_scalar(row$logo,'');image<-get_team_image_name(row$club,logo=logo)
      if(grepl('^https://',logo,ignore.case=TRUE))logo else
        if(nzchar(image)&&grepl('^[A-Za-z0-9_.-]+$',image))paste0(TEAM_LOGO_URL,image,'.png')else''
    }
    picker_click<-function(id,pick)paste0('Shiny.setInputValue(',
      jsonlite::toJSON(session$ns('player_choice'),auto_unbox=TRUE),',',
      jsonlite::toJSON(list(id=id,picker=pick$sequence),auto_unbox=TRUE),",{priority:'event'});")
    open_picker<-function(bench,index){
      shiny::req(source(),draft(),!saving())
      picker_sequence<<-picker_sequence+1L;picker_notice('')
      pick<-list(bench=bench,index=index,generation=generation,key=loaded_key,
        strategy=draft()$strategy,sequence=picker_sequence)
      selected(pick);old<-if(bench)draft()$bench[index]else draft()$starters[index]
      dialog<-shiny::modalDialog(title=if(bench)paste('Substitute',index)else paste('Select',lineup_slot_roles(draft()$strategy)[index]),
        shiny::uiOutput(session$ns('picker_error')),shiny::uiOutput(session$ns('picker_players')),
        footer=shiny::tagList(
          if(nzchar(old))shiny::tags$button(type='button',class='btn btn-warning fm-lineup-remove',
            onclick=picker_click('',pick),'Remove player'),shiny::modalButton('Cancel')),
        easyClose=TRUE,fade=FALSE)
      shiny::showModal(dialog)
    }
    output$picker_error<-shiny::renderUI({
      if(nzchar(picker_notice()))fm_info(picker_notice(),error=TRUE,inline=TRUE)
    })
    output$picker_players<-shiny::renderUI({
      pick<-selected();shiny::req(pick,source(),draft())
      shiny::req(current(pick$generation,pick$key),identical(pick$strategy,draft()$strategy))
      rows<-roster();positions<-lineup_positions(rows,source()$rules$multiposition);leaving<-departures()$excluded_ids
      eligible<-if(pick$bench)rep(TRUE,nrow(rows))else vapply(positions,function(p)lineup_slot_roles(draft()$strategy)[pick$index]%in%p,logical(1))
      rows<-rows[eligible,,drop=FALSE];score<-scores();averages<-score$average[match(rows$id,score$id)];positions<-positions[eligible]
      value<-draft();old<-if(pick$bench)value$bench[pick$index]else value$starters[pick$index]
      membership_order<-ifelse(rows$id%in%value$bench,1L,ifelse(rows$id%in%value$starters,3L,2L))
      order_rows<-order(membership_order,-averages,rows$name,rows$id,na.last=TRUE)
      rows<-rows[order_rows,,drop=FALSE];averages<-averages[order_rows];positions<-positions[order_rows]
      number<-function(value)if(length(value)==1L&&is.finite(value))format(round(value,2),trim=TRUE)else'—'
      marker<-function(icon,label)shiny::span(class='fm-lineup-choice-membership',
        title=label,role='img',`aria-label`=label,shiny::icon(icon))
      shiny::tagList(
        fm_info(paste0('Select a player to switch. Average uses the last ',length(attr(score,'rounds')),
          ' completed rounds (requested ',n_rounds(),').')),
        shiny::div(class='fm-lineup-note fm-lineup-picker-membership-note',
          marker('circle-check','Starting XI'),' XI · ',marker('chair','Bench'),' Bench · ',
          marker('crosshairs','Current slot'),' Current · including unsaved changes'),
        shiny::div(class='fm-lineup-picker-heading',`aria-hidden`='true',shiny::span(),shiny::span('Player'),shiny::span('Total pts'),shiny::span(paste0('Avg (',n_rounds(),')'))),
        shiny::div(class='fm-lineup-picker-list',role='group',`aria-label`='Eligible players',
          if(!nrow(rows))shiny::div(class='fm-lineup-note','No squad players can play in this position.'),
          lapply(seq_len(nrow(rows)),function(i){
            row<-rows[i,,drop=FALSE];url<-crest_url(row);unavailable<-analytics_known_unavailable(row)
            starter_slot<-match(row$id,value$starters);bench_slot<-match(row$id,value$bench)
            membership<-if(!is.na(starter_slot))'starter'else if(!is.na(bench_slot))'bench'else'none'
            membership_label<-if(membership=='starter')'Starting XI'else if(membership=='bench')paste('Bench',bench_slot)else'Not selected'
            shiny::tags$button(type='button',class='fm-lineup-choice',`data-player-id`=row$id,
              `data-lineup-membership`=membership,
              `data-position`=paste(positions[[i]],collapse='/'),`aria-pressed`=if(identical(row$id,old))'true'else'false',
              title=if(row$id%in%leaving)paste(row$name,'leaves by the next round start')else row$name,
              disabled=if(saving()||row$id%in%leaving)'disabled',onclick=picker_click(as.character(row$id),pick),
              if(nzchar(url))shiny::tags$img(class='fm-lineup-choice-crest',src=url,alt=row$club,width=24,height=24,
                loading='lazy',decoding='async',onerror="this.style.visibility='hidden'")else shiny::span(),
              shiny::span(class='fm-lineup-choice-name',
                shiny::span(class='fm-lineup-choice-label',lineup_player_label(row$name),
                  lineup_status_icon(row$status),
                  if(membership!='none')marker(if(identical(row$id,old))'crosshairs'else if(membership=='starter')'circle-check'else'chair',
                    paste0(if(identical(row$id,old))'Current slot · 'else'',membership_label))),
                shiny::tags$small(paste0(paste(positions[[i]],collapse='/'),if(unavailable)' · injured/suspended'else'',
                  if(row$id%in%leaving)' · leaves by round start'else''))),
              shiny::span(class='fm-lineup-choice-total',`aria-label`='Total points',number(row$points)),
              shiny::span(class='fm-lineup-choice-average',`aria-label`=paste('Average points over',n_rounds(),'completed rounds'),number(averages[i])))
          })))
    })
    slot_event<-function(bench,index)list(bench=bench,index=index,generation=generation,strategy=draft()$strategy)
    shiny::observeEvent(input$slot_click,{
      shiny::req(source(),draft(),!saving(),is_module_active())
      click<-input$slot_click
      shiny::req(is.list(click),is.logical(click$bench),length(click$bench)==1L,!is.na(click$bench),
        is.numeric(click$index),length(click$index)==1L,is.finite(click$index),
        click$index%in%seq_len(if(click$bench)4L else 11L),
        current(click$generation,loaded_key),identical(click$strategy,draft()$strategy))
      if(click$bench&&!isTRUE(source()$rules$bench_enabled))return()
      open_picker(click$bench,as.integer(click$index))
    })
    shiny::observeEvent(input$player_choice,{
      shiny::req(selected(),draft(),source(),!saving(),is_module_active())
      pick<-selected();choice<-input$player_choice
      shiny::req(is.list(choice),identical(choice$picker,pick$sequence),current(pick$generation,pick$key),identical(pick$strategy,draft()$strategy))
      tryCatch({
        if(choice$id%in%current_departures()$excluded_ids)stop('This player leaves by the next round start and cannot be selected.')
        edit(lineup_assign(draft(),roster(),source()$rules,pick$bench,pick$index,choice$id));shiny::removeModal();selected(NULL)},
        error=function(error)picker_notice(conditionMessage(error)))
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
      shiny::div(class='fm-lineup-note',if(is.null(next_round))fm_info('Next-round schedule unavailable; saving is disabled.',error=TRUE,pending=loading())else
        paste0('Next scheduled round ',next_round$number),
        if(dirty())shiny::strong(' · Unsaved changes'))
    })
    output$fixtures<-shiny::renderUI({
      shiny::req(is_module_active());who<-identity();shiny::req(who)
      next_round<-target()
      view<-lineup_fixture_view(round_catalog(),next_round,list(account_id=who$account,
        championship_id=who$championship,competition_id=who$competition,season=who$season),
        clubs_state=fixture_clubs_read(login=login_token(),championship_id=championship_id()))
      shiny::tags$section(class='fm-lineup-fixtures',`aria-label`='Next-round matches',
        shiny::h3(if(is.null(next_round))'Next-round matches'else paste0('Next-round matches · Round ',next_round$number)),
        shiny::p(class='fm-lineup-fixtures-note','Home team first · Times in ',fm_display_timezone()),
        if(nzchar(view$message))shiny::p(class='fm-lineup-fixtures-note',role='status',view$message),
        if(nrow(view$rows))shiny::tags$ul(class='fm-lineup-fixtures-list',lapply(seq_len(nrow(view$rows)),function(i){
          row<-view$rows[i,,drop=FALSE]
          home<-if(nzchar(row$home_name))row$home_name else 'Home team unconfirmed'
          away<-if(nzchar(row$away_name))row$away_name else 'Away team unconfirmed'
          club<-function(name,logo,away=FALSE){
            url<-lineup_fixture_crest_url(logo)
            shiny::span(class=paste('fm-lineup-fixture-team',if(away)'fm-lineup-fixture-away'),
              `aria-label`=paste(if(away)'Away:'else'Home:',name),
              if(nzchar(url))shiny::tags$img(class='fm-lineup-fixture-crest',src=url,alt='',`aria-hidden`='true',
                width=24,height=24,loading='lazy',decoding='async',onerror="this.style.display='none'"),
              shiny::span(class='fm-lineup-fixture-team-name',name))
          }
          shiny::tags$li(class='fm-lineup-fixture',`data-match-id`=row$match_id,
            shiny::tags$time(class='fm-lineup-fixture-time',datetime=if(nzchar(row$kickoff))row$kickoff,
              fm_format_datetime(row$kickoff,missing='Kickoff to be confirmed')),
            shiny::div(class='fm-lineup-fixture-teams',
              club(home,row$home_logo),
              shiny::span(class='fm-lineup-fixture-vs','vs'),
              club(away,row$away_logo,away=TRUE)))
        })),
        if(isTRUE(view$crest_stale))shiny::p(class='fm-lineup-fixtures-note',
          paste0('Club crests updated ',fm_format_datetime(view$crest_observed_at),' · Awaiting sync update')),
        if(!is.null(view$observed_at))shiny::p(class='fm-lineup-fixtures-note',
          paste0('Schedule updated ',fm_format_datetime(view$observed_at),if(view$stale)' · Awaiting sync update'else '')))
    })
    output$status<-shiny::renderUI({
      shiny::req(is_module_active());status<-history$status();score<-scores()
      basis<-best_basis();departure<-departures();changed<-character()
      if(!is.null(basis)&&!is.null(draft())&&identical(basis$lineup,lineup_identity(draft()))) {
        if(!identical(basis$n,n_rounds()))changed<-c(changed,'last N rounds')
        if(!identical(basis$include,isTRUE(input$include_doubtful)))changed<-c(changed,'doubtful-player setting')
        if(!identical(isTRUE(basis$coverage),isTRUE(input$require_bench_coverage)))changed<-c(changed,'bench coverage setting')
        if(identical(basis$n,n_rounds())&&!nzchar(catalog_issue())&&length(wanted())&&!identical(basis$rounds,wanted()))changed<-c(changed,'completed-round window')
        if(isTRUE(departure$complete)&&!identical(basis$excluded_ids %||% character(),departure$excluded_ids))changed<-c(changed,'player departures')
      }
      shiny::tagList(
        if(nzchar(best_note()))shiny::div(class='fm-lineup-note fm-lineup-best-note',best_note()),
        shiny::div(role='status',`aria-live`='polite',`aria-busy`=if(!is.null(best_request()))'true'else'false',
          if(nzchar(notice()))if(notice_error())fm_info(notice(),error=TRUE,inline=TRUE)else
            shiny::div(class='fm-lineup-note fm-lineup-message',notice())),
        if(length(changed))fm_info(error=TRUE,class='fm-lineup-best-warning',
          paste0('Optimization inputs changed: ',paste(changed,collapse=', '),'. Optimize to update this lineup.')),
        if(!isTRUE(departure$complete))fm_info(error=!is.null(departure$error),pending=loading()||identical(departure$status,"loading"),
          paste('Departure eligibility is not verified.',departure$error %||% 'Waiting for clause departure details.')),
        if(nzchar(cache_issue()))fm_info(cache_issue(),error=TRUE),
        if(nzchar(catalog_issue()))fm_info(catalog_issue(),error=TRUE)else
          if(nrow(score)&&(any(!is.finite(score$average))||(status$failed %||% 0L)>0L||!is.null(status$save_error)))
            fm_info(pending=((status$pending %||% 0L)+(status$remaining %||% 0L)+(status$saving %||% 0L)>0L)&&(status$failed %||% 0L)==0L&&is.null(status$save_error),error=(status$failed %||% 0L)>0L||!is.null(status$save_error),
          if(any(!is.finite(score$average)))paste(sum(is.finite(score$average)),'of',nrow(score),'players have complete scores for',length(wanted()),'completed rounds.'),
          if((status$pending %||% 0L)+(status$remaining %||% 0L)>0L)' Loading missing history…',
          if((status$failed %||% 0L)>0L)' Some history reads failed; use Refresh to retry.',
          if((status$saving %||% 0L)>0L)' Saving acquired history…',if(!is.null(status$save_error))status$save_error))
    })
    tile<-function(id,role,index,bench=FALSE){
      rows<-roster();row<-rows[match(id,rows$id),,drop=FALSE];occupied<-nzchar(id)&&id%in%rows$id
      average<-scores()$average[match(id,scores()$id)]
      crest<-if(occupied)crest_url(row)else''
      position<-if(!occupied)''else if(bench)fm_scalar(lineup_positions(row,FALSE)[[1]],'')else role
      # A direct event works as soon as a refreshed tile enters the DOM; dynamic
      # actionButton counters/bindings may not be ready when the first click lands.
      onclick<-paste0('Shiny.setInputValue(',jsonlite::toJSON(session$ns('slot_click'),auto_unbox=TRUE),',',
        jsonlite::toJSON(slot_event(bench,index),auto_unbox=TRUE),
        ",{priority:'event'});")
      shiny::tags$button(id=session$ns(paste0(if(bench)'bench_'else'starter_',index)),type='button',onclick=onclick,`data-player-id`=if(occupied)id else'',shiny::tagList(
        if(bench)shiny::span(class='fm-lineup-order',paste('Bench',index)),
        shiny::div(class='fm-lineup-points',if(length(average)&&is.finite(average))format(round(average,2),trim=TRUE)else'—'),
        shiny::div(class='fm-lineup-portrait',if(occupied&&nzchar(row$photo)&&grepl('^[A-Za-z0-9_.-]+$',row$photo))
          shiny::tags$img(src=paste0(PHOTO_URL,'/',row$photo),alt='',loading='lazy',onerror="this.style.display='none'")else shiny::span(class='fm-lineup-silhouette',if(occupied)'●'else'+'),
          if(occupied)lineup_status_icon(row$status)),
        shiny::div(class='fm-lineup-name',`data-position`=position,
          if(nzchar(crest))shiny::tags$img(class='fm-lineup-crest',src=crest,alt=row$club,title=row$club,
            width=14,height=14,loading='lazy',decoding='async',onerror="this.style.display='none'"),
          shiny::span(class='fm-lineup-label',if(occupied)lineup_player_label(row$name)else'Empty position'))),
        class=paste('fm-lineup-player',if(!occupied)'fm-lineup-empty'),disabled=if(saving())'disabled',
        title=if(occupied)paste('Change',row$name)else paste('Select',role))
    }
    output$pitch<-shiny::renderUI({
      shiny::req(is_module_active(),draft(),source());value<-draft();roles<-lineup_slot_roles(value$strategy)
      shiny::div(class='fm-round-lineup',
        shiny::div(class='fm-lineup-layout',
        shiny::div(class='fm-lineup-pitch',role='group',`aria-label`=paste('Pitch, formation',value$strategy),
          shiny::div(class='fm-lineup-markings',`aria-hidden`='true',shiny::div(class='fm-lineup-circle'),shiny::div(class='fm-lineup-goal-top'),shiny::div(class='fm-lineup-goal-bottom')),
          lapply(unique(roles),function(role){indices<-which(roles==role);shiny::div(class='fm-lineup-row',style=paste0('--lineup-count:',length(indices)),lapply(indices,function(i)tile(value$starters[i],role,i)))})),
        if(source()$rules$bench_enabled)shiny::div(class='fm-lineup-bench',shiny::h4('Substitutes · best to worst'),
          shiny::div(class='fm-lineup-bench-row',lapply(seq_len(4L),function(i)tile(value$bench[i],'Substitute',i,TRUE))),
          shiny::div(class='fm-lineup-coverage',lapply(c('DEF','MID','FWD'),function(role){
            covered<-lineup_bench_coverage(roster(),value$bench,source()$rules$multiposition)
            shiny::span(class=if(!role%in%covered)'fm-lineup-warning',paste(role,if(role%in%covered)'✓'else'—'))})))))
    })
    output$captain_control<-shiny::renderUI({
      shiny::req(is_module_active(),draft(),source());if(!source()$rules$captain_enabled)return(NULL)
      ids<-draft()$starters[nzchar(draft()$starters)];rows<-roster()
      shiny::selectInput(session$ns('captain'),'Captain',choices=c('Select captain'='',setNames(ids,rows$name[match(ids,rows$id)])),selected=draft()$captain)
    })
    output$save_control<-shiny::renderUI({
      shiny::req(is_module_active());issues<-if(is.null(draft())||is.null(source()))'Lineup is loading'else lineup_validate(draft(),roster(),source()$rules)
      if(!is.null(draft()))issues<-c(issues,departure_issues(draft()))
      if(is.null(target()))issues<-c(issues,'Next round is not established')
      shiny::tagList(shiny::actionButton(session$ns('save'),if(saving())'Saving…'else'Save to LigaRadar',icon=shiny::icon('floppy-disk'),
        disabled=saving()||loading()||length(issues)>0L||!dirty()),if(length(issues))fm_info(paste(unique(issues),collapse=' · '),error=!loading(),pending=loading(),inline=TRUE))
    })
    shiny::observeEvent(input$save,{
      shiny::req(source(),draft(),target(),!saving(),!loading(),dirty());value<-draft();baseline<-source();ctx<-context();next_round<-target();key<-loaded_key;g<-generation;auth<-login_token()
      issues<-c(lineup_validate(value,roster(),baseline$rules),departure_issues(value,current_departures()));if(length(issues)){notice(paste(issues,collapse=' · '),error=TRUE);return()}
      best_request(NULL);saving(TRUE);notice('Checking the official lineup and current squad…')
      fail<-function(text){if(current(g,key)){saving(FALSE);notice(text,error=TRUE)};NULL}
      verify<-function(){
        if(!current(g,key))return(NULL)
        notice('Verifying saved lineup…')
        promises::then(lineup_fetch_live(auth,ctx,next_round,owner,is_current=function()current(g,key)),function(result)tryCatch({
          if(!current(g,key))return(NULL)
          saving(FALSE);source(result);persist(result,ctx,next_round$id,g,key)
          if(identical(lineup_identity(result$official),lineup_identity(value))){draft(result$official);notice('Saved to LigaRadar and verified.');best_note('')}
          else notice('LigaRadar did not confirm the requested lineup. Your draft is retained; no automatic retry was sent.',error=TRUE)
          NULL
        },error=function(error)fail('Saved lineup could not be validated. Your draft is retained.')),function(error)fail('The save response is uncertain and readback failed. Your draft is retained. Check LigaRadar before saving again.'))
        NULL
      }
      promises::then(lineup_fetch_live(auth,ctx,next_round,owner,is_current=function()current(g,key)),function(fresh)tryCatch({
        if(!current(g,key))return(NULL)
        if(!identical(lineup_identity(fresh$official),lineup_identity(baseline$official))){source(fresh);return(fail('The official lineup changed in another session. Your draft is retained. Undo changes or review it before saving again.'))}
        if(is.null(shiny::isolate(target()))||!identical(shiny::isolate(target())$id,next_round$id)||next_round$starts_at<=Sys.time())return(fail('The round context changed before saving. Refresh the lineup.'))
        if(!identical(fresh$competition_id,baseline$competition_id)||!identical(fresh$season,baseline$season)||!identical(fresh$scoring_version,baseline$scoring_version))return(fail('The competition, season or scoring rules changed. Refresh before saving.'))
        rows<-lineup_roster_frame(fresh);issues<-c(lineup_validate(value,rows,fresh$rules),shiny::isolate(departure_issues(value,current_departures())))
        if(length(issues))return(fail(paste(issues,collapse=' · ')))
        locked<-lineup_locked_changes(fresh,value)
        if(length(locked))return(fail(paste('LigaRadar has locked selected positions:',paste(locked,collapse=', '))))
        wire<-lineup_modified_wire(value,rows,fresh$rules);changes<-lineup_changes(fresh$source$wire,wire)
        notice('Saving lineup to LigaRadar…')
        promises::then(lineup_submit(auth,ctx$championship_id,ctx$user_team_id,fresh$source$wire,wire,owner,changes),function(result)verify(),function(error)verify())
        NULL
      },error=function(error)fail(paste('Lineup validation failed.',conditionMessage(error)))),function(error)fail(paste('Current lineup could not be verified.',resource_failure_message(error))))
      invisible(NULL)
    })
    list(draft=shiny::reactive(draft()),official=shiny::reactive(if(is.null(source()))NULL else source()$official),
      source=shiny::reactive(source()),saving=shiny::reactive(saving()),scores=scores)
  })
}
