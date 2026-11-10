#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))

run <- function() {
  old_env <- Sys.getenv(c('supabase_project_url','supabase_secret_key'),unset=NA_character_)
  old_fallback <- SB_URL
  on.exit({
    for(name in names(old_env))if(is.na(old_env[[name]]))Sys.unsetenv(name)else do.call(Sys.setenv,setNames(list(old_env[[name]]),name))
    SB_URL <<- old_fallback
    options(futmondo.offline=TRUE)
  },add=TRUE)
  checks <- 0L
  check <- function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
  check('root and REST endpoint settings normalize to one unchanged project origin',{
    origin <- 'https://database.invalid'
    variants <- c(origin,paste0(origin,'/'),paste0(origin,'///'),
      paste0('  ',origin,'/rest/v1  '),paste0(origin,'/rest/v1///'),
      paste0(origin,'/rest/v1/rest/v1/'))
    for(value in variants){Sys.setenv(supabase_project_url=value);stopifnot(identical(get_sb_url(),origin))}
    Sys.setenv(supabase_project_url='http://127.0.0.1:1234/gateway/rest/v1/')
    stopifnot(identical(get_sb_url(),'http://127.0.0.1:1234/gateway'))
  })
  check('fallback and later environment changes use the same normalization',{
    Sys.unsetenv('supabase_project_url');SB_URL <<- ' https://fallback.invalid/rest/v1/ '
    stopifnot(identical(get_sb_url(),'https://fallback.invalid'))
    Sys.setenv(supabase_project_url='https://changed.invalid/rest/v1/rest/v1///')
    stopifnot(identical(get_sb_url(),'https://changed.invalid'))
    Sys.setenv(supabase_project_url='');stopifnot(identical(get_sb_url(),'https://fallback.invalid'))
  })
  port_file <- tempfile();server_log <- tempfile()
  system2('node',c('test/supabase_url_fixture.cjs',shQuote(port_file)),wait=FALSE,stdout=server_log,stderr=server_log)
  until <- Sys.time()+3
  while(!file.exists(port_file)&&Sys.time()<until)Sys.sleep(.01)
  stopifnot(file.exists(port_file))
  base <- paste0('http://127.0.0.1:',readLines(port_file,warn=FALSE))
  on.exit(invisible(try(httr::GET(paste0(base,'/shutdown'),httr::timeout(1)),silent=TRUE)),add=TRUE)
  Sys.setenv(supabase_secret_key='synthetic-local-key');options(futmondo.offline=FALSE)
  PLAYER_SUMMARY_URL <<- paste0(base,'/1/player/summary')
  # Only the database paths under test perform HTTP. Other initial factual data
  # is synthetic and cannot contact an account or real service.
  get_lineup_from_team <<- function(...)list(pointSystem='fixture')
  score <- paste0(normalize_league_rules(list(),list(pointSystem='fixture'),'league')$scoring_version)
  await <- function(p){
    done<-FALSE;value<-failure<-NULL
    promises::then(p,function(x){value<<-x;done<<-TRUE},function(e){failure<<-e;done<<-TRUE})
    until<-Sys.time()+5
    while(!done&&Sys.time()<until){later::run_now(.01);Sys.sleep(.001)}
    stopifnot(done);if(!is.null(failure))stop(failure);value
  }
  stats <- function()as.data.frame(jsonlite::fromJSON(httr::content(httr::GET(paste0(base,'/stats')),as='text',encoding='UTF-8')))
  auth <- c(userid='url-fixture',token='synthetic')
  for(i in 1:2){
    league <- paste0('league-',i)
    configured <- if(i==1L)base else paste0(' ',base,'/rest/v1/ ')
    Sys.setenv(supabase_project_url=configured)
    ctx <- resource_context(auth,league,'team',season='unknown',scoring_version=score,session_id=paste0('url-',i))
    fetch <- function()async_adapter_fetch('read_league_round_dataset',list(championship_id=league,context=ctx),ctx)
    clear_api_cache();before<-nrow(stats());beat<-FALSE
    later::later(function()beat<<-TRUE,.02)
    started<-Sys.time();request<-fetch();dispatch<-as.numeric(difftime(Sys.time(),started,units='secs'))
    response_done<-FALSE
    promises::then(request,function(x){response_done<<-TRUE})
    while(!beat)later::run_now(.005)
    stopifnot(!response_done)
    empty<-await(request)
    stopifnot(beat,isTRUE(empty$complete),nrow(empty$points)==0L)
    player_ctx<-ctx;player_ctx$player_id<-'p'
    stamp<-format(Sys.time(),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')
    observations<-data.frame(player_id='p',championship_id=league,season='unknown',scoring_version=score,
      round=1:3,points=c(0,-2,NA_real_),score_status=c('unknown','unknown','unavailable'),
      observed_at=stamp,occurred_at=NA_character_,round_start_at=NA_character_)
    ack<-NULL
    stopifnot(isTRUE(persist_recent_points_snapshot(list(observations=observations,checked_at=stamp),
      player_ctx,function(ok)ack<<-ok)))
    until<-Sys.time()+5
    while(is.null(ack)&&Sys.time()<until){later::run_now(.01);Sys.sleep(.001)}
    stopifnot(isTRUE(ack),background_sync_status('url-fixture')$pending==0L)
    clear_api_cache();stored<-await(fetch())
    stopifnot(isTRUE(stored$complete),identical(as.numeric(stored$points$points),c(0,-2,NA_real_)),
      nrow(stored$checks)==1L,stored$checks$point_count==3L,
      identical(recent_points_saved_players(stored$points,ctx,stored$checks),'p'))
    # New controller + cleared process caches must reuse the successful check,
    # including its unavailable score, without requesting player summaries.
    clear_api_cache()
    shiny::testServer(function(input,output,session){
      source<-recent_player_points_source(session,function()auth,function()league,function()'team',
        function()data.frame(id='p'),required_rounds=function()3L,catalog_scope=FALSE)
    },{
      until<-Sys.time()+5
      repeat{
        later::run_now(.01);session$flushReact()
        status<-source$status()
        if(status$completed==1L&&status$matrix_complete)break
        stopifnot(Sys.time()<until)
      }
      stopifnot(status$failed==0L,status$saving==0L,is.null(status$save_error),!length(status$initial_errors),
        identical(as.numeric(source$history()$points),c(0,-2,NA_real_)))
    })
    requests<-stats();requests<-requests[seq.int(before+1L,nrow(requests)),,drop=FALSE]
    expected_paths<-c('/rest/v1/player_round_dataset','/rest/v1/player_match_observations',
      '/rest/v1/player_round_history_checks',rep('/rest/v1/player_round_dataset',4L))
    stopifnot(identical(requests$path,expected_paths),identical(requests$method,c('GET','POST','POST',rep('GET',4L))))
    check(paste(if(i==1L)'project root'else 'REST endpoint','uses seven requests, ordered writes and durable history reuse'),TRUE)
    cat(sprintf('MEASURE variant=%d requests=7 summary_reads=0 dispatch_seconds=%.4f delayed_heartbeat=TRUE\n',i,dispatch))
  }
  cat(sprintf('SUPABASE BASE URL: %d passed / 0 failed\n',checks))
}
run()
