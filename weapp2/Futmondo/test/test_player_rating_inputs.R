#!/usr/bin/env Rscript
# Offline API-response -> roster -> player-card rating regressions.
suppressPackageStartupMessages({library(shiny);source('futmondo_functions.R');source('intelligence_engine.R');source('Modules/Selected_Player_Module.R')})
checks<-0L
check<-function(name,expr) {force(expr);checks<<-checks+1L;cat('[PASS]',name,'\n')}
played<-list(id='played',name='Observed player',value=20e6,points=22,change=-1e6,status='',
  average=list(average=11,homeAverage='11.00',awayAverage='11.00',averageLastFive='11.00',
    matches=2,fitness=list(11,11),total='22.0'))
missing<-list(id='missing',name='Missing match data',value=2e6,points=0,change=0,status='')
preseason<-list(id='preseason',name='No appearances',value=3e6,points=0,change=0,status='',
  average=list(average=0,matches=0,fitness=list(),averageLastFive='NaN'))
no_fitness<-list(id='no-fitness',name='Observed summary',value=5e6,points=12,change=0,status='ok',
  average=list(average=6,matches=2,averageLastFive=7))
fixtures<-list(played,missing,preseason,no_fitness)
requests<-0L
futmondo_post<-function(url,...) {
  requests<<-requests+1L
  answer<-if(identical(url,CHAMPIONSHIP_PLAYERS)) list(players=fixtures) else fixtures
  stopifnot(url%in%c(ROSTER_URL,MARKET_URL,CHAMPIONSHIP_PLAYERS))
  structure(list(status_code=200L,headers=list('Content-Type'='application/json'),
    content=charToRaw(jsonlite::toJSON(list(answer=answer),auto_unbox=TRUE)),url=url),class='response')
}
get_real_clubs<-function(...) data.frame()
get_roster_bids<-function(...) data.frame()
get_my_market_players<-function(...) data.frame()
auth<-c(token='offline',userid='fixture-user')
roster<-get_players_from_team(auth,'fixture-league','fixture-team')
market<-as.data.frame(get_market_players(auth,'fixture-league','fixture-team'))
catalog<-suppressWarnings(get_championship_players(auth,'fixture-league'))
check('actual roster parser preserves every player and canonical match fields', {
  stopifnot(nrow(roster)==4L,setequal(roster$id,vapply(fixtures,`[[`,character(1),'id')),
    all(c('average.average','average.matches','average.averageLastFive','average.fitness')%in%names(roster)),
    !any(c('average','matches','averageLastFive')%in%names(roster)),!anyDuplicated(names(roster)))
  p<-roster[roster$id=='played',]
  stopifnot(p$average.average==11,p$average.matches==2,p$average.averageLastFive==11,p$average.total==22)
})
check('same observed player has identical roster, market and catalog ratings', {
  scores<-lapply(list(roster,market,catalog),calculate_fis_score)
  reference<-scores[[1]][scores[[1]]$id=='played',]
  for(frame in scores) {
    p<-frame[frame$id=='played',]
    stopifnot(p$fis_score==reference$fis_score,p$data_coverage==.8,
      is.finite(p$perf),is.finite(p$form),is.finite(p$efficiency),is.finite(p$momentum),
      is.na(p$fixture_risk),p$fis_status=='partial')
  }
})
check('missing averages and zero appearances remain unscored', {
  for(frame in list(roster,market,catalog)) {
    rated<-calculate_fis_score(frame)
    for(id in c('missing','preseason')) {
      p<-rated[rated$id==id,]
      stopifnot(is.na(p$fis_score),p$data_coverage==.2,is.na(p$perf),is.na(p$form),
        is.na(p$efficiency),is.na(p$fixture_risk),p$fis_status=='unavailable')
    }
  }
})
check('missing fitness does not delete observed statistics or invent recent scores', {
  parsed<-normalize_player_average(list(average=6,matches=2))
  stopifnot(parsed$average.average==6,parsed$average.matches==2,
    is.null(parsed$average.averageLastFive),is.null(parsed$average.total),
    identical(normalize_player_average(NULL),list()),identical(normalize_player_average(list()),list()))
  p<-roster[roster$id=='no-fitness',]
  rated<-calculate_fis_score(p)
  stopifnot(p$average.average==6,p$average.averageLastFive==7,rated$data_coverage==1)
})
check('recent averages use observed scores only and preserve reported means', {
  parsed<-normalize_player_average(list(fitness=list(10,0,5)))
  stopifnot(parsed$average.averageLastFive==5,parsed$average.total==15,
    parsed$average.fitness=='10,0,5')
  reported<-normalize_player_average(list(fitness=list(10,0,5),averageLastFive=6))
  stopifnot(reported$average.averageLastFive==6)
  for(scores in list(list(),list(NULL),list(10,NA),list(10,'unknown'))) {
    parsed<-normalize_player_average(list(fitness=scores))
    stopifnot(is.null(parsed$average.averageLastFive),is.null(parsed$average.total))
  }
})
check('player card reuses restored match indicators and explains unknown availability', {
  rated<-calculate_fis_score(roster)
  p<-rated[rated$id=='played',]
  card<-selected_player_fis_row(p)
  stopifnot(card$fis_score==p$fis_score,card$data_coverage==.8,
    is.finite(card$perf),is.finite(card$form),is.finite(card$efficiency))
  locate<-function(x) {
    if(is.call(x)&&identical(x[[1]],as.name('<-'))&&
       identical(paste(deparse(x[[2]]),collapse=''),'output$fis_panel')) return(x[[3]][[2]])
    if(is.call(x)||is.expression(x)||is.pairlist(x)) for(y in as.list(x)) {
      if(missing(y)) next
      found<-locate(y);if(!is.null(found)) return(found)
    }
    NULL
  }
  env<-new.env(parent=globalenv());env$selected_player<-function() p
  render<-function() NULL;body(render)<-locate(parse('Modules/Selected_Player_Module.R'));environment(render)<-env
  session<-shiny::MockShinySession$new()
  html<-shiny::withReactiveDomain(session,as.character(render()))
  session$close()
  stopifnot(grepl('Data coverage: 80%',html,fixed=TRUE),grepl('Not reported',html,fixed=TRUE),
    grepl('has not reported an availability status',html,fixed=TRUE),!grepl('Descriptive rating N/A',html,fixed=TRUE))
})
check('roster remains cached after normalization', {
  before<-requests
  again<-get_players_from_team(auth,'fixture-league','fixture-team')
  stopifnot(requests==before,identical(again,roster))
})
cat('PLAYER RATING INPUTS:',checks,'passed / 0 failed\n')
