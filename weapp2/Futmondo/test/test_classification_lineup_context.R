#!/usr/bin/env Rscript
# Standings publications can invalidate a reactive without changing its value.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
bind_factual_reads<-function(...)invisible(NULL)
shared_mvp_source<-function(...)reactive(list())
shared_rival_finance_source<-function(...)reactive(list(loading=TRUE))
auth<-reactiveVal(c(userid='lineup-context-fixture',token='synthetic'))
league<-reactiveVal('league');pulse<-reactiveVal(0L);failed_history<-reactiveVal(FALSE)
catalog<-reactiveVal(list(rounds=data.frame(round_id=c('r1','r2'),round_number=1:2,is_finished=TRUE)))
get_classification_round_catalog<-function(...)catalog()
get_user_team_rounds<-function(...){
  pulse()
  if(failed_history())stop(resource_failure(category='schema',resource='rounds'))
  list(list(round=1,points=4),list(round=2,points=8))
}
get_user_team_info<-function(...)list(configuration=list())
get_round_dreamteam<-function(...)list()
get_round_lineup_snapshot<-function(...){
  pulse()
  list(answer=list(strategy='3-5-2',status='running',pointSystem='custom',
    players=list(list(id='p',name='Fixture Player',position=0,rated=TRUE,points=0,customPoints=8)),bench=list(players=list())))
}
events<-new.env(parent=emptyenv());events$modals<-character()
settle<-function(session)for(i in 1:10){later::run_now(0);session$flushReact()}
checks<-0L;check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
shiny::testServer(classification_Server,args=list(is_module_active=reactive(TRUE),login_token=auth,
  championship_id=league,user_team_id=reactive('own'),
  user_teams_RV=reactive(data.frame(teamid='a',teamname='Fixture Team',points=12))),{
  root<-session$rootScope()
  root$sendModal<-function(type,message)events$modals<-c(events$modals,type)
  session$setInputs(classification_view='rounds');settle(session)
  open_lineup<-function(){
    session$setInputs(lineup_team_click=list(id='a',context=lineup_context_RV(),nonce=runif(1)))
    settle(session)
  }
  open_lineup()
  check('first click opens the selected team lineup',{
    stopifnot(!is.null(selected_lineup_RV()),tail(events$modals,1)=='show',
      grepl('8*',output$round_lineup_ui$html,fixed=TRUE))
  })
  before<-events$modals;pulse(1L);settle(session)
  check('same-round history publication keeps the first lineup open',{
    stopifnot(!is.null(selected_lineup_RV()),identical(events$modals,before))
  })
  refreshed<-catalog();refreshed$rounds$checked_at<-'new observation';catalog(refreshed);settle(session)
  check('catalog metadata refresh preserves a lineup with the same official round identity',{
    stopifnot(!is.null(selected_lineup_RV()),identical(events$modals,before))
  })
  failed_history(TRUE);settle(session)
  check('independent source error remains visible without dismissing the lineup',{
    stopifnot(!is.null(selected_lineup_RV()),identical(events$modals,before),
      grepl('unavailable',output$classification_status$html,fixed=TRUE))
  })
  failed_history(FALSE);settle(session)
  session$setInputs(round_button=list(number=1,context=round_buttons_context_RV()));settle(session)
  check('changing the selected round still dismisses the old lineup',{
    stopifnot(is.null(selected_lineup_RV()),tail(events$modals,1)=='remove')
  })
  open_lineup();league('another-league');settle(session)
  check('changing league still dismisses the old lineup',{
    stopifnot(is.null(selected_lineup_RV()),tail(events$modals,1)=='remove')
  })
  open_lineup();auth(c(userid='another-account',token='synthetic'));settle(session)
  check('changing account still dismisses the old lineup',{
    stopifnot(is.null(selected_lineup_RV()),tail(events$modals,1)=='remove')
  })
})
cat(sprintf('LINEUP CONTEXT: %d passed / 0 failed\n',checks))
