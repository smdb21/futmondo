#!/usr/bin/env Rscript
# Isolated Your Team preference tests. No API calls, credentials or hiring.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({library(shiny);library(shinydashboard);library(reactable)})
source('data_contracts.R')
source('Modules/Players_in_Teams_Module.R')
checks<-0L
check<-function(name,code){force(code);checks<<-checks+1L;cat('PASS',name,'\n')}
http_attempts<-0L
for(method in c('GET','POST','PUT','PATCH','DELETE')) suppressMessages(trace(method,where=asNamespace('httr'),print=FALSE,
  tracer=quote({http_attempts<<-http_attempts+1L;stop('Network is forbidden in coach preference tests')})))
# The unrelated roster widget is covered in test_ui_reliability.R.
players_table_UI<-function(id,...) div(id=id)
check('Your Team includes the namespaced coach control',{
  html<-as.character(players_in_teams_UI('squad'))
  stopifnot(grepl('squad-coach-preference',html,fixed=TRUE),grepl('Automatic coach',html,fixed=TRUE))
})
auth<-reactiveVal(c(token='test',userid='account-a'))
league<-reactiveVal('league-a');team<-reactiveVal('team-a')
testServer(squad_coach_Server,args=list(login_token=auth,championship_id=league,user_team_id=team),{
  session$flushReact()
  key_a<-context()$input_id
  check('new league is checked by default and explicitly pending',{
    stopifnot(preference()$enabled,!preference()$execution_available,
      preference()$reason=='hiring_endpoint_unverified',
      grepl('checked',output$preference$html,fixed=TRUE),
      grepl('Pending:',output$status$html,fixed=TRUE))
  })
  check('unchecking changes the preference and status',{
    do.call(session$setInputs,setNames(list(FALSE),key_a))
    stopifnot(!preference()$enabled,!preference()$execution_available,
      grepl('off for this league',output$status$html,fixed=TRUE))
  })
  check('league switching restores preferences and ignores delayed prior-league input',{
    league('league-b');team('team-b');session$flushReact()
    key_b<-context()$input_id
    stopifnot(key_b!=key_a,preference()$enabled)
    do.call(session$setInputs,setNames(list(FALSE),key_a))
    stopifnot(preference()$enabled)
    league('league-a');team('team-a');session$flushReact()
    stopifnot(!preference()$enabled)
  })
  check('preferences cannot leak across accounts or teams',{
    team('team-another');session$flushReact();stopifnot(preference()$enabled)
    team('team-a');auth(c(token='test-b',userid='account-b'));session$flushReact()
    stopifnot(preference()$enabled,preference()$account_id=='account-b')
    auth(NULL);session$flushReact();stopifnot(is.null(preference()))
  })
})
auth(c(token='test',userid='account-a'))
testServer(squad_coach_Server,args=list(login_token=auth,championship_id=league,user_team_id=team),{
  session$flushReact()
  check('a new app session starts checked without promising execution',{
    stopifnot(preference()$enabled,!preference()$execution_available)
  })
})
stored_preferences<-new.env(parent=emptyenv())
read_league_preference<-function(user_id,championship_id,user_team_id) stored_preferences[[paste(user_id,championship_id,user_team_id)]]
save_league_preference<-function(user_id,championship_id,user_team_id,enabled) {
  stored_preferences[[paste(user_id,championship_id,user_team_id)]]<-enabled;TRUE
}
testServer(squad_coach_Server,args=list(login_token=auth,championship_id=league,user_team_id=team),{
  session$flushReact();do.call(session$setInputs,setNames(list(FALSE),context()$input_id))
})
testServer(squad_coach_Server,args=list(login_token=auth,championship_id=league,user_team_id=team),{
  session$flushReact()
  check('acknowledged preference survives a new app session',stopifnot(!preference()$enabled))
})
check('no hiring requests or other HTTP were sent',stopifnot(http_attempts==0L))
for(method in c('GET','POST','PUT','PATCH','DELETE')) suppressMessages(untrace(method,where=asNamespace('httr')))
cat(sprintf('COACH PREFERENCE: %d passed / 0 failed\n',checks))
