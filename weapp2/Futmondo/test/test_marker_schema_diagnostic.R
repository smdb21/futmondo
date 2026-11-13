options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
run<-function(body,url='https://fixture.invalid/rest/v1/player_round_history_checks?private=query',status=400L){
 logs<-character();failed<-FALSE
 task<-new.env(parent=emptyenv());task$url<-url;task$reject<-function(error)failed<<-TRUE;task$resolve<-function(value)NULL
 .fm_transport$tasks[['schema-fixture']]<-task
 withCallingHandlers(fm_transport_finish('schema-fixture',list(status_code=status,content=charToRaw(body))),
   message=function(m){logs<<-c(logs,conditionMessage(m));invokeRestart('muffleMessage')})
 list(logs=logs,failed=failed)
}
x<-run('{"code":"PGRST204","message":"Could not find the \'competition_id\' column of \'player_round_history_checks\' in the schema cache","private":"must-not-log"}')
stopifnot(x$failed,length(x$logs)==1L,grepl('missing_column=competition_id',x$logs,fixed=TRUE),!grepl('query|must-not-log|https',x$logs))
for(body in c('private unparseable response','{"code":"PGRST204","message":"private token must-not-log"}','{"code":"OTHER","message":"private response"}')){
 x<-run(body);stopifnot(x$failed,!length(x$logs))
}
x<-run('{"code":"PGRST204","message":"Could not find the \'competition_id\' column of \'player_round_history_checks\'"}',url='https://fixture.invalid/rest/v1/other')
stopifnot(x$failed,!length(x$logs))
cat('PASS schema diagnostics expose only a validated missing marker column; private bodies and queries stay unlogged; failed writes stay failed\n')
