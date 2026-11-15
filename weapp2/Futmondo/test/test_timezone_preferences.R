#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth<-function(id)c(userid=id,token='fixture',user_name=paste0(id,'@example.invalid'))
Sys.setenv(admin='admin@example.invalid')
settle<-function(session=NULL){for(i in 1:25){later::run_now(0);if(!is.null(session))session$flushReact()}}
# Known answers include offset input, midnight rollover, both Spanish DST
# transitions and a half-hour zone; values and underlying instants never change.
times<-c('2026-03-29T00:59:00Z','2026-03-29T01:00:00Z','2026-10-25T00:30:00Z','2026-10-25T01:30:00Z')
stopifnot(identical(fm_format_datetime(times,'Europe/Madrid'),c('29/03/2026 01:59 CET','29/03/2026 03:00 CEST','25/10/2026 02:30 CEST','25/10/2026 02:30 CET')),
 fm_format_datetime('2026-11-15T06:02:48Z','America/Los_Angeles')=='14/11/2026 22:02 PST',
 fm_format_datetime('2026-11-15T07:02:48+01:00','Asia/Kolkata')=='15/11/2026 11:32 IST',
 fm_format_datetime('not a date')=='Date unavailable',fm_format_datetime(NA)=='Date unavailable',
 identical(fm_format_datetime(character()),character()),
 fm_format_datetime(times[1],'invalid')=='29/03/2026 01:59 CET',
 as.numeric(fm_local_midnight(as.Date('2026-03-30'))-fm_local_midnight(as.Date('2026-03-29')),units='hours')==23,
 as.numeric(fm_local_midnight(as.Date('2026-10-26'))-fm_local_midnight(as.Date('2026-10-25')),units='hours')==25)
cat('PASS common format, offsets, day rollover, DST and local calendar boundaries\n')
# Local calendar filtering uses full source instants, including explicit offsets.
transfers<-data.frame(created=c('2026-03-28T22:59:59Z','2026-03-28T23:00:00Z',
 '2026-03-29T23:59:59+02:00','2026-03-29T22:00:00Z'),price=1:4,buyer_team_id='a',seller_team_id='')
filtered<-rivals_buying_power_values(transfers,data.frame(teamid='a',teamname='A'),metric='investment',
 start_date=fm_local_midnight(as.Date('2026-03-29')),end_date=fm_local_midnight(as.Date('2026-03-30'))-0.000001,initial_budget=100)
stopifnot(filtered$value==5,as.numeric(rivals_parse_datetime('2026-03-29T23:59:59+02:00'))==as.numeric(fm_time('2026-03-29T21:59:59Z')))
cat('PASS source-offset preservation and transfer filtering across a 23-hour local day\n')

f<-new.env();f$rows<-list();f$calls<-list();f$delay<-FALSE;f$resolve<-NULL;f$fail<-FALSE
store<-function(user_id=NULL,timezone=NULL,label='',mode='get',token=NULL,owner='',after='',search=''){
 f$calls[[length(f$calls)+1L]]<-list(id=user_id,mode=mode,timezone=timezone)
 if(f$fail)return(promises::promise_reject(simpleError('fixture unavailable')))
 if(mode=='list')return(promises::promise_resolve(unname(f$rows)))
 value<-f$rows[[user_id]]
 if(mode=='set'||(mode=='initialize'&&is.null(value))){value<-list(user_id=user_id,timezone=timezone,display_name=label);f$rows[[user_id]]<-value}
 if(f$delay)return(promises::promise(function(resolve,reject)f$resolve<-resolve))
 promises::promise_resolve(value)
}
app<-function(input,output,session){
 token<-shiny::reactive(if(is.null(input$user)||input$user=='')NULL else auth(input$user))
 fm_timezone_preferences(session,token,store)
 output$label<-shiny::renderText(fm_format_datetime('2026-11-15T06:02:48Z'))
}
shiny::testServer(app,{
 session$setInputs(worker_timezone_detected='America/Los_Angeles',user='a');settle(session)
 stopifnot(output$label=='14/11/2026 22:02 PST',length(f$calls)==1L)
 session$setInputs(worker_timezone_detected='Asia/Tokyo');settle(session)
 stopifnot(output$label=='14/11/2026 22:02 PST',length(f$calls)==1L)
 session$setInputs(display_timezone='Europe/Madrid',save_timezone=1);settle(session)
 stopifnot(output$label=='15/11/2026 07:02 CET',f$rows$a$timezone=='Europe/Madrid')
 session$setInputs(user='b');settle(session)
 stopifnot(output$label=='15/11/2026 15:02 JST',f$rows$a$timezone=='Europe/Madrid')
 session$setInputs(user='');settle(session);stopifnot(output$label=='15/11/2026 07:02 CET')
})
shiny::testServer(app,{
 session$setInputs(worker_timezone_detected='America/New_York',user='a');settle(session)
 stopifnot(output$label=='15/11/2026 07:02 CET')
})
cat('PASS one initialization, saved override across sessions, browser changes and account isolation\n')
f$fail<-TRUE
shiny::testServer(app,{
 session$setInputs(worker_timezone_detected='America/Los_Angeles',user='a');settle(session)
 stopifnot(grepl('temporary timezone',output$timezone_preference_status$html),output$label=='14/11/2026 22:02 PST')
 f$fail<-FALSE;session$setInputs(retry_timezone_preference=1);settle(session)
 stopifnot(is.null(output$timezone_preference_status),output$label=='15/11/2026 07:02 CET')
 f$fail<-TRUE;session$setInputs(display_timezone='Asia/Tokyo',save_timezone=1);settle(session)
 stopifnot(grepl('not confirmed',output$timezone_preference_status$html),output$label=='15/11/2026 07:02 CET')
 f$fail<-FALSE;session$setInputs(retry_timezone_preference=2);settle(session)
 stopifnot(output$label=='15/11/2026 07:02 CET')
})
f$delay<-TRUE
shiny::testServer(app,{
 session$setInputs(worker_timezone_detected='Asia/Tokyo',user='a');settle(session)
 heartbeat<-FALSE;later::later(function()heartbeat<<-TRUE,0);settle(session);stopifnot(heartbeat)
 session$setInputs(user='');settle(session);f$resolve(f$rows$a);settle(session)
 stopifnot(output$label=='15/11/2026 07:02 CET',is.null(output$timezone_preference_status))
});f$delay<-FALSE
shiny::testServer(app,{
 session$setInputs(worker_timezone_detected='not-valid',user='fallback');settle(session)
 stopifnot(f$rows$fallback$timezone=='Europe/Madrid')
})
cat('PASS truthful failed persistence/retry, delayed nonblocking load, logout guard and Spain fallback\n')
# Every representative presentation adapter consumes the same account timezone.
shiny::testServer(function(input,output,session){
 session$userData$display_timezone<-shiny::reactiveVal('America/Los_Angeles')
 output$labels<-shiny::renderText(paste(notification_datetime_labels('2026-11-15T06:02:48Z'),
  incoming_clause_time('2026-11-15T06:02:48Z'),format_table_date('2026-11-15T06:02:48Z'),
  rivals_last_access_cell('2026-11-15T06:02:48Z'),collapse='|'))
},{session$flushReact();stopifnot(identical(strsplit(output$labels,' ',fixed=TRUE)[[1]][1],'14/11/2026'),!grepl('UTC|CET',output$labels))})
# Admin saves must match the editor that is actually displayed. Stale editors,
# duplicate clicks and late replies after authorization changes cannot cross users.
shiny::testServer(function(input,output,session){
 token<-shiny::reactive(auth(if(isTRUE(input$regular))'a'else'admin'))
 fm_admin_timezones(session,token,function()TRUE,store)
},{
 session$flushReact();settle(session)
 before<-length(f$calls)
 session$setInputs(timezone_user='a',timezone_editing='b',timezone_choice='Asia/Tokyo',timezone_save=1);settle(session)
 stopifnot(length(f$calls)==before)
 session$setInputs(timezone_editing='a',timezone_save=2);settle(session)
 stopifnot(f$rows$a$timezone=='Asia/Tokyo',length(f$calls)==before+1L)
 f$delay<-TRUE
 session$setInputs(timezone_user='b',timezone_editing='b',timezone_choice='Asia/Kolkata',timezone_save=3);settle(session)
 calls<-length(f$calls);session$setInputs(timezone_save=4);settle(session);stopifnot(length(f$calls)==calls)
 session$setInputs(regular=TRUE);settle(session);f$resolve(f$rows$b);settle(session)
 session$setInputs(timezone_save=5);settle(session);stopifnot(length(f$calls)==calls)
});f$delay<-FALSE
cat('PASS admin editor/account binding, duplicate-save guard and authorization-change isolation\n')

# API authorization, wire payload, parser and acknowledgement use no live DB.
get_sb_url<-function()'https://fixture.invalid';get_sb_key<-function()'synthetic'
requests<-list();answer<-'[{"user_id":"a","timezone":"Asia/Tokyo","display_name":"A"}]'
http<-function(url,method='GET',body=NULL,headers=character(),...){requests[[length(requests)+1L]]<<-list(url=url,method=method,body=body);promises::promise_resolve(list(content=charToRaw(answer)))}
fm_async_http<-http;fm_async_read_http<-http
await<-function(p){done<-FALSE;value<-NULL;error<-NULL;promises::then(p,function(x){value<<-x;done<<-TRUE;NULL},function(e){error<<-e;done<<-TRUE;NULL});settle();stopifnot(done);if(!is.null(error))stop(error);value}
rejected<-function(p)inherits(tryCatch(await(p),error=identity),'error')
stopifnot(rejected(fm_timezone_store('b','Asia/Tokyo',mode='set',token=auth('a'))),
 rejected(fm_timezone_store(mode='list',token=auth('a'))),rejected(fm_timezone_store('a','invalid',mode='set',token=auth('a'))),length(requests)==0L,
 await(fm_timezone_store('a','Asia/Tokyo',mode='initialize',token=auth('a')))$timezone=='Asia/Tokyo',
 grepl('initialize_user_timezone',requests[[1]]$url),jsonlite::fromJSON(requests[[1]]$body)$p_timezone=='Asia/Tokyo')
for(bad in c('[]','{}','null','[{"user_id":"b","timezone":"Asia/Tokyo"}]','[{"user_id":"a","timezone":"invalid"}]')){
 answer<-bad;stopifnot(rejected(fm_timezone_store('a','Asia/Tokyo',mode='set',token=auth('admin'))))
}
answer<-'[{"user_id":"a","timezone":"Europe/Madrid"}]'
stopifnot(rejected(fm_timezone_store('a','Asia/Tokyo',mode='set',token=auth('admin'))))
cat('PASS shared display adapters, authorization, bounded async requests and acknowledged saves\n')
elapsed<-system.time(fm_format_datetime(rep(fm_time('2026-11-15T06:02:48Z'),1000),'America/Los_Angeles'))[['elapsed']]
cat(sprintf('MEASURE 1000 labels from parsed instants: %.3f seconds\n',elapsed))
