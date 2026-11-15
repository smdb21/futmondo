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

f<-new.env();f$rows<-list();f$calls<-list();f$delay<-FALSE;f$resolve<-NULL;f$fail<-FALSE;f$uncertain<-FALSE
store<-function(user_id=NULL,timezone=NULL,mode='get',token=NULL,owner=''){
 f$calls[[length(f$calls)+1L]]<-list(id=user_id,mode=mode,timezone=timezone)
 if(f$fail)return(promises::promise_reject(simpleError('fixture unavailable')))
 value<-f$rows[[user_id]]
 if(mode=='set'){
  value<-list(user_id=user_id,timezone=timezone,source='user');f$rows[[user_id]]<-value
  if(f$uncertain)return(promises::promise_reject(simpleError('lost acknowledgement')))
 }
 if(f$delay)return(promises::promise(function(resolve,reject)f$resolve<-resolve))
 promises::promise_resolve(value)
}
app<-function(input,output,session){
 token<-shiny::reactive(if(is.null(input$user)||input$user=='')NULL else auth(input$user))
 fm_timezone_preferences(session,token,store)
 output$label<-shiny::renderText(fm_format_datetime('2026-11-15T06:02:48Z'))
 output$beats<-shiny::renderText(input$heartbeat %||% 0L)
}
shiny::testServer(app,{
 session$setInputs(worker_timezone_detected='America/Los_Angeles',user='a');settle(session)
 stopifnot(output$label=='14/11/2026 22:02 PST',length(f$calls)==1L,f$calls[[1]]$mode=='get',is.null(f$rows$a))
 session$setInputs(worker_timezone_detected='Asia/Tokyo');settle(session)
 stopifnot(output$label=='15/11/2026 15:02 JST',length(f$calls)==1L)
 session$setInputs(display_timezone='America/Los_Angeles',save_timezone=1);settle(session)
 stopifnot(output$label=='14/11/2026 22:02 PST',f$rows$a$source=='user')
 session$setInputs(user='b');settle(session)
 stopifnot(output$label=='15/11/2026 15:02 JST',is.null(f$rows$b),f$rows$a$timezone=='America/Los_Angeles')
 session$setInputs(user='');settle(session);stopifnot(output$label=='15/11/2026 07:02 CET')
})
shiny::testServer(app,{
 session$setInputs(worker_timezone_detected='America/New_York',user='a');settle(session)
 stopifnot(output$label=='14/11/2026 22:02 PST')
})
cat('PASS one personal lookup, detected default, explicit durable override/reload and account isolation\n')
for(source in c('browser','admin','user')){
 f$rows$legacy<-list(user_id='legacy',timezone='Asia/Tokyo',source=source)
 before<-length(f$calls)
 shiny::testServer(app,{
  session$setInputs(user='legacy');settle(session)
  stopifnot(output$label==if(source=='user')'15/11/2026 15:02 JST'else'15/11/2026 07:02 CET',
   length(f$calls)==before+1L,identical(f$rows$legacy$source,source))
 })
}
cat('PASS legacy automatic/admin values do not become personal choices; explicit user choices persist without rewriting history\n')
shiny::testServer(app,{
 session$setInputs(user='late');settle(session)
 calls<-length(f$calls);stopifnot(output$label=='15/11/2026 07:02 CET')
 session$setInputs(worker_timezone_detected='America/Los_Angeles');settle(session)
 stopifnot(output$label=='14/11/2026 22:02 PST',length(f$calls)==calls)
 session$setInputs(worker_timezone_detected='Invalid/Zone');settle(session)
 stopifnot(output$label=='15/11/2026 07:02 CET',length(f$calls)==calls)
 session$setInputs(display_timezone='Asia/Kolkata',save_timezone=1);settle(session)
 session$setInputs(worker_timezone_detected='America/New_York');settle(session)
 stopifnot(output$label=='15/11/2026 11:32 IST')
})
cat('PASS late detection updates only an unset default without I/O; invalid detection falls back to Spain and saved choices win\n')
f$rows$race<-list(user_id='race',timezone='UTC',source='user');f$delay<-TRUE
shiny::testServer(app,{
 before<-length(f$calls)
 session$setInputs(user='race',worker_timezone_detected='America/Los_Angeles');settle(session)
 stopifnot(output$label=='14/11/2026 22:02 PST',length(f$calls)==before+1L)
 f$resolve(f$rows$race);settle(session)
 stopifnot(output$label=='15/11/2026 06:02 UTC')
 session$setInputs(worker_timezone_detected='Asia/Tokyo');settle(session)
 stopifnot(output$label=='15/11/2026 06:02 UTC',length(f$calls)==before+1L)
});f$delay<-FALSE
cat('PASS delayed saved preference replaces provisional detection and remains authoritative without extra I/O\n')


f$fail<-TRUE
shiny::testServer(app,{
 session$setInputs(worker_timezone_detected='America/Los_Angeles',user='a');settle(session)
 stopifnot(grepl('Temporarily using the detected timezone',output$timezone_preference_status$html),output$label=='14/11/2026 22:02 PST')
 f$fail<-FALSE;session$setInputs(retry_timezone_preference=1);settle(session)
 stopifnot(is.null(output$timezone_preference_status),output$label=='14/11/2026 22:02 PST')
 f$fail<-TRUE;session$setInputs(display_timezone='Asia/Tokyo',save_timezone=1);settle(session)
 stopifnot(grepl('not confirmed',output$timezone_preference_status$html),output$label=='14/11/2026 22:02 PST')
 f$fail<-FALSE;session$setInputs(retry_timezone_preference=2);settle(session)
 stopifnot(output$label=='14/11/2026 22:02 PST')
})
f$delay<-TRUE
shiny::testServer(app,{
 session$setInputs(user='a');settle(session)
 session$setInputs(heartbeat=1);settle(session);stopifnot(output$beats=='1')
 session$setInputs(user='');settle(session);f$resolve(f$rows$a);settle(session)
 stopifnot(output$label=='15/11/2026 07:02 CET',is.null(output$timezone_preference_status))
});f$delay<-FALSE
shiny::testServer(app,{
 session$setInputs(user='a');settle(session)
 f$delay<-TRUE
 session$setInputs(display_timezone='Asia/Kolkata',save_timezone=1);settle(session)
 calls<-length(f$calls);session$setInputs(save_timezone=2,heartbeat=1);settle(session)
 stopifnot(length(f$calls)==calls,output$beats=='1')
 session$setInputs(user='');settle(session);f$resolve(f$rows$a);settle(session)
 stopifnot(output$label=='15/11/2026 07:02 CET')
});f$delay<-FALSE
shiny::testServer(app,{
 session$setInputs(user='a');settle(session);f$uncertain<-TRUE
 session$setInputs(display_timezone='Asia/Tokyo',save_timezone=1);settle(session)
 count<-length(f$calls);session$setInputs(save_timezone=2);settle(session);stopifnot(length(f$calls)==count)
 f$uncertain<-FALSE;session$setInputs(retry_timezone_preference=1);settle(session)
 stopifnot(output$label=='15/11/2026 15:02 JST',length(f$calls)==count+1L,tail(f$calls,1)[[1]]$mode=='get')
})
cat('PASS truthful read/save failures, uncertain-save readback, duplicate guard, held read/write responsiveness and logout isolation\n')
# Every representative presentation adapter consumes the same account timezone.
shiny::testServer(function(input,output,session){
 session$userData$display_timezone<-shiny::reactiveVal('America/Los_Angeles')
 output$labels<-shiny::renderText(paste(notification_datetime_labels('2026-11-15T06:02:48Z'),
  incoming_clause_time('2026-11-15T06:02:48Z'),format_table_date('2026-11-15T06:02:48Z'),
  rivals_last_access_cell('2026-11-15T06:02:48Z'),collapse='|'))
},{session$flushReact();stopifnot(identical(strsplit(output$labels,' ',fixed=TRUE)[[1]][1],'14/11/2026'),!grepl('UTC|CET',output$labels))})
stopifnot(!exists('fm_admin_timezones',mode='function'),!grepl('User timezones|timezone_admin',as.character(admin_UI('admin'))))
cat('PASS no administrator timezone directory or cross-user editor\n')

# API authorization, wire payload, parser and acknowledgement use no live DB.
get_sb_url<-function()'https://fixture.invalid';get_sb_key<-function()'synthetic'
requests<-list();answer<-'[{"user_id":"a","timezone":"Asia/Tokyo","source":"user"}]'
http<-function(url,method='GET',body=NULL,headers=character(),...){requests[[length(requests)+1L]]<<-list(url=url,method=method,body=body);promises::promise_resolve(list(content=charToRaw(answer)))}
fm_async_http<-http;fm_async_read_http<-http
await<-function(p){done<-FALSE;value<-NULL;error<-NULL;promises::then(p,function(x){value<<-x;done<<-TRUE;NULL},function(e){error<<-e;done<<-TRUE;NULL});settle();stopifnot(done);if(!is.null(error))stop(error);value}
rejected<-function(p)inherits(tryCatch(await(p),error=identity),'error')
for(who in c('a','admin')){
 stopifnot(rejected(fm_timezone_store('b','Asia/Tokyo',mode='set',token=auth(who))),
  rejected(fm_timezone_store('b',mode='get',token=auth(who))),
  rejected(fm_timezone_store(mode='list',token=auth(who))),
  rejected(fm_timezone_store(who,'Asia/Tokyo',mode='initialize',token=auth(who))))
}
stopifnot(rejected(fm_timezone_store('a','invalid',mode='set',token=auth('a'))),length(requests)==0L,
 await(fm_timezone_store('a','Asia/Tokyo',mode='set',token=auth('a')))$timezone=='Asia/Tokyo',
 jsonlite::fromJSON(requests[[1]]$body)$p_source=='user')
answer<-'[{"user_id":"admin","timezone":"UTC","source":"user"}]'
stopifnot(await(fm_timezone_store('admin','UTC',mode='set',token=auth('admin')))$timezone=='UTC',
 jsonlite::fromJSON(tail(requests,1)[[1]]$body)$p_source=='user')
answer<-'[]';stopifnot(is.null(await(fm_timezone_store('a',mode='get',token=auth('a')))))
for(bad in c('[]','{}','null','[{"user_id":"b","timezone":"Asia/Tokyo","source":"user"}]',
 '[{"user_id":"a","timezone":"invalid","source":"user"}]','[{"user_id":"a","timezone":"Asia/Tokyo"}]',
 '[{"user_id":"a","timezone":"Asia/Tokyo","source":"admin"}]')){
 answer<-bad;stopifnot(rejected(fm_timezone_store('a','Asia/Tokyo',mode='set',token=auth('a'))))
}
cat('PASS shared display adapters, authorization, bounded async requests and acknowledged saves\n')
elapsed<-system.time(fm_format_datetime(rep(fm_time('2026-11-15T06:02:48Z'),1000),'America/Los_Angeles'))[['elapsed']]
cat(sprintf('MEASURE 1000 labels from parsed instants: %.3f seconds\n',elapsed))
