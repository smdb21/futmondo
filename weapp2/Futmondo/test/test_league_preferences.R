options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
answer<-list(championships=list(list(id='football',sport='soccer'),list(id='hoops',sport='basket'),list(id='linked',league='nba'),list(id='unknown',name='Basketball fans')),leagues=list(list(id='nba',sport='basketball')))
attr(answer,'observed_at')<-as.POSIXct('2026-11-11',tz='UTC')
filtered<-fm_filter_league_answer(answer)
stopifnot(identical(vapply(filtered$championships,function(x)x$id,character(1)),c('football','unknown')),identical(attr(filtered,'observed_at'),attr(answer,'observed_at')))
cat('PASS source sport fields block basketball while unknown sports and source timestamps are preserved\n')
settle<-function(s)for(i in 1:25){later::run_now(0);s$flushReact()}
auth<-function(id)c(userid=id,token='synthetic')
f<-new.env();f$rows<-list(a=list(championship_id='second'),b=list(championship_id='first'),blocked=list(championship_id='hoops'))
f$calls<-list();f$held<-list();f$delay<-FALSE;f$fail<-FALSE;f$held_writes<-list();f$delay_write<-FALSE
items<-list(list(id='first',sport='soccer'),list(id='second',sport='football'),list(id='hoops',sport='basket'))
store<-function(user_id,championship_id=NULL,selected_at=NULL,owner=''){
 f$calls[[length(f$calls)+1L]]<-list(user=user_id,id=championship_id)
 if(is.null(championship_id)){
  value<-f$rows[[user_id]]
  if(f$delay)return(promises::promise(function(resolve,reject)f$held[[length(f$held)+1L]]<-list(resolve=resolve,value=value)))
  return(promises::promise_resolve(value))
 }
 if(f$fail)return(promises::promise_resolve(FALSE))
 if(f$delay_write)return(promises::promise(function(resolve,reject)f$held_writes[[length(f$held_writes)+1L]]<-list(resolve=resolve,id=championship_id,user=user_id)))
 f$rows[[user_id]]<-list(championship_id=championship_id);promises::promise_resolve(TRUE)
}
writes<-function()sum(vapply(f$calls,function(x)!is.null(x$id),logical(1)))
server<-function(input,output,session){
 token<-shiny::reactive(if(is.null(input$user)||input$user=='')NULL else auth(input$user))
 choices<-shiny::reactive(if(identical(input$only_hoops,TRUE))items[3L]else if(identical(input$only_first,TRUE))items[1L]else items)
 nav<-fm_league_preferences(session,token,choices,store)
}
shiny::testServer(server,{
 session$setInputs(user='a');settle(session)
 stopifnot(nav$selected()=='second',writes()==0L)
 session$setInputs(selected_league='second');settle(session);stopifnot(writes()==0L)
 session$setInputs(selected_league='first');settle(session)
 stopifnot(nav$selected()=='first',f$rows$a$championship_id=='first',writes()==1L)
 session$setInputs(selected_league='hoops');settle(session);stopifnot(nav$selected()=='first',writes()==1L)
 session$setInputs(user='');settle(session);stopifnot(is.null(nav$selected()))
 session$setInputs(user='a');settle(session);stopifnot(nav$selected()=='first',writes()==1L)
 session$setInputs(selected_league='first');settle(session)
 session$setInputs(user='blocked');settle(session);stopifnot(nav$selected()=='first',writes()==2L,f$rows$blocked$championship_id=='first')
 session$setInputs(user='b');settle(session);stopifnot(nav$selected()=='first',writes()==2L)
})
cat('PASS account restore, no restore writes, cross-session reuse, blocked selections and account isolation\n')
f$delay<-TRUE
shiny::testServer(server,{
 session$setInputs(user='a');settle(session);stopifnot(is.null(nav$selected()))
 session$setInputs(user='b');settle(session)
 f$held[[length(f$held)-1L]]$resolve(list(championship_id='second'));settle(session);stopifnot(is.null(nav$selected()))
 f$held[[length(f$held)]]$resolve(list(championship_id='first'));settle(session);stopifnot(nav$selected()=='first')
})
cat('PASS delayed preference reads do not start a default league or publish across accounts\n')
shiny::testServer(server,{
 session$setInputs(user='a');settle(session)
 session$setInputs(selected_league='second');settle(session)
 stopifnot(nav$selected()=='second',f$rows$a$championship_id=='second')
 f$held[[length(f$held)]]$resolve(list(championship_id='first'));settle(session)
 stopifnot(nav$selected()=='second')
})
cat('PASS a late restore cannot override a deliberate league change\n')

f$delay<-FALSE;f$fail<-TRUE
shiny::testServer(server,{
 session$setInputs(user='b');settle(session)
 session$setInputs(selected_league='first');settle(session)
 session$setInputs(selected_league='second');settle(session)
 stopifnot(nav$selected()=='second',grepl('not been saved',output$league_preference_status$html))
 f$fail<-FALSE;session$setInputs(retry_league_preference=1);settle(session)
 stopifnot(f$rows$b$championship_id=='second',is.null(output$league_preference_status))
 session$setInputs(only_first=TRUE);settle(session);stopifnot(nav$selected()=='first',f$rows$b$championship_id=='first')
})
cat('PASS unacknowledged saves remain retryable; removed leagues fall back to a valid choice\n')
f$delay_write<-TRUE
shiny::testServer(server,{
 session$setInputs(user='b');settle(session)
 session$setInputs(selected_league='first');settle(session)
 session$setInputs(selected_league='second');settle(session)
 session$setInputs(selected_league='first');settle(session)
 session$setInputs(selected_league='second');settle(session)
 stopifnot(length(f$held_writes)==1L)
 f$held_writes[[1]]$resolve(TRUE);settle(session)
 stopifnot(length(f$held_writes)==2L,f$held_writes[[2]]$id=='second')
 f$held_writes[[2]]$resolve(TRUE);settle(session)
 stopifnot(nav$selected()=='second')
})
cat('PASS rapid changes coalesce to the latest selection with one in-flight write\n')
f$delay_write<-FALSE
shiny::testServer(server,{
 session$setInputs(user='a');settle(session)
 session$setInputs(selected_league='second');settle(session)
 before<-writes();session$setInputs(only_hoops=TRUE);settle(session)
 stopifnot(length(nav$selected())==0L,writes()==before)
 session$setInputs(only_hoops=FALSE);settle(session)
 stopifnot(nav$selected()=='second',writes()==before)
})
cat('PASS an all-basketball list has no selected league and cannot overwrite the saved football choice\n')
# Independently inspect the real REST contract with synthetic responses.
get_sb_url<-function()'https://fixture.invalid';get_sb_key<-function()'synthetic-server-key'
requests<-list();fm_async_http<-function(url,method,body,headers,owner,timeout){
 requests[[length(requests)+1L]]<<-list(url=url,method=method,body=body,timeout=timeout)
 promises::promise_resolve(list(content=charToRaw(if(method=='GET')'[{"championship_id":"second","selected_at":"2026-11-11T00:00:00Z"}]'else'')))
}
p<-fm_league_store('a');q<-fm_league_store('a','second','2026-11-11T00:00:00Z');for(i in 1:10)later::run_now(0)
stopifnot(length(requests)==2L,grepl('user_id=eq.a',requests[[1]]$url,fixed=TRUE),requests[[2]]$method=='POST',
 jsonlite::fromJSON(requests[[2]]$body)$championship_id=='second',requests[[1]]$timeout==8)
cat('PASS durable store reads one scoped row and sends a timestamped account/league upsert\n')
