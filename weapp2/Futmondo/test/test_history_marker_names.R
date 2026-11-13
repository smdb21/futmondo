options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
ctx<-resource_context(account_id='marker-fixture',championship_id='league',user_team_id='mine',player_id='p',season='season',competition_id='competition',scoring_version='score')
stamp<-fm_time('2026-11-13T07:43:00.123456Z')
stopifnot(length(names(stamp))==1L)
baseline<-data.frame(championship_id='league',checked_at=format(stamp,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))
wire_fields<-function(json)names(jsonlite::fromJSON(json,simplifyVector=FALSE)[[1L]])
stopifnot('_row'%in%wire_fields(jsonlite::toJSON(baseline,auto_unbox=TRUE)))
expected<-c('championship_id','competition_id','player_id','season','scoring_version','checked_at','result_status','point_count')
points<-data.frame(player_id='p',championship_id='league',competition_id='competition',season='season',scoring_version='score',round=1,points=-2,score_status='final',observed_at='2026-11-13T07:43:00.123456Z',occurred_at=NA_character_,round_start_at=NA_character_)
rownames(points)<-'named-source-row'
payload<-list(context=ctx,observations=points,checked_at=stamp)
get_sb_url<-function()'https://fixture.invalid';get_sb_key<-function()'synthetic'
requests<-list();ack<-logical()
fm_async_http<-function(url,method,body,headers,owner){promises::promise(function(resolve,reject){requests[[length(requests)+1L]]<<-list(url=url,body=body,resolve=resolve,reject=reject)})}
drain<-function()for(i in 1:40)later::run_now(0)
options(futmondo.offline=FALSE)
stopifnot(persist_recent_points_snapshot(payload,ctx,function(ok)ack<<-c(ack,ok)));drain()
score<-jsonlite::fromJSON(requests[[1]]$body);stopifnot(!'_row'%in%wire_fields(requests[[1]]$body),score$points==-2)
requests[[1]]$resolve(list(status_code=201L));drain();marker<-jsonlite::fromJSON(requests[[2]]$body)
stopifnot(setequal(wire_fields(requests[[2]]$body),expected),marker$point_count==1L,abs(as.numeric(fm_time(marker$checked_at))-as.numeric(stamp))<1e-6,!length(ack))
requests[[2]]$reject(resource_failure(resource='database',category='http',http_status=400L));drain();stopifnot(identical(ack,FALSE))
# Replay exactly the retained acquisition; no source function participates.
stopifnot(persist_recent_points_snapshot(payload,ctx,function(ok)ack<<-c(ack,ok)));drain()
requests[[3]]$resolve(list(status_code=201L));drain();requests[[4]]$resolve(list(status_code=201L));drain()
stopifnot(identical(ack,c(FALSE,TRUE)),identical(requests[[1]]$body,requests[[3]]$body),identical(requests[[2]]$body,requests[[4]]$body))
cat('PASS named ISO timestamps and score rows serialize only factual columns; marker failure stays failed and source-free replay acknowledges in order\n')
# Exercise the direct writer using the same named frame, without real HTTP.
POST<-function(url,query,body,...){stopifnot(!'_row'%in%wire_fields(body));fm_httr_response(list(status_code=201L,content=charToRaw('[]'),times=list(total=0)),url)}
stopifnot(supabase_post_direct('player_match_observations',points)==201L)
cat('PASS direct DB writer also strips row metadata and preserves negative scores\n')
# Evaluate only the bridge serializer definition; never start its acquisition loop.
bridge<-new.env(parent=.GlobalEnv);path<-tempfile();bridge$pipe_out<-file(path,'w')
for(expr in parse('worker/helper.R'))if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&identical(expr[[2]],as.name('fm_pipe_send')))eval(expr,bridge)
bridge$fm_pipe_send(list(type='persist',table='player_match_observations',payload=points));close(bridge$pipe_out)
bridge_json<-paste(readLines(path,warn=FALSE),collapse='');wire<-jsonlite::fromJSON(bridge_json);bridge_raw<-jsonlite::fromJSON(bridge_json,simplifyVector=FALSE);unlink(path)
stopifnot(!'_row'%in%names(bridge_raw$payload[[1L]]),wire$payload$points==-2)
cat('PASS Python persistence bridge sends no implicit R row names\n')
options(futmondo.offline=TRUE)
