options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
ctx<-list(account_id='fixture',championship_id='league',competition_id='competition',season='2026',user_team_id='team')
now<-Sys.time();begin<-format(now+86400,'%Y-%m-%dT%H:%M:%OS3Z',tz='UTC')
calendar<-list(account_id=ctx$account_id,championship_id=ctx$championship_id,competition_id=ctx$competition_id,season=ctx$season,
 evidence_type='eligibility',source_id=paste0('calendar:v1:',as.character(openssl::sha256('native-league'))),observed_at=rival_finance_stamp(now-10),
 payload=list(kind='league_round_calendar',version=2,completeness='complete',scope=c(ctx[c('account_id','championship_id','competition_id','season')],list(league_id='native-league')),
 valid_until=as.numeric(now)+100,rows=list(list(round_id='opaque13',round_number=13,is_finished=NA,status='next')),
 fixture_rounds=list(list(round_id='opaque13',begin_process=begin,fixture_dates=list(begin),all_fixtures_dated=TRUE))))
read<-function(e=list(calendar),context=ctx,id='opaque13',number=13)classification_saved_future(e,context,id,number,now)
future<-read();stopifnot(isTRUE(future$not_started),nrow(future$rows)==0L,identical(future$starts_at,begin),
 as.numeric(attr(future,'observed_at'))==as.numeric(fm_time(calendar$observed_at)),future$valid_until<=as.numeric(now)+60)
# Preserve the same result through the database's simplified JSON shape.
json_calendar<-jsonlite::fromJSON(jsonlite::toJSON(calendar,auto_unbox=TRUE,null='null',na='null'))
stopifnot(isTRUE(read(list(json_calendar))$not_started))
for(field in c('account_id','championship_id','competition_id','season')){
 bad<-calendar;bad[[field]]<-'foreign';stopifnot(is.null(read(list(bad))))
 bad<-calendar;bad$payload$scope[[field]]<-'foreign';stopifnot(is.null(read(list(bad))))
}
for(kind in c('expired','future-observation','excess-ttl','old-version','partial','unverified','wrong-source','already-started','finished','undated','playing')){
 bad<-calendar
 if(kind=='expired')bad$payload$valid_until<-as.numeric(now)-1
 if(kind=='future-observation')bad$observed_at<-rival_finance_stamp(now+100)
 if(kind=='excess-ttl')bad$payload$valid_until<-as.numeric(now)+1e8
 if(kind=='old-version')bad$payload$version<-1
 if(kind=='partial')bad$payload$completeness<-'partial'
 if(kind=='unverified')bad$payload$fixture_rounds<-NULL
 if(kind=='wrong-source')bad$source_id<-'calendar:wrong'
 if(kind=='already-started')bad$payload$fixture_rounds[[1]]$begin_process<-rival_finance_stamp(now-100)
 if(kind=='playing')bad$payload$rows[[1]]$status<-'playing'
 if(kind=='finished')bad$payload$rows[[1]]$is_finished<-TRUE
 if(kind=='undated')bad$payload$fixture_rounds[[1]]$all_fixtures_dated<-FALSE
 stopifnot(is.null(read(list(bad))))
}
stopifnot(is.null(read(id='other')),is.null(read(number=12)),is.null(read(context=modifyList(ctx,list(season='unknown')))))
newer<-calendar;newer$observed_at<-rival_finance_stamp(now-5);newer$payload$fixture_rounds<-NULL
stopifnot(is.null(read(list(calendar,newer))))
# Exercise the source getter against durable evidence, with source HTTP forbidden.
resource_context<-function(...)ctx
read_rival_finance_evidence<-function(...)list(evidence=list(calendar),complete=TRUE)
futmondo_post<-function(...)stop('Unexpected source HTTP')
clear_api_cache()
actual<-get_classification_round_ranking(c(userid='fixture',token='synthetic'),'league','team','opaque13',13)
stopifnot(isTRUE(actual$not_started),identical(actual$starts_at,begin))
clear_api_cache()
reopened<-get_classification_round_ranking(c(userid='fixture',token='synthetic'),'league','team','opaque13',13)
stopifnot(isTRUE(reopened$not_started),identical(as.numeric(attr(reopened,'observed_at')),as.numeric(fm_time(calendar$observed_at))))
cat('PASS future standings use fresh scoped persisted fixture dates; zero source HTTP, original timestamp, no zero scores, JSON/restart reuse and stale/conflicting/context guards\n')
