#!/usr/bin/env Rscript
# Known-answer public-client-shaped fixtures; no real account or network access.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
now<-as.POSIXct('2026-11-16T12:00:00',tz='UTC')
scope<-list(account_id='fixture-account',championship_id='league',competition_id='competition',season='2026-27')
match_record<-function(id,home,away,date)list(`_id`=id,h=list(`_id`=paste0('h-',id),name=home),
 a=list(id=paste0('a-',id),name=away),info=list(date=date),st='scheduled',private='discard me')
records<-list(
 list(`_id`='old',matches=list(match_record('previous','Old Home','Old Away','2026-11-15T10:00:00Z'))),
 list(`_id`='next',matches=list(match_record('late','Real Madrid','Barcelona','2026-11-22T20:00:00Z'),
  match_record('early','Sevilla','Betis','2026-11-21T18:00:00Z'))),
 list(`_id`='later',matches=list(match_record('later-game','Later Home','Later Away','2026-11-28T20:00:00Z'))))
matches<-league_fixture_matches(records)
stopifnot(league_fixture_matches_valid(matches),all(matches$rounds$complete),
 !grepl('private|discard me',jsonlite::toJSON(matches,auto_unbox=TRUE)))
rounds<-data.frame(round_id=c('old','next','later'),round_number=1:3,
 begin_process=c('2026-11-15T10:00:00Z','2026-11-21T18:00:00Z','2026-11-28T20:00:00Z'))
attr(rounds,'fixture_calendar')<-list(matches=matches,scope=scope,status='ok',observed_at=now-60,valid_until=as.numeric(now)+60)
view<-function(data=rounds,target=lineup_target(data,now),context=scope)lineup_fixture_view(data,target,context,now)
selected<-view()
stopifnot(selected$complete,!selected$stale,identical(selected$rows$match_id,c('early','late')),
 identical(selected$rows$home_name,c('Sevilla','Real Madrid')),identical(selected$rows$away_name,c('Betis','Barcelona')),
 fm_format_datetime(selected$rows$kickoff[1],timezone='Europe/Madrid')=='21/11/2026 19:00 CET',
 fm_format_datetime(selected$rows$kickoff[1],timezone='America/Los_Angeles')=='21/11/2026 10:00 PST')
cat('PASS exact next-round identity, home/away ordering, kickoff ordering, and account timezone\n')

wire<-fm_worker_json(fm_worker_encode(rounds));compressed<-history_baseline_compress(charToRaw(wire))
descriptor<-fm_worker_descriptor('get_finished_rounds',list(championship_id='league'),scope)
saved<-list(key=descriptor$resource_key,manifest=list(account_id=scope$account_id,resource_key=descriptor$resource_key,
 handler='get_finished_rounds',context=descriptor$context,json_bytes=nchar(wire,type='bytes'),parts=1L,
 sha256=paste0(openssl::sha256(compressed)),observed_at=rival_finance_stamp(now-60),checked_at=rival_finance_stamp(now-59),
 valid_until=rival_finance_stamp(now+60),status='ok',completeness='complete'),chunks=list(jsonlite::base64_enc(compressed)))
restored<-fm_worker_restore(saved,scope$account_id)$data
stopifnot(identical(view(restored),selected),
 inherits(try(fm_worker_restore(saved,'another-account'),silent=TRUE),'try-error'))
for(field in names(scope)){
 foreign<-scope;foreign[[field]]<-paste0('other-',field)
 stopifnot(nrow(view(context=foreign)$rows)==0L,nzchar(view(context=foreign)$message))
}
cat('PASS actual durable worker codec/restart retains fixtures and source clocks; account/league/competition/season isolation\n')

partial<-records;partial[[2]]$matches[[2]]$info$date<-'';partial[[2]]$matches[[2]]$a$name<-''
data<-rounds;attr(data,'fixture_calendar')$matches<-league_fixture_matches(partial)
result<-view(data)
stopifnot(!result$complete,nrow(result$rows)==2L,nzchar(result$message),
 result$rows$kickoff[result$rows$match_id=='early']=='',
 result$rows$away_name[result$rows$match_id=='early']=='')
duplicate<-records;duplicate[[2]]$matches[[2]]$`_id`<-'late'
bad<-league_fixture_matches(duplicate)
stopifnot(league_fixture_matches_valid(bad),!bad$rounds$complete[2],!any(bad$rows$round_id=='next'))
bad<-matches;bad$rounds$fixture_count[2]<-1L
stopifnot(!league_fixture_matches_valid(bad))
missing<-league_fixture_matches(list(list(`_id`='next')))
stopifnot(league_fixture_matches_valid(missing),is.na(missing$rounds$fixture_count),!missing$rounds$complete,
 !league_fixture_matches_valid(matches,'foreign-round'),!league_fixture_matches_valid(list(version=1L)),
 !league_fixture_matches_valid(list(version=list(c(1,2)))))
data<-rounds;attr(data,'fixture_calendar')$valid_until<-as.numeric(now)-1
stale<-view(data);stopifnot(stale$stale,identical(stale$rows,selected$rows),identical(stale$observed_at,selected$observed_at))
data<-rounds;attr(data,'fixture_calendar')$status<-'error';stopifnot(!nrow(view(data)$rows))
data<-rounds;attr(data,'fixture_calendar')<-NULL;stopifnot(!nrow(view(data)$rows),nzchar(view(data)$message))
stopifnot(!nrow(view(target=NULL)$rows))
cat('PASS unconfirmed kickoff/opponent, duplicates, corrupt counts, old snapshots and failures remain distinct from complete or empty schedules\n')

# Keep parser/serialization costs bounded at a normal full 380-match season.
season<-lapply(1:38,function(i)list(`_id`=paste0('r',i),matches=lapply(1:10,function(j)
 match_record(paste0('m',i,'-',j),'Home','Away','2026-12-01T20:00:00Z'))))
parse_time<-system.time(full<-league_fixture_matches(season))[['elapsed']]
data<-rounds;attr(data,'fixture_calendar')$matches<-full
selection_time<-system.time(for(i in 1:20)lineup_fixture_view(data,list(id='r12'),scope,now))[['elapsed']]/20
stopifnot(nrow(full$rows)==380L,league_fixture_matches_valid(full))
cat(sprintf('MEASURE 380 fixtures: parse %.3fs; selection/validation %.4fs; memory %d bytes; no display I/O\n',
 parse_time,selection_time,as.numeric(object.size(full))))
cat('LINEUP FIXTURES: all checks passed\n')
