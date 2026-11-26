#!/usr/bin/env Rscript
# Known-answer public-client-shaped fixtures; no real account or network access.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
now<-as.POSIXct('2026-11-16T12:00:00',tz='UTC')
scope<-list(account_id='fixture-account',championship_id='league',competition_id='competition',season='2026-27')
match_record<-function(id,home,away,date)list(`_id`=id,h=list(`_id`=paste0('h-',id),name=home,logo=paste0('home-',id,'.png')),
 a=list(id=paste0('a-',id),name=away,logo=paste0(TEAM_LOGO_URL,'away-',id,'.png')),info=list(date=date),st='scheduled',private='discard me')
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
stopifnot(league_fixture_matches_current(matches),identical(selected$rows$home_logo,c('home-early.png','home-late.png')),
 identical(selected$rows$away_logo,paste0(TEAM_LOGO_URL,c('away-early.png','away-late.png'))),
 lineup_fixture_crest_url(selected$rows$home_logo[1])==paste0(TEAM_LOGO_URL,'home-early.png'),
 lineup_fixture_crest_url(selected$rows$away_logo[1])==selected$rows$away_logo[1],
 lineup_fixture_crest_url('club')==paste0(TEAM_LOGO_URL,'club.png'),
 lineup_fixture_crest_url('club.webp')==paste0(TEAM_LOGO_URL,'club.webp'))
for(logo in c('',NA,'javascript:alert(1)','../club.png','http://example.invalid/club.png'))stopifnot(lineup_fixture_crest_url(logo)=='')
legacy<-matches;legacy$version<-1L;legacy$rows[c('home_logo','away_logo')]<-NULL
decoded<-league_fixture_matches_decode(jsonlite::fromJSON(jsonlite::toJSON(legacy,auto_unbox=TRUE),simplifyVector=FALSE))
data<-rounds;attr(data,'fixture_calendar')$matches<-decoded
stopifnot(league_fixture_matches_valid(decoded),!league_fixture_matches_current(decoded),
 identical(view(data)$rows$match_id,selected$rows$match_id),identical(view(data)$observed_at,selected$observed_at))
attr(data,'fixture_calendar')$refresh_error<-'Match calendar update failed; showing the saved schedule.'
stopifnot(view(data)$stale,nzchar(view(data)$message),nrow(view(data)$rows)==2L,
 identical(view(data)$observed_at,selected$observed_at))
without_logo<-records;without_logo[[2]]$matches[[1]]$h$logo<-NULL
data<-rounds;attr(data,'fixture_calendar')$matches<-league_fixture_matches(without_logo)
stopifnot(view(data)$complete,view(data)$rows$home_logo[2]=='')
cat('PASS source crest filenames/URLs persist through worker hydration; legacy fixtures remain readable and missing/unsafe logos never become invented images\n')

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

# Source match order, matchday numbers, calendar chronology and the editable
# team lineup are independent. The same round ID resumes after other rounds.
days <- c('2026-11-27T20:00:00Z','2026-12-02T18:00:00Z','2026-12-02T20:30:00Z',
  '2026-12-04T20:00:00Z','2027-02-07T17:00:00Z')
test_round <- function(id,game_ids,times)list(`_id`=id,matches=Map(function(game,at)
  match_record(game,paste('home',game),paste('away',game),at),game_ids,times))
calendar <- league_fixture_matches(list(test_round('r23',c('feb','second','first'),days[c(5,3,2)]),
  test_round('r15','regular15',days[4]),test_round('r14','regular14',days[1])))
split_rounds <- data.frame(round_id=c('r14','r15','r23'),round_number=c(14,15,23),
  begin_process=c(days[1],days[4],days[2]),is_finished=FALSE)
attr(split_rounds,'fixture_calendar') <- list(matches=calendar,scope=scope,status='ok',observed_at=now-60,
  valid_until=as.numeric(now)+900)
attr(split_rounds,'fixture_windows') <- lineup_fixture_windows(split_rounds,calendar)
active <- list(championships=list(list(id='league',league='competition',userteam=list(id='team'))),
  rounds=list(list(`_id`='r14',number=14,championshipId='competition',beginProcess=days[1])))
target <- function(date)lineup_active_target(active,'league','team',fm_time(date))
attr(split_rounds,'lineup_active') <- list(target=target(now))
refs <- lineup_match_rounds(split_rounds,now)
stopifnot(lineup_target(split_rounds,now)$id=='r14',refs$next_round$id=='r14',refs$upcoming_split$id=='r23',
  identical(lineup_fixture_view(split_rounds,refs$upcoming_split,scope,now)$rows$match_id,c('first','second')),
  refs$upcoming_split$groups==2L,refs$upcoming_split$later_start==fm_time(days[5]))
pending <- split_rounds
pending$begin_process[pending$round_id=='r23'] <- ''
partial_calendar <- attr(pending,'fixture_calendar')$matches
partial_calendar$rows$kickoff[partial_calendar$rows$match_id=='feb'] <- ''
partial_calendar$rounds$complete[partial_calendar$rounds$round_id=='r23'] <- FALSE
attr(pending,'fixture_windows') <- lineup_fixture_windows(pending,partial_calendar)
attr(pending,'fixture_calendar')$matches <- partial_calendar
early <- lineup_match_rounds(pending,now)$upcoming_split
partial_view <- lineup_fixture_view(pending,early,scope,now)
stopifnot(early$id=='r23',early$partial,is.null(early$groups),early$starts_at==fm_time(days[2]),
  !partial_view$complete,identical(partial_view$rows$match_id,c('first','second')),lineup_target(pending,now)$id=='r14')
dec <- fm_time('2026-12-01T12:00:00Z')
stopifnot(is.null(lineup_target(split_rounds,dec)),lineup_match_rounds(split_rounds,dec)$next_round$id=='r23')
active$rounds[[1]] <- list(`_id`='r23',number=23,championshipId='competition',beginProcess=days[2])
attr(split_rounds,'lineup_active') <- list(target=target(dec))
restored <- fm_worker_decode(jsonlite::fromJSON(fm_worker_json(fm_worker_encode(split_rounds)),simplifyVector=FALSE))
stopifnot(lineup_target(restored,dec)$id=='r23',lineup_target(restored,dec)$starts_at==fm_time(days[2]),
  is.null(lineup_target(restored,fm_time(days[2]))),is.null(lineup_active_target(active,'league','other',dec)),
  is.null(lineup_active_target(active,'league','team',dec,competition_id='another-competition')))
active$rounds <- c(active$rounds,active$rounds)
stopifnot(is.null(target(dec)))
feb <- fm_time('2027-02-01T12:00:00Z')
refs <- lineup_match_rounds(restored,feb)
stopifnot(refs$next_round$id=='r23',refs$next_round$group==2L,is.null(lineup_target(restored,feb)),
  identical(lineup_fixture_view(restored,refs$next_round,scope,feb)$rows$match_id,'feb'))
active$rounds <- list(list(`_id`='r23',number=23,championshipId='competition',beginProcess=days[5]))
attr(restored,'lineup_active') <- list(target=target(feb))
stopifnot(isTRUE(lineup_target(restored,feb)$reopened),lineup_target(restored,feb)$starts_at==fm_time(days[5]))
history <- data.frame(player_id='p',round=c(14,15,23,24),points=c(-1,2,8,0))
restored$is_finished <- c(TRUE,TRUE,FALSE)
stopifnot(identical(lineup_completed_window(restored,2),c(14,15)))
# Another full matchday closed in the interval. J23 enters once, on closure.
done <- data.frame(round_id=c('r14','r15','r23','r24'),round_number=c(14,15,23,24),is_finished=TRUE,
  begin_process=c(days[1],days[4],days[2],'2027-01-31T19:00:00Z'))
attr(done,'fixture_windows') <- attr(restored,'fixture_windows')
scores <- lineup_average_scores(data.frame(id='p'),history,done,2)
stopifnot(identical(attr(scores,'rounds'),c(24,23)),scores$average==4,scores$coverage==2L)
cat('PASS split matchdays retain one round identity, two December fixtures, an independent full-XI deadline and a separate February fixture group across durable replay\n')

# Keep parser/serialization costs bounded at a normal full 380-match season.
season<-lapply(1:38,function(i)list(`_id`=paste0('r',i),matches=lapply(1:10,function(j)
 match_record(paste0('m',i,'-',j),'Home','Away','2026-12-01T20:00:00Z'))))
parse_time<-system.time(full<-league_fixture_matches(season))[['elapsed']]
data<-rounds;attr(data,'fixture_calendar')$matches<-full
all_rounds <- data.frame(round_id=paste0('r',1:38),round_number=1:38,begin_process='')
window_time <- system.time(attr(all_rounds,'fixture_windows')<-lineup_fixture_windows(all_rounds,full))[['elapsed']]
reuse_time <- system.time(for(i in 1:100)lineup_match_rounds(all_rounds,now))[['elapsed']]/100
selection_time<-system.time(for(i in 1:20)lineup_fixture_view(data,list(id='r12'),scope,now))[['elapsed']]/20
stopifnot(nrow(full$rows)==380L,league_fixture_matches_valid(full))
cat(sprintf('MEASURE 380 fixtures: parse %.3fs; selection/validation %.4fs; memory %d bytes; no display I/O\n',
 parse_time,selection_time,as.numeric(object.size(full))))
cat(sprintf('MEASURE 380-fixture group projection %.3fs, warm window selection %.4fs, %d persisted bytes in this fixture\n',
  window_time,reuse_time,as.numeric(object.size(attr(all_rounds,'fixture_windows')))))
cat('LINEUP FIXTURES: all checks passed\n')
