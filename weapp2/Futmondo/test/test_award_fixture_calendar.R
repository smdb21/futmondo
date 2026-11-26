#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R')})
# Match the official client's answer.rounds[].matches[].info.date contract.
records<-list(
 list(`_id`='r1',number=1,status='closed',matches=list(list(info=list(date='2026-02-02T20:00:00Z')),list(info=list(date='2026-02-01T18:00:00Z')))),
 list(`_id`='r2',number=2,status='running',matches=list(list(info=list(date='2026-12-09T19:00:00Z')),list(info=list(date='')))))
fixtures<-award_fixture_calendar(records)
stopifnot(award_fixture_calendar_valid(fixtures),fixtures$begin_process[1]=='2026-02-01T18:00:00Z',
 fixtures$all_fixtures_dated[1],!fixtures$all_fixtures_dated[2],fixtures$begin_process[2]=='',
 identical(unlist(fixtures$fixture_dates[[2]],use.names=FALSE),'2026-12-09T19:00:00Z'))
cat('PASS capture-shaped match dates retain postponed fixtures and require complete dates for an earliest kickoff\n')
original_cache<-get_cached_data;original_answer<-mvp_api_answer;original_url<-get_sb_url;original_key<-get_sb_key;original_get<-supabase_get;original_defer<-defer_persistence
get_cached_data<-function(key,expr,...)force(expr)
get_sb_url<-function()'';get_sb_key<-function()''
reads<-0L
mvp_api_answer<-function(login,url,query){reads<<-reads+1L;stopifnot(url=='https://api.futmondo.com/2/league/matches',identical(query,list(leagueId='league-id')));list(status='ok',answer=list(rounds=records))}
auth<-c(userid='u',token='synthetic')
metadata<-get_award_round_metadata(auth,'championship','league-id',current_round_id='r2')
stopifnot(reads==1L,award_calendar_rows_valid(metadata$rows),award_fixture_calendar_valid(metadata$fixture_rounds),metadata$complete,
 identical(names(metadata$rows),c('round_id','round_number','is_finished','status')))
cat('PASS bulk metadata keeps its existing four-column ranking contract and adds fixture dates\n')
saved<-NULL
defer_persistence<-function(fn,args,context,on_complete=NULL,...){saved<<-args[[2]][[1]];if(is.function(on_complete))on_complete(TRUE);TRUE}
ctx<-resource_context(auth,'championship',season='unknown')
stopifnot(persist_award_calendar_snapshot(metadata,ctx),saved$payload$version==2L)
# The Data API returns JSONB object keys ordered by length and then name.
jsonb_order<-function(x)x[order(nchar(names(x)),names(x))]
saved$payload$scope<-jsonb_order(saved$payload$scope)
saved$payload$rows<-lapply(seq_len(nrow(saved$payload$rows)),function(i)jsonb_order(as.list(saved$payload$rows[i,])))
get_sb_url<-function()'https://database.synthetic.invalid';get_sb_key<-function()'synthetic-test-key'
supabase_get<-function(...)data.frame(account_id=saved$account_id,championship_id=saved$championship_id,competition_id=saved$competition_id,season=saved$season,
 evidence_type=saved$evidence_type,source_id=saved$source_id,observed_at=saved$observed_at,payload=I(list(saved$payload)))
restored<-get_award_round_metadata(auth,'championship','league-id',current_round_id='r2')
stopifnot(reads==1L,restored$from_saved,award_fixture_calendar_valid(restored$fixture_rounds),
 identical(as.character(restored$fixture_rounds$begin_process),as.character(metadata$fixture_rounds$begin_process)))
stopifnot(award_fixture_calendar_valid(award_fixture_calendar_decode(list())))
cat('PASS versioned fixture metadata reuses the acknowledged database observation without another bulk request\n')
# Ownership extraction uses the same bulk metadata, including the delayed fixture.
original_catalog<-get_award_round_catalog
get_award_round_catalog<-function(...)list(league_id='league-id',current_round_id='r2',rounds=data.frame(round_id=c('r1','r2'),begin_process=''))
result<-get_rival_ownership_round_catalog(auth,'championship','a')
stopifnot(result$status=='ok',result$rows$begin_process[1]=='2026-02-01T18:00:00Z',
 length(result$rows$fixture_dates[[2]])==1L,reads==1L)
cat('PASS ownership calendar recovers historical first-match timing and future-fixture evidence from the saved bulk source\n')
# Corrupt optional persisted dates cause a source cache miss rather than an
# unsupported result that would strand otherwise retrievable ownership history.
saved$payload$fixture_rounds[[1]]$fixture_dates<-list('invalid-date')
recovered<-get_award_round_metadata(auth,'championship','league-id',current_round_id='r2')
stopifnot(reads==2L,recovered$status=='ok',award_fixture_calendar_valid(recovered$fixture_rounds),!isTRUE(recovered$from_saved))
cat('PASS malformed persisted fixture timestamps are reacquired from the authoritative bulk source\n')

get_award_round_catalog<-original_catalog;get_cached_data<-original_cache;mvp_api_answer<-original_answer;get_sb_url<-original_url;get_sb_key<-original_key;supabase_get<-original_get;defer_persistence<-original_defer

# First instalment of round 23. The compact catalog only knows that the
# processing marker jumped, and labels all other rounds as future.
ids<-paste0('official-',14:23)
compact<-list(league_id='source-league',current_round_id=ids[10],current_round_number=23,
 identity_complete=TRUE,rounds=data.frame(round_id=ids,round_number=14:23,
 is_finished=c(TRUE,rep(NA,9)),status='future',begin_process=''))
records<-lapply(seq_along(ids),function(i)list(`_id`=ids[i],number=i+13L,
 status=if(i==1L)'closed'else if(i==10L)'closed'else'future'))
records[[10]]$matches<-lapply(1:10,function(i)list(`_id`=paste0('game-',i),
 h=list(`_id`=paste0('h',i),name='Home'),a=list(`_id`=paste0('a',i),name='Away'),
 st=if(i<=2L)'F'else'P',info=list(date=if(i<=2L)'2026-12-02T19:00:00Z'else'2027-02-14T19:00:00Z')))
metadata<-list(status='ok',complete=TRUE,rows=data.frame(round_id=rev(ids),round_number=23:14,
 status=rev(c('closed',rep('future',8),'closed')),is_finished=rev(c(TRUE,rep(NA,8),TRUE))),
 fixture_rounds=award_fixture_calendar(records),fixture_matches=league_fixture_matches(records))
attr(metadata,'observed_at')<-fm_time('2026-12-03T09:00:00Z')
gap<-award_calendar_gaps(metadata$fixture_matches)[[ids[10]]]
stopifnot(gap$slow_from==as.numeric(fm_time('2026-12-03T19:00:00Z')),
 gap$resume_at==as.numeric(fm_time('2027-02-13T19:00:00Z')))
ordinary<-metadata$fixture_matches;ordinary$rows$kickoff[ordinary$rows$status!='F']<-'2026-12-04T19:00:00Z'
stopifnot(!length(award_calendar_gaps(ordinary)))
incomplete<-metadata$fixture_matches;incomplete$rounds$complete<-FALSE
stopifnot(!length(award_calendar_gaps(incomplete)))
stored_phase<-NULL
persist_rival_finance_evidence<-function(rows,context){stored_phase<<-rows[[1]];TRUE}
durable<-metadata
durable$rows<-durable$rows[,c('round_id','round_number','is_finished','status')]
durable$scope<-c(ctx[c('account_id','championship_id','competition_id','season')],list(league_id='league-id',current_round_id=ids[10]))
durable$valid_until<-as.numeric(attr(durable,'observed_at'))+60
stopifnot(persist_award_calendar_snapshot(durable,ctx),identical(stored_phase$payload$split_gaps[[ids[10]]],gap),
 length(stored_phase$payload$split_gaps)==1L)
get_finished_round_catalog<-function(...)compact
bulk_reads<-0L
authoritative_metadata<-get_award_round_metadata
get_award_round_metadata<-function(...){bulk_reads<<-bulk_reads+1L;metadata}
catalog<-get_classification_round_catalog(auth,'championship')
due<-award_coverage(catalog$rounds,data.frame(),current_round_number=23,current_round_id=ids[10])
stopifnot(bulk_reads==1L,catalog$complete,identical(catalog$rounds$partially_played,seq_along(ids)==10L),
 identical(due$round_id[due$required],ids[1]),all(catalog$rounds$is_finished[-1L]%in%FALSE),
 identical(catalog$rounds$first_kickoff[10],'2026-12-02T19:00:00Z'),
 identical(catalog$rounds$begin_process[10],catalog$rounds$first_kickoff[10]))
# Reopen the database-shaped fixture objects with their original source clock.
to_jsonb<-function(x)jsonlite::fromJSON(jsonlite::toJSON(x,auto_unbox=TRUE,na='null',null='null'),simplifyVector=FALSE)
reopened<-metadata
reopened$fixture_matches<-league_fixture_matches_decode(to_jsonb(metadata$fixture_matches))
reopened$fixture_rounds<-award_fixture_calendar_decode(to_jsonb(metadata$fixture_rounds))
stopifnot(identical(award_join_calendar(compact$rounds,reopened)$rounds,catalog$rounds),bulk_reads==1L)
# An early 'closed' phase stays open after the lineup marker moves back to 15.
compact$current_round_id<-ids[2];compact$current_round_number<-15
compact$rounds$is_finished[10]<-TRUE
compact$rounds$begin_process[10]<-'2027-02-14T19:00:00Z'
catalog<-get_award_round_catalog(auth,'championship')
stopifnot(bulk_reads==2L,catalog$rounds$partially_played[10],!catalog$rounds$is_finished[10],
 identical(catalog$rounds$first_kickoff[10],'2026-12-02T19:00:00Z'),
 identical(catalog$rounds$begin_process[10],compact$rounds$begin_process[10]))
# A fully numbered compact calendar can also move beyond the split round.
compact$current_round_id<-'r24';compact$current_round_number<-24
compact$rounds$is_finished[2:9]<-FALSE
past_split<-get_award_round_catalog(auth,'championship')
stopifnot(bulk_reads==3L,past_split$rounds$partially_played[10],!past_split$rounds$is_finished[10])
# A legacy saved calendar may have dates but no individual game statuses.
games<-metadata$fixture_matches;metadata$fixture_matches<-NULL
stopifnot(award_calendar_round_states(metadata)$partially_played[1])
metadata$fixture_matches<-games
metadata$fixture_matches$rows$status<-'F'
attr(metadata,'observed_at')<-fm_time('2027-02-15T09:00:00Z')
final<-get_award_round_catalog(auth,'championship')
stopifnot(final$rounds$is_finished[10],!final$rounds$partially_played[10],
 identical(final$rounds$round_id[final$rounds$is_finished%in%TRUE],ids[c(1,10)]))
cat('PASS one joined bulk calendar distinguishes the two-game phase of round 23, eight unplayed round numbers and the final ten-game closure\n')

# A compact closure after J24 and a failed bulk refresh cannot turn a previously
# observed partial J23 into a final XI. The failed result must allow a fresh retry.
get_award_round_metadata<-authoritative_metadata
compact$league_id<-'league-id'
old_phase<-stored_phase
stamp<-Sys.time()-2*resource_policy('award_round_metadata_championship')$ttl
old_phase$observed_at<-rival_finance_stamp(stamp)
old_phase$payload$valid_until<-as.numeric(stamp)+60
get_sb_url<-function()'https://database.synthetic.invalid';get_sb_key<-function()'synthetic-test-key'
source_count<-0L;db_count<-0L
supabase_get<-function(table,query){
  stopifnot(table=='rival_finance_evidence_latest',query$limit==1L)
  db_count<<-db_count+1L
  data.frame(account_id=old_phase$account_id,championship_id=old_phase$championship_id,competition_id=old_phase$competition_id,
    season=old_phase$season,evidence_type=old_phase$evidence_type,source_id=old_phase$source_id,
    observed_at=old_phase$observed_at,payload=I(list(old_phase$payload)))
}
mvp_api_answer<-function(login,url,query){source_count<<-source_count+1L;mvp_result('connection',reason='Calendar connection failed.')}
clear_api_cache()
failed<-get_award_round_catalog(auth,'championship')
xi<-data.frame(round_id=ids[10],player_id=paste0('p',1:11),is_mvp=seq_len(11)==1,is_finished=TRUE)
stopifnot(source_count==1L,db_count==1L,!failed$complete,failed$rounds$partially_played[10],!failed$rounds$is_finished[10],
  identical(failed$reason,'Calendar connection failed.'),!any(award_calendar_selections(xi,failed$rounds)$is_finished),
  as.numeric(attr(failed,'observed_at'))==as.numeric(fm_time(old_phase$observed_at)),
  attr(failed,'valid_until')<=as.numeric(Sys.time()),all(failed$rounds$partially_played[-10]%in%FALSE))
recorded_url<-get_sb_url;get_sb_url<-function()''
again<-get_award_round_catalog(auth,'championship')
stopifnot(source_count==2L,db_count==1L,again$rounds$partially_played[10],
  as.numeric(attr(again,'observed_at'))==as.numeric(fm_time(old_phase$observed_at)))
get_sb_url<-recorded_url
mvp_api_answer<-function(login,url,query){
  source_count<<-source_count+1L
  for(i in seq_along(records[[10]]$matches))records[[10]]$matches[[i]]$st<-'F'
  list(status='ok',answer=list(rounds=records))
}
recovered<-get_award_round_catalog(auth,'championship')
stopifnot(source_count==3L,db_count==2L,recovered$complete,!recovered$rounds$partially_played[10],recovered$rounds$is_finished[10])
cat('PASS one failed bulk refresh retains the expired positive J23 phase under its original source clock until the next successful all-final acquisition\n')
