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
get_sb_url<-function()'https://database.synthetic.invalid';get_sb_key<-function()'synthetic-test-key'
supabase_get<-function(...)data.frame(account_id=saved$account_id,championship_id=saved$championship_id,season=saved$season,
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
