#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
ctx<-resource_context(c(userid='account',token='private-fixture'),'league','team',season='S1',scoring_version='score')
players<-list(list(id='p1',name='Player',points=0,role='portero',team=list(name='Real Madrid',logo='source-club.png')),
 list(id='p2',name='Other player',points=-5,role='defensa',team='Barcelona',logo='other-club.png'))
parsed_roster<-lineup_roster_records(players)
stopifnot(parsed_roster[[1]]$logo=='source-club.png',parsed_roster[[1]]$club=='Real Madrid',
 parsed_roster[[1]]$points==0,parsed_roster[[2]]$points== -5,parsed_roster[[2]]$logo=='other-club.png',parsed_roster[[2]]$club=='Barcelona')
snapshot<-list(version=1L,roster=parsed_roster,official=lineup_empty(),
 rules=list(allowed='4-4-2',multiposition=FALSE,bench_enabled=TRUE,captain_enabled=FALSE),
 source=list(wire=list(strategy='4-4-2',players=list()),locked=list()),season='S1',scoring_version='score',
 observed_at='2026-11-07T00:00:00Z',expires_at='2026-11-07T00:00:30Z')
row<-list(account_id='account',championship_id='league',user_team_id='team',target_round='r7',season='S1',scoring_version='score',
 payload=snapshot,observed_at='2026-11-07T00:00:00+00:00',expires_at='2026-11-07T00:00:30+00:00',complete=TRUE)
get_sb_url<-function()'https://fixture.invalid';get_sb_key<-function()'private-database-fixture'
requests<-list();writes<-list();failure<-NULL;value<-NULL
fm_async_read_http<-function(url,...){requests[[length(requests)+1L]]<<-url;promises::promise_resolve(list(content=charToRaw(jsonlite::toJSON(list(row),auto_unbox=TRUE,null='null'))))}
fm_async_http<-function(url,method,body,headers,...){writes[[length(writes)+1L]]<<-list(url=url,body=body);promises::promise_resolve(list(status_code=201L))}
settle<-function(p){failure<<-NULL;value<<-NULL;promises::then(p,function(x){value<<-x;NULL},function(e){failure<<-e;NULL});for(i in 1:20)later::run_now(0)}
settle(lineup_snapshot_store(ctx,'r7'))
stopifnot(is.null(failure),is.character(value$official$starters),length(value$official$starters)==11L,
 !length(writes),grepl('season=eq.S1',requests[[1]],fixed=TRUE),grepl('scoring_version=eq.score',requests[[1]],fixed=TRUE))
stopifnot(lineup_roster_frame(value)$logo[1]=='source-club.png',lineup_roster_frame(value)$club[1]=='Real Madrid')
cat('PASS source crests survive normalized roster parsing and persisted JSON hydration\n')
cat('PASS source arrays normalize on hydration, UTC representations compare equally, known context filters and reads do not write\n')
row$account_id<-'other';settle(lineup_snapshot_store(ctx,'r7'));stopifnot(!is.null(failure),is.null(value));row$account_id<-'account'
row$scoring_version<-'other';settle(lineup_snapshot_store(ctx,'r7'));stopifnot(!is.null(failure));row$scoring_version<-'score'
cat('PASS mismatched account or known scoring scope cannot hydrate\n')
settle(lineup_snapshot_store(ctx,'r7',snapshot))
stopifnot(isTRUE(value),length(writes)==1L,!grepl('private-fixture|private-database-fixture|token',writes[[1]]$body),
 grepl('"complete":true',writes[[1]]$body,fixed=TRUE))
stored<-jsonlite::fromJSON(writes[[1]]$body,simplifyVector=FALSE)
stopifnot(stored$payload$roster[[1]]$logo=='source-club.png',stored$payload$roster[[1]]$points==0,
 stored$payload$roster[[2]]$points== -5)
legacy<-snapshot;legacy$roster<-lapply(legacy$roster,function(player){player$logo<-NULL;player$points<-NULL;player})
stopifnot(all(lineup_roster_frame(legacy)$logo==''),nrow(lineup_roster_frame(NULL))==0L,all(is.na(lineup_roster_frame(legacy)$points)),
 !lineup_snapshot_fresh(legacy,now=fm_time(legacy$observed_at)))
cat('PASS normalized successful snapshot writes exclude authentication material; logos persist and older snapshots remain readable\n')
row$payload$rules$bench_enabled<-'unknown';settle(lineup_snapshot_store(ctx,'r7'));stopifnot(!is.null(failure))
cat('PASS malformed cached capability state is rejected\nLINEUP STORE: 4 groups passed / 0 failed\n')
