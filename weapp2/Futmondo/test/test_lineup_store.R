#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
ctx<-resource_context(c(userid='account',token='private-fixture'),'league','team',season='S1',scoring_version='score')
snapshot<-list(version=1L,roster=list(list(id='p1',name='Player',role='portero',role2='')),official=lineup_empty(),
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
cat('PASS source arrays normalize on hydration, UTC representations compare equally, known context filters and reads do not write\n')
row$account_id<-'other';settle(lineup_snapshot_store(ctx,'r7'));stopifnot(!is.null(failure),is.null(value));row$account_id<-'account'
row$scoring_version<-'other';settle(lineup_snapshot_store(ctx,'r7'));stopifnot(!is.null(failure));row$scoring_version<-'score'
cat('PASS mismatched account or known scoring scope cannot hydrate\n')
settle(lineup_snapshot_store(ctx,'r7',snapshot))
stopifnot(isTRUE(value),length(writes)==1L,!grepl('private-fixture|private-database-fixture|token',writes[[1]]$body),
 grepl('"complete":true',writes[[1]]$body,fixed=TRUE))
cat('PASS normalized successful snapshot writes exclude authentication material\n')
row$payload$rules$bench_enabled<-'unknown';settle(lineup_snapshot_store(ctx,'r7'));stopifnot(!is.null(failure))
cat('PASS malformed cached capability state is rejected\nLINEUP STORE: 4 groups passed / 0 failed\n')
