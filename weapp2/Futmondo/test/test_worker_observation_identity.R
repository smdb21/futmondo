# Historical database identity survives freshness expiry without authorizing a
# live action or renewing the original source/scoring deadlines.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
now<-base::Sys.time();Sys.time<-function()now
auth<-c(userid='identity-fixture',token='synthetic')
s<-fm_worker_account_state(auth[['userid']]);s$known<-TRUE
s$account<-list(enabled=TRUE,auth_status='active',generation=1L)
active<-list(championships=list(list(id='game',league='competition',ps='stats')),
 leagues=list(list(`_id`='competition',season='source-season')))
attr(active,'observed_at')<-now
publish_active_league_contexts(auth,active)
before<-resource_context(auth,'game','team')
key<-fm_league_context_key(auth[['userid']],'game')
original<-.fm_league_contexts$items[[key]]
now<-now+301
stopifnot(is.null(league_context_metadata('game',auth[['userid']])),
 identical(resource_context(auth,'game','team'),before),
 identical(.fm_league_contexts$items[[key]],original))
cat('PASS stored identity survives expiry; fresh lookup rejects it and all source clocks stay unchanged\n')

compact<-active;compact$championships[[1]]$ps<-NULL
attr(compact,'observed_at')<-now
publish_active_league_contexts(auth,compact)
stopifnot(league_context_metadata('game',auth[['userid']])$scoring_version=='unknown',
 identical(resource_context(auth,'game','team'),before),
 identical(.fm_league_contexts$items[[key]]$payload$scoring_expires_at,original$payload$scoring_expires_at))
cat('PASS compact membership keeps historical scoring identity without renewing its expired evidence\n')

explicit<-resource_context(auth,'game','team',season='unknown',scoring_version='unknown',competition_id='unknown')
stopifnot(explicit$season=='unknown',explicit$scoring_version=='unknown',explicit$competition_id=='unknown',
 resource_context(account_id='different',championship_id='game')$season=='unknown',
 resource_context(auth,'different-game')$season=='unknown')
s$account$enabled<-FALSE
stopifnot(resource_context(auth,'game','team')$scoring_version=='unknown')
s$account$enabled<-TRUE
options(futmondo.worker_live=TRUE)
stopifnot(resource_context(auth,'game','team')$scoring_version=='unknown')
options(futmondo.worker_live=FALSE)
cat('PASS explicit unknown, different accounts/leagues, disabled worker and live checks keep strict scopes\n')

changed<-active;changed$leagues[[1]]$season<-'next-source-season'
now<-now+1;attr(changed,'observed_at')<-now
publish_active_league_contexts(auth,changed)
stopifnot(resource_context(auth,'game','team')$season=='next-source-season',
 !identical(resource_context(auth,'game','team'),before))
clear_league_contexts(auth[['userid']])
stopifnot(resource_context(auth,'game','team')$season=='unknown')
cat('PASS a newly observed season or explicit clear replaces the old identity immediately\n')
