options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
for(expr in parse('worker/helper.R'))if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
 as.character(expr[[2]])=='fm_worker_discover')eval(expr,.GlobalEnv)
auth<-c(userid='discovery-fixture',token='synthetic')
ctx<-resource_context(auth,'league','mine',competition_id='competition',season='season',scoring_version='score')
champ<-list(id='league',userteam=list(id='mine'))
active<-list(championships=list(champ))
get_active_championships<-function(login)active
get_classification_round_catalog<-function(login,championship_id)list(user_team_id='mine',current_round_id='live',
 rounds=data.frame(round_id=c('closed','live','future'),round_number=1:3,is_finished=c(TRUE,FALSE,FALSE)))
roster<-observed_data(data.frame(id=c('outgoing','stay')),observed_at=Sys.time())
history<-observed_data(data.frame(player_id=c('incoming','outgoing'),buyer_team_id=c('mine','other'),seller_team_id=c('other','mine'),created=c('2026-11-10T00:00:00Z','2026-11-11T00:00:00Z')),observed_at=Sys.time())
get_players_from_team<-function(login,championship_id,user_team_id)roster
get_championship_pressroom<-function(login,championship_id)history
source_calls<-0L
fm_worker_saved_resource<-function(handler,args,context)switch(handler,
 get_active_championships=active,get_classification_round_catalog=get_classification_round_catalog(auth,'league'),
 get_players_from_team=roster,get_championship_pressroom=history,NULL)
# Seed metadata exactly as an authoritative active-league acquisition does.
remember_league_context(list(account_id=ctx$account_id,championship_id=ctx$championship_id,
 competition_id=ctx$competition_id,season=ctx$season,scoring_version=ctx$scoring_version,
 observed_at=rival_finance_stamp(),expires_at=rival_finance_stamp(Sys.time()+300),complete=TRUE,payload=list()))
handlers<-function(plan)vapply(plan,function(d)d$handler,character(1))
plan<-fm_worker_discover('get_active_championships',active,resource_context(auth),auth)
stopifnot(all(c('get_resolved_league_context','get_notifications','get_notification_unread','get_teams',
 'get_market_players','get_players_from_team','get_user_team_moneymovements','fm_worker_finances')%in%handlers(plan)),
 !anyDuplicated(vapply(plan,function(d)d$resource_key,character(1))),
 !grepl('synthetic',fm_worker_json(plan),fixed=TRUE))
resolved<-fm_worker_discover('get_resolved_league_context',ctx,ctx,auth)
stopifnot(all(vapply(resolved,function(d)identical(d$context$season,'season')&&identical(d$context$scoring_version,'score'),logical(1))))
cat('PASS account enrollment and resolved context create core schedules without browser demand or credentials\n')
league_ctx<-ctx;league_ctx$user_team_id<-''
plan<-fm_worker_discover('get_teams',data.frame(teamid=c('mine','rival')),league_ctx,auth)
stopifnot(length(plan)==8L,all(c('mine','rival')%in%vapply(plan,function(d)d$args$user_team_id,character(1))))
catalog<-get_classification_round_catalog(auth,'league')
plan<-fm_worker_discover('get_classification_round_catalog',catalog,league_ctx,auth)
stopifnot(length(plan)==5L,!grepl('future',fm_worker_json(plan),fixed=TRUE),
 all(c('get_classification_round_ranking','get_round_dreamteam','get_official_dreamteam_result')%in%handlers(plan)))
rival<-ctx;rival$user_team_id<-'rival'
plan<-fm_worker_discover('get_user_team_rounds',list(),rival,auth)
stopifnot(length(plan)==2L,all(handlers(plan)=='get_round_lineup_snapshot'),
 all(vapply(plan,function(d)d$args$user_team_id=='rival',logical(1))),
 sort(vapply(plan,function(d)d$interval_seconds,integer(1)))==c(300L,86400L))
cat('PASS rival data, historical lineups and award/ranking views are discovered from authoritative team/round identities; future rounds excluded\n')
players<-data.frame(id=c('p1','p2','p2'))
plan<-fm_worker_discover('get_championship_players',players,league_ctx,auth)
stopifnot(length(plan)==6L,all(vapply(plan,function(d)if(d$handler=='get_player_summary')d$args$user_team_id=='mine'else TRUE,logical(1))),
 !any(handlers(plan)%in%c('get_player_recent_points','get_clause_transfer_summary','get_player_card_history')))
cat('PASS catalog resolves the account team and creates one source summary per player, with no independent derived-view acquisition jobs\n')
plan<-fm_worker_discover('get_championship_pressroom',history,league_ctx,auth)
stopifnot(length(plan)==2L,all(handlers(plan)=='get_player_summary'),
 setequal(vapply(plan,function(d)d$args$player_id,character(1)),c('incoming','outgoing')),
 all(vapply(plan,function(d)d$args$user_team_id=='mine'&&d$interval_seconds==300L,logical(1))))
plan<-fm_worker_discover('get_players_from_team',roster,ctx,auth)
stopifnot(length(plan)==3L,!anyDuplicated(vapply(plan,function(d)d$resource_key,character(1))))
stopifnot(!length(fm_worker_discover('get_players_from_team',roster,rival,auth)))
cat('PASS roster/history discover incoming and outgoing candidates and do not create redundant rival-team summary scopes\n')

for(handler in c('get_active_championships','get_notifications','get_notification_unread')){
 a<-fm_worker_descriptor(handler,list(login=auth),ctx)
 b<-fm_worker_descriptor(handler,list(),resource_context(auth))
 stopifnot(identical(a,b))
 foreign<-ctx;foreign$account_id<-'another-account'
 stopifnot(!identical(a$resource_key,fm_worker_descriptor(handler,list(),foreign)$resource_key))
}
cat('PASS account-only source identities ignore page league context while preserving account isolation\n')

# Missing bootstrap inputs do not start extra source requests or invent jobs.
fm_worker_saved_resource<-function(...)NULL
stopifnot(!length(fm_worker_discover('get_championship_pressroom',history,league_ctx,auth)),
 !length(fm_worker_discover('get_user_team_rounds',list(),rival,auth)))
cat('PASS missing discovery dependencies wait for autonomous schedules without starting another source query\n')

ref<-list(round_id='closed',round_number=1,is_finished=TRUE)
plain<-fm_worker_descriptor('get_official_dreamteam_result',list(championship_id='league',reference=ref),ctx)
hinted<-fm_worker_descriptor('get_official_dreamteam_result',list(championship_id='league',reference=c(ref,list(user_team_id='mine',current_round_number=2,current_round_id='live'))),ctx)
stopifnot(identical(plain,hinted))
cat('PASS completed award queries ignore GUI-only selection hints while retaining authoritative round identity and finality\n')
