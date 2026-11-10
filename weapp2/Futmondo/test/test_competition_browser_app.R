# Two-competition, authenticated synthetic app. HTTP is blocked by the base fixture.
expressions<-parse('test/test_application_offline.R')
for(expr in expressions){
 if(is.call(expr)&&identical(expr[[1]],as.name('app_check'))&&identical(expr[[2]],'full server login, navigation, refresh and logout'))break
 eval(expr,envir=.GlobalEnv)
}
baseline_card<-Sys.getenv('FUTMONDO_COMPETITION_BASELINE_CARD','')
if(nzchar(baseline_card))source(baseline_card,local=.GlobalEnv)
fixture_reads<-new.env(parent=emptyenv())
fixture_delay<-function(value,seconds)promises::promise(function(resolve,reject)later::later(function()resolve(value),seconds))
fixture_active<-list(championships=list(
 list(id='game-domestic',name='World Friends',league='competition-domestic',ps='press',sport='football',country='GB',userteam=list(id='team1',name='Your Domestic Team')),
 list(id='game-european',name='World Friends',league='competition-european',ps='press',sport='football',country='EU',userteam=list(id='team-european',name='Your European Team'))),
 leagues=list(list(`_id`='competition-domestic',name='Domestic Football',season='2026-2027'),
  list(`_id`='competition-european',name='European Football',season='2026-2027')))
fixture_meta<-function(championship_id){
 row<-if(identical(championship_id,'game-european'))2L else 1L
 list(competition=fixture_active$leagues[[row]]$`_id`,team=fixture_active$championships[[row]]$userteam$id,
  club=if(row==1L)'Domestic Club'else'European Club',name=if(row==1L)'Domestic Player'else'European Player')
}
fixture_players<-function(championship_id,market=FALSE){
 metadata<-fixture_meta(championship_id)
 if(market){d<-app_market;if(championship_id=='game-european')d<-d[1,,drop=FALSE]}else d<-app_roster
 d$name<-paste(metadata$name,seq_len(nrow(d)));d$team<-metadata$club;d$teamId<-paste0(metadata$competition,'-club')
 if(!market){d$userteamId<-metadata$team;d$user_team_id<-metadata$team;d$userTeam<-metadata$team}
 d$observed_at<-format(Sys.time(),'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC')
 d
}
get_active_championships<-function(login){
 value<-observed_data(fixture_active,observed_at=Sys.time());publish_active_league_contexts(login,value);value
}
get_championships<-function(login,...)unlist(fixture_active$championships[[1]])
get_players_from_team<-function(login,championship_id,user_team_id,...){
 d<-fixture_players(championship_id)
 if(championship_id=='game-european')d<-d[FALSE,,drop=FALSE]
 d
}
get_market_players<-function(login,championship_id,user_team_id,...)fixture_players(championship_id,TRUE)
get_championship_players<-function(login,championship_id,...){
 roster<-get_players_from_team(login,championship_id,fixture_meta(championship_id)$team)
 dplyr::bind_rows(roster,fixture_players(championship_id,TRUE))
}
get_teams<-function(login,championship_id,...){
 if(championship_id=='game-european')data.frame(teamid='team-european',id='team-european',teamname='Your European Team',name='Your European Team',points=0,position=1,teamValue=0,is_active=TRUE)else app_teams
}
get_user_team_info<-function(login,championship_id,user_team_id,...)list(budget=if(championship_id=='game-european')30000000 else 0,
 withheld=0,maxBid=10000000,teamValue=if(championship_id=='game-european')0 else 34000000,configuration=app_cfg,points=0)
get_financial_snapshot<-function(login,championship_id,user_team_id,...)app_financial
get_lineup_from_team<-function(login,championship_id,user_team_id,...)list(pointSystem='press',custom=NULL,players=data.frame(),bench=list(players=data.frame()),lineup_config=list(),budget=0)
get_finished_rounds<-function(login,championship_id,...){
 if(championship_id=='game-european')data.frame(round_id='european-round-1',round_number=1,begin_process=format(Sys.time()-3600,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),is_finished=FALSE)
 else transform(app_rounds,round_id=paste0('domestic-round-',round_number))
}
fixture_round_rows<-function(championship_id,player_ids){
 metadata<-fixture_meta(championship_id);rounds<-if(championship_id=='game-european')1 else 1:2
 if(!length(player_ids))return(data.frame())
 rows<-expand.grid(player_id=player_ids,round=rounds,stringsAsFactors=FALSE)
 rows$championship_id<-championship_id;rows$competition_id<-metadata$competition;rows$season<-'2026-2027'
 rows$scoring_version<-fm_scoring_version('press');rows$points<-if(championship_id=='game-european')17 else ifelse(rows$round==1,0,-2)
 rows$score_status<-'unknown';rows$observed_at<-format(Sys.time()-60,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC')
 rows$occurred_at<-NA_character_;rows$round_start_at<-NA_character_;rows$round_id<-paste0(if(championship_id=='game-european')'european'else'domestic','-round-',rows$round);rows$match_id<-''
 rows
}
read_league_round_dataset<-function(championship_id,context,...){
 stopifnot(context$competition_id==fixture_meta(championship_id)$competition,context$season=='2026-2027',context$scoring_version==fm_scoring_version('press'))
 players<-get_championship_players(app_auth,championship_id)
 rows<-fixture_round_rows(championship_id,players$id)
 checks<-data.frame(championship_id=championship_id,competition_id=context$competition_id,player_id=players$id,season=context$season,scoring_version=context$scoring_version,
  checked_at=format(Sys.time()-30,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),result_status='scores',point_count=if(championship_id=='game-european')1L else 2L)
 list(points=rows,checks=checks,complete=TRUE)
}
read_card_round_history<-function(context,...)structure(fixture_round_rows(context$championship_id,context$player_id),complete=TRUE)
read_card_price_history<-function(...)structure(data.frame(),complete=TRUE)
get_player_summary<-function(login,championship_id,user_team_id,player_id,...){
 player<-fixture_players(championship_id,TRUE)[1,,drop=FALSE];player$id<-player_id
 points<-if(championship_id=='game-european')list(list(round=1,points=17,roundId='european-round-1'))else
  list(list(round=1,points=0,roundId='domestic-round-1'),list(round=2,points=-2,roundId='domestic-round-2'))
 observed_data(list(data=as.list(player),championship=list(season='2026-2027',competitionId=fixture_meta(championship_id)$competition,owner=list()),
  points=points,points_received=TRUE,prices=list(),owners=list(),bids=list(),market=list()),observed_at=Sys.time())
}
# A player-only endpoint may now describe another real competition after a move.
# This domestic metadata must never attach to the European league's round 1.
get_player_matches<-function(login,player_id,...)list(player=list(id=player_id),league=list(`_id`='competition-domestic',season='2026-2027'),matches=list(
 list(r=1,id='domestic-match-1',h=list(name='Domestic Home',score=0),a=list(name='Domestic Away',score=0),info=list(date='2026-08-15')),
 list(r=2,id='domestic-match-2',h=list(name='Domestic Home Two',score=2),a=list(name='Domestic Away Two',score=1),info=list(date='2026-08-22'))))
get_player_full_profile<-function(login,player_id,...)list(player=list(id=player_id),league=list(`_id`='competition-domestic',season='2026-2027'),profile=list(nationality='Synthetic',height=180,weight=75),prices=list())
get_team_acquisition_capacity<-function(...)list(status='ok',roster=list(count=0,cap=20,status='ok'),outstanding=list(count=0),
 funds=list(spendable_budget=10000000),rules=list(minimum_bid_at_value=TRUE),valid_until=as.numeric(Sys.time())+300)
fm_async_login<-function(...)fixture_delay(app_auth,.2)
fixture_original_async<-async_adapter_fetch
fixture_profile_started<-FALSE
async_adapter_fetch<-function(fn,args=list(),context,owner=context$session_id,is_current=function()TRUE,...){
 name<-if(is.character(fn))fn else deparse(substitute(fn))
 if(name=='get_player_summary'&&identical(Sys.getenv('FUTMONDO_COMPETITION_PROFILE'),'true')&&!fixture_profile_started){
  fixture_profile_started<<-TRUE;Rprof('/tmp/futmondo-competition-card.prof',interval=.005);later::later(function()Rprof(NULL),1.5)
 }
 fixture_reads[[name]]<-(fixture_reads[[name]] %||% 0L)+1L
 delay<-switch(name,get_active_championships=.2,get_player_summary=2,get_player_matches=2.8,get_player_full_profile=2.5,
  read_card_round_history=2.5,read_card_price_history=2.5,get_market_players=.8,get_players_from_team=.8,.05)
 promises::then(fixture_delay(NULL,delay),function(unused)fixture_original_async(name,args,context,owner,is_current,...))
}
browser_ui<-source('ui.R',local=.GlobalEnv)$value
browser_ui<-shiny::tagList(browser_ui,shiny::div(style='display:none',shiny::textOutput('fixture_heartbeat')))
browser_server<-function(input,output,session){
 clear_api_cache();app_server(input,output,session)
 output$fixture_heartbeat<-shiny::renderText(input$fixture_heartbeat)
 shiny::outputOptions(output,'fixture_heartbeat',suspendWhenHidden=FALSE)
 shiny::observeEvent(input$fixture_snapshot,session$sendCustomMessage('competition-fixture-stats',list(reads=as.list(fixture_reads),http=offline_http_attempts)))
}
shiny::runApp(shiny::shinyApp(browser_ui,browser_server),host='127.0.0.1',port=as.integer(Sys.getenv('FUTMONDO_COMPETITION_TEST_PORT','38862')),launch.browser=FALSE)
