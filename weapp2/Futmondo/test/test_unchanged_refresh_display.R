# Exercise the real header projections: freshness and refresh are not new amounts.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
now<-base::Sys.time();Sys.time<-function()now
fixture_auth<-c(userid='stable-display-fixture',token='synthetic')
state<-function(value,phase='fresh',at=now){
 data<-observed_data(value,observed_at=at,status=if(phase=='fresh')'ok'else'stale')
 attr(data,'valid_until')<-as.numeric(at)+60
 resource_state(data,status=switch(phase,fresh='ok',stale='stale',loading='loading',failed='stale'),
  error=if(phase=='failed')resource_failure(category='network')else NULL)
}
settle<-function(session)for(i in 1:12){later::run_now(0);session$flushReact()}
team_value<-list(budget=1000,withheld=0,teamValue=10000,configuration=list(mnmp=.5))
market_value<-data.frame(bid_id='bid',bid_price=100)
root_exprs<-as.list(parse('server.R')[[1]][[3]])[-1L]
shiny::testServer(function(input,output,session){
 login_token_RV<-function()fixture_auth
 team<-shiny::reactiveVal(state(team_value));market<-shiny::reactiveVal(state(market_value))
 primary_team_state<-function()team();primary_market_state<-function()market()
 startup_primary_ready<-function()TRUE
 schedule_fault<-shiny::reactiveVal(FALSE)
 round_catalog_RV<-function(){if(schedule_fault())stop("Synthetic schedule failure");data.frame(round_number=14,begin_process=format(now+86400,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),is_finished=FALSE)}
 for(expr in root_exprs)if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
  paste(deparse(expr[[2]]),collapse='')%in%c('account_finance_visible','output$account_finance_summary','output$round_finance_amounts','output$round_clause_pending','round_finance_RV','round_finance_card_RV','round_finance_status_display',
   'output$round_finance_status','round_clause_pending_display','round_finance_checked','output$round_finance_checked',
   'round_heading_state','output$round_countdown'))eval(expr,envir=environment())
},{
 session$setInputs(tabs='today');settle(session)
 original<-output$round_finance_amounts;clock<-output$round_finance_checked
 shell<-output$account_finance_summary
 schedule_fault(TRUE);settle(session)
 stopifnot(inherits(tryCatch(output$round_countdown,error=identity),"error"),
  identical(shell,output$account_finance_summary),identical(original,output$round_finance_amounts))
 schedule_fault(FALSE);settle(session)
 cat('PASS an independently failed schedule cannot hide the summary shell or known finances\n')
 elapsed<-system.time(for(i in 1:10){
  now<<-now+60
  for(phase in c('stale','loading','fresh')){
   team(state(team_value,phase,now));market(state(market_value,phase,now));settle(session)
   stopifnot(identical(original,output$round_finance_amounts))
  }
 })[['elapsed']]
 stopifnot(output$round_finance_checked!=clock,team()$observed_at==now)
 cat(sprintf('PASS ten stale/loading/fresh cycles preserve the exact balance card while original clocks advance (%.3fs)\n',elapsed))
 team(state(team_value,'failed'));settle(session)
 stopifnot(identical(original,output$round_finance_amounts),grepl('could not load',output$round_finance_status$html,fixed=TRUE),
  grepl('fm-info-error',output$round_finance_status$html,fixed=TRUE),!is.null(team()$error))
 cat('PASS genuine read failures update the independent status without repainting known money or concealing the error\n')
 changed<-team_value;changed$budget<-2000;team(state(changed));settle(session)
 stopifnot(!identical(original,output$round_finance_amounts),grepl('Balance: 2.000 EUR',output$round_finance_amounts$html,fixed=TRUE))
 changed$budget<--10;team(state(changed));settle(session)
 stopifnot(grepl('round-countdown-warning',output$round_finance_amounts$html,fixed=TRUE),grepl('Restore at least',output$round_finance_status$html,fixed=TRUE))
 team(state(changed,'stale'));settle(session)
 stopifnot(grepl('round-countdown-warning',output$round_finance_amounts$html,fixed=TRUE),grepl('out of date',output$round_finance_status$html,fixed=TRUE),
  round_finance_RV()$class=='round-countdown-unavailable')
 cat('PASS changed observed money updates immediately; negative observed amounts remain marked while stale action validity stays unavailable\n')
 team(resource_state(status='loading'));market(resource_state(status='loading'));settle(session)
 stopifnot(grepl('Balance: Loading',output$round_finance_amounts$html,fixed=TRUE),!grepl('2.000 EUR',output$round_finance_amounts$html,fixed=TRUE))
 cat('PASS dropping source data clears old amounts instead of treating unknown values as zero\n')
})
# Scheduled-job polls can change metadata without changing any visible reserve.
original_clause_enabled<-fm_clause_enabled;original_clause_jobs<-fm_clause_jobs
fm_clause_enabled<-function()TRUE
jobs<-shiny::reactiveVal(list(list(id='job',kind='scheduled',state='scheduled',reserved_slot=1,
 reserved_amount=100,player_name='Player',opens_at=format(now+86400,'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),result_code='')))
fm_clause_jobs<-function(...)jobs()
shiny::testServer(function(input,output,session){
 login_token_RV<-function()fixture_auth;championship_id_RV<-function()'league';user_team_id_RV<-function()'mine'
 value<-team_value;value$maxBid<-6000
 primary_team_state<-function()state(value);primary_market_state<-function()state(market_value)
 startup_primary_ready<-function()TRUE;round_catalog_RV<-function()data.frame()
 for(expr in root_exprs)if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
  paste(deparse(expr[[2]]),collapse='')%in%c('account_finance_visible','output$account_finance_summary','output$round_finance_amounts','output$round_clause_pending','round_finance_RV','round_finance_card_RV','round_finance_status_display',
   'output$round_finance_status','round_clause_pending_display','round_finance_checked','output$round_finance_checked',
   'round_heading_state','output$round_countdown'))eval(expr,envir=environment())
},{
 session$setInputs(tabs='today');settle(session);original<-output$round_finance_amounts
 for(i in 1:10){next_jobs<-jobs();next_jobs[[1]]$updated_at<-now+i;jobs(next_jobs);settle(session)
  stopifnot(identical(original,output$round_finance_amounts))}
 next_jobs<-jobs();next_jobs[[1]]$reserved_amount<-200;jobs(next_jobs);settle(session)
 stopifnot(!identical(original,output$round_finance_amounts),grepl('Scheduled clause purchases: 200 EUR',output$round_clause_pending$html,fixed=TRUE))
 cat('PASS ten scheduled-job metadata polls preserve the header; a changed reserved amount updates the shared financial ledger/display\n')
})
fm_clause_enabled<-original_clause_enabled;fm_clause_jobs<-original_clause_jobs
# The new squad projection shares source observations, without clock-only redraws.
shiny::testServer(function(input,output,session){
 login_token_RV<-function()fixture_auth;championship_id_RV<-function()'league';user_team_id_RV<-function()'mine'
 roster<-shiny::reactiveVal(state(data.frame(id=paste0('p',1:18))))
 info<-shiny::reactiveVal(state(list(configuration=list(maxPlayersInRoster=20))))
 transfers<-shiny::reactiveVal(list(status='ok',outgoing_status='ok',records=data.frame(player_id=c('bought1','bought2')),
  outgoing_records=data.frame(player_id='p1')))
 roster_source_readers<-list(roster=function(...)roster())
 primary_team_state<-function()info();clause_transfers_RV<-function()transfers()
 round_finance_card_RV<-function()list(active_offer_count=1L)
 for(expr in root_exprs)if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&
  paste(deparse(expr[[2]]),collapse='')%in%c('account_finance_visible','output$account_finance_summary',
   'account_squad_RV','account_squad_card_RV','output$account_squad_checked','output$account_squad'))eval(expr,envir=environment())
},{
 session$setInputs(tabs='today');settle(session)
 original<-output$account_squad;clock<-output$account_squad_checked;shell<-output$account_finance_summary
 stopifnot(grepl('Players: 18',original$html,fixed=TRUE),grepl('Effective players: 19 / 20',original$html,fixed=TRUE),
  grepl('No more offers allowed.',original$html,fixed=TRUE))
 elapsed<-system.time(for(i in 1:10){
  now<<-now+60;roster(state(roster()$data,at=now));info(state(info()$data,at=now));settle(session)
  stopifnot(identical(original,output$account_squad),identical(shell,output$account_finance_summary))
 })[['elapsed']]
 stopifnot(!identical(clock,output$account_squad_checked))
 cat(sprintf('PASS ten squad source-clock updates preserve counts/full warning and shell while clocks advance (%.3fs)\n',elapsed))
 roster(resource_state(status='loading'));transfers(NULL);settle(session)
 stopifnot(grepl('Players: Loading',output$account_squad$html,fixed=TRUE),
  !grepl('No more offers allowed',output$account_squad$html,fixed=TRUE),identical(shell,output$account_finance_summary))
 roster(resource_state(status='unavailable'));transfers(list(status='partial'));settle(session)
 stopifnot(grepl('Players: Unavailable',output$account_squad$html,fixed=TRUE),
  !grepl('No more offers allowed',output$account_squad$html,fixed=TRUE))
 session$setInputs(tabs='login');settle(session);stopifnot(is.null(output$account_finance_summary))
 cat('PASS context loading/failure clears private counts and full warning without removing target-page shell; unrelated pages hide it\n')
})
# Equivalent native/worker numeric representations must not repaint player rows.
selected_player_Server<-function(...)NULL
recent_player_points_source<-function(...)list(history=function()data.frame(),status=function()NULL)
source_rows<-shiny::reactiveVal(data.frame(id=c('p1','p2'),name=c('One','Two'),role='defensa',points=c(-1,0),value=c(1000,2000)))
updates<-0L;updateReactable<-function(outputId,data=NULL,...){if(!is.null(data))updates<<-updates+1L;invisible(NULL)}
shiny::testServer(players_table_Server,args=list(players_table_RV=source_rows,user_teams_RV=function()NULL,optional_ready=function()FALSE),{
 settle(session);original<-output$players_table;before<-updates
 elapsed<-system.time(for(i in 1:10){
  rows<-source_rows();rows$points<-if(i%%2L)as.integer(rows$points)else as.numeric(rows$points)
  attr(rows,'observed_at')<-now+i;attr(rows,'fetch_status')<-if(i%%2L)'stale'else'ok'
  attr(rows,'worker_completeness')<-'complete';attr(rows,'valid_until')<-as.numeric(now)+i
  source_rows(rows);settle(session)
  stopifnot(identical(original,output$players_table),updates==before)
 })[['elapsed']]
 cat(sprintf('PASS ten equivalent player snapshots send no widget render or update despite numeric type/freshness/source-clock changes (%.3fs)\n',elapsed))
 rows<-source_rows();rows$value[1]<-1234;source_rows(rows);settle(session)
 stopifnot(!identical(original,output$players_table))
 cat('PASS a genuine player value change still rebuilds its R-formatted cells\n')
})
source_rows<-shiny::reactiveVal(data.frame(id=paste0('p',1:524),name=paste('Player',1:524),role='defensa',
 points=seq(-10,513),value=seq(1e6,by=1000,length.out=524)))
shiny::testServer(players_table_Server,args=list(players_table_RV=source_rows,user_teams_RV=function()NULL,optional_ready=function()FALSE),{
 settle(session);original<-output$players_table;before<-updates
 elapsed<-system.time(for(i in 1:10){
  rows<-source_rows();attr(rows,'observed_at')<-now+i
  rows$points<-if(i%%2L)as.integer(rows$points)else as.numeric(rows$points)
  source_rows(rows);settle(session)
 })[['elapsed']]
 stopifnot(identical(original,output$players_table),updates==before)
 cat(sprintf('PASS 524-player unchanged snapshots: ten cycles %.3fs, visible snapshot %.1fKiB, zero widget renders/updates\n',elapsed,as.numeric(object.size(table_display_snapshot()))/1024))
})
