#!/usr/bin/env Rscript
# Actual table -> card -> successful clause callback; every read/write is offline.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
record_acquired_resource<-function(...)invisible(TRUE)
auth<-c(userid='clause-refresh-fixture',token='synthetic')
score<-paste0(normalize_league_rules(list(),list(pointSystem='fixture'),'league')$scoring_version)
stamp<-Sys.time()-30;ids<-sprintf('p%04d',seq_len(527L))
players<-data.frame(id=ids,name=paste('Player',ids),slug=ids,role='defensa',team='Club',value=1000,
 change=0,points=15,user_team_id='rival',clause_price=1200,clause_transferred=FALSE,
 clause_date='2020-01-01T00:00:00Z',market_inMarket=FALSE,price=NA_real_)
observations<-function(id)data.frame(player_id=id,championship_id='league',season='unknown',scoring_version=score,
 round=1:5,points=1:5,score_status='unknown',observed_at=format(stamp,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),
 occurred_at=NA_character_,round_start_at=NA_character_)
payload<-function(id)list(context=resource_context(auth,'league','mine',id,scoring_version=score),checked_at=Sys.time(),observations=observations(id))
saved<-dplyr::bind_rows(lapply(head(ids,-1L),observations))
checks<-data.frame(player_id=head(ids,-1L),championship_id='league',season='unknown',scoring_version=score,
 checked_at=format(stamp,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),result_status='scores',point_count=5L)
calls<-character();pending<-new.env(parent=emptyenv());acks<-list();writes<-0L;mutated<-FALSE
capacity<-function()list(status='ok',valid_until=as.numeric(Sys.time())+15,
 roster=list(status='ok',count=17,cap=20,effective_count=17),funds=list(spendable_budget=10000),
 outstanding=list(count=0),rules=list(minimum_bid_at_value=TRUE))
get_team_acquisition_capacity<-function(...)capacity()
get_championship_players<-function(login,championship_id)players
get_lineup_from_team<-function(...)list(pointSystem='fixture')
get_player_summary<-function(...)list(championship=list(),points=list(),prices=list())
get_finished_rounds<-function(...)data.frame(round_number=1:5,is_finished=TRUE)
buy_roster_clause<-function(...){writes<<-writes+1L;mutated<<-TRUE;list(success=TRUE)}
log_market_transaction<-function(...)invisible(TRUE)
persist_recent_points_snapshot<-function(payload,context,on_complete){acks[[length(acks)+1L]]<<-on_complete;TRUE}
async_adapter_fetch<-function(fn,args=list(),context,...){
 calls<<-c(calls,paste(fn,args$player_id %||% '',sep=':'))
 if(fn=='get_player_recent_points')return(promises::promise(function(resolve,reject)pending[[args$player_id]]<-resolve))
 if(fn=='get_championship_players'&&mutated)return(promises::promise(function(resolve,reject)pending$catalog<-resolve))
 value<-switch(fn,read_league_round_dataset=list(points=saved,checks=checks,complete=TRUE),
  get_team_acquisition_capacity=capacity(),get_championship_players=players,get_lineup_from_team=list(pointSystem='fixture'),
  get_finished_rounds=get_finished_rounds(),get_player_summary=get_player_summary(),
  get_player_full_profile=list(profile=list()),get_player_matches=list(matches=list()),
  read_card_round_history=data.frame(),read_card_price_history=data.frame(),stop(paste('Unexpected source',fn)))
 promises::promise_resolve(value)
}
getReactableState<-function(outputId,name,session)switch(name,selected=session$input$fixture_selected,page=1L,pageSize=10L,NULL)
settle<-function(session)for(i in seq_len(20L)){later::run_now(0);session$flushReact()}
n<-0L;check<-function(name,code){force(code);n<<-n+1L;cat('PASS',name,'\n')}
clear_api_cache()
server<-function(input,output,session){
 refresh<-reactiveVal(0L)
 reader<-factual_read_adapter(session,'get_championship_players',reactive(auth),reactive('league'),reactive('mine'),refresh_trigger=refresh)
 rows<-reactive(reader(auth,'league'))
 table<-players_table_Server('table',rows,reactive(data.frame(teamid='rival',teamname='Rival')),
  reactive(auth),reactive('league'),reactive('mine'),refresh_trigger=refresh)
}
elapsed<-system.time(shiny::testServer(server,{
 settle(session);pending[[tail(ids,1L)]](payload(tail(ids,1L)));settle(session)
 controller<-session$userData$shared_round_points
 check('527-player matrix completes while one save remains pending',{
  stopifnot(controller$status()$completed==527L,controller$status()$saving==1L,length(acks)==1L,
   sum(startsWith(calls,'get_player_recent_points:'))==1L)
 })
 session$setInputs(`table-fixture_selected`=1L);settle(session)
 session$setInputs(`table-selected_player-btn_pay_clause`=1L);settle(session)
 before<-calls;before_history<-controller$dataset()$points;before_checks<-controller$entry()$checks
 before_table<-output[['table-players_table']]
 session$setInputs(`table-selected_player-submit_clause`=1L);settle(session)
 check('successful clause refreshes catalog but sends zero historical player requests',{
  added<-tail(calls,length(calls)-length(before))
  stopifnot(writes==1L,refresh()==1L,is.function(pending$catalog),
   sum(startsWith(added,'get_championship_players:'))==1L,
   !any(startsWith(added,'get_player_recent_points:')),!any(startsWith(added,'read_league_round_dataset:')))
 })
 check('same-context refresh preserves visible rows, historical checks and pending saves',{
  stopifnot(identical(before_table,output[['table-players_table']]),nrow(rows())==527L,
   identical(controller$dataset()$points,before_history),identical(controller$entry()$checks,before_checks),
   controller$status()$completed==527L,controller$status()$saving==1L)
 })
 fresh<-players;fresh$clause_transferred[1]<-TRUE;pending$catalog(fresh);settle(session)
 check('fresh ownership state replaces the retained table without restarting history',{
  stopifnot(isTRUE(rows()$clause_transferred[1]),sum(startsWith(calls,'get_player_recent_points:'))==1L,
   controller$status()$completed==527L,controller$status()$saving==1L)
 })
 before<-sum(startsWith(calls,'get_player_recent_points:'));manual<-round_history_refresh_trigger(session);manual(manual()+1L);settle(session)
 check('explicit global history refresh still queries score corrections with two slots',{
  stopifnot(sum(startsWith(calls,'get_player_recent_points:'))==before+2L,controller$status()$pending==2L,controller$status()$saving==1L)
 })
 acks[[1]](TRUE);settle(session)
 check('original save acknowledgement remains valid after mutation and manual refresh',{
  stopifnot(controller$status()$saving==0L,tail(ids,1L)%in%controller$entry()$checks$player_id)
 })
}))[['elapsed']]
cat(sprintf('527-player successful clause regression: %.3fs locally; 0 mutation history reads, one catalog refresh.\n',elapsed))
clear_api_cache();resolvers<-list();attempt<-0L
shiny::testServer(function(input,output,session){
 refresh<-reactiveVal(0L);league<-reactiveVal('league')
 state<-shared_resource(session,'card_capacity',function()resource_context(auth,league(),'mine'),
  fetch=function(ctx){attempt<<-attempt+1L;promises::promise(function(resolve,reject)resolvers[[attempt]]<<-list(resolve=resolve,reject=reject))},refresh=refresh)
},{
 settle(session);resolvers[[1]]$resolve(capacity());settle(session);refresh(1L);settle(session)
 check('retained acquisition evidence is stale and its loading gate remains closed',{
  stopifnot(identical(state()$status,'loading'),identical(state()$freshness,'stale'),!is.null(state()$data))
 })
 resolvers[[2]]$reject(resource_failure(category='network'));settle(session)
 check('failed refresh retains truthful stale evidence without another automatic request',{
  stopifnot(identical(state()$status,'stale'),identical(state()$freshness,'stale'),!is.null(state()$error),attempt==2L)
 })
 league('other');settle(session)
 check('context switch never exposes the previous league data',stopifnot(is.null(state()$data),state()$status=='loading'))
})
# An expired factual table refresh keeps rows and cannot retrigger on each read.
clear_api_cache();mutated<-FALSE;calls<-character()
clock_now<-base::Sys.time();Sys.time<-function()clock_now
shiny::testServer(function(input,output,session){
 pulse<-reactiveVal(0L)
 reader<-factual_read_adapter(session,'get_championship_players',reactive(auth),reactive('league'),reactive('mine'))
 rows<-reactive({pulse();reader(auth,'league')})
 observe(rows())
},{
 settle(session);stopifnot(nrow(rows())==527L)
 clock_now<<-clock_now+resource_policy('get_championship_players')$ttl+1
 mutated<<-TRUE;pulse(1L);settle(session)
 check('expiry preserves table rows and dispatches only one asynchronous refresh',{
  stopifnot(nrow(rows())==527L,identical(attr(rows(),'fetch_status'),'stale'),
   sum(startsWith(calls,'get_championship_players:'))==2L)
  for(i in 2:5){pulse(i);settle(session);stopifnot(nrow(rows())==527L)}
  stopifnot(sum(startsWith(calls,'get_championship_players:'))==2L)
 })
 pending$catalog(players);settle(session)
 check('successful expiry refresh restores fresh table provenance',{
  stopifnot(nrow(rows())==527L,!identical(attr(rows(),'fetch_status'),'stale'),
   sum(startsWith(calls,'get_championship_players:'))==2L)
 })
})
rm('Sys.time',envir=.GlobalEnv)
cat(sprintf('CLAUSE REFRESH SCOPE: %d passed / 0 failed\n',n))
