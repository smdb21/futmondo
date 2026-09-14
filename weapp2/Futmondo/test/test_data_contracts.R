#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source("futmondo_functions.R");source("supabase_connector.R");library(shiny);source("Modules/Login_Module.R");source("Modules/Admin_Module.R")})
passed<-0L
check<-function(name,expr) {force(expr);passed<<-passed+1L;cat("PASS",name,"\n")}
check("explicit missing, empty and observed data",{
  stopifnot(as_fetch_result(NULL)$status=="unavailable",
    as_fetch_result(observed_data(data.frame()))$status=="empty",
    as_fetch_result(observed_data(0))$data==0)
})
check("cache isolates authenticated users and retains stale observations",{
  clear_api_cache()
  fetch<-function(login,value,ttl=300) get_cached_data("same_request",{if(is.null(value))stop("offline");value},ttl)
  a<-c(token="t",userid="one");b<-c(token="t",userid="two")
  stopifnot(unname(fetch(a,11))==11,unname(fetch(b,22))==22)
  stale<-fetch(a,NULL,ttl=-1)
  stopifnot(stale==11,attr(stale,"fetch_status")=="stale")
  clear_api_cache("one")
  stopifnot(exists(api_cache_key("same_request",b),api_cache_env),
    !exists(api_cache_key("same_request",a),api_cache_env))
})
check("auction reconstruction retains losers and authoritative winner",{
  d<-data.frame(id=c("auction","sale"),created="2026-09-01T04:00:00Z",player_id="p",
    buyer_team_id=c("winner",""),buyer_team_name=c("Winner","Market"),seller_team_id=c("","seller"),price=c(110,90))
  d$bids<-I(list(list(list(u=list(`_id`="loser",name="Loser"),bid=100,`_id`="b1"),
    list(u=list(`_id`="winner",name="Winner"),bid=105,`_id`="b2")),list()))
  bids<-normalize_bid_observations(d,"league")
  stopifnot(nrow(bids)==2,sum(bids$is_winner)==1,bids$amount[bids$is_winner]==110,
    bids$amount[!bids$is_winner]==100)
  auctions<-normalize_auction_observations(d,"league")
  stopifnot(nrow(auctions)==1,!auctions$visibility_complete)
})
check("missing points and actual zero stay distinct; ambiguous minutes untrusted",{
  s<-list(points=list(list(round=1,points=0,minutesPlayed=1,initialLineUp=FALSE),
    list(round=2,points=NULL),list(round=3,points=-2,finished=TRUE)))
  d<-normalize_player_match_observations(s,"p","c")
  stopifnot(d$points[1]==0,is.na(d$points[2]),d$score_status[2]=="unavailable",
    d$score_status[3]=="final",all(is.na(d$participation)))
})
check("summary scores before the current match round are finalized",{
  s<-list(match=list(r=list(number=3)),points=list(list(round=1,points=4),list(round=2,points=8),list(round=3,points=1)))
  d<-normalize_player_match_observations(s,"p","c")
  stopifnot(identical(d$score_status,c("final","final","provisional")))
})
check("configured scoring weights are not normalized",{
  rules<-normalize_league_rules(list(configuration=list(moneyPerPoint=123,cpt=TRUE)),
    list(custom=list(media=list(list(pct=20),list(pct=20))),multiposition=FALSE))
  stopifnot(rules$money_per_point==123,rules$captain_enabled,
    sum(vapply(rules$scoring$media,function(x)x$pct,numeric(1)))==40)
})
check("notification schema and unknown actions are preserved",{
  d<-normalize_notifications(list(list(`_id`="n",created="2026-09-01T00:00:00Z",type="market",action="newAction",readed=FALSE,
    directObject=list(id="p",name="<script>"),context=list(id="c",name="League"))))
  stopifnot(nrow(d)==1,d$action=="newAction",d$championship_id=="c",!d$is_read,d$player_name=="<script>")
})
check("failed acknowledgement does not clear notification cache",{
  saved<-notification_request
  notification_request<-function(...)fetch_result(list(code="failure"))
  auth<-c(token="t",userid="u")
  key<-api_cache_key("notifications_list",auth);api_cache_env[[key]]<-list(data="sentinel",time=Sys.time())
  stopifnot(!mark_notification_read(auth,"n"),exists(key,api_cache_env))
  notification_request<-function(...)fetch_result(list(code="notification.markReaded.ok"))
  stopifnot(mark_notification_read(auth,"n"),!exists(key,api_cache_env))
  notification_request<-saved
})
check("login markup contains no server credentials",{
  Sys.setenv(user_name="secret-sentinel-email",password="secret-sentinel-password")
  html<-as.character(login_UI("login"))
  stopifnot(!grepl("secret-sentinel",html,fixed=TRUE),grepl("logout_button",html,fixed=TRUE))
})
check("admin authority uses verified login and matches only configured account",{
  stopifnot(!is_authorized_admin(NULL,"admin@example.org"),
    !is_authorized_admin(c(token="t",userid="u"),"admin@example.org"),
    !is_authorized_admin(c(token="t",userid="u",user_name="other"),"admin@example.org"),
    is_authorized_admin(c(token="t",userid="u",user_name="ADMIN@example.org"),"admin@example.org"))
})
check("natural conflict targets cover observations and scenarios",{
  stopifnot(supabase_conflict_key("bid_observations")=="championship_id,auction_id,bidder_id",
    supabase_conflict_key("market_transactions")=="championship_id,source_event_id",
    supabase_conflict_key("player_daily_snapshots")=="player_id,championship_id,snapshot_date")
})
check("financial snapshot preserves zero and debt",{
  saved_info<-get_user_team_info;saved_capacity<-get_acquisition_capacity
  get_acquisition_capacity<-function(...)list(status="ok",roster=list(count=17,cap=20),
    funds=list(api_bid_limit=1000,team_value=10000,debt_limit=5000,minimum_balance=-5000,
      projected_committed_balance=-100,spendable_budget=4900),
    outstanding=list(count=0,total_amount=0,completeness="complete"))
  get_user_team_info<-function(...)list(budget=-100,withheld=0,maxBid=1000,teamValue=10000,configuration=list())
  clear_api_cache()
  fin<-get_financial_snapshot(c(token="t",userid="negative"),"c","t")
  stopifnot(fin$cash== -100,fin$spendable_budget==4900,fin$legal_bid_limit==1000)
  get_user_team_info<-function(...)list(budget=0,withheld=0,maxBid=0,configuration=list())
  clear_api_cache()
  stopifnot(get_financial_snapshot(c(token="t",userid="zero"),"c","t")$cash==0)
  get_user_team_info<-saved_info;get_acquisition_capacity<-saved_capacity
})
cat(sprintf("DATA CONTRACTS: %d passed / 0 failed\n",passed))
