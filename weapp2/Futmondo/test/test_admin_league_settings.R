options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source("futmondo_functions.R");library(shiny);source("Modules/Admin_Module.R")})
x<-admin_league_settings(list(configuration=list(moneyPerPoint=100,rankingMode="top",usersToRank=3)),
 list(configuration=list(moneyPerPoint=0,rankingMode="flop",usersToRank=-1),initialRosterEmpty=FALSE),"fixture-league")
get<-function(label,column="value")x[x$setting==label,column]
stopifnot(get("Per point")=="0 EUR",get("Ranking direction")=="Lowest scores first",
 get("Ranking recipients")=="All eligible teams",get("Start with an empty squad")=="No",
 get("Starting cash")=="Not provided",
 get("Maximum squad size")=="Not provided")
cat("PASS configuration precedence, zero/false values and no invented opening budget\n")
x<-admin_league_settings(list(configuration=list(budget=0,usersToRank=5,mvpPlayer=10)),championship_id="other")
stopifnot(get("Starting cash")=="0 EUR",grepl("40%, 30%",get("Ranking recipients","explanation")))
x<-admin_league_settings(championship_id="other")
stopifnot(get("Starting cash")=="Not provided",get("Full season")=="Not provided")
cat("PASS known distribution and unavailable settings remain distinct\n")
x<-admin_league_settings(list(configuration=list(token="never-display",password="never-display",rankingMode="unexpected")))
stopifnot(!any(grepl("never-display",unlist(x))),grepl("unrecognized",get("Ranking direction")))
stopifnot(grepl("league_settings",as.character(admin_UI("admin"))))
cat("PASS unknown fields excluded and Admin section present\n")

x<-admin_league_settings(list(configuration=list(playerMoveInDays=2,enableCoach="automatic",extraPay=0,marketTimes=1,playerRetention=0,enablingClause=10,loans=FALSE,rtv="false",vmb=-1,mtoffset="-60",mtype="normal",ac=TRUE)),
 list(configuration=list(playerMoveInDays=7)),"other")
stopifnot(get("Clause arrival delay")=="7 days",get("Coach mode")=="Automatic",get("Loans")=="No",get("Extra payment")=="0",
 get("Markets per day")=="1",get("Minimum holding period")=="0 days",get("Clause protection after signing")=="10 days",get("Deduct initial squad value")=="No",get("Visible offers")=="All",get("mtoffset")=="-60",get("Market type")=="Normal",get("ac")=="Yes")
html<-as.character(admin_league_settings_UI(x,stale=TRUE))
stopifnot(grepl('fm-admin-settings-grid',html,fixed=TRUE),grepl('fm-admin-setting',html,fixed=TRUE),grepl('<summary>',html,fixed=TRUE),grepl('Other reported settings (2)',html,fixed=TRUE),grepl('Showing saved settings',html,fixed=TRUE),!grepl('<details[^>]* open',html))
cat("PASS variable clause-arrival delay and omitted captured settings in compact groups\n")
for(days in c(0,1,2,7,10)){
 x<-admin_league_settings(info=list(configuration=list(playerMoveInDays=days,fullSeason="false",loans="true")),championship_id="other")
 stopifnot(get("Clause arrival delay")==paste0(days,if(days==1)" day"else" days"),get("Full season")=="No",get("Loans")=="Yes")
}
x<-admin_league_settings(championship_id="other");stopifnot(get("Clause arrival delay")=="Not provided")
cat("PASS zero and varying configured delays have no fixed-day fallback\n")

for(budget in c(0,42000000,300000000)){
 x<-admin_league_settings(list(configuration=list(budget=123)),list(budget=63047149,configuration=list(budget=budget)))
 stopifnot(get("Starting cash")==paste0(format(budget,big.mark=".",decimal.mark=",",scientific=FALSE,trim=TRUE)," EUR"))
}
x<-admin_league_settings(info=list(configuration=list(mnmp=.5,dspct=1,ccr=3,ctt=FALSE,mcpw=-1,rcp=-1,cpt=FALSE,blc=TRUE,apm=TRUE,aes=TRUE,dbp=TRUE,mbp=0,ddt=TRUE,acl=TRUE,avmb=TRUE,mbmv=TRUE,mpm=0,rtv="automatic")))
stopifnot(get("Squad value available for bids")=="50%",get("Direct-sale percentage")=="100%",get("Clause price range")=="Expensive",
 get("Daily clause transfer time")=="00:05",get("Weekly clauses against a team")=="No limit",get("Recover clause investment")=="None",
 get("Captain enabled")=="No",get("Lock postponed-match lineups")=="Yes",get("Multiposition players")=="Yes",get("Extra formations")=="Yes",
 get("Prohibit clause blocking")=="Yes",get("Maximum blocked players")=="No limit",get("Prohibit clause renewal")=="Yes",
 get("Allow loan cancellation")=="Yes",get("Show number of bids")=="Yes",get("Minimum bid is player value")=="Yes",
 get("Players offered for sale")=="No limit",get("Deduct initial squad value")=="Automatic")
x<-admin_league_settings(info=list(configuration=list(ctt="true",rcp=.25,ccr=2,mcpw=3,mbp=2,mpm=4,vmb=3,dspct=.05,mnmp=0)))
stopifnot(get("Daily clause transfer time")=="12:05",get("Recover clause investment")=="25%",get("Clause price range")=="Intermediate",
 get("Weekly clauses against a team")=="3",get("Maximum blocked players")=="2",get("Players offered for sale")=="4",get("Visible offers")=="3",
 get("Direct-sale percentage")=="5%",get("Squad value available for bids")=="0%")
x<-admin_league_settings();stopifnot(get("Daily clause transfer time")=="Not provided",get("Recover clause investment")=="Not provided")
cat("PASS configured opening budget, verified code mappings, units and sentinel choices\n")
