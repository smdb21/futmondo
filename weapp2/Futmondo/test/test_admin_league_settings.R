options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source("futmondo_functions.R");library(shiny);source("Modules/Admin_Module.R")})
x<-admin_league_settings(list(configuration=list(moneyPerPoint=100,rankingMode="top",usersToRank=3)),
 list(configuration=list(moneyPerPoint=0,rankingMode="flop",usersToRank=-1),initialRosterEmpty=FALSE),rival_cash_league_id)
get<-function(label,column="value")x[x$setting==label,column]
stopifnot(get("Per point")=="0 EUR",get("Ranking direction")=="Lowest scores first",
 get("Ranking recipients")=="All eligible teams",get("Start with an empty squad")=="No",
 get("Starting cash")=="300.000.000 EUR",grepl("User-confirmed",get("Starting cash","explanation")),
 get("Maximum squad size")=="Not provided")
cat("PASS configuration precedence, zero/false values and labeled opening fallback\n")
x<-admin_league_settings(list(configuration=list(budget=0,usersToRank=5,mvpPlayer=10)),championship_id="other")
stopifnot(get("Starting cash")=="0 EUR",grepl("40%, 30%",get("Ranking recipients","explanation")))
x<-admin_league_settings(championship_id="other")
stopifnot(get("Starting cash")=="Not provided",get("Full season")=="Not provided")
cat("PASS known distribution and unavailable settings remain distinct\n")
x<-admin_league_settings(list(configuration=list(token="never-display",password="never-display",rankingMode="unexpected")))
stopifnot(!any(grepl("never-display",unlist(x))),grepl("unrecognized",get("Ranking direction")))
stopifnot(grepl("league_settings",as.character(admin_UI("admin"))))
cat("PASS unknown fields excluded and Admin section present\n")
