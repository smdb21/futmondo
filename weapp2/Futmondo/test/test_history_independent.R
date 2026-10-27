#!/usr/bin/env Rscript
# Independent known-answer review supplied by the Luna investigator.
suppressPackageStartupMessages(library(dplyr));Sys.setenv(TZ="UTC")
base<-Sys.getenv("FUTMONDO_BASELINE","/tmp/futmondo-redesign-baseline-gtos1hyz");app<-normalizePath(".")
load_fn<-function(path,name,env){z<-Filter(function(x)is.call(x)&&identical(x[[1]],as.name("<-"))&&identical(x[[2]],as.name(name)),as.list(parse(path)));stopifnot(length(z)>0L);eval(z[[length(z)]],env)}
if(dir.exists(base)){
 old<-new.env(parent=globalenv());sys.source(file.path(base,"data_contracts.R"),old)
 load_fn(file.path(base,"Modules/Selected_Player_Module.R"),"build_player_points_trace",old)
 load_fn(file.path(base,"Modules/Selected_Player_Module.R"),"player_match_points_trace",old)
 daily<-data.frame(recorded_at=c("2026-08-01T10:00:00Z","2026-08-08T10:00:00Z"),points=c(4,12))
 rnds<-data.frame(round_number=1:2,begin_process=daily$recorded_at,is_finished=TRUE)
 old_cum<-old$build_player_points_trace(daily,rnds)$points_df$points
 old_na<-old$player_match_points_trace(data.frame(round=1:2,points=c(0,-2),score_status="final",occurred_at=NA_character_,round_start_at=NA_character_,observed_at=NA_character_))
 stopifnot(identical(old_cum,c(4,12)),!old_na$has_points)
 cat("BASELINE: cumulative 4,12 rendered as round scores; undated zero/negative scores omitted.\n")
}else cat("Baseline reproduction skipped: snapshot unavailable.\n")
new<-new.env(parent=globalenv());sys.source(file.path(app,"data_contracts.R"),new);sys.source(file.path(app,"history_repository.R"),new)
load_fn(file.path(app,"Modules/Selected_Player_Module.R"),"player_match_points_trace",new)
load_fn(file.path(app,"Modules/Players_Table_Module.R"),"add_recent_points_average",new)
bind<-function(name,fn){environment(fn)<-new;assign(name,fn,envir=new)}
bind("resource_context",function(login,championship_id,user_team_id,player_id=NULL,season="unknown",scoring_version="unknown",...)list(championship_id=as.character(championship_id),player_id=if(is.null(player_id))""else as.character(player_id),season=as.character(season),scoring_version=as.character(scoring_version)))
bind("league_season_context",function(championship_id,observed_season=NULL,...)if(!is.null(observed_season))as.character(observed_season)else "unknown")
bind("get_lineup_from_team",function(...)list());bind("normalize_league_rules",function(...)list(scoring_version="V1"))
new$summary_fixture<-list(championship=list(season="S1"),points=list(list(round=3,points=8,isFinished=TRUE),list(round=4,points=-2,isFinished=TRUE),list(round=5,points=3,isFinished=FALSE)),history_complete=FALSE)
attr(new$summary_fixture,"observed_at")<-as.POSIXct("2026-08-10 12:00:00",tz="UTC")
bind("get_player_summary",function(...)summary_fixture)
new$stored<-data.frame(championship_id=c("L","L","L","L","L2","L","L"),player_id="P",season=c("S1","S1","S1","S1","S1","S2","S1"),scoring_version=c("V1","V1","V1","V1","V1","V1","V2"),round=c(1,2,3,5,9,8,7),points=c(0,-4,6,10,900,800,700),score_status="final",occurred_at=NA_character_,observed_at=c(NA,NA,"2026-08-02T12:00:00Z","2026-08-02T12:00:00Z",NA,NA,NA))
bind("read_card_round_history",function(context,max_rows=5000L){x<-stored;attr(x,"complete")<-FALSE;x})
bind("read_card_price_history",function(...){x<-data.frame(recorded_at=character(),value=numeric());attr(x,"complete")<-TRUE;x})
bind("get_cached_data",function(name,expr,context)eval(substitute(expr),parent.frame()))
card<-new$get_player_card_history("token","L","T","P");want<-c(0,-4,8,-2,3)
stopifnot(identical(as.numeric(card$points$round_number),as.numeric(1:5)),identical(as.numeric(card$points$points),want),card$status=="partial",sum(is.na(card$points$date))==5L,card$points$score_status[5]=="provisional")
fin<-card$points[card$points$score_status=="final",,drop=FALSE]
trace<-new$player_match_points_trace(data.frame(round=fin$round_number,points=fin$points,score_status=fin$score_status,occurred_at=fin$date,observed_at=fin$observed_at))
stopifnot(trace$has_points,identical(as.numeric(trace$points_df$points),c(0,-4,8,-2)),sum(is.na(trace$points_df$date))>=2L)
table<-new$add_recent_points_average(data.frame(id="P"),transform(fin,player_id="P",round=round_number),n=4L)
stopifnot(isTRUE(all.equal(table$recent_points_avg,0.5)))
cat("PASS independent context isolation, partial merge, corrections, undated scores, provisional status and mean 0.5.\n")
