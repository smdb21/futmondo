# Scoped factual history; unknown dates and context remain explicit.
history_context_filter<-function(rows,context){
  if(!is.data.frame(rows)||!nrow(rows))return(data.frame())
  keep<-rep(TRUE,nrow(rows))
  for(field in c("championship_id","player_id","season","scoring_version")){
    expected<-context[[field]] %||% ""
    if(!nzchar(expected))next
    if(!field%in%names(rows)){keep<-keep & expected=="unknown";next}
    actual<-as.character(rows[[field]]);actual[is.na(actual)|!nzchar(actual)]<-"unknown"
    keep<-keep & actual==expected
  }
  rows[keep,,drop=FALSE]
}
reconcile_player_points<-function(stored,live,context){
  normalize<-function(rows,source){
    rows<-history_context_filter(rows,context);if(!nrow(rows))return(data.frame())
    round<-suppressWarnings(as.numeric(rows$round %||% rows$round_number));pts<-suppressWarnings(as.numeric(rows$points))
    if(length(round)!=nrow(rows)||length(pts)!=nrow(rows))return(data.frame())
    value<-data.frame(round_number=round,points=pts,date=as.character(rows$occurred_at %||% rows$round_start_at %||% rep(NA_character_,nrow(rows))),score_status=as.character(rows$score_status %||% rep("provisional",nrow(rows))),observed_at=as.character(rows$observed_at %||% rep(NA_character_,nrow(rows))),source=source,stringsAsFactors=FALSE)
    value$score_status[is.na(value$score_status)|!value$score_status%in%c("final","provisional")]<-"provisional"
    value[is.finite(value$round_number)&is.finite(value$points),,drop=FALSE]
  }
  rows<-dplyr::bind_rows(normalize(stored,"supabase"),normalize(live,"futmondo"))
  if(!nrow(rows))return(data.frame(round_number=numeric(),points=numeric(),date=character(),score_status=character(),observed_at=character(),source=character()))
  stamp<-as.numeric(fm_time(rows$observed_at));stamp[is.na(stamp)]<--Inf
  rows<-rows[order(rows$round_number,stamp,rows$score_status=="final",rows$source=="futmondo"),,drop=FALSE]
  rows<-rows[!duplicated(rows$round_number,fromLast=TRUE),,drop=FALSE]
  rows[order(rows$round_number),,drop=FALSE]
}
history_read_pages<-function(table,query,max_rows=20000L,page_size=500L,cursor_fields=NULL){
  stopifnot(length(query$order)==1L,nzchar(query$order),max_rows>0,page_size>0)
  chunks<-list();offset<-0L;complete<-FALSE
  repeat{
    q<-query;q$limit<-min(as.integer(page_size),as.integer(max_rows-offset))
    if(is.null(cursor_fields))q$offset<-offset
    batch<-supabase_get(table,q)
    if(is.null(batch)){if(!length(chunks))return(NULL);break}
    if(!nrow(batch)){complete<-TRUE;break}
    chunks[[length(chunks)+1L]]<-batch;offset<-offset+nrow(batch)
    if(offset>=max_rows)break
    if(!is.null(cursor_fields)){
      a<-cursor_fields[[1]];b<-cursor_fields[[2]]
      if(!all(c(a,b)%in%names(batch)))stop("History cursor columns are missing")
      last<-batch[nrow(batch),,drop=FALSE];av<-as.character(last[[a]]);bv<-as.character(last[[b]])
      if(anyNA(c(av,bv)))stop("History cursor is unavailable")
      query$or<-paste0("(",a,".gt.",av,",and(",a,".eq.",av,",",b,".gt.",bv,"))")
    }
  }
  rows<-dplyr::bind_rows(chunks);attr(rows,"complete")<-complete
  attr(rows,"fetch_status")<-if(complete)if(nrow(rows))"ok" else "empty" else "partial"
  rows
}
read_card_price_history<-function(context,max_rows=5000L){
  history_read_pages("player_history",list(championship_id=paste0("eq.",context$championship_id),player_id=paste0("eq.",context$player_id),select="id,player_id,championship_id,value,observed_at,recorded_at",order="recorded_at.asc,id.asc"),max_rows=max_rows,cursor_fields=c("recorded_at","id"))
}
read_card_round_history<-function(context,max_rows=5000L){
  history_read_pages("player_match_observations",list(championship_id=paste0("eq.",context$championship_id),player_id=paste0("eq.",context$player_id),season=paste0("eq.",context$season),scoring_version=paste0("eq.",context$scoring_version),select="player_id,championship_id,season,scoring_version,round,points,score_status,round_start_at,occurred_at,observed_at",order="round.asc,observed_at.asc"),max_rows=max_rows,cursor_fields=c("round","observed_at"))
}
normalize_card_valuation<-function(stored,summary){
  live<-summary$prices
  if(is.list(live)&&!is.data.frame(live))live<-tryCatch(dplyr::bind_rows(live),error=function(e)data.frame())
  if(!is.data.frame(live))live<-data.frame()
  normalize<-function(rows,source){
    if(!is.data.frame(rows)||!nrow(rows))return(data.frame())
    date<-rows$recorded_at %||% rows$date;value<-rows$value %||% rows$price
    if(is.null(date)||is.null(value))return(data.frame())
    out<-data.frame(recorded_at=format(fm_time(date),"%Y-%m-%dT%H:%M:%OSZ",tz="UTC"),value=suppressWarnings(as.numeric(value)),source=source,observed_at=as.character(rows$observed_at %||% rep(NA_character_,nrow(rows))),stringsAsFactors=FALSE)
    out[!is.na(fm_time(out$recorded_at))&is.finite(out$value),,drop=FALSE]
  }
  rows<-dplyr::bind_rows(normalize(stored,"supabase"),normalize(live,"futmondo"))
  if(!nrow(rows))return(data.frame(recorded_at=character(),value=numeric(),source=character(),observed_at=character()))
  rows<-rows[order(fm_time(rows$recorded_at),rows$source=="futmondo"),,drop=FALSE]
  rows[!duplicated(rows$recorded_at,fromLast=TRUE),,drop=FALSE]
}
get_player_card_history<-function(login,championship_id,user_team_id,player_id,refresh=FALSE,season=NULL,scoring_version=NULL){
  base<-resource_context(login,championship_id,user_team_id,player_id)
  if(refresh){invalidate_resources(base,pattern="^player_card_history$|^player_summary_");invalidate_resources(resource_context(login,championship_id,user_team_id),pattern="^lineup_")}
  summary<-tryCatch(get_player_summary(login,championship_id,user_team_id,player_id),error=function(e)NULL)
  api_season<-league_season_context(championship_id,summary$championship$season %||% summary$championship$seasonId)
  selected_season<-season %||% api_season
  lineup<-tryCatch(get_lineup_from_team(login,championship_id,user_team_id),error=function(e)NULL)
  api_scoring<-normalize_league_rules(list(),lineup,championship_id)$scoring_version
  ctx<-resource_context(login,championship_id,user_team_id,player_id,selected_season,scoring_version %||% api_scoring)
  get_cached_data("player_card_history",{
    stored<-tryCatch(read_card_round_history(ctx),error=function(e)NULL);prices<-tryCatch(read_card_price_history(ctx),error=function(e)NULL)
    live<-if(!is.null(summary))normalize_player_match_observations(summary,player_id,championship_id,scoring_version=api_scoring,season=api_season,observed_at=attr(summary,"observed_at") %||% Sys.time())else data.frame()
    previous_points<-reconcile_player_points(stored,data.frame(),ctx)
    points<-reconcile_player_points(stored,live,ctx);valuation<-normalize_card_valuation(prices,summary)
    known<-ctx$season!="unknown"&&ctx$scoring_version!="unknown"
    complete<-known&&isTRUE(summary$history_complete)&&!identical(attr(summary,"fetch_status"),"stale")&&isTRUE(attr(stored,"complete"))&&isTRUE(attr(prices,"complete"))
    list(summary=summary,points=points,previous_points=previous_points,valuation=valuation,context=ctx,source=unique(c(points$source,valuation$source)),status=if(is.null(summary)&&is.null(stored)&&is.null(prices))"unavailable" else if(!nrow(points)&&!nrow(valuation))"empty" else if(complete)"ok" else "partial",freshness=if(identical(attr(summary,"fetch_status"),"stale"))"stale" else "fresh",completeness=if(complete)"complete" else "partial",coverage=list(database_rounds=if(is.data.frame(stored))nrow(stored)else 0L,live_rounds=nrow(live),season_known=ctx$season!="unknown",scoring_known=ctx$scoring_version!="unknown"),reason=if(!known)"Season or scoring context is unknown; this history is kept separate from known contexts." else if(!complete)"Historical coverage is partial." else "")
  },context=ctx)
}
