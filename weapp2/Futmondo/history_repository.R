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
    value<-data.frame(round_number=round,points=pts,date=as.character(rows$occurred_at %||% rows$round_start_at %||% rep(NA_character_,nrow(rows))),score_status=as.character(rows$score_status %||% rep("unknown",nrow(rows))),observed_at=as.character(rows$observed_at %||% rep(NA_character_,nrow(rows))),source=source,stringsAsFactors=FALSE)
    value$match_id<-as.character(rows$match_id %||% rep("",nrow(rows)));value$round_id<-as.character(rows$round_id %||% rep("",nrow(rows)))
    value$score_status[is.na(value$score_status)|!value$score_status%in%c("final","provisional","unknown","unavailable")]<-"unknown"
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

# Pure assembly allows summary points to render before either stored history read.
assemble_player_card_history <- function(summary,lineup,stored,prices,context) {
  api_season<-league_season_context(context$championship_id,summary$championship$season %||% summary$championship$seasonId)
  api_scoring<-normalize_league_rules(list(),lineup,context$championship_id)$scoring_version
  live<-if(!is.null(summary))normalize_player_match_observations(summary,context$player_id,context$championship_id,
    scoring_version=api_scoring,season=api_season,observed_at=attr(summary,"observed_at") %||% Sys.time())else data.frame()
  points<-reconcile_player_points(stored,live,context)
  valuation<-normalize_card_valuation(prices,summary)
  list(summary=summary,points=points,previous_points=reconcile_player_points(stored,data.frame(),context),
    valuation=valuation,context=context,status=if(!nrow(points)&&!nrow(valuation))"empty"else "partial",
    freshness=if(identical(attr(summary,"fetch_status"),"stale"))"stale"else "fresh")
}

# Current player responses share the card request context. Their round labels
# can join live scores and fixtures even when optional IDs are omitted. Saved
# history still requires explicit context; contradictory source evidence wins.
build_player_recent_rounds <- function(points,matches,context,limit=5L,current_player_responses=FALSE) {
  if(!is.data.frame(points)||!all(c("round_number","points")%in%names(points)))points<-data.frame()
  if(!is.data.frame(matches)||!"round"%in%names(matches))matches<-data.frame()
  known<-function(x)length(x)==1L&&!is.na(x)&&nzchar(as.character(x))&&!identical(as.character(x),"unknown")
  field<-function(row,name)if(name%in%names(row))fm_scalar(row[[name]],"")else ""
  if(nrow(points)){
    points<-points[is.finite(suppressWarnings(as.numeric(points$round_number))),,drop=FALSE]
    points<-points[order(as.numeric(points$round_number),decreasing=TRUE),,drop=FALSE]
  }
  if(nrow(matches))matches<-unique(matches)
  match_ids<-if("match_id"%in%names(matches))as.character(matches$match_id)else rep("",nrow(matches))
  ambiguous_ids<-unique(match_ids[nzchar(match_ids)&!is.na(match_ids)&(duplicated(match_ids)|duplicated(match_ids,fromLast=TRUE))])
  column<-function(name,default="")if(name%in%names(matches))as.character(matches[[name]])else rep(default,nrow(matches))
  seasons<-column("season","unknown");competitions<-column("competition_id");round_ids<-column("round_id")
  numeric_rounds<-suppressWarnings(as.numeric(matches$round))
  expected_season<-context$season %||% "unknown";expected_competition<-context$competition_id %||% ""
  valid<-function(x)!is.na(x)&nzchar(x)&x!="unknown"
  compatible<-rep(TRUE,nrow(matches))
  if(known(expected_season))compatible<-compatible&(!valid(seasons)|seasons==expected_season)
  if(known(expected_competition))compatible<-compatible&(!valid(competitions)|competitions==expected_competition)
  compatible<-compatible&!match_ids%in%ambiguous_ids
  scoped<-if(known(expected_season)&&known(expected_competition))valid(seasons)&seasons==expected_season&valid(competitions)&competitions==expected_competition else rep(FALSE,nrow(matches))
  used<-rep(FALSE,nrow(matches));groups<-list()
  for(i in seq_len(nrow(points))){
    point<-points[i,,drop=FALSE]
    round<-as.numeric(point$round_number)
    exact_match<-if(known(field(point,"match_id")))match_ids==field(point,"match_id")else rep(FALSE,nrow(matches))
    exact_round<-if(known(field(point,"round_id")))round_ids==field(point,"round_id")else rep(FALSE,nrow(matches))
    round_agrees<-!is.finite(numeric_rounds)|numeric_rounds==round
    live_card<-isTRUE(current_player_responses)&&known(context$player_id)&&identical(field(point,"source"),"futmondo")
    fallback_compatible<-rep(TRUE,nrow(matches))
    if(known(field(point,"match_id")))fallback_compatible<-fallback_compatible&(!valid(match_ids)|exact_match)
    if(known(field(point,"round_id")))fallback_compatible<-fallback_compatible&(!valid(round_ids)|exact_round)
    indices<-which(compatible&round_agrees&(exact_match|exact_round|
      ((scoped|live_card)&fallback_compatible&numeric_rounds==round)))
    # Conflicting duplicate point observations must not attach a fixture twice.
    indices<-indices[!used[indices]];used[indices]<-TRUE
    groups[[length(groups)+1L]]<-list(round_number=as.numeric(point$round_number),points=as.numeric(point$points),
      score_status=field(point,"score_status"),fixtures=matches[indices,,drop=FALSE])
  }
  unassociated<-matches[!used,,drop=FALSE]
  if(nrow(unassociated))unassociated<-head(unassociated[order(unassociated$round,decreasing=TRUE,na.last=TRUE),,drop=FALSE],limit)
  list(rounds=head(groups,limit),unassociated=unassociated)
}
