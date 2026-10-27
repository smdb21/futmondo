library(httr)
library(jsonlite)
library(dplyr)

# Retrieve Config variables from environment
SB_URL <- Sys.getenv("supabase_project_url")
SB_KEY <- Sys.getenv("supabase_secret_key")

# Dynamic retrieval helpers -- re-check the environment each call so that
# a later readRenviron() in global.R is respected.
get_sb_url <- function() {
  url <- Sys.getenv("supabase_project_url")
  if (is.null(url) || url == "") SB_URL else url
}
get_sb_key <- function() {
  key <- Sys.getenv("supabase_secret_key")
  if (is.null(key) || key == "") SB_KEY else key
}

# Keep only safe diagnostic codes; response text can contain private row data.
record_persistence_failure <- function(table_name, http_status = NULL, error_code = NULL, category = NULL) {
  table_name <- if (length(table_name) == 1L && !is.na(table_name) &&
    grepl("^[a-z_]+$", table_name)) table_name else "unknown"
  code <- if (is.character(error_code) && length(error_code) == 1L &&
    !is.na(error_code) && grepl("^[A-Z0-9]{4,12}$", error_code)) error_code else ""
  status <- if (length(http_status) == 1L && is.numeric(http_status) &&
    is.finite(http_status)) http_status else NA_real_
  if (is.null(category)) category <- if (status %in% c(401, 403)) "authorization" else
    if (code %in% c("PGRST204", "PGRST205", "42P01", "42703", "42P10")) "schema" else
    if (is.na(status) || status == 429 || status >= 500) "connection" else "write"
  issue <- list(table = table_name, category = category, http_status = status, code = code,
    stage=getOption("futmondo.persistence_stage",""))
  options(futmondo.persistence_failures = getOption("futmondo.persistence_failures", 0L) + 1L,
    futmondo.persistence_issues = c(getOption("futmondo.persistence_issues", list()), list(issue)))
  message("[Persistence] table=", table_name, " category=", category,
    if (!is.na(status)) paste0(" HTTP=", status), if (nzchar(code)) paste0(" code=", code))
  invisible(issue)
}

supabase_post_direct <- function(table_name, payload, conflict = NULL, ignore_duplicates = FALSE) {
  if (isTRUE(getOption("futmondo.offline", FALSE))) return(NULL)
  if (is.null(payload) || (is.data.frame(payload) && nrow(payload) == 0) || (is.list(payload) && length(payload) == 0)) {
    return(NULL)
  }
  # Defensive check for loaded credentials
  sb_url <- get_sb_url()
  sb_key <- get_sb_key()
  if (is.null(sb_url) || sb_url == "" || is.null(sb_key) || sb_key == "") {
    record_persistence_failure(table_name, category = "configuration")
    return(NULL)
  }
  
  
  url <- paste0(sb_url, "/rest/v1/", table_name)
  
  headers <- c(
    "apikey" = sb_key,
    "Authorization" = paste("Bearer", sb_key),
    "Content-Type" = "application/json",
    "Prefer" = if (isTRUE(ignore_duplicates)) "resolution=ignore-duplicates" else "resolution=merge-duplicates"
  )
  
  # Perform request defensively
  tryCatch({
    response <- POST(url, query = if (!is.null(conflict)) list(on_conflict = conflict) else NULL, body = toJSON(payload, auto_unbox = TRUE, na = "null"), add_headers(.headers = headers), httr::timeout(15), httr::config(connecttimeout = 5))
    code <- status_code(response)
    if (code >= 200 && code < 300) {
      if(identical(table_name,"round_dream_team")) invalidate_mvp_saved_cache()
      print(paste0("[Supabase] Successfully synced data to table: ", table_name, " (HTTP ", code, ")"))
    } else {
      details <- tryCatch(jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8")),
        error = function(e) NULL)
      record_persistence_failure(table_name, code, if (is.list(details)) details$code else NULL)
    }
    return(code)
  }, error = function(e) {
    record_persistence_failure(table_name, category = "connection")
    return(NULL)
  })
}

sync_championship_to_supabase <- function(championship) {
  if (is.null(championship) || length(championship) == 0) return()
  
  payload <- list(
    id = as.character(championship["id"]),
    name = as.character(championship["name"]),
    mode = as.character(championship["mode"]),
    sport = as.character(championship["sport"])
  )
  
  supabase_post("championships", payload)
}

sync_real_clubs_to_supabase <- function(clubs_df) {
  if (!is.data.frame(clubs_df) || nrow(clubs_df) == 0 ||
      !"teamId" %in% names(clubs_df)) return(invisible(TRUE))

  ids <- trimws(as.character(clubs_df$teamId))
  club_names <- if ("team" %in% names(clubs_df)) trimws(as.character(clubs_df$team)) else rep(NA_character_, nrow(clubs_df))
  logos <- if ("logo" %in% names(clubs_df)) as.character(clubs_df$logo) else rep(NA_character_, nrow(clubs_df))
  known_name <- !is.na(club_names) & nzchar(club_names)
  # Unassigned players have no catalog identity. A nonempty ID with missing
  # enrichment still needs a row so the following player write has a valid FK.
  club_names[!known_name] <- ids[!known_name]
  payload <- data.frame(id = ids, name = club_names, logo = logos, stringsAsFactors = FALSE)
  payload <- payload[order(!known_name), , drop = FALSE]
  payload <- payload[!is.na(payload$id) & nzchar(payload$id), , drop = FALSE]
  payload <- dplyr::distinct(payload, id, .keep_all = TRUE)
  if (!nrow(payload)) return(invisible(TRUE))

  tryCatch(supabase_post("real_clubs", payload), error = function(e) {
    record_persistence_failure("real_clubs", category = "write")
    invisible(FALSE)
  })
}

sync_players_to_supabase <- function(players_df) {
  if (is.null(players_df) || nrow(players_df) == 0) return()

  # Ensure minimum expected columns are present
  required <- c("id", "name", "slug")
  if (!all(required %in% colnames(players_df))) return()

  # Match the club seed's normalized IDs. Missing/blank IDs represent an
  # unassigned player and become SQL NULL instead of an invalid foreign key.
  real_club_raw <- if ("teamId" %in% colnames(players_df)) trimws(as.character(players_df$teamId)) else rep(NA_character_, nrow(players_df))
  real_club_raw[is.na(real_club_raw) | !nzchar(real_club_raw)] <- NA_character_

  payload <- data.frame(
    id = as.character(players_df$id),
    name = as.character(players_df$name),
    slug = as.character(players_df$slug),
    photo = if ("photo" %in% colnames(players_df)) as.character(players_df$photo) else NA_character_,
    real_club_id = real_club_raw,
    stringsAsFactors = FALSE
  )

  # Dedup
  payload <- payload %>% dplyr::distinct(id, .keep_all = TRUE)

  # Batch large payloads to avoid payload-size limits on the Supabase REST endpoint
  batch_size <- 100
  n <- nrow(payload)
  print(paste0("[Supabase] Syncing ", n, " players to 'players' (batch size: ", batch_size, ")."))

  tryCatch({
    for (start_idx in seq(1, n, by = batch_size)) {
      end_idx <- min(start_idx + batch_size - 1, n)
      batch <- payload[start_idx:end_idx, , drop = FALSE]
      supabase_post("players", batch)
    }
  }, error = function(e) {
    print(paste0("[Supabase] Error during players sync: ", e$message))
  })
}

sync_user_teams_to_supabase <- function(teams_df, championship_id) {
  if (is.null(teams_df) || nrow(teams_df) == 0) return()
  
  # Standardize column naming
  team_ids <- if ("teamid" %in% colnames(teams_df)) teams_df$teamid else if ("id" %in% colnames(teams_df)) teams_df$id else return()
  team_names <- if ("teamname" %in% colnames(teams_df)) teams_df$teamname else if ("name" %in% colnames(teams_df)) teams_df$name else "Unknown Team"
  
  payload <- data.frame(
    id = as.character(team_ids),
    championship_id = as.character(championship_id),
    name = as.character(team_names),
    budget = if ("budget" %in% colnames(teams_df)) as.numeric(teams_df$budget) else NA_real_,
    points = if ("points" %in% colnames(teams_df)) as.integer(teams_df$points) else NA_real_,
    position = if ("position" %in% colnames(teams_df)) as.integer(teams_df$position) else NA_integer_,
    team_value = if ("team_value" %in% colnames(teams_df)) as.numeric(teams_df$team_value) else if ("teamValue" %in% colnames(teams_df)) as.numeric(teams_df$teamValue) else NA_real_,
    is_active = if ("is_active" %in% colnames(teams_df)) as.logical(teams_df$is_active) else TRUE,
    stringsAsFactors = FALSE
  )

  payload <- payload %>% dplyr::distinct(id, .keep_all = TRUE)

  supabase_post("user_teams", payload)
}

log_user_team_history <- function(teams_df, round_number = NULL, championship_id=NULL) {
  if (is.null(teams_df) || nrow(teams_df) == 0) return()
  teams_df<-preserve_observation_time(teams_df)
  if(identical(attr(teams_df,'fetch_status'),'stale'))return(invisible(FALSE))
  teams_df<-teams_df[!is.na(fm_time(teams_df$observed_at)),,drop=FALSE]
  if(!nrow(teams_df))return(invisible(FALSE))
  
  team_ids <- if ("teamid" %in% colnames(teams_df)) teams_df$teamid else if ("id" %in% colnames(teams_df)) teams_df$id else return()
  active_cnt <- length(unique(team_ids))
  
  payload <- data.frame(
    user_team_id = as.character(team_ids),
    observed_at = as.character(teams_df$observed_at),
    championship_id = if(!is.null(championship_id)) as.character(championship_id) else if("championship_id" %in% names(teams_df)) as.character(teams_df$championship_id) else NA_character_,
    points = if ("points" %in% colnames(teams_df)) as.integer(teams_df$points) else NA_real_,
    budget = if ("budget" %in% colnames(teams_df)) as.numeric(teams_df$budget) else NA_real_,
    position = if ("position" %in% colnames(teams_df)) as.integer(teams_df$position) else NA_integer_,
    team_value = if ("team_value" %in% colnames(teams_df)) as.numeric(teams_df$team_value) else if ("teamValue" %in% colnames(teams_df)) as.numeric(teams_df$teamValue) else NA_real_,
    round_number = if (!is.null(round_number)) as.integer(round_number) else if ("round_number" %in% colnames(teams_df)) as.integer(teams_df$round_number) else NA_integer_,
    active_teams_count = as.integer(active_cnt),
    stringsAsFactors = FALSE
  )

  supabase_post("user_team_history", payload)
}

log_player_history <- function(players_df, championship_id) {
  if (is.null(players_df) || nrow(players_df) == 0) return()
  if (!"id" %in% colnames(players_df)) return()
  players_df <- preserve_observation_time(players_df)
  if (identical(attr(players_df,'fetch_status'),'stale')) return(invisible(FALSE))
  players_df <- players_df[!is.na(fm_time(players_df$observed_at)),,drop=FALSE]
  if(!nrow(players_df)) return(invisible(FALSE))

  # Helper: coerce to numeric, replacing "NaN", NaN, or non-numeric with NA_real_
  safe_numeric <- function(x) {
    if (is.null(x)) return(rep(NA_real_, nrow(players_df)))
    vals <- suppressWarnings(as.numeric(as.character(x)))
    vals[is.na(vals) | is.nan(vals)] <- NA_real_
    vals
  }

  avg_points_val <- safe_numeric(
    if ("average.average" %in% colnames(players_df)) players_df$average.average else NULL
  )

  avg_last_five_val <- safe_numeric(
    if ("average.averageLastFive" %in% colnames(players_df)) players_df$average.averageLastFive else NULL
  )

  payload <- data.frame(
    player_id = as.character(players_df$id),
    championship_id = as.character(championship_id),
    observed_at = as.character(players_df$observed_at),
    value = if ("value" %in% colnames(players_df)) as.integer(players_df$value) else NA_integer_,
    change = if ("change" %in% colnames(players_df)) as.integer(players_df$change) else NA_integer_,
    points = if ("points" %in% colnames(players_df)) as.integer(players_df$points) else NA_real_,
    avg_points = avg_points_val,
    avg_last_five = avg_last_five_val,
    matches = if ("average.matches" %in% colnames(players_df)) as.integer(players_df$average.matches) else NA_real_,
    stringsAsFactors = FALSE
  )

  # Batch large payloads to avoid payload-size limits on the Supabase REST endpoint
  batch_size <- 100
  n <- nrow(payload)
  print(paste0("[Supabase] Syncing ", n, " player history records to 'player_history' (batch size: ", batch_size, ")."))

  tryCatch({
    for (start_idx in seq(1, n, by = batch_size)) {
      end_idx <- min(start_idx + batch_size - 1, n)
      batch <- payload[start_idx:end_idx, , drop = FALSE]
      supabase_post("player_history", batch)
    }
  }, error = function(e) {
    print(paste0("[Supabase] Error during player history sync: ", e$message))
  })
}

log_market_transaction <- function(player_id, championship_id, buyer_team_id, seller_team_id = NULL, price, is_clause = FALSE) {
  payload <- list(
    player_id = as.character(player_id),
    championship_id = as.character(championship_id),
    buyer_team_id = if (!is.null(buyer_team_id)) as.character(buyer_team_id) else NULL,
    seller_team_id = if (!is.null(seller_team_id)) as.character(seller_team_id) else NULL,
    price = as.numeric(price),
    is_clause = as.logical(is_clause)
  )
  
  supabase_post("market_transactions", payload)
}

supabase_get <- function(table_name, query_params = list()) {
  if (isTRUE(getOption("futmondo.offline", FALSE))) return(NULL)
  # Defensive check for loaded credentials
  sb_url <- get_sb_url()
  sb_key <- get_sb_key()
  if (is.null(sb_url) || sb_url == "" || is.null(sb_key) || sb_key == "") {
    return(NULL)
  }
  
  url <- paste0(sb_url, "/rest/v1/", table_name)
  
  headers <- c(
    "apikey" = sb_key,
    "Authorization" = paste("Bearer", sb_key),
    "Accept" = "application/json"
  )
  
  tryCatch({
    response <- GET(url, query = query_params, add_headers(.headers = headers), httr::timeout(15), httr::config(connecttimeout = 5))
    code <- status_code(response)
    if (code >= 200 && code < 300) {
      data <- fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
      return(as.data.frame(data))
    } else {
      print(paste0("[Supabase] Warning: Received GET HTTP code ", code, " from table: ", table_name))
      return(NULL)
    }
  }, error = function(e) {
    print(paste0("[Supabase] GET Connection error for table ", table_name, ": ", e$message))
    return(NULL)
  })
}

# Retrieve saved MVP selections for a league. The query is cached because the
# data changes only when the dream-team synchronisation writes a completed round.
get_round_mvps <- function(championship_id) {
  empty <- data.frame(
    championship_id = character(), round_id = character(), round_number = numeric(),
    player_id = character(), player_name = character(), player_role = character(),
    points = numeric(), is_mvp = logical(), is_finished = logical(), stringsAsFactors = FALSE
  )
  if (is.null(championship_id) || !length(championship_id) ||
      is.na(championship_id[1]) || !nzchar(as.character(championship_id[1]))) return(empty)
  cache_key <- paste0("round_mvps_", as.character(championship_id[1]))
  get_cached_data(cache_key, {
    rows <- supabase_read_all("round_dream_team", list(
      championship_id = paste0("eq.", as.character(championship_id[1])),
      is_mvp = "is.true",
      is_finished = "is.true",
      select = "championship_id,round_id,round_number,player_id,player_name,player_role,points,is_mvp,is_finished",
      order = "round_number.desc,player_id.asc"
    ))
    if (is.null(rows)) stop("Round MVP data is unavailable.")
    rows
  }, context=resource_context(account_id="database",championship_id=championship_id))
}

# Read finalized per-round points for one player. This is separate from daily
# valuation history, which only holds aggregate point totals.
get_player_round_points_history<-function(player_id,championship_id,season="unknown",scoring_version="unknown"){
 ctx<-resource_context(account_id="database",championship_id=championship_id,player_id=player_id,season=season,scoring_version=scoring_version)
 rows<-read_card_round_history(ctx);if(!is.data.frame(rows))return(rows)
 rows<-history_context_filter(rows,ctx);if(nrow(rows))rows<-rows[!duplicated(rows$round,fromLast=TRUE),,drop=FALSE];rows
}

get_player_historical_data<-function(player_id,championship_id){read_card_price_history(resource_context(account_id="database",championship_id=championship_id,player_id=player_id))}

get_league_standings_history <- function(championship_id) {
  query <- list(
    select = "points,budget,position,team_value,recorded_at,user_teams!inner(name,championship_id)",
    championship_id = paste0("eq.", championship_id),
    "user_teams.championship_id" = paste0("eq.", championship_id),
    order = "recorded_at.asc,id.asc"
  )
  
  df <- supabase_read_all("user_team_history", query)
  if (!is.null(df) && nrow(df) > 0 && "user_teams" %in% colnames(df)) {
    if (is.list(df$user_teams) || is.data.frame(df$user_teams)) {
      df$teamname <- df$user_teams$name
    } else {
      df$teamname <- "Unknown Team"
    }
    df$user_teams <- NULL
  }
  return(df)
}

get_user_teams_finances <- function(championship_id) {
  query <- list(
    championship_id = paste0("eq.", championship_id),
    select = "id,name,budget,points,position"
  )
  supabase_get("user_teams", query)
}

get_league_finances_history <- function(championship_id) {
  query <- list(
    select = "budget,team_value,points,position,recorded_at,user_teams!inner(name,championship_id)",
    championship_id = paste0("eq.", championship_id),
    "user_teams.championship_id" = paste0("eq.", championship_id),
    order = "recorded_at.asc,id.asc"
  )

  df <- supabase_read_all("user_team_history", query)
  if (!is.null(df) && nrow(df) > 0 && "user_teams" %in% colnames(df)) {
    if (is.list(df$user_teams) || is.data.frame(df$user_teams)) {
      df$teamname <- df$user_teams$name
    } else {
      df$teamname <- "Unknown Team"
    }
    df$user_teams <- NULL
  }
  return(df)
}

supabase_fact_batch_valid<-function(operations){
 is.list(operations)&&length(operations)>0L&&length(operations)<=2L&&all(vapply(operations,function(op)is.list(op)&&length(op)==4L&&is.character(op[[1]])&&length(op[[1]])==1L&&op[[1]]%in%c("players","market_transactions")&&is.data.frame(op[[2]])&&nrow(op[[2]])>0L&&nrow(op[[2]])<=100L,logical(1)))
}
supabase_fact_batch_direct<-function(operations){
 if(!supabase_fact_batch_valid(operations))return(FALSE)
 for(op in operations){code<-tryCatch(do.call(supabase_post_direct,op),error=function(e)NULL);if(!is.numeric(code)||length(code)!=1L||is.na(code)||code<200||code>=300)return(FALSE)}
 invisible(TRUE)
}
sync_pressroom_transactions_to_supabase<-function(pressroom_df,championship_id,deferred=FALSE){
 if(!is.data.frame(pressroom_df)||!nrow(pressroom_df))return(invisible(TRUE))
 required<-c("id","player_id","buyer_team_id","seller_team_id","price","created")
 if(!all(required%in%names(pressroom_df)))return(invisible(FALSE))
 ids<-as.character(pressroom_df$player_id)
 if(anyNA(ids)||any(!nzchar(trimws(ids)))){record_persistence_failure("players",category="write");return(FALSE)}
 d<-pressroom_df;labels<-as.character(d$player_name %||% d$player_id);unknown<-is.na(labels)|!nzchar(trimws(labels));labels[unknown]<-ids[unknown]
 parents<-data.frame(id=ids,name=labels,slug="",stringsAsFactors=FALSE);parents<-parents[order(unknown),,drop=FALSE];parents<-parents[!duplicated(parents$id),,drop=FALSE]
 buyer<-as.character(d$buyer_team_id);seller<-as.character(d$seller_team_id);buyer[is.na(buyer)|!nzchar(buyer)]<-NA_character_;seller[is.na(seller)|!nzchar(seller)]<-NA_character_
 payload<-data.frame(player_id=ids,championship_id=as.character(championship_id),buyer_team_id=buyer,seller_team_id=seller,price=suppressWarnings(as.numeric(d$price)),is_clause=FALSE,source_event_id=as.character(d$id),provenance="futmondo_pressroom",transaction_date=as.character(d$created))
 payload<-payload[!duplicated(payload$source_event_id),,drop=FALSE]
 domain<-if(requireNamespace("shiny",quietly=TRUE))shiny::getDefaultReactiveDomain()else NULL
 queued<-(!is.null(domain)||!is.null(getOption("futmondo.persistence_context")))&&exists("defer_persistence",mode="function")
 for(i in seq.int(1L,nrow(payload),by=100L)){
  batch<-payload[i:min(i+99L,nrow(payload)),,drop=FALSE];parent_batch<-parents[parents$id%in%batch$player_id,,drop=FALSE]
  operations<-list(list("players",parent_batch,"id",TRUE),list("market_transactions",batch,supabase_conflict_key("market_transactions"),FALSE))
  ok<-if(queued)defer_persistence("supabase_fact_batch_direct",list(operations))else supabase_fact_batch_direct(operations)
  if(!isTRUE(ok))return(FALSE)
 };invisible(TRUE)
}

get_pressroom_transactions_from_supabase <- function(championship_id) {
  if (is.null(championship_id) || championship_id == "") return(NULL)

  query <- list(
    championship_id = paste0("eq.", championship_id),
    select = "player_id,buyer_team_id,seller_team_id,price,created_at",order="created_at.asc,id.asc"
  )

  tryCatch({
    df <- supabase_read_all("market_transactions", query)
    return(df)
  }, error = function(e) {
    print(paste0("[Supabase] Error fetching pressroom transactions: ", e$message))
    return(NULL)
  })
}

# ============================================================
# Database Reset and Initialization Functions
# ============================================================

supabase_delete <- function(table_name, filter = "id=neq.00000000-0000-0000-0000-000000000000") {
  sb_url <- get_sb_url()
  sb_key <- get_sb_key()
  if (is.null(sb_url) || sb_url == "" || is.null(sb_key) || sb_key == "") {
    return(list(status = "skipped", reason = "credentials not loaded"))
  }

  url <- paste0(sb_url, "/rest/v1/", table_name)

  headers <- c(
    "apikey" = sb_key,
    "Authorization" = paste("Bearer", sb_key),
    "Accept" = "application/json",
    "Prefer" = "return=minimal, count=exact"
  )

  # Parse filter string "col=op.value" into a named list for the query parameter
  filter_parts <- strsplit(filter, "=", fixed = TRUE)[[1]]
  filter_list <- setNames(list(filter_parts[2]), filter_parts[1])

  tryCatch({
    response <- httr::DELETE(url, query = filter_list, add_headers(.headers = headers), httr::timeout(15))
    code <- status_code(response)
    if (code >= 200 && code < 300) {
      return(list(status = "deleted", http_code = code))
    } else {
      return(list(status = "error", http_code = code))
    }
  }, error = function(e) {
    return(list(status = "error", reason = e$message))
  })
}

supabase_primary_key <- function(table_name) {
  # A nonnullable identity component for count and delete filters.
  if(table_name %in% c("auction_observations","bid_observations","market_observations","league_rule_snapshots","observation_revisions")) "championship_id" else if(identical(table_name,"player_match_observations")) "player_id" else "id"
}

supabase_known_tables <- function() {
  c("championships", "real_clubs", "players", "user_teams", "user_team_history", "player_history", "market_transactions", "round_dream_team", "player_daily_snapshots", "auction_observations", "bid_observations", "market_observations", "player_match_observations", "league_rule_snapshots", "observation_revisions")
}

supabase_delete_all <- function(table_name) {
  if (!(table_name %in% supabase_known_tables())) {
    print(paste0("[Supabase] Unknown table: ", table_name))
    return(list(status = "error", reason = "unknown table"))
  }
  supabase_delete(table_name, filter = paste0(supabase_primary_key(table_name), "=not.is.null"))
}

supabase_reset_database <- function(force = FALSE) {
  if (!force) {
    print("[Supabase] Reset cancelled: set force = TRUE to proceed.")
    return(list())
  }

  reset_order <- c(
    "observation_revisions", "bid_observations", "auction_observations", "market_observations", "player_match_observations", "league_rule_snapshots",
    "player_daily_snapshots",
    "round_dream_team",
    "market_transactions",
    "player_history",
    "user_team_history",
    "user_teams",
    "players",
    "real_clubs",
    "championships"
  )

  results <- list()

  for (tbl in reset_order) {
    cat(paste0("[Reset] Deleting all rows from: ", tbl, " ... "))
    res <- supabase_delete_all(tbl)
    results[[tbl]] <- paste0(res$status,
                              if (!is.null(res$http_code)) paste0(" (HTTP ", res$http_code, ")") else "",
                              if (!is.null(res$reason)) paste0(": ", res$reason) else "")
    cat(results[[tbl]], "\n")
  }

  return(results)
}

# ============================================================
# Row Count Helper
# ============================================================

get_table_row_counts <- function() {
  sb_url <- get_sb_url()
  sb_key <- get_sb_key()
  if (is.null(sb_url) || sb_url == "" || is.null(sb_key) || sb_key == "") {
    warning("[Row Counts] Supabase credentials not loaded in .Renviron. Returning empty result.")
    return(data.frame(table_name = character(), row_count = integer(), stringsAsFactors = FALSE))
  }

  tables <- supabase_known_tables()

  results <- vector("list", length(tables))
  names(results) <- tables

  for (tbl in tables) {
    url <- paste0(sb_url, "/rest/v1/", tbl)

    headers <- c(
      "apikey" = sb_key,
      "Authorization" = paste("Bearer", sb_key),
      "Accept" = "application/json",
      "Prefer" = "count=exact",
      "Range-Unit" = "items",
      "Range" = "0-0"
    )

    tryCatch({
      response <- GET(url, query = list(select = supabase_primary_key(tbl), limit = "0"), add_headers(.headers = headers), httr::timeout(15))
      code <- status_code(response)

      if (code >= 200 && code < 300) {
        content_range <- response$headers[["content-range"]]
        if (!is.null(content_range) && length(content_range) > 0) {
          # content-range looks like "0-0/999" or "items 0-0/999"
          parts <- strsplit(content_range, "/")[[1]]
          total <- as.integer(trimws(parts[length(parts)]))
        } else {
          # An absent count header cannot establish an empty table.
          total <- NA_integer_
        }
        results[[tbl]] <- total
      } else {
        results[[tbl]] <- NA_integer_
      }
    }, error = function(e) {
      results[[tbl]] <- NA_integer_
    })
  }

  df <- data.frame(
    table_name = names(results),
    row_count = unlist(results),
    stringsAsFactors = FALSE
  )

  return(df)
}


init_supabase_db <- function(verbose = FALSE) {
  sb_url <- get_sb_url()
  sb_key <- get_sb_key()
  if (is.null(sb_url) || sb_url == "" || is.null(sb_key) || sb_key == "") {
    warning("[Init] Supabase credentials not loaded in .Renviron. Skipping database verification.")
    return(FALSE)
  }

  required_tables <- supabase_known_tables()

  all_ok <- TRUE

  for (tbl in required_tables) {
    url <- paste0(sb_url, "/rest/v1/", tbl)

    headers <- c(
      "apikey" = sb_key,
      "Authorization" = paste("Bearer", sb_key),
      "Accept" = "application/json"
    )

    tryCatch({
      response <- GET(url, query = list(select = supabase_primary_key(tbl), limit = "1"), add_headers(.headers = headers), httr::timeout(15))
      code <- status_code(response)
      if (code >= 200 && code < 300) {
        if (verbose) cat(paste0("[Init] Table OK: ", tbl, " (HTTP ", code, ")\n"))
      } else {
        all_ok <- FALSE
        response_body <- tryCatch(
          httr::content(response, as = "text", encoding = "UTF-8"),
          error = function(e) ""
        )
        body_suffix <- if (is.character(response_body) && length(response_body) == 1L && nzchar(response_body)) paste0(": ", response_body) else ""
        warning(paste0("[Init] Table check failed for '", tbl, "': HTTP ", code, body_suffix))
        if (verbose) cat(paste0("[Init] Table FAIL: ", tbl, " (HTTP ", code, ")\n"))
      }
    }, error = function(e) {
      all_ok <<- FALSE
      warning(paste0("[Init] Table check error for '", tbl, "': ", e$message))
      if (verbose) cat(paste0("[Init] Table ERROR: ", tbl, " (", e$message, ")\n"))
    })
  }

  return(all_ok)
}

# ============================================================
# Round Dream Team Sync
# ============================================================

# ============================================================
# Observed daily snapshots
# ============================================================

log_player_daily_snapshots <- function(players_df, championship_id) {
  if (is.null(players_df) || nrow(players_df) == 0) return()
  if (!"id" %in% colnames(players_df)) return()
  players_df<-preserve_observation_time(players_df)
  if(identical(attr(players_df,'fetch_status'),'stale'))return(invisible(FALSE))
  players_df<-players_df[!is.na(fm_time(players_df$observed_at)),,drop=FALSE]
  if(!nrow(players_df))return(invisible(FALSE))

  # Helper: safe numeric coercion
  safe_num <- function(x, default = NA_real_) {
    if (is.null(x)) return(rep(default, nrow(players_df)))
    vals <- suppressWarnings(as.numeric(as.character(x)))
    vals[is.na(vals) | is.nan(vals)] <- default
    vals
  }

  payload <- data.frame(
    player_id = as.character(players_df$id),
    championship_id = as.character(championship_id),
    value = if ("value" %in% colnames(players_df)) as.integer(safe_num(players_df$value)) else rep(NA_integer_, nrow(players_df)),
    daily_change = if ("change" %in% colnames(players_df)) as.integer(safe_num(players_df$change)) else rep(NA_integer_, nrow(players_df)),
    points = if ("points" %in% colnames(players_df)) as.integer(safe_num(players_df$points)) else rep(NA_integer_, nrow(players_df)),
    status = if ("status" %in% colnames(players_df)) as.character(players_df$status) else rep("ok", nrow(players_df)),
    snapshot_date = format(fm_time(players_df$observed_at),'%Y-%m-%d',tz='UTC'),
    observed_at = as.character(players_df$observed_at),
    stringsAsFactors = FALSE
  )

  # Eligibility and observed ratings vary by league and belong in snapshots.
  for(field in c("role","role2","primary_role")) payload[[field]] <- if(field %in% names(players_df)) as.character(players_df[[field]]) else NA_character_
  payload$rating <- if("rating" %in% names(players_df)) safe_num(players_df$rating) else NA_real_
  # Add optional columns if present
  if (any(c("userteamId", "user_team_id") %in% colnames(players_df))) {
    owner_ids <- as.character(if ("userteamId" %in% names(players_df)) players_df$userteamId else players_df$user_team_id)
    owner_ids[is.na(owner_ids) | owner_ids == ""] <- NA_character_
    payload$owner_team_id <- owner_ids
  } else {
    payload$owner_team_id <- rep(NA_character_, nrow(players_df))
  }

  if ("market_inMarket" %in% colnames(players_df)) {
    payload$is_on_market <- as.logical(players_df$market_inMarket)
  } else {
    payload$is_on_market <- rep(NA, nrow(players_df))
  }

  if ("clause_price" %in% colnames(players_df)) {
    payload$clause_price <- as.integer(safe_num(players_df$clause_price, NA_real_))
  } else {
    payload$clause_price <- rep(NA_integer_, nrow(players_df))
  }

  # Deduplicate by player_id + championship_id + snapshot_date
  payload <- payload %>% dplyr::distinct(player_id, championship_id, snapshot_date, .keep_all = TRUE)

  batch_size <- 100
  n <- nrow(payload)
  print(paste0("[Supabase] Syncing ", n, " player daily snapshots (batch size: ", batch_size, ")."))

  tryCatch({
    for (start_idx in seq(1, n, by = batch_size)) {
      end_idx <- min(start_idx + batch_size - 1, n)
      batch <- payload[start_idx:end_idx, , drop = FALSE]
      supabase_post("player_daily_snapshots", batch)
    }
  }, error = function(e) {
    print(paste0("[Supabase] Error during player_daily_snapshots sync: ", e$message))
  })
}

supabase_conflict_key <- function(table_name) {
  keys <- c(user_team_history="user_team_id,championship_id,observed_at",
    player_history = "player_id,championship_id,observed_at",
    player_daily_snapshots = "player_id,championship_id,snapshot_date",
    round_dream_team = "championship_id,round_number,player_id",
    market_transactions = "championship_id,source_event_id",
    auction_observations = "championship_id,auction_id",
    bid_observations = "championship_id,auction_id,bidder_id",
    market_observations = "championship_id,listing_id,observed_at",
    player_match_observations = "player_id,championship_id,season,scoring_version,round,observed_at",
    league_rule_snapshots = "championship_id,scoring_version,observed_at",
    sale_observations="user_id,championship_id,offer_id,observed_at",
    fixture_observations="championship_id,season,scoring_version,player_id,round,observed_at",
    auction_opportunities="championship_id,listing_id,observed_at")
  if (table_name %in% names(keys)) unname(keys[[table_name]]) else NULL
}

supabase_post<-function(table_name,payload,conflict=supabase_conflict_key(table_name),ignore_duplicates=FALSE){
 if(is.null(payload)||(is.data.frame(payload)&&!nrow(payload)))return(invisible(204L))
 if(isTRUE(getOption("futmondo.offline",FALSE)))return(invisible(204L))
 if(is.data.frame(payload)&&nrow(payload)>100L){
  results<-lapply(seq.int(1L,nrow(payload),by=100L),function(i)supabase_post(table_name,payload[i:min(i+99L,nrow(payload)),,drop=FALSE],conflict,ignore_duplicates))
  return(invisible(if(all(vapply(results,function(x)isTRUE(x)||(is.numeric(x)&&length(x)==1L&&x>=200&&x<300),logical(1))))TRUE else FALSE))
 }
 domain<-if(requireNamespace("shiny",quietly=TRUE))shiny::getDefaultReactiveDomain()else NULL
 if((!is.null(domain)||!is.null(getOption("futmondo.persistence_context")))&&exists("defer_persistence",mode="function"))return(defer_persistence("supabase_post_direct",list(table_name,payload,conflict,ignore_duplicates)))
 supabase_post_direct(table_name,payload,conflict,ignore_duplicates)
}

supabase_read_all<-function(table_name,query=list(),max_rows=20000L){
 if(is.null(query$order))query$order<-switch(table_name,player_match_observations="season.asc,scoring_version.asc,player_id.asc,round.asc,observed_at.asc",auction_observation_history="observed_at.asc,id.asc",bid_observation_history="observed_at.asc,id.asc","recorded_at.asc,id.asc")
 history_read_pages(table_name,query,max_rows=max_rows)
}

read_player_price_history <- function(championship_id, player_ids = NULL) {
  q <- list(championship_id = paste0("eq.", championship_id),
    select = "player_id,value,observed_at,recorded_at", observed_at="not.is.null", order = "observed_at.asc")
  if (length(player_ids)) q$player_id <- paste0("in.(", paste(player_ids, collapse = ","), ")")
  d <- tryCatch(supabase_read_all("player_history", q), error = function(e) NULL)
  if (!is.null(d) && "recorded_at" %in% names(d)) names(d)[names(d) == "recorded_at"] <- "captured_at"
  d
}

read_auction_observations <- function(championship_id, cutoff=Sys.time()) {
  read_observation_revisions('auction_observation_history',championship_id,cutoff,c('championship_id','auction_id'))
}
read_bid_observations <- function(championship_id, cutoff=Sys.time()) {
  read_observation_revisions('bid_observation_history',championship_id,cutoff,c('championship_id','auction_id','bidder_id'))
}

read_observation_revisions <- function(table, championship_id, cutoff, keys) {
  d <- tryCatch(supabase_read_all(table,list(championship_id=paste0('eq.',championship_id),
    observed_at=paste0('lte.',format(fm_time(cutoff),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')),
    legacy_timing='eq.false',order='observed_at.asc')),error=function(e)NULL)
  if(!is.data.frame(d)||!nrow(d)) return(d)
  d <- d[!is.na(fm_time(d$observed_at)) & fm_time(d$observed_at)<=fm_time(cutoff),,drop=FALSE]
  d <- d[order(fm_time(d$observed_at)),,drop=FALSE]
  d[!duplicated(d[keys],fromLast=TRUE),,drop=FALSE]
}

supabase_patch <- function(table_name, payload, filters) {
  if (isTRUE(getOption("futmondo.offline", FALSE))) return(FALSE)
  tryCatch({
    r <- httr::PATCH(paste0(get_sb_url(), "/rest/v1/", table_name), query = filters,
      body = jsonlite::toJSON(payload, auto_unbox = TRUE, na = "null"),
      httr::add_headers(apikey = get_sb_key(), Authorization = paste("Bearer", get_sb_key()),
        `Content-Type` = "application/json"), httr::timeout(15))
    httr::status_code(r) %in% 200:299
  }, error = function(e) FALSE)
}

persist_auction_history <- function(pressroom_df, championship_id) {
  stamp <- attr(pressroom_df,'observed_at')
  if(is.null(stamp) || identical(attr(pressroom_df,'fetch_status'),'stale')) return(invisible(FALSE))
  sync_pressroom_transactions_to_supabase(pressroom_df, championship_id)
  supabase_post("auction_observations", normalize_auction_observations(pressroom_df, championship_id,stamp))
  supabase_post("bid_observations", normalize_bid_observations(pressroom_df, championship_id,stamp))
  invisible(TRUE)
}

read_player_match_history<-function(championship_id,cutoff=Sys.time(),final_only=TRUE,season="unknown",scoring_version="unknown",player_ids=NULL,round_min=NULL,round_max=NULL,max_rows=5000L){
 q<-list(championship_id=paste0("eq.",championship_id),season=paste0("eq.",season),scoring_version=paste0("eq.",scoring_version),observed_at=paste0("lte.",format(fm_time(cutoff),"%Y-%m-%dT%H:%M:%OSZ",tz="UTC")),order="player_id.asc,round.asc,observed_at.asc")
 q$select<-"player_id,championship_id,season,scoring_version,round,points,score_status,observed_at"
 if(!is.null(round_min))q$round<-paste0("gte.",round_min)
 if(!is.null(round_max))q$and<-paste0("(round.lte.",round_max,")")
 if(length(player_ids))q$player_id<-paste0("in.(",paste(unique(player_ids),collapse=","),")")
 if(final_only)q$score_status<-"eq.final"
 d<-supabase_read_all("player_match_observations",q,max_rows=max_rows);if(!is.data.frame(d)||!nrow(d))return(d)
 keep<-d$championship_id==championship_id & d$season==season & d$scoring_version==scoring_version & !is.na(fm_time(d$observed_at)) & fm_time(d$observed_at)<=fm_time(cutoff)
 if(final_only)keep<-keep & d$score_status=="final"
 d<-d[keep%in%TRUE,,drop=FALSE];d<-d[order(d$player_id,d$round,fm_time(d$observed_at)),,drop=FALSE]
 d$is_final<-d$score_status=="final";d$round_id<-paste(d$season,d$scoring_version,d$round,sep=":")
 d[!duplicated(d[c("player_id","season","scoring_version","round")],fromLast=TRUE),,drop=FALSE]
}
source("history_repository.R")
