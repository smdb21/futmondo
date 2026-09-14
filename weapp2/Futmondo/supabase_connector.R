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
  issue <- list(table = table_name, category = category, http_status = status, code = code)
  options(futmondo.persistence_failures = getOption("futmondo.persistence_failures", 0L) + 1L,
    futmondo.persistence_issues = c(getOption("futmondo.persistence_issues", list()), list(issue)))
  message("[Persistence] table=", table_name, " category=", category,
    if (!is.na(status)) paste0(" HTTP=", status), if (nzchar(code)) paste0(" code=", code))
  invisible(issue)
}

supabase_post_direct <- function(table_name, payload, conflict = NULL) {
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
    "Prefer" = "resolution=merge-duplicates" # Upsert on PK matching
  )
  
  # Perform request defensively
  tryCatch({
    response <- POST(url, query = if (!is.null(conflict)) list(on_conflict = conflict) else NULL, body = toJSON(payload, auto_unbox = TRUE, na = "null"), add_headers(.headers = headers), httr::timeout(15), httr::config(connecttimeout = 5))
    code <- status_code(response)
    if (code >= 200 && code < 300) {
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
    points = if ("points" %in% colnames(teams_df)) as.integer(teams_df$points) else 0,
    position = if ("position" %in% colnames(teams_df)) as.integer(teams_df$position) else NA_integer_,
    team_value = if ("team_value" %in% colnames(teams_df)) as.numeric(teams_df$team_value) else if ("teamValue" %in% colnames(teams_df)) as.numeric(teams_df$teamValue) else 0,
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
    points = if ("points" %in% colnames(teams_df)) as.integer(teams_df$points) else 0,
    budget = if ("budget" %in% colnames(teams_df)) as.numeric(teams_df$budget) else NA_real_,
    position = if ("position" %in% colnames(teams_df)) as.integer(teams_df$position) else NA_integer_,
    team_value = if ("team_value" %in% colnames(teams_df)) as.numeric(teams_df$team_value) else if ("teamValue" %in% colnames(teams_df)) as.numeric(teams_df$teamValue) else 0,
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
    value = if ("value" %in% colnames(players_df)) as.integer(players_df$value) else 0L,
    change = if ("change" %in% colnames(players_df)) as.integer(players_df$change) else 0L,
    points = if ("points" %in% colnames(players_df)) as.integer(players_df$points) else 0,
    avg_points = avg_points_val,
    avg_last_five = avg_last_five_val,
    matches = if ("average.matches" %in% colnames(players_df)) as.integer(players_df$average.matches) else 0,
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
    rows <- supabase_get("round_dream_team", list(
      championship_id = paste0("eq.", as.character(championship_id[1])),
      is_mvp = "is.true",
      is_finished = "is.true",
      select = "championship_id,round_id,round_number,player_id,player_name,player_role,points,is_mvp,is_finished",
      order = "round_number.desc"
    ))
    if (is.null(rows)) stop("Round MVP data is unavailable.")
    rows
  })
}

get_player_historical_data <- function(player_id, championship_id) {
  query <- list(
    player_id = paste0("eq.", player_id),
    championship_id = paste0("eq.", championship_id),
    select = "value,change,points,recorded_at",
    order = "recorded_at.asc"
  )
  supabase_get("player_history", query)
}

get_league_standings_history <- function(championship_id) {
  query <- list(
    select = "points,budget,position,team_value,recorded_at,user_teams!inner(name,championship_id)",
    championship_id = paste0("eq.", championship_id),
    "user_teams.championship_id" = paste0("eq.", championship_id),
    order = "recorded_at.asc"
  )
  
  df <- supabase_get("user_team_history", query)
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
    order = "recorded_at.asc"
  )

  df <- supabase_get("user_team_history", query)
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

sync_pressroom_transactions_to_supabase <- function(pressroom_df, championship_id) {
  if (is.null(pressroom_df) || nrow(pressroom_df) == 0) return()

  required_cols <- c("player_id", "buyer_team_id", "seller_team_id", "price", "created")
  if (!all(required_cols %in% colnames(pressroom_df))) return()

  # Sanitize buyer_team_id and seller_team_id: empty strings or invalid values become NA_character_
  # which serializes to null in JSON for PostgreSQL ON DELETE SET NULL FK compatibility
  buyer_ids <- as.character(pressroom_df$buyer_team_id)
  seller_ids <- as.character(pressroom_df$seller_team_id)
  buyer_ids[buyer_ids == "" | is.na(buyer_ids)] <- NA_character_
  seller_ids[seller_ids == "" | is.na(seller_ids)] <- NA_character_

  payload <- data.frame(
    player_id = as.character(pressroom_df$player_id),
    championship_id = as.character(championship_id),
    buyer_team_id = buyer_ids,
    seller_team_id = seller_ids,
    price = as.numeric(pressroom_df$price),
    is_clause = FALSE,
    source_event_id = as.character(pressroom_df$id),
    provenance = "futmondo_pressroom",
    transaction_date = as.character(pressroom_df$created),
    stringsAsFactors = FALSE
  )

  # Deduplicate by player_id + championship_id + buyer_team_id + seller_team_id + price + transaction_date
  payload <- payload %>% dplyr::distinct(player_id, championship_id, buyer_team_id, seller_team_id, price, transaction_date, .keep_all = TRUE)

  # Batch large payloads to avoid payload-size limits on the Supabase REST endpoint
  batch_size <- 200
  n <- nrow(payload)
  print(paste0("[Supabase] Syncing ", n, " pressroom transactions to market_transactions (batch size: ", batch_size, ")."))

  tryCatch({
    for (start_idx in seq(1, n, by = batch_size)) {
      end_idx <- min(start_idx + batch_size - 1, n)
      batch <- payload[start_idx:end_idx, , drop = FALSE]
      supabase_post("market_transactions", batch)
    }
  }, error = function(e) {
    print(paste0("[Supabase] Error during pressroom transaction sync: ", e$message))
  })
}

get_pressroom_transactions_from_supabase <- function(championship_id) {
  if (is.null(championship_id) || championship_id == "") return(NULL)

  query <- list(
    championship_id = paste0("eq.", championship_id),
    select = "player_id,buyer_team_id,seller_team_id,price,created_at"
  )

  tryCatch({
    df <- supabase_get("market_transactions", query)
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
  if (identical(table_name, "manager_dna_profiles")) "team_id" else "id"
}

supabase_known_tables <- function() {
  c("championships", "real_clubs", "players", "user_teams", "user_team_history", "player_history", "market_transactions", "round_dream_team", "player_daily_snapshots", "manager_dna_profiles", "decision_log", "user_smart_alerts")
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
    "user_smart_alerts",
    "decision_log",
    "manager_dna_profiles",
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

  tables <- c(
    "championships",
    "real_clubs",
    "players",
    "user_teams",
    "user_team_history",
    "player_history",
    "market_transactions",
    "round_dream_team",
    "player_daily_snapshots",
    "manager_dna_profiles",
    "decision_log",
    "user_smart_alerts"
  )

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
          # Fallback: if no content-range, try counting via a minimal select
          body_text <- httr::content(response, as = "text", encoding = "UTF-8")
          total <- 0L
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

  required_tables <- c(
    "championships",
    "real_clubs",
    "players",
    "user_teams",
    "user_team_history",
    "player_history",
    "market_transactions",
    "round_dream_team",
    "player_daily_snapshots",
    "manager_dna_profiles",
    "decision_log",
    "user_smart_alerts"
  )

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

sync_round_dreamteam_to_supabase <- function(login, championship_id, round_id, round_number) {
  if (is.null(login) || is.null(championship_id) || is.null(round_id) || is.null(round_number)) {
    return(0L)
  }

  tryCatch({
    ans <- get_round_dreamteam(login, championship_id, round_id)

    if (is.null(ans) || !is.list(ans) || !("players" %in% names(ans)) || !("mvp" %in% names(ans))) {
      print(paste0("[DreamTeam] No valid dream team data for round ", round_number, "."))
      return(0L)
    }

    players_list <- ans$players
    mvp_id <- as.character(ans$mvp)

    if (is.null(players_list) || length(players_list) == 0) {
      print(paste0("[DreamTeam] No players in dream team for round ", round_number, "."))
      return(0L)
    }

    dreamteam_df <- do.call(rbind, lapply(players_list, function(p) {
      data.frame(
        championship_id = as.character(championship_id),
        round_id = as.character(round_id),
        round_number = as.numeric(round_number),
        player_id = as.character(p$id),
        player_name = as.character(p$name),
        player_role = as.character(p$role),
        points = as.integer(p$points),
        is_mvp = (as.character(p$id) == mvp_id),
        is_finished = TRUE,
        stringsAsFactors = FALSE
      )
    }))

    supabase_post("round_dream_team", dreamteam_df)

    count <- nrow(dreamteam_df)
    print(paste0("[DreamTeam] Synced ", count, " players for round ", round_number, "."))
    return(count)
  }, error = function(e) {
    print(paste0("[DreamTeam] Error syncing round ", round_number, ": ", e$message))
    return(0L)
  })
}

sync_all_championship_dreamteams <- function(login, championship_id, verbose = TRUE) {
  if (is.null(login) || is.null(championship_id)) {
    if (verbose) print("[DreamTeam] Missing login or championship_id. Skipping.")
    return(list(status = "skipped", total_rounds = 0L, total_players = 0L))
  }

  tryCatch({
    finished_rounds <- get_finished_rounds(login, championship_id)

    if (is.null(finished_rounds) || nrow(finished_rounds) == 0) {
      if (verbose) print("[DreamTeam] No finished rounds found.")
      return(list(status = "ok", total_rounds = 0L, total_players = 0L))
    }

    finished <- finished_rounds[finished_rounds$is_finished == TRUE, ]

    if (nrow(finished) == 0) {
      if (verbose) print("[DreamTeam] No finished rounds to sync.")
      return(list(status = "ok", total_rounds = 0L, total_players = 0L))
    }

    if (verbose) print(paste0("[DreamTeam] Syncing dream teams for ", nrow(finished), " finished round(s)."))

    total_players <- 0L
    round_results <- list()

    for (i in seq_len(nrow(finished))) {
      r_id <- as.character(finished$round_id[i])
      r_num <- as.numeric(finished$round_number[i])

      if (verbose) cat(paste0("  [DreamTeam] Round ", r_num, "... "))

      synced <- sync_round_dreamteam_to_supabase(login, championship_id, r_id, r_num)
      total_players <- total_players + synced
      round_results[[as.character(r_num)]] <- synced
    }

    if (verbose) print(paste0("[DreamTeam] Complete. Total players synced: ", total_players))
    return(list(status = "ok", total_rounds = nrow(finished), total_players = total_players, per_round = round_results))
  }, error = function(e) {
    print(paste0("[DreamTeam] Error syncing all dream teams: ", e$message))
    return(list(status = "error", message = e$message, total_rounds = 0L, total_players = 0L))
  })
}

# ============================================================
# Intelligence Engine Sync Functions
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
    value = if ("value" %in% colnames(players_df)) as.integer(safe_num(players_df$value, 0)) else rep(0L, nrow(players_df)),
    daily_change = if ("change" %in% colnames(players_df)) as.integer(safe_num(players_df$change, 0)) else rep(0L, nrow(players_df)),
    points = if ("points" %in% colnames(players_df)) as.integer(safe_num(players_df$points, 0)) else rep(0L, nrow(players_df)),
    fis_score = if ("fis_score" %in% colnames(players_df)) safe_num(players_df$fis_score, NA_real_) else rep(NA_real_, nrow(players_df)),
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

sync_manager_dna_profiles <- function(dna_df, championship_id) {
  if (is.null(dna_df) || nrow(dna_df) == 0) return()

  # dna_df is expected to have columns: team_id, aggressiveness, avg_overpayment_pct,
  # fav_position, trading_frequency, avg_holding_days, total_trades
  required_cols <- c("team_id", "aggressiveness", "avg_overpayment_pct",
                     "fav_position", "trading_frequency", "avg_holding_days", "total_trades")
  missing <- setdiff(required_cols, colnames(dna_df))
  if (length(missing) > 0) {
    print(paste0("[Supabase] sync_manager_dna_profiles: missing columns: ", paste(missing, collapse = ", ")))
    return()
  }

  payload <- data.frame(
    team_id = as.character(dna_df$team_id),
    championship_id = as.character(championship_id),
    aggressiveness = suppressWarnings(as.numeric(dna_df$aggressiveness)),
    avg_overpayment_pct = suppressWarnings(as.numeric(dna_df$avg_overpayment_pct)),
    fav_position = as.character(dna_df$fav_position),
    trading_frequency = suppressWarnings(as.numeric(dna_df$trading_frequency)),
    avg_holding_days = suppressWarnings(as.numeric(dna_df$avg_holding_days)),
    total_trades = as.integer(dna_df$total_trades),
    stringsAsFactors = FALSE
  )

  payload <- payload %>% dplyr::distinct(team_id, .keep_all = TRUE)

  print(paste0("[Supabase] Syncing ", nrow(payload), " manager DNA profiles."))

  tryCatch({
    supabase_post("manager_dna_profiles", payload)
  }, error = function(e) {
    print(paste0("[Supabase] Error during manager_dna_profiles sync: ", e$message))
  })
}

log_decision <- function(championship_id, user_team_id, player_id, recommendation_type,
                         recommended_value, actual_action, confidence, roi) {
  payload <- list(
    championship_id = as.character(championship_id),
    user_team_id = as.character(user_team_id),
    player_id = as.character(player_id),
    recommendation_type = as.character(recommendation_type),
    recommended_value = if (!is.null(recommended_value)) as.numeric(recommended_value) else NULL,
    actual_action_taken = if (!is.null(actual_action)) as.character(actual_action) else NULL,
    confidence_pct = if (!is.null(confidence)) as.numeric(confidence) else NULL,
    outcome_roi = if (!is.null(roi)) as.numeric(roi) else NULL
  )

  tryCatch({
    supabase_post("decision_log", payload)
  }, error = function(e) {
    print(paste0("[Supabase] Error during decision_log insert: ", e$message))
  })
}

fetch_user_smart_alerts <- function(user_team_id, championship_id, user_id = NULL) {
  if (is.null(user_id)) {
    domain <- shiny::getDefaultReactiveDomain()
    user_id <- if (!is.null(domain)) domain$userData$futmondo_user_id else NULL
  }
  if (is.null(user_id) || !nzchar(user_id)) return(NULL)
  if (is.null(user_team_id) || user_team_id == "") return(NULL)
  if (is.null(championship_id) || championship_id == "") return(NULL)

  query <- list(
    user_team_id = paste0("eq.", user_team_id),
    championship_id = paste0("eq.", championship_id),
    select = "id,alert_type,title,message,severity,is_read,created_at,championship_id,player_id",
    user_id = paste0("eq.", user_id),
    order = "created_at.desc"
  )

  tryCatch({
    df <- supabase_read_all("user_smart_alerts", query)
    return(df)
  }, error = function(e) {
    print(paste0("[Supabase] Error fetching user_smart_alerts: ", e$message))
    return(NULL)
  })
}

# ============================================================
# Full Database Population
# ============================================================

populate_entire_database <- function(login, championship_id, verbose = TRUE) {
  results <- list()

  tryCatch({
    # ---- Step 1: Sync Championship ----
    if (verbose) cat("[Populate] Step 1: Syncing championship...\n")
    tryCatch({
      championships_data <- get_championships(login, championship_name = NULL)

      if (!is.null(championships_data) && length(championships_data) > 0) {
        # get_championships returns an unlisted vector; names encode structure as "index.field"
        prefixes <- sub("\\..*", "", names(championships_data))
        unique_prefixes <- unique(prefixes)

        synced <- 0L
        for (pfx in unique_prefixes) {
          idx <- startsWith(names(championships_data), paste0(pfx, "."))
          champ <- championships_data[idx]

          # If championship_id is specified, only sync that championship
          champ_id_val <- as.character(champ["id"])
          if (!is.null(championship_id) && !is.na(championship_id) && championship_id != "" &&
              !is.na(champ_id_val) && champ_id_val != as.character(championship_id)) {
            next
          }

          payload <- list(
            id = champ_id_val,
            name = as.character(champ["name"]),
            mode = as.character(champ["mode"]),
            sport = as.character(champ["sport"])
          )
          supabase_post("championships", payload)
          synced <- synced + 1L
        }
        if (verbose) print(paste0("[Populate] Synced ", synced, " championship(s)."))
        results[["championships"]] <- list(status = "ok", count = synced)
      } else {
        if (verbose) print("[Populate] No championship data retrieved.")
        results[["championships"]] <- list(status = "ok", count = 0L)
      }
    }, error = function(e) {
      if (verbose) print(paste0("[Populate] Step 1 FAILED: ", e$message))
      results[["championships"]] <- list(status = "error", message = e$message)
    })

    # ---- Step 2: Sync Real Clubs ----
    if (verbose) cat("[Populate] Step 2: Syncing real clubs...\n")
    tryCatch({
      clubs <- get_real_clubs(login, championship_id)
      if (verbose) print(paste0("[Populate] Retrieved ", if (!is.null(clubs) && nrow(clubs) > 0) nrow(clubs) else 0, " real clubs."))
      sync_real_clubs_to_supabase(clubs)
      results[["real_clubs"]] <- list(status = "ok", count = if (!is.null(clubs) && nrow(clubs) > 0) nrow(clubs) else 0L)
    }, error = function(e) {
      if (verbose) print(paste0("[Populate] Step 2 FAILED: ", e$message))
      results[["real_clubs"]] <- list(status = "error", message = e$message)
    })

    # ---- Step 3: Sync Players Catalog ----
    if (verbose) cat("[Populate] Step 3: Syncing players catalog...\n")
    tryCatch({
      players <- get_championship_players(login, championship_id)
      if (verbose) print(paste0("[Populate] Retrieved ", if (!is.null(players) && nrow(players) > 0) nrow(players) else 0, " players."))
      sync_players_to_supabase(players)
      results[["players"]] <- list(status = "ok", count = if (!is.null(players) && nrow(players) > 0) nrow(players) else 0L)
    }, error = function(e) {
      if (verbose) print(paste0("[Populate] Step 3 FAILED: ", e$message))
      results[["players"]] <- list(status = "error", message = e$message)
    })

    # ---- Step 4: Sync User Teams ----
    if (verbose) cat("[Populate] Step 4: Syncing user teams...\n")
    tryCatch({
      teams <- get_teams(login, championship_id)
      if (verbose) print(paste0("[Populate] Retrieved ", if (!is.null(teams) && nrow(teams) > 0) nrow(teams) else 0, " user teams."))
      sync_user_teams_to_supabase(teams, championship_id)
      results[["user_teams"]] <- list(status = "ok", count = if (!is.null(teams) && nrow(teams) > 0) nrow(teams) else 0L)
    }, error = function(e) {
      if (verbose) print(paste0("[Populate] Step 4 FAILED: ", e$message))
      results[["user_teams"]] <- list(status = "error", message = e$message)
    })

    # ---- Step 5: Sync Standings Snapshot ----
    if (verbose) cat("[Populate] Step 5: Syncing standings snapshot...\n")
    tryCatch({
      teams_for_history <- get_teams(login, championship_id)
      log_user_team_history(teams_for_history,championship_id=championship_id)
      count <- if (!is.null(teams_for_history) && nrow(teams_for_history) > 0) nrow(teams_for_history) else 0L
      if (verbose) print(paste0("[Populate] Logged ", count, " standings snapshot(s)."))
      results[["user_team_history"]] <- list(status = "ok", count = count)
    }, error = function(e) {
      if (verbose) print(paste0("[Populate] Step 5 FAILED: ", e$message))
      results[["user_team_history"]] <- list(status = "error", message = e$message)
    })

    # ---- Step 6: Sync Player History ----
    if (verbose) cat("[Populate] Step 6: Syncing player history...\n")
    tryCatch({
      players_for_history <- get_championship_players(login, championship_id)
      log_player_history(players_for_history, championship_id)
      count <- if (!is.null(players_for_history) && nrow(players_for_history) > 0) nrow(players_for_history) else 0L
      if (verbose) print(paste0("[Populate] Logged ", count, " player history record(s)."))
      results[["player_history"]] <- list(status = "ok", count = count)
    }, error = function(e) {
      if (verbose) print(paste0("[Populate] Step 6 FAILED: ", e$message))
      results[["player_history"]] <- list(status = "error", message = e$message)
    })

    # ---- Step 7: Sync Pressroom Transactions ----
    if (verbose) cat("[Populate] Step 7: Syncing pressroom transactions...\n")
    tryCatch({
      pressroom <- get_championship_pressroom(login, championship_id)
      count <- if (!is.null(pressroom) && nrow(pressroom) > 0) nrow(pressroom) else 0L
      if (verbose) print(paste0("[Populate] Retrieved ", count, " pressroom transactions."))
      sync_pressroom_transactions_to_supabase(pressroom, championship_id)
      results[["market_transactions"]] <- list(status = "ok", count = count)
    }, error = function(e) {
      if (verbose) print(paste0("[Populate] Step 7 FAILED: ", e$message))
      results[["market_transactions"]] <- list(status = "error", message = e$message)
    })

    # ---- Step 8: Sync Round Dream Teams ----
    if (verbose) cat("[Populate] Step 8: Syncing round dream teams...\n")
    tryCatch({
      dreamteam_result <- sync_all_championship_dreamteams(login, championship_id, verbose)
      results[["round_dream_team"]] <- list(
        status = dreamteam_result$status,
        total_rounds = dreamteam_result$total_rounds,
        total_players = dreamteam_result$total_players
      )
    }, error = function(e) {
      if (verbose) print(paste0("[Populate] Step 8 FAILED: ", e$message))
      results[["round_dream_team"]] <- list(status = "error", message = e$message)
    })

    # ---- Step 9: Log Player Daily Snapshots with FIS scores ----
    if (verbose) cat("[Populate] Step 9: Logging player daily snapshots with FIS scores...\n")
    tryCatch({
      players_for_snapshot <- get_championship_players(login, championship_id)
      if (!is.null(players_for_snapshot) && nrow(players_for_snapshot) > 0) {
        # Compute FIS scores
        players_for_snapshot <- calculate_fis_score(players_for_snapshot)
        if (verbose) print(paste0("[Populate] Computed FIS scores for ", nrow(players_for_snapshot), " players."))
        log_player_daily_snapshots(players_for_snapshot, championship_id)
        results[["player_daily_snapshots"]] <- list(
          status = "ok",
          count = nrow(players_for_snapshot)
        )
      } else {
        if (verbose) print("[Populate] No player data for snapshots.")
        results[["player_daily_snapshots"]] <- list(status = "ok", count = 0L)
      }
    }, error = function(e) {
      if (verbose) print(paste0("[Populate] Step 9 FAILED: ", e$message))
      results[["player_daily_snapshots"]] <- list(status = "error", message = e$message)
    })

    # ---- Step 10: Calculate and sync Manager DNA profiles ----
    if (verbose) cat("[Populate] Step 10: Calculating and syncing manager DNA profiles...\n")
    tryCatch({
      teams_for_dna <- get_teams(login, championship_id)
      pressroom_for_dna <- get_championship_pressroom(login, championship_id)

      if (!is.null(teams_for_dna) && nrow(teams_for_dna) > 0) {
        team_ids <- if ("teamid" %in% colnames(teams_for_dna)) teams_for_dna$teamid else if ("id" %in% colnames(teams_for_dna)) teams_for_dna$id else character(0)

        dna_results <- lapply(team_ids, function(tid) {
          calculate_manager_dna(tid, pressroom_for_dna, teams_for_dna)
        })

        # Convert list of DNA results to data frame
        dna_df <- do.call(rbind, lapply(dna_results, function(dna) {
          data.frame(
            team_id = as.character(dna$team_id),
            aggressiveness = as.numeric(dna$aggressiveness),
            avg_overpayment_pct = as.numeric(dna$avg_overpayment_pct),
            fav_position = as.character(dna$fav_position),
            trading_frequency = as.numeric(dna$trading_frequency),
            avg_holding_days = as.numeric(dna$avg_holding_days),
            total_trades = as.integer(dna$total_trades),
            stringsAsFactors = FALSE
          )
        }))

        sync_manager_dna_profiles(dna_df, championship_id)
        if (verbose) print(paste0("[Populate] Synced DNA profiles for ", nrow(dna_df), " teams."))
        results[["manager_dna_profiles"]] <- list(status = "ok", count = nrow(dna_df))
      } else {
        if (verbose) print("[Populate] No team data for DNA profiles.")
        results[["manager_dna_profiles"]] <- list(status = "ok", count = 0L)
      }
    }, error = function(e) {
      if (verbose) print(paste0("[Populate] Step 10 FAILED: ", e$message))
      results[["manager_dna_profiles"]] <- list(status = "error", message = e$message)
    })

  }, error = function(e) {
    print(paste0("[Populate] Fatal error during full database population: ", e$message))
    results[["fatal_error"]] <- list(status = "error", message = e$message)
  })

  return(results)
}
# Explicit conflict targets for natural-key observations, not generated IDs.
supabase_conflict_key <- function(table_name) {
  keys <- c(user_team_history="user_team_id,championship_id,observed_at",
    player_history = "player_id,championship_id,observed_at",
    player_daily_snapshots = "player_id,championship_id,snapshot_date",
    round_dream_team = "championship_id,round_number,player_id",
    market_transactions = "championship_id,source_event_id",
    auction_observations = "championship_id,auction_id",
    bid_observations = "championship_id,auction_id,bidder_id",
    market_observations = "championship_id,listing_id,observed_at",
    forecast_records = "id",
    player_match_observations = "player_id,championship_id,season,scoring_version,round,observed_at",
    league_rule_snapshots = "championship_id,scoring_version,observed_at",
    transfer_scenarios = "user_id,championship_id,user_team_id,name",
    user_smart_alerts = "user_id,championship_id,event_key",
    observation_subscriptions="user_id,championship_id,user_team_id",
    sale_observations="user_id,championship_id,offer_id,observed_at",
    model_evaluations="id",
    fixture_observations="championship_id,season,scoring_version,player_id,round,observed_at",
    auction_opportunities="championship_id,listing_id,observed_at")
  if (table_name %in% names(keys)) unname(keys[[table_name]]) else NULL
}

supabase_post <- function(table_name, payload, conflict = supabase_conflict_key(table_name)) {
  if (isTRUE(getOption("futmondo.offline", FALSE))) return(invisible(204L))
  domain <- if (requireNamespace("shiny", quietly = TRUE)) shiny::getDefaultReactiveDomain() else NULL
  if (!is.null(domain) && exists("defer_persistence", mode = "function")) {
    return(defer_persistence("supabase_post_direct", list(table_name, payload, conflict)))
  }
  supabase_post_direct(table_name, payload, conflict)
}

supabase_read_all <- function(table_name, query = list(), max_rows = 100000L) {
  key <- paste0("db_", table_name, "_", jsonlite::toJSON(query, auto_unbox = TRUE))
  get_cached_data(key, {
    rows <- list()
    for (offset in seq.int(0L, max_rows - 1L, by = 1000L)) {
      q <- query; q$limit <- 1000L; q$offset <- offset
      batch <- supabase_get(table_name, q)
      if (is.null(batch)) stop("Database unavailable")
      if (!nrow(batch)) break
      rows[[length(rows) + 1L]] <- batch
      if (nrow(batch) < 1000L) break
    }
    dplyr::bind_rows(rows)
  }, timeout_sec = 60)
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

save_transfer_scenario <- function(user_id, championship_id, user_team_id, name, scenario) {
  if (!nzchar(trimws(name)) || !nzchar(user_id)) return(FALSE)
  payload <- list(user_id = user_id, championship_id = championship_id, user_team_id = user_team_id,
    name = trimws(name), scenario = scenario,
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))
  # User needs acknowledgement before seeing 'Saved'. Bounded request; unlike
  # background telemetry this user-initiated persistence must return its status.
  code <- supabase_post_direct("transfer_scenarios", payload,
    supabase_conflict_key("transfer_scenarios"))
  success <- is.numeric(code) && code >= 200 && code < 300
  if (isTRUE(success)) clear_api_cache(user_id)
  isTRUE(success)
}
read_transfer_scenarios <- function(user_id, championship_id, user_team_id) {
  tryCatch(supabase_read_all("transfer_scenarios", list(user_id = paste0("eq.", user_id),
    championship_id = paste0("eq.", championship_id), user_team_id = paste0("eq.", user_team_id),
    order = "updated_at.desc")), error = function(e) NULL)
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

collect_account_observations <- function(login, championship_id, user_team_id) {
  if (!valid_login(login)) return(FALSE)
  active <- get_active_championships(login)
  selected <- Filter(function(x) identical(fm_scalar(x$id), as.character(championship_id)) &&
    identical(fm_scalar(x$userteam$id), as.character(user_team_id)), active$championships)
  if (length(selected)!=1L) return(FALSE)
  champ <- selected[[1]]
  sync_championship_to_supabase(unlist(champ))
  players <- get_championship_players(login, championship_id)
  teams <- get_teams(login, championship_id)
  sync_real_clubs_to_supabase(players); sync_players_to_supabase(players)
  sync_user_teams_to_supabase(teams, championship_id)
  log_user_team_history(teams,championship_id=championship_id); log_player_history(players, championship_id)
  # Full-catalog ownership comes from userteamId; unknown market visibility is NA.
  log_player_daily_snapshots(calculate_fis_score(players), championship_id)
  pressroom <- get_championship_pressroom(login, championship_id)
  persist_auction_history(pressroom, championship_id)
  info <- get_user_team_info(login,championship_id,user_team_id)
  lineup <- tryCatch(get_lineup_from_team(login,championship_id,user_team_id),error=function(e)NULL)
  rules <- normalize_league_rules(info,lineup,championship_id)
  version <- rules$scoring_version
  stamp <- format(fm_time(attr(info,'observed_at') %||% NA_character_),"%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
  season <- league_season_context(championship_id,champ$season %||% champ$seasonId)
  if(!is.na(stamp) && !identical(attr(info,'fetch_status'),'stale')) supabase_post("league_rule_snapshots",list(championship_id=championship_id,scoring_version=version,
    season=season,observed_at=stamp,configuration=list(scoring=lineup$custom,point_system=lineup$pointSystem,settings=info$configuration,capabilities=rules)))
  roster <- get_players_from_team(login,championship_id,user_team_id)
  market <- tryCatch(get_market_players(login,championship_id,user_team_id),error=function(e)NULL)
  opportunities<-normalize_listing_opportunities(market,championship_id)
  if(nrow(opportunities)) {
    previous<-tryCatch(supabase_read_all('auction_opportunities',list(championship_id=paste0('eq.',championship_id),order='observed_at.asc')),error=function(e)NULL)
    if(is.data.frame(previous)&&nrow(previous))for(i in seq_len(nrow(opportunities))) {
      known<-previous$first_observed_at[previous$listing_id==opportunities$listing_id[i]]
      if(length(known))opportunities$first_observed_at[i]<-as.character(known[1])
    }
    supabase_post('auction_opportunities',opportunities)
  }
  market_stamp <- attr(market,'observed_at')
  if(!is.null(market) && nrow(market) && !is.null(market_stamp) && !identical(attr(market,'fetch_status'),'stale')) {
    stamp <- format(market_stamp,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')
    expiry_col <- intersect(c("date","expires","expirationDate","expiration"),names(market))
    for(i in seq_len(nrow(market))) {
      expires <- if(length(expiry_col)) fm_scalar(market[[expiry_col[1]]][i]) else NA_character_
      # An unknown listing cycle cannot be joined to a past auction by player alone.
      listing <- if(!is.na(expires)) paste(market$id[i],expires,sep=":") else paste(market$id[i],stamp,sep=":unknown:")
      supabase_post("market_observations",list(championship_id=championship_id,player_id=as.character(market$id[i]),
        listing_id=listing,observed_at=stamp,expires_at=expires,value=fm_number(market$value[i]),
        asking_price=fm_number(market$price[i]),bidder_count=fm_number(market$numberOfBids[i])))
    }
  }
  ids <- unique(as.character(players$id))
  subscription <- tryCatch(supabase_read_all('observation_subscriptions',list(user_id=paste0('eq.',login[['userid']]),
    championship_id=paste0('eq.',championship_id),user_team_id=paste0('eq.',user_team_id))),error=function(e)NULL)
  after <- if(is.data.frame(subscription)&&nrow(subscription)==1L) subscription$last_player_id else NULL
  batch <- collection_player_batch(ids,after,30L)
  completed <- tryCatch(get_finished_rounds(login,championship_id),error=function(e)NULL)
  finished_numbers <- if(is.data.frame(completed)) completed$round_number[completed$is_finished %in% TRUE] else numeric()
  completed_id <- after
  for(id in batch) {
    summary <- tryCatch(get_player_summary(login,championship_id,user_team_id,id),error=function(e)NULL)
    if(is.null(summary)||is.null(attr(summary,'observed_at'))||identical(attr(summary,'fetch_status'),'stale')) next
    fixture<-normalize_fixture_observation(summary,id,championship_id,season,version)
    if(nrow(fixture))supabase_post('fixture_observations',fixture)
    obs <- normalize_player_match_observations(summary,id,championship_id,version,season,
      observed_at=fm_time(attr(summary,'observed_at')),rounds=completed)
    if(nrow(fixture)&&nrow(obs))obs$occurred_at[obs$round==fixture$round]<-fixture$occurred_at
    if(nrow(obs)) obs$score_status[obs$round %in% finished_numbers & is.finite(obs$points)] <- "final"
    if(nrow(obs)) supabase_post("player_match_observations",obs)
    completed_id <- id
  }
  if(length(completed_id)==1L && !is.na(completed_id)) supabase_post('observation_subscriptions',
    list(user_id=login[['userid']],championship_id=championship_id,user_team_id=user_team_id,last_player_id=completed_id))
  # Alerts are supplementary. A transient alert/offer failure must not mark the
  # completed history collection as failed or stop the next collection cycle.
  if(exists("build_insight_alerts",mode="function")) tryCatch({
    fin <- get_financial_snapshot(login,championship_id,user_team_id)
    offers <- tryCatch(get_roster_bids(login,championship_id,user_team_id),error=function(e)NULL)
    sale_rows <- normalize_sale_observations(offers,roster,login[['userid']],championship_id,user_team_id)
    if(nrow(sale_rows)) supabase_post('sale_observations',sale_rows)
    alerts <- build_insight_alerts(login[["userid"]],championship_id,user_team_id,fin,roster,offers,market=market)
    if(nrow(alerts)) supabase_post("user_smart_alerts",alerts)
  }, error=function(e) message("[Observations] Insight alerts skipped for this collection."))
  TRUE
}

read_model_auctions <- function(championship_id, auctions=NULL) {
  d <- if(is.null(auctions))read_auction_observations(championship_id) else auctions
  if(is.null(d) || !nrow(d)) return(d)
  d$visibility <- ifelse(!is.na(d$visibility_complete) & d$visibility_complete,"complete","unknown")
  d$reference_observed_at <- if("reference_at" %in% names(d)) d$reference_at else NA_character_
  if(!"eligible_manager_ids" %in% names(d)) d$eligible_manager_ids <- rep(list(character()),nrow(d))
  if(!"reference_value" %in% names(d)) d$reference_value <- NA_real_
  prices <- read_player_price_history(championship_id,unique(d$player_id))
  if(!is.null(prices) && nrow(prices)) {
    times <- fm_time(prices$observed_at)
    captured <- if('captured_at'%in%names(prices))fm_time(prices$captured_at) else times
    for(i in seq_len(nrow(d))) {
      if(is.finite(fm_number(d$reference_value[i]))) next
      cutoff <- fm_time(d$settled_at[i])
      eligible <- which(prices$player_id==d$player_id[i] & !is.na(times) & times<cutoff & !is.na(captured)&captured<cutoff &
        times>=cutoff-86400 & is.finite(prices$value) & prices$value>0)
      if(length(eligible)) {
        row <- eligible[which.max(times[eligible])]
        d$reference_value[i] <- prices$value[row]
        d$reference_observed_at[i] <- as.character(prices$observed_at[row])
      }
    }
  }
  d
}


# Never mix scoring configurations or seasons. Historic snapshots keep their own cutoff.
read_player_match_history <- function(championship_id, cutoff=Sys.time(), final_only=TRUE,
                                      season=NULL, scoring_version=NULL) {
  q <- list(championship_id=paste0("eq.",championship_id),
    observed_at=paste0("lte.",format(fm_time(cutoff),"%Y-%m-%dT%H:%M:%OSZ",tz="UTC")),
    order="observed_at.asc")
  if (isTRUE(final_only)) q$score_status <- "eq.final"
  if (!is.null(season)) q$season <- paste0("eq.",season)
  if (!is.null(scoring_version)) q$scoring_version <- paste0("eq.",scoring_version)
  d <- tryCatch(supabase_read_all("player_match_observations",q),error=function(e)NULL)
  if (is.null(d) || !nrow(d)) return(d)
  t <- fm_time(d$observed_at)
  d <- d[!is.na(t) & t<=fm_time(cutoff),,drop=FALSE]
  if (!nrow(d)) return(d)
  d <- d[!is.na(d$season) & d$season!="unknown" & !is.na(d$scoring_version) & d$scoring_version!="unknown",,drop=FALSE]
  if(!nrow(d)) return(d)
  d$is_final <- d$score_status=="final"
  if (isTRUE(final_only)) d <- d[d$is_final %in% TRUE,,drop=FALSE]
  if (!nrow(d)) return(d)
  # Default to the latest observed season/scoring context; no cross-mode pooling.
  context <- d[which.max(fm_time(d$observed_at)),,drop=FALSE]
  d <- d[d$season==context$season & d$scoring_version==context$scoring_version,,drop=FALSE]
  fixtures<-tryCatch(supabase_read_all('fixture_observations',list(championship_id=paste0('eq.',championship_id),
    season=paste0('eq.',context$season),scoring_version=paste0('eq.',context$scoring_version),
    observed_at=paste0('lte.',format(fm_time(cutoff),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')),order='observed_at.asc')),error=function(e)NULL)
  if(is.data.frame(fixtures)&&all(c('player_id','round','occurred_at','observed_at')%in%names(fixtures))) {
    fixtures<-fixtures[!is.na(fm_time(fixtures$observed_at))&fm_time(fixtures$observed_at)<=fm_time(cutoff),,drop=FALSE]
    fixtures<-fixtures[!duplicated(fixtures[c('player_id','round')],fromLast=TRUE),,drop=FALSE]
    if(!'occurred_at'%in%names(d))d$occurred_at<-NA_character_
    idx<-match(paste(d$player_id,d$round),paste(fixtures$player_id,fixtures$round))
    d$occurred_at[!is.na(idx)]<-as.character(fixtures$occurred_at[idx[!is.na(idx)]])
  }
  d$round_id <- paste(d$season,d$scoring_version,d$round,sep=":")
  d <- d[order(fm_time(d$observed_at)),,drop=FALSE]
  d[!duplicated(d[c("player_id","round_id")],fromLast=TRUE),,drop=FALSE]
}

persist_forecast <- function(user_id, championship_id, player_id=NULL, model_version,
                             forecast_type, cutoff, horizon=NULL, prediction, season=NULL, scoring_version=NULL) {
  if (!nzchar(fm_scalar(user_id,"")) || !nzchar(fm_scalar(championship_id,""))) return(FALSE)
  stamp <- format(fm_time(cutoff),"%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
  if (length(stamp)!=1L || is.na(stamp)) return(FALSE)
  key <- jsonlite::toJSON(list(user_id,championship_id,player_id,model_version,forecast_type,stamp,horizon,prediction,season,scoring_version),auto_unbox=TRUE)
  payload <- list(id=as.character(openssl::sha256(charToRaw(key))),user_id=user_id,
    championship_id=championship_id,player_id=player_id,model_version=model_version,
    forecast_type=forecast_type,cutoff=stamp,horizon=horizon,prediction=prediction,
    season=season,scoring_version=scoring_version)
  supabase_post("forecast_records",payload)
}

record_forecast_outcome <- function(user_id, championship_id, forecast_id, outcome, evaluated_at=Sys.time()) {
  # Explicit ownership prevents a caller from attaching outcomes to another account.
  supabase_patch("forecast_records",list(outcome=outcome,
    evaluated_at=format(evaluated_at,"%Y-%m-%dT%H:%M:%OSZ",tz="UTC")),
    list(id=paste0("eq.",forecast_id),user_id=paste0("eq.",user_id),
      championship_id=paste0("eq.",championship_id),evaluated_at="is.null"))
}
