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

supabase_post <- function(table_name, payload) {
  # Defensive check for loaded credentials
  sb_url <- get_sb_url()
  sb_key <- get_sb_key()
  if (is.null(sb_url) || sb_url == "" || is.null(sb_key) || sb_key == "") {
    print("[Supabase] Skipping sync: Supabase environment credentials are not loaded in .Renviron.")
    return(NULL)
  }
  
  if (is.null(payload) || (is.data.frame(payload) && nrow(payload) == 0) || (is.list(payload) && length(payload) == 0)) {
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
    response <- POST(url, body = toJSON(payload, auto_unbox = TRUE, na = "null"), add_headers(.headers = headers))
    code <- status_code(response)
    if (code >= 200 && code < 300) {
      print(paste0("[Supabase] Successfully synced data to table: ", table_name, " (HTTP ", code, ")"))
    } else {
      print(paste0("[Supabase] Warning: Received HTTP code ", code, " from table: ", table_name))
    }
    return(code)
  }, error = function(e) {
    print(paste0("[Supabase] Connection error during post to table ", table_name, ": ", e$message))
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
  if (is.null(clubs_df) || nrow(clubs_df) == 0) return()
  
  # Expected columns: teamId, team, logo
  if (!"teamId" %in% colnames(clubs_df)) return()
  
  payload <- clubs_df %>%
    dplyr::select(id = teamId, name = team, logo) %>%
    dplyr::distinct(id, .keep_all = TRUE)
  
  supabase_post("real_clubs", payload)
}

sync_players_to_supabase <- function(players_df) {
  if (is.null(players_df) || nrow(players_df) == 0) return()

  # Ensure minimum expected columns are present
  required <- c("id", "name", "slug")
  if (!all(required %in% colnames(players_df))) return()

  # Sanitize real_club_id: NA, NULL, or empty string become NA_character_
  # so PostgreSQL FK real_clubs(id) accepts it as NULL
  real_club_raw <- if ("teamId" %in% colnames(players_df)) as.character(players_df$teamId) else rep(NA_character_, nrow(players_df))
  real_club_raw[is.na(real_club_raw) | real_club_raw == ""] <- NA_character_

  # Clean up rating: coerce to integer, replacing NaN / string artefacts with NA
  raw_rating <- if ("rating" %in% colnames(players_df)) players_df$rating else rep(NA_integer_, nrow(players_df))
  rating_num <- suppressWarnings(as.numeric(raw_rating))
  rating <- as.integer(rating_num)
  rating[is.na(rating) | is.nan(rating_num)] <- NA_integer_

  payload <- data.frame(
    id = as.character(players_df$id),
    name = as.character(players_df$name),
    slug = as.character(players_df$slug),
    role = if ("role" %in% colnames(players_df)) as.character(players_df$role) else NA_character_,
    role2 = if ("role2" %in% colnames(players_df)) as.character(players_df$role2) else NA_character_,
    photo = if ("photo" %in% colnames(players_df)) as.character(players_df$photo) else NA_character_,
    real_club_id = real_club_raw,
    status = if ("status" %in% colnames(players_df)) as.character(players_df$status) else NA_character_,
    rating = rating,
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
    budget = if ("budget" %in% colnames(teams_df)) as.numeric(teams_df$budget) else 0,
    points = if ("points" %in% colnames(teams_df)) as.integer(teams_df$points) else 0,
    position = if ("position" %in% colnames(teams_df)) as.integer(teams_df$position) else NA_integer_,
    team_value = if ("team_value" %in% colnames(teams_df)) as.numeric(teams_df$team_value) else if ("teamValue" %in% colnames(teams_df)) as.numeric(teams_df$teamValue) else 0,
    is_active = if ("is_active" %in% colnames(teams_df)) as.logical(teams_df$is_active) else TRUE,
    stringsAsFactors = FALSE
  )

  payload <- payload %>% dplyr::distinct(id, .keep_all = TRUE)

  supabase_post("user_teams", payload)
}

log_user_team_history <- function(teams_df, round_number = NULL) {
  if (is.null(teams_df) || nrow(teams_df) == 0) return()
  
  team_ids <- if ("teamid" %in% colnames(teams_df)) teams_df$teamid else if ("id" %in% colnames(teams_df)) teams_df$id else return()
  active_cnt <- length(unique(team_ids))
  
  payload <- data.frame(
    user_team_id = as.character(team_ids),
    points = if ("points" %in% colnames(teams_df)) as.integer(teams_df$points) else 0,
    budget = if ("budget" %in% colnames(teams_df)) as.numeric(teams_df$budget) else 0,
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
    response <- GET(url, query = query_params, add_headers(.headers = headers))
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
    response <- httr::DELETE(url, query = filter_list, add_headers(.headers = headers))
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

supabase_delete_all <- function(table_name) {
  bigint_pk_tables <- c("user_team_history", "player_history", "market_transactions", "round_dream_team")
  text_pk_tables <- c("championships", "real_clubs", "players", "user_teams")

  if (table_name %in% bigint_pk_tables) {
    supabase_delete(table_name, filter = "id=gte.0")
  } else if (table_name %in% text_pk_tables) {
    supabase_delete(table_name, filter = "id=neq.00000000-0000-0000-0000-000000000000")
  } else {
    print(paste0("[Supabase] Unknown PK type for table: ", table_name))
    return(list(status = "error", reason = "unknown PK type"))
  }
}

supabase_reset_database <- function(force = FALSE) {
  if (!force) {
    print("[Supabase] Reset cancelled: set force = TRUE to proceed.")
    return(list())
  }

  reset_order <- c(
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
    "round_dream_team"
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
      response <- GET(url, query = list(select = "id", limit = "0"), add_headers(.headers = headers))
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
    "round_dream_team"
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
      response <- GET(url, query = list(select = "id", limit = "1"), add_headers(.headers = headers))
      code <- status_code(response)
      if (code == 200) {
        if (verbose) cat(paste0("[Init] Table OK: ", tbl, " (HTTP 200)\n"))
      } else {
        all_ok <- FALSE
        warning(paste0("[Init] Table check failed for '", tbl, "': HTTP ", code))
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
      log_user_team_history(teams_for_history)
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

  }, error = function(e) {
    print(paste0("[Populate] Fatal error during full database population: ", e$message))
    results[["fatal_error"]] <- list(status = "error", message = e$message)
  })

  return(results)
}